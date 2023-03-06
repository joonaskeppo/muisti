(ns muisti.lang.tokenizer
  (:require [clojure.string :as str]
            [muisti.utils :refer [syms->map cond-with]]
            [muisti.lang.utils :refer [add-error read-delimited]]))

(defn make-token
  [{:keys [type lexeme line value]
    :or   {line 1}}]
  (syms->map type lexeme line value))

(defn make-scanner
  [{:keys [src start current line errors]
    :or   {start 0 current 0 line 1 errors []}}]
  (syms->map src start current line errors))

(defn current-char [{:keys [current src] :as _scanner}]
  (get src current))

(defn previous-char [{:keys [current src] :as _scanner}]
  (get src (dec current)))

(defn at-end? [{:keys [current src]}]
  (>= current (count src)))

(defn inc-line [scanner]
  (update scanner :line inc))

(declare newline?)

(defn advance
  ([scanner]
   (advance scanner 1))
  ([scanner num]
   (loop [scanner scanner num num]
     (if (zero? num)
       scanner
       (if (newline? scanner)
         (recur (update (inc-line scanner) :current inc) (dec num))
         (recur (update scanner :current inc) (dec num)))))))

(defn advance-while
  ([scanner pred]
   (advance-while scanner pred identity))
  ([scanner pred iter]
   (->> scanner
        (iterate (comp iter advance))
        (drop-while pred)
        first)))

(defn rem-src
  "Get remaining of `src`"
  [{:keys [src current] :as _scanner}]
  (subs src current))

(defn add-token
  [{:keys [line] :as scanner} args]
  (let [token (make-token (update args :line #(or % line)))]
    (update scanner :tokens #(cons token %))))

(defn append-text-token
  "Either add a new text token, or expand last text token (if found)"
  [scanner text]
  (let [{:keys [type] :as token} (first (:tokens scanner))]
    (if (= ::text type)
      (let [token* (update token :lexeme #(str % text))]
        (update scanner :tokens (comp #(cons token* %) rest)))
      (add-token scanner {:type ::text :lexeme (str text)}))))

(defn multiple-newlines?
  "Does `src` start with whitespace containing two or more newlines?"
  [src]
  (->> src (take-while #(Character/isWhitespace %)) (filter #(= \newline %)) count (<= 2)))

(declare regexes)

(defn extract-front-matter
  "Extracts front matter.
  Assumes newline-separated key-value pairs, where keys are keywords and values are any EDN values."
  [src]
  (loop [src* src
         fm-str ""]
    (cond
      (or (multiple-newlines? src*) (str/blank? src*))
      fm-str

      (Character/isWhitespace (first src*))
      (recur (subs src* 1) (str fm-str (first src*)))

      (re-find (:keyword regexes) (str/triml src*))
      (let [line      (->> src* (take-while #(not= \newline %)) (apply str))
            remainder (->> src* (drop-while #(not= \newline %)) (apply str))]
        (recur remainder (str fm-str line)))

      :else
      nil)))

(defn get-line-tokens [{:keys [tokens line] :as _scanner}]
  (take-while #(= line (:line %)) tokens))

(defn get-non-blank-tokens [scanner]
  (->> (get-line-tokens scanner)
       (filter #(not (#{::newline ::tab} (:type %))))))

;; --- predicates ---

(defn nothing-left? [scanner]
  (str/blank? (rem-src scanner)))

(defn blank? [scanner]
  (when-let [ch (current-char scanner)]
    (str/blank? (str ch))))

(defn numeric? [scanner]
  (when-let [ch (current-char scanner)]
    (try
      (Integer/parseInt (str ch))
      (catch Exception _ false))))

(defn newline? [scanner]
  (= \newline (current-char scanner)))

(def whitespace?
  (every-pred blank? (complement newline?)))

(defn front-matter? [{:keys [tokens] :as scanner}]
  (and (not (some #(not= ::newline (:type %)) tokens))
       (extract-front-matter (str/triml (rem-src scanner)))))

(def regexes
  "Some regexes to check for token existence, *not* for parsing"
  {:tab              #"^(\t|\s{2,})"
   :keyword          #"^:[a-zA-Z\d\*\+\!\-\_\?]+"
   :component        #"^\[:[a-zA-Z\d\*\+\!\-\_\?]+\s+"
   :heading          #"^\s*\#{1,}\s+[^\s]+"
   :unordered-bullet #"^(\-|\*|\+)\s+"
   :ordered-bullet   #"^\d+\.\s+"
   :blockquote       #"^\>\s+[^\s]+.*"})

(defn comment?
  "If a comment token encountered on current line.
  No content tokens can be found on line."
  [scanner]
  (and (= \; (current-char scanner))
       (empty? (get-non-blank-tokens scanner))))

(defn blockquote?
  "Blockquote token next?"
  [scanner]
  (and (re-find (:blockquote regexes) (rem-src scanner))
       (empty? (get-non-blank-tokens scanner))))

(defn tab?
  "A 'tab' at start of blank line?"
  [scanner]
  (and (re-find (:tab regexes) (rem-src scanner))
       (empty? (get-non-blank-tokens scanner))))

(defn component?
  "Hiccup-looking 'component'?"
  [scanner]
  (re-find (:component regexes) (rem-src scanner)))

(defn heading?
  [scanner]
  (and (re-find (:heading regexes) (rem-src scanner))
       (empty? (get-non-blank-tokens scanner))))

(defn unordered-bullet?
  [scanner]
  (and (re-find (:unordered-bullet regexes) (rem-src scanner))
       (empty? (get-non-blank-tokens scanner))))

(defn ordered-bullet?
  [scanner]
  (and (re-find (:ordered-bullet regexes) (rem-src scanner))
       (empty? (get-non-blank-tokens scanner))))

(defn code-block?
    [scanner]
  ;; TODO: should check for newline before ticks?
    (= "```" (apply str (take 3 (rem-src scanner)))))

;; --- scanning ---

(defn scan-front-matter [scanner]
  (let [scanner (advance-while scanner blank?)]
    (if-let [fm-str (extract-front-matter (rem-src scanner))]
      (-> scanner
          (add-token {:type ::front-matter :lexeme fm-str})
          (advance (count fm-str))
          (advance-while newline?)) ;; remove any blank lines prior to actual content
      (-> scanner
          (add-error "Unable to scan front matter")
          ;; force scanner to end as we can't continue
          (assoc :src "")))))

(defn scan-component [{:keys [line] :as scanner}]
  (let [out-str (read-delimited (rem-src scanner) "[" "]")]
    (-> scanner
        (advance (count out-str))
        (add-token {:type ::component :line line :lexeme out-str}))))

(defn scan-blockquote
  [{:keys [line] :as scanner}]
  (loop [scanner        scanner
         blockquote-str ""]
    (if (nothing-left? scanner)
      (add-token scanner {:type ::blockquote :line line :lexeme blockquote-str})
      (case (current-char scanner)
        \newline (if (= \newline (current-char (advance scanner))) ;; start of next block -> end this one
                   (add-token scanner {:type ::blockquote :line line :lexeme blockquote-str})
                   (recur (advance scanner) (str blockquote-str "\n")))
        \>       (let [scanner   (-> scanner advance (advance-while whitespace?))
                       next-part (->> scanner rem-src (take-while #(not= \newline %)) (apply str))]
                   (recur (advance scanner (count next-part))
                          (str blockquote-str next-part)))
        ;; otherwise (e.g., \tab or breaking chars)
        (let [scanner (advance-while scanner whitespace?)]
          (if (not (#{\> \newline} (current-char scanner)))
            (add-token scanner {:type ::blockquote :line line :lexeme blockquote-str})
            (recur scanner blockquote-str)))))))

(defn scan-comment
  "Ignore line completely"
  [{:keys [line] :as scanner}]
  (-> scanner
      (update :tokens (fn [tkns] (drop-while #(= line (:line %)) tkns)))
      (advance-while (complement newline?))))

(defn scan-tab [scanner]
  (let [single-tab-spaces         2
        [blanks-count blanks-val] (->> (rem-src scanner)
                                       (take-while #(str/blank? (str %)))
                                       (reduce (fn [[blanks val] v]
                                                 (case v
                                                   \tab [(inc blanks) (+ val single-tab-spaces)]
                                                   [(inc blanks) (inc val)]))
                                               [0 0]))]
    (-> scanner
        (advance blanks-count)
        (add-token {:type ::tab :value (int (Math/floor (/ blanks-val single-tab-spaces)))}))))

(defn scan-heading [scanner]
  (let [scanner (advance-while scanner whitespace?) ;; get rid of any left padding
        hashes  (count (take-while #(= \# %) (rem-src scanner)))
        scanner (advance-while (advance scanner hashes) whitespace?)
        text    (->> (rem-src scanner) (take-while #(not= \newline %)) (apply str))]
    (-> (advance scanner (count text))
        (add-token {:type ::heading :lexeme text :value hashes}))))

(defn scan-text [scanner]
  (if-let [text (some->> (rem-src scanner)
                         (take-while #(not= \newline %))
                         (apply str)
                         (re-find #"^(\w|\d|\,|\.|\s|\:|\;)+")
                         first)]
    (-> scanner (append-text-token text) (advance (count text)))
    (if (= \\ (current-char scanner)) ;; escape the escape
      (advance scanner)
      (-> scanner (append-text-token (current-char scanner)) advance))))

(defn scan-newline [scanner]
  (-> scanner advance (add-token {:type ::newline})))

(defn scan-unordered-bullet [scanner]
  (-> scanner
      (add-token {:type ::unordered-bullet})
      advance
      (advance-while whitespace?)))

(defn scan-ordered-bullet [scanner]
  (-> scanner
      (add-token {:type ::ordered-bullet})
      (advance-while numeric?)
      advance ;; skip over '.'
      (advance-while whitespace?)))

(defn scan-code-block [scanner]
  (let [out-str        (read-delimited (rem-src scanner) "```\n" "\n```" {:drop-delimiters true})
        delimiters-len 8]
    (-> scanner
        (add-token {:type ::code-block :lexeme out-str})
        (advance (+ delimiters-len (count out-str))))))

(defn scan-token [scanner]
  (cond-with scanner
             nothing-left?     (advance-while scanner blank?)
             comment?          (scan-comment scanner)
             front-matter?     (scan-front-matter scanner)
             newline?          (scan-newline scanner)
             tab?              (scan-tab scanner)
             component?        (scan-component scanner)
             blockquote?       (scan-blockquote scanner)
             heading?          (scan-heading scanner)
             code-block?       (scan-code-block scanner)
             unordered-bullet? (scan-unordered-bullet scanner)
             ordered-bullet?   (scan-ordered-bullet scanner)
             some?             (scan-text scanner)))

(defn scan-tokens
  ([src]
   (scan-tokens src {:line 1}))
  ([src {:keys [line]}]
   (loop [{:keys [current] :as scanner} (make-scanner {:src src :line line})]
     (if (at-end? scanner)
       (-> scanner (add-token {:type ::eof}) :tokens reverse)
       (recur (-> scanner (assoc :start current) scan-token))))))
