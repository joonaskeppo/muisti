(ns muisti.lang.component
  (:require [muisti.utils :refer [deep-merge]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn whitespace? [x]
  (or (Character/isWhitespace x)
      ;; Clojure interprets commas as whitespace (and we will too)
      (= \, x)))

(def not-whitespace?
  (complement whitespace?))

(def ^:private default-extractors
  {::vector            (fn [{:keys [input]}]
                         (when-let [ext (edn/read-string input)]
                           (when (vector? ext) ext)))
   ::map               (fn [{:keys [input]}]
                         (when-let [ext (edn/read-string input)]
                           (when (map? ext) ext)))
   ::identity          (fn [{:keys [input]}] input)
   ::&edn              (fn [{:keys [input]}]
                          (->> (str/split input #" ")
                               (filter (complement str/blank?))
                               (mapv edn/read-string))) })

(defn extract-input-with-single-pattern
  "Extracts input string with a single `extraction-fs` pattern vector"
  [input token extraction-fs]
  (loop [acc               []
         input             input
         [next-ef :as efs] extraction-fs
         line              (:line token)]
    (cond
      (and (empty? input) (empty? efs))
      acc

      (or (empty? input) (empty? efs))
      nil

      :else
      (let [line      (+ line (count (take-while #(= \newline %) (seq input))))
            input     (apply str (drop-while whitespace? input))
            next-ef   (get default-extractors next-ef next-ef)
            extracted (try
                        (next-ef {:input input :line line})
                        (catch Exception _ ::error))]
        (when-not (#{::error nil} extracted)
          (recur (conj acc extracted)
                 (drop (count (str extracted)) input)
                 (rest efs)
                 line))))))

(defn extract
  "Extract input with first (or only) matching case of `extraction-patterns`"
  [input token extraction-patterns]
  (if-not (every? vector? extraction-patterns)
    (extract-input-with-single-pattern input token extraction-patterns)
    (loop [[pattern :as patterns] extraction-patterns]
      (when (seq patterns)
        (or (extract-input-with-single-pattern input token pattern)
            (recur (rest patterns)))))))

(defn split-lexeme-parts
  "Split `lexeme` into component type and input string"
  [lexeme]
  (let [lexeme         (subs lexeme 1 (- (count lexeme) 1)) ;; drop square brackets
        component-type (->> lexeme (drop 1) (take-while not-whitespace?) (apply str))
        input          (->> lexeme (drop (inc (count component-type))) (drop-while whitespace?) (apply str))]
    [(keyword component-type) input]))

(defn resolve-uri
  [env to]
  (cond
    (string? to) to
    (vector? to) (let [fmt-str (if (str/ends-with? (:root-path env) "/")
                                 "%s%s" "%s/%s")]
                   (format fmt-str (:root-path env) (str/join "/" to)))))

(def formatter->hiccup
  "Syntax sugar for text formatters"
  {:b :strong
   :i :em
   :d :del
   :u :u
   :c :code})

(defmulti parse
  (fn [{:keys [type]}] type))

(defmethod parse :link
  [{:keys [env input token parse-fn]}]
  (let [[destination title] (extract input token [::vector (parse-fn {:modifier :verbatim})])
        to                  (mapv str destination)]
    (-> title
        (update :attrs deep-merge {:links #{{:note/id to}}})
        (update :hiccup (fn [inner]
                          (into [:a {:href (resolve-uri env to)}] inner))))))

(defmethod parse :tag
  [{:keys [input token]}]
  (let [extractor-fns [[::&edn]]
        [tags]        (extract input token extractor-fns)]
    {:attrs  {:tags             (set tags)
              ;; generate mapping of tag -> tokens
              :muisti.meta/tags (->> (distinct tags) ;; retain ordering
                                     (map (fn [tag] [tag [token]]))
                                     (into {}))}
     :hiccup (into [:span {:class "mu-tags-group"} ]
                   (for [tag tags]
                     [:span {:class "mu-tag"} (str tag)]))}))

(defmethod parse :default
  [{:keys [type input token parse-fn]}]
  (let [ext     (extract input token [[::map (parse-fn {:modifier :verbatim})]
                                      [(parse-fn {:modifier :verbatim})]])
        props   (when (= 2 (count ext)) (first ext))
        content (peek ext)
        type    (get formatter->hiccup type type)] ;; by default, assume sugar
    (update content :hiccup (fn [inner]
                              (if props
                                (into [type props] inner)
                                (into [type] inner))))))
