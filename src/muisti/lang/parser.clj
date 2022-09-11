(ns muisti.lang.parser
  (:require [clojure.edn :as edn]
            [muisti.utils :refer [syms->map deep-merge]]
            [muisti.lang.tokenizer :as tk]
            [muisti.lang.component :as cmp]
            [muisti.lang.utils :refer [add-error children]])
  (:refer-clojure :exclude [peek]))

(defn make-parser
  [{:keys [env tokens current output aux errors]
    :or   {current 0
           output  {:hiccup [] :attrs {}}
           errors  []}}]
  (syms->map env tokens current output aux errors))

(def non-paragraph-tokens
  #{::tk/eof
    ::tk/front-matter
    ::tk/heading
    ::tk/code-block
    ::tk/blockquote})

(def bullet-tokens
  #{::tk/unordered-bullet
    ::tk/ordered-bullet})

(def paragraph-tokens
  (complement non-paragraph-tokens))

(defn peek [{:keys [tokens current]}]
  (get (vec tokens) current))

(def peek-type
  (comp :type peek))

(defn peek-next [{:keys [tokens current]}]
  (get (vec tokens) (inc current)))

(def peek-next-type
  (comp :type peek-next))

(defn get-next-tokens [{:keys [tokens current]}]
  (vec (drop current tokens)))

(defn advance
  ([parser]
   (advance parser 1))
  ([parser num]
   (update parser :current #(+ % num))))

(defn advance-while
  ([parser pred]
   (advance-while parser pred identity))
  ([parser pred iter]
   (->> parser
        (iterate (comp iter advance))
        (drop-while pred)
        first)))

(defn done? [parser]
  (empty? (get-next-tokens parser)))

(defn end-nested-context?
  [parser]
  (or (non-paragraph-tokens (peek-type parser))
      (= ::tk/newline (peek-type parser) (peek-next-type parser))))

(declare get-next-bullet current-list-item)

(defn less-nested-list?
  [parser]
  (when-let [next-bullet (get-next-bullet parser)]
    (let [prev-bullet (current-list-item (update parser :current dec))]
      ;; "pop" stack of lists
      (< (:value next-bullet)
         (:value prev-bullet)))))

(defn nest-list?
  "Create a new list within a list item"
  [parser]
  (when-let [next-bullet (get-next-bullet parser)]
    (let [prev-bullet (current-list-item (update parser :current dec))]
      (and (= (:type next-bullet) (:type prev-bullet))
           (> (:value next-bullet)
              (:value prev-bullet))))))

(defn new-list?
  "Create a new list, separate from any previous ones?"
  [parser]
  (when-let [next-bullet (get-next-bullet parser)]
    (let [prev-bullet (current-list-item (update parser :current dec))]
      (or (nil? prev-bullet)
          (not= (:type next-bullet) (:type prev-bullet))
          (< (:value next-bullet)
             (:value prev-bullet))))))

(defn new-list-item?
  [parser]
  (let [t1 (peek-type parser)
        t2 (peek-next-type parser)]
    (or (bullet-tokens t1)
        (and (= ::tk/tab t1)
             (bullet-tokens t2)))))

(defn end-list?
  "End current list?"
  [parser]
  (or (end-nested-context? parser)
      (less-nested-list? parser)))

(defn new-paragraph?
  [{{:keys [hiccup]}   :output
    {:keys [inner]}    :aux
    {:keys [modifier]} :env
    :as                parser}]
  (let [type-first  (peek-type parser)
        type-second (peek-next-type parser)]
    (and (not (non-paragraph-tokens type-first))
         (not inner)
         (not (#{:inline :verbatim} modifier))
         (or (empty? hiccup)
             (and (= ::tk/newline type-first)
                  (= ::tk/newline type-second)
                  (paragraph-tokens
                   (peek-type (advance-while parser #(= ::tk/newline (peek-type %))))))))))

(defn current-list-item
  "Finds current list item context, if available"
  [{:keys [tokens] :as parser}]
  (let [rev-tokens (->> tokens (take (inc (:current parser))) reverse)]
    (loop [[tk-1 tk-2 :as rtkns] rev-tokens]
      (cond
        (or (empty? rtkns)
            ;; if two consecutive newlines -> separate list/paragraph
            (= ::tk/newline (:type tk-1) (:type tk-2)))
        nil

        (#{::tk/unordered-bullet ::tk/ordered-bullet} (:type tk-1))
        {:type  (:type tk-1)
         :value (if (= ::tk/tab (:type tk-2)) (:value tk-2) 0)}

        :else
        (recur (rest rtkns))))))

(defn get-next-bullet
  [parser]
  (let [has-tab   (= ::tk/tab (peek-type parser))
        tab-value (if has-tab (:value (peek parser)) 0)
        ?bullet   (if has-tab
                    (peek-next parser)
                    (peek parser))]
    (when (bullet-tokens (:type ?bullet))
      {:type  (:type ?bullet)
       :value tab-value})))

(declare parse parse-token-group)

(defn parse-until
  [{:keys [pred parser context]}]
  (loop [[last-parser :as parsers] (list (assoc parser :output {:attrs {} :hiccup []}))]
    (cond
      (empty? (get-next-tokens last-parser))
      (-> parser
          (add-error (format "Failed parsing in context: %s" context))
          (assoc :tokens []))

      (pred last-parser)
      (let [parsers (reverse parsers)
            output  {:attrs  (apply deep-merge (map (comp :attrs :output) parsers))
                     :hiccup (reduce into [] (map (comp :hiccup :output) parsers))}]
        (assoc parser :output output :current (:current last-parser)))

      :else
      (recur (conj parsers (-> last-parser
                               (assoc-in [:aux :inner] true)
                               (assoc :output {})
                               parse-token-group))))))

(defn parse-inline-formatter
  [[token-type html-elt] parser]
  (let [inner-parser (parse-until {:pred    (fn [inner-parser]
                                              (= token-type (peek-type inner-parser)))
                                   :parser  (advance parser)
                                   :context "inline formatter"})]
    (-> parser
        (update-in [:output :hiccup] conj (into [html-elt] (get-in inner-parser [:output :hiccup])))
        (update-in [:output :attrs] deep-merge (get-in inner-parser [:output :attrs]))
        (assoc :current (inc (:current inner-parser))))))

(declare parse-list parse-list-item)

(defn parse-list-item
  [parser list-start-idx]
  (let [pre-list-item? #(#{::tk/tab ::tk/unordered-bullet ::tk/ordered-bullet} (peek-type %))
        inner-parser   (if (and (> (:current parser) list-start-idx) (nest-list? parser))
                         (parse-list parser)
                         (parse-until {:pred    (some-fn new-list-item? end-list?)
                                       :parser  (advance-while parser pre-list-item?)
                                       :context "list item"}))]
    (-> parser
        (assoc :current (:current inner-parser))
        (update-in [:output :attrs] deep-merge (get-in inner-parser [:output :attrs]))
        (update-in [:output :hiccup] conj (into [:li] (get-in inner-parser [:output :hiccup]))))))

(defn parse-list
  [parser]
  (let [parser                 (advance-while parser #(= ::tk/tab (peek-type %)))
        start-idx              (:current parser)
        list-html-elt          (if (= ::tk/unordered-bullet (peek-type parser)) :ul :ol)]
    (loop [[last-parser :as parsers] (list parser)]
      (cond
        (and (> (:current last-parser) start-idx) (end-list? last-parser))
        (let [parsers (reverse parsers)
              output  {:attrs  (apply deep-merge (map (comp :attrs :output) parsers))
                       :hiccup (vector (into [list-html-elt] (reduce into [] (map (comp :hiccup :output) parsers))))}]
          (assoc parser :output output :current (:current last-parser)))

        :else
        (recur (conj parsers (-> last-parser
                                 (advance-while #(= ::tk/newline (peek-type %)))
                                 (assoc :output {:attrs {} :hiccup []})
                                 (parse-list-item start-idx))))))))

(declare parse-component)

(defn parse-component
  "Parses a component token.
  For both normal usage and nested-within-Hiccup parsing."
  [{:keys [env] :as parser}]
  (let [{:keys [lexeme] :as tk} (peek parser)
        [component-type input]  (cmp/split-lexeme-parts lexeme)
        output                  (cmp/parse {:env      env
                                            :type     component-type
                                            :input    input
                                            :token    tk
                                            :parse-fn (fn [args]
                                                        (fn [{:keys [input] :as env*}]
                                                          (->> (dissoc env* :input)
                                                               (deep-merge env args)
                                                               (parse input))))})]
    (-> parser
        advance
        (update :output deep-merge (update output :hiccup #(conj [] %))))))

;; --- token groups ---

(defmulti parse-token-group
  (fn [parser]
    (cond
      (new-paragraph? parser) :paragraph
      (new-list? parser)      :list
      :else                   (peek-type parser))))

(defmethod parse-token-group :list
  [parser]
  (parse-list (assoc parser :output {:attrs {} :hiccup []})))

(defmethod parse-token-group :paragraph
  [parser]
  (let [parser       (advance-while parser #(= ::tk/newline (peek-type %)))
        inner-parser (parse-until {:pred end-nested-context? :parser parser :context "paragraph"})]
    (-> parser
        (assoc :current (:current inner-parser))
        (update-in [:output :attrs] deep-merge (get-in inner-parser [:output :attrs]))
        (update-in [:output :hiccup] conj (into [:p] (get-in inner-parser [:output :hiccup]))))))

;; a special type of newline that doesn't delineate paragraphs
(defmethod parse-token-group ::tk/newline
  [parser]
  (advance parser))

;; see above
(defmethod parse-token-group ::tk/tab
  [parser]
  (advance parser))

(defmethod parse-token-group ::tk/front-matter
  [parser]
  (let [{:keys [lexeme]} (peek parser)]
    (-> parser
        advance
        (assoc-in [:output :attrs :front-matter] (edn/read-string lexeme)))))

(defmethod parse-token-group ::tk/blockquote
  [{:keys [env] :as parser}]
  (let [{:keys [lexeme]} (peek parser)
        inner            (parse lexeme env)]
    (-> parser
        advance
        (update-in [:output :hiccup] conj (into [:blockquote] (children (:hiccup inner)))))))

(defmethod parse-token-group ::tk/heading
  [parser]
  (let [{:keys [lexeme value]} (peek parser)]
    (-> parser
        advance
        (update-in [:output :hiccup] conj [(keyword (str "h" value)) lexeme]))))

(defmethod parse-token-group ::tk/code-block
  [parser]
  (let [{:keys [lexeme]} (peek parser)]
    (-> parser
        advance
        (update-in [:output :hiccup] conj [:code lexeme]))))

(defmethod parse-token-group ::tk/text
  [parser]
  (let [{:keys [lexeme]} (peek parser)]
    (-> parser
        advance
        (update-in [:output :hiccup] conj lexeme))))

(defmethod parse-token-group ::tk/bold
  [parser]
  (parse-inline-formatter [::tk/bold :strong] parser))

(defmethod parse-token-group ::tk/italic
  [parser]
  (parse-inline-formatter [::tk/italic :em] parser))

(defmethod parse-token-group ::tk/strikethrough
  [parser]
  (parse-inline-formatter [::tk/strikethrough :del] parser))

(defmethod parse-token-group ::tk/code-inline
  [parser]
  (let [{:keys [lexeme]} (peek parser)]
    (-> parser
        advance
        (update-in [:output :hiccup] conj [:pre lexeme]))))

(defmethod parse-token-group ::tk/code-block
  [parser]
  (let [{:keys [lexeme]} (peek parser)]
    (-> parser
        advance
        (update-in [:output :hiccup] conj [:code lexeme]))))

(defmethod parse-token-group ::tk/component
  [parser]
  (parse-component parser))

(defmethod parse-token-group ::tk/eof
  [parser]
  (advance parser))

(defn finalize-output
  "Finalize the data in the output map into the desired form"
  [output modifier]
  (case modifier
    :verbatim output
    :inline   (update output :hiccup #(into [:span] %))
    ;; otherwise, just wrap into `:div`
    (update output :hiccup #(into [:div] %))))

(defn parse
  ([src]
   (parse src nil))
  ([src env]
   (let [tokens (tk/scan-tokens src {:line (get env :line 1)})
         parser (make-parser {:env     (dissoc env :line)
                              :tokens  tokens
                              :current 0
                              :output  {:attrs {} :hiccup []}})]
     (loop [parser parser]
       (if (done? parser)
         (-> parser :output (finalize-output (:modifier env)))
         (recur (parse-token-group parser)))))))
