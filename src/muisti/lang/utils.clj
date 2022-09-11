(ns muisti.lang.utils
  (:require [clojure.string :as str]))

(defn fmt-error [line message]
  (format "[line %d] %s" line message))

(defn add-error
  "Add error to scanner/parser"
  [{:keys [line] :as this} message]
  (update this :errors conj (fmt-error line message)))

(defn hiccup?
  "Is `x` in the form of Hiccup?"
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defn children
  "Get children of hiccup vector"
  [h]
  (when (hiccup? h)
    (let [r (rest h)]
      (if (map? (first r))
        (rest r)
        r))))

(defn empty-hiccup? [h]
  (or (empty? h)
      (empty? (children h))))

(defn read-delimited
  "Read input string from a given `start-delimiter` to `end-delimiter` (as strings).
  Assumes that delimiter count must be balanced, so will continue reading until an equivalent
  number of `end-delimiter` vs `start-delimiter` substrings are found.
  Returns resultant string.
  Additional `opts`:
  - `escape-strings`: ignore contents inside double quotes?
  - `drop-delimiters`: remove start and end delimiters from resultant string?"
  ([s start-delimiter end-delimiter]
   (read-delimited s start-delimiter end-delimiter {}))
  ([s start-delimiter end-delimiter {:keys [escape-strings drop-delimiters] :as _opts}]
   (when (str/starts-with? s start-delimiter)
     (let [len-start (count start-delimiter)
           len-end   (count end-delimiter)]
       (loop [acc           ""
              index         0
              opening-count 0
              closing-count 0]
         (let [[ch :as s*] (subs s index)]
           (cond
             (and (> opening-count 0)
                  (if (= start-delimiter end-delimiter)
                    (= 2 opening-count)
                    (= opening-count closing-count)))
             (if drop-delimiters
               (->> acc
                    (drop len-start)
                    (take (- (count acc) len-start len-end))
                    (apply str))
               acc)

             (nil? ch)
             nil

             ;; escaped chars are always ignored, regardless
             (and (> index 0) (= \\ (first (drop (dec index) s))))
             (recur (str acc ch) (inc index) opening-count closing-count)

             (and escape-strings (= ch \"))
             (let [inner-str (read-delimited s* "\"" "\"")]
               (recur (str acc inner-str) (+ index (count inner-str)) opening-count closing-count))

             :else
             (cond
               (str/starts-with? s* start-delimiter) (let [index (+ len-start index)]
                                                       (recur (subs s 0 index) index (inc opening-count) closing-count))
               (str/starts-with? s* end-delimiter)   (let [index (+ len-end index)]
                                                       (recur (subs s 0 index) index opening-count (inc closing-count)))
               :else                                 (let [index (inc index)]
                                                       (recur (subs s 0 index) index opening-count closing-count))))))))))
