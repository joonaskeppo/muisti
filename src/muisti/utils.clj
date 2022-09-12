(ns muisti.utils
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

;; --- cond macros ---

(defmacro cond-let
  "Like a `cond` that instead of a single test value
  takes a [var-name test] vector for each branch instead.
  If test evaluates to logical true, var-name is bound
  to test in the then-clause it is paired with."
  [& body]
  {:style/indent 1}
  (when (seq body)
    (let [[[binding then] & rem] (partition 2 body)]
      `(if-let ~binding
         ~then
         (cond-let ~@(reduce into (mapv vec rem)))))))

(defmacro cond-with
  "Pass `arg` to each predicate in a cond form."
  {:style/indent 1}
  [arg & body]
  (let [pairs (->> body
                   (partition 2)
                   (mapcat (fn [[pred then]]
                             (list (list pred arg) then))))]
    `(cond ~@pairs)))

;; --- files + directories ---

(declare fmap)

(defn normalize-path
  [path]
  (if (str/starts-with? path "~")
    (str/replace-first path "~" (System/getProperty "user.home"))
    path))

(defn vectorize-path
  [path]
  (let [rx (re-pattern (System/getProperty "file.separator"))]
    (condp = (type path)
      java.io.File (-> path .getCanonicalPath (str/split rx))
      (-> path (str/split rx)))))

(defn get-dir-contents
  "Get `dir` contents as a map of `:files` and `:dirs`.
  Optional `opts` passable as map or set:
  - `:hidden`: include hidden files
  - `:dotfile-or-dir`: include dotfiles or dotted paths"
  ([dir]
   (get-dir-contents dir nil))
  ([dir opts]
   (let [dir  (normalize-path dir)
         data (->> (clojure.java.io/file dir)
                   file-seq
                   (keep (fn [v]
                           (let [is-file   (.isFile v)
                                 is-dir    (.isDirectory v)
                                 is-hidden (.isHidden v)]
                             (when (and (or (:hidden opts) (not is-hidden))
                                        (or (:dotfile-or-dir opts)
                                            (->> (.getCanonicalPath v)
                                                 vectorize-path
                                                 (not-any? #(str/starts-with? % ".")))))
                               (cond
                                 is-dir  [:dirs v]
                                 is-file [:files v])))))
                   (group-by first)
                   (fmap #(map second %)))]
     (assoc data :root-path dir))))

(defn read-project-file [dir file]
  (->> (file-seq (clojure.java.io/file dir))
       (some (fn [item]
               (when (and (= file (.getName item)) (.isFile item))
                 item)))
       slurp))

(comment
  (read-project-file "/" "deps.edn")

  (get-dir-contents "~/notes"))

;; --- generic fns ---

(defn find-first [pred xs]
  (some #(when (pred %) %) xs))

(defn find-first*
  "Similar to `find-first`, except instead of returning
  matching value, it returns a vector/tuple of [matching-value predicate-value]."
  [pred xs]
  (loop [[x & rem :as xs] xs]
    (cond-let
     [_ (empty? xs)] nil
     [ret (pred x)]  [x ret]
     [_ :else]       (recur rem))))

(defn assoc-if
  [m k pred v]
  (if (pred m) (assoc m k v) m))

(defn assoc-in-if
  [m path pred v]
  (if (pred m) (assoc-in m path v) m))

(defn ?assoc
  "Like `assoc`, but only if map `m` contains key(s)"
  ([m k v]
   (if (contains? m k) (assoc m k v) m))
  ([m k v & pairs]
   (loop [acc          (?assoc m k v)
          [[k* v* :as pair] & rem] (partition 2 pairs)]
     (if (nil? pair)
       acc
       (recur (?assoc acc k* v*) rem)))))

(defn ?assoc-in
  ([m path v]
   (let [k           (last path)
         parent-path (vec (butlast path))
         parent      (get-in m parent-path)]
     (if (contains? parent k)
       (assoc-in m path v)
       m))))

(defn ?update
  "Like `update`, but only if map `m` contains key(s)"
  ([m k f]
   (if (contains? m k) (update m k f) m))
  ([m k f v]
   (if (contains? m k) (update m k f v) m))
  ([m k f v1 v2]
   (if (contains? m k) (update m k f v1 v2) m))
  ([m k f v1 v2 & rem]
   (if (contains? m k) (apply update m k f v1 v2 rem) m)))

(defn deep-merge
  ([a] a)
  ([a b]
   (cond
     (map? a)        (->> b
                          (map (fn [[k v]] [k (deep-merge (get a k) v)]))
                          (into a))
     (vector? a)     (into a b)
     (set? a)        (set/union a b)
     (sequential? a) (concat a b)
     :else           b))
  ([a b & xs]
   (loop [acc (deep-merge a b)
          [x :as xs] xs]
     (if (empty? xs)
       acc
       (recur (deep-merge acc x) (rest xs))))))

(defmacro syms->map [& syms]
  `(zipmap (map keyword '~syms) [~@syms]))

(defn subseq?
  "Does `b` contain all elements of `a`, in that order?"
  [a b]
  (->> (for [len (range 1 (inc (count b)))]
         (take len b))
       (some #(= a %))))

(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))

(defn map-keys [f m]
  (->> m (map (fn [[k v]] [(f k) v])) (into {})))

(defn map-vals [f m]
  (->> m (map (fn [[k v]] [k (f v)])) (into {})))

(defprotocol Functor
  (fmap* [x f]))

(extend-protocol Functor
  clojure.lang.Associative
  (fmap* [m f] (->> m (map (fn [[k v]] [k (f v)])) (into {}))))

(def fmap (flip fmap*))
