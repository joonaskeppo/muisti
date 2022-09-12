(ns muisti.notes
  "Helpers and handlers for dealing with Muisti notes directories"
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [muisti.utils :refer [get-dir-contents vectorize-path subseq? deep-merge]]
            [muisti.lang.parser :as parser]))

(def ^:dynamic *cache-manifests* true)

(defonce ^:private manifests (atom {}))

(defn manifest-file?
  [file]
  (= "manifest.edn" (.getName file)))

(defn note-file?
  [file]
  (str/ends-with? (.getName file) ".mu"))

(defn canonize-notes-path
  "Canonize path to a file or dir within notes directory."
  [{:keys [root-path] :as _dir-contents} path]
  (let [root-v (vectorize-path root-path)
        path-v (vectorize-path path)]
    (assert (subseq? root-v path-v))
    (->> path-v
         (drop (count root-v))
         (mapv #(str/replace % #".mu" "")))))

(defn get-applicable-manifests
  "Get seq of manifest files that are applicable to the given File (i.e., file or dir).
  Returns seq in application order; from least to most specific."
  [dir-contents file]
  (let [->dir-path    #(if (.isDirectory %)
                         (vectorize-path %)
                         (butlast (vectorize-path %)))
        file-dir-path (->dir-path file)]
    (->> (:files dir-contents)
         (filter (fn [v]
                   (and (manifest-file? v)
                        (subseq? (->dir-path v) file-dir-path)))))))

(defn build-manifest*
  [dir-contents file]
  (->> (get-applicable-manifests dir-contents file)
       (map (comp edn/read-string slurp))
       (apply deep-merge)))

(defn clear-manifests-cache []
  (reset! manifests {}))

(defn build-manifest
  "Builds resultant manifest for a given `file` (dir or file).
  Caches result if `*cache-manifests*` is true.
  Merges all applicable manifests in order of ascending specificity."
  [dir-contents file]
  (if *cache-manifests*
    (or (get @manifests file)
        (let [res (build-manifest* dir-contents file)]
          (swap! manifests assoc file res)
          res))
    (build-manifest* dir-contents file)))

(def parse-note
  (comp parser/parse slurp))

(defn parse-notes
  [{:keys [files] :as dir-contents}]
  (into {}
        (comp (filter note-file?)
              (map (fn [file]
                     [(canonize-notes-path dir-contents file) (parse-note file)])))
        files))

;; TODO: refactor so that this fn would be unnecessary
(defn flatten-note
  [{:keys [attrs] :as note}]
  (-> note
      (dissoc :attrs)
      (merge (dissoc attrs :id :hiccup))))

(defn bundle-note
  "Bundle note data with manifest data"
  [dir-contents file]
  (let [manifest-data (build-manifest dir-contents file)
        note-id       (canonize-notes-path dir-contents file)]
    (-> (parse-note file)
        (assoc :id note-id)
        (update :attrs #(deep-merge manifest-data %))
        flatten-note)))

(defn bundle-notes
  "Bundle all note data with their respective manifest data"
  [{:keys [files] :as dir-contents}]
  (into []
        (comp (filter note-file?)
              (map #(bundle-note dir-contents %)))
        files))

(comment
  (clear-manifests-cache)

  (let [dir-contents (get-dir-contents "~/notes")]
    (parse-notes dir-contents))

  (parse-note (->> "~/notes" get-dir-contents :files (filter note-file?) first))

  (let [dir-contents (get-dir-contents "~/notes")
        file         (->> dir-contents :files (filter note-file?) first)]
    (bundle-note dir-contents file))

  (let [dir-contents (get-dir-contents "~/notes")]
    (bundle-notes dir-contents))

  (let [dir-contents (get-dir-contents "~/notes")]
    (build-manifest dir-contents (last (:files dir-contents)))))
