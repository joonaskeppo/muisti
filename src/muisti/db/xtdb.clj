(ns muisti.db.xtdb
  (:require [muisti.utils :refer [get-dir-contents]]
            [muisti.notes :as notes]
            [clojure.edn :as edn]
            [xtdb.api :as xt]))

(defn- extract-query-parts
  "Extract semantic parts of a query string"
  [query-str]
  (let [[_ var-sym statements _] (re-matches #"^\s*(\S+)\s+\|\s+((\[.+\]+)\s*)+" query-str)]
    {:var-sym    (symbol var-sym)
     :statements (edn/read-string (format "[%s]" statements))}))

(comment
  ;; querying for documents linked by an intermediary document
  (extract-query-parts "?y | [?this :links ?x] [?x :links ?y]"))

(defn contextual-q
  [node current-note-id simplified-query]
  (let [{:keys [var-sym statements]} (extract-query-parts simplified-query)]
    (xt/q (xt/db node)
          {:find  [var-sym]
           :where (into [['?this :xt/id current-note-id]] statements)})))

(defn submit-notes
  "Submit latest versions of notes, straight from the source."
  [node path-to-notes]
  (let [docs (->> path-to-notes
                  get-dir-contents
                  notes/bundle-notes
                  (mapv (fn [{:keys [id] :as doc}]
                          (-> doc
                              (assoc :xt/id {:note/id id})
                              (dissoc :id)))))]
    (xt/submit-tx node (for [doc docs] [::xt/put doc]))))

(comment
  (defonce test-node (xt/start-node {}))

  (submit-notes test-node "~/notes")

  (contextual-q test-node {:note/id ["projects" "mu"]} "?this | [?this :links ?x]"))
