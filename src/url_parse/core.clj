(ns url-parse.core
  (:require [instaparse.core :as insta]
            [clojure.set]))

(def url-regex #"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?")
(def default-pattern {:domain [] :path [] :queryparam []})
(def parse-pattern
  (insta/parser
   "patterns = entry+
    entry = typename <'('> val <')'> <delim> <whitespace>?
    typename = 'host' | 'path' | 'queryparam'
    val = #'[A-Za-z0-9.?/=]+'
    delim = ';'
    whitespace = #'\\s+'"
   :output-format :hiccup))


(defn parse-querystring [querystring]
  (if (empty? querystring)
      {}
      (->> (clojure.string/split querystring #"&")
           (map #(clojure.string/split % #"="))
           (map (fn [[k v]] [(keyword k) v]))
           (into {}))))


(defn process-parsed-node [node]
  (case (node :typename)
    "host" {:domain [(re-pattern (node :val))]}
    "path" {:path [; pattern
                   (re-pattern (-> (node :val)
                                   (clojure.string/replace
                                    #"\?([A-Za-z]+)"
                                    "(.{0,64})")))
                   ; keys
                   (map keyword
                     (map second (re-seq #"\?([A-Za-z]+)" (node :val))))]}
    "queryparam" (let [query-splitted (clojure.string/split
                                       (node :val) #"\=")]
                   (if (= (count query-splitted) 2)
                     {:qp [(->> query-splitted
                                (map #(clojure.string/replace % #"\?" ""))
                                (map keyword))]}
                     nil))))


(defn new-pattern [pattern-str]
  (let [parsed-pattern (parse-pattern pattern-str)
        parsed-nodes (if (vector? parsed-pattern)
                         (->> parsed-pattern
                           rest
                           (map rest)
                           (map #(into {} %))) '())]
    (->> (map process-parsed-node parsed-nodes)
         (apply merge-with into))))


(defn deconstruct-path [pattern path]
  (zipmap (nth (pattern :path) 1)
          (->> (re-seq (or (first (pattern :path)) #"") path)
               first
               rest)))


(defn deconstruct-querystring [pattern qs]
  (clojure.set/rename-keys
   (select-keys qs (map first (pattern :qp)))
   (apply array-map (flatten (pattern :qp)))))


(defn recognize [pattern url]
  (let [pattern (or pattern default-pattern)
        matched-url (re-matches url-regex url)
        host (nth matched-url 4)
        path (if (> (count (nth matched-url 5)) 0)
               (subs (nth matched-url 5) 1)
               "")
        querystring (nth matched-url 7)
        querystring-parsed (parse-querystring querystring)]
    (when (re-matches (or (first (pattern :domain)) #"") host)
      (if (pattern :qp)
        (when (every? #(contains? querystring-parsed %)
                      (map first (pattern :qp)))
          (seq (merge-with into
                 (deconstruct-querystring pattern querystring-parsed)
                 (deconstruct-path pattern path))))
        (seq (deconstruct-path pattern path))))))


(defn -main
  "Decode URL by pattern"
  [& args])
