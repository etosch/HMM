(ns preprocess
  (:require [clojure.contrib.duck-streams :as ds])
  (:require [clojure.contrib.str-utils2 :as string-utils])
  (:require [clojure.contrib.io :as io]))

;; Read entire file into memory.
;; going to need to hold whole list in memory - do periodic writes


(defn stringify [list-of-sentences]
  (loop [s "" sents (apply concat list-of-sentences)]
    (println (first sents))
    (if (empty? sents)
      s
      (recur (str s (apply str (map #(str (:bio %) " " (:lexeme %) " " (:pos %) "\n")
				    (first sents))) "\n\n")
	     (rest sents)))))
      
(defn process-file [in-filename]
  (let [sent (.split (slurp in-filename) "\n\n")]
    (filter #(> (count %) 0) (for [s sent] (process-sent s)))))
    

(defn process-sent [sent]
  (set-bio-tags
   (for [a (lazy-seq (re-seq #"\[??.*?/.*?\s+?\]?" sent))]
     {:bio (cond (string-utils/contains? a "[") "B"
		 (string-utils/contains? a "]") "I")
      :pos (string-utils/drop (re-find #"/\S+" a) 1)
      :lexeme (string-utils/butlast (re-find #"\S+/" a) 1)})
   false))

(defn set-bio-tags [sent-map inside]
  (cond (empty? sent-map) '()
	(and inside (nil? (:bio (first sent-map)))) (concat (list (assoc (first sent-map) :bio "I")) (set-bio-tags (rest sent-map) inside))
	(and (not inside) (nil? (:bio (first sent-map)))) (concat (list (assoc (first sent-map) :bio "O")) (set-bio-tags (rest sent-map) inside))
	(not (nil? (:bio (first sent-map)))) (concat (list (first sent-map)) (set-bio-tags (rest sent-map) (not inside)))))


(defn process-batch [directory batch-size output-file]
  (assert (.isDirectory (io/file dir)))
  (do
    (
  (loop [dirs (.list (io/file dir))]
    (if (empty? dirs)
      'ok
      (do
       (->> (take batch-size directory)
	    (map #(str directory "/" %))
	    (map process-file)
	    (stringify)
	    (ds/append-spit output-file))
       (recur (drop batch-size dirs))))))