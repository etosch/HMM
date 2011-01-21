(ns core.decoding
  (:require [core.training]
	    [core.evaluation])
  (:use [core.training]
	[core.evaluation])
  (:gen-class))

(defmacro $
  [mapping key]
  (list mapping key (list mapping "Unknown" 0)))

(defn argmax
  [aarray]
  (if (and (seq? aarray) (vector? (first aarray)))
    (first (last (sort-by #(nth % 1) aarray)))
    (throw (java.lang.RuntimeException. "vector-seq in max-vector must be a list of vectors"))))

;;;;;;;;;;;;;;;;; Probabilities ;;;;;;;;;;;;;;;;; 
(defn get-max-prob-for-one-state
  [^PersistentVector emiss
   ^String state
   ^PersistentArrayMap prev-matrix-entry
   keys transitions emissions]
  (apply max (map #(Math/log (* ($ prev-matrix-entry %)
				($ ($ transitions %) (last state))
				($ ($ emissions (last state)) emiss))) keys)))

(defn update-viterbi
  [emiss prob-matrix keys transitions emissions]
  (concat prob-matrix
	  (for [state keys]
	    {state (get-max-prob-for-one-state
		    emiss state (last prob-matrix) keys transitions emissions)})))

(defn initialize-viterbi [initial-emission hmm]
  (let [spin (fn [l] (if (= 1 (count l)) l (rest l)))
	state-keys (keys (last (:transitions hmm)))]
    (loop [trellis (list (apply merge
				(for [s state-keys]
				  {s (Math/log (* ($ (:starts hmm) (first s))
						  ($ ($ (first (:emissions hmm)) (first s))
						     (first initial-emission))))})))
	   emiss (take 1 initial-emission)
	   emissions (spin (:emissions hmm))
	   transitions (spin (:transitions hmm))]
      (println trellis "\n"
	       emiss "\n"
	       emissions "\n"
	       transitions "\n")
      (if (= emiss initial-emission)
	trellis
	(recur (concat trellis
		       (update-viterbi
			emiss
			trellis
			state-keys
			(first transitions)
			(first emissions)))
	       (take (inc (count emiss)) initial-emission)
	       (spin emissions)
	       (spin transitions))))))

;;;;;;;;;;;;;;;;; Backpointers ;;;;;;;;;;;;;;;;; 

(defn get-backpointer-for-one-state
  [emiss state prev-matrix-entry keys transitions]
  (argmax (for [s keys] [s (* ($ prev-matrix-entry s)
			      ($ ($ transitions s) state))])))

(defn update-backpointer
  [emiss path prob-matrix keys transitions]
  (concat path
	  (for [state keys]
	    {state (get-backpointer-for-one-state
		    emiss state (last prob-matrix) keys transitions)})))

(defn initialize-backtrace [initial-emission prob-matrix hmm]
  (let [spin (fn [l] (if (= 1 (count l)) l (rest l)))
	state-keys (keys (last (:transitions hmm)))]
    (loop [backtrace (list (zipmap state-keys) (repeat "Start"))
	   emiss (subvec initial-emission 0 0)
	   transitions (spin (:transitions hmm))]
      (if (= emiss initial-emission)
	backtrace
	(recur (update-backpointer emiss backtrace prob-matrix
				   (subvec 0 (dec (count emiss)))
				   (first transitions))
	       (subvec initial-emission 0 (count emiss))
	       (spin transitions))))))
	       
				   
(defn
  ^{:doc "Takes in a emission sequence (in the form of a sentence) and an hmm."}
  viterbi-make-path [emiss-seq hmm]
  (let [transition-window (ngram-window (:n hmm) emiss-seq)]
    (loop [prob-matrix (initialize-viterbi (vec (map :obs (first transition-window))) hmm) ;;where i left off
	   path (initialize-backtrace (vec (map :obs (first (ngram-window (:n hmm) emiss-seq)))) prob-matrix hmm)
	 e (map #(vec (map :obs %))
		((ngram-window (:n hmm))  (.subList emiss-seq (max 1 (dec (:n hmm))) (count emiss-seq))))]
    (cond (= 0 (count e)) ;;[path prob-matrix]
	  [(take (count emiss-seq) path) (take (count emiss-seq) prob-matrix)]
	  :else (recur (update-viterbi (first e) prob-matrix hmm)
		       (update-backpointer (first e) path prob-matrix hmm)
		       (rest e)))))

(defn viterbi [emiss-seq hmm]
  (let [path-prob-vec (viterbi-make-path emiss-seq hmm)
	path-matrix (first path-prob-vec)
	prob-matrix (last path-prob-vec)
	max-state (argmax (seq (last prob-matrix)))]
    (loop [path (list max-state)
	   backpointers (reverse path-matrix)]
      (if (empty? backpointers))
	(rest (reverse path))
	(recur (concat path (list (get (first backpointers) (last path))))
	       (rest backpointers))))))

(defn predict-state-tags [emissions-list hmm]
  (map #(viterbi % hmm) emissions-list))

(defn classify-aggregate
  [hmm classify-directory output-directory training]
  (doall
   (let [pairs (for [file (.listFiles (java.io.File. classify-directory))]
		 (let [data (read-line (slurp file))
		       classified-seq (viterbi (list data) hmm)]
		   (spit (str classified-results)(str output-directory (java.io.File/separator) (.getName file)))
		   [data classified-seq]))]
     (-> (metrics (keys (:starts hmm)) (pairs 1) (map :state (pairs 0)))
	 (write-metrics-to-file hmm training (str output-directory (java.io.File/separator) "metrics.txt"))))))
	 