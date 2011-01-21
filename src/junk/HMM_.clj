(ns core.HMM
  (:require [clojure.contrib.generic.math-functions :as Math])
  (:use [clojure.contrib.duck-streams :only (append-spit)])
  (:gen-class)
  )

;; For simplicity, this code compiles both the general HMM code and the code specific for the task
;; together in one namespace. I did this to make running it easier. Ideally, I would separate out
;; this assignment's code from the general code.

;; I had been trying to use Rob Levy's clojure-python code from
;; https://github.com/rplevy/clojure-python.git
;; but had great difficulty accessing his code from my namespace.
;; I also tried running a Jython interpreter, but ran into interop issues
;; there as well.

;; Set up Jython to be able to read nltk
;; Need to make sure that the initialization of PythonInterpreter occurs
;; before anything else
;; (def props (java.util.Properties.))
;; (.setProperty props "python.path"
;; 	      "/home/g/grad/etosch/nltk/lib/python2.5/site-packages:/home/g/grad/etosch/yaml/lib/python2.5/site-packages:/home/g/grad/etosch/numpy:resources:")	      
;; (org.python.util.PythonInterpreter/initialize (. java.lang.System getProperties) props (into-array String [""]))
;; (def pyinterp (PythonInterpreter.))

(defstruct ^{:doc "An emission is a mapping of an observation and a state"}
  emission :obs :state)

(defstruct ^{:doc "An HMM is a mapping of model parameters and a record of the n-gram emission probabilities. Right now the smoothing parameter corresponds to the laplacian parameter."}
    HMM :starts :starts2 :starts3 :emissions :states :smoothing :n)

(defstruct ^{:doc "An ngram is mapping of an ngram function and the number of levels"} ngram :gram-function :n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRAINING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn
  ^{:doc "Takes n corresponding to the number of -gram emissions and returns a
 function that takes in an emission list and returns the n adjacent emissions for
 every emission in the emission list -- EMISSION LIST IS PER SENTENCE."}
  ngram-window [n]
  (let [funstring "(fn [emission-list] (lazy-seq (map (fn ["]
    (loop [i 1 
	   args (str "arg" i)
	   body " emission-list "
	   next "(rest emission-list)"]
      (if (= i n)
	(eval (read-string (str funstring args "] (vector " args "))" body ")))")))
	(recur (inc i)
	       (str args (str " arg" (inc i)))
	       (str body next)
	       (str "(rest " next ")"))))))

(defn starting-frequencies
  ^{:doc "Constructs starting frequencies for a tag set, an emissions list and a smoothing paramter.
 The emissions-list should be the product of parseData."}
  [state-tag-set emissions-list smoothing-parameter]
  (assert (and (seq? emissions-list) (seq? (first emissions-list))(map? (first (first emissions-list)))))
  (let [s (for [tag state-tag-set]
	    [tag
	     (/ (+ smoothing-parameter
		   (count (filter #(= tag (:state %)) (map first emissions-list))))
		(+ (* smoothing-parameter (count state-tag-set))
		   (count (map first emissions-list))))])]
    (zipmap (map first s) (map last s))))

(defn transition-frequencies
  ^{:doc "Constructs the transition frequences for state data."}
  [state-tag-set transition-vectors smoothing-parameter]
  ;; state-tag-set e.g. #("B" "I" "O")
  ;; transition-vectors e.g. [{:obs "NN", :state "B"} {:obs ",", :state "O"}]...
  (let [to-map (for [from-tag state-tag-set]
		 (let [from-map (for [to-tag state-tag-set] 
				  [to-tag
				   (/ (+ smoothing-parameter
					 (count (filter
						 #(= to-tag (:state (last %)))
						 (filter #(= from-tag (:state (first %))) transition-vectors))))
				      (+ (* smoothing-parameter (count state-tag-set))
					 (count (filter #(= from-tag (:state (first %))) transition-vectors))))])]
		   [from-tag (zipmap (map first from-map) (map last from-map))]))]
    (zipmap (map first to-map) (map last to-map))))

(defn emissions-at-given-state
  [state n-grams]
  ^{:doc "Returns a list of observation vectors for some n-gram observation, where
 the last tag is equal to the given state."}
  (map #(vec (map :obs %)) (filter #(= state (:state (last %))) n-grams)))

(defn emission-frequencies-at-given-state
  ^{:doc "Calculates the emission frequencies for a set of observation tags. Rather than providing a tag set,
 it draws from the provided observations, with a provision for unseen data. This function is meant to run over
 large datasets, as it will not smooth properly for data sets where the training data's observation set is both
 finite and known."}
  [state n-grams smoothing-parameter]
  (let [emissions-at-this-tag (emissions-at-given-state state n-grams)
	emission-set (inc (count (set emissions-at-this-tag)))]
    (loop [e emissions-at-this-tag
	   freq-counts '()]
      (if (= 0 (count e))
	(let [emiss-counts (concat freq-counts
				   (list ["Unknown" (/ smoothing-parameter
						       (+ (count emissions-at-this-tag)
							  (* smoothing-parameter emission-set)))]))]
	      (zipmap (map first emiss-counts) (map last emiss-counts)))
	(recur (filter #(not (= (first e) %)) e)
	       (concat freq-counts (list [(first e)
					  (/ (+ smoothing-parameter
						(count (filter #(= (first e) %) e)))
					     (+ (count emissions-at-this-tag)
						(* smoothing-parameter emission-set)
						))])))))))

(defn emissions-frequencies
  ^{:doc "Calculates the emission frequencies for the n-grams in the training data."}
  [state-tag-set n-grams smoothing-parameter]
  (let [state-ngram-map
	(for [state state-tag-set]
	  [state 
	   (emission-frequencies-at-given-state
	    ;;	    (emissions-at-given-state state n-grams)
	    state
	    n-grams 
	    smoothing-parameter)])]
    (zipmap (map first state-ngram-map) (map last state-ngram-map))))

(defn
  ^{:doc "Takes in a set of state tags and a set of observation tags, a smoothing
 parameter for LaPlace smoothing, an ngram set observations, and the n corresponding
 to that ngram."}
  supervised-train-HMM
  [state-tag-set smoothing-parameter emissions-list n]
  {:starts (starting-frequencies state-tag-set emissions-list smoothing-parameter)
   :transitions (transition-frequencies
		 state-tag-set
		 (apply concat (map (ngram-window 2) emissions-list))
		 smoothing-parameter)
   :emissions (emissions-frequencies
	       state-tag-set
	       (apply concat (map (ngram-window n) emissions-list))
	       smoothing-parameter)
   :starts2 (emissions-frequencies
	     state-tag-set
	     (apply concat (map (ngram-window 1) emissions-list))
	     smoothing-parameter)
   :starts3 (emissions-frequencies
	     state-tag-set
	     (apply concat (map (ngram-window 2) emissions-list))
	     smoothing-parameter)
   :smoothing smoothing-parameter
   :n n})

(defn hmm-equality-check [hmm]
  (if (not (and (= (reduce + (vals (:starts hmm))) 1)
		(= (reduce + (flatten (map vals (vals (:transitions hmm))))) (count (:transitions hmm)))
		(= (reduce + (flatten (map vals (vals (:emissions hmm))))) (count (:emissions hmm)))))
    (throw (java.lang.RuntimeException.
	    "Marginal probabilities in HMM do not sum to 1."))
    'ok))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECODING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn argmax [aarray]
  (if (and (seq? aarray) (vector? (first aarray)))
    (first (last (sort-by #(nth % 1) aarray)))
    (throw (java.lang.RuntimeException. "vector-seq in max-vector must be a list of vectors"))))

(defn get-smooth [mapping key]
  (or (get mapping key) (get mapping "Unknown") 0))
 
(defn get-max-prob-for-one-state [emiss state prev-matrix-entry hmm]
  (apply max (map (fn [s] (* (get-smooth prev-matrix-entry s)
			     (get-smooth (get-smooth (:transitions hmm) s) state)
			     (get-smooth (get-smooth (:emissions hmm) state) emiss)))
		  (keys (:starts hmm)))))

(defn get-backpointer-for-one-state [emiss state prev-matrix-entry hmm]
  (argmax (for [s (keys (:starts hmm))]
	    [s (* (get-smooth prev-matrix-entry s)
		  (get-smooth (get-smooth (:transitions hmm) s) state))])))
		
(defn update-viterbi [emiss prob-matrix hmm]
  (let [aa (for [state (keys (:starts hmm))] [state (get-max-prob-for-one-state emiss state (last prob-matrix) hmm)])]
  (concat prob-matrix (list (zipmap (map first aa) (map last aa))))))

(defn update-backpointer [emiss path prob-matrix hmm]
  (let [aa (for [state (keys (:starts hmm))] [state (get-backpointer-for-one-state emiss state (last prob-matrix) hmm)])]
    (concat path (list (zipmap (map first aa) (map last aa))))))

(defn initialize-viterbi-unigram [initial-emission hmm]
  (list (zipmap (keys (:starts hmm))
		(map #(* (get-smooth (:starts hmm) %)
			 (get-smooth (get-smooth (:emissions hmm) %) initial-emission))
		     (keys (:starts hmm))))))

(defn initialize-viterbi-bigram [initial-emission hmm]
  (let [prob-matrix (initialize-viterbi-unigram (first initial-emission) hmm)]
  (concat prob-matrix  (update-viterbi initial-emission prob-matrix hmm))))

(defn initialize-viterbi-trigram [initial-emission hmm]
  (let [prob-matrix (initialize-viterbi-bigram (take 2 initial-emission) hmm)]
  (concat prob-matrix  (update-viterbi initial-emission prob-matrix hmm))))

(defn initialize-viterbi [initial-emission hmm]
  (let [n (:n hmm)]
    (cond (= n 1) (initialize-viterbi-unigram initial-emission hmm)
	  (= n 2) (initialize-viterbi-bigram initial-emission hmm)
	  (= n 3) (initialize-viterbi-trigram initial-emission hmm)
	  :else (throw (java.lang.RuntimeException "supplied n-gram not supported")))))

(defn initialize-backtrace-unigram [hmm]
  (list (zipmap (keys (:starts hmm)) (repeat "Start"))))

(defn initialize-backtrace-bigram [emiss prob-matrix hmm]
  (update-backpointer emiss (initialize-backtrace-unigram hmm) prob-matrix hmm))

(defn initialize-backtrace-trigram [emiss prob-matrix hmm]
  (update-backpointer emiss (initialize-backtrace-bigram emiss prob-matrix hmm) prob-matrix hmm))
   
(defn initialize-backtrace [initial-emission prob-matrix hmm]
  (let [n (:n hmm)]
    (cond (= n 1) (initialize-backtrace-unigram hmm)
	  (= n 2) (initialize-backtrace-bigram initial-emission prob-matrix hmm)
	  (= n 3) (initialize-backtrace-trigram initial-emission prob-matrix hmm)
	  :else (throw (java.lang.RuntimeException "supplied n-gram not supported")))))
(defn
  ^{:doc "Takes in a emission sequence (in the form of a sentence) and an hmm."}
  viterbi-make-path [emiss-seq hmm]
  (assert (and (hmm-equality-check hmm)(seq? emiss-seq)(map? (first emiss-seq))))
  (loop [prob-matrix (initialize-viterbi (vec (map :obs (first ((ngram-window (:n hmm)) emiss-seq)))) hmm)
	 path (initialize-backtrace (vec (map :obs (first ((ngram-window (:n hmm)) emiss-seq)))) prob-matrix hmm)
	 e (map #(vec (map :obs %))
		((ngram-window (:n hmm))  (.subList emiss-seq (max 1 (dec (:n hmm))) (count emiss-seq))))]
;;		 (rest emiss-seq)))] ;;won't work for trigrams
;;    (cond (and (= 0 (count e)) (= (count emiss-seq) (count path))) [path prob-matrix]
;;	  (and (= 0 (count e))(> (count e) (count emiss-seq)))
    ;;	  [(.subList
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
      (if (= 0 (count backpointers))
	(rest (reverse path))
	(recur (concat path (list (get (first backpointers) (last path))))
	       (rest backpointers))))))

(defn predict-state-tags [emissions-list hmm]
  ^{:doc "Takes in an emissions list and runs viterbi on it. Returns predicted
 state tags"}
  (map #(viterbi % hmm) emissions-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn num-hits [condition predicted gold-standard]
  (assert (= (count (flatten predicted)) (count (flatten gold-standard))))
  (reduce + (map #(if (condition %1 %2) 1 0) (flatten predicted) (flatten gold-standard))))

(defn metrics-at-tag [tag predicted gold-standard]
  ^{:doc "Takes in a given tag, the emissions with the predicted tags, and the
 gold standard data and returns a mapping of precision, recall, and f-measure."}
  (assert (= (count (flatten predicted)) (count (flatten gold-standard))))
  (let [true-positive (num-hits #(= tag %1 %2) predicted gold-standard)
	false-positive (num-hits #(and (= tag %1) (not (= tag %2))) predicted gold-standard)
	false-negative (num-hits #(and (not (= tag %1)) (= tag %2)) predicted gold-standard)]
    {:precision (double (if (= 0 true-positive) 0 (/ true-positive (+ true-positive false-positive))))
     :recall (double (if (= 0 true-positive) 0 (/ true-positive (+ true-positive false-negative))))
     :f-measure (double (if (= 0 true-positive)
			  0 (/
			     (* 2 (/ true-positive (+ true-positive false-positive)) (/ true-positive (+ true-positive false-negative)))
			     (+ (/ true-positive (+ true-positive false-negative)) (/ true-positive (+ true-positive false-positive))))))
     }))

(defn metrics [taglist predicted gold-standard]
  (map #(vector % (metrics-at-tag % predicted gold-standard)) taglist))

(defn write-metrics-to-file [mlist hmm type filename]
  (let [meta-info (str type "\t" (:n hmm) "-gram\t" (:smoothing hmm) "\t")
	write-me (map (fn [x] (str meta-info
				   (first x)
				   "\t"
				   (apply str (map #(str % "\t") (vals (last x))))))
		      mlist)]
    (for [line write-me]
      (do 
	(append-spit filename (str line "\n"))
	'ok))))

