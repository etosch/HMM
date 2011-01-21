;; training.clj
;; Naive approach, with minimal optimizations to a general sequential classifier
;; by Emma Tosch
;; etosch@gmail.com
;; 15 January 2011

;; pi = starting parameters
;; A = matrix (map) of transition parameters
;; B = matrix (map) of emission parameters

(ns core.training
  #^{:author "Emma Tosch"
     :doc "A sequential classifier for both supervised and unsupervised training"}
  (:require [clojure.contrib.generic.math-functions :as Math]
	    [clojure.set :as Set]
	    [clojure.contrib.profile :as Profile])
  (:use [clojure.contrib.duck-streams :only (append-spit)]
	[clojure.contrib.string :only (replace-str)])
  (:gen-class))

;;(set! Profile/*enable-profiling* false)

(defn vector-begins [m k]
  ^{:doc "Returns all key/value pairs in m where the key in m is a vector that begins with v"}
  (let [v (if (vector? k) k (vector k))]
    (apply merge
	   (for [k2 (Set/select (fn [x] (= v (subvec x 0 (count v)))) (set (keys m)))]
	     {k2 (m k2)}))))

(defn vector-ends [m k]
  ^{:doc "Returns all key/value pairs in m where the key in m is a vector that begins with v"}
  (let [v (if (vector? k) k (vector k))]
    (apply merge
	   (for [k2 (Set/select (fn [x] (= v (subvec x (- (count x) (count v))))) (set (keys m)))]
	     {k2 (m k2)}))))

(defn load-hmm
  [hmm-file]
  (read-string (slurp hmm-file)))

(defn spin
  [lst]
  (if (empty? (rest lst)) lst (rest lst)))

(defn ngram-window
  [n emissions-list]
  (apply concat (map #(map vec (partition n 1 %)) emissions-list)))

(defn enumerate-tags
  [{:keys [state-tag-set order]}]
   (loop [n order v1 (map vector state-tag-set) v2 (map vector state-tag-set)]
     (if (= n 0) v1
	 (recur (dec n)
		v2
		(for [s1 v1 s2 v2] (into s1 s2))))))

(defn pi-supervised
  [emissions-list {:keys [state-tag-set smoothing-parameter]}]
  (let [starts (map first emissions-list)]
    (apply merge
	   (for [state state-tag-set]
	     {state
	      (/ (+ smoothing-parameter
		    (count (filter #(= state (:state %)) starts)))
		 (+ (* smoothing-parameter (count state-tag-set))
		    (count starts)))}))))

(declare pi-unsupervised)

(defn A-state-to-state
  [from-tag-vector to-tag transition-vectors {:keys [state-tag-set smoothing-parameter]}]
  (Profile/prof
   :A-state-to-state
   (let [prior-n-states (filter #(= from-tag-vector (vec (map :state (butlast %)))) transition-vectors)]
     {to-tag (/ (+ smoothing-parameter
		   (count (filter #(= to-tag (:state (last %))) prior-n-states)))
		(+ (* smoothing-parameter (count state-tag-set))
		   (count prior-n-states)))})))

(defn A-supervised
  ^{:doc "Constructs the transition frequences for state data."}
  [transition-vectors {:keys [state-tag-set smoothing-parameter order] :as hmm}]
  (Profile/prof
   :A-supervised
   (let [from-tag-vectors (enumerate-tags hmm)]
     (apply merge (for [from from-tag-vectors]
		    {from (apply merge (for [to state-tag-set]
					 (A-state-to-state
					  from to transition-vectors hmm)))})))))

(defn forward-sum
  [trellis state emission {:keys [transition-map emission-map state-tag-set n order]}]
  (list (loop [s state-tag-set
	       sum 0]
	  (println "forward-sum: " s "\n" trellis "\n" state "\n" emission "\n" transition-map "\n" order)
;;	  (println "forward-sum\t" s "\n" sum)
	  (if (empty? s)
	    sum
	    (recur (rest s)
		   (+ sum (* (reduce + (for [k (Set/select #(= (subvec % 0 order) state) (set (keys trellis)))]
					 (last (trellis k))))
			     ((transition-map (first s)) (last state)))))))))
;;			     ((emission-map (last state)) (vec (take n (map :obs emission)))
	;;		      ((emission-map (last state)) "Unknown")))))))))

;; NOTE - WILL HAVE TO CHANGE IMPLEMENTATION OF UNKNOWN TO A VECTOR AND

(defn forward-recursion
  [trellis [emission] data {:keys [order]}]
  (println "forward-recursion: " trellis "\n" emission "\n" data "\n" order "\n")
  (apply merge (for [state (:state-tag-set data)]
		 {state (concat (trellis state)
				(forward-sum trellis (subvec state 0 order) emission (assoc data :order order)))})))

(defn forward-initialize
  [{:keys [window state-tag-set] :as data} {:keys [starts order n emissions transitions]}]
  (loop [no 1
	 e (spin emissions)
	 t (spin transitions)
	 trellis (apply merge (for [s state-tag-set]
				{s (list (Math/log (* (starts (s 0))
						      (((first emissions) (s 0))
						       [(:obs ((first window) 0))]
							 (((first emissions) (s 0)) "Unknown")))))}))]
  (println trellis)
    (if (= no (max n order))
      trellis
      (recur (inc no)(spin e)(spin t)
	     (forward-recursion
	      trellis window
	      (assoc data :state-tag-set
		     (enumerate-tags {:state-tag-set (set (map first state-tag-set))
				      :order no}))
	      {:order no})))))
				;; (assoc (assoc data
				;; 			:n (if (>= no n) n (inc no)))
				;; 		 :order (if (>= no order) order (inc no))))))))

;;  			   (let [s (subvec state 0 (inc n))]
				
				    

(defn forward-termination
  [trellis {:keys [state-tag-set]} {:keys [ends]}]
  (apply merge (for [state state-tag-set]
		 {state (concat (trellis state) (ends state))})))
	
(defn forward [emission-seq state-tags hmm]
  (Profile/prof
   :forward
   (let [data {:emission-map (first (:emissions hmm))
	       :transition-map (first (:transitions hmm))
	       :window (ngram-window (max (:order hmm 1) (:ngram hmm 1)) (list emission-seq))
	       :state-tag-set (enumerate-norder-state-tags hmm)}]
     (loop [trellis (forward-initialize data hmm)
	    e (rest (:window data))]
       (println "trellis: " trellis "\n" )
       (if (empty? e)
	 (forward-termination trellis data hmm)
	 (recur (forward-recursion trellis e data hmm)
		(rest e)))))))

(defn backward [emission-seq state-tag-set hmm]
  (Profile/prof
   :backward
   (let [emission-map (first (:emissions hmm))
	 transition-map (first (:transitions hmm))
	 update-score (fn [t state e]
			(reduce + (for [s state-tag-set]
				    (* (last (t s))
				       ((transition-map (vector s)) state)
				       ((emission-map state)(vector (last e))
					((emission-map state)(vector "Unknown")))))))]
     (loop [trellis (zipmap state-tag-set (for [_ (range (count state-tag-set))] (list 1.0)))
	    e emission-seq]
       (println trellis "\n"
		(for [s state-tag-set state state-tag-set]
		  ((emission-map state) (vector (last e))((emission-map state) (vector "Unknown")))))
       (if (nil? e)
	 (reduce + (for [s state-tag-set]
		     (* ((:starts hmm) s)(last (trellis s)))))
	 (recur (apply merge (for [state state-tag-set]
			       {state (concat (trellis state) (list (update-score trellis state e)))}))
		(butlast e)))))))

  
(defn emissions-at-given-state
  [state n-grams]
  ^{:doc "Returns a list of observation vectors for some n-gram observation, where
 the last tag is equal to the given state."}
  (Profile/prof
   :emissions-at-given-state
   (map #(vec (map :obs %)) (filter #(= state (:state (last %))) n-grams))))

;; should rewrite this to make more legible and get rid of zipmap
(defn emission-frequencies-at-given-state
  ^{:doc "Calculates the emission frequencies for a set of observation tags. Rather than providing a tag set,
 it draws from the provided observations, with a provision for unseen data. This function is meant to run over
 large datasets, as it will not smooth properly for data sets where the training data's observation set is both
 finite and known."}
  [state n-grams smoothing-parameter]
  (Profile/prof
   :emission-frequencies-at-given-state
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
						 ))]))))))))


(defn B-supervised
  ^{:doc "Calculates the emission frequencies for the n-grams in the training data."}
  [n-gram-window {:keys [state-tag-set smoothing-parameter]}]
  (Profile/prof
  :B-supervised
  (apply merge (for [state state-tag-set]
		 {state (emission-frequencies-at-given-state state n-gram-window smoothing-parameter)}))))

(defn hmm-equality-check [hmm]
  (Profile/prof
   :hmm-equality-check
   (if (not (and (= (reduce + (vals (:starts hmm))) 1)
		(= (reduce + (flatten (map vals (vals (:transitions hmm))))) (count (:transitions hmm)))
		(= (reduce + (flatten (map vals (vals (:emissions hmm))))) (count (:emissions hmm)))))
    (throw (java.lang.RuntimeException.
	    "Marginal probabilities in HMM do not sum to 1."))
    'ok)))

(defn write-hmm
  [hmm hmm-name]
   :write-hmm
   (spit (str hmm-name) hmm))
  
(defn
  ^{:doc "Takes in a set of state tags and a set of observation tags, a smoothing
 parameter for LaPlace smoothing, an ngram set observations, and the n corresponding
 to that ngram."}
  supervised-train-HMM
  [state-tag-set smoothing-parameter emissions-list ngram order]
  (Profile/prof
   :supervised-train-HMM
   (let [data {:smoothing-parameter smoothing-parameter
	       :n ngram
	       :order order
	       :state-tag-set state-tag-set}]
     (merge data 
	    {:starts (pi-supervised emissions-list data)
	     :ends (pi-supervised (map reverse emissions-list) data)
	     :transitions (for [nth (map inc (range order))]
			    (A-supervised (ngram-window (inc nth) emissions-list)
					  (assoc data :order nth)))
	     :emissions (for [nth (map inc (range ngram))]
			  (B-supervised (ngram-window nth emissions-list) data))}))))

(declare unsupervised-train-HMM)
;; (defn unsupervised-train-HMM
;;   [state-set emissions-list n-gram initial-hmm epsilon]
;;   (Profile/prof
;;    :unsupervised-train-HMM
;;    (let [alpha (forward (first emissions-list) (keys (:transitions initial-hmm)) initial-hmm)
;; 	 beta (backward (first emissions-list)
	       
;; 	       {:starts (pi-unsupervised)
;; 		:transitions (A-unsupervised)
;; 		:emissions (B-unsupervised)
;; 		:n n-gram})))))

(defn train-hmm [training order ngram smoothing training-directory state-file hmm-file]
  (Profile/prof
   :train-hmm
   (let [emissions-list (for [file (.listFiles (java.io.File. training-directory))]
			  (read-string (replace-str "\\" "" (slurp file))))]
     (cond (= training 'supervised)
	   (supervised-train-HMM (set (map :state (flatten emissions-list)))
				 (double smoothing)
				 emissions-list
				 (int ngram))
	   (= training 'unsupervised)
	   (unsupervised-train-HMM (set (slurp state-file))
				   emissions-list
				   (int ngram)
				   (load-hmm hmm-file))))))