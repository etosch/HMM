(ns HMM
  (:require [clojure.contrib.generic.math-functions :as Math])
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



(defstruct
    ^{:doc "An emission is a mapping of an observation and a state"}
    emission :obs :state)

(defstruct
    ^{:doc "An HMM is a mapping of model parameters and a record of the n-gram emission probabilities. Right now the smoothing parameter corresponds to the laplacian parameter."}
    HMM :starts :emissions :states :smoothing :n)

(defstruct
    ^{:doc "An ngram is mapping of an ngram function and the number of levels"}
  ngram :gram-function :n)

(defn
  ^{:doc "Takes n corresponding to the number of -gram emissions and returns a function that takes in an emission list and returns the n adjacent emissions for every emission in the emission list."}
  ;; I would like to have done this with a macro, but wasn't sure how to suppress evaluation during an unknown number of recursive calls.
  ;; I figured this was safer, especially since the strings are hard-coded.
  ngram-window [n]
  (let [funstring "(fn [emission-list] (lazy-seq (map (fn ["
	index (if (= n 1) 2 n)] ;;forces a two-window for unigrams, since these emissions are also used for transition probs
    (loop [i 1
	   args (str "arg" i)
	   body " emission-list "
	   next "(rest emission-list)"]
      (if (= index i)
	(eval (read-string (str funstring args "] (vector " args "))" body ")))")))
	(recur (inc i)
	       (str args (str " arg" (inc i)))
	       (str body next)
	       (str "(rest " next ")"))))))

(defn
  ^{:doc "Takes in two filters, two tagsets, and an ngram list (a list of vectors corresponding to the ngram window around an emission. The filter functions take in members of each tagset and return functions that are used to filter ngrams. The second list of ngrams (the one called inside the inner map) is the first filtered list. "}
  get-mappings
  [filter1 filter2 tagset1 tagset2 smoothing-parameter ngrams]
  (defn arg-check []
    (if (not (and (fn? filter1) (fn? filter2) (list? tagset1) (list? tagset2) (list? ngrams) (vector? (first ngrams)) (map? (first (first ngrams)))))
      (throw (java.lang.RuntimeException "Wrong argument type"))))
  ;;  (arg-check)
  (zipmap tagset1 (map (fn [tag1]
			 (let [filtered-list (filter (filter1 tag1) ngrams)
			       denominator (+ (count filtered-list) (* smoothing-parameter (count tagset2)))]
			   (zipmap tagset2 (map (fn [tag2]
						  (/ (+ smoothing-parameter (count (filter (filter2 tag2) filtered-list))) denominator)) tagset2))))
		       tagset1)))


(defn
  ^{:doc "Takes in a set of state tags and a set of observation tags, a smoothing parameter for LaPlace smoothing, an ngram set observations, and the n corresponding to that ngram. "
    }
  train-HMM [state-tag-set obs-tag-set smoothing-parameter ngrams n]
  {:starts (let [start-tags (map #(:state (nth % 1)) (filter #(= (:state (nth % 0)) "S") ngrams))
		 denominator (+ (count start-tags) (* smoothing-parameter (count state-tag-set)))]
	     (zipmap state-tag-set (map (fn [tag] (/ (+ smoothing-parameter (count (filter #(= % tag) start-tags))) denominator)) state-tag-set)))
   ;; probability of the current tag, given the previous tag
   ;; outer is the previous tag; inner is the current tag
   ;; ie probability of transitioning from outer tag to inner tag
   :transitions (get-mappings #(fn [x] (and (= (:state (nth x 0)) %)
					    (not (or (= (:state (nth x 1)) "S")
						     (= (:state (nth x 1)) "E")))))
			      #(fn [x] (= (:state (nth x 1)) %))
			      state-tag-set
			      state-tag-set
			      smoothing-parameter
			      ngrams
			      )
   :emissions (cond (= n 1) (get-mappings #(fn [x] (and (= (:state (nth x 0)) %) (contains? obs-tag-set (:obs (nth x 0)))))
					  #(fn [y] (= (vector (:obs (nth  y 0))) %))
					  state-tag-set
					  (map vector obs-tag-set)
					  smoothing-parameter
					  ngrams)
		    (= n 2) (get-mappings #(fn [x] (= (:state (nth x 0)) %))
					  #(fn [y] (and (= (:obs (nth y 0)) (nth % 0))
							(= (:obs (nth y 1)) (nth % 1))))
					  (map vector state-tag-set)
					  (apply concat (map (fn [x] (map (fn [y] (vector x y)) obs-tag-set)) obs-tag-set)))
		    (= n 3) (get-mappings #(fn [x] (= (:state (nth x 0)) %))
					  #(fn [y] (and (= (:obs (nth y 0)) (nth % 0))
							(= (:obs (nth y 1)) (nth % 1))
							(= (:obs (nth y 2)) (nth % 2))))
					  (map vector state-tag-set)
					  (apply concat (apply concat (map (fn [x] (map (fn [y] (map (fn [z] (vector x y))
												     obs-tag-set))
											obs-tag-set))
									   obs-tag-set)))))
   :smoothing smoothing-parameter
   :n n})

(defn hmm-equality-check [hmm]
  (if (not (and (= (reduce + (vals (:starts hmm))) 1)
		(= (reduce + (flatten (map vals (vals (:transitions hmm))))) (count (:transitions hmm)))
		(= (reduce + (flatten (map vals (vals (:emissions hmm))))) (count (:emissions hmm)))))
    (throw (java.lang.RuntimeException.
	      "Marginal probabilities in HMM do not sum to 1."))))

;; I wrote these two functions with the intention to better display my output, but they fail when double returns
;; the scientific notation representation of a number.

;; (defn truncate-at-precision [number places]
;;   (let [a1 (rest (.split (str (double number)) ""))
;; 	parse-double (fn [x] (java.lang.Double/parseDouble x))
;; 	parse-int (fn [x] (java.lang.Integer/parseInt x))]
;;     (loop [counter places
;; 	   a2 a1 
;; 	   through-leading-zeros? false
;; 	   rounded-char ""]
;; ;;      (println counter "\t" (seq a2) "\t" through-leading-zeros? "\t" rounded-char)
;;       (if (= 1 counter)
;; 	(parse-double (str rounded-char))
;; 	(recur (if through-leading-zeros? (dec counter) counter)
;; 	       (if (= 1 (count a2)) (concat a2 (seq "0")) (rest a2))
;; 		(if (and (not through-leading-zeros?) (some #(= (first a2) %) (rest (.split "123456789" "")))) true through-leading-zeros?)
;; 		(str rounded-char (first a2)))))))
;; (defn
;;   to-double [mapping places]
;;   (let [keys (keys mapping)
;; 	vals (vals mapping)]
;;     (zipmap keys (map #(truncate-at-precision % places) vals))))
  
(defn max-vector [vector-seq]
  (if (and (seq? vector-seq) (vector? (first vector-seq)))
    (last (sort-by #(nth % 1) vector-seq))
    (throw (java.lang.RuntimeException. "vector-seq in max-vector must be a list of vectors"))))

(defn
  ^{:doc "Returns the value at the nested key, where the keys are ordered from outer to inner"}
  get-nested [mapping outer-key inner-key] (get (get mapping outer-key) inner-key))

(defn get-max-prob-and-state [outer-tag inner-tag-list current-emission lattice-entry hmm]
  ^{:doc "Takes in the outer tag value, the inner tag list, the current emission in ngram format, and the lattice entry at the appropriate tag."}
  ;; outer-tag e.g. "B"
  ;; inner-tag-list e.g. (["NNP"] ["UH"]...)
  ;; current-emission e.g. ([{obs: "NNP" state: "nil"}{obs: "UH" state: "nil"}])
  ;; lattice --> {"B" (["S" 0.01]["I" 0.31]...), "I" ...}
  ;;  (max-vector
  (max-vector 
   (for [inner-state-tag inner-tag-list]
     [inner-state-tag
      (* (last lattice-entry)
	 (get-nested (:transitions hmm) inner-state-tag outer-tag)
	 (let [obs (vector (if (= (:n hmm) 1)
			     (:obs (first current-emission))
			     (map #(:obs %) current-emission)))
	       prob (get-nested (:emissions hmm) outer-tag obs)]
	   (if (nil? prob)
	     (doall (println obs "not found")
		    0)
	     prob)))])))

(defn print-lattice [lattice]
  (loop [lines (seq lattice)
	 string ""]
    (if (= 0 (count lines))
      (do 
	(println string)
	'ok)
      (recur (next lines)
	     (str string "\n\n" (first (first lines)) "\t" (first (rest (first lines))))))))
      
(defn
  ^{:doc "Takes in a emission sequence (in the form of a sentence) and an hmm."}
  viterbi [emiss-seq hmm]
  (do
    (hmm-equality-check hmm)
    (if (not (seq? emiss-seq))(throw (java.lang.RuntimeException. "emiss-seq must be a sequence")))
    (if (not (map? (first emiss-seq)))(throw (java.lang.RuntimeException "emiss-seq must be a sequence of maps")))
    (if (not (nil? (some #(or (= (:state %) "S") (= (:state %) "E")) emiss-seq))) (throw (java.lang.RuntimeException "emiss-seq must not contain start or end states"))))
  (loop [prob-matrix (let [ngrams (first ((ngram-window (:n hmm)) emiss-seq))]
		       (zipmap (keys (:starts hmm))
			       (map (fn [s]
				      (list [""
					     (* (get (:starts hmm) s)
						(get (get (:emissions hmm) s)
						     (if (= (:n hmm) 1) (vector (:obs (first ngrams)))
							 (apply vector (map #(:obs %) ngrams)))))]))
				    (keys (:starts hmm)))))
	 e (rest emiss-seq)]
;;    (println prob-matrix "\n\n" e "\n\n")
    (if (empty? e)
      (let [final-probs (zipmap (keys (:starts hmm)) (map #(last (last (get prob-matrix %))) (keys (:starts hmm))))
	    max-val  (reduce max (vals final-probs))
	    key-of-max-val (first (filter #(= (get final-probs %) max-val) (keys (:starts hmm))))
	    path (get prob-matrix key-of-max-val)]
;;	(println final-probs "\n---\n" max-val "\n---\n" key-of-max-val "\n---\n" path)
	(map first path))
      (recur (zipmap (keys prob-matrix)
		     (map (fn [x y] (concat x (list y)))
			  (vals prob-matrix)
			  (map #(get-max-prob-and-state % (keys (:starts hmm)) (if (= (count e) 1) e (first ((ngram-window (:n hmm)) e))) (last (get prob-matrix %)) hmm)
			       (keys (:starts hmm)))))
	     (rest e)))))
	
  
(defn predict_state_tags [emissions_list hmm]
  ^{:doc "Takes in an emissions list and runs viterbi on it. Returns predicted state tags"}
  (map #(viterbi % hmm) emissions_list))

(defn set_state_tags [list_of_state_tags emissions_to_be_classified]
  ^{:doc "Takes in a list or predicted state tags and emissions to be classified. Returns a new set of classified emissions."}
  (map (fn [state_tag sent] 
	 (map (fn [tag emiss] (assoc emiss :state tag)) state_tag sent))
       list_of_state_tags emissions_to_be_classified))

(defn metrics [tag predicted gold_standard]
  ^{:doc "Takes in a given tag, the emissions with the predicted tags, and the gold standard data and returns a mapping of precision, recall, and f-measure."}
  (defn num_hits [condition]
    (reduce + (map (fn [p_sent g_sent]
		     (reduce + (map (fn [p_emiss g_emiss] (if (condition p_emiss g_emiss) 1 0)) p_sent g_sent)))
		     predicted gold_standard)))
  (let [true_positive (num_hits (fn [p g] (= tag (:state p) (:state g))))
	false_positive (num_hits (fn [p g] (and (= tag (:state p)) (not (= tag (:state g))))))
	false_negative (num_hits (fn [p g] (and (not (= tag (:state p))) (= tag (:state g)))))]
    {:precision (double (if (= 0 true_positive) 0 (/ true_positive (+ true_positive false_positive))))
     :recall (double (if (= 0 true_positive) 0 (/ true_positive (+ true_positive false_negative))))
     :f-measure (double (if (= 0 true_positive)
			  0 (/
			     (* 2 (/ true_positive (+ true_positive false_positive)) (/ true_positive (+ true_positive false_negative)))
			     (+ (/ true_positive (+ true_positive false_negative)) (/ true_positive (+ true_positive false_positive))))))
     }))

;; Functions that are specific to this task

(defn
  ^{:doc "Reads in a source file and prepares it for HMM consumption. The expected source file has words annotated with state (bio tags) and observations (pos tags and lexemes). Each sentence is separated by more than one line break. Each word and its annotations is separated by exactly one line break. Each line corresponds to the format BIO_TAG LEXEME POS. The obs_index corresponds to the correct index for the intended observation. When performing training with parts of speech, this index will be 2. When performing training with lexemes, this index will be 1. For this task, the state index will always be 0"
    }
  parseData [filename obs_index state_index]
  (defn lazyconcat [sent] (lazy-seq (if (empty? sent) '() (concat (.split (first sent) "\n")(lazyconcat (rest sent))))))
  (lazy-seq (map (fn [sent] (let [feat_vector (.split sent "\\s+")]
			      (struct-map emission :state (nth feat_vector state_index) :obs (nth feat_vector obs_index))))
		 (lazyconcat (map (fn [sent] (str "S nil nil\n" sent "\nE nil nil")) (.split (slurp filename) "\n\n"))))))

(defn
  ^{:doc "Returns the first sentence in an emissions list. An emissions list is read in through the function parse_data, which expects sentences to be annotated with bio tags as states, pos tags as observations, and lexemes. It adds start and end states to differentiate the sentences for use in the HMM. First-sent extracts the sublist of the first sentence in an emissions list, stripping off the start and end tags."
    }
  first-sent [emission_list]
  (let [emiss (first emission_list)]
    (if (empty? emission_list)
      '()
      (if (= (:state emiss) "S")
	(first-sent (rest emission_list))
	(if (= (:state emiss) "E")
	  '()
	  (concat (list emiss) (first-sent (rest emission_list))))))))

(defn
    ^{:doc "reconstruct_sentences [x] takes in a emissions list with begin and end tags and outputs a list containing lists of emission structs, partitioned by sentence."
    :test (fn [] (let [testfun (fn [state obs] (struct-map emission :state state :obs obs))
		       test (list (testfun "S" "nil") (testfun "NNP" "I") (testfun "VB" "O") (testfun "." "O") (testfun "E" "nil")
				  (testfun "S" "nil") (testfun "VBZ" "O") (testfun "E" "nil"))]
		   (assert (= (reconstruct_sentences test) (concat (.subList test 1 3) (.subList test 5 5))))))
    }
  reconstruct_sentences [emissions_list]
  (if (empty? emissions_list) '()
      (lazy-seq (concat (list (first-sent emissions_list))
			(reconstruct_sentences (.subList emissions_list (+ 2 (count (first-sent emissions_list))) (count emissions_list)))))))

	
(defn getLines [filename] (seq (.split (slurp filename) "\\s+")))
;; ;; Testing functions	 
;; (defn test-class-sanity-check []
;;      ;Makes sure that the testing and classifier_output aren't out of order
;;      (let [tag_val_list (fn [tagname emiss] (map (fn [e] (tagname e)) emiss))]
;;        (and (= (tag_val_list ':lexeme testing) (tag_val_list ':lexeme classifier_output))
;; 	    (= (tag_val_list ':pos testing) (tag_val_list ':pos classifier_output)))))

;; Main

(defn main [& args]

  (let [hid (set (getLines (nth args 0)))
	obs (set (getLines (nth args 1)))
	;; need to modify parseData to ensure every sentence ends with "E"
	training (let [data (parseData (nth args 2) 2 0)]
		   (if (= (:state (last data)) "E") data (concat data (list {:obs nil :state "E"}))))
	testing (let [data (parseData (nth args 3) 2 0)]
		  (if (= (:state (last data)) "E") data (concat data (list {:obs nil :state "E"}))))
	unigram-window ((ngram-window 1) training)
;;	bigram-window ((ngram-window 2) training)
;;	trigram-window ((ngram-window 3) training)
	uni01 (train-HMM hid obs (/ 1 10) unigram-window 1 true)
;;	uni03 (train-HMM hid obs (/ 3 10) unigram-window 1)
;;	uni05 (train-HMM hid obs (/ 5 10) unigram-window 1)
;;	uni10 (train-HMM hid obs 1 unigram-window 1)
	s (reconstruct_sentences testing)]
        ;; bi01 (train-HMM hid obs (/ 1 10) bigram-window 2)
	;; bi03 (train-HMM hid obs (/ 3 10) bigram-window 2)
	;; bi05 (train-HMM hid obs (/ 5 10) bigram-window 2)
	;; bi10 (train-HMM hid obs 1 bigram-window 2)
	;; tri01 (train-HMM hid obs (/ 1 10) trigram-window 3)
	;; tri03 (train-HMM hid obs (/ 3 10) trigram-window 3)
	;; tri05 (train-HMM hid obs (/ 5 10) trigram-window 3)
	;; tri10 (train-HMM hid obs 1 trigram-window 3)
	
    (println (metrics "B" (set_state_tags (predict_state_tags s uni01) s) s))
    (println (metrics "I" (set_state_tags (predict_state_tags s uni01) s) s))
    (println (metrics "O" (set_state_tags (predict_state_tags s uni01) s) s))
    (print "Generate HMM")))