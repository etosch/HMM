(ns core.BIO
  (:gen-class)
  (:use [core.HMM]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATA PROCESSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that are specific to this task

(defn
  ^{:doc "Reads in a source file and prepares it for HMM consumption.
 The expected source file has words annotated with state (bio tags) and
 observations (pos tags and lexemes). Each sentence is separated by more
 than one line break. Each word and its annotations is separated by exactly
 one line break. Each line corresponds to the format BIO_TAG LEXEME POS.
 The obs_index corresponds to the correct index for the intended observation.
 When performing training with parts of speech, this index will be 2.
 When performing training with lexemes, this index will be 1. For this task,
 the state index will always be 0"}
  parseData [filename obs_index state_index]
  (lazy-seq
   (for [sent (.split (slurp filename) "\n\n")]
     (for [emiss (.split sent "\n")]
       (let [feat_vector (.split emiss "\\s+")]
	 (struct-map emission
	   :state (nth feat_vector state_index)
	   :obs (nth feat_vector obs_index)))))))
  
;; Main

(defn -main [& args]
  ;; need to add use of Snowball stemmer for words and a facility to read in input
  ;; training trigrams fails due to running out of memory
  (doall
   (let [hid (set (.split (slurp "resources/hidden") "\n"))
	 training (parseData "resources/wsj_15_18_train" 2 0)
	 testing (parseData "resources/wsj_20_test" 2 0)
	 outFile (if (> (count args) 0) (first args) "output.txt")]
     (spit outFile "Type\tn-grams\tSmoothingParam\tState\tPrecision\tRecall\tF-Measure\n")
     (for [smoothing (list (/ 1 10) (/ 3 10) (/ 1 2) 1)
	   gram (list 1 2)]
       (let [hmm (supervised-train-HMM hid smoothing training gram)]
	 (write-metrics-to-file (metrics hid (predict-state-tags testing hmm)
					 (map #(map :state %) testing))
				hmm "supervised" outFile))))))