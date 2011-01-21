(ns core.main
  (:gen-class)
  (:require [core.training]
	    [core.decoding]
	    [core.evaluation])
  (:use [core.training]
	[core.decoding]
	[core.evaluation]))

(defn -main [& args]
  ;; not sure how this works with no args
  (let [args-map (apply hash-map (map symbol args))
	training ('--training args-map 'unsupervised)
	order ('--order args-map '1)
	ngram ('--ngram args-map '1)
	smoothing ('--smoothing args-map '.1)
	state-file ('--state-file args-map)
	hmm-file ('--hmm args-map)
	hmm-name ('--hmm-name args-map  (str "HMM" (System/currentTimeMillis)))
	training-directory ('--training-directory args-map)
	testing-file ('--testing-file args-map)
	classify-directory ('--classify-directory args-map)]
    (cond (and (nil? testing-file)(nil? classify-directory)(not (nil? training-directory)))
	  (-> (train-hmm training order ngram smoothing training-directory state-file hmm-file)
	      (write-hmm hmm-name))
	  (and (nil? testing-file)(not (nil? classify-directory))(not (nil? training-directory)))
	  (dorun
	   (println "Warning: Attempting to train and execute without testing")
	   (-> (train-hmm training order ngram smoothing training-directory state-file hmm-file)
	       (classify-aggregate classify-directory)))
	  (and (not (nil? testing-file))(not (nil? classify-directory))(not (nil? training-directory)))
	  (dorun
	   (println "Warning: Attempting to train, test, and execute in a single run")
	   (let [hmm (train-hmm training order ngram smoothing training-directory state-file)]
	     (test-hmm hmm testing-file (set (.split "\n" (slurp state-file))))
	     (classify-aggregate hmm classify-directory)))
	  (and (not (nil? testing-file))(nil? classify-directory)(not (nil? training-directory)))
	  (dorun
	   (-> (train-hmm training order ngram smoothing training-directory state-file hmm-file)
	       (test-hmm testing-file)))
	  (and (not (nil? hmm-file)) (not (nil? testing-file)) (not (nil? classify-directory)))
	  (dorun
	   (let [hmm (load-hmm hmm-file)]
	     (test-hmm hmm)
	     (classify-directory hmm)))
	  (and (not (nil? hmm-file)) (not (nil? testing-file)) (nil? classify-directory))
	  (dorun
	   (-> (load-hmm hmm-file)
	       (test-hmm)))
	  (and (not (nil? hmm-file)) (nil? testing-file) (not (nil? classify-directory)))
	  (dorun
	   (-> (load-hmm hmm-file)
	       (classify-aggregate classify-directory)))
	  :else (println "Usage: --training\n--order\n--ngram\n--smoothing\n--hmm-file\n--hmm-name\n--training-directory\n--testing-file\n--classify-directory"))))
	       