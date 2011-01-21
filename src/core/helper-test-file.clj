;;(use 'clojure.contrib.profile)
(def s123 (set (.split (slurp "/home/emma/Documents/cosi134/HMM/resources/BIO/hidden") "\n")))
(def e123 (for [file (.listFiles (java.io.File. "/home/emma/Documents/cosi134/HMM/resources/BIO/POS/training"))]
		      (read-string (slurp file))))

(clojure.contrib.profile/with-profile-data
  (def hmm-1-1
       (clojure.contrib.profile/prof
	:hmm-1-1
	(core.training/supervised-train-HMM
	 s123
	 1.0
	 e123
	 1 1)))
  (def hmm-1-2
       (clojure.contrib.profile/prof
	:hmm-1-2
       (core.training/supervised-train-HMM
	s123
	1.0
	e123
        1 2)))
  (def hmm-2-2
       (clojure.contrib.profile/prof
	:hmm-2-2
	(core.training/supervised-train-HMM
	 s123
	 1.0
	 e123
	 2 2))))
;; (clojure.contrib.profile/with-profile-data
;;   (def hmm-3-3
;;        (clojure.contrib.profile/prof
;; 	:hmm-3-3
;; 	(core.training/supervised-train-HMM
;; 	 state-tag-set
;; 	 smoothing-parameter
;; 	 emissions-list
;; 	 3 3))))


