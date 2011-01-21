  ;; (defn max_vector [vector_seq]
  ;;   (if (and (seq? vector_seq) (vector? (first vector_seq)))
  ;;     (last (sort-by #(nth % 1) vector_seq))
  ;;     (java.lang.RuntimeException. "vector_seq in max_vector must be a list of vectors")))
  (hmm_sanity_check)
  (emiss_proper_form)
  (let [starts (:starts hmm)
	transitions (:transitions hmm) 
	emissions (:emissions hmm)
	ngram ((ngram-window (:n hmm)) emiss_seq)]
    ;; (defn compute_next_row [matrix last_ngram_emiss]
    ;;   (if (and (map? matrix) (seq? (first (vals matrix))) (vector? (first (first (vals matrix)))))
    ;; 	(zipmap (keys starts)
    ;; 		(map (fn [outer_state_tag]
    ;; 		       (concat (get matrix outer_state_tag)
    ;; 			       (list (max_vector
    ;; 				      (map (fn [inner_state_tag]
    ;; 					     [inner_state_tag
    ;; 					      (* (nth (last (get matrix inner_state_tag)) 1)
    ;; 						 (get (get transitions inner_state_tag) outer_state_tag)
    ;; 						 (let [prob (get (get emissions outer_state_tag)
    ;; 								 (vector (if (= (:n hmm) 1)
    ;; 									   (:obs (nth last_ngram_emiss 0))
    ;; 									   (map #(:obs %) last_ngram_emiss))))]
    ;; 						   (if (nil? prob) (let [obs (map #(:obs %) last_ngram_emiss)]
    ;; 								     (println obs "not found") 0) prob)))]
    ;; 					     )
    ;; 					   (keys starts)))))
    ;; 		       )
    ;; 		     (keys starts)))
    ;; 	(java.lang.RuntimeException.
    ;; 	 "vector_seq in compute_next_row must be a list of vectors")))
    (loop [matrix (zipmap
		   (keys starts)
		   (map (fn [state_tag]
			  (* (let [prob (get starts state_tag)] (if (nil? prob) 0 prob))
			     (let [prob (get (get emissions  state_tag) (vector (:obs (nth (first ngram) 0))))]
			       (if (nil? prob) 0 prob))))
			(keys starts)))
	   e ngram ; have just taken care of the first observation when computing the first row of the matrix
	   iters 0
	   path '()
	   ]
      (if (empty? e)
	path
;;	(let [most_probable_entry (last (sort-by #(nth (last (nth % 1)) 1) (concat matrix)))]
					;       (println most_probable_entry)
					;       (concat (list (nth most_probable_entry 0))
;;	  (map #(nth % 0) (nth most_probable_entry 1)))
	(recur (compute_next_row matrix (first e))
	       (rest e)
	       (inc iters))))))




  (defn compute_next_row [matrix last_ngram_emiss]
    (if (and (map? matrix) (seq? (first (vals matrix))) (vector? (first (first (vals matrix)))))
      (zipmap (keys starts)
	      (map (fn [outer_state_tag]
		     (concat (get matrix outer_state_tag)
					 (keys starts)))
		   (keys starts)))
      (java.lang.RuntimeException.
       "vector_seq in compute_next_row must be a list of vectors")))
