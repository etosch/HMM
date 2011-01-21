(ns HMM
  (:require (clojure-python.core))
  (:use (clojure-python.core))
					; (:import (org.python.util PythonInterpreter))
  (:gen-class))

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

(defn -main [& args]
  (println "Run HMM on some input dataset"))

(defn getTags [filename]
  (seq (.split (slurp filename) "\\s+")))

(def hid (getTags "./resources/hidden"))
(def obs (getTags "./resources/observed"))


;(def projroot "/home/g/grad/etosch/Desktop/134/HMM")

(def obs (getTags (apply str (list projroot "/resources/tags"))))

;; don't forget to set python path for jython so it can access yaml and nltk

(. clojure-python.core/init "/home/g/grad/etosch/nltk/lib/python2.5/site-packages/")
;(py-import :os)

