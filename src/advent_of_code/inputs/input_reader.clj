(ns advent-of-code.inputs.input-reader
  (:require [clojure.java.io :as java.io]))

; Reads the txt and is accessible via "lines"
(defmacro with-lines
  [path & body]
  `(with-open [rdr# (java.io/reader ~path)]
     (let [~'lines (line-seq rdr#)]
       ~@body)))