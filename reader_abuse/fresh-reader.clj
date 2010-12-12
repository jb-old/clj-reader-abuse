(ns reader-abuse.fresh-reader
    (:refer-clojure :exclude [read])
    (:use [reader-abuse.core :only [get-reader-macro, set-reader-macro]]))

; Let's write out own entirely-Lisp reader, so that we can extend it without
; having to deal with Java interop. If we want to entirely replace the Clojure
; one we could define a reader macro on `(`, but we probably won't go for
; the required compatability.
; 
; https://github.com/richhickey/clojure/blob/1.1.x/src/jvm/clojure/lang/LispReader.java
  
(def eof-values #{65535, -1, nil})

(defn java-reader-seq [java-reader]
  "Produces a lazy seq of the characters in the java-reader, with their
  `line-number` as metadata."
  
  (lazy-seq
    (let [l (.getLineNumber java-reader)
          i (.read java-reader)]
      (if (not (eof-values i))
          (cons
            (char i) ^ {:line-number l}
            (java-reader-seq java-reader))))))

(defn adapt-reader [clj-reader]
  "Makes a Java reader function for a Clojure reader function -- it must
  consume at least one character!"
  
  (fn [initial-character, java-reader]
    (clj-reader
      (if (not (eof-values initial-character))
          (cons (char initial-character)
                (java-reader-seq java-reader))))))

; clojure readers return `[form, rest-of-chars]`.
; `::nil` is used to indicate no form being returned, `nil` is `nil`.

(defn read-whitespace [chars]
  (if (whitespace (first chars))
      (recur read-whitespace (rest chars))
      [::nil, chars]))

(defn read-eof [chars]
  (if (= nil (first chars))
    [::eof, nil]
    [::nil, chars]))

(def default-readers
  [read-eof]
  [read-whitespace]
)

(defn read-given-macros [macros]
  
)

(defrecord Reader [macros]
  clojure.lang.IFn
  (invoke [this, chars]
    (read-given-macros chars, (:macros this))))

(def read (Reader default-macros))
