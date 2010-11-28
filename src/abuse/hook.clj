; This module provides a reader macro `#R` allowing you to execute any
; function in the reader. The function must take a single argument, which is
; the reader. Example:
; 
;     (println #R(fn [r] (char (.read r)))!)
; 
; Would print `!`.

(ns abuse.hook
    (:use abuse.core))

(defn read-hook
  [reader initial-char]
    ((eval (read reader true nil true)) reader))

(set-reader-macro "#R" read-hook)
