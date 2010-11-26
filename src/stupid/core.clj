; ## Oh No Clojure!
; ### Ramblingtroduction
; I saw [Sweet-expressions](http://goo.gl/oGzNs). It has good idea and made
; some of my existing ones click with their lispful potential. Saw that he
; introduced new syntax from within the language itself. Starting researching,
; now I know what reader macros are. Also that Clojure doesn't support them.
; I agree with the reasoning, but would like to do it anyway, and found [a
; blog post](http://goo.gl/aGpNU) which explains that since the language core
; is in Java you can just mess with it directly to do what you want, and
; provided an example. Marvelous.
(ns stupid.core)

; We'll begin by getting references to the arrays containg the reader macros
; and dispatch macros. They're indexed on a `char`.
(def reader-macros
  ; This gets the [`Field`](http://goo.gl/yJV8Z) containing the reader
  ; macros. It's private
  (let [field
         (.getDeclaredField clojure.lang.LispReader "macros")]
       ; so we make it public
       (.setAccessible field true)
       ; and get the value. If we were getting a non-static field we would
       ; specify an instance instead of `nil`.
       (.get field nil)))

(def reader-dispatch-macros
  (let [field
         (.getDeclaredField clojure.lang.LispReader "dispatchMacros")]
       (.setAccessible field true)
       (.get field nil)))

; The reader macros are called with two arguments, a 
; [LineNumberingPushbackReader](goo.gl/7zvGf) and the character that caused
; it to be invoked. Since the character isn't going to change for a macro I'm
; defining this function to drop the second argument.
(defn drop-2nd-arg
  [f] (fn [a b] (f a)))

; This function is used to define a reader macro. It takes a character or
; string identifying which macro it is setting and a function to set it to.
(defn def-reader-macro
  [symbol fun]
    (cond
      (char? symbol)
        (aset reader-macros (int symbol) (drop-2nd-arg fun))
      (= (count symbol) 1)
        (aset reader-macros (int (first symbol)) (drop-2nd-arg fun))
      (and (= (count symbol) 2) (= (first symbol) "#"))
        (aset reader-dispatch-macros (int (first (rest symbol)))
              (drop-2nd-arg fun))
      :else
        (throw (Exception. "Bad symbol for reader macro."))))

; What to do? Let's start simple. I'll define `$` to grab all text through to
; the next occurence and just return `:foo`.
(def-reader-macro \$
  (fn [reader]
    (while (not= (.read reader) (int \$)))
    (println "?" reader "?")
    :foo))

; Oh my.
(println $gisengioesngoi#(*GHT#(P:g3nogyg9h3g
  wg34gwjg3ngoi34hg94bi\ufku,vyf7!! ffafefaf
  WOPPPPPP kuh-pow!!!@\$)
