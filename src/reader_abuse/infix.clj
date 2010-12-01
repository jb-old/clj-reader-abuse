; *Inspired by [David A Wheeler's Curly Infix syntax](http://goo.gl/zARpL).*
; 
; This module defines a macro `infixed` allowing the user to write infix
; expressions. A reader macro will be later be added to make this more
; syntactically concise. For example, if the chosen syntax were
; `#[forms...]`:
; 
;     #[a + b + c + d] -> (+ a b c d)
;     #[a + #[b / c]] -> (+ a (/ b c))
; 
; Only one operator may be used in an expression. Support for mixing certain
; comparison operations (identified by `(isa operator ::boolean-and-able)`)
; will be added to work as follows:
; 
;     #[a < b < c = d > e] -> (and (< a b c) (= c d) (> d e))

(ns reader-abuse.infix
    (:use reader-abuse.core))

(for [operator [< <= = == not= >= >]]
  (derive operator ::boolean-and-able))

(defmacro infixed
  [& forms]
    (let [vforms (vec forms)]
      (if (even? (count vforms))
        (throw (Exception. "Even number of forms in infix expression.")))
      (if (= 0 (count forms))
        (throw (Exception. "Infix expression needs forms.")))
      (if (= 1 (count forms))
        (first forms)
        (if (apply = (take-nth 2 (rest vforms))) ; 
          (list* (first forms) (take-nth 2 vforms))
          (if (every? #(isa? % ::boolean-and-able) (take-nth 2 (rest vforms)))
            (throw (Exception. "NOT YET IMPLEMENTED SORRY"))
            (throw (Exception. (str
              "Distinct operators in infix expression must all be "
              "::boolean-and-able."))))))))
