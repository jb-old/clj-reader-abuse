(ns reader-abuse.trying
    (:require reader-abuse.iexprs)
    (:require reader-abuse.hook))

(do #I

ns reader-abuse.trying
  :import
    javax.swing JFrame JPanel KeyStroke AbstractAction

defn foo
  [x]
    foo x 5
  
  [x y]
    * x y

println "Hello World"

let [x 5]
  println "x is" x
  
  doseq [y (range x)]
    println "y approaching" x "and is at" y
    println (foo x y)


)