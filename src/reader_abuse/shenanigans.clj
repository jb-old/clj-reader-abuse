(ns reader-abuse.shenanigans
    (:use reader-abuse.iexprs))

(do #I

defn square [x]
  * x x

defn fib [x]
  if (< x 2)
    x
    + (fib (- x 1)) (fib (- x 2))

println (fib 10)

) ; #I apparently infiniteloops on EOF, hence the wrapping.