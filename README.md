Clojure Reader Abuse
--------------------

- `reader-abuse.core` provides functions for getting and setting Clojure
  reader macros, as well as some general Java reflection helpers.
- `reader-abuse.hook` adds the reader macro `#R` which reads the next form and
  evaluates it with the reader as the argument.
- `reader-abuse.iexpers` adds the reader macro `#I` which activates an
  indentation-based syntax inspired by [SRFI-49](http://goo.gl/32CgC).

### #I-expressions

`reader-abuse.iexpers` adds a reader macro `#I` which activates an alternate
syntax inspired by [SRFI-49](http://goo.gl/32CgC)'s I-expressions which I am
calling #I-expressions. The syntax continues until an unmatched \)/\]/\} is
encountered or a line has an indentation of less than the first line.

I find this helps my Python-accustomed mind read and write the code more
effectively. It's probably only suitable for personal projects as elsewhere
this sort of hack will probably be scorned.

A complete explanation will come after the code is working entirely properly,
for now this example should make it mostly clear.
    
    (println #I
      +
        * 2 3
        / 4 2)
    
    #I println
      1 2 3 4
    
    #I
    defn foo
      [a]
        foo a 0
      [a b]
         + (* a b) 2
    
    def x (foo 1 2)
    
    println
      foo
      x

The above is equivalent to the following.

    (println (do
      (+
        (* 2 3)
        (/ 4 2))))
    
    (println
      1 2 3 4)
    
    (do
      (defn foo
        ([a]
          (foo a 0))
        ([a b]
           (+ (* a b) 2)))
    
      (def x (foo 1 2))
    
      (println
        foo
        x))

### Author/License/Apology

Copyright 2010 Jeremy Banks <<jeremy@jeremybanks.ca>> and released under the
MIT license.

I'm new to Clojure, please accept my apologies for this being a horrible mess.
Also it's not finished and much is broken.
