(ns reader-abuse.physics
  :use [reader-abuse.mine])
(do #I

defrecord point [x y]
defrecord scene [objects]

def merge+ (partial merge-with +)

def point-object
  {:apply laws-of-physics
   :type :point
   :d (point 0 0)
   :v (point 0 0)}

def circular-object
  merge point-object
    {:type :ciricle
     :r 0
     :r-hz 0}

"A law of physics takes an object and returns a function that alters an
object in the way the corresponding physics would in its state. Or it returns
nil. That's also possible"

def laws-of-physics
  vector
    defn apply-velocity [o]
      if (:v o)
        fn [p] (assoc p :d (merge+ (:d p) (:v o)))
    
    defn apply-acceleration [o]
      if (:a o)
        fn [p] (assoc p :v (merge+ (:v p) (:a o)))

defn initial-scene
  scene
    vector
      line {:apply nil
            :d (point 0 0.5)}



) ; for a bug in #Iexprs and syntax highlighting