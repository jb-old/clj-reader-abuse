(ns reader-abuse.trying
    (:require reader-abuse.iexprs)
    (:require reader-abuse.infix)
    (:require reader-abuse.hook))
(do #I

ns reader-abuse.trying
  :require
    [clojure.pprint :as pprint]
    [clojure.contrib.duck-streams :as duck]

defn normalize-weights [weighted-results]
  "Normalizes the weights in a sequence of value -> weight maps to have a
  total weight of 1.
  
  This'll work on any sequence of pairs, and it'll sum weights for the same
  value appropriately, so you can just feed it a sequence of samples."
  
  let [weight-sum (reduce + (map value weighted-values))]
    into {}
      for [[result weight] weighted-values]
        [result #[weight / weight-sum]]

defn weighted-sample
  "Randomly samples an object given a sequence of value -> weight maps.
  You probably want the total mass to be 1."
  
  [normalized-weighted-results]
    weighted-sample normalized-weighted-values (rand)
  
  [normalized-weighted-results remaining-weight]
    let [current-weight (value (first remaining-values))]
      if #[remaining-space < current-weight]
        (first remaining-values) :value
        recur (rest remaining-values) #[remaining-space - current-weight]

defn take-all [n s]
  "(vec (take n s)) or nil if #[n < (count s)]."
  
  let [taken-vec (vec (take n s))]
    if #[(count taken-vec) = n]
      taken-vec

defn chains [n s]
  "Returns a lazy sequence of all length n subsequences of s."
  
  let [chain (take-all n s)]
    if chain
      lazy-seq
        cons
          chain
          chains n (rest s)

defn vec-split-last [v]
  let [n (count v)]
    list
      subvec v 0 #[n - 1]
      nth v #[n - 1]

defn following-occurrences
  "Given a sequence of 2-item sequences and returns a map from items that
  occur first to a map from the items that appear after them to the number of
  times they do so."
  
  [s]
    following-occurrences s {}
  
  [s m]
    let [[tail head] (first s)
          new-counts (merge-with #(merge-with + %1 %2)
                                 m { tail { head 1 } })]
        if (seq (rest s))
          recur (rest s) new-counts
          new-counts

defn map-keys [f m]
  "Maps a function over the keys of a map."
  
  into {}
    map
      fn [[key_ value]]
        [(f key_) value]
      m

defn map-values [f m]
  "Maps a function over the values of a map."
  
  into {}
    map
      fn [[key_ value]]
        [key_ (f value)]
      m

defn following-weights [s]
  map-values
    fn [value]
      map
        fn [[key_ occurrences]]
          { :value key_ :weight occurrences }
        value
    following-occurrences s

defn following-weights-normalized [s]
  map-values
    normalize-weights
    following-weights s

defn markov-evaluate [n s]
  following-weights-normalized
    map
      vec-split-last
      chains #[n + 1] (concat [::start] s [::end])

defn markov-sample
  [weights starting-point]
    weighted-sample (weights (vec starting-point))
  
  [weights]
    markov-sample weights (rand-nth (keys weights))

defn markov-sample-chain
  [weights starting-point terminator]
    let [sample (markov-sample weights starting-point)]
      if #[sample not= terminator]
        lazy-seq
          cons
            sample
            markov-sample-chain
              weights
              conj
                subvec (vec starting-point) 1
                sample
  
  [weights starting-point]
    markov-sample-chain
      weights
      starting-point
      ::end
  
  [weights]
    markov-sample-chain
      weights
      rand-nth (vec (filter #(= ::start (first %))) (weights :keys))

def holmes-evaluation
  markov-evaluate 5 (duck/slurp* "/Users/jeremy/holmes.txt")

defn go []
  println
   str
      "<pre>The "
      apply str (markov-sample-chain holmes-evaluation " The " \.)
      "</pre>"

(go)
)