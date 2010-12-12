(ns tasks
  (:use cake cake.core cake.tasks.test 
        [clojure.test :only [*stack-trace-depth*]]
        [cake.project :only [project-eval]]))

(print project-eval)
(def original-project-eval project-eval)

(undeftask test)
(deftask test #{compile}
  (binding [project-eval original-project-eval]
    (println *stack-trace-depth* "in" (Thread/currentThread))
    (run-project-tests)))