(ns day1.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:gen-class))

(def data-file (io/resource
                "input.txt"))

(def initial-state {:direction [0 -1] :location [0 0]})

(defn taxicab [[x y]]
  (+ (abs x) (abs y)))

(defn update-state [state token]
  (let [dir (get token 0)
        step (Integer/parseInt (str/trim  (subs token 1)))
        [dx dy] (state :direction)
        [x y] (state :location)
        [nx ny] (if (= \L dir) [dy (- dx)] [(- dy) dx])]
    {:direction [nx ny] :location [(+ x (* nx step)) (+ y (* ny step))]
     }
    ))

(defn follow-instructions [instructions]
  (let [tokens (str/split instructions #",\s*")]
    (reduce update-state initial-state tokens)
    ))

(defn distance [state]
  (taxicab (state :location)))

(defn solve []
  (distance (follow-instructions (slurp data-file))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
