(ns day1.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn load-input [] (slurp (io/resource
                            "input.txt")))

(def initial-state {:direction [0 -1] :location [0 0]})

(defn taxicab [[x y]]
  (+ (abs x) (abs y)))

(defn update-state [state token]
  (let [dir (get token 0)
        step (Integer/parseInt (str/trim  (subs token 1)))
        [dx dy] (state :direction)
        [x y] (state :location)
        [nx ny] (if (= \L dir) [dy (- dx)] [(- dy) dx])]
    {:direction [nx ny] :location [(+ x (* nx step)) (+ y (* ny step))]}))

(defn follow-instructions [tokens]
  (reduce update-state initial-state tokens))

(defn tokenize [input]
  (str/split input #",\s*"))

(defn distance [state]
  (taxicab (state :location)))

(defn solve1 [input]
  (distance (follow-instructions (tokenize input))))

(defn max [& args]
  (loop [max nil
         seq args]
    (if (empty? seq) max
        (let [n (first seq)]
          (if (or (nil? max) (< max n)) (recur n (rest seq)) (recur max (rest seq)))))))

(defn add-five [x] (+ x 5))

;(= __ (add-five 3))

(defn reverse-interleave [col n]
  (for [start (range n)]
    ((defn every-nth [col n start]
       (for [i (range start (count col) n)]
         (nth col i))) col n start)))

(defn calc-intersection' [[x1 y1] [x2 y2]])

(defn calc-intersection [[head & tail]]
  nil ; todo, return coordinates of overlap if there is an overlap with head, with any of the tail
  )

(defn solve2 [input]
  (let [states (reductions update-state initial-state (tokenize input))
        visited-positions (map :location states)
        pairs (reverse (partition 2 1 visited-positions))]
    (loop [list pairs]
      (if (empty? list) nil
          (let [intersection (calc-intersection list)]
            (if (nil? intersection)
              (recur (rest list))
              (intersection)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (solve1 (load-input))))
