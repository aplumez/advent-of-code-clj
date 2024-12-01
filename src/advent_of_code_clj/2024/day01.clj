(ns advent-of-code-clj.2024.day01
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [advent-of-code-clj.utils :as u]
   [clojure.string :as string]))

(def input (->> (puzzle 2024 1)
                (string/split-lines)
                (map #(string/split % #"\s+"))
                (map #(map parse-long %))))

(def solution1 (->> input
                    (u/transpose)
                    (map sort)
                    (u/transpose)
                    (reduce (fn [acc [a b]]
                              (+ acc (abs (- a b))))
                            0)))

(def solution2
  (let [[left right] (u/transpose input)]
    (->> left
         (map (fn [e]
                (-> (filter #(= e %) right)
                    (count)
                    (* e))))
         (reduce +))))
