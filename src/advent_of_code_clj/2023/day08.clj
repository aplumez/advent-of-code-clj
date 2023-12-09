(ns advent-of-code-clj.2023.day08
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

(def directions (cycle (first (str/split-lines (puzzle 2023 8)))))

(def nodes (->> (str/split-lines (puzzle 2023 8))
                (drop 2)
                (mapv #(let [[node choices] (str/split % #" = ")
                             choices (str/replace choices #"\(|\)|\s" "")]
                         [node (str/split choices #"\,")]))))

(defn find-next-from-step [node starting-step pred]
  (loop [current-node node, step starting-step]
    (if (and (pred current-node) (not= starting-step step))
      [current-node step]
      (let [[_ [left right]] (->> nodes (filter #(= current-node (first %))) first)
            next-node (if (= (nth directions step) \L) left right)]
        (recur next-node (inc step))))))

(def solution1 (last (find-next-from-step "AAA" 0 #(= % "ZZZ"))))

(defn find-steps [node pred]
  (loop [steps #{}, ending-nodes []]
    (let [current-node (or (last ending-nodes) node)
          starting-step (if-not (empty? steps) (apply max steps) 0)
          [next-node next-step]
          (find-next-from-step current-node starting-step pred)]
      (if (contains? (set ending-nodes) next-node)
        steps
        (recur (conj steps next-step) (conj ending-nodes next-node))))))

(def solution2
  (->> (for [starting-node (filter #(str/ends-with? (first %) "A") nodes)
             :let [steps (find-steps (first starting-node) #(str/ends-with? % "Z"))]]
         (reduce #(math/lcm %1 %2) steps))
       (reduce #(math/lcm %1 %2))))
