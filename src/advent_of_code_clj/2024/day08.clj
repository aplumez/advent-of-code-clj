(ns advent-of-code-clj.2024.day08
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [clojure.string :as string]
   [clojure.set :as set]))

(defonce input
  (->> (puzzle 2024 8)
       (string/split-lines)
       (mapv #(string/split % #""))))

(defn antenna-map
  [input]
  (let [antenna-map (atom {})]
    (doall
     (for [y (range (count input))
           x (range (count (get input y)))
           :let [antenna (get-in input [y x])]
           :when (not= antenna ".")]
       (->> (update @antenna-map antenna (fnil conj #{}) [x y])
            (reset! antenna-map))))
    @antenna-map))

(defn antinodes
  [[x1 y1] [x2 y2]]
  (let [[d-x1 d-y1] [(- x1 x2) (- y1 y2)]
        [d-x2 d-y2] [(- x2 x1) (- y2 y1)]]
    (->> #{[(+ x1 d-x1) (+ y1 d-y1)]}
         (set/union #{[(+ x2 d-x2) (+ y2 d-y2)]})
         (remove #(or (= % [x1 y1]) (= % [x2 y2]))))))

(defn out-of-bounds?
  [input [x y]]
  (not (and (<= 0 x (dec (count (first input))))
            (<= 0 y (dec (count input))))))

(def solution1
  (->> (map (fn [[_ el]]
              (reduce
               (fn [acc [x y]]
                 (->> el
                      (map (partial antinodes [x y]))
                      (reduce set/union #{})
                      (remove (partial out-of-bounds? input))
                      (set)
                      (set/union acc)))
               #{}
               el))
            (antenna-map input))
       (reduce (fn [acc el]
                 (set/union acc el))
               #{})
       (count)))

(defn antinode-iterator
  [[x y] [dir-x dir-y]]
  (iterate (fn [[x y]] [(+ x dir-x) (+ y dir-y)])
           [x y]))

(defn resonant-antinodes
  [input [x1 y1] [x2 y2]]
  (when (not= [x1 y1] [x2 y2])
    (let [[d-x1 d-y1] [(- x1 x2) (- y1 y2)]
          [d-x2 d-y2] [(- x2 x1) (- y2 y1)]
          x1-antinodes
          (take-while (complement (partial out-of-bounds? input))
                      (antinode-iterator [x1 y1] [d-x1 d-y1]))
          x2-antinodes
          (take-while (complement (partial out-of-bounds? input))
                      (antinode-iterator [x2 y2] [d-x2 d-y2]))]
      (set/union (set x1-antinodes) (set x2-antinodes)))))

(def solution2
  (->> (map (fn [[_ el]]
              (reduce
               (fn [acc [x y]]
                 (->> el
                      (map (partial resonant-antinodes input [x y]))
                      (reduce set/union #{})
                      (remove (partial out-of-bounds? input))
                      (set)
                      (set/union acc)))
               #{}
               el))
            (antenna-map input))
       (reduce (fn [acc el]
                 (set/union acc el))
               #{})
       (count)))
