(ns advent-of-code-clj.2023.day02
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [clojure.string :as str]))

(def input (-> (puzzle 2023 2)
               (str/split #"\n")))

(def parsed-input
  (let [parse-fn
        (fn [game-line]
          (map #(str/split % #",")
               (-> game-line
                   (str/replace #"Game (\d)+:" "")
                   (str/split #";"))))
        ease-fn
        (fn [index game]
          {:id (inc index)
           :results (map (fn [game-line]
                           (->> game-line
                                (map #(-> % str/trim (str/split #" ") reverse))
                                (flatten)
                                (apply hash-map)
                                (#(update-vals % parse-long)))) game)})]
    (->> input
         (map parse-fn)
         (map-indexed ease-fn))))

(def solution1
  (let [filter-fn
        (fn [{:keys [results]}]
          (every? (fn [result]
                    (and (<= (get result "red" 0) 12)
                         (<= (get result "green" 0) 13)
                         (<= (get result "blue" 0) 14)))
                  results))]
    (->> parsed-input
         (filter filter-fn)
         (map :id)
         (reduce +))))

(def solution2
  (let [max-key-fn
        (fn [results color]
          [color (get (apply max-key #(get % color 0) results) color)])]
    (->> parsed-input
         (map
          (fn [{:keys [results]}]
            (->> ["blue" "red" "green"]
                 (mapv #(max-key-fn results %))
                 (map second)
                 (apply *))))
         (reduce +))))
