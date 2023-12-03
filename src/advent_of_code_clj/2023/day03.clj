(ns advent-of-code-clj.2023.day03
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [advent-of-code-clj.utils :as u]
            [clojure.string :as str]))

(def input (-> (puzzle 2023 3)
               (str/split-lines)))

(def not-sym #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.})

(defn parse-input
  [input]
  {:tiles (mapv vec input)
   :all-numbers (mapv #(u/re-seq-pos #"\d+" %) input)})

(defn is-part?
  [tiles row-index col-index number]
  (let [len (count number)]
    (some identity (for [y (range (dec row-index) (+ row-index 2))
                         x (range (dec col-index) (inc (+ col-index len)))]
                     (not (not-sym (get-in tiles [y x] \.)))))))

(defn find-parts
  [{:keys [tiles all-numbers]}]
  (for [row-index (range (count all-numbers))
        {:keys [start group]} (get all-numbers row-index)
        :when (is-part? tiles row-index start group)]
    (parse-long group)))

(def solution1
  (->> input
       (parse-input)
       (find-parts)
       (reduce +)))

(defn get-adjacent-gears
  [tiles row-index col-index number]
  (for [y (range (dec row-index) (+ row-index 2))
        x (range (dec col-index) (inc (+ col-index (count number))))
        :when (= (get-in tiles [y x]) \*)]
    {:number (parse-long number)
     :gear [y x]}))

(defn group-by-gears
  [{:keys [tiles all-numbers]}]
  (->> (for [row-index (range (count all-numbers))
             {:keys [start group]} (get all-numbers row-index)]
         (get-adjacent-gears tiles row-index start group))
       (map first)
       (remove nil?)
       (group-by :gear)
       (vals)
       (map #(map :number %))))

(def solution2
  (->> input
       (parse-input)
       (group-by-gears)
       (remove #(< (count %) 2))
       (map #(apply * %))
       (reduce +)))
