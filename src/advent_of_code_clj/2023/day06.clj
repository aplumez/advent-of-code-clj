(ns advent-of-code-clj.2023.day06
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [clojure.string :as str]))

(def input1 [[48 261] [93 1192] [84 1019] [66 1063]])
(def input2 [[48938466 261119210191063]])

(defn solve [[race-time record]]
  (for [hold-time (range 1 race-time)
        :let [distance (* (- race-time hold-time) hold-time)]
        :when (> distance record)]
    1))

(def solution1
  (->> input1
       (mapv (comp count solve))
       (reduce *)))


(def solution2
  (->> input2
       (mapv (comp count solve))
       (reduce *)))
