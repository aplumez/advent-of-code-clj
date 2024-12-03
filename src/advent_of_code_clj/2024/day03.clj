(ns advent-of-code-clj.2024.day03
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [clojure.string :as string]))

(def input (->> (puzzle 2024 3)))

(defn parse-mul
  [s]
  (->> s
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map rest)
       (map (partial map parse-long))
       (map (partial reduce *))
       (reduce +)))

(def solution1 (parse-mul input))

(def solution2 (-> input
                   (string/replace #"\s+" "") ;; <- This made me lose way too much time...
                   (string/replace #"don't\(\)(.+?)(do\(\)|$)" "")))
