(ns advent-of-code-clj.2024.day02
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [clojure.string :as string]))

(def input (->> (puzzle 2024 2)
                (string/split-lines)
                (map #(string/split % #"\s"))
                (mapv (partial mapv parse-long))))

(defn safe-results
  [coll]
  (let [pairs (partition 2 1 coll)
        inc? (->> pairs
                  (map (partial reduce -))
                  (#(> (count (filter pos? %))
                       (count (filter neg? %)))))]
    (->> pairs
         (reduce (fn [acc [a b]]
                   (conj acc (and (<= 1 (abs (- a b)) 3)
                                  (if inc?
                                    (> a b)
                                    (> b a)))))
                 []))))

(defn safe-level?
  ([tolerance coll]
   (safe-level? tolerance 0 coll))
  ([tolerance nb-removed coll]
   (loop [coll coll
          nb-removed nb-removed]
     (if (> nb-removed tolerance)
       false
       (let [results (safe-results coll)
             first-false (.indexOf results false)
             last-false (.lastIndexOf results false)]
         (condp = (count (filter false? results))

           0 true

           1 (or (safe-level? tolerance
                              (inc nb-removed)
                              (vec (concat (subvec coll 0 first-false)
                                           (subvec coll (min (inc first-false)
                                                             (count coll))))))
                 (safe-level? tolerance
                              (inc nb-removed)
                              (vec (concat (subvec coll 0 (inc first-false))
                                           (subvec coll (min (+ 2 first-false)
                                                             (count coll)))))))

           2 (if (= 1 (- last-false first-false))
               (recur (vec (concat (subvec coll 0 (inc first-false))
                                   (subvec coll (inc last-false))))
                      (inc nb-removed))
               false)

           false))))))

(def solution1 (->> input
                    (filter (partial safe-level? 0))
                    (count)))

(def solution2 (->> input
                    (filter (partial safe-level? 1))
                    (count)))

[solution1 solution2]
