(ns advent-of-code-clj.2024.day04
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [advent-of-code-clj.utils :as u]
   [clojure.string :as string]))

(def input (->> (puzzle 2024 4)))

(defn find-directions
  [dataset x y find-char]
  (->> (for [x2 (range (if (> x 0) -1 0)
                       (if (< x (dec (count (first dataset)))) 2 1))
             y2 (range (if (> y 0) -1 0)
                       (if (< y (dec (count dataset))) 2 1))]
         (when-not (every? zero? [x2 y2])
           (when (= (-> dataset (nth (+ y y2)) (nth (+ x x2)))
                    find-char)
             [x2 y2])))
       (remove nil?)))

(def solution1 (let [dataset (->> input
                                  (string/split-lines)
                                  (map #(string/split % #"")))
                     max-x (dec (count (first dataset)))
                     max-y (dec (count dataset))]
                 (->> (for [y (range (count dataset))
                            x (range (count (nth dataset y)))
                            :let [char (-> dataset (nth y) (nth x))]
                            :when (= char "X")]
                        (do
                          (for [[x2 y2] (find-directions dataset x y "M")
                                :let [m-x (+ x x2)
                                      m-y (+ y y2)
                                      a-x (+ m-x x2)
                                      a-y (+ m-y y2)
                                      s-x (+ a-x x2)
                                      s-y (+ a-y y2)
                                      a-char (when (and (>= a-x 0) (<= a-x max-x)
                                                        (>= a-y 0) (<= a-y max-y))
                                               (-> dataset (nth a-y) (nth a-x)))
                                      s-char (when (and (>= s-x 0) (<= s-x max-x)
                                                        (>= s-y 0) (<= s-y max-y))
                                               (-> dataset (nth s-y) (nth s-x)))]
                                :when (and (= a-char "A")
                                           (= s-char "S"))]
                            true)))
                      (flatten)
                      (count))))

(defn opposed?
  [directions]
  (= 0
     (->> directions
          (u/transpose)
          (map (partial reduce +))
          (reduce +))))

(defn find-directions-without-zero
  [dataset x y find-char]
  (->> (for [x2 (range (if (> x 0) -1 0)
                       (if (< x (dec (count (first dataset)))) 2 1))
             y2 (range (if (> y 0) -1 0)
                       (if (< y (dec (count dataset))) 2 1))]
         (when-not (some zero? [x2 y2])
           (when (= (-> dataset (nth (+ y y2)) (nth (+ x x2)))
                    find-char)
             [x2 y2])))
       (remove nil?)))

(def solution2 (let [dataset (->> input
                                  (string/split-lines)
                                  (map #(string/split % #"")))]
                 (->> (for [y (range (count dataset))
                            x (range (count (nth dataset y)))
                            :let [char (-> dataset (nth y) (nth x))
                                  m-dirs (find-directions-without-zero dataset x y "M")
                                  s-dirs (find-directions-without-zero dataset x y "S")]
                            :when (and (= char "A")
                                       (= 2 (count m-dirs))
                                       (not (opposed? m-dirs))
                                       (= 2 (count s-dirs))
                                       (not (opposed? s-dirs)))]
                        true)
                      (count))))


