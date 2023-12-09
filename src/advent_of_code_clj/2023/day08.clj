(ns advent-of-code-clj.2023.day08
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

(def directions (-> (puzzle 2023 8)
                    (str/split-lines)
                    (first)))

(def nodes (->> (-> (puzzle 2023 8)
                    (str/split-lines))
                (drop 2)
                (mapv (fn [line]
                        (let [[node choices] (str/split line #" = ")
                              choices (str/replace choices #"\(|\)|\s" "")
                              [left right] (str/split choices #"\,")]
                          {:node node
                           :left left
                           :right right})))))

(defn lookup-node
  [node nodes]
  (->> nodes (filter #(= node (:node %))) first))

(def solution1
  (let [nb-directions (count directions)]
    (loop [current-node "AAA"
           step 1]
      (if (= current-node "ZZZ")
        (dec step)
        (let [current-direction (->> (mod (dec step) nb-directions)
                                     (get directions))
              {:keys [left right]} (lookup-node current-node nodes)
              next-node (if (= current-direction \L) left right)]
          (recur next-node (inc step)))))))

(defn find-next-from-step [node nodes pred starting-step]
  (let [nb-directions (count directions)]
    (loop [current-node node
           step starting-step]
      (if (and (pred current-node) (not= starting-step step))
        {:node current-node :step step}
        (let [current-direction (->> (mod step nb-directions)
                                     (get directions))
              {:keys [left right]} (lookup-node current-node nodes)
              next-node (if (= current-direction \L) left right)]
          (recur next-node (inc step)))))))

(defn find-steps [node nodes size-limit]
  (loop [steps #{}
         ending-nodes []]
    (let [current-node (or (last ending-nodes) node)
          starting-step (if-not (empty? steps) (apply max steps) 0)
          {next-node :node next-step :step}
          (find-next-from-step current-node nodes #(str/ends-with? % "Z") starting-step)]
      (if (contains? (set ending-nodes) next-node)
        steps
        (if (>= (count steps) size-limit)
          (conj steps next-step)
          (recur (conj steps next-step) (conj ending-nodes next-node)))))))

(let [starting-nodes (filter #(str/ends-with? (:node %) "A") nodes)
      indexes (for [starting-node starting-nodes]
                (first (find-steps (:node starting-node) nodes 1)))]
  (reduce (fn [acc index]
            (math/lcm acc index)) indexes))
