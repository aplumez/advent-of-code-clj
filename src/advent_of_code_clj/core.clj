(ns advent-of-code-clj.core
  (:require [environ.core :as env]
            [clj-http.client :as client]))

(defonce puzzles (atom {}))

(def url (partial format "https://adventofcode.com/%d/day/%d/input"))
(def session (env/env :aoc-session))
(def headers {"Cookie" (format "session=%s" session)})

(defn puzzle [year day]
  (let [puzzle (get-in @puzzles [year day])]
    (if puzzle
      puzzle
      (let [retrieved (->> (client/get (url year day) {:headers headers})
                           :body)]
        (swap! puzzles assoc-in [year day] retrieved)
        retrieved))))
