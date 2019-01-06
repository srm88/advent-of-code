(ns advent-of-code.frequencies
  (:require [clojure.string :as string]))

(def check? #{:one :two})

(def debug? false)

(defn debug
  [& args]
  (when debug?
    (apply println args)))

(defn check
  [f expected arg]
  (println "input    " (apply str (take 70 (str arg))))
  (println (str "expected: " expected " got: " (f arg))))

(defn read-input []
  (let [problem-name (subs (str (ns-name *ns*))
                           (count "advent-of-code."))]
    (string/split-lines (slurp (str problem-name ".input")))))

;; ######
;; Part 1
;; ######

(defn frequency [numbers]
  (->> numbers
       (map #(Integer/parseInt %))
       (reduce + 0)))

(def check-frequency (partial check frequency))

(when (:one check?)
  (check-frequency 3 ["+1" "-2" "+3" "+1"])
  (check-frequency 3 ["+1" "+1" "+1"])
  (check-frequency 0 ["+1" "+1" "-2"])
  (check-frequency -6 ["-1" "-2" "-3"])
  (check-frequency "?" (read-input)))

;; ######
;; Part 2
;; ######

(defn duplicate [numbers]
  (loop [ring (cycle (map #(Integer/parseInt %) numbers))
         seen #{0}
         freq 0]
    (let [number (first ring)
          sum (+ freq number)]
      (if (contains? seen sum)
        sum
        (recur (rest ring)
               (conj seen sum)
               sum)))))

(def check-duplicate (partial check duplicate))

(when (:two check?)
  (check-duplicate 0 ["+1" "-1"])
  (check-duplicate 10 ["+3" "+3" "+4" "-2" "-4"])
  (check-duplicate 5 ["-6" "+3" "+8" "+5" "-6"])
  (check-duplicate 14 ["+7" "+7" "-2" "-7" "-4"])
  (check-duplicate "?" (read-input)))
