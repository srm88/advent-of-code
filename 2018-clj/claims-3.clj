(ns advent-of-code.claims
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))

(def check? #{:one})

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


(defn parse-claim [claim]
  (let [[id _at offset dimensions] (string/split claim #" ")
        [from-left from-top] (string/split (subs offset 0 (dec (count offset))) #",")
        [width height] (string/split dimensions #"x")]
    {:id id
     :from-left (Integer/parseInt from-left)
     :from-top (Integer/parseInt from-top)
     :width (Integer/parseInt width)
     :height (Integer/parseInt height)}))

(defn coords-in-claim [claim]
  (for [offset-x (range (:width claim))
        offset-y (range (:height claim))]
    [(+ (:from-left claim) offset-x)
     (+ (:from-top claim) offset-y)]))

(defn ->>log [thing]
  (pprint thing)
  thing)

(defn claimed-square-inches [claims]
  (->> claims
       (map parse-claim)
       (map (fn [claim]
              (into {} (for [coord (coords-in-claim claim)]
                         [coord 1]))))
       (apply merge-with +)
       vals
       (filter #(> % 1))
       count))

(def check-claimed-square-inches (partial check claimed-square-inches))

(when (:one check?)
  (check-claimed-square-inches 4 ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])
  (check-claimed-square-inches "?" (read-input)))

;; ######
;; Part 2
;; ######


;(def check- (partial check ))

;(when (:two check?)
  ;(check- x)
  ;(check- "?" (read-input)))
