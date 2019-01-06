(ns advent-of-code.manhattan
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))

(def check? #{:one})

(def debug? true)

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


(defn parse-point [idx line]
  (let [[x y] (string/split line #", ")]
    {:id idx
     :point [(Integer/parseInt x)
             (Integer/parseInt y)]}))

(defn abs [x]
  (if (> 0 x) (- x) x))

(defn manhattan [[x-a y-a] [x-b y-b]]
  (+ (abs (- x-a x-b))
     (abs (- y-a y-b))))

(defn closest-point-to [location points]
  (if (= 1 (count points))
    (first points)
    (let [distances (->> (for [point points]
                           [(manhattan location (:point point)) point])
                         (sort-by first <)
                         (apply vector))]
      ; nil indicates a tie, otherwise return the point
      (if (= (get-in distances [0 0])
             (get-in distances [1 0]))
        nil
        (get-in distances [0 1])))))

(defn bounding-box [points]
  {:min-x (apply min (map #(get-in % [:point 0]) points))
   :max-x (apply max (map #(get-in % [:point 0]) points))
   :min-y (apply min (map #(get-in % [:point 1]) points))
   :max-y (apply max (map #(get-in % [:point 1]) points))})

(defn on-edge? [[x y] bounds]
  (or (= x (:min-x bounds))
      (= x (:max-x bounds))
      (= y (:min-y bounds))
      (= y (:max-y bounds))))

(defn largest-finite-area [lines]
  (let [points (map-indexed parse-point lines)
        bounds (bounding-box points)
        closest-points (->> (for [x (range (:min-x bounds) (inc (:max-x bounds)))
                                  y (range (:min-y bounds) (inc (:max-y bounds)))]
                              [[x y] (closest-point-to [x y] points)])
                            (into {}))
        infinite-points (->> (for [x (range (:min-x bounds) (inc (:max-x bounds)))
                                   y (range (:min-y bounds) (inc (:max-y bounds)))
                                   :when (on-edge? [x y] bounds)]
                               (closest-points [x y]))
                             (filter some?)
                             (map :id)
                             set)]
    (->> closest-points
         (map val)      ; discard the grid coords, we only care about points now
         (filter some?) ; get rid of ties
         (map :id)
         (filter #(not (infinite-points %)))
         (reduce #(update %1 %2 (fnil inc 0)) {}) ; count appearances per point
         (sort-by val >)
         first
         val)))

(def check-largest-finite-area (partial check largest-finite-area))

(when (:one check?)
  (check-largest-finite-area 17 ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])
  (check-largest-finite-area "?" (read-input)))

;; ######
;; Part 2
;; ######


;(def check- (partial check ))

;(when (:two check?)
  ;(check- x)
  ;(check- "?" (read-input)))
