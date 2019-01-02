(ns advent-of-code.checksum
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

(defn letter-counts [s]
  (reduce (fn [counts c] (update counts c (fnil inc 0)))
          {}
          s))

(defn checksum [box-ids]
  (let [id-counts (map (comp set vals letter-counts) box-ids)]
    (* (count (filter #(contains? % 2) id-counts))
       (count (filter #(contains? % 3) id-counts)))))

(def check-checksum (partial check checksum))

(when (:one check?)
  (check-checksum 12 ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])
  (check-checksum "?" (read-input)))

;; ######
;; Part 2
;; ######


(defn same-letters [id-a id-b]
  (apply str (for [[char-a char-b] (map vector id-a id-b)
                   :when (= char-a char-b)]
               char-a)))

(defn common-letters [box-ids]
  (let [off-by-one-count (dec (count (first box-ids)))]
    (->> (for [id-a box-ids
               id-b box-ids
               :when (not= id-a id-b)]
           (same-letters id-a id-b))
         (filter #(= (count %) off-by-one-count))
         first)))

(def check-common-letters (partial check common-letters))

(when (:two check?)
  (check-common-letters "fgij" ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])
  (check-common-letters "?" (read-input)))
