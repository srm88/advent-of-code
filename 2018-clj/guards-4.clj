(ns advent-of-code.guards
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

(defn parse-timestamp
  "year-month-day hour:minute"
  [ts]
  (let [[date-part time-part] (string/split ts #" ")
        [year month day] (string/split date-part #"-")
        [hour minute] (string/split time-part #":")]
    {:year (Integer/parseInt year)
     :month (Integer/parseInt month)
     :day (Integer/parseInt day)
     :hour (Integer/parseInt hour)
     :minute (Integer/parseInt minute)}))

(defn compare-ts [ts-a ts-b]
  (->> [:year :month :day :hour :minute]
       (map #(compare (% ts-a) (% ts-b)))
       (filter #(not= 0 %))
       first))

(defn timestamp->minutes [ts]
  (-> (:minute ts)
      (+ (* 60 (:hour ts)))
      (+ (* 24 60 (:day ts)))
      (+ (* 31 24 60 (:month ts)))
      (+ (* 12 31 24 60 (:year ts)))))

(defn parse-event [event]
  (let [tokens (string/split event #" ")]
    (case (first tokens)
      "Guard" (-> tokens second (subs 1) (Integer/parseInt))
      "falls" :sleep
      "wakes" :wake)))

(defn parse-log [log]
  (let [[ts-part event-part] (string/split log #"] ")
        ts-part (subs ts-part 1)] ; Strip off leading "["
    (assoc (parse-timestamp ts-part) :event (parse-event event-part))))

(defn guard-naps [logs]
  (-> (reduce (fn [info log]
                (cond
                  (= :sleep (:event log)) (assoc info :sleep (:minute log))
                  (= :wake (:event log))  (update info :naps conj {:guard (:guard info)
                                                                   :sleep (:sleep info)
                                                                   :wake (:minute log)})
                  :else                   (assoc info :guard (:event log))))
              {}
              logs)
      :naps))

(defn sleep-counts-by-minute [naps]
  (->> naps
       (map #(range (:sleep %) (:wake %)))
       flatten
       (reduce #(update %1 %2 (fnil inc 0)) {})))

(defn guard-minute [logs]
  (let [naps (->> logs (map parse-log) (sort compare-ts) guard-naps)
        sleep-times (->> naps
                         (map #(hash-map (:guard %) (- (:wake %) (:sleep %))))
                         (apply merge-with +))
        sleepiest-guard (->> sleep-times
                             (sort-by val >)
                             first
                             first)
        sleepiest-minute (->> naps
                              (filter #(= (:guard %) sleepiest-guard))
                              sleep-counts-by-minute
                              (sort-by val >)
                              first
                              first)]
    (* sleepiest-guard sleepiest-minute)))

(def check-guard-minute (partial check guard-minute))

(when (:one check?)
  (check-guard-minute 240 ["[1518-11-01 00:00] Guard #10 begins shift"
                           "[1518-11-01 00:05] falls asleep"
                           "[1518-11-01 00:25] wakes up"
                           "[1518-11-01 00:30] falls asleep"
                           "[1518-11-01 00:55] wakes up"
                           "[1518-11-01 23:58] Guard #99 begins shift"
                           "[1518-11-02 00:40] falls asleep"
                           "[1518-11-02 00:50] wakes up"
                           "[1518-11-03 00:05] Guard #10 begins shift"
                           "[1518-11-03 00:24] falls asleep"
                           "[1518-11-03 00:29] wakes up"
                           "[1518-11-04 00:02] Guard #99 begins shift"
                           "[1518-11-04 00:36] falls asleep"
                           "[1518-11-04 00:46] wakes up"
                           "[1518-11-05 00:03] Guard #99 begins shift"
                           "[1518-11-05 00:45] falls asleep"
                           "[1518-11-05 00:55] wakes up"])
  (check-guard-minute "?" (read-input)))

;; ######
;; Part 2
;; ######


(defn minute-guard [logs]
  (let [naps (->> logs (map parse-log) (sort compare-ts) guard-naps)
        guards (->> naps (map :guard) set)
        [guard [minute freq]] (->> (for [guard guards]
                                     [guard (->> naps
                                                 (filter #(= (:guard %) guard))
                                                 sleep-counts-by-minute
                                                 (sort-by val >)
                                                 first)])
                                   (into {})
                                   (sort-by (comp val val) >)
                                   (first))]
    (* guard minute)))

(def check-minute-guard (partial check minute-guard))

(when (:two check?)
  (check-minute-guard 4455 ["[1518-11-01 00:00] Guard #10 begins shift"
                            "[1518-11-01 00:05] falls asleep"
                            "[1518-11-01 00:25] wakes up"
                            "[1518-11-01 00:30] falls asleep"
                            "[1518-11-01 00:55] wakes up"
                            "[1518-11-01 23:58] Guard #99 begins shift"
                            "[1518-11-02 00:40] falls asleep"
                            "[1518-11-02 00:50] wakes up"
                            "[1518-11-03 00:05] Guard #10 begins shift"
                            "[1518-11-03 00:24] falls asleep"
                            "[1518-11-03 00:29] wakes up"
                            "[1518-11-04 00:02] Guard #99 begins shift"
                            "[1518-11-04 00:36] falls asleep"
                            "[1518-11-04 00:46] wakes up"
                            "[1518-11-05 00:03] Guard #99 begins shift"
                            "[1518-11-05 00:45] falls asleep"
                            "[1518-11-05 00:55] wakes up"])
  (check-minute-guard "?" (read-input)))
