(ns test-cloj.core
  (:gen-class)
    (:require [clojure.data.csv :as csv]
             [clojure.java.io :as io]))

(def default-iterations "100000")
(def default-conf "80")

(defn get-task
    "Get the name and values from csv"
    ([input] (get-task input {}))
    ([input coll]
     (if (empty? input)
            coll
            (recur (rest input) (assoc coll (get (first input) 0)
                                    {:best (Float/parseFloat (get (first input) 1)), 
                                     :most (Float/parseFloat (get (first input) 2)), 
                                     :worst (Float/parseFloat (get (first input) 3))})))))


(defn get-total
    "Get the total numbers for the three point estimates"
    ([input] (get-total input {:best 0.0 :most 0.0 :worst 0.0}))
    ([input totals]
     (if (empty? input)
         totals
         (recur 
             (rest input)
             (-> totals
                 (assoc-in [:best] (+ (:best totals) (Float/parseFloat (get (first input) 1))))
                 (assoc-in [:most] (+ (:most totals) (Float/parseFloat (get (first input) 2))))
                 (assoc-in [:worst] (+ (:worst totals) (Float/parseFloat (get (first input) 3)))))))))



(defn pert-analysis
    "Perform the PERT analysis"
    [best most worst]
    (float (/ (+ best (* most 4) worst) 6)))


(defn pm-std-deviation
    "Perform the PM standard deviation calculation"
    [best worst]
    (float (/ (+ best worst) 6)))


(defn estimate-range
    "Calculate the estimate range"
    [best most worst]
    (let [pert (pert-analysis best most worst)
          sdv (pm-std-deviation best worst)]
        (sort [(- pert sdv) (+ pert sdv)])))


(defn build-outcome-coll
    "Builds the collection of individual outcomes"
    ([lower upper] 
     (build-outcome-coll lower upper default-iterations ()))
    ([lower upper its] 
     (build-outcome-coll lower upper its ()))
    ([lower upper its coll]
     (if (= its 0)
         coll
         (recur lower upper (dec its) (cons (+ (rand (+ (- upper lower) 0.001)) lower) coll)))))


(defn occurrences
    "Count the occurrences (actually when the confidence level is reached - this is achieved
    by passing back the first value at the conf percentile)"
    ([coll] 
     (occurrences coll default-conf default-iterations default-iterations))
    ([coll conf] 
     (occurrences coll conf default-iterations default-iterations))
    ([coll conf its] 
     (occurrences coll conf its its))
    ([coll conf its permanent-its]
     (if (= its 
            (int (* (int (/ permanent-its 100)) (- 100 conf)))) 
         (first coll)
         (recur (rest coll) conf (dec its) permanent-its))))


(defn -main
  "Takes a file from the command line and runs monte carlo on it. Outputs to outfile. Optional iterations and confidence values"
    ([infile outfile] (-main infile outfile default-iterations default-conf))
    ([infile outfile its] (-main infile outfile its default-conf))
    ([infile outfile its conf]
     (let [its (Integer/parseInt its)
           conf (Integer/parseInt conf)]
    (doseq [[k v] (with-open [rdr (io/reader infile)]
                        (get-task (rest (csv/read-csv rdr))))]
        (with-open [out (io/writer outfile :append true)]
            (csv/write-csv out [[k, (format "%.2f" (occurrences 
                                                       (sort
                                                           (let [best (:best v)
                                                                 most (:most v)
                                                                 worst (:worst v)
                                                                 lower (first (estimate-range best most worst))
                                                                 upper (first (rest (estimate-range best most worst)))]
                                                               (build-outcome-coll lower upper its))) conf its))]])))
    (with-open [out2 (io/writer outfile :append true)]
        (csv/write-csv out2 [["Total", (format "%.2f" (occurrences
                                                          (sort
                                                              (let [output (with-open [rdr2 (io/reader infile)] (get-total (rest (csv/read-csv rdr2))))
                                                                    best (:best output)
                                                                    most (:most output)
                                                                    worst (:worst output)
                                                                    lower (first (estimate-range best most worst))
                                                                    upper (first (rest (estimate-range best most worst)))]          
                                                                  (build-outcome-coll lower upper its))) conf its))]])))))
