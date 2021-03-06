(ns clojure-playground.core
  (:gen-class))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn increment-coll [coll]
  (if (first coll)
    (cons (inc (first coll))
          (increment-coll (rest coll)))))
(defn increment-even [coll]
  (if (first coll)
    (if (even? (first coll))
      (cons (inc (first coll))
            (increment-even (rest coll)))
      (cons (first coll) (increment-even (rest coll))))
    coll))

(defn transform-all [f coll]
  (if (first coll)
    (cons (f (first coll))
          (transform-all f (rest coll)))
    coll))

(defn expand [f number count]
  (if (pos? count)
    (cons number (expand f (f number) (dec count)))))


(defn expand-diff [f number]
  (cons number (lazy-seq (expand-diff f (f number)))))
(def reverse-map
  (map (fn [x y] {x y}) [:a :b :c] [1 2 3]))

(defn multiply-pairs []
  (map (fn [pair] (* (nth pair 0) (nth pair 1)))
       (partition 2 3
                  (filter odd?
                          (iterate inc 0)))))

(defn count-char [s]
  (count (filter #(= \c %) s)))


(defn own-filter [f coll]
  (if (first coll)
    (if (f (first coll))
      (cons (first coll)
            (lazy-seq (own-filter f (rest coll))))
      (own-filter f (rest coll)))
    coll))

(defn fib ([]
           (fib 0 1))
  ([a b]
   (let [sum (+ a b)]
     (cons a
           (lazy-seq (fib b sum))))))

(defn palindrome? [x]
  (= (seq (str x)) (reverse (str x))))

(defn count-palindrome []
  (->> (range 10000) (own-filter palindrome?) (count)))

(defmacro id [fun & args]
  "Macro
     Returns evaluation of passed function and arguments"
  `(~fun ~@args))

(def logging-enabled false)

(defmacro log [message]
  (when logging-enabled
    (prn message)))
