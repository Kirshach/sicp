;;  Fibonacci example

(defn fib-recur [n] 
  (case n
    0 0
    1 1
    (+ 
      (fib-recur (- n 1))
      (fib-recur (- n 2)))))

(defn fib-iter 
  ([n] (fib-iter 0 1 n))
  ([a b n] 
    (if (= n 1)
        b
        (fib-iter b (+ a b) (dec n)))))

;; Tree recursion. Coin exchange example

(def coins [1 2 5 10 50])

(defn count-combinations
  ([sum] (count-combinations sum coins))
  ([sum coins]
   (cond
     (= sum 0) 1
     (or (< sum 0) (empty? coins)) 0
     :else (+ (count-combinations sum (pop coins))
              (count-combinations (- sum (last coins)) coins)))))

;; ex. 1.11

(defn f-recur [n] 
  (if (< n 3) 
      n
      (+ (f-recur (- n 1))
         (f-recur (- n 2))
         (f-recur (- n 3)))))

(defn f-iter 
  ([n] (f-iter 0 1 2 n))
  ([a b c n] 
   ;; assuming n >= 3
   (if (= n 0)
       c
       (f-iter a b (+ a b c) (dec n)))))

;; ex.  1.12
(defn get-new-line 
  [prev-line]
  (loop [previous prev-line
         current [1]]
    (if (= 1 (count previous))
        (conj current 1) 
        (recur (rest previous)
               (conj 
                 current 
                 (+ (first previous) 
                    (second previous)))))))

(defn pascal-triangle 
  ;; assuming depth >= 2
  ([depth] 
   (pascal-triangle [[1] [1 1]] (- depth 2)))
  ([lines depth] 
   (if (= depth 0)
       (print-triangle lines)
       (pascal-triangle 
         (conj lines 
               (get-new-line (last lines))) 
         (dec depth)))))

;; formatting functions
(defn get-print-line 
  [lines-count line-index line]
  (let [spaces-before (- lines-count line-index)]
    (str 
      (apply str (repeat spaces-before " "))
      (clojure.string/join " " line))))

(defn print-triangle
  [lines]
  (clojure.string/join 
    "\n" 
    (map-indexed 
      (partial get-print-line (count lines)) 
      lines)))
