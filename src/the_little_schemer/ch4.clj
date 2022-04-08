(ns the-little-schemer.ch4)
(require '[clojure.core.match :refer [match]])

;; Ch4. Numbers Games

;; inc is add1
(inc 67)

;; dec is sub1
(dec 5)

;; We only consider non-negative number here

(zero? 0)

;; original version defined in the book
;; I don't like this implementation
(defn o+' [n m]
  (cond
    (zero? m) n
    :else (inc (o+' n (dec m)))))

;; tail recursion
(defn o+ [n m]
  (cond
    (zero? m) n
    :else     (recur (inc n) (dec m))))

(defn o- [n m]
  (cond
    (zero? m) n
    :else     (recur (dec n) (dec m))))

(o- 14 3)

;; tup or tuple is defined as list of number

(defn addup [tup]
  (let [go (fn [tup acc]
             (let [x  (first tup)
                   xs (rest tup)]
               (cond
                 (empty? tup) acc
                 :else        (recur xs (o+ acc x)))))]
    (go tup 0)))

(defn addup' [tup] (reduce o+ 0 tup))

;; what is the terminal condition line of `addup`?
;; ((null? tup) 0)

;; How is tup defined?
;; It's either an empty list
;; or it contains a number and a rest that is also tup

;; What is used in the natural recursion on a list?
;; (rest list)

;; What is used in the natural recursion on a tup?
;; (rest tup)

;; How many question do we need to ask about a list?
;; Two, because it's either empty or it's a "something" and a rest
;; which again is a list

;; What is the natural terminal condition for numbers?
;; (zero? n)

;; What is the natural recursion on a number?
;; (dec 1)

;; The First Commandment (first revision)
;; When recurring on a list of atoms, lat, ask two questions about it:
;; (null? lat) and else
;; When recurring on a number, n, ask two questions about it:
;; (zero? n) and else

(defn addup'' [tup]
  (cond
    (empty? tup) 0
    :else        (o+ (first tup) (addup'' (rest tup)))))
;; Notice the similarity between this line
;; and the last line of function rember
;; (cons (first lat) (rember a (rest)))

(addup   '(3 5 2 8))
(addup'  '(3 5 2 8))
(addup'' '(3 5 2 8))

;; not working. n itself shouldn't decrease
;(defn x [n m]
;  (cond
;    (zero? m) n
;    :else     (recur (o+ n n) (dec m))))

(defn x [n m]
  (let [go (fn [n m acc]
             (cond
               (zero? m) acc
               :else     (recur n (dec m) (o+ acc n))))]
    (go n m 0)))

(defn x' [n m]
  (cond
    (zero? m) 0
    :else     (o+ n (x n (dec m)))))
;; n x m         = n + (n x (m - 1))
;; (n x (m - 1)) = n + (n x (m - 2))
;; (n x (m - 2)) = n + (n x (m - 3))
;; ...
;; (n x (m - (m - 1)) = n + (n x (m - m)) = n + n x 0 = n x 1 = n

;; The Fourth Commandment (first revision)
;; Always change at least one argument while recurring.
;; It must be changed to closer to termination.
;; The changing argument must be tested in the termination condition.
;; when using (rest), test termination with (empty?)
;; when using (dec) , test termination with (zero?)

(x  5 3)
(x' 5 3)

;; The Fifth Commandment
;; When building a value with +, always use 0 for the value of the terminating line,
;; for adding 0 does not change the value of an addition
;; When building a value with *, always use 1 for the value of the terminating line,
;; for multiplying by 1 does not change the value of a multiplication.
;; When building a value with cons, always consider () empty list for the value of the terminating line.

;; If you're thinking of Monoid, you're with me.
;; The mempty of + is 0
;; The mempty of * is 1
;; The mempty of concat is [] (empty list)

(defn tup+ [tup1 tup2]
  (let [go (fn [tup1 tup2 acc]
             (cond
               (or (empty? tup1) (empty? tup2)) (reverse acc)
               :else                            (recur (rest tup1) (rest tup2)
                                                       (cons (o+ (first tup1) (first tup2)) acc))))]
    (go tup1 tup2 '())))

(let [tup1 '(3 6 9 11 4)
      tup2 '(8 5 2 0  7)]
  (tup+ tup1 tup2))

(let [tup1 '(2 3)
      tup2 '(4 6)]
  (tup+ tup1 tup2))

;; What is unusual about tup+
;; It looks at each element of two tups at the same time
;; in other words, it recurs on two tups

(defn tup+' [tup1 tup2]
  (cond
    (or (empty? tup1) (empty? tup2)) '()
    :else                            (cons (o+   (first tup1) (first tup2))
                                           (tup+ (rest  tup1) (rest  tup2)))))

(let [tup1 '(2 3)
      tup2 '(4 6)]
  (tup+' tup1 tup2))

(> 12 133)
(> 120 11)

;; clojure already has >. I will call the new one "bigger?"

(defn bigger? [n m]
  (cond
    (zero? n) false                                         ; Switch the (zero?) lines will get different result
    (zero? m) true
    :else     (recur (dec n) (dec m))))

(bigger? 12 133)
(bigger? 120 11)
(bigger? 3 3)

(defn smaller? [n m]
  (cond
    (zero? m) false
    (zero? n) true                                         ; Switch the (zero?) lines will get different result
    :else     (recur (dec n) (dec m))))

(smaller? 8 3)
(smaller? 6 6)

(defn eq? [n m]
  (cond
    (bigger?  n m) false
    (smaller? n m) false
    :else         true))

(defn eq?' [n m]
  (cond
    (zero? m) (zero? n)
    (zero? n) false
    :else    true))

(defn pow' [n m]
  (cond
    (zero? m) 1
    :else (x n (pow' n (dec m)))))

(defn pow [n m]
  (let [go (fn [n m acc]
             (cond
               (zero? m) acc
               :else     (recur n (dec m) (x n acc))))]
    (go n m 1)))

(defn div [n m]
  (let [go (fn [n m acc]
             (cond
               (smaller? n m) acc
               :else          (recur (o- n m) m (inc acc))))]
    (go n m 0)))

;; This is like quotient
(defn div' [n m]
  (cond
    (smaller? n m) 0
    :else          (inc (div' (o- n m) m))))

(div 15 4)
;; How do we get there?
;; (15 / 4) = 1 + (11 / 4)
;;          = 1 + (1 + (7 / 4))
;;          = 1 + (1 + (1 + (3 / 4))) ;; 3 < 4 so return 0
;;          = 1 + (1 + (1 + (0))

(defn length [l]
  (let [go (fn [l acc]
             (cond
               (empty? l) acc
               :else      (recur (rest l) (inc acc))))]
    (go l 0)))

(defn length' [l]
  (cond
    (empty? l) 0
    :else      (inc (length' (rest l)))))

(length '(:hotdogs :with :mustard :sauerkraut :and :pickles))

(defn pick [n lat]
  (cond
    (<= n 0) nil                                         ;; make sure you won't break it when n <= 0
    (zero?   (dec n)) (first lat)
    :else    (recur (dec n) (rest lat))))

(pick 0 '(:hotdogs :with :hot :mustard))                 ;; the repl would have been dead because it never reaches bottom
(pick 3 '(:hotdogs :with :hot :mustard))                 ;; index start with 1, return :hot

(defn rempick [n lat]
  (let [go (fn [n lat before]
             (cond
               (<= n  0)             lat
               (>  n  (length lat))  lat
               (zero? (dec n))       (concat (reverse before) (rest lat))
               :else                 (recur (dec n) (rest lat) (cons (first lat) before))))]
    (go n lat '())))

(defn rempick' [n lat]
  (cond
    (zero? (dec n)) (rest lat)
    :else (cons (first lat) (rempick (dec n) (rest lat)))))

(rempick 3 '(:hotdogs :with :hot :mustard))                 ;; index start with 1. will return the list unmodified if n exceed its range

(number? 76)
(number? :tomato)

(defn no-nums' [l]
  (cond
    (empty? l) '()
    (number? (first l)) (recur (rest l))
    :else               (cons  (first l) (no-nums' (rest l)))))

(defn no-nums [l]
  (let [go (fn [l before]
             (cond
               (empty? l)          (reverse before)
               (number? (first l)) (recur (rest l) before)
               :else               (recur (rest l) (cons (first l) before))))]
    (go l '())))

(no-nums  '(5 :pears 6 :prunes 9 :dates))
(no-nums' '(5 :pears 6 :prunes 9 :dates))

(defn all-nums [l]
  (cond
    (empty? l) '()
    (number? (first l)) (cons  (first l) (all-nums (rest l))) ;swap the condition of number? and else
    :else               (recur (rest l)) ))

(all-nums '(5 :pears 6 :prunes 9 :dates))

;; this function is meaning less because clojure's = can work on various types,
;; but it shows how to implement a generic eq?
(defn eqan? [a1 a2]
  (cond
    (and (number? a1) (number? a2)) (eq? a1 a2)             ;; eq? for numbers
    (or  (number? a1) (number? a2)) false
    :else             (= a1 a2)))                           ;; = for all others atoms

(defn occur [a lat]
  (let [go (fn [a lat acc]
             (cond
               (empty? lat) acc
               (= (first lat) a) (recur a (rest lat) (inc acc))
               :else             (recur a (rest lat) acc)))]
    (go a lat 0)))

(defn occur' [a lat]
  (cond
    (empty? lat) 0
    (= (first lat) a) (inc (occur' a (rest lat)))
    :else             (recur a (rest lat))))

(defn one?' [n]
  (cond (zero? n) false
        :else     (zero? (dec n))))

;; this is how a normal person implement one?
(defn one? [n] (= n 1))
;; use one? to implement rempick
;; just replace (zero? (dec n)) with (one? n)

