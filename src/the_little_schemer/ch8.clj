(ns the-little-schemer.ch8)

;; Ch8. Lambda the Ultimate

(let [test? #(not (= % 5))
      l '(6 2 5 3)]
  (filter test? l))

(defn rember-f [test? a l]
  (cond
    (empty? l) '()
    (test? (first l) a) (recur test? a (rest l))
    :else               (cons (first l) (rember-f test? a (rest l)))))

(let [test? =
      a     5
      l     '(6 2 5 3)]
  (rember-f test? a l))

;; Currying
;; https://www.jianshu.com/p/4bf39816e786
;; Curry Haskell 1927 在普林斯顿大学当讲师时重新发现了 Moses Schönfinkel 关于组合子逻辑的成果。
;; Moses Schönfinkel的成果预言了很多 Curry 在做的研究，于是他就跑去哥廷根大学与熟悉Moses Schönfinkel 工作的
;; Heinrich Behmann、Paul Bernays两人一起工作，并于 1930 年以一篇组合子逻辑的论文拿到了博士学位。
;; Curry Brooks Haskell 整个职业生涯都在研究组合子，实际开创了这个研究领域，
;; λ演算中用单参数函数来表示多个参数函数的方法被称为 Currying (柯里化)，虽然 Curry 同学多次指出这个其实是
;; Schönfinkel 已经搞出来的，不过其他人都是因为他用了才知道，所以这名字就这定下来了；

;; Thank you, Moses Schönfinkel
;; It's not called "Schönfinkel-ing"
;; Thank you, Haskell B. Curry

;; I assume that you know what curring is

;; Just use clojure's partial to create curried function
(def rember-eq? (partial rember-f =))
(rember-eq? :tuna '(:tuna :salad :is :good))

(defn seqL [new old l]
  (cons new (cons old l)))

(defn seqR [new old l]
  (cons old (cons new l)))

;; Why we wrote these functions?
;; Because they express what the two differing line in insertL and insertR express

;; clojure has "seq" function. I don't want to mix them up.
(defn insert-g [seqfun new old l]
  (let [x (first l) xs (rest l)]
    (cond
      (empty? l) '()
      ;(= old x) (seqfun new old (insert-g seqfun new old xs)) ; or use xs to insert once
      (= old x) (seqfun new old xs)
      :else     (cons x (insert-g seqfun new old xs)))))

;; substitute
(defn seqS [new old l]
  (cons new l))

;; a helper function to create rember
(defn seqrem [new old l] l)

(defn rember''' [a l]
  (insert-g seqrem :_not_used a l))

;; What you have just seen is the power of abstraction
;; The Ninth Commandment
;; Abstract common patterns with new function

;; skip some line
;; What is multirember&co

;; The name col is short for "collector"
;; A collector is sometimes called a "continuation"
;; https://stackoverflow.com/questions/40819103/how-do-collector-functions-work-in-scheme

;; The Tenth Commandment
;; Build functions to collect more than one value at a time
