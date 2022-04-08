(ns the-little-schemer.ch2)
(require '[clojure.core.match :refer [match]])

;; Ch2. Do it, do it again, and again, and again...

; defn is the same as
; (def lat?
;   (fn [l]
;     (cond ...)))
; fn is lambda
; #t is true
; #f is false

;; use recur instead of lat? inside the function?
(defn lat?
  "a lat is a list of keywords"
  [l]
  (cond
    (empty? l) true
    (keyword? (first l)) (lat? (rest l))
    :else false))

;; using pattern matching and recur
;; See https://github.com/clojure/core.match/wiki/Basic-usage#sequential-types
(defn lat?' [l]
  (match [l]
         [([] :seq)] true
         [([x & xs] :seq)] (and (keyword? x) (recur xs))
         :else false))

;; (empty? l) asks if the argument l is empty list
;; If it is , the value of the application is true
;; If it's not, we ask the next question.

;; the next question
;; (keyword? (first l)) asks if the first S-expression of the list l is a keyword
;; If (first l) is a keyword
;; we want to know if the rest of l is also composed only of keywords
;; If (first l) is not a keyword
;; we ask the next question

;; (lat? (rest )) finds out if the rest of the list l is composed only of keywords
;; by referring to the function with a new argument

;; lat? looks at each S-expression in a list, in turn
;; and asks if each S-expression is a keyword
;; until it runs out of S-expression
;; If it runs out without encountering a list, the value is true
;; If it finds a list or something else, the value is false

;; `else` asks if `else` is true
;; The question `else` is always true
;; :else false
;; If `:else` is true then the answer is false

(let [l1 '()
      l2 '(:a :b :c)]
  (or (empty? l1) (keyword? l2)))

;; (or ...) is the boolean OR
;; (or ...) asks two questions, one at a time.
;; If the first one is true it stops and answers true.
;; Otherwise, it asks the second question and the answer with whatever the second question answers.

;; using pattern matching and clojure tail recursion optimization
;; elixir or Haskell will be more natural to express this
(defn member?'' [a l]
  (match [l]
         [([]       :seq)] false
         [([x & xs] :seq)] (or (= x a) (recur a xs))))

; Can't have 2 overloads with same arity
; You can't do pattern matching in parameter
; which is different in elixir
;(defn member?'''
;  ([_ []]  false)
;  ([a [x & xs]] (or (= x a (member?''' a xs)))))

;; with clojure tail recursion optimization
(defn member?' [a l]
  (cond
    (empty? l) false
    :else (or (= (first l) a)
              (recur a (rest l)))))

;; You should know what "recur" do
(defn member? [a l]
  (cond
    (empty? l) false
    :else (or (= (first l) a)
              (recur a (rest l)))))

(let [a :poached
      lat '(:fried :eggs :and :scrambled :eggs)]
  (member? a lat))

;; The First Commandment
;; (preliminary)
;; Always ask null? as the first question in expressing any function
;; I don't know why, but it makes sense.

; Yes, else is a question whose value is always true.

