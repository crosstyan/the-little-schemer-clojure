(ns the-little-schemer.ch1)
(require '[clojure.core.match :refer [match]])


;; http://software-ninja-ninja.blogspot.com/2011/08/clojure-patterns-cons-car-and-cdr.html
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/car-_0026-cdr.html
;; The CAR of a list is, quite simply, the first item in the list.
;; You cannot ask for the car of an atom

;; Ch1. Toys
;; what is the car of (a b c)
;; I will use clojure's keyword as atom
;; Maybe I can use quote as well?
;; '(a b c) is kind of the same

;; I MADE A MISTAKE
;; I should have use symbol as atom
;; symbol? is the same as atom?
;; but I'm lazy to change all of them
;; Actually there's not much difference until later
;; when you're going to write parser or other stuff

(first '(:a :b :c))

(first '((:a :b :c) :x :y :z))

; (first :hotdog)
(first '())                                                 ; return nil

;; The Law of Car
;; The primitive car is defined only for non-empty lists
;; But we are using first here, which returns nil for empty list
;; car :: [a] -> a

;; You will need rainbow parenthesis
(first '(((:hotdog)) (:and) (:pickle) :relish))

(let [l '(((:hotdog)) (:and) (:pickle) :relish)]
  (first (first l)))

;; cdr
;; rest

(rest '(:a :b :c))

(rest '((:a :b :c) :x :y :z))

(rest '())                                                  ; return () empty list

;; The Law of Cdr
;; The primitive cdr is defined only for non-empty lists.
;; The cdr of any non-empty list is always another list.
;; But we are using rest here, which returns () for empty list
;; cdr :: [a] -> [a]

(let [l '((:b) (:x :y) ((:c)))]
  (first (rest l)))

(let [l '((:b) (:x :y) ((:c)))]
  (rest (rest l)))

;; What is the cons of the atom a and the list l
;; where a is peanut
;; and l is (butter and jelly)

;; cons is still cons which add an element to the head of list
(let [a :peanut
      l '(:butter :and :jelly)]
  (cons a l))

;; `cons` takes two arguments
;; the first one is any S-expression
;; the second one is any list

(cons :a '())

; (cons ((:a :b :c)) :b)
; no answer

; The law of Cons
; The primitive cons takes two arguments
; The second argument to cons must be a list
; The result is a list
; cons :: a -> [a] -> [a]

(let [s :a
      l '((:b) :c :d)]
  (cons s (first l)))

(let [s :a
      l '((:b) :c :d)]
  (cons s (rest l)))

;; null? basically means empty?
(empty? '())
(empty? (quote ()))

;(empty? :spaghetti) ;; no answer

;; The Law of Null?
;; The primitive null? defined only for lists
;; null? :: [a] | [] -> Bool
;; The equivalent is empty? in clojure

;; We use keyword as atom
(keyword? :s)

;; one equal "=" is the equivalent of eq?
(let [a1 :Harry
      a2 :Harry]
  (= a1 a2))

(let [a1 :margarine
      a2 :butter]
  (= a1 a2))

(let [l1 '()
      l2 '(:strawberry)]
  (= l1 l2))
; No answer () and (:strawberry) are lists
; In practice, list may be arguments of eq?
; like in clojure

(= 6 7)
; same as above

;; The Law of Eq?
;; The primitive eq? takes two arguments.
;; Each must be a non-numeric atom
;; Well, not in clojure
