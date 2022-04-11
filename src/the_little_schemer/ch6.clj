(ns the-little-schemer.ch6)

;; Ch6. Shadows

;; What is (quote a)
;; 'a
;; What is (quote +)
;; The atom +, not the operation +

;; The Eighth Commandment
;; Use help function to abstract from representation

;; No idea what this chapter is about
;; except (1 2 3) is ((()) (() ()) (() () ()))
;; which is non sense

;; No I'm just kidding
;; I don't interested in parser stuff.

;; I was wrong about clojure
;; keywords in clojure are not the same as atoms in scheme
;; but symbol

;; In Lisp, atoms are symbols, numbers, and
;; many other things. Here, atoms are only symbols.

;; https://clojuredocs.org/clojure.core/symbol_q
;; symbol? is the same as atom?

; For the purpose of this chapter, an
; arithmetic expression is either an atom
; (including numbers), or two arithmetic
; expressions combined by +, *, or ↑ (U+2191)

;; borrow the function from
;; https://github.com/quux00/little-schemer/blob/master/clojure/littleclj.clj
(defn atom?                       ; Ch.1, p.10
  "Predicate test for whether an entity is atom
   where atom is defined as not a list (not (list? x))
   using the Clojure built-in list? predicate"
  [x] (not (list? x)))

(defn numbered?' [aexp]
  (let [x (first aexp)
        xs (rest aexp)]
    (cond
      (atom? aexp)    (number? aexp)
      (= (first xs) '+) (and (numbered? x) (numbered? (rest (rest aexp))))
      (= (first xs) '*) (and (numbered? x) (numbered? (rest (rest aexp))))
      (= (first xs) '↑) (and (numbered? x) (numbered? (rest (rest aexp)))))))


;; aexp was already understood to be an arithmetic expression
;; so it skips checking is the operation is valid
(defn numbered? [aexp]
  (let [x (first aexp)
        xs (rest aexp)]
    (cond
      (atom? aexp) (number? aexp)
      :else        (and (numbered? x) (numbered? (rest (rest aexp)))))))

;; https://en.wikipedia.org/wiki/Church_encoding
;; https://zh.wikipedia.org/wiki/%E9%82%B1%E5%A5%87%E6%95%B0
;; https://the-little-schemer.readthedocs.io/zh_CN/latest/chapter_06.html
