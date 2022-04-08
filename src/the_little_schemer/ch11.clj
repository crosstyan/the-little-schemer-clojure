(ns the-little-schemer.ch11)
(require '[clojure.core.match :refer [match]])

;; Lambda the Ultimate is the Chapter 8 of The Little Lisper
;; I played enough JavaScript, so I know that functions are values.
;; I read enough The Little Lisper and know some Haskell, so I know what recursion is.

;; Two in a row
;; an atom appears twice
;; We know that there's at least one element in lat.
;; We must find out whether the next element in lat, if there's one,
;; is identical to this element

(defn is-first? [a lat]
  (cond
    (= a (first lat)) true
    :else             false))

(defn two-in-a-row? [lat]
  (cond
    (empty? lat) false
    :else       (or (is-first? (first lat) (rest lat)) (two-in-a-row? (rest lat)))))

;; Should we change the definitions of two-in-a-row? and is-first? in such a way that
;; two-in-a-row? leaves the decision of whether continuing the search is useful to the revised version of is-first?

;; define but not binding (unbound)
(def is-first?')

(defn two-in-a-row?' [lat]
  (cond
    (empty? lat) false
    :else (is-first?' (first lat) (rest lat))))

(defn is-first?' [a lat]
  (cond
    (empty? lat) false
    :else        (or (= (first lat) a) (two-in-a-row?' lat))))
