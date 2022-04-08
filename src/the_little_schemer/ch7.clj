(ns the-little-schemer.ch7)


;; Ch7. Friends and Relations

;; Set. Element that not appear more than once
;; clojure already has set?
(defn isSet? [lat]
  (cond
    (empty? lat) true
    (member? (first lat) (rest lat)) false
    :else                            (recur (rest lat))))

(defn mk-set' [lat]
  (cond
    (empty? lat) '()
    (member? (first lat) (rest lat)) (recur (rest lat))
    :else                            (cons (first lat) (mk-set' (rest lat)))))

(defn mk-set [lat]
  (cond
    (empty? lat) '()
    :else (cons (first lat) (mk-set (multirember (first lat) (rest lat))))))

;; the function mk-set remembers to `cons` the first atom in the lat onto the result of the natural recursion
;; after removing all occurrences of the first atom from the rest of the lat

(mk-set' '(:apple :peach :pear :peach :plum :apple :lemon :peach))
(mk-set  '(:apple :peach :pear :peach :plum :apple :lemon :peach))

(defn subset? [set1 set2]
  (let [x (first set1) xs (rest set1)]
    (cond
      ; (or (empty? set1) (empty? set2)) true
      ;; we already know that set1 should be the smaller set
      (empty? set1) true
      :else (and (member? x set2) (subset? xs set2)))))

(let [set1 '(5 :chicken :wings)
      set2 '(5 :hamburgers 2 :pieces :fried :chicken :and :light :duckling :wings)]
  (subset? set1 set2))

(let [set1 '(4 :pounds :of :horseradish)
      set2 '(:four :poinds :chicken :and 5 :ounces :horseradish)]
  (subset? set1 set2))

(defn eqset? [set1 set2]
  (and (subset? set1 set2) (subset? set2 set1)))

(defn intersect? [set1 set2]
  (cond
    ;; maybe just
    ;; (empty? set1) false
    ;; (or (empty? set1) (empty? set2)) false
    ;; the only changing set is set1
    ;; set2 will never be empty unless it's empty at first
    (empty? set1) false
    :else (or (member? (first set1) set2) (intersect? (rest set1) set2))))

(let [set2 '(:stewed :tomatoes :and :macaroni)
      set1 '(:macaroni :and :cheese)]
  (intersect? set1 set2))

(defn intersect [set1 set2]
  (cond
    (empty? set1) '()
    (member? (first set1) set2)       (cons (first set1) (intersect (rest set1) set2))
    :else                             (recur (rest set1) set2)))

(defn union [set1 set2]
  (cond
    (empty? set1) set2
    (member? (first set1) set2) (recur (rest set1) set2)
    ;:else                      (cons (first set1) set2))) ;; you need recursion to keep this going, or it will end here
    :else                       (cons (first set1) (union (rest set1) set2))))

;; the only difference between union and difference here is the termination return
;; union is just add the difference to set2, but
;; difference add the difference to '()
(defn difference [set1 set2]
  (let [x (first set1) xs (rest set1)]
    (cond
      (empty? set1)   '()
      (member? x set2) (recur xs set2)
      :else            (cons x (difference xs set2)))))

;; l-set is a list of set
;; ((a b c) (c a d e) (e f g h a b))
(defn intersect-all [l-set]
  (let [x (first l-set) xs (rest l-set)]
    (cond
      ;(empty? l-set) '()
      ;; the intersect of x and empty set is x
      ;; instead of '()
      (empty? xs) x
      :else (intersect x (intersect-all xs)))))

;; the tail recursion version
(defn intersect-all' [l-set]
  (let [go (fn [l-set result]
             (cond
               (empty? l-set) result
               :else          (recur (rest l-set) (intersect result (first l-set)))))]
    (go l-set (first l-set))))

(intersect-all '((6 :pears :and)
                 (3 :peaches :and 6 :peppers)
                 (8 :pears :and 6 :plums)
                 (:and 6 :prunes :with :some :apples)))

(intersect-all' '((6 :pears :and)
                  (3 :peaches :and 6 :peppers)
                  (8 :pears :and 6 :plums)
                  (:and 6 :prunes :with :some :apples)))

;; A pair is a list with only two S-expression

(defn a-pair? [x]
  (cond
    (not (seq? x))           false
    (empty? x)               false
    (empty? (rest x))        false
    (empty? (rest (rest x))) true
    :else                    false))

(a-pair? '(3 7))
(a-pair? '((2) (:pair)))
(a-pair? '(:full (:house)))

;; How can you build a pair with two atoms?
;; (cons x1 (cons x2 '()))
;; How can you build a pair with two S-expression
;; (cons x1 (cons x2 '()))

(defn fst [p] (first p))
(defn snd [p] (first (rest p)))
(defn build [s1 s2] (cons s1 (cons s2 '())))
(defn thd [p] (first (rest (rest p))))

;; We use rel to stand for relation
;; rel should be a set of pairs

;; It's not the function we're talking with lambda
;; just a simple mapping
(defn fun? [rel] (set? (firsts' rel)))                      ;; use tail recursion version

(defn revpair [pair] (build (snd pair) (fst pair)))

(defn revrel [rel]
  (let [x (first rel) xs (rest rel)]
    (cond
      (empty? rel) '()
      :else         (cons (revpair x) (revrel xs)))))
;:else         (cons (build (snd x) (fst x)) (revrel xs)))))

(let [rel '((8 :a) (:pumpkin :pie) (:got :sick))]
  (revrel rel))

(defn getsnds [fun]
  (cond
    (empty? fun) '()
    :else         (cons (snd (first fun)) (getsnds (rest fun)))))

(def seconds getsnds)

(defn fullfun? [fun] (isSet? (getsnds fun)))

(let [fun '((8 3) (4 8) (7 6) (6 2) (3 4))]
  (fullfun? fun))

(let [fun '((8 3) (4 2) (7 6) (6 2) (3 4))]
  (fullfun? fun))

;; another name to fullfun?
;; one-to-one

(defn one-to-one [fun]
  (fun? (revrel fun)))
