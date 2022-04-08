(ns the-little-schemer.ch3)
(require '[clojure.core.match :refer [match]])

;; Ch3. Cons the Magnificent

;; `Rember` stands for remove a member

;; This Code is NOT working
(defn rember' [a lat]
  (cond
    (empty? lat) '()                                        ;; Either a lat is empty or it contains at least one atom.
    :else (cond (= (first lat) a) (rest lat)
                :else (recur a (rest lat)))))               ;; Either equal or not
; We dropped "a", but we also lost all the
; atoms preceding and.

;; So why don't we simplify right away?
;; Because then a function's structure does not coincide with its argument's structure.
;; Functions like rember can always be simplified in this manner.

; The Second Commandment
; Use cons to build lists

;; Can only recur from tail position
(defn rember [a lat]
  (cond
    (empty? lat) '()
    (= (first lat) a) (rest lat)
    :else (cons (first lat) (rember a (rest lat)))))

;; If lat is (:bacon :lettuce :and :tomato) and "a" is :and
;; What is the meaning of (cons (first lat) (rember a (rest lat)))
;; It says to `cons` the first of lat i.e. :bacon onto the value of (rember a (rest lat))
;; which we don't know yet
;; next iteration
;; It says to `cons` the first of (rest lat) i.e. :lettuce onto the value of (rember a (rest (rest lat)))
;; next iteration
;; we now have a value for (rember a (rest (rest lat))) which is (rest (rest (rest lat))) i.e. (:tomato)

;; Use middle function to realize tail recursion
;; https://stackoverflow.com/questions/5844653/haskell-why-the-convention-to-name-a-helper-function-go
;; go as the generic name for tail-recursive worker loops
(defn rember'' [a lat]
  (let [go (fn [a lat before]
             (cond
               (empty? lat) '()
               (= (first lat) a) (concat (reverse before) (rest lat)) ;; you have to reverse it
               :else (recur a (rest lat) (cons (first lat) before))))] ;; because cons add elem to head of list
    (go a lat '())))

;; firsts
;; or zip function in Haskell
;  The function firsts takes one argument, a
;  list, which is either a null list or contains
;  only non-empty lists. It builds another list
;  composed of the first S-expression of each
;  internal list .
;; firsts :: [[a]] -> [a]

;; Can only recur from tail position
(defn firsts [l]
  (cond
    (empty? l) '()
    :else (cons (first (first l)) (firsts (rest l)))))

;; The Third Commandment
;; When building a list, describe the first typical element
;; and then `cons` it onto the natural recursion
;; (cons (first (first l)) (firsts (rest l)))
;;       ----------------  -----------------
;;        typical elems    natural recursion

;; Use middle function to realize tail recursion
(defn firsts' [l]
  (let [go (fn [l result]
             (cond
               (empty? l) (reverse result)                  ; You have to reverse it. See the pattern?
               :else (recur (rest l) (cons (first (first l)) result))))]
    (go l '())))

;; insertR new old lat
;; Insert new in the right of old
;; insertR :: a -> a -> [a] -> [a]
;; The function insertR builds a `lat` with `new` inserted to the right of the first occurrence of `old`.

(defn insertR [new old lat]
  (cond
    (empty? lat) '()
    (= old (first lat)) (cons old (cons new (rest lat)))
    :else (cons (first lat) (insertR new old (rest lat)))))

(defn insertR' [new old lat]
  (let [go (fn [new old lat before]
             (cond
               (empty? lat) '()
               (= old (first lat)) (concat (reverse before) (cons old (cons new (rest lat))))
               :else (recur new old (rest lat) (cons (first lat) before))))]
    (go new old lat '())))

(insertR  :topping :fudge '(:ice :cream :with :fudge :for :dessert))
(insertR' :topping :fudge '(:ice :cream :with :fudge :for :dessert))

(defn insertL [new old lat]
  (cond
    (empty? lat) '()
    (= old (first lat)) (cons new lat)                      ; (cons new lat) == (cons new (cons old (res lat)))
    :else (cons (first lat) (insertL new old (rest lat)))))

(insertL :topping :fudge '(:ice :cream :with :fudge :for :dessert))

;; subst i.e. substitute
(defn subst [new old lat]
  (cond
    (empty? lat) '()
    (= old (first lat)) (cons new (rest lat))
    :else (cons (first lat) (subst new old (rest lat)))))

(subst :topping :fudge '(:ice :cream :with :fudge :for :dessert))

;; I ignore some recur form. You can do complete them as homework.

;; remove all the member of a
;; see rember function above
;; but this one will remove all the appearance of "a"
(defn multirember [a lat]
  (cond
    (empty? lat) '()
    (= (first lat) a) (multirember a (rest lat))       ;I deliberately ignore the second :else to keep the code concise
    :else (cons (first lat) (multirember a (rest lat)))))

;; Can you see how multirember works?
;; Possibly not, so we will go through the steps necessary to arrive at the value (coffee tea and hick)

(defn multirember' [a lat]
  (let [go (fn [a lat before]
             (cond
               (empty? lat) (reverse before)
               (= (first lat) a) (recur a (rest lat) before)
               :else (recur a (rest lat) (cons (first lat) before))))]
    (go a lat '())))

(multirember :cup '(:coffee :cup :tea :cup :and :hick :cup))
(multirember' :cup '(:coffee :cup :tea :cup :and :hick :cup))

;; The Fourth Commandment
;; Always change at least one argument while recurring
;; It must be changed to be closer to termination
;; The changing argument must be tested in the termination condition:
;; when using cdr, test termination with "null?"

(defn multiinsertR [new old lat]
  (match [lat]
         [([]       :seq)] '()
         [([x & xs] :seq)]  (cond (= old x) (cons x (cons new (multiinsertR new old xs)))
                                  :else     (cons x (multiinsertR new old xs)))))

(multiinsertR :cat :cup '(:coffee :cup :tea :cup :and :hick :cup))

(defn multiinsertL [new old lat]
  (let [x  (first lat)
        xs (rest  lat)]           ; this will work because first and rest will work with empty, list at least not panic
    (cond
      (empty? lat) '()
      (= x old) (cons new (cons old (multiinsertL new old xs)))
      :else     (cons x (multiinsertL new old xs)))))

(multiinsertL :cat :cup '(:coffee :cup :tea :cup :and :hick :cup))

