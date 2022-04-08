(ns the-little-schemer.ch5)
(require '[clojure.core.match :refer [match]])

;; Ch5. *Oh My Gawd* It's Full of Stars
;; https://wiki.c2.com/?MyGodItsFullOfStars

;(defn rember* [a l]
;  (cond
;    (empty? l) '()
;    (keyword? (first l)) (cond (= (first l) a) (recur a (rest l)) ; If (first l) a keyword
;                               :else           (cons (first l) (rember* a (rest l))))
;    :else                (cons (rember* a (first l)) (rember* a (rest l))))) ; Or (first l) is a list

;; I don't know is there any way to write a tail recursion
;; nested list is tricky
(defn rember* [a l]
  (cond
    (empty? l) '()
    (seq? (first l))    (cons (rember* a (first l)) (rember* a (rest l))) ; If (first l) is a list
    (= (first l) a)     (recur a (rest l))                  ; or it equals a (which means it's not a list)
    :else               (cons (first l)             (rember* a (rest l))))) ; or it's not a list nor equals a (the most common case)

;; Notice that now we are recurring
;; down the car of the list , instead of just the
;; cdr of the list.

;; rember* remove member of nested list
(let [l '((:coffee) :cup ((:tea) :cup) (:and (:hick) :cup))
      a :cup]
  (rember* a l))

(defn insertR* [new old l]
  (let [x  (first l)
        xs (rest l)]
    (cond
      (empty? l) '()
      (seq? x)  (cons (insertR* new old x) (insertR* new old xs))
      (= x old) (cons old (cons new (insertR* new old xs))) ; this question should be asked in the :else section
      :else     (cons x                    (insertR* new old xs)))))

(let [l '((:how :much (:wood))
          :could
          ((:a (:wood) :chuck))
          (((:chuck)))
          (:if (:a) ((:wood :chuck)))
          :could :chuck :wood)
      new :roast
      old :chuck]
  (insertR* new old l))

;; The First Commandment (final version)
;; When recurring on a list of atoms, lat, ask two questions about it:
;; (empty? lat) and else
;; When recurring on a number, n, ask two questions about it:
;; (zero? n) and else
;; When recurring on a list of S-expressions, l, ask three question about it:
;; (empty? l) (seq? (first l)) and :else
;; equals to the original
;; (null?  l) else                 (atom? (car l))
;; Anyway, what you interest should be if (first l) is a list or an atom
;; (first l) shouldn't be something else in the world of The Little Schemer

;; How are rember* and multirember different?
;; `multirember` does not recur with the car.
;; The function rember* recurs with the car as well as with cdr.
;; It recurs with the car when it finds out that the car is a list

;; Three questions and recur with the car as well as with the cdr
;; whenever the car is a list
;; Because all *-function work on lists that are either
;; - empty
;; - an atom consed on to a list
;; - or a list consed onto a list

;; The Fourth Commandment (final version)
;; Always change at least one argument while recurring
;; When recurring on al ist of atoms, use (rest lat)
;; When recurring on a list of S-expressions, l, use (first l) and (rest l)
;; if (seq? (first l)) is true.
;; It must be changed to be closer to termination.
;; The changing argument must be tested in the termination condition
;; when using rest, test termination with empty?
;; when using dec , test termination with zero?

(defn occur*-won't-work [a l]
  (let [go (fn [a l acc]
             (cond
               (empty? l) acc
               (seq? (first l)) (recur a (first l) acc)
               (= a (first l))  (recur a (rest l) (inc acc))
               :else            (recur a (rest l) acc)))]
    (go a l 0)))

(defn occur* [a l]
  (cond
    (empty? l) 0
    ; (seq? (first l)) (recur a (first l)) ;; why it's wrong?
    ; you have to add those branch together
    ; only recur won't do anything
    (seq? (first l)) (o+ (occur* a (first l)) (occur* a (rest l)))
    (= a (first l))  (inc (occur* a (rest l)))
    :else            (recur a (rest l))))

(let [l '((:banana)
          (:split ((((:banana :ice)))
                   (:cream (:banana))
                   :sherbet))
          (:banana)
          (:bread)
          (:banana :brandy))
      a :banana]
  (occur* a l))

(defn subst* [new old l]
  (let [x  (first l)
        xs (rest  l)]
    (cond
      (empty? l) '()
      (seq? x)    (cons (subst* new old x) (subst* new old xs))
      (= old x)   (cons new (subst* new old xs))
      :else       (cons x (subst* new old xs)))))

(let [l '((:banana)
          (:split ((((:banana :ice)))
                   (:cream (:banana))
                   :sherbet))
          (:banana)
          (:bread)
          (:banana :brandy))
      old :banana
      new :orange]
  (subst* new old l))

(defn insertL* [new old l]
  (let [x  (first l)
        xs (rest  l)]
    (cond
      (empty? l) '()
      (seq? x)    (cons (insertL* new old x) (insertL* new old xs))
      (= old x)   (cons new (cons old (insertL* new old xs)))
      :else       (cons x (insertL* new old xs)))))

(let [l '((:how :much (:wood))
          :could
          ((:a (:wood) :chuck))
          (((:chuck)))
          (:if (:a) ((:wood :chuck)))
          :could :chuck :wood)
      new :pecker
      old :chuck]
  (insertL* new old l))

(defn member* [a l]
  (let [x  (first l)
        xs (rest  l)]
    (cond
      (empty? l) false
      (seq? x)   (or (member* a x) (member* a xs))
      (= a x)    true
      :else      (recur a xs))))

(let [a :chips
      l '((:potato) (:chips ((:with) :fish) (:chips)))]
  (member* a l))

;; The function leftmost finds the leftmost element in a non-empty list of S-expressions
;; that does not contain the empty list

;; Is leftmost a *-function?
;; It works on list of S-expressions, but it only recurs on the car (so the answer is no)
;; We agreed that leftmost works on non-empty lists that don't contain empty lists

(defn leftmost [l]
  (cond
    (empty? l) nil
    (seq? (first l)) (recur (first l))
    :else            (first l)))

(leftmost '((:potato) (:chips ((:with) :fish) (:chips))))
(leftmost '(((:hot) (:tuna (:and))) :cheese))
(leftmost '(((() :four)) 17 (:seventeen)))
(leftmost '())

;; What's eqlist?
;; eqlist? -> [a] -> [a] -> Bool
;; a function that determines if two lists are equal
;; How many questions will eqlist? have to ask about its arguements?
;; Nine
;; Each argument may be either
;; - empty
;; - an atom consed onto a list
;; - a list consed onto a list

;; check the original eqlist? implementation
;; I don't care it so much
(defn eqlist? [l1 l2]
  (let [x (first l1)
        y (first l2)
        xs (rest l1)
        ys (rest l2)]
    (cond
      (and (empty? l1) (empty? l2)) true
      (and (seq? x) (seq? y)) (recur x y)
      (= x y) (recur xs ys)                                 ; That's cheating because = can compare list
      :else   false)))                                      ; But the appearance of "seq?" indeed simplify things a lot

(let [l1 '(:strawberry :ice :cream)
      l2 '(:strawberry :ice :cream)]
  (eqlist? l1 l2))

(let [l1 '(:strawberry :ice :cream)
      l2 '(:strawberry :cream :ice)]
  (eqlist? l1 l2))

(let [l1 '(:beef ((:sausage)) (:and (:soda)))
      l2 '(:beef ((:sausage)) (:and (:soda)))]
  (eqlist? l1 l2))

(let [l1 '(:beef ((:sausage)) (:and (:soda)))
      l2 '(:beef ((:salami)) (:and (:soda)))]
  (eqlist? l1 l2))

;; The Sixth Commandment
;; Simplify only after the function is correct
