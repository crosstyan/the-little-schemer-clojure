(ns the-little-schemer.core)
(require '[clojure.core.match :refer [match]])

;; Alt + Shift + P to send expression to repl in Cursive

;; You cannot ask for the car of an atom

;; http://software-ninja-ninja.blogspot.com/2011/08/clojure-patterns-cons-car-and-cdr.html
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/car-_0026-cdr.html
;; The CAR of a list is, quite simply, the first item in the list.

;; Ch1. Toys
;; what is the car of (a b c)
;; I will use clojure's keyword as atom
;; Maybe I can use quote as well?
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

;; I will only use recur next time
(defn member? [a l]
  (cond
    (empty? l) false
    :else (or (= (first l) a)
              (member? a (rest l)))))

(let [a :poached
      lat '(:fried :eggs :and :scrambled :eggs)]
  (member? a lat))

;; The First Commandment
;; (preliminary)
;; Always ask null? as the first question in expressing any function
;; I don't know why, but it makes sense.

; Yes, else is a question whose value is always true.

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
;;
