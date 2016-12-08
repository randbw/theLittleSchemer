;; atom? provided by preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
	    ((eq? (car lat) a) (cdr lat)
	     (else (cons (car lat)
			 (rember a
				 (cdr lat))))))))))

(define remberSimple
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
		 (rember a (cdr lat)))))))

(rember and (bacon lettuce and tomato))

(define firsts
  (lambda (lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
	    ((atom? (car lat)) (firsts (cdr lat)))
	    (else (cons (car (car lat))
			(firsts (cdr lat)))))))))

;; ((1 2) 3 (4 5))
;; (1 (2 3) (4 5))
;; all atoms/lists are s expressions
  
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons (car lat)
		   (cons new (cdr lat))))
	    (else (cons (car lat)
			(insertR new old
				 (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
	    ((eq? (car lat) old) (cons new (cons old (cdr lat))))
	    (else
	     (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new (cdr lat)))
	    (else (cons (car lat)
			(subst new old
			       (cdr lat)))))))))
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
	    ((or (eq? (car lat) o1) (eq? (car lat) o2))
	     (cons new (cdr lat))
	     (else (cons (car lat)
			 (subst2 new o1 o2 (cdr lat))))))))))

(define multirember
  (lambda (a lat)
    (cond
     (null? lat) (quote())
     (else
      (cond
       ((eq? a (car lat)) (multirember a (cdr lat)))
       (else (cons (car lat)
		   (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else
      (cond
       ((eq? (car lat) old)
	(cons old (cons new
			(multiinsertR new old (cdr lat)))))
       (else
	(cons (car lat)
	      (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else
      (cond
       ((eq? (car lat) old)
	(cons new (cons old
			(multiinsertL new old (cdr lat))))
	(else
	 (cons (car lat)
	       (multiinsertL new old (cdr lat))))))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else
      ((eq? (car lat) old)
       (cons new
	     (multisubst new old (cdr lat))))
      (else
       (cons (car lat)
	     (multisubst new old (cdr lat))))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

;; mine is tail recursive?
(define +
  (lambda (num adder)
    (cond
     ((zero? adder) num)
     (else
      (+ (add1 num) (sub1 adder))))))

;; BOOK SAYS
(define +
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (+ n (sub1 m)))))))

(define -
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (- n (sub1 m)))))))

;; tup is a list of numbers
;; i.e. (1 2 3 4 5)
;; (1 2 3 4 apple) is a list of atoms aka lat

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (+ (car tup) (addtup (cdr tup)))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (+ n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2))
      (quote()))
     (else
      (cons (+ (car tup1) (car tup2))
	    (tup+ (cdr tup1) (cdr tup2)))))))
;; above only deals with tups that are the same length

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (+ (car tup1) (car tup2))
	    (tup+ (cdr tup1) (cdr tup2)))))))

;; deals with tups of any size

(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (= (sub1 n) (sub1 m))))))

;; rewrite above using < and >
(define =
  (lambda (n m)
    (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t))))

(define exp
  (lambda (n m)
    (cond
     ((= 0 m) 1)
     (else
      (x n (exp n (sub1 m)))))))

(define /
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (/ (- n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ;;((= 1 n) (car lat))
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else
      (cons (car lat)
	    (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote()))
     (else
      (cond
       ((number? car lat) (no-nums (cdr lat)))
       (else
	(cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote()))
     (else
      (cond
       ((number? (car lat))
	(cons (car lat) (all-nums (cdr lat))))
       (else
	(all-nums (cdr lat))))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else
      (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eqan? a (car lat))
	(add1 (occur a (cdr lat))))
       (else
	(occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (cond
     ((= n 1) #t)
     (else #f))))

;; TLS suggests the following 2 ways:
(define one?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (zero? (sub1 n))))))

(define one?
  (lambda (n)
    (cond
     (else (= n 1)))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else
      (cons (car lat)
	    (rempick (sub1 n) (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote()))
     ((lat? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
     (else
      (cond
       ((eqan? (car l) a) (rember* a (cdr l)))
       (else
	(cons (car l) (rember* a (cdr l)))))))))
;; my solution checked for lat? instead of atom?

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote()))
     ((atom? (car l))
      (cond
       ((eqan? (car l) a) (rember* a (cdr l)))
       (else
	(cons (car l) (rember* a (cdr l))))))
     (else
      (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote()))
     ((atom? (car l))
      (cond
       ((eq? old (car l))
	(cons old
	      (cons new
		    (insertR* new old (cdr l)))))
       (else
	(cons (car l)
	      (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l))
	    (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eqan? a (car l))
	(add1 (occur* a (cdr l))))
       (else
	(occur* a (cdr l)))))
     (else
      (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote()))
     ((atom? (car l))
      (cond
       ((eq? old (car l))
	(cons new
	      (subst* new old (cdr l))))
       (else (cons (car l)
		   (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l))
	    (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new
	      (cons old
		    (insertL* new old (cdr l)))))
       (else
	(cons (car l)
	      (insertL* new old (cdr l))))))
     (else
      (cons
       (insertL* new old (car l))
       (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a)
	  (member* a (cdr l))))
     (else
      (or (member* a (car l))
	  (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else
      (leftmost (car l))))))

;; "and" goes through list of predicates checking if they are all true. if one
;; is false it stops and returns false otherwise if it gets to end it returns true

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((null? l1)
      (cond
       ((null? l2) #t)
       (else #f)))
     ((atom? (car l1))
      (cond
       ((atom? (car l2))
	(cond
	 ((eqan? (car l1) (car l2))
	  (eqlist? (cdr l1) (cdr l2)))
	 (else #f)))))
     (else
      (cond
       ((atom? (car l2)) #f)
       ((null? l2) #f)
       (else
	(and
	 (eqlist? (car l1) (car l2))
	 (eqlist? (cdr l1) (cdr l2)))))))))

;; book solution
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((and (null? l1) (atom? (car l2))) #f)
     ((null? l1) #f)
     ((and (atom? (car l1)) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))
     ((atom? (car l1)) #f)
     ((null? l2) #f)
     ((atom? (car l2)) #f)
     (else
      (and (eqlist? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))

;; rewrite
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else
      (and (eqlist? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else
      (eqlist? s1 s2)))))

;; rewrite eqlist? using equal?
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2) #f))
     (else
      (and (equal? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))

;; simplify
(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote()))
     ((atom? car l)
      (cond
       ((equal? (car l) s) (cdr l))
       (else
	(cons (car l)
	      (rember s (cdr l))))))
     (else (cond
	    ((equal? (car l) s) (cdr l))
	    (else
	     (cons (car l)
		   (rember s (cdr l)))))))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote()))
     ((equal? (car l) s) (cdr l))
     (else
      (cons (car l) (rember s (cdr l)))))))

;; simplify
(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new
	      (cons old
		    (insertL* new old (cdr l)))))
       (else
	(cons (car l)
	      (insertL* new old (cdr l))))))
     (else
      (cons
       (insertL* new old (car l))
       (insertL* new old (cdr l)))))))

;; trick question; you cannot as you must know whether or not (car l) is an atom
;; before you ask (eq? (car l) old)

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) (quote +))
      (and (number? aexp) (number? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote *))
      (and (number? aexp) (number? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote ^))
      (and (number? aexp) (number? (car (cdr (cdr aexp))))))
     (else rekt))))

;; simplify
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and
       (number? (car aexp))
       (number? (car (cdr (cdr aexp)))))))))

;; value for representations with operator in middle, i.e. (1 + 3)
(define value
  (lambda (nexp)
    (cond
     ((and (atom? nexp) (number? nexp)) nexp)
     ((eq? (car (cdr aexp)) (quote +))
      (+ (value (car nexp))
	 (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr aexp)) (quote *))
      (* (value (car nexp))
	 (value (car (cdr (cdr nexp))))))
     (else
      (^ (value (car nexp))
	 (value (car (cdr (cdr nexp)))))))))

;; value for (+ 1 3)
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) (quote +))
      (+ (value (car (cdr nexp)))
	 (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) (quote *))
      (* (value (car (cdr nexp)))
	 (value (car (cdr (cdr nexp))))))
     (else
      (eq? (car nexp) (quote ^))
      (^ (value (car (cdr nexp)))
	 (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (nexp)
    (car nexp)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +))
      (+ (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote *))
      (* (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp))))
     (else
      (^ (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp)))))))

;; rewriting 1st-sub-exp and operator for (1 + 3) arithmetic expressions
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))
(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define zero-test
  (lambda (n)
    (null? n)))

(define add-one
  (lambda (n)
    (cons (quote ()) n)))

(define sub-one
  (lambda (n)
    (cdr n)))

(define +
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (+ (add-one n) (sub-one m))))))

;; book has below
(define +
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add-one (+ n (sub-one m)))))))

(define lat?
  (lambda (l)
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

;; Chapter 7 - Sets <- beginning of me marking chapters.
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else
      (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((member? (car lat) (cdr lat))
      (cons (car lat) (makeset (rember (car lat) (cdr lat)))))
     (else
      (cons (car lat) (makeset (cdr lat)))))))

;; book has different form of method which returns different result to one they gave
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else
      (cons (car lat) (makeset (cdr lat)))))))

;; write makeset with multirember
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((member? (car lat) (cdr lat))
      (cons (car lat) (multirember (car lat) (cdr lat))))
     (else
      (cons (car lat) (makeset (cdr lat)))))))

;; simplified
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cons (car lat)
	    (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2)
      (subset? (cdr set1) set2))
     (else #f))))

;; subset? with (and ...)
(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
	   (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else
      (intersect? (cdr set1) set2)))))

;; with or
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or
       (member? (car set1) set2)
       (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2)
      (cons (car set1)
	    (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1)
	    (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     (null? (cdr l-set) (car l-set))
     (else
      (intersect (car l-set)
		 (intersectall (cdr l-set)))))))

(define a-pair
  (lambda (l)
    (cond
     ((atom? l) #f)
     ((null? l) #f)
     ((null? (cdr l)) #f)
     ((null? (cdr (cdr l))) #t)
     (else #f))))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;; a finite function is a list of pairs where none of the first elements are repeated
;; i.e. the "firsts" make a set

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else
      (cons (build (second (car rel)) (first (car rel)))
	    (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

;; rewrite revrel using revpair
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else
      (cons (revpair (car rel))
	    (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (and
     (fun? fun)
     (fun? (revrel fun)))))

;; TLS has version below. doesn't take into account the firsts
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

;; TLS defines seconds just like firsts
;; I wrote my version (below) relevant to pairs
(define seconds
  (lambda (rel)
    (firsts (revrel rel))))

;; fullfun can be thought of as one-to-one
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

;; Chapter 8 - Lambda the Ultimate
(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) (quote ()))
     ((test? (car l) a) (cdr l))
     (else
      (cons (car l)
	    (rember-f test? a (cdr l)))))))

(define curry-time ;; TLS calls this eq?-c, -c meaning curry
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) (quote ()))
       ((test? a (car l)) (cdr l))
       (else
	(cons (car l)
	      ((rember-f test?) a (cdr l))))))))

;; how to rename (rember-f test?) where test? is eq?
(define test? (eq?))
(define rember-eq? (rember-f test?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((test? old (car l))
	(cons new (cons old (cdr l))))
       (else
	(cons (car l)
	      ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((test? old (car l))
	(cons old (cons new (cdr l))))
       (else
	(cons (car l)
	      (cons ((insertR-f test?) new old (cdr l)))))))))
