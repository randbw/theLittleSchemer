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
	    ((eq? (car lat) old) (cons new lat))
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
