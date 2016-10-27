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
