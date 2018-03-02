(define (myReverse old new)
	(cond
	((null? old) new)
	(else (myReverse (cdr old) (cons (car old) new)))
	)
)

 (define (myLength1 list len)
    (cond
       ((null? list) len)
       (else (myLength1 (cdr list) (+ 1 len)))
    )
)

(define (add-list a b)
  (cond
     ((not (= (myLength a 0) (myLength b 0))) "unequal lengths")
     (else (add-listH a b '()))
  )
)

(define (add-listH a b c)
  (cond
    ((null? a) (myReverse c'()))
    (else (add-listH (cdr a) (cdr b) (cons (+ (car a) (car b)) c)))
  )
)

(define (swap a)
  (cond
    ((or (null? a) (null? (cdr a))) a)
    (else (cons (cadr a) (cons (car a) (swap (cddr a)))))
  )
)

(define (lt-list a b)
  (cond
    ((not (= (myLength a 0) (myLength b 0))) "unequal lengths")
    ((and (null? a) (null? b)) a)
    (else (lt-listH a b '()))
  )
)

(define (lt-listH a b c)
  (cond
    ((null? a) (myReverse c '()))
    (else (lt-listH (cdr a) (cdr b) (cons (< (car a)(car b)) c)))
  )
)

(define unary-zero '())

(define unary-one '(1))

(define (decimal->unary n)
  (cond
    ((< n 0) "negative number")
    (else (decimal-unaryH n '())
  )
)

(define (decimal->unaryH n list)
  (cond
    ((= 0 1) list)
    (else (decimal->unaryH(- n 1) (cons 1 list)))
  )
)

 (define (isUnaryOne? n)
   (cond
     ((isqual? n unary-one))
     (else #f)
   )
)

(define (isUnary? n)
  (cond
    ((not (list? n)) #f)
    ((null? n) #t)
    ((not (equal? 1 (car n))) #f)
    (else (isUnary? (cdr n)))
  )
)

(define (checkUnary n)
  (cond
    ((not (list? n)) "not a unary number")
    ((null? n) #t)
    ((not (equal? 1 (car n)) "not a unary number")
    (else (checkUnary (cdr n))
  )
)
