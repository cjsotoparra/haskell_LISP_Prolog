#lang racket
(define (sum s)
  (cond
    ((null? s) 0)
    (else (+ (car s) (sum (cdr s))))
   )
 )

(define (deep-sum l)
  (cond
    ((null? l) 0)
    ((number? l) l)
    ((list? l)
         (+ (deep-sum (car l))
            (deep-sum (cdr l))))
    (else l)
    )
  )

(define (deep-inc s i)
  (cond
    ((null? s) s)
    ((number? s) (+ s i))
    ((list? s)
            (cons (deep-inc (car s) i)
            (deep-inc (cdr s) i)))
    (else s)
    )
  )

(define lat '(1 2 3))

(require racket/trace)
(trace deep-sum)
(trace deep-inc)