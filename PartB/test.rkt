;#lang slideshow

#lang racket

(provide (all-defined-out))

(define s "hello")

(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f (cdr xs)))))

(define cube
  [lambda (x)
    (* x x x)])

(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

(my-map fact '(1 2 3 4 5 7 8 9 10))