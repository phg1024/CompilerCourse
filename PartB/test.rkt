;#lang slideshow

#lang racket

(provide (all-defined-out))

(define s "hello")

(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f (cdr xs)))))
