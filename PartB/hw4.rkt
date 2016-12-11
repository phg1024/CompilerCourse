
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (let ([res (s)])
  (if (= 0 n) null
      (cons (car res) (stream-for-n-steps (cdr res) (- n 1))))))

(define (stream-maker f init next)
  (letrec ([g (lambda (i) (cons (f i) (lambda () (g (next i)))))])
    (cdr (g init))))

(define funny-number-stream
  (stream-maker
   (lambda (i) (if (= 0 (remainder i 5)) (- i) i))
   0
   (lambda (i) (+ i 1))))

(define dan-then-dog
  (letrec ([names (vector "dog.jpg" "dan.jpg")])
    (stream-maker
     (lambda (i) (vector-ref names (remainder i (vector-length names))))
     0
     (lambda (i) (+ i 1)))))

(define (stream-add-zero s)
  (lambda ()
    (let ([res (s)])
    (cons (cons 0 (car res)) (stream-add-zero (cdr res))))))

(define (cycle-lists xs ys)
  (let ([i 0])
    (define stream-cycle-lists
      (lambda ()
      (begin (set! i (+ i 1))
             (cons (cons (list-nth-mod xs (- i 1)) (list-nth-mod ys (- i 1)))
                   stream-cycle-lists))))
    stream-cycle-lists))

(define (vector-assoc v vec)
  (letrec ([last-pos (- (vector-length vec) 1)]
           [find-helper (lambda (i)
                          (if (> i last-pos)
                              #f
                              (let ([e (vector-ref vec i)])
                                 (if (and (pair? e) (equal? v (car e)))
                                     e
                                     (find-helper (+ i 1))))))])
    (find-helper 0)))

(define (cached-assoc xs n)
  (letrec ([i 0]
           [cache (make-vector n #f)]
           [fetch (lambda (v)
                    (let ([e (vector-assoc v cache)])
                      (if e
                          e
                          (let ([res (assoc v xs)])
                            (if res
                                (begin (vector-set! cache i res)
                                       (set! i (remainder (+ i 1) n))
                                       res)
                                res)))))])
    fetch))