#lang racket

(define-syntax &
  (syntax-rules ()
    [(& e1 e2) (and e1 e2)]))

(& #t #f)

(define-syntax for
  (syntax-rules (in)
    [(for e1 in e2 e3)
     (letrec ([iter
               (lambda (lst)
                 (cond ((null? lst) null)
                       (#t
                        (letrec ([x (car lst)])
                        (begin 
                        (if (procedure? e3) (e3 x) e3)
                        (iter (cdr lst)))))))])
       (iter e2))]
    ))

(for x in (list 1 2 3) (lambda (x) (begin (println x) (println "yes"))))
(for x in (list 1 2 3) (print "owo"))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([final e1]
           [iter (lambda ()
                   (let ([cur e2])
                     (if (< cur final) (iter)
                         #t)])
       ())]))
