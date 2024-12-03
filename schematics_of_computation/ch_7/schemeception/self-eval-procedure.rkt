#!/usr/bin/racket
#lang racket

(require compatibility/mlist)
(require "eval-error.rkt")

; pg 389

(provide 
    make-procedure
    is-procedure?
    procedure-params
    procedure-body
    procedure-env)

(define procedure-flag '**procedure**)

(define (make-procedure args body env)
    (list procedure-flag args body env))

(define (is-procedure? proc)
    (and (pair? proc) (eqv? (car proc) procedure-flag)))

(define (procedure-params proc)
    (if (is-procedure? proc)
        (list-ref proc 1)
        (evaluator-error "Procedure expected" proc)))

(define (procedure-body proc)
    (if (is-procedure? proc)
        (list-ref proc 2)
        (evaluator-error "Procedure expected" proc)))

(define (procedure-env proc)
    (if (is-procedure? proc)
        (list-ref proc 3)
        (evaluator-error "Procedure expected" proc)))

(define root-env '())
(define my-procedure (make-procedure 'x (lambda (x) (print x)) root-env))
(print my-procedure)
(print (is-procedure? my-procedure))
(print (procedure-params my-procedure))
(print (procedure-body my-procedure))
(print (procedure-env my-procedure))