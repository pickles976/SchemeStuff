#!/usr/bin/racket
#lang racket

(require compatibility/mlist)

(require "self-eval-procedure.rkt")
(require "self-eval-frames.rkt")
(require "eval-error.rkt")

; pg 390

(provide 
    is-primitive?
    primitive-procedure
    make-primitive-environment)

(define is-primitive? procedure?)

(define (primitive-procedure) (lambda (x) x))

(define (make-primitive-environment)
    (lambda () 
        (let ([pf (extend-environment '() '() '())])
            (let ([def (lambda (name proc) (def-binding name proc pf))])
                ; mafs
                (def '+ (lambda (x) (+ (car x) (car (cdr x)))))  
                (def '- (lambda (x) (- (car x) (car (cdr x))))) 
                (def '* (lambda (x) (* (car x) (car (cdr x))))) 
                (def '/ (lambda (x) (/ (car x) (car (cdr x))))) 
                (def '< (lambda (x) (< (car x) (car (cdr x))))) 
                (def '= (lambda (x) (= (car x) (car (cdr x))))) 
                (def '> (lambda (x) (> (car x) (car (cdr x))))) 
                (def 'pi 3.1415926) 

                ; lists
                (def 'car (lambda (x) (car (car x)))) 
                (def 'cdr (lambda (x) (cdr (car x)))) 
                (def 'cons (lambda (x) (cons (car x) (car (cdr x)))))
                (def 'null? (lambda (x) (null? (car x))))  
                (def 'eqv? (lambda (x) (eqv? (car x) (car (cdr x))))) 

                ; printing
                (def 'display (lambda (x) (print (car x)))) 
                (def 'newline (lambda (x) (newline))) 
            )
        pf)))
