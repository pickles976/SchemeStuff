#!/usr/bin/racket
#lang racket

; https://projecteuler.net/problem=2

(require "helpers.rkt")

(define (is-even? x)
    (equal? (modulo x 2) 0))

; Return a list of all fib numbers below some limit
(define (fib-helper prev curr limit)
    (if (> curr limit)
        '()
        (cons curr (fib-helper curr (+ prev curr) limit)))) ; build list recursively

(define (fib limit)
    (fib-helper 1 1 limit))

; get a list of all fibonnaci numbers up to some limit
(define fib-list (fib 4000000))
(display fib-list)
(newline)

(display 
    (reduce ; sum all list
        (lambda (a b) (+ a b)) 
        0
        (filter ; filter even items in fib list
            (lambda (x) (is-even? x)) 
                fib-list)))
(newline)