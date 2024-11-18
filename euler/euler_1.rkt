#!/usr/bin/racket
#lang racket

; https://projecteuler.net/problem=1

(require "helpers.rkt")

(define (is-multiple? factor number) 
    (equal? (modulo number factor) 0))

(define (is-any-multiple? factors number)
    (reduce 
        (lambda (a b) (or a b)) 
        #f 
        (map (lambda (x) (is-multiple? x number)) factors)))


(define range-list (make-range 1 999))

(display (reduce
            (lambda (a b) (+ a b))
            0 
            (filter (lambda (x) (is-any-multiple? '(3 5) x))  range-list)))
(newline)