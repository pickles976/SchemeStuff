#!/usr/bin/racket
#lang racket

; https://projecteuler.net/problem=3
; I was too smooth-brained for this one, I copied the solution from Python:
; https://stackoverflow.com/questions/14138053/project-euler-3-with-python-most-efficient-method


; Divides a number by factor until cannot be further divided
(define (divide-recursive dividend divisor)
    (if (equal? (modulo dividend divisor) 0) ; is divisible?
        (divide-recursive (/ dividend divisor) divisor) ; divide and recurse
        dividend)) ; return

; recursion helper
(define (prime-helper number i)
    (if (< (* i i) number) 
        (prime-helper (divide-recursive number i) (+ i 1)) ; divide number and increment 1
        number)) ; we did it Joe!

(define (greatest-prime-factor number)
    (prime-helper number 2))

(define bignum 600851475143)

(display (greatest-prime-factor bignum))
(newline)
