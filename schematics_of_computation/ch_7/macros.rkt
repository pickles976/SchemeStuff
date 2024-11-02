#!/usr/bin/racket
#lang racket

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define first 1)
(define last 2)

(print first)
(newline)
(swap first last)
(print first)

(define-syntax-rule (increment! x)
    (+ x 1))