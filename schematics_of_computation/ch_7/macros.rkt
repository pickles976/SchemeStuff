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
(newline)

(define-syntax-rule (increment! x)
    (+ x 1))


(define-syntax-rule (set-car! item val)
  (set! item (cons val (cdr item))))

(define-syntax-rule (set-cdr! item val)
  (set! item (cons (car item) val)))

; Test making cons mutable
(define my-list (cons 1 3))
(set-car! my-list 2)
(set-cdr! my-list 4)
(print my-list)