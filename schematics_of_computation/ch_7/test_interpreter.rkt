#!/usr/bin/racket
#lang racket

(require test-engine/racket-tests)
(require "dream_interpreter.rkt")

(check-expect (expr-constant? "test") #t)
(check-expect (expr-constant? 1) #t)
(check-expect (expr-constant? #f) #t)
(check-expect (expr-constant? 'symbol) #f)

(dream-expr-eval '(times 2 (times 3 5)))
; (dream-eval '(program (print (plus 2 2))))