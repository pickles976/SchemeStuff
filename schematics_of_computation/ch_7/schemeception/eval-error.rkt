#!/usr/bin/racket
#lang racket

(require compatibility/mlist)
(require pretty-format)


(provide 
    evaluator-error)

(define (evaluator-error what irritant)
    (display what irritant))

; (define (evaluator-error what irritant)
;    (pretty-printf #t "~%Error in self-eval: ~a:" what)
;    (if (not eqv? (void) irritant)
;        (display irritant))
;    (pretty-printf #t "~%...resetting to top level.~%")
;    (top-level))