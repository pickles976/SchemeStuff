#!/usr/bin/racket
#lang racket

(provide
    make-range
    reduce
    filter
)

(define (make-range start end)
  (if (> start end)
      '() ; Base case: return an empty list when start exceeds end
      (cons start (make-range (+ start 1) end)))) ; Recursive step

(define (reduce fn base-value lis)
   (if (null? lis)
       base-value 
       (fn (car lis) 
           (reduce fn base-value (cdr lis)))))

(define (filter pred lst)
  (cond
    ((null? lst) '()) ; If the list is empty, return an empty list
    ((pred (car lst)) ; If the predicate is true for the first element
     (cons (car lst) (filter pred (cdr lst)))) ; Include it in the result
    (else (filter pred (cdr lst))))) ; Otherwise, skip the element