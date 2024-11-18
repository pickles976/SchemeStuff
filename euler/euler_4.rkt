#!/usr/bin/racket
#lang racket

; https://projecteuler.net/problem=4

(define bignum 998001)

; count down from bignum
; check if it is palindromic
; FALSE: goto 1
; TRUE: 
;   check 3-digit product:
;       FALSE: goto 1
;       TRUE: done!

; check palindromic
; convert to string
; return equals string (reversed string)

; check 3-digit product
;   args: n
;   i = 999
;   while i > 99
;       n = divide-recursive i
;       if n > 99:
;       
