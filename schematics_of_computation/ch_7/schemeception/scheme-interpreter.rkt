#!/usr/bin/racket
#lang racket

(require compatibility/mlist)

(require "self-eval-procedure.rkt")
(require "self-eval-frames.rkt")
(require "self-eval-primitives.rkt")

(define (self-eval exp env)
    (simple-form exp env))

(define (constant? exp)
    (or (number? exp) (boolean? exp) (string? exp)))

(define (simple-form exp env)
    (cond
        [(constant? exp) exp] ; if expr is constant, return that constant
        [(symbol? exp) (get-binding exp env)] ; if expr is a symbol, get the value of the binding
        [(pair? exp)  ; if expr is a pair
            (let ([b (massv (car exp) special-forms)]) ; check if b is in special forms
                (if b 
                    (cdr b) self-application) ; if b is a special form, apply procedure
                exp env)] ; perform application
        [else (evaluator-error "Incorrect expression" exp)]))

(define (self-application exp env)
    (let*))