#!/usr/bin/racket
#lang racket

; pg 391
; I don't really fully understand what is happening here

(require compatibility/mlist)

(require "self-eval-procedure.rkt")
(require "self-eval-frames.rkt")
(require "self-eval-primitives.rkt")
(require "eval-error.rkt")

(define (self-eval exp env)
    (simple-form exp env))

(define (constant? exp)
    (or (number? exp) (boolean? exp) (string? exp)))

; must be constant, variable, or pair
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
    (let*
        ([eval-arg (lambda (arg) (self-eval arg env))]
        [evaluated-args (map eval-arg exp)] ; recursively evaluate all args
        [operator (car evaluated-args)] ; the the operator from the list
        [operands (cdr evaluated-args)]) ; get the operants from the list of evaluated args
        (cond
            [(is-primitive? operator) ; call the primitive's procedure
                ((primitive-procedure operator) operands)]
            [(is-procedure? operator) ; recursively call procedure in exyended environment
                (self-sequence
                    (procedure-body operator)
                    (extend-environment
                        (procedure-params operator)
                        operands
                        (procedure-env operator)))]
            [else  
                (evaluator-error "Procedure required" exp)])))

(define (self-sequence exp env)
    (letrec
        ([seq (lambda (val exp env)
            (if (null? exp)
                val
                (seq (self-eval (car exp) env) (cdr exp) env)))])
        (seq unbound exp env)))

(define special-forms '())

(define (define-special symb proc)
    (let ([x (assv symb special-forms)])
        (if x
            (set-cdr! x proc)
            (set! special-forms
                (cons (cons symb proc) special-forms)))))

(define (define-special-forms)
    (lambda ()
        (define-special 'quote
            (lambda (exp env) (get-arg exp 1)))
        (define-special 'lambda
            (lambda (exp env)
                (make-procedure
                    (get-arg exp 1) (get-rest exp 2) env)))
        (define-special 'set!
            (lambda (exp env)
                (set-binding (get-arg exp 1)
                    (self-eval (get-arg exp 2) env) env)))
        (define-special 'if
            (lambda (exp env)
                (if (self-eval (get-arg exp 1) env)
                    (self-eval (get-arg exp 2) env)
                    (self-eval (get-arg exp 3) env))))
        (define-special 'begin
            (lambda (exp env)
                (self-sequence (get-rest exp 1) env)))
        (define-special 'define
            (lambda (exp env)
                (def-binding (get-arg exp 1)
                    (self-eval (get-arg exp 2) env) env))
                (void))
        (define-special 'define-macro
            (lambda (exp env)
                (def-macro (get-arg exp 1)
                    (self-eval (get-arg exp 2) env) env))
                (void))
    )
)

(define (def-macro mac-name expander)
    (define-special mac-name    
        (lambda (exp env)
            (self-eval 
                (self-sequence  
                    (procedure-body expander)
                    (extend-environment (procedure-params expander)
                        (list exp) (procedure-env expander)))
                    env))))


(self-eval (+ 1 2))