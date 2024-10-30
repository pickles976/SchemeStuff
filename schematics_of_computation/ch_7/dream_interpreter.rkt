#!/usr/bin/racket
#lang racket

(provide 
    expr-constant?
    expr-variable?
    expr-op
    expr-args
    dream-expr-eval
)

(define no-val '*no-value*)

; Check if argument is a constant
(define (expr-constant? expr) 
    (or (number? expr) (boolean? expr) (string? expr)))

; Check if an argument is a symbol
(define (expr-variable? expr) (symbol? expr))

; An expression is just a list of the form (operator arg1 arg2 ...)
; This gets the operator
(define (expr-op expr) (car expr))

; This gets the operands
(define (expr-args expr) (cdr expr))

; Evaluates an expression
; expr -> (operator [expr]) | {constant} | {variable}
(define (dream-expr-eval expr)
    (cond 
        [(expr-constant? expr) expr] ; Return a constant
        [
            else  
            (cond 
                [(expr-variable? expr)
                (variable-value expr)] ; return variable value
                [else
                (let ([op (operator-procedure (expr-op expr))] ; Get operator
                    [operands (map dream-expr-eval (expr-args expr))]) ; map-recurse to get all args
                (op operands))]) ; apply operator to operands
        ]))

(define operator-name-list
    (list
        (cons 'plus
            (lambda (operands)
                (apply + operands)))
        (cons 'minus
            (lambda (operands)
                (apply - operands)))
        (cons 'times
            (lambda (operands)
                (apply * operands)))
        (cons 'divide
            (lambda (operands)
                (apply / operands)))
        (cons 'modulo
            (lambda (operands)
                (apply modulo operands)))
        (cons 'sqrt 
            (lambda (operands)
                (apply sqrt operands)))
        (cons 'negative 
            (lambda (operands)
                (apply - operands)))
        (cons 'floor 
            (lambda (operands)
                (apply floor operands)))
    )
)

; Takes the operator symbol and returns the operator function
; assv finds the first element in a list with the value, and returns it
; (assv 5 '((2 3) (5 7) (11 13)))         ⇒  (5 7)
(define (operator-procedure op-symbol)
    (let ([x (assv op-symbol operator-name-list)])
        (if (not x)
            (begin
                (error "Invalid operator!\n")
                (display x)
                (newline)
            )
            (cdr x))))


(define *variables* '())

(define (variable-clear)
    (set! *variables* '()))

; This puts a binding name in the global variable space
(define (variable-declare name)
    (let ([x (assv name *variables*)]) ; find the first item in *variables* with name "name"
        (if x ; if variable found
            (error 'variable-declare
                "Variable ~a already declared" name)
            (set! *variables* ; append a box with [name `*no value* to the list of variables]
                (cons 
                    (mcons name no-val)
                    *variables*)))))

; Sets the binding value equal to "value" in the global variable space
(define (variable-assign name value)
    (let ([x (assv name *variables*)])
        (if (not x)
            (error 'variable-assign "Variable ~a not declared" name)
            (set-mcdr! x value))))

; Read value stored at binding
(define (variable-value name)
    (let ([x (assv name *variables*)])
        (if (not x)
            no-val
            (cdr x))))

