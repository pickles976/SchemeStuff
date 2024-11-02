#!/usr/bin/racket
#lang racket

; pg 382

(provide 
    extend-environment
    get-binding
    set-binding
    def-binding)

; Extend the environment by creating a new frame, with names bound to values, whose parent is env.
(define (extend-environment names values env)
    (letrec 
        ([new-frame
            (lambda (n v)
                (if (and (null? n) (null? v)) ; no args
                '()
                (if (null? n) ; values, but no args
                    (evaluator-error
                        "Too many arguments!" (cons names values))
                    (if (null? v)
                        (evaluator-error 
                            "Too few arguments!" (cons names values))
                        (if (and (pair? n) (symbol? (car n))) ; if n is a list, and the first element is a symbol
                            (cons                      ; construct a list like: ((name . value) (name . value) '()) (this is an env)   
                                (cons (car n) (car v)) 
                                (new-frame (cdr n) (cdr v)))
                            (evaluator-error 
                                "Invalid parameter list!" names))))))])
        (cons (new-frame names values) env))) ; append our new env to the list of envs

; use assv to check all environments to find a variable
(define (find-binding symb env)
    (if (null? env)
        #f ; all environments traversed and no binding found
        (let ((x (assv symb (car env)))) ; see if there is an item in the env with the same name as 'symb'
            (if (not x)
                (find-binding symb (cdr env)) ; recurse if not found
                x))))

; 