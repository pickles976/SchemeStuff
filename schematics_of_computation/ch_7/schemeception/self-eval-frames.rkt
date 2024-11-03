#!/usr/bin/racket
#lang racket

(require compatibility/mlist)
(require "eval-error.rkt")

; pg 387

(provide 
    extend-environment
    find-binding
    get-binding
    set-binding
    def-binding
)

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
                            (mcons                      ; construct a mutable list like: ((name . value) (name . value) '()) (this is an env)   
                                (mcons (car n) (car v))  
                                (new-frame (cdr n) (cdr v)))
                            (evaluator-error 
                                "Invalid parameter list!" names))))))])
        (mcons (new-frame names values) env))) ; append our new env to the list of envs

; use assv to check all environments to find a variable
(define (find-binding symb env)
    (if (null? env)
        #f ; all environments traversed and no binding found
        (let ((x (massv symb (mcar env)))) ; see if there is an item in the env with the same name as 'symb'
            (if (not x)
                (find-binding symb (mcdr env)) ; recurse if not found
                x))))

; get the value of a variable
(define (get-binding symb env)
    (let ([x (find-binding symb env)])
        (if (not x )
            (evaluator-error "Unbound variable (get)" symb)
            (mcdr x))))

; set the value of a binding
(define (set-binding symb val env)
    (let ([x (find-binding symb env)])
        (if (not x)
            (evaluator-error "Unbound variable (set)" symb)
            (set-mcdr! x val))))

; 
(define (def-binding symb val env)
    (let ([x (massv symb (mcar env))])
        (if (not x)
            (set-mcar! env (mcons (mcons symb val) (mcar env)))
            (set-mcdr! x val))))

(define root-env (extend-environment (list 'x) (list 1) '())) ; Create a root env with binding x=1
(print root-env) 
(newline)

(define child-env (extend-environment (list 'y 'z) (list 2 3) root-env)) ; append a sub env with y=2 and z=3
(print child-env)
(newline)

(def-binding 'a 10 child-env) ; add a=10 to child env
(print child-env)
(newline)

(set-binding 'a 12 child-env) ; set a=12
(print child-env)
(newline)