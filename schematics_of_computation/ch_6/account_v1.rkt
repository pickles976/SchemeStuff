#!/usr/bin/racket
#lang racket

; Implementation of account from pg 309 of Schematics of Computation

(require racket/class)
(require pretty-format)

(define account%
    (class object%
        (init-field balance name) ; Initialization args (they are mapped to fields of the same name)
        ; (init account-balance account-name) EXAMPLE ALTERNATIVE
        ; (define balance account-balance)
        ; (define name account-name)
        (super-new) ; superclass initialization
        (define/public (get-balance) balance)
        (define/public (withdraw amt)
            (cond 
                [(> (- balance amt) 0)
                    (set! balance (- balance amt))]
                [else
                    (pretty-printf "Overdrafts not permitted for account: ~s\n" name)
                ])
            balance
        )
        (define/public (deposit amt)
            (set! balance (+ balance amt))
            balance
        )
    )
)

(define account (new account% [balance 10] [name "Seabass Checking"]))
(send account get-balance) ; 10
(send account withdraw 5) ; 5 ILLEGAL!
(send account withdraw 7) ; 5
(send account deposit 3) ; 8
(send account withdraw 7) ; 1
