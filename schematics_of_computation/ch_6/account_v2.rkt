#!/usr/bin/racket
#lang racket

; pg. 311

(require racket/class)
(require pretty-format)

(define account%
    (class object%
        (init-field balance name) ; Initialization args (they are mapped to fields of the same name)
        (define transactions '()) 
        (super-new) ; superclass initialization
        (define/public (get-balance) balance)
        (define/public (withdraw amt)
            (cond 
                [(> (- balance amt) 0)
                    (set! balance (- balance amt))
                    (set! transactions 
                        (cons
                            (cons 'withdraw amt) transactions))]
                [else
                    (pretty-printf "Overdrafts not permitted for account: ~s" name)
                ])
            balance
        )
        (define/public (deposit amt)
            (set! balance (+ balance amt))
            (set! transactions 
                (cons
                    (cons 'deposit amt) transactions))
            balance
        )
        (define/public (clear) (set! transactions '()))
        (define/public (process-transactions proc) ; Apply a procedure to all the transactions
            (for-each proc transactions))
    )
)

(define account (new account% [balance 10] [name "Seabass Checking"]))
(send account get-balance) ; 10
(send account withdraw 5) ; 5 ILLEGAL!
(send account withdraw 7) ; 5
(send account deposit 3) ; 8
(send account withdraw 7) ; 1

(send account process-transactions (lambda (x) (display x)))
