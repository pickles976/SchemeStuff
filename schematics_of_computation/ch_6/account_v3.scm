; pg. 315
; TODO: implement code from pg 315


(define account (new account% [balance 10] [name "Seabass Checking"]))
(send account get-balance) ; 10
(send account withdraw 5) ; 5 ILLEGAL!
(send account withdraw 7) ; 5
(send account deposit 3) ; 8
(send account withdraw 7) ; 1

(send account process-transactions (lambda (x) (display x)))
