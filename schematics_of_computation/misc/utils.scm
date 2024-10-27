; Definitions from "The Schematics of Computation"

(define filter
    (lambda (test lst)
        (if (null? lst)
            '() ; if null return immediately
            (if (test (car lst)) 
                (cons (car lst) (filter test (cdr lst))) ; return current term and recurse
                (filter test (cdr lst)))))) ; ignore current term

(define reduce
    (lambda (op base x)
        (if (null? x)
            base
            (op (car x) (reduce op base (cdr x))))))

; variadic args
(define (func . args)
    args
)

; variadic + required args
(define (average x . nums)
    (/ (apply + x nums)
       (+ 1 (length nums))
    )
  )