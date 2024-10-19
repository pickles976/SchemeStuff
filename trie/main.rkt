#!/usr/bin/racket
#lang racket

; Hash function for characters a-Z
(define (hash char)
    (- (char->integer (char-upcase char)) 65 ))

(define (unhash num)
    (char-upcase (integer->char (+ num 65))))

; Node constructor 
; ( list[Node | False], Boolean )
(define (make-node   )
    (mcons (make-vector 26 #f) #f))

; Add a child node to an existing node, for a specified character
(define (insert-char char node)
    (let ([index (hash char)])
        (vector-set! (mcar node) index (make-node))))

; Get the child node for the given char
(define (get-node-from-char char node)
    (vector-ref (mcar node) (hash char)))

; Method to insert a string
(define (insert-string string trie-node)
    (insert-string-i string trie-node 0))

; Internal method
(define (insert-string-i string trie-node index)
    (if (= index (string-length string))
        ; string is empty
        (begin
            (set-mcdr! trie-node #t)
            (newline)
        )
        (begin
            (display (string-ref string index))
            ; Insert node if it does not exist
            (cond 
                [(not (get-node-from-char (string-ref string index) trie-node))
                    (insert-char (string-ref string index) trie-node)])
            (insert-string-i string (get-node-from-char (string-ref string index) trie-node) (+ index 1))
        )
    )
)

; Print all strings in trie
(define (get-all-strings-in-trie trie-node)
    (get-all-strings-i "" trie-node))

; Helper
(define (get-all-strings-i string trie-node)
    (if (mcdr trie-node)
        string ; node is terminal, return list of string
            (map ; for all child nodes
                (lambda (index)
                    (let ((child-node (get-node-from-char (unhash index) trie-node)))
                        (if child-node
                            (get-all-strings-i 
                                (string-append string (make-string 1 (unhash index))) ; string + char
                                child-node
                            )
                            ""
                        )
                    )
                )
                (range 0 25)
            )
    )
)

; TODO: check if string is in trie
; TODO: return all strings that match string



(define my-trie (make-node))

(insert-string "HATS" my-trie)
(insert-string "HATCH" my-trie)
(get-all-strings-in-trie my-trie)
