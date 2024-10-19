#!/usr/bin/racket
#lang racket

(provide
    make-trie-node
    insert-string
    get-all-strings
    get-matches
)

(struct trie-node ([characters #:mutable] [is-terminal #:mutable]))

; Hash function for characters a-Z (converts lowercase to uppercase)
(define (hash char)
    (- (char->integer (char-upcase char)) 65 ))

(define (unhash num)
    (char-upcase (integer->char (+ num 65))))

; Node constructor 
(define (make-trie-node)
    (trie-node (make-vector 26 #f) #f))

; Add a child node to an existing node, for a specified character
(define (insert-node-for-char char node)
    (let ([index (hash char)])
        (vector-set! (trie-node-characters node) index (make-trie-node))))

; Get the child node for the given char
(define (get-node-from-char char node)
    (vector-ref (trie-node-characters node) (hash char)))

(define (node-exists-for-char? char node) 
    (not (boolean? (get-node-from-char char node))))

; Function to insert a string
(define (insert-string string node)
    (insert-string-i string node 0))

; Internal function
(define (insert-string-i string node index)
    (if (= index (string-length string))
        (begin ; string is empty
            (set-trie-node-is-terminal! node #t)
            (newline)
        )
        (begin
            (let ([char (string-ref string index)])
                (display char)
                (cond ; Insert node if it does not exist
                    [(not (node-exists-for-char? char node))
                        (insert-node-for-char char node)])
                (insert-string-i string (get-node-from-char char node) (+ index 1)) ; Recurse
            )
        )
    )
)

; Print all strings in trie
(define (get-all-strings node)
    (get-all-strings-i "" node))

; Internal function
(define (get-all-strings-i string node)
    (if (trie-node-is-terminal node)
        (list string) ; node is terminal, return list of string
        (flatten
            (filter (lambda (item) (not (void? item)))
                (map ; for all child nodes
                    (lambda (index)
                        (let ([char (unhash index)])
                            (cond 
                                [(node-exists-for-char? char node) 
                                (get-all-strings-i 
                                    (string-append string (make-string 1 char)) ; string + char
                                    (get-node-from-char char node))]) ; child node
                        )
                    )
                    (range 0 25)
                )
            )
        )
    )
)

; Check if string is in trie
(define (get-matches string node)
    (get-matches-i string node 0))

; Internal function
(define (get-matches-i string node index)
    (if (= index (string-length string))
        (begin ; string is empty
            (get-all-strings-i string node)
        )
        (begin
            (let ([char (string-ref string index)])
                (cond ; If no node exists, return empty
                    [(not (node-exists-for-char? char node)) '()])
                (get-matches-i string (get-node-from-char char node) (+ index 1))
            )
        )
    )
)
