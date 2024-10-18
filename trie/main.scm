; Hash function for characters a-Z
(define hash
    (lambda (char) (- (char->integer (char-upcase char)) 65 )))

; Node constructor 
; ( list[False], True)
(define make-node   
    (lambda () (cons (make-vector 26 #f) #t)))

; Add a child node to an existing node, for a specified character
(define insert-char
    (lambda (char node)
        (let ((index (hash char)))
            (vector-set! (car node) index (make-node)))))

; Get the child node for the given char
(define get-node-from-char
    (lambda (char node)
        (vector-ref (car node) (hash char))))

; Method to insert a string
(define insert-string
    (lambda (string trie-node)
        (insert-string-i string trie-node 0)))

; Internal method
(define insert-string-i
    (lambda (string trie-node index)
        ; if string is empty
        (if (= index (string-length string))
            (begin
                (display "Inserted: ")
                (display string)
                (newline)
            )
            (if (get-node-from-char (string-ref string index) trie-node)
                ; If node is not false
                (begin
                    (display "Node is not terminal. \n")
                    (display (string-ref string index))
                    (newline)
                    (insert-string-i string (get-node-from-char (string-ref string index) trie-node) (+ index 1))
                )
                ; If node is false
                (begin
                    (display "Node is terminal. \n")
                    (display (string-ref string index))
                    (newline)
                    (insert-char (string-ref string index) trie-node)
                    (set-cdr! trie-node #f)
                    (insert-string-i string (get-node-from-char (string-ref string index) trie-node) (+ index 1))
                )
            )
        )
    )
)

(define my-trie (make-node))

(insert-string "HATS" my-trie)
(insert-string "HATCH" my-trie)
