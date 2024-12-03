(define (range start end)
   (if (= start end)
      (list start)
      (append (range start (- end 1)) (list end))))

; Hash function for characters a-Z
(define hash
    (lambda (char) (- (char->integer (char-upcase char)) 65 )))

(define unhash
    (lambda (num) (char-upcase (integer->char (+ num 65)))))

; Node constructor 
; ( list[Node | False], Boolean )
(define make-node   
    (lambda () (cons (make-vector 26 #f) #f)))

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
        (if (= index (string-length string))
            ; string is empty
            (begin
                (set-cdr! trie-node #t)
                (newline)
            )
            (begin
                (display (string-ref string index))
                ; Insert node if it does not exist
                (if (not (get-node-from-char (string-ref string index) trie-node))
                    (insert-char (string-ref string index) trie-node)
                )
                (insert-string-i string (get-node-from-char (string-ref string index) trie-node) (+ index 1))
            )
        )
    )
)

; Print all strings in trie
(define get-all-strings-in-trie
    (lambda (trie-node) (get-all-strings-i "" trie-node)))

; Helper
(define get-all-strings-i
    (lambda (string trie-node)
        (if (cdr trie-node)
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
)

; TODO: check if string is in trie
; TODO: return all strings that match string



(define my-trie (make-node))

(insert-string "HATS" my-trie)
(insert-string "HATCH" my-trie)
(get-all-strings-in-trie my-trie)
