# Running Scheme

Running scheme with Racket interpreter

```commandline
plt-r5rs ./main.scm
```

```
        (if (= index (string-length string))
            string
            (if (get-node-from-char ((string-ref string index) trie-node))
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
```