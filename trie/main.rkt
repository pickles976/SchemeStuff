#!/usr/bin/racket
#lang racket

(require "trie.rkt")

(define my-trie (make-trie-node))

(insert-string "HATS" my-trie)
(insert-string "HATCH" my-trie)
(insert-string "POTS" my-trie)
(insert-string "PLANT" my-trie)
(insert-string "PLANET" my-trie)
(get-all-strings my-trie)
(get-matches "HAT" my-trie)
(get-matches "PLA" my-trie)