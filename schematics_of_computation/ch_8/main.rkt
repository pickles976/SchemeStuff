#!/usr/bin/racket
#lang r5rs

(module dbops
    (export make-table table-add-rows! table-select table-find table-update! table-project table-join table-for-each-row)
    (define (make-table column-specifications)
            (construct-table (make-table-layout column-specifications) '()))
    (define (table-add-rows! data)
        (for-each ; item in 'data'
            (lambda (item)
                (table-add-single-row! table (make-row table item)))
                data))
    (define (table-select table name predicate value)
        (construct-table (table-layout table)
            (select-rowpack
                (table-layout table) (table-rowpack table)
                name predicate value)))
    (define (table-find table name1 predicate value name2)
        (let ((answer
                (table-project
                    (table-select table name1 predicate value) ; select items from table
                    (list name2))) ; idk what this is
            (map car (table-rowpack answer)))))
    
    
)