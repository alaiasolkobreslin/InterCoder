#lang racket

(require brag/support)
(require "bnf.rkt")

(define a-parsed-value
    (parse (list (token 'LPAREN)
                 (token 'ID "x")
                 (token 'RPAREN))))

