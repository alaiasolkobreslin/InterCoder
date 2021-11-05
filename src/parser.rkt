#lang racket

(require br-parser-tools/lex)
(require brag/support)
(require "bnf.rkt")

(define (tokenize ip)
    (port-count-lines! ip)
    (define my-lexer
      (lexer-src-pos
       [(repetition 1 +inf.0 numeric)
        (token 'NUM (string->number lexeme))]
       ["lambda"
        (token 'LAMBDA "lambda")]
       ["+"
        (token 'PLUS "+")]
       ["-"
        (token 'MINUS "-")]
       ["*"
        (token 'TIMES "*")]
       ["!"
        (token 'NOT "!")]                  
       ["("
        (token 'LPAREN "(")]
       [")"
        (token 'RPAREN ")")]  
       [whitespace
        (token 'WHITESPACE lexeme #:skip? #t)]
      ;  [any-string
      ;   (token 'ID lexeme)]
       [(eof)
        (void)]))
    (define (next-token) (my-lexer ip))
    next-token)

(define a-sample-input-port (open-input-string "(+ 3 2)"))
(define token-thunk (tokenize a-sample-input-port))
(define another-stx (parse token-thunk))
(syntax->datum another-stx)