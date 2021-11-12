#lang racket

(require br-parser-tools/lex)
(require brag/support)
(require "bnf.rkt")

;;; (struct 
(struct expr (a b c d e))


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

;;; (define (simplify1 stx)
;;;     (match stx
;;;         ;;; [(ID) ID]
;;;         ;;; [(expr "(" e ")") e]
;;;         ;;; [(expr _) 3]
;;;         [(expr a x y z d) 2]
;;;         ;;; [(e) (e)]
;;;         ;;; [(LPAREN PLUS expr expr RPAREN) (PLUS expr expr)]
;;;         ;;; [(LPAREN MINUS expr expr RPAREN) (MINUS expr expr)]
;;;         ;;; [(LPAREN TIMES expr expr RPAREN) (TIMES expr expr)]
;;;         ;;; [(LPAREN NOT expr RPAREN) (NOT expr)]
;;;         ;;; [(LPAREN MINUS expr RPAREN) (MINUS expr)]
;;;         ;;; [(LPAREN expr expr RPAREN) (APP expr expr)]
;;;         ;;; [(LPAREN LAMBDA LPAREN ID* RPAREN expr RPAREN) ]
;;;         ;;; [(LPAREN expr expr RPAREN) ]

;;;    )
;;; )

(define-syntax (simplify2 stx)
    (syntax-case stx ()
        ;;; [(ID) ID]
        [(_ (expr _ inner_exp _)) #'(inner_exp)]
        ;;; [(LPAREN PLUS expr expr RPAREN) (PLUS expr expr)]
        ;;; [(LPAREN MINUS expr expr RPAREN) (MINUS expr expr)]
        ;;; [(LPAREN TIMES expr expr RPAREN) (TIMES expr expr)]
        ;;; [(LPAREN NOT expr RPAREN) (NOT expr)]
        ;;; [(LPAREN MINUS expr RPAREN) (MINUS expr)]
        ;;; [(LPAREN expr expr RPAREN) (APP expr expr)]
        ;;; [(LPAREN LAMBDA LPAREN ID* RPAREN expr RPAREN) ]
        ;;; [(LPAREN expr expr RPAREN) ]

    )
)


;;; (simplify1 (syntax->datum (parse (tokenize (open-input-string "(+ 3 2)")))))
(syntax->datum (simplify1 (parse (tokenize (open-input-string "(+ 3 2)")))))
 