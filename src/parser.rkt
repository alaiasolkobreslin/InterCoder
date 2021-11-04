#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))

; adapted from https://gist.github.com/danking/1068185/b93e3bfc77ccfe58a35179c8502d834c0016adcf

(define-tokens a (NUM))
(define-empty-tokens b (+ - * EOF LPAREN RPAREN))
(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (re-? (re-or "-" "-")) (uinteger digit)
           (re-? (re-: "." (re-? (uinteger digit))))))))
(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))
(define-lex-abbrevs
  (digit10 (char-range "0" "9"))
  (number10 (number digit10)))

(define lisp-lexer
           (lexer
            ((re-+ number10) (token-NUM (string->number lexeme)))
            ("-" (token--))
            ("+" (token-+))
            ("*" (token-*))
            ("(" (token-LPAREN))
            (")" (token-RPAREN))
            ;; recursively calls the lexer which effectively skips whitespace
            (whitespace (lisp-lexer input-port))
            ((eof) (token-EOF))))

(define lisp-parser
           (parser
            (start exp)
            (end EOF)
            (error void)
            (tokens a b)
            (precs (left - +))
            (grammar
             (exp ((NUM) $1)
                  ((exp + exp) (+ $1 $3))
                  ((exp - exp) (- $1 $3))))))

(define (lex-this lexer input) (lambda () (lexer input)))

(let ((input (open-input-string "3 - 3.3 + 6")))
           (lisp-parser (lex-this lisp-lexer input)))