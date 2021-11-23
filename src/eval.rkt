#lang rosette

(require rosette)
(require rosette/lib/synthax)
(require json)

(struct num (i))
(struct plus (e1 e2))
(struct minus (e1 e2))
(struct times (e1 e2))
(struct neg (e))

(define-grammar (binexpr x y)     ; Grammar of int expressions over two inputs:
  [expr
   (choose x y                    ; <expr> := x | y |
           ((bop) (expr) (expr))  ;           (<bop> <expr> <expr>) |
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [bop
   (choose +  -  * modulo)]       ; <bop>  := plus  | minus | times |
  [uop
   (choose -)])                   ; <uop>  := neg

(define-grammar (unaryexpr x)     ; Grammar of int expressions over one inputs:
  [expr
   (choose x                      ; <expr> := x | y |
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [uop
   (choose - !)])                 ; <uop>  := neg

(define (plus-interp depth n1 n2)
  (binexpr n1 n2 #:depth depth))

(define (minus-interp depth n1 n2)
  (binexpr n1 n2 #:depth depth))

(define (times-interp depth n1 n2)
  (binexpr n1 n2 #:depth depth))

(define (modulo-interp depth n1 n2)
  (binexpr n1 n2 #:depth depth))

(define (neg-interp depth n1)
  (unaryexpr n1 #:depth depth))

(define (assert-bin-examples js impl x y)
  (define examples (hash-ref js 'examples))
  (define depth (hash-ref js 'depth))
  (map (lambda (ex)
    (let ()
    (define x-hash (hash-ref ex 'x))
    (define y-hash (hash-ref ex 'y)) 
    (define res (hash-ref ex 'res))
    (if (and (equal? x x-hash) (equal? y y-hash)) 
      (assert (equal? (impl depth x y) res))
      (assert (equal? 1 1))))) examples))

(define (assert-unary-examples js impl x)
  (define examples (hash-ref js 'examples))
  (define depth (hash-ref js 'depth))
  (map (lambda (ex)
    (let ()
    (define x-hash (hash-ref ex 'x))
    (define res (hash-ref ex 'res))
    (if (equal? x x-hash) 
      (assert (equal? (impl depth x) res))
      (assert (equal? 1 1))))) examples))

(define (check-plus js impl x y)
  (define plus-examples (hash-ref js 'plus))
  (assert-bin-examples plus-examples impl x y))

(define (check-minus js impl x y)
  (define minus-examples (hash-ref js 'minus))
  (assert-bin-examples minus-examples impl x y))

(define (check-times js impl x y)
  (define minus-examples (hash-ref js 'times))
  (assert-bin-examples minus-examples impl x y))

(define (check-neg js impl x)
  (define neg-examples (hash-ref js 'neg))
  (assert-unary-examples neg-examples impl x))

(define (check-modulo js impl x y)
  (define modulo-examples (hash-ref js 'modulo))
  (assert-bin-examples modulo-examples impl x y))

(define-symbolic l h integer?)

; (define file (read-line "examples/normal.json" 'any))
(define contents (call-with-input-file "examples/normal.json" read-json))

(check-plus contents + 1 2)

(define sol
    (synthesize
     #:forall    (list l h)
     #:guarantee (and (check-plus contents plus-interp l h) 
     (check-minus contents minus-interp l h) 
     (check-times contents times-interp l h)
     (check-neg contents neg-interp l)
     (check-modulo contents modulo-interp l h))))

sol
(print-forms sol)

; (define (eval e)
;   (match e
;     [(num i) i]
;     [(plus e1 e2) (evaluate (plus-interp (eval e1) (eval e2)) sol)]
;     [(minus e1 e2) (evaluate (minus-interp (eval e1) (eval e2)) sol)]))

; (eval (plus (minus (num 10)(num 2)) (num 2)))