#lang rosette

(require rosette)
(require rosette/lib/synthax)     ; Require the sketching library.

; (define int32? (bitvector 32))

(define-grammar (plus x y)        ; Grammar of int expressions over two inputs:
  [expr
   (choose x y (?? integer?)      ; <expr> := x | y | <integer constant> |
           ((bop) (expr) (expr))  ;           (<bop> <expr> <expr>) |
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [bop
   (choose +  -  *)]        ; <bop>  := plus  | minus | times |
  [uop
   (choose -)])                 ; <uop>  := neg

(define (plus-interp n1 n2)
  (plus n1 n2 #:depth 2))

(define (check-plus impl x y)
  (if (and (equal? x 1) (equal? y 2)) 
    (assert (equal? (impl x y) 3))
    (if (and (equal? x 4) (equal? y 1))
      (assert (equal? (impl x y) 5))
      (assert (equal? 1 1)))))

(define-symbolic l h integer?)

(define cex (verify (check-plus + l h)))
cex

(define sol
    (synthesize
     #:forall    (list l h)
     #:guarantee (check-plus plus-interp l h)))

sol
(print-forms sol)