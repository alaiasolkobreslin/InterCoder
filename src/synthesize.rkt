#lang rosette

(require rosette)
(require rosette/lib/synthax)     ; Require the sketching library.

(define int32? (bitvector 32))

(define-grammar (plus x y)        ; Grammar of int32 expressions over two inputs:
  [expr
   (choose x y (?? int32?)        ; <expr> := x | y | <32-bit integer constant> |
           ((bop) (expr) (expr))  ;           (<bop> <expr> <expr>) |
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [bop
   (choose (+)  (-)  (*))]        ; <bop>  := plus  | minus | times |
  [uop
   (choose (not))])               ; <uop>  := not

(define (plus-interp n1 n2)
  (plus n1 n2 #:depth 3))

(define (check-plus impl x y)
  (if (and (equal? x 1) (equal? y 2)) 
    (assert (equal? (impl x y) 3)) 
    (assert (equal? 1 1))))



; (define (int32 i)
;   (bv i int32?))

(check-plus + 1 2)

(define-symbolic l h integer?)

; (define sol
;     (synthesize
;      #:forall    (list l h)
;      #:guarantee (check-plus plus-interp l h)))

; sol

; (define cex (verify (check-plus + l h)))
; cex


(define cex (verify (check-plus + 1 2)))
cex
