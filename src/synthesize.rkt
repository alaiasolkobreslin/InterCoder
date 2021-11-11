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
  ; (assert (eq? (+ x y)(impl x y))))
  ; (assert #t))
  (if (and (equal? x 1) (equal? y 2)) 
    (assert (equal? (impl x y) 3)) 
    (assert (equal? 1 1))))

(define (int32 i)
  (bv i int32?))

; (check-plus + (int32 2) (int32 3))

(define-symbolic l h int32?)

; (define sol
;     (synthesize
;      #:forall    (list l h)
;      #:guarantee (check-plus plus-interp l h)))

; sol

(define cex (verify (check-plus + l h)))
;(define cex (verify (assert (eq? (int32 1) (int32 1)))))
; (define cex (verify (begin (assert (eq? (int32 1) (int32 1))))))
cex




; (define (bvmid lo hi)  ; (lo + hi) / 2
;   (bvsdiv (bvadd lo hi) (int32 2)))

; (define (check-mid impl lo hi)     ; Assuming that
;   (assume (bvsle (int32 0) lo))    ; 0 ≤ lo and
;   (assume (bvsle lo hi))           ; lo ≤ hi,
;   (define mi (impl lo hi))         ; and letting mi = impl(lo, hi) and
;   (define diff                     ; diff = (hi - mi) - (mi - lo),
;     (bvsub (bvsub hi mi)
;            (bvsub mi lo)))         ; we require that
;   (assert (bvsle lo mi))           ; lo ≤ mi,
;   (assert (bvsle mi hi))           ; mi ≤ hi,
;   (assert (bvsle (int32 0) diff))  ; 0 ≤ diff, and
;   (assert (bvsle diff (int32 1)))) ; diff ≤ 1.

; (define cexx (verify (check-mid bvmid l h)))
; cexx