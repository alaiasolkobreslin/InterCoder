#lang rosette

(require rosette)
(require rosette/lib/synthax) 

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
   (choose +  -  *)]              ; <bop>  := plus  | minus | times |
  [uop
   (choose -)])                   ; <uop>  := neg

(define (plus-interp n1 n2)
  (binexpr n1 n2 #:depth 1))

(define (minus-interp n1 n2)
  (binexpr n1 n2 #:depth 1))

(define (check-plus impl x y)
  (if (and (equal? x 1) (equal? y 2)) 
    (assert (equal? (impl x y) -1))
    (if (and (equal? x 4) (equal? y 1))
      (assert (equal? (impl x y) 3))
      (assert (equal? 1 1)))))

(define (check-minus impl x y)
  (if (and (equal? x 1) (equal? y 2)) 
    (assert (equal? (impl x y) 3))
    (if (and (equal? x 4) (equal? y 1))
      (assert (equal? (impl x y) 5))
      (assert (equal? 1 1)))))

(define-symbolic l h integer?)

(define sol
    (synthesize
     #:forall    (list l h)
     #:guarantee (and (check-plus plus-interp l h) (check-minus minus-interp l h))))

sol
(print-forms sol)

(define (eval e)
  (match e
    [(num i) i]
    [(plus e1 e2) (evaluate (plus-interp (eval e1) (eval e2)) sol)]
    [(minus e1 e2) (evaluate (minus-interp (eval e1) (eval e2)) sol)]))

(eval (plus (minus (num 10)(num 2)) (num 2)))