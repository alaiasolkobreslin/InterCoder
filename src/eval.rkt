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
   (choose +  -  *)]              ; <bop>  := plus  | minus | times |
  [uop
   (choose -)])                   ; <uop>  := neg

(define (plus-interp n1 n2)
  (binexpr n1 n2 #:depth 1))

(define (minus-interp n1 n2)
  (binexpr n1 n2 #:depth 1))

(define (times-interp n1 n2)
  (binexpr n1 n2 #:depth 1))

(define (assert-bin-examples js impl x y)
  (define examples (hash-ref js 'examples))
  (map (lambda (ex)
    (let ()
    (define x-hash (hash-ref ex 'x))
    (define y-hash (hash-ref ex 'y)) 
    (define res (hash-ref ex 'res))
    (if (and (equal? x x-hash) (equal? y y-hash)) 
      (assert (equal? (impl x y) res))
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

(define-symbolic l h integer?)

; (define file (read-line "examples/normal.json" 'any))
(define contents (call-with-input-file "examples/normal.json" read-json))

(check-plus contents + 1 2)

(define sol
    (synthesize
     #:forall    (list l h)
     #:guarantee (and (check-plus contents plus-interp l h) 
     (check-minus contents minus-interp l h) 
     (check-times contents times-interp l h))))

sol
(print-forms sol)

; (define (eval e)
;   (match e
;     [(num i) i]
;     [(plus e1 e2) (evaluate (plus-interp (eval e1) (eval e2)) sol)]
;     [(minus e1 e2) (evaluate (minus-interp (eval e1) (eval e2)) sol)]))

; (eval (plus (minus (num 10)(num 2)) (num 2)))