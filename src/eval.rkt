#lang rosette

(require rosette)
(require rosette/lib/synthax)
(require json)

(struct num (i))
(struct bool (b))
(struct conj (b1 b2))
(struct disj (b1 b2))
(struct naur (b))
(struct plus (e1 e2))
(struct minus (e1 e2))
(struct times (e1 e2))
(struct modu (e1 e2))
(struct neg (e))
(struct tern (e1 e2 e3))


(define-grammar (intunaryexpr x)     ; Grammar of int expressions over one inputs:
  [expr
   (choose x                      ; <expr> := x | y |
          ;;;  ((thrup) (expr) (expr) (expr))
           ((bop) (expr) (expr))
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [uop
   (choose - )]                 ; <uop>  := neg
  [bop
   (choose +  -  * modulo)])      ; <bop>  := plus  | minus | times |
  ;;; [thrup 
  ;;;   (choose + - *)])

(define-grammar (intbinexpr x y)     ; Grammar of int expressions over two inputs:
  [expr
   (choose x y                    ; <expr> := x | y |
          ;;;  ((thrup) (expr) (expr) (expr))
           ((bop) (expr) (expr))  ;           (<bop> <expr> <expr>) |
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [uop
   (choose - )]                   ; <uop>  := neg
  [bop
   (choose +  -  * modulo)])      ; <bop>  := plus  | minus | times |
  ;;; [thrup 
  ;;;   (choose + - *)])

(define-grammar (boolunaryexpr x)     ; Grammar of int expressions over one inputs:
  [expr
   (choose x                      ; <expr> := x | y |
          ;;;  ((thrup) (expr) (expr) (expr))
           ((bop) (expr) (expr))
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [uop
   (choose !)]                 ; <uop>  := neg
  [bop
   (choose (and) (or))])      ; <bop>  := plus  | minus | times |
  ;;; [thrup 
  ;;;   (choose + - *)])

(define-grammar (boolbinexpr x y)     ; Grammar of int expressions over two inputs:
  [expr
   (choose x y                    ; <expr> := x | y |
          ;;;  ((thrup) (expr) (expr) (expr))
           ((bop) (expr) (expr))  ;           (<bop> <expr> <expr>) |
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [uop
   (choose !)]                   ; <uop>  := neg
  [bop
   (choose (and) (or))])      ; <bop>  := plus  | minus | times |
  ;;; [thrup 
  ;;;   (choose + - *)])



;;; (define-grammar (triexpr x y z)     ; Grammar of int expressions over two inputs:
;;;   [expr
;;;    (choose x y                 ; <expr> := x | y |
;;;            ((thrup) (expr) (expr) (expr))
;;;            ((bop) (expr) (expr))  ;           (<bop> <expr> <expr>) |
;;;            ((uop) (expr)))]   ;           (<uop> <expr>)
;;;   [thrup 
;;;     (choose + - *)]
;;;   [bop
;;;    (choose +  -  *)]              ; <bop>  := plus  | minus | times |
;;;   [uop
;;;    (choose -)]) 

(define (plus-interp depth n1 n2)
  (intbinexpr n1 n2 #:depth depth))

(define (minus-interp depth n1 n2)
  (intbinexpr n1 n2 #:depth depth))

(define (times-interp depth n1 n2)
  (intbinexpr n1 n2 #:depth depth))

(define (modulo-interp depth n1 n2)
  (intbinexpr n1 n2 #:depth depth))

(define (neg-interp depth n1)
  (intunaryexpr n1 #:depth depth))

(define (and-interp depth n1 n2)
  (boolbinexpr n1 n2 #:depth depth))

(define (or-interp depth n1 n2)
  (boolbinexpr n1 n2 #:depth depth))

(define (naur-interp depth n1)
  (boolunaryexpr n1 #:depth depth))

(define (assert-bin-examples js impl x y)
  (define examples (hash-ref js 'examples))
  (define depth (hash-ref js 'depth))
  (map (lambda (ex)
    (let ()
    (define x-hash (hash-ref ex 'x))
    (define x-hash_ (if (or (equal? x-hash "true") (equal? x-hash "false")) (equal? x-hash "true") x-hash))
    (define y-hash (hash-ref ex 'y))
    (define y-hash_ (if (or (equal? y-hash "true") (equal? y-hash "false")) (equal? y-hash "true") y-hash))
    (define res (hash-ref ex 'res))
    (define res_ (if (or (equal? res "true") (equal? res "false")) (equal? res "true") res))
    (if (and (equal? x x-hash_) (equal? y y-hash_)) 
      (assert (equal? (impl depth x y) res_))
      (assert (equal? 1 1))))) examples))

(define (assert-unary-examples js impl x)
  (define examples (hash-ref js 'examples))
  (define depth (hash-ref js 'depth))
  (map (lambda (ex)
    (let ()
    (define x-hash (hash-ref ex 'x))
    (define x-hash_ (if (or (equal? x-hash "true") (equal? x-hash "false")) (equal? x-hash "true") x-hash))
    (define res (hash-ref ex 'res))
    (define res_ (if (or (equal? res "true") (equal? res "false")) (equal? res "true") res))

    (if (equal? x x-hash_) 
      (assert (equal? (impl depth x) res_))
      (assert (equal? 1 1))))) examples))


(define (check-plus js impl x y)
  (define plus-examples (hash-ref js 'plus))
  (assert-bin-examples plus-examples impl x y))

(define (check-naur js impl x)
  (define naur-examples (hash-ref js 'naur))
  (assert-unary-examples naur-examples impl x))

;;; (define (check-and js impl x y)
;;;   (define and-examples (hash-ref js 'and))
;;;   (assert-bin-examples and-examples impl x y))

;;; (define (check-or js impl x y)
;;;   (define or-examples (hash-ref js 'or))
;;;   (assert-bin-examples or-examples impl x y))

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
(define-symbolic a b boolean?)

; (define file (read-line "examples/normal.json" 'any))
(define contents (call-with-input-file "examples/normal.json" read-json))

(check-plus contents + 1 2)

(define sol
    (synthesize
     #:forall    (list l h a)
     #:guarantee (and 
      (check-plus contents plus-interp l h) 
      (check-minus contents minus-interp l h) 
      (check-times contents times-interp l h)
      (check-neg contents neg-interp l)
      (check-naur contents naur-interp a)
      (check-modulo contents modulo-interp l h))))
      ;;; (check-and contents and-interp a b)
      ;;; (check-or contents or-interp a b)

sol
(print-forms sol)

; calling interp with depth = 1 is a placeholder for now
(define (eval e)
  (match e
    [(num i) i]
    [(bool b) b]
    [(conj b1 b2) (evaluate (or-interp 1 (eval b1) (eval b2)) sol)]
    [(disj b1 b2) (evaluate (and-interp 1 (eval b1) (eval b2)) sol)]
    [(naur b) (evaluate (naur-interp 1 (eval b)) sol)]
    [(neg e) (evaluate (neg-interp 1 (eval e)) sol)]

    [(plus e1 e2) (evaluate (plus-interp 1 (eval e1) (eval e2)) sol)]
    [(minus e1 e2) (evaluate (minus-interp 1 (eval e1) (eval e2)) sol)]
    [(times e1 e2) (evaluate (times-interp 1 (eval e1) (eval e2)) sol)]
    [(modu e1 e2) (evaluate (modulo-interp 1 (eval e1) (eval e2)) sol)]))

(define expr (plus (minus (num 10) (num 2)) (num 2)))
(eval expr)