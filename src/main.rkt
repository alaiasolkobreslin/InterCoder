#lang racket

(require json)

; Starter code for main function:
; want user to input specification json file, then edit this code to add
; a REPL once the parser gets finished
(let main ()
    (display "Please enter a json file: ")
    (define file (read-line (current-input-port) 'any))
    (define contents (call-with-input-file file read-json))
    (define keys (hash-keys contents))
    (define modu (hash-ref contents 'modulo))
    (define examples (hash-ref modu 'examples))
    
    ; Will eventually delete this code below
    (printf "input: ~a, length: ~a, last character: ~a\n"
        examples
        ; (define modu (hash-ref contents 'modulo))
        ; (define example (hash-ref modu 'examples))
        ; (list? examples)
        (string-length file)
        (char->integer (string-ref file (- (string-length file) 1))))
    (main))

