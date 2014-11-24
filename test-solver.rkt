#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 12 4 4))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

(define code
(send parser ast-from-string "
        mov     r0, r2, asl #2
        rsb     r5, r0, r2
        mul     r2, r6, r2
        mov     r5, r5, asl #11
        sub     r5, r5, #12
        rsb     r3, r2, r3
        mla     r3, r5, r10, r3
        mla     r1, r11, r1, r3
"))


(define sketch
(send parser ast-from-string "
sub r5, r2, r2, lsl 2
mul r2, r6, r2
mov r5, r5, lsl 11
sub r5, r5, 12
rsb r3, r2, r3
mla r3, r5, r10, r3
mla r1, r11, r1, r3
")) 

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))

;; Return counterexample if code and sketch are different.
;; Otherwise, return #f.
(define ex 
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 1] [mem])))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
