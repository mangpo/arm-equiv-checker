#lang s-exp rosette

(require "arm-machine.rkt" "arm-printer.rkt" "arm-parser.rkt"
         "arm-solver.rkt"
         "arm-simulator-rosette.rkt" 
         )

(current-bitwidth 32)
(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 5 4 4)) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

;; Input machine state
(define input-state (progstate (vector 0 0 0 0 0)
                               (vector 0 0 0 0)
                               0 2))

(define code
(send parser ast-from-string "
eor r4, r3, r3
str r0, r4, r4
mvn r3, r4
asr r2, r0, r3
str r2, r4, 4
cmpne r4, r0
smmulne r2, r3, r0
orrne r0, r3, 14
str r2, fp, -8
"))

(define encoded-code (send printer encode code))
(pretty-display "Print code")
(send printer print-syntax code)
(pretty-display "Print encoded code")
(send printer print-struct encoded-code)

;; ;; Section 3: Symbolic inputs
;; ;; Concrete program with symbolic inputs
(define output-state
  (send simulator-rosette interpret encoded-code input-state))
(pretty-display "Output from simulator in rosette.")
(send machine display-state output-state)
(newline)

;; ;; Section 3: Symbolic inputs
;; ;; Concrete program with symbolic inputs
(define (sym-input)
  (define-symbolic* in number?)
  in)
(define input-state-sym (default-state machine sym-input))

(pretty-display "Interpret concrete program with symbolic inputs...")
(define output-state-sym
  (send simulator-rosette interpret encoded-code input-state-sym))
(send machine display-state output-state-sym)


