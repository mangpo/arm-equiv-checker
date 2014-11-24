#lang s-exp rosette

(require "arm-ast.rkt" "arm-machine.rkt" "arm-simulator-rosette.rkt")
(provide arm-solver%)

(define arm-solver%
  (class object%
    (super-new)
    (init-field machine printer)
    (public counterexample)

    (define simulator (new arm-simulator-rosette% [machine machine]))
    (define bit (get-field bit machine))

    (define (sym-input)
      (define-symbolic* input number?)
      input
      )

    ;; Used for generate input and counterexample.
    (define (evaluate-state state sol)
      (define-syntax-rule (eval x model)
        (let ([ans (evaluate x model)])
          (if (term? ans) 0 ans)))

      (define regs (vector-copy (progstate-regs state)))
      (define memory (vector-copy (progstate-memory state)))
      (define z (eval (progstate-z state) sol))
      (define fp (progstate-fp state))
      
      (for ([i (vector-length regs)]
            [reg regs])
           (vector-set! regs i (eval reg sol)))
      
      (for ([i (vector-length memory)]
            [mem memory])
           (vector-set! memory i (eval mem sol)))

      (progstate regs memory z fp))

    (define (assert-output state1 state2 constraint)
      (when debug (pretty-display "start assert-output"))
      (define regs (progstate-regs constraint))
      (define memory (progstate-memory constraint))
      (define z (progstate-z constraint))

      (define regs1 (progstate-regs state1))
      (define memory1 (progstate-memory state1))
      (define z1 (progstate-z state1))

      (define regs2 (progstate-regs state2))
      (define memory2 (progstate-memory state2))
      (define z2 (progstate-z state2))
      
      (for ([r regs]
            [r1 regs1]
            [r2 regs2])
           (when r (assert (equal? r1 r2))))

      (for ([m memory]
            [m1 memory1]
            [m2 memory2])
           (when m (assert (equal? m1 m2))))

      (when z (assert (equal? z1 z2)))

      (when debug (pretty-display "end assert-output"))
      )

    (define (counterexample spec program constraint)
      (when debug 
            (pretty-display (format "program-eq? START bit = ~a" bit))
            (pretty-display "spec:")
            (print-struct spec)
            (pretty-display "program:")
            (print-struct program)
            ;; (pretty-display "constraint:")
            ;; (display-state constraint)
            )
      
      (clear-asserts)
      (current-bitwidth bit)
      (define start-state (send machine get-state sym-input))
      (define spec-state #f)
      (define program-state #f)
      
      (define (interpret-spec!)
        ;;(pretty-display ">>> interpret spec")
        (set! spec-state (send simulator interpret spec start-state))
        ;;(pretty-display ">>> done interpret spec")
        )
      
      (define (compare)
        ;;(pretty-display ">>> interpret program")
        (set! program-state (send simulator interpret program start-state))
        ;;(pretty-display ">>> done interpret program")
        
        ;; (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
        ;; (display-state spec-state)
        ;; (pretty-display ">>>>>>>>>>> PROG >>>>>>>>>>>>>")
        ;; (display-state program-state)
        
        (assert-output spec-state program-state constraint)
        ;;(pretty-display "done check output")
        )

      (with-handlers* 
       ([exn:fail? 
         (lambda (e)
           (when debug (pretty-display "program-eq? SAME"))
           (clear-terms!)
           (if (equal? (exn-message e) "verify: no counterexample found")
               #f
               (raise e)))])
       (let ([model (verify #:assume (interpret-spec!) #:guarantee (compare))])
         (when debug (pretty-display "program-eq? DIFF"))
         (let ([state (evaluate-state start-state model)])
           (clear-terms!)
           state)
         )))

    ))
