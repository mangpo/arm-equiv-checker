#lang racket

(require "arm-ast.rkt")

(provide arm-machine% (all-defined-out))

(struct progstate (regs memory z fp))
(struct progstate+ progstate (extra))

(define debug #f)

(define-syntax-rule (build-vector n init)
  (let ([vec (make-vector n)])
    (for ([i (in-range n)])
	 (vector-set! vec i (init)))
    vec))

;; Macros to create init state for testing
(define-syntax default-state
  (syntax-rules (reg mem set-z set-fp)
    ((default-state machine init)
     (progstate (build-vector (send machine get-nregs) init) 
                (build-vector (send machine get-nmems) init)
		(init)
		(send machine get-fp)))

    ((default-state machine init [set-z vz] [set-fp vfp])
     (progstate (build-vector (send machine get-nregs) init) 
                (build-vector (send machine get-nmems) init)
		vz
		vfp))

    ((default-state machine init [reg (a b) ...] [mem (c d) ...] [set-z vz] [set-fp vfp])
     (let ([state (default-state machine init [set-z vz] [set-fp vfp])])
       (vector-set! (progstate-regs state) a b)
       ...
       (vector-set! (progstate-memory state) c d)
       ...
       state))

    ((default-state machine init [reg (a b) ...] [mem (c d) ...])
     (let ([state (default-state machine init)])
       (vector-set! (progstate-regs state) a b)
       ...
       (vector-set! (progstate-memory state) c d)
       ...
       state))

    ((default-state [reg valr] [mem valm])
     (progstate valr valm 0 (send machine get-fp)))
    ((default-state [mem valm] [reg valr])
     (progstate valr valm 0 (send machine get-fp)))))

(define (lam-t) #t)
(define (lam-f) #f)

;; Macros to create output state constraint
(define-syntax constraint
  (syntax-rules (all none reg mem mem-all)
    ((constraint machine all) (default-state machine lam-t [set-z #t] [set-fp #f]))

    ((constraint machine none) (default-state machine lam-f [set-z #f] [set-fp #f]))

    ((constraint machine [reg r ...] [mem-all])
     (let ([state (default-state machine lam-f [reg (r #t) ...] [mem] [set-z #f] [set-fp #f])])
       (struct-copy progstate state 
                    [memory (make-vector (send machine get-nmems) #t)])))

    ((constraint machine [reg r ...] [mem m ...])
     (default-state machine lam-f [reg (r #t) ...] [mem (m #t) ...] [set-z #f] [set-fp #f]))

    ((constraint machine [reg r ...] [mem m ...] [z vz])
     (default-state machine lam-f [reg (r #t) ...] [mem (m #t) ...] [set-z vz] [set-fp #f]))

    ((constraint [mem m ...] [reg r ...])
     (constraint [reg r ...] [mem m ...]))

    ((constraint [reg r ...] [mem m ...])
     (default-state lam-f [reg (r #t) ...] [mem (m #t) ...] [set-z #f] [set-fp #f]))
    ))

(define arm-machine%
  (class object%
    (super-new)
    (init-field [bit 32]
                [inst-id
                 '#(nop 
                     add sub rsb
                     add# sub# rsb#
                     and orr eor bic orn
                     and# orr# eor# bic# orn#
                     mov mvn
                     mov# mvn# movw# movt#
                     rev rev16 revsh rbit
                     asr lsl lsr
                     asr# lsl# lsr#
                     mul mla mls
                     smull umull
                     smmul smmla smmls
                     ;; sdiv udiv
		     uxtah uxth uxtb
                     bfc bfi
                     sbfx ubfx
                     clz
                     ldr str
                     ldr# str#
                     tst cmp
                     tst# cmp#
                     )]
                [shf-inst-id '#(nop asr lsl lsr asr# lsl# lsr#)]
		[inst-with-shf '(add sub rsb and orr eor bic orn mov mvn)]
		[cond-inst-id '#(eq ne ls hi cc cs)]
                [nop-id 0]
                [perline 8])

    (define nregs 5)
    (define nmems 1)
    (define fp 0)

    (define/public (get-nregs) nregs)
    (define/public (get-nmems) nmems)
    (define/public (get-fp) fp)
    (define/public (get-shf-inst-id x)
      (vector-member x shf-inst-id))
    (define/public (get-shf-inst-name x)
      (vector-ref shf-inst-id x))
    (define/public (get-cond-inst-id x)
      (vector-member x cond-inst-id))
    (define/public (get-cond-inst-name x)
      (vector-ref cond-inst-id x))

    (define/public (get-config)
      (list nregs nmems fp))

    ;; info: (list nregs nmem)
    (define/public (set-config info)
      (set! nregs (first info))
      (set! nmems (second info))
      (set! fp (third info))
      )

    (define/public (get-state init)
      (default-state this init [set-z -1] [set-fp fp]))

    (define/public (get-inst-id opcode)
      (vector-member opcode inst-id))

    (define/public (get-inst-name id)
      (vector-ref inst-id id))

    (define (print-line v)
      (define count 0)
      (for ([i v])
           (when (= count perline)
	     (newline)
	     (set! count 0))
           (display i)
           (display " ")
           (set! count (add1 count))
           )
      (newline)
      )

    ;; Pretty print functions
    (define/public (display-state s)
      (pretty-display "REGS:")
      (print-line (progstate-regs s))
      (pretty-display "MEMORY:")
      (print-line (progstate-memory s))
      (pretty-display (format "Z: ~a" (progstate-z s)))
      )

    ))

    
