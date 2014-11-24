#lang s-exp rosette

(require "ops-rosette.rkt" 
         "arm-ast.rkt"
         "arm-machine.rkt")
(provide arm-simulator-rosette%)

(define arm-simulator-rosette%
  (class object%
    (super-new)
    (init-field machine)
    (public interpret performance-cost)
        
    ;; Get frequently used information from arm-machine
    (define bit (get-field bit machine)) ;; 32
    (define nop-id (send machine get-inst-id `nop))
    (define inst-id (get-field inst-id machine))
    (define shf-inst-id (get-field shf-inst-id machine))
    (define ninsts (vector-length inst-id))
    (define n-shf-insts (vector-length shf-inst-id))

    (define (shl a b) (<< a b bit))
    (define (ushr a b) (>>> a b bit))

    ;; If the highest order bit is 1, 
    ;; then the function returns the corresponding negative number.
    ;; This is crucial to make sure Rosette is consistent with Racket.
    (define-syntax-rule (finitize-bit x) (finitize x bit))

    (define byte0 0)
    (define byte1 (quotient bit 4))
    (define byte2 (* 2 byte1))
    (define byte3 (* 3 byte1))
    (define byte4 bit)
    (define byte-mask (sub1 (arithmetic-shift 1 byte1)))
    (define low-mask (sub1 (shl 1 byte2)))
    (define high-mask (shl low-mask byte2))
    (define mask (sub1 (arithmetic-shift 1 bit)))

    ;;;;;;;;;;;;;;;;; helper functions ;;;;;;;;;;;;;;;;;;;
    (define-syntax-rule (bool->num b) (if b 1 0))

    (define-syntax-rule (bvop op)     
      (lambda (x y) (finitize-bit (op x y))))

    (define-syntax-rule (bvuop op)     
      (lambda (x) (finitize-bit (op x))))

    (define-syntax-rule (bvcmp op) 
      (lambda (x y) (bool->num (op x y))))

    (define-syntax-rule (bvucmp op)   
      (lambda (x y) 
        (bool->num (if (equal? (< x 0) (< y 0)) (op x y) (op y x)))))

    (define-syntax-rule (bvshift op)
      (lambda (x y)
        (finitize-bit (op x (bitwise-and byte-mask y)))))

    (define-syntax-rule (bvbit op)
      (lambda (a b)
        (if (and (>= b 0) (< b bit))
            (finitize-bit (op a (shl 1 b)))
            a)))

    (define bvadd (bvop +))
    (define bvsub (bvop -))
    (define bvrsub (bvop (lambda (x y) (- y x))))

    (define bvnot (lambda (x) (finitize-bit (bitwise-not x))))
    (define bvand (bvop bitwise-and))
    (define bvor  (bvop bitwise-ior))
    (define bvxor (bvop bitwise-xor))
    (define bvandn (lambda (x y) (finitize-bit (bitwise-and x (bitwise-not y)))))
    (define bviorn  (lambda (x y) (finitize-bit (bitwise-ior x (bitwise-not y)))))

    (define bvrev (bvuop rev))
    (define bvrev16 (bvuop rev16))
    (define bvrevsh (bvuop revsh))
    (define bvrbit (bvuop rbit))

    (define bvshl  (bvshift shl))
    (define bvshr  (bvshift >>))
    (define bvushr (bvshift ushr))

    (define uxtah (bvop (lambda (x y) (+ x (bitwise-and y low-mask)))))
    (define uxth (lambda (x) (finitize-bit (bitwise-and x low-mask))))
    (define uxtb (lambda (x) (finitize-bit (bitwise-and x byte-mask))))

    (define bvmul (bvop *))
    (define bvmla (lambda (a b c) (finitize-bit (+ c (* a b)))))
    (define bvmls (lambda (a b c) (finitize-bit (- c (* a b)))))
    (define bvsmmla (lambda (a b c) (finitize-bit (+ c (bvsmmul a b)))))
    (define bvsmmls (lambda (a b c) (finitize-bit (- c (bvsmmul a b)))))

    (define (bvsmmul x y) (smmul x y bit))
    (define (bvummul x y) (ummul x y bit))
    (define (bvudiv n d)
      (if (< d 0)
          (if (< n d) 1 0)
          (let* ([q (shl (quotient (ushr n 2) d) 2)]
                 [r (- n (* q d))])
            (finitize-bit (if (or (> r d) (< r 0)) q (add1 q))))))
      

    (define (movlo to c)
      (finitize-bit (bitwise-ior (bitwise-and to high-mask) c)))
    
    (define (movhi to c)
      (finitize-bit (bitwise-ior (bitwise-and to low-mask) (shl c byte2))))

    (define (setbit d a width shift)
      (assert (and (>= shift 0) (<= shift bit)))
      (assert (and (>= width 0) (<= width bit)))
      (let* ([mask (sub1 (shl 1 width))]
             [keep (bitwise-and d (bitwise-not (shl mask shift)))]
             [insert (bvshl (bitwise-and a mask) shift)])
        (finitize-bit (bitwise-ior keep insert))))

    (define (clrbit d width shift)
      (assert (and (>= shift 0) (<= shift bit)))
      (assert (and (>= width 0) (<= width bit)))
      (let* ([keep (bitwise-not (shl (sub1 (shl 1 width)) shift))])
        (finitize-bit (bitwise-and keep d))))

    (define (ext d a width shift)
      (assert (and (>= shift 0) (<= shift bit)))
      (assert (and (>= width 0) (<= width bit)))
      (finitize-bit (bitwise-and (>> a shift) (sub1 (shl 1 width)))))

    (define (sext d a width shift)
      (assert (and (>= shift 0) (<= shift bit)))
      (assert (and (>= width 0) (<= width bit)))
      (let ([keep (bitwise-and (>> a shift) (sub1 (shl 1 width)))])
	(bitwise-ior
	 (if (= (bitwise-bit-field keep (sub1 width) width) 1)
	     (shl -1 width)
	     0)
	 (finitize-bit keep))))

    (define (clz x)
      (let ([mask (shl 1 (sub1 bit))]
            [count 0]
            [still #t])
        (for ([i bit])
             (when still
                   (let ([res (bitwise-and x mask)])
                     (set! x (shl x 1))
                     (if (= res 0)
                         (set! count (add1 count))
                         (set! still #f)))))
        count))

    (define (rev a)
      (bitwise-ior 
       (shl (bitwise-bit-field a byte0 byte1) byte3)
       (shl (bitwise-bit-field a byte1 byte2) byte2)
       (shl (bitwise-bit-field a byte2 byte3) byte1)
       (bitwise-bit-field a byte3 byte4)))

    (define (rev16 a)
      (bitwise-ior 
       (shl (bitwise-bit-field a byte2 byte3) byte3)
       (shl (bitwise-bit-field a byte3 byte4) byte2)
       (shl (bitwise-bit-field a byte0 byte1) byte1)
       (bitwise-bit-field a byte1 byte2)))

    (define (revsh a)
      (bitwise-ior 
       (if (= (bitwise-bit-field a (sub1 byte1) byte1) 1) high-mask 0)
       (shl (bitwise-bit-field a byte0 byte1) byte1)
       (bitwise-bit-field a byte1 byte2)))

    (define (rbit a)
      (let ([res 0])
        (for ([i bit])
             (set! res (bitwise-ior (shl res 1) (bitwise-and a 1)))
             (set! a (>> a 1)))
        res))

    (define (tst x y) (if (= (bitwise-and x y) 0) 0 1))
    (define (cmp x y)
      (define my-x (finitize-bit x))
      (define my-y (finitize-bit y))
      (cond
       [(= my-x my-y) 0]
       [(or (and (>= my-x 0) (>= my-y 0))
	    (and (< my-x 0) (< my-y 0)))
	(cond
	 [(< my-x my-y) 2]
	 [else 3])]
       [(< my-y 0) 2]
       [else 3]))
    
    ;;;;;;;;;;;;;;;;;;;;;;;; Main interpret function ;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Interpret a given program from a given state.
    ;; state: initial progstate
    (define (interpret program state)
      ;;(pretty-display `(interpret))
      (define regs (vector-copy (progstate-regs state)))
      (define memory (vector-copy (progstate-memory state)))
      (define z (progstate-z state)) ;; flag
      (define fp (progstate-fp state))

      ;; Interpret one instruction.
      (define (interpret-instruction instruction)
        (define op (inst-op instruction))
        (define args (inst-args instruction))
        (define cond-type (arm-inst-cond instruction))
        (define shfop (arm-inst-shfop instruction))
        ;;(pretty-display `(interpret-instruction ,z ,op ,cond-type))
        
        (define-syntax inst-eq
          (syntax-rules ()
            ((inst-eq x) (equal? op (vector-member x inst-id)))
            ((inst-eq a b ...) (or (equal? op (vector-member a inst-id))
                                   (equal? op (vector-member b inst-id))
                                   ...))))

        (define-syntax-rule (args-ref args i) (vector-ref args i))

        ;; Execute the instruction if the flag agrees with the conditional code.
        (define (exec)

          ;; Execute optimal shift on the 2nd operand.
          (define (opt-shift x)
            (define op (arm-inst-shfop instruction))
            (define k (arm-inst-shfarg instruction))
            (define-syntax-rule (shf-inst-eq xx) 
              (equal? op (vector-member xx shf-inst-id)))

            (define val #f)
            (define (rr f) (f (vector-ref regs x) (vector-ref regs k)))
            (define (ri f) (f (vector-ref regs x) k))
            (cond
             [(or (equal? op #f) (shf-inst-eq `nop))
              (vector-ref regs x)]
             [(shf-inst-eq `lsr) (rr bvushr)]
             [(shf-inst-eq `asr) (rr bvshr)]
             [(shf-inst-eq `lsl) (rr bvshl)]
             
             ;; shift i
             [(shf-inst-eq `lsr#) (ri bvushr)]
             [(shf-inst-eq `asr#) (ri bvshr)]
             [(shf-inst-eq `lsl#) (ri bvshl)]
             [else
              (assert #f (format "undefine optional shift: ~a" op))]
             )
            )

          ;;;;;;;;;;;;;;;;;;;;; instruction format functions ;;;;;;;;;;;;;;;;;;;
          ;; rrr = opcode reg reg reg
          (define (rrr f [shf #f])
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define reg-b-val
	      (if shf
		  (opt-shift b)
		  (vector-ref regs b)))

            (define val (f (vector-ref regs a) reg-b-val))
            (vector-set! regs d val))

          ;; rrrr = opcode reg reg reg reg
          (define (rrrr f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define c (args-ref args 3))
            (define val (f (vector-ref regs a) (vector-ref regs b) (vector-ref regs c)))
            (vector-set! regs d val))

          ;; ddrr = opcode reg reg reg reg
          ;; dd indicates that the first two regs are destination registers.
          (define (ddrr f-lo f-hi)
            (define d-lo (args-ref args 0))
            (define d-hi (args-ref args 1))
            (assert (not (= d-lo d-hi)))
            (define a (args-ref args 2))
            (define b (args-ref args 3))
            (define val-lo (f-lo (vector-ref regs a) (vector-ref regs b)))
            (define val-hi (f-hi (vector-ref regs a) (vector-ref regs b)))
            (vector-set! regs d-lo val-lo)
            (vector-set! regs d-hi val-hi))

          ;; rr = opcode reg reg
          (define (rr f [shf #f])
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define reg-a-val
	      (if shf
		  (opt-shift a)
		  (vector-ref regs a)))
            (define val (f reg-a-val))
            (vector-set! regs d val))

          ;; ri = opcode reg imm
          (define (ri f)
            (define d (args-ref args 0))
            (define a (check-imm-mov (args-ref args 1)))
            (define val (f a))
            (vector-set! regs d val))

          ;; r!i = opcode reg imm
          ;; r! indicates that the first register is read and write.
          (define (r!i f)
            (define d (args-ref args 0))
            (define a (check-imm-mov (args-ref args 1)))
            (define val (f (vector-ref regs d) a))
            (vector-set! regs d val))

          ;; rri = opcode reg reg imm
          (define (rri f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (check-imm (args-ref args 2)))
            (define val (f (vector-ref regs a) b))
            (vector-set! regs d val))

          ;; rrb = opcode reg reg imm, where (0 <= imm <= bit)
          (define (rrb f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define val (f (vector-ref regs a) b))
            (vector-set! regs d val))

          ;; store
          (define (str reg-offset)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define base
              (if (= a -1) fp (vector-ref regs a)))
            (define index 
              (finitize-bit
               (if reg-offset
                   (+ base (vector-ref regs b))
                   (+ base b))))
            (define val (vector-ref regs d))
            (vector-set! memory index val))

          ;; load
          (define (ldr reg-offset)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define base
              (if (= a -1) fp (vector-ref regs a)))
            (define index 
              (finitize-bit
               (if reg-offset
                   (+ base (vector-ref regs b))
                   (+ base b))))
            (define val (vector-ref memory index))
            (vector-set! regs d val))

          (define (rrbb f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define width (args-ref args 3))
            (define shift (args-ref args 2))
            (define val (f (vector-ref regs d) (vector-ref regs a) width shift))
            (vector-set! regs d val))

          ;; clrbit
          (define (r!bb f)
            (define d (args-ref args 0))
            (define width (args-ref args 2))
            (define shift (args-ref args 1))
            (define val (f (vector-ref regs d) width shift))
            (vector-set! regs d val))

          ;; Set flag z
          ;; z=rr = opcode reg reg
          (define (z=rr f)
            (define a (args-ref args 0))
            (define b (args-ref args 1))
	    (set! z (f (vector-ref regs a) (vector-ref regs b))))

          ;; Set flag z
          ;; z=ri = opcode reg imm
          (define (z=ri f)
            (define a (args-ref args 0))
            (define b (check-imm (args-ref args 1)))
	    (set! z (f (vector-ref regs a) b)))

          (cond
           ;; basic
           [(inst-eq `nop) (void)]
           [(inst-eq `add) (rrr bvadd #t)]
           [(inst-eq `sub) (rrr bvsub #t)]
           [(inst-eq `rsb) (rrr bvrsub #t)]

           [(inst-eq `and) (rrr bitwise-and #t)]
           [(inst-eq `orr) (rrr bitwise-ior #t)]
           [(inst-eq `eor) (rrr bitwise-xor #t)]
           [(inst-eq `bic) (rrr bvandn #t)]
           [(inst-eq `orn) (rrr bviorn #t)]

           ;; basic with imm
           [(inst-eq `add#) (rri bvadd)]
           [(inst-eq `sub#) (rri bvsub)]
           [(inst-eq `rsb#) (rri bvrsub)]

           [(inst-eq `and#) (rri bitwise-and)]
           [(inst-eq `orr#) (rri bitwise-ior)]
           [(inst-eq `eor#) (rri bitwise-xor)]
           [(inst-eq `bic#) (rri bvandn)]
           [(inst-eq `orn#) (rri bviorn)]
           
           ;; move
           [(inst-eq `mov) (rr identity #t)]
           [(inst-eq `mvn) (rr bvnot #t)]
           
           ;; move with imm
           [(inst-eq `mov#) (ri identity)]
           [(inst-eq `mvn#) (ri bvnot)]
           [(inst-eq `movt#) (r!i movhi)]
           [(inst-eq `movw#) (r!i movlo)]

           ;; bit permute
           [(inst-eq `rev)   (rr bvrev)]
           [(inst-eq `rev16) (rr bvrev16)]
           [(inst-eq `revsh) (rr bvrevsh)]
           [(inst-eq `rbit)  (rr bvrbit)]

           ;; div & mul
           [(inst-eq `mul)  (rrr bvmul)]
           [(inst-eq `mla)  (rrrr bvmla)]
           [(inst-eq `mls)  (rrrr bvmls)]

           [(inst-eq `smmul) (rrr bvsmmul)]
           [(inst-eq `smmla) (rrrr bvsmmla)]
           [(inst-eq `smmls) (rrrr bvsmmls)]

           [(inst-eq `smull) (ddrr bvmul bvsmmul)]
           [(inst-eq `umull) (ddrr bvmul bvummul)]

           ;; [(inst-eq `sdiv) (rrr quotient)]
           ;; [(inst-eq `udiv) (rrr bvudiv)]

           [(inst-eq `uxtah) (rrr uxtah)]
           [(inst-eq `uxth) (rr uxth)]
           [(inst-eq `uxtb) (rr uxtb)]
           
           ;; shift Rd, Rm, Rs
           ;; only the least significant byte of Rs is used.
           [(inst-eq `lsr) (rrr bvushr)]
           [(inst-eq `asr) (rrr bvshr)]
           [(inst-eq `lsl) (rrr bvshl)]
           
           ;; shift with imm
           [(inst-eq `lsr#) (rrb bvushr)]
           [(inst-eq `asr#) (rrb bvshr)]
           [(inst-eq `lsl#) (rrb bvshl)]

           ;; others
           [(inst-eq `bfc)  (r!bb  clrbit)]
           [(inst-eq `bfi)  (rrbb setbit)]
           [(inst-eq `sbfx) (rrbb sext)]
           [(inst-eq `ubfx) (rrbb ext)]
           [(inst-eq `clz)  (rr clz)]

           ;; load/store
           [(inst-eq `ldr#) (ldr #f)]
           [(inst-eq `str#) (str #f)]
           [(inst-eq `ldr)  (ldr #t)]
           [(inst-eq `str)  (str #t)]

           ;; compare
           [(inst-eq `tst) (z=rr tst)]
           [(inst-eq `cmp) (z=rr cmp)]

           ;; compare with imm
           [(inst-eq `tst#) (z=ri tst)]
           [(inst-eq `cmp#) (z=ri cmp)]

           [else (assert #f "undefine instruction")]))

	;; z: eq 0, ne 1, lt 1, gt 3
	;; cond: eq 0, ne 1, ls 2, hi 3, cc 4, cs 5
	(define-syntax-rule (assert-op) (assert (and (>= op 0) (< op ninsts))))
        (cond
         [(or (equal? z -1) (equal? cond-type -1)
              (inst-eq `tst `cmp `tst# `cmp#))
	  (assert (and (>= cond-type -1) (<= cond-type 5)))
          (exec)]

	 [(equal? cond-type 0) ;; eq
	  (if (equal? z 0) (exec) (assert-op))]

	 [(equal? cond-type 1) ;; ne
	  (if (member z (list 1 2 3)) (exec) (assert-op))]

	 [(equal? cond-type 2) ;; ls
	  (if (member z (list 0 2)) (exec) (assert-op))]

	 [(equal? cond-type 3) ;; hi
	  (if (equal? z 3) (exec) (assert-op))]

	 [(equal? cond-type 4) ;; cc
	  (if (equal? z 2) (exec) (assert-op))]

	 [(equal? cond-type 5) ;; cs
	  (if (member z (list 0 3)) (exec) (assert-op))]
	 
         [else (assert #f (format "illegal cond-type ~a" cond-type))]
         )        
        (assert (or (equal? shfop #f) (and (>= shfop 0) (< shfop n-shf-insts))))
        )

      (for ([x program])
           (interpret-instruction x))
      
      (progstate regs memory z fp))

    (define (performance-cost code)
      (define cost 0)
      (define-syntax-rule (add-cost x) (set! cost (+ cost x)))
      (for ([x code])
           (let ([op (inst-op x)]
                 [shfop (inst-shfop x)])
             (define-syntax-rule (inst-eq a ...) 
               (or (equal? op (vector-member a inst-id)) ...))
             (define-syntax-rule (shf-inst-eq a ...) 
               (or (equal? shfop (vector-member a shf-inst-id)) ...))

             (cond
              [(inst-eq `nop) (void)]
              [(inst-eq `str `str# `ldr `ldr#) (add-cost 3)]
              [(inst-eq `mul `mla `mls `smmul `smmla `smmls) (add-cost 5)]
              [(inst-eq `smull `umull `sdiv `udiv) (add-cost 6)]
              [(inst-eq `tst `cmp `tst# `cmp#) (add-cost 2)]
              [(inst-eq `sbfx `ubfx `bfc `bfi) (add-cost 2)]
              [(and (inst-eq `add `sub `rsb `and `orr `eor `bic `orn `mov `mvn)
                    (shf-inst-eq `lsr `asr `lsl))
               (add-cost 2)]

              [else (add-cost 1)])
             ))
      (when debug (pretty-display `(performance ,cost)))
      cost)

    (define legal-imm 
      (append (for/list ([i 12]) (arithmetic-shift #xff (* 2 i)))
              (list #xff000000 (- #xff000000))))

    (define-syntax-rule (check-imm x) 
      (assert-return 
       (ormap (lambda (i) (= (bitwise-and x i) (bitwise-and x mask))) legal-imm) 
       "illegal immediate"
       x))

    (define-syntax-rule (check-imm-mov x) 
      (assert-return 
       (= (bitwise-and x #xffff) x) 
       "illegal mov immediate"
       x))

    ))
  
