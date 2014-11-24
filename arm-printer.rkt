#lang racket

(require "arm-ast.rkt" "arm-machine.rkt")

(provide arm-printer%)

(define arm-printer%
  (class object%
    (super-new)
    (init-field machine)
    (public encode decode print-struct print-syntax)

    (define (print-struct-inst x [indent ""])
      (pretty-display (format "~a(inst ~a ~a ~a ~a ~a)" 
                              indent (inst-op x) (inst-args x)
                              (inst-shfop x) (inst-shfarg x)
                              (inst-cond x))))

    (define (print-syntax-inst x [indent ""])
      (define op (inst-op x))
      (define args (vector-copy (inst-args x)))
      (define shfop (inst-shfop x))
      (define shfarg (inst-shfarg x))
      (when (or (equal? op "str") (equal? op "ldr"))
	    (when (not (equal? (substring (vector-ref args 2) 0 1) "r"))
		  (vector-set! args 2 (number->string (* 4 (string->number (vector-ref args 2)))))))
      (display (format "~a~a~a ~a" indent op (inst-cond x) (string-join (vector->list args) ", ")))
      (when (and shfop (not (equal? shfop "nop")))
	    (display (format ", ~a ~a" shfop shfarg)))
      (newline))

    (define (name->id name)
      ;;(pretty-display `(name->id ,name))
      (cond
       [(string->number name)
        (string->number name)]
       
       [(and (> (string-length name) 1) (equal? (substring name 0 1) "r"))
        (string->number (substring name 1))]

       [(equal? name "fp") -1]
       
       [else 
        (raise (format "encode: name->id: undefined for ~a" name))]))

    ;; Convert an instruction in string format into
    ;; into an instructions encoded using numbers.
    (define (encode-inst x)
      (define op (inst-op x))
      (define args (inst-args x))
      (define args-len (vector-length args))
      (when (and (> args-len 0)
		 (not (equal? "r" (substring (vector-ref args (sub1 args-len)) 0 1)))
                 (not (member (string->symbol op) '(bfc bfi sbfx ubfx))))
	    (set! op (string-append op "#")))
      (define shfop (inst-shfop x))
      (define shfarg (inst-shfarg x))
      (when (and shfarg (not (equal? "r" (substring shfarg 0 1))))
	    (set! shfop (string-append shfop "#")))
      
      (let ([cond-type (arm-inst-cond x)])
        (arm-inst (send machine get-inst-id (string->symbol op))
                  (vector-map name->id args)
                  (and shfop (send machine get-shf-inst-id (string->symbol shfop)))
                  (and shfarg (name->id shfarg))
		  (if (equal? cond-type "")
		      -1
		      (send machine get-cond-inst-id (string->symbol cond-type))))))
                

    ;; Convert an instruction encoded using numbers
    ;; into an instructions in string format.
    (define (decode-inst x)
      (define opcode (send machine get-inst-name (inst-op x)))
      ;;(pretty-display `(decode-inst ,opcode ,(inst-shfop x)))
      (define args (inst-args x))
      (define cond-type (inst-cond x))
      (define class-id (send machine get-class-id opcode))
      (define shf (member opcode (get-field inst-with-shf machine)))
      (define shfop (and shf (inst-shfop x) (send machine get-shf-inst-name (inst-shfop x))))
      (define shfarg (and shf (inst-shfarg x)))
      
      (define-syntax-rule (make-inst x ...)
        (make-inst-main (list x ...)))

      (define (convert-op op)
	(let* ([str (symbol->string op)]
	       [len (string-length str)])
	  (if (equal? (substring str (sub1 len)) "#")
	      (substring str 0 (sub1 len))
	      str)))

      (define (convert-shfarg arg op)
	(let* ([str (symbol->string op)]
	       [len (string-length str)])
	  (if (equal? (substring str (sub1 len)) "#")
	      (imm arg)
	      (reg arg))))
      
      (define (make-inst-main fs)
        (define new-args (for/vector ([f fs] [arg args]) (f arg)))
        (arm-inst (convert-op opcode) new-args
		  (and shfop (convert-op shfop))
		  (and shfarg (convert-shfarg shfarg shfop))
		  (if (= cond-type -1) "" (send machine get-cond-inst-name cond-type))))

      (define (reg x) (format "r~a" x))
      (define (index x)
        (cond
         [(= x -1) "fp"]
         [else (reg x)]))

      (define imm number->string)

      (cond
       [(equal? class-id 0) (make-inst reg reg reg)]
       [(equal? class-id 1) (make-inst reg reg imm)]
       [(equal? class-id 2) (make-inst reg reg)]
       [(equal? class-id 3) (make-inst reg imm)]
       [(equal? class-id 4) (make-inst reg reg reg reg)]
       [(equal? class-id 5) (make-inst reg reg imm imm)]
       [(equal? class-id 6) (make-inst reg index imm)]
       [(member opcode '(bfc)) (make-inst reg imm imm)]
       [(equal? opcode `nop) (arm-inst "nop" (vector) #f #f "")]
       [else (raise (format "decode-inst: undefined for ~a" opcode))]))


    (define (encode code)
      (vector-map encode-inst code))

    (define (decode code)
      (vector-map decode-inst code))

    (define (print-struct x [indent ""])
      (define (inc ind) (string-append ind " "))
      (cond
       [(or (list? x) (vector? x))
        (for ([i x]) (print-struct i indent))]
       
       [(inst? x) (print-struct-inst x indent)]
       
       [(block? x)
        (print-struct (block-body x) indent)]
       
       [(label? x)
        (pretty-display (format "~a(label ~a" indent (label-name x)))
        (print-struct (label-body x) (inc indent))
        (pretty-display (format "~a)" indent))]
       
       [else
        (pretty-display (format "~a~a" indent x))]))

    (define (print-syntax x [indent ""])
      (define (inc ind) (string-append ind " "))
      (define (f x indent)
        (cond
         [(inst? x) (print-syntax-inst x indent)]
         
         [(or (list? x) (vector? x))
          (for ([i x]) (f i indent))]
         
         [(block? x)
          (when (block-info x)
                (pretty-display (format "~a; ~a" indent (block-info x))))
          (f (block-body x) indent)]
         
         [(label? x)
          (when (label-name x) 
                (pretty-display (format "~a~a" indent (label-name x))))
          (f (label-body x) (inc indent))]
         
         [else
          (pretty-display (format "~a~a" indent x))]))
      (f x indent))

    ))
