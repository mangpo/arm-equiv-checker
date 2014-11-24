#lang racket

(provide (all-defined-out))

(struct arm-inst (op args shfop shfarg cond))

(define-syntax-rule (inst? x) (arm-inst? x))
(define-syntax-rule (inst-op x) (arm-inst-op x))
(define-syntax-rule (inst-args x) (arm-inst-args x))
(define-syntax-rule (inst-cond x) (arm-inst-cond x))
(define-syntax-rule (inst-shfop x) (arm-inst-shfop x))
(define-syntax-rule (inst-shfarg x) (arm-inst-shfarg x))

(struct block (body org info))
(struct label (name body info)) 