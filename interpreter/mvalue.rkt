#lang racket
(provide mvalue)
(require "simpleParser.rkt")
(require "env.rkt")
(require "helper.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 3
;;;; Mvalue calculation functions
;;;; *********************************************************************************************************
; This file provides
(provide (all-defined-out))


; The abstracted functions
(define statement-op car)
(define operand1 cadr)
(define operand2 caddr)
(define 2-operand 3)
(define 1-operand 2)

;; Function:    (operator? statement operator)
;; Parameters:  statement is the parsed statement to evaluate. First element should be
;;                the operator represented by an atom
;;              operator is the operator to check for.
;;              len is the number of expected elements in the list (including operator
;;                and operands)
;; Description: Helper function that compares operator of given list to given operator.
;;              Returns true if it matches, false if not.
(define operator?
  (lambda (statement operator)
    (eq? (statement-op statement) operator)
  )
)

;; Function:    (2_op_switch expr)
;; Parameters:  expr is the list that represents the parse tree. Must contain an operator
;;                as the first element, then two operands as the subsequent elemnts.
;; Description: Returns the correct function to use for the given operation.
(define 2_op_switch
  (lambda (expr)
    (cond
      ;; cases with arithmetic operators
      ((operator? expr '+) +)
      ((operator? expr '-) -)
      ((operator? expr '*) *)
      ((operator? expr '/) quotient)
      ((operator? expr '%) remainder)

      ; Cases with comparison operators
      ((operator? expr '==) eq?)
      ((operator? expr '!=) (lambda (op1 op2) (not (eq? op1 op2))))
      ((operator? expr '< ) <)
      ((operator? expr '> ) >)
      ((operator? expr '<=) <=)
      ((operator? expr '>=) >=)
      ((operator? expr '&&) (lambda (op1 op2) (and op1 op2)))
      ((operator? expr '||) (lambda (op1 op2) (or op1 op2)))

      ; Operator not recognized
      (else                 (error "Error: Executing invalid expression.\nExpression: " expr))
    )
  )
)

;; Function:    (1_op_switch expr)
;; Parameters:  expr is the list that represents the parse tree. Must contain an operator
;;                as the first element, then one operands as the subsequent elemnt.
;; Description: Returns the correct function to use for the given operation.
(define 1_op_switch
  (lambda (expr)
    (cond
      ((operator? expr '-) (lambda (op1) (* -1 op1)))
      ((operator? expr '!) (lambda (op1) (not op1)))
      (else                (error "Error: Executing invalid expression.\nExpression: " expr))
    )
  )
)
