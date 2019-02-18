#lang racket
;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 1
;;;; Mvalue calculation functions
;;;; *********************************************************************************************************
; This file provides
(provide mvalue)

; Dependencies
(require "simpleParser.rkt")
(require "state.rkt")
(require "helper.rkt")

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
    (eq? (statement-op statement) operator)))

;; Function:    (2_op_switch lis)
;; Parameters:  lis is the list that represents the parse tree. Must contain an operator
;;                as the first element, then two operands as the subsequent elemnts.
;; Description: Returns the correct function to use for the given operation.
(define 2_op_switch
  (lambda (lis)
    (cond
      ((operator? lis '+) +)
      ((operator? lis '-) -)
      ((operator? lis '*) *)
      ((operator? lis '/) quotient)
      ((operator? lis '%) remainder)

      ; Cases with comparison operators
      ((operator? lis '==) eq?)
      ((operator? lis '!=) (lambda (op1 op2) (not (eq? op1 op2))))
      ((operator? lis '< ) <)
      ((operator? lis '> ) >)
      ((operator? lis '<=) <=)
      ((operator? lis '>=) >=)
      ((operator? lis '&&) (lambda (op1 op2) (and op1 op2)))
      ((operator? lis '||) (lambda (op1 op2) (or op1 op2)))

      ; Operator not recognized
      (else (error "Error: Executing invalid expression.\nExpression: " lis)))))

;; Function:    (1_op_switch lis)
;; Parameters:  lis is the list that represents the parse tree. Must contain an operator
;;                as the first element, then one operands as the subsequent elemnt.
;; Description: Returns the correct function to use for the given operation.
(define 1_op_switch
  (lambda (lis)
    (cond
      ((operator? lis '-) (lambda (op1) (* -1 op1)))
      ((operator? lis '!) (lambda (op1) (not op1)))
      (else (error "Error: Executing invalid expression.\nExpression: " lis)))))

;; Function:    (mvalue lis s)
;; Parameters:  lis is list representing the parse tree
;;              s is the list representing state, which contains the name-value bindings
;; Description: Evaluates the given expression using the given state.
(define mvalue
  (lambda (lis s)
    (cond
      ((null? lis) (error "Error: Evaluating null statement"))

      ; Base cases
      ((number? lis)     lis)
      ((eq? lis 'true)   #t)
      ((eq? lis 'false)  #f)
      ((not (list? lis)) (find lis s))

      ((eq? (length lis) 1-operand) ((lambda (func) (func (mvalue (operand1 lis) s))) (1_op_switch lis)))
      ((eq? (length lis) 2-operand) ((lambda (func) (func (mvalue (operand1 lis) s) (mvalue (operand2 lis) s))) (2_op_switch lis)))

      (else (error "Error: Executing invalid expression.\nExpression: " lis)))))

;; Function:    (mvalue* lis s)
;; Parameters:  lis is list representing the parse tree
;;              s is the list representing state, which contains the name-value bindings
;; Description: Evaluates the given expression using the given state. Calls a version of
;;              mvalue that uses continuation passing style.
(define mvalue*
  (lambda (lis s)
    (mvalue-cps lis s (lambda (v) v))))

;; Function:    (mvalue-cps lis s return)
;; Parameters:  lis is the list representing the parse tree
;;              s is the list representing state, which contains name-value bindings.
;;              return is the function handle to use to return the value
;; Description: Evaluates the given exxpression using the given state. Uses continuation
;;              passing style.
(define mvalue-cps
  (lambda (lis s return)
    (cond
      ((null? lis) (error "Error: Evaluating null statement"))

      ; Base Cases
     ((number? lis) (return lis))
     ((eq? lis 'true) (return #t))
     ((eq? lis 'false)  (return #f))
     ((not (list? lis)) (return (find lis s)))

     ; Cases with operators
     ((eq? (length lis) 1-operand) ((lambda (func) (mvalue-cps (operand1 lis) s (lambda (v) (return (func v))))) (1_op_switch lis)))
     ((eq? (length lis) 2-operand) ((lambda (operator) (mvalue-cps (operand1 lis)
                                                                   s
                                                                   (lambda (v1) (mvalue-cps (operand2 lis)
                                                                                            s
                                                                                            (lambda (v2) (return (operator v1 v2))))))) (2_op_switch lis))))))