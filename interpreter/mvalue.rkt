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
  (lambda (statement operator len)
    (and (eq? (statement-op statement) operator)
         (eq? (length statement) len))))

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

      ; Cases with mathematical operators
      ((operator? lis '+ 2-operand) (+         (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '- 2-operand) (-         (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '- 1-operand) (*         (mvalue (operand1 lis) s) -1))
      ((operator? lis '* 2-operand) (*         (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '/ 2-operand) (quotient  (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '% 2-operand) (remainder (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))

      ; Cases with comparison operators
      ((operator? lis '== 2-operand) (eq?      (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '!= 2-operand) (not (eq? (mvalue (operand1 lis) s) (mvalue (operand2 lis) s))))
      ((operator? lis '<  2-operand) (<        (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '>  2-operand) (>        (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '<= 2-operand) (<=       (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '>= 2-operand) (>=       (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '&& 2-operand) (and      (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '|| 2-operand) (or       (mvalue (operand1 lis) s) (mvalue (operand2 lis) s)))
      ((operator? lis '!  1-operand) (not      (mvalue (operand1 lis) s)))

      (else (error "Error: Evaluating invalid expression\nExpression: " lis)))))

