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

(define 2_op_switch
  (lambda (lis)
    (cond
      ((operator? lis '+ 2-operand) +)
      ((operator? lis '- 2-operand) -)
      ((operator? lis '* 2-operand) *)
      ((operator? lis '/ 2-operand) quotient)
      ((operator? lis '% 2-operand) remainder)
      ((operator? lis '- 1-operand) (lambda (op1) (* -1 op1)))

      ; Cases with comparison operators
      ((operator? lis '== 2-operand) eq?)
      ((operator? lis '!= 2-operand) (lambda (op1 op2) (not (eq? op1 op2))))
      ((operator? lis '<  2-operand) <)
      ((operator? lis '>  2-operand) >)
      ((operator? lis '<= 2-operand) <=)
      ((operator? lis '>= 2-operand) >=)
      ((operator? lis '&& 2-operand) (lambda (op1 op2) (and op1 op2)))
      ((operator? lis '|| 2-operand) (lambda (op1 op2) (or op1 op2)))
      ((operator? lis '!  1-operand) (lambda (op1) (not op1)))

      ; Operator not recognized
      (else (error "Error: Executing invalid expression.\nExpression: " lis)))))
      
(define 1_op_switch
  (lambda (lis)
    (cond
      ((operator? lis '- 1-operand) (lambda (op1) (* -1 op1)))
      ((operator? lis '! 1-operand) (lambda (op1) (not op1))))))

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


(define cps_tail_recur_helper
  (lambda (cps_func op1 op2 operator state return)
    (cps_func op1
              state
              (lambda (v1) (cps_func op2
                                     state
                                     (lambda (v2) (return (operator v1 v2))))))))

;(define cps_tail_recur_helper2
;  (lambda (cps_func recur operator state return v_prev)
;    (if (null? (cdr recur))
;        (cps_func (car recur)
;                  state
;                  (lambda (v) (return (operator v_prev v))))
;        (cps_func (car recur)
;                  state
;                  (lambda (v) (cps_tail_recur_helper2 cps_func (cdr recur) operator state return v))))))

(define mvalue*
  (lambda (lis s)
    (mvalue-cps lis s (lambda (v) v))))

;(define mvalue-cps
;  (lambda (lis s return)
;    (cond
;      ((null? lis) (error error "Error: Evaluating null statement"))
;
;      ; Base Cases
;     ((number? lis) (return lis))
;     ((eq? lis 'true) (return #t))
;     ((eq? lis 'false)  (return #f))
;     ((not (list? lis)) (return (find lis s)))
;
;     ; Cases with mathematical operators
;     ((operator? lis '+ 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) +         s return))
;     ((operator? lis '* 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) *         s return))
;     ((operator? lis '/ 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) quotient  s return))
;     ((operator? lis '% 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) remainder s return))
;     ((operator? lis '- 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) -         s return))
;     ((operator? lis '- 1-operand) (mvalue-cps (operand1 lis) s (lambda (v) (return (* v -1)))))
;
;     ; Cases with comparison operators
;     ((operator? lis '== 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) eq? s return))
;     ((operator? lis '!= 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) (lambda (op1 op2) (not (eq? op1 op2))) s return))
;     ((operator? lis '<  2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) < s return))
;     ((operator? lis '>  2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) > s return))
;     ((operator? lis '<= 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) <= s return))
;     ((operator? lis '>= 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) >= s return))
;     ((operator? lis '&& 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) (lambda (op1 op2) (and op1 op2) s return)))
;     ((operator? lis '|| 2-operand) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) (lambda (op1 op2) (or op1 op2) s return)))
;     ((operator? lis '!  1-operand) (mvalue-cps (operand1 lis) s (lambda (v) (return (not v))))))))

(define mvalue-cps
  (lambda (lis s return)
    (cond
      ((null? lis) (error "Error: Evaluating null statement"))

      ; Base Cases
     ((number? lis) (return lis))
     ((eq? lis 'true) (return #t))
     ((eq? lis 'false)  (return #f))
     ((not (list? lis)) (return (find lis s)))

     ; Cases with mathematical operators
     ((eq? (length lis) 1-operand) ((lambda (func) (mvalue-cps (operand1 lis) s (lambda (v) (return (func v))))) (1_op_switch lis)))
     ((eq? (length lis) 2-operand) ((lambda (operator) (cps_tail_recur_helper mvalue-cps (operand1 lis) (operand2 lis) operator s return)) (2_op_switch lis))))))