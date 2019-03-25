#lang racket
;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 2
;;;; Mvalue calculation functions
;;;; *********************************************************************************************************
; This file provides
(provide mvalue)

; Dependencies
(require "simpleParser.rkt")
(require "env.rkt")
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

;; Function:    (mvalue expr env)
;; Parameters:  expr is list representing the parse tree
;;              s is the list representing env, which contains the name-value bindings
;; Description: Evaluates the given expression using the given env.
(define mvalue
  (lambda (expr env)
    (cond
      ((null? expr) (error "Error: Evaluating null statement"))

      ; Base cases
      ((number? expr)
        expr)

      ((eq? expr 'true)
        #t)

      ((eq? expr 'false)
        #f)

      ((not (list? expr)) ; if the expression is a variable, lookup the variable
        (find expr env))

      ((eq? (length expr) 1-operand) ; call the 1-operand operator on the operand
        ((lambda (func) (func (mvalue (operand1 expr) env))) (1_op_switch expr)))

      ((eq? (length expr) 2-operand) ; call the 2-operand operator on the operands
        ((lambda (func) (func (mvalue (operand1 expr) env) (mvalue (operand2 expr) env)))
         (2_op_switch expr)
        ))

      (else
        (error "Error: Executing invalid expression.\nExpression: " expr))
    )
  )
)

;; Function:    (mvalue-list param-exprs env throw)
;; Parameters:  exprs - list containing parse tree expressions to be evaluated
;;              env   - the environment to use to evaluate expressions
;;              throw - a throw continuation to pass to the mvalue function evaluating the expressions
;; Description: Evaluates a list of expressions using the mvalue function, the given environment, and
;;              the given throw continuation. Returns a list of the values the expressions evaluate to.
(define mvalue-list
  (lambda (exprs env throw)
    (if (null? exprs)
        '()
        (cons (mvalue (car exprs) env throw) (mvalue-list (cdr exprs) env throw)))))

;; Function:    (mvalue-list-cps param-exprs env throw)
;; Description: Same as mvalue-list above, but uses tail recursion and continuation passing style instead.
(define mvalue-list-cps
  (lambda (exprs env throw)
    ((lambda (cps-func)
      (cps-func exprs env throw (lambda (v) v)))
     (lambda (exprs env throw cps-cont)
       (if (null? exprs)
           (cps-cont '())
           (mvalue-list-cps (cdr exprs) env throw (lambda (v) (cps-cont (cons (mvalue (car exprs) env throw) v)))))
     )
    )
  )
)