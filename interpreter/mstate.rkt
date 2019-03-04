#lang racket
(provide mstate)
(provide return-var)
(provide multiple-returns-error)
(provide assign-error)
(provide boolean-mismatch-error)
(provide undefined-op-error)
(require "state.rkt")
(require "mvalue.rkt")
(require "helper.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 1
;;;; Mstate calculation functions
;;;; *********************************************************************************************************

;;;; *********************************************************************************************************
;;;; constants
;;;; *********************************************************************************************************

;; define the name of the return variable that the interpreter will look for at the end of the program
(define return-var 'return)

;; define the value for an undeclared variable
(define undefined-var 'undefined)

;; define the length of each statement
(define return-len 2)
(define declare-len 2)
(define declare-assign-len 3)
(define assign-len 3)
(define if-len 3)
(define if-else-len 4)
(define while-len 3)

;;;; *********************************************************************************************************
;;;; expression/operator location definitions
;;;; *********************************************************************************************************

;; operator for any type of statement
(define statement-op caar)

;; the statement currently being evaluated
(define current-statement car)

;; the rest of the statements int eh parse tree after the currently selected one
(define next-statement cdr)

;; expression for return operator
(define return-expr cadar)

;; name of variable being declared/assigned
(define var-name cadar)

;; value a variable is being assigned
(define var-value caddar)

;; if condition, body statement, and else body statement
(define if-cond cadar)
(define if-body caddar)
(define else-body
  (lambda (ptree)
    (car (cdddar ptree)))) ; cadddar

;; while condition and budy statement
(define while-cond cadar)
(define while-body caddar)

;;;; *********************************************************************************************************
;;;; helper functions
;;;; *********************************************************************************************************

;; Function:    (operator? ptree op statement-len)
;; Parameters:  ptree         parse tree in the format ((statement-op args...) ...)
;;              op            an atom that the statement-op should be checked against
;;              statement-len the expected length of the statement that op corresponds to
;; Description: checks if the current statement in the parse tree matches the specified
;;              operation and the expected length of the statement for that operation
(define operator?
  (lambda (ptree op statement-len)
    (and (eq? (statement-op ptree) op)
         (eq? (len (current-statement ptree)) statement-len))))

(define multiple-returns-error
  (lambda () (error "Error: Multiple returns")))

(define assign-error
  (lambda (name) (error "Error: assigning value before declaration\nVariable: " name)))

(define boolean-mismatch-error
  (lambda (condition) (error "Error: Invalid condition. Does not evaluate to a boolean.\nCondition: "
                             condition)))

(define undefined-op-error
  (lambda (ptree) (error "Error: Undefined operation.\nParse tree: " ptree)))

;;;; *********************************************************************************************************
;;;; return operator
;;;; *********************************************************************************************************

;; Function:    (return-statement ptree state)
;; Parameters:  ptree parse tree in the format ((return return-expr) ...)
;;              state binding list in the form defined in state.rkt
;; Description: adds a variable with the name in return-var to the state with the value of the
;;              return expression
(define return-statement
  (lambda (ptree state)
    ;; make sure the return variable doesn't exist
    ;; (otherwise there are multiple return statements)
    (if (exists? return-var state)
      (multiple-returns-error)
      (add return-var (mvalue (return-expr ptree) state) state)))) ; return a new state with the return value

;;;; *********************************************************************************************************
;;;; declaration operator
;;;; *********************************************************************************************************

;; Function:    (declare-statement ptree state)
;; Parameters:  ptree parse tree in the format ((var var-name) ...)
;;              state binding list in the form defined in state.rkt
;; Description: adds a new undefined variable variable to the state
(define declare-statement
  (lambda (ptree state)
    (add (var-name ptree) undefined-var state)))

;;;; *********************************************************************************************************
;;;; declaration/assignment operator
;;;; *********************************************************************************************************

;; Function:    (declare-assign-statement ptree state)
;; Parameters:  ptree parse tree in the format ((var var-name var-value) ...)
;;              state binding list in the form defined in state.rkt
;; Description: adds a new variable to the state and assigns it the specified value from the parse tree
(define declare-assign-statement
  (lambda (ptree state)
    ;; extract the name and value from the ptree and add the to the state
    (add (var-name ptree)
         (mvalue (var-value ptree) state)
         state)))

;;;; *********************************************************************************************************
;;;; assignment operator
;;;; *********************************************************************************************************

;; Function:    (assign-statement ptree state)
;; Parameters:  ptree parse tree in the format ((= var-name var-value) ...)
;;              state binding list in the form defined in state.rkt
;; Description: changes the value of the specifed variable in the parse tree to the new value
(define assign-statement
  (lambda (ptree state)
    ;; extract the name of the variable name and pass it into the following function
    ((lambda (name)
      ;; make sure the variable has been declared
      (if (exists? name state)
        (change-value name
                (mvalue (var-value ptree) state)
                state)
        (assign-error name)))
     (var-name ptree))))

;;;; *********************************************************************************************************
;;;; if operator
;;;; *********************************************************************************************************

;; Function:    (if-statement ptree state)
;; Parameters:  ptree parse tree in the format ((if if_cond if_body) ...)
;;              state binding list in the form defined in state.rkt
;; Description: calculate the new state after evaluating the if statement at the beginning of the parse tree
;; Note:        that this function is used when the else-body is NOT included
(define if-statement
  (lambda (ptree state)
    ;; evaluate the if statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate (list (if-body ptree)) state))
        ((eq? condition #f) state) ; condition was false, so don't change the state
        (else               (boolean-mismatch-error))))
     (mvalue (if-cond ptree) state))))

;;;; *********************************************************************************************************
;;;; if/else operator
;;;; *********************************************************************************************************

;; Function:    (if-else-statement ptree state)
;; Parameters:  ptree parse tree in the format ((if if_cond if_body else_body) ...)
;;              state binding list in the form defined in state.rkt
;; Description: Calculate the new state after evaluating the
;;              if/else statement at the beginning of the parse tree
;; Note:        This function is used when the else-body IS included
(define if-else-statement
  (lambda (ptree state)
    ;; evaluate the if/else statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate (list (if-body ptree)) state)) ; cond true, so evaluate the if-body
        ((eq? condition #f) (mstate (list (else-body ptree)) state)) ; cond false, so evaluate the else body
        (else               (boolean-mismatch-error))))
     (mvalue (if-cond ptree) state))))

;;;; *********************************************************************************************************
;;;; while operator
;;;; *********************************************************************************************************

;; Function:    (while-statement ptree state)
;; Parameters:  ptree parse tree in the format ((while while-cond while-body) ...)
;;              state binding list in the form defined in state.rkt
;; Description: calculate the new state after evaluating the while
;;              statement at the beginning of the parse tree
(define while-statement
  (lambda (ptree state)
    ;; evaluate the while loop based on the value of the while-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (while-statement ptree (mstate (list (while-body ptree)) state))) ; evaluate the body again
        ((eq? condition #f) state) ; done evaluating the while loop
        (else               (boolean-mismatch-error))))
     (mvalue (while-cond ptree) state))))

;;;; *********************************************************************************************************
;;;; State Calculation
;;;; *********************************************************************************************************

;; Function:    (operator_switch ptree)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;; Description: determines the state function to use based on the statement-op in ptree
(define operator_switch
  (lambda (ptree)
    (cond
      ((operator? ptree 'return return-len)      return-statement) ; ptree == (((return value) ...)
      ((operator? ptree 'while while-len)        while-statement) ; ptree == ((while cond body) ...)
      ((operator? ptree '= assign-len)           assign-statement) ; ptree == ((= name newvalue) ...)
      ((operator? ptree 'var declare-len)        declare-statement) ; ptree == ((var name) ...)
      ((operator? ptree 'var declare-assign-len) declare-assign-statement) ; ptree == ((var name value) ...)
      ((operator? ptree 'if if-len)              if-statement) ; ptree == ((if cond body) ...)
      ((operator? ptree 'if if-else-len)         if-else-statement) ; ptree == ((if cond body else-body) ...)
      (else                                      (undefined-op-error ptree)))))

;; Function:    (mstate ptree state)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;;              state binding list in the form defined in state.rkt
;; Description: Performs the the operations in the parse tree based on the state to return the new state
(define mstate
  (lambda (ptree state)
    (if (null? ptree)
        state
        ((lambda (func) (mstate (next-statement ptree) (func ptree state)))
         (operator_switch ptree)))))