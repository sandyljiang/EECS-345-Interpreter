#lang racket
(provide (all-defined-out))
(require "simpleParser.rkt")
(require "state.rkt")
(require "mvalue.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 1
;;;; *********************************************************************************************************

;;;; *********************************************************************************************************
;;;; string constants
;;;; *********************************************************************************************************

;; define the name of the return variable that the interpreter will look for at the end of the program
(define return-var 'return)

;; define the value for an undeclared variable
(define undefined-var 'undefined)

;;;; *********************************************************************************************************
;;;; expression/operator location definitions
;;;; *********************************************************************************************************

;; operator for any type of statement
(define statement-op caar)

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
    (car (cdddar ptree))))

;; while condition and budy statement
(define while-cond cadar)
(define while-body caddar)

;;;; *********************************************************************************************************
;;;; helper functions
;;;; *********************************************************************************************************

;; Function:    (len-acc lis acc)
;; Parameters:  lis the list to find the length of
;;              acc the accumulator that stores the current length
;; Description: calculate the length of a list using an accumulator
(define len-acc
  (lambda (lis acc)
    (if (null? lis)
      acc
      (len-acc (cdr lis) (+ 1 acc)))))

;; Function:    (len lis)
;; Parameters:  lis the list to find the length of
;; Description: calculate the length of a list
(define len
  (lambda (lis)
    (len-add lis 0)))

;;;; *********************************************************************************************************
;;;; return operator
;;;; *********************************************************************************************************

;; Function:    (return-op? ptree)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;; Description: checks the parse tree for the return operator in the statment-op
(define return-op?
  (lambda (ptree)
    (eq? (statement-op ptree) return-var)))

;; Function:    (return-op ptree state)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;;              state binding list in the form defined in state.rkt
;; Description: adds a variable with the name in return-var to the state with the value of the
;;              return expression
(define return-op
  (lambda (ptree state)
    ;; make sure the return variable doesn't exist
    ;; (otherwise there are multiple return statements)
    (if (exists? return-var state)
      (error 'multiplereturns)
      (add return-var (mvalue return-expr state) state)))) ; return a new state with the return value

;;;; *********************************************************************************************************
;;;; declaration operator
;;;; *********************************************************************************************************

;; Function:    (declare-op? ptree)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;; Description: checks the parse tree for the declare operator in the statment-op
(define declare-op?
  (lambda (ptree)
    (and (eq? (statement-op ptree) 'var) (eq? (len ptree) 2))))

;; Function:    (declare-op ptree state)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;;              state binding list in the form defined in state.rkt
;; Description: adds a new undefined variable variable to the state
(define declare-op
  (lambda (ptree state)
    (add (var-name ptree) undefined-var)))

;;;; *********************************************************************************************************
;;;; declaration/assignment operator
;;;; *********************************************************************************************************

;; Function:    (declare-assign-op? ptree)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;; Description: checks the parse tree for the declare-assign operator in the statment-op
(define declare-assign-op?
  (lambda (ptree)
    (and (eq? (statement-op ptree) 'var) (eq? (len ptree) 3))))

;; Function:    (declare-assign-op ptree state)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;;              state binding list in the form defined in state.rkt
;; Description: adds a new variable to the state and assigns it the specified value from the parse tree
(define declare-assign
  (lambda (ptree state)
    ;; extract the name and value from the ptree and add the to the state
    (add (var-name ptree)
       (mvalue (var-value ptree) state)
       state)))

;;;; *********************************************************************************************************
;;;; assignment operator
;;;; *********************************************************************************************************

;; Function:    (assign-op? ptree)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;; Description: checks the parse tree for the assignment operator in the statment-op
(define assign-op?
  (lambda (ptree)
    (eq? (statement-op ptree) '=)))

;; Function:    (assign-op ptree state)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;;              state binding list in the form defined in state.rkt
;; Description: changes the value of the specifed variable in the parse tree to the new value
(define assign
  (lambda (ptree state)
    ;; extract the name of the variable name and pass it into the following function
    ((lambda (name)
      ;; make sure the variable has been declared
      (if (exists? name state)
        (change-value name
                (mvalue (var-value ptree) state)
                state)
        (error 'assignbeforeuse)))
     (var-name ptree))))

;;;; *********************************************************************************************************
;;;; if operator
;;;; *********************************************************************************************************

;; Function:    (if-op? ptree)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;; Description: checks the parse tree for the if operator in the statment-op
(define if-op?
  (lambda (ptree)
    (and (eq? (statement-op ptree) 'if) (eq? (len ptree) 3))))

;; Function:    (if-op ptree state)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;;              state binding list in the form defined in state.rkt
;; Description: calculate the new state after evaluating the if statement at the beginning of the parse tree
;; Note:        that this function is used when the else-body is NOT included
(define if-op
  (lambda (ptree state)
    ;; evaluate the if statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate (if-body ptree) state))
        ((eq? condition #f) state) ; condition was false, so don't change the state
        (else       (error 'invalidcondition))))
     (mvalue (if-cond ptree) state))))

;;;; *********************************************************************************************************
;;;; if/else operator
;;;; *********************************************************************************************************

;; Function:    (if-else-op? ptree)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;; Description: checks the parse tree for the if/else operator in the statment-op
(define if-else-op?
  (lambda (ptree)
    (and (eq? (statement-op ptree) 'if) (eq? (len ptree) 4))))

;; Function:    (if-else-op ptree state)
;; Parameters:  ptree parse tree in the format ((if if_cond if_body else_body) ...)
;;              state binding list in the form defined in state.rkt
;; Description: Calculate the new state after evaluating the
;;              if/else statement at the beginning of the parse tree
;; Note:        This function is used when the else-body IS included
(define if-else-op
  (lambda (ptree state)
    ;; evaluate the if/else statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate (if-body ptree) state)) ; cond true, so evaluate the if-body
        ((eq? condition #f) (mstate (else-body ptree) state)) ; cond false, so evaluate the else body
        (else               (error 'invalidcondition))))
     (mvalue (if-cond ptree) state))))

;;;; *********************************************************************************************************
;;;; while operator
;;;; *********************************************************************************************************

;; Function:    (while-op? ptree)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;; Description: checks the parse tree for the while operator in the statment-op
(define while-op?
  (lambda (ptree)
    (eq? (statement-op ptree) 'while)))

;; Function:    (while-op ptree state)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;;              state binding list in the form defined in state.rkt
;; Description: calculate the new state after evaluating the while
;;              statement at the beginning of the parse tree
(define while-op
  (lambda (ptree state)
    ;; evaluate the while loop based on the value of the while-cond
    ((lambda (b)
      (cond
        ((eq? b #t) (while-op ptree (mstate (while-body ptree) state))) ; evaluate the body again
        ((eq? b #f) state) ; done evaluating the while loop
        (else       (error 'invalidcondition))))
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
      ((return-op? ptree)         return-op) ; ptree == (((return value) ...)
      ((while-op? ptree)          while-op) ; ptree == ((while cond body) ...)
      ((assign-op? ptree)         assign-op) ; ptree == ((= name newvalue) ...)
      ((declare-op? ptree)        declare-op) ; ptree == ((var name) ...)
      ((declare-assign-op? ptree) declare-assign-op) ; ptree == ((var name value) ...)
      ((if-op? ptree)             if-op) ; ptree == ((if cond body) ...)
      ((if-else-op? ptree)        if-else-op) ; ptree == ((if cond body else-body) ...)
      (else                       (error 'undefinedoperation)))))

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

;;;; *********************************************************************************************************
;;;; Interpreter
;;;; *********************************************************************************************************

;; Interprets the code in the file specified by filename and returns the value
(define interpret
  (lambda (filename)
    (find return-var
          (mstate (parser "simple.txt")
                  '(() ())))))