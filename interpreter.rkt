#lang racket
(provide (all-defined-out))
(require "simpleParser.rkt")
(require "state.rkt")

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
(define else-body cadddar)

;; while condition and budy statement
(define while-cond cadar)
(define while-body caddar)

;;;; *********************************************************************************************************
;;;; helper functions
;;;; *********************************************************************************************************

; calculate the length of a list using an accumulator
(define len-acc
  (lambda (lis acc)
    (if (null? lis)
      acc
      (len-acc (cdr lis) (+ 1 acc))
    )
  )
)

;; calculate the length of a list
(define len
  (lambda (lis)
    (len-add lis 0)
  )
)

;;;; *********************************************************************************************************
;;;; return operator
;;;; *********************************************************************************************************

;; checks the parse tree for the return operator in the statment-op
;; param ptree: list in the format ((statement-op args...) ...)
;; returns:     true if statement-op == return-var
(define return-op?
  (lambda (ptree)
    (eq? (statement-op ptree) return-var)
  )
)

;; adds a variable with the name in return-var to the state with the value of the
;; return expression
;; param ptree: list in the form ((return return-expr) ...)
;; param state: list in the form defined in state.rkt
;; returns:     the new state after the assignment of the return variable
(define return-op
  (lambda (ptree state)
    ;; make sure the return variable doesn't exist
    ;; (otherwise there are multiple return statements)
    (if (exists? return-var state)
      (error 'multiplereturns)
      (add return-var (mvalue return-expr state) state) ; return a new state with the return value
    )
  )
)

;;;; *********************************************************************************************************
;;;; declaration operator
;;;; *********************************************************************************************************

;; checks the parse tree for the declare operator in the statment-op
;; param ptree: list in the format ((statement-op args...) ...)
;; returns:     true if statement-op == 'var and there is only one argument
(define declare-op?
  (lambda (ptree)
    (and (eq? (statement-op ptree) 'var) (eq? (len ptree) 2))
  )
)

;; adds a new undefined variable variable to the state
;; param ptree: list in the form ((var name) ...)
;; param state: list in the form defined in state.rkt
;; returns:     the new state after the declaration
(define declare-op
  (lambda (ptree state)
    (add (var-name ptree) undefined-var)
  )
)

;;;; *********************************************************************************************************
;;;; declaration/assignment operator
;;;; *********************************************************************************************************

;; checks the parse tree for the declare-assign operator in the statment-op
;; param ptree: list in the format ((statement-op args...) ...)
;; returns:     true if statement-op == 'var and there are exactly two arguments
(define declare-assign-op?
  (lambda (ptree)
    (and (eq? (statement-op ptree) 'var) (eq? (len ptree) 3))
  )
)

;; adds a new variable to the state and assigns it the specified value from the parse tree
;; param ptree: list in the form ((var name value) ...)
;; param state: list in the form defined in state.rkt
;; returns:     the new state after the declaration and assignment
(define declare-assign
  (lambda (ptree state)
    ;; extract the name and value from the ptree and add the to the state
    (add (var-name ptree)
       (mvalue (var-value ptree) state)
       state)
  )
)

;;;; *********************************************************************************************************
;;;; assignment operator
;;;; *********************************************************************************************************

;; checks the parse tree for the assignment operator in the statment-op
;; param ptree: list in the format ((statement-op args...) ...)
;; returns:     true if statement-op == '=
(define assign-op?
  (lambda (ptree)
    (eq? (statement-op ptree) '=)
  )
)

;; changes the value of the specifed variable in the parse tree to the new value
;; param ptree: list in the form ((= name newvalue) ...)
;; param state: list in the form defined in state.rkt
;; returns:     the new state after the assignment
(define assign
  (lambda (ptree state)
    ;; extract the name of the variable name and pass it into the following function
    ((lambda (name)
      ;; make sure the variable has been declared
      (if (exists? name state)
        (change-value name
                (mvalue (var-value ptree) state)
                state)
        (error 'assignbeforeuse)
      )
     )
     (var-name ptree)
    )
  )
)

;;;; *********************************************************************************************************
;;;; if operator
;;;; *********************************************************************************************************

;; checks the parse tree for the if operator in the statment-op
;; param ptree: list in the format ((statement-op args...) ...)
;; returns:     true if statement-op == 'if and there are exactly two arguments
(define if-op?
  (lambda (ptree)
    (and (eq? (statement-op ptree) 'if) (eq? (len ptree) 3))
  )
)

;; calculate the new state after evaluating the if statement at the beginning of the parse tree
;; note that this function is used when the else-body is NOT included
;; param ptree: list in the form ((if if-cond if-body) ...)
;; param state: list in the form defined in state.rkt
;; returns:     the new state after evaluating the if statement
(define if-op
  (lambda (ptree state)
    ;; evaluate the if statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate (if-body ptree) state))
        ((eq? condition #f) state) ; condition was false, so don't change the state
        (else       (error 'invalidcondition))
      )
     )
     (mvalue (if-cond ptree) state)
    )
  )
)

;;;; *********************************************************************************************************
;;;; if/else operator
;;;; *********************************************************************************************************

;; checks the parse tree for the if/else operator in the statment-op
;; param ptree: list in the format ((statement-op args...) ...)
;; returns:     true if statement-op == 'if and there are exactly three arguments
(define if-else-op?
  (lambda (ptree)
    (and (eq? (statement-op ptree) 'if) (eq? (len ptree) 4))
  )
)

;; calculate the new state after evaluating the if/else statement at the beginning of the parse tree
;; note that this function is used when the else-body IS included
;; param ptree: list in the form ((if if-cond if-body else-body) ...)
;; param state: list in the form defined in state.rkt
;; returns:     the new state after evaluating the if/else statement
(define if-else-op
  (lambda (ptree state)
    ;; evaluate the if/else statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate (if-body ptree) state)) ; cond true, so evaluate the if-body
        ((eq? condition #f) (mstate (else-body ptree) state)) ; cond false, so evaluate the else body
        (else               (error 'invalidcondition))
      )
     )
     (mvalue (if-cond ptree) state)
    )
  )
)

;;;; *********************************************************************************************************
;;;; while operator
;;;; *********************************************************************************************************

;; checks the parse tree for the while operator in the statment-op
;; param ptree: list in the format ((statement-op args...) ...)
;; returns:     true if statement-op == 'while
(define while-op?
  (lambda (ptree)
    (eq? (statement-op ptree) 'while)
  )
)

;; calculate the new state after evaluating the while statement at the beginning of the parse tree
;; param ptree: list in the form ((while while-cond while-body) ...)
;; param state: list in the form defined in state.rkt
;; returns:     the new state after evaluating the while statement
(define while-op
  (lambda (ptree state)
    ;; evaluate the while loop based on the value of the while-cond
    ((lambda (b)
      (cond
        ((eq? b #t) (while-op ptree (mstate (while-body ptree) state))) ; evaluate the body again
        ((eq? b #f) state) ; done evaluating the while loop
        (else       (error 'invalidcondition))))
     (mvalue (while-cond ptree) state)
    )
  )
)

;;;; *********************************************************************************************************
;;;; value and state calculation
;;;; *********************************************************************************************************

;; Calculate the value of an expression from the parse tree
;; note that this function applies to boolean and arithmetic operations
;; Variables used in the expression are pulled from the state
(define mvalue
  (lambda (expr state)
    1
  )
)

(define operator_switch
  (lambda (ptree)
    (cond
      ((return-op? ptree) return-op) ; ptree == (((return value) ...)

      ((while-op? ptree) while-op) ; ptree == ((while cond body) ...)

      ((assign-op? ptree) assign-op) ; ptree == ((= name newvalue) ...)

      ((declare-op? ptree) declare-op) ; ptree == ((var name) ...)

      ((declare-assign-op? ptree) declare-assign-op) ; ptree == ((var name value) ...)

      ((if-op? ptree) if-op) ; ptree == ((if cond body) ...)

      ((if-else-op? ptree) if-else-op) ; ptree == ((if cond body else-body) ...)

      (else
        (error 'undefinedoperation)))))

;; takes a parse tree and state and performs the the operations in the parse tree to get the new state
;; param ptree: list in the format ((statement-op args...) ...)
;; param state: list in the form defined in state.rkt
;; returns:     the state after evaluating all statements in the parse tree
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