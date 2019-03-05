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

;; define the value for the throw variable before it gets assigned
(define throw-var 'throw)

;; define the value for an undeclared variable
(define undefined-var 'undefined)

;; define the length of each statement
(define return-len 2)
(define break-len 1)
(define throw-len 2)
(define continue-len 1)
(define declare-len 2)
(define declare-assign-len 3)
(define assign-len 3)
(define begin-len 2)
(define if-len 3)
(define if-else-len 4)
(define while-len 3)
(define try-catch-len 4)

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

;; expression for throw operator
(define throw-expr cadar)

;; expression for statement list in begin block
(define stmt-list cdar)

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

;try catch body statement
(define try-block cadar)
(define catch-block
  (lambda (ptree)
    (cdr (cdr (caddar ptree)))))
(define final-block
  (lambda (ptree)
    (cdr (car (cdddar ptree)))));caddar
(define return-e
   (lambda (ptree)
    (car (cdr (caddar ptree)))))

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

;;;; *********************************************************************************************************
;;;; error functions
;;;; *********************************************************************************************************
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

;; Function:    (return-statement ptree state break throw continue)
;; Parameters:  ptree    - parse tree in the format ((return return-expr) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - the return continuation to call in this function
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: adds a variable with the name in return-var to the state with the value of the
;;              return expression. If one already exists, previous value is overwritten (For cases where
;;              return is in a finally block).
(define return-statement
  (lambda (ptree state return break throw continue)
    ;; make sure the return variable doesn't exist
    ;; (otherwise there are multiple return statements)
    (if (exists? return-var state)
      (return (change-value return-var (mvalue (return-expr ptree) state) state)) ; return a new state with the return value changed
      (multiple-returns-error))))

;;;; *********************************************************************************************************
;;; break operator
;;;; *********************************************************************************************************

;; Function:    (break-statement ptree state return break throw continue)
;; Parameters:  ptree    - parse tree in the format ((break) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - the break continuation to call in this function
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: Calls the break continuation and passes the given state as an argument.
;;              Used to break out of execution of a loop.
(define break-statement
  (lambda (ptree state return break throw continue)
    (break (remove-top-layer state))))

;;;; *********************************************************************************************************
;;; continue operator
;;;; *********************************************************************************************************

;; Function:    (continue-statement ptree state return break throw continue)
;; Parameters:  ptree    - parse tree in the format ((continue) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - the continue continuation to call in this function
;; Description: Calls the continue continuation and passes the given state as an argument.
;;              Used to return to the condition of a loop.
(define continue-statement
  (lambda (ptree state return break throw continue)
    (continue (remove-top-layer state))))

;;;; *********************************************************************************************************
;;;; throw operator
;;;; *********************************************************************************************************

;; Function:    (throw-statement ptree state break throw continue)
;; Parameters:  ptree    - parse tree in the format ((return return-expr) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - the throw continuation to call in this function
;;              continue - a continue continuation
;; Description: Calls the throw continuation and passes the value of the throw expression as an argument.
(define throw-statement
  (lambda (ptree state return break throw continue)
    (throw (add throw-var (mvalue (throw-expr ptree)) (push-layer (remove-top-layer state))))))

;;;; *********************************************************************************************************
;;;; declaration operator
;;;; *********************************************************************************************************

;; Function:    (declare-statement ptree state)
;; Parameters:  ptree    - parse tree in the format ((var var-name) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: adds a new undefined variable variable to the state
(define declare-statement
  (lambda (ptree state return break throw continue)
    (add (var-name ptree) undefined-var state)))

;;;; *********************************************************************************************************
;;;; declaration/assignment operator
;;;; *********************************************************************************************************

;; Function:    (declare-assign-statement ptree state)
;; Parameters:  ptree    - parse tree in the format ((var var-name var-value) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: adds a new variable to the state and assigns it the specified value from the parse tree
(define declare-assign-statement
  (lambda (ptree state return break throw continue)
    ;; extract the name and value from the ptree and add the to the state
    (add (var-name ptree)
         (mvalue (var-value ptree) state)
         state)))

;;;; *********************************************************************************************************
;;;; assignment operator
;;;; *********************************************************************************************************

;; Function:    (assign-statement ptree state)
;; Parameters:  ptree    - parse tree in the format ((= var-name var-value) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: changes the value of the specifed variable in the parse tree to the new value
(define assign-statement
  (lambda (ptree state return break throw continue)
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
;;;; block/begin function
;;;; *********************************************************************************************************

;; Function:    (begin-statement ptree state return break throw continue)
;; Parameters:  ptree    - parse tree in the format (begin (statement-list ...) )
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: Creates a "code block" of statements where a new layer is added to the state for locally scoped
;;              variables and bindings are added, but not visible outside of the block
(define begin-statement
  (lambda (ptree state return break throw continue)
    (remove-top-layer
      (mstate (stmt-list ptree) (push-layer state) return break throw continue))))

;;;; *********************************************************************************************************
;;;; if operator
;;;; *********************************************************************************************************

;; Function:    (if-statement ptree state)
;; Parameters:  ptree    - parse tree in the format ((if if_cond if_body) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: calculate the new state after evaluating the if statement at the beginning of the parse tree
;; Note:        that this function is used when the else-body is NOT included
(define if-statement
  (lambda (ptree state return break throw continue)
    ;; evaluate the if statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate (list (if-body ptree)) state return break throw continue))
        ((eq? condition #f) state) ; condition was false, so don't change the state
        (else               (boolean-mismatch-error condition))))
     (mvalue (if-cond ptree) state))))

;;;; *********************************************************************************************************
;;;; if/else operator
;;;; *********************************************************************************************************

;; Function:    (if-else-statement ptree state)
;; Parameters:  ptree    - parse tree in the format ((if if_cond if_body else_body) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: Calculate the new state after evaluating the
;;              if/else statement at the beginning of the parse tree
;; Note:        This function is used when the else-body IS included
(define if-else-statement
  (lambda (ptree state return break throw continue)
    ;; evaluate the if/else statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate (list (if-body ptree)) state return break throw continue)) ; cond true, so evaluate the if-body
        ((eq? condition #f) (mstate (list (else-body ptree)) state return break throw continue)) ; cond false, so evaluate the else body
        (else               (boolean-mismatch-error condition))))
     (mvalue (if-cond ptree) state))))

;;;; *********************************************************************************************************
;;;; while operator
;;;; *********************************************************************************************************

;; Function:    (while-statement ptree state return break throw continue)
;; Parameters:  ptree parse tree in the format ((while while-cond while-body) ...)
;;              state binding list in the form defined in state.rkt
;; Description: calculate the new state after evaluating the while
;;              statement at the beginning of the parse tree
(define while-statement
  (lambda (ptree state return break throw continue)
    ;; evaluate the while loop based on the value of the while-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t)
          (while-statement ptree
            (call/cc (lambda (c) (mstate (list (while-body ptree)) state return break throw c)))
            return
            break
            throw
            continue)) ; evaluate the body again
        ((eq? condition #f) state) ; done evaluating the while loop
        (else               (boolean-mismatch-error condition))))
     (mvalue (while-cond ptree) state))))

;psuedo code try statement
(define try-statement
  (lambda (ptree state return break throw continue)
    ;; evaluate the try statement based on the block
      (cond
        ((null? (final-block ptree)) state)
        (else (mstate
               ((final-block ptree)
                      (call/cc
                          (lambda (t)
                              (mstate (try-block ptree) state
                                                    (lambda (v) (return   (mstate (final-block (remove-top-layer v) return break throw continue))));return
                                                    (lambda (v) (break    (mstate (final-block (remove-top-layer v) return break throw continue)))) ;break
                                                    (lambda (v) (t        (mstate (catch-block (change-value throw-var (return-e ptree) v) return break throw continue)))) ;throw
                                                    (lambda (v) (continue (mstate (final-block (remove-top-layer v) return break throw continue)))) ;continue
                                                    ))) return break throw continue))))))
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
      ;((operator? ptree 'while while-len)        while-statement) ; ptree == ((while cond body) ...)
      ((operator? ptree '= assign-len)           assign-statement) ; ptree == ((= name newvalue) ...)
      ((operator? ptree 'var declare-len)        declare-statement) ; ptree == ((var name) ...)
      ((operator? ptree 'var declare-assign-len) declare-assign-statement) ; ptree == ((var name value) ...)
      ((operator? ptree 'if if-len)              if-statement) ; ptree == ((if cond body) ...)
      ((operator? ptree 'if if-else-len)         if-else-statement) ; ptree == ((if cond body else-body) ...)
      ((operator? ptree 'break break-len)        break-statement)
      ((operator? ptree 'throw throw-len)        throw-statement)
      ((operator? ptree 'continue continue-len)  continue-statement)
      ((eq? (statement-op ptree) 'begin)         begin-statement)
      ((operator? ptree 'try try-catch-len)      try-statement) ; ptree == ((try block catch block final block) ...)
      (else                                      (undefined-op-error ptree)))))

;; Function:    (mstate ptree state return break throw continue)
;; Parameters:  ptree parse tree in the format ((statement-op args...) ...)
;;              state binding list in the form defined in state.rkt
;; Description: Performs the the operations in the parse tree based on the state to return the new state
(define mstate
  (lambda (ptree state return break throw continue)
    (cond
      ((null? ptree)
        state)
      ((operator? ptree 'while while-len)
        (mstate (next-statement ptree) (call/cc (lambda (b) (while-statement ptree state return b throw continue))) return break throw continue))
      (else
        ((lambda (func) (mstate (next-statement ptree) (func ptree state return break throw continue) return break throw continue))
         (operator_switch ptree))))))