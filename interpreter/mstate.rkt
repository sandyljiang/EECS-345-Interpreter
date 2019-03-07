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
;;;; Interpreter Part 2
;;;; Mstate calculation functions
;;;; *********************************************************************************************************

;;;; *********************************************************************************************************
;;;; constants
;;;; *********************************************************************************************************

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

;; expression passed to the return operator
(define return-expr cadar)

;; expression passed to the throw operator
(define throw-expr cadar)

;; the statement list in begin block
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
    (car (cdddar ptree)) ; cadddar
  )
)

;; while condition and body statement
(define while-cond cadar)
(define while-body caddar)

;; catch parse tree
(define catch-section caddar)
;; finally parse tree
(define finally-section
  (lambda (ptree)
    (car (cdddar ptree))
  )
)

;; These functions add a 'begin to the parse tree to create a new block for the try body
;; try catch body statement
(define try-block
  (lambda (ptree)
    (list (cons 'begin (cadar ptree)))
  )
)
;; catch block body
(define catch-block
  (lambda (ptree)
    (list (cons 'begin (caddr (caddar ptree))))
  )
)
;; finally block body
(define final-block
  (lambda (ptree)
    (list (cons 'begin (cadar (cdddar ptree)))) ; caddar
  )
)
;; value of the expression passed to the throw operator
(define catch-arg
  (lambda (ptree)
    (caadr (caddar ptree))
  )
)


;;;; *********************************************************************************************************
;;;; helper functions
;;;; *********************************************************************************************************

;; Function:    (operator? ptree op statement-len)
;; Parameters:  ptree         - parse tree in the format ((statement-op args...) ...)
;;              op            - an atom that the statement-op should be checked against
;;              statement-len - the expected length of the statement that op corresponds to
;; Description: checks if the current statement in the parse tree matches the specified
;;              operation and the expected length of the statement for that operation
(define operator?
  (lambda (ptree op statement-len)
    (and (eq? (statement-op ptree) op)
         (eq? (len (current-statement ptree)) statement-len))
  )
)

;; Function:    (has-catch? ptree)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;; Description: checks if there is a catch block associated with the try
(define has-catch?
  (lambda (ptree)
    (not (null? (catch-section ptree)))
  )
)

;; Function:    (has-finally? ptree)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;; Description: checks if there is a finally block associated with the try
(define has-finally?
  (lambda (ptree)
    (not (null? (finally-section ptree)))
  )
)

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

(define invalid-try-error
  (lambda (ptree) (error "Error: try without catch or finally. ptree: " ptree)))

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
      (multiple-returns-error)
    )
  )
)

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
    (break state)
  )
)

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
    (continue state)
  )
)

;;;; *********************************************************************************************************
;;;; throw operator
;;;; *********************************************************************************************************

;; Function:    (throw-statement ptree state break throw continue)
;; Parameters:  ptree    - parse tree in the format ((throw throw-expr) ...)
;;              state    - state binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - the throw continuation to call in this function
;;              continue - a continue continuation
;; Description: Calls the throw continuation and passes the value of the throw expression as an argument.
(define throw-statement
  (lambda (ptree state return break throw continue)
    (throw (change-value throw-var (mvalue (throw-expr ptree) state) state))
  )
)

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
    (add (var-name ptree) undefined-var state)
  )
)

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
         state)
  )
)

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
     (var-name ptree)
    )
   )
 )

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
      (mstate (stmt-list ptree)
              (push-layer state)
              return
              (lambda (v) (break (remove-top-layer v)))
              (lambda (v) (throw (remove-top-layer v)))
              (lambda (v) (continue (remove-top-layer v)))))
   )
 )

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
        (else               (boolean-mismatch-error condition))
      )
     )
     (mvalue (if-cond ptree) state)
    )
   )
)

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
        (else               (boolean-mismatch-error condition))
      )
     )
     (mvalue (if-cond ptree) state)
    )
   )
)

;;;; *********************************************************************************************************
;;;; while operator
;;;; *********************************************************************************************************

;; Function:    (while-statement ptree state return break throw continue)
;; Parameters:  ptree - parse tree in the format ((while while-cond while-body) ...)
;;              state - binding list in the form defined in state.rkt
;; Description: calculate the new state after evaluating the while
;;              statement at the beginning of the parse tree
(define while-statement
  (lambda (ptree state return break throw continue)
    ;; evaluate the while loop based on the value of the while-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t)
          (while-statement ptree
                           (call/cc (lambda (continue-state)
                                      (mstate (list (while-body ptree))
                                              state
                                              return
                                              break
                                              throw
                                              continue-state)
                            ))
                            return
                            break
                            throw
                            continue)) ; evaluate the body again
        ((eq? condition #f) state) ; done evaluating the while loop
        (else               (boolean-mismatch-error condition))
      )
     )
     (mvalue (while-cond ptree) state)
    )
  )
)

;;;; *********************************************************************************************************
;;;; try operator
;;;; *********************************************************************************************************

;; Function:    (try-catch ptree state return break throw continue)
;; Parameters:  ptree    - parse tree in the format
;;                         ((try try-block (catch (catch-arg) catch-block) ()) ...)
;;              state    - binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: calculate the new state after evaluating the try-catch
;;              statement at the beginning of the parse tree
(define try-catch
  (lambda (ptree state return break throw continue)
    (call/cc (lambda (exit-catch)
               (mstate (try-block ptree)
                       state
                       return
                       break
                       (lambda (throw-state)
                         (exit-catch (mstate (catch-block ptree)
                                             (add (catch-arg ptree)
                                                  (find throw-var throw-state)
                                                  throw-state)
                                             return
                                             break
                                             throw
                                             continue))
                       )
                       continue)
    ))
  )
)

;; Function:    (try-finally ptree state return break throw continue)
;; Parameters:  ptree    - parse tree in the format
;;                         ((try try-block () (finally finally-block)) ...)
;;              state    - binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: calculate the new state after evaluating the try-finally
;;              statement at the beginning of the parse tree
(define try-finally
  (lambda (ptree state return break throw continue)
    (mstate (final-block ptree)
            (mstate (try-block ptree)
                    state
                    (lambda (return-state)
                      (return (mstate (final-block ptree)
                                      return-state
                                      return
                                      break
                                      throw
                                      continue))
                    )
                    (lambda (break-state)
                      (break (mstate (final-block ptree)
                                     break-state
                                     return
                                     break
                                     throw
                                     continue))
                    )
                    (lambda (throw-state)
                      (throw (mstate (final-block ptree)
                                     throw-state
                                     return
                                     break
                                     throw
                                     continue))
                    )
                    (lambda (continue-state)
                      (continue (mstate (final-block ptree)
                                        continue-state
                                        return
                                        break
                                        throw
                                        continue))
                    ))
            return
            break
            throw
            continue)
  )
)

;; Function:    (try-catch-finally ptree state return break throw continue)
;; Parameters:  ptree    - parse tree in the format
;;                         ((try try-block (catch (catch-arg) catch-block) (finally finally-block)) ...)
;;              state    - binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: calculate the new state after evaluating the try-catch-finally
;;              statement at the beginning of the parse tree
(define try-catch-finally
  (lambda (ptree state return break throw continue)
    (mstate (final-block ptree)
            (call/cc (lambda (exit-catch)
                        (mstate (try-block ptree)
                                state
                                (lambda (return-state)
                                  (return (mstate (final-block ptree)
                                                  return-state
                                                  return
                                                  break
                                                  throw
                                                  continue))
                                )
                                (lambda (break-state)
                                  (break (mstate (final-block ptree)
                                                 break-state
                                                 return
                                                 break
                                                 throw
                                                 continue))
                                )
                                (lambda (throw-state)
                                  (exit-catch (mstate (catch-block ptree)
                                                      (add (catch-arg ptree)
                                                           (find throw-var throw-state)
                                                           throw-state)
                                                      return
                                                      break
                                                      throw
                                                      continue))
                                )
                                (lambda (continue-state)
                                  (continue (mstate (final-block ptree)
                                                    continue-state
                                                    return
                                                    break
                                                    throw
                                                    continue))
                                ))
            ))
            return
            break
            throw
            continue)
  )
)

;; Function:    (try-statement ptree state return break throw continue)
;; Parameters:  ptree    - parse tree in the format
;;                         ((try try-block (catch (catch-arg) catch-block) (finally finally-block)) ...)
;;                         or
;;                         ((try try-block () (finally finally-block)) ...)
;;                         or
;;                         ((try try-block (catch (catch-arg) catch-block) ()) ...)
;;                         or
;;              state    - binding list in the form defined in state.rkt
;;              return   - a return continuation
;;              break    - a break continuation
;;              throw    - a throw continuation
;;              continue - a continue continuation
;; Description: calculate the new state after evaluating the try
;;              statement at the beginning of the parse tree
(define try-statement
  (lambda (ptree state return break throw continue)
    ;; evaluate the try statement based on the block
    (cond
      ((and (not (has-finally? ptree)) (not (has-catch? ptree)))
        (invalid-try-error ptree))

      ((and (not (has-finally? ptree)) (has-catch? ptree)) ; if try-catch
        (try-catch ptree state return break throw continue))

      ((and (has-finally? ptree) (not (has-catch? ptree))) ; if try-finally
        (try-finally ptree state return break throw continue))

      (else                                                ; if try-catch-finally
        (try-catch-finally ptree state return break throw continue))
    )
  )
)


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
      (else                                      (undefined-op-error ptree))
    )
  )
)

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
        (mstate (next-statement ptree)
                (call/cc (lambda (while-break)
                           (while-statement ptree
                                            state
                                            return
                                            while-break
                                            throw
                                            continue)
                ))
                return
                break
                throw
                continue))
      (else
        ((lambda (func)
           (mstate (next-statement ptree)
                   (func ptree state return break throw continue)
                   return
                   break
                   throw
                   continue))
         (operator_switch ptree)
        ))
    )
  )
)
