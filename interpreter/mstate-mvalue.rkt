#lang racket
(provide (all-defined-out))
(require "env.rkt")
(require "helper.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 3
;;;; mstate calculation functions
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
(define func-def-len 4)

; length of statements passed to mvalue
(define operand1 cadr)
(define operand2 caddr)
(define 2-operand 3)
(define 1-operand 2)

;;;; *********************************************************************************************************
;;;; expression/operator location definitions
;;;; *********************************************************************************************************

;; operator for mvalue operators
(define mvalue-statement-op car)

;; operator for any type of statement
(define statement-op caar)

;; the statement currently being evaluated
(define current-statement car)

;; the rest of the statements in the parse tree after the currently selected one
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
    (car (cdddar ptree)))) ; cadddar

;; while condition and body statement
(define while-cond cadar)
(define while-body caddar)

;; catch parse tree
(define catch-section caddar)

;; finally parse tree
(define finally-section
  (lambda (ptree)
    (car (cdddar ptree))))

;; These functions add a 'begin to the parse tree to create a new block for the try body
;; try catch body statement
(define try-block
  (lambda (ptree)
    (list (cons 'begin (cadar ptree)))))

;; catch block body
(define catch-block
  (lambda (ptree)
    (list (cons 'begin (caddr (caddar ptree))))))

;; finally block body
(define final-block
  (lambda (ptree)
    (list (cons 'begin (cadar (cdddar ptree)))))) ; caddar

;; value of the expression passed to the throw operator
(define catch-arg
  (lambda (ptree)
    (caadr (caddar ptree))))

;; The function definition's name
(define func-def-name cadar)

;; The function definition's params
(define func-def-params caddar)

;; The function definition's body
(define func-def-body
  (lambda (ptree)
    (car (cdddar ptree)))) ; cadddar

;; The function call's name
(define func-call-name cadar)

;; The function call's parameter list
(define func-call-params cddar)

;; The function call's name
(define mvalue-func-call-name cadr)

;; The function call's parameter list
(define mvalue-func-call-params cddr)

(define LHS-dot-ptree cadar)
(define RHS-dot-ptree caddar)

;; The right hand side of the dot operator
(define dot-rhs caddr)

;; The left hand side of the dot operator
(define dot-lhs cadr)

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
         (eq? (len (current-statement ptree)) statement-len))))

;; Function:    (has-catch? ptree)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;; Description: checks if there is a catch block associated with the try
(define has-catch?
  (lambda (ptree)
    (not (null? (catch-section ptree)))))

;; Function:    (has-finally? ptree)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;; Description: checks if there is a finally block associated with the try
(define has-finally?
  (lambda (ptree)
    (not (null? (finally-section ptree)))))

;;;; *********************************************************************************************************
;;;; error functions
;;;; *********************************************************************************************************
(define multiple-returns-error
  (lambda () (error "Error: Multiple returns")))

(define assign-error
  (lambda (name) (error "Error: assigning value before declaration\nVariable: " name)))

(define boolean-mismatch-error
  (lambda (condition)
    (error "Error: Invalid condition. Does not evaluate to a boolean.\nCondition: " condition)))

(define undefined-op-error
  (lambda (ptree) (error "Error: Undefined operation.\nParse tree: " ptree)))

(define invalid-try-error
  (lambda (ptree) (error "Error: try without catch or finally. ptree: " ptree)))

(define break-error
  (lambda (env) (error "Error: break outside of loop.\nenv: " env)))

(define throw-error
  (lambda (env) (error "Error: throw outside of try.\nenv: " env)))

(define continue-error
  (lambda (env) (error "Error: continue outside of loop.\nenv: " env)))

(define return-error
  (lambda (env) (error "Error: Invalid use of return. \nenv: " env)))

;;;; *********************************************************************************************************
;;;; return operator
;;;; *********************************************************************************************************

;; Function:    (return-statement ptree env break throw continue)
;; Parameters:  ptree         - parse tree in the format ((return return-expr) ...)
;;              env           - env binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - the return continuation to call in this function
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: adds a variable with the name in return-var to the env with the value of the
;;              return expression. If one already exists, previous value is overwritten
;;              (For cases where return is in a finally block).
(define return-statement
  (lambda (ptree env class-closure instance return break throw continue)
    (return env (mvalue (return-expr ptree) env class-closure instance throw)))) ; pass the return value up

;;;; *********************************************************************************************************
;;; break operator
;;;; *********************************************************************************************************

;; Function:    (break-statement ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format ((break) ...)
;;              env           - env binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - the break continuation to call in this function
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: Calls the break continuation and passes the given env as an argument.
;;              Used to break out of execution of a loop.
(define break-statement
  (lambda (ptree env class-closure instance return break throw continue)
    (break env)))

;;;; *********************************************************************************************************
;;; continue operator
;;;; *********************************************************************************************************

;; Function:    (continue-statement ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format ((continue) ...)
;;              env           - env binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - the continue continuation to call in this function
;; Description: Calls the continue continuation and passes the given env as an argument.
;;              Used to return to the condition of a loop.
(define continue-statement
  (lambda (ptree env class-closure instance return break throw continue)
    (continue env)))

;;;; *********************************************************************************************************
;;;; throw operator
;;;; *********************************************************************************************************

;; Function:    (throw-statement ptree env break throw continue)
;; Parameters:  ptree         - parse tree in the format ((throw throw-expr) ...)
;;              env           - env binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - the throw continuation to call in this function
;;              continue      - a continue continuation
;; Description: Calls the throw continuation and passes the value of the throw expression as an argument.
(define throw-statement
  (lambda (ptree env class-closure instance return break throw continue)
    (throw (change-value throw-var (mvalue (throw-expr ptree) env class-closure instance throw) env))))

;;;; *********************************************************************************************************
;;;; declaration operator
;;;; *********************************************************************************************************

;; Function:    (declare-statement ptree env)
;; Parameters:  ptree         - parse tree in the format ((var var-name) ...)
;;              env           - env binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: adds a new undefined variable variable to the env
(define declare-statement
  (lambda (ptree env class-closure instance return break throw continue)
    (add (var-name ptree) undefined-var env)))

;;;; *********************************************************************************************************
;;;; declaration/assignment operator
;;;; *********************************************************************************************************

;; Function:    (declare-assign-statement ptree env)
;; Parameters:  ptree         - parse tree in the format ((var var-name var-value) ...)
;;              env           - env binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: adds a new variable to the env and assigns it the specified value from the parse tree
(define declare-assign-statement
  (lambda (ptree env class-closure instance return break throw continue)
    ;; extract the name and value from the ptree and add the to the env
    (add (var-name ptree)
         (mvalue (var-value ptree) env class-closure instance throw)
         env)))

;;;; *********************************************************************************************************
;;;; assignment operator
;;;; *********************************************************************************************************

;; Function:    (assign-statement ptree env)
;; Parameters:  ptree         - parse tree in the format ((= var-name var-value) ...)
;;              env           - env binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: changes the value of the specifed variable in the parse tree to the new value
(define assign-statement
  (lambda (ptree env class-closure instance return break throw continue)
    ;; extract the name of the variable name and pass it into the following function
    ((lambda (name)
      ;; make sure the variable has been declared
      (if (exists? name env)
        (change-value name
                (mvalue (var-value ptree) env class-closure instance throw)
                env)
        (assign-error name)))
     (var-name ptree))))

;;;; *********************************************************************************************************
;;;; block/begin function
;;;; *********************************************************************************************************

;; Function:    (begin-statement ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format (begin (statement-list ...))
;;              env           - env binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: Creates a "code block" of statements where a new layer is added to the env for locally
;;              scoped variables and bindings are added, but not visible outside of the block.
(define begin-statement
  (lambda (ptree env class-closure instance return break throw continue)
    (remove-top-layer (mstate (stmt-list ptree)
                              (push-layer env)
                              class-closure
                              instance
                              return
                              (lambda (v) (break (remove-top-layer v)))
                              (lambda (v) (throw (remove-top-layer v)))
                              (lambda (v) (continue (remove-top-layer v)))))))

;;;; *********************************************************************************************************
;;;; if operator
;;;; *********************************************************************************************************

;; Function:    (if-statement ptree env)
;; Parameters:  ptree    - parse tree in the format ((if if_cond if_body) ...)
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: calculate the new env after evaluating the if statement at the beginning of the parse tree
;; Note:        that this function is used when the else-body is NOT included
(define if-statement
  (lambda (ptree env class-closure instance return break throw continue)
    ;; evaluate the if statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate
                             (list (if-body ptree)) env class-closure instance return break throw continue))
        ((eq? condition #f) env) ; condition was false, so don't change the env
        (else               (boolean-mismatch-error condition))))
     (mvalue (if-cond ptree) env class-closure instance throw))))

;;;; *********************************************************************************************************
;;;; if/else operator
;;;; *********************************************************************************************************

;; Function:    (if-else-statement ptree env)
;; Parameters:  ptree         - parse tree in the format ((if if_cond if_body else_body) ...)
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: Calculate the new env after evaluating the
;;              if/else statement at the beginning of the parse tree
;; Note:        This function is used when the else-body IS included
(define if-else-statement
  (lambda (ptree env class-closure instance return break throw continue)
    ;; evaluate the if/else statement based on the value of the if-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t) (mstate
                             (list (if-body ptree)) env class-closure instance return break throw continue))
        ((eq? condition #f) (mstate
                             (list (else-body ptree)) env class-closure instance return break throw continue))
        (else               (boolean-mismatch-error condition))))
     (mvalue (if-cond ptree) env class-closure instance throw))))

;;;; *********************************************************************************************************
;;;; while operator
;;;; *********************************************************************************************************

;; Function:    (while-statement ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format ((while while-cond while-body) ...)
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: calculate the new env after evaluating the while
;;              statement at the beginning of the parse tree
(define while-statement
  (lambda (ptree env class-closure instance return break throw continue)
    ;; evaluate the while loop based on the value of the while-cond
    ((lambda (condition)
      (cond
        ((eq? condition #t)
          (while-statement ptree
                           (call/cc (lambda (continue-env)
                                      (mstate (list (while-body ptree))
                                              env
                                              class-closure
                                              instance
                                              return
                                              break
                                              throw
                                              continue-env)))
                            class-closure
                            instance
                            return
                            break
                            throw
                            continue)) ; evaluate the body again
        ((eq? condition #f) env) ; done evaluating the while loop
        (else               (boolean-mismatch-error condition))))
     (mvalue (while-cond ptree) env class-closure instance throw))))

;;;; *********************************************************************************************************
;;;; try operator
;;;; *********************************************************************************************************

;; Function:    (try-catch ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format
;;                              ((try try-block (catch (catch-arg) catch-block) ()) ...)
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: calculate the new env after evaluating the try-catch
;;              statement at the beginning of the parse tree
(define try-catch
  (lambda (ptree env class-closure instance return break throw continue)
    (call/cc (lambda (exit-catch)
               (mstate (try-block ptree)
                       env
                       class-closure
                       instance
                       return
                       break
                       (lambda (throw-env)
                         (exit-catch (mstate (catch-block ptree)
                                             (add (catch-arg ptree)
                                                  (find throw-var throw-env)
                                                  throw-env)
                                             class-closure
                                             instance
                                             return
                                             break
                                             throw
                                             continue)))
                       continue)))))

;; Function:    (try-finally ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format
;;                              ((try try-block () (finally finally-block)) ...)
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: calculate the new env after evaluating the try-finally
;;              statement at the beginning of the parse tree
(define try-finally
  (lambda (ptree env class-closure instance return break throw continue)
    (mstate (final-block ptree)
            (mstate (try-block ptree)
                    env
                    class-closure
                    instance
                    (lambda (return-env return-value)
                      (begin (mstate (final-block ptree)
                                     return-env
                                     class-closure
                                     instance
                                     return
                                     break
                                     throw
                                     continue)
                             (return return-value)))
                    (lambda (break-env)
                      (break (mstate (final-block ptree)
                                     break-env
                                     class-closure
                                     instance
                                     return
                                     break
                                     throw
                                     continue)))
                    (lambda (throw-env)
                      (throw (mstate (final-block ptree)
                                     throw-env
                                     class-closure
                                     instance
                                     return
                                     break
                                     throw
                                     continue)))
                    (lambda (continue-env)
                      (continue (mstate (final-block ptree)
                                        continue-env
                                        class-closure
                                        instance
                                        return
                                        break
                                        throw
                                        continue))))
            class-closure
            instance
            return
            break
            throw
            continue)))

;; Function:    (try-catch-finally ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format
;;                              ((try try-block (catch (catch-arg) catch-block) (finally finally-block)) ...)
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: calculate the new env after evaluating the try-catch-finally
;;              statement at the beginning of the parse tree
(define try-catch-finally
  (lambda (ptree env class-closure instance return break throw continue)
    (mstate (final-block ptree)
            (call/cc (lambda (exit-catch)
                        (mstate (try-block ptree)
                                env
                                class-closure
                                instance
                                (lambda (return-env return-value)
                                  (begin (mstate (final-block ptree)
                                                 return-env
                                                 class-closure
                                                 instance
                                                 return
                                                 break
                                                 throw
                                                 continue)
                                         (return return-value)))
                                (lambda (break-env)
                                  (break (mstate (final-block ptree)
                                                 break-env
                                                 class-closure
                                                 instance
                                                 return
                                                 break
                                                 throw
                                                 continue)))
                                (lambda (throw-env)
                                  (exit-catch (mstate (catch-block ptree)
                                                      (add (catch-arg ptree)
                                                           (find throw-var throw-env)
                                                           throw-env)
                                                      class-closure
                                                      instance
                                                      return
                                                      break
                                                      throw
                                                      continue)))
                                (lambda (continue-env)
                                  (continue (mstate (final-block ptree)
                                                    continue-env
                                                    class-closure
                                                    instance
                                                    return
                                                    break
                                                    throw
                                                    continue))))))
            class-closure
            instance
            return
            break
            throw
            continue)))

;; Function:    (try-statement ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format
;;                              ((try try-block (catch (catch-arg) catch-block) (finally finally-block)) ...)
;;                              or
;;                              ((try try-block () (finally finally-block)) ...)
;;                              or
;;                              ((try try-block (catch (catch-arg) catch-block) ()) ...)
;;                              or
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: calculate the new env after evaluating the try
;;              statement at the beginning of the parse tree
(define try-statement
  (lambda (ptree env class-closure instance return break throw continue)
    ;; evaluate the try statement based on the block
    (cond
      ((and (not (has-finally? ptree)) (not (has-catch? ptree)))
        (invalid-try-error ptree))
      ((and (not (has-finally? ptree)) (has-catch? ptree)) ; if try-catch
        (try-catch ptree env class-closure instance return break throw continue))
      ((and (has-finally? ptree) (not (has-catch? ptree))) ; if try-finally
        (try-finally ptree env class-closure instance return break throw continue))
      (else                                                ; if try-catch-finally
        (try-catch-finally ptree env class-closure instance return break throw continue)))))

;;;; *********************************************************************************************************
;;;; function definition operator
;;;; *********************************************************************************************************

;; Function:    (function-def-statement ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format
;;                              (function func-name func-param-list func-body)
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: calculate the new env after evaluating function definition
;;              statement at the beginning of the parse tree
(define function-def-statement
  (lambda (ptree env class-closure instance return break throw continue)
    (add-function
     (func-def-name ptree)
     (func-def-params ptree)
     (func-def-body ptree)
     (lambda (current-env) class-closure)
     env)))

;;;; *********************************************************************************************************
;;;; function definition operator
;;;; *********************************************************************************************************

;; Function:    (function-call-statement ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format
;;                              (funcall func-call-name func-call-params)
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: Calculate the new environment after evaluating the function body in the closure
;;              bound to the function being called at the beginning of the parse tree.
(define function-call-statement
  (lambda (ptree env class-closure instance return break throw continue)
    (begin
     ; Getting the func-env from the closure
     ((lambda (closure)
        (call/cc (lambda (return-cont)
                   (mstate (closure-body closure)
                           (add-multiple-vars (closure-params closure)
                                              (mvalue-list (func-call-params ptree) env class-closure instance throw)
                                              (push-layer ((closure-env closure))))
                           class-closure
                           instance
                           (lambda (e v) (return-cont e))
                           break-error
                           (lambda (e) (throw env))
                           continue-error))))
      ; Finds the closure bound to the given function's name, passes into closure param above
      (find (func-call-name ptree) env))
     env)))

;;;; *********************************************************************************************************
;;;; Env Calculation
;;;; *********************************************************************************************************

;; Function:    (operator_switch ptree)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;; Description: determines the env function to use based on the statement-op in ptree
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
      ((operator? ptree 'try try-catch-len)      try-statement) ; ptree == ((try catch final block)...)
      ((operator? ptree 'function func-def-len)  function-def-statement)
      ((eq? (statement-op ptree) 'funcall)       function-call-statement)
      (else                                      (undefined-op-error ptree)))))

;; Function:    (mstate ptree env return break throw continue)
;; Parameters:  ptree         - parse tree in the format ((statement-op args...) ...)
;;              env           - binding list in the form defined in env.rkt
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              return        - a return continuation
;;              break         - a break continuation
;;              throw         - a throw continuation
;;              continue      - a continue continuation
;; Description: Performs the the operations in the parse tree based on the env to return the new env
(define mstate
  (lambda (ptree env class-closure instance return break throw continue)
    (cond
      ((null? ptree)
        env)
      ((operator? ptree 'while while-len)
        (mstate (next-statement ptree)
                (call/cc (lambda (while-break)
                           (while-statement ptree
                                            env
                                            class-closure
                                            instance
                                            return
                                            while-break
                                            throw
                                            continue)))
                return
                break
                throw
                continue))
      (else
        ((lambda (func)
           (mstate (next-statement ptree)
                   (func ptree env class-closure instance return break throw continue)
                   class-closure
                   instance
                   return
                   break
                   throw
                   continue))
         (operator_switch ptree))))))

;;;; ********************************************************************************************************
;;;; Mvalue Helper functions
;;;; ********************************************************************************************************

;; Function:    (dot-value expr env class-closure instance throw)
;; Parameters:  expr          - list representing the parse tree in the form ((dot LHS-dot RHS-dot) ...)
;;              env           - the environment to use to evaluate expressions
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              throw         - a throw continuation to pass to the mvalue function evaluating the
;;                              expressions
;; Description: Evaluates the dot expression using the given env.
(define dot-value
  (lambda (expr env class-closure instance throw)
    (lookup-instance-fields (RHS-dot-ptree expr)
                            (get-dot-LHS (LHS-dot-ptree expr) env class-closure instance throw))))

;; Function:    (mvalue-operator? statement operator)
;; Parameters:  statement - the parsed statement to evaluate. First element should be
;;                          the operator represented by an atom
;;              operator  - the operator to check for.
;;              len       - the number of expected elements in the list (including operator
;;                          and operands)
;; Description: Helper function that compares operator of given list to given operator.
;;              Returns true if it matches, false if not.
(define mvalue-operator?
  (lambda (statement operator)
    (eq? (mvalue-statement-op statement) operator)))

;; Function:    (2_op_switch expr)
;; Parameters:  expr - the list that represents the parse tree. Must contain an operator
;;                     as the first element, then two operands as the subsequent elemnts.
;; Description: Returns the correct function to use for the given operation.
(define 2_op_switch
  (lambda (expr)
    (cond
      ;; cases with arithmetic operators
      ((mvalue-operator? expr '+)  +)
      ((mvalue-operator? expr '-)  -)
      ((mvalue-operator? expr '*)  *)
      ((mvalue-operator? expr '/)  quotient)
      ((mvalue-operator? expr '%)  remainder)

      ;; Cases with comparison operators
      ((mvalue-operator? expr '==)  eq?)
      ((mvalue-operator? expr '!=)  (lambda (op1 op2) (not (eq? op1 op2))))
      ((mvalue-operator? expr '< )  <)
      ((mvalue-operator? expr '> )  >)
      ((mvalue-operator? expr '<=)  <=)
      ((mvalue-operator? expr '>=)  >=)
      ((mvalue-operator? expr '&&)  (lambda (op1 op2) (and op1 op2)))
      ((mvalue-operator? expr '||)  (lambda (op1 op2) (or op1 op2)))



      ;; Operator not recognized
      (else                        (error "Error: Executing invalid expression.\nExpression: " expr)))))

;; Function:    (1_op_switch expr)
;; Parameters:  expr - the list that represents the parse tree. Must contain an operator
;;                     as the first element, then one operands as the subsequent elemnt.
;; Description: Returns the correct function to use for the given operation.
(define 1_op_switch
  (lambda (expr)
    (cond
      ((mvalue-operator? expr '-) (lambda (op1) (* -1 op1)))
      ((mvalue-operator? expr '!) (lambda (op1) (not op1)))
      (else                       (error "Error: Executing invalid expression.\nExpression: " expr)))))

;;;; ********************************************************************************************************
;;;; Mvalue
;;;; ********************************************************************************************************

(define get-dot-LHS
  (lambda (LHS-of-dot env class-closure instance throw)
    (mvalue (list LHS-of-dot) env class-closure instance throw)))

;; Function:    (mvalue expr env)
;; Parameters:  expr - list representing the parse tree
;;              env           - the environment to use to evaluate expressions
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              throw         - a throw continuation to pass to the mvalue function evaluating the
;;                              expressions
;; Description: Evaluates the given expression using the given env.
(define mvalue
  (lambda (expr env class-closure instance throw)
    (cond
      ((null? expr)
        (error "Error: Evaluating null statement"))
      ((number? expr)
        expr)
      ((eq? expr 'true)
        #t)
      ((eq? expr 'false)
        #f)
      ((not (list? expr)) ; if the expression is a variable, lookup the variable
        (find expr env)) ;; TODO: change to call lookup function
      ((eq? (mvalue-statement-op expr) 'funcall)
        (if (list? (mvalue-func-call-name expr)) ; If the function call is a dot operator
            (let* ((LHS (get-dot-LHS (dot-lhs (mvalue-func-call-name expr)) env class-closure instance throw))
                   (RHS (lookup-function-closure (dot-rhs (mvalue-func-call-name expr)) LHS)))
                 (mstate (closure-body closure)
                                  (add-multiple-vars (cons 'this (closure-params closure))
                                                     (cons LHS (mvalue-list (mvalue-func-call-params expr) env class-closure instance throw))
                                                     (push-layer ((closure-env closure))))
                                  class-closure
                                  instance
                                  (lambda (e v) (return-cont v))
                                  break-error
                                  (lambda (e) (throw env))
                                  continue-error)
            )

              ; if there is a dot operator, then evaluate the LHS to get the object-closure
              ;    then get the function closure of the RHS from the object closure of the LHS
              ;    cons the object closure onto the params of the function call
              ;    evaluate the function with the new param list and the class-closure as the (get-class-closure object-type)
              ; if there is not a dot operator, then look up the function closure in the env
              ;    cons the containing instance onto the new param list to indicate that this DNE
              ;    evaluate the function with the new param list and the class-closure as the (get-class-closure object-type)

              ;;; note that you now need the instance in everything. also the class-name must now be class-closure
            ((lambda (closure) ; Getting the func-env from the closure
              (call/cc (lambda (return-cont)
                (mstate (closure-body closure)
                                  (add-multiple-vars (cons 'this (closure-params closure))
                                                     (cons instance (mvalue-list (mvalue-func-call-params expr) env class-closure instance throw))
                                                     (push-layer ((closure-env closure))))
                                  class-closure
                                  instance
                                  (lambda (e v) (return-cont v))
                                  break-error
                                  (lambda (e) (throw env))
                                  continue-error))))
            (lookup-function-closure (mvalue-func-call-name expr) env class-closure)))
        
      ((eq? (mvalue-statement-op expr) 'dot)
        (dot-value expr env class-closure instance throw)
      ((eq? (length expr) 1-operand) ; call the 1-operand operator on the operand
        ((lambda (func) (func (mvalue (operand1 expr) env class-closure instance throw))) (1_op_switch expr)))

      ((eq? (length expr) 2-operand) ; call the 2-operand operator on the operands
        ((lambda (func) (func (mvalue (operand1 expr) env class-closure throw) (mvalue (operand2 expr) env class-closure instance throw)))
         (2_op_switch expr)))
      (else
        (error "Error: Executing invalid expression.\nExpression: " expr)))))

;; Function:    (mvalue-list param-exprs env throw)
;; Parameters:  exprs         - list containing parse tree expressions to be evaluated
;;              env           - the environment to use to evaluate expressions
;;              class-closure - the class-closure object that is currently in scope
;;              instance      - the object closure that is currently being used
;;              throw         - a throw continuation to pass to the mvalue function evaluating the
;;                              expressions
;; Description: Evaluates a list of expressions using the mvalue function, the given environment,
;;              and the given throw continuation. Returns a list of the values the expressions
;;              evaluate to.
(define mvalue-list
  (lambda (exprs env class-closure instance throw)
    (if (null? exprs)
        '()
        (cons (mvalue (car exprs) env throw) (mvalue-list (cdr exprs) env class-closure instance throw)))))
