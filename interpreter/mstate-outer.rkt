#lang racket
(provide mstate-outer)
(require "env.rkt")
(require "helper.rkt")
(require "mstate-mvalue.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 3
;;;; mstate-out function declaratons and variable declarations
;;;; *********************************************************************************************************

(define class-def-name
  (lambda (ptree)
    (cadr ptree)))

(define class-def-super
  (lambda (ptree)
    (cdr (caddr ptree)))); is it the (extends A) part that we want? or the A? if A then (cdr ((caddr))

(define method-name
  (lambda (ptree)
    (cadr ptree)))

(define method-names-def
  (lambda (env)
    (cadr (env)))

(define method-closures-def
  (lambda (env)
    (caddr env)))
  
(define method-list
  (lambda (env)
    (current-layer (env))))

(define static-method-names-def
  (lambda (env)
    (cadr env)))

(define static-method-closures-def
  (lambda (env)
    (caddr env)))

(define static-method-list
  (lambda (env)
    (current-layer (remove-top-layer env))))

(define instance-field-names-def
  (lambda (env)
    (cadr env)))

;; defines the initial body environment for the mstate-class-body
(define initial-body-env (push-layer (push-layer (empty-env))))

(define next-statement
  (lambda (ptree)
    (cdr ptree))) ; still confused on this one

;; Function:    (outer-operator_switch ptree)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;; Description: determines the env function to use based on the statement-op in ptree
(define outer-operator_switch
  (lambda (ptree)
    (cond
      ((operator? ptree 'var declare-len)        declare-var) ; ptree == ((var name) ...)
      ((operator? ptree 'function func-def-len)  function-def-statement)
      ((operator? ptree 'static-function func-def-len) declare-static-function)
      (else                                      (undefined-op-error ptree)))))

;; Function:    (mstate-outer ptree env)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;;              env - binding list in the form defined in env.rkt
;; Description: Performs the the operations in the parse tree based on the env to return the new env
(define mstate-class-body
  (lambda (ptree env)
    (cond
      ((null? ptree)
        env)
      (else
        ((lambda (func)
           (mstate-outer (next-statement ptree)
                   (func ptree env return-error break-error throw-error continue-error)))
         (outer-operator_switch ptree))))))

(define mstate-class-def
  (lambda (ptree env)
    (if ((not (null? ptree)))
      ((lambda (body-env)
        (mstate-class-def (next-statement ptree)
                          (add-class-closure env
                                             (class-def-name ptree)
                                             (class-def-super ptree)
                                             (method-names body-env)
                                             (method-closures body-env)
                                             (static-method-names body-env)
                                             (static-method-closures body-env)
                                             (instance-field-names body-env)))

       )
       (mstate-class-body (initial-body-env) body)
      )
    )
  )
)

(define declare-var
  (lambda (env ptree)
    (cons (method-list env)
          (cons (static-method-list env)
                (add (method-name ptree) undefined-var (remove-top-layer (remove-top-layer env)))))))

(define declare-static-function
  (lambda (env ptree)
    (cons (method-list env)
          (add (method-name ptree) undefined-var (remove-top-layer env)))))

