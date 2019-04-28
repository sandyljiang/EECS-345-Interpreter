#lang racket
(provide (all-defined-out))
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
    (cadar ptree)))

(define class-def-super
  (lambda (ptree)
    ((lambda (super-list)
      (if (null? super-list)
        super-list
        (cadr super-list) ; need to extract the name and throw out the "extends"
      )
    )
    (caddar ptree))))

(define class-def-body
  (lambda (ptree)
    (car (cdddar ptree))))

(define method-name
  (lambda (ptree)
    (cadar ptree)))

(define method-names-def
  (lambda (env)
    (names env)))

(define method-closures-def
  (lambda (env)
    (values env)))

(define method-list
  (lambda (env)
    (current-layer env)))

(define static-method-names-def
  (lambda (env)
    (names (remove-top-layer env))))

(define static-method-closures-def
  (lambda (env)
    (values (remove-top-layer env))))

(define static-method-list
  (lambda (env)
    (current-layer (remove-top-layer env))))

(define instance-field-names-def
  (lambda (env)
    (names (remove-top-layer (remove-top-layer env)))))

(define instance-field-values-def
  (lambda (env)
    (values (remove-top-layer (remove-top-layer env)))))

;; defines the initial body environment for the mstate-class-body
(define initial-body-env
  (lambda ()
    (push-layer (push-layer empty-env))))

;; Function:    (outer-operator_switch ptree)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;; Description: determines the env function to use based on the statement-op in ptree
(define outer-operator_switch
  (lambda (ptree)
    (cond
      ((operator? ptree 'var declare-len)        declare-var-outer) ; ptree == ((var name) ...)
      ((operator? ptree 'var declare-assign-len) declare-assign-outer)
      ((operator? ptree 'function func-def-len)  declare-function-outer)
      ((operator? ptree 'static-function func-def-len) declare-static-function-outer)
      (else                                      (undefined-op-error ptree)))))

;; Function:    (mstate-class-body ptree env)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;;              env - binding list in the form defined in env.rkt
;; Description: Parses through a ptree to create an environment with all the field necessary to
;;              create a class closure
(define mstate-class-body
  (lambda (ptree env class-name-to-declare)
    (cond
      ((null? ptree)
        env)
      (else
        ((lambda (func)
           (mstate-class-body (next-statement ptree)
                              (func ptree env class-name-to-declare)
                              class-name-to-declare))
         (outer-operator_switch ptree))))))

;; Function:    (mstate-class-def ptree env)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;;              env - binding list in the form defined in env.rkt
;; Description: Parses through a ptree with only class definitions and creates an environment
;;              with all of their closures
(define mstate-class-def
  (lambda (ptree env)
    (if (not (null? ptree))
      ((lambda (body-env)
        (mstate-class-def (next-statement ptree)
                          (add-class-closure env
                                             (class-def-name ptree)
                                             (class-def-super ptree)
                                             (method-names-def body-env)
                                             (method-closures-def body-env)
                                             (static-method-names-def body-env)
                                             (static-method-closures-def body-env)
                                             (instance-field-names-def body-env)
                                             (instance-field-values-def body-env))))
       (mstate-class-body (class-def-body ptree) (initial-body-env) (class-def-name ptree)))
      env)))

;; Function:    (declare-var-outer ptree env class-name-to-declare)
;; Parameters:  ptree                 - parse tree in the format ((var var-name) ...)
;;              env                   - env binding list in the form defined in env.rkt
;;              class-name-to-declare - the name of the class being parsed
;; Description: adds a new undefined variable variable to the instance fields layer of env
(define declare-var-outer
  (lambda (ptree env class-name-to-declare)
    (cons (method-list env)
          (cons (static-method-list env)
                (add (var-name ptree) undefined-var (remove-top-layer (remove-top-layer env)))))))

;; Function:    (declare-assign-outer ptree env class-name-to-declare)
;; Parameters:  ptree                 - parse tree in the format ((var var-name var-value) ...)
;;              env                   - env binding list in the form defined in env.rkt
;;              class-name-to-declare - the name of the class being parsed
;; Description: adds a new variable with value from ptree to the instance fields layer of env
;; Note:        does NOT call mvalue on the variable, just puts it into the field - assuming
;;              only numbers and booleans are valid values in class definition
(define declare-assign-outer
  (lambda (ptree env class-name-to-declare)
    ;; extract the name and value from the ptree and add the to the env
    (cons (method-list env)
          (cons (static-method-list env)
                (add (var-name ptree)
                     (var-value ptree) ; assuming it can only be a number or bool and dont need to mvalue it
                     (remove-top-layer (remove-top-layer env)))))))

;; Function:    (declare-static-function-outer ptree env class-name-to-declare)
;; Parameters:  ptree                 - parse tree in the format
;;                                      ((static-function func-def-name func-def-params func-def-body) ...)
;;              env                   - env binding list in the form defined in env.rkt
;;              class-name-to-declare - the name of the class being parsed
;; Description: adds a new static function closure to the static methods layer of env
(define declare-static-function-outer
  (lambda (ptree env class-name-to-declare)
    (cons (method-list env)
          (add-function (func-def-name ptree)
                        (cons 'this (func-def-params ptree))
                        (func-def-body ptree)
                        (func-def-class-closure class-name-to-declare)
                        (remove-top-layer env)))))

;; Function:    (declare-function-outer ptree env class-name-to-declare)
;; Parameters:  ptree                 - parse tree in the format
;;                                      ((function func-def-name func-def-params func-def-body) ...)
;;              env                   - env binding list in the form defined in env.rkt
;;              class-name-to-declare - the name of the class being parsed
;; Description: adds a new member function closure to the methods layer of env
(define declare-function-outer
  (lambda (ptree env class-name-to-declare)
    (add-function (func-def-name ptree)
                  (cons 'this (func-def-params ptree))
                  (func-def-body ptree)
                  (func-def-class-closure class-name-to-declare)
                  env)))
