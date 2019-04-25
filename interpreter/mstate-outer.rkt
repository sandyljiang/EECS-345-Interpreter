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

(define-syntax debug
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(let ((x ,(cadr slist))) (begin (print x) (newline) x)))
  )
)

(define class-def-name
  (lambda (ptree)
    (cadar ptree)))

(define class-def-super
  (lambda (ptree)
    ((lambda (super-list)
      (if (null? super-list)
        super-list
        (cadr super-list)
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
           (mstate-class-body (next-statement ptree)
                   (func ptree env return-error break-error throw-error continue-error)))
         (outer-operator_switch ptree))))))

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
                                             (instance-field-names-def body-env)))

       )
       (mstate-class-body (class-def-body ptree) (initial-body-env))
      )
      env
    )
  )
)

(define declare-var
  (lambda (ptree env return break throw continue)
    (cons (method-list env)
          (cons (static-method-list env)
                (add (method-name ptree) undefined-var (remove-top-layer (remove-top-layer env)))))))

(define declare-static-function
  (lambda (ptree env return break throw continue)
    (cons (method-list env)
          (add-function (func-def-name ptree) (func-def-params ptree) (func-def-body ptree) (remove-top-layer env)))))

