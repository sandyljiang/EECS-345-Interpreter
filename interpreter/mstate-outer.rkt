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

;; Function:    (outer-operator_switch ptree)
;; Parameters:  ptree - parse tree in the format ((statement-op args...) ...)
;; Description: determines the env function to use based on the statement-op in ptree
(define outer-operator_switch
  (lambda (ptree)
    (cond
      ((operator? ptree 'var declare-len)        declare-statement) ; ptree == ((var name) ...)
      ((operator? ptree 'var declare-assign-len) declare-assign-statement) ; ptree == ((var name value) ...)
      ((operator? ptree 'function func-def-len)  function-def-statement)
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
