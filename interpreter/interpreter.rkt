#lang racket
(provide (all-defined-out))
(require "classParser.rkt")
(require "env.rkt")
(require "mstate-outer.rkt")
(require "mstate-mvalue.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 3
;;;; *********************************************************************************************************


(define-syntax debug
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(let ((x ,(cadr slist))) (begin (print x) (newline) x)))
  )
)

;; Interprets the code in the file specified by filename and returns the value
(define interpret
  (lambda (filename classname)
    (let* ; using let here since we would need 3-4 lambdas to handle all the values used more than once
      ((parse-retval (lambda (retval)
                       (cond
                         ((list? retval)  (error "Error: No return in main function"))
                         ((eq? retval #t) 'true)
                         ((eq? retval #f) 'false)
                         (else            retval))))
       (class-env (mstate-class-def (parser filename) (initial-env)))
       (class-closure (find (string->symbol classname) class-env))
       (main-function-closure (lookup-non-local-function 'main class-closure)))
      (call/cc (lambda (return-cont)
      (display class-env) (newline)
      (display class-closure) (newline)
      (display main-function-closure) (newline)
                 (parse-retval (mstate (closure-body main-function-closure) ; get the parse tree
                                       (push-layer (append ((closure-env main-function-closure)) class-env)) ; get the env
                                       class-closure
                                       '() ; no "this" since main is a static function
                                       (lambda (e v) (return-cont v))
                                       break-error
                                       throw-error
                                       continue-error)))))))
