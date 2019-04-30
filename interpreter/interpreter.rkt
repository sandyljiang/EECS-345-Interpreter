#lang racket
(provide (all-defined-out))
(require "classParser.rkt")
(require "env.rkt")
(require "mstate-outer.rkt")
(require "mstate-mvalue.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 4
;;;; *********************************************************************************************************

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
      (parse-retval (call/cc (lambda (return-cont)
                               (mstate (closure-body main-function-closure) ; get the parse tree
                                       ; get the env
                                       (push-layer ((closure-env main-function-closure) '()))
                                       class-closure
                                       '() ; no "this" since main is a static function
                                       (lambda (e v) (return-cont v))
                                       break-error
                                       throw-error
                                       continue-error)))))))
