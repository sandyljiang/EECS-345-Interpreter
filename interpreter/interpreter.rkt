#lang racket
(provide (all-defined-out))
(require "functionParser.rkt")
(require "env.rkt")
(require "mstate-outer.rkt")
(require "mstate.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 3
;;;; *********************************************************************************************************

;; Interprets the code in the file specified by filename and returns the value
(define interpret
  (lambda (filename)
    ((lambda (retval)
      (cond
        ((list? retval)  (error "Error: No return in main function"))
        ((eq? retval #t) 'true)
        ((eq? retval #f) 'false)
        (else            retval)))
     (call/cc (lambda (return)
                (mvalue '(funcall main)
                        (mstate-outer (parser filename)
                                      (initial-env))
                        throw-error))))))
