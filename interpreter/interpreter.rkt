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

;; Interprets the code in the file specified by filename and returns the value
(define interpret
  (lambda (filename classname)
    ((lambda (retval)
      (cond
        ((list? retval)  (error "Error: No return in main function"))
        ((eq? retval #t) 'true)
        ((eq? retval #f) 'false)
        (else            retval)))
     (mvalue '(funcall main)
             (mstate-class-def (initial-state) (parser filename))
             classname
             throw-error))))
