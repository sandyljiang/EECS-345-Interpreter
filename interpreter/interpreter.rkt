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
    ;; convert boolean values from the return value to the correct format
    ((lambda (v)
      (cond
        ((eq? v #t) 'true)
        ((eq? v #f) 'false)
        (else       v)
      )
     )
     ;; interpret the code and get the return value
     ((lambda (v)
        (if (not (list? v))
          v
          (error "no return")
        )
      )
      (call/cc (lambda (return)
                 (mvalue '(funcall main)
                         (mstate-outer (parser filename)
                                       (initial-env))
                         throw-error))
               )
      )
     )
    )
  )
