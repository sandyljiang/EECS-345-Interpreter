#lang racket
(provide (all-defined-out))
(require "simpleParser.rkt")
(require "state.rkt")
(require "mstate.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 1
;;;; *********************************************************************************************************

(define break-error
  (lambda (state) (error "Error: break outside of loop.\nstate: " state)))

(define throw-error
  (lambda (state) (error "Error: throw outside of try.\nstate: " state)))

(define continue-error
  (lambda (state) (error "Error: continue outside of loop.\nstate: " state)))

;; Interprets the code in the file specified by filename and returns the value
(define interpret
  (lambda (filename)
    ;; convert boolean values from the return value to the correct format
    ((lambda (v)
      (cond
        ((eq? v #t) 'true)
        ((eq? v #f) 'false)
        (else       v)))
     ;; interpret the code and get the return value
     (find return-var
       (call/cc
         (lambda (return)
           (mstate (parser filename)
                   (initial-state)
                   return
                   break-error
                   throw-error
                   continue-error
           )
         )
       )
     )
    )
  )
)
