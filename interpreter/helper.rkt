#lang racket
(provide len)
(provide return-var)
(provide throw-var)

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 1
;;;; Helper functions for the project
;;;; *********************************************************************************************************

;; define the name of the return variable that the interpreter will look for at the end of the program
(define return-var 'return)

;; define the value for the throw variable before it gets assigned
(define throw-var 'throw)

;; Function:    (len-acc lis acc)
;; Parameters:  lis the list to find the length of
;;              acc the accumulator that stores the current length
;; Description: calculate the length of a list using an accumulator
(define len-acc
  (lambda (lis acc)
    (if (null? lis)
      acc
      (len-acc (cdr lis) (+ 1 acc)))))

;; Function:    (len lis)
;; Parameters:  lis the list to find the length of
;; Description: calculate the length of a list
(define len
  (lambda (lis)
    (len-acc lis 0)))