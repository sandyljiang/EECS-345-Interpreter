#lang racket
(require "simpleParser.rkt")
(require "state.rkt")

; calculate the length of a list using an accumulator
(define len-acc
  (lambda (lis acc)
    (if (null? lis)
        acc
        (len-acc (cdr lis) (+ 1 acc)))))

(define len
  (lambda (lis)
    (len-acc lis 0)))

;; Function:    (mvalue lis s)
;; Parameters:  lis is list representing the parse tree
;;              s is the list representing state, which contains the name-value bindings
;; Description: Evaluates the given expression using the given state.
(define mvalue
  (lambda (lis s)
    (cond
      [(null? lis) (error 'undefined "undefined lis")]

      ; Base cases
      [(number? lis) lis]
      [(eq? lis 'true) #t]
      [(eq? lis 'false) #f]
      [(not (list? lis)) (find lis s)]

      ; Cases with mathematical operators
      [(eq? (op lis) '+) (+ (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(and (eq? (op lis) '-) (eq? (len lis) '3)) (- (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(and (eq? (op lis) '-) (eq? (len lis) '2)) (* -1 (mvalue (opd1 lis) s))]
      [(eq? (op lis) '*) (* (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(eq? (op lis) '/) (quotient (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(eq? (op lis) '%) (remainder (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]

      ; Cases with comparison operators
      [(eq? (op lis) '==) (eq? (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(eq? (op lis) '!=) (not (eq? (mvalue (opd1 lis) s) (mvalue (opd2 lis) s)))]
      [(eq? (op lis) '<)  (<   (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(eq? (op lis) '>)  (>   (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(eq? (op lis) '<=) (<=  (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(eq? (op lis) '>=) (>=  (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(eq? (op lis) '&&) (and (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(eq? (op lis) '||) (or  (mvalue (opd1 lis) s) (mvalue (opd2 lis) s))]
      [(eq? (op lis) '!)  (not (mvalue (opd1 lis) s))])))

; The abstracted functions
(define op car)
(define opd1 cadr)
(define opd2 caddr)

