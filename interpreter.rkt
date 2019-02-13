#lang racket
(provide (all-defined-out))
(require "simpleParser.rkt")
(require "state.rkt")

(define state_op caar)

(define next_operation cdr)

(define return_expr cadar)

(define declare_name cadar)

; calcualte the length of a list using an accumulator
; tail recursion - the last step  in the function body is a recursion call
    ; function returns whatever is in the recursive call
    ; the stack frame just gets reused instead of making a new one because the old value is no longer necessary
        ; can just return the straight value instead of going through all the returns - much faster
    ; this is really only for functional languages like scheme, haskell, etc
(define len-acc
    (lambda (lis acc)
        (if (null? lis)
            acc
            (len-acc (cdr lis) (+ 1 acc))
        )
    )
)

(define len
    (lambda (lis)
        (len-add lis 0)
    )
)

(define mvalue
    (lambda (expr state)
        1
    )
)

(define return_op
    (lambda (ptree state)
        '(() ())
    )
)

(define declare
    (lambda (ptree state)
        (mstate (add (declare_name ptree) 'undefined state))
    )
)



;; takes a parse tree and state and performs the the operations in the parse tree to get the new state\
;; ptree should be in format ((state_op args) (state_op args) ...)
(define mstate
    (lambda (ptree state)
        (cond
            ((null? ptree)
                state
            )
            ((eq? (state_op ptree) 'return) ;; (car ptree): (return value)
                (mstate (next_operation ptree) (return_op ptree state))
            )
            ((and (eq? (state_op ptree) 'var) (eq? (len (car ptree)) 2)) ;; (car ptree): (var name)
                (mstate (next_operation ptree) (mstate))
            )
            (else
                (error 'undefinedoperation)
            )
        )
    )
)

(define interpret
    (lambda (filename)
        (find
            'return
            (mstate
                (parser "simple.txt")
                '(() ())
            )
        )
    )
)