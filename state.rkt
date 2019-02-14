#lang racket
(provide (all-defined-out))

(define names car)
(define values cadr)
(define current_name caar)
(define current_value caadr)
(define next_names cdar)
(define next_values cdadr)

(define null_state
    (lambda (state)
        (and (null? (names state)) (null? (values state)))
    )
)

;; the state is invalid if either names or values are null and the other is not
(define invalid_state
    (lambda (state)
        (or
            (and (null? (names state)) (not (null? (values state))))
            (and (not (null? (names state))) (null? (values state)))
        )
    )
)

(define next_state
    (lambda (state)
        (cons
            (next_names state)
            (cons (next_values state) '())
        )
    )
)

;; state should be in the form ((name1 name2 ...) (value1 value2 ...))
;; valid values are numbers and t/f and 'dne
(define find
    (lambda (name state)
        (cond
            ((invalid_state state) ;; TODO: this way of checking for invalid state only catches it if the state hasnt been found by the time there is and error
                                   ;; for example, (find 'x '((a b x d) (1 2 34))) will return 34 instead of erroring here
                                   ;; not sure if this is a problem though
                (error 'invalidstate)
            )
            ((null_state state)
                #f
            )
            ((eq? (current_name state) name)
                (current_value state)
            )
            (else
                (find name (next_state state))
            )
        )
    )
)

(define remove
    (lambda (name state)
        '(() ())
    )
)

(define add
    (lambda (name value state)
        '(() ())
    )
)

(define exists?
    (lambda (name state)
        #t
    )
)

(define change_value
    (lambda (name newvalue state)
        (add name newvalue (remove name state))
    )
)