#lang racket
(provide (all-defined-out))

(define names car)
(define values cadr)
(define current-name caar)
(define current-value caadr)
(define next-names cdar)
(define next-values cdadr)

(define null-state
    (lambda (state)
        (and (null? (names state)) (null? (values state)))
    )
)

;; the state is invalid if either names or values are null and the other is not
(define invalid-state
    (lambda (state)
        (or
            (and (null? (names state)) (not (null? (values state))))
            (and (not (null? (names state))) (null? (values state)))
        )
    )
)

(define next-state
    (lambda (state)
        (cons
            (next-names state)
            (list (next-values state))
        )
    )
)

;; state should be in the form ((name1 name2 ...) (value1 value2 ...))
;; valid values are numbers and t/f and 'dne
(define find
    (lambda (name state)
        (cond
            ((invalid-state state) ;; TODO: this way of checking for invalid state only catches it if the state hasnt been found by the time there is and error
                                   ;; for example, (find 'x '((a b x d) (1 2 34))) will return 34 instead of erroring here
                                   ;; not sure if this is a problem though
                (error 'invalidstate)
            )
            ((null-state state)
                #f
            )
            ((eq? (current-name state) name)
                (current-value state)
            )
            (else
                (find name (next-state state))
            )
        )
    )
)

(define add
  (lambda (name value state)
    (if (exists? name state)
      (error 'undefined "name already in state")
      (cons (cons name (names state))
            (list (cons value (values state)))))))

;; Function:    (remove-acc name front state)
;; Parameters:  name     is the name of the binding to remove
;;              front is the accumulator of the bindings list
;;              state   is the bindings list
;; Description: Helper function for function (remove ...) that uses an accumulator to
;;              implement efficient removal of given atom name from the given bindings list.
(define remove-acc
  (lambda (name front state)
    (cond
      ((invalid-state state)
       (error "Error in Remove: Name and value binding mismatch")) ; Raise Error

      ((null-state state) ; Both name and value lists null
       front)

      ((eq? name (current-name state))                      ; Found Element to remove
       (cons (append (names front) (next-names state))
             (list (append (values front) (next-values state))))) ; remove element

      (else                                           ; Recurse onto rest of list
       (remove-acc name
                   (cons (append (names front) (list (current-name state)))
                         (list (append (values front) (list (current-value state)))))
                   (cons (next-names state) (list (next-values state))))))))

;; Function:    (remove name state)
;; Parameters:  name is the name of the binding to remove
;;              state is the binding list
;; Description: Removes given name/atom from the given bindings list.
;;              Removes both the name and bound value.
(define remove
  (lambda (name state)
    (remove-acc name '(() ()) state)))

;; Function:    (exists? name state)
;; Parameters:  name is the name of the binding to check for
;;              state is the binding list
;; Description: Checks if a binding with the given name exists.
;;              Returns #t if it does, #f if not
(define exists?
  (lambda (name state)
    (cond
      ((null-state state)                   #f)
      ((eq? name (current-name state)) #t)
      (else                            (exists? name (next-state state))))))

(define change-value
    (lambda (name newvalue state)
        (add name newvalue (remove name state))
    )
)