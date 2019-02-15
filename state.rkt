#lang racket
(provide (all-defined-out))

;; TODO: define the binding list format
;; state should be in the form ((name1 name2 ...) (value1 value2 ...))
;; valid values are numbers and t/f and 'dne

(define names car)
(define values cadr)
(define current-name caar)
(define current-value caadr)
(define next-names cdar)
(define next-values cdadr)

;; Function:    (null-state? state)
;; Parameters:  state the binding list to check if it is an empty state
;; Description: the state is invalid if the names and values lists inside it are null
;;              ie state == '(() ())
(define null-state?
  (lambda (state)
    (and (null? (names state)) (null? (values state)))))

;; Function:    (invalid-state? state)
;; Parameters:  state the binding list to check if valid
;; Description: the state is invalid if either names or values are null and the other is not
(define invalid-state?
  (lambda (state)
    (or
      (and (null? (names state)) (not (null? (values state))))
      (and (not (null? (names state))) (null? (values state))))))

;; Function:    (next-state state)
;; Parameters:  state the binding list to find the next state from
;; Description: Effectively like calling cdr for the binding list.
;;              takes the cdrs of the names and values and creates a new binding list using those lists
(define next-state
  (lambda (state)
    (cons (next-names state)
          (list (next-values state)))))

;; Function:    (find name state)
;; Parameters:  name  the name of the variable to find int he state
;;              state the binding list to search
;; Description: Searches the state for the name and returns the associated value
(define find
  (lambda (name state)
    (cond ;; TODO: fix formatting
      ((invalid-state? state)
        (error "Error: Invalid state given " state))

      ((null-state? state)
        (error "Error: Variable not defined " name))

      ((and (eq? (current-name state) name) (eq? (current-value state) 'undefined))
        (error "Error: Using variable '" name "' before definition"))

      ((eq? (current-name state) name)
        (current-value state))

      (else
        (find name (next-state state))))))

;; Function:    (add name value state)
;; Parameters:  name  the name of the variable to add to the state
;;              value the value to associate with the name
;;              state the binding list to add the name/value pair to
;; Description: Adds a new name/value pair to the state.
;; Note:        This function throws an error if the value it is adding already exists in the state
(define add
  (lambda (name value state)
    (if (exists? name state)
      (error "Error: Double declaration of variable.\nVariable: " name)
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
      ((invalid-state? state)
       (error "Error in Remove: Name and value binding mismatch\nState: " state)) ; Raise Error

      ((null-state? state) ; Both name and value lists null
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
      ((null-state? state)             #f)
      ((eq? name (current-name state)) #t)
      (else                            (exists? name (next-state state))))))

;; Function:    (change-value name new-value state)
;; Parameters:  name      the name of the variable to change in the state
;;              new-value the new value to assign to the variable name
;;              state     the binding list to change the value in
;; Description: Changes a variable in the state to a have a new value.
;; Note:        This function does not change the state if name is not in the state
(define change-value
    (lambda (name new-value state)
        (add name new-value (remove name state))
    )
)