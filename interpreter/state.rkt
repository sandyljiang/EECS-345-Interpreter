#lang racket
(provide (all-defined-out))

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 1
;;;; State handling functions
;;;; *********************************************************************************************************

(define null-layer '(() ()))
(define initial-state (list null-layer))
(define undeclared-var 'undeclared)
(define undefined-var 'undefined)

;;;; *********************************************************************************************************
;;;; State format
;;;;
;;;; The state will be stored in a binding list with the following format:
;;;; ((name1 name2 ...) (value1 value2 ...))
;;;;
;;;; The (name1 name2 ...) sublist is the names list
;;;; the (value1 value2 ...) sublist is the values list
;;;; *********************************************************************************************************

(define names caar)
(define values cadar)
(define current-name caaar)
(define current-value caadar)
(define next-names cdaar)
(define next-values cdadar)
(define current-layer car)
(define next-layer cdr)

;; Function:    (null-state? state)
;; Parameters:  state the binding list to check if it is an empty state
;; Description: the layer is null if the names and values lists inside it are null
;;              ie layer == '(() ())
(define null-layer?
  (lambda (state)
    (and (null? (names state)) (null? (values state)))))

(define null-state?
  (lambda (state)
    (cond
      ((null? state)
        #t
      )
      ((null-layer? state)
        (null-state? (next-layer state))
      )
      (else
        #f
      )
    )
  )
)

;; Function:    (invalid-state? state)
;; Parameters:  state the binding list to check if valid
;; Description: the state is invalid if either names or values are null and the other is not
(define invalid-layer?
  (lambda (state)
    (or (and (null? (names state)) (not (null? (values state))))
        (and (not (null? (names state))) (null? (values state))))))

;; Function:    (next-state state)
;; Parameters:  state the binding list to find the next state from
;; Description: Effectively like calling cdr for the binding list.
;;              takes the cdrs of the names and values and creates a new binding list using those lists
(define next-state
  (lambda (state)
    (cons (cons (next-names state)
                (list (next-values state)))
          (next-layer state))))

(define push-layer
  (lambda (state)
    (cons null-layer state)
  )
)

(define remove-top-layer
  (lambda (state)
    (next-layer state)
  )
)

;; Function:    (find name state)
;; Parameters:  name  the name of the variable to find in the state
;;              state the binding list to search
;; Description: Searches the state for the name and returns the associated value
(define find-box
  (lambda (name state)
    (cond
      ((null-state? state)
        undeclared-var)

      ((invalid-layer? state)
        (error "Error: Invalid state given.\nState: " state))

      ((null-layer? state)
        (find-box name (next-layer state)))

      ((eq? (current-name state) name)
        (current-value state))

      (else
        (find-box name (next-state state))))))

(define find
  (lambda (name state)
    (unbox ((lambda (box-found)
              (cond
                ((eq? box-found undeclared-var)
                  (error "Error: Variable not declared.\nVariable: " name))
                ((eq? (unbox box-found) undefined-var)
                  (error "Error: Using variable before definition.\nVariable: " name))
                (else
                  box-found)
              )
            )
            (find-box name state)
           )
    )
  )
)

;; Function:    (add name value state)
;; Parameters:  name  the name of the variable to add to the state
;;              value the value to associate with the name
;;              state the binding list to add the name/value pair to
;; Description: Adds a new name/value pair to the state.
;; Note:        This function throws an error if the value it is adding already exists in the state
(define add
  (lambda (name value state)
    (if (exists-in-top-layer? name state)
      (error "Error: Double declaration of variable.\nVariable: " name)
      (cons (cons (cons name (names state))
                  (list (cons (box value) (values state))))
            (next-layer state)))))

(define exists-in-top-layer?
  (lambda (name state)
    (cond
      ((null-state? state)
        #f
      )
      ((null-layer? state)
        #f
      )
      ((eq? name (current-name state))
        #t
      )
      (else
        (exists-in-top-layer? name (next-state state))
      )
    )
  )
)

;; Function:    (exists? name state)
;; Parameters:  name is the name of the binding to check for
;;              state is the binding list
;; Description: Checks if a binding with the given name exists.
;;              Returns #t if it does, #f if not
(define exists?
  (lambda (name state)
    (cond
      ((null-state? state)               #f)
      ((exists-in-top-layer? name state) #t)
      (else                              (exists? name (next-layer state))))))



;; Function:    (change-value name new-value state)
;; Parameters:  name      the name of the variable to change in the state
;;              new-value the new value to assign to the variable name
;;              state     the binding list to change the value in
;; Description: Changes a variable in the state to a have a new value.
;; Note:        This function does not change the state if name is not in the state
(define change-value
  (lambda (name new-value state)
    ((lambda (box-found)
       (cond
         ((eq? box-found undeclared-var)
           state)
         (else
           (begin (set-box! box-found new-value) state))
       )
     )
     (find-box name state)
     )
  )
)