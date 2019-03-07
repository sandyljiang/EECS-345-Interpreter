#lang racket
(provide (all-defined-out))
(require "helper.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 2
;;;; State handling functions
;;;; *********************************************************************************************************

;; definition for a layer with no values in it
(define null-layer '(() ()))

;; definition for the starting state with one null alyer in it
(define empty-state (list null-layer))

;; definition for the return value for find when the variable is not found
(define undeclared-var 'undeclared)

;;;; *********************************************************************************************************
;;;; error functions
;;;; *********************************************************************************************************
(define undeclared-error
  (lambda (name) (error "Error: Variable not declared.\nVariable: " name)))

(define undefined-error
  (lambda (name) (error "Error: Using variable before definition.\nVariable: " name)))

(define double-declare-error
  (lambda (name) (error "Error: Double declaration of variable.\nVariable: " name)))

(define invalid-state-error
  (lambda (state) (error "Error: Invalid state given.\nState: " state)))

;;;; *********************************************************************************************************
;;;; State format
;;;;
;;;; The state will be stored in a binding list with layers with the following format:
;;;; (((name1 name2 ...) (value1 value2 ...)) ((namea nameb ...) (valuea valueb ...)) ...)
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

;; Function:    (initial-state)
;; Description: creates the initial state for the interpreter which has
;;              undefined 'throw and 'return variables in it
(define initial-state
  (lambda ()
    (add throw-var undefined-var (add return-var undefined-var empty-state))
  )
)

;; Function:    (null-layer? state)
;; Parameters:  state the binding list to check if the top layer is empty
;; Description: the top layer is null if the names and values lists inside it are null
;;              ie layer == '(() ())
(define null-layer?
  (lambda (state)
    (and (null? (names state)) (null? (values state)))
  )
)

;; Function:    (null-state? state)
;; Parameters:  state the binding list to check if it is an empty state
;; Description: the state is null if all layers are empty or there are no layers
(define null-state?
  (lambda (state)
    (cond
      ((null? state) ; all layers in state were empty
        #t)

      ((null-layer? state) ; top layer is empty, so check the next one
        (null-state? (next-layer state)))

      (else ; layer/state was not empty
        #f)
    )
  )
)

;; Function:    (invalid-state? state)
;; Parameters:  state the binding list to check if top layer is valid
;; Description: the state is invalid if either names or values are null and the other is not
(define invalid-layer?
  (lambda (state)
    (or (and (null? (names state)) (not (null? (values state))))
        (and (not (null? (names state))) (null? (values state))))
  )
)

;; Function:    (next-state state)
;; Parameters:  state the binding list to find the next state from
;; Description: Effectively like calling cdr for the binding list.
;;              removes the first name and value from the list and returns the result
(define next-state
  (lambda (state)
    (cons (cons (next-names state)
                (list (next-values state)))
          (next-layer state))
  )
)

;; Function:    (push-layer state)
;; Parameters:  state the binding list to add a new empty layer to
;; Description: Adds a new empty layer to the state
(define push-layer
  (lambda (state)
    (cons null-layer state)
  )
)

;; Function:    (remove-top-layer state)
;; Parameters:  state the binding list to remove the top layer from
;; Description: Removes the top layer in the state
(define remove-top-layer
  (lambda (state)
    (next-layer state)
  )
)

;; Function:    (find-box name state)
;; Parameters:  name  the name of the variable to find in the state
;;              state the binding list to search
;; Description: Searches the state for the name and returns a box with the associated value
(define find-box
  (lambda (name state)
    (cond
      ((null-state? state) ; reached an empty state, so the variable does not exist
        undeclared-var)

      ((invalid-layer? state) ; state is corrupt
        (invalid-state-error state))

      ((null-layer? state) ; variable was not in this layer, so check the next
        (find-box name (next-layer state)))

      ((eq? (current-name state) name) ; found the variable
        (current-value state))

      (else ; recurse on the state without the current name and value
        (find-box name (next-state state)))
    )
  )
)

;; Function:    (find name state)
;; Parameters:  name  the name of the variable to find in the state
;; Description: Searches the state for the name and returns the associated value
;; Note:        The function throws an error is the variable was not found or is undefined
(define find
  (lambda (name state)
    (unbox ((lambda (box-found)
              (cond
                ((eq? box-found undeclared-var)
                  (undeclared-error name))
                ((eq? (unbox box-found) undefined-var)
                  (undefined-error name))
                (else
                  box-found)
              )
            )
            (find-box name state)
           ))
  )
)

;; Function:    (add name value state)
;; Parameters:  name  the name of the variable to add to the state
;;              value the value to associate with the name
;;              state the binding list to add the name/value pair to
;; Description: Adds a new name/value pair to the top layer of the state.
;; Note:        This function throws an error if the value it is adding already exists in the state
(define add
  (lambda (name value state)
    (if (exists-in-top-layer? name state)
      (double-declare-error name)
      (cons (cons (cons name (names state))
                  (list (cons (box value) (values state))))
            (next-layer state))
    )
  )
)

;; Function:    (exists-in-top-layer? name state)
;; Parameters:  name  the name of the variable to check if it is in the top
;;                    layer of the state
;;              state the binding list to check for the name in the top layer
;; Description: Checks if a variable exists in the top layer of the state.
;;              returns #t if it does, #f otherwise
(define exists-in-top-layer?
  (lambda (name state)
    (cond
      ((null-state? state)             #f)
      ((null-layer? state)             #f)
      ((eq? name (current-name state)) #t)
      (else                            (exists-in-top-layer? name (next-state state)))
    )
  )
)

;; Function:    (exists? name state)
;; Parameters:  name  is the name of the binding to check for
;;              state is the binding list
;; Description: Checks if a binding with the given name exists anywhere in the state.
;;              Returns #t if it does, #f if not
(define exists?
  (lambda (name state)
    (cond
      ((null-state? state)               #f)
      ((exists-in-top-layer? name state) #t)
      (else                              (exists? name (next-layer state)))
    )
  )
)



;; Function:    (change-value name new-value state)
;; Parameters:  name      the name of the variable to change in the state
;;              new-value the new value to assign to the variable name
;;              state     the binding list to change the value in
;; Description: Changes a variable in the state to a have a new value.
;; Note:        This function does not change the state if name is not in the state
;; Note:        The variable changed is the one in the highest layer in the state
(define change-value
  (lambda (name new-value state)
    ((lambda (box-found)
       (cond
         ((eq? box-found undeclared-var) state)
         (else                           (begin (set-box! box-found new-value) state))
       )
     )
     (find-box name state)
    )
  )
)