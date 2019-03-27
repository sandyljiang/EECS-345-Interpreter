#lang racket
(provide (all-defined-out))
(require "helper.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 3
;;;; env handling functions
;;;; *********************************************************************************************************

;; definition for a layer with no values in it
(define null-layer '(() ()))

;; definition for the starting env with one null layer in it
(define empty-env (list null-layer))

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

(define invalid-env-error
  (lambda (env) (error "Error: Invalid env given.\nenv: " env)))

(define name-value-length-error
  (lambda (names values)
    (error "Error: names list does not map 1-1 with values list.\nNames:" names 'Values: values)
  )
)

;;;; *********************************************************************************************************
;;;; env format
;;;;
;;;; The env will be stored in a binding list with layers with the following format:
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

;;;; *********************************************************************************************************
;;;; Closure format
;;;;
;;;; The closure for a function will be stored in a list with the following format
;;;; (param-list func-body (lambda () get-env)
;;;;
;;;; The param-list is itself a list of atoms that represent the names of the parameters for the function.
;;;; The func-body is a parse tree that defines what the function does.
;;;; The get-env lambda function is a function that returns the environment that represents the
;;;;     bindings available for the function.  It is a lambda instead of just a env to allow recursion
;;;;     to work because the function needs to be able to access itself which means that it needs to be added
;;;;     to the env that is associated with itself.
;;;; *********************************************************************************************************

(define closure-params car) ; returns the param list of the function
(define closure-body cadr)  ; returns the function body parse tree
(define closure-env caddr)  ; returns a function to get the environment

;; Function:    (initial-env)
;; Description: creates the initial env for the interpreter which has
;;              undefined 'throw and 'return variables in it
(define initial-env
  (lambda ()
    (add throw-var undefined-var empty-env)
  )
)

;; Function:    (null-layer? env)
;; Parameters:  env the binding list to check if the top layer is empty
;; Description: the top layer is null if the names and values lists inside it are null
;;              ie layer == '(() ())
(define null-layer?
  (lambda (env)
    (and (null? (names env)) (null? (values env)))
  )
)

;; Function:    (null-env? env)
;; Parameters:  env the binding list to check if it is an empty env
;; Description: the env is null if all layers are empty or there are no layers
(define null-env?
  (lambda (env)
    (cond
      ((null? env) ; all layers in env were empty
        #t)

      ((null-layer? env) ; top layer is empty, so check the next one
        (null-env? (next-layer env)))

      (else ; layer/env was not empty
        #f)
    )
  )
)

;; Function:    (invalid-env? env)
;; Parameters:  env the binding list to check if top layer is valid
;; Description: the env is invalid if either names or values are null and the other is not
(define invalid-layer?
  (lambda (env)
    (or (and (null? (names env)) (not (null? (values env))))
        (and (not (null? (names env))) (null? (values env))))
  )
)

;; Function:    (next-env env)
;; Parameters:  env the binding list to find the next env from
;; Description: Effectively like calling cdr for the binding list.
;;              removes the first name and value from the list and returns the result
(define next-env
  (lambda (env)
    (cons (cons (next-names env)
                (list (next-values env)))
          (next-layer env))
  )
)

;; Function:    (push-layer env)
;; Parameters:  env the binding list to add a new empty layer to
;; Description: Adds a new empty layer to the env
(define push-layer
  (lambda (env)
    (cons null-layer env)
  )
)

;; Function:    (remove-top-layer env)
;; Parameters:  env the binding list to remove the top layer from
;; Description: Removes the top layer in the env
(define remove-top-layer
  (lambda (env)
    (next-layer env)
  )
)

;; Function:    (find-box name env)
;; Parameters:  name  the name of the variable to find in the env
;;              env the binding list to search
;; Description: Searches the env for the name and returns a box with the associated value
(define find-box
  (lambda (name env)
    (cond
      ((null-env? env) ; reached an empty env, so the variable does not exist
        undeclared-var)

      ((invalid-layer? env) ; env is corrupt
        (invalid-env-error env))

      ((null-layer? env) ; variable was not in this layer, so check the next
        (find-box name (next-layer env)))

      ((eq? (current-name env) name) ; found the variable
        (current-value env))

      (else ; recurse on the env without the current name and value
        (find-box name (next-env env)))
    )
  )
)

;; Function:    (find name env)
;; Parameters:  name  the name of the variable to find in the env
;; Description: Searches the env for the name and returns the associated value
;; Note:        The function throws an error is the variable was not found or is undefined
(define find
  (lambda (name env)
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
            (find-box name env)
           ))
  )
)

;; Function:    (add name value env)
;; Parameters:  name  the name of the variable to add to the env
;;              value the value to associate with the name
;;              env the binding list to add the name/value pair to
;; Description: Adds a new name/value pair to the top layer of the env.
;; Note:        This function throws an error if the value it is adding already exists in the env
(define add
  (lambda (name value env)
    (if (exists-in-top-layer? name env)
      (double-declare-error name)
      (cons (cons (cons name (names env))
                  (list (cons (box value) (values env))))
            (next-layer env))
    )
  )
)

;; Function:    (add-function name param-list func-body env)
;; Parameters:  name       - the name of the function to add to the env
;;              param-list - a list of atoms that represent the parameter names for a functions
;;              func-body  - a parse tree used to evaluate the function
;;              env        - the environment when the function is declared to keep track of
;;                           what is in the function's scope
;; Description: Adds a new function closure to the top layer of the env
;; Note:        This function throws an error if the name of the function exists in the env
(define add-function
  (lambda (name param-list func-body env)
    (add name
         (list param-list
               func-body
               (lambda () ; the env is accessed via function to allow access to itself
                 (add-function name param-list func-body env)
               ))
         env)
  )
)

;; Function:    (add-multiple-vars names values env)
;; Parameters:  names  - A list of names of the variables to add to the evironment/env
;;              values - A list of values that maps 1-1 with the names list
;;              env    - the environment to add the name/value pairs to
;; Description: Adds each name/value pair in the names and values list to the
;;              environment and returns the new env
;; Note:        This function throws an error if any of the names already exist in env
;;              This function throws an error if the length of names and values do not match
(define add-multiple-vars
  (lambda (name-list value-list env)
    (cond
      ((and (null? name-list) (null? value-list))
        env)
      ((eq? (len name-list) (len value-list))
        (add (car name-list)
             (car value-list)
             (add-multiple-vars (cdr name-list) (cdr value-list) env)))
      (else
        (name-value-length-error name-list value-list))
    )
  )
)

;; Function:    (exists-in-top-layer? name env)
;; Parameters:  name  the name of the variable to check if it is in the top
;;                    layer of the env
;;              env the binding list to check for the name in the top layer
;; Description: Checks if a variable exists in the top layer of the env.
;;              returns #t if it does, #f otherwise
(define exists-in-top-layer?
  (lambda (name env)
    (cond
      ((null-env? env)             #f)
      ((null-layer? env)             #f)
      ((eq? name (current-name env)) #t)
      (else                            (exists-in-top-layer? name (next-env env)))
    )
  )
)

;; Function:    (exists? name env)
;; Parameters:  name  is the name of the binding to check for
;;              env is the binding list
;; Description: Checks if a binding with the given name exists anywhere in the env.
;;              Returns #t if it does, #f if not
(define exists?
  (lambda (name env)
    (cond
      ((null-env? env)               #f)
      ((exists-in-top-layer? name env) #t)
      (else                              (exists? name (next-layer env)))
    )
  )
)

;; Function:    (change-value name new-value env)
;; Parameters:  name      the name of the variable to change in the env
;;              new-value the new value to assign to the variable name
;;              env     the binding list to change the value in
;; Description: Changes a variable in the env to a have a new value.
;; Note:        This function does not change the env if name is not in the env
;; Note:        The variable changed is the one in the highest layer in the env
(define change-value
  (lambda (name new-value env)
    ((lambda (box-found)
       (cond
         ((eq? box-found undeclared-var) env)
         (else                           (begin (set-box! box-found new-value) env))
       )
     )
     (find-box name env)
    )
  )
)
