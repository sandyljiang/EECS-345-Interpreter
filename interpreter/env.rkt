#lang racket
(provide (all-defined-out))
(require "helper.rkt")

;;;; *********************************************************************************************************
;;;; Jared Cassarly (jwc160), Shota Nemoto (srn24), Sandy Jiang (sxj409)
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 3
;;;; env handling functions
;;;; *********************************************************************************************************

(define-syntax debug
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(let ((x ,(cadr slist))) (begin (print x) (newline) (newline) x)))
  )
)

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
    (error "Error: names list does not map 1-1 with values list.\nNames:" names 'Values: values)))

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
(define closure-class cadddr)

;;;; *********************************************************************************************************
;;;; Class Closure format
;;;;
;;;; The closure for a function will be stored in a list with the following format
;;;; (super method-names method-closures static-method-names static-method-closures instance-field-names)
;;;;
;;;; super                   - a symbol that represents the class
;;;; method-names            - a list of symbols that represent the non-static functions in the class
;;;; method-closures         - a list of function closures that correspond to the method names
;;;; static-method-names     - a list of symbols that represent the static functions in the class
;;;; static-method-closures  - a list of function closures that correspond to the static method names
;;;; instance-field-names    - a list of symbols that represents the instance variables in the class
;;;; instance-field-values   - a list of values that represents the default values for the instance variables
;;;; *********************************************************************************************************

(define super car)
(define method-names cadr)
(define method-closures caddr)
(define static-method-names cadddr)
(define static-method-closures
  (lambda (ptree)
    (car (cddddr ptree))))
(define instance-field-names
  (lambda (ptree)
    (cadr (cddddr ptree))))
(define class-instance-field-values
  (lambda (ptree)
    (caddr (cddddr ptree))))

(define get-class-methods
  (lambda (class-closure)
    (list (list (method-names class-closure)
                (method-closures class-closure)))))

(define get-class-static-methods
  (lambda (class-closure)
    (list (list (static-method-names class-closure)
                (static-method-closures class-closure)))))

(define get-class-vars
  (lambda (class-closure)
    (list (list (instance-field-names class-closure)
                (class-instance-field-values class-closure)))))

;; Function:    (find name env)
;; Parameters:  class-closure - the closure to extract the functions from
;; Description: extracts the functions from the class closure and returns them in the
;;              following format:
;;
;; (((method-names) (method-closures)) ((static-method-names) (static-method-closures)))
(define get-class-functions
  (lambda (class-closure)
    (list (list (method-names class-closure)
                (method-closures class-closure))
          (list (static-method-names class-closure)
                (static-method-closures class-closure)))))

;; Function:    (class-constructor class-closure)
;; Parameters:  class-closure - the closure to extract the instance field names and values
;;                              from
;; Description: creates a new object closure (see below) with the default instance-field-values
;;              in the instance-field-values of that object
(define class-constructor
  (lambda (class-closure)
    (cons class-closure
          (list (rebox (class-instance-field-values class-closure))))))

;;;; *********************************************************************************************************
;;;; Object/Instance Closure format
;;;;
;;;; The closure for a function will be stored in a list with the following format
;;;; (class-closure instance-field-values)
;;;;
;;;; class-closure         - a class closure object
;;;; instance-field-values - a list of values that correspond to the instance-field names
;;;; *********************************************************************************************************

(define get-class-closure car)
(define object-instance-field-values cadr)

(define get-object-instance-fields
  (lambda (object-closure)
    (list (list (method-names (get-class-closure object-closure))
                (method-closures (get-class-closure object-closure)))
          (list (instance-field-names (get-class-closure object-closure))
                (object-instance-field-values object-closure)))))

;; Function:    (initial-env)
;; Description: creates the initial env for the interpreter which has
;;              undefined 'throw and 'return variables in it
(define initial-env
  (lambda ()
    (add throw-var undefined-var empty-env)))

;; Function:    (null-layer? env)
;; Parameters:  env the binding list to check if the top layer is empty
;; Description: the top layer is null if the names and values lists inside it are null
;;              ie layer == '(() ())
(define null-layer?
  (lambda (env)
    (and (null? (names env)) (null? (values env)))))

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
        #f))))

;; Function:    (invalid-env? env)
;; Parameters:  env the binding list to check if top layer is valid
;; Description: the env is invalid if either names or values are null and the other is not
(define invalid-layer?
  (lambda (env)
    (or (and (null? (names env)) (not (null? (values env))))
        (and (not (null? (names env))) (null? (values env))))))

;; Function:    (next-env env)
;; Parameters:  env the binding list to find the next env from
;; Description: Effectively like calling cdr for the binding list.
;;              removes the first name and value from the list and returns the result
(define next-env
  (lambda (env)
    (cons (cons (next-names env)
                (list (next-values env)))
          (next-layer env))))

;; Function:    (push-layer env)
;; Parameters:  env the binding list to add a new empty layer to
;; Description: Adds a new empty layer to the env
(define push-layer
  (lambda (env)
    (cons null-layer env)))

;; Function:    (remove-top-layer env)
;; Parameters:  env the binding list to remove the top layer from
;; Description: Removes the top layer in the env
(define remove-top-layer
  (lambda (env)
    (next-layer env)))

(define search-values
  (lambda (index values-list)
    (cond
      ((null? values-list)
        (error "Error: could not find value"))
      ((zero? index)
        (car values-list))
      (else
        (search-values (- index 1) (cdr values-list))))))

;; Function:    (find-box name env)
;; Parameters:  name  the name of the variable to find in the env
;;              env the binding list to search
;; Description: Searches the env for the name and returns a box with the associated value
(define find-box-acc
  (lambda (name env acc values-to-search)
    (cond
      ((and (null? (names env)) (null-env? (next-layer env))) ; reached an empty env, so the variable does not exist
        undeclared-var)
      ((null? (names env))  ; variable was not in this layer, so check the next
        (find-box-acc name (next-layer env) (- (len (names (next-layer env))) 1) (values (next-layer env))))
      ((eq? (current-name env) name) ; found the variable
        (search-values acc values-to-search))
      (else ; recurse on the env without the current name and value
        (find-box-acc name (next-env env) (- acc 1) values-to-search)))))

(define find-box
  (lambda (name env)
    (find-box-acc name env (- (len (names env)) 1) (values env))))

;; Function:    (find-with-undeclared-handler name env)
;; Parameters:  name    - the name of the variable to find in the env
;;              env     - the environment to search in
;;              handler - the function that takes no arguments to handle an undeclared variable
;;                        and returns a box or throws an error
;; Description: Searches the env for the name and returns the associated value
;;              if the variable is not found, then call the handler
(define find-with-undeclared-handler
  (lambda (name env handler)
    (unbox ((lambda (box-found)
              (cond
                ((eq? box-found undeclared-var)
                  (handler))
                ((eq? (unbox box-found) undefined-var)
                  (undefined-error name))
                (else
                  box-found)))
            (find-box name env)))))

;; Function:    (find name env)
;; Parameters:  name - the name of the variable to find in the env
;;              env  - the environment to search in
;; Description: Searches the env for the name and returns the associated value
;; Note:        The function throws an error if the variable was not found or is undefined
(define find
  (lambda (name env)
    (find-with-undeclared-handler name env (lambda () (undeclared-error name)))))

;; Function:    (lookup-non-local-function name env)
;; Parameters:  name       - the name of the function to find in the class-closure of class-name
;;              env        - the environment to search in
;;              class-name - the name of the class to find the function in
;; Description: Searches the env for the class-name and find the function name in it
;; Note:        The function throws an error if the function or class was
;;              not found or was undefined
(define lookup-non-local-function-box
    (lambda (name class-closure)
      ;; find the class closure, get its function and  find the requested function
      (find-box name (get-class-functions class-closure))))

;; Function:    (lookup-non-local-function name env)
;; Parameters:  name       - the name of the function to find in the class-closure of class-name
;;              env        - the environment to search in
;;              class-name - the name of the class to find the function in
;; Description: Searches the env for the class-name and find the function name in it
;; Note:        The function throws an error if the function or class was
;;              not found or was undefined
(define lookup-non-local-function
    (lambda (name class-closure)
      ;; find the class closure, get its function and  find the requested function
      (find name (get-class-functions class-closure))))

;; Function:    (lookup-instance-fields name object-closure)
;; Parameters:  name           - the name of  variable/function to find in the object-closure
;;              object-closure - the object closure to lookup values from
;; Description: Searches the object-closure's instance fields for name and returns the value
;; Note:        The function throws an error if the variable/function or class was
;;              not found or was undefined
(define lookup-instance-fields
  (lambda (name object-closure)
    (find name (get-object-instance-fields object-closure))))

;; Function:    (lookup-function-closure name env)
;; Parameters:  name       - the name of the function to find in the class-closure of class-name
;;              env        - the environment to search in
;;              class-name - the name of the class to find the function in
;; Description: Searches the env for the function and returns its closure if found locally.
;;              otherwise, it searches the class-closure of class-name for the function closure
;; Note:        The function throws an error if the function or class was
;;              not found or was undefined
;; Note:        If there is a local variable with the same name as the function, this finds that
(define lookup-function-closure
  (lambda (name env class-closure)
    (find-with-undeclared-handler name
                                  env
                                  (lambda ()
                                    (lookup-non-local-function-box name class-closure)))))

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
                  (list (append (values env) (list (box value)))))
            (next-layer env)))))

;; Function:    (func-def-class-closure)
;; Parameters:  class-name-to-declare - the name of the class used to lookup the class-closure
;; Description: Returns a function that looks up the class-closure based on the class-name to declare
;;              in an environment passed into the returned function
(define func-def-class-closure
  (lambda (class-name-to-declare)
    (lambda (current-env) (find class-name-to-declare current-env))))

;; Function:    (add-function name param-list func-body env)
;; Parameters:  name       - the name of the function to add to the env
;;              param-list - a list of atoms that represent the parameter names for a functions
;;              func-body  - a parse tree used to evaluate the function
;;              env        - the environment when the function is declared to keep track of
;;                           what is in the function's scope
;; Description:  Adds a new function closure to the top layer of the env
;; Note:        This function throws an error if the name of the function exists in the env
(define add-function
  (lambda (name param-list func-body class env func-env)
    (add name
         (list param-list
               func-body
               (lambda (method-closures values) ; the env is accessed via function to allow access to itself
                 (add-function name param-list func-body class func-env func-env))
               (lambda (current-env)
                 (unbox class)))
         env)))

(define add-member-function
  (lambda (name param-list func-body class env func-env)
    (add name
         (list param-list
               func-body
               (lambda (method-closures values) ; the env is accessed via function to allow access to itself
                 (append (list (list (method-names (unbox class)) method-closures)
                               (list (instance-field-names (unbox class)) values))
                         func-env))
               (lambda (current-env)
                 (unbox class)))
         env)))

;; Function:    (add-class-closure env name super method-names method-closures smn smc ifn)
;; Parameters:  env              - the environment to search in
;;              name             - the name of the class that is being added to the closure
;;              super            - the name of the parent of the class
;;              method-names     - the method names in the class
;;              method-closures  - the closures of the class
;;              smn              - the static method names list (name shortened to keep
;;                                 param declarations on one line)
;;              smc              - the static method closures list (name shortened to keep
;;                                 param declarations on one line)
;;              ifn              - the instance field names list (name shortened to keep
;;                                 param declarations on one line)
;;              ifv              - the default values list assigned to instance-field names
;;                                 (name shortened to keep param declarations on one line)
;; Description: adds a class closure to the environment
(define add-class-closure
  (lambda (env name super class-method-names class-method-closures smn smc ifn ifv)
    (add name
         (list super class-method-names class-method-closures smn smc ifn ifv)
         env)))

(define find-super
  (lambda (class-closure env)
    (if (null? (super class-closure))
      '()
      (find (super class-closure) env))))

(define update-class-with-super
  (lambda (env name)
    (let* ((class (find name env))
           (super-object (find-super class env)))
      (if (null? super-object)
          env
          (change-value name
              (list (super class)
                    (append (method-names class) (method-names super-object))
                    (append (method-closures super-object) (method-closures class))
                    (append (static-method-names class) (static-method-names super-object))
                    (append (static-method-closures super-object) (static-method-closures class))
                    (append (instance-field-names class) (instance-field-names super-object))
                    (append (class-instance-field-values super-object) (class-instance-field-values class)))
              env)))))

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
        (name-value-length-error name-list value-list)))))

;; Function:    (exists-in-top-layer? name env)
;; Parameters:  name  the name of the variable to check if it is in the top
;;                    layer of the env
;;              env the binding list to check for the name in the top layer
;; Description: Checks if a variable exists in the top layer of the env.
;;              returns #t if it does, #f otherwise
(define exists-in-top-layer?
  (lambda (name env)
    (cond
      ((null-env? env)               #f)
      ((null-layer? env)             #f)
      ((eq? name (current-name env)) #t)
      (else                          (exists-in-top-layer? name (next-env env))))))

;; Function:    (exists? name env)
;; Parameters:  name  is the name of the binding to check for
;;              env is the binding list
;; Description: Checks if a binding with the given name exists anywhere in the env.
;;              Returns #t if it does, #f if not
(define exists?
  (lambda (name env)
    (cond
      ((null-env? env)                 #f)
      ((exists-in-top-layer? name env) #t)
      (else                            (exists? name (next-layer env))))))

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
         (else                           (begin (set-box! box-found new-value) env))))
     (find-box name env))))


(define change-class-vars
  (lambda (var-name var-value env class-name-to-declare)
    (let* ((current-class (find class-name-to-declare env))
           (new-vars (add var-name var-value (get-class-vars current-class)))
           (new-names (names new-vars))
           (new-values (values new-vars)))
      (change-value class-name-to-declare
                    (list (super current-class)
                          (method-names current-class)
                          (method-closures current-class)
                          (static-method-names current-class)
                          (static-method-closures current-class)
                          new-names
                          new-values)
                    env))))

(define change-class-methods
  (lambda (func-name func-params func-body class-name-to-declare env)
    (let* ((class-box (find-box class-name-to-declare env))
           (current-class (find class-name-to-declare env))
           (new-methods (add-member-function func-name func-params func-body class-box (get-class-methods current-class) env))
           (new-names (names new-methods))
           (new-values (values new-methods)))
      (change-value class-name-to-declare
                    (list (super current-class)
                          new-names
                          new-values
                          (static-method-names current-class)
                          (static-method-closures current-class)
                          (instance-field-names current-class)
                          (class-instance-field-values current-class))
                    env))))

(define change-class-static-methods
  (lambda (func-name func-params func-body class-name-to-declare env)
    (let* ((class-box (find-box class-name-to-declare env))
           (current-class (find class-name-to-declare env))
           (new-methods (add-function func-name func-params func-body class-box (get-class-static-methods current-class) env))
           (new-names (names new-methods))
           (new-values (values new-methods)))
      (change-value class-name-to-declare
                    (list (super current-class)
                          (method-names current-class)
                          (method-closures current-class)
                          new-names
                          new-values
                          (instance-field-names current-class)
                          (class-instance-field-values current-class))
                    env))))

;; Function:    (find-in-super name env instance)
;; Parameters:  name       - the name of the the super in the new environment
;;              env        - the environment to use to evaluate expressions
;;              instance   - the object closure that is currently being used
;; Description:

(define find-in-super
  (lambda (name env instance)
    (let* ([super-closure (find (super (get-class-closure instance)) env)]
           [super-method-names (method-names super-closure)]
           [super-ifn (instance-field-names super-closure)]
           [new-env (list (list super-method-names
                                (method-closures (get-class-closure instance)))
                          (list super-ifn
                                (object-instance-field-values instance)))])
      (find name new-env))))

