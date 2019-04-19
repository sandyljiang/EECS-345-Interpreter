(define lookup-non-local-function
    (lambda (name env class-name)
        ((lambda (class-closure)
            (find name (get-class-functions class-closure))
         )
         (find class-name env)
        )
    )
)

things to do:

sandy
1. define car/cdr access functions in mstate-class-def
   define add-class-closure

jared
2. define class closure accessor methods and relevant car/cdr access functions

shota
3. define mstate-class-body and update the op switch for it (replaces mstate-outer) and relevant car/cdr access functions

