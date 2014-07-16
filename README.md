tiny-lisp
=========

tiny-lisp is a small implementation of LISP, written in standard C11. It is
intended to show how features such as macros, tail recursion and a copying
garbage collector can be implemented in C.

Language features
-----------------

* read-eval-print loop
* numbers, strings, symbols and lists
* functions and macros
* lexical scoping
* scheme-style dotted parameter lists
* scheme-style tail recursion
* copying garbage collector

### Numeric operations

tiny-lisp supports the four arithmetic operations `+`, `-`, `*`, `/` as well as
the relational operations `=`, `<`, `<=`, `>`, `>=`:

    (- 5)                  ; -5
    (- 5 3.2 .6)           ; 1.2
    (< 2 4 6.8)            ; t
    (> 4 8)                ; nil

### List operations

`(list ...)` returns a list containing the supplied arguments:

    (list)                 ; nil
    (list 1 2 3)           ; (1 2 3)
    (list 1 2 (+ 1 2))     ; (1 2 3)

`(cons x y)` creates a new cons cell, the `car` of which is `x` and the `cdr`
of which is `y`:

    (cons 1 2)             ; (1 . 2)
    (cons 1 '(2))          ; (1 2)

`(car x)`, `(cdr x)` return the `car` and `cdr` of a list respectively:

    (car '(1 2 3))         ; 1
    (cdr '(1 2 3))         ; (2 3)

### Predicates

`(eq x y)` returns `t` if `x` and `y` refer to the same object, or if they are
numbers with the same value, or if they are string objects with identical
content:

    (eq 1 1)               ; t
    (eq 1 2)               ; nil
    (eq 'a 'a)             ; t
    (eq 'a 'b)             ; nil
    (eq "hello" "hello")   ; t
    (eq "hello" "world")   ; nil
    (eq '(1 2) '(1 2))     ; nil

`(equal x y)` returns `t` if `x` and `y` are objects of identical structure and
each of their elements satisfy the `eq` predicate, otherwise `nil`:

    (equal '(1 2) '(1))    ; nil
    (equal '(1 2) '(1 2))  ; t

`(null x)` returns `t` if `x` is `nil`, otherwise `nil`:

    (null nil)             ; t
    (null 1)               ; nil
    (null "hello")         ; nil
    (null '(1 2))          ; nil

`(atom x)` returns `t` if `x` is not a cons cell, otherwise `nil`:

    (atom nil)             ; t
    (atom 1)               ; t
    (atom "hello")         ; t
    (atom '(1 2))          ; nil

`(consp x)` returns `t` if `x` is a cons cell, otherwise `nil`:

    (consp nil)            ; nil
    (consp 1)              ; nil
    (consp "hello")        ; nil
    (consp '(1 2))         ; t

`(listp x)` returns `t` if `x` is a cons cell or `nil`, otherwise `nil`:

    (listp nil)            ; t
    (listp 1)              ; nil
    (listp "hello")        ; nil
    (listp '(1 2))         ; t

`(zerop x)` returns `t` if `x` is the number zero, otherwise `nil`:

    (zerop 0)              ; t
    (zerop 1)              ; nil
    (zerop "hello")        ; error: "hello" is not a number

### Logical operations

`(not x)` returns `t` if `x` is `nil`, otherwise `nil` (`not` is therefore
identical to `null`, but is usually used in conjunction with boolean logic).

`(and ...)` evaluates its arguments one at a time from left to right. As soon
as any argument evaluates to `nil`, `and` returns `nil` without evaluating the
remaining arguments. Otherwise, it returns the value produced by evaluating
its last argument. If no arguments are supplied, `and` returns `t`:

    (and)                  ; t
    (and 1 2 3)            ; 3
    (and 1 nil 3)          ; nil

`(or ...)` evaluates its arguments one at a time from left to right. As soon
as any argument does not evaluate to `nil`, `or` returns its value without
evaluating the remaining arguments. Otherwise, it returns `nil`:

    (or)                   ; nil
    (or 1 2 3)             ; 1
    (or nil nil 3)         ; 3

### Output operations

`(princ x)` prints `x` to the standard output:

    (princ "Hello!\n")     ; prints Hello! followed by a newline

`(print x)` is similar to `princ` but its output is preceded by a newline and
followed by a space. In addition, `print` differs in its handling of strings
which it outputs exactly the way they were entered, including reproducing
escape sequences:

    (print "\"a\\b\"\n")   ; prints "\"a\\b\"\n"

### Conditionals

`(if test then [else])` returns the result of evaluating `then` if `test`
does not evaluate to `nil`, otherwise returns the result of evaluating `else`
or `nil`:

    (if 1 2 3)             ; 2
    (if nil 2 3)           ; 3
    (if nil 2)             ; nil

`(cond ...)` takes zero or more clauses, each of the form `(test [expr...])`.
`cond` returns the result of evaluating `expr...` of the first clause for which
`test` does not evaluate to `nil` without evaluating the remaining clauses. If
a clause does not supply `expr...`, the result of evaluating `test` is returned
instead. If every `test` evaluates to `nil`, or no clauses are given, `cond`
returns `nil`:

    (cond)                 ; nil
    (cond (nil "hello")
          (t   "world"))   ; "world"
    (cond (nil "hello")
          ("world"))       ; "world"

### Defining variables

`(setq name expr ...)` binds the result of evaluating `expr` to the symbol
`name`. More than one `name`-`expr` pair can be given. `setq` returns the value
bound to the last symbol:

    (setq a 1
          b 2)             ; 2
    a                      ; 1
    b                      ; 2

### Defining functions

`(lambda params expr...)` creates an anonymous function with parameters
`params` and body `expr...`. `params` can be `nil`, a symbol, or a list of
symbols. If `params` is a symbol or a dotted list of symbols, the function will
accept a variable number of arguments:

    ((lambda nil
       "hello"))           ; "hello"
    ((lambda (a b)
       (+ a b)) 1 2)       ; 3
    ((lambda (a b . c)
       c) 1 2 3 4 5)       ; (3 4 5)
    ((lambda args
       args) 1 2 3 4 5)    ; (1 2 3 4 5)

`(defun name params expr...)` creates a function and binds it to the symbol
`name`:

    (defun plus1 (x)
      (+ x 1))             ; #<Lambda (x)>
    (plus1 2)              ; 3

### Defining macros

`(macro params expr...)` creates an anonymous macro with parameters `params`
and body `expr...`.

`(defmacro name params expr...)` creates a macro and binds it to the symbol
`name`.

Macros are different from functions in that they do not evaluate their
arguments when called. Instead, we can think of them as taking expressions as
input and returning a new expression as output.

Imagine we want to implement `(when test expr...)`:

    (setq x '(1 2 3))      ; (1 2 3)
    (when (consp x)
          (car x))         ; 1
    (setq x "hello")
    (when (consp x)
          (car x))         ; nil

`when`, if implemented as a function, would not work correctly, since `expr...`
would be evaluated as soon as `when` is called:

    (setq x "hello")
    (when (consp x)
          (car x))         ; error: "hello" is not a list

However, we can implement `when` as a macro that wraps `expr...` in a call to
`if`:

    (defmacro when (test . expr)
      (list 'if test (cons 'progn expr)))

`(when (consp x) (car x))` would then produce the expression
`(if (consp x) (progn (car x)))` which yields the expected behaviour.
