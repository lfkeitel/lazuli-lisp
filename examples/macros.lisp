;; Define a macro named "defun"
;; The % marks expressions that will be unquoted when the macro
;; is executed. % only has an effect inside quasiquoted lists.
;; Here marked with `
(define-syntax defun (name args body)
    `(setf '%name (lambda %args %body)))

;; expand-macro will execute a macro and return the list generated
;; by the macro. This would normally be executed as an expression.
(print (expand-macro (defun say-hello (name)
    (print "Hello," name))))

;; Call the macro to define a function named "say-hello"
(defun say-hello (name)
    (print "Hello," name))

;; Call previously defined function
(say-hello "Earth!")

;; The symbol pair %@ unquotes and expands a list.
;; The below macro takes a list whose elements are expanded, or flattened
;; so the print command receives multiple arguments instead of a single
;; list argument.
(define-syntax list-expand (list)
    `(print %@list))

(define-syntax list-no-expand (list)
    `(print %list))

(print (expand-macro (list-expand (item1 item2 item3))))
(list-expand (item1 item2 item3))

(print (expand-macro (list-no-expand (item1 item2 item3))))
; (list-no-expand (item1 item2 item3)) ; This will fail because the interpreter
    ; will attempt to execute the command item1 with two arguments but
    ; item1 isn't defined.
