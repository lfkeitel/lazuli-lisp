;; Define a macro named "defun"
;; The ~ mark expressions that will be unquoted when the macro
;; is executed. ~ only has an effect inside quasiquoted lists.
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
