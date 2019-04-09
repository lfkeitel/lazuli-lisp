; Variable definitions
(defvar a 5)
(defvar b 6)

; Arithmetic, equality
(print (+ a b))
(print (eq a b))
(print (eq 5 5))
(print (eq 5 6))
(print (eq 5 "Hello"))
(print (eq "Hello" "Hello"))
(print (eq "hello" "Hello"))
(print (not (eq "hello" "Hello")))
(print (eq (eq "hello" "Hello") '()))
(print (eq (not (eq "hello" "Hello")) :t))

; Logical operations
(print (and :t :t))
(print (and :t '()))
(print (and '() :t))
(print (or :t '()))
(print (or '() :t))
