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

(defvar str1 "Hello")
(defvar str2 "hello")

; If expression with true and false branches
(if (eq str1 str1)
    (print str1 "and" str1 "are equal")
    (print str1 "and" str1 "are not equal"))

(if (eq str1 str2)
    (print str1 "and" str2 "are equal")
    (print str1 "and" str2 "are not equal"))

; If expression with only true branch
(if (eq 5 6) (print "Math done goofed"))
(if (eq 2 2) (print "Math still good"))

(print) ; Just print a new line

; Ifs are expressions and return their executed branch
(print (if (eq 2 2) "Ifs are expressions"))
(print (if (eq 2 3) "Ifs return false if their don't have an else branch"))
