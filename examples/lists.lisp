(define d 5)

(define list1 '(a b c)) ; Quote a literal list
(define list2 `(%d e f)) ; Quasiquote to expand external symbols

(define new-list (concat list1 list2)) ; Concat two lists

(print new-list) ; New list

(print (head new-list)) ; First element of new list
(print (tail new-list)) ; All elements except the first of new list
