# Builtin Functions

All CAPS indicate a symbol of value. "..." indicate one or more of the previous.
Square brackets indicate optional arguments.

## Symbol Manipulation

`(define SYM VAL)`
`(define-syntax SYM VAL)`
`(setq SYM VAL)`
`(setf SYM VAL)`

## Output

`(print VAL)`
`(expand-macro VAL)`

## Quoting

`(quote VAL)` or `'()`
`(quasiquote VAL)` or `\`()`

## Execution

`(progn () ()...)`
`(lambda (PARAMS, ...) BODY)`
`(eval VAL)`

## List Manipulation

`(list ITEM...)`

## Arithmetic

`(+ OPERAND...)`
`(- OPERAND...)`
`(* OPERAND...)`
`(/ OPERAND...)`

## Logic

`(eq VAL...)`
`(not VAL)`
`(and VAL...)`
`(or VAL...)`
`(if CONDITION TRUE [FALSE])`

## Maps

`(make-map KEY VALUE [KEY VALUE...])`
`(get-map MAP KEY)`
`(set-map MAP KEY VALUE)`
