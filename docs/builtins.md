# Runtime Notes

All CAPS indicate a symbol of value. "..." indicate one or more of the previous.
Square brackets indicate optional arguments.

## Types

- Symbol - Holds values and functions in Lazuli, symbols themselves are valid
  value types.
- Keyword - Special symbols that always returns themselves. Can't hold values or
  functions. Good for map and property keys.
- Number - 64 bit integer
- Float - 64 bit floating point number
- String - Collection of valid UTF8 unicode code points
- List - Singly linked list of values
- Function - Takes arguments, runs code, and returns a value
- Map - Collection of key value pairs
- Empty - Indicates lack of a value, similar to null. This type can't be created
  directly but is returned by several functions.

## Symbol Manipulation

- `(define SYM VAL)`
- `(defineg SYM VAL)` - Define symbol in global scope
- `(define-syntax SYM VAL)`
- `(setq SYM VAL)`
- `(setf SYM VAL)`

## Output

- `(print VAL)`
- `(expand-macro VAL)`

## Quoting

- `(quote VAL)` or `'()`
- `(quasiquote VAL)` or `` `() ``
- `(unquote VAL)` or `%val`
- `(unquote-splice LIST)` or `%@val`

## Execution

- `(progn () ()...)`
- `(lambda (PARAMS, ...) BODY)`
- `(eval VAL)`
- `(loop BODY)`
- `(while COND BODY)`
- `(include FILE)` - The file is included as if it was inline. All symbols are
  accessable to the included script

## List Manipulation

- `(list ITEM...)`
- `(concat LIST...)`
- `(head LIST)`
- `(tail LIST)`

## Arithmetic

- `(+ OPERAND...)`
- `(- OPERAND...)`
- `(* OPERAND...)`
- `(/ OPERAND...)`

## Logic

- `(eq VAL...)`
- `(not VAL)`
- `(and VAL...)`
- `(or VAL...)`
- `(if CONDITION TRUE [FALSE])`

## Conversions

- `(parse-int VAL)`
- `(parse-float VAL)`

## Maps

- `(make-map KEY VALUE [KEY VALUE...])`

## Collections

COLLECTION is either a Symbol or Map. For Symbols, their property tables are
used.

- `(set-key COLLECTION KEY VALUE)`
- `(get-key COLLECTION KEY)`
- `(has-key COLLECTION KEY)`

## String Manipulation

- `(string-concat VAL...)`
- `(string-replace STR OLD NEW [COUNT])` - Replace COUNT instances of OLD with NEW
  in string STR. If COUNT is not given, string-replace will replace all instances
  of OLD
- `(string-split STR PATTERN [COUNT])` - Split STR into COUNT parts separated by
  PATTERN. If COUNT is not given, all possible splits will be made.
