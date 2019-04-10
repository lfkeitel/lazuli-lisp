# Truthiness in Lazuli

Lazuli use loose truthiness instead of a hard true/false boolean.

The following values are considered "falsey":

- Empty string - `""`
- Zero - `0`
- Empty list - `'()`
- F keyword - `:f`

Everything else is considered to be "truthy". If you want to use boolean values,
use the keywords `:t` and `:f` for true and false respectively.
