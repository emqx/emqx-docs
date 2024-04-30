# Variform Expressions

Variform is a lightweight, expressive language designed for string manipulation and runtime evaluation.
It is not a full-fledged programming language but rather a specialized tool that can be embedded within
configurations or EMQX to perform string operations dynamically.

## Syntax

To illustrate:

```js
function_call(clientid, another_function_call(username))
```

This expression combines or manipulates clientid and username to generate a new string value.

Variform supports below literals:

- Integer: For example, `42`.
- Float: For example, `3.14`.
- String: ASCII characters between single quotes `'` or double quotes `"`.
- Array: Elements between `[` and `]`, separated by a comma `,`.
- Variable: Referencing to predefined values, for example `clientid`.
- Function: Predefined functions, for example, `concat([...])`.

Variform does not support the following:

- Arithmetic operations
- Loops
- User-defined variables
- User-defined functions
- Exception handling and error recovery
- Boolean literals. Booleans may be produced intermediately as return values from a built-in functions such as `num_gt` (which stands for 'is number greater'),
  but cannot be wirtten as a literal. The condition functions (`iif` and `coalesce`) take empty string for `false` otherwise `true`.
- Escape sequence in string literals. Call the `unescape` function to unescape special characters.

Below is a configuration example with a Variform expression embedded.

```js
mqtt {
    client_attrs_init = [
        {
            # Extract the prefix of client ID before the first -
            expression = "nth(1, tokens(clientid, '-'))"
            # And set as client_attrs.group
            set_as_attr = group
        }
    ]
}
```

::: tip
When unescape function is required in the expression, it's a good idea to use triple quote (`"""`) strings in HOCON config
so there is no need to perform double escaping.

For example

```
## For multi-line client ID, take the first line.
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

## Pre-defined Functions

EMQX includes a rich set of string, array, random, and hashing functions similar to those available in rule engine string functions.
These functions can be used to manipulate and format the extracted data. For instance, `lower()`, `upper()`,
and `concat()` help in adjusting the format of extracted strings, while `hash()` and `hash_to_range()` allow for creating hashed or ranged outputs based on the data.

Below are the functions that can be used in the expressions:

- **String functions**:
  - [String Operation Functions](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - A new function any_to_string/1 is also added to convert any intermediate non-string value to a string.
- **Array functions**: [nth/2](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
- **Random functions**: rand_str, rand_int
- **Schema-less encode/decode functions**:
  - [bin2hexstr(Data)](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_encode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - `int2hexstr(Integer)`: Encode an integer to hex string. e.g. 15 as 'F' (uppercase).
- **Hash functions**:
  - `hash(Algorihtm, Data)`: Algorithm can be one of: md4 | md5, sha (or sha1) | sha224 | sha256 | sha384 | sha512 | sha3_224 | sha3_256 | sha3_384 | sha3_512 | shake128 | shake256 | blake2b | blake2s
  - `hash_to_range(Input, Min, Max)`: Use sha256 to hash the Input data and map the hash to an integer between Min and Max inclusive ( Min =< X =< Max)
  - `map_to_rage(Input, Min, Max)`: Map the input to an integer between Min and Max inclusive (Min =< X =< Max)
- **Compare functions**:
  - `num_eq(A, B)`: Return 'true' if two numbers are the same, otherwise 'false'.
  - `num_gt(A, B)`: Return 'true' if A is greater than B, otherwise 'false'.
  - `num_gte(A, B)`: Return 'true' if A is not less than B, otherwise 'false'.
  - `num_lt(A, B)`: Return 'true' if A is less than B, otherwise 'false'.
  - `num_lte(A, B)`: Return 'true' if A is not greater than B, otherwise 'false'.
  - `str_eq(A, B)`: Return 'true' if two strings are the same, otherwise 'false', otherwise 'false'.
  - `str_gt(A, B)`: Return 'true' if A is behind B in lexicographic order, otherwise 'false'.
  - `str_gte(A, B)`: Return 'true' if A is not before B in lexicographic order, otherwise 'false'.
  - `str_lt(A, B)`: Return 'true' if A is before B in lexicographic order, otherwise 'false'.
  - `str_lte(A, B)`: Return 'true' if A is not after B in lexicographic order, otherwise 'false'.

## Conditions

Variform expression so far has no comprehensive control flows. The `iif` function is a conditional expression used
to evaluate a condition and return one of two values depending on the result of the condition. This function is
inspired by similar constructs in other programming languages, adapted here for use in a programming expression
that lacks loops and variable bindings.

```js
iif(Condition, ThenExpression, ElseExpression)
```

### Parameters

- **Condition** (Boolean or String): Specifies the condition to be evaluated.
  - If a Boolean (`true` or `false`), it directly evaluates to `true` or `false`.
    - If a String, it evaluates to `false` if the string is empty, and `true` otherwise.
    - **ThenExpression**: The expression or value that is returned if `Condition` evaluates to `true`.
    - **ElseExpression**: The expression or value that is returned if `Condition` evaluates to `false`.

### Returns

- The result of `ThenExpression` if `Condition` is `true`.
- The result of `ElseExpression` if `Condition` is `false`.

## Error Handling

As the default behavior of scripting environments like Bash, Variform expression is designed to yield an empty string ("") in scenarios where errors occur, such as unbound variables or exceptions during runtime.

- Unbound Variables: If an expression references a variable that has not been defined or is out of scope (unbound), the expression will evaluate to an empty string.
- Runtime Exceptions: Any exceptions that occur during the execution of an expression, whether due to incorrect function usage, invalid data types, or other unforeseen issues, will result in the expression yielding an empty string. For example, array index out of range.

## Example Expressions

- `nth(1, tokens(clientid, '.'))`:  Extract the prefix of a dot-separated client ID.
- `strlen(username, 0, 5)`: Extract a partial username.
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`: Extract digits from client ID using a regular expression. If the regular expression yields empty string, then return `'000'`.
- `iif(true, "Value if true", "Value if false")`: Returns `Value if true`
- `iif("", "Value if true", "Value if false")`: Returns `Value if false`
- `iif("hello", "Value if true", "Value if false")`: Returns `Value if true`
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`: Returns `foo` if `clientid` starts with `foo.`, otherwise `bar`.
