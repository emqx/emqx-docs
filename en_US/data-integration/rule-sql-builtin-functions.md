# Built-in SQL Functions

The rule engine proffers a variety of built-in functions. You can utilize these functions within SQL to accomplish basic data processing, including:

- [Mathematical](#mathematical-functions),
- [Data Type Judgment](#data-type-judgment-functions)
- [Data Type Conversion](#data-type-conversion-functions),
- [String Operations](#string-operation-functions),
- [Map Operations](#map-operation-functions),
- [Array Operations](#array-operation-functions),
- [Hashing](#hashing-functions)
- [Compression and Decompression](#compression-and-decompression-functions)
- [Bit Operations](#bit-operation-functions)
- [Bit Sequence Operations](#bit-sequence-operation-functions)
- [Encoding and Decoding](#encoding-and-decoding-functions)
- [Date and Time Conversion](#date-and-time-conversion-functions)
- [UUID Functions](#uuid-functions)
- [System Function](#system-function)
- [Conditional Functions](#conditional-functions)

In this section, all function declarations conform to the following format:

```bash
FuncName(Arg 1: Type 1 | ..., ...) -> Type 1 | ...
```

For instance, `abs(X: integer | float) -> integer | float` implies that the data type of argument `X` can be either integer or float, and correspondingly, the return value's data type can also be integer or float.

Be aware that if the provided argument exceeds the stipulated range or employs an unsupported data type, it will result in the current SQL execution failing, incrementing the failure count by one.

:::tip

1. Some escape sequences need to be unescaped when used, see [unescape function](#unescapestring-string---string).
2. Since EMQX 5.0 version, EMQX also supports using  [jq Syntax](https://stedolan.github.io/jq/manual/) for complex data transformation, you may read the [jq Fucntion](./rule-sql-jq.md) section for more information.

:::

## Mathematical Functions

EMQX supports a wide range of mathematical functions:

- Trigonometric and hyperbolic functions, include sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh.
- Numerical functions, include abs, ceil, floor, round, sqrt, fmod.
- Exponential and logarithmic functions, include exp, power, log, log10, log2.

### abs(X: integer | float) -> integer | float

Returns the absolute value of number `X`. Example:

```bash
abs(-12) = 12
abs(-1.2) = 1.2
```

### acos(X: integer | float) -> float

Returns the arc cosine of `X`, expressed in radians. The range of `X` is `[-1, 1]`. Example:

```bash
acos(0.5) = 1.0471975511965976
```

### acosh(X: integer | float) -> float

Returns the hyperbolic arccosine of `X`, expressed in radians. `X` must be greater than or equal to 1. Example:

```bash
acosh(1.5) = 0.9624236501192069
```

### asin(X: integer | float) -> float

Returns the arc sine of `X`, expressed in radians. The range of `X` is `[-1, 1]`. Example:

```bash
asin(0.5) = 0.5235987755982988
```

### asinh(X: integer | float) -> float

Returns the hyperbolic arcsine of `X`. Example:

```bash
asinh(0.5) = 0.48121182505960347
```

### atan(X: integer | float) -> float

Returns the arc tangent of `X`, expressed in radians. Example:

```bash
atan(0.5) = 0.46364760900080615
```

### atanh(X: integer | float) -> float

Returns the hyperbolic arctangent of `X`, where `X` ranges between `(-1, 1)`. Example:

```bash
atanh(0.5) = 0.5493061443340549
```

### ceil(X: integer | float) -> integer

Rounds upward, yielding the smallest integer greater than or equal to the given `X`. Example:

```bash
ceil(0.8) = 1
```

### cos(X: integer | float) -> float

Returns the cosine of the angle `X` expressed in radians. Example:

```bash
cos(0.5) = 0.8775825618903728
```

### cosh(X: integer | float) -> float

Returns the hyperbolic cosine of `X`. Example:

```bash
cosh(0.5) = 1.1276259652063807
```

### exp(X: integer | float) -> float

Returns the natural number e to the power of `X`, i.e., `e^X`. Example:

```bash
exp(1) = 2.718281828459045
```

### floor(X: integer | float) -> integer

Returns the largest integer less than or equal to the given `X`. Example:

```bash
floor(3.6) = 3
```

### fmod(X: integer | float, Y: integer | float) -> float

Returns the remainder of `X` divided by `Y` as a floating-point number. Example:

```bash
fmod(6.5, 2.5) = 1.5
```

### log(X: integer | float) -> float

Returns the natural logarithm of the number `X`, where `X` must be greater than 0. Example:

```bash
log(7.38905609893065) = 2.0
```

### log10(X: integer | float) -> float

Returns the logarithm base 10 of the number `X`, where `X` must be greater than 0. Example:

```bash
log10(100) = 2.0
```

### log2(X: integer | float) -> float

Returns the logarithm base 2 of the number `X`, where `X` must be greater than 0. Example:

```bash
log2(8) = 3.0
log2(8.5) = 3.0874628412503395
```

### round(X: integer | float) -> integer

Rounds the number `X` to the nearest integer. Example:

```bash
round(4.5) = 5
```

### power(X: integer | float, Y: integer | float) -> float

Returns `X` to the power of `Y`, i.e., `X^Y`. Example:

```bash
power(2, 3) = 8.0
```

### random() -> float

Returns a random floating-point number in the range `[0, 1)`. Example:

```bash
random() = 0.5400050092601868
```

### sin(X: integer | float) -> float

Returns the sine of angle `X`, expressed in radians. Example:

```bash
sin(0.5) = 0.479425538604203
```

### sinh(X: integer | float) -> float

Returns the hyperbolic sine of `X`. Example:

```bash
sinh(0.5) = 0.5210953054937474
```

### sqrt(X: integer | float) -> float

Returns the square root of the number `X`. Example:

```bash
sqrt(9) = 3.0
```

### tan(X: integer | float) -> float

Returns the tangent of angle `X` (expressed in radians). Example:

```bash
tan(0.5) = 0.5463024898437905
```

### tanh(X: integer | float) -> float

Returns the hyperbolic tangent of `X`. Example:

```bash
tanh(0.5) = 0.46211715726000974
```

## Data Type Judgment Functions

Data type judgment functions can be used to check the data type of a specified field and indicate whether the field conforms to the specified data type through a boolean value.

### is_array(Term: any) -> boolean

> 'any' signifies all data types.

Determine whether `Term` is of array type. Example:

```bash
is_array([1, 2]) = true
is_array(json_decode('[{"value": 1}]')) = true
is_array(json_decode('{"value": 1}')) = false
is_array(0.5) = false
is_array('[1, 2]') = false
```

### is_bool(Term: any) -> boolean

Determine whether `Term` is of boolean type. Example:

```bash
is_bool(true) = true
is_bool(false) = false
is_bool('true') = false
```

### is_float(Term: any) -> boolean

Determine whether `Term` is of float type. Example:

```bash
is_float(123.4) = true
is_float(123) = false
```

### is_int(Term: any) -> boolean

Determine whether `Term` is of integer type. Example:

```bash
is_int(123) = true
is_int(123.4) = false
```

### is_map(Term: any) -> boolean

Determine whether `Term` is of map type. Example:

```bash
is_map(json_decode('{"value": 1}')) = true
is_map(json_decode('[{"value": 1}]')) = false
```

### is_null(Term: any) -> boolean

Determine whether the variable `Term` is undefined.
This function is used to determine whether a variable is assigned a value, but the value can be JSON `null`.

Example:

```sql
is_null(this_is_an_unassigned_variable) = true
is_null(map_get('b', json_decode('{"a": 1}'))) = true
is_null(map_get('b', json_decode('{"b": null}'))) = false
```

### is_null_var(Term: any) -> boolean

Determine whether the variable `Term` is undefined, or `null`.
Example:

```sql
is_null_var(this_is_an_unassigned_variable) = true
is_null_var(map_get('b', json_decode('{"a": 1}'))) = true
is_null_var(map_get('b', json_decode('{"b": null}'))) = true
```

### is_not_null_var(Term: any) -> boolean

The inverse of `is_null_var`, determine whether the variable `Term` is defined and not `null`.

### is_num(Term: any) -> boolean

Determine whether `Term` is of integer or float type. Example:

```bash
is_num(123) = true
is_num(123.4) = true
is_num('123') = false
```

### is_str(Term: any) -> boolean

Determine whether `Term` is of string type. Example:

```bash
is_str('123') = true
is_str(123) = false
```

## Data Type Conversion Functions

### bool(Term: boolean | integer | string) -> boolean

Convert `Term` to a boolean. `Term` can only be boolean type, integer type with 0 and 1, or string type with true and false.

Example:

```bash
# Correct
bool(true) = true
bool(0) = false
bool('false') = false

# Wrong
bool(20)
bool('True')
```

### float(Term: float | integer | string) -> float

Convert `Term` to a float.

if the type of `Term` is string, scientific notation can be used, such as `float('3.14e4')`. The float type supports up to 16 significant digits. When the valid digits of the floating-point number represented by the string 'Term' exceed 16, rounding errors may occur in the conversion.

Example:

```bash
float(20) = 20.0

float('3.14') = 3.14
float('3.14e4') = 31400
float('3.14e+4') = 31400
float('3.14e-4') = 0.000314
float('3.14E-4') = 0.000314

# Once the significant digits exceed 16, due to rounding errors, disparate inputs may yield identical outputs.
float('0.12345678901234566') = 0.12345678901234566
float('0.12345678901234567') = 0.12345678901234566
```

### float(Term: float | integer | string, Decimals: integer) -> float

Convert `Term` to a floating-point number containing at most `Decimals` digits after the decimal point, with the range of `Decimals` being `(0, 253]`. The other behavior is the same as `float/1`. Example:

```bash
float('3.1415926', 3) = 3.142
float('0.000012345', 5) = 0.00001
```

### float2str(Float: float, Decimals: integer) -> string

Convert the floating-point number `Float` to a string, at most containing `Decimals` digits following the decimal point, with trailing zeros being truncated. The range for `Decimals` is `[0, 253]`. If the significant digits of `Float` exceed 16, rounding errors may occur during the conversion.

Since floating-point numbers cannot be stored precisely in computers, when `Decimals` is greater than the number of decimal places in `Float` (including leading zeros), `float2str` may return a decimal representation of the binary approximation of `Float`.

Example:

```bash
float2str(0.1, 5) = '0.1'
float2str(0.1, 20) = '0.10000000000000000555'
float2str(0.1, 25) = '0.1000000000000000055511151'
float2str(0.00000000001, 20) = '0.00000000001'

# trailing zeros will be truncated
float2str(0.100001, 5) = '0.1'

# Once the significant digits exceed 16, due to rounding errors, disparate inputs may yield identical outputs.
float2str(123456789.01234565, 8) = '123456789.01234566'
float2str(123456789.01234566, 8) = '123456789.01234566'
```

### int(Term: boolean | float | integer | string) -> integer

Convert `Term` into an integer.

When `Term` is a boolean, true will be converted to the number 1, and false will be converted to the number 0.

When `Term` is a float, `Term` will be rounded down, converting it into the largest integer less than or equal to `Term`.

When `Term` is a string, `Term` must contain at least one numerical character, can possess an optional prefix composed of a single `+` or `-` character, and leading zeros will be disregarded. Mathematical notation representation is supported.

When `Term` is an integer, `Term` will be returned as is.

Example:

```bash
# Correct
int(true) = 1
int(3.14) = 3
int(-3.14) = 4
int('-100') = -100
int('+200') = 200
int('0010') = 10
int('3.1415e2') = 314
int(substr('Number 100', 7)) = 100

# Wrong
int('-100+200')
int('Number 100')
```

### str(Term: any) -> string

Convert any type of `Term` into a string.

When `Term` is a map or array, the `str` function will attempt encode `Term` using JSON.

When `Term` is a float, the `str` function will return the corresponding string, truncating any zeros at the end. The returned string will house a maximum of 10 digits post decimal point. To return more decimal places, please use the `float2str` function.

Example:

```bash
str(100) = '100'
str(nth(1, json_decode('[false]'))) = 'false'
str(json_decode({"msg": "hello"})) = '{"msg":"hello"}'
str(json_decode('[{"msg": "hello"}]')) = '[{"msg":"hello"}]'

# Trailing zeros are truncated
str(0.300000004) = '0.3'

# Contains at most 10 number of digits past the decimal point
str(3.14159265359) = '3.1415926536'
str(0.000000314159265359) = '0.0000003142'
```

## String Operation Functions

String functions can be used for case transformations, space removal, substring extraction, replacement, escaping/unescaping, and other operations.

### ascii(Char: string) -> integer

Returns the ASCII code corresponding to character `Char`. If `Char` contains multiple characters, only the code for the first character is returned. Example:

```bash
ascii('a') = 97
ascii('abc') = 97
```

### concat(Str1: string, Str2: string) -> string

Concatenates `Str1` and `Str2` into a single string. Example:

```bash
concat('Name:', 'John') = 'Name:John'
```

### find(String: string, SearchPattern: string) -> string

Searches for the substring `SearchPattern` in `String`, deletes all content before `SearchPattern` in `String`, and returns the remaining part of the string. If `SearchPattern` is not found, an empty string will be returned. This function is equivalent to `find(String, SearchPattern, 'leading')`.

Example:

```bash
find('..., Value: 1.2', 'Value:') = 'Value: 1.2'
find('..., Value: 1.2', 'Data') = ''
```

### find(String: string, SearchPattern: string, Direction: string) -> string

Same as `find/2`, but allows the specification of the direction of the search for the substring `SearchPattern` using `Direction`. Example:

```bash
find('Front, Middle, End', ', ', 'leading') = ', Middle, End'
find('Front, Middle, End', ', ', 'trailing') = ', End'
```

### join_to_string(Sep: string, Array: array) -> string

Joins the elements of `Array` into a single string using the separator `Sep`. Example:

```bash
join_to_string(', ', ['a', 'b', 'c']) = 'a, b, c'
```

### lower(String: string) -> string

Converts uppercase letters in the string `String` to lowercase. Example:

```bash
lower('Hello') = 'hello'
```

### ltrim(String: string) -> string

Same as `trim/1`, but only removes leading whitespace characters from the `String`. Example:

```bash
ltrim('\t  hello  \n') = 'hello  \n'
ltrim('\t  hello \r\n') = 'hello  \r\n'
```

### pad(String: string, Length: integer) -> string

Pads a `String` with trailing spaces to the specified length. Example:

```bash
pad('hello', 8) = 'hello   '
```

### pad(String: string, Length: integer, Direction: string) -> string

Same as `pad/2`, but you can use `Direction` to specify the direction of padding. `leading` means filling leading spaces, `trailing` means filling trailing spaces, and `both` means filling both leading and trailing spaces.

When specifying `Direction` as `both`, if the number of spaces to be filled is an odd number, the last space will be filled at the end.

Example:

```bash
pad('hello', 8, 'leading') = '   hello'
pad('hello', 8, 'trailing') = 'hello   '
pad('hello', 8, 'both') = ' hello  '
```

### pad(String: string, Length: integer, Direction: string, Char: string) -> string

Same as `pad/3`, but can be padded with the specified grapheme cluster `Char`.

Since the rule engine does not check whether `Char` is a legal grapheme cluster, `Char` will be processed as one character length no matter how many characters it contains. Example:

```bash
pad('hello', 8, 'trailing', '!') = 'hello!!!'
pad('hello', 8, 'trailing', '\r\n') = 'hello\r\n\r\n\r\n'
pad('hello', 8, 'trailing', 'abc') = 'helloabcabcabc'
```

### regex_match(String: string, Expression: string) -> boolean

Determine whether the string `String`matches the regular expression `Expression`. Example:

```bash
regex_match('123', '^\d+$') = true
regex_match('a23', '^\d+$') = false
```

### regex_replace(String: string, Expression: string, Replacement: string) -> string

Use string `Replacement` to replace the portion of `String` that matches the regular expression `Expression`. If no matching part is found, the original `String` will be returned. Example:

```bash
regex_replace('hello 123', '\d+', 'world') = 'hello world'
regex_replace('a;b; c', ';\s*', ',') = 'a,b,c'
```

### regex_extract(String: string, Expression: string) -> [string]

::: tip

This function has been introduced since EMQX v5.7.1.

:::

This function non-global searches for the regular expression pattern with capture groups in the given string.
It can be used to extract parts of a string based on a regular expression, excluding the complete match itself.

If matches are found, it returns a list of all captured groups from these matches. If no matches are found or there are no groups captured, it returns an empty list.

Examples:

```bash
regex_extract('Number: 12345', '(\d+)') -> ['12345']
regex_extract('Hello, world!', '(\w+).*\s(\w+)') -> ['Hello', 'world']
regex_extract('No numbers here!', '(\d+)') -> []
regex_extract('Date: 2021-05-20', '(\d{4})-(\d{2})-(\d{2})') -> ['2021', '05', '20']
```

### replace(String: string, SearchPattern: string, Replacement: string) -> string

Replaces all `SearchPatterns` in `String` with `Replacement`. Example:

```bash
replace('ab..cd..ef', '..', '**') = 'ab**cd**ef'
replace('ab..cd..ef', '..', '') = 'abcdef'
```

### replace(String: string, SearchPattern: string, Replacement: string, Where: string) -> string

Replaces occurrences of `SearchPattern` in `String` with `Replacement`.

`Where` has the following possible values:

- `all`: Replace all `SearchPatterns`, equivalent to `replace/3`.
- `leading`: Replaces only the leading `SearchPattern`.
- `trailing`: Replace only the trailing `SearchPattern`.

Example:

```bash
replace('ab..cd..ef', '..', '**', 'all') = 'ab**cd**ef'
replace('ab..cd..ef', '..', '**', 'leading') = 'ab**cd..ef'
replace('ab..cd..ef', '..', '**', 'trailing') = 'ab..cd**ef'
```

### reverse(String: string) -> string

Reverse a string. Example:

```bash
reverse('hello') = 'olleh'
```

### rm_prefix(String: string, Prefix: string) -> string

Removes the prefix `Prefix` from the string `String`. If `String` does not start with `Prefix`, the original `String` will be returned. Example:

```bash
rm_prefix('foo/bar', 'foo/') = 'bar'
rm_prefix('foo/bar', 'xxx/') = 'foo/bar'
```

### rtrim(String: string) -> string

Same as `trim/1`, but only removes trailing whitespace characters from the `String`. Example:

```bash
rtrim('\t  hello  \n') = '\t  hello'
rtrim('\t  hello \r\n') = '\t  hello'
```

### split(String: string, Separator: string) -> array

Splits a `String` into substrings using `Separator` and returns an array of these substrings.

Two or more adjacent `Separators` are not treated as one, so the split result may contain empty strings. `split/2` trims the output results by default and filters out the empty strings. If you want to remain them, please use `split(String, Separator, 'notrim')`.

Separator can be composed of multiple characters, but they will be treated as a whole. If you want to specify multiple delimiting characters at once, please use the `tokens` function.

Example:

```bash
split('a;', ';') = ['a']
split('a;b;c', ';') = ['a', 'b', 'c']
split('a;;b;;c', ';') = ['a', 'b', 'c']

# Note the space before Howell Wise
split('Sienna Blake; Howell Wise', ';') = ['Sienna Blake', ' Howell Wise']
split('Sienna Blake; Howell Wise', '; ') = ['Sienna Blake', 'Howell Wise']
```

### split(String: string, Separator: string, Option: string) -> array

Same as `split/2`, but you can use `Option` to specify the position of the delimiter that needs to be processed, and whether the empty string needs to be returned.

`Option` has the following possible values:

- `notrim:` handles all delimiters in the string, and the returned result may contain empty strings.
- `leading`: Only the leading delimiter is processed, and the returned result does not contain empty strings.
- `leading_notrim`: Only the leading delimiter is processed, and the returned result may contain an empty string.
- `trailing`: Only the trailing delimiter is processed, and the returned result does not contain the empty string.
- `trailing_notrim`: Only the trailing delimiter is processed, and the returned result may contain an empty string.

Example:

```bash
split('a;;b;;c', ';', 'notrim') = ['a', '', 'b', '', 'c']
split('a;b;c', ';', 'leading') = ['a', 'b;c']
split('a;b;c', ';', 'trailing') = ['a;b', 'c']
split(';a;b;c', ';', 'leading_notrim') = ['', 'a;b;c']
split('a;b;c;', ';', 'trailing_notrim') = ['a;b;c', '']
```

### sprintf(Format, ...) -> string

Returns a string formatted according to `Format`. The `Format` string contains ordinary characters and control sequences used for formatting.

The format of the control sequence is generally: `~F.P.PadModC`.

Character `C` determines the type of control sequence to use. This is the only required field. `F`, `P`, `Pad` and `Mod` are all optional. For a detailed introduction to them, see: https://www.erlang.org/doc/man/io.html#fwrite-1.

Example:

```bash
sprintf('hello, ~s!', 'steve') = 'hello, steve!'
sprintf('count: ~p~n', 100) = 'count: 100\n'
```

### strlen(String: string) -> integer

Returns the length of `String`. Example:

```bash
strlen('hello') = 5
strlen('hello\n') = 6
```

### substr(String: string, Start: integer) -> string

Returns all characters in `String` starting from position `Start` to the end of the string. The subscript of the string starts from 0, that is, position 0 corresponds to "h" in the string "hello". Example:

```bash
substr('hello', 0) = 'hello'
substr('hello world', 6) = 'world'
```

### substr(String: string, Start: integer, Length: integer) -> string

Returns the substring starting from position `Start` in `String` and having a maximum length of `Length`. The subscript of the string starts from 0. Example:

```bash
substr('hello world!', 6, 5) = 'world'
```

### tokens(String: string, SeparatorList: string) -> array

Returns a list of substrings of `String` split by the characters in `SeparatorList`.

Two or more adjacent delimiters will be treated as one, so no empty string will occur.

Example:

```bash
tokens('a,b;c,d', ',;') = ['a', 'b', 'c', 'd']
tokens('a;;b', ';') = ['a', 'b']
```

### tokens(String: string, SeparatorList:string, NoCRLF: string) -> array

Same as `tokens/2`, but you can specify `NoCRLF` as `nocrlf` to split carriage return and line feed characters at the same time. Example:

```bash
tokens('a\rb\nc\r\nd', ';', 'nocrlf') = ['a', 'b', 'c', 'd']
```

### trim(String: string) -> string

Removes leading and trailing characters from a `String` that should be considered whitespace, such as spaces, tabs, form feeds, and newline characters. Note that `\r\n` is considered a grapheme cluster in the Unicode standard, so `\r\n` will be deleted altogether. Example:

```bash
trim('\t  hello  \n') = 'hello'
trim('\t  hello \r\n') = 'hello'
```

### unescape(String: string) -> string

The unescape function converts escape sequences back to their represented characters. When escape sequences are used in SQL, this function should be used to unescape them first for proper processing.

::: tip

This function has been introduced since EMQX v5.7.0.

:::

For example, when the Payload is a newline-separated string:

```bash
32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3
87.2
12.3
my-device
```

If you want to split the Payload into an array using `\n`. The following SQL will not execute as expected:

```sql
SELECT split(payload, '\n') as device_info FROM 't/#'
```

Output result:

```json
{
  "device_info": [
    "32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3\n87.2\n12.3\nmy-device"
  ]
}
```

Using the unescape function to unescape `\n`, you can get the desired result:

```sql
SELECT split(payload, unescape('\n')) as device_info FROM 't/#'
```

Output result:

```json
{
  "device_info": [
    "32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3",
    "87.2",
    "12.3",
    "my-device"
  ]
}
```

**The unescape function supports the following escape sequences:**

- Standard C escape sequences:

  - `\n` for newline (LF)
  - `\t` for horizontal tab (HT)
  - `\r` for carriage return (CR)
  - `\b` for backspace (BS)
  - `\f` for formfeed (FF)
  - `\v` for vertical tab (VT)
  - `\'` for single quote (')
  - `\"` for double quote (")
  - `\\` for backslash ()
  - `\?` for question mark (?)
  - `\a` for alert (bell, BEL)

- Hexadecimal escape codes:

  - `\xH...` where `H...` is one or more hexadecimal digits (0-9, A-F, a-f), allowing for the encoding of arbitrary utf32 characters.

If an escape sequence is not recognized, or if the hexadecimal escape does not form a valid Unicode character, the function throws an exception.

### upper(String: string) -> string

Converts lowercase letters in a `String` to uppercase letters. Example:

```bash
upper('hello') = 'Hello'
```

## Map Operation Functions

### map_get(Key: string, Map: map) -> any

Returns the value of the specified `Key` in the `Map`, or `undefined` if the `Key` does not exist in the Map. Example:

```bash
map_get('msg', json_decode('{"msg": "hello"}')) = 'hello'
map_get('data', json_decode('{"msg": "hello"}')) = undefined
```

### map_get(Key: srting, Map: map, Default: any) -> any

Same as `map_get/2`, but when `Key` does not exist, the specified `Default` will be returned. Example:

```bash
map_get('data', json_decode('{"msg": "hello"}'), '') = ''
map_get('value', json_decode('{"data": [1.2, 1.3]}'), []) = []
```

### map_keys(Map: map) -> array

Returns an array of all keys in the `Map`. Example:

```bash
map_keys(json_decode('{"a": 1, "b": 2}')) = ['a', 'b']
```

### map_put(Key: string, Value: any, Map: map) -> map

Insert the `Key` and associated `Value` into the `Map` and return the updated map. If the `Key` already exists in the original `Map`, the old associated value will be replaced with the new Value. Example:

```bash
map_get('b', map_put('b', 1, json_decode('{"a": 1}'))) = 1
map_get('a', map_put('a', 2, json_decode('{"a": 1}'))) = 2
```

### map_to_redis_hset_args(Map) -> list

::: tip 

This function has been introduced since EMQX v5.7.1.

:::

This function transforms a map into a list of field names and values, used for formatting the Redis `HSET` (or `HMSET`) command.

The conversion is specified by a rule such as `SELECT map_to_redis_hset_args(payload.value) as hset_fields FROM t/1`. This prepares the `hset_fields` variable for integration into a Redis action command template, formatted as `HMSET name1 ${hset_fields}`.

For instance, if `payload.value` is the map `{"a" : 1, "b": 2}`, the resulting command could be `HMSET name1 b 2 a 1`. Note that the order of the fields in the map is non-deterministic.

### map_to_entries(Map: map) -> array

Converts a `Map` into an array of objects containing `key` and `value` fields. Example:

```bash
map_to_entries(json_decode('{"a": 1, "b": 2}')) = [{"key": "a", "value": 1},{"key": "b", "value": 2}]
```

### map_values(Map: map) -> array

Returns an array of all values in the `Map`. Example:

```bash
map_values(json_decode('{"a": 1, "b": 2}')) = [1, 2]
```

### mget(Key: string | array, Map: map) -> any

Returns the value of the specified `Key` in the `Map`, or `undefined` if the `Key` does not exist in the `Map`. You can use an array to specify multiple keys at once to get associated values from a nested map. Example:

```bash
mget('c', json_decode('{"a": {"b": 1}}')) = undefined
json_decode(mget('a', json_decode('{"a": {"b": 1}}'))) = '{"b": 1}'
mget(['a', 'b'], json_decode('{"a": {"b": 1}}')) = 1
```

### mput(Key: string | array, Value: any, Map: map) -> map

Insert the `Key` and associated `Value` into the `Map` and return the updated map. If the `Key` already exists in the original `Map`, the old associated value will be replaced with the new value. You can use an array to specify multiple keys at once to insert data into a nested map. Example:

```bash
mget(['a', 'b'], mput(['a', 'b'], 2, json_decode('{"a": {"b": 1}}'))) = 2
mget(['a', 'b'], mput(['a', 'b'], 2, json_decode('{"c": 1}'))) = 2
```

## Array Operation Functions

### contains(Item: any, Array: array) -> boolean

Determine whether the array `Array` contains the specified `Item`. Example:

```bash
contains(2, [1, 2, 3]) = true
contains(2.3, [1.8, 2.5, 2.0]) = false
contains('John', ['John', 'David']) = true
contains([1, 2], [a, b, [1, 2]]) = true
contains(json_decode('{"a": 1}'), [json_decode('{"a": 1}'), json_decode('{"b": 2}')]) = true
```

### first(Array: array) -> any

Returns the first element in the array `Array`. `Array` cannot be empty. Example:

```bash
# Correct
first(['John', 'David']) = 'John'

# Wrong
first([])
```

### last(Array: array) -> any

Returns the last element in the array `Array`. `Array` cannot be empty. Example:

```bash
# Correct
last(['John', 'David']) = 'David'

# Wrong
last([])
```

### length(Array: array) -> integer

Returns the length of the array `Array`, that is, the number of elements in the `Array`. Example:

```bash
length([1,2,3,4]) = 4
length([]) = 0
```

### nth(N: integer, Array: array) -> any

Returns the Nth element in `Array`. `N` should not be larger than the length of `Array`. Example:

```bash
# Correct
nth(1, [1,2,3]) = 1

# Wrong
nth(0, [1,2,3])
nth(4, [1,2,3])
```

### sublist(Length: integer, Array: array) -> any

Returns a subarray starting from the 1st element in the array `Array` and having a maximum length of `Length`. If `Length` is greater than the length of `Array`, the entire array will be returned. Example:

```bash
sublist(3, [1,2,3,4]) = [1,2,3]
sublist(10, [1,2,3,4]) = [1,2,3,4]
```

### sublist(Start: integer, Length: integer, Array:array) -> any

Same as `sublist/2`, but you can use `Start` to specify which element to start returning from. If `Start` + `Length` is greater than the length of `Array`, the entire array will be returned. Example:

```bash
sublist(2, 10, [1,2,3,4]) = [2,3,4]
```

## Hashing Functions

### md5(String: string) -> string

Computes an MD5 hash value of a fixed length of 128 bits for a `String` of any length. The hash value will be returned as text consisting of 32 hexadecimal digits. The letters in the returned string are fixed to lowercase (a ~ f).

Example:

```bash
md5('hello') = '5d41402abc4b2a76b9719d911017c592'
```

### sha(String: string) -> string

Computes a SHA hash value of a fixed length of 160 bits for a `String` of any length using the **SHA-1** algorithm. The hash value will be returned as text consisting of 40 hexadecimal digits. The letters in the returned string are fixed to lowercase (a ~ f).

Example:

```bash
sha('hello') = 'aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d'
```

### sha256(String: string) -> string

Computes a SHA hash value of a fixed length of 256 bits for a `String` of any length using the **SHA-2** algorithm. The hash value will be returned as text consisting of 64 hexadecimal digits. The letters in the returned string are fixed to lowercase (a ~ f).

Example:

```bash
sha256('hello') = '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824'
```

## Compression and Decompression Functions

Note: Binary data cannot be JSON encoded directly, you must call the `bin2hexstr` function to convert it into the corresponding string composed of hexadecimal digits.

### gunzip(Data: binary) -> binary | string

To decompress `Data`, `Data` must contain a gz header and a checksum at the end. Example:

```bash
gunzip(hexstr2bin('1F8B0800000000000013CB48CDC9C9070086A6103605000000')) = 'hello'
```

### gzip(Data: binary | string) -> binary

Use the DEFLATE algorithm to compress `Data`, and the returned compression result includes the gz header and the checksum at the tail. Example:

```bash
bin2hexstr(gzip('hello')) = '1F8B0800000000000013CB48CDC9C9070086A6103605000000'
```

### unzip(Data: binary) -> binary | string

Decompress `Data`. `Data` should not contain the zlib header and the checksum at the tail. Example:

```bash
unzip(hexstr2bin('CB48CDC9C90700')) = 'hello'
```

### zip(Data: binary | string) -> binary

Use the DEFLATE algorithm to compress `Data`, and the returned compression result does not include the zlib header and the checksum at the tail. Example:

```bash
bin2hexstr(zip('hello')) = 'CB48CDC9C90700'
```

### zip_compress(Data: binary | string) -> binary

Use the DEFLATE algorithm to compress `Data`. The returned compression result contains the zlib header and the checksum at the tail. Example:

```bash
bin2hexstr(zip_compress('hello')) = '789CCB48CDC9C90700062C0215'
```

### zip_uncompress(Data: binary) -> binary | string

To decompress `Data`, `Data` must contain a zlib header and a checksum at the end. Example:

```bash
zip_uncompress(hexstr2bin('789CCB48CDC9C90700062C0215')) = 'hello'
```

## Bit Operation Functions

### bitand(Num1: integer, Num2: integer) -> integer

Returns the **bitwise AND** result of `Num1` and `Num2`. Both input and output are signed integers. Example:

```bash
bitand(10, 8) = 8
bitand(-10, -8) = -16
```

### bitnot(Num: integer) -> integer

Returns the **bitwise negation** result of `Num`. Both input and output are signed integers. Example:

```bash
bitnot(10) = -11
bitnot(-12) = 11
```

### bitsl(Num: integer, Shift: integer) -> integer

Shift `Num` bitwise to the left by `Shift` bits, filling the right margin with 0. Example:

```bash
bitsl(8, 2) = 32
bitsl(-8, 2) = -32
```

### bitsr(Num: integer, Shift: integer) -> integer

Shift `Num` to the right by `Shift` bits, and fill the left blank with the sign bit (that is, 0 for positive numbers and 1 for negative numbers). Example:

```bash
bitsr(8, 2) = 2
bitsr(8, 4) = 0
bitsr(-8, 2) = -2
bitsr(-8, 6) = -1
```

### bitor(Num1: integer, Num2: integer) -> integer

Returns the **bitwise OR** result of `Num1` and `Num2`. Example:

```bash
bitor(10, 8) = 10
bitor(-10, -8) = -2
```

### bitxor(Num1: integer, Num2: integer) -> integer

Returns the **bitwise XOR** result of `Num1` and `Num2`. Example:

```bash
bitxor(10, 8) = 2
bitxor(-10, -8) = 14
```

## Bit Sequence Operation Functions

The rule engine provides functions for manipulating bit sequences. For example `subbits` is used to extract a sequence of bits and convert it to a specified data type.

:::tip

The `binary` type represents a byte sequence, each byte consists of 8 bits, so the number of bits in any binary must be an integer multiple of 8. The `bitstring` type represents a bit sequence, which can consist of any number of bits.

Put simply, while every `binary` is a `bitstring`, the reverse is not always true.

It's important to note that `bitstring`, when its length is not divisible by 8, is not directly serializable to external formats like JSON.

Typically, it serves as an intermediate value before being converted to an integer or other suitable types.

:::

### bitsize(Bin: binary) -> integer

Returns the number of bits in the bit sequence `Bin`. Example:

```bash
bitsize('abc') = 24
bitsize('你好') = 48
```

### byteszie(Bin: binary) -> integer

Returns the number of bytes in the byte sequence `Bin`. Example:

```bash
byteszie('abc') = 3
byteszie('你好') = 6
```

### subbits(Bin: binary, BitNum: integer) -> integer

Starting from the starting position of the byte sequence `Bin`, obtain the bits of length `BitNum` and convert them into unsigned integers according to the big-endian sequence. This function is equivalent to `subbits(Bytes, 1, BitNum, 'integer', 'unsigned', 'big')`.

Example:

```bash
# 159 = 0x9F
subbits(hexstr2bin('9F4E58'), 8) = 159

# 40782 = 0x9F4E
subbits(hexstr2bin('9F4E58'), 16) = 40782

# bin2hexstr(base64_decode('n05Y')) = '9F4E58'
subbits(base64_decode('n05Y'), 8) = 159
```

### subbits(Bin: binary, Start: integer, BitNum: integer) -> integer

Starting from the position `Start` of the byte sequence `Bin` (the starting position is 1), obtain the bits with a length of `BitNum` and convert them into an unsigned integer according to the big-endian sequence. This function is equivalent to `subbits(Bytes, Start, BitNum, 'integer', 'unsigned', 'big')`.

Example:

```bash
# 159 = 0x9F
subbits(hexstr2bin('9F4E58'), 1, 8) = 159

# 78 = 0x4E
subbits(hexstr2bin('9F4E58'), 9, 8) = 78

# bin2hexstr(base64_decode('n05Y')) = '9F4E58'
subbits(base64_decode('n05Y'), 9, 4) = 4
```

### subbits(Bin: binary, Start: integer, BitNum: integer, OutputType: string, Signedness: string, Endianness: string) -> bitstring | integer | float

Starting from position `Start` of the byte sequence `Bin` (the starting position is 1), obtain the bits of length `BitNum`, and in accordance with the requested byte order, `Endianness`, and sign attribute, `Signedness`, convert them into data of the specified type `OutputType`.

Possible values for `OutputType` are:

- bits: abbreviation of bitstring
- integer
- float

Possible values for `Signedness` are:

- signed
- unsigned

Possible values for `Endianness` are:

- big
- little

Note that when `OutputType` is `float`, the parameter `Signedness` does not take effect. When `OutputType` is `bits`, the parameters `Signedness` and `Endianness` do not take effect.

Example:

```bash
# 40782 = 0x9F4E
subbits(hexstr2bin('9F4E58'), 1, 16, 'integer', 'unsigned', 'big') = 40782
subbits(hexstr2bin('9F4E58'), 1, 16, 'integer', 'signed', 'big') = -24754

# 20127 = 0x4E9F
subbits(hexstr2bin('9F4E58'), 1, 16, 'integer', 'unsigned', 'little') = 20127

subbits(hexstr2bin('9F4E58'), 1, 16, 'float', 'unsigned', 'big') = -0.00713348388671875
subbits(hexstr2bin('9F4E58'), 1, 16, 'float', 'signed', 'big') = -0.00713348388671875
```

## Encoding and Decoding Functions

### base64_decode(Data: string) -> bytes | string

Encode `Data` to base64 format. Example:

```bash
base64_decode('aGVsbG8=') = 'hello'
bin2hexstr(base64_decode('y0jN')) = 'CB48CD'
```

### base64_encode(Data: binary | string) -> string

Decode `Data` from base64 format. Example:

```bash
base64_encode('hello') = 'aGVsbG8='
base64_encode(hexstr2bin('CB48CD')) = 'y0jN'
```

### json_decode(Data: string) -> array | map

Decode `Data` from JSON format. Example:

```bash
map_get('a', json_decode('{"a": 1}')) = 1
```

### json_encode(Data: array | map) -> string

Encode `Data` to JSON format. Example:

```bash
json_encode([1,2,3]) = '[1,2,3]'
```

### bin2hexstr(Data: binary) -> string

Convert binary data to the corresponding string of hexadecimal digits. Example:

```bash
bin2hexstr(zip('hello')) = 'CB48CDC9C90700'
```

### hexstr2bin(Data: string) -> binary

Converts a string of hexadecimal digits to the corresponding binary data. Example:

```bash
unzip(hexstr2bin('CB48CDC9C90700')) = 'hello'
```

### Schema Registry Functions

::: tip

The Schema Resigtry is an EMQX Enterprise edition feature.

:::

EMQX Enterprise also supports using `schema_encode` and `schema_decode` functions to decode and encode [Protobuf (Protocol Buffers)](https://developers.google.com/protocol-buffers) and [Avro](https://avro.apache.org/) data according to a specified schema. You can read more about these functions in [Schema Registry](./schema-registry.md). 

### schema_encode(SchemaID: string, Data: map) -> binary

Encodes `Data` using the specified Avro Schema. Create a schema in the Schema Registry to get the ID.

### schema_encode(SchemaID: string, Data: map, MsgType: string) -> binary

Encodes `Data` using the specified Protobuf Schema. Create a schema in the Schema Registry to get the ID. `MsgType` is used to specify the message type corresponding to `Data` in Protobuf Schema.

### schema_decode(SchemaID: string, Bin: binary) -> map

Decodes `Bin` using the specified Avro Schema. Create a schema in the Schema Registry to get the ID.

### schema_decode(SchemaID: string, Bin: binary, MsgType: string) -> map

Decodes `Bin` using the specified Protobuf Schema. Create a schema in the Schema Registry to get the ID. `MsgType` is used to specify the message type corresponding to Data in Protobuf Schema.

### **Sparkplug B Functions**

EMQX Enterprise also has special purpose functions for decoding and encoding Sparkplug B messages (`sparkplug_decode` and `sparkplug_encode`). You can read more about the sparkplug functions in [Sparkplug B](./sparkplug.md).

## Date and Time Conversion Functions

### date_to_unix_ts(Unit: string, FormatString: string, DateTimeString: string) -> integer

Parses the datetime string `DateTimeString` according to the format string `FormatString`, converting it to Unix time in specified time unit `Unit`.

`second`, `millisecond`, `microsecond` and `nanosecond` are available Units.

The placeholders that can be used in `FormatString` are as follows:

| Placeholder | Meaning | Value range |
| ------ | ---------------------------------- | ----- ---------------- |
| `%Y` | Four-digit year | 0000 - 9999 |
| `%m` | Two-digit month | 01 - 12 |
| `%d` | Two-digit day of the month | 01 - 31 |
| `%H` | Two-digit hour in 24-hour format | 00 - 24 |
| `%M` | Two-digit minute | 00 - 59 |
| `%S` | Two-digit second | 00 - 59 |
| `%N` | Nanoseconds | 000000000 - 999999999 |
| `%6N` | Microseconds, the first six digits of nanoseconds | 000000 - 999999 |
| `%3N` | Milliseconds, the first three digits of nanoseconds | 000 - 999 |
| `%z` | Time zone offset in the format `±hhmm` | -1159 - +1159 |
| `%:z` | Time zone offset in the format `±hh:mm` | -11:59 - +11:59 |
| `%::z` | Time zone offset in the format `±hh:mm:ss` | -11:59:59 - +11:59:59 |

Example:

```bash
date_to_unix_ts('second', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00+08:00') = 1708671600
```

### date_to_unix_ts(Unit: string, Offset: string | integer, FormatString: string, DateTimeString: string) -> integer

If the `DateTimeString` does not contain a time zone offset, you can use `Offset` to manually specify the offset, other behavior are the same as `date_to_unix_ts/3`. `Offset` can be a string or the number of seconds expressed directly as an integer.

When `Offset` is a string, the following format can be used:

- `Z` or `z`, representing UTC offset 00:00.
- `±hh[:mm][:ss]` or `±hh[mm][ss]`, positive or negative time offset from UTC.
- `local`, indicates the offset corresponding to the system's local time zone.

Example:

```bash
date_to_unix_ts('second', '+08:00', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00') = 1708671600
date_to_unix_ts('second', 'Z', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 07:00:00') = 1708671600
date_to_unix_ts('second', 14400, '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00') = 1708686000
```

### format_date(Unit: string, Offset: string | integer, FormatString: string, Time: Integer) -> string

Converts a Unix time to a datetime string in the specified format. `Unit` represents the time unit of the Unix time Time to be converted, `Offset` represents the time zone offset in the output date and time, and `FormatString` represents the output date and time format.

See `date_to_unix_ts/3, 4` for possible values of `Unit`, `Offset` and `FormatString`.

Example:

```bash
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%z', 1708933353472) = '2024-02-26 15:42:33.472000+0800'
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%:z', 1708933353472) = '2024-02-26 15:42:33.472000+08:00'
format_date('millisecond', '+08:20:30', '%Y-%m-%d %H:%M:%S.%3N%::z', 1708933353472) = '2024-02-26 16:03:03.472+08:20:30'
format_date('millisecond', 'Z', '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 07:42:33.472+08:00'
format_date('millisecond', 28800, '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 15:42:33.472+08:00'
```

### now_rfc3339() -> string

Returns the current system time as an RFC3339 datetime string in seconds. Example:

```bash
now_rfc3339() = '2024-02-23T10:26:20+08:00'
```

### now_rfc3339(Unit: string) -> string

Same as `now_rfc3339/0`, but you can use `Unit` to specify the time unit, supporting `second`, `millisecond`, `microsecond` and `nanosecond`. Example:

```bash
now_rfc3339('microsecond') = '2024-02-23T10:26:38.009706+08:00'
```

### now_timestamp() -> integer

Returns the current system time as a Unix timestamp in seconds. Example:

```bash
now_timestamp() = 1708913853
```

### now_timestamp(Unit: string) -> integer

Same as `now_timestamp/0`, but you can use `Unit` to specify the time unit, supporting `second`, `millisecond`, `microsecond` and `nanosecond`. Example:

```bash
now_timestamp('microsecond') = 1708913828814315
```

### rfc3339_to_unix_ts(DateTimeString: string) -> integer

Converts an RFC3339-compliant datetime string to a Unix timestamp. `2024-02-23T15:56:30Z` is a typical RFC3339 date and time string, which represents UTC time on February 23, 2024, 15:56:30.

Example:

```bash
rfc3339_to_unix_ts('2024-02-23T15:56:30Z') = 1708703790
rfc3339_to_unix_ts('2024-02-23T15:56:30+08:00') = 1708674990
```

### rfc3339_to_unix_ts(DateTimeString: string, Unit: string) -> integer

Same as `rfc3339_to_unix_ts/1`, but you can use `Unit` to specify the unit of returned Unix timestamp, supporting `second`, `millisecond`, `microsecond` and `nanosecond`. Example:

```bash
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'second') = 1708703790
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'millisecond') = 1708703790870
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'microsecond') = 1708703790870000
rfc3339_to_unix_ts('2024-02-23T15:56:30.535904509Z', 'nanosecond') = 1708703790535904509
```

### timezone_to_offset_seconds(Offset: string) -> integer

Converts a time zone offset as a string to an integer in seconds. The following are supported time offset representations:

- `Z` or `z`, representing UTC offset 00:00.
- `±hh[:mm][:ss]` or `±hh[mm][ss]`, positive or negative time offset from UTC.
- `local`, indicates the offset corresponding to the system's local time zone.

Example:

```bash
timezone_to_offset_seconds('Z') = 0
timezone_to_offset_seconds('+08:00') = 28800
timezone_to_offset_seconds('local') = 28800
```

### unix_ts_to_rfc3339(Time: integer) -> string

Converts a Unix timestamp in seconds to an RFC3339-compliant datetime string, using the system's local time zone. Example:

```bash
unix_ts_to_rfc3339(1708671600) = '2024-02-23T15:00:00+08:00'
```

### unix_ts_to_rfc3339(Time: integer, Unit: string) -> string

Same as `unix_ts_to_rfc3339/0`, but you can use `Unit` to specify the time unit, supporting `second`, `millisecond`, `microsecond` and `nanosecond`. Example:

```bash
unix_ts_to_rfc3339(1708671600766, 'millisecond') = '2024-02-23T15:00:00.766+08:00'
```

### MongoDB Time Functions

::: tip

Functions in this section applies to the EMQX Enterprise edition only.

:::

### mongo_date() -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

Returns the current time as a MongoDB ISODate type or string. Only supported for use in MongoDB related actions and SQL tests, and only in SQL tests `mongo_date()` returns a string, such as `ISODate("2024-02-23T15:00:00.123Z")`. Returns other than strings from `mongo_date()` are not currently supported as input to other functions.

Example:

```bash
mongo_date() = 'ISODate("2024-02-23T15:00:00.123Z")'
```

### mongo_date(Timestamp: integer) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

Converts the specified Unix timestamp in milliseconds to the MongoDB ISODate type or string. Other behaviors are the same as `mongo_date/0`.

Example:

```bash
mongo_date(now_timestamp('millisecond')) = 'ISODate(2024-02-23T15:48:57.871Z)'
```

### mongo_date(Timestamp: integer, Unit: string) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

Converts the specified Unix timestamp to the MongoDB ISODate type or string. You can specify the unit of the input timestamp through `Unit`. Other behaviors are the same as `mongo_date/0`.

Possible values for `Unit` are:

- `second`
- `millisecond`
- `microsecond`
- `nanosecond`

Example:

```bash
mongo_date(now_timestamp('microsecond'), 'microsecond') = 'ISODate(2024-02-23T15:51:01.232Z)'
```

## UUID Functions

### uuid_v4() -> string

Generates a version 4 UUID. Example:

```bash
uuid_v4() = 'f5bb7bea-a371-4df7-aa30-479add04632b'
```

### uuid_v4_no_hyphen() -> string

Generates a version 4 UUID without hyphens. Example:

```bash
uuid_v4_no_hyphen() = 'd7a39aa4195a42068b962eb9a665503e'
```

## System Function

### getenv(Name)

Return the value of the environment variable `Name` with the following constraints:

- Prefix `EMQXVAR_` is added before reading from OS environment variables. For example, `getenv('FOO_BAR')` is to read `EMQXVAR_FOO_BAR`.
- Values are immutable once loaded from the OS environment.

## Conditional Functions

### coalesce(Value1: any, Value2: any) -> any

Returns `Value2` if `Value1` is null.
This is useful in cases where you want to check if a data field is null and replace it with a default value.

For example, `coalesce(payload.value, 0)` returns `payload.value` if it is not null, or `0` if it is null.
It's equivalent to SQL expression `CASE WHEN is_null(payload.value) THEN 0 ELSE payload.value END`, but more concise.

::: tip Note

In EMQX rule SQL, a null-value's string form is by default `'undefined'`.

:::

### coalesce_ne(Value1: any, Value2: any) -> any

Similar to `coalesce`, but returns `Value2` if `Value1` is null or empty string.

::: tip Note

In EMQX rule SQL, a null-value's string form is by default `'undefined'`.

:::
