# Functions available in SQL statements
## Mathematical functions

|function name|Purpose|parameter|Returned value|
|--- |--- |--- |--- |
|abs|Absolute value|Operand|absolute value|
|cos|Cosine|Operand|Cosine value|
|cosh|Hyperbolic cosine|Operand|Hyperbolic cosine value|
|acos|Inverse cosine|Operand|Inverse cosine value|
|acosh|Inverse hyperbolic cosine|Operand|Inverse hyperbolic cosine value|
|sin|Sine|Operand|Sine value|
|sinh|Hyperbolic sine|Operand|Hyperbolic sine value|
|asin|Arcsine|Operand|Arcsine value|
|asinh|inverse hyperbolic sine|Operand|inverse hyperbolic sine value|
|tan|tangent|Operand|tangent value|
|tanh|Hyperbolic tangent|Operand|Hyperbolic tangent value|
|atan|Arc tangent|Operand|Arc tangent value|
|atanh|Inverse hyperbolic tangent|Operand|Inverse hyperbolic tangent value|
|ceil|Round up|Operand|Integer value|
|floor|Round down|Operand|Integer value|
|round|rounding|Operand|Integer value|
|exp|Exponentiation|Operand|X power of e|
|power|Exponential operation|1. Left operand x <br />2. Right operand y|Y power of X|
|sqrt|Square root operation|Operand|Square root|
|fmod|Floating point modulus function|1. left Operand <br />2.right Operand|module|
|log|Logarithm to e|Operand|value|
|log10|Logarithm to 10|Operand|value|
|log2|Logarithm to 2|Operand|value|

```erlang
abs(-12) = 12
cos(1.5) = 0.0707372016677029
cosh(1.5) = 2.352409615243247
acos(0.0707372016677029) = 1.5
acosh(2.352409615243247) = 1.5
sin(0.5) = 0.479425538604203
sinh(0.5) = 0.5210953054937474
asin(0.479425538604203) = 0.5
asinh(0.5210953054937474) = 0.5
tan(1.4) = 5.797883715482887
tanh(1.4) = 0.8853516482022625
atan(5.797883715482887) = 1.4
atanh(0.8853516482022625) = 1.4000000000000001
ceil(1.34) = 2
floor(1.34) = 1
round(1.34) = 1
round(1.54) = 2
exp(10) = 22026.465794806718
power(2, 10) = 1024
sqrt(2) = 1.4142135623730951
fmod(-32, 5) = -2
log10(1000) = 3
log2(1024) = 10
```

## Data type judgment function


|Function name|Purpose|parameter|Returned value|
|--- |--- |--- |--- |
|is_null|Judge if the variable is null|Data|Boolean data.if it is empty (undefined), return true, otherwise return false|
|is_not_null|Judge if the variable is not null|Data|Boolean data.if it is empty (undefined), return false, otherwise return true|
|is_str|Judge whether the variable is String type|Data|Boolean data.|
|is_bool|Judge if the variable is Boolean type|Data|Boolean data.|
|is_int|Judge whether the variable is Integer type|Data|Boolean data.|
|is_float|Judge whether the variable is Float type|Data|Boolean data.|
|is_num|Judge whether the variable is a numeric type, including Integer and Float types|Data|Boolean data.|
|is_map|Judge whether the variable is Map type|Data|Boolean data.|
|is_array|Judge whether the variable is Array type|Data|Boolean data.|


```erlang
is_null(undefined) = true
is_not_null(1) = true
is_str(1) = false
is_str('val') = true
is_bool(true) = true
is_int(1) = true
is_float(1) = false
is_float(1.234) = true
is_num(2.3) = true
is_num('val') = false
```

## Data type conversion function

|function name|purpose|parameter|returned value|
|--- |--- |--- |--- |
|str|Convert data to String type|Data|Data of type String. Failure to convert will cause SQL matching to fail|
|str_utf8|Convert data to UTF-8 String type|Data|UTF-8 String type data. Failure to convert will cause SQL matching to fail|
|bool|Convert data to Boolean type|Data|Boolean data. Failure to convert will cause SQL matching to fail|
|int|Convert data to integer type|Data|Integer type data. Failure to convert will cause SQL matching to fail|
|float|Convert data to floating type|Data|Floating type data. Failure to convert will cause SQL matching to fail|
|map|Convert data to Map type|Data|Map type data. Failure to convert will cause SQL matching to fail|


```erlang
str(1234) = '1234'
str_utf8(1234) = '1234'
bool('true') = true
int('1234') = 1234
float('3.14') = 3.14
```


## String functions

|Function name|Purpose|parameter|returned value|
|--- |--- |--- |--- |
|lower|convert to lowercase|input string|Lowercase string|
|upper|convert to uppercase|input string|uppercase string|
|trim|Remove left and right space|input string|output string|
|ltrim|Remove the left space|input string|output string|
|rtrim|Remove the right space|input string|output string|
|reverse|String inversion|input string|output string|
|strlen|string length|input string|Integer value|
|substr|Take a substring of characters|1. input string <br />2. Start position. Note: Subscripts start at 1|substring|
|substring|Take a substring of characters|1. input string <br />2. Start position <br />3. End position. Note: Subscripts start at 1|substring|
|split|String splitting|1. input string <br />2. split string|Split string array|
|split|String splitting|1. input string <br />2. split string <br />3. Find the first separator on the left or right, optional value is 'leading' or 'trailing'|Split string array|
|split|split string|1. input string <br />2. split string <br />3. Find the first separator on the left or right, optional value is 'leading' or 'trailing'|Split string array|


```erlang
lower('AbC') = 'abc'
lower('abc') = 'abc'

upper('AbC') = 'ABC'` `lower('ABC') = 'ABC'

trim(' hello  ') = 'hello'

ltrim(' hello  ') = 'hello  '

rtrim(' hello  ') = ' hello'

reverse('hello') = 'olleh'

strlen('hello') = 5

substr('abcdef', 2) = 'cdef'
substr('abcdef', 2, 3) = 'cde'

split('a/b/ c', '/') = ['a', 'b', ' c']
split('a/b/ c', '/', 'leading') = ['a', 'b/ c']
split('a/b/ c', '/', 'trailing') = ['a/b', ' c']

```

## Map function

|function name|purpose|parameter|returned value|
|--- |--- |--- |--- |
|map_get|Take the value of a Key in the Map, or return a null value if failed|1. Key <br />2. Map|The value of a Key in the Map. Support nested keys, such as "a.b.c"|
|map_get|Take the value of a Key in the Map, if failed, return the specified default value|1. Key <br />2. Map <br />3. Default Value|The value of a Key in the Map. Support nested keys, such as "a.b.c"|
|map_put|Insert value into Map|1. Key <br />2. Value <br />3. Map|The inserted Map. Support nested keys, such as "a.b.c"|


```erlang
map_get('a', json_decode( '{ "a" : 1 }' )) = 1
map_get('b', json_decode( '{ "a" : 1 }' ), 2) = 2
map_get('a', map_put('a', 2, json_decode( '{ "a" : 1 }' ))) = 2
```


## Array function

|function name|purpose|parameter|returned value|
|--- |--- |--- |--- |
|nth|Take the nth element, and subscripts start at 1|Original array|Nth element|
|length|Get the length of an array|Original array|the length of an array|
|sublist|Take a sub-array of length len starting from the first element. Subscripts start at 1|1. length len <br />2. Original array|sub-array|
|sublist|Take a sub-array of length len starting from the nth element. Subscripts start at 1|1. start position n <br />2. length len <br />3. Original array|sub-array|
|first|Take the first element. Subscripts start at 1|Original array|1st element|
|last|take the last element|Original array|the last element|
|contains|Determine whether the data is in the array|1. data <br />2. Original array|Boolean value|


```erlang
nth(2, [1,2,3,4]) = 2
length([1,2,3,4]) = 4
sublist(3, [1,2,3,4]) = [1,2,3,4]
sublist(1,2,[1,2,3,4]) = [1, 2]
first([1,2,3,4]) = 1
last([1,2,3,4]) = 4
contains(2, [1,2,3,4]) = true
```

## Hash function

|function name|purpose|parameter|returned value|
|--- |--- |--- |--- |
|md5|evaluate MD5|data|MD5 value|
|sha|evaluate SHA|data|SHA value|
|sha256|evaluate SHA256|data|SHA256 value|


```erlang
md5('some val') = '1b68352b3e9c2de52ffd322e30bffcc4'
sha('some val') = 'f85ba28ff5ea84a0cbfa118319acb0c5e58ee2b9'
sha256('some val') = '67f97635d8a0e064f60ba6e8846a0ac0be664f18f0c1dc6445cd3542d2b71993'
```

## Bit functions

| Function  | Purpose| Parameters| Returned value | 
|-----------|-------------|--------|----------------------|
| `subbits` | Get a given length of bits from the beginning of a binary, and then convert it to an unsigned integer (big-endian).  | 1. The binary <br />2. The length of bits to get | The unsigned integer |
| `subbits` | Get a given length of bits start from the specified offset of a binary, and then convert it to an unsigned integer (big-endian). Offsets are start from 1. | 1. The binary <br />2. The offset <br />3. The length of bits to get| The unsigned integer |
| `subbits` | Get a given length of bits start from the specified offset of a binary, and then convert it to a data type according to the arguments provided. Offsets are start from 1. | 1. The binary <br />2. The offset <br />3. The length of bits to get <br />4. Data Type, can be one of 'integer', 'float', 'bits' <br />5. Signedness, only works for integers, can be one of 'unsigned', 'signed', <br />6. Endianness, only works for integers, can be one of 'big', 'little' | The data got from the binary |

```erlang
subbits('abc', 8) = 97
subbits('abc', 9, 8) = 98
subbits('abc', 17, 8) = 99
subbits('abc', 9, 16, 'integer', 'signed', 'big') = 25187
subbits('abc', 9, 16, 'integer', 'signed', 'little') = 25442
```


## Decoding and encoding functions

{% emqxce %}

| Function | Purpose                             |        Parameters         | Returned value    |
| -------- | ------------------------------------|-------------------------- | --------------------------- |
| `base64_encode` | BASE64 encode   | The binary to be encoded | The encoded base64-formatted string |
| `base64_decode` | BASE64 decode   | The base64-formatted string to be decoded | The decoded binary |
| `json_encode` | JSON encode   | The data to be encoded | The JSON string |
| `json_decode` | JSON decode   | The JSON string to be decoded | The decoded data |
| `bin2hexstr` | Binary to Hex String | The binary | The hex string |
| `hexstr2bin` | Binary to Hex String | The hex string | The binary |


{% endemqxce %}


{% emqxee %}


| Function | Purpose                             |        Parameters         | Returned value |
| -------- | ------------------------------------|------------------------- | --------------------------- |
| `base64_encode` | BASE64 encode   | The binary to be encoded | The encoded base64-formatted string |
| `base64_decode` | BASE64 decode   | The base64-formatted string to be decoded | The decoded binary |
| `json_encode` | JSON encode   | The data to be encoded | The JSON string |
| `json_decode` | JSON decode   | The JSON string to be decoded | The decoded data |
| `schema_encode` | Encode according to schema. This requires the [schema registry](schema-registry.md) | 1. The Schema ID defined by schema registry 2. The data to be encoded 3..N. The remaining arguments according to the schema type | The encoded data |
| `schema_decode` | Decode according to schema. This requires the [schema registry](schema-registry.md) | 1. The Schema ID defined by schema registry 2. The data to be decoded 3..N. The remaining arguments according to the schema type | The decoded data |
| `bin2hexstr` | Binary to Hex String | The binary | The hex string |
| `hexstr2bin` | Binary to Hex String | The hex string | The binary |

{% endemqxee %}


```erlang
base64_encode('some val') = 'c29tZSB2YWw='
base64_decode('c29tZSB2YWw=') = 'some val'
json_encode(json_decode( '{ "a" : 1 }' )) = '{"a":1}'
bin2hexstr(hexstr2bin('ABEF123')) = 'ABEF123'
```

## Time functions

| Function | Purpose  | Parameters     | Returned value |
| -------- | -------- |--------------- | -------------- |
| `format_date` | Get the time and output the time string in the specified format | 1. time precision <br />2. time offset <br />3. time format string | string |
| `format_date` | Output string with integer timestamp in specified format | 1. time precision<br />2.  time offset<br />3. time format string<br />4. timestamp | string |
| `date_to_unix_ts` | Parse a string using the specified format | 1. time precision<br />2.  time offset<br />3. time format string<br />4. string | integer |

Time Precision

second,
millisecond,
micorsecond,
nanosecond,

Time Offset

The user can specify the time zone of the output time through this string, For example, the East Eight time zone can be represented by "+08:00". If the input is empty, the system default time zone will be used.

Time Format String

The user can specify the output style through the format string.

| Parameters | Description |
| ---- | ---- |
| %y | year |
| %m | month |
| %d | day |
| %H | hour， 24-hour clock |
| %M | minute |
| %S | second，the time precision parameter will affect its output |
| %Z | time zone，the time offset parameter will affect its output |

The user can parse the time string using the specified format.

```erlang
format_date('nanosecond', '+08:00', '%y-%m-%d %H:%M:%S%Z') = '2022-04-15 19:05:55.930812260+08:00'
format_date('nanosecond', '+08:00', '%y-%m-%d %H:%M:%S%Z', 1650020755930812200) = '2022-04-15 19:05:55.930812200+08:00'
date_to_unix_ts('nanosecond', '+08:00', '%y-%m-%d %H:%M:%S%Z', '2022-04-15 19:05:55.930812260+08:00') = 1650020755930812200
```
