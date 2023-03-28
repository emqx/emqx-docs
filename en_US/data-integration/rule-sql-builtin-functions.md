# Built-in SQL Functions

The EMQX rule engine's SQL like language supports a variety of built-in functions for doing basic data transformation.
These function are described in this document.
The most powerful build-in function is the JQ function, which is documented in a [seperate document](./rule-sql-jq.md).
The JQ function allows you to do complex data transformation using the [JQ language](https://stedolan.github.io/jq/manual/).
However, using JQ is not always necessary, and the built-in functions in this document can often get the job done.
It is recommended to use the built-in functions described in this document when possible, and only use JQ if the built-in functions are not sufficient as JQ is a more complex language can therefore increase the risk of bugs.
How to use the build-in functions in rules is described in the [rule-engine language documentation](./rule-sql-syntax.md).


## Mathematical Functions

| Function Name | Purpose                         | Parameter                                  | Returned Value                   |
| ------------- | ------------------------------- | ------------------------------------------ | -------------------------------- |
| abs           | Absolute value                  | Operand                                    | absolute value                   |
| cos           | Cosine                          | Operand                                    | Cosine value                     |
| cosh          | Hyperbolic cosine               | Operand                                    | Hyperbolic cosine value          |
| acos          | Inverse cosine                  | Operand                                    | inverse cosine value             |
| acosh         | Inverse hyperbolic cosine       | Operand                                    | inverse hyperbolic cosine value  |
| sin           | Sine                            | Operand                                    | sine value                       |
| sinh          | Hyperbolic sine                 | Operand                                    | hyperbolic sine value            |
| asin          | Arcsine                         | Operand                                    | arcsine value                    |
| asinh         | inverse hyperbolic sine         | Operand                                    | Inverse hyperbolic sine value    |
| tan           | tangent                         | Operand                                    | Tangent value                    |
| tanh          | Hyperbolic tangent              | Operand                                    | hyperbolic tangent value         |
| atan          | Arc tangent                     | Operand                                    | arc tangent value                |
| atanh         | Inverse hyperbolic tangent      | Operand                                    | inverse hyperbolic tangent value |
| ceil          | Round up                        | Operand                                    | integer value                    |
| floor         | Round down                      | Operand                                    | integer value                    |
| round         | rounding                        | Operand                                    | integer value                    |
| exp           | Exponentiation                  | Operand                                    | x power of e                     |
| power         | Exponential operation           | 1. Left operand x <br />2. Right operand y | y power of X                     |
| sqrt          | Square root operation           | Operand                                    | square root                      |
| fmod          | Floating point modulus function | 1. left Operand <br />2.right Operand      | modulo (remainder)               |
| log           | Logarithm to e                  | Operand                                    | the natural logarithm            |
| log10         | Logarithm to 10                 | Operand                                    | logarithm with base 10           |
| log2          | Logarithm to 2                  | Operand                                    | logarithm with base 2            |

**Examples:**

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

| Function name | Purpose                                                                         | parameter | Returned value                                                               |
| ------------- | ------------------------------------------------------------------------------- | --------- | ---------------------------------------------------------------------------- |
| is_null       | Check if a field is undefined                                                   | Data      | Boolean data.                                                                |
| is_not_null   | Check if a field is not undefined                                               | Data      | Boolean data.                                                                |
| is_str        | Check whether the value is of String type                                       | Data      | Boolean data.                                                                |
| is_bool       | Check if the value is of Boolean type                                           | Data      | Boolean data.                                                                |
| is_int        | Check if the value is of Integer type                                           | Data      | Boolean data.                                                                |
| is_float      | Check if the value is of Float type                                        | Data      | Boolean data.                                                                |
| is_num        | Check if the value is of numeric type, including Integer and Float types | Data      | Boolean data.                                                                |
| is_map        | Check if the value is of Map type                                          | Data      | Boolean data.                                                                |
| is_array      | Check if the value is of Array type                                        | Data      | Boolean data.                                                                |


**Examples:**


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

## Data Type Conversion Functions

| function name | purpose                                             | parameter                    | returned value                                                             |
| ------------- | --------------------------------------------------- | ---------------------------- | -------------------------------------------------------------------------- |
| str           | Convert data to String type                         | Data                         | Data of type String. Failure to convert will cause SQL matching to fail    |
| str_utf8      | Convert data to UTF-8 String type                   | Data                         | UTF-8 String type data. Failure to convert will cause SQL matching to fail |
| bool          | Convert data to Boolean type                        | Data                         | Boolean data. Failure to convert will cause SQL matching to fail           |
| int           | Convert data to integer type                        | Data                         | Integer type data. Failure to convert will cause SQL matching to fail      |
| float         | Convert data to floating type                       | Data                         | Floating type data. Failure to convert will cause SQL matching to fail     |
| float2str     | Convert a float to string using the given precision | 1. Float Number 2. Precision | String                                                                     |
| map           | Convert data to Map type                            | Data                         | Map type data. Failure to convert will cause SQL matching to fail          |

**Examples:**

```erlang
str(1234) = '1234'
str_utf8(1234) = '1234'
bool('true') = true
int('1234') = 1234
float('3.14') = 3.14
float2str(20.2, 10) = '20.2'
float2str(20.2, 17) = '20.19999999999999928'
```

Note that when converting a floating-point type to a string, the output may need to be rounded.
For details, see: [floating-point-guide](https://floating-point-gui.de/)

## String Functions

| Function name | Purpose                        | parameter                                                                                                                               | returned value     |
| ------------- | ------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------- | ------------------ |
| lower         | convert to lowercase           | input string                                                                                                                            | Lowercase string   |
| upper         | convert to uppercase           | input string                                                                                                                            | uppercase string   |
| trim          | Remove left and right space    | input string                                                                                                                            | output string      |
| ltrim         | Remove the left space          | input string                                                                                                                            | output string      |
| rtrim         | Remove the right space         | input string                                                                                                                            | output string      |
| reverse       | String inversion               | input string                                                                                                                            | output string      |
| strlen        | string length                  | input string                                                                                                                            | Integer value      |
| substr        | Take a substring of characters | 1. input string <br />2. Start position. Note: Subscripts start at 1                                                                    | substring          |
| substring     | Take a substring of characters | 1. input string <br />2. Start position <br />3. End position. Note: Subscripts start at 1                                              | substring          |
| split         | String splitting               | 1. input string <br />2. split string                                                                                                   | Split string array |
| split         | String splitting               | 1. input string <br />2. split string <br />3. Find the first separator on the left or right, optional value is 'leading' or 'trailing' | Split string array |
| split         | split string                   | 1. input string <br />2. split string <br />3. Find the first separator on the left or right, optional value is 'leading' or 'trailing' | Split string array |

**Examples:**

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

## Map Functions

| function name | purpose                                                                           | parameter                                  | returned value                                                      |
| ------------- | --------------------------------------------------------------------------------- | ------------------------------------------ | ------------------------------------------------------------------- |
| map_get       | Take the value of a Key in the Map, or return a null value if failed              | 1. Key <br />2. Map                        | The value of a Key in the Map. Support nested keys, such as "a.b.c" |
| map_get       | Take the value of a Key in the Map, if failed, return the specified default value | 1. Key <br />2. Map <br />3. Default Value | The value of a Key in the Map. Support nested keys, such as "a.b.c" |
| map_put       | Insert value into Map                                                             | 1. Key <br />2. Value <br />3. Map         | The inserted Map. Support nested keys, such as "a.b.c"              |

**Examples:**

```erlang
map_get('a', json_decode( '{ "a" : 1 }' )) = 1
map_get('b', json_decode( '{ "a" : 1 }' ), 2) = 2
map_get('a', map_put('a', 2, json_decode( '{ "a" : 1 }' ))) = 2
```

## Array Functions

| function name | purpose                                                                               | parameter                                                       | returned value         |
| ------------- | ------------------------------------------------------------------------------------- | --------------------------------------------------------------- | ---------------------- |
| nth           | Take the nth element, and subscripts start at 1                                       | Original array                                                  | Nth element            |
| length        | Get the length of an array                                                            | Original array                                                  | the length of an array |
| sublist       | Take a sub-array of length len starting from the first element. Subscripts start at 1 | 1. length len <br />2. Original array                           | sub-array              |
| sublist       | Take a sub-array of length len starting from the nth element. Subscripts start at 1   | 1. start position n <br />2. length len <br />3. Original array | sub-array              |
| first         | Take the first element. Subscripts start at 1                                         | Original array                                                  | 1st element            |
| last          | take the last element                                                                 | Original array                                                  | the last element       |
| contains      | Determine whether the data is in the array                                            | 1. data <br />2. Original array                                 | Boolean value          |


**Examples:**

```erlang
nth(2, [1,2,3,4]) = 2
length([1,2,3,4]) = 4
sublist(3, [1,2,3,4]) = [1,2,3,4]
sublist(1,2,[1,2,3,4]) = [1, 2]
first([1,2,3,4]) = 1
last([1,2,3,4]) = 4
contains(2, [1,2,3,4]) = true
```

## Hash Function

| function name | purpose         | parameter | returned value |
| ------------- | --------------- | --------- | -------------- |
| md5           | evaluate MD5    | data      | MD5 value      |
| sha           | evaluate SHA    | data      | SHA value      |
| sha256        | evaluate SHA256 | data      | SHA256 value   |

**Examples:**

```erlang
md5('some val') = '1b68352b3e9c2de52ffd322e30bffcc4'
sha('some val') = 'f85ba28ff5ea84a0cbfa118319acb0c5e58ee2b9'
sha256('some val') = '67f97635d8a0e064f60ba6e8846a0ac0be664f18f0c1dc6445cd3542d2b71993'
```

## Compression and Decompression Functions

| Function         | Purpose                                              | Parameters             | Returned value         |
| ---------------- | ---------------------------------------------------- | ---------------------- | ---------------------- |
| `gzip`           | Compresses data with gz headers and checksum.        | Raw binary data        | Compressed binary data |
| `gunzip`         | Decompresses data with gz headers and checksum.      | Compressed binary data | Raw binary data        |
| `zip`            | Compresses data without zlib headers and checksum.   | Raw binary data        | Compressed binary data |
| `unzip`          | Decompresses data without zlib headers and checksum. | Compressed binary data | Raw binary data        |
| `zip_compress`   | Compresses data with zlib headers and checksum.      | Raw binary data        | Compressed binary data |
| `zip_uncompress` | Decompresses data with zlib headers and checksum.   | Compressed binary data | Raw binary data        |


**Examples:**

```erlang
bin2hexstr(gzip('hello world')) = '1F8B0800000000000003CB48CDC9C95728CF2FCA49010085114A0D0B000000'
gunzip(hexstr2bin('1F8B0800000000000003CB48CDC9C95728CF2FCA49010085114A0D0B000000')) = 'hello world'

bin2hexstr(zip('hello world')) = 'CB48CDC9C95728CF2FCA490100'
unzip(hexstr2bin('CB48CDC9C95728CF2FCA490100')) = 'hello world'

bin2hexstr(zip_compress('hello world')) = '789CCB48CDC9C95728CF2FCA4901001A0B045D'
zip_uncompress(hexstr2bin('789CCB48CDC9C95728CF2FCA4901001A0B045D')) = 'hello world'
```

## Bit Functions

| Function  | Purpose                                                                                                                                                                   | Parameters                                                                                                                                                                                                                                                                                      | Returned value               |
| --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------- |
| `subbits` | Get a given length of bits from the beginning of a binary, and then convert it to an unsigned integer (big-endian).                                                       | 1. The binary <br />2. The length of bits to get                                                                                                                                                                                                                                                | The unsigned integer         |
| `subbits` | Get a given length of bits start from the specified offset of a binary, and then convert it to an unsigned integer (big-endian). Offsets are start from 1.                | 1. The binary <br />2. The offset <br />3. The length of bits to get                                                                                                                                                                                                                            | The unsigned integer         |
| `subbits` | Get a given length of bits start from the specified offset of a binary, and then convert it to a data type according to the arguments provided. Offsets are start from 1. | 1. The binary <br />2. The offset <br />3. The length of bits to get <br />4. Data Type, can be one of 'integer', 'float', 'bits' <br />5. Signedness, only works for integers, can be one of 'unsigned', 'signed', <br />6. Endianness, only works for integers, can be one of 'big', 'little' | The data got from the binary |



**Examples:**

```erlang
subbits('abc', 8) = 97
subbits('abc', 9, 8) = 98
subbits('abc', 17, 8) = 99
subbits('abc', 9, 16, 'integer', 'signed', 'big') = 25187
subbits('abc', 9, 16, 'integer', 'signed', 'little') = 25442
```

## Decoding and Encoding Functions

| Function        | Purpose              | Parameters                                | Returned value                      |
| --------------- | -------------------- | ----------------------------------------- | ----------------------------------- |
| `base64_encode` | BASE64 encode        | The binary to be encoded                  | The encoded base64-formatted string |
| `base64_decode` | BASE64 decode        | The base64-formatted string to be decoded | The decoded binary                  |
| `json_encode`   | JSON encode          | The data to be encoded                    | The JSON string                     |
| `json_decode`   | JSON decode          | The JSON string to be decoded             | The decoded data                    |
| `bin2hexstr`    | Binary to Hex String | The binary                                | The hex string                      |
| `hexstr2bin`    | Binary to Hex String | The hex string                            | The binary                          |


**Examples:**

```erlang
base64_encode('some val') = 'c29tZSB2YWw='
base64_decode('c29tZSB2YWw=') = 'some val'
json_encode(json_decode( '{ "a" : 1 }' )) = '{"a":1}'
bin2hexstr(hexstr2bin('ABEF123')) = 'ABEF123'
```

{% emqxee %}
| Function | Purpose | Parameters | Returned value |
| -------- | ------------------------------------|------------------------- | --------------------------- |
| `schema_encode` | Encode according to schema. The schema should be created before using this function | 1. The Schema ID defined by schema registry 2. The data to be encoded 3..N. The remaining arguments according to the schema type | The encoded data |
| `schema_decode` | Decode according to schema. The schema should be created before using this function | 1. The Schema ID defined by schema registry 2. The data to be decoded 3..N. The remaining arguments according to the schema type | The decoded data |

<!-- For examples of schema_encode() and schema_decode(), see [schema registry](schema-registry.md) -->
{% endemqxee %}

## Time and Date Functions


The following table contains the time units that are used in the functions described below. The time unit is specified as a string (for example, `now_timestamp('second')`).

| Name          | Precision   | Example             |
| ------------- | ----------- | ------------------- |
| `second`      | second      | 1653557821          |
| `millisecond` | millisecond | 1653557852982       |
| `microsecond` | microsecond | 1653557892926417    |
| `nanosecond`  | nanosecond  | 1653557916474793000 |

| Function             | Purpose                                                                 | Parameters                                                                                                                                                                                                                                                                                              | Returned value                    |
| -------------------- | ----------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------- |
| `now_timestamp`      | Return the unix epoch of now in second                                  | -                                                                                                                                                                                                                                                                                                       | The unix epoch                    |
| `now_timestamp`      | Return the unix epoch of now, in given time unit                        | 1. The time unit                                                                                                                                                                                                                                                                                        | The unix epoch                    |
| `now_rfc3339`        | Create a RFC3339 time string of now in second                           | -                                                                                                                                                                                                                                                                                                       | The time string of format RFC3339 |
| `now_rfc3339`        | Create a RFC3339 time string of now, in given time unit                 | 1. The time unit                                                                                                                                                                                                                                                                                        | The time string of format RFC3339 |
| `unix_ts_to_rfc3339` | Convert an unix epoch (in second) to RFC3339 time string                | 1. The unix epoch in second                                                                                                                                                                                                                                                                             | The time string of format RFC3339 |
| `unix_ts_to_rfc3339` | Convert an unix epoch to RFC3339 time string, using the given time unit | 1. The unix epoch </br>2. The time unit                                                                                                                                                                                                                                                                 | The time string of format RFC3339 |
| `rfc3339_to_unix_ts` | Convert a RFC3339 time string (in second) to unix epoch                 | 1. The time string of format RFC3339                                                                                                                                                                                                                                                                    | The unix epoch                    |
| `rfc3339_to_unix_ts` | Convert a RFC3339 time string to unix epoch, using the given time unit  | 1. The time string of format RFC3339 </br>2. The time unit                                                                                                                                                                                                                                              | The unix epoch                    |
| `format_date`        | Timestamp to formatted time                                             | 1. The time unit (refer to The time unit)</br>2. The time offset (refer to time offset definition)</br>3. The date format (refer to time string codec format)</br>4. The timestamp (optional parameter, default is current time)                                                                        | Formatted time                    |
| `date_to_unix_ts`    | Formatted time to timestamp                                             | 1. The time unit (refer to the following table for definition) </br>2. The time offset (optional, when not filled, use the time offset in the formatted time string, refer to the time offset definition) </br>3. The date format (refer to time string codec format) </br>4. The formatted time string | The unix epoch                    |


**Time String Format Syntax**

| Placeholder | Definition                 | Range                  |
| ----------- | -------------------------- | ---------------------- |
| `%Y`        | year                       | 0000 - 9999            |
| `%m`        | month                      | 01 - 12                |
| `%d`        | day                        | 01 - 31                |
| `%H`        | hour                       | 00 - 12                |
| `%M`        | minute                     | 00 - 59                |
| `%S`        | second                     | 01 - 59                |
| `%N`        | nanosecond                 | 000000000 - 999999999  |
| `%3N`       | millisecond                | 000000 - 999999        |
| `%6N`       | microsecond                | 000 - 000              |
| `%z`        | time offset [+\|-]HHMM     | -1159 to +1159         |
| `%:z`       | time offset [+\|-]HH:MM    | -11:59 to +11:59       |
| `%::z`      | time offset [+\|-]HH:MM:SS | -11:59:59 to +11:59:59 |

**The Time Offset**

| Offset           | Definition                 | Examples                                                                                                     |
| ---------------- | -------------------------- | ------------------------------------------------------------------------------------------------------------ |
| `z`              | UTC Zulu Time              | `+00:00`                                                                                                     |
| `Z`              | UTC Zulu Time. Same as `z` | `+00:00`                                                                                                     |
| `local`          | System Time                | Automatic </br>Beijing `+08:00`</br> Zulu `+00:00` </br>Stockholm, Sweden `+02:00` </br>Los Angeles `-08:00` |
| `[+\|-]HHMM`     | `%z`                       | Beijing `+0800` </br>Zulu `+0000` </br>Stockholm, Sweden `+0200` </br>Los Angeles `-0800`                    |
| `[+\|-]HH:MM`    | `%:z`                      | Beijing `+08:00` </br>Zulu `+00:00` </br>Stockholm, Sweden `+02:00` </br>Los Angeles `-08:00`                |
| `[+\|-]HH:MM:SS` | `%::z`                     | Beijing `+08:00:00` </br>Zulu `+00:00:00` </br>Stockholm, Sweden `+02:00:00` </br>Los Angeles `-08:00:00`    |
| integer()        | Seconds                    | Beijing 28800 </br>Zulu 0 </br>Stockholm, Sweden 7200 </br>Los Angeles -28800                                |

**Examples:**

```SQL
now_timestamp() = 1650874276
now_timestamp('millisecond') = 1650874318331
now_rfc3339() = '2022-04-25T16:08:41+08:00'
now_rfc3339('millisecond') = '2022-04-25T16:10:10.652+08:00'
unix_ts_to_rfc3339(1650874276) = '2022-04-25T16:11:16+08:00'
unix_ts_to_rfc3339(1650874318331, 'millisecond') = '2022-04-25T16:11:58.331+08:00'
rfc3339_to_unix_ts('2022-04-25T16:11:16+08:00') = 1650874276
rfc3339_to_unix_ts('2022-04-25T16:11:58.331+08:00', 'millisecond') = 1650874318331
format_date('second', '+0800', '%Y-%m-%d %H:%M:%S%:z', 1653561612) = '2022-05-26 18:40:12+08:00'
format_date('second', 'local', '%Y-%m-%d %H:%M:%S%:z') = "2022-05-26 18:48:01+08:00"
format_date('second', 0, '%Y-%m-%d %H:%M:%S%:z') = '2022-05-26 10:42:41+00:00'
date_to_unix_ts('second', '%Y-%m-%d %H:%M:%S%:z', '2022-05-26 18:40:12+08:00') = 1653561612
date_to_unix_ts('second', 'local', '%Y-%m-%d %H-%M-%S', '2022-05-26 18:40:12') = 1653561612
date_to_unix_ts('second', '%Y-%m-%d %H-%M-%S', '2022-05-26 10:40:12') = 1653561612
```

**MongoDB Time Functions**

{% emqxee %}
| Function | Purpose | Parameters | Returned value |
| -------- | ------------------------------------|-------------------------- | --------------------------- |
| `mongo_date` | Create a mongodb ISODate type of now | - | the ISODate |
| `mongo_date` | Create a mongodb ISODate type from the given unix epoch in millisecond | 1. unix epoch in millisecond | the ISODate |
| `mongo_date` | Create a mongodb ISODate type from the given unix epoch in given time unit | 1. unix epoch 2. time unit, can be one of 'second', 'millisecond', 'microsecond' or 'nanosecond' | the ISODate |

The time unit can be one of 'second', 'millisecond', 'microsecond' or 'nanosecond'.

```SQL
mongo_date() = 'ISODate("2012-12-19T06:01:17.171Z")'
mongo_date(timestamp) = 'ISODate("2012-12-19T06:01:17.171Z")'
mongo_date(timestamp, 'millisecond') = 'ISODate("2012-12-19T06:01:17.171Z")'
```

{% endemqxee %}

## JQ Functions for Transforming JSON Data

In addition to the above built-in functions, EMQX also integrates JQ functions for processing JSON data, please refer to [JQ Functions](./rule-sql-jq.md).
