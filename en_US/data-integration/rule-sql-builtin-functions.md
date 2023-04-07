# Built-in SQL Functions

The EMQX rule engine's SQL-like language supports a variety of built-in functions for doing basic data transformation, including [mathematical](#mathematical-functions), [data type judgment](#data-type-judgment-function), [conversion](#data-type-conversion-functions), [string](#string-functions), [map](#map-functions), [array](#array-functions), [hash](#hash-function), [compression and decompression](#compression-and-decompression-functions), [bit](#bit-functions), [decoding and encoding](#decoding-and-encoding-functions), and [time and date](#time-and-date-functions) functions that are available in EMQX.

:::tip

Since EMQX 5.0 version, EMQX also supports using  [JQ language](https://stedolan.github.io/jq/manual/) for complex data transformation, you may read the [JQ Fucntion](./rule-sql-jq.md) section for more information.

:::

## Mathematical Functions

EMQX supports a wide range of mathematical functions:

- Trigonometric and hyperbolic functions: include sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
- Numerical functions: include abs, ceil, floor, round, sqrt, fmod
- Exponential and logarithmic functions: including exp, power, log, log10, and log2

See the table below for a complete list of mathematical functions supported. 

| Function Name | Description                           | Parameter                                  |
| ------------- | ------------------------------------- | ------------------------------------------ |
| abs           | Absolute value                        | Operand                                    |
| cos           | Cosine value                          | Operand                                    |
| cosh          | Hyperbolic cosine value               | Operand                                    |
| acos          | Inverse cosine value                  | Operand                                    |
| acosh         | Inverse hyperbolic cosine value       | Operand                                    |
| sin           | Sine value                            | Operand                                    |
| sinh          | Hyperbolic sine value                 | Operand                                    |
| asin          | Arcsine value                         | Operand                                    |
| asinh         | Inverse hyperbolic sine value         | Operand                                    |
| tan           | Tangent value                         | Operand                                    |
| tanh          | Hyperbolic tangent value              | Operand                                    |
| atan          | Arc tangent value                     | Operand                                    |
| atanh         | Inverse hyperbolic tangent value      | Operand                                    |
| ceil          | Round up (integer)                    | Operand                                    |
| floor         | Round down (integer)                  | Operand                                    |
| round         | Rounding (integer)                    | Operand                                    |
| fmod          | modulo<br> (remainder)                | 1. left Operand <br />2.right Operand      |
| exp           | Exponentiation<br>x power of e        | Operand                                    |
| power         | Exponential operation<br>y power of X | 1. Left operand x <br />2. Right operand y |
| sqrt          | Square root                           | Operand                                    |
| log           | Logarithm to e                        | Operand                                    |
| log10         | Logarithm to 10                       | Operand                                    |
| log2          | Logarithm to 2                        | Operand                                    |

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

## Data Type Judgment Function

EMQX has built-in functions for data type judgments. These functions are used to check the data type of a specific field in a message and return a boolean value indicating whether or not the field conforms to the specified data type. 

See the table below for a complete list of data type judgment functions supported. 

| Function Name | Description                                                  | Parameter |
| ------------- | ------------------------------------------------------------ | --------- |
| is_null       | Check if a field is undefined<br>Boolean                     | Data      |
| is_not_null   | Check if a field is defined<br/>Boolean                      | Data      |
| is_str        | Check if the value is of String type<br/>Boolean             | Data      |
| is_bool       | Check if the value is of Boolean type<br/>Boolean            | Data      |
| is_int        | Check if the value is of Integer type<br/>Boolean            | Data      |
| is_float      | Check if the value is of Float type<br/>Boolean              | Data      |
| is_num        | Check if the value is of numeric type<br>Integer or Float<br/>Boolean | Data      |
| is_map        | Check if the value is of Map type<br/>Boolean                | Data      |
| is_array      | Check if the value is of Array type<br/>Boolean              | Data      |

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

EMQX has built-in functions that allow you to convert the data type of a specific field in a message to a new data type.

See the table below for a complete list of data type judgment functions supported. 

| Function Name | Description                                          | Parameter                    |
| ------------- | ---------------------------------------------------- | ---------------------------- |
| str *         | Convert data to String type                          | Data                         |
| str_utf8      | Convert data to UTF-8 String type                    | Data                         |
| bool          | Convert data to Boolean type                         | Data                         |
| int           | Convert data to Integer type                         | Data                         |
| float         | Convert data to Float type                           | Data                         |
| float2str     | Convert a float to a string with the given precision | 1. Float Number 2. Precision |
| map           | Convert data to Map type                             | Data                         |

[^*]: When converting a floating-point type to a string, the output may need to be rounded.

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

:::tip

Data type conversion failures will cause SQL matching to fail, please proceed with caution. 

:::

## String Functions

EMQX provides several built-in functions for manipulating strings in the rule engine, for example, case conversion, space removing, and sting length count. 

See the table below for a complete list of string functions supported. 

| Function Name              | Description                    | Parameter                                                    |
| -------------------------- | ------------------------------ | ------------------------------------------------------------ |
| lower                      | Convert to lowercase           | Input string                                                 |
| upper                      | Convert to uppercase           | Input string                                                 |
| trim                       | Remove left and right space    | Input string                                                 |
| ltrim                      | Remove left space              | Input string                                                 |
| rtrim                      | Remove right space             | Input string                                                 |
| reverse                    | String inversion               | Input string                                                 |
| strlen                     | String length                  | Input string                                                 |
| substr                     | Take a substring of characters | 1. Input string <br />2. Start position (starting at position 1). |
| substr<br>(with end)       | Take a substring of characters | 1. Input string <br />2. Start position (starting at position 1). <br />3. End position. <br> |
| split                      | String split                   | 1. Input string <br />2. Separator                           |
| split <br>(with direction) | String split                   | 1. Input string <br />2. Separator <br />3. Direction, optional value: `leading` or `trailing` |

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

EMQX has built-in functions that allow you to manipulate maps, and perform operations such as adding key-value pairs to a map and retrieving values. <!--is this only applicable to erlang maps? shall we add a note here?-->

See the table below for a complete list of map functions supported. 

| Function Name              | Description                                                  | Parameter                                  |
| -------------------------- | ------------------------------------------------------------ | ------------------------------------------ |
| map_get                    | Retrieve the value associated with a specified key in the Map <br>Or return null if the key is not found | 1. Key <br />2. Map                        |
| map_get<br> (with default) | Retrieve the value associated with a specified key in the Map, <br>Or return the specified default value if the key is not found | 1. Key <br />2. Map <br />3. Default Value |
| map_put                    | Insert a key-value pair into the Map                         | 1. Key <br />2. Value <br />3. Map         |

**Examples:**

```erlang
map_get('a', json_decode( '{ "a" : 1 }' )) = 1
map_get('b', json_decode( '{ "a" : 1 }' ), 2) = 2
map_get('a', map_put('a', 2, json_decode( '{ "a" : 1 }' ))) = 2
```

## Array Functions

EMQX provides several built-in functions for working with arrays in the rule engine. These functions allow you to perform operations such as filtering, mapping, and reducing on arrays within incoming messages. 

See the table below for a complete list of array functions supported. 

| Function Name | Purpose                                                      | Parameters                                           |
| ------------- | ------------------------------------------------------------ | ---------------------------------------------------- |
| `nth`         | Returns the nth element of an array. <br>Subscripts start at 1. | 1. Array <br />2. n (integer)                        |
| `length`      | Returns the length of an array.                              | Array                                                |
| `sublist`     | Returns a sub-array of length len starting from the first element. <br>Subscripts start at 1. <br><!--what does Subscripts start at 1 mean?--> | 1. Array <br />2. len (integer)                      |
| `sublist`     | Returns a sub-array of length len starting from the nth element. <br>Subscripts start at 1. | 1. Array <br />2. n (integer) <br />3. len (integer) |
| `first`       | Returns the first element of an array. <br>Subscripts start at 1. | Array                                                |
| `last`        | Returns the last element of an array.                        | Array                                                |
| `contains`    | Returns a boolean indicating if the data is in the array.    | 1. data <br />2. Array                               |

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

EMQX supports using D5, SHA, and SHA256 to ensure data integrity and security.

See the table below for a complete list of Hush functions supported. 

| Function Name | Description                     | Parameter |
| ------------- | ------------------------------- | --------- |
| md5           | Calculate the MD5 hash value    | Data      |
| sha           | Calculate the SHA hash value    | Data      |
| sha256        | Calculate the SHA256 hash value | Data      |

**Examples:**

```erlang
md5('some val') = '1b68352b3e9c2de52ffd322e30bffcc4'
sha('some val') = 'f85ba28ff5ea84a0cbfa118319acb0c5e58ee2b9'
sha256('some val') = '67f97635d8a0e064f60ba6e8846a0ac0be664f18f0c1dc6445cd3542d2b71993'
```

## Compression and Decompression Functions

EMQX uses compression and decompression functions to reduce network bandwidth usage and improve system performance, where, the compression functions are used to reduce the amount of data that needs to be transmitted over the network,  the decompression functions are used to decompress the compressed payload data of MQTT messages. 

See the table below for a complete list of compression and decompression functions supported. 

| Function         | Purpose                                              | Parameters                                                  |
| ---------------- | ---------------------------------------------------- | ----------------------------------------------------------- |
| `gzip`           | Compresses with gzip headers and checksum.           | `raw_data` <br>(binary)                                     |
| `gunzip`         | Decompresses with gzip headers and checksum.         | `compressed_data` <br>(binary)                              |
| `zip`            | Compresses without zlib headers and checksum.        | `raw_data` <br>(binary), `compression_level` <br>(optional) |
| `unzip`          | Decompresses data without zlib headers and checksum. | `compressed_data` <br>(binary)                              |
| `zip_compress`   | Compresses with zlib headers and checksum.           | `raw_data`<br> (binary) `compression_level` <br>(optional)  |
| `zip_uncompress` | Decompresses with zlib headers and checksum.         | `compressed_data` <br>(binary)                              |


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

EMQX uses the `subbits` function to extract a sequence of bits from a binary or bitstring and convert it to a specified data type. 

See the table below for the syntax supported. 

| Function                                            | Description                                                  | Parameters                                                   |
| --------------------------------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| `subbits`                                           | Returns an unsigned integer (big-endian) obtained by extracting a specified number of bits from the beginning of a binary input. | 1. Binary input <br />2. Number of bits to extract           |
| `subbits`<br>(with offset)                          | Returns an unsigned integer (big-endian) obtained by extracting a specified number of bits starting from a given offset in a binary input. <br>Offsets are indexed starting from 1. | 1. Binary input  <br />2. Starting offset <br />3. Number of bits to extract |
| `subbits`<br>(with offset and data type conversion) | Returns a data value obtained by extracting a specified number of bits starting from a given offset in a binary input and after data type conversion. <br>Offsets are indexed starting from 1. | 1. Binary input <br />2. Starting offset<br />3. Number of bits to extract <br />4. Data Type, can be `integer`, `float`, `bits`<br /><br>If set to `integer`, you can continue to set:<br>- Signedness: `unsigned`, `signed`, <br />6. Endianness: `big`, `little` |

**Examples:**

```erlang
subbits('abc', 8) = 97
subbits('abc', 9, 8) = 98
subbits('abc', 17, 8) = 99
subbits('abc', 9, 16, 'integer', 'signed', 'big') = 25187
subbits('abc', 9, 16, 'integer', 'signed', 'little') = 25442
```

## Decoding and Encoding Functions

EMQX uses encoding and decoding functions to convert data from one format to another.

See the table below for a complete list of encoding and decoding functions supported. 

| Function        | Description          | Parameters                                |
| --------------- | -------------------- | ----------------------------------------- |
| `base64_encode` | BASE64 encode        | The binary to be encoded                  |
| `base64_decode` | BASE64 decode        | The base64-formatted string to be decoded |
| `json_encode`   | JSON encode          | The data to be encoded                    |
| `json_decode`   | JSON decode          | The JSON string to be decoded             |
| `bin2hexstr`    | Binary to Hex String | The binary                                |
| `hexstr2bin`    | Binary to Hex String | The hex string                            |

**Examples:**

```erlang
base64_encode('some val') = 'c29tZSB2YWw='
base64_decode('c29tZSB2YWw=') = 'some val'
json_encode(json_decode( '{ "a" : 1 }' )) = '{"a":1}'
bin2hexstr(hexstr2bin('ABEF123')) = 'ABEF123'
```

{% emqxee %}

EMQX Enterprise also supports using schema encoding and decoding functions to encode and decode data according to a specified schema. See the table below for a detailed explanation of the functions. 

| Function | Description | Parameters | Returned value |
| -------- | ------------------------------------|------------------------- | --------------------------- |
| `schema_encode` | Encode data according to a pre-defined schema. | 1. Schema ID defined by schema registry <br>2. Data to be encoded <br>3 ... N. Remaining arguments according to the schema type | The encoded data |
| `schema_decode` | Decode data according to a pre-defined schema. | 1. Schema ID defined by schema registry<br> 2. Data to be decoded <br>3..N. Remaining arguments according to the schema type | The decoded data |

<!-- For examples of schema_encode() and schema_decode(), see [schema registry](schema-registry.md) -->
{% endemqxee %}

## Time and Date Functions

EMQX uses the following functions for handling time and date, and the time unit supported by these functions are `second`, `millisecond`, `microsecond`, and `nanosecond`.

| Function             | Purpose                                                      | Parameters                                                   |
| -------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| `now_timestamp`      | Return the current unix epoch timestamp<br>Unit: `second`    | -                                                            |
| `now_timestamp`      | Return the current unix epoch timestamp with a self-defined unit | Time unit                                                    |
| `now_rfc3339`        | Create the current RFC3339 time string<br/>Unit: `second`    | -                                                            |
| `now_rfc3339`        | Create the current RFC3339 time string with a self-defined unit | Time unit                                                    |
| `unix_ts_to_rfc3339` | Convert an unix epoch (in second) to RFC3339 time string     | Unix epoch in second                                         |
| `unix_ts_to_rfc3339` | Convert an unix epoch to RFC3339 time string                 | 1. Unix epoch <br>2. Time unit                               |
| `rfc3339_to_unix_ts` | Convert an RFC3339 time string (in second) to unix epoch     | 1. Time string of format RFC3339                             |
| `rfc3339_to_unix_ts` | Convert an RFC3339 time string to unix epoch with a self-defined unit | 1. Time string of format RFC3339 <br>2. Time unit            |
| `format_date`        | Timestamp to formatted time                                  | 1. Time unit (refer to The time unit)<br>2. Time offset (refer to time offset definition)<br>3. Date format (refer to time string codec format) <!--I do not understand what are these refer to?--><br>4. Timestamp (optional parameter, default is current time) |
| `date_to_unix_ts`    | Formatted time to timestamp                                  | 1. Time unit (refer to the following table for definition) <br>2. Time offset (optional, when not filled, use the time offset in the formatted time string, refer to the time offset definition) <br>3. Date format (refer to time string codec format) <br>4. Formatted time string |

**Syntax of Time String Format**

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

**Time Offset**

| Offset           | Definition                 | Examples                                                     |
| ---------------- | -------------------------- | ------------------------------------------------------------ |
| `z`              | UTC Zulu Time              | `+00:00`                                                     |
| `Z`              | UTC Zulu Time. Same as `z` | `+00:00`                                                     |
| `local`          | System Time                | Automatic <br>Beijing `+08:00`<br>Zulu `+00:00` <br>Stockholm, Sweden `+02:00` <br>Los Angeles `-08:00` |
| `[+\|-]HHMM`     | `%z`                       | Beijing `+0800` <br>Zulu `+0000` <br>Stockholm, Sweden `+0200` <br>Los Angeles `-0800` |
| `[+\|-]HH:MM`    | `%:z`                      | Beijing `+08:00` <br>Zulu `+00:00` <br>Stockholm, Sweden `+02:00` <br>Los Angeles `-08:00` |
| `[+\|-]HH:MM:SS` | `%::z`                     | Beijing `+08:00:00` <br>Zulu `+00:00:00` <br>Stockholm, Sweden `+02:00:00` <br>Los Angeles `-08:00:00` |
| integer()        | Seconds                    | Beijing 28800 <br>Zulu 0 <br>Stockholm, Sweden 7200 <br>Los Angeles -28800 |

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
| Function | Purpose | Parameters |
| -------- | ------------------------------------|-------------------------- |
| `mongo_date` | Create a mongodb ISODate type of now | - |
| `mongo_date` | Create a mongodb ISODate type from the given unix epoch in millisecond | 1. Unix epoch in millisecond |
| `mongo_date` | Create a mongodb ISODate type from the given unix epoch in given time unit | 1. Unix epoch <br>2. Time unit, can be one of 'second', 'millisecond', 'microsecond' or 'nanosecond' |

The time unit can be one of 'second', 'millisecond', 'microsecond' or 'nanosecond'.

```SQL
mongo_date() = 'ISODate("2012-12-19T06:01:17.171Z")'
mongo_date(timestamp) = 'ISODate("2012-12-19T06:01:17.171Z")'
mongo_date(timestamp, 'millisecond') = 'ISODate("2012-12-19T06:01:17.171Z")'
```

{% endemqxee %}
