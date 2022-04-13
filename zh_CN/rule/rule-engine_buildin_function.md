---
# 编写日期
date: 2020-02-20 17:46:13
# 作者 Github 名称
author: wivwiv, terry-xiaoyu
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---

# SQL 语句中可用的函数
## 数学函数

|函数名|函数作用|参数|返回值|
|--- |--- |--- |--- |
|abs|绝对值|1. 被操作数|绝对值|
|cos|余弦|1. 被操作数|余弦值|
|cosh|双曲余弦|1. 被操作数|双曲余弦值|
|acos|反余弦|1. 被操作数|反余弦值|
|acosh|反双曲余弦|1. 被操作数|反双曲余弦值|
|sin|正弦|1. 被操作数|正弦值|
|sinh|双曲正弦|1. 被操作数|双曲正弦值|
|asin|反正弦|1. 被操作数|值|
|asinh|反双曲正弦|1. 被操作数|反双曲正弦值|
|tan|正切|1. 被操作数|正切值|
|tanh|双曲正切|1. 被操作数|双曲正切值|
|atan|反正切|1. 被操作数|反正切值|
|atanh|反双曲正切|1. 被操作数|反双曲正切值|
|ceil|上取整|1. 被操作数|整数值|
|floor|下取整|1. 被操作数|整数值|
|round|四舍五入|1. 被操作数|整数值|
|exp|幂运算|1. 被操作数|e 的 x 次幂|
|power|指数运算|1. 左操作数 x <br />2. 右操作数 y|x 的 y 次方|
|sqrt|平方根运算|1. 被操作数|平方根|
|fmod|负点数取模函数|1. 左操作数 <br />2. 右操作数|模|
|log|以 e 为底对数|1. 被操作数|值|
|log10|以 10 为底对数|1. 被操作数|值|
|log2|以 2 为底对数|1. 被操作数|值|

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

## 数据类型判断函数

|函数名|函数作用|参数|返回值|
|--- |--- |--- |--- |
|is_null|判断变量是否为空值|Data|Boolean 类型的数据。如果为空值(undefined) 则返回 true，否则返回 false|
|is_not_null|判断变量是否为非空值|Data|Boolean 类型的数据。如果为空值(undefined) 则返回 false，否则返回 true|
|is_str|判断变量是否为 String 类型|Data|Boolean 类型的数据。|
|is_bool|判断变量是否为 Boolean 类型|Data|Boolean 类型的数据。|
|is_int|判断变量是否为 Integer 类型|Data|Boolean 类型的数据。|
|is_float|判断变量是否为 Float 类型|Data|Boolean 类型的数据。|
|is_num|判断变量是否为数字类型，包括 Integer 和 Float 类型|Data|Boolean 类型的数据。|
|is_map|判断变量是否为 Map 类型|Data|Boolean 类型的数据。|
|is_array|判断变量是否为 Array 类型|Data|Boolean 类型的数据。|

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

## 数据类型转换函数


|函数名|函数作用|参数|返回值|
|--- |--- |--- |--- |
|str|将数据转换为 String 类型|Data|String 类型的数据。无法转换将会导致 SQL 匹配失败|
|str_utf8|将数据转换为 UTF-8 String 类型|Data|UTF-8 String 类型的数据。无法转换将会导致 SQL 匹配失败|
|bool|将数据转换为 Boolean 类型|Data|Boolean 类型的数据。无法转换将会导致 SQL 匹配失败|
|int|将数据转换为整数类型|Data|整数类型的数据。无法转换将会导致 SQL 匹配失败|
|float|将数据转换为浮点型类型|Data|浮点型类型的数据。无法转换将会导致 SQL 匹配失败|
|map|将数据转换为 Map 类型|Data|Map 类型的数据。无法转换将会导致 SQL 匹配失败|

```erlang
str(1234) = '1234'
str_utf8(1234) = '1234'
bool('true') = true
int('1234') = 1234
float('3.14') = 3.14
```


## 字符串函数

| 函数名   |   函数作用    |    参数     |       返回值       |
| ------- | ------------ | ----------- | ------------------ |
| lower | 转为小写 | 1. 原字符串 | 小写字符串         |
| upper   | 转为大写      | 1. 原字符串     | 大写字符串         |
| trim    | 去掉左右空格  | 1. 原字符串     | 去掉空格后的字符串 |
| ltrim   | 去掉左空格    | 1. 原字符串     | 去掉空格后的字符串 |
| rtrim   | 去掉右空格    | 1. 原字符串     | 去掉空格后的字符串 |
| reverse | 字符串反转    | 1. 原字符串     | 翻转后的字符串 |
| strlen  | 取字符串长度    | 1. 原字符串     | 整数值，字符长度 |
| substr  | 取字符的子串  | 1. 原字符串 <br />2. 起始位置. 注意: 下标从 0 开始 | 子串 |
| substr  | 取字符的子串  | 1. 原字符串 <br />2. 起始位置 <br />3. 要取出的子串长度. 注意: 下标从 0 开始 | 子串 |
| split   | 字符串分割    | 1. 原字符串 <br />2. 分割符子串 | 分割后的字符串数组 |
| split   | 字符串分割, 只查找左边第一个分隔符 | 1. 原字符串 <br />2. 分割符子串 <br />3. 'leading' | 分割后的字符串数组 |
| split   | 字符串分割, 只查找右边第一个分隔符 | 1. 原字符串 <br />2. 分割符子串 <br />3. 'trailing' | 分割后的字符串数组 |
| concat   | 字符串拼接  | 1. 左字符串 <br />2. 右符子串 | 拼接后的字符串 |
| tokens   | 字符串分解(按照指定字符串符分解)  | 1. 输入字符串 <br />2. 分割符或字符串 | 分解后的字符串数组 |
| tokens   | 字符串分解(按照指定字符串和换行符分解)  | 1. 输入字符串 <br />2. 分割符或字符串 <br />3. 'nocrlf' | 分解后的字符串数组 |
| sprintf   | 字符串格式化, 格式字符串的用法详见 https://erlang.org/doc/man/io.html#fwrite-1 里的 Format 部分 | 1. 格式字符串 <br />2,3,4... 参数列表。参数个数不定 | 分解后的字符串数组 |
| pad   | 字符串补足长度，补空格，从尾部补足  | 1. 原字符串 <br />2. 字符总长度 | 补足后的字符串 |
| pad   | 字符串补足长度，补空格，从尾部补足  | 1. 原字符串 <br />2. 字符总长度 <br />3. 'trailing' | 补足后的字符串 |
| pad   | 字符串补足长度，补空格，从两边补足  | 1. 原字符串 <br />2. 字符总长度 <br />3. 'both' | 补足后的字符串 |
| pad   | 字符串补足长度，补空格，从头部补足  | 1. 原字符串 <br />2. 字符总长度 <br />3. 'leading' | 补足后的字符串 |
| pad   | 字符串补足长度，补指定字符，从尾部补足  | 1. 原字符串 <br />2. 字符总长度 <br />3. 'trailing' <br />4. 指定用于补足的字符 | 补足后的字符串 |
| pad   | 字符串补足长度，补指定字符，从两边补足  | 1. 原字符串 <br />2. 字符总长度 <br />3. 'both' <br />4. 指定用于补足的字符 | 补足后的字符串 |
| pad   | 字符串补足长度，补指定字符，从头部补足  | 1. 原字符串 <br />2. 字符总长度 <br />3. 'leading' <br />4. 指定用于补足的字符 | 补足后的字符串 |
| replace | 替换字符串中的某子串，查找所有匹配子串替换  | 1. 原字符串 <br />2. 要被替换的子串 <br />3. 指定用于替换的字符串 | 替换后的字符串 |
| replace | 替换字符串中的某子串，查找所有匹配子串替换  | 1. 原字符串 <br />2. 要被替换的子串 <br />3. 指定用于替换的字符串 <br />4. 'all' | 替换后的字符串 |
| replace | 替换字符串中的某子串，从尾部查找第一个匹配子串替换  | 1. 原字符串 <br />2. 要被替换的子串 <br />3. 指定用于替换的字符串 <br />4. 'trailing' | 替换后的字符串 |
| replace | 替换字符串中的某子串，从头部查找第一个匹配子串替换  | 1. 原字符串 <br />2. 要被替换的子串 <br />3. 指定用于替换的字符串 <br />4. 'leading' | 替换后的字符串 |
| regex_match | 判断字符串是否与某正则表达式匹配  | 1. 原字符串 <br />2. 正则表达式 | true 或 false |
| regex_replace | 替换字符串中匹配到某正则表达式的子串  | 1. 原字符串 <br />2. 正则表达式 <br />3. 指定用于替换的字符串 | 替换后的字符串 |
| ascii | 返回字符对应的 ASCII 码  | 1. 字符 | 整数值，字符对应的 ASCII 码 |
| find | 查找并返回字符串中的某个子串，从头部查找  | 1. 原字符串 <br />2. 要查找的子串 | 查抄到的子串，如找不到则返回空字符串 |
| find | 查找并返回字符串中的某个子串，从头部查找  | 1. 原字符串 <br />2. 要查找的子串 <br />3. 'leading' | 查抄到的子串，如找不到则返回空字符串 |
| find | 查找并返回字符串中的某个子串，从尾部查找  | 1. 原字符串 <br />2. 要查找的子串 <br />3. 'trailing' | 查抄到的子串，如找不到则返回空字符串 |

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

concat('a', '/bc') = 'a/bc'
'a' + '/bc' = 'a/bc'

tokens(' a/b/ c', '/') = [' a', 'b', ' c']
tokens(' a/b/ c', '/ ') = ['a', 'b', 'c']
tokens(' a/b/ c\n', '/ ') = ['a', 'b', 'c\n']
tokens(' a/b/ c\n', '/ ', 'nocrlf') = ['a', 'b', 'c']
tokens(' a/b/ c\r\n', '/ ', 'nocrlf') = ['a', 'b', 'c']

sprintf('hello, ~s!', 'steve') = 'hello, steve!'
sprintf('count: ~p~n', 100) = 'count: 100\n'

pad('abc', 5) = 'abc  '
pad('abc', 5, 'trailing') = 'abc  '
pad('abc', 5, 'both') = ' abc '
pad('abc', 5, 'leading') = '  abc'
pad('abc', 5, 'trailing', '*') = 'abc**'
pad('abc', 5, 'trailing', '*#') = 'abc*#*#'
pad('abc', 5, 'both', '*') = '*abc*'
pad('abc', 5, 'both', '*#') = '*#abc*#'
pad('abc', 5, 'leading', '*') = '**abc'
pad('abc', 5, 'leading', '*#') = '*#*#abc'

replace('ababef', 'ab', 'cd') = 'cdcdef'
replace('ababef', 'ab', 'cd', 'all') = 'cdcdef'
replace('ababef', 'ab', 'cd', 'trailing') = 'abcdef'
replace('ababef', 'ab', 'cd', 'leading') = 'cdabef'

regex_match('abc123', '[a-zA-Z1-9]*') = true

regex_replace('ab1cd3ef', '[1-9]', '[&]') = 'ab[1]cd[3]ef'
regex_replace('ccefacef', 'c+', ':') = ':efa:ef'

ascii('a') = 97

find('eeabcabcee', 'abc') = 'abcabcee'
find('eeabcabcee', 'abc', 'leading') = 'abcabcee'
find('eeabcabcee', 'abc', 'trailing') = 'abcee'
```


## Map 函数

|函数名|函数作用|参数|返回值|
|--- |--- |--- |--- |
|map_get|取 Map 中某个 Key 的值，如果没有则返回空值|1. Key <br />2. Map|Map 中某个 Key 的值。支持嵌套的 Key，比如 "a.b.c"|
|map_get|取 Map 中某个 Key 的值，如果没有则返回指定默认值|1. Key <br />2. Map <br />3. Default Value|Map 中某个 Key 的值。支持嵌套的 Key，比如 "a.b.c"|
|map_put|向 Map 中插入值|1. Key <br />2. Value <br />3. Map|插入后的 Map。支持嵌套的 Key，比如 "a.b.c"|

```erlang
map_get('a', json_decode( '{ "a" : 1 }' )) = 1
map_get('b', json_decode( '{ "a" : 1 }' ), 2) = 2
map_get('a', map_put('a', 2, json_decode( '{ "a" : 1 }' ))) = 2
```


## 数组函数

|函数名|函数作用|参数|返回值|
|--- |--- |--- |--- |
|nth|取第 n 个元素，下标从 1 开始|1. 起始位置 <br /> 2. 原数组|第 n 个元素|
|length|获取数组的长度|1. 原数组|数组长度|
|sublist|取从第一个元素开始、长度为 len 的子数组。下标从 1 开始|1. 长度 len <br />2. 原数组|子数组|
|sublist|取从第 n 个元素开始、长度为 len 的子数组。下标从 1 开始|1. 起始位置 n <br />2. 长度 len <br />3. 原数组|子数组|
|first|取第 1 个元素。下标从 1 开始|1. 原数组|第 1 个元素|
|last|取最后一个元素。|1. 原数组|最后一个元素|
|contains|判断数据是否在数组里面|1. 数据 <br />2. 原数组|Boolean 值|

```erlang
nth(2, [1,2,3,4]) = 2
length([1,2,3,4]) = 4
sublist(3, [1,2,3,4]) = [1,2,3,4]
sublist(1,2,[1,2,3,4]) = [1, 2]
first([1,2,3,4]) = 1
last([1,2,3,4]) = 4
contains(2, [1,2,3,4]) = true
```

## 哈希函数

| 函数名 | 函数功能| 参数| 返回值 |
|-------| ------ | -- | ----- |
| md5	| 求 MD5 值 | 数据 | MD5 值 |
| sha	| 求 SHA 值 | 数据 | SHA 值 |
| sha256 | 求 SHA256 值 | 数据 | SHA256 值 |

```erlang
md5('some val') = '1b68352b3e9c2de52ffd322e30bffcc4'
sha('some val') = 'f85ba28ff5ea84a0cbfa118319acb0c5e58ee2b9'
sha256('some val') = '67f97635d8a0e064f60ba6e8846a0ac0be664f18f0c1dc6445cd3542d2b71993'
```
## 压缩解压缩函数

| 函数名 | 函数功能 |        参数         | 返回值 |
| -------- | -------------- |--------------- | --------------------|
| `gzip` | 压缩数据，结果包含 gz 数据头和校验和  | 原始的二进制数据 | 压缩后的二进制数据 |
| `gunzip` | 解压缩数据，原始数据中包含 gz 数据头和校验和 | 压缩后的二进制数据 | 原始的二进制数据 |
| `zip` | 压缩数据，结果不包含 zlib 数据头和校验和 | 原始的二进制数据 | 压缩后的二进制数据 |
| `unzip` | 解压缩数据，原始数据中不包含 zlib 数据头和校验和 | 压缩后的二进制数据 | 原始的二进制数据 |
| `zip_compress` |  压缩数据，结果包含 zlib 数据头和校验和 | 原始的二进制数据 | 压缩后的二进制数据 |
| `zip_uncompress` | 解压缩数据，原始数据中包含 zlib 数据头和校验和  | 压缩后的二进制数据 | 原始的二进制数据 |

```erlang
bin2hexstr(gzip('hello world')) = '1F8B0800000000000003CB48CDC9C95728CF2FCA49010085114A0D0B000000'
gunzip(hexstr2bin('1F8B0800000000000003CB48CDC9C95728CF2FCA49010085114A0D0B000000')) = 'hello world'

bin2hexstr(zip('hello world')) = 'CB48CDC9C95728CF2FCA490100'
unzip(hexstr2bin('CB48CDC9C95728CF2FCA490100')) = 'hello world'

bin2hexstr(zip_compress('hello world')) = '789CCB48CDC9C95728CF2FCA4901001A0B045D'
zip_uncompress(hexstr2bin('789CCB48CDC9C95728CF2FCA4901001A0B045D')) = 'hello world'
```

## 比特操作函数

| 函数名 | 函数功能| 参数| 返回值 |
| ----- | ------ | -- | ----- |
|subbits| 从二进制数据的起始位置获取指定长度的比特位, 然后转换为无符号整型 (大端). | 1. 二进制数据 <br />2. 要获取的长度(bits) | 无|subbits符号整数 |
|subbits| 从二进制数据的指定下标位置获取指定长度的比特位, 然后转换为无符号整型 (大端). 下标是从 1 开始的 | 1. 二进制数据 <br />2. 起|subbits始位置的下标 <br />3. 要获取的长度(bits) | 无符号整数 |
|subbits| 从二进制数据的指定下标位置获取指定长度的比特位, 然后按照给定的参数转换为想要的数据类型. 下标是从 1 开始的. | 1. 二进制数据 <br />2. 起始位置的下标 <br />3. 要获取的长度(bits) <br />4. 数据类型，可选值：'integer', 'float', 'bits' <br />5. 符号类型, 只对整型数据有效, 可选值：'unsigned', 'signed', <br />6. 大端还是小端, 只对整型数据有效, 可选值：'big', 'little' | 获取到的数据 |

```erlang
subbits('abc', 8) = 97
subbits('abc', 9, 8) = 98
subbits('abc', 17, 8) = 99
subbits('abc', 9, 16, 'integer', 'signed', 'big') = 25187
subbits('abc', 9, 16, 'integer', 'signed', 'little') = 25442
```


## 编解码函数

| 函数名 | 函数功能 |        参数         | 返回值 |
| -------- | -------------- |--------------- | --------------------------- |
| `base64_encode` | BASE64 编码 | 要编码的二进制数据 | Base64 编码的字符串 |
| `base64_decode` | BASE64 解码 | Base64 编码的字符串 | 解码后的二进制数据 |
| `json_encode` | JSON 编码 | 要转成 JSON 的数据结构 | JSON 字符串 |
| `json_decode` | JSON 解码 | 要解码的 JSON 字符串 | 解码后的数据结构 |
| `bin2hexstr` | 二进制数据转为 Hex 字符串 | 二进制数据 | Hex 字符串 |
| `hexstr2bin` | Hex 字符串转为二进制数据 | Hex 字符串 | 二进制数据 |

```erlang
base64_encode('some val') = 'c29tZSB2YWw='
base64_decode('c29tZSB2YWw=') = 'some val'
json_encode(json_decode( '{ "a" : 1 }' )) = '{"a":1}'
bin2hexstr(hexstr2bin('ABEF123')) = 'ABEF123'
```
