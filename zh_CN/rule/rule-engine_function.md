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

<table style="width:99%;">
<colgroup>
<col style="width: 12%" />
<col style="width: 23%" />
<col style="width: 41%" />
<col style="width: 20%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>abs</td>
<td>绝对值</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>绝对值</td>
</tr>
<tr class="odd">
<td>cos</td>
<td>余弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>余弦值</td>
</tr>
<tr class="even">
<td>cosh</td>
<td>双曲余弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>双曲余弦值</td>
</tr>
<tr class="odd">
<td>acos</td>
<td>反余弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反余弦值</td>
</tr>
<tr class="even">
<td>acosh</td>
<td>反双曲余弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反双曲余弦值</td>
</tr>
<tr class="odd">
<td>sin</td>
<td>正弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>正弦值</td>
</tr>
<tr class="even">
<td>sinh</td>
<td>双曲正弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>双曲正弦值</td>
</tr>
<tr class="odd">
<td>asin</td>
<td>反正弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>值</td>
</tr>
<tr class="even">
<td>asinh</td>
<td>反双曲正弦</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反双曲正弦值</td>
</tr>
<tr class="odd">
<td>tan</td>
<td>正切</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>正切值</td>
</tr>
<tr class="even">
<td>tanh</td>
<td>双曲正切</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>双曲正切值</td>
</tr>
<tr class="odd">
<td>atan</td>
<td>反正切</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反正切值</td>
</tr>
<tr class="even">
<td>atanh</td>
<td>反双曲正切</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>反双曲正切值</td>
</tr>
<tr class="odd">
<td>ceil</td>
<td>上取整</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>整数值</td>
</tr>
<tr class="even">
<td>floor</td>
<td>下取整</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>整数值</td>
</tr>
<tr class="odd">
<td>round</td>
<td>四舍五入</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>整数值</td>
</tr>
<tr class="even">
<td>exp</td>
<td>幂运算</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>e 的 x 次幂</td>
</tr>
<tr class="odd">
<td>power</td>
<td>指数运算</td>
<td><ol type="1">
<li>左操作数 x 2. 右操作数 y</li>
</ol></td>
<td>x 的 y 次方</td>
</tr>
<tr class="even">
<td>sqrt</td>
<td>平方根运算</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>平方根</td>
</tr>
<tr class="odd">
<td>fmod</td>
<td>负点数取模函数</td>
<td><ol type="1">
<li>左操作数 2. 右操作数</li>
</ol></td>
<td>模</td>
</tr>
<tr class="even">
<td>log</td>
<td>以 e 为底对数</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>值</td>
</tr>
<tr class="odd">
<td>log10</td>
<td>以 10 为底对数</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>值</td>
</tr>
<tr class="even">
<td>log2</td>
<td>以 2 为底对数</td>
<td><ol type="1">
<li>被操作数</li>
</ol></td>
<td>值</td>
</tr>
</tbody>
</table>

## 数据类型判断函数

<table>
<colgroup>
<col style="width: 9%" />
<col style="width: 35%" />
<col style="width: 6%" />
<col style="width: 48%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>is_null</td>
<td>判断变量是否为空值</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。如果为空值(undefined) 则返回 true，否则返回 false</td>
</tr>
<tr class="odd">
<td>is_not_null</td>
<td>判断变量是否为非空值</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。如果为空值(undefined) 则返回 false，否则返回 true</td>
</tr>
<tr class="even">
<td>is_str</td>
<td>判断变量是否为 String 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="odd">
<td>is_bool</td>
<td>判断变量是否为 Boolean 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="even">
<td>is_int</td>
<td>判断变量是否为 Integer 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="odd">
<td>is_float</td>
<td>判断变量是否为 Float 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="even">
<td>is_num</td>
<td>判断变量是否为数字类型，包括 Integer 和 Float 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="odd">
<td>is_map</td>
<td>判断变量是否为 Map 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
<tr class="even">
<td>is_array</td>
<td>判断变量是否为 Array 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。</td>
</tr>
</tbody>
</table>

## 数据类型转换函数

<table>
<colgroup>
<col style="width: 9%" />
<col style="width: 29%" />
<col style="width: 9%" />
<col style="width: 51%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>str</td>
<td>将数据转换为 String 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>String 类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="odd">
<td>str_utf8</td>
<td>将数据转换为 UTF-8 String 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>UTF-8 String 类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="even">
<td>bool</td>
<td>将数据转换为 Boolean 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Boolean 类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="odd">
<td>int</td>
<td>将数据转换为整数类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>整数类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="even">
<td>float</td>
<td>将数据转换为浮点型类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>浮点型类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
<tr class="odd">
<td>map</td>
<td>将数据转换为 Map 类型</td>
<td><ol type="1">
<li>Data</li>
</ol></td>
<td>Map 类型的数据。无法转换将会导致 SQL 匹配失败</td>
</tr>
</tbody>
</table>

## 字符串函数

| 函数名   |   函数作用    |    参数     |       返回值       |       举例       |
| ------- | ------------ | ----------- | ------------------ | ----------------- |
| <img width=120/>lower | <img width=300/>转为小写 | <img width=200/>1. 原字符串 | 小写字符串         |<img width=400/>`lower('AbC') = 'abc'`<br/><br/>`lower('abc') = 'abc' `|
| upper   | 转为大写      | 1. 原字符串     | 大写字符串         |`upper('AbC') = 'ABC'`<br/><br/>`lower('ABC') = 'ABC' `|
| trim    | 去掉左右空格  | 1. 原字符串     | 去掉空格后的字符串 |`trim(' hello  ') = 'hello' `|
| ltrim   | 去掉左空格    | 1. 原字符串     | 去掉空格后的字符串 |`ltrim(' hello  ') = 'hello  '`|
| rtrim   | 去掉右空格    | 1. 原字符串     | 去掉空格后的字符串 |`rtrim(' hello  ') = ' hello'`|
| reverse | 字符串反转    | 1. 原字符串     | 翻转后的字符串 |`reverse('hello') = 'olleh' `|
| strlen  | 取字符串长度    | 1. 原字符串     | 整数值，字符长度 |`strlen('hello') = 5 `|
| substr  | 取字符的子串  | 1. 原字符串 2. 起始位置. 注意: 下标从 0 开始 | 子串 |`substr('abcdef', 2) = 'cdef' `|
| substr  | 取字符的子串  | 1. 原字符串 2. 起始位置 3. 要取出的子串长度. 注意: 下标从 0 开始 | 子串 |`substr('abcdef', 2, 3) = 'cde' `|
| split   | 字符串分割    | 1. 原字符串 2. 分割符子串 | 分割后的字符串数组 | `split('a/b/ c', '/') = ['a', 'b', ' c']` |
| split   | 字符串分割, 只查找左边第一个分隔符 | 1. 原字符串 2. 分割符子串 3. 'leading' | 分割后的字符串数组 | `split('a/b/ c', '/', 'leading') = ['a', 'b/ c']` |
| split   | 字符串分割, 只查找右边第一个分隔符 | 1. 原字符串 2. 分割符子串 3. 'trailing' | 分割后的字符串数组 | `split('a/b/ c', '/', 'trailing') = ['a/b', ' c']` |
| concat   | 字符串拼接  | 1. 左字符串 2. 右符子串 | 拼接后的字符串 | `concat('a', '/bc') = 'a/bc'`<br/><br/>`'a' + '/bc' = 'a/bc'` |
| tokens   | 字符串分解(按照指定字符串符分解)  | 1. 输入字符串 2. 分割符或字符串 | 分解后的字符串数组 | `tokens(' a/b/ c', '/') = [' a', 'b', ' c']`<br/><br/>`tokens(' a/b/ c', '/ ') = ['a', 'b', 'c']`<br/><br/>`tokens(' a/b/ c\n', '/ ') = ['a', 'b', 'c\n']` |
| tokens   | 字符串分解(按照指定字符串和换行符分解)  | 1. 输入字符串 2. 分割符或字符串 3. 'nocrlf' | 分解后的字符串数组 | `tokens(' a/b/ c\n', '/ ', 'nocrlf') = ['a', 'b', 'c']`<br/><br/>`tokens(' a/b/ c\r\n', '/ ', 'nocrlf') = ['a', 'b', 'c']` |
| sprintf   | 字符串格式化, 格式字符串的用法详见 https://erlang.org/doc/man/io.html#fwrite-1 里的 Format 部分 | 1. 格式字符串 2,3,4... 参数列表。参数个数不定 | 分解后的字符串数组 | `sprintf('hello, ~s!', 'steve') = 'hello, steve!'`<br/><br/>`sprintf('count: ~p~n', 100) = 'count: 100\n'` |
| pad   | 字符串补足长度，补空格，从尾部补足  | 1. 原字符串 2. 字符总长度 | 补足后的字符串 | `pad('abc', 5) = 'abc  '` |
| pad   | 字符串补足长度，补空格，从尾部补足  | 1. 原字符串 2. 字符总长度 3. 'trailing' | 补足后的字符串 | `pad('abc', 5, 'trailing') = 'abc  '` |
| pad   | 字符串补足长度，补空格，从两边补足  | 1. 原字符串 2. 字符总长度 3. 'both' | 补足后的字符串 | `pad('abc', 5, 'both') = ' abc '` |
| pad   | 字符串补足长度，补空格，从头部补足  | 1. 原字符串 2. 字符总长度 3. 'leading' | 补足后的字符串 | `pad('abc', 5, 'leading') = '  abc'` |
| pad   | 字符串补足长度，补指定字符，从尾部补足  | 1. 原字符串 2. 字符总长度 3. 'trailing' 4. 指定用于补足的字符 | 补足后的字符串 |  `pad('abc', 5, 'trailing', '*') = 'abc**'`<br/><br/>`pad('abc', 5, 'trailing', '*#') = 'abc*#*#'` |
| pad   | 字符串补足长度，补指定字符，从两边补足  | 1. 原字符串 2. 字符总长度 3. 'both' 4. 指定用于补足的字符 | 补足后的字符串 |  `pad('abc', 5, 'both', '*') = '*abc*'`<br/><br/>`pad('abc', 5, 'both', '*#') = '*#abc*#'` |
| pad   | 字符串补足长度，补指定字符，从头部补足  | 1. 原字符串 2. 字符总长度 3. 'leading' 4. 指定用于补足的字符 | 补足后的字符串 | `pad('abc', 5, 'leading', '*') = '**abc'`<br/><br/>`pad('abc', 5, 'leading', '*#') = '*#*#abc'` |
| replace | 替换字符串中的某子串，查找所有匹配子串替换  | 1. 原字符串 2. 要被替换的子串 3. 指定用于替换的字符串 | 替换后的字符串 | `replace('ababef', 'ab', 'cd') = 'cdcdef'` |
| replace | 替换字符串中的某子串，查找所有匹配子串替换  | 1. 原字符串 2. 要被替换的子串 3. 指定用于替换的字符串 4. 'all' | 替换后的字符串 | `replace('ababef', 'ab', 'cd', 'all') = 'cdcdef'` |
| replace | 替换字符串中的某子串，从尾部查找第一个匹配子串替换  | 1. 原字符串 2. 要被替换的子串 3. 指定用于替换的字符串 4. 'trailing' | 替换后的字符串 | `replace('ababef', 'ab', 'cd', 'trailing') = 'abcdef'` |
| replace | 替换字符串中的某子串，从头部查找第一个匹配子串替换  | 1. 原字符串 2. 要被替换的子串 3. 指定用于替换的字符串 4. 'leading' | 替换后的字符串 | `replace('ababef', 'ab', 'cd', 'leading') = 'cdabef'` |
| regex_match | 判断字符串是否与某正则表达式匹配  | 1. 原字符串 2. 正则表达式 | true 或 false | `regex_match('abc123', '[a-zA-Z1-9]*') = true` |
| regex_replace | 替换字符串中匹配到某正则表达式的子串  | 1. 原字符串 2. 正则表达式 3. 指定用于替换的字符串 | 替换后的字符串 | `regex_replace('ab1cd3ef', '[1-9]', '[&]') = 'ab[1]cd[3]ef'`<br/><br/>`regex_replace('ccefacef', 'c+', ':') = ':efa:ef'` |
| ascii | 返回字符对应的 ASCII 码  | 1. 字符 | 整数值，字符对应的 ASCII 码 | `ascii('a') = 97` |
| find | 查找并返回字符串中的某个子串，从头部查找  | 1. 原字符串 2. 要查找的子串 | 查抄到的子串，如找不到则返回空字符串 | `find('eeabcabcee', 'abc') = 'abcabcee'` |
| find | 查找并返回字符串中的某个子串，从头部查找  | 1. 原字符串 2. 要查找的子串 3. 'leading' | 查抄到的子串，如找不到则返回空字符串 | `find('eeabcabcee', 'abc', 'leading') = 'abcabcee'` |
| find | 查找并返回字符串中的某个子串，从尾部查找  | 1. 原字符串 2. 要查找的子串 3. 'trailing' | 查抄到的子串，如找不到则返回空字符串 | `find('eeabcabcee', 'abc', 'trailing') = 'abcee'` |

## Map 函数

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 34%" />
<col style="width: 22%" />
<col style="width: 35%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>map_get</td>
<td>取 Map 中某个 Key 的值，如果没有则返回空值</td>
<td><ol type="1">
<li>Key 2. Map</li>
</ol></td>
<td>Map 中某个 Key 的值。支持嵌套的 Key，比如 "a.b.c"</td>
</tr>
<tr class="odd">
<td>map_get</td>
<td>取 Map 中某个 Key 的值，如果没有则返回指定默认值</td>
<td><ol type="1">
<li>Key 2. Map 3. Default Value</li>
</ol></td>
<td>Map 中某个 Key 的值。支持嵌套的 Key，比如 "a.b.c"</td>
</tr>
<tr class="even">
<td>map_put</td>
<td>向 Map 中插入值</td>
<td><ol type="1">
<li>Key 2. Value 3. Map</li>
</ol></td>
<td>插入后的 Map。支持嵌套的 Key，比如 "a.b.c"</td>
</tr>
</tbody>
</table>

## 数组函数

<table>
<colgroup>
<col style="width: 9%" />
<col style="width: 47%" />
<col style="width: 31%" />
<col style="width: 12%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>nth</td>
<td>取第 n 个元素，下标从 1 开始</td>
<td><ol type="1">
<li>原数组</li>
</ol></td>
<td>第 n 个元素</td>
</tr>
<tr class="odd">
<td>length</td>
<td>获取数组的长度</td>
<td><ol type="1">
<li>原数组</li>
</ol></td>
<td>数组长度</td>
</tr>
<tr class="even">
<td>sublist</td>
<td>取从第一个元素开始、长度为 len 的子数组。下标从 1 开始</td>
<td><ol type="1">
<li>长度 len 2. 原数组</li>
</ol></td>
<td>子数组</td>
</tr>
<tr class="odd">
<td>sublist</td>
<td>取从第 n 个元素开始、长度为 len 的子数组。下标从 1 开始</td>
<td><ol type="1">
<li>起始位置 n 2. 长度 len 3. 原数组</li>
</ol></td>
<td>子数组</td>
</tr>
<tr class="even">
<td>first</td>
<td>取第 1 个元素。下标从 1 开始</td>
<td><ol type="1">
<li>原数组</li>
</ol></td>
<td>第 1 个元素</td>
</tr>
<tr class="odd">
<td>last</td>
<td>取最后一个元素。</td>
<td><ol type="1">
<li>原数组</li>
</ol></td>
<td>最后一个元素</td>
</tr>
<tr class="even">
<td>contains</td>
<td>判断数据是否在数组里面</td>
<td><ol type="1">
<li>数据 2. 原数组</li>
</ol></td>
<td>Boolean 值</td>
</tr>
</tbody>
</table>

## 哈希函数

<table style="width:99%;">
<colgroup>
<col style="width: 12%" />
<col style="width: 20%" />
<col style="width: 13%" />
<col style="width: 16%" />
</colgroup>
<tbody>
<tr class="odd">
<td>函数名</td>
<td>函数作用</td>
<td>参数</td>
<td>返回值</td>
</tr>
<tr class="even">
<td>md5</td>
<td>求 MD5 值</td>
<td><ol type="1">
<li>数据</li>
</ol></td>
<td>MD5 值</td>
</tr>
<tr class="odd">
<td>sha</td>
<td>求 SHA 值</td>
<td><ol type="1">
<li>数据</li>
</ol></td>
<td>SHA 值</td>
</tr>
<tr class="even">
<td>sha256</td>
<td>求 SHA256 值</td>
<td><ol type="1">
<li>数据</li>
</ol></td>
<td>SHA256 值</td>
</tr>
</tbody>
</table>

## 比特操作函数

| 函数名  | 函数功能| 参数| 返回值 | 示例 |
|-----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------|-------------------------------------------------------|
| `subbits` | 从二进制数据的起始位置获取指定长度的比特位, 然后转换为无符号整型 (大端). | 1. 二进制数据 2. 要获取的长度(bits) | 无符号整数 | `subbits(payload, 10)`                                |
| `subbits` | 从二进制数据的指定下标位置获取指定长度的比特位, 然后转换为无符号整型 (大端). 下标是从 1 开始的 | 1. 二进制数据 2. 起始位置的下标 3. 要获取的长度(bits) | 无符号整数 | `subbits(payload, 1, 10)`                             |
| `subbits` | 从二进制数据的指定下标位置获取指定长度的比特位, 然后按照给定的参数转换为想要的数据类型. 下标是从 1 开始的. | 1. 二进制数据 2. 起始位置的下标 3. 要获取的长度(bits) 4. 数据类型，可选值：'integer', 'float', 'bits' 5. 符号类型, 只对整型数据有效, 可选值：'unsigned', 'signed', 6. 大端还是小端, 只对整型数据有效, 可选值：'big', 'little' | 获取到的数据 | `subbits(payload, 1, 10, 'integer', 'signed', 'big')` |

## 编解码函数


| 函数名 | 函数功能 |        参数         | 返回值 |
| -------- | -------------- |--------------- | --------------------------- |
| `base64_encode` | BASE64 编码 | 要编码的二进制数据 | Base64 编码的字符串 |
| `base64_decode` | BASE64 解码 | Base64 编码的字符串 | 解码后的二进制数据 |
| `json_encode` | JSON 编码 | 要转成 JSON 的数据结构 | JSON 字符串 |
| `json_decode` | JSON 解码 | 要解码的 JSON 字符串 | 解码后的数据结构 |
| `bin2hexstr` | 二进制数据转为 Hex 字符串 | 二进制数据 | Hex 字符串 |
| `hexstr2bin` | Hex 字符串转为二进制数据 | Hex 字符串 | 二进制数据 |



