# 内置 SQL 函数

规则引擎提供了各种内置函数，您可以在 SQL 中使用这些函数实现基本的数据处理，包括 [数学运算](#数学运算函数)、[数据类型判断](#数据类型判断函数)、[数据类型转换](#数据类型转换函数)、[字符串操作](#字符串操作函数)、[映射操作](#映射操作函数)、[数组操作](#数组操作函数)、[哈希](#哈希函数)、[压缩与解压缩](#压缩与解压缩函数)、[位操作](#比特位操作函数)、[位序列操作](#位序列操作函数)、[编解码](#编解码函数) 以及 [日期与时间转换](#日期与时间函数)。

在本章节中，所有函数的声明都遵循以下格式：

```bash
FuncName(Arg 1: Type 1 | ..., ...) -> Type 1 | ...
```

例如，`abs(X: integer | float) -> integer | float` 意味着参数 `X` 的数据类型可以是 integer 或者 float，并且其返回值的数据类型同样可以是 integer 或 float。

注意，如果传入参数的值超出了限定范围或者使用了不支持的数据类型，将导致当前 SQL 执行失败，并使执行失败计数加 1。

:::tip 提示

1. 一些转义字符使用时需要进行反转义，参考 [unescape 函数](#unescapestring-string---string)。
2. 从 EMQX 5.0 版本开始，EMQX 还支持使用 [jq 语法](https://stedolan.github.io/jq/manual/) 处理复杂的 JSON 数据，您可以阅读 [jq 函数](./rule-sql-jq.md) 部分了解更多信息。

:::


## 数学运算函数

EMQX 支持广泛的数学函数：

- 三角函数和双曲函数，包括 sin、cos、tan、asin、acos、atan、sinh、cosh、tanh、asinh、acosh、atanh。
- 数值函数，包括 abs、ceil、floor、round、sqrt、fmod。
- 指数和对数函数，包括 exp、power、log、log10、log2。

### abs(X: integer | float) -> integer | float

返回数字 `X` 的绝对值。

示例：

```bash
abs(-12) = 12
abs(-1.2) = 1.2
```

### acos(X: integer | float) -> float

返回 `X` 的反余弦值，以弧度表示。`X` 的取值范围为 `[-1, 1]`。示例：

```bash
acos(0.5) = 1.0471975511965976
```

### acosh(X: integer | float) -> float

返回 `X` 的反双曲余弦值，以弧度表示。`X` 必须大于等于 1。示例：

```bash
acosh(1.5) = 0.9624236501192069
```

### asin(X: integer | float) -> float

返回 `X` 的反正弦值，以弧度表示。`X` 的取值范围为 `[-1, 1]`。示例：

```bash
asin(0.5) = 0.5235987755982988
```

### asinh(X: integer | float) -> float

返回 `X` 的反双曲正弦值。示例：

```bash
asinh(0.5) = 0.48121182505960347
```

### atan(X: integer | float) -> float

返回 `X` 的反正切值，以弧度表示。示例：

```bash
atan(0.5) = 0.46364760900080615
```

### atanh(X: integer | float) -> float

返回 `X` 的反双曲正切值，`X` 的取值范围为 `(-1, 1)`。示例：

```bash
atanh(0.5) = 0.5493061443340549
```

### ceil(X: integer | float) -> integer

向上舍入，返回大于等于给定 `X` 的最小整数。示例：

```bash
ceil(0.8) = 1
```

### cos(X: integer | float) -> float

返回角度 `X` （以弧度表示）的余弦值。示例：

```bash
cos(0.5) = 0.8775825618903728
```

### cosh(X: integer | float) -> float

返回 `X` 的双曲余弦值。示例：

```bash
cosh(0.5) = 1.1276259652063807
```

### exp(X: integer | float) -> float

返回自然常数 e 的 `X` 次方，即 `e^X`。示例：

```bash
exp(1) = 2.718281828459045
```

### floor(X: integer | float) -> integer

向下舍入，返回小于等于给定 `X` 的最大整数。示例：

```bash
floor(3.6) = 3
```

### fmod(X: integer | float, Y: integer | float) -> float

以浮点数形式返回 `X` 除以 `Y` 的余数。示例：

```bash
fmod(6.5, 2.5) = 1.5
```

### log(X: integer | float) -> float

返回数字 `X` 的自然对数，`X` 必须大于 0。示例：

```bash
log(7.38905609893065) = 2.0
```

### log10(X: integer | float) -> float

返回数字 `X` 以 10 为底的对数，，`X` 必须大于 0。示例：

```bash
log10(100) = 2.0
```

### log2(X: integer | float) -> float

返回数字 `X` 以 2 为底的对数，，`X` 必须大于 0。示例：

```bash
log2(8) = 3.0
log2(8.5) = 3.0874628412503395
```

### round(X: integer | float) -> integer

对数字 `X` 进行四舍五入，返回最接近的整数。示例：

```bash
round(4.5) = 5
```

### power(X: integer | float, Y: integer | float) -> float

返回基数 `X` 的指数 `Y` 次幂，即 `X^Y`。示例：

```bash
power(2, 3) = 8.0
```

### random() -> float

返回一个随机浮点数，范围是 `[0, 1)`。示例：

```bash
random() = 0.5400050092601868
```

### sin(X: integer | float) -> float

返回角度 `X` （以弧度表示）的正弦值。示例：

```bash
sin(0.5) = 0.479425538604203
```

### sinh(X: integer | float) -> float

返回 `X` 的双曲正弦值。示例：

```bash
sinh(0.5) = 0.5210953054937474
```

### sqrt(X: integer | float) -> float

返回数字 `X` 的平方根。示例：

```bash
sqrt(9) = 3.0
```

### tan(X: integer | float) -> float

返回角度 `X` （以弧度表示）的正切值。示例：

```bash
tan(0.5) = 0.5463024898437905
```

### tanh(X: integer | float) -> float

返回 `X` 的双曲正切值。示例：

```bash
tanh(0.5) = 0.46211715726000974
```

## 数据类型判断函数

数据类型判断函数可用于检查指定字段的数据类型，并通过布尔值指示该字段是否符合指定的数据类型。

### is_array(Term: any) -> boolean

> any 表示所有数据类型。

判断 `Term` 是否为 array 类型。示例：

```bash
is_array([1, 2]) = true
is_array(json_decode('[{"value": 1}]')) = true
is_array(json_decode('{"value": 1}')) = false
is_array(0.5) = false
is_array('[1, 2]') = false
```

### is_bool(Term: any) -> boolean

判断 `Term` 是否为 boolean 类型。示例：

```bash
is_bool(true) = true
is_bool(false) = false
is_bool('true') = false
```

### is_float(Term: any) -> boolean

判断 `Term` 是否为 float 类型。示例：

```bash
is_float(123.4) = true
is_float(123) = false
```

### is_int(Term: any) -> boolean

判断 `Term` 是否为 integer 类型。示例：

```bash
is_int(123) = true
is_int(123.4) = false
```

### is_map(Term: any) -> boolean

判断 `Term` 是否为 map 类型。示例：

```bash
is_map(json_decode('{"value": 1}')) = true
is_map(json_decode('[{"value": 1}]')) = false
```

### is_null(Term: any) -> boolean

判断变量 `Term` 是否未定义。示例：

```sql
is_null(this_is_an_unassigned_variable) = true
is_null(map_get('b', json_decode('{"a": 1}'))) = true
```

### is_num(Term: any) -> boolean

判断 `Term` 是否为 integer 或 float 类型。示例：

```bash
is_num(123) = true
is_num(123.4) = true
is_num('123') = false
```

### is_str(Term: any) -> boolean

判断 `Term` 是否为 string 类型。示例：

```bash
is_str('123') = true
is_str(123) = false
```

## 数据类型转换函数

### bool(Term: boolean | integer | string) -> boolean

将 `Term` 转换为 boolean 类型，Term 只能是 boolean 类型、integer 类型的 0 和 1，或者 string 类型的 ture 和 false。

示例：

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

将 `Term` 转换为 float 类型。

当 `Term` 为 string 类型时，可以使用科学计数法，例如 `float('3.14e4')`。float 类型最多支持 16 位有效数字，如果字符串 Term 表示的浮点数的有效数字超过了 16 位时，转换可能出现舍入误差。

示例：

```bash
float(20) = 20.0

float('3.14') = 3.14
float('3.14e4') = 31400
float('3.14e+4') = 31400
float('3.14e-4') = 0.000314
float('3.14E-4') = 0.000314

# 有效数字超过 16 位后，由于舍入误差，不同的输入可能产生同样的输出
float('0.12345678901234566') = 0.12345678901234566
float('0.12345678901234567') = 0.12345678901234566
```

### float(Term: float | integer | string, Decimals: integer) -> float

将 Term 转换成小数点后最多包含 Decimals 位数字的浮点数，Decimals 的取值范围为 `(0, 253]`。其余行为与 `float/1` 相同。示例：

```bash
float('3.1415926', 3) = 3.142
float('0.000012345', 5) = 0.00001
```

### float2str(Float: float, Decimals: integer) -> string

将浮点数 `Float` 转换为小数点后最多包含 `Decimals` 位数字的字符串，字符串末尾的 0 将被截断。`Decimals` 的取值范围为 `[0, 253]`。如果 Float 的有效数字超过了 16 位，那么转换将可能出现舍入误差。

由于浮点数在计算机中无法精确存储，所以当 `Decimals` 大于 `Float` 的小数位数（包括前导零）时，`float2str` 可能返回 `Float` 的二进制近似值的十进制形式。

示例：

```bash
float2str(0.1, 5) = '0.1'
float2str(0.1, 20) = '0.10000000000000000555'
float2str(0.1, 25) = '0.1000000000000000055511151'
float2str(0.00000000001, 20) = '0.00000000001'

# 末尾的零将被截断
float2str(0.100001, 5) = '0.1'

# 有效数字超过 16 位后，由于舍入误差，不同的输入可能产生同样的输出
float2str(123456789.01234565, 8) = '123456789.01234566'
float2str(123456789.01234566, 8) = '123456789.01234566'
```

### int(Term: boolean | float | integer | string) -> integer

将 `Term` 转换为 integer 类型。

当 `Term` 为 boolean 类型时，ture 将被转换为数字 1，false 将被转换为数字 0。

当 `Term` 为 float 类型时，`Term` 将被向下舍入，转换为小于等于 `Term` 的最大整数。

当 `Term` 为 string 类型时，`Term` 必须至少包含一个数字字符，可以具有单个 `+` 或 `-` 字符组成的可选前缀，前导零将被忽略。支持使用数学计数法表示。

当 `Term` 为 integer 类型时，`Term` 将原样返回。

示例：

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

将任意类型的 `Term` 转换为 string 类型。

当 `Term` 为 map 或 array 类型时，str 函数将尝试对 `Term` 进行 JSON 编码。

当 `Term` 为 float 类型时，str 函数将返回对应的字符串，字符串末尾的 0 将被截断。返回的字符串在小数点后最多包含 10 位数字，如果想要返回更多小数位，请使用 float2str 函数。

示例：

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

## 字符串操作函数

字符串函数可用于对字符串的大小写转换、空格删除、子串截取、转义/反转义、替换等处理。

### ascii(Char: string) -> integer

返回字符 Char 对应的 ASCII 编号，如果 Char 包含多个字符，仅返回第一个字符的对应编号。示例：

```bash
ascii('a') = 97
ascii('abc') = 97
```

### concat(Str1: string, Str2: string) -> string

将 Str1 和 Str2 合并为一个字符串。示例：

```bash
concat('Name:', 'John') = 'Name:John'
```

### find(String: string, SearchPattern: string) -> string

在 String 中查找子串 SearchPattern，删除 String 中 SearchPattern 之前的所有内容，并返回字符串中的其余部分。如果未找到 SearchPattern，则返回一个空字符串。同 `find(String, SearchPattern, 'leading')`

示例：

```bash
find('..., Value: 1.2', 'Value:') = 'Value: 1.2'
find('..., Value: 1.2', 'Data') = ''
```

### find(String: string, SearchPattern: string, Direction: string) -> string

同 `find/2`，但可以使用 Direction 指定查找子串 SearchPattern 的方向。示例：

```bash
find('Front, Middle, End', ', ', 'leading') = ', Middle, End'
find('Front, Middle, End', ', ', 'trailing') = ', End'
```

### lower(String: string) -> string

将字符串 String 中的大写字母转换为小写字母。示例：

```bash
lower('Hello') = 'hello'
```

### ltrim(String: string) -> string

同 `trim/1`，但仅删除字符串 String 中前导的空白字符。示例：

```bash
ltrim('\t  hello  \n') = 'hello  \n'
ltrim('\t  hello \r\n') = 'hello  \r\n'
```

### pad(String: string, Length: integer) -> string

为 String 填充尾随空格，使其达到指定长度 Length。示例：

```bash
pad('hello', 8) = 'hello   '
```

### pad(String: string, Length: integer, Direction: string) -> string

同 `pad/2`，但可以使用 Direction 指定空格填充的方向。leading 表示填充前导空格，trailing 表示填充尾随空格，both 表示同时填充前导和尾随空格。

指定 Direction 为 both 时，如果需要填充的空格数量为奇数，那么最后一个空格将被填充在尾部。

示例：

```bash
pad('hello', 8, 'leading') = '   hello'
pad('hello', 8, 'trailing') = 'hello   '
pad('hello', 8, 'both') = ' hello  '
```

### pad(String: string, Length: integer, Direction: string, Char: string) -> string

同 `pad/3`，但可以使用指定字素簇 Char 进行填充。

由于规则引擎不检查 Char 是否是一个合法的字素簇，所以 Char 无论包含了多少字符，都会按一个字符长度处理。示例：

```bash
pad('hello', 8, 'trailing', '!') = 'hello!!!'
pad('hello', 8, 'trailing', '\r\n') = 'hello\r\n\r\n\r\n'
pad('hello', 8, 'trailing', 'abc') = 'helloabcabcabc'
```

### regex_match(String: string, Expression: string) -> boolean

判断字符串 String 是否与正则表示式 Expression 匹配。示例：

```bash
regex_match('123', '^\d+$') = true
regex_match('a23', '^\d+$') = false
```

### regex_replace(String: string, Expression: string, Replacement: string) -> string

使用字符串 Replacement 替换 String 中与正则表达式 Expression 匹配的部分。如果未找到匹配部分，则返回原始的 String。示例：

```bash
regex_replace('hello 123', '\d+', 'world') = 'hello world'
regex_replace('a;b; c', ';\s*', ',') = 'a,b,c'
```

### regex_extract(String: string, Expression: string) -> [string]

::: tip

此函数在 EMQX v5.7.1 中引入。

:::

这个函数在给定字符串中进行非全局搜索，查找带有捕获组的正则表达式模式。
它可以用来根据正则表达式提取字符串的部分内容，排除完整匹配本身。

如果找到匹配项，它将返回这些匹配项中所有捕获组的列表。如果没有找到匹配项或没有捕获到任何组，则返回一个空列表。

示例：

```bash
regex_extract('Number: 12345', '(\d+)') -> ['12345']
regex_extract('Hello, world!', '(\w+).*\s(\w+)') -> ['Hello', 'world']
regex_extract('No numbers here!', '(\d+)') -> []
regex_extract('Date: 2021-05-20', '(\d{4})-(\d{2})-(\d{2})') -> ['2021', '05', '20']
```

### replace(String: string, SearchPattern: string, Replacement: string) -> string

将 String 中的所有 SearchPattern 都替换为 Replacement。示例：

```bash
replace('ab..cd..ef', '..', '**') = 'ab**cd**ef'
replace('ab..cd..ef', '..', '') = 'abcdef'
```

### replace(String: string, SearchPattern: string, Replacement: string, Where: string) -> string

将出现在 String 中的 SearchPattern 替换为 Replacement。

Where 有以下可取值：

- all：替换所有的 SearchPattern，等同于 `replace/3`。
- leading：仅替换前导 SearchPattern。
- trailing：仅替换尾随 SearchPattern。

示例：

```bash
replace('ab..cd..ef', '..', '**', 'all') = 'ab**cd**ef'
replace('ab..cd..ef', '..', '**', 'leading') = 'ab**cd..ef'
replace('ab..cd..ef', '..', '**', 'trailing') = 'ab..cd**ef'
```

### reverse(String: string) -> string

反转字符串 String。示例：

```bash
reverse('hello') = 'olleh'
```

### rtrim(String: string) -> string

同 `trim/1`，但仅删除字符串 String 中尾随的空白字符。示例：

```bash
rtrim('\t  hello  \n') = '\t  hello'
rtrim('\t  hello \r\n') = '\t  hello'
```

### split(String: string, Separator: string) -> array

使用分隔符 Separator 将字符串 String 分割成子字符串并返回一个由这些子字符串组成的数组。

两个或多个相邻的 Separator 不会被视为一个，所以分割结果中可能包含空字符串。`split/2` 默认对输出结果进行了修整，过滤了其中空字符串，如需保留，请使用 `split(String, Separator, 'notrim')`。

Separator 可以由多个字符组成，但它们将被视为一个整体。如果想要一次指定多个分隔字符，请使用 `tokens` 函数。

示例：

```bash
split('a;', ';') = ['a']
split('a;b;c', ';') = ['a', 'b', 'c']
split('a;;b;;c', ';') = ['a', 'b', 'c']

# Note the space before Howell Wise
split('Sienna Blake; Howell Wise', ';') = ['Sienna Blake', ' Howell Wise']
split('Sienna Blake; Howell Wise', '; ') = ['Sienna Blake', 'Howell Wise']
```

### split(String: string, Separator: string, Option: string) -> array

同 `split/2`，但可以使用 Option 指定需要处理的分隔符的位置，以及是否需要返回空字符串。

Option 有以下可取值：

- notrim：处理字符串中的所有分隔符，返回的结果中可能包含空字符串。
- leading：仅处理前导的分隔符，返回的结果中不包含空字符串。
- leading_notrim：仅处理前导的分隔符，返回的结果中可能包含空字符串。
- trailing：仅处理尾随的分隔符，返回的结果中不包含空字符串。
- trailing_notrim：仅处理尾随的分隔符，返回的结果中可能包含空字符串。

示例：

```bash
split('a;;b;;c', ';', 'notrim') = ['a', '', 'b', '', 'c']
split('a;b;c', ';', 'leading') = ['a', 'b;c']
split('a;b;c', ';', 'trailing') = ['a;b', 'c']
split(';a;b;c', ';', 'leading_notrim') = ['', 'a;b;c']
split('a;b;c;', ';', 'trailing_notrim') = ['a;b;c', '']
```

### sprintf(Format, ...) -> string

返回一个按照 Format 格式化的字符串。Format 字符串包含普通字符以及用于格式化的控制序列。

控制序列的格式一般为：`~F.P.PadModC`。

字符 C 确定要使用的控制序列的类型。这是唯一必填字段。 F、P、Pad 和 Mod 都是可选的。关于它们的详细介绍，请参阅：https://www.erlang.org/doc/man/io.html#fwrite-1。

示例：

```bash
sprintf('hello, ~s!', 'steve') = 'hello, steve!'
sprintf('count: ~p~n', 100) = 'count: 100\n'
```

### strlen(String: string) -> integer

返回字符串 String 的长度。示例：

```bash
strlen('hello') = 5
strlen('hello\n') = 6
```

### substr(String: string, Start: integer) -> string

返回 String 中从位置 Start 开始到字符串末尾的所有字符，字符串的下标从 0 开始，即位置 0 对应的是字符串 “hello” 中的 “h”。示例：

```bash
substr('hello', 0) = 'hello'
substr('hello world', 6) = 'world'
```

### substr(String: string, Start: integer, Length: integer) -> string

返回 String 中从位置 Start 开始，最大长度为 Length 的子字符串，字符串的下标从 0 开始。示例：

```bash
substr('hello world!', 6, 5) = 'world'
```

### tokens(String: string, SeparatorList: string) -> array

返回 String 被 SeparatorList 中的字符分割后的子串列表。

两个或多个相邻的分隔符将被视为一个，所以不会出现空字符串。

示例：

```bash
tokens('a,b;c,d', ',;') = ['a', 'b', 'c', 'd']
tokens('a;;b', ';') = ['a', 'b']
```

### tokens(String: string, SeparatorList:string, NoCRLF: string) -> array

同 `tokens/2`，但可以指定 NoCRLF 为 nocrlf 表示同时分割回车符和换行符。示例：

```bash
tokens('a\rb\nc\r\nd', ';', 'nocrlf') = ['a', 'b', 'c', 'd']
```

### trim(String: string) -> string

删除字符串 String 中前导和尾随的应被视为空白的字符，例如空格、制表符、换页符及换行符。注意，在 Unicode 标准中 `\r\n` 被视为一个字素簇，所以 `\r\n` 会被一并被删除。示例：

```bash
trim('\t  hello  \n') = 'hello'
trim('\t  hello \r\n') = 'hello'
```

### unescape(String: string) -> string

反转义函数，用于将转义字符转换回它们表示的字符。当 SQL 中使用了转义字符时，需要首先使用该函数进行反转义才能正确使用。

::: tip

此函数在 EMQX v5.7.0 中引入。

:::

例如当 Payload 为换行字符串：

```bash
32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3
87.2
12.3
my-device
```

需要按照 `\n` 分割 Payload 为数组，如下 SQL 将无法按预期执行：

```sql
SELECT split(payload, '\n') as device_info FROM 't/#'
```

输出结果：

```json
{
  "device_info": [
    "32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3\n87.2\n12.3\nmy-device"
  ]
}
```

使用 unescape 函数对 `\n` 反转义后，可以得到期望的结果：

```sql
SELECT split(payload, unescape('\n')) as device_info FROM 't/#'
```

输出结果：

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

**unescape 函数支持以下转义字符：**

标准 C 转义序列：

- `\n` 表示换行符（LF）
- `\t` 表示水平制表符（HT）
- `\r` 表示回车符（CR）
- `\b` 表示退格符（BS）
- `\f` 表示换页符（FF）
- `\v` 表示垂直制表符（VT）
- `\'` 表示单引号（'）
- `\"` 表示双引号（"）
- `\\` 表示反斜杠
- `\?` 表示问号（?）
- `\a` 表示警告符（响铃符，BEL）

十六进制转义码：

- `\xH...` 其中 `H...` 是一个或多个十六进制数字（0-9, A-F, a-f），允许编码任意的 UTF-32 字符。

如果传入未识别的转义符，或者十六进制转义码包含无效的 Unicode 字符，则该函数将抛出异常。

### upper(String: string) -> string

将字符串 String 中的小写字母转换为大写字母。示例：

```bash
upper('hello') = 'Hello'
```

## 映射操作函数

### map_get(Key: string, Map: map) -> any

返回 Map 中指定 Key 的值，如果该 Key 在 Map 中不存在，则返回 undefined。示例：

```bash
map_get('msg', json_decode('{"msg": "hello"}')) = 'hello'
map_get('data', json_decode('{"msg": "hello"}')) = undefined
```

### map_get(Key: srting, Map: map, Default: any) -> any

同 `map_get/2`，但 Key 不存在时，将返回指定的 Default。示例：

```bash
map_get('data', json_decode('{"msg": "hello"}'), '') = ''
map_get('value', json_decode('{"data": [1.2, 1.3]}'), []) = []
```

### map_put(Key: string, Value: any, Map: map) -> map

将 Key 与关联的 Value 插入到 Map 中，返回更新后的 Map。如果原始 Map 中该 Key 已经存在，那么旧的关联值将被替换为新的 Value。示例：

```bash
map_get('b', map_put('b', 1, json_decode('{"a": 1}'))) = 1
map_get('a', map_put('a', 2, json_decode('{"a": 1}'))) = 2
```

### map_to_redis_hset_args(Map) -> list

::: tip

此函数在 EMQX v5.7.1 中引入。

:::

此函数将映射转换为字段名称和值的列表，用于格式化 Redis 的 `HSET`（或 `HMSET`）命令。
转换规则如 `SELECT map_to_redis_hset_args(payload.value) as hset_fields FROM t/1`，
这样可以准备好 `hset_fields` 变量，以便集成到 Redis 动作命令模板中，格式为 `HMSET name1 ${hset_fields}`。
例如，如果 `payload.value` 是映射 `{"a" : 1, "b": 2}`，则生成的命令可能为 `HMSET name1 b 2 a 1`。
请注意，映射中的字段顺序是不确定的。

### mget(Key: string | array, Map: map) -> any

返回 Map 中指定 Key 的值，如果该 Key 在 Map 中不存在，则返回 undefined。可以使用数组一次指定多个 Key 以便从嵌套的 Map 中获取关联的值。示例：

```bash
mget('c', json_decode('{"a": {"b": 1}}')) = undefined
json_decode(mget('a', json_decode('{"a": {"b": 1}}'))) = '{"b": 1}'
mget(['a', 'b'], json_decode('{"a": {"b": 1}}')) = 1
```

### mput(Key: string | array, Value: any, Map: map) -> map

将 Key 与关联的 Value 插入到 Map 中，返回更新后的 Map。如果原始 Map 中该 Key 已经存在，那么旧的关联值将被替换为新的 Value。可以使用数组一次指定多个 Key 以便向嵌套的 Map 插入数据。示例：

```bash
mget(['a', 'b'], mput(['a', 'b'], 2, json_decode('{"a": {"b": 1}}'))) = 2
mget(['a', 'b'], mput(['a', 'b'], 2, json_decode('{"c": 1}'))) = 2
```

## 数组操作函数

### contains(Item: any, Array: array) -> boolean

判断数组 Array 是否包含指定的 Item。示例：

```bash
contains(2, [1, 2, 3]) = true
contains(2.3, [1.8, 2.5, 2.0]) = false
contains('John', ['John', 'David']) = true
contains([1, 2], [a, b, [1, 2]]) = true
contains(json_decode('{"a": 1}'), [json_decode('{"a": 1}'), json_decode('{"b": 2}')]) = true
```

### first(Array: array) -> any

返回数组 Array 中的第一个元素。Array 不可为空。示例：

```bash
# Correct
first(['John', 'David']) = 'John'

# Wrong
first([])
```

### last(Array: array) -> any

返回数组 Array 中的最后一个元素。Array 不可为空。示例：

```bash
# Correct
last(['John', 'David']) = 'David'

# Wrong
last([])
```

### length(Array: array) -> integer

返回数组 Array 的长度，即 Array 中元素的个数。示例：

```bash
length([1,2,3,4]) = 4
length([]) = 0
```

### nth(N: integer, Array: array) -> any

返回数组 Array 中的第 N 个元素。N 不应大于 Array 长度。示例：

```bash
# Correct
nth(1, [1,2,3]) = 1

# Wrong
nth(0, [1,2,3])
nth(4, [1,2,3])
```

### sublist(Length: integer, Array: array) -> any

返回从数组 Array 中第 1 个元素开始，最大长度为 Length 的子数组。如果 Length 大于 Array 长度，将返回整个数组。示例：

```bash
sublist(3, [1,2,3,4]) = [1,2,3]
sublist(10, [1,2,3,4]) = [1,2,3,4]
```

### sublist(Start: integer, Length: integer, Array:array) -> any

同 `sublist/2`，但可以使用 Start 指定从第几个元素开始返回。如果 Start + Length 大于 Array 长度，那么将返回整个数组。示例：

```bash
sublist(2, 10, [1,2,3,4]) = [2,3,4]
```

## 哈希函数

### md5(String: string) -> string

为任意长度字符串 String 计算长度固定为 128 位的 MD5 散列值。该散列值将以 32 个十六进制数字组成的文本形式返回。返回字符串中的字母固定为小写形式（a ~ f）。

示例：

```bash
md5('hello') = '5d41402abc4b2a76b9719d911017c592'
```

### sha(String: string) -> string

使用 **SHA-1** 算法为任意长度字符串 String 计算长度固定为 160 位的 SHA 散列值。该散列值将以 40 个十六进制数字组成的文本形式返回。返回字符串中的字母固定为小写形式（a ~ f）。

示例：

```bash
sha('hello') = 'aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d'
```

### sha256(String: string) -> string

使用 **SHA-2** 算法为任意长度字符串 String 计算长度固定为 256 位的 SHA 散列值。该散列值将以 64 个十六进制数字组成的文本形式返回。返回字符串中的字母固定为小写形式（a ~ f）。

示例：

```bash
sha256('hello') = '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824'
```

## 压缩与解压缩函数

注意：二进制数据无法直接进行 JSON 编码，必须调用 bin2hexstr 函数将其转换成对应的由十六进制数字组成的字符串。

### gunzip(Data: binary) -> binary | string

解压 Data，Data **必须包含 gz 头部和位于尾部的校验和**。示例：

```bash
gunzip(hexstr2bin('1F8B0800000000000013CB48CDC9C9070086A6103605000000')) = 'hello'
```

### gzip(Data: binary | string) -> binary

使用 DEFLATE 算法压缩 Data，返回的压缩结果中 **包含 gz 头部和位于尾部的校验和**。示例：

```bash
bin2hexstr(gzip('hello')) = '1F8B0800000000000013CB48CDC9C9070086A6103605000000'
```

### unzip(Data: binary) -> binary | string

解压 Data，Data 中 **不应包含 zlib 头部和位于尾部的校验和**。示例：

```bash
unzip(hexstr2bin('CB48CDC9C90700')) = 'hello'
```

### zip(Data: binary | string) -> binary

使用 DEFLATE 算法压缩 Data，返回的压缩结果中 **不包含 zlib 头部和位于尾部的校验和**。示例：

```bash
bin2hexstr(zip('hello')) = 'CB48CDC9C90700'
```

### zip_compress(Data: binary | string) -> binary

使用 DEFLATE 算法压缩 Data，返回的压缩结果中 **包含 zlib 头部和位于尾部的验和**。示例：

```bash
bin2hexstr(zip_compress('hello')) = '789CCB48CDC9C90700062C0215'
```

### zip_uncompress(Data: binary) -> binary | string

解压 Data，Data **必须包含 zlib 头部和位于尾部的校验和**。示例：

```bash
zip_uncompress(hexstr2bin('789CCB48CDC9C90700062C0215')) = 'hello'
```

## 比特位操作函数

### bitand(Num1: integer, Num2: integer) -> integer

返回 Num1 和 Num2 的 **按位与** 结果，输入输出均为有符号整型。示例：

```bash
bitand(10, 8) = 8
bitand(-10, -8) = -16
```

### bitnot(Num: integer) -> integer

返回 Num 的 **按位取反** 结果，输入输出均为有符号整型。示例：

```bash
bitnot(10) = -11
bitnot(-12) = 11
```

### bitsl(Num: integer, Shift: integer) -> integer

将 Num 按位左移 Shift 位，右侧空白由 0 填充。示例：

```bash
bitsl(8, 2) = 32
bitsl(-8, 2) = -32
```

### bitsr(Num: integer, Shift: integer) -> integer

将 Num 按位右移 Shift 位，左侧空白填充符号位（即正数补 0，负数补 1）。示例：

```bash
bitsr(8, 2) = 2
bitsr(8, 4) = 0
bitsr(-8, 2) = -2
bitsr(-8, 6) = -1
```

### bitor(Num1: integer, Num2: integer) -> integer

返回 Num1 和 Num2 的 **按位或** 结果。示例：

```bash
bitor(10, 8) = 10
bitor(-10, -8) = -2
```

### bitxor(Num1: integer, Num2: integer) -> integer

返回 Num1 和 Num2 的 **按位异或** 结果。示例：

```bash
bitxor(10, 8) = 2
bitxor(-10, -8) = 14
```

## 位序列操作函数

规则引擎提供了一些用于操作位序列的函数。例如 subbits 用于提取位序列并将其转换为指定的数据类型。

:::tip

binary 类型表示一个字节序列，每个字节由 8 个比特位组成，所以任意一个 binary 中的比特位一定是 8 的整数倍。bitstring 类型表示一个位序列，它可以由任意数量的比特位组成。

简单地说，每个 binary 都是 bitstring，但反过来则不一定。

需要注意的是，长度不是 8 的整数倍的 bitstring 不能直接序列化为 JSON 字符串这样的外部格式。它通常作为中间值存在，然后被转换为整数或其他合适的类型使用。

:::

### bitsize(Bin: binary) -> integer

返回位序列 Bin 的比特位数。示例：

```bash
bitsize('abc') = 24
bitsize('你好') = 48
```

### byteszie(Bin: binary) -> integer

返回字节序列 Bin 的字节数。示例：

```bash
byteszie('abc') = 3
byteszie('你好') = 6
```

### subbits(Bin: binary, BitNum: integer) -> integer

从字节序列 Bin 的起始位置开始，获取长度为 BitNum 的比特位，按照大端序列将其转换为无符号整型。等同于 `subbits(Bytes, 1, BitNum, 'integer', 'unsigned', 'big')`。

示例：

```bash
# 159 = 0x9F
subbits(hexstr2bin('9F4E58'), 8) = 159

# 40782 = 0x9F4E
subbits(hexstr2bin('9F4E58'), 16) = 40782

# bin2hexstr(base64_decode('n05Y')) = '9F4E58'
subbits(base64_decode('n05Y'), 8) = 159
```

### subbits(Bin: binary, Start: integer, BitNum: integer) -> integer

从字节序列 Bin 的位置 Start 开始（起始位置为 1），获取长度为 BitNum 的比特位，按照大端序列将其转换为无符号整型。等同于 `subbits(Bytes, Start, BitNum, 'integer', 'unsigned', 'big')`。

示例：

```bash
# 159 = 0x9F
subbits(hexstr2bin('9F4E58'), 1, 8) = 159

# 78 = 0x4E
subbits(hexstr2bin('9F4E58'), 9, 8) = 78

# bin2hexstr(base64_decode('n05Y')) = '9F4E58'
subbits(base64_decode('n05Y'), 9, 4) = 4
```

### subbits(Bin: binary, Start: integer, BitNum: integer, OutputType: string, Signedness: string, Endianness: string) -> bitstring | integer | float

从字节序列 Bin 的位置 Start 开始（起始位置为 1），获取长度为 BitNum 的比特位，按照要求的字节顺序 Endianness 和符号性 Signedness 将其转换为 OutputType 类型的数据。

OutputType 的可取值有：

- bits：bitstring 的缩写
- integer
- float

Signedness 的可取值有：

- signed
- unsigned

Endianness 的可取值有：

- big
- little

注意，OutputType 为 float 时，参数 Signedness 不生效，OutputType 为 bits 时，参数 Signedness 和 Endianness 不生效。

示例：

```bash
# 40782 = 0x9F4E
subbits(hexstr2bin('9F4E58'), 1, 16, 'integer', 'unsigned', 'big') = 40782
subbits(hexstr2bin('9F4E58'), 1, 16, 'integer', 'signed', 'big') = -24754

# 20127 = 0x4E9F
subbits(hexstr2bin('9F4E58'), 1, 16, 'integer', 'unsigned', 'little') = 20127

subbits(hexstr2bin('9F4E58'), 1, 16, 'float', 'unsigned', 'big') = -0.00713348388671875
subbits(hexstr2bin('9F4E58'), 1, 16, 'float', 'signed', 'big') = -0.00713348388671875
```

## 编解码函数

### base64_decode(Data: string) -> bytes | string

对 Data 进行 Base64 解码。

```bash
base64_decode('aGVsbG8=') = 'hello'
bin2hexstr(base64_decode('y0jN')) = 'CB48CD'
```

### base64_encode(Data: binary | string) -> string

对 Data 进行 Base64 编码。示例：

```bash
base64_encode('hello') = 'aGVsbG8='
base64_encode(hexstr2bin('CB48CD')) = 'y0jN'
```

### json_decode(Data: string) -> array | map

对 Data 进行 JSON 解码。示例：

```bash
map_get('a', json_decode('{"a": 1}')) = 1
```

### json_encode(Data: array | map) -> string

对 Data 进行 JSON 编码。示例：

```bash
json_encode([1,2,3]) = '[1,2,3]'
```

### bin2hexstr(Data: binary) -> string

将二进制数据转换为对应的由十六进制数字组成的字符串。示例：

```bash
bin2hexstr(zip('hello')) = 'CB48CDC9C90700'
```

### hexstr2bin(Data: string) -> binary

将由十六进制数字组成的字符串转换为对应的二进制数据。示例：

```bash
unzip(hexstr2bin('CB48CDC9C90700')) = 'hello'
```

### Schema Registry

在 EMQX 企业版中， schema registry 提供了`schema_decode` 和 `schema_encode` 功能，可以为 [Protobuf (Protocol Buffers)](https://developers.google.com/protocol-buffers) 和 [Avro](https://avro.apache.org/) 格式的数据进行编解码。 关于功能详情，请见[编解码](./schema-registry.md)。

### schema_encode(SchemaID: string, Data: map) -> binary

使用指定的 Avro Schema 对 `Data` 进行编码。在 Schema Registry 中创建 Schema 以获取 ID。

### schema_encode(SchemaID: string, Data: map, MsgType: string) -> binary

使用指定的 Protobuf Schema 对 `Data` 进行编码。在 Schema Registry 中创建 Schema 以获取 ID。MsgType 用于指定 Data 在 Protobuf Schema 中对应的消息类型。

### schema_decode(SchemaID: string, Bin: binary) -> map

使用指定的 Avro Schema 对 `Bin` 进行解码。在 Schema Registry 中创建 Schema 以获取 ID。

### schema_decode(SchemaID: string, Bin: binary, MsgType: string) -> map

使用指定的 Protobuf Schema 对 `Bin` 进行解码。在 Schema Registry 中创建 Schema 以获取 ID。MsgType 用于指定 Data 在 Protobuf Schema 中对应的消息类型。

### Sparkplug B

EMQX 企业版还有专门用于解码和编码 Sparkplug B 消息的特殊用途函数（`sparkplug_decode` 和`sparkplug_encode`）。您可以在 [Sparkplug B](./sparkplug.md) 中了解有关 Sparkplug 函数的更多信息。

## 日期与时间函数

### date_to_unix_ts(Unit: string, FormatString: string, DateTimeString: string) -> integer

按照格式字符串 FormatString 解析日期时间字符串 DateTimeString，将其转换为时间单位为 Unit 的 Unix 时间。

`second`, `millisecond`, `microsecond` 和 `nanosecond` 为可以使用的 Unit。

FormatString 中可以使用的占位符如下：

| 占位符 | 含义                                 | 取值范围              |
| ------ | ------------------------------------ | --------------------- |
| `%Y`   | 由四位数字表示的年份                 | 0000 - 9999           |
| `%m`   | 由两位数字表示的月份                 | 01 - 12               |
| `%d`   | 由两位数字表示的月中的日期           | 01 - 31               |
| `%H`   | 由两位数字表示的小时，采用 24 小时制 | 00 - 24               |
| `%M`   | 由两位数字表示的分钟                 | 00 - 59               |
| `%S`   | 由两位数字表示的秒钟                 | 00 - 59               |
| `%N`   | 纳秒                                 | 000000000 - 999999999 |
| `%6N`  | 微秒，即取纳秒的前六位数字           | 000000 - 999999       |
| `%3N`  | 毫秒，即取纳秒的前三位数字           | 000 - 999             |
| `%z`   | 时区偏移量，格式为 `±hhmm`           | -1159 - +1159         |
| `%:z`  | 时区偏移量，格式为 `±hh:mm`          | -11:59 - +11:59       |
| `%::z` | 时区偏移量，格式为 `±hh:mm:ss`       | -11:59:59 - +11:59:59 |

示例：

```bash
date_to_unix_ts('second', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00+08:00') = 1708671600
```

### date_to_unix_ts(Unit: string, Offset: string | integer, FormatString: string, DateTimeString: string) -> integer

如果 DateTimeString 中未包含时区偏移量，则可以使用 Offset 手动指定该偏移量，其他行为同 `date_to_unix_ts/3`。Offset 可以是字符串，也可以是直接以整型表示的秒数。

当 Offset 为字符串时，可以使用以下格式：

- `Z` 或 `z`，表示 UTC 偏移量 00:00。
- `±hh[:mm][:ss]` 或 `±hh[mm][ss]`，相对 UTC 的正负时间偏移量。
- `local`，表示系统本地时区对应的偏移量。

示例：

```bash
date_to_unix_ts('second', '+08:00', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00') = 1708671600
date_to_unix_ts('second', 'Z', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 07:00:00') = 1708671600
date_to_unix_ts('second', 14400, '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00') = 1708686000
```

### format_date(Unit: string, Offset: string | integer, FormatString: string, Time: Integer) -> string

将 Unix 时间转换为指定格式的日期时间字符串。Unit 表示待转换 Unix 时间 Time 的时间单位，Offset 表示输出的日期时间中的时区偏移量，FormatString 则表示输出的日期时间格式。

Unit，Offset 和 FormatString 的可取值参见 `date_to_unix_ts/3, 4`。

示例：

```bash
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%z', 1708933353472) = '2024-02-26 15:42:33.472000+0800'
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%:z', 1708933353472) = '2024-02-26 15:42:33.472000+08:00'
format_date('millisecond', '+08:20:30', '%Y-%m-%d %H:%M:%S.%3N%::z', 1708933353472) = '2024-02-26 16:03:03.472+08:20:30'
format_date('millisecond', 'Z', '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 07:42:33.472+08:00'
format_date('millisecond', 28800, '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 15:42:33.472+08:00'
```

### now_rfc3339() -> string

以 RFC3339 日期时间字符串形式返回当前系统时间，时间单位为秒。示例：

```bash
now_rfc3339() = '2024-02-23T10:26:20+08:00'
```

### now_rfc3339(Unit: string) -> string

同 `now_rfc3339/0`，但可以使用 Unit 指定时间单位，支持 `second`, `millisecond`, `microsecond` 和 `nanosecond`。示例：

```bash
now_rfc3339('microsecond') = '2024-02-23T10:26:38.009706+08:00'
```

### now_timestamp() -> integer

以 Unix 时间戳形式返回当前系统时间，时间单位为秒。示例：

```bash
now_timestamp() = 1708913853
```

### now_timestamp(Unit: string) -> integer

同 `now_timestamp/0`，但可以使用 Unit 指定时间单位，支持 `second`, `millisecond`, `microsecond` 和 `nanosecond`。示例：

```bash
now_timestamp('microsecond') = 1708913828814315
```

### rfc3339_to_unix_ts(DateTimeString: string) -> integer

将符合 RFC3339 标准的日期时间字符串转换为 Unix 时间戳。`2024-02-23T15:56:30Z` 就是一个典型的 RFC3339 日期时间字符串，它表示 UTC 时间 2024 年 2 月 23 日，15 点 56 分 30 秒。

示例：

```bash
rfc3339_to_unix_ts('2024-02-23T15:56:30Z') = 1708703790
rfc3339_to_unix_ts('2024-02-23T15:56:30+08:00') = 1708674990
```

### rfc3339_to_unix_ts(DateTimeString: string, Unit: string) -> integer

同 `rfc3339_to_unix_ts/1`，但可以使用 Unit 指定返回的 Unix 时间戳单位，支持 `second`, `millisecond`, `microsecond` 和 `nanosecond`。示例：

```bash
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'second') = 1708703790
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'millisecond') = 1708703790870
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'microsecond') = 1708703790870000
rfc3339_to_unix_ts('2024-02-23T15:56:30.535904509Z', 'nanosecond') = 1708703790535904509
```

### timezone_to_offset_seconds(Offset: string) -> integer

将字符串形式的时区偏移量转换为以整型表示的秒数。以下是支持的时间偏移量表示形式：

- `Z` 或 `z`，表示 UTC 偏移量 00:00。
- `±hh[:mm][:ss]` 或 `±hh[mm][ss]`，相对 UTC 的正负时间偏移量。
- `local`，表示系统本地时区对应的偏移量。

示例：

```bash
timezone_to_offset_seconds('Z') = 0
timezone_to_offset_seconds('+08:00') = 28800
timezone_to_offset_seconds('local') = 28800
```

### unix_ts_to_rfc3339(Time: integer) -> string

将单位为秒的 Unix 时间戳转换为符合 RFC3339 标准的日期时间字符串，使用系统本地时区。示例：

```bash
unix_ts_to_rfc3339(1708671600) = '2024-02-23T15:00:00+08:00'
```

### unix_ts_to_rfc3339(Time: integer, Unit: string) -> string

同 `unix_ts_to_rfc3339/0`，但可以使用 Unit 指定时间单位，支持 `second`, `millisecond`, `microsecond` 和 `nanosecond`。示例：

```bash
unix_ts_to_rfc3339(1708671600766, 'millisecond') = '2024-02-23T15:00:00.766+08:00'
```

### 专用于 MongoDB 的时间函数

::: tip

本节内容仅适用于 EMQX 企业版。

:::

### mongo_date() -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

以 MongoDB ISODate 类型或字符串形式返回当前时间。仅支持在 MongoDB 相关动作和 SQL 测试中使用，并且仅在 SQL 测试时 `mongo_date()` 返回字符串，例如 `ISODate("2024-02-23T15:00:00.123Z")`。暂不支持将 `mongo_date()` 除字符串以外的返回作为其他函数的输入。

示例：

```bash
mongo_date() = 'ISODate("2024-02-23T15:00:00.123Z")'
```

### mongo_date(Timestamp: integer) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

将指定的以毫秒为单位的 Unix 时间戳转换为 MongoDB ISODate 类型或字符串。其他行为同 `mongo_date/0`。

示例：

```bash
mongo_date(now_timestamp('millisecond')) = 'ISODate(2024-02-23T15:48:57.871Z)'
```

### mongo_date(Timestamp: integer, Unit: string) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

将指定的 Unix 时间戳转换为 MongoDB ISODate 类型或字符串，可以通过 Unit 指定输入时间戳的单位。其他行为同 `mongo_date/0`。

Unit 的可取值有：

- `second`
- `millisecond`
- `microsecond`
- `nanosecond`

示例：

```bash
mongo_date(now_timestamp('microsecond'), 'microsecond') = 'ISODate(2024-02-23T15:51:01.232Z)'
```

## UUID 函数

### uuid_v4() -> string

生成版本 4 UUID。示例：

```bash
uuid_v4() = 'f5bb7bea-a371-4df7-aa30-479add04632b'
```

### uuid_v4_no_hyphen() -> string

生成不包含连字符的版本 4 UUID。示例：

```bash
uuid_v4_no_hyphen() = 'd7a39aa4195a42068b962eb9a665503e'
```

