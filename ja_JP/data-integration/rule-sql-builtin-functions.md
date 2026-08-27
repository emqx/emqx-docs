# 組み込みSQL関数

ルールエンジンは多様な組み込み関数を提供しています。これらの関数はSQL内で利用でき、基本的なデータ処理を実現します。以下のカテゴリがあります：

- [数学関数](#mathematical-functions)
- [データ型判定関数](#data-type-judgment-functions)
- [データ型変換関数](#data-type-conversion-functions)
- [文字列操作関数](#string-operation-functions)
- [マップ操作関数](#map-operation-functions)
- [配列操作関数](#array-operation-functions)
- [ハッシュ関数](#hashing-functions)
- [圧縮・解凍関数](#compression-and-decompression-functions)
- [ビット操作関数](#bit-operation-functions)
- [ビット列操作関数](#bit-sequence-operation-functions)
- [エンコード・デコード関数](#encoding-and-decoding-functions)
- [日時変換関数](#date-and-time-conversion-functions)
- [UUID関数](#uuid-functions)
- [システム関数](#system-function)
- [条件関数](#conditional-functions)

本節の関数宣言はすべて以下の形式に準拠しています：

```bash
FuncName(Arg 1: Type 1 | ..., ...) -> Type 1 | ...
```

例えば、`acos(X: integer | float) -> float` は引数 `X` のデータ型が整数または浮動小数点数であり、戻り値の型は浮動小数点数であることを示します。

指定された引数が範囲外であったり、サポートされていないデータ型の場合、現在のSQL実行は失敗し、失敗回数が1増加しますのでご注意ください。

:::tip

1. 一部のエスケープシーケンスは使用時にアンエスケープが必要です。詳細は[unescape関数](#unescapestring-string---string)を参照してください。  
2. EMQX 5.0以降、複雑なデータ変換に[jq構文](https://stedolan.github.io/jq/manual/)もサポートしています。詳細は[jq関数](./rule-sql-jq.md)をご覧ください。

:::

## 数学関数

EMQXは幅広い数学関数をサポートしています：

- 三角関数および双曲線関数：sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh  
- 数値関数：abs, ceil, floor, round, sqrt, fmod  
- 指数関数および対数関数：exp, power, log, log10, log2  

### abs(X: integer) -> integer

整数 `X` の絶対値を返します。例：

```bash
abs(-12) = 12
```

:::tip
浮動小数点数の絶対値には `ceil` または `floor` 関数の利用を推奨します。
:::

### acos(X: integer | float) -> float

`X` のアークコサイン（ラジアン単位）を返します。`X` の範囲は `[-1, 1]` です。例：

```bash
acos(0.5) = 1.0471975511965976
```

### acosh(X: integer | float) -> float

`X` の双曲線アークコサイン（ラジアン単位）を返します。`X` は1以上でなければなりません。例：

```bash
acosh(1.5) = 0.9624236501192069
```

### asin(X: integer | float) -> float

`X` のアークサイン（ラジアン単位）を返します。`X` の範囲は `[-1, 1]` です。例：

```bash
asin(0.5) = 0.5235987755982988
```

### asinh(X: integer | float) -> float

`X` の双曲線アークサインを返します。例：

```bash
asinh(0.5) = 0.48121182505960347
```

### atan(X: integer | float) -> float

`X` のアークタンジェント（ラジアン単位）を返します。例：

```bash
atan(0.5) = 0.46364760900080615
```

### atanh(X: integer | float) -> float

`X` の双曲線アークタンジェントを返します。`X` の範囲は `(-1, 1)` です。例：

```bash
atanh(0.5) = 0.5493061443340549
```

### ceil(X: integer | float) -> integer

`X` 以上の最小の整数に切り上げます。例：

```bash
ceil(0.8) = 1
```

### cos(X: integer | float) -> float

角度 `X`（ラジアン単位）のコサインを返します。例：

```bash
cos(0.5) = 0.8775825618903728
```

### cosh(X: integer | float) -> float

`X` の双曲線コサインを返します。例：

```bash
cosh(0.5) = 1.1276259652063807
```

### exp(X: integer | float) -> float

自然対数の底 `e` の `X` 乗を返します。例：

```bash
exp(1) = 2.718281828459045
```

### floor(X: integer | float) -> integer

`X` 以下の最大の整数を返します。例：

```bash
floor(3.6) = 3
```

### fmod(X: integer | float, Y: integer | float) -> float

`X` を `Y` で割った余りを浮動小数点数で返します。例：

```bash
fmod(6.5, 2.5) = 1.5
```

### log(X: integer | float) -> float

`X` の自然対数を返します。`X` は0より大きい必要があります。例：

```bash
log(7.38905609893065) = 2.0
```

### log10(X: integer | float) -> float

`X` の常用対数（底10）を返します。`X` は0より大きい必要があります。例：

```bash
log10(100) = 2.0
```

### log2(X: integer | float) -> float

`X` の底2の対数を返します。`X` は0より大きい必要があります。例：

```bash
log2(8) = 3.0
log2(8.5) = 3.0874628412503395
```

### round(X: integer | float) -> integer

`X` を最も近い整数に丸めます。例：

```bash
round(4.5) = 5
```

### power(X: integer | float, Y: integer | float) -> float

`X` の `Y` 乗を返します。例：

```bash
power(2, 3) = 8.0
```

### random() -> float

`[0, 1)` の範囲のランダムな浮動小数点数を返します。例：

```bash
random() = 0.5400050092601868
```

### sin(X: integer | float) -> float

角度 `X`（ラジアン単位）のサインを返します。例：

```bash
sin(0.5) = 0.479425538604203
```

### sinh(X: integer | float) -> float

`X` の双曲線サインを返します。例：

```bash
sinh(0.5) = 0.5210953054937474
```

### sqrt(X: integer | float) -> float

`X` の平方根を返します。例：

```bash
sqrt(9) = 3.0
```

### tan(X: integer | float) -> float

角度 `X`（ラジアン単位）のタンジェントを返します。例：

```bash
tan(0.5) = 0.5463024898437905
```

### tanh(X: integer | float) -> float

`X` の双曲線タンジェントを返します。例：

```bash
tanh(0.5) = 0.46211715726000974
```

## データ型判定関数

指定したフィールドのデータ型を判定し、指定のデータ型に合致するかどうかを真偽値で返します。

### is_array(Term: any) -> boolean

`Term` が配列型か判定します。`any` はすべてのデータ型を意味します。例：

```bash
is_array([1, 2]) = true
is_array(json_decode('[{"value": 1}]')) = true
is_array(json_decode('{"value": 1}')) = false
is_array(0.5) = false
is_array('[1, 2]') = false
```

### is_bool(Term: any) -> boolean

`Term` がブール型か判定します。例：

```bash
is_bool(true) = true
is_bool(false) = false
is_bool('true') = false
```

### is_float(Term: any) -> boolean

`Term` が浮動小数点型か判定します。例：

```bash
is_float(123.4) = true
is_float(123) = false
```

### is_int(Term: any) -> boolean

`Term` が整数型か判定します。例：

```bash
is_int(123) = true
is_int(123.4) = false
```

### is_map(Term: any) -> boolean

`Term` がマップ型か判定します。例：

```bash
is_map(json_decode('{"value": 1}')) = true
is_map(json_decode('[{"value": 1}]')) = false
```

### is_null(Term: any) -> boolean

変数 `Term` が未定義か判定します。値がJSONの `null` であっても代入済みとみなされます。

例：

```sql
is_null(this_is_an_unassigned_variable) = true
is_null(map_get('b', json_decode('{"a": 1}'))) = true
is_null(map_get('b', json_decode('{"b": null}'))) = false
```

### is_null_var(Term: any) -> boolean

変数 `Term` が未定義または `null` か判定します。

例：

```sql
is_null_var(this_is_an_unassigned_variable) = true
is_null_var(map_get('b', json_decode('{"a": 1}'))) = true
is_null_var(map_get('b', json_decode('{"b": null}'))) = true
```

### is_not_null_var(Term: any) -> boolean

`is_null_var` の逆で、変数 `Term` が定義済みかつ `null` でないか判定します。

### is_num(Term: any) -> boolean

`Term` が整数または浮動小数点型か判定します。例：

```bash
is_num(123) = true
is_num(123.4) = true
is_num('123') = false
```

### is_str(Term: any) -> boolean

`Term` が文字列型か判定します。例：

```bash
is_str('123') = true
is_str(123) = false
```

### is_empty(Array or Map) -> boolean

配列またはマップが空か判定します。例：

```bash
is_empty(json_decode('{}')) = true
is_empty('{}') = true
is_empty('{"key" : 1}') = false
is_empty(map_get('key', '{"key" : []}')) = true
is_empty(map_get('key', '{"key" : [1}')) = false
```

## データ型変換関数

### bool(Term: boolean | integer | string) -> boolean

`Term` をブール型に変換します。`Term` はブール型、0または1の整数、または文字列の `true` または `false` のみ許容されます。

例：

```bash
# 正常
bool(true) = true
bool(0) = false
bool('false') = false

# エラー
bool(20)
bool('True')
```

### float(Term: float | integer | string) -> float

`Term` を浮動小数点数に変換します。

`Term` が文字列の場合、科学的記数法も使用可能です（例：`float('3.14e4')`）。浮動小数点数は最大16桁の有効数字をサポートします。文字列で表現される浮動小数点数の有効数字が16桁を超える場合、変換時に丸め誤差が生じる可能性があります。

例：

```bash
float(20) = 20.0

float('3.14') = 3.14
float('3.14e4') = 31400
float('3.14e+4') = 31400
float('3.14e-4') = 0.000314
float('3.14E-4') = 0.000314

# 有効数字が16桁を超えると丸め誤差により異なる入力が同じ出力になることがあります。
float('0.12345678901234566') = 0.12345678901234566
float('0.12345678901234567') = 0.12345678901234566
```

### float(Term: float | integer | string, Decimals: integer) -> float

`Term` を小数点以下最大 `Decimals` 桁の浮動小数点数に変換します。`Decimals` の範囲は `(0, 253]` です。その他の挙動は `float/1` と同じです。例：

```bash
float('3.1415926', 3) = 3.142
float('0.000012345', 5) = 0.00001
```

### float2str(Float: float, Decimals: integer) -> string

浮動小数点数 `Float` を小数点以下最大 `Decimals` 桁の文字列に変換し、末尾のゼロは切り捨てます。`Decimals` の範囲は `[0, 253]` です。`Float` の有効数字が16桁を超える場合、変換時に丸め誤差が生じる可能性があります。

浮動小数点数はコンピュータ上で正確に格納できないため、`Decimals` が `Float` の小数点以下桁数（先行ゼロ含む）より大きい場合、`float2str` は `Float` の2進近似の10進表現を返す場合があります。

例：

```bash
float2str(0.1, 5) = '0.1'
float2str(0.1, 20) = '0.10000000000000000555'
float2str(0.1, 25) = '0.1000000000000000055511151'
float2str(0.00000000001, 20) = '0.00000000001'

# 末尾のゼロは切り捨てられます
float2str(0.100001, 5) = '0.1'

# 有効数字が16桁を超えると丸め誤差により異なる入力が同じ出力になることがあります。
float2str(123456789.01234565, 8) = '123456789.01234566'
float2str(123456789.01234566, 8) = '123456789.01234566'
```

### int(Term: boolean | float | integer | string) -> integer

`Term` を整数に変換します。

- `Term` がブール型の場合、true は1、false は0に変換されます。  
- `Term` が浮動小数点型の場合、`Term` 以下の最大の整数に切り捨てられます。  
- `Term` が文字列の場合、少なくとも1つの数字を含み、先頭に `+` または `-` の1文字の接頭辞を持つことができ、先行ゼロは無視されます。数学的表記もサポートされます。  
- `Term` が整数の場合、そのまま返されます。

例：

```bash
# 正常
int(true) = 1
int(3.14) = 3
int(-3.14) = 4
int('-100') = -100
int('+200') = 200
int('0010') = 10
int('3.1415e2') = 314
int(substr('Number 100', 7)) = 100

# エラー
int('-100+200')
int('Number 100')
```

### str(Term: any) -> string

任意の型の `Term` を文字列に変換します。

`Term` がマップまたは配列の場合、`str` 関数は `Term` をJSONエンコードしようとします。

`Term` が浮動小数点数の場合、末尾のゼロを切り捨てた対応する文字列を返します。返される文字列は小数点以下最大10桁を保持します。より多くの小数桁を返すには `float2str` 関数を使用してください。

例：

```bash
str(100) = '100'
str(nth(1, json_decode('[false]'))) = 'false'
str(json_decode({"msg": "hello"})) = '{"msg":"hello"}'
str(json_decode('[{"msg": "hello"}]')) = '[{"msg":"hello"}]'

# 末尾のゼロは切り捨てられます
# 小数点以下最大10桁を保持
str(0.30000000040) = '0.3000000004'
str(0.30000000004) = '0.3'

# 小数点以下10桁で丸められます
# 10桁目以降で丸め
str(3.14159265359) = '3.1415926536'
str(0.000000314159265359) = '0.0000003142'
```

### str_utf8(Term: any) -> string

任意の `Term` をUTF-8エンコードされた文字列に変換します。

その他の挙動は `str(Any)` と同じです。

```bash
str_utf8(100) = '100'
str_utf8(nth(1, json_decode('[false]'))) = 'false'
str_utf8(json_decode({"msg": "hello"})) = '{"msg":"hello"}'
str_utf8(json_decode('[{"msg": "hello"}]')) = '[{"msg":"hello"}]'

# 末尾のゼロは切り捨てられます
# 小数点以下最大10桁を保持
str_utf8(0.30000000040) = '0.3000000004'
str_utf8(0.30000000004) = '0.3'

# 小数点以下10桁で丸められます
# 10桁目以降で丸め
str_utf8(3.14159265359) = '3.1415926536'
str_utf8(0.000000314159265359) = '0.0000003142'
```

### str_utf16_le(Term: any) -> binary

任意の `Term` をUTF-16リトルエンディアンエンコードされたバイナリ文字列に変換します。

::: tip

UTF-16リトルエンディアンエンコード文字列はJSONオブジェクト内で正しく表示されない場合があります。EMQXでは通常バイナリデータとして扱われます。可読な16進文字列に変換するには `bin2hexstr` 関数を使用してください。  
このエンコードはMicrosoft SQL Serverなど、リトルエンディアンUTF-16を使用するシステムで一般的に使われます。

:::

```bash
# Unicodeの `h`:
# |                          h(\u68)                              |
# | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 1 | 0 | 1 | 0 | 0 | 0 | (ビッグエンディアン)
# |              0x00             |              0x68             |
# | 0 | 1 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | (リトルエンディアン)
# |              0x68             |              0x00             |
str_utf16_le('h') = 'h\u0000'

bin2hexstr(str_utf16_le('hello')) = '680065006C006C006F00'
```

## 文字列操作関数

文字列の大文字・小文字変換、空白除去、部分文字列抽出、置換、エスケープ／アンエスケープなどに使用します。

### ascii(Char: string) -> integer

文字 `Char` のASCIIコードを返します。複数文字の場合は先頭文字のコードのみ返します。例：

```bash
ascii('a') = 97
ascii('abc') = 97
```

### concat(Str1: string, Str2: string) -> string

`Str1` と `Str2` を連結した文字列を返します。例：

```bash
concat('Name:', 'John') = 'Name:John'
```

### find(String: string, SearchPattern: string) -> string

`String` 内で部分文字列 `SearchPattern` を検索し、`SearchPattern` より前の内容を削除して残りを返します。見つからなければ空文字列を返します。これは `find(String, SearchPattern, 'leading')` と同等です。

例：

```bash
find('..., Value: 1.2', 'Value:') = 'Value: 1.2'
find('..., Value: 1.2', 'Data') = ''
```

### find(String: string, SearchPattern: string, Direction: string) -> string

`find/2` と同様ですが、検索方向を `Direction` で指定できます。例：

```bash
find('Front, Middle, End', ', ', 'leading') = ', Middle, End'
find('Front, Middle, End', ', ', 'trailing') = ', End'
```

### join_to_string(Sep: string, Array: array) -> string

配列 `Array` の要素を区切り文字 `Sep` で連結した文字列を返します。例：

```bash
join_to_string(', ', ['a', 'b', 'c']) = 'a, b, c'
```

### lower(String: string) -> string

文字列 `String` の大文字を小文字に変換します。例：

```bash
lower('Hello') = 'hello'
```

### ltrim(String: string) -> string

`trim/1` と同様ですが、先頭の空白文字のみ削除します。例：

```bash
ltrim('\t  hello  \n') = 'hello  \n'
ltrim('\t  hello \r\n') = 'hello  \r\n'
```

### pad(String: string, Length: integer) -> string

`String` の末尾にスペースを追加して指定長 `Length` にパディングします。例：

```bash
pad('hello', 8) = 'hello   '
```

### pad(String: string, Length: integer, Direction: string) -> string

`pad/2` と同様ですが、`Direction` でパディング方向を指定できます。`leading` は前方、`trailing` は後方、`both` は両端にスペースを埋めます。

`both` 指定時、埋めるスペース数が奇数の場合は最後のスペースが末尾に追加されます。

例：

```bash
pad('hello', 8, 'leading') = '   hello'
pad('hello', 8, 'trailing') = 'hello   '
pad('hello', 8, 'both') = ' hello  '
```

### pad(String: string, Length: integer, Direction: string, Char: string) -> string

`pad/3` と同様ですが、指定したグラフェムクラスタ `Char` でパディングします。

ルールエンジンは `Char` が合法なグラフェムクラスタかをチェックしないため、`Char` の文字数に関わらず1文字として扱われます。例：

```bash
pad('hello', 8, 'trailing', '!') = 'hello!!!'
pad('hello', 8, 'trailing', '\r\n') = 'hello\r\n\r\n\r\n'
pad('hello', 8, 'trailing', 'abc') = 'helloabcabcabc'
```

### regex_match(String: string, Expression: string) -> boolean

文字列 `String` が正規表現 `Expression` にマッチするか判定します。例：

```bash
regex_match('123', '^\d+$') = true
regex_match('a23', '^\d+$') = false
```

### regex_replace(String: string, Expression: string, Replacement: string) -> string

正規表現 `Expression` にマッチした部分を文字列 `Replacement` に置換します。マッチしなければ元の文字列を返します。例：

```bash
regex_replace('hello 123', '\d+', 'world') = 'hello world'
regex_replace('a;b; c', ';\s*', ',') = 'a,b,c'
```

### regex_extract(String: string, Expression: string) -> [string]

::: tip

EMQX v5.7.1以降で導入された関数です。

:::

正規表現のキャプチャグループを用いて文字列から部分抽出します。完全一致部分は除外されます。

マッチがあればキャプチャグループのリストを返し、なければ空リストを返します。

例：

```bash
regex_extract('Number: 12345', '(\d+)') -> ['12345']
regex_extract('Hello, world!', '(\w+).*\s(\w+)') -> ['Hello', 'world']
regex_extract('No numbers here!', '(\d+)') -> []
regex_extract('Date: 2021-05-20', '(\d{4})-(\d{2})-(\d{2})') -> ['2021', '05', '20']
```

### replace(String: string, SearchPattern: string, Replacement: string) -> string

`String` 内のすべての `SearchPattern` を `Replacement` に置換します。例：

```bash
replace('ab..cd..ef', '..', '**') = 'ab**cd**ef'
replace('ab..cd..ef', '..', '') = 'abcdef'
```

### replace(String: string, SearchPattern: string, Replacement: string, Where: string) -> string

`String` 内の `SearchPattern` を `Replacement` に置換します。

`Where` の値は以下の通りです：

- `all`: すべて置換（`replace/3` と同等）  
- `leading`: 先頭のみ置換  
- `trailing`: 末尾のみ置換  

例：

```bash
replace('ab..cd..ef', '..', '**', 'all') = 'ab**cd**ef'
replace('ab..cd..ef', '..', '**', 'leading') = 'ab**cd..ef'
replace('ab..cd..ef', '..', '**', 'trailing') = 'ab..cd**ef'
```

### reverse(String: string) -> string

文字列を逆順にします。例：

```bash
reverse('hello') = 'olleh'
```

### rm_prefix(String: string, Prefix: string) -> string

`String` の先頭にある `Prefix` を削除します。`Prefix` で始まらなければ元の文字列を返します。例：

```bash
rm_prefix('foo/bar', 'foo/') = 'bar'
rm_prefix('foo/bar', 'xxx/') = 'foo/bar'
```

### rtrim(String: string) -> string

`trim/1` と同様ですが、末尾の空白文字のみ削除します。例：

```bash
rtrim('\t  hello  \n') = '\t  hello'
rtrim('\t  hello \r\n') = '\t  hello'
```

### split(String: string, Separator: string) -> array

`String` を区切り文字 `Separator` で分割し、部分文字列の配列を返します。

連続する複数の区切り文字は1つとして扱われず、空文字列が結果に含まれる場合があります。`split/2` はデフォルトで結果をトリムし、空文字列を除外します。空文字列を残したい場合は `split(String, Separator, 'notrim')` を使用してください。

区切り文字は複数文字でも構い、全体で1つの区切り文字として扱われます。複数の区切り文字を同時に指定したい場合は `tokens` 関数を使用してください。

例：

```bash
split('a;', ';') = ['a']
split('a;b;c', ';') = ['a', 'b', 'c']
split('a;;b;;c', ';') = ['a', 'b', 'c']

# Howell Wise の前の空白に注意
split('Sienna Blake; Howell Wise', ';') = ['Sienna Blake', ' Howell Wise']
split('Sienna Blake; Howell Wise', '; ') = ['Sienna Blake', 'Howell Wise']
```

### split(String: string, Separator: string, Option: string) -> array

`split/2` と同様ですが、`Option` で区切り文字の処理位置や空文字列の返却有無を指定できます。

`Option` の値は以下の通りです：

- `notrim`: 文字列中のすべての区切り文字を処理し、空文字列を含む結果を返す  
- `leading`: 先頭の区切り文字のみ処理し、空文字列は含まない  
- `leading_notrim`: 先頭の区切り文字のみ処理し、空文字列を含む可能性あり  
- `trailing`: 末尾の区切り文字のみ処理し、空文字列は含まない  
- `trailing_notrim`: 末尾の区切り文字のみ処理し、空文字列を含む可能性あり  

例：

```bash
split('a;;b;;c', ';', 'notrim') = ['a', '', 'b', '', 'c']
split('a;b;c', ';', 'leading') = ['a', 'b;c']
split('a;b;c', ';', 'trailing') = ['a;b', 'c']
split(';a;b;c', ';', 'leading_notrim') = ['', 'a;b;c']
split('a;b;c;', ';', 'trailing_notrim') = ['a;b;c', '']
```

### sprintf(Format, ...) -> string

`Format` に従いフォーマットされた文字列を返します。`Format` は通常文字と制御シーケンスを含みます。

制御シーケンスの形式は一般に `~F.P.PadModC` です。

`C` は制御シーケンスの種類を示し必須です。`F`, `P`, `Pad`, `Mod` は任意です。詳細は https://www.erlang.org/doc/apps/stdlib/io.html#fwrite-1 を参照してください。

例：

```bash
sprintf('hello, ~s!', 'steve') = 'hello, steve!'
sprintf('count: ~p~n', 100) = 'count: 100\n'
```

### strlen(String: string) -> integer

文字列 `String` の長さを返します。例：

```bash
strlen('hello') = 5
strlen('hello\n') = 6
```

### substr(String: string, Start: integer) -> string

`String` の位置 `Start` から末尾までの部分文字列を返します。文字列の添字は0始まりです。例：

```bash
substr('hello', 0) = 'hello'
substr('hello world', 6) = 'world'
```

### substr(String: string, Start: integer, Length: integer) -> string

`String` の位置 `Start` から最大長 `Length` の部分文字列を返します。添字は0始まりです。例：

```bash
substr('hello world!', 6, 5) = 'world'
```

### tokens(String: string, SeparatorList: string) -> array

`String` を `SeparatorList` に含まれる文字で分割し、部分文字列のリストを返します。

連続する区切り文字は1つとして扱われ、空文字列は発生しません。

例：

```bash
tokens('a,b;c,d', ',;') = ['a', 'b', 'c', 'd']
tokens('a;;b', ';') = ['a', 'b']
```

### tokens(String: string, SeparatorList:string, NoCRLF: string) -> array

`tokens/2` と同様ですが、`NoCRLF` に `nocrlf` を指定すると改行コードも区切り文字として扱います。例：

```bash
tokens('a\rb\nc\r\nd', ';', 'nocrlf') = ['a', 'b', 'c', 'd']
```

### trim(String: string) -> string

`String` の先頭と末尾から空白文字（スペース、タブ、フォームフィード、改行など）を削除します。`\r\n` はUnicodeのグラフェムクラスタとして一括削除されます。例：

```bash
trim('\t  hello  \n') = 'hello'
trim('\t  hello \r\n') = 'hello'
```

### unescape(String: string) -> string

エスケープシーケンスを元の文字に戻します。SQLでエスケープシーケンスを使用する際は、適切に処理するためにこの関数でアンエスケープしてください。

::: tip

EMQX v5.7.0以降で導入された関数です。

:::

例えば、ペイロードが改行区切りの文字列の場合：

```bash
32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3
87.2
12.3
my-device
```

`\n` で分割したい場合、以下のSQLは期待通りに動作しません：

```sql
SELECT split(payload, '\n') as device_info FROM 't/#'
```

出力結果：

```json
{
  "device_info": [
    "32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3\n87.2\n12.3\nmy-device"
  ]
}
```

`unescape` 関数でアンエスケープすると期待通りの結果が得られます：

```sql
SELECT split(payload, unescape('\n')) as device_info FROM 't/#'
```

出力結果：

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

**unescape関数は以下のエスケープシーケンスをサポートします：**

- 標準Cエスケープシーケンス：

  - `\n`：改行（LF）  
  - `\t`：水平タブ（HT）  
  - `\r`：復帰（CR）  
  - `\b`：バックスペース（BS）  
  - `\f`：フォームフィード（FF）  
  - `\v`：垂直タブ（VT）  
  - `\'`：シングルクォート（'）  
  - `\"`：ダブルクォート（"）  
  - `\\`：バックスラッシュ（\）  
  - `\?`：クエスチョンマーク（?）  
  - `\a`：アラート（ベル、BEL）  

- 16進エスケープコード：

  - `\xH...`：`H...` は1つ以上の16進数字（0-9, A-F, a-f）で、任意のUTF-32文字をエンコード可能。

認識できないエスケープシーケンスや無効なUnicode文字の場合、例外が発生します。

### upper(String: string) -> string

`String` の小文字を大文字に変換します。例：

```bash
upper('hello') = 'Hello'
```

## マップ操作関数

### map_get(Key: string, Map: map) -> any

`Map` の指定した `Key` の値を返します。`Key` が存在しなければ `undefined` を返します。例：

```bash
map_get('msg', json_decode('{"msg": "hello"}')) = 'hello'
map_get('data', json_decode('{"msg": "hello"}')) = undefined
```

### map_get(Key: string, Map: map, Default: any) -> any

`map_get/2` と同様ですが、`Key` が存在しない場合は指定した `Default` を返します。例：

```bash
map_get('data', json_decode('{"msg": "hello"}'), '') = ''
map_get('value', json_decode('{"data": [1.2, 1.3]}'), []) = []
```

### map_keys(Map: map) -> array

`Map` のすべてのキーを配列で返します。例：

```bash
map_keys(json_decode('{"a": 1, "b": 2}')) = ['a', 'b']
```

### maptab_lookup(Table: string, Key: string | integer) -> map | undefined

::: tip

EMQX 6.1.5以降で利用可能です。EMQX Mapping Tablesプラグインがインストール・起動されている必要があります。

:::

[EMQX Mapping Tablesプラグイン](../extensions/plugin-catalog/6.2/emqx-maptabs.md)で管理されるマッピングテーブルの行を検索します。テーブルやキーが存在しない、またはキーの型が一致しない場合は `undefined` を返します。

キーの比較は型変換なしの厳密な等価比較です。例えば整数キー `50` と文字列キー `'50'` は異なるキーです。

例：

```bash
maptab_lookup('signals', 1) = json_decode('{"signal_name":"temperature_c","start_bit":17,"length":8,"type":"integer","signedness":"signed","endian":"big"}')
maptab_lookup('signals', 3) = undefined
```

### maptab_lookup(Table: string, Key: string | integer, DefaultRow: map) -> map

`maptab_lookup/2` と同様ですが、検索失敗時に指定した `DefaultRow` を返します。`DefaultRow` は `map_new()`, `map_put(...)`, `json_decode('{...}')` で作成します。

返された行を他関数の入力に使う場合は、その関数が必要とするすべてのフィールドを含めてください。空のデフォルト行（例：`map_new()`）はフィールドがすべて `undefined` のままです。

例：

```bash
maptab_lookup('signals', 3, json_decode('{"signal_name":"Unknown","start_bit":17,"length":48,"type":"bits","signedness":"unsigned","endian":"big"}')) = json_decode('{"signal_name":"Unknown","start_bit":17,"length":48,"type":"bits","signedness":"unsigned","endian":"big"}')
```

### maptab_lookup(Table: string, Key: string | integer, Field: string) -> any | undefined

一致した行の指定フィールドを検索します。テーブル、キー、フィールドが存在しない、またはキーの型が一致しない場合は `undefined` を返します。

例：

```bash
maptab_lookup('signals', 1, 'signal_name') = 'temperature_c'
maptab_lookup('signals', 3, 'signal_name') = undefined
```

### maptab_lookup(Table: string, Key: string | integer, Field: string, Default: any) -> any

`maptab_lookup(Table, Key, Field)` と同様ですが、検索失敗時に指定した `Default` を返します。

例：

```bash
maptab_lookup('signals', 3, 'signal_name', 'Unknown') = 'Unknown'
```

`FOREACH` 内で `maptab_lookup` を使う場合、欠損する可能性のある行のフィールドアクセスはガードしてください。例えば、`maptab_lookup('signals', item_id)` が `undefined` を返すと、`sig.start_bit` を `subbits` に渡すとメッセージのSQL実行全体が失敗します。`CASE WHEN is_map(sig)` を使うか、完全なデフォルト行を用意してください。

### map_put(Key: string, Value: any, Map: map) -> map

`Map` に `Key` と対応する `Value` を挿入し、更新されたマップを返します。既存の `Key` があれば値を上書きします。例：

```bash
map_get('b', map_put('b', 1, json_decode('{"a": 1}'))) = 1
map_get('a', map_put('a', 2, json_decode('{"a": 1}'))) = 2
```

### map_to_redis_hset_args(Map) -> list

::: tip

EMQX v5.7.1以降で導入された関数です。

:::

マップをRedisの `HSET`（または `HMSET`）コマンド用のフィールド名と値のリストに変換します。

例：`SELECT map_to_redis_hset_args(payload.value) as hset_fields FROM t/1` のように使用し、Redisアクションのテンプレートで `HMSET name1 ${hset_fields}` のように利用します。

例えば、`payload.value` が `{"a" : 1, "b": 2}` のマップなら、結果は `HMSET name1 b 2 a 1` のようになります。マップのフィールド順序は非決定的です。

### map_to_entries(Map: map) -> array

`Map` を `key` と `value` フィールドを持つオブジェクトの配列に変換します。例：

```bash
map_to_entries(json_decode('{"a": 1, "b": 2}')) = [{"key": "a", "value": 1},{"key": "b", "value": 2}]
```

### map_values(Map: map) -> array

`Map` のすべての値を配列で返します。例：

```bash
map_values(json_decode('{"a": 1, "b": 2}')) = [1, 2]
```

### mget(Key: string | array, Map: map) -> any

`Map` の指定した `Key` の値を返します。`Key` が存在しなければ `undefined` を返します。配列で複数キーを指定するとネストされたマップから値を取得できます。例：

```bash
mget('c', json_decode('{"a": {"b": 1}}')) = undefined
json_decode(mget('a', json_decode('{"a": {"b": 1}}'))) = '{"b": 1}'
mget(['a', 'b'], json_decode('{"a": {"b": 1}}')) = 1
```

### mput(Key: string | array, Value: any, Map: map) -> map

`Map` に `Key` と対応する `Value` を挿入し、更新されたマップを返します。既存の `Key` があれば値を上書きします。配列で複数キーを指定するとネストされたマップにデータを挿入できます。例：

```bash
mget(['a', 'b'], mput(['a', 'b'], 2, json_decode('{"a": {"b": 1}}'))) = 2
mget(['a', 'b'], mput(['a', 'b'], 2, json_decode('{"c": 1}'))) = 2
```

### map_size(Map: map) -> any

`Map` のキー数を返します。例：

```bash
map_size(json_decode('{}')) = 0
map_size(json_decode('{"msg": "hello"}')) = 1
```

## 配列操作関数

### contains(Item: any, Array: array) -> boolean

配列 `Array` に指定した `Item` が含まれるか判定します。例：

```bash
contains(2, [1, 2, 3]) = true
contains(2.3, [1.8, 2.5, 2.0]) = false
contains('John', ['John', 'David']) = true
contains([1, 2], [a, b, [1, 2]]) = true
contains(json_decode('{"a": 1}'), [json_decode('{"a": 1}'), json_decode('{"b": 2}')]) = true
```

### first(Array: array) -> any

配列 `Array` の最初の要素を返します。`Array` は空であってはなりません。例：

```bash
# 正常
first(['John', 'David']) = 'John'

# エラー
first([])
```

### last(Array: array) -> any

配列 `Array` の最後の要素を返します。`Array` は空であってはなりません。例：

```bash
# 正常
last(['John', 'David']) = 'David'

# エラー
last([])
```

### length(Array: array) -> integer

配列 `Array` の長さ（要素数）を返します。例：

```bash
length([1,2,3,4]) = 4
length([]) = 0
```

### nth(N: integer, Array: array) -> any

配列 `Array` のN番目の要素を返します。`N` は配列長以下でなければなりません。例：

```bash
# 正常
nth(1, [1,2,3]) = 1

# エラー
nth(0, [1,2,3])
nth(4, [1,2,3])
```

### sublist(Length: integer, Array: array) -> any

配列 `Array` の先頭から最大長 `Length` の部分配列を返します。`Length` が配列長を超える場合は全体を返します。例：

```bash
sublist(3, [1,2,3,4]) = [1,2,3]
sublist(10, [1,2,3,4]) = [1,2,3,4]
```

### sublist(Start: integer, Length: integer, Array:array) -> any

`sublist/2` と同様ですが、`Start` で開始位置を指定できます。`Start` + `Length` が配列長を超える場合は全体を返します。例：

```bash
sublist(2, 10, [1,2,3,4]) = [2,3,4]
```

## ハッシュ関数

### md5(String: string) -> string

任意長の文字列 `String` の128ビット固定長MD5ハッシュ値を計算します。結果は32桁の16進数文字列で、小文字(a〜f)固定です。

例：

```bash
md5('hello') = '5d41402abc4b2a76b9719d911017c592'
```

### sha(String: string) -> string

任意長の文字列 `String` の160ビット固定長SHA-1ハッシュ値を計算します。結果は40桁の16進数文字列で、小文字(a〜f)固定です。

例：

```bash
sha('hello') = 'aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d'
```

### sha256(String: string) -> string

任意長の文字列 `String` の256ビット固定長SHA-2ハッシュ値を計算します。結果は64桁の16進数文字列で、小文字(a〜f)固定です。

例：

```bash
sha256('hello') = '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824'
```

### hash_to_range(Value: string, Min: integer, Max: integer) -> integer

EMQX 6.2.3以降で導入。

`Value` をSHA-256でハッシュ化し、ハッシュ値を `[Min, Max]` の範囲の整数にマッピングします。`Min` は `Max` 以下でなければなりません。

メッセージのフィールドから安定したバケットやシャード番号を得たい場合に便利です。例えばトピックのセグメントでデバイスを複数ルールやアクションに分散する場合など。

例：

```bash
hash_to_range('A_C001', 0, 3) = 0
hash_to_range(nth(2, tokens(topic, '/')), 0, 3)
```

### map_to_range(Value: integer | string, Min: integer, Max: integer) -> integer

EMQX 6.2.3以降で導入。

`Value` を `[Min, Max]` の範囲の整数にマッピングします。`Min` は `Max` 以下でなければなりません。

`Value` が整数の場合は直接マッピングし、非空文字列の場合はバイナリ表現を符号なし整数に変換してからマッピングします。

例：

```bash
map_to_range(7, 0, 3) = 3
map_to_range('a', 0, 3) = 1
```

## 圧縮・解凍関数

注：バイナリデータはJSONエンコードできないため、16進文字列に変換するには `bin2hexstr` 関数を使用してください。

### gunzip(Data: binary) -> binary | string

`Data` を解凍します。`Data` はgzヘッダーと末尾のチェックサムを含む必要があります。例：

```bash
gunzip(hexstr2bin('1F8B0800000000000013CB48CDC9C9070086A6103605000000')) = 'hello'
```

### gzip(Data: binary | string) -> binary

DEFLATEアルゴリズムで `Data` を圧縮し、gzヘッダーと末尾のチェックサムを含む圧縮結果を返します。例：

```bash
bin2hexstr(gzip('hello')) = '1F8B0800000000000013CB48CDC9C9070086A6103605000000'
```

### unzip(Data: binary) -> binary | string

`Data` を解凍します。`Data` はzlibヘッダーと末尾のチェックサムを含まない必要があります。例：

```bash
unzip(hexstr2bin('CB48CDC9C90700')) = 'hello'
```

### zip(Data: binary | string) -> binary

DEFLATEアルゴリズムで `Data` を圧縮し、zlibヘッダーと末尾のチェックサムを含まない圧縮結果を返します。例：

```bash
bin2hexstr(zip('hello')) = 'CB48CDC9C90700'
```

### zip_compress(Data: binary | string) -> binary

DEFLATEアルゴリズムで `Data` を圧縮します。返される圧縮結果はzlibヘッダーと末尾のチェックサムを含みます。例：

```bash
bin2hexstr(zip_compress('hello')) = '789CCB48CDC9C90700062C0215'
```

### zip_uncompress(Data: binary) -> binary | string

`Data` を解凍します。`Data` はzlibヘッダーと末尾のチェックサムを含む必要があります。例：

```bash
zip_uncompress(hexstr2bin('789CCB48CDC9C90700062C0215')) = 'hello'
```

### lz4_compress(Data: binary | string) -> binary

EMQX 6.2.3以降で導入。

LZ4フレーム形式で `Data` を圧縮します。例：

```bash
lz4_uncompress(lz4_compress('hello')) = 'hello'
```

### lz4_uncompress(Data: binary) -> binary | string

EMQX 6.2.3以降で導入。

LZ4フレーム形式の `Data` を解凍します。例：

```bash
lz4_uncompress(lz4_compress('hello')) = 'hello'
```

## ビット操作関数

### bitand(Num1: integer, Num2: integer) -> integer

`Num1` と `Num2` のビットごとのANDを返します。入力・出力は符号付き整数です。例：

```bash
bitand(10, 8) = 8
bitand(-10, -8) = -16
```

### bitnot(Num: integer) -> integer

`Num` のビットごとの否定を返します。入力・出力は符号付き整数です。例：

```bash
bitnot(10) = -11
bitnot(-12) = 11
```

### bitsl(Num: integer, Shift: integer) -> integer

`Num` を左に `Shift` ビットシフトし、右側は0で埋めます。例：

```bash
bitsl(8, 2) = 32
bitsl(-8, 2) = -32
```

### bitsr(Num: integer, Shift: integer) -> integer

`Num` を右に `Shift` ビットシフトし、左側は符号ビットで埋めます（正数は0、負数は1）。例：

```bash
bitsr(8, 2) = 2
bitsr(8, 4) = 0
bitsr(-8, 2) = -2
bitsr(-8, 6) = -1
```

### bitor(Num1: integer, Num2: integer) -> integer

`Num1` と `Num2` のビットごとのORを返します。例：

```bash
bitor(10, 8) = 10
bitor(-10, -8) = -2
```

### bitxor(Num1: integer, Num2: integer) -> integer

`Num1` と `Num2` のビットごとのXORを返します。例：

```bash
bitxor(10, 8) = 2
bitxor(-10, -8) = 14
```

## ビット列操作関数

ルールエンジンはビット列操作関数を提供しています。例えば `subbits` はビット列から指定長のビットを抽出し、指定データ型に変換します。

:::tip

`binary` 型はバイト列を表し、1バイトは8ビットで、ビット数は8の倍数でなければなりません。`bitstring` 型は任意長のビット列を表します。

簡単に言うと、すべての `binary` は `bitstring` ですが、逆は必ずしも真ではありません。

`bitstring` は長さが8の倍数でない場合、JSONなど外部形式に直接シリアライズできません。通常は整数など適切な型に変換する前の中間値として使います。

:::

### bitsize(Bin: binary) -> integer

ビット列 `Bin` のビット数を返します。例：

```bash
bitsize('abc') = 24
bitsize('你好') = 48
```

### byteszie(Bin: binary) -> integer

バイト列 `Bin` のバイト数を返します。例：

```bash
byteszie('abc') = 3
byteszie('你好') = 6
```

### subbits(Bin: binary, BitNum: integer) -> integer

バイト列 `Bin` の先頭から長さ `BitNum` のビットを取得し、ビッグエンディアンの符号なし整数に変換します。`subbits(Bytes, 1, BitNum, 'integer', 'unsigned', 'big')` と同等です。

例：

```bash
# 159 = 0x9F
subbits(hexstr2bin('9F4E58'), 8) = 159

# 40782 = 0x9F4E
subbits(hexstr2bin('9F4E58'), 16) = 40782

# bin2hexstr(base64_decode('n05Y')) = '9F4E58'
subbits(base64_decode('n05Y'), 8) = 159
```

### subbits(Bin: binary, Start: integer, BitNum: integer) -> integer

バイト列 `Bin` の位置 `Start`（1始まり）から長さ `BitNum` のビットを取得し、ビッグエンディアンの符号なし整数に変換します。`subbits(Bytes, Start, BitNum, 'integer', 'unsigned', 'big')` と同等です。

例：

```bash
# 159 = 0x9F
subbits(hexstr2bin('9F4E58'), 1, 8) = 159

# 78 = 0x4E
subbits(hexstr2bin('9F4E58'), 9, 8) = 78

# bin2hexstr(base64_decode('n05Y')) = '9F4E58'
subbits(base64_decode('n05Y'), 9, 4) = 4
```

### subbits(Bin: binary, Start: integer, BitNum: integer, OutputType: string, Signedness: string, Endianness: string) -> bitstring | integer | float

バイト列 `Bin` の位置 `Start`（1始まり）から長さ `BitNum` のビットを取得し、指定のバイト順 `Endianness` と符号属性 `Signedness` に従い、指定型 `OutputType` に変換します。

- `OutputType` の値：  
  - `bits`（bitstringの略）  
  - `integer`  
  - `float`  
- `Signedness` の値：  
  - `signed`  
  - `unsigned`  
- `Endianness` の値：  
  - `big`  
  - `little`  

`OutputType` が `float` の場合、`Signedness` は無効です。`OutputType` が `bits` の場合、`Signedness` と `Endianness` は無効です。

例：

```bash
# 40782 = 0x9F4E
subbits(hexstr2bin('9F4E58'), 1, 16, 'integer', 'unsigned', 'big') = 40782
subbits(hexstr2bin('9F4E58'), 1, 16, 'integer', 'signed', 'big') = -24754

# 20127 = 0x4E9F
subbits(hexstr2bin('9F4E58'), 1, 16, 'integer', 'unsigned', 'little') = 20127

subbits(hexstr2bin('9F4E58'), 1, 16, 'float', 'unsigned', 'big') = -0.00713348388671875
subbits(hexstr2bin('9F4E58'), 1, 16, 'float', 'signed', 'big') = -0.00713348388671875
```

## エンコード・デコード関数

### base64_decode(Data: string) -> bytes | string

`Data` をbase64形式からデコードします。例：

```bash
base64_decode('aGVsbG8=') = 'hello'
bin2hexstr(base64_decode('y0jN')) = 'CB48CD'
```

### base64_decode(Data: string, Option1: string, ...) -> bytes | string

::: tip

EMQX 6.0.2以降でオプション付きで導入された関数です。

:::

オプション付きでbase64デコードを行います。

**オプション：**

- `no_padding`：パディング文字（`=`）なしでデコード  
- `urlsafe`：URLセーフbase64（`-` と `_` を使用）でデコード  

複数オプションの組み合わせも可能で、順序は結果に影響しません。

例：

```sql
-- URLセーフbase64をデコード
SELECT base64_decode(payload, 'urlsafe') as decoded FROM "t/#"

-- パディングなしURLセーフbase64をデコード
SELECT base64_decode(payload, 'urlsafe', 'no_padding') as decoded FROM "t/#"
```

### base64_encode(Data: binary | string) -> string

`Data` をbase64形式にエンコードします。例：

```bash
base64_encode('hello') = 'aGVsbG8='
base64_encode(hexstr2bin('CB48CD')) = 'y0jN'
```

### base64_encode(Data: binary | string, Option1: string, ...) -> string

::: tip

EMQX 6.0.2以降でオプション付きで導入された関数です。

:::

オプション付きでbase64エンコードを行います。

**オプション：**

- `no_padding`：パディング文字（`=`）なしでエンコード  
- `urlsafe`：URLセーフbase64（`-` と `_` を使用）でエンコード  

複数オプションの組み合わせも可能で、順序は結果に影響しません。

例：

```sql
-- パディングなしでエンコード
SELECT base64_encode(payload, 'no_padding') as encoded FROM "t/#"

-- URLセーフ文字でエンコード
SELECT base64_encode(payload, 'urlsafe') as encoded FROM "t/#"

-- パディングなしかつURLセーフでエンコード
SELECT base64_encode(payload, 'no_padding', 'urlsafe') as encoded FROM "t/#"
```

### json_decode(Data: string) -> array | map

`Data` をJSON形式からデコードします。例：

```bash
map_get('a', json_decode('{"a": 1}')) = 1
```

### json_encode(Data: array | map) -> string

`Data` をJSON形式にエンコードします。例：

```bash
json_encode([1,2,3]) = '[1,2,3]'
```

### bin2hexstr(Data: binary) -> string

バイナリデータを16進文字列に変換します。例：

```bash
bin2hexstr(zip('hello')) = 'CB48CDC9C90700'
```

### hexstr2bin(Data: string) -> binary

16進文字列をバイナリデータに変換します。例：

```bash
unzip(hexstr2bin('CB48CDC9C90700')) = 'hello'
```

### sqlserver_bin2hexstr(Data: binary | string) -> string

任意のバイナリデータをMicrosoft SQL Serverのバイナリ型に変換します。`0x` プレフィックス付きのHEXエンコード文字列となります。

::: tip

Microsoft SQL Serverの `CONVERT` 関数と組み合わせて、UTF-8非対応のSQL ServerにUTF-16リトルエンディアンエンコードのUnicode文字列を書き込む際に利用できます。

:::

```
sqlserver_bin2hexstr('hello') = '0x68656C6C6F'
sqlserver_bin2hexstr(str_utf16_le('hello')) = '0x680065006C006C006F00'
sqlserver_bin2hexstr(str_utf16_le('你好')) = '0x604F7D59'
```

### スキーマレジストリ関数

EMQXは `schema_encode` と `schema_decode` 関数を使い、指定したスキーマに従って[Protobuf](https://developers.google.com/protocol-buffers)や[Avro](https://avro.apache.org/)データのエンコード・デコードをサポートしています。詳細は[スキーマレジストリ](./schema-registry.md)を参照してください。

### schema_encode(SchemaID: string, Data: map) -> binary

指定したAvroスキーマで `Data` をエンコードします。スキーマレジストリでIDを取得してください。

### schema_encode(SchemaID: string, Data: map, MsgType: string) -> binary

指定したProtobufスキーマで `Data` をエンコードします。スキーマレジストリでIDを取得してください。`MsgType` はProtobufスキーマ内のメッセージタイプを指定します。

### schema_decode(SchemaID: string, Bin: binary) -> map

指定したAvroスキーマで `Bin` をデコードします。スキーマレジストリでIDを取得してください。

### schema_decode(SchemaID: string, Bin: binary, MsgType: string) -> map

指定したProtobufスキーマで `Bin` をデコードします。スキーマレジストリでIDを取得してください。`MsgType` はProtobufスキーマ内のメッセージタイプを指定します。

### **Sparkplug B関数**

EMQXはSparkplug Bメッセージのデコード・エンコード用に特別な関数 `spb_decode` と `spb_encode` も備えています。詳細は[Sparkplug B](./sparkplug.md)を参照してください。

## 日時変換関数

### date_to_unix_ts(Unit: string, FormatString: string, DateTimeString: string) -> integer

日時文字列 `DateTimeString` をフォーマット文字列 `FormatString` に従って解析し、Unix時間に変換します。時間単位は `Unit` で指定します。

利用可能な単位は `second`, `millisecond`, `microsecond`, `nanosecond` です。

`FormatString` で使えるプレースホルダーは以下の通りです：

| プレースホルダー | 意味 | 値の範囲 |
| ----------- | ------- | ------------|
| `%Y` | 4桁の年 | 0000 - 9999 |
| `%m` | 2桁の月 | 01 - 12 |
| `%d` | 2桁の日 | 01 - 31 |
| `%H` | 2桁の24時間制時 | 00 - 24 |
| `%M` | 2桁の分 | 00 - 59 |
| `%S` | 2桁の秒 | 00 - 59 |
| `%N` | ナノ秒 | 000000000 - 999999999 |
| `%6N` | マイクロ秒（ナノ秒の上位6桁） | 000000 - 999999 |
| `%3N` | ミリ秒（ナノ秒の上位3桁） | 000 - 999 |
| `%z` | タイムゾーンオフセット（±hhmm） | -1159 - +1159 |
| `%:z` | タイムゾーンオフセット（±hh:mm） | -11:59 - +11:59 |
| `%::z` | タイムゾーンオフセット（±hh:mm:ss） | -11:59:59 - +11:59:59 |

例：

```bash
date_to_unix_ts('second', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00+08:00') = 1708671600
```

### date_to_unix_ts(Unit: string, Offset: string | integer, FormatString: string, DateTimeString: string) -> integer

`DateTimeString` にタイムゾーンオフセットが含まれない場合、`Offset` で手動指定できます。その他は `date_to_unix_ts/3` と同様です。`Offset` は文字列または秒数の整数で指定可能です。

文字列の形式は以下の通りです：

- `Z` または `z`：UTCオフセット00:00  
- `±hh[:mm][:ss]` または `±hh[mm][ss]`：UTCからの正負の時刻オフセット  
- `local`：システムのローカルタイムゾーンのオフセット  

例：

```bash
date_to_unix_ts('second', '+08:00', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00') = 1708671600
date_to_unix_ts('second', 'Z', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 07:00:00') = 1708671600
date_to_unix_ts('second', 14400, '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00') = 1708686000
```

### format_date(Unit: string, Offset: string | integer, FormatString: string, Time: Integer) -> string

Unix時間 `Time` を指定フォーマットの日時文字列に変換します。`Unit` はUnix時間の単位、`Offset` は出力日時のタイムゾーンオフセット、`FormatString` は出力フォーマットです。

`date_to_unix_ts/3,4` を参照してください。

例：

```bash
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%z', 1708933353472) = '2024-02-26 15:42:33.472000+0800'
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%:z', 1708933353472) = '2024-02-26 15:42:33.472000+08:00'
format_date('millisecond', '+08:20:30', '%Y-%m-%d %H:%M:%S.%3N%::z', 1708933353472) = '2024-02-26 16:03:03.472+08:20:30'
format_date('millisecond', 'Z', '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 07:42:33.472+08:00'
format_date('millisecond', 28800, '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 15:42:33.472+08:00'
```

### now_rfc3339() -> string

現在時刻をRFC3339形式の秒単位日時文字列で返します。例：

```bash
now_rfc3339() = '2024-02-23T10:26:20+08:00'
```

### now_rfc3339(Unit: string) -> string

`now_rfc3339/0` と同様ですが、`Unit` で時間単位を指定できます。`second`, `millisecond`, `microsecond`, `nanosecond` をサポート。例：

```bash
now_rfc3339('microsecond') = '2024-02-23T10:26:38.009706+08:00'
```

### now_timestamp() -> integer

現在時刻をUnixタイムスタンプ（秒単位）で返します。例：

```bash
now_timestamp() = 1708913853
```

### now_timestamp(Unit: string) -> integer

`now_timestamp/0` と同様ですが、`Unit` で時間単位を指定できます。例：

```bash
now_timestamp('microsecond') = 1708913828814315
```

### rfc3339_to_unix_ts(DateTimeString: string) -> integer

RFC3339準拠の日時文字列をUnixタイムスタンプに変換します。例：

```bash
rfc3339_to_unix_ts('2024-02-23T15:56:30Z') = 1708703790
rfc3339_to_unix_ts('2024-02-23T15:56:30+08:00') = 1708674990
```

### rfc3339_to_unix_ts(DateTimeString: string, Unit: string) -> integer

`rfc3339_to_unix_ts/1` と同様ですが、`Unit` で返すUnixタイムスタンプの単位を指定できます。例：

```bash
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'second') = 1708703790
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'millisecond') = 1708703790870
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'microsecond') = 1708703790870000
rfc3339_to_unix_ts('2024-02-23T15:56:30.535904509Z', 'nanosecond') = 1708703790535904509
```

### timezone_to_offset_seconds(Offset: string) -> integer

タイムゾーンオフセット文字列を秒数の整数に変換します。サポートされる形式は：

- `Z` または `z`：UTCオフセット00:00  
- `±hh[:mm][:ss]` または `±hh[mm][ss]`：UTCからの正負の時刻オフセット  
- `local`：システムのローカルタイムゾーンのオフセット  

例：

```bash
timezone_to_offset_seconds('Z') = 0
timezone_to_offset_seconds('+08:00') = 28800
timezone_to_offset_seconds('local') = 28800
```

### unix_ts_to_rfc3339(Time: integer) -> string

Unixタイムスタンプ（秒単位）をシステムのローカルタイムゾーンでRFC3339準拠の日時文字列に変換します。例：

```bash
unix_ts_to_rfc3339(1708671600) = '2024-02-23T15:00:00+08:00'
```

### unix_ts_to_rfc3339(Time: integer, Unit: string) -> string

`unix_ts_to_rfc3339/0` と同様ですが、`Unit` で時間単位を指定できます。例：

```bash
unix_ts_to_rfc3339(1708671600766, 'millisecond') = '2024-02-23T15:00:00.766+08:00'
```

### MongoDB時間関数

### mongo_date() -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

現在時刻をMongoDBのISODate型または文字列で返します。MongoDB関連アクションとSQLテストでのみサポートされます。SQLテストでは文字列（例：`ISODate("2024-02-23T15:00:00.123Z")`）を返します。その他の関数の入力としては文字列以外は現在サポートされていません。

例：

```bash
mongo_date() = 'ISODate("2024-02-23T15:00:00.123Z")'
```

### mongo_date(Timestamp: integer) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

指定したミリ秒単位UnixタイムスタンプをMongoDBのISODate型または文字列に変換します。その他は `mongo_date/0` と同様です。

例：

```bash
mongo_date(now_timestamp('millisecond')) = 'ISODate(2024-02-23T15:48:57.871Z)'
```

### mongo_date(Timestamp: integer, Unit: string) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

指定したUnixタイムスタンプをMongoDBのISODate型または文字列に変換します。`Unit` で入力タイムスタンプの単位を指定できます。その他は `mongo_date/0` と同様です。

利用可能な単位は：

- `second`  
- `millisecond`  
- `microsecond`  
- `nanosecond`  

例：

```bash
mongo_date(now_timestamp('microsecond'), 'microsecond') = 'ISODate(2024-02-23T15:51:01.232Z)'
```

## UUID関数

### uuid_v4() -> string

バージョン4のUUIDを生成します。例：

```bash
uuid_v4() = 'f5bb7bea-a371-4df7-aa30-479add04632b'
```

### uuid_v4_no_hyphen() -> string

ハイフンなしのバージョン4UUIDを生成します。例：

```bash
uuid_v4_no_hyphen() = 'd7a39aa4195a42068b962eb9a665503e'
```

## システム関数

### getenv(Name)

環境変数 `Name` の値を返します。以下の制約があります：

- OS環境変数から読み取る際は接頭辞 `EMQXVAR_` が付加されます。例えば `getenv('FOO_BAR')` は `EMQXVAR_FOO_BAR` を読み取ります。  
- OS環境から読み込んだ値は不変です。

## 条件関数

### coalesce(Value1: any, Value2: any) -> any

`Value1` がnullの場合に `Value2` を返します。データフィールドがnullかどうかを判定し、デフォルト値に置き換える用途に便利です。

例えば、`coalesce(payload.value, 0)` は `payload.value` がnullでなければその値を、nullなら `0` を返します。SQL式の `CASE WHEN is_null(payload.value) THEN 0 ELSE payload.value END` と同等ですが簡潔です。

::: tip 注意

EMQXルールSQLではnull値の文字列表現はデフォルトで `'undefined'` です。

:::

### coalesce_ne(Value1: any, Value2: any) -> any

`coalesce` と似ていますが、`Value1` がnullまたは空文字列の場合に `Value2` を返します。

::: tip 注意

EMQXルールSQLではnull値の文字列表現はデフォルトで `'undefined'` です。

:::
