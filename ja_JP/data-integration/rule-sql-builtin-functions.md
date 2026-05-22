# 組み込みSQL関数

<<<<<<< HEAD
ルールエンジンは多様な組み込み関数を提供しています。これらの関数はSQL内で利用でき、基本的なデータ処理を実現します。以下のカテゴリがあります：
=======
ルールエンジンは多様な組み込み関数を提供しています。これらの関数はSQL内で利用でき、以下のような基本的なデータ処理を実現できます。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
本節の関数宣言はすべて以下の形式に準拠しています：
=======
本節の関数宣言はすべて以下の形式に準拠しています。
>>>>>>> origin/release-6.1

```bash
FuncName(引数1: 型1 | ..., ...) -> 型1 | ...
```

<<<<<<< HEAD
例えば、`acos(X: integer | float) -> float` は、引数 `X` のデータ型が整数または浮動小数点数であり、戻り値のデータ型は浮動小数点数であることを示します。

なお、指定された引数が定められた範囲を超えるか、サポートされていないデータ型を使用した場合、現在のSQL実行は失敗し、失敗回数が1増加します。

:::tip

1. 一部のエスケープシーケンスは使用時にアンエスケープが必要です。詳細は[unescape関数](#unescapestring-string---string)を参照してください。  
2. EMQX 5.0以降、複雑なデータ変換に[jq構文](https://stedolan.github.io/jq/manual/)もサポートしています。詳細は[jq関数](./rule-sql-jq.md)を参照してください。
=======
例えば、`acos(X: integer | float) -> float` は引数 `X` のデータ型が整数または浮動小数点数のいずれかであり、返り値のデータ型は浮動小数点数であることを示します。

引数が指定された範囲を超えたり、サポートされていないデータ型を使用した場合、現在のSQL実行は失敗し、失敗カウントが1増加しますのでご注意ください。

:::tip

1. 一部のエスケープシーケンスは使用時にアンエスケープが必要です。詳細は [unescape関数](#unescapestring-string---string) を参照してください。  
2. EMQX 5.0以降、複雑なデータ変換のために [jq構文](https://stedolan.github.io/jq/manual/) の使用もサポートしています。詳細は [jq関数](./rule-sql-jq.md) をご覧ください。
>>>>>>> origin/release-6.1

:::

## 数学関数

EMQXは幅広い数学関数をサポートしています。

- 三角関数および双曲線関数：sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
- 数値関数：abs, ceil, floor, round, sqrt, fmod
- 指数関数および対数関数：exp, power, log, log10, log2

### abs(X: integer) -> integer

整数 `X` の絶対値を返します。例：

```bash
abs(-12) = 12
```

:::tip
<<<<<<< HEAD
浮動小数点数の絶対値演算には、`ceil` または `floor` 関数を使用してください。
=======
浮動小数点数の絶対値演算には、代わりに `ceil` または `floor` 関数を使用してください。
>>>>>>> origin/release-6.1
:::

### acos(X: integer | float) -> float

`X` のアークコサインをラジアンで返します。`X` の範囲は `[-1, 1]` です。例：

```bash
acos(0.5) = 1.0471975511965976
```

### acosh(X: integer | float) -> float

<<<<<<< HEAD
`X` の双曲線アークコサインをラジアンで返します。`X` は1以上でなければなりません。例：
=======
`X` の双曲線アークコサイン（ラジアン単位）を返します。`X` は1以上である必要があります。例：
>>>>>>> origin/release-6.1

```bash
acosh(1.5) = 0.9624236501192069
```

### asin(X: integer | float) -> float

`X` のアークサインをラジアンで返します。`X` の範囲は `[-1, 1]` です。例：

```bash
asin(0.5) = 0.5235987755982988
```

### asinh(X: integer | float) -> float

`X` の双曲線アークサインを返します。例：

```bash
asinh(0.5) = 0.48121182505960347
```

### atan(X: integer | float) -> float

`X` のアークタンジェントをラジアンで返します。例：

```bash
atan(0.5) = 0.46364760900080615
```

### atanh(X: integer | float) -> float

`X` の双曲線アークタンジェントを返します。`X` は `(-1, 1)` の範囲です。例：

```bash
atanh(0.5) = 0.5493061443340549
```

### ceil(X: integer | float) -> integer

`X` 以上の最小の整数に切り上げます。例：

```bash
ceil(0.8) = 1
```

### cos(X: integer | float) -> float

角度 `X`（ラジアン）のコサインを返します。例：

```bash
cos(0.5) = 0.8775825618903728
```

### cosh(X: integer | float) -> float

`X` の双曲線コサインを返します。例：

```bash
cosh(0.5) = 1.1276259652063807
```

### exp(X: integer | float) -> float

自然対数の底 e の `X` 乗を返します。例：

```bash
exp(1) = 2.718281828459045
```

### floor(X: integer | float) -> integer

`X` 以下の最大の整数に切り捨てます。例：

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

`[0, 1)` の範囲でランダムな浮動小数点数を返します。例：

```bash
random() = 0.5400050092601868
```

### sin(X: integer | float) -> float

角度 `X`（ラジアン）のサインを返します。例：

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

角度 `X`（ラジアン）のタンジェントを返します。例：

```bash
tan(0.5) = 0.5463024898437905
```

### tanh(X: integer | float) -> float

`X` の双曲線タンジェントを返します。例：

```bash
tanh(0.5) = 0.46211715726000974
```

## データ型判定関数

<<<<<<< HEAD
データ型判定関数は、指定したフィールドのデータ型を判定し、そのフィールドが指定したデータ型に合致するかどうかを真偽値で返します。

### is_array(Term: any) -> boolean

`Term` が配列型かどうか判定します。例：
=======
指定したフィールドのデータ型を判定し、指定のデータ型に合致するかどうかを真偽値で返します。

### is_array(Term: any) -> boolean

`Term` が配列型かどうか判定します。`any` はすべてのデータ型を意味します。例：
>>>>>>> origin/release-6.1

```bash
is_array([1, 2]) = true
is_array(json_decode('[{"value": 1}]')) = true
is_array(json_decode('{"value": 1}')) = false
is_array(0.5) = false
is_array('[1, 2]') = false
```

### is_bool(Term: any) -> boolean

`Term` がブール型かどうか判定します。例：

```bash
is_bool(true) = true
is_bool(false) = false
is_bool('true') = false
```

### is_float(Term: any) -> boolean

<<<<<<< HEAD
`Term` が浮動小数点型かどうか判定します。例：
=======
`Term` が浮動小数点数型か判定します。例：
>>>>>>> origin/release-6.1

```bash
is_float(123.4) = true
is_float(123) = false
```

### is_int(Term: any) -> boolean

`Term` が整数型かどうか判定します。例：

```bash
is_int(123) = true
is_int(123.4) = false
```

### is_map(Term: any) -> boolean

`Term` がマップ型かどうか判定します。例：

```bash
is_map(json_decode('{"value": 1}')) = true
is_map(json_decode('[{"value": 1}]')) = false
```

### is_null(Term: any) -> boolean

<<<<<<< HEAD
変数 `Term` が未定義かどうか判定します。  
この関数は変数に値が割り当てられているかを判定するために使い、値がJSONの `null` であっても未定義とはみなしません。
=======
変数 `Term` が未定義か判定します。  
この関数は変数に値が割り当てられているかを判定しますが、値がJSONの `null` であっても未定義とはみなしません。
>>>>>>> origin/release-6.1

例：

```sql
is_null(this_is_an_unassigned_variable) = true
is_null(map_get('b', json_decode('{"a": 1}'))) = true
is_null(map_get('b', json_decode('{"b": null}'))) = false
```

### is_null_var(Term: any) -> boolean

変数 `Term` が未定義または `null` かどうか判定します。例：

```sql
is_null_var(this_is_an_unassigned_variable) = true
is_null_var(map_get('b', json_decode('{"a": 1}'))) = true
is_null_var(map_get('b', json_decode('{"b": null}'))) = true
```

### is_not_null_var(Term: any) -> boolean

<<<<<<< HEAD
`is_null_var` の逆で、変数 `Term` が定義されており `null` でないか判定します。

### is_num(Term: any) -> boolean

`Term` が整数型または浮動小数点型か判定します。例：
=======
`is_null_var` の逆で、変数 `Term` が定義されておりかつ `null` でないかを判定します。

### is_num(Term: any) -> boolean

`Term` が整数型または浮動小数点数型か判定します。例：
>>>>>>> origin/release-6.1

```bash
is_num(123) = true
is_num(123.4) = true
is_num('123') = false
```

### is_str(Term: any) -> boolean

`Term` が文字列型かどうか判定します。例：

```bash
is_str('123') = true
is_str(123) = false
```

### is_empty(Array or Map) -> boolean

<<<<<<< HEAD
`Array` または `Map` が空かどうか判定します。例：
=======
配列またはマップが空かどうか判定します。例：
>>>>>>> origin/release-6.1

```bash
is_empty(json_decode('{}')) = true
is_empty('{}') = true
is_empty('{"key" : 1}') = false
is_empty(map_get('key', '{"key" : []}')) = true
is_empty(map_get('key', '{"key" : [1}')) = false
```

## データ型変換関数

### bool(Term: boolean | integer | string) -> boolean

<<<<<<< HEAD
`Term` をブール型に変換します。`Term` はブール型、0または1の整数型、または文字列型で `true` または `false` のみ許容されます。
=======
`Term` をブール型に変換します。`Term` はブール型、整数型（0または1）、または文字列型（"true"または"false"）のみ許容されます。
>>>>>>> origin/release-6.1

例：

```bash
# 正常
bool(true) = true
bool(0) = false
bool('false') = false

# 誤った例
bool(20)
bool('True')
```

### float(Term: float | integer | string) -> float

`Term` を浮動小数点数に変換します。

<<<<<<< HEAD
`Term` が文字列の場合、科学的記数法も使用可能です（例：`float('3.14e4')`）。浮動小数点数は最大16桁の有効数字をサポートします。文字列で表現された浮動小数点数の有効数字が16桁を超える場合、変換に誤差が生じる可能性があります。
=======
`Term` が文字列の場合、科学的記数法も使用可能です（例：`float('3.14e4')`）。浮動小数点数は最大16桁の有効数字をサポートします。有効数字が16桁を超える場合、変換時に丸め誤差が発生する可能性があります。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
`Term` を小数点以下最大 `Decimals` 桁の浮動小数点数に変換します。`Decimals` の範囲は `(0, 253]` です。その他の挙動は `float/1` と同じです。例：
=======
`Term` を小数点以下最大 `Decimals` 桁の浮動小数点数に変換します。`Decimals` の範囲は `(0, 253]` です。その他の挙動は `float/1` と同様です。例：
>>>>>>> origin/release-6.1

```bash
float('3.1415926', 3) = 3.142
float('0.000012345', 5) = 0.00001
```

### float2str(Float: float, Decimals: integer) -> string

<<<<<<< HEAD
浮動小数点数 `Float` を文字列に変換します。小数点以下最大 `Decimals` 桁まで含み、末尾のゼロは切り捨てられます。`Decimals` の範囲は `[0, 253]` です。`Float` の有効数字が16桁を超える場合、変換時に丸め誤差が生じる可能性があります。

浮動小数点数はコンピュータ上で正確に保存できないため、`Decimals` が `Float` の小数点以下桁数（先頭のゼロも含む）より大きい場合、`float2str` は `Float` の2進近似の10進表現を返すことがあります。
=======
浮動小数点数 `Float` を小数点以下最大 `Decimals` 桁の文字列に変換し、末尾のゼロは切り捨てます。`Decimals` の範囲は `[0, 253]` です。`Float` の有効数字が16桁を超える場合、変換時に丸め誤差が発生する可能性があります。

浮動小数点数はコンピュータ上で正確に格納できないため、`Decimals` が `Float` の小数点以下桁数（先行ゼロ含む）より大きい場合、`float2str` は `Float` の2進近似の10進表現を返すことがあります。
>>>>>>> origin/release-6.1

例：

```bash
float2str(0.1, 5) = '0.1'
float2str(0.1, 20) = '0.10000000000000000555'
float2str(0.1, 25) = '0.1000000000000000055511151'
float2str(0.00000000001, 20) = '0.00000000001'

# 末尾のゼロは切り捨てられる
float2str(0.100001, 5) = '0.1'

# 有効数字が16桁を超えると丸め誤差により異なる入力が同じ出力になることがあります。
float2str(123456789.01234565, 8) = '123456789.01234566'
float2str(123456789.01234566, 8) = '123456789.01234566'
```

### int(Term: boolean | float | integer | string) -> integer

`Term` を整数に変換します。

<<<<<<< HEAD
- `Term` がブール型の場合、`true` は1、`false` は0に変換されます。  
- `Term` が浮動小数点数の場合、`Term` 以下の最大の整数に切り捨てられます。  
- `Term` が文字列の場合、少なくとも1つの数字を含み、先頭に `+` または `-` の1文字の接頭辞を持つことができ、先頭のゼロは無視されます。数学的表記もサポートされます。  
=======
- `Term` がブール型の場合、true は 1、false は 0 に変換されます。
- `Term` が浮動小数点数の場合、`Term` 以下の最大の整数に切り捨てられます。
- `Term` が文字列の場合、数値文字を1つ以上含み、先頭に `+` または `-` の1文字の接頭辞を持つことができ、先行ゼロは無視されます。数学的表記もサポートします。
>>>>>>> origin/release-6.1
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

# 誤った例
int('-100+200')
int('Number 100')
```

### str(Term: any) -> string

任意の `Term` を文字列に変換します。

<<<<<<< HEAD
- `Term` がマップまたは配列の場合、`str` 関数は `Term` をJSONエンコードしようとします。  
- `Term` が浮動小数点数の場合、末尾のゼロを切り捨てた対応する文字列を返します。返される文字列は小数点以下最大10桁まで保持します。より多くの小数桁を返すには `float2str` 関数を使用してください。
=======
- `Term` がマップまたは配列の場合、`str` 関数は `Term` をJSONエンコードしようとします。
- `Term` が浮動小数点数の場合、末尾のゼロを切り捨てた対応する文字列を返します。返される文字列は小数点以下最大10桁を含みます。より多くの小数点以下桁数を返すには `float2str` 関数を使用してください。
>>>>>>> origin/release-6.1

例：

```bash
str(100) = '100'
str(nth(1, json_decode('[false]'))) = 'false'
str(json_decode({"msg": "hello"})) = '{"msg":"hello"}'
str(json_decode('[{"msg": "hello"}]')) = '[{"msg":"hello"}]'

# 末尾のゼロは切り捨てられます
# 小数点以下最大10桁まで保持
str(0.30000000040) = '0.3000000004'
str(0.30000000004) = '0.3'

<<<<<<< HEAD
# 小数点以下10桁で丸め
# 10桁目以降で丸め
=======
# 小数点以下10桁で四捨五入
# 10桁目以降で丸められます
>>>>>>> origin/release-6.1
str(3.14159265359) = '3.1415926536'
str(0.000000314159265359) = '0.0000003142'
```

### str_utf8(Term: any) -> string

任意の `Term` をUTF-8エンコードされた文字列に変換します。

<<<<<<< HEAD
その他の挙動は `str(Any)` と同一です。
=======
動作は `str(Any)` と同様です。
>>>>>>> origin/release-6.1

```bash
str_utf8(100) = '100'
str_utf8(nth(1, json_decode('[false]'))) = 'false'
str_utf8(json_decode({"msg": "hello"})) = '{"msg":"hello"}'
str_utf8(json_decode('[{"msg": "hello"}]')) = '[{"msg":"hello"}]'

# 末尾のゼロは切り捨てられます
# 小数点以下最大10桁まで保持
str_utf8(0.30000000040) = '0.3000000004'
str_utf8(0.30000000004) = '0.3'

<<<<<<< HEAD
# 小数点以下10桁で丸め
# 10桁目以降で丸め
=======
# 小数点以下10桁で四捨五入
# 10桁目以降で丸められます
>>>>>>> origin/release-6.1
str_utf8(3.14159265359) = '3.1415926536'
str_utf8(0.000000314159265359) = '0.0000003142'
```

### str_utf16_le(Term: any) -> binary

任意の `Term` をUTF-16リトルエンディアンエンコードされたバイナリ文字列に変換します。

::: tip

<<<<<<< HEAD
UTF-16リトルエンディアンエンコード文字列はJSONオブジェクト内で正しく表示されない場合があります。EMQXでは通常バイナリデータとして扱われます。可読の16進文字列に変換するには `bin2hexstr` 関数を使用してください。  
このエンコードはMicrosoft SQL Serverなど、リトルエンディアンUTF-16を使用するシステムで一般的に用いられます。
=======
UTF-16リトルエンディアンエンコード文字列はJSONオブジェクト内で正しく表示されない場合があります。EMQXでは通常バイナリデータとして扱われます。読みやすい16進数文字列に変換するには `bin2hexstr` 関数を使用してください。  
このエンコードはMicrosoft SQL Serverなど、リトルエンディアンUTF-16を使用するシステムで一般的に利用されます。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
文字列関数は大文字小文字変換、空白除去、部分文字列抽出、置換、エスケープ／アンエスケープなどに使用できます。

### ascii(Char: string) -> integer

文字 `Char` のASCIIコードを返します。`Char` に複数文字が含まれる場合は最初の1文字のコードのみ返します。例：
=======
文字列の大文字・小文字変換、空白除去、部分文字列抽出、置換、エスケープ／アンエスケープなどに使用します。

### ascii(Char: string) -> integer

文字 `Char` のASCIIコードを返します。複数文字の場合は最初の1文字のコードを返します。例：
>>>>>>> origin/release-6.1

```bash
ascii('a') = 97
ascii('abc') = 97
```

### concat(Str1: string, Str2: string) -> string

`Str1` と `Str2` を連結して返します。例：

```bash
concat('Name:', 'John') = 'Name:John'
```

### find(String: string, SearchPattern: string) -> string

<<<<<<< HEAD
`String` 内で部分文字列 `SearchPattern` を検索し、`SearchPattern` より前の内容を削除して残りの文字列を返します。`SearchPattern` が見つからなければ空文字列を返します。この関数は `find(String, SearchPattern, 'leading')` と同等です。
=======
`String` 内で部分文字列 `SearchPattern` を検索し、`SearchPattern` より前の内容を削除して残りを返します。`SearchPattern` が見つからない場合は空文字列を返します。`find(String, SearchPattern)` は `find(String, SearchPattern, 'leading')` と同等です。
>>>>>>> origin/release-6.1

例：

```bash
find('..., Value: 1.2', 'Value:') = 'Value: 1.2'
find('..., Value: 1.2', 'Data') = ''
```

### find(String: string, SearchPattern: string, Direction: string) -> string

`find/2` と同様ですが、`Direction` で検索方向を指定できます。例：

```bash
find('Front, Middle, End', ', ', 'leading') = ', Middle, End'
find('Front, Middle, End', ', ', 'trailing') = ', End'
```

### join_to_string(Sep: string, Array: array) -> string

<<<<<<< HEAD
`Array` の要素を区切り文字 `Sep` で連結して1つの文字列にします。例：
=======
`Array` の要素を区切り文字 `Sep` で連結して返します。例：
>>>>>>> origin/release-6.1

```bash
join_to_string(', ', ['a', 'b', 'c']) = 'a, b, c'
```

### lower(String: string) -> string

`String` の大文字を小文字に変換します。例：

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

<<<<<<< HEAD
`String` の末尾にスペースを埋めて指定長 `Length` にします。例：
=======
`String` の末尾にスペースを追加し、指定した長さにパディングします。例：
>>>>>>> origin/release-6.1

```bash
pad('hello', 8) = 'hello   '
```

### pad(String: string, Length: integer, Direction: string) -> string

<<<<<<< HEAD
`pad/2` と同様ですが、`Direction` で埋める方向を指定できます。`leading` は前方、`trailing` は後方、`both` は両端にスペースを埋めます。

`both` 指定時に埋めるスペース数が奇数の場合、最後のスペースは末尾に埋められます。
=======
`pad/2` と同様ですが、`Direction` でパディング方向を指定できます。  
`leading` は先頭にスペースを埋め、`trailing` は末尾に埋め、`both` は両端に埋めます。  
`both` 指定時、埋めるスペース数が奇数の場合は最後のスペースが末尾に追加されます。
>>>>>>> origin/release-6.1

例：

```bash
pad('hello', 8, 'leading') = '   hello'
pad('hello', 8, 'trailing') = 'hello   '
pad('hello', 8, 'both') = ' hello  '
```

### pad(String: string, Length: integer, Direction: string, Char: string) -> string

`pad/3` と同様ですが、指定したグラフェムクラスタ `Char` で埋めます。

<<<<<<< HEAD
ルールエンジンは `Char` が合法なグラフェムクラスタかチェックしないため、`Char` の長さに関わらず1文字として処理されます。例：
=======
ルールエンジンは `Char` が合法なグラフェムクラスタかをチェックしないため、`Char` の文字数に関わらず1文字として処理されます。例：
>>>>>>> origin/release-6.1

```bash
pad('hello', 8, 'trailing', '!') = 'hello!!!'
pad('hello', 8, 'trailing', '\r\n') = 'hello\r\n\r\n\r\n'
pad('hello', 8, 'trailing', 'abc') = 'helloabcabcabc'
```

### regex_match(String: string, Expression: string) -> boolean

`String` が正規表現 `Expression` にマッチするか判定します。例：

```bash
regex_match('123', '^\d+$') = true
regex_match('a23', '^\d+$') = false
```

### regex_replace(String: string, Expression: string, Replacement: string) -> string

<<<<<<< HEAD
正規表現 `Expression` にマッチする部分を文字列 `Replacement` に置換します。マッチがなければ元の文字列を返します。例：
=======
`String` の正規表現 `Expression` にマッチする部分を `Replacement` に置換します。マッチがなければ元の文字列を返します。例：
>>>>>>> origin/release-6.1

```bash
regex_replace('hello 123', '\d+', 'world') = 'hello world'
regex_replace('a;b; c', ';\s*', ',') = 'a,b,c'
```

### regex_extract(String: string, Expression: string) -> [string]

::: tip

この関数はEMQX v5.7.1以降で利用可能です。

:::

<<<<<<< HEAD
正規表現パターンのキャプチャグループを使って文字列から部分抽出を行います。完全マッチは含みません。

マッチがあればすべてのキャプチャグループのリストを返し、マッチがなければ空リストを返します。
=======
正規表現のキャプチャグループを用いて文字列から部分文字列を抽出します。完全一致部分は含まれません。

マッチがあればキャプチャされたすべてのグループのリストを返し、マッチなしまたはキャプチャなしの場合は空リストを返します。
>>>>>>> origin/release-6.1

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

`Where` は以下の値を取ります：

<<<<<<< HEAD
- `all`: すべて置換（`replace/3` と同等）
- `leading`: 先頭の1つのみ置換
- `trailing`: 末尾の1つのみ置換
=======
- `all`: すべての `SearchPattern` を置換（`replace/3` と同等）
- `leading`: 先頭の `SearchPattern` のみ置換
- `trailing`: 末尾の `SearchPattern` のみ置換
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
文字列 `String` の先頭から `Prefix` を削除します。`String` が `Prefix` で始まらない場合は元の文字列を返します。例：
=======
`String` の先頭にある `Prefix` を削除します。`String` が `Prefix` で始まらない場合は元の文字列を返します。例：
>>>>>>> origin/release-6.1

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

`String` を区切り文字 `Separator` で分割し、配列で返します。

<<<<<<< HEAD
連続する複数の区切り文字は1つとして扱われず、空文字列が結果に含まれる場合があります。`split/2` はデフォルトで結果をトリムし空文字列を除外します。空文字列も保持したい場合は `split(String, Separator, 'notrim')` を使用してください。

区切り文字は複数文字でも構い、全体として扱われます。複数の区切り文字を同時に指定したい場合は `tokens` 関数を使ってください。
=======
連続する複数の区切り文字は1つとして扱われず、空文字列が結果に含まれる場合があります。`split/2` はデフォルトで結果をトリムし空文字列を除外します。空文字列を残したい場合は `split(String, Separator, 'notrim')` を使用してください。

区切り文字は複数文字でも可能ですが、1つのまとまりとして扱われます。複数の区切り文字を指定したい場合は `tokens` 関数を使用してください。
>>>>>>> origin/release-6.1

例：

```bash
split('a;', ';') = ['a']
split('a;b;c', ';') = ['a', 'b', 'c']
split('a;;b;;c', ';') = ['a', 'b', 'c']

# Howell Wise の前のスペースに注意
split('Sienna Blake; Howell Wise', ';') = ['Sienna Blake', ' Howell Wise']
split('Sienna Blake; Howell Wise', '; ') = ['Sienna Blake', 'Howell Wise']
```

### split(String: string, Separator: string, Option: string) -> array

<<<<<<< HEAD
`split/2` と同様ですが、`Option` で処理する区切り文字の位置や空文字列の返却を指定できます。
=======
`split/2` と同様ですが、`Option` で処理対象の区切り文字の位置や空文字列の返却有無を指定できます。
>>>>>>> origin/release-6.1

`Option` の値は以下の通り：

<<<<<<< HEAD
- `notrim`: 文字列中のすべての区切り文字を処理し、空文字列を含む結果を返す
- `leading`: 先頭の区切り文字のみ処理し、空文字列は含まない
- `leading_notrim`: 先頭の区切り文字のみ処理し、空文字列を含む可能性あり
- `trailing`: 末尾の区切り文字のみ処理し、空文字列は含まない
- `trailing_notrim`: 末尾の区切り文字のみ処理し、空文字列を含む可能性あり
=======
- `notrim`: 文字列内のすべての区切り文字を処理し、空文字列を含む結果を返す
- `leading`: 先頭の区切り文字のみ処理し、空文字列を含まない結果を返す
- `leading_notrim`: 先頭の区切り文字のみ処理し、空文字列を含む結果を返す
- `trailing`: 末尾の区切り文字のみ処理し、空文字列を含まない結果を返す
- `trailing_notrim`: 末尾の区切り文字のみ処理し、空文字列を含む結果を返す
>>>>>>> origin/release-6.1

例：

```bash
split('a;;b;;c', ';', 'notrim') = ['a', '', 'b', '', 'c']
split('a;b;c', ';', 'leading') = ['a', 'b;c']
split('a;b;c', ';', 'trailing') = ['a;b', 'c']
split(';a;b;c', ';', 'leading_notrim') = ['', 'a;b;c']
split('a;b;c;', ';', 'trailing_notrim') = ['a;b;c', '']
```

### sprintf(Format, ...) -> string

<<<<<<< HEAD
`Format` に従って書式化した文字列を返します。`Format` 文字列は通常の文字と書式制御シーケンスを含みます。

制御シーケンスの形式は一般に `~F.P.PadModC` です。  
`C` は制御シーケンスの種類を決定し、必須です。`F`、`P`、`Pad`、`Mod` は任意です。詳細は https://www.erlang.org/doc/apps/stdlib/io.html#fwrite-1 を参照してください。
=======
`Format` に従ってフォーマットされた文字列を返します。`Format` 文字列は通常の文字とフォーマット制御シーケンスを含みます。

制御シーケンスの形式は一般に `~F.P.PadModC` です。

`C` は制御シーケンスの種類を示し必須です。`F`, `P`, `Pad`, `Mod` は任意です。詳細は https://www.erlang.org/doc/apps/stdlib/io.html#fwrite-1 を参照してください。
>>>>>>> origin/release-6.1

例：

```bash
sprintf('hello, ~s!', 'steve') = 'hello, steve!'
sprintf('count: ~p~n', 100) = 'count: 100\n'
```

### strlen(String: string) -> integer

`String` の長さを返します。例：

```bash
strlen('hello') = 5
strlen('hello\n') = 6
```

### substr(String: string, Start: integer) -> string

`String` の `Start` 位置（0始まり）から末尾までの部分文字列を返します。例：

```bash
substr('hello', 0) = 'hello'
substr('hello world', 6) = 'world'
```

### substr(String: string, Start: integer, Length: integer) -> string

`String` の `Start` 位置（0始まり）から最大 `Length` 文字の部分文字列を返します。例：

```bash
substr('hello world!', 6, 5) = 'world'
```

### tokens(String: string, SeparatorList: string) -> array

<<<<<<< HEAD
`String` を `SeparatorList` に含まれる文字で分割し、空文字列を含まない配列を返します。連続する区切り文字は1つとして扱われます。
=======
`String` を `SeparatorList` に含まれる任意の文字で分割し、空文字列を含まない配列を返します。連続する区切り文字は1つとして扱われます。
>>>>>>> origin/release-6.1

例：

```bash
tokens('a,b;c,d', ',;') = ['a', 'b', 'c', 'd']
tokens('a;;b', ';') = ['a', 'b']
```

### tokens(String: string, SeparatorList:string, NoCRLF: string) -> array

`tokens/2` と同様ですが、`NoCRLF` に `nocrlf` を指定すると改行コード（CR, LF）も区切り文字として扱います。例：

```bash
tokens('a\rb\nc\r\nd', ';', 'nocrlf') = ['a', 'b', 'c', 'd']
```

### trim(String: string) -> string

<<<<<<< HEAD
`String` の先頭と末尾の空白文字（スペース、タブ、改ページ、改行など）を削除します。`\r\n` はUnicodeのグラフェムクラスタとしてまとめて削除されます。例：
=======
`String` の先頭および末尾の空白文字（スペース、タブ、改ページ、改行など）を削除します。Unicode標準で `\r\n` はグラフェムクラスタとして扱われるため、`\r\n` はまとめて削除されます。例：
>>>>>>> origin/release-6.1

```bash
trim('\t  hello  \n') = 'hello'
trim('\t  hello \r\n') = 'hello'
```

### unescape(String: string) -> string

<<<<<<< HEAD
エスケープシーケンスを元の文字に戻します。SQL内でエスケープシーケンスを使用する場合、正しく処理するためにこの関数でアンエスケープしてください。
=======
エスケープシーケンスを元の文字に戻します。SQLでエスケープシーケンスを使う場合は、正しく処理するためにこの関数でアンエスケープしてください。
>>>>>>> origin/release-6.1

::: tip

この関数はEMQX v5.7.0以降で利用可能です。

:::

例えば、ペイロードが改行区切りの文字列の場合：

```bash
32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3
87.2
12.3
my-device
```

<<<<<<< HEAD
`\n` で分割したい場合、以下のSQLは期待通りに動作しません：
=======
`\n` で分割したい場合、以下のSQLは期待通りに動作しません。
>>>>>>> origin/release-6.1

```sql
SELECT split(payload, '\n') as device_info FROM 't/#'
```

出力：

```json
{
  "device_info": [
    "32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3\n87.2\n12.3\nmy-device"
  ]
}
```

`unescape` 関数で `\n` をアンエスケープすると期待通りの結果が得られます。

```sql
SELECT split(payload, unescape('\n')) as device_info FROM 't/#'
```

出力：

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

**`unescape` 関数がサポートするエスケープシーケンス：**

- 標準Cエスケープシーケンス：

<<<<<<< HEAD
  - `\n`（改行 LF）
  - `\t`（水平タブ HT）
  - `\r`（復帰 CR）
  - `\b`（バックスペース BS）
  - `\f`（改ページ FF）
  - `\v`（垂直タブ VT）
  - `\'`（シングルクォート '）
  - `\"`（ダブルクォート "）
  - `\\`（バックスラッシュ \）
  - `\?`（疑問符 ?）
  - `\a`（警告音 BEL）

- 16進エスケープコード：

  - `\xH...`（`H...` は1つ以上の16進数字、任意のutf32文字をエンコード可能）

認識されないエスケープシーケンスや無効なUnicode文字の場合、例外が発生します。
=======
  - `\n`：改行（LF）
  - `\t`：水平タブ（HT）
  - `\r`：復帰（CR）
  - `\b`：バックスペース（BS）
  - `\f`：改ページ（FF）
  - `\v`：垂直タブ（VT）
  - `\'`：シングルクォート（'）
  - `\"`：ダブルクォート（"）
  - `\\`：バックスラッシュ（\）
  - `\?`：疑問符（?）
  - `\a`：警告音（ベル、BEL）

- 16進エスケープコード：

  - `\xH...`：`H...` は1文字以上の16進数（0-9, A-F, a-f）で任意のUTF-32文字をエンコード可能。

認識できないエスケープシーケンスや無効なUnicode文字の場合、例外が発生します。
>>>>>>> origin/release-6.1

### upper(String: string) -> string

`String` の小文字を大文字に変換します。例：

```bash
upper('hello') = 'HELLO'
```

## マップ操作関数

### map_get(Key: string, Map: map) -> any

<<<<<<< HEAD
`Map` の指定した `Key` の値を返します。`Key` が存在しなければ `undefined` を返します。例：
=======
`Map` の指定した `Key` の値を返します。存在しなければ `undefined` を返します。例：
>>>>>>> origin/release-6.1

```bash
map_get('msg', json_decode('{"msg": "hello"}')) = 'hello'
map_get('data', json_decode('{"msg": "hello"}')) = undefined
```

### map_get(Key: string, Map: map, Default: any) -> any

`map_get/2` と同様ですが、`Key` が存在しない場合は `Default` を返します。例：

```bash
map_get('data', json_decode('{"msg": "hello"}'), '') = ''
map_get('value', json_decode('{"data": [1.2, 1.3]}'), []) = []
```

### map_keys(Map: map) -> array

`Map` のすべてのキーを配列で返します。例：

```bash
map_keys(json_decode('{"a": 1, "b": 2}')) = ['a', 'b']
```

### map_put(Key: string, Value: any, Map: map) -> map

`Map` に `Key` と対応する `Value` を挿入し、更新されたマップを返します。既存の `Key` があれば値を上書きします。例：

```bash
map_get('b', map_put('b', 1, json_decode('{"a": 1}'))) = 1
map_get('a', map_put('a', 2, json_decode('{"a": 1}'))) = 2
```

### map_to_redis_hset_args(Map) -> list

::: tip

この関数はEMQX v5.7.1以降で利用可能です。

:::

マップをRedisの `HSET`（または `HMSET`）コマンド用のフィールド名と値のリストに変換します。

<<<<<<< HEAD
例：`SELECT map_to_redis_hset_args(payload.value) as hset_fields FROM t/1` のように使い、`hset_fields` をRedisアクションのテンプレートに組み込みます。  
例えば `payload.value` が `{"a" : 1, "b": 2}` の場合、コマンドは `HMSET name1 b 2 a 1` のようになります。マップの順序は非決定的です。
=======
例：`SELECT map_to_redis_hset_args(payload.value) as hset_fields FROM t/1` のように使い、Redisアクションのテンプレートで `HMSET name1 ${hset_fields}` として利用します。

例えば、`payload.value` が `{"a" : 1, "b": 2}` の場合、生成されるコマンドは `HMSET name1 b 2 a 1` となります。マップのフィールドの順序は非決定的です。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
`Map` の指定した `Key` の値を返します。`Key` が存在しなければ `undefined` を返します。配列を指定するとネストされたマップから複数のキーを一度に取得できます。例：
=======
`Map` の指定した `Key` の値を返します。`Key` が存在しない場合は `undefined` を返します。配列で複数のキーを指定すると、ネストしたマップから対応する値を取得できます。例：
>>>>>>> origin/release-6.1

```bash
mget('c', json_decode('{"a": {"b": 1}}')) = undefined
json_decode(mget('a', json_decode('{"a": {"b": 1}}'))) = '{"b": 1}'
mget(['a', 'b'], json_decode('{"a": {"b": 1}}')) = 1
```

### mput(Key: string | array, Value: any, Map: map) -> map

<<<<<<< HEAD
`Map` に `Key` と対応する `Value` を挿入し、更新されたマップを返します。`Key` が既に存在する場合は値を上書きします。配列を指定するとネストされたマップに複数のキーを一度に挿入できます。例：
=======
`Map` に `Key` と対応する `Value` を挿入し、更新されたマップを返します。既存の `Key` があれば値を上書きします。配列で複数のキーを指定すると、ネストしたマップにデータを挿入できます。例：
>>>>>>> origin/release-6.1

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

配列 `Array` の最初の要素を返します。`Array` は空であってはいけません。例：

```bash
# 正常
first(['John', 'David']) = 'John'

# 誤った例
first([])
```

### last(Array: array) -> any

配列 `Array` の最後の要素を返します。`Array` は空であってはいけません。例：

```bash
# 正常
last(['John', 'David']) = 'David'

# 誤った例
last([])
```

### length(Array: array) -> integer

配列 `Array` の長さ（要素数）を返します。例：

```bash
length([1,2,3,4]) = 4
length([]) = 0
```

### nth(N: integer, Array: array) -> any

<<<<<<< HEAD
`Array` のN番目の要素を返します。`N` は `Array` の長さ以下でなければなりません。例：
=======
配列 `Array` の `N` 番目の要素を返します。`N` は `Array` の長さ以下でなければなりません。例：
>>>>>>> origin/release-6.1

```bash
# 正常
nth(1, [1,2,3]) = 1

# 誤った例
nth(0, [1,2,3])
nth(4, [1,2,3])
```

### sublist(Length: integer, Array: array) -> any

<<<<<<< HEAD
配列 `Array` の先頭から最大 `Length` 要素の部分配列を返します。`Length` が `Array` の長さを超える場合は全配列を返します。例：
=======
配列 `Array` の先頭から最大 `Length` 要素の部分配列を返します。`Length` が `Array` の長さを超える場合は全体を返します。例：
>>>>>>> origin/release-6.1

```bash
sublist(3, [1,2,3,4]) = [1,2,3]
sublist(10, [1,2,3,4]) = [1,2,3,4]
```

### sublist(Start: integer, Length: integer, Array:array) -> any

<<<<<<< HEAD
`sublist/2` と同様ですが、`Start` で開始位置を指定できます。`Start` + `Length` が `Array` の長さを超える場合は全配列を返します。例：
=======
`sublist/2` と同様ですが、`Start` で返す開始要素を指定できます。`Start` + `Length` が `Array` の長さを超える場合は全体を返します。例：
>>>>>>> origin/release-6.1

```bash
sublist(2, 10, [1,2,3,4]) = [2,3,4]
```

## ハッシュ関数

### md5(String: string) -> string

<<<<<<< HEAD
任意長の文字列 `String` の128ビット固定長MD5ハッシュ値を計算します。ハッシュ値は32桁の16進数文字列で返され、小文字（a〜f）で固定されます。
=======
任意長の文字列 `String` の128ビット固定長MD5ハッシュ値を計算します。ハッシュ値は32桁の16進数文字列（小文字）で返されます。
>>>>>>> origin/release-6.1

例：

```bash
md5('hello') = '5d41402abc4b2a76b9719d911017c592'
```

### sha(String: string) -> string

<<<<<<< HEAD
任意長の文字列 `String` の160ビット固定長SHA-1ハッシュ値を計算します。ハッシュ値は40桁の16進数文字列で返され、小文字（a〜f）で固定されます。
=======
任意長の文字列 `String` の160ビット固定長SHA-1ハッシュ値を計算します。ハッシュ値は40桁の16進数文字列（小文字）で返されます。
>>>>>>> origin/release-6.1

例：

```bash
sha('hello') = 'aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d'
```

### sha256(String: string) -> string

<<<<<<< HEAD
任意長の文字列 `String` の256ビット固定長SHA-2ハッシュ値を計算します。ハッシュ値は64桁の16進数文字列で返され、小文字（a〜f）で固定されます。
=======
任意長の文字列 `String` の256ビット固定長SHA-2ハッシュ値を計算します。ハッシュ値は64桁の16進数文字列（小文字）で返されます。
>>>>>>> origin/release-6.1

例：

```bash
sha256('hello') = '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824'
```

## 圧縮・解凍関数

<<<<<<< HEAD
注：バイナリデータは直接JSONエンコードできないため、`bin2hexstr` 関数で16進文字列に変換してください。
=======
注：バイナリデータは直接JSONエンコードできないため、16進数文字列に変換するには `bin2hexstr` 関数を使用してください。
>>>>>>> origin/release-6.1

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

DEFLATEアルゴリズムで `Data` を圧縮し、zlibヘッダーと末尾のチェックサムを含む圧縮結果を返します。例：

```bash
bin2hexstr(zip_compress('hello')) = '789CCB48CDC9C90700062C0215'
```

### zip_uncompress(Data: binary) -> binary | string

`Data` を解凍します。`Data` はzlibヘッダーと末尾のチェックサムを含む必要があります。例：

```bash
zip_uncompress(hexstr2bin('789CCB48CDC9C90700062C0215')) = 'hello'
```

## ビット操作関数

### bitand(Num1: integer, Num2: integer) -> integer

<<<<<<< HEAD
`Num1` と `Num2` のビットごとのAND演算結果を返します。入力と出力は符号付き整数です。例：
=======
`Num1` と `Num2` のビットごとのAND演算結果を返します。入力・出力は符号付き整数です。例：
>>>>>>> origin/release-6.1

```bash
bitand(10, 8) = 8
bitand(-10, -8) = -16
```

### bitnot(Num: integer) -> integer

<<<<<<< HEAD
`Num` のビットごとの否定を返します。入力と出力は符号付き整数です。例：
=======
`Num` のビットごとの否定演算結果を返します。入力・出力は符号付き整数です。例：
>>>>>>> origin/release-6.1

```bash
bitnot(10) = -11
bitnot(-12) = 11
```

### bitsl(Num: integer, Shift: integer) -> integer

`Num` を左に `Shift` ビットシフトし、右側を0で埋めます。例：

```bash
bitsl(8, 2) = 32
bitsl(-8, 2) = -32
```

### bitsr(Num: integer, Shift: integer) -> integer

`Num` を右に `Shift` ビットシフトし、左端を符号ビット（正数は0、負数は1）で埋めます。例：

```bash
bitsr(8, 2) = 2
bitsr(8, 4) = 0
bitsr(-8, 2) = -2
bitsr(-8, 6) = -1
```

### bitor(Num1: integer, Num2: integer) -> integer

`Num1` と `Num2` のビットごとのOR演算結果を返します。例：

```bash
bitor(10, 8) = 10
bitor(-10, -8) = -2
```

### bitxor(Num1: integer, Num2: integer) -> integer

`Num1` と `Num2` のビットごとのXOR演算結果を返します。例：

```bash
bitxor(10, 8) = 2
bitxor(-10, -8) = 14
```

## ビット列操作関数

ルールエンジンはビット列を操作する関数を提供しています。例えば `subbits` はビット列から指定長のビットを抽出し、指定データ型に変換します。

:::tip

<<<<<<< HEAD
`binary` 型はバイト列を表し、1バイトは8ビットです。したがって `binary` のビット数は8の倍数でなければなりません。  
`bitstring` 型は任意のビット数のビット列を表します。

つまり、すべての `binary` は `bitstring` ですが、逆は必ずしも真ではありません。

`bitstring` は長さが8の倍数でない場合、JSONなどの外部形式に直接シリアライズできません。通常は整数などに変換する前の中間値として使われます。
=======
`binary` 型はバイト列を表し、1バイトは8ビットなので、`binary` のビット数は8の倍数でなければなりません。  
`bitstring` 型は任意長のビット列を表し、8の倍数でなくてもよいです。

簡単に言うと、すべての `binary` は `bitstring` ですが、逆は必ずしも真ではありません。

`bitstring` は長さが8の倍数でない場合、JSONなどの外部形式に直接シリアライズできません。通常は整数などに変換する前の中間値として使います。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
バイト列 `Bin` の先頭から `BitNum` ビットを抽出し、ビッグエンディアンの符号なし整数に変換します。これは `subbits(Bytes, 1, BitNum, 'integer', 'unsigned', 'big')` と同等です。
=======
バイト列 `Bin` の先頭から `BitNum` ビットを抽出し、ビッグエンディアンの符号なし整数に変換します。`subbits(Bytes, 1, BitNum, 'integer', 'unsigned', 'big')` と同等です。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
バイト列 `Bin` の `Start` 位置（1始まり）から `BitNum` ビットを抽出し、ビッグエンディアンの符号なし整数に変換します。これは `subbits(Bytes, Start, BitNum, 'integer', 'unsigned', 'big')` と同等です。
=======
バイト列 `Bin` の `Start` 位置（1始まり）から `BitNum` ビットを抽出し、ビッグエンディアンの符号なし整数に変換します。`subbits(Bytes, Start, BitNum, 'integer', 'unsigned', 'big')` と同等です。
>>>>>>> origin/release-6.1

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

バイト列 `Bin` の `Start` 位置（1始まり）から `BitNum` ビットを抽出し、指定されたバイト順 `Endianness` と符号属性 `Signedness` に従い、指定データ型 `OutputType` に変換します。

- `OutputType` の値：`bits`（bitstringの略）、`integer`、`float`
- `Signedness` の値：`signed`、`unsigned`
- `Endianness` の値：`big`、`little`

<<<<<<< HEAD
`OutputType` が `float` の場合、`Signedness` は無効です。  
`OutputType` が `bits` の場合、`Signedness` と `Endianness` は無効です。
=======
`OutputType` が `float` の場合、`Signedness` は無効です。`OutputType` が `bits` の場合、`Signedness` と `Endianness` は無効です。
>>>>>>> origin/release-6.1

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

このオプション付き関数はEMQX 6.0.2以降で導入されました。

:::

オプションを指定してBase64形式からデコードします。

**オプション：**

<<<<<<< HEAD
- `no_padding`: パディング文字 `=` を期待せずデコード
- `urlsafe`: URLセーフBase64形式（`-` と `_` を使用）でデコード

オプションは単独または組み合わせて使用可能で、順序は結果に影響しません。
=======
- `no_padding`：パディング文字（`=`）なしでデコード。パディングが省略されたBase64文字列に有用。
- `urlsafe`：URLセーフBase64形式（`-` と `_` を使用）でデコード。

単独または組み合わせて使用可能。順序は結果に影響しません。
>>>>>>> origin/release-6.1

例：

```sql
-- URLセーフBase64をデコード
SELECT base64_decode(payload, 'urlsafe') as decoded FROM "t/#"

-- パディングなしURLセーフBase64をデコード
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

このオプション付き関数はEMQX 6.0.2以降で導入されました。

:::

オプションを指定してBase64形式にエンコードします。

**オプション：**

<<<<<<< HEAD
- `no_padding`: パディング文字 `=` なしでエンコード
- `urlsafe`: URLセーフBase64形式（`-` と `_` を使用）でエンコード

オプションは単独または組み合わせて使用可能で、順序は結果に影響しません。
=======
- `no_padding`：パディング文字（`=`）なしでエンコード。
- `urlsafe`：URLセーフBase64形式でエンコードし、`+` と `/` の代わりに `-` と `_` を使用。

単独または組み合わせて使用可能。順序は結果に影響しません。
>>>>>>> origin/release-6.1

例：

```sql
-- パディングなしでエンコード
SELECT base64_encode(payload, 'no_padding') as encoded FROM "t/#"

-- URLセーフでエンコード
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

バイナリデータを対応する16進数文字列に変換します。例：

```bash
bin2hexstr(zip('hello')) = 'CB48CDC9C90700'
```

### hexstr2bin(Data: string) -> binary

16進数文字列を対応するバイナリデータに変換します。例：

```bash
unzip(hexstr2bin('CB48CDC9C90700')) = 'hello'
```

### sqlserver_bin2hexstr(Data: binary | string) -> string

任意のバイナリデータをMicrosoft SQL Serverのバイナリ型に変換します。`0x` プレフィックス付きの16進数文字列になります。

::: tip

<<<<<<< HEAD
この関数はMicrosoft SQL Serverの `CONVERT` 関数と組み合わせて、UTF-16リトルエンディアンエンコードされたUnicode文字列をUTF-8非対応のSQL Serverバージョンに書き込む際に利用できます。
=======
この関数はMicrosoft SQL Serverの `CONVERT` 関数と組み合わせて使用し、UTF-16リトルエンディアンエンコードされたUnicode文字列をUTF-8非対応のSQL Serverに書き込むのに使えます。
>>>>>>> origin/release-6.1

:::

```
sqlserver_bin2hexstr('hello') = '0x68656C6C6F'
sqlserver_bin2hexstr(str_utf16_le('hello')) = '0x680065006C006C006F00'
sqlserver_bin2hexstr(str_utf16_le('你好')) = '0x604F7D59'
```

### スキーマレジストリ関数

<<<<<<< HEAD
EMQXは `schema_encode` と `schema_decode` 関数を使い、指定したスキーマに従って [Protobuf (Protocol Buffers)](https://developers.google.com/protocol-buffers) や [Avro](https://avro.apache.org/) データのエンコード・デコードをサポートしています。詳細は[スキーマレジストリ](./schema-registry.md)を参照してください。
=======
EMQXは `schema_encode` と `schema_decode` 関数を使い、指定したスキーマに従って [Protobuf (Protocol Buffers)](https://developers.google.com/protocol-buffers) や [Avro](https://avro.apache.org/) データのエンコード・デコードをサポートしています。詳細は [スキーマレジストリ](./schema-registry.md) をご覧ください。
>>>>>>> origin/release-6.1

### schema_encode(SchemaID: string, Data: map) -> binary

指定したAvroスキーマで `Data` をエンコードします。スキーマレジストリにスキーマを作成しIDを取得します。

### schema_encode(SchemaID: string, Data: map, MsgType: string) -> binary

指定したProtobufスキーマで `Data` をエンコードします。スキーマレジストリにスキーマを作成しIDを取得します。`MsgType` はProtobufスキーマ内のメッセージタイプを指定します。

### schema_decode(SchemaID: string, Bin: binary) -> map

指定したAvroスキーマで `Bin` をデコードします。スキーマレジストリにスキーマを作成しIDを取得します。

### schema_decode(SchemaID: string, Bin: binary, MsgType: string) -> map

指定したProtobufスキーマで `Bin` をデコードします。スキーマレジストリにスキーマを作成しIDを取得します。`MsgType` はProtobufスキーマ内のメッセージタイプを指定します。

### **Sparkplug B 関数**

<<<<<<< HEAD
EMQXはSparkplug Bメッセージのデコード・エンコード用の特別な関数（`spb_decode` と `spb_encode`）も提供しています。詳細は[Sparkplug B](./sparkplug.md)を参照してください。
=======
EMQXはSparkplug Bメッセージのデコード・エンコード用に特別な関数（`spb_decode` と `spb_encode`）も持っています。詳細は [Sparkplug B](./sparkplug.md) をご覧ください。
>>>>>>> origin/release-6.1

## 日時変換関数

### date_to_unix_ts(Unit: string, FormatString: string, DateTimeString: string) -> integer

日時文字列 `DateTimeString` をフォーマット文字列 `FormatString` に従って解析し、指定した時間単位 `Unit` のUnix時間に変換します。

使用可能な `Unit` は `second`, `millisecond`, `microsecond`, `nanosecond` です。

`FormatString` で使用可能なプレースホルダー：

| プレースホルダー | 意味 | 値の範囲 |
| ----------- | ------- | ------------|
| `%Y` | 4桁の年 | 0000 - 9999 |
| `%m` | 2桁の月 | 01 - 12 |
| `%d` | 2桁の日 | 01 - 31 |
| `%H` | 24時間制の2桁の時 | 00 - 24 |
| `%M` | 2桁の分 | 00 - 59 |
| `%S` | 2桁の秒 | 00 - 59 |
| `%N` | ナノ秒 | 000000000 - 999999999 |
| `%6N` | マイクロ秒（ナノ秒の最初の6桁） | 000000 - 999999 |
| `%3N` | ミリ秒（ナノ秒の最初の3桁） | 000 - 999 |
| `%z` | タイムゾーンオフセット（±hhmm） | -1159 - +1159 |
| `%:z` | タイムゾーンオフセット（±hh:mm） | -11:59 - +11:59 |
| `%::z` | タイムゾーンオフセット（±hh:mm:ss） | -11:59:59 - +11:59:59 |

例：

```bash
date_to_unix_ts('second', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00+08:00') = 1708671600
```

### date_to_unix_ts(Unit: string, Offset: string | integer, FormatString: string, DateTimeString: string) -> integer

<<<<<<< HEAD
`DateTimeString` にタイムゾーンオフセットが含まれない場合、`Offset` で手動指定できます。その他の挙動は `date_to_unix_ts/3` と同じです。`Offset` は文字列または秒数の整数で指定可能です。
=======
`DateTimeString` にタイムゾーンオフセットが含まれない場合、`Offset` で手動指定できます。その他の挙動は `date_to_unix_ts/3` と同様です。`Offset` は文字列または秒数の整数で指定可能です。
>>>>>>> origin/release-6.1

文字列の場合、以下の形式が使えます：

- `Z` または `z`：UTCオフセット00:00
- `±hh[:mm][:ss]` または `±hh[mm][ss]`：UTCからの正負の時間オフセット
- `local`：システムのローカルタイムゾーンのオフセット

例：

```bash
date_to_unix_ts('second', '+08:00', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00') = 1708671600
date_to_unix_ts('second', 'Z', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 07:00:00') = 1708671600
date_to_unix_ts('second', 14400, '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00') = 1708686000
```

### format_date(Unit: string, Offset: string | integer, FormatString: string, Time: Integer) -> string

<<<<<<< HEAD
Unix時間 `Time` を指定フォーマットの日時文字列に変換します。`Unit` はUnix時間の単位、`Offset` は出力日時のタイムゾーンオフセット、`FormatString` は出力フォーマットです。
=======
Unix時間 `Time` を指定フォーマットの日時文字列に変換します。`Unit` はUnix時間の単位、`Offset` は出力日時のタイムゾーンオフセット、`FormatString` は出力日時のフォーマットです。
>>>>>>> origin/release-6.1

`date_to_unix_ts/3, 4` を参照してください。

例：

```bash
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%z', 1708933353472) = '2024-02-26 15:42:33.472000+0800'
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%:z', 1708933353472) = '2024-02-26 15:42:33.472000+08:00'
format_date('millisecond', '+08:20:30', '%Y-%m-%d %H:%M:%S.%3N%::z', 1708933353472) = '2024-02-26 16:03:03.472+08:20:30'
format_date('millisecond', 'Z', '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 07:42:33.472+08:00'
format_date('millisecond', 28800, '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 15:42:33.472+08:00'
```

### now_rfc3339() -> string

<<<<<<< HEAD
現在のシステム時刻をRFC3339形式の日時文字列（秒単位）で返します。例：
=======
現在のシステム時刻を秒単位のRFC3339日時文字列で返します。例：
>>>>>>> origin/release-6.1

```bash
now_rfc3339() = '2024-02-23T10:26:20+08:00'
```

### now_rfc3339(Unit: string) -> string

`now_rfc3339/0` と同様ですが、`Unit` で時間単位を指定できます。`second`, `millisecond`, `microsecond`, `nanosecond` をサポート。例：

```bash
now_rfc3339('microsecond') = '2024-02-23T10:26:38.009706+08:00'
```

### now_timestamp() -> integer

現在のシステム時刻をUnixタイムスタンプ（秒単位）で返します。例：

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

`rfc3339_to_unix_ts/1` と同様ですが、`Unit` で返却するUnixタイムスタンプの単位を指定できます。例：

```bash
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'second') = 1708703790
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'millisecond') = 1708703790870
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'microsecond') = 1708703790870000
rfc3339_to_unix_ts('2024-02-23T15:56:30.535904509Z', 'nanosecond') = 1708703790535904509
```

### timezone_to_offset_seconds(Offset: string) -> integer

<<<<<<< HEAD
タイムゾーンオフセット文字列を秒数の整数に変換します。サポートされる形式：
=======
タイムゾーンオフセット文字列を秒数の整数に変換します。対応する形式は以下の通りです：
>>>>>>> origin/release-6.1

- `Z` または `z`：UTCオフセット00:00
- `±hh[:mm][:ss]` または `±hh[mm][ss]`：UTCからの正負の時間オフセット
- `local`：システムのローカルタイムゾーンのオフセット

例：

```bash
timezone_to_offset_seconds('Z') = 0
timezone_to_offset_seconds('+08:00') = 28800
timezone_to_offset_seconds('local') = 28800
```

### unix_ts_to_rfc3339(Time: integer) -> string

<<<<<<< HEAD
Unixタイムスタンプ（秒単位）をシステムのローカルタイムゾーンを用いてRFC3339準拠の日時文字列に変換します。例：
=======
Unixタイムスタンプ（秒単位）をシステムのローカルタイムゾーンのRFC3339準拠日時文字列に変換します。例：
>>>>>>> origin/release-6.1

```bash
unix_ts_to_rfc3339(1708671600) = '2024-02-23T15:00:00+08:00'
```

### unix_ts_to_rfc3339(Time: integer, Unit: string) -> string

`unix_ts_to_rfc3339/0` と同様ですが、`Unit` で時間単位を指定できます。例：

```bash
unix_ts_to_rfc3339(1708671600766, 'millisecond') = '2024-02-23T15:00:00.766+08:00'
```

### MongoDB日時関数

### mongo_date() -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

<<<<<<< HEAD
現在時刻をMongoDBのISODate型または文字列で返します。MongoDB関連アクションおよびSQLテストでのみサポートされます。SQLテストでは文字列（例：`ISODate("2024-02-23T15:00:00.123Z")`）を返します。その他の関数への入力としては文字列以外は現在サポートされていません。
=======
現在時刻をMongoDBのISODate型または文字列で返します。MongoDB関連のアクションやSQLテストでのみサポートされ、SQLテストでは文字列（例：`ISODate("2024-02-23T15:00:00.123Z")`）を返します。文字列以外は他関数の入力としては未対応です。
>>>>>>> origin/release-6.1

例：

```bash
mongo_date() = 'ISODate("2024-02-23T15:00:00.123Z")'
```

### mongo_date(Timestamp: integer) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

<<<<<<< HEAD
指定したミリ秒単位UnixタイムスタンプをMongoDBのISODate型または文字列に変換します。その他は `mongo_date/0` と同様です。
=======
ミリ秒単位のUnixタイムスタンプをMongoDB ISODate型または文字列に変換します。その他は `mongo_date/0` と同様です。
>>>>>>> origin/release-6.1

例：

```bash
mongo_date(now_timestamp('millisecond')) = 'ISODate(2024-02-23T15:48:57.871Z)'
```

### mongo_date(Timestamp: integer, Unit: string) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

<<<<<<< HEAD
指定したUnixタイムスタンプをMongoDBのISODate型または文字列に変換します。`Unit` で入力タイムスタンプの単位を指定できます。その他は `mongo_date/0` と同様です。
=======
UnixタイムスタンプをMongoDB ISODate型または文字列に変換します。`Unit` で入力タイムスタンプの単位を指定可能です。その他は `mongo_date/0` と同様です。
>>>>>>> origin/release-6.1

`Unit` の値：

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

<<<<<<< HEAD
- OS環境変数から読み取る際に `EMQXVAR_` プレフィックスが付加されます。例えば `getenv('FOO_BAR')` は `EMQXVAR_FOO_BAR` を読み取ります。  
=======
- OS環境変数から読み取る際に接頭辞 `EMQXVAR_` が付加されます。例えば、`getenv('FOO_BAR')` は `EMQXVAR_FOO_BAR` を読み取ります。
>>>>>>> origin/release-6.1
- OS環境から読み込んだ値は不変です。

## 条件関数

### coalesce(Value1: any, Value2: any) -> any

<<<<<<< HEAD
`Value1` がnullの場合に `Value2` を返します。データフィールドがnullかどうかを判定し、デフォルト値に置き換える場合に便利です。
=======
`Value1` がnullの場合に `Value2` を返します。データフィールドがnullかどうかを判定し、デフォルト値に置き換えたい場合に便利です。
>>>>>>> origin/release-6.1

例えば、`coalesce(payload.value, 0)` は `payload.value` がnullでなければその値を返し、nullなら0を返します。  
SQL式の `CASE WHEN is_null(payload.value) THEN 0 ELSE payload.value END` と同等ですが簡潔です。

::: tip 注意

EMQXルールSQLではnull値の文字列表現はデフォルトで `'undefined'` です。

:::

### coalesce_ne(Value1: any, Value2: any) -> any

`coalesce` に似ていますが、`Value1` がnullまたは空文字列の場合に `Value2` を返します。

::: tip 注意

EMQXルールSQLではnull値の文字列表現はデフォルトで `'undefined'` です。

:::
