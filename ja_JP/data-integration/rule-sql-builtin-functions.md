# 組み込みSQL関数

ルールエンジンはさまざまな組み込み関数を提供しています。これらの関数はSQL内で利用でき、基本的なデータ処理を実現します。以下のカテゴリがあります：

- [数学関数](#mathematical-functions)
- [データ型判定関数](#data-type-judgment-functions)
- [データ型変換関数](#data-type-conversion-functions)
- [文字列操作関数](#string-operation-functions)
- [マップ操作関数](#map-operation-functions)
- [配列操作関数](#array-operation-functions)
- [ハッシュ関数](#hashing-functions)
- [圧縮・解凍関数](#compression-and-decompression-functions)
- [ビット演算関数](#bit-operation-functions)
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

例えば、`acos(X: integer | float) -> float` は引数 `X` のデータ型が整数または浮動小数点数であり、戻り値の型は浮動小数点数であることを意味します。

引数が指定範囲を超えたりサポートされていないデータ型を使用した場合、現在のSQL実行は失敗し、失敗回数が1増加しますのでご注意ください。

:::tip

1. 一部のエスケープシーケンスは使用時にアンエスケープが必要です。詳細は [unescape関数](#unescapestring-string---string) を参照してください。  
2. EMQX 5.0以降、複雑なデータ変換には [jq構文](https://stedolan.github.io/jq/manual/) の使用もサポートしています。詳細は [jq関数](./rule-sql-jq.md) セクションをご覧ください。

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
浮動小数点数の絶対値演算には `ceil` または `floor` 関数の使用を推奨します。
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

自然対数の底 e の `X` 乗を返します。例：

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

`[0, 1)` の範囲でランダムな浮動小数点数を返します。例：

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

指定したフィールドのデータ型を判定し、指定したデータ型に合致するかどうかを真偽値で返します。

### is_array(Term: any) -> boolean

`Term` が配列型かどうかを判定します。例：

```bash
is_array([1, 2]) = true
is_array(json_decode('[{"value": 1}]')) = true
is_array(json_decode('{"value": 1}')) = false
is_array(0.5) = false
is_array('[1, 2]') = false
```

### is_bool(Term: any) -> boolean

`Term` がブール型かどうかを判定します。例：

```bash
is_bool(true) = true
is_bool(false) = false
is_bool('true') = false
```

### is_float(Term: any) -> boolean

`Term` が浮動小数点型かどうかを判定します。例：

```bash
is_float(123.4) = true
is_float(123) = false
```

### is_int(Term: any) -> boolean

`Term` が整数型かどうかを判定します。例：

```bash
is_int(123) = true
is_int(123.4) = false
```

### is_map(Term: any) -> boolean

`Term` がマップ型かどうかを判定します。例：

```bash
is_map(json_decode('{"value": 1}')) = true
is_map(json_decode('[{"value": 1}]')) = false
```

### is_null(Term: any) -> boolean

変数 `Term` が未定義かどうかを判定します。値がJSONの `null` であっても割り当て済みとみなします。

例：

```sql
is_null(this_is_an_unassigned_variable) = true
is_null(map_get('b', json_decode('{"a": 1}'))) = true
is_null(map_get('b', json_decode('{"b": null}'))) = false
```

### is_null_var(Term: any) -> boolean

変数 `Term` が未定義または `null` かどうかを判定します。例：

```sql
is_null_var(this_is_an_unassigned_variable) = true
is_null_var(map_get('b', json_decode('{"a": 1}'))) = true
is_null_var(map_get('b', json_decode('{"b": null}'))) = true
```

### is_not_null_var(Term: any) -> boolean

`is_null_var` の逆で、変数 `Term` が定義済みかつ `null` でないかを判定します。

### is_num(Term: any) -> boolean

`Term` が整数または浮動小数点型かどうかを判定します。例：

```bash
is_num(123) = true
is_num(123.4) = true
is_num('123') = false
```

### is_str(Term: any) -> boolean

`Term` が文字列型かどうかを判定します。例：

```bash
is_str('123') = true
is_str(123) = false
```

### is_empty(Array or Map) -> boolean

配列またはマップが空かどうかを判定します。例：

```bash
is_empty(json_decode('{}')) = true
is_empty('{}') = true
is_empty('{"key" : 1}') = false
is_empty(map_get('key', '{"key" : []}')) = true
is_empty(map_get('key', '{"key" : [1}')) = false
```

## データ型変換関数

### bool(Term: boolean | integer | string) -> boolean

`Term` をブール型に変換します。`Term` はブール型、整数型の0または1、または文字列型の `true` または `false` のみ許容されます。

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

`Term` が文字列の場合、指数表記も使用可能です（例：`float('3.14e4')`）。浮動小数点数は最大16桁の有効数字をサポートします。文字列で表現された浮動小数点数の有効数字が16桁を超えると、変換時に丸め誤差が発生する可能性があります。

例：

```bash
float(20) = 20.0

float('3.14') = 3.14
float('3.14e4') = 31400
float('3.14e+4') = 31400
float('3.14e-4') = 0.000314
float('3.14E-4') = 0.000314

# 有効数字が16桁を超えると丸め誤差により異なる入力が同じ出力になることがある
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

浮動小数点数 `Float` を小数点以下最大 `Decimals` 桁の文字列に変換し、末尾のゼロは切り捨てます。`Decimals` の範囲は `[0, 253]` です。`Float` の有効数字が16桁を超えると変換時に丸め誤差が発生する可能性があります。

浮動小数点数はコンピュータ上で正確に格納できないため、`Decimals` が `Float` の小数点以下の桁数（先行ゼロ含む）より大きい場合、`float2str` は `Float` の2進近似値の10進表現を返すことがあります。

例：

```bash
float2str(0.1, 5) = '0.1'
float2str(0.1, 20) = '0.10000000000000000555'
float2str(0.1, 25) = '0.1000000000000000055511151'
float2str(0.00000000001, 20) = '0.00000000001'

# 末尾のゼロは切り捨てられる
float2str(0.100001, 5) = '0.1'

# 有効数字が16桁を超えると丸め誤差により異なる入力が同じ出力になることがある
float2str(123456789.01234565, 8) = '123456789.01234566'
float2str(123456789.01234566, 8) = '123456789.01234566'
```

### int(Term: boolean | float | integer | string) -> integer

`Term` を整数に変換します。

- `Term` がブール型の場合、`true` は1、`false` は0に変換されます。  
- `Term` が浮動小数点型の場合、`Term` 以下の最大整数に切り捨てられます。  
- `Term` が文字列の場合、少なくとも1つの数字を含み、先頭に `+` または `-` の単一文字の接頭辞があってもよく、先行ゼロは無視されます。数学的表記もサポートされます。  
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

- `Term` がマップまたは配列の場合、`str` 関数は `Term` をJSONエンコードしようとします。  
- `Term` が浮動小数点数の場合、末尾のゼロを切り捨てた対応する文字列を返します。返される文字列は小数点以下最大10桁まで保持します。より多くの小数桁を返すには `float2str` 関数を使用してください。

例：

```bash
str(100) = '100'
str(nth(1, json_decode('[false]'))) = 'false'
str(json_decode({"msg": "hello"})) = '{"msg":"hello"}'
str(json_decode('[{"msg": "hello"}]')) = '[{"msg":"hello"}]'

# 末尾のゼロは切り捨てられる
# 小数点以下最大10桁まで保持
str(0.30000000040) = '0.3000000004'
str(0.30000000004) = '0.3'

# 小数点以下10桁で丸められる
# 10桁目以降で丸め
str(3.14159265359) = '3.1415926536'
str(0.000000314159265359) = '0.0000003142'
```

### str_utf8(Term: any) -> string

任意の `Term` をUTF-8エンコードされた文字列に変換します。

その他の挙動は `str(Any)` と同一です。

```bash
str_utf8(100) = '100'
str_utf8(nth(1, json_decode('[false]'))) = 'false'
str_utf8(json_decode({"msg": "hello"})) = '{"msg":"hello"}'
str_utf8(json_decode('[{"msg": "hello"}]')) = '[{"msg":"hello"}]'

# 末尾のゼロは切り捨てられる
# 小数点以下最大10桁まで保持
str_utf8(0.30000000040) = '0.3000000004'
str_utf8(0.30000000004) = '0.3'

# 小数点以下10桁で丸められる
# 10桁目以降で丸め
str_utf8(3.14159265359) = '3.1415926536'
str_utf8(0.000000314159265359) = '0.0000003142'
```

### str_utf16_le(Term: any) -> binary

任意の `Term` をUTF-16リトルエンディアンエンコードされたバイナリ文字列に変換します。

::: tip

UTF-16リトルエンディアンエンコード文字列はJSONオブジェクト内で正しく表示されないことがあります。EMQXでは通常バイナリデータとして扱われます。読みやすい16進数文字列に変換するには `bin2hexstr` 関数を使用してください。  
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

文字列の大文字小文字変換、空白除去、部分文字列抽出、置換、エスケープ／アンエスケープなどに使用します。

### ascii(Char: string) -> integer

文字 `Char` のASCIIコードを返します。`Char` が複数文字の場合は最初の文字のコードのみ返します。例：

```bash
ascii('a') = 97
ascii('abc') = 97
```

### concat(Str1: string, Str2: string) -> string

`Str1` と `Str2` を連結して1つの文字列にします。例：

```bash
concat('Name:', 'John') = 'Name:John'
```

### find(String: string, SearchPattern: string) -> string

`String` 内で部分文字列 `SearchPattern` を検索し、`SearchPattern` より前の部分を削除して残りを返します。`SearchPattern` が見つからない場合は空文字列を返します。この関数は `find(String, SearchPattern, 'leading')` と同等です。

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

`Array` の要素を区切り文字 `Sep` で連結して1つの文字列にします。例：

```bash
join_to_string(', ', ['a', 'b', 'c']) = 'a, b, c'
```

### lower(String: string) -> string

文字列 `String` の大文字を小文字に変換します。例：

```bash
lower('Hello') = 'hello'
```

### ltrim(String: string) -> string

`trim/1` と同様ですが、先頭の空白文字のみを削除します。例：

```bash
ltrim('\t  hello  \n') = 'hello  \n'
ltrim('\t  hello \r\n') = 'hello  \r\n'
```

### pad(String: string, Length: integer) -> string

`String` の末尾にスペースを追加し、指定した長さ `Length` にします。例：

```bash
pad('hello', 8) = 'hello   '
```

### pad(String: string, Length: integer, Direction: string) -> string

`pad/2` と同様ですが、`Direction` でパディング方向を指定できます。`leading` は先頭にスペースを追加、`trailing` は末尾に追加、`both` は両端に追加します。

`both` 指定時、追加するスペース数が奇数の場合は最後のスペースが末尾に追加されます。

例：

```bash
pad('hello', 8, 'leading') = '   hello'
pad('hello', 8, 'trailing') = 'hello   '
pad('hello', 8, 'both') = ' hello  '
```

### pad(String: string, Length: integer, Direction: string, Char: string) -> string

`pad/3` と同様ですが、指定したグラフェムクラスタ `Char` でパディングします。

ルールエンジンは `Char` が合法なグラフェムクラスタかどうかをチェックしないため、`Char` の文字数に関わらず1文字分として扱います。例：

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

正規表現 `Expression` にマッチした部分を文字列 `Replacement` に置換します。マッチしない場合は元の文字列を返します。例：

```bash
regex_replace('hello 123', '\d+', 'world') = 'hello world'
regex_replace('a;b; c', ';\s*', ',') = 'a,b,c'
```

### regex_extract(String: string, Expression: string) -> [string]

::: tip

この関数はEMQX v5.7.1以降で導入されました。

:::

正規表現パターンのキャプチャグループを非グローバルに検索し、マッチした部分のキャプチャグループをリストで返します。マッチしない場合やキャプチャグループがない場合は空リストを返します。

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

- `all`: すべての `SearchPattern` を置換（`replace/3` と同等）
- `leading`: 先頭の `SearchPattern` のみ置換
- `trailing`: 末尾の `SearchPattern` のみ置換

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

文字列 `String` の先頭にある `Prefix` を削除します。`String` が `Prefix` で始まらない場合は元の文字列を返します。例：

```bash
rm_prefix('foo/bar', 'foo/') = 'bar'
rm_prefix('foo/bar', 'xxx/') = 'foo/bar'
```

### rtrim(String: string) -> string

`trim/1` と同様ですが、末尾の空白文字のみを削除します。例：

```bash
rtrim('\t  hello  \n') = '\t  hello'
rtrim('\t  hello \r\n') = '\t  hello'
```

### split(String: string, Separator: string) -> array

`String` を区切り文字 `Separator` で分割し、部分文字列の配列を返します。

連続する複数の区切り文字は1つとして扱われず、分割結果に空文字列が含まれることがあります。`split/2` はデフォルトで結果をトリムし、空文字列を除外します。空文字列を保持したい場合は `split(String, Separator, 'notrim')` を使用してください。

`Separator` は複数文字でも構い、それらは1つのまとまりとして扱われます。複数の区切り文字を同時に指定したい場合は `tokens` 関数を使用してください。

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

`split/2` と同様ですが、`Option` で処理する区切り文字の位置や空文字列の返却を指定できます。

`Option` の値は以下の通りです：

- `notrim`: 文字列中のすべての区切り文字を処理し、空文字列を含む可能性あり
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

`Format` に従ってフォーマットされた文字列を返します。`Format` 文字列は通常の文字と制御シーケンスを含みます。

制御シーケンスの形式は一般に `~F.P.PadModC` です。

`C` は制御シーケンスの種類を決定し、唯一必須のフィールドです。`F`, `P`, `Pad`, `Mod` はすべて省略可能です。詳細は https://www.erlang.org/doc/apps/stdlib/io.html#fwrite-1 を参照してください。

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

`String` の位置 `Start` から末尾までの部分文字列を返します。文字列の添字は0始まりで、例えば "hello" の位置0は "h" です。例：

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

`String` を `SeparatorList` に含まれる任意の文字で分割し、部分文字列のリストを返します。

連続する複数の区切り文字は1つとして扱われ、空文字列は発生しません。

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

`String` の先頭と末尾の空白文字（スペース、タブ、フォームフィード、改行など）を削除します。Unicode標準で `\r\n` はグラフェムクラスタとして扱われるため、`\r\n` はまとめて削除されます。例：

```bash
trim('\t  hello  \n') = 'hello'
trim('\t  hello \r\n') = 'hello'
```

### unescape(String: string) -> string

エスケープシーケンスを元の文字に戻します。SQL内でエスケープシーケンスを使用する場合は、正しく処理するためにこの関数でアンエスケープしてください。

::: tip

この関数はEMQX v5.7.0以降で導入されました。

:::

例えば、ペイロードが改行区切りの文字列の場合：

```bash
32A48702-1FA6-4E7C-97F7-8EA3EA48E8A3
87.2
12.3
my-device
```

`\n` でペイロードを分割したい場合、以下のSQLは期待通り動作しません：

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

`unescape` 関数で `\n` をアンエスケープすると期待通りの結果が得られます：

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

**`unescape` 関数がサポートするエスケープシーケンス：**

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

  - `\xH...`：`H...` は1文字以上の16進数字（0-9, A-F, a-f）で、任意のUTF-32文字をエンコード可能。

認識できないエスケープシーケンスや無効なUnicode文字の場合は例外がスローされます。

### upper(String: string) -> string

`String` の小文字を大文字に変換します。例：

```bash
upper('hello') = 'Hello'
```

## マップ操作関数

### map_get(Key: string, Map: map) -> any

`Map` の指定した `Key` の値を返します。`Key` が存在しない場合は `undefined` を返します。例：

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

### map_put(Key: string, Value: any, Map: map) -> map

`Map` に `Key` と対応する `Value` を挿入し、更新済みのマップを返します。`Key` が既に存在する場合は値を上書きします。例：

```bash
map_get('b', map_put('b', 1, json_decode('{"a": 1}'))) = 1
map_get('a', map_put('a', 2, json_decode('{"a": 1}'))) = 2
```

### map_to_redis_hset_args(Map) -> list

::: tip

この関数はEMQX v5.7.1以降で導入されました。

:::

マップをRedisの `HSET`（または `HMSET`）コマンド用のフィールド名と値のリストに変換します。

例：`SELECT map_to_redis_hset_args(payload.value) as hset_fields FROM t/1` のように使用し、Redisアクションのテンプレートで `HMSET name1 ${hset_fields}` として利用できます。

例えば、`payload.value` が `{"a" : 1, "b": 2}` の場合、生成されるコマンドは `HMSET name1 b 2 a 1` となります。マップのフィールド順序は非決定的です。

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

`Map` の指定した `Key` の値を返します。`Key` が存在しない場合は `undefined` を返します。配列を指定するとネストしたマップから複数キーを一度に取得できます。例：

```bash
mget('c', json_decode('{"a": {"b": 1}}')) = undefined
json_decode(mget('a', json_decode('{"a": {"b": 1}}'))) = '{"b": 1}'
mget(['a', 'b'], json_decode('{"a": {"b": 1}}')) = 1
```

### mput(Key: string | array, Value: any, Map: map) -> map

`Map` に `Key` と対応する `Value` を挿入し、更新済みのマップを返します。`Key` が既に存在する場合は値を上書きします。配列を指定するとネストしたマップに複数キーを一度に挿入できます。例：

```bash
mget(['a', 'b'], mput(['a', 'b'], 2, json_decode('{"a": {"b": 1}}'))) = 2
mget(['a', 'b'], mput(['a', 'b'], 2, json_decode('{"c": 1}'))) = 2
```

### map_size(Map: map) -> any

`Map` のキーの数を返します。例：

```bash
map_size(json_decode('{}')) = 0
map_size(json_decode('{"msg": "hello"}')) = 1
```

## 配列操作関数

### contains(Item: any, Array: array) -> boolean

配列 `Array` が指定した `Item` を含むか判定します。例：

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

# エラー
first([])
```

### last(Array: array) -> any

配列 `Array` の最後の要素を返します。`Array` は空であってはいけません。例：

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

配列 `Array` のN番目の要素を返します。`N` は配列の長さを超えてはいけません。例：

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

任意長の文字列 `String` の128ビット固定長MD5ハッシュ値を計算します。ハッシュ値は32桁の16進数文字列で返され、小文字（a〜f）固定です。

例：

```bash
md5('hello') = '5d41402abc4b2a76b9719d911017c592'
```

### sha(String: string) -> string

任意長の文字列 `String` の160ビット固定長SHA-1ハッシュ値を計算します。ハッシュ値は40桁の16進数文字列で返され、小文字（a〜f）固定です。

例：

```bash
sha('hello') = 'aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d'
```

### sha256(String: string) -> string

任意長の文字列 `String` の256ビット固定長SHA-2ハッシュ値を計算します。ハッシュ値は64桁の16進数文字列で返され、小文字（a〜f）固定です。

例：

```bash
sha256('hello') = '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824'
```

### hash_to_range(Value: string, Min: integer, Max: integer) -> integer

EMQX 6.2.3以降で導入。

`Value` をSHA-256でハッシュし、ハッシュ値を `[Min, Max]` の範囲の整数にマッピングします。`Min` は `Max` 以下である必要があります。

メッセージフィールドから安定したバケットやシャード番号を取得したい場合に有用です。例えば、トピックのセグメントでデバイスを複数のルールやアクションに分散する際など。

例：

```bash
hash_to_range('A_C001', 0, 3) = 0
hash_to_range(nth(2, tokens(topic, '/')), 0, 3)
```

### map_to_range(Value: integer | string, Min: integer, Max: integer) -> integer

EMQX 6.2.3以降で導入。

`Value` を `[Min, Max]` の範囲の整数にマッピングします。`Min` は `Max` 以下である必要があります。

`Value` が整数の場合は直接マッピングし、非空文字列の場合はバイナリ表現を符号なし整数に変換してからマッピングします。

例：

```bash
map_to_range(7, 0, 3) = 3
map_to_range('a', 0, 3) = 1
```

## 圧縮・解凍関数

注意：バイナリデータは直接JSONエンコードできないため、16進文字列に変換するには `bin2hexstr` 関数を使用してください。

### gunzip(Data: binary) -> binary | string

`Data` を解凍します。`Data` はgzヘッダと末尾のチェックサムを含む必要があります。例：

```bash
gunzip(hexstr2bin('1F8B0800000000000013CB48CDC9C9070086A6103605000000')) = 'hello'
```

### gzip(Data: binary | string) -> binary

DEFLATEアルゴリズムで `Data` を圧縮し、返される圧縮結果はgzヘッダと末尾のチェックサムを含みます。例：

```bash
bin2hexstr(gzip('hello')) = '1F8B0800000000000013CB48CDC9C9070086A6103605000000'
```

### unzip(Data: binary) -> binary | string

`Data` を解凍します。`Data` はzlibヘッダと末尾のチェックサムを含まない必要があります。例：

```bash
unzip(hexstr2bin('CB48CDC9C90700')) = 'hello'
```

### zip(Data: binary | string) -> binary

DEFLATEアルゴリズムで `Data` を圧縮し、返される圧縮結果はzlibヘッダと末尾のチェックサムを含みません。例：

```bash
bin2hexstr(zip('hello')) = 'CB48CDC9C90700'
```

### zip_compress(Data: binary | string) -> binary

DEFLATEアルゴリズムで `Data` を圧縮します。返される圧縮結果はzlibヘッダと末尾のチェックサムを含みます。例：

```bash
bin2hexstr(zip_compress('hello')) = '789CCB48CDC9C90700062C0215'
```

### zip_uncompress(Data: binary) -> binary | string

`Data` を解凍します。`Data` はzlibヘッダと末尾のチェックサムを含む必要があります。例：

```bash
zip_uncompress(hexstr2bin('789CCB48CDC9C90700062C0215')) = 'hello'
```

## ビット演算関数

### bitand(Num1: integer, Num2: integer) -> integer

`Num1` と `Num2` のビットごとのANDを返します。入力と出力は符号付き整数です。例：

```bash
bitand(10, 8) = 8
bitand(-10, -8) = -16
```

### bitnot(Num: integer) -> integer

`Num` のビットごとの否定を返します。入力と出力は符号付き整数です。例：

```bash
bitnot(10) = -11
bitnot(-12) = 11
```

### bitsl(Num: integer, Shift: integer) -> integer

`Num` を左に `Shift` ビットシフトし、右端は0で埋めます。例：

```bash
bitsl(8, 2) = 32
bitsl(-8, 2) = -32
```

### bitsr(Num: integer, Shift: integer) -> integer

`Num` を右に `Shift` ビットシフトし、左端は符号ビットで埋めます（正数は0、負数は1）。例：

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

ルールエンジンはビット列操作用の関数を提供しています。例えば `subbits` はビット列から指定長のビットを抽出し、指定データ型に変換します。

:::tip

`binary` 型はバイト列を表し、1バイトは8ビットなので、`binary` のビット数は8の倍数でなければなりません。  
`bitstring` 型は任意長のビット列を表し、8の倍数でなくてもよいです。

つまり、すべての `binary` は `bitstring` ですが、逆は必ずしも真ではありません。

`bitstring` は長さが8の倍数でない場合、JSONなどの外部フォーマットに直接シリアライズできません。通常は整数などに変換する前の中間値として使用されます。

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

バイト列 `Bin` の先頭から長さ `BitNum` のビットを抽出し、ビッグエンディアン順で符号なし整数に変換します。これは `subbits(Bytes, 1, BitNum, 'integer', 'unsigned', 'big')` と同等です。

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

バイト列 `Bin` の位置 `Start`（1始まり）から長さ `BitNum` のビットを抽出し、ビッグエンディアン順で符号なし整数に変換します。これは `subbits(Bytes, Start, BitNum, 'integer', 'unsigned', 'big')` と同等です。

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

バイト列 `Bin` の位置 `Start`（1始まり）から長さ `BitNum` のビットを抽出し、指定されたバイト順 `Endianness` と符号属性 `Signedness` に従い、指定型 `OutputType` に変換します。

`OutputType` の値：

- bits：bitstringの略
- integer
- float

`Signedness` の値：

- signed
- unsigned

`Endianness` の値：

- big
- little

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

`Data` をBase64形式からデコードします。例：

```bash
base64_decode('aGVsbG8=') = 'hello'
bin2hexstr(base64_decode('y0jN')) = 'CB48CD'
```

### base64_decode(Data: string, Option1: string, ...) -> bytes | string

::: tip

このオプション付き関数はEMQX 6.0.2以降で導入されました。

:::

オプション付きでBase64形式の `Data` をデコードします。

**オプション：**

- `no_padding`：パディング文字 `=` を期待せずにデコードします。パディングなしBase64文字列に有効です。  
- `urlsafe`：URLセーフBase64形式（`+` と `/` の代わりに `-` と `_`）でデコードします。

これらのオプションは単独または組み合わせて使用可能で、順序は結果に影響しません。

例：

```sql
-- URLセーフBase64をデコード
SELECT base64_decode(payload, 'urlsafe') as decoded FROM "t/#"

-- パディングなしURLセーフBase64をデコード
SELECT base64_decode(payload, 'urlsafe', 'no_padding') as decoded FROM "t/#"
```

### base64_encode(Data: binary | string) -> string

`Data` をBase64形式にエンコードします。例：

```bash
base64_encode('hello') = 'aGVsbG8='
base64_encode(hexstr2bin('CB48CD')) = 'y0jN'
```

### base64_encode(Data: binary | string, Option1: string, ...) -> string

::: tip

このオプション付き関数はEMQX 6.0.2以降で導入されました。

:::

オプション付きでBase64形式にエンコードします。

**オプション：**

- `no_padding`：パディング文字 `=` を付けずにエンコードします。  
- `urlsafe`：URLセーフBase64形式（`+` と `/` の代わりに `-` と `_`）でエンコードします。URLに埋め込みやすくなります。

これらのオプションは単独または組み合わせて使用可能で、順序は結果に影響しません。

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

任意のバイナリデータをMicrosoft SQL Serverのバイナリ型に変換します。`0x` プレフィックス付きのHEXエンコード文字列となります。

::: tip

この関数はMicrosoft SQL Serverの `CONVERT` 関数と組み合わせて、UTF-16リトルエンディアンエンコードされたUnicode文字列をUTF-8非対応のSQL Serverバージョンに書き込む際に使用できます。

:::

```
sqlserver_bin2hexstr('hello') = '0x68656C6C6F'
sqlserver_bin2hexstr(str_utf16_le('hello')) = '0x680065006C006C006F00'
sqlserver_bin2hexstr(str_utf16_le('你好')) = '0x604F7D59'
```

### スキーマレジストリ関数

EMQXは指定したスキーマに従い、[Protobuf (Protocol Buffers)](https://developers.google.com/protocol-buffers) および [Avro](https://avro.apache.org/) データのエンコード・デコードを行う `schema_encode` と `schema_decode` 関数もサポートしています。詳細は [スキーマレジストリ](./schema-registry.md) を参照してください。

### schema_encode(SchemaID: string, Data: map) -> binary

指定したAvroスキーマで `Data` をエンコードします。スキーマレジストリでスキーマを作成しIDを取得してください。

### schema_encode(SchemaID: string, Data: map, MsgType: string) -> binary

指定したProtobufスキーマで `Data` をエンコードします。スキーマレジストリでスキーマを作成しIDを取得してください。`MsgType` はProtobufスキーマ内の `Data` に対応するメッセージタイプを指定します。

### schema_decode(SchemaID: string, Bin: binary) -> map

指定したAvroスキーマで `Bin` をデコードします。スキーマレジストリでスキーマを作成しIDを取得してください。

### schema_decode(SchemaID: string, Bin: binary, MsgType: string) -> map

指定したProtobufスキーマで `Bin` をデコードします。スキーマレジストリでスキーマを作成しIDを取得してください。`MsgType` はProtobufスキーマ内の `Data` に対応するメッセージタイプを指定します。

### **Sparkplug B関数**

EMQXはSparkplug Bメッセージのデコード・エンコード用の特殊関数（`spb_decode` と `spb_encode`）も備えています。詳細は [Sparkplug B](./sparkplug.md) を参照してください。

## 日時変換関数

### date_to_unix_ts(Unit: string, FormatString: string, DateTimeString: string) -> integer

日時文字列 `DateTimeString` をフォーマット文字列 `FormatString` に従って解析し、指定された時間単位 `Unit` のUnix時間に変換します。

利用可能な `Unit` は `second`, `millisecond`, `microsecond`, `nanosecond` です。

`FormatString` で使用可能なプレースホルダーは以下の通りです：

| プレースホルダー | 意味 | 値の範囲 |
| ----------- | ------- | ------------|
| `%Y` | 4桁の年 | 0000 - 9999 |
| `%m` | 2桁の月 | 01 - 12 |
| `%d` | 2桁の日 | 01 - 31 |
| `%H` | 24時間制の2桁の時 | 00 - 24 |
| `%M` | 2桁の分 | 00 - 59 |
| `%S` | 2桁の秒 | 00 - 59 |
| `%N` | ナノ秒 | 000000000 - 999999999 |
| `%6N` | マイクロ秒（ナノ秒の先頭6桁） | 000000 - 999999 |
| `%3N` | ミリ秒（ナノ秒の先頭3桁） | 000 - 999 |
| `%z` | タイムゾーンオフセット（±hhmm形式） | -1159 - +1159 |
| `%:z` | タイムゾーンオフセット（±hh:mm形式） | -11:59 - +11:59 |
| `%::z` | タイムゾーンオフセット（±hh:mm:ss形式） | -11:59:59 - +11:59:59 |

例：

```bash
date_to_unix_ts('second', '%Y-%m-%d %H:%M:%S%:z', '2024-02-23 15:00:00+08:00') = 1708671600
```

### date_to_unix_ts(Unit: string, Offset: string | integer, FormatString: string, DateTimeString: string) -> integer

`DateTimeString` にタイムゾーンオフセットが含まれない場合、`Offset` で手動指定できます。その他の挙動は `date_to_unix_ts/3` と同じです。`Offset` は文字列または秒数の整数で指定可能です。

文字列の場合、以下の形式が使えます：

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

Unix時間 `Time` を指定フォーマットの日時文字列に変換します。`Unit` はUnix時間の単位、`Offset` は出力日時のタイムゾーンオフセット、`FormatString` は出力日時のフォーマットを表します。

`date_to_unix_ts/3, 4` の `Unit`, `Offset`, `FormatString` と同じ値が使用可能です。

例：

```bash
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%z', 1708933353472) = '2024-02-26 15:42:33.472000+0800'
format_date('millisecond', '+08:00', '%Y-%m-%d %H:%M:%S.%6N%:z', 1708933353472) = '2024-02-26 15:42:33.472000+08:00'
format_date('millisecond', '+08:20:30', '%Y-%m-%d %H:%M:%S.%3N%::z', 1708933353472) = '2024-02-26 16:03:03.472+08:20:30'
format_date('millisecond', 'Z', '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 07:42:33.472+08:00'
format_date('millisecond', 28800, '%Y-%m-%d %H:%M:%S.%3N%:z', 1708933353472) = '2024-02-26 15:42:33.472+08:00'
```

### now_rfc3339() -> string

現在のシステム時刻を秒単位のRFC3339日時文字列で返します。例：

```bash
now_rfc3339() = '2024-02-23T10:26:20+08:00'
```

### now_rfc3339(Unit: string) -> string

`now_rfc3339/0` と同様ですが、`Unit` で時間単位を指定できます。`second`, `millisecond`, `microsecond`, `nanosecond` をサポート。例：

```bash
now_rfc3339('microsecond') = '2024-02-23T10:26:38.009706+08:00'
```

### now_timestamp() -> integer

現在のシステム時刻を秒単位のUnixタイムスタンプで返します。例：

```bash
now_timestamp() = 1708913853
```

### now_timestamp(Unit: string) -> integer

`now_timestamp/0` と同様ですが、`Unit` で時間単位を指定できます。`second`, `millisecond`, `microsecond`, `nanosecond` をサポート。例：

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

`rfc3339_to_unix_ts/1` と同様ですが、`Unit` で返却するUnixタイムスタンプの単位を指定できます。`second`, `millisecond`, `microsecond`, `nanosecond` をサポート。例：

```bash
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'second') = 1708703790
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'millisecond') = 1708703790870
rfc3339_to_unix_ts('2024-02-23T15:56:30.87Z', 'microsecond') = 1708703790870000
rfc3339_to_unix_ts('2024-02-23T15:56:30.535904509Z', 'nanosecond') = 1708703790535904509
```

### timezone_to_offset_seconds(Offset: string) -> integer

タイムゾーンオフセットの文字列を秒数の整数に変換します。以下の形式をサポートします：

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

秒単位のUnixタイムスタンプをシステムのローカルタイムゾーンでRFC3339準拠の日時文字列に変換します。例：

```bash
unix_ts_to_rfc3339(1708671600) = '2024-02-23T15:00:00+08:00'
```

### unix_ts_to_rfc3339(Time: integer, Unit: string) -> string

`unix_ts_to_rfc3339/0` と同様ですが、`Unit` で時間単位を指定できます。`second`, `millisecond`, `microsecond`, `nanosecond` をサポート。例：

```bash
unix_ts_to_rfc3339(1708671600766, 'millisecond') = '2024-02-23T15:00:00.766+08:00'
```

### MongoDB時間関数

### mongo_date() -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

現在時刻をMongoDBのISODate型または文字列で返します。MongoDB関連のアクションやSQLテストでのみサポートされ、SQLテストでは文字列（例：`ISODate("2024-02-23T15:00:00.123Z")`）を返します。その他の関数の入力としては文字列以外は現在サポートされていません。

例：

```bash
mongo_date() = 'ISODate("2024-02-23T15:00:00.123Z")'
```

### mongo_date(Timestamp: integer) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

指定したミリ秒単位UnixタイムスタンプをMongoDBのISODate型または文字列に変換します。その他の挙動は `mongo_date/0` と同じです。

例：

```bash
mongo_date(now_timestamp('millisecond')) = 'ISODate(2024-02-23T15:48:57.871Z)'
```

### mongo_date(Timestamp: integer, Unit: string) -> [MongoDB ISODate](https://www.mongodb.com/docs/manual/reference/method/Date/) | string

指定したUnixタイムスタンプをMongoDBのISODate型または文字列に変換します。`Unit` で入力タイムスタンプの単位を指定できます。その他の挙動は `mongo_date/0` と同じです。

利用可能な `Unit`：

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

- OS環境変数を読み取る際に接頭辞 `EMQXVAR_` が付加されます。例えば `getenv('FOO_BAR')` は `EMQXVAR_FOO_BAR` を読み取ります。  
- OS環境変数から読み込んだ値は不変です。

## 条件関数

### coalesce(Value1: any, Value2: any) -> any

`Value1` がnullの場合に `Value2` を返します。データフィールドがnullかどうかをチェックし、デフォルト値に置き換えたい場合に便利です。

例えば、`coalesce(payload.value, 0)` は `payload.value` がnullでなければその値を返し、nullなら0を返します。SQL式の `CASE WHEN is_null(payload.value) THEN 0 ELSE payload.value END` と同等ですが簡潔です。

::: tip 注意

EMQXルールSQLではnull値の文字列表現はデフォルトで `'undefined'` です。

:::

### coalesce_ne(Value1: any, Value2: any) -> any

`coalesce` と似ていますが、`Value1` がnullまたは空文字列の場合に `Value2` を返します。

::: tip 注意

EMQXルールSQLではnull値の文字列表現はデフォルトで `'undefined'` です。

:::
