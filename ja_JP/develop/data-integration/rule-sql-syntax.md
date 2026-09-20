# Rule SQL リファレンス

EMQX は、データの抽出、フィルタリング、拡張、変換のためにルールで SQL ベースの構文を使用します。この SQL ライクな構文には、`SELECT` と `FOREACH` の2種類のステートメントがあります。

| ステートメント | 説明                                                         |
| -------------- | ------------------------------------------------------------ |
| `SELECT`       | SQL ステートメントの結果が単一のメッセージとなる場合に使用します。 |
| `FOREACH`      | 単一の入力メッセージからゼロ個以上のメッセージを生成する場合に使用します。 |

各ルールは正確に1つのステートメントを持つことができます。SQL ステートメントは豊富な組み込み関数を提供しており、簡単な変換やタイムスタンプの作成などが可能です。

また、SQL ステートメントは式の中に [jq プログラム](https://stedolan.github.io/jq/) を埋め込むことをサポートしており、必要に応じて複雑なデータ変換を行えます。式は `SELECT` と `FOREACH` ステートメントの中に埋め込むことができます。`SELECT` と `FOREACH` ステートメントで参照可能なフィールドについては、[データソースとフィールド](./rule-sql-events-and-fields.md) を参照してください。

## `SELECT` ステートメント

`SELECT` ステートメントは、入力メッセージから特定のフィールドを選択し、フィールド名の変更、データ変換、条件に基づくメッセージのフィルタリングを行います。

ルールエンジンの SQL における `SELECT` ステートメントの基本形式は以下の通りです。

```sql
SELECT <fields_expressions> FROM <topic> [WHERE <conditions>]
```

`SELECT` 句で出力に含めるフィールド（メッセージのペイロードおよびメタデータの両方から）を指定し、`WHERE` 句で特定の条件に基づいてメッセージをフィルタリングできます。

### `FROM` 句

`FROM` 句はクエリのデータソースを指定します。特定のトピックや条件に合致するイベントからデータを選択できます。

#### トピックによる選択

例えば、トピックパターン `t/#` と `my/other/topic` にパブリッシュされたすべてのメッセージに適用するルールを定義したい場合、以下のように記述します。

```sql
SELECT clientid, payload.clientid as myclientid FROM "t/#", "my/other/topic"
```

ここで、

- `SELECT` 句は出力に含めるフィールドを指定します。
  - `clientid` はメタデータのクライアントIDです。
  - `payload.clientid` はメッセージペイロード内のクライアントIDです。ペイロード内のすべてのフィールドは `payload` の下に格納されています。
    - `as` 構文により `payload.clientid` フィールドを `myclientid` として名前変更しています。

#### イベントによる選択

ルールをイベントに紐付けることも可能です。例えば、クライアント `c1` が EMQX に接続を開始した際の IP アドレスとポート番号を取得したい場合、以下のように記述します。

```sql
SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
```

::: tip

利用可能なすべてのイベントは EMQX ダッシュボードのルール編集時の **Events** タブで確認できます。

:::

### `WHERE` 句

`WHERE` 句は、`FROM` 句で指定したトピックやイベントのフィルタに加えて、メッセージが満たすべき追加条件を指定するオプションの方法です。

例えば、トピック `t/#` のメッセージのうち、ユーザー名が `eric` のメッセージのみをフィルタリングする場合は以下のように記述します。

```sql
SELECT * FROM "t/#" WHERE username = 'eric'
```

::: tip

`WHERE` 句で使用するフィールドは、メッセージのメタデータまたはペイロード内に存在するフィールドでなければなりません。そうでない場合はエラーになります。

:::

### 式の利用

[式](#expressions-and-operations) は `SELECT` 句や `WHERE` 句でデータ変換に使用できます。例えば、以下の SQL 文は `clientid` フィールドの値を大文字に変換し、サフィックスを追加して、出力メッセージのフィールド名を `cid` としています。

```sql
SELECT (upper(clientid) + '_UPPERCASE_LETTERS') as cid FROM "t/#"
```

括弧付きの算術式を使った例は以下の通りです。

```sql
SELECT (payload.integer_field + 2) * 2 as num FROM "t/#"
```

複雑な構造を持つペイロードのフィールドにドット記法でアクセスすることも可能です（ペイロードが JSON 形式であることが前提です）。

```sql
SELECT payload.a.b.c.deep as my_field FROM "t/#"
```

以下の例は、`WHERE` 句で等価演算子（=）を使い、特定の値を持つフィールドをテストしています。`SELECT` 句の `*` はメタデータとペイロードのすべてを出力メッセージに転送します。

```sql
SELECT * FROM "t/#" WHERE payload.x.y = 1
```

`WHERE` 句では `and` と `or` 演算子を使って複雑なブール式を作成できます。

```sql
SELECT * FROM "t/#" WHERE payload.name = "sensor_1" and payload.temperature > 39
```

## `FOREACH` ステートメント

`FOREACH` ステートメントは `SELECT` ステートメントのより一般的な形と考えられます。各入力メッセージに対してゼロ個以上の出力メッセージを生成できます。特定の条件に基づいてデータをフィルタリングし、その結果を MQTT トピックやデータブリッジに出力する際に使用します。

ルールエンジン SQL における `FOREACH` ステートメントの基本形式は以下の通りです。

```sql
FOREACH <expression_that_evaluates_to_array> [as <name>]
[DO <fields_expressions>]
[INCASE <condition>]
FROM <topic>
[WHERE <condition>]
```

`FOREACH` ステートメントは、`FOREACH` 句で入力メッセージから配列を作成するところから始まります。`FROM` と `WHERE` 句は `SELECT` ステートメントの対応する句と同じ目的で同様に機能します。`FOREACH` ステートメントには、`FOREACH`、`FROM`、`WHERE` 句に加えて、以下の2つのオプション句があります。

| 句        | 必須/任意 | 説明                                                         |
| --------- | --------- | ------------------------------------------------------------ |
| `DO`      | 任意      | `FOREACH` で選択された配列の各要素を変換します。<br /><br />`SELECT` ステートメントの `SELECT` 句に対応し、同じ式を受け入れます。 |
| `INCASE`  | 任意      | 指定した条件に合致しない配列要素をフィルタリングします。<br /><br />`WHERE` 句と同じ式を受け入れます。 |

::: tip

`FOREACH` 句以外のすべての句は `SELECT` ステートメントの対応する句と一致するため、`FOREACH` ステートメントは前述の通り `SELECT` ステートメントの一般化と見なせます。以下の2つのステートメントは等価です（`jq('.', payload)` はペイロードを配列でラップしています）。

```sql
FOREACH jq('.', payload) as it
DO it.field_1, it.field_2 
FROM "t/#"
```

```sql
SELECT payload.field_1, payload.field_2
FROM "t/#"
```

:::

上記の `FOREACH` 句の `as` 構文は配列の要素に名前を付けて、`DO` 句内で「現在の」要素を参照しやすくするために使います。`as name` 部分を省略すると、デフォルトで `item` という名前が使われます。

以下は `FOREACH` ステートメントを使って2つの値を出力する例です。両方の値は `value` という1つのフィールドのみを持ち、`value` の値はそれぞれメッセージの `field_1` と `field_2` の値です。

```sql
FOREACH jq('[.field_1, .field_2]', payload) 
DO item as value
FROM "t/#"
```

`FOREACH` ステートメントは入力データが配列形式であることを要求します。入力メッセージがすでに配列を含む場合は、そのまま `FOREACH` ステートメントを適用できます。

例えば、トピック `t/#` にパブリッシュされたメッセージで、センサーの `idx` が1以上の場合にタイムスタンプ、クライアントID、センサー名、インデックスを出力したい場合は以下のように記述します。

```sql
FOREACH
    payload.sensors as sensor  
DO
    timestamp,
    clientid,
    upper(sensor.name) as name,
    sensor.idx as idx
INCASE
    sensor.idx >= 1
FROM "t/#"
```

ここで、

- `FOREACH` 句は入力メッセージのペイロード中の `sensors` フィールドを配列として指定し、配列要素に `sensor` という名前を付けています。
- `DO` 句は出力に含めるフィールドを指定しています。
  - `timestamp` は入力メッセージのメタデータからのタイムスタンプです。
  - `clientid` は入力メッセージのメタデータからのクライアントIDです。
  - `sensor.name` は組み込みの `upper` 関数で大文字化され、`as` 構文で `name` と名前変更されます。ここでの `sensor` は `FOREACH` 句で選択された配列の現在の要素を指します。
  - `sensor.idx` は `as` 句で `idx` と名前変更されます。
- `INCASE` 句は追加のフィルタ条件を指定し、`idx` フィールドの値が1以上のセンサーのみを対象とします。
- `FROM` 句はトピックパターン `t/#` にマッチするメッセージを対象とします。

ルールを作成したら、本番環境に投入する前に必ずテストすることを推奨します。ダッシュボードの UI には、サンプルメッセージでルールをテストできる機能があります。SQL ステートメントのテスト方法の詳細は [ルールのテスト](./rule-get-started.md#test-the-rule) を参照してください。上記のルールは以下の JSON フォーマットのペイロードでテストできます。

```json
{"sensors": [
    {"idx":0, "name":"t0"},
    {"idx":1, "name":"t1"},
    {"idx":2, "name":"t2"}
  ]
}
```

入力メッセージが配列を含まない場合は、`jq` 関数を使ってペイロードを配列でラップできます。例えば以下のように記述します。

```sql
FOREACH jq('.', payload) 
DO item.field_1, item.field_2 
FROM "t/#"
```

EMQX は高度な変換のために `jq` 関数の使用をサポートしています。詳細なコード例は [組み込みの `jq` 関数](./rule-sql-jq.md) を参照してください。

## 式と演算

EMQX のルール構文では、データ変換やメッセージのフィルタリングに式を使用できます。これらの式は `SELECT`、`FOREACH`、`DO`、`INCASE`、`WHERE` などの様々な句で利用可能です。このセクションでは式の使い方を詳述します。以下は式を構成する演算子の一覧です。なお、[組み込み関数](./rule-sql-builtin-functions.md)も多数利用可能です。

### 算術演算

| 演算子 | 用途                                | 戻り値                      |
| ------ | ----------------------------------- | --------------------------- |
| `+`    | 加算、または文字列の連結             | 合計、または連結された文字列 |
| `-`    | 減算                                | 差分                        |
| `*`    | 乗算                                | 積                          |
| `/`    | 除算                                | 商                          |
| `div`  | 整数除算                            | 整数の商                    |
| `mod`  | 剰余                                | 剰余                        |

### 論理演算

| 演算子 | 用途               | 戻り値       |
| ------ | ------------------ | ------------ |
| `>`    | より大きい         | true/false   |
| `<`    | より小さい         | true/false   |
| `<=`   | 以下               | true/false   |
| `>=`   | 以上               | true/false   |
| `<>`   | 等しくない         | true/false   |
| `!=`   | 等しくない         | true/false   |
| `=`    | 完全に等しいか判定 | true/false   |
| `=~`   | トピックがトピックフィルターにマッチするか判定（トピックマッチング専用） | true/false   |
| `and`  | 論理積             | true/false   |
| `or`   | 論理和             | true/false   |

### CASE 式

`CASE` 式は条件付きの処理を行うために使用できます。`CASE` 式は他の言語の if-then-else 文に相当します。以下の例で使い方を示します。

```sql
SELECT
  CASE WHEN payload.x < 0 THEN 0
       WHEN payload.x > 7 THEN 7
       ELSE payload.x
  END as x
FROM "t/#"
```

例えば、メッセージが以下の場合、

```json
{"x": 8}
```

出力は以下のようになります。

```json
{"x": 7}
```

## さらに例

### `SELECT` ステートメントの例

- トピック "t/a" のメッセージからすべてのフィールドを抽出する：

    ```sql
    SELECT * FROM "t/a"
    ```

- トピック "t/a" または "t/b" のメッセージからすべてのフィールドを抽出する：

    ```sql
    SELECT * FROM "t/a","t/b"
    ```

- トピックが 't/#' にマッチするメッセージからすべてのフィールドを抽出する：

    ```sql
    SELECT * FROM "t/#"
    ```

- トピックが 't/#' にマッチするメッセージから `qos`、`username`、`clientid` フィールドを抽出する（出力メッセージのペイロードにこれらのフィールドが含まれます）：

    ```sql
    SELECT qos, username, clientid FROM "t/#"
    ```

- ペイロードに `username` フィールドがあり、その値が 'Steven' のメッセージから `username` フィールドを抽出する（`FROM` 句でのトピックフィルター '#' の使用は、すべてのメッセージに対してルールがチェックされるため推奨されません）：

    ```sql
    SELECT username FROM "#" WHERE username='Steven'
    ```

- 入力メッセージのペイロードの `x` フィールドを抽出し、出力メッセージでフィールド名を `y` に変更する。`WHERE` 句で新しいエイリアス `y` を使用可能。ペイロードが `{"x": 1}` のメッセージにマッチし、`{"x": 2}` にはマッチしません。

    ```sql
    SELECT payload.x as x FROM "tests/test_topic_1" WHERE y = 1
    ```

- ペイロードが `{"x": {"y": 1}}` のメッセージ（例：`{"x": {"y": 1}, "other": "field"}` も含む）にマッチする SQL 文：

    ```sql
    SELECT * FROM "#" WHERE payload.x.y = 1
    ```

- クライアントIDが 'c1' の MQTT クライアントが接続した場合、そのソース IP アドレスとポート番号を抽出する：

    ```sql
    SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
    ```

- トピックが 't/topic' にマッチし、QoS レベルが 1 のすべてのサブスクリプションにマッチし、クライアントIDを抽出する：

    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic = 'my/topic' and qos = 1
    ```

- 上記の例と似ていますが、トピックマッチ演算子 `=~` を使い、トピックフィルター 't/#' にマッチさせる：

    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic =~ 't/#' and qos = 1
    ```

- キー "foo" のユーザープロパティを抽出する（ユーザープロパティは MQTT 5.0 プロトコルの新機能であり、古い MQTT バージョンには該当しません）：

    ```sql
    SELECT pub_props.'User-Property'.foo as foo FROM "t/#"
    ```

::: tip

- `FROM` 句のトピックはダブルクォート（`""`）またはシングルクォート（`''`）で囲む必要があります。
- `WHERE` 句の条件で文字列を使う場合はシングルクォート（`''`）で囲みます。
- `FROM` 句に複数のトピックがある場合はカンマ（`,`）で区切ります。例：`SELECT * FROM "t/1", "t/2"`。
- ペイロードの内部フィールドにアクセスするにはドット記法（`.`）を使います。例えば、ネストされた JSON の場合は `payload.outer_field.inner_field` のように指定します。
- ペイロードにエイリアスを付けることはパフォーマンスに影響するため避けてください。例：`SELECT payload as p` は推奨されません。
- 一部のエスケープシーケンスは使用時にアンエスケープが必要です。詳細は [unescape 関数](./rule-sql-builtin-functions.md#unescapestring-string---string) を参照してください。

:::

### `FOREACH` ステートメントの例

クライアントID `c_steve` のメッセージがトピック `t/1` に届き、メッセージ本文は JSON 形式で、`sensors` フィールドが複数のオブジェクトを含む配列であるとします。例：

```json
{
    "date": "2020-04-24",
    "sensors": [
        {"name": "a", "idx":0},
        {"name": "b", "idx":1},
        {"name": "c", "idx":2}
    ]
}
```

#### 例1

`sensors` 配列の各オブジェクトをトピック `sensors/${idx}` に再パブリッシュし、内容は `${name}` とします。上記の入力例では、ルールエンジンは以下の3つのメッセージを発行します。

1. トピック: sensors/0  
   内容: a  
2. トピック: sensors/1  
   内容: b  
3. トピック: sensors/2  
   内容: c  

このルールのアクション設定は以下の通りです。

- アクションタイプ: メッセージ再パブリッシュ
- 送信先トピック: `sensors/${idx}`
- 送信先 QoS: 2
- メッセージ内容テンプレート: `${name}`

SQL 文は以下のように記述します。

```sql
FOREACH
    payload.sensors
FROM "t/#"
```

この SQL 文の `FOREACH` 句は、走査すべき配列 `sensors` を指定しています。`FOREACH` ステートメントは結果の配列の各オブジェクトに対して「メッセージ再パブリッシュ」アクションを実行するため、3回実行されます。

#### 例2

`sensors` 配列の中で `idx` フィールドの値が1以上のオブジェクトのみをトピック `sensors/${idx}` に再パブリッシュし、内容は `clientid=${clientid},name=${name},date=${date}` とします。上記の入力例では、`idx` が0の要素はフィルタされるため、2つのメッセージが発行されます。

1. トピック: sensors/1  
   内容: clientid=c_steve,name=b,date=2023-04-24  
2. トピック: sensors/2  
   内容: clientid=c_steve,name=c,date=2023-04-24  

このルールのアクション設定は以下の通りです。

- アクションタイプ: メッセージ再パブリッシュ
- 送信先トピック: `sensors/${idx}`
- 送信先 QoS: 2
- メッセージ内容テンプレート: `clientid=${clientid},name=${name},date=${date}`

SQL 文は以下のように記述します。

```sql
FOREACH
    payload.sensors
DO
    clientid,
    item.name as name,
    item.idx as idx
INCASE
    item.idx >= 1
FROM "t/#"
```

この SQL 文で、

- `FOREACH` 句は配列 `sensors` を走査対象として指定しています。
- `DO` 句は各操作に必要なフィールドを選択しています。`clientid` はメッセージのメタデータから、`name` と `idx` は現在のセンサーオブジェクトから選択しています。
- `item` は `sensors` 配列の現在のオブジェクトを表します。
- `INCASE` 句は配列オブジェクトのフィルタ条件を指定し、条件に合わないオブジェクトは無視されます。

`DO` と `INCASE` 句では、現在のオブジェクトにアクセスするために `item` を使うか、`FOREACH` 句の `as` 構文で変数名をカスタマイズできます。したがって、上記の SQL は以下のようにも書けます。

```sql
FOREACH
    payload.sensors as s
DO
    clientid,
    s.name as name,
    s.idx as idx
INCASE
    s.idx >= 1
FROM "t/#"
```

#### 例3

例2を拡張し、クライアントIDの `c_steve` の `c_` プレフィックスを削除します。

ルールエンジンには `FOREACH`、`DO`、`INCASE` 句で呼び出せる組み込み関数が多数あります。`c_steve` を `steve` に変換したい場合は、例2の SQL を以下のように変更します。

```sql
FOREACH
    payload.sensors as s
DO
    nth(2, tokens(clientid,'_')) as clientid,
    s.name as name,
    s.idx as idx
INCASE
    s.idx >= 1
FROM "t/#"
```

複数の式を `FOREACH` 句に記述できますが、最後の式が走査対象の配列を指定している必要があります。

例えば、入力メッセージのペイロードが以下のように構造化されている場合：

```json
{
    "date": "2020-04-24",
    "data": {
        "sensors": [
            {"name": "a", "idx":0},
            {"name": "b", "idx":1},
            {"name": "c", "idx":2}
        ]
    }
}
```

`FOREACH` 句でペイロードのデータに別名を付けてから配列を選択できます。

```sql
FOREACH
    payload.data as d
    d.sensors as s
...
```

これは以下と同等です。

```sql
FOREACH
    payload.data.sensors as s
...
```

この機能は複雑な構造のペイロードを扱う際に便利です。
