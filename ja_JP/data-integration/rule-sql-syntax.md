# Rule SQL リファレンス

EMQX のルールでは、データの抽出、フィルタリング、拡張、変換のために SQL ベースの構文を使用します。この SQL ライクな構文には、`SELECT` と `FOREACH` の2種類のステートメントがあります。

| ステートメント | 説明                                                         |
| -------------- | ------------------------------------------------------------ |
| `SELECT`       | SQL ステートメントの結果が単一のメッセージとなる場合に使用します。 |
| `FOREACH`      | 単一の入力メッセージからゼロ個以上のメッセージを生成する場合に使用します。 |

各ルールは正確に1つのステートメントを持つことができます。SQL ステートメントは豊富な組み込み関数を提供しており、簡単な変換やタイムスタンプの生成などが可能です。

また、SQL ステートメントは式内に [jq プログラム](https://stedolan.github.io/jq/) を埋め込むことをサポートしており、必要に応じて複雑なデータ変換を行うことができます。式は `SELECT` および `FOREACH` ステートメント内に埋め込むことができます。`SELECT` と `FOREACH` ステートメントで参照可能なフィールドについては、[データソースとフィールド](./rule-sql-events-and-fields.md)を参照してください。

<img src="./assets/rules/data-integration-arch.jpg" alt="データ統合アーキテクチャ" style="zoom:40%;" />

## `SELECT` ステートメント

`SELECT` ステートメントは、入力メッセージから特定のフィールドを選択し、フィールドの名前変更、データ変換、条件に基づくメッセージのフィルタリングを行います。

ルールエンジンの SQL における `SELECT` ステートメントの基本形式は以下の通りです。

```sql
SELECT <fields_expressions> FROM <topic> [WHERE <conditions>]
```

`SELECT` 句で出力に含めるフィールド（メッセージのペイロードおよびメタデータ両方から）を指定し、`WHERE` 句で特定の条件に基づいてメッセージをフィルタリングできます。

### `FROM` 句

`FROM` 句はクエリのデータソースを指定するために使います。特定のトピックや条件に合致するイベントからデータを選択できます。

#### トピックによる選択

例えば、`t/#` と `my/other/topic` にマッチするトピックにパブリッシュされたすべてのメッセージに適用されるルールを定義したい場合、以下のように記述します。

```sql
SELECT clientid, payload.clientid as myclientid FROM "t/#", "my/other/topic"
```

ここで、

- `SELECT` 句は出力に含めるフィールドを指定します。

  - `clientid`：メタデータ内のクライアントID

  - `payload.clientid`：メッセージペイロード内のクライアントID。ペイロード内のすべてのフィールドは `payload` の下に格納されています。

    - `as` 構文は `payload.clientid` フィールドの名前を `myclientid` に変更します。

#### イベントによる選択

ルールをイベントに紐づけることも可能です。例えば、クライアント `c1` が EMQX に接続を開始した際のIPアドレスとポート番号を取得したい場合、以下のように記述します。

```sql
SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
```

::: tip

利用可能なすべてのイベントは EMQX ダッシュボードのルール編集画面の **Events** タブで確認できます。

:::

### `WHERE` 句

`WHERE` 句は、`FROM` 句で指定したトピックやイベントのフィルタに加え、メッセージが満たすべき追加条件を指定してメッセージのフィルタリングを細かく行うためのオプションです。

例えば、トピック `t/#` のメッセージのうち、ユーザー名が `eric` であるメッセージだけをフィルタリングする場合は以下のように記述します。

```sql
SELECT * FROM "t/#" WHERE username = 'eric'
```

::: tip

`WHERE` 句で使用するフィールドは、メッセージのメタデータまたはペイロードに存在するフィールドでなければなりません。存在しない場合はエラーになります。

:::

### 式の利用

[式](#expressions-and-operations)は `SELECT` および `WHERE` 句でデータ変換に使用できます。例えば、以下の SQL 文は `clientid` フィールドの値を大文字に変換し、サフィックスを追加して、結果を `cid` という名前で出力します。

```sql
SELECT (upper(clientid) + '_UPPERCASE_LETTERS') as cid FROM "t/#"
```

括弧付きの算術式を使ったデータ変換の例は以下の通りです。

```sql
SELECT (payload.integer_field + 2) * 2 as num FROM "t/#"
```

複雑な構造のペイロード内のフィールドにドット記法でアクセスすることも可能です（ペイロードが JSON フォーマットであることが前提です）。

```sql
SELECT payload.a.b.c.deep as my_field FROM "t/#"
```

以下の例は、`WHERE` 句で等価演算子（=）を使い、特定の値を持つフィールドをテストする例です。`SELECT *` はメタデータとペイロードのすべてを出力メッセージに転送します。

```sql
SELECT * FROM "t/#" WHERE payload.x.y = 1
```

`WHERE` 句では `and` および `or` 演算子を使って複雑な論理式を作成できます。

```sql
SELECT * FROM "t/#" WHERE payload.name = "sensor_1" and payload.temperature > 39
```

## `FOREACH` ステートメント

`FOREACH` ステートメントは `SELECT` ステートメントのより一般的な形と考えられます。入力メッセージごとにゼロ個以上の出力メッセージを生成できます。特定の条件に基づいてデータをフィルタリングし、結果を MQTT トピックやデータブリッジに出力する場合に使用します。

ルールエンジン SQL における `FOREACH` ステートメントの基本形式は以下の通りです。

```sql
FOREACH <expression_that_evaluates_to_array> [as <name>]
[DO <fields_expressions>]
[INCASE <condition>]
FROM <topic>
[WHERE <condition>]
```

`FOREACH` ステートメントは、配列を作成するための `FOREACH` 句で始まります。`FROM` および `WHERE` 句は `SELECT` ステートメントの同名句と同じ目的で動作します。`FOREACH` ステートメントには、`FOREACH`、`FROM`、`WHERE` 句に加え、以下の2つのオプション句があります。

| 句        | 必須/任意 | 説明                                                         |
| --------- | --------- | ------------------------------------------------------------ |
| `DO`      | 任意      | `FOREACH` で選択した配列の各要素を変換します。<br /><br />`SELECT` ステートメントの `SELECT` 句に対応し、同じ式を受け入れます。 |
| `INCASE`  | 任意      | 指定した条件に合致しない配列要素をフィルタリングします。<br /><br />`WHERE` 句と同じ式を受け入れます。 |

::: tip

`FOREACH` 句以外のすべての句は `SELECT` ステートメントの対応する句と対応しています。つまり、`FOREACH` ステートメントは前述の通り `SELECT` ステートメントの一般化と見なせます。以下の2つのステートメントは等価です（`jq('.', payload)` はペイロードを配列にラップしています）。

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

`FOREACH` 句の `as` 構文は配列の各要素に名前を付けるために使われ、`DO` 句内で「現在の」要素を参照しやすくします。`as name` 部分を省略すると、デフォルトで `item` という名前が使われます。

以下は、`FOREACH` ステートメントを使って2つの値を出力する例です。どちらの値も `value` という1つのフィールドのみを持ち、`value` の値はそれぞれメッセージの `field_1` と `field_2` の値です。

```sql 
FOREACH jq('[.field_1, .field_2]', payload) 
DO item as value
FROM "t/#"
```

`FOREACH` ステートメントは入力データが配列形式であることを要求します。入力メッセージがすでに配列を含んでいる場合は、そのまま `FOREACH` ステートメントを適用できます。

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

- `FOREACH` 句は入力メッセージのペイロード内の `sensors` フィールドを配列として指定し、配列要素に `sensor` という名前を付けています。
- `DO` 句は出力に含めるフィールドを指定しています。
  - `timestamp` は入力メッセージのメタデータからのタイムスタンプです。
  - `clientid` は入力メッセージのメタデータからのクライアントIDです。
  - `sensor.name` は組み込み関数 `upper` で大文字化され、`as` 構文で `name` に名前変更されます。ここでの `sensor` は `FOREACH` 句で指定された配列の現在の要素を指します。
  - `sensor.idx` は `as` 句で `idx` に名前変更されます。
- `INCASE` 句は追加のフィルタ条件を指定し、`idx` フィールドの値が1以上のセンサーのみを対象とします。
- `FROM` 句はトピックパターン `t/#` にマッチするメッセージを対象とします。

ルール作成後は、本番環境に投入する前に必ずテストすることを推奨します。ダッシュボードの UI にはサンプルメッセージでルールをテストできる機能があります。SQL ステートメントのテスト方法については、[ルールのテスト](./rule-get-started.md#test-the-rule)を参照してください。上記ルールは以下の JSON フォーマットのペイロードを入力としてテストできます。

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

EMQX は高度な変換のために `jq` 関数の使用をサポートしています。詳細なコード例は[組み込みの `jq` 関数](./rule-sql-jq.md)を参照してください。

## 式と演算

EMQX のルール構文では、データ変換やメッセージのフィルタリングに式を使用でき、`SELECT`、`FOREACH`、`DO`、`INCASE`、`WHERE` などの句で利用可能です。このセクションでは式の使い方を詳述します。以下は式を構成するために使用できる演算子の一覧です。また、[組み込み関数](./rule-sql-builtin-functions.md)も幅広く利用できます。

### 算術演算

| 演算子 | 用途                                   | 戻り値                      |
| ------ | -------------------------------------- | --------------------------- |
| `+`    | 加算、または文字列の連結               | 合計値、または連結された文字列 |
| `-`    | 減算                                   | 差分                        |
| `*`    | 乗算                                   | 積                          |
| `/`    | 除算                                   | 商                          |
| `div`  | 整数除算                               | 整数の商                    |
| `mod`  | 剰余                                   | 剰余                        |

### 論理演算

| 演算子 | 用途               | 戻り値       |
| ------ | ------------------ | ------------ |
| `>`    | より大きい         | true/false   |
| `<`    | より小さい         | true/false   |
| `<=`   | 以下               | true/false   |
| `>=`   | 以上               | true/false   |
| `<>`   | 等しくない         | true/false   |
| `!=`   | 等しくない         | true/false   |
| `=`    | 2つのオペランドが完全に等しいかをチェック。値の比較に使用可能 | true/false   |
| `=~`   | トピックがトピックフィルターにマッチするかをチェック。トピックマッチング専用 | true/false   |
| `and`  | 論理積             | true/false   |
| `or`   | 論理和             | true/false   |

### CASE 式

`CASE` 式は条件付きの処理を行うために使用できます。`CASE` 式は他言語の if-then-else 文に相当します。以下の例で使い方を示します。

```sql
SELECT
  CASE WHEN payload.x < 0 THEN 0
       WHEN payload.x > 7 THEN 7
       ELSE payload.x
  END as x
FROM "t/#"
```

メッセージが以下の場合、

```json
{"x": 8}
```

出力は以下のようになります。

```json
{"x": 7}
```

## さらに例

### `SELECT` ステートメントの例

- トピック "t/a" のメッセージからすべてのフィールドを抽出する:

    ```sql
    SELECT * FROM "t/a"
    ```

- トピック "t/a" または "t/b" のメッセージからすべてのフィールドを抽出する:

    ```sql
    SELECT * FROM "t/a","t/b"
    ```

- トピックが 't/#' にマッチするメッセージからすべてのフィールドを抽出する:

    ```sql
    SELECT * FROM "t/#"
    ```

- トピックが 't/#' にマッチする入力メッセージから `qos`、`username`、`clientid` フィールドを抽出する（出力メッセージのペイロードにこれらのフィールドが含まれます）:

    ```sql
    SELECT qos, username, clientid FROM "t/#"
    ```

- ペイロードに `username` フィールドがあり、その値が 'Steven' のメッセージから `username` フィールドを抽出する（`FROM` 句で `#` を使うのは推奨されません。これはすべてのメッセージに対してルールが評価されるためです）:

    ```sql
    SELECT username FROM "#" WHERE username='Steven'
    ```

- 入力メッセージのペイロードの `x` フィールドを抽出し、出力メッセージで `y` に名前変更する。`y` は `WHERE` 句でも使用可能。この SQL はペイロードが `{"x": 1}` のメッセージにマッチし、`{"x": 2}` のメッセージにはマッチしません。

    ```sql
    SELECT payload.x as x FROM "tests/test_topic_1" WHERE y = 1
    ```

- ペイロードが `{"x": {"y": 1}}` のメッセージ（例：`{"x": {"y": 1}, "other": "field"}` も含む）にマッチする SQL:

    ```sql
    SELECT * FROM "#" WHERE payload.x.y = 1
    ```

- クライアントID が 'c1' の MQTT クライアントが接続した場合、そのソース IP アドレスとポート番号を抽出する:

    ```sql
    SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
    ```

- トピック 't/topic' にマッチし、QoS レベルが 1 のすべてのサブスクリプションにマッチし、`clientid` を出力メッセージに抽出する:

    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic = 'my/topic' and qos = 1
    ```

- 上記の例と似ていますが、トピックマッチ演算子 `=~` を使ってトピックフィルター 't/#' にマッチさせる例:

    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic =~ 't/#' and qos = 1
    ```

- キー "foo" のユーザープロパティを抽出する（ユーザープロパティは MQTT 5.0 の新機能であり、古い MQTT バージョンには該当しません）:

    ```sql
    SELECT pub_props.'User-Property'.foo as foo FROM "t/#"
    ```

::: tip

- `FROM` 句のトピックはダブルクォーテーション (`""`) またはシングルクォーテーション (`''`) で囲む必要があります。
- `WHERE` 句の条件で文字列を使う場合はシングルクォーテーション (`''`) で囲みます。
- `FROM` 句に複数のトピックを指定する場合はカンマ（`,`）で区切ります。例：`SELECT * FROM "t/1", "t/2"`。
- ペイロードがネストされた JSON 構造の場合、ドット記法 (`.`) を使って内部フィールドにアクセスできます。例：`payload.outer_field.inner_field`。
- ペイロードに対してエイリアスを作成するのはパフォーマンスに影響するため避けてください。例：`SELECT payload as p` は推奨されません。
- 一部のエスケープシーケンスは使用時にアンエスケープが必要です。詳細は [unescape 関数](./rule-sql-builtin-functions.md#unescapestring-string---string) を参照してください。
:::

### `FOREACH` ステートメントの例

クライアントID `c_steve` のメッセージがトピック `t/1` に届くとします。メッセージ本文は JSON 形式で、`sensors` フィールドは複数のオブジェクトを含む配列です。例は以下の通りです。

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

#### 例 1

`sensors` 配列の各オブジェクトを、オブジェクト内の `idx` を使ってトピック `sensors/${idx}` に、`name` の内容 `${name}` をメッセージとして再パブリッシュする必要があります。上記の入力例では、ルールエンジンは以下の3つのメッセージを発行します。

1. トピック: sensors/0  
   内容: a  
2. トピック: sensors/1  
   内容: b  
3. トピック: sensors/2  
   内容: c  

このルールのアクション設定は以下の通りです。

- アクションタイプ: メッセージ再パブリッシュ
- ターゲットトピック: `sensors/${idx}`
- ターゲット QoS: 2
- メッセージ内容テンプレート: `${name}`

SQL ステートメントは以下のように記述します。

```sql
FOREACH
    payload.sensors
FROM "t/#"
```

この SQL では、`FOREACH` 句で配列 `sensors` を指定しています。`FOREACH` ステートメントは結果の配列の各オブジェクトに対して「メッセージ再パブリッシュ」アクションを実行するため、3回の再パブリッシュが行われます。

#### 例 2

`sensors` 配列のうち、`id` フィールドの値が1以上のオブジェクトのみを対象に、トピック `sensors/${idx}` に以下の内容で再パブリッシュします。

```
clientid=${clientid},name=${name},date=${date}
```

上記の入力例では、`id` が0の要素は除外されるため、2つのメッセージが発行されます。

1. トピック: sensors/1  
   内容: clientid=c_steve,name=b,date=2023-04-24  
2. トピック: sensors/2  
   内容: clientid=c_steve,name=c,date=2023-04-24  

このルールのアクション設定は以下の通りです。

- アクションタイプ: メッセージ再パブリッシュ
- ターゲットトピック: `sensors/${idx}`
- ターゲット QoS: 2
- メッセージ内容テンプレート: `clientid=${clientid},name=${name},date=${date}`

SQL ステートメントは以下の通りです。

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

この SQL では、`FOREACH` 句で配列 `sensors` を指定し、`DO` 句で各操作に必要なフィールドを選択しています。`clientid` はメッセージのメタデータから、`name` と `idx` は現在のセンサーオブジェクトから取得します。`item` は `sensors` 配列の現在のオブジェクトを表します。`INCASE` 句は配列オブジェクトのフィルタ条件を指定し、条件に合わないオブジェクトは無視されます。

`DO` と `INCASE` 句では `item` を使って現在のオブジェクトにアクセスできますが、`FOREACH` 句の `as` 構文で変数名をカスタマイズすることも可能です。したがって、上記の SQL は以下のようにも書けます。

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

#### 例 3

例 2 を拡張し、`clientid` フィールドの `c_steve` の `c_` プレフィックスを削除します。

ルールエンジンには `FOREACH`、`DO`、`INCASE` 句で呼び出せる多数の組み込み関数があります。`c_steve` を `steve` に変換したい場合、例 2 の SQL を以下のように変更します。

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

複数の式を `FOREACH` 句に記述できますが、最後の式は必ず配列を指定する必要があります。

例えば、入力メッセージのペイロードが以下のような形式の場合、

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

`FOREACH` 句でペイロードの `data` に別名を付けてから配列を選択できます。

```sql
FOREACH
    payload.data as d
    d.sensors as s
...
```

これは以下と等価です。

```sql
FOREACH
    payload.data.sensors as s
...
```

この機能は複雑な構造のペイロードを扱う際に便利です。
