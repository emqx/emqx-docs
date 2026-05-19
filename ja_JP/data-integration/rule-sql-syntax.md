# ルールSQLリファレンス

EMQXでは、データの抽出、フィルタリング、拡張、および変換のために、ルールでSQLベースの構文を使用します。このSQLライクな構文には、`SELECT` と `FOREACH` の2種類のステートメントがあります。

| ステートメント | 説明                                                  |
| -------------- | ----------------------------------------------------- |
| `SELECT`       | SQLステートメントの結果が単一のメッセージとなる場合に使用します。 |
| `FOREACH`      | 1つの入力メッセージからゼロ個以上のメッセージを生成する場合に使用します。 |

各ルールはちょうど1つのステートメントを持つことができます。SQLステートメントは豊富な組み込み関数を提供し、簡単な変換やタイムスタンプの作成などが可能です。

また、SQLステートメントは式の中に[jqプログラム](https://stedolan.github.io/jq/)を埋め込むことをサポートしており、必要に応じて複雑なデータ変換を行えます。式は`SELECT`および`FOREACH`ステートメント内に埋め込むことができます。`SELECT`および`FOREACH`ステートメントで参照可能なフィールドについては、[データソースとフィールド](./rule-sql-events-and-fields.md)を参照してください。

## `SELECT` ステートメント

`SELECT`ステートメントは、入力メッセージから特定のフィールドを選択し、フィールド名の変更、データの変換、条件に基づくメッセージのフィルタリングを行います。

ルールエンジンSQLにおける`SELECT`ステートメントの基本形式は以下の通りです。

```sql
SELECT <fields_expressions> FROM <topic> [WHERE <conditions>]
```

`SELECT`句で出力に含めるフィールド（メッセージのペイロードおよびメタデータの両方から）を指定し、`WHERE`句で特定の条件に基づいてメッセージをフィルタリングできます。

### `FROM`句

`FROM`句はクエリのデータソースを指定します。特定のトピックや条件に合致するイベントからデータを選択できます。

#### トピックによる選択

例えば、`t/#`および`my/other/topic`にパブリッシュされたすべてのメッセージに適用されるルールを定義したい場合、以下のように記述します。

```sql
SELECT clientid, payload.clientid as myclientid FROM "t/#", "my/other/topic"
```

ここで、

- `SELECT`句は出力に含めるフィールドを指定します。
  - `clientid`：メタデータ内のクライアントID
  - `payload.clientid`：メッセージペイロード内のクライアントID。ペイロード内のすべてのフィールドは`payload`の下に格納されています。
    - `as`構文は`payload.clientid`フィールドを`myclientid`に名前変更しています。

#### イベントによる選択

ルールをイベントに紐づけることも可能です。例えば、クライアント`c1`がEMQXへの接続を開始した際にIPアドレスとポート番号を取得したい場合、以下のように記述します。

```sql
SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
```

::: tip

利用可能なすべてのイベントは、EMQXダッシュボードのルール編集画面の**Events**タブで確認できます。

:::

### `WHERE`句

`WHERE`句は、`FROM`句で指定したトピックやイベントのフィルタに加えて、メッセージが満たすべき追加条件を指定し、メッセージのフィルタリングをより詳細に行うためのオプションです。

例えば、トピック`t/#`のメッセージのうち、ユーザー名が`eric`であるメッセージのみをフィルタリングするSQLは以下の通りです。

```sql
SELECT * FROM "t/#" WHERE username = 'eric'
```

::: tip

`WHERE`句で使用するフィールドは、メッセージのメタデータまたはペイロードに存在するフィールドでなければなりません。そうでない場合はエラーになります。

:::

### 式の利用

[式](#expressions-and-operations)は`SELECT`句や`WHERE`句でデータの変換に使用可能です。例えば、以下のSQLは`clientid`フィールドの値を大文字に変換し、接尾辞を追加して、結果を`cid`という名前で出力します。

```sql
SELECT (upper(clientid) + '_UPPERCASE_LETTERS') as cid FROM "t/#"
```

以下は括弧付きの算術式を使ったデータ変換の例です。

```sql
SELECT (payload.integer_field + 2) * 2 as num FROM "t/#"
```

ドット表記を使い、複雑な構造のペイロード内のフィールドにアクセスすることもできます（ペイロードがJSON形式であることが前提です）。

```sql
SELECT payload.a.b.c.deep as my_field FROM "t/#"
```

以下は`WHERE`句で等価演算子（=）を使い、特定の値を持つフィールドをテストする例です。`SELECT *`はすべてのメタデータとペイロードを出力メッセージに転送します。

```sql
SELECT * FROM "t/#" WHERE payload.x.y = 1
```

`WHERE`句では`and`や`or`演算子を使い、複雑なブール式を形成できます。

```sql
SELECT * FROM "t/#" WHERE payload.name = "sensor_1" and payload.temperature > 39
```

## `FOREACH` ステートメント

`FOREACH`ステートメントは`SELECT`ステートメントのより一般的な形と考えられます。1つの入力メッセージからゼロ個以上の出力メッセージを生成できます。特定の条件に基づいてデータをフィルタリングし、結果をMQTTトピックやデータブリッジに出力する際に使用します。

ルールエンジンSQLにおける`FOREACH`ステートメントの基本形式は以下の通りです。

```sql
FOREACH <expression_that_evaluates_to_array> [as <name>]
[DO <fields_expressions>]
[INCASE <condition>]
FROM <topic>
[WHERE <condition>]
```

`FOREACH`ステートメントは、入力メッセージから配列を作成する`FOREACH`句から始まります。`FROM`句と`WHERE`句は`SELECT`ステートメントの同名句と同じ目的・動作を持ちます。`FOREACH`ステートメントにはさらに2つのオプション句があります。

| 句       | 必須/任意 | 説明                                                  |
| -------- | --------- | ----------------------------------------------------- |
| `DO`     | 任意      | `FOREACH`で選択された配列の各要素を変換します。<br /><br />`SELECT`ステートメントの`SELECT`句に対応し、同じ式を受け入れます。 |
| `INCASE` | 任意      | 指定した条件に合致しない配列要素をフィルタリングします。<br /><br />`WHERE`句と同じ式を受け入れます。 |

::: tip

`FOREACH`句を除き、`FOREACH`ステートメントの他の句はすべて`SELECT`ステートメントの対応する句と対応しています。つまり、`FOREACH`ステートメントは前述の通り`SELECT`ステートメントの一般化と見なせます。以下の2つのステートメントは等価です（`jq('.', payload)`はペイロードを配列でラップしています）。

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

上記の`FOREACH`句の`as`構文は配列要素に名前を付けるために使われ、`DO`句内で「現在の」要素を簡単に参照できます。`as name`部分を省略すると、デフォルトで`item`という名前が使われます。

以下は`FOREACH`ステートメントを使って2つの値を出力する例です。両方の値は`value`という1つのフィールドのみを持ち、`value`の値は1つ目のメッセージの`field_1`の値、2つ目のメッセージの`field_2`の値です。

```sql 
FOREACH jq('[.field_1, .field_2]', payload) 
DO item as value
FROM "t/#"
```

`FOREACH`ステートメントは入力データが配列形式であることを要求します。入力メッセージがすでに配列を含む場合は、直接`FOREACH`ステートメントを適用できます。

例えば、トピック`t/#`にパブリッシュされたメッセージで、センサーの`idx`が1以上の場合にタイムスタンプ、クライアントID、センサー名、インデックスを出力したい場合、以下のコードを使えます。

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

- `FOREACH`句は入力メッセージのペイロード内の`sensors`フィールドを配列として指定し、配列要素に`sensor`という名前を付けています。
- `DO`句は出力に含めるフィールドを指定しています。
  - `timestamp`は入力メッセージのメタデータからのタイムスタンプです。
  - `clientid`は入力メッセージのメタデータからのクライアントIDです。
  - `sensor.name`は組み込みの`upper`関数で大文字化され、`as`構文で`name`に名前変更されます。ここでの`sensor`は`FOREACH`句で選択された配列の現在の要素を指します。
  - `sensor.idx`は`as`句で`idx`に名前変更されます。
- `INCASE`句は追加のフィルタ条件を指定し、`idx`フィールドの値が1以上のセンサーのみを取得します。
- `FROM`句はトピックパターン`t/#`にマッチするメッセージを対象とします。

ルール作成後は、本番環境に投入する前に必ずテストすることを推奨します。ダッシュボードUIにはサンプルメッセージでルールをテストできる機能があります。SQLステートメントのテスト方法については[ルールのテスト](./rule-get-started.md#test-the-rule)を参照してください。上記のルールは以下のJSON形式のペイロードを入力としてテストできます。

```json
{"sensors": [
    {"idx":0, "name":"t0"},
    {"idx":1, "name":"t1"},
    {"idx":2, "name":"t2"}
  ]
}
```

入力メッセージに配列が含まれていない場合は、`jq`関数を使ってペイロードを配列でラップできます。例えば以下のように記述します。

```sql
FOREACH jq('.', payload) 
DO item.field_1, item.field_2 
FROM "t/#"
```

EMQXは高度な変換のために`jq`関数の使用をサポートしています。詳しいコード例は[組み込み`jq`関数](./rule-sql-jq.md)を参照してください。

## 式と演算

EMQXのルール構文では、データの変換やメッセージのフィルタリングのために式を使用できます。これらの式は`SELECT`、`FOREACH`、`DO`、`INCASE`、`WHERE`などの句で利用可能です。このセクションでは式の使用方法について説明します。以下は式を形成するために使える演算子の一覧です。また、多数の[組み込み関数](./rule-sql-builtin-functions.md)も式内で使用可能です。

### 算術演算

| 演算子 | 用途                                  | 戻り値                      |
| ------ | ------------------------------------- | --------------------------- |
| `+`    | 加算、または文字列の連結               | 合計、または連結された文字列 |
| `-`    | 減算                                  | 差分                        |
| `*`    | 乗算                                  | 積                          |
| `/`    | 除算                                  | 商                          |
| `div`  | 整数除算                              | 整数の商                    |
| `mod`  | 剰余                                  | 剰余                        |

### 論理演算

| 演算子 | 用途               | 戻り値       |
| ------ | ------------------ | ------------ |
| `>`    | より大きい         | true/false   |
| `<`    | より小さい         | true/false   |
| `<=`   | 以下               | true/false   |
| `>=`   | 以上               | true/false   |
| `<>`   | 等しくない         | true/false   |
| `!=`   | 等しくない         | true/false   |
| `=`    | 2つのオペランドが完全に等しいかを判定。値の比較に使用可能 | true/false   |
| `=~`   | トピックがトピックフィルターにマッチするかを判定。トピックマッチング専用 | true/false   |
| `and`  | 論理積             | true/false   |
| `or`   | 論理和             | true/false   |

### CASE式

`CASE`式は条件分岐を実行するために使用できます。`CASE`式は他言語のif-then-else文に相当します。以下の例で使い方を示します。

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

### `SELECT`ステートメントの例

- トピック`t/a`のメッセージからすべてのフィールドを抽出：

    ```sql
    SELECT * FROM "t/a"
    ```

- トピック`t/a`または`t/b`のメッセージからすべてのフィールドを抽出：

    ```sql
    SELECT * FROM "t/a","t/b"
    ```

- トピック`t/#`にマッチするメッセージからすべてのフィールドを抽出：

    ```sql
    SELECT * FROM "t/#"
    ```

- トピック`t/#`にマッチするメッセージから`qos`、`username`、`clientid`フィールドを抽出（出力メッセージのペイロードにこれらのフィールドが含まれます）：

    ```sql
    SELECT qos, username, clientid FROM "t/#"
    ```

- ペイロードに`username`フィールドがあり、その値が`Steven`である任意のメッセージから`username`フィールドを抽出（`FROM`句で`#`のようなトピックフィルターを使うことは推奨されません。これはすべてのメッセージに対してルールをチェックする必要があるためです）：

    ```sql
    SELECT username FROM "#" WHERE username='Steven'
    ```

- 入力メッセージのペイロードから`x`フィールドを抽出し、出力メッセージで`y`に名前変更。`y`は`WHERE`句でも使用可能。このSQLはペイロードが`{"x": 1}`のメッセージにマッチし、`{"x": 2}`にはマッチしません。

    ```sql
    SELECT payload.x as x FROM "tests/test_topic_1" WHERE y = 1
    ```

- ペイロードが`{"x": {"y": 1}}`（例：`{"x": {"y": 1}, "other": "field"}`）のメッセージにマッチするSQL：

    ```sql
    SELECT * FROM "#" WHERE payload.x.y = 1
    ```

- クライアントIDが`c1`のMQTTクライアントが接続した場合、そのソースIPアドレスとポート番号を抽出：

    ```sql
    SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
    ```

- トピック`my/topic`にマッチし、QoSレベルが1のサブスクリプションすべてにマッチし、`clientid`を抽出：

    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic = 'my/topic' and qos = 1
    ```

- 上記の例と似ていますが、トピックマッチ演算子`=~`を使い、トピックフィルター`t/#`にマッチするものにマッチ：

    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic =~ 't/#' and qos = 1
    ```

- キーが`foo`のユーザープロパティを抽出（ユーザープロパティはMQTT 5.0で新規追加されたため、古いMQTTバージョンには該当しません）：

    ```sql
    SELECT pub_props.'User-Property'.foo as foo FROM "t/#"
    ```

::: tip

- `FROM`句のトピックはダブルクォーテーション（`""`）またはシングルクォーテーション（`''`）で囲む必要があります。
- `WHERE`句の条件で文字列を使う場合はシングルクォーテーション（`''`）で囲みます。
- `FROM`句に複数のトピックがある場合はカンマ（`,`）で区切ります。例：`SELECT * FROM "t/1", "t/2"`。
- ペイロードの内部フィールドにアクセスするにはピリオド（`.`）を使います。例：ネストされたJSON構造の`payload.outer_field.inner_field`。
- ペイロードにエイリアスを付けるとパフォーマンスに影響するため、`SELECT payload as p`のような使い方は避けてください。
- 一部のエスケープシーケンスは使用時にアンエスケープが必要です。詳細は[unescape関数](./rule-sql-builtin-functions.md#unescapestring-string---string)を参照してください。

:::

### `FOREACH`ステートメントの例

クライアントIDが`c_steve`のメッセージがトピック`t/1`に届き、メッセージ本文はJSON形式で`sensors`フィールドが複数のオブジェクトを含む配列であるとします。例：

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

`sensors`配列の各オブジェクトを、オブジェクトの`idx`を使ったトピック`sensors/${idx}`に再パブリッシュし、内容はオブジェクトの`name`とします。上記の入力例に対し、ルールエンジンは以下の3つのメッセージを発行します。

1. トピック: sensors/0  
   内容: a  
2. トピック: sensors/1  
   内容: b  
3. トピック: sensors/2  
   内容: c  

このルールのアクション設定は以下の通りです。

- アクションタイプ: メッセージ再パブリッシュ
- 送信先トピック: `sensors/${idx}`
- 送信先QoS: 2
- メッセージ内容テンプレート: `${name}`

SQLステートメントは以下のように記述します。

```sql
FOREACH
    payload.sensors
FROM "t/#"
```

上記SQLの`FOREACH`句は配列`sensors`を指定しています。`FOREACH`ステートメントは結果の配列の各オブジェクトに対して「メッセージ再パブリッシュ」アクションを実行するため、3回アクションが実行されます。

#### 例2

`sensors`配列のうち、`id`フィールドの値が1以上のオブジェクトのみを、トピック`sensors/${idx}`に再パブリッシュし、内容は`clientid=${clientid},name=${name},date=${date}`とします。上記の入力例に対し、`id`が0の要素は除外されるため、2つのメッセージが発行されます。

1. トピック: sensors/1  
   内容: clientid=c_steve,name=b,date=2023-04-24  
2. トピック: sensors/2  
   内容: clientid=c_steve,name=c,date=2023-04-24  

このルールのアクション設定は以下の通りです。

- アクションタイプ: メッセージ再パブリッシュ
- 送信先トピック: `sensors/${idx}`
- 送信先QoS: 2
- メッセージ内容テンプレート: `clientid=${clientid},name=${name},date=${date}`

SQLステートメントは以下のように記述します。

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

上記SQLの`FOREACH`句は配列`sensors`を指定し、`DO`句で各操作に必要なフィールドを選択しています。`clientid`はメッセージのメタデータから、`name`と`idx`は現在のセンサーオブジェクトから選択されます。`item`は`sensors`配列の現在のオブジェクトを表します。`INCASE`句は配列オブジェクトのフィルタ条件を指定し、条件に合わないオブジェクトは無視されます。

`DO`句と`INCASE`句では、`item`で現在のオブジェクトにアクセスできますが、`FOREACH`句の`as`構文で変数名をカスタマイズすることも可能です。したがって、上記SQLは以下のようにも書けます。

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

例2を拡張し、クライアントIDの`c_steve`の`c_`プレフィックスを削除します。

ルールエンジンには`FOREACH`、`DO`、`INCASE`句で呼び出せる多数の組み込み関数があります。`c_steve`を`steve`に変換したい場合、例2のSQLを以下のように変更します。

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

複数の式を`FOREACH`句に記述可能ですが、最後の式が配列を指定している必要があります。

例えば、入力メッセージのペイロードが以下のような形式の場合：

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

`FOREACH`句でペイロードの`data`に別名を付けてから配列を選択できます。

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
