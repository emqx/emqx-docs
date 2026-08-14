# スキーマレジストリ

EMQX スキーマレジストリは、MQTT メッセージのペイロードをエンコード、デコード、検証するためのスキーマを定義および管理できます。ルールはスキーマレジストリの機能を呼び出して、Avro や Protobuf などのバイナリペイロードをルールエンジンが処理可能なデータにデコードしたり、処理済みデータを下流システム向けにエンコードしたり、JSON データを JSON スキーマに対して検証したりできます。

デバイスと下流アプリケーションが異なるフォーマットでデータを交換する場合にスキーマレジストリを使用します。スキーマ定義とカスタムコーデック設定を一元管理することで、各ルールやアプリケーションで変換ロジックを個別に実装することなく、ルールが一貫したフォーマットでメッセージを処理できます。

以下の図はスキーマレジストリの利用例です。複数のデバイスが異なるフォーマットでデータを報告し、スキーマレジストリで統一された内部フォーマットにデコードされてからバックエンドアプリケーションに転送されます。

<img src="./assets/schema-registry.png" alt="スキーマレジストリ" style="zoom:67%;" />

## 対応スキーマタイプ

EMQX スキーマレジストリは以下の内部スキーマタイプをサポートしています。

| スキーマタイプ | 説明 | 例 |
| --- | --- | --- |
| [Avro](https://avro.apache.org) | [Map フォーマット](#rule-engine-internal-data-format-map) から Avro バイナリデータへのエンコードおよび Avro バイナリデータから Map フォーマットへのデコードを行います。 | [スキーマレジストリの例 - Avro](./schema-registry-example-avro.md) |
| [Protobuf](https://developers.google.com/protocol-buffers/) | Map フォーマットから Protobuf バイナリデータへのエンコードおよび Protobuf バイナリデータから Map フォーマットへのデコードを行います。 | [スキーマレジストリの例 - Protobuf](./schema-registry-example-protobuf.md) |
| [JSON Schema](https://json-schema.org/) | 入力された JSON データやルールエンジンで生成された JSON データが JSON スキーマに準拠しているか検証します。 | [スキーマレジストリの例 - JSON Schema](./schema-registry-example-json.md) |
| 外部 HTTP サーバー | カスタムコーデックロジックを実装した HTTP サービスにペイロードのエンコード・デコードを委譲します。 | [スキーマレジストリの例 - 外部 HTTP サーバー](./schema-registry-example-external-http.md) |

外部 HTTP サーバーと外部スキーマレジストリは異なる統合機能です。外部 HTTP サーバーは内部スキーマタイプの一つで、エンコード・デコード処理をカスタム HTTP サービスに委譲します。外部スキーマレジストリは別途設定され、ルール処理中に設定された Confluent スキーマレジストリから Avro スキーマを取得します。詳細は[外部スキーマレジストリ](#external-schema-registry)をご覧ください。

### JSON Schema サポート

EMQX 6.0.4 以降、スキーマレジストリは JSON Schema draft-03、draft-04、draft-06、draft 2019-09、draft 2020-12 をサポートしています。EMQX は `$schema` フィールドの値に基づいて JSON Schema のバージョンを選択します。`$schema` が省略された場合は draft-06 が使用されます。

完全な例と各ドラフトの制限については[スキーマレジストリの例 - JSON Schema](./schema-registry-example-json.md)をご参照ください。

## アーキテクチャ設計

EMQX はパブリッシュされたメッセージのエンコード、デコード、スキーマ仕様への準拠検証にスキーマを使用できます。Avro や Protobuf などの組み込みエンコードフォーマットのスキーマテキストを管理します。

スキーマ API はスキーマ名による追加、照会、削除操作を提供するため、エンコード・デコード時にはスキーマ名を指定する必要があります。

![architecture](./assets/schema_registry/schema_registry1.svg)

一般的なユースケースとして、ルールエンジンがスキーマレジストリのエンコード・デコードインターフェースを呼び出し、その結果のエンコードまたはデコード済みデータを後続のアクションの入力として利用します。

エンコード呼び出しの例:

```erlang
schema_encode(SchemaName, Map) -> Bytes
```

デコード呼び出しの例:

```erlang
schema_decode(SchemaName, Bytes) -> Map
```

JSON エンコードされた MQTT メッセージのデータをエンコードする場合は、スキーマ関数でエンコードする前に `json_decode` 関数で Map 内部フォーマットにデコードする必要があります。例:

```erlang
schema_encode(SchemaName, json_decode(JSONData)) -> Bytes
```

JSON データがエンコード前またはデコード後に JSON スキーマに準拠しているか検証する場合は、以下のスキーマ検証例を使用します。

```erlang
schema_check(SchemaName, Map | Bytes) -> Boolean
```

## スキーマレジストリとルールエンジン

EMQX のメッセージ処理層は、メッセージング、ルールエンジン、データ変換の3つに分けられます。

EMQX の PUB/SUB システムはメッセージを指定されたトピックにルーティングします。ルールエンジンはデータに対するビジネスルールを柔軟に設定し、メッセージをルールにマッチングさせて対応するアクションを指定します。データフォーマットの変換はルールマッチングの前に行われ、マップ形式に変換してルールマッチングに参加できるようにします。

<img src="./assets/SchemaAndRuleEngine.png" alt="スキーマとルールエンジン" style="zoom:67%;" />

### ルールエンジン内部データフォーマット（Map）

ルールエンジン内部で使用されるデータフォーマットは Erlang の Map です。元のデータがバイナリや他のフォーマットの場合は、上記の `schema_decode` や `json_decode` などのコーデック関数を使って Map に変換する必要があります。JSON オブジェクトに非常に似ています。

Map はキーと値の形式のデータ構造で、`#{key => value}` の形をとります。例えば、`user = #{id => 1, name => "Steve"}` は `id` が `1`、`name` が `"Steve"` の `user` Map を定義しています。

SQL 文は `.` 演算子でネストされた Map のフィールドを抽出・追加できます。以下は SQL 文による Map 操作の例です。

```sql
SELECT user.id AS my_id
```

この SQL 文のフィルター結果は `#{my_id => 1}` となります。

### JSON コーデック

ルールエンジンの SQL 文は JSON 形式の文字列のエンコード・デコードをサポートしています。JSON 文字列を Map フォーマットに変換する SQL 関数は `json_decode()` と `json_encode()` です。

```sql
SELECT json_decode(payload) AS p FROM "t/#" WHERE p.x = p.y
```

上記の SQL 文は、トピックが `t/a` でペイロードが JSON 文字列 `{"x": 1, "y": 1}` の MQTT メッセージにマッチします。

`json_decode(payload) as p` は JSON 文字列を以下の Map データ構造にデコードし、`WHERE` 句で `p.x` や `p.y` として Map 内のフィールドを利用できるようにします。

```erlang
#{
  p => #{
    x => 1,
    y => 1
  }
}
```

**注意:** `AS` 句はデコードしたデータにキーを割り当て、後続の操作で利用できるようにするために必要です。

## 外部スキーマレジストリ

EMQX 5.8.1 以降、外部 Confluent スキーマレジストリ（CSR）を設定可能になりました。この機能により、ルール処理中に外部レジストリから動的にスキーマを取得し、効率的なメッセージのエンコード・デコードが可能です。

### ダッシュボードで外部スキーマレジストリを作成

EMQX ダッシュボードから外部スキーマレジストリを直接設定でき、スキーマ統合の管理が簡単です。

EMQX ダッシュボードの **Smart Data Hub** -> **Schema Registry** に移動し、スキーマページの **External** タブを選択します。

右上の **Create** ボタンをクリックし、以下の項目を設定します。

- **Name**: エンコード・デコード関数で使用する外部スキーマレジストリ名を入力します。
- **Type**: 外部スキーマレジストリのタイプを選択します。現在は `Confluent` のみ対応しています。
- **URL**: Confluent スキーマレジストリのエンドポイントを入力します。
- **Authentication**: `Basic auth` を選択した場合、外部レジストリにアクセスするための認証情報（ユーザー名とパスワード）を入力します。

設定が完了したら **Create** をクリックします。

### 設定ファイルで外部スキーマレジストリを設定

EMQX の設定ファイルで外部 Confluent スキーマレジストリを設定することも可能です。以下は設定例です。

```hcl
schema_registry {
  external {
    my_external_registry {
      type = confluent
      url = "https://confluent.registry.url:8081"
      auth {
        username = "myuser"
        password = "secret"
      }
    }
  }
}
```

この例では、

- `my_external_registry` が外部スキーマレジストリの名前です。
- `type = confluent` は外部レジストリのタイプを指定しています。
- `url` は Confluent スキーマレジストリのエンドポイントです。
- `auth` は外部レジストリにアクセスするための認証情報（ユーザー名とパスワード）です。

### ルールエンジンで外部スキーマレジストリを使用

外部レジストリを設定後、EMQX ルールエンジンの複数の関数で外部レジストリに保存されたスキーマを利用してペイロードのエンコード・デコードが可能です。

以下の関数は設定された外部 CSR を利用します。

```sql
avro_encode('my_external_registry', payload, my_schema_id)
avro_decode('my_external_registry', payload, my_schema_id)
schema_encode_and_tag('my_local_avro_schema', 'my_external_registry', payload, 'my_subject')
schema_decode_tagged('my_external_registry', payload)
```

#### 関数使用例

以下の関数使用例では、以下の例値と変数名を使用しています。

- `my_external_registry` は EMQX で設定した外部レジストリ名です。
- `my_schema_id` は CSR に登録されたスキーマ ID（CSR では常に整数）です。
- `my_local_avro_schema` は EMQX にローカル設定された Avro スキーマ名です。
- `my_subject` は CSR で定義されたサブジェクト名です。

##### `avro_encode`

`avro_encode` は外部レジストリのスキーマ ID を使ってペイロードをエンコードします。スキーマは実行時に動的に取得され、その後キャッシュされます。Confluent スキーマレジストリではスキーマ ID は整数です。

::: tip 注意

エンコード時のペイロードはルールエンジンの内部データフォーマットであるデコード済み Map である必要があります。これが例で `json_decode` を使う理由です。

:::

例:

```sql
select
  -- 123 は CSR に登録されたスキーマ ID
  avro_encode('my_external_registry', json_decode(payload), 123) as encoded
from 't'
```

##### `avro_decode`

この関数は外部レジストリの指定されたスキーマ ID に基づいて Avro ペイロードをデコードします。スキーマは実行時に動的に取得され、その後キャッシュされます。

例:

```sql
select
  -- 123 は CSR に登録されたスキーマ ID
  avro_decode('my_external_registry', payload, 123) as decoded
from 't'
```

##### `schema_encode_and_tag`

この関数はローカルに登録された Avro スキーマ名、外部 CSR スキーマ名、サブジェクトを使って、ペイロード（すでに内部 Map フォーマット）をエンコードし、結果のペイロードにスキーマ ID タグを付けます。スキーマ ID はローカルスキーマを CSR に登録した際のものです。

例:

```sql
select
  schema_encode_and_tag(
    'my_local_avro_schema',
    'my_external_registry',
    json_decode(payload),
    'my_subject'
  ) as encoded
from 't'
```

##### `schema_decode_tagged`

この関数は CSR 名を使って、スキーマ ID タグ付きのペイロードをデコードします。

```sql
select
  schema_decode_tagged(
    'my_external_registry',
    payload
  ) as decoded
from 't'
```
