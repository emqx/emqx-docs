# スキーマレジストリ

EMQX スキーマレジストリは、MQTT メッセージのペイロードのエンコード、デコード、および検証のためのスキーマを定義・管理する機能を提供します。ルールはスキーマレジストリの関数を呼び出して、Avro や Protobuf といったバイナリペイロードをルールエンジンが処理可能なデータにデコードしたり、処理済みデータを下流システム向けにエンコードしたり、JSON データを JSON Schema に対して検証したりできます。

デバイスと下流アプリケーションが異なるフォーマットでデータをやり取りする場合にスキーマレジストリを使用します。スキーマ定義やカスタムコーデック設定を一元管理することで、各ルールやアプリケーションで変換ロジックを個別に実装することなく、メッセージを一貫したフォーマットで処理できます。

以下の図はスキーマレジストリの利用例です。複数のデバイスが異なるフォーマットでデータを報告し、スキーマレジストリがそれらを統一された内部フォーマットにデコードしてからバックエンドアプリケーションに転送しています。

<img src="./assets/schema-registry.png" alt="スキーマレジストリ" style="zoom:67%;" />

## 対応スキーマタイプ

EMQX スキーマレジストリは以下の内部スキーマタイプをサポートしています。

| スキーマタイプ | 説明 | 例 |
| --- | --- | --- |
| [Avro](https://avro.apache.org) | [Mapフォーマット](#rule-engine-internal-data-format-map)からAvroバイナリデータへのエンコードおよびAvroバイナリデータからMapフォーマットへのデコードを行います。 | [スキーマレジストリの例 - Avro](./schema-registry-example-avro.md) |
| [Protobuf](https://developers.google.com/protocol-buffers/) | MapフォーマットからProtobufバイナリデータへのエンコードおよびProtobufバイナリデータからMapフォーマットへのデコードを行います。 | [スキーマレジストリの例 - Protobuf](./schema-registry-example-protobuf.md) |
| [JSON Schema](https://json-schema.org/) | 入力されたJSONデータやルールエンジンで生成されたJSONデータがJSON Schemaに準拠しているかを検証します。 | [スキーマレジストリの例 - JSON Schema](./schema-registry-example-json.md) |
| 外部HTTPサーバー | カスタムコーデックロジックを実装したHTTPサービスにペイロードのエンコード・デコードを委任します。 | [スキーマレジストリの例 - 外部HTTPサーバー](./schema-registry-example-external-http.md) |

外部HTTPサーバーと外部スキーマレジストリは異なる統合です。外部HTTPサーバーは内部スキーマタイプの一つで、エンコード・デコードをカスタムHTTPサービスに委任します。一方、外部スキーマレジストリは別途設定し、ルール処理中に設定されたConfluentスキーマレジストリからAvroスキーマを取得します。詳細は[外部スキーマレジストリ](#external-schema-registry)をご覧ください。

### JSON Schema サポート

EMQX 6.0.4以降、スキーマレジストリはJSON Schema draft-03、draft-04、draft-06、draft 2019-09、draft 2020-12をサポートしています。EMQXは`$schema`フィールドの値に基づいてJSON Schemaのバージョンを選択します。`$schema`が省略された場合はdraft-06が使用されます。

完全な例および各ドラフトの制限事項については[スキーマレジストリの例 - JSON Schema](./schema-registry-example-json.md)を参照してください。

## アーキテクチャ設計

EMQXはスキーマを用いて、パブリッシュされたメッセージのエンコード、デコード、およびスキーマ仕様への準拠検証を行います。AvroやProtobufなどの組み込みエンコード形式のスキーマテキストを管理します。

スキーマAPIはスキーマ名による追加、照会、削除操作を提供するため、エンコード・デコード時にはスキーマ名を指定する必要があります。

![architecture](./assets/schema_registry/schema_registry1.svg)

一般的なユースケースとしては、ルールエンジンがスキーマレジストリのエンコード・デコードインターフェースを呼び出し、エンコードまたはデコードしたデータを後続のアクションの入力として利用します。

エンコード呼び出しの例：

```erlang
schema_encode(SchemaName, Map) -> Bytes
```

デコード呼び出しの例：

```erlang
schema_decode(SchemaName, Bytes) -> Map
```

JSONエンコードされたMQTTメッセージからデータをエンコードする場合、スキーマ関数でエンコードする前に`json_decode`関数でMap内部フォーマットにデコードする必要があります。例：

```erlang
schema_encode(SchemaName, json_decode(JSONData)) -> Bytes
```

JSONデータがJSONスキーマに準拠しているかをエンコード前またはデコード後に検証する場合は、以下のスキーマ検証例を使用します。

```erlang
schema_check(SchemaName, Map | Bytes) -> Boolean
```

## スキーマレジストリとルールエンジン

EMQXのメッセージ処理層は、メッセージング、ルールエンジン、データ変換の3つの部分に分けられます。

EMQXのPUB/SUBシステムはメッセージを指定されたトピックにルーティングします。ルールエンジンはデータに対するビジネスルールを柔軟に設定し、メッセージをルールにマッチさせて対応するアクションを指定します。データフォーマットの変換はルールマッチングの前に行われ、マッチングに参加可能なMapフォーマットに変換されてからマッチング処理が行われます。

<img src="./assets/SchemaAndRuleEngine.png" alt="スキーマとルールエンジン" style="zoom:67%;" />

### ルールエンジン内部データフォーマット（Map）

ルールエンジン内部で使用されるデータフォーマットはErlangのMapです。そのため、元のデータがバイナリや他のフォーマットの場合は、上記の`schema_decode`や`json_decode`などのコーデック関数でMapに変換する必要があります。MapはJSONオブジェクトに非常に似ています。

Mapはキーと値のペアで構成されるデータ構造で、`#{key => value}`の形式を取ります。例えば、`user = #{id => 1, name => "Steve"}`は`id`が`1`、`name`が`"Steve"`の`user` Mapを定義します。

SQL文では`.`演算子を使ってネストされたMapのフィールドを抽出・追加できます。以下はSQL文でのMap操作の例です。

```sql
SELECT user.id AS my_id
```

このSQL文のフィルター結果は`#{my_id => 1}`となります。

### JSONコーデック

ルールエンジンのSQL文はJSON形式の文字列のエンコード・デコードをサポートしています。JSON文字列をMapフォーマットに変換するSQL関数は`json_decode()`および`json_encode()`です。

```sql
SELECT json_decode(payload) AS p FROM "t/#" WHERE p.x = p.y
```

上記SQL文は、トピック`t/a`のペイロードがJSON文字列`{"x": 1, "y": 1}`であるMQTTメッセージにマッチします。

`json_decode(payload) as p`はJSON文字列を以下のMapデータ構造にデコードし、`WHERE`句で`p.x`や`p.y`としてMap内のフィールドを利用可能にします。

```erlang
#{
  p => #{
    x => 1,
    y => 1
  }
}
```

**注意:** `AS`句はデコードしたデータにキーを割り当て、後続の操作で使用できるようにするために必須です。

## 外部スキーマレジストリ

EMQX 5.8.1以降、外部Confluentスキーマレジストリ（CSR）の設定をサポートしています。この機能により、ルール処理中に外部レジストリから動的にスキーマを取得し、効率的なメッセージのエンコード・デコードが可能になります。

### ダッシュボードでの外部スキーマレジストリ作成

EMQXダッシュボードから直接外部スキーマレジストリを設定でき、スキーマ統合の管理が容易です。

EMQXダッシュボードの **Smart Data Hub** -> **Schema Registry** に移動し、スキーマページの **External** タブを選択します。

右上の **Create** ボタンをクリックし、以下の項目を設定します。

- **Name**: エンコード・デコード関数で使用する外部スキーマレジストリ名を入力します。
- **Type**: 外部スキーマレジストリのタイプを選択します。現在は`Confluent`のみ対応しています。
- **URL**: Confluentスキーマレジストリのエンドポイントを入力します。
- **Authentication**: `Basic auth`を選択した場合、外部レジストリにアクセスするための認証情報（ユーザー名とパスワード）を入力します。

設定完了後、**Create** をクリックします。

### 設定ファイルでの外部スキーマレジストリ設定

EMQXの設定ファイルで外部Confluentスキーマレジストリを設定する例は以下の通りです。

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

- `my_external_registry` は外部スキーマレジストリに割り当てる名前です。
- `type = confluent` は外部レジストリのタイプを指定しています。
- `url` はConfluentスキーマレジストリのエンドポイントです。
- `auth` は外部レジストリにアクセスするための認証情報（ユーザー名とパスワード）を含みます。

### ルールエンジンでの外部スキーマレジストリ利用

外部レジストリを設定した後、EMQXルールエンジンで外部レジストリに格納されたスキーマを使ってペイロードのエンコード・デコードを行う複数の関数が利用可能です。

以下の関数は設定済みの外部CSRを利用します。

```sql
avro_encode('my_external_registry', payload, my_schema_id)
avro_decode('my_external_registry', payload, my_schema_id)
schema_encode_and_tag('my_local_avro_schema', 'my_external_registry', payload, 'my_subject')
schema_decode_tagged('my_external_registry', payload)
```

#### 関数利用例

以下の例では、次の値と変数名を使用しています。

- `my_external_registry`: EMQXで外部レジストリに割り当てた名前
- `my_schema_id`: CSRに登録されたスキーマID（CSRでは常に整数）
- `my_local_avro_schema`: EMQXにローカルで設定されたAvroスキーマ名
- `my_subject`: CSRで定義されたサブジェクト名

##### `avro_encode`

`avro_encode`は外部レジストリのスキーマIDを使ってペイロードをエンコードします。スキーマは実行時に動的に取得され、その後キャッシュされます。ConfluentスキーマレジストリではスキーマIDは整数です。

::: tip 注意

エンコード時のペイロードはルールエンジンの内部データフォーマットであるデコード済みMapである必要があるため、例では`json_decode`を使用しています。

:::

例：

```sql
select
  -- 123はCSRに登録されたスキーマID
  avro_encode('my_external_registry', json_decode(payload), 123) as encoded
from 't'
```

##### `avro_decode`

この関数は外部レジストリの指定されたスキーマIDに基づいてAvroペイロードをデコードします。スキーマは実行時に動的に取得され、その後キャッシュされます。

例：

```sql
select
  -- 123はCSRに登録されたスキーマID
  avro_decode('my_external_registry', payload, 123) as decoded
from 't'
```

##### `schema_encode_and_tag`

この関数はローカルに登録されたAvroスキーマ、外部CSRスキーマ名、およびサブジェクトを使って、すでに内部Mapフォーマットになっているペイロードをエンコードし、結果のペイロードにスキーマIDのタグ付けを行います。スキーマIDはローカルスキーマをCSRに登録することで得られます。

例：

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

この関数はCSR名を使って、スキーマIDでタグ付けされたペイロードをデコードします。

```sql
select
  schema_decode_tagged(
    'my_external_registry',
    payload
  ) as decoded
from 't'
```
