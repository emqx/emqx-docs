# スキーマレジストリ

EMQX スキーマレジストリは、MQTTメッセージのペイロードのエンコード、デコード、および検証のためのスキーマを定義・管理する機能を提供します。ルールはスキーマレジストリの関数を呼び出して、AvroやProtobufなどのバイナリペイロードをルールエンジンが処理可能なデータにデコードしたり、処理済みデータを下流システム向けにエンコードしたり、JSONデータをJSONスキーマに対して検証したりできます。

デバイスと下流アプリケーションが異なるフォーマットでデータをやり取りする場合にスキーマレジストリを使用します。スキーマ定義やカスタムコーデック設定を一元管理することで、各ルールやアプリケーションで変換ロジックを個別に実装することなく、一貫したフォーマットでメッセージを処理できます。

下図はスキーマレジストリの利用例です。複数のデバイスが異なるフォーマットでデータを報告し、スキーマレジストリで統一された内部フォーマットにデコードした後、バックエンドアプリケーションに転送されます。

<img src="./assets/schema-registry.png" alt="スキーマレジストリ" style="zoom:67%;" />

## 対応スキーマタイプ

EMQX スキーマレジストリは以下の内部スキーマタイプをサポートしています。

| スキーマタイプ | 説明 | 例 |
| --- | --- | --- |
| [Avro](https://avro.apache.org) | [Map形式](#rule-engine-internal-data-format-map)のデータをAvroバイナリデータにエンコードし、AvroバイナリデータをMap形式にデコードします。 | [スキーマレジストリの例 - Avro](./schema-registry-example-avro.md) |
| [Protobuf](https://developers.google.com/protocol-buffers/) | Map形式のデータをProtobufバイナリデータにエンコードし、ProtobufバイナリデータをMap形式にデコードします。 | [スキーマレジストリの例 - Protobuf](./schema-registry-example-protobuf.md) |
| [JSON Schema](https://json-schema.org/) | 入力されたJSONデータやルールエンジンが生成したJSONデータがJSONスキーマに準拠しているか検証します。 | [スキーマレジストリの例 - JSON Schema](./schema-registry-example-json.md) |
| 外部HTTPサーバー | カスタムコーデックロジックを実装したHTTPサービスにペイロードのエンコード・デコードを委譲します。 | [スキーマレジストリの例 - 外部HTTPサーバー](./schema-registry-example-external-http.md) |

外部HTTPサーバーと外部スキーマレジストリは異なる連携です。外部HTTPサーバーは内部スキーマタイプの一つで、カスタムHTTPサービスにエンコード・デコードを委譲します。一方、外部スキーマレジストリは別途設定し、ルール処理時に設定済みのConfluentスキーマレジストリからAvroスキーマを取得します。詳細は[外部スキーマレジストリ](#external-schema-registry)を参照してください。

### JSON Schemaのサポート

EMQX 6.0.4以降、スキーマレジストリはJSON Schema draft-03、draft-04、draft-06、draft 2019-09、draft 2020-12をサポートしています。EMQXは`$schema`フィールドの値に基づいてJSON Schemaのバージョンを選択し、`$schema`が省略された場合はdraft-06を使用します。

完全な例と各ドラフトの制限事項については[スキーマレジストリの例 - JSON Schema](./schema-registry-example-json.md)をご覧ください。

## アーキテクチャ設計

EMQXはスキーマを用いてパブリッシュされたメッセージのエンコード、デコード、およびスキーマ仕様への準拠検証を行えます。AvroやProtobufなどの組み込みエンコードフォーマットのスキーマテキストを管理します。

スキーマAPIはスキーマ名による追加、照会、削除操作を提供するため、エンコード・デコード時にはスキーマ名を指定する必要があります。

![architecture](./assets/schema_registry/schema_registry1.svg)

一般的なユースケースとしては、ルールエンジンからスキーマレジストリが提供するエンコード・デコードインターフェースを呼び出し、その結果のデータを後続のアクションの入力として利用します。

エンコード呼び出しの例：

```erlang
schema_encode(SchemaName, Map) -> Bytes
```

デコード呼び出しの例：

```erlang
schema_decode(SchemaName, Bytes) -> Map
```

MQTTメッセージのデータがJSONエンコードされている場合、スキーマ関数でエンコードする前に`json_decode`関数でMap内部形式にデコードする必要があります。例えば：

```erlang
schema_encode(SchemaName, json_decode(JSONData)) -> Bytes
```

JSONデータがスキーマに準拠しているかをエンコード前またはデコード後に検証する場合は、以下のスキーマ検証例を使用します。

```erlang
schema_check(SchemaName, Map | Bytes) -> Boolean
```

## スキーマレジストリとルールエンジン

EMQXのメッセージ処理層は、メッセージング、ルールエンジン、データ変換の3つに分けられます。

EMQXのPUB/SUBシステムはメッセージを指定されたトピックにルーティングします。ルールエンジンはデータに対するビジネスルールを柔軟に設定でき、メッセージをルールにマッチさせて対応するアクションを指定します。データフォーマット変換はルールマッチング処理の前に行われ、データをルールマッチングに参加可能なMap形式に変換してからマッチングを行います。

<img src="./assets/SchemaAndRuleEngine.png" alt="スキーマレジストリとルールエンジン" style="zoom:67%;" />

### ルールエンジン内部データ形式（Map）

ルールエンジン内部で使用されるデータ形式はErlangのMapです。元のデータがバイナリや他の形式の場合は、上記の`schema_decode`や`json_decode`などのコーデック関数でMapに変換する必要があります。JSONオブジェクトに非常に似ています。

Mapは`#{key => value}`の形式を持つキー・バリューのデータ構造です。例えば、`user = #{id => 1, name => "Steve"}`は`id`が`1`、`name`が`"Steve"`の`user` Mapを定義します。

SQL文は`.`演算子を提供し、ネストされたMapフィールドの抽出や追加が可能です。以下はSQL文でのMap操作の例です。

```sql
SELECT user.id AS my_id
```

このSQL文のフィルター結果は`#{my_id => 1}`となります。

### JSONコーデック

ルールエンジンのSQL文はJSON形式文字列のエンコード・デコードをサポートしています。JSON文字列をMap形式に変換するSQL関数は`json_decode()`と`json_encode()`です。

```sql
SELECT json_decode(payload) AS p FROM "t/#" WHERE p.x = p.y
```

上記SQL文は、ペイロードの内容がJSON文字列`{"x": 1, "y": 1}`でトピックが`t/a`のMQTTメッセージにマッチします。

`json_decode(payload) as p`はJSON文字列を以下のMapデータ構造にデコードし、`WHERE`句で`p.x`や`p.y`のようにMap内のフィールドを利用可能にします。

```erlang
#{
  p => #{
    x => 1,
    y => 1
  }
}
```

**注意:** `AS`句はデコードしたデータをキーに割り当てるために必要で、後続の操作で利用可能にします。

## 外部スキーマレジストリ

バージョン5.8.1以降、EMQXは外部Confluentスキーマレジストリ（CSR）の設定をサポートしています。この機能により、ルール処理時に外部レジストリから動的にスキーマを取得し、効率的なメッセージのエンコード・デコードが可能になります。

### ダッシュボードでの外部スキーマレジストリ作成

EMQXダッシュボードから直接外部スキーマレジストリを設定でき、スキーマ連携の管理が容易です。

EMQXダッシュボードの **Smart Data Hub** -> **Schema Registry** に移動し、スキーマページの **External** タブを選択します。

右上の **Create** ボタンをクリックし、以下の項目を設定します。

- **Name**: エンコード・デコード関数で使用する外部スキーマレジストリ名を入力します。
- **Type**: 外部スキーマレジストリのタイプを選択します。現在は`Confluent`のみ対応しています。
- **URL**: Confluentスキーマレジストリのエンドポイントを入力します。
- **Authentication**: `Basic auth`を選択した場合、外部レジストリへのアクセス認証情報（ユーザー名とパスワード）を入力します。

設定完了後、**Create** をクリックします。

### 設定ファイルによる外部スキーマレジストリの設定

EMQX設定ファイルで外部Confluentスキーマレジストリを設定する例は以下の通りです。

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

この例では：

- `my_external_registry` は外部スキーマレジストリに割り当てた名前です。
- `type = confluent` は外部レジストリのタイプを指定します。
- `url` はConfluentスキーマレジストリのエンドポイントです。
- `auth` は外部レジストリへのアクセス認証情報（ユーザー名とパスワード）です。

### ルールエンジンでの外部スキーマレジストリ利用

外部レジストリを設定すると、EMQXルールエンジンで以下の関数を使い、外部レジストリに保存されたスキーマを用いたペイロードのエンコード・デコードが可能です。

```sql
avro_encode('my_external_registry', payload, my_schema_id)
avro_decode('my_external_registry', payload, my_schema_id)
schema_encode_and_tag('my_local_avro_schema', 'my_external_registry', payload, 'my_subject')
schema_decode_tagged('my_external_registry', payload)
```

#### 関数利用例

以下の例では、以下の値と変数名を使用しています。

- `my_external_registry`: EMQXで外部レジストリに割り当てた名前
- `my_schema_id`: CSRに登録されたスキーマID（CSRでは常に整数）
- `my_local_avro_schema`: EMQXにローカル登録されたAvroスキーマ名
- `my_subject`: CSRで定義されたサブジェクト名

##### `avro_encode`

`avro_encode`は外部レジストリのスキーマIDを使ってペイロードをエンコードします。スキーマは実行時に動的に取得され、その後キャッシュされます。ConfluentスキーマレジストリではスキーマIDは整数です。

::: tip 注意

エンコード時のペイロードはルールエンジンの内部データ形式であるデコード済みMapである必要があります。これが例で`json_decode`を使う理由です。

:::

例：

```sql
select
  -- 123はCSRに登録されたスキーマID
  avro_encode('my_external_registry', json_decode(payload), 123) as encoded
from 't'
```

##### `avro_decode`

指定した外部レジストリのスキーマIDに基づき、Avroペイロードをデコードします。スキーマは実行時に動的に取得され、以降キャッシュされます。

例：

```sql
select
  -- 123はCSRに登録されたスキーマID
  avro_decode('my_external_registry', payload, 123) as decoded
from 't'
```

##### `schema_encode_and_tag`

ローカル登録されたAvroスキーマ、外部CSRスキーマ名、サブジェクトを使い、ペイロード（すでに内部Map形式）をエンコードし、結果のペイロードにスキーマIDタグを付与します。スキーマIDはローカルスキーマをCSRに登録して取得します。

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

CSR名を使って、スキーマIDタグ付きのペイロードをデコードします。

```sql
select
  schema_decode_tagged(
    'my_external_registry',
    payload
  ) as decoded
from 't'
```
