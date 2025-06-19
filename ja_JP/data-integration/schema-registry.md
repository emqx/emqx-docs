# スキーマレジストリ

IoTデバイス端末の多様性や各メーカーによる異なるコーディングフォーマットのため、プラットフォーム上のアプリケーションがデバイス管理のためにIoTプラットフォームにアクセスする際に、統一されたデータフォーマットの必要性が生じます。

スキーマレジストリは、エンコードおよびデコードに使用されるスキーマを管理し、エンコードやデコードのリクエストを処理して結果を返します。スキーマレジストリはルールエンジンと連携して、さまざまなシナリオにおけるデバイスアクセスやルール設計に適応可能です。

EMQXのスキーマレジストリは現在、以下の形式のコーデックをサポートしています：

- [Avro](https://avro.apache.org)
- [Protobuf](https://developers.google.com/protocol-buffers/)
- [JSON Schema](https://json-schema.org/)
- 外部HTTPサーバー

AvroおよびProtobufはスキーマ依存のデータフォーマットです。エンコードされたデータはバイナリ形式で、デコードされたデータは[Map形式](#rule-engine-internal-data-format-map)になります。デコードされたデータはルールエンジンや他のプラグインで直接利用可能です。スキーマレジストリはAvroやProtobufなどの組み込みエンコードフォーマットのスキーマテキストを管理します。

JSONスキーマは、入力されたJSONオブジェクトがスキーマ定義に準拠しているか、またはルールエンジンから出力されたJSONオブジェクトが下流にデータを生成する前に有効かどうかを検証するために使用できます。

外部HTTPサーバーは、ペイロードのすべてのデコードおよびエンコードを設定されたブラックボックスサーバーを経由させ、そのロジックを処理します。カスタムのエンコード／デコードロジックを実装したい場合に有用です。

以下の図はスキーマレジストリの適用例を示しています。複数のデバイスが異なるフォーマットでデータを報告し、スキーマレジストリで統一された内部フォーマットにデコードされ、バックエンドアプリケーションに転送されます。

<img src="./assets/schema-registry.png" alt="スキーマレジストリ" style="zoom:67%;" />

## アーキテクチャ設計

EMQXは、パブリッシュされるメッセージのエンコード、デコード、スキーマ仕様への準拠検証にスキーマを利用できます。AvroやProtobufなどの組み込みエンコードフォーマットのスキーマテキストを管理しています。

スキーマAPIはスキーマ名による追加、照会、削除操作を提供するため、エンコードやデコード時にはスキーマ名の指定が必要です。

![architecture](./assets/schema_registry/schema_registry1.svg)

一般的なユースケースとしては、ルールエンジンからスキーマレジストリが提供するエンコード・デコードインターフェースを呼び出し、その結果のエンコード済みまたはデコード済みデータを後続のアクションの入力として利用します。

エンコード呼び出しの例：

```erlang
schema_encode(SchemaName, Map) -> Bytes
```

デコード呼び出しの例：

```erlang
schema_decode(SchemaName, Bytes) -> Map
```

JSONエンコードされたMQTTメッセージのデータをエンコードする際は、スキーマ関数でエンコードする前に`json_decode`関数でMap内部フォーマットにデコードする必要があります。例：

```erlang
schema_encode(SchemaName, json_decode(Map)) -> Bytes
```

JSONデータがJSONスキーマに準拠しているかをエンコード前またはデコード後に検証する場合は、以下のスキーマ検証関数を使用します：

```erlang
schema_check(SchemaName, Map | Bytes) -> Boolean
```

## スキーマレジストリとルールエンジン

EMQXのメッセージ処理層は、メッセージング、ルールエンジン、データ変換の3つの部分に分けられます。

EMQXのPUB/SUBシステムはメッセージを指定されたトピックにルーティングします。ルールエンジンはデータに対するビジネスルールを柔軟に設定でき、メッセージをルールにマッチングさせて対応するアクションを指定します。データフォーマットの変換はルールマッチングの前に行われ、データをルールマッチングに参加可能なMap形式に変換してからマッチングします。

<img src="./assets/SchemaAndRuleEngine.png" alt="スキーマとルールエンジン" style="zoom:67%;" />

### ルールエンジン内部データフォーマット（Map）

ルールエンジン内部で使用されるデータフォーマットはErlangのMapです。元のデータがバイナリや他の形式の場合は、上記の`schema_decode`や`json_decode`などのコーデック関数を使ってMapに変換する必要があります。JSONオブジェクトに非常に近い構造です。

Mapはキーと値のペアのデータ構造で、`#{key => value}`の形式を取ります。例えば、`user = #{id => 1, name => "Steve"}`は`id`が`1`、`name`が`"Steve"`の`user` Mapを定義しています。

SQL文は`.`演算子を提供し、ネストされたMapのフィールドを抽出・追加できます。以下はSQL文でのMap操作の例です：

```sql
SELECT user.id AS my_id
```

このSQL文のフィルター結果は`#{my_id => 1}`となります。

### JSONコーデック

ルールエンジンのSQL文はJSON形式の文字列のエンコード・デコードをサポートしています。JSON文字列をMap形式に変換するSQL関数は`json_decode()`と`json_encode()`です：

```sql
SELECT json_decode(payload) AS p FROM "t/#" WHERE p.x = p.y
```

上記SQL文は、ペイロードの内容がJSON文字列`{"x" = 1, "y" = 1}`で、トピックが`t/a`のMQTTメッセージにマッチします。

`json_decode(payload) as p`はJSON文字列を以下のMapデータ構造にデコードし、`WHERE`句でp.xやp.yを使ってフィールドを参照可能にします。

```erlang
#{
  p => #{
    x => 1,
    y => 1
  }
}
```

**注意：** `AS`句はデコードしたデータにキーを割り当て、後続の操作で利用可能にするために必須です。

## 外部スキーマレジストリ

バージョン5.8.1以降、EMQXは外部のConfluent Schema Registry（CSR）を設定可能になりました。この機能により、ルール処理中に外部レジストリからスキーマを動的に取得し、効率的にメッセージのエンコード・デコードが行えます。

### ダッシュボードでの外部スキーマレジストリ作成

EMQXダッシュボードから直接外部スキーマレジストリを設定でき、スキーマ統合の管理が容易です。

EMQXダッシュボードの **Smart Data Hub** -> **Schema Registry** に移動し、スキーマページの **External** タブを選択します。

右上の **Create** ボタンをクリックし、以下の項目を設定します：

- **Name**：エンコード・デコード関数で使用する外部スキーマレジストリ名を入力します。
- **Type**：外部スキーマレジストリの種類を選択します。現在は`Confluent`のみ対応しています。
- **URL**：Confluent Schema Registryのエンドポイントを入力します。
- **Authentication**：`Basic auth`を選択した場合、外部レジストリにアクセスするための認証情報（ユーザー名とパスワード）を入力します。

設定完了後、**Create** をクリックします。

### 設定ファイルによる外部スキーマレジストリの設定

EMQXの設定ファイルで外部Confluent Schema Registryを設定することも可能です。以下は設定例です：

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
- `type = confluent` は外部レジストリの種類を指定しています。
- `url` はConfluent Schema Registryのエンドポイントです。
- `auth` は外部レジストリにアクセスするための認証情報（ユーザー名とパスワード）です。

### ルールエンジンでの外部スキーマレジストリ利用

外部レジストリを設定すると、EMQXルールエンジンで外部レジストリに格納されたスキーマを使ってペイロードのエンコード・デコードを行う複数の関数が利用可能になります。

以下の関数は設定済みの外部CSRを利用します：

```sql
avro_encode('my_external_registry', payload, my_schema_id)
avro_decode('my_external_registry', payload, my_schema_id)
schema_encode_and_tag('my_local_avro_schema', 'my_external_registry', payload, 'my_subject')
schema_decode_tagged('my_external_registry', payload)
```

#### 関数利用例

以下の関数利用例では、次の値と変数名を使用しています：

- `my_external_registry`：EMQXで外部レジストリに割り当てた名前
- `my_schema_id`：CSRに登録されたスキーマID（CSRでは常に整数）
- `my_local_avro_schema`：EMQXにローカル設定されたAvroスキーマ名
- `my_subject`：CSRで定義されたサブジェクト名

##### `avro_encode`

`avro_encode`は外部レジストリのスキーマIDを使ってペイロードをエンコードします。スキーマは実行時に動的に取得され、その後の実行でキャッシュされます。Confluent Schema RegistryではスキーマIDは整数です。

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

この関数は外部レジストリの指定スキーマIDに基づきAvroペイロードをデコードします。スキーマは実行時に動的に取得され、その後の操作でキャッシュされます。

例：

```sql
select
  -- 123はCSRに登録されたスキーマID
  avro_decode('my_external_registry', payload, 123) as decoded
from 't'
```

##### `schema_encode_and_tag`

この関数はローカル登録済みAvroスキーマ、外部CSRスキーマ名、サブジェクトを使い、（すでに内部Map形式の）ペイロードをエンコードし、結果のペイロードにスキーマIDでタグ付けします。スキーマIDはローカルスキーマをCSRに登録した際のものです。

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
