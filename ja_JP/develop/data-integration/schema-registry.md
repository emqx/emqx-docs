# スキーマレジストリ

IoTデバイス端末の多様性や各メーカーによる異なるコーディングフォーマットのため、プラットフォーム上のアプリケーションがデバイス管理のためにIoTプラットフォームにアクセスする際に、統一されたデータフォーマットの必要性が生じます。

スキーマレジストリは、エンコードおよびデコードに使用されるスキーマを管理し、エンコードやデコードのリクエストを処理して結果を返します。スキーマレジストリはルールエンジンと連携して、さまざまなシナリオにおけるデバイスアクセスやルール設計に適応可能です。

EMQXのスキーマレジストリは現在、以下のフォーマットのコーデックをサポートしています：

- [Avro](https://avro.apache.org)
- [Protobuf](https://developers.google.com/protocol-buffers/)
- [JSON Schema](https://json-schema.org/)
- 外部HTTPサーバー

AvroおよびProtobufはスキーマ依存のデータフォーマットです。エンコードされたデータはバイナリであり、デコードされたデータは[Mapフォーマット](#rule-engine-internal-data-format-map)となります。デコードされたデータはルールエンジンや他のプラグインで直接利用可能です。スキーマレジストリはAvroやProtobufなどの組み込みエンコードフォーマットのスキーマテキストを保持します。

JSONスキーマは、入力JSONオブジェクトがスキーマ定義に準拠しているか、またはルールエンジンから出力されたJSONオブジェクトが下流にデータを生成する前に有効かどうかを検証するために使用できます。

外部HTTPサーバーは、ペイロードのすべてのデコードおよびエンコードを設定されたブラックボックスサーバー経由で処理し、ロジックを担当します。カスタムのエンコード／デコードロジックを持ちたい場合に有用です。

以下の図はスキーマレジストリのアプリケーション例を示しています。複数のデバイスが異なるフォーマットでデータを報告し、スキーマレジストリがそれらを統一された内部フォーマットにデコードしてからバックエンドアプリケーションに転送します。

<img src="./assets/schema-registry.png" alt="スキーマレジストリ" style="zoom:67%;" />

## アーキテクチャ設計

EMQXはスキーマを用いてメッセージのエンコード、デコード、およびパブリッシュされたメッセージがスキーマ仕様に準拠しているかの検証を行えます。AvroやProtobufなどの組み込みエンコードフォーマットのスキーマテキストを保持します。

スキーマAPIはスキーマ名による追加、照会、削除操作を提供し、エンコードやデコード時にはスキーマ名の指定が必要です。

![architecture](./assets/schema_registry/schema_registry1.svg)

一般的なユースケースとして、ルールエンジンがスキーマレジストリのエンコード・デコードインターフェースを呼び出し、エンコードまたはデコードされたデータを後続のアクションの入力として利用します。

エンコード呼び出しの例：

```erlang
schema_encode(SchemaName, Map) -> Bytes
```

デコード呼び出しの例：

```erlang
schema_decode(SchemaName, Bytes) -> Map
```

JSONエンコードされたMQTTメッセージのデータをエンコードする場合、スキーマ関数でエンコードする前に`json_decode`関数でMap内部フォーマットにデコードする必要があります。例えば：

```erlang
schema_encode(SchemaName, json_decode(Map)) -> Bytes
```

エンコード前またはデコード後にJSONデータがJSONスキーマに準拠しているか検証する場合は、以下のスキーマ検証例を使用します：

```erlang
schema_check(SchemaName, Map | Bytes) -> Boolean
```

## スキーマレジストリとルールエンジン

EMQXのメッセージ処理層は、メッセージング、ルールエンジン、データ変換の3つの部分に分かれます。

EMQXのPUB/SUBシステムはメッセージを指定されたトピックにルーティングします。ルールエンジンはデータに対するビジネスルールを柔軟に設定でき、メッセージをルールにマッチさせて対応するアクションを指定します。データフォーマットの変換はルールマッチング処理の前に行われ、データをルールマッチングに参加可能なMapフォーマットに変換してからマッチングを行います。

<img src="./assets/SchemaAndRuleEngine.png" alt="スキーマとルールエンジン" style="zoom:67%;" />

### ルールエンジン内部データフォーマット（Map）

ルールエンジン内部で使用されるデータフォーマットはErlangのMapです。そのため、元のデータがバイナリや他のフォーマットの場合は、上記の`schema_decode`や`json_decode`などのコーデック関数でMapに変換する必要があります。MapはJSONオブジェクトに非常に似ています。

Mapはキーと値のペアのデータ構造で、`#{key => value}`の形式を取ります。例えば、`user = #{id => 1, name => "Steve"}`は`id`が`1`、`name`が`"Steve"`の`user` Mapを定義します。

SQL文は`.`演算子を提供し、ネストされたMapフィールドの抽出や追加を行えます。以下はSQL文によるMap操作の例です：

```sql
SELECT user.id AS my_id
```

このSQL文のフィルター結果は`#{my_id => 1}`となります。

### JSONコーデック

ルールエンジンのSQL文はJSON形式の文字列のエンコードおよびデコードをサポートしています。JSON文字列をMap形式に変換するSQL関数は`json_decode()`と`json_encode()`です：

```sql
SELECT json_decode(payload) AS p FROM "t/#" WHERE p.x = p.y
```

上記のSQL文は、ペイロードがJSON文字列`{"x" = 1, "y" = 1}`であり、トピックが`t/a`のMQTTメッセージにマッチします。

`json_decode(payload) as p`はJSON文字列を以下のMapデータ構造にデコードし、`WHERE`句で`p.x`や`p.y`としてMap内のフィールドを利用可能にします。

```erlang
#{
  p => #{
    x => 1,
    y => 1
  }
}
```

**注意:** `AS`句はデコードしたデータをキーに割り当て、後続の操作で利用できるようにするために必須です。

## 外部スキーマレジストリ

バージョン5.8.1以降、EMQXは外部のConfluentスキーマレジストリ（CSR）を設定可能になりました。この機能により、ルール処理中に外部レジストリからスキーマを動的に取得し、効率的なメッセージのエンコード・デコードが可能となります。

### ダッシュボードで外部スキーマレジストリを作成

EMQXダッシュボードから直接外部スキーマレジストリを設定でき、スキーマ統合の管理が容易です。

EMQXダッシュボードの **Smart Data Hub** -> **Schema Registry** に移動し、スキーマページの **External** タブを選択します。

右上の **Create** ボタンをクリックし、以下の項目を設定します：

- **Name**：エンコード・デコード関数で使用する外部スキーマレジストリ名を入力します。
- **Type**：外部スキーマレジストリの種類を選択します。現在は`Confluent`のみ対応しています。
- **URL**：Confluentスキーマレジストリのエンドポイントを入力します。
- **Authentication**：`Basic auth`を選択した場合、外部レジストリへのアクセス認証情報（ユーザー名とパスワード）を入力します。

設定完了後、**Create** をクリックします。

### 設定ファイルで外部スキーマレジストリを設定

EMQXの設定ファイルで外部Confluentスキーマレジストリを設定することも可能です。設定例は以下の通りです：

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
- `url` はConfluentスキーマレジストリのエンドポイントです。
- `auth` は外部レジストリへのアクセス認証情報（ユーザー名とパスワード）を含みます。

### ルールエンジンで外部スキーマレジストリを利用

外部レジストリを設定すると、EMQXルールエンジンで外部レジストリに保存されたスキーマを使ってペイロードのエンコード・デコードを行う複数の関数が利用可能です。

以下の関数は設定済みの外部CSRを利用します：

```sql
avro_encode('my_external_registry', payload, my_schema_id)
avro_decode('my_external_registry', payload, my_schema_id)
schema_encode_and_tag('my_local_avro_schema', 'my_external_registry', payload, 'my_subject')
schema_decode_tagged('my_external_registry', payload)
```

#### 関数利用例

以下の例では、次の値および変数名を使用しています：

- `my_external_registry`：EMQXで外部レジストリに割り当てた名前
- `my_schema_id`：CSRに登録されたスキーマID（CSRでは常に整数）
- `my_local_avro_schema`：EMQXにローカル設定されたAvroスキーマ名
- `my_subject`：CSRで定義されたサブジェクト名

##### `avro_encode`

`avro_encode`は外部レジストリのスキーマIDを用いてペイロードをエンコードします。スキーマは実行時に動的に取得され、以降の実行でキャッシュされます。ConfluentスキーマレジストリではスキーマIDは整数です。

::: tip 注意

エンコード時、ペイロードはルールエンジンの内部データフォーマットであるデコード済みMapである必要があります。これが例で`json_decode`を使用している理由です。

:::

例：

```sql
select
  -- 123はCSRに登録されたスキーマID
  avro_encode('my_external_registry', json_decode(payload), 123) as encoded
from 't'
```

##### `avro_decode`

この関数は外部レジストリの指定されたスキーマIDに基づきAvroペイロードをデコードします。スキーマは実行時に動的に取得され、以降の操作でキャッシュされます。

例：

```sql
select
  -- 123はCSRに登録されたスキーマID
  avro_decode('my_external_registry', payload, 123) as decoded
from 't'
```

##### `schema_encode_and_tag`

この関数はローカル登録済みAvroスキーマ、外部CSRスキーマ名、サブジェクトを使い、ペイロード（すでに内部Mapフォーマット）をエンコードし、結果のペイロードにスキーマIDタグを付与します。スキーマIDはローカルスキーマをCSRに登録して取得します。

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

この関数はCSR名を使って、スキーマIDタグ付きのペイロードをデコードします。

```sql
select
  schema_decode_tagged(
    'my_external_registry',
    payload
  ) as decoded
from 't'
```
