# Sparkplug B

<<<<<<< HEAD
[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf)は、[Eclipse FoundationのTAHUプロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソース仕様であり、MQTTのための明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用IoT分野における相互運用性と一貫性の実現です。

SparkplugエンコーディングスキームのバージョンB（Sparkplug B）は、監視制御およびデータ収集（SCADA）システム、リアルタイム制御システム、およびデバイス向けのMQTTネームスペースを定義しています。メトリクス、プロセス変数、デバイスの状態情報を含む構造化されたデータフォーマットを簡潔かつ処理しやすい形式でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug Bを使用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTTネットワーク内のデバイス間でシームレスな通信を実現できます。

このページでは、EMQXにおけるSparkplug Bの実装方法について、データフォーマット、機能、および実践的な例を交えて解説します。

## Sparkplug B データフォーマット

Sparkplug Bは、データ通信を標準化するために明確に定義されたペイロード構造を利用します。コアには[Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers)を用いてSparkplugメッセージを構造化し、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQXは、[スキーマレジストリ](./schema-registry.md)機能を通じてSparkplug Bを高度にサポートしています。スキーマレジストリを使用すると、Sparkplug Bを含むさまざまなデータフォーマットのカスタムエンコーダーおよびデコーダーを作成できます。レジストリに[適切なSparkplug Bスキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)を定義することで、EMQXのルールエンジン内で`schema_decode`および`schema_encode`関数を使用し、指定されたフォーマットに準拠したデータのアクセスや操作が可能です。

さらに、EMQXはSparkplug Bの組み込みサポートも提供しており、この特定のフォーマットに対してはスキーマレジストリを使用する必要がありません。`spb_encode`および`spb_decode`関数がEMQXに標準搭載されており、ルールエンジン内でのSparkplug Bメッセージのエンコードおよびデコードを簡素化します。

:::: tip

以前の`sparkplug_encode`および`sparkplug_decode`関数は、`bytes_value`の処理がSparkplug仕様と互換性がなかったため非推奨となりました。  
代わりに、更新された`spb_encode`および`spb_decode`関数をご利用ください。

::::
=======
[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse Foundation の TAHU プロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソースの仕様で、MQTT のための明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用 IoT 分野における相互運用性と一貫性の実現です。

Sparkplug エンコーディングスキームのバージョン B（Sparkplug B）は、監視制御およびデータ収集（SCADA）システム、リアルタイム制御システム、およびデバイス向けの MQTT ネームスペースを定義します。メトリクス、プロセス変数、デバイスの状態情報を含む構造化データ形式を簡潔かつ処理しやすい形でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug B を使用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTT ネットワーク内のデバイス間でシームレスな通信を可能にします。

本ページでは、EMQX における Sparkplug B の実装方法について、データ形式、機能、実践例を交えて解説します。

## Sparkplug Bのデータ形式

Sparkplug B は、データ通信の標準化のために明確に定義されたペイロード構造を利用します。その中核には、Sparkplug メッセージの構造化に [Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers) を採用しており、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQX は [スキーマレジストリ](./schema-registry.md) 機能を通じて Sparkplug B を高度にサポートしています。スキーマレジストリを使うことで、Sparkplug B を含む様々なデータ形式のカスタムエンコーダーおよびデコーダーを作成できます。レジストリに[適切な Sparkplug B スキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)を定義することで、EMQX のルールエンジン内で `schema_decode` および `schema_encode` 関数を用いて、指定フォーマットに準拠したデータのアクセスや操作が可能になります。

さらに、EMQX は Sparkplug B に対する組み込みサポートも提供しており、この特定のフォーマットに関してはスキーマレジストリを使用する必要がありません。`spb_encode` および `spb_decode` 関数がルールエンジン内で利用可能で、Sparkplug B メッセージのエンコードおよびデコードを簡素化しています。
>>>>>>> origin/release-6.1

:::: tip

<<<<<<< HEAD
EMQXは、Sparkplug Bデータのエンコードおよびデコードのために、ルールエンジンSQL関数`spb_encode`と`spb_decode`を提供しています。[実践例](#examples-for-using-spb_decode-and-spb_encode)では、これらの関数をさまざまなシナリオで使用する方法を解説しています。

Sparkplug Bのエンコードおよびデコード関数は、ルールエンジンとその`jq`関数の柔軟性により、多様なタスクに利用可能です。ルールエンジンと`jq`関数の詳細については、以下のページをご参照ください。
=======
以前の `sparkplug_encode` および `sparkplug_decode` 関数は、`bytes_value` の扱いが Sparkplug 仕様と互換性がなかったため非推奨となりました。  
代わりに、更新された `spb_encode` および `spb_decode` 関数をご利用ください。

::::

:::: tip

EMQX は Sparkplug B データのエンコードおよびデコード用に、ルールエンジン SQL 関数 `spb_encode` と `spb_decode` を提供しています。  
[実践例](#examples-for-using-spb_decode-and-spb_encode)では、これらの関数を様々なシナリオでどのように使うかを解説しています。

Sparkplug B のエンコード・デコード関数は、ルールエンジンとその `jq` 関数の柔軟性により、多様な処理を実現できます。ルールエンジンおよび `jq` 関数の詳細は以下のページをご参照ください。
>>>>>>> origin/release-6.1

* [ルールの作成](./rule-get-started.md)
* [ルールエンジン SQL 言語](./rule-sql-syntax.md)
* [ルールエンジンの JQ 関数](./rule-sql-jq.md)
* [JQ プログラミング言語の完全な説明](https://stedolan.github.io/jq/manual/)

### spb_decode

<<<<<<< HEAD
`spb_decode`関数はSparkplug Bメッセージをデコードするために使用します。例えば、Sparkplug Bエンコードされたメッセージの内容に基づいて特定のトピックにメッセージを転送したり、Sparkplug Bメッセージを何らかの形で変更したい場合に利用します。生のSparkplug Bエンコード済みペイロードを、より扱いやすい形式に変換し、さらに処理や解析が可能になります。
=======
`spb_decode` 関数は Sparkplug B メッセージをデコードするために使用します。例えば、Sparkplug B エンコードされたメッセージの内容に基づいて特定のトピックにメッセージを転送したり、メッセージを何らかの形で変更したい場合に利用します。生の Sparkplug B エンコードされたペイロードを、より扱いやすい形式に変換し、さらに処理や解析を行いやすくします。
>>>>>>> origin/release-6.1

使用例:

```sql
select
  spb_decode(payload) as decoded
from t
```

<<<<<<< HEAD
上記の例では、`payload`はデコードしたい生のSparkplug Bメッセージを指します。

[Sparkplug B Protobufスキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto)は、メッセージ構造の理解に役立ちます。

### spb_encode

`spb_encode`関数はデータをSparkplug Bメッセージにエンコードするために使用します。MQTTクライアントやシステムの他のコンポーネントにSparkplug Bメッセージを送信する際に特に有用です。
=======
上記の例では、`payload` はデコードしたい生の Sparkplug B メッセージを指します。

[Sparkplug B Protobuf スキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto)はメッセージ構造の理解に役立ちます。

### spb_encode

`spb_encode` 関数はデータを Sparkplug B メッセージにエンコードするために使用します。MQTT クライアントやシステムの他のコンポーネントに Sparkplug B メッセージを送信する際に特に有用です。
>>>>>>> origin/release-6.1

使用例:

```sql
select
  spb_encode(json_decode(payload)) as encoded
from t
```

<<<<<<< HEAD
上記の例では、`payload`はSparkplug Bメッセージにエンコードしたいデータを指します。

## Sparkplug B エイリアスマッピング

Sparkplug B仕様では、デバイスがオンラインになる際（NBIRTH / DBIRTHメッセージ送信時）に、各メトリクスに数値の`alias`を割り当てることが許可されています。その後のデータ更新（NDATA / DDATAメッセージとして送信）では、メッセージサイズとネットワークオーバーヘッドを削減するために、完全なメトリクス名（`name`）の代わりに`alias`のみをパブリッシュすることができます。

これらのエイリアスのみの更新を正しく解釈するためには、受信側がSparkplug Bのセッション状態を追跡し、各エイリアスを元のメトリクス名に解決できる必要があります。

実際には、EMQXはSparkplug Bデータの中央処理および配信ハブとして機能します。ルールエンジンを使用して、EMQXはデコード済みデータを標準MQTTクライアントやデータプラットフォームなどの非Sparkplug Bクライアントに転送します。これらの下流システムは通常、Sparkplug Bの状態管理を実装していないため、エイリアスのみのデータは扱いにくくなります。

EMQX 6.0.2以降、`spb_decode`関数はSparkplug Bエイリアスマッピングをサポートするように強化されました。この強化により、EMQXはデコード時にメトリクス名を自動的に復元し、下流システムが扱いやすいデータを生成できます。

### Sparkplug B エイリアスマッピングの仕組み

エイリアスマッピングが有効な場合、EMQXは以下のようにSparkplug Bメッセージを処理します。

1. **NBIRTH / DBIRTHメッセージの処理**

   クライアントがNBIRTHまたはDBIRTHメッセージをパブリッシュすると、EMQXはペイロード内のメトリクスを調査し、`alias`と`name`の両方が定義されているメトリクスのエイリアスマッピングを記録します。

2. **セッションごとのマッピングの維持**

   エイリアスマッピングはMQTTクライアントのセッションごとに管理され、Sparkplug Bのセマンティクスに従います。

   - ノードレベルのメトリクス（NBIRTH / NDATA）とデバイスレベルのメトリクス（DBIRTH / DDATA）は別々に追跡されます。
   - 異なるクライアントのマッピングは完全に分離され、互いに干渉しません。

3. **`spb_decode`出力の強化**

   ルールエンジンがNDATAまたはDDATAメッセージに対して`spb_decode`を呼び出し、かつメトリクスに`alias`はあるが`name`がない場合、EMQXは事前に記録したマッピングを使って対応するメトリクス名を自動的に復元します。

   その結果、デコードされたメッセージには常に明確で読みやすいメトリクス名が含まれ、ルール処理や変換、転送に適した形式となります。

4. **セッション終了時のクリーンアップ**

   クライアントが切断されると、そのクライアントに関連付けられたエイリアスマッピングは削除されます。EMQXはセッション終了後にSparkplug Bの状態を保持または復元しません。

### エイリアスマッピングの設定

エイリアスマッピングはデフォルトで有効です。EMQXがSparkplug Bメトリクスのエイリアスを追跡および復元しないようにしたい場合は、設定ファイルで無効化できます。
=======
上記の例では、`payload` は Sparkplug B メッセージにエンコードしたいデータを指します。

## Sparkplug B エイリアスマッピング

Sparkplug B 仕様では、デバイスがオンラインになる際（NBIRTH / DBIRTH メッセージ送信時）に、各メトリクスに数値の `alias` を割り当てることができます。以降のデータ更新（NDATA / DDATA メッセージ）では、メッセージサイズやネットワークオーバーヘッドを削減するために、メトリクス名（`name`）の代わりに `alias` のみをパブリッシュすることが可能です。

このエイリアスのみの更新を正しく解釈するには、受信側が Sparkplug B セッション状態を管理し、各エイリアスを元のメトリクス名に解決できる必要があります。

実際には、EMQX は Sparkplug B データの中央処理および配信ハブとして機能します。ルールエンジンを用いて、EMQX はデコード済みデータを標準 MQTT クライアントやデータプラットフォームなどの非 Sparkplug B クライアントに転送します。これらの下流システムは通常 Sparkplug B 状態管理を実装していないため、エイリアスのみのデータは扱いにくいものとなります。

EMQX 6.0.2 以降、`spb_decode` 関数は Sparkplug B エイリアスマッピングをサポートするよう強化されました。この強化により、EMQX はデコード時にメトリクス名を自動的に復元し、下流システムが扱いやすいデータを生成できるようになっています。

### Sparkplug B エイリアスマッピングの仕組み

エイリアスマッピングが有効な場合、EMQX は以下のように Sparkplug B メッセージを処理します。

1. **NBIRTH / DBIRTH メッセージの処理**

   クライアントが NBIRTH または DBIRTH メッセージをパブリッシュすると、EMQX はペイロード内のメトリクスを調査し、`alias` と `name` の両方が定義されているメトリクスのエイリアスから名前へのマッピングを記録します。

2. **セッションごとのマッピング管理**

   エイリアスマッピングは MQTT クライアントのセッション単位で管理され、Sparkplug B のセマンティクスに従います。

   - ノードレベルのメトリクス（NBIRTH / NDATA）とデバイスレベルのメトリクス（DBIRTH / DDATA）は別々に管理されます。
   - 異なるクライアント間のマッピングは完全に分離され、互いに干渉しません。

3. **`spb_decode` 出力の強化**

   ルールエンジンが NDATA または DDATA メッセージに対して `spb_decode` を呼び出し、かつメトリクスに `alias` はあるが `name` がない場合、EMQX は記録済みのマッピングを使って対応するメトリクス名を自動的に復元します。

   その結果、デコードされたメッセージには常に明確で読みやすいメトリクス名が含まれ、ルール処理、変換、転送に適した形となります。

4. **セッション終了時のクリーンアップ**

   クライアントが切断されると、そのセッションに関連付けられたエイリアスマッピングは削除されます。EMQX はセッション終了後に Sparkplug B 状態を保持または復元しません。

### エイリアスマッピングの設定

エイリアスマッピングはデフォルトで有効です。EMQX による Sparkplug B メトリクスエイリアスの追跡および復元を無効にしたい場合は、設定ファイルで以下のように設定してください。
>>>>>>> origin/release-6.1

```hocon
schema_registry {
  sparkplugb {
    enable_alias_mapping = false
  }
}
```

> **注意**:
>
<<<<<<< HEAD
> - エイリアスマッピングは、マッピングが有効な状態で受信したNBIRTH / DBIRTHメッセージからのみ作成されます。
> - クライアントがすでにバースメッセージを送信している場合、エイリアスマッピングを適用するには再接続してNBIRTH / DBIRTHを再送信する必要があります。

### エイリアスマッピングの例

この例では、EMQXダッシュボードとMQTTXを使用して、エイリアスのみのDDATAメッセージを完全なメトリクス名を含むJSONデータに変換し、その結果を非Sparkplug Bクライアントに転送する方法を示します。

#### 目的

- **Sparkplug Bデバイス**：DBIRTHで`name + alias`を宣言し、DDATAでは`alias`のみをパブリッシュ。
- **EMQX**：`spb_decode`を使ってメトリクス名を自動復元。
- **下流サブスクライバー**：Sparkplug Bの知識なしに標準JSONメッセージを受信。

#### 前提条件

- EMQX 6.0.2以降、Sparkplug Bエイリアスマッピング有効（`enable_alias_mapping = true`）
- [MQTTX](https://mqttx.app/)

#### ステップ1: EMQXダッシュボードでルール作成

1. ダッシュボード左メニューの**Integration** -> **Rules**をクリック。

2. **+ Create**をクリックして新規ルール作成画面へ。

3. **SQL Editor**に以下を入力：
=======
> - エイリアスマッピングは、マッピング有効時に受信した NBIRTH / DBIRTH メッセージからのみ作成されます。
> - クライアントがすでにバースメッセージを送信済みの場合、エイリアスマッピングを適用するには再接続して NBIRTH / DBIRTH を再送信する必要があります。

### エイリアスマッピングの例

この例では、EMQX ダッシュボードと MQTTX を使って、エイリアスのみの DDATA メッセージをフルメトリクス名を含む JSON データに変換し、非 Sparkplug B クライアントに転送する方法を示します。

#### 目的

- **Sparkplug B デバイス**：DBIRTH で `name + alias` を宣言し、DDATA では `alias` のみをパブリッシュ。
- **EMQX**：`spb_decode` を使いメトリクス名を自動復元。
- **下流サブスクライバー**：Sparkplug B の知識なしに標準 JSON メッセージを受信。

#### 前提条件

- EMQX 6.0.2 以降、Sparkplug B エイリアスマッピング有効（`enable_alias_mapping = true`）
- [MQTTX](https://mqttx.app/)

#### ステップ 1: EMQX ダッシュボードでルール作成

1. ダッシュボードの左メニューから **Integration** -> **Rules** をクリック。

2. **+ Create** をクリックして新規ルール作成画面へ。

3. **SQL Editor** に以下を入力。
>>>>>>> origin/release-6.1

   ```sql
   SELECT
     spb_decode(payload) AS decoded
   FROM "spBv1.0/+/DDATA/+/+"
   ```

   > **補足**:
   >
<<<<<<< HEAD
   > - このルールはすべてのSparkplug B DDATAメッセージにマッチします。
   > - `spb_decode(payload)`はペイロードをデコードし、エイリアスマッピングが有効な場合はエイリアスからメトリクス名を自動復元します。

4. **+ Add Action**をクリックし、アクションを追加。

5. アクションタイプとして**Republish**を選択。
=======
   > - このルールはすべての Sparkplug B DDATA メッセージにマッチします。
   > - `spb_decode(payload)` はペイロードをデコードし、エイリアスマッピング有効時はエイリアスからメトリクス名を自動復元します。

4. **+ Add Action** をクリックしてアクションを追加。

5. アクションタイプに **Republish** を選択。
>>>>>>> origin/release-6.1

6. アクション設定：

   - **Topic**: `decoded/sparkplug/data`
   - **Payload**: `${decoded}`

<<<<<<< HEAD
7. **Add**をクリック。

8. **Save**をクリックしてルール作成完了。

   ![sparkplugb_alias_mapping_create_rule](./assets/sparkplugb_alias_mapping_create_rule.png)

#### ステップ2: MQTTXでサブスクライバー準備

1. MQTTXを開き、EMQXブローカーへの新規接続を作成。

2. トピック`decoded/sparkplug/data`をサブスクライブ。

このサブスクライバーは、プレーンなJSONデータを期待する**非Sparkplug Bクライアント**を表します。

#### ステップ3: MQTTXでSparkplug Bデバイスをシミュレート

以下のペイロードは読みやすさのため論理的にJSONで示しています。実際のメッセージ送信時はSparkplug B Protobufエンコード（Base64）を使用してください。

1. DBIRTH（エイリアス宣言）をトピック`spBv1.0/group1/DBIRTH/eon1/device1`に送信。
=======
7. **Add** をクリック。

8. **Save** をクリックしてルール作成完了。

   ![sparkplugb_alias_mapping_create_rule](./assets/sparkplugb_alias_mapping_create_rule.png)

#### ステップ 2: MQTTX でサブスクライバー準備

1. MQTTX を開き、EMQX ブローカーへの新規接続を作成。

2. トピック `decoded/sparkplug/data` をサブスクライブ。

このサブスクライバーは、プレーンな JSON データを期待する非 Sparkplug B クライアントを表します。

#### ステップ 3: MQTTX で Sparkplug B デバイスをシミュレート

以下のペイロードは可読性のため論理的な JSON で示しています。実際のメッセージ送信時は Sparkplug B Protobuf エンコード（Base64）を使用してください。

1. DBIRTH（エイリアス宣言）をトピック `spBv1.0/group1/DBIRTH/eon1/device1` に送信。
>>>>>>> origin/release-6.1

   **論理ペイロード（例）**

   ```json
   {
     "metrics": [
       {
         "name": "Device/Temperature",
         "alias": 0,
         "datatype": 9,
         "value": 72.5
       },
       {
         "name": "Device/Pressure",
         "alias": 1,
         "datatype": 9,
         "value": 101.3
       }
     ]
   }
   ```

   > **補足**:
   >
<<<<<<< HEAD
   > - Sparkplug Bでは`datatype`は符号なし整数で定義されており、値`9`はFloatデータ型を表します（Sparkplug B仕様による）。
   > - EMQXはこの時点でエイリアスと名前のマッピングを記録します。
   > - このステップはDDATA送信前に必ず実行してください。

2. DDATA（エイリアスのみ）をトピック`spBv1.0/group1/DDATA/eon1/device1`に送信。
=======
   > - Sparkplug B 仕様により、`datatype` は符号なし整数で定義され、値 `9` は Float データ型を表します。
   > - EMQX はこの時点でエイリアスから名前へのマッピングを記録します。
   > - このステップは DDATA 送信前に必ず実行してください。

2. DDATA（エイリアスのみ）をトピック `spBv1.0/group1/DDATA/eon1/device1` に送信。
>>>>>>> origin/release-6.1

   **論理ペイロード（例）**

   ```json
   {
     "metrics": [
       { "alias": 0, "value": 73.1 },
       { "alias": 1, "value": 100.9 }
     ]
   }
   ```

<<<<<<< HEAD
#### ステップ4: デコード結果の確認

MQTTXで`decoded/sparkplug/data`をサブスクライブしていると、以下のようなメッセージを受信します：
=======
#### ステップ 4: デコード結果の確認

MQTTX の `decoded/sparkplug/data` サブスクライバーは以下を受信します。
>>>>>>> origin/release-6.1

```json
{
  "metrics": [
    {
      "alias": 0,
      "name": "Device/Temperature",
      "value": 73.1
    },
    {
      "alias": 1,
      "name": "Device/Pressure",
      "value": 100.9
    }
  ]
}
```

<<<<<<< HEAD
以下のことが確認できます：

- 元のDDATAメッセージには`name`が含まれていませんでした。
- `spb_decode`が自動的に以下を復元しました：
  - `"Device/Temperature"`
  - `"Device/Pressure"`
- 下流のサブスクライバーはSparkplug Bの状態管理やエイリアス解釈を行う必要がありません。

## `spb_decode` と `spb_encode` の使用例

このセクションでは、`spb_decode`および`spb_encode`関数を使ったSparkplug Bメッセージの処理例を紹介します。ここで示す例は可能な操作の一部に過ぎません。

以下のような構造のSparkplug Bエンコード済みメッセージを受け取るシナリオを想定します：
=======
以下のことが確認できます。

- 元の DDATA メッセージには `name` が含まれていませんでした。
- `spb_decode` が自動的に以下を復元しました。
  - `"Device/Temperature"`
  - `"Device/Pressure"`
- 下流のサブスクライバーは Sparkplug B 状態を保持したりエイリアスを解釈したりする必要がありません。

## `spb_decode` と `spb_encode` の使用例

このセクションでは、`spb_decode` と `spb_encode` 関数を使った Sparkplug B メッセージの処理例を紹介します。ここで示す例は可能な処理の一部に過ぎません。

以下のような構造の Sparkplug B エンコード済みメッセージを受け取るシナリオを想定します。
>>>>>>> origin/release-6.1

```json
{
  "timestamp": 1678094561521,
  "seq": 88,
  "metrics": [
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_1sec",
      "int_value": 424,
      "datatype": 2
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_5sec",
      "int_value": 84,
      "datatype": 2
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_10sec",
      "int_value": 42,
      "datatype": 2
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_run",
      "int_value": 1,
      "datatype": 5
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_reset",
      "int_value": 0,
      "datatype": 5
    }
  ]
}
```

### データの抽出

<<<<<<< HEAD
デバイスから`my/sparkplug/topic`トピックでメッセージを受け取り、その中の`counter_group1/counter1_run`メトリクスだけを抽出して、JSON形式で`interesting_counters/counter1_run_updates`トピックに転送したい場合の例です。以下の手順でEMQXダッシュボードにルールを作成し、[MQTTX](https://mqttx.app/)でテストします。
=======
デバイスからトピック `my/sparkplug/topic` でメッセージを受け取り、`counter_group1/counter1_run` メトリクスのみを JSON 形式でトピック `interesting_counters/counter1_run_updates` に転送したい場合の手順を示します。EMQX ダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/) クライアントツールで動作を確認します。
>>>>>>> origin/release-6.1

#### ダッシュボードでのルール作成

<<<<<<< HEAD
1. EMQXダッシュボードにアクセスし、左メニューの**Integration** -> **Rules**をクリック。**+ Create**をクリックしてルール作成画面へ。

2. **SQL Editor**に以下を入力：
=======
1. EMQX ダッシュボードの左メニューから **Integration** -> **Rules** を選択し、**+ Create** をクリックしてルール作成画面へ。

2. **SQL Editor** に以下の SQL 文を入力。
>>>>>>> origin/release-6.1

   ```sql
   FOREACH
   jq('
         .metrics[] |
         select(.name == "counter_group1/counter1_run")
      ',
      spb_decode(payload)) AS item
   DO item
   FROM "my/sparkplug/topic"
   ```

<<<<<<< HEAD
   ここでは`jq`関数を使い、メトリクス配列を反復処理し、名前が`counter_group1/counter1_run`のメトリクスだけを抽出しています。

   ::: tip

   Sparkplug B仕様では、値が変化したときのみデータを送信することが推奨されているため、ペイロードにメトリクスの一部だけが含まれることがあります。指定した名前のメトリクスが存在しない場合、このルールは何も出力しません。

   :::

3. 画面右の**+ Add Action**をクリックし、アクションタイプから`Republish`を選択。リパブリッシュ先トピックに`interesting_counters/counter1_run_updates`を入力し、ペイロードに`${item}`を設定。**Add**をクリック。

4. ルール作成画面に戻り、**Create**をクリック。ルール一覧に作成したルールが表示されます。

#### ルールのテスト

MQTTXクライアントツールを使い、Sparkplug Bメッセージを`my/sparkplug/topic`にパブリッシュし、メッセージが変換されて`interesting_counters/counter1_run_updates`にJSON形式で転送されることを確認します。

1. MQTTXクライアントを起動し、EMQXブローカーに接続。MQTTXの詳細は[MQTTXクライアント](../messaging/publish-and-subscribe.md)を参照。

2. 新規サブスクリプションを作成し、`interesting_counters/counter1_run_updates`をサブスクライブ。

3. 画面右下のメッセージ送信欄に`my/sparkplug/topic`をトピックとして入力。ペイロードタイプは`Base64`を選択。

4. 以下のBase64エンコード済みSparkplug Bメッセージをペイロード欄に貼り付け。これは前述のSparkplugメッセージ例をエンコードしたものです。
=======
   ここで `jq` 関数はメトリクス配列を反復処理し、名前が `"counter_group1/counter1_run"` のメトリクスのみを抽出します。

   ::: tip

   Sparkplug B 仕様では、データは変化時のみ送信することが推奨されており、ペイロードに含まれるメトリクスは一部のみの場合があります。指定した名前のメトリクスが存在しない場合、このルールは何も出力しません。

   :::

3. 画面右側の **+ Add Action** をクリックし、アクションタイプから `Republish` を選択。  
   再パブリッシュ先トピックに `interesting_counters/counter1_run_updates` を入力し、ペイロードには `${item}` を設定。**Add** をクリック。

4. **Create** をクリックしてルールを作成。

#### ルールのテスト

MQTTX クライアントツールを使って、Sparkplug B メッセージをトピック `my/sparkplug/topic` にパブリッシュし、メッセージが変換されてトピック `interesting_counters/counter1_run_updates` に JSON 形式で転送されることを確認します。

1. MQTTX クライアントを開き、EMQX ブローカーに接続。詳細は [MQTTX クライアント](../messaging/publish-and-subscribe.md) を参照。

2. 新規サブスクリプションを作成し、トピック `interesting_counters/counter1_run_updates` をサブスクライブ。

3. メッセージ送信エリアでトピックに `my/sparkplug/topic` を入力し、ペイロードタイプを `Base64` に設定。

4. 以下の Base64 エンコード済み Sparkplug B メッセージをペイロード欄に貼り付け。これは前述のエンコード済みメッセージ例に対応しています。
>>>>>>> origin/release-6.1

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

5. 送信ボタンをクリック。

<<<<<<< HEAD
   正常に動作すれば、以下のようなJSON形式のメッセージが受信されます。
=======
   正常に動作すれば、以下のような JSON 形式のメッセージを受信します。
>>>>>>> origin/release-6.1

   ```json
   {
       "timestamp":1678094561525,
       "name":"counter_group1/counter1_run",
       "int_value":1,
       "datatype":5
   }
   ```

### データの更新

<<<<<<< HEAD
誤ったメトリクス`counter_group1/counter1_run`をSparkplug Bエンコード済みペイロードから削除して転送したい場合の例です。
=======
誤ったメトリクス `counter_group1/counter1_run` を検出し、転送前に Sparkplug B エンコード済みペイロードから削除したい場合を考えます。
>>>>>>> origin/release-6.1

[データ抽出](#データ抽出)の例と同様に、EMQX ダッシュボードで以下のルールを作成し、再パブリッシュアクションを設定します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 削除するメトリクス名を保存
   "counter_group1/counter1_run" as $to_delete |
<<<<<<< HEAD
   # $to_deleteと異なるメトリクスだけを抽出
=======
   # $to_delete 以外のメトリクスを抽出
>>>>>>> origin/release-6.1
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # 新しいメトリクスでペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

<<<<<<< HEAD
このルールでは、`spb_decode`でメッセージをデコードし、`jq`で指定したメトリクスを除外しています。`DO`句の`spb_encode`で再度メッセージをエンコードします。

リパブリッシュアクションのペイロードには`${updated_payload}`を指定してください。これは更新されたSparkplug Bエンコード済みメッセージの変数名です。
=======
このルールでは、`spb_decode` でメッセージをデコードし、`jq` で指定した名前のメトリクスを除外しています。`DO` 節で `spb_encode` を使い再エンコードしています。

再パブリッシュアクションでは、ペイロードに `${updated_payload}` を指定してください。これは更新済みの Sparkplug B エンコード済みメッセージの変数名です。
>>>>>>> origin/release-6.1

同様に、メトリクスの値を更新することも可能です。例えば、`counter_group1/counter1_run` の値を 0 に更新したい場合は以下のルールを使用します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 更新対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_update |
<<<<<<< HEAD
   # 指定したメトリクスの値を更新
=======
   # 指定メトリクスの値を更新
>>>>>>> origin/release-6.1
   [
     .metrics[] |
     if .name == $to_update
        then .int_value = 0
        else .
     end
   ] as $updated_metrics |
   # 新しいメトリクスでペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

<<<<<<< HEAD
また、新しいメトリクス`counter_group1/counter1_new`を値42で追加したい場合は、以下のルールを使用します。
=======
また、新しいメトリクス `counter_group1/counter1_new` を値 42 で追加したい場合は以下のルールを使用します。
>>>>>>> origin/release-6.1

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 既存メトリクスを保存
   $payload | .metrics as $old_metrics |
   # 新しいメトリクス値
   {
     "name": "counter_group1/counter1_new",
     "int_value": 42,
     "datatype": 5
   } as $new_value |
   # 新しいメトリクス配列を作成
   ($old_metrics + [ $new_value ]) as $updated_metrics |
   # 新しいメトリクスでペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

### メッセージのフィルタリング

<<<<<<< HEAD
メトリクス`counter_group1/counter1_run`の値が0より大きいメッセージだけを転送したい場合の例です。
=======
`counter_group1/counter1_run` メトリクスの値が 0 より大きいメッセージのみを転送したい場合、以下のルールを使用します。
>>>>>>> origin/release-6.1

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
<<<<<<< HEAD
   # 値が0以下のメッセージを除外
=======
   # メトリクス値が 0 以下の場合は空にする
>>>>>>> origin/release-6.1
   if $value > 0 then $payload else empty end
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

<<<<<<< HEAD
このルールでは、`jq`関数が指定メトリクスの値が0以下の場合に空配列を出力するため、ルールに接続されたアクションは何もトリガーされません。

### メッセージの分割

Sparkplug Bエンコード済みメッセージを複数のメッセージに分割し、メトリクス配列の各メトリクスを個別のSparkplug Bエンコード済みメッセージとしてリパブリッシュしたい場合の例です。
=======
このルールでは、`jq` 関数が条件に合わない場合に空配列を出力するため、値が 0 以下のメッセージはルールに接続されたアクションに転送されません。

### メッセージの分割

Sparkplug B エンコード済みメッセージを複数のメッセージに分割し、メトリクス配列の各メトリクスを別々の Sparkplug B エンコード済みメッセージとして再パブリッシュしたい場合、以下のルールを使用します。
>>>>>>> origin/release-6.1

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとにメッセージを出力
   .metrics[] |
        . as $metric |
<<<<<<< HEAD
        # 現在のメトリクスだけを含むメトリクス配列に置き換え
=======
        # 現在のメトリクスだけを含む配列に置き換え
>>>>>>> origin/release-6.1
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

<<<<<<< HEAD
このルールでは、`jq`関数が複数のアイテムを含む配列を出力します（メトリクス配列に複数の要素がある場合）。ルールに接続されたすべてのアクションは配列の各アイテムごとにトリガーされます。リパブリッシュアクションのペイロードには`${output_payload}`を設定してください。これは`DO`句でエンコード済みメッセージに割り当てた名前です。

### メッセージ分割と内容に応じたトピック送信

Sparkplug Bエンコード済みメッセージを分割しつつ、例えばメトリクス名に基づいて異なるトピックに送信したい場合の例です。出力トピック名は`"my_metrics/"`とメトリクス名を連結して作成します。

以下のように少し修正したコードで実現可能です。
=======
このルールでは、`jq` 関数が複数のアイテムを出力し、ルールに接続されたすべてのアクションが各アイテムごとにトリガーされます。再パブリッシュアクションのペイロードには `${output_payload}` を指定してください。これは `DO` 節で割り当てた Sparkplug B エンコード済みメッセージの変数名です。

### メッセージを分割し、内容に応じてトピックに送信

Sparkplug B エンコード済みメッセージを分割しつつ、例えばメトリクス名に基づいて各メッセージを異なるトピックに送信したい場合を考えます。出力トピック名は `"my_metrics/"` とメトリクス名を連結して作成するとします。以下のようにコードを少し修正して実現可能です。
>>>>>>> origin/release-6.1

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとにメッセージを出力
   .metrics[] |
        . as $metric |
<<<<<<< HEAD
        # 現在のメトリクスだけを含むメトリクス配列に置き換え
=======
        # 現在のメトリクスだけを含む配列に置き換え
>>>>>>> origin/release-6.1
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO
spb_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

<<<<<<< HEAD
リパブリッシュアクションのトピック名には`${output_topic}`を設定してください。これは`DO`句で出力トピック名として割り当てた変数です。ペイロードには`${output_payload}`を設定します。

`jq`関数呼び出しは`DO`句内で`first`関数でラップされており、最初の（唯一の）出力オブジェクトを取得しています。
=======
再パブリッシュアクションの設定では、トピック名に `${output_topic}` を指定し、ペイロードに `${output_payload}` を設定してください。`${output_topic}` は `DO` 節で出力トピック名として割り当てた変数です。

`jq` 関数呼び出しは `DO` 節内で `first` 関数でラップされ、最初で唯一の出力オブジェクトを取得しています。
>>>>>>> origin/release-6.1
