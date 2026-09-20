# Sparkplug B

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse FoundationのTAHUプロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソースの仕様で、MQTT向けに明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用IoT分野における相互運用性と一貫性の実現です。

SparkplugエンコーディングスキームのバージョンB（Sparkplug B）は、監視制御およびデータ収集（SCADA）システム、リアルタイム制御システム、およびデバイス向けのMQTTネームスペースを定義します。メトリクス、プロセス変数、デバイスの状態情報を含む構造化データ形式を簡潔かつ処理しやすい形でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug Bを使用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTTネットワーク内のデバイス間のシームレスな通信を可能にします。

本ページでは、EMQXにおけるSparkplug Bの実装方法、データ形式、機能、実用例について解説します。

## Sparkplug B データ形式

Sparkplug Bは、データ通信の標準化のために明確に定義されたペイロード構造を利用します。コアには[Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers)を用いてSparkplugメッセージを構造化し、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQXは[スキーマレジストリ](./schema-registry.md)機能を通じてSparkplug Bを高度にサポートしています。スキーマレジストリを使うことで、Sparkplug Bを含むさまざまなデータ形式に対してカスタムエンコーダーおよびデコーダーを作成可能です。レジストリに[適切なSparkplug Bスキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)を定義すれば、EMQXのルールエンジン内で`schema_decode`および`schema_encode`関数を使い、指定した形式に準拠したデータのアクセスや操作ができます。

さらに、EMQXはSparkplug Bに対して組み込みサポートを提供しており、この特定の形式に関してはスキーマレジストリを使用せずに済みます。`spb_encode`および`spb_decode`関数がEMQXに標準搭載されており、ルールエンジン内でSparkplug Bメッセージのエンコード・デコードを簡単に行えます。

:::: tip

以前の`sparkplug_encode`および`sparkplug_decode`関数は、`bytes_value`の処理がSparkplug仕様と互換性がなかったため非推奨となりました。  
代わりに、更新された`spb_encode`および`spb_decode`関数をご利用ください。

::::

## Sparkplug B 関数

EMQXはSparkplug Bデータのエンコード・デコード用に2つのルールエンジンSQL関数、`spb_encode`と`spb_decode`を提供しています。[実用例](#examples-for-using-spb_decode-and-spb_encode)では、さまざまなシナリオでのこれら関数の使い方を解説しています。

Sparkplug Bのエンコード・デコード関数は、ルールエンジンの柔軟性と`jq`関数の組み合わせにより、多様な処理に利用可能です。ルールエンジンと`jq`関数の詳細は以下のページをご参照ください。

* [ルールの作成](./rule-get-started.md)
* [ルールエンジンSQL言語](./rule-sql-syntax.md)
* [ルールエンジンのJQ関数](./rule-sql-jq.md)
* [JQプログラミング言語の詳細](https://stedolan.github.io/jq/manual/)

### spb_decode

`spb_decode`関数はSparkplug Bメッセージをデコードするために使用します。たとえば、Sparkplug Bエンコードされたメッセージの内容に基づいて特定のトピックに転送したり、メッセージを何らかの形で変更したい場合に利用します。生のSparkplug Bエンコード済みペイロードを、より扱いやすい形式に変換し、さらに処理や解析が可能になります。

使用例：

```sql
select
  spb_decode(payload) as decoded
from t
```

上記の例では、`payload`はデコードしたい生のSparkplug Bメッセージを指します。

[Sparkplug B Protobufスキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto)はメッセージ構造の理解に役立ちます。

### spb_encode

`spb_encode`関数はデータをSparkplug Bメッセージにエンコードするために使用します。MQTTクライアントやシステムの他コンポーネントにSparkplug Bメッセージを送信する際に特に有用です。

使用例：

```sql
select
  spb_encode(json_decode(payload)) as encoded
from t
```

上記の例では、`payload`はSparkplug Bメッセージにエンコードしたいデータを指します。

## Sparkplug B エイリアスマッピング

Sparkplug B仕様では、デバイスがオンラインになる際（NBIRTH / DBIRTHメッセージ送信時）に、各メトリクスに数値の`alias`を割り当てることができます。以降のデータ更新（NDATA / DDATAメッセージ）では、メッセージサイズとネットワークオーバーヘッドを削減するために、完全なメトリクス名（`name`）の代わりに`alias`のみをパブリッシュすることが可能です。

これらのエイリアスのみの更新を正しく解釈するには、受信側がSparkplug Bのセッション状態を管理し、各エイリアスを元のメトリクス名に復元できる必要があります。

実際には、EMQXはSparkplug Bデータの中央処理および配信ハブとして機能します。ルールエンジンを使って、EMQXはデコード済みデータを標準MQTTクライアントやデータプラットフォームなどの非Sparkplug Bクライアントに転送します。これら下流システムは通常Sparkplug Bの状態管理を実装していないため、エイリアスのみのデータは扱いにくい問題があります。

EMQX 6.0.2以降、`spb_decode`関数はSparkplug Bエイリアスマッピングをサポートするよう強化されました。この強化により、EMQXはデコード時にメトリクス名を自動復元し、下流システムでのデータ利用が容易になります。

### Sparkplug B エイリアスマッピングの仕組み

エイリアスマッピングが有効な場合、EMQXは以下のようにSparkplug Bメッセージを処理します。

1. **NBIRTH / DBIRTHメッセージの処理**

   クライアントがNBIRTHまたはDBIRTHメッセージをパブリッシュすると、EMQXはペイロード内のメトリクスを調査し、`alias`と`name`の両方が定義されているメトリクスについてエイリアスから名前へのマッピングを記録します。

2. **セッションごとのマッピング管理**

   エイリアスマッピングはMQTTクライアントのセッション単位で管理され、Sparkplug Bの意味論に従います。

   - ノードレベルのメトリクス（NBIRTH / NDATA）とデバイスレベルのメトリクス（DBIRTH / DDATA）は別々に管理されます。
   - 異なるクライアント間のマッピングは完全に分離され、相互に干渉しません。

3. **`spb_decode`出力の強化**

   ルールエンジンがNDATAまたはDDATAメッセージに対して`spb_decode`を呼び出し、かつメトリクスに`alias`はあるが`name`がない場合、EMQXは記録済みのマッピングを使って対応するメトリクス名を自動復元します。

   その結果、デコード済みメッセージには常に明確で読みやすいメトリクス名が含まれ、ルール処理、変換、転送に適した形式となります。

4. **セッション終了時のクリーンアップ**

   クライアントが切断されると、そのセッションに関連付けられたエイリアスマッピングは削除されます。EMQXはセッション終了後にSparkplug B状態を保持または復元しません。

### エイリアスマッピングの設定

エイリアスマッピングはデフォルトで有効です。EMQXにSparkplug Bメトリクスのエイリアス追跡および復元を行わせたくない場合は、設定ファイルで無効化できます。

```hocon
schema_registry {
  sparkplugb {
    enable_alias_mapping = false
  }
}
```

> **注意**:
>
> - エイリアスマッピングは有効時に受信したNBIRTH / DBIRTHメッセージからのみ作成されます。
> - クライアントがすでにバースメッセージを送信済みの場合、エイリアスマッピングを適用するには再接続してNBIRTH / DBIRTHを再送信する必要があります。

### エイリアスマッピングの例

この例では、EMQXダッシュボードとMQTTXを使って、エイリアスのみのDDATAメッセージをフルメトリクス名を含むJSONデータに変換し、非Sparkplug Bクライアントに転送する方法を示します。

#### 目的

- **Sparkplug Bデバイス**：DBIRTHで`name + alias`を宣言し、DDATAでは`alias`のみをパブリッシュ。
- **EMQX**：`spb_decode`でメトリクス名を自動復元。
- **下流サブスクライバー**：Sparkplug Bの知識なしに標準JSONメッセージを受信。

#### 前提条件

- EMQX 6.0.2以降、Sparkplug Bエイリアスマッピング有効（`enable_alias_mapping = true`）
- [MQTTX](https://mqttx.app/)

#### ステップ1：EMQXダッシュボードでルール作成

1. ダッシュボードの左メニューから **Integration** -> **Rules** をクリック。

2. **+ Create** をクリックして新規ルール作成画面へ。

3. **SQLエディター**に以下を入力：

   ```sql
   SELECT
     spb_decode(payload) AS decoded
   FROM "spBv1.0/+/DDATA/+/+"
   ```

   > **注意**:
   >
   > - ルールはすべてのSparkplug B DDATAメッセージにマッチします。
   > - `spb_decode(payload)`はペイロードをデコードし、エイリアスマッピング有効時はエイリアスからメトリクス名を自動復元します。

4. **+ Add Action** をクリックしてアクションを追加。

5. アクションタイプに **Republish** を選択。

6. アクション設定：

   - **Topic**：`decoded/sparkplug/data`
   - **Payload**：`${decoded}`

7. **Add** をクリック。

8. **Save** をクリックしてルール作成完了。

   ![sparkplugb_alias_mapping_create_rule](./assets/sparkplugb_alias_mapping_create_rule.png)

#### ステップ2：MQTTXでサブスクライバー準備

1. MQTTXを開き、EMQXブローカーへの新規接続を作成。

2. トピック`decoded/sparkplug/data`をサブスクライブ。

このサブスクライバーは、プレーンなJSONデータを期待する**非Sparkplug Bクライアント**を表します。

#### ステップ3：MQTTXでSparkplug Bデバイスをシミュレート

以下のペイロードは可読性のため論理的なJSON形式で示しています。実際のパブリッシュ時はSparkplug B Protobufエンコード（Base64）を使用してください。

1. DBIRTH（エイリアス宣言）をトピック`spBv1.0/group1/DBIRTH/eon1/device1`に送信。

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
   > - Sparkplug Bでは`datatype`は符号なし整数で定義され、値`9`はFloatデータ型を示します。
   > - EMQXはこの時点でエイリアスから名前へのマッピングを記録します。
   > - このステップはDDATA送信前に必ず実行してください。

2. DDATA（エイリアスのみ）をトピック`spBv1.0/group1/DDATA/eon1/device1`に送信。

   **論理ペイロード（例）**

   ```json
   {
     "metrics": [
       { "alias": 0, "value": 73.1 },
       { "alias": 1, "value": 100.9 }
     ]
   }
   ```

#### ステップ4：デコード結果の確認

MQTTXで`decoded/sparkplug/data`をサブスクライブしていると、以下のようなJSONメッセージを受信します。

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

確認できる点：

- 元のDDATAメッセージには`name`が含まれていません。
- `spb_decode`が自動的に以下を復元しました：
  - `"Device/Temperature"`
  - `"Device/Pressure"`
- 下流のサブスクライバーはSparkplug B状態管理やエイリアス解釈を行う必要がありません。

## `spb_decode` と `spb_encode` の使用例

本節では、`spb_decode`および`spb_encode`関数を用いたSparkplug Bメッセージ処理の実用例を紹介します。以下の例は可能な操作の一部に過ぎません。

以下の構造を持つSparkplug Bエンコード済みメッセージを想定します。

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

### データ抽出

デバイスから`my/sparkplug/topic`トピックでメッセージを受信し、`counter_group1/counter1_run`メトリクスのみをJSON形式で`interesting_counters/counter1_run_updates`トピックに転送したい場合の手順を示します。EMQXダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/)で動作検証を行います。

#### ダッシュボードでルール作成

1. EMQXダッシュボードの左ナビゲーションメニューから **Integration** -> **Rules** を選択し、**+ Create** をクリックしてルール作成画面へ。

2. **SQLエディター**に以下を入力：

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

   ここで`jq`関数はメトリクス配列を反復処理し、名前が`counter_group1/counter1_run`のメトリクスのみを抽出しています。

   ::: tip

   Sparkplug B仕様では、データは変化時のみ送信することが推奨されており、そのためペイロードに含まれるメトリクスは一部のみの場合があります。指定した名前のメトリクスが存在しない場合、このルールは何も出力しません。

   :::

3. 画面右側の **+ Add Action** をクリックし、アクションタイプから`Republish`を選択。  
   再パブリッシュ先トピックに`interesting_counters/counter1_run_updates`を入力し、ペイロードには`${item}`を指定。  
   **Add** をクリック。

4. ルール作成画面に戻り、**Create** をクリック。ルール一覧に新規ルールが追加されます。

#### ルールのテスト

MQTTXクライアントツールを使い、Sparkplug Bメッセージを`my/sparkplug/topic`にパブリッシュし、変換・転送が行われることを確認します。

1. MQTTXクライアントを起動し、EMQXブローカーに接続します。MQTTXの詳細は[MQTTXクライアント](../../get-started/messaging/publish-and-subscribe.md)を参照してください。

2. 新規サブスクリプションを作成し、トピック`interesting_counters/counter1_run_updates`をサブスクライブ。

3. メッセージ送信欄にトピック`my/sparkplug/topic`を入力し、ペイロードタイプを`Base64`に設定。

4. 以下のBase64エンコード済みSparkplug Bメッセージをペイロード欄に貼り付け。これは前述のSparkplugメッセージ例のエンコード版です。

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

5. 送信ボタンをクリック。

正常に動作すれば、以下のようなJSON形式のメッセージを受信できます。

```json
{
    "timestamp":1678094561525,
    "name":"counter_group1/counter1_run",
    "int_value":1,
    "datatype":5
}
```

### データ更新

誤ったメトリクス`counter_group1/counter1_run`をSparkplug Bエンコード済みペイロードから削除して転送したい場合、[データ抽出](#データ抽出)の例に類似したルールをEMQXダッシュボードで作成します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 削除対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_delete |
   # $to_deleteと異なるメトリクスのみ抽出
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # 新しいメトリクス配列でペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

このルールでは、`spb_decode`でメッセージをデコードし、`jq`で指定した名前のメトリクスを除外しています。`DO`句の`spb_encode`で再度エンコードします。

再パブリッシュアクションでは、更新済みSparkplug Bメッセージを示す`${updated_payload}`をペイロードに指定してください。

同様に、メトリクスの値を更新することも可能です。たとえば、`counter_group1/counter1_run`の値を0に更新する場合は以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 更新対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_update |
   # $to_updateの値を0に更新
   [
     .metrics[] |
     if .name == $to_update
        then .int_value = 0
        else .
     end
   ] as $updated_metrics |
   # 新しいメトリクス配列でペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

また、新しいメトリクス`counter_group1/counter1_new`（値42）を追加する場合は以下のルールを利用できます。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 既存のメトリクスを保存
   $payload | .metrics as $old_metrics |
   # 新しいメトリクス値
   {
     "name": "counter_group1/counter1_new",
     "int_value": 42,
     "datatype": 5
   } as $new_value |
   # 新しいメトリクス配列を作成
   ($old_metrics + [ $new_value ]) as $updated_metrics |
   # ペイロードに更新済みメトリクス配列を設定
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

### メッセージのフィルタリング

メトリクス`counter_group1/counter1_run`の値が0より大きいメッセージのみ転送したい場合、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # $to_filterの値が0以下なら空出力（転送しない）
   if $value > 0 then $payload else empty end
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

このルールでは、`jq`関数が指定メトリクスの値が0以下の場合に空配列を出力し、ルールに接続されたアクションは何もトリガーされません。

### メッセージの分割

Sparkplug Bエンコード済みメッセージを複数のメッセージに分割し、メトリクス配列の各メトリクスを個別のSparkplug Bエンコード済みメッセージとして再パブリッシュしたい場合、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1メッセージ出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスのみをmetrics配列に設定
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

このルールでは、`jq`関数が複数のアイテムを含む配列を出力し、ルールに接続されたすべてのアクションが各アイテムごとにトリガーされます。再パブリッシュアクションのペイロードには`${output_payload}`を指定してください。`output_payload`は`DO`句で割り当てたSparkplug Bエンコード済みメッセージの名前です。

### メッセージの分割と内容に基づくトピック振り分け

Sparkplug Bエンコード済みメッセージを分割しつつ、たとえばメトリクス名に基づいて各メッセージを異なるトピックに送信したい場合、以下のようにトピック名を動的に生成できます。ここではトピック名を`"my_metrics/"`にメトリクス名を連結した文字列とします。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1メッセージ出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスのみをmetrics配列に設定
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO
spb_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

再パブリッシュアクションのトピック名には`${output_topic}`を、ペイロードには`${output_payload}`を指定してください。  
`jq`関数呼び出しは`DO`句内で`first`関数でラップし、最初の（かつ唯一の）出力オブジェクトを取得しています。
