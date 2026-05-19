# Sparkplug B

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse FoundationのTAHUプロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソース仕様であり、MQTT向けに明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用IoT分野における相互運用性と一貫性の実現です。

SparkplugエンコーディングスキームのバージョンB（Sparkplug B）は、監視制御およびデータ収集（SCADA）システム、リアルタイム制御システム、およびデバイス向けのMQTTネームスペースを定義します。メトリクス、プロセス変数、デバイスの状態情報を含む構造化データ形式を簡潔かつ処理しやすい形でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug Bを利用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTTネットワーク内のデバイス間でシームレスな通信を可能にします。

本ページでは、EMQXにおけるSparkplug Bの実装方法について、データ形式、機能、および実用例を交えて解説します。

## Sparkplug Bのデータ形式

Sparkplug Bは、データ通信の標準化のために明確に定義されたペイロード構造を利用します。コア部分では、[Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers)を用いてSparkplugメッセージを構造化し、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQXは、[スキーマレジストリ](./schema-registry.md)機能を通じてSparkplug Bを高度にサポートしています。スキーマレジストリを利用することで、Sparkplug Bを含むさまざまなデータ形式に対してカスタムエンコーダーおよびデコーダーを作成できます。レジストリに[適切なSparkplug Bスキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)を定義することで、EMQXのルールエンジン内で`schema_decode`および`schema_encode`関数を使用し、指定された形式に準拠したデータのアクセスや操作が可能です。

さらに、EMQXはSparkplug Bをネイティブにサポートしており、この特定の形式に対してスキーマレジストリを使用する必要はありません。`spb_encode`および`spb_decode`関数がルールエンジン内で利用可能であり、Sparkplug Bメッセージのエンコードおよびデコードを簡素化します。

:::: tip

以前の`sparkplug_encode`および`sparkplug_decode`関数は、`bytes_value`の取り扱いがSparkplug仕様と互換性がなかったため非推奨となっています。  
代わりに、更新された`spb_encode`および`spb_decode`関数をご利用ください。

::::

## Sparkplug Bの関数

EMQXは、Sparkplug Bデータのエンコードおよびデコードのために2つのルールエンジンSQL関数、`spb_encode`と`spb_decode`を提供しています。[実用例](#practical-examples)のセクションでは、これらの関数をさまざまなシナリオでどのように使用するかを解説しています。

Sparkplug Bのエンコードおよびデコード関数は、ルールエンジンとその`jq`関数の柔軟性により、多様な処理に利用可能です。ルールエンジンおよび`jq`関数の詳細については、以下のページを参照してください。

* [ルールの作成](./rule-get-started.md)
* [ルールエンジンSQL言語](./rule-sql-syntax.md)
* [ルールエンジンのJQ関数](./rule-sql-jq.md)
* [JQプログラミング言語の完全な説明](https://stedolan.github.io/jq/manual/)

### spb_decode

`spb_decode`関数は、Sparkplug Bメッセージをデコードするために使用します。たとえば、Sparkplug Bでエンコードされたメッセージの内容に基づいて特定のトピックにメッセージを転送したり、Sparkplug Bメッセージを何らかの形で変更したりする場合に利用します。生のSparkplug Bエンコード済みペイロードを、さらに処理や解析がしやすい形式に変換します。

使用例：

```sql
select
  spb_decode(payload) as decoded
from t
```

上記の例では、`payload`はデコードしたい生のSparkplug Bメッセージを指します。

[Sparkplug BのProtobufスキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto)は、メッセージ構造の理解に役立ちます。

### spb_encode

`spb_encode`関数は、データをSparkplug Bメッセージにエンコードするために使用します。これは、Sparkplug BメッセージをMQTTクライアントやシステムの他のコンポーネントに送信する必要がある場合に特に有用です。

使用例：

```sql
select
  spb_encode(json_decode(payload)) as encoded
from t
```

上記の例では、`payload`はSparkplug Bメッセージにエンコードしたいデータを指します。

## 実用例

このセクションでは、`spb_decode`および`spb_encode`関数を用いたSparkplug Bメッセージの処理に関する実用例を示します。ここで示す例は、可能な操作のごく一部に過ぎません。

以下のような構造を持つSparkplug Bエンコード済みメッセージを想定します。

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

たとえば、トピック`my/sparkplug/topic`からデバイスのメッセージを受信し、その中の`counter_group1/counter1_run`メトリクスのみをJSON形式のメッセージとして別のトピック`interesting_counters/counter1_run_updates`に転送したい場合、以下の手順でEMQXダッシュボードにルールを作成し、[MQTTX](https://mqttx.app/)クライアントツールでテストできます。

#### ダッシュボードでのルール作成

1. EMQXダッシュボードにアクセスし、左のナビゲーションメニューから**Integration** -> **Rules**をクリックします。**+ Create**をクリックして**Create Rule**ページに入ります。

2. **SQL Editor**に以下のSQL文を入力します。

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

   ここで、`jq`関数はメトリクス配列を反復処理し、名前が`counter_group1/counter1_run`のものだけを抽出しています。

   ::: tip

   Sparkplug B仕様では、データは変化があった場合のみ送信することが推奨されているため、ペイロードにはメトリクスの一部のみが含まれることがあります。指定した名前のアイテムが配列に存在しない場合、このルールは何も出力しません。

   :::

3. ページ右側の**+ Add Action**をクリックし、**Action**のドロップダウンリストから`Republish`を選択します。リパブリッシュ先のトピックに`interesting_counters/counter1_run_updates`を入力し、ペイロード欄に`${item}`を入力して**Add**をクリックします。

4. **Create Rule**ページに戻り、**Create**をクリックします。ルール一覧に作成したルールが表示されます。

#### ルールのテスト

MQTTXクライアントツールを使って、Sparkplug Bメッセージをトピック`my/sparkplug/topic`にパブリッシュし、メッセージが変換されてJSON形式でトピック`interesting_counters/counter1_run_updates`に転送されることを確認します。

1. MQTTXクライアントデスクトップを開き、EMQXブローカーに接続します。MQTTXの詳細は[MQTTXクライアント](../messaging/publish-and-subscribe.md)を参照してください。

2. 新規サブスクリプションを作成し、トピック`interesting_counters/counter1_run_updates`をサブスクライブします。

3. 画面右下のメッセージ送信エリアで、トピックに`my/sparkplug/topic`を入力し、ペイロードタイプに`Base64`を選択します。

4. 以下のBase64エンコード済みSparkplug Bメッセージをコピーし、ペイロード欄に貼り付けます。これは前述のSparkplugメッセージ例をエンコードしたものです。

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

5. 送信ボタンをクリックしてメッセージを送信します。

   正常に動作していれば、以下のようなJSON形式のメッセージを受信できます。

   ```json
   {
       "timestamp":1678094561525,
       "name":"counter_group1/counter1_run",
       "int_value":1,
       "datatype":5
   }
   ```

### データの更新

誤ったメトリクス`counter_group1/counter1_run`をSparkplug Bエンコード済みペイロードから削除してからメッセージを転送したい場合を考えます。

[データの抽出](#データの抽出)の例と同様に、EMQXダッシュボードで以下のルールを作成し、リパブリッシュアクションを設定します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 削除するメトリクス名を保存
   "counter_group1/counter1_run" as $to_delete |
   # $to_deleteと異なる名前のメトリクスのみ抽出
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # 新しいメトリクスでペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

このルールでは、`spb_decode`でメッセージをデコードし、`jq`で名前が`counter_group1/counter1_run`のメトリクスを除外しています。その後、`DO`句で`spb_encode`を使って再度メッセージをエンコードしています。

リパブリッシュアクションのペイロードには`${updated_payload}`を指定してください。これは更新されたSparkplug Bエンコード済みメッセージの変数名です。

同様に、メトリクスの値を更新する場合も`spb_decode`と`spb_encode`を利用できます。たとえば、`counter_group1/counter1_run`の値を0に更新したい場合は、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 更新対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_update |
   # 名前が$to_updateのメトリクスの値を更新
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

また、新しいメトリクス`counter_group1/counter1_new`（値は42）を追加したい場合は、以下のルールを使用します。

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
   # 新しいメトリクスでペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

### メッセージのフィルタリング

メトリクス`counter_group1/counter1_run`の値が0より大きいメッセージのみを転送したい場合、以下のルールを使用できます。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # $to_filterの値が0以下のメッセージを除外
   if $value > 0 then $payload else empty end
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

上記のルールでは、`jq`関数が`counter_group1/counter1_run`の値が0以下の場合に空配列を出力します。これにより、値が0以下のメッセージはルールに接続されたアクションに転送されません。

### メッセージの分割

Sparkplug Bエンコード済みメッセージを複数のメッセージに分割し、メトリクス配列内の各メトリクスを個別のSparkplug Bエンコード済みメッセージとしてリパブリッシュしたい場合、以下のルールで実現できます。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1つのメッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスのみをメトリクス配列に設定
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

このルールでは、`jq`関数がメトリクス配列内の複数アイテムを出力します。ルールに接続されたすべてのアクションは配列内の各アイテムに対してトリガーされます。リパブリッシュアクションのペイロードには`${output_payload}`を指定してください。これは`DO`句でSparkplug Bエンコード済みメッセージに割り当てた名前です。

### メッセージの分割とコンテンツに基づくトピックへの送信

Sparkplug Bエンコード済みメッセージを分割しつつ、たとえばメトリクス名に基づいて各メッセージを異なるトピックに送信したい場合を考えます。出力トピック名は、文字列`"my_metrics/"`にメトリクス名を連結して構成するとします。以下のように少し修正したコードで実現可能です。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1つのメッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスのみをメトリクス配列に設定
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO
spb_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

リパブリッシュアクションの設定では、トピック名に`${output_topic}`を指定します。これは`DO`句で出力トピック名として割り当てた変数です。ペイロードには`${output_payload}`を指定してください。

`jq`関数の呼び出しは`DO`句内で`first`関数でラップされており、最初の1つだけの出力オブジェクトを取得しています。
