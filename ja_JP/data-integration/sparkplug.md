# Sparkplug B

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse FoundationのTAHUプロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソース仕様であり、MQTTのための明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用IoT分野における相互運用性と一貫性の実現です。

SparkplugエンコーディングスキームのバージョンB（Sparkplug B）は、監視制御およびデータ取得（SCADA）システム、リアルタイム制御システム、およびデバイス向けのMQTTネームスペースを定義しています。メトリクス、プロセス変数、デバイスの状態情報を含む構造化データ形式を簡潔かつ処理しやすい形でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug Bを使用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTTネットワーク内のデバイス間でシームレスな通信を可能にします。

本ページでは、EMQXにおけるSparkplug Bの実装方法について、データ形式、機能、実用例を交えて解説します。

## Sparkplug B データ形式

Sparkplug Bは、データ通信の標準化のために明確に定義されたペイロード構造を利用します。コアには[Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers)を用いてSparkplugメッセージを構造化しており、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQXは、[Schema Registry](./schema-registry.md)機能を通じてSparkplug Bを高度にサポートしています。Schema Registryを使うことで、Sparkplug Bを含むさまざまなデータ形式に対するカスタムエンコーダーおよびデコーダーを作成可能です。レジストリに[適切なSparkplug Bスキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)を定義することで、EMQXのルールエンジン内で`schema_decode`および`schema_encode`関数を用いて、指定されたフォーマットに準拠したデータのアクセスや操作が可能になります。

さらに、EMQXはSparkplug Bに対する組み込みサポートも提供しており、この特定のフォーマットに関してはSchema Registryを使う必要がありません。`sparkplug_encode`および`sparkplug_decode`関数がEMQXに標準搭載されており、ルールエンジン内でのSparkplug Bメッセージのエンコード・デコードを簡単に行えます。

## Sparkplug B 関数

EMQXは、Sparkplug Bデータのエンコードおよびデコード用に2つのルールエンジンSQL関数、`sparkplug_encode`と`sparkplug_decode`を提供しています。[実用例](#practical-examples)セクションでは、これらの関数をさまざまなシナリオでどのように使うかを解説しています。

Sparkplug Bのエンコード・デコード関数は、ルールエンジンとその`jq`関数の柔軟性により、多様なタスクに利用可能です。ルールエンジンおよび`jq`関数の詳細は以下のページをご参照ください。

* [ルールの作成](./rule-get-started.md)
* [ルールエンジンSQL言語](./rule-sql-syntax.md)
* [ルールエンジンのJQ関数](./rule-sql-jq.md)
* [JQプログラミング言語の完全な説明](https://stedolan.github.io/jq/manual/)

### sparkplug_decode

`sparkplug_decode`関数はSparkplug Bメッセージをデコードするために使用します。例えば、Sparkplug Bでエンコードされたメッセージの内容に基づいて特定のトピックに転送したり、メッセージを何らかの形で変更したい場合に利用します。生のSparkplug Bエンコード済みペイロードを、さらに処理や解析がしやすい形式に変換します。

使用例：

```sql
select
  sparkplug_decode(payload) as decoded
from t
```

上記例では、`payload`がデコード対象の生のSparkplug Bメッセージを指します。

[Sparkplug B Protobufスキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto)はメッセージ構造の理解に役立ちます。

### sparkplug_encode

`sparkplug_encode`関数はデータをSparkplug Bメッセージにエンコードするために使用します。MQTTクライアントやシステムの他のコンポーネントにSparkplug Bメッセージを送信する際に特に有用です。

使用例：

```sql
select
  sparkplug_encode(json_decode(payload)) as encoded
from t
```

上記例では、`payload`がSparkplug Bメッセージにエンコードしたいデータを指します。

## 実用例

このセクションでは、`sparkplug_decode`および`sparkplug_encode`関数を用いたSparkplug Bメッセージの取り扱いに関する実用例を紹介します。ここで示す例は可能な操作のほんの一部です。

以下の構造を持つSparkplug Bエンコード済みメッセージを例に考えます。

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

デバイスからトピック `my/sparkplug/topic` にメッセージが届き、その中の `counter_group1/counter1_run` メトリクスだけを抽出して、JSON形式のメッセージとして別のトピック `interesting_counters/counter1_run_updates` に転送したい場合の手順を示します。EMQXダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/)クライアントツールでテストする方法を説明します。

#### ダッシュボードでルール作成

1. EMQXダッシュボードにアクセスし、左側のナビゲーションメニューから **Integration** -> **Rules** をクリックします。**+ Create** をクリックして **Create Rule** ページに入ります。

2. **SQL Editor** に以下のSQL文を入力します。

   ```sql
   FOREACH
   jq('
         .metrics[] |
         select(.name == "counter_group1/counter1_run")
      ',
      sparkplug_decode(payload)) AS item
   DO item
   FROM "my/sparkplug/topic"
   ```

   ここでは、`jq`関数を使ってメトリクス配列を反復処理し、名前が `"counter_group1/counter1_run"` のものだけを抽出しています。

   ::: tip

   Sparkplug B仕様では、データが変化したときのみ送信することが推奨されているため、ペイロードにはメトリクスのサブセットしか含まれないことがあります。指定した名前のアイテムが配列に存在しない場合、このルールは何も出力しません。

   :::

3. ページ右側の **+ Add Action** をクリックし、**Action** ドロップダウンリストから `Republish` を選択します。リパブリッシュ先のトピックに `interesting_counters/counter1_run_updates` を入力し、**Payload** フィールドには `${item}` と入力します。**Add** をクリックします。

4. **Create Rule** ページに戻り、**Create** をクリックします。ルール一覧に新しいルールが作成されたことが確認できます。

#### ルールのテスト

MQTTXクライアントツールを使って、Sparkplug Bメッセージをトピック `my/sparkplug/topic` にパブリッシュし、メッセージが変換されてJSON形式で `interesting_counters/counter1_run_updates` トピックに転送されることを確認します。

1. MQTTXクライアントデスクトップを開き、EMQXブローカーに接続します。MQTTXの詳細は[MQTTXクライアント](../messaging/publish-and-subscribe.md)を参照してください。

2. 新しいサブスクリプションを作成し、トピック `interesting_counters/counter1_run_updates` をサブスクライブします。

3. 画面右下のメッセージ送信エリアにトピック `my/sparkplug/topic` を入力し、ペイロードタイプを `Base64` に設定します。

4. 以下のBase64エンコード済みSparkplug Bメッセージをコピーしてペイロード欄に貼り付けます。これは前述のSparkplugメッセージのエンコード例に対応しています。

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

5. 送信ボタンをクリックしてメッセージを送信します。

   正常に動作すれば、以下のようなJSON形式のメッセージが受信できます。

   ```json
   {
       "timestamp":1678094561525,
       "name":"counter_group1/counter1_run",
       "int_value":1,
       "datatype":5
   }
   ```

### データ更新

誤ったメトリクス `counter_group1/counter1_run` を発見し、メッセージ転送前にSparkplug Bエンコード済みペイロードから削除したい場合を考えます。

[データ抽出](#データ抽出)の例と同様に、EMQXダッシュボードで以下のルールを作成し、リパブリッシュアクションを設定します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 削除対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_delete |
   # $to_deleteの名前を持つメトリクスを除外
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # 新しいメトリクス配列でペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

このルールでは、`sparkplug_decode`でメッセージをデコードし、`jq`で名前が`counter_group1/counter1_run`のメトリクスを除外しています。その後、`DO`句の`sparkplug_encode`で再度メッセージをエンコードしています。

リパブリッシュアクションのペイロードには、更新後のSparkplug Bエンコード済みメッセージに割り当てた`${updated_payload}`を指定してください。

同様に、メトリクスの値を更新することも可能です。例えば、`counter_group1/counter1_run`の値を0に更新したい場合は、以下のルールを使用します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 更新対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_update |
   # $to_updateの名前を持つメトリクスの値を更新
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
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS item
FROM "my/sparkplug/topic"
```

また、新しいメトリクス `counter_group1/counter1_new` を値42で追加したい場合は、以下のルールを利用できます。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 既存のメトリクスを保存
   $payload | .metrics as $old_metrics |
   # 新しいメトリクスの値
   {
     "name": "counter_group1/counter1_new",
     "int_value": 42,
     "datatype": 5
   } as $new_value |
   # 新しいメトリクス配列を作成
   ($old_metrics + [ $new_value ]) as $updated_metrics |
   # 新しいメトリクス配列でペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS item 
FROM "my/sparkplug/topic"
```

### メッセージのフィルタリング

メトリクス名 `counter_group1/counter1_run` の値が0より大きいメッセージのみを転送したい場合、以下のルールを使えます。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # $to_filterの値が0以下のメッセージは除外
   if $value > 0 then $payload else empty end
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS item 
FROM "my/sparkplug/topic"
```

このルールでは、`jq`関数が`counter_group1/counter1_run`の値が0以下の場合に空配列を出力します。したがって、値が0以下のメッセージはルールに接続されたアクションに転送されません。

### メッセージの分割

Sparkplug Bエンコード済みメッセージを複数のメッセージに分割し、メトリクス配列内の各メトリクスを別々のSparkplug Bエンコード済みメッセージとしてリパブリッシュしたい場合、以下のルールで実現できます。

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
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

このルールでは、`jq`関数が複数のアイテムを含む配列を出力します（メトリクス配列に複数アイテムがある場合）。ルールに接続されたすべてのアクションは配列内の各アイテムごとにトリガーされます。

リパブリッシュアクションのペイロードには、`DO`句で割り当てた`${output_payload}`を指定してください。

### メッセージの分割と内容に基づくトピックへの送信

Sparkplug Bエンコード済みメッセージを分割しつつ、例えばメトリクス名に基づいてそれぞれ異なるトピックに送信したい場合を考えます。出力トピック名は、文字列 `"my_metrics/"` とメトリクス名を連結して構成するとします。以下のように少し修正したコードで実現可能です。

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
   sparkplug_decode(payload)) AS item
DO
sparkplug_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

リパブリッシュアクションの設定では、トピック名に`${output_topic}`を指定します。これは`DO`句で出力トピックに割り当てた名前です。ペイロードには`${output_payload}`を指定してください。

`jq`関数の呼び出しは`DO`句内で`first`関数に包まれており、最初の1つの出力オブジェクトを取得しています。
