# Sparkplug B

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse Foundation の TAHU プロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソースの仕様で、MQTT における明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用 IoT 分野における相互運用性と一貫性を実現することです。

Sparkplug エンコーディングスキームのバージョン B（Sparkplug B）は、監視制御およびデータ収集（SCADA）システム、リアルタイム制御システム、およびデバイス向けの MQTT ネームスペースを定義しています。メトリクス、プロセス変数、デバイスの状態情報を含む構造化されたデータ形式を簡潔かつ処理しやすい形式でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug B を利用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTT ネットワーク内のデバイス間でシームレスな通信を実現できます。

本ページでは、EMQX における Sparkplug B の実装方法について、データ形式、機能、および実用的な例を交えて解説します。

## Sparkplug B データ形式

Sparkplug B は、データ通信の標準化のために明確に定義されたペイロード構造を利用します。コアには [Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers) を用いて Sparkplug メッセージを構造化しており、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQX は [スキーマレジストリ](./schema-registry.md) 機能を通じて Sparkplug B を高度にサポートしています。スキーマレジストリを使用すると、Sparkplug B を含むさまざまなデータ形式のカスタムエンコーダーおよびデコーダーを作成できます。レジストリに [適切な Sparkplug B スキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto) を定義することで、EMQX のルールエンジン内で `schema_decode` および `schema_encode` 関数を使い、指定した形式に準拠したデータへアクセス・操作が可能です。

さらに、EMQX は Sparkplug B に対する組み込みサポートも提供しており、この特定の形式に対してはスキーマレジストリを使用する必要がありません。`spb_encode` および `spb_decode` 関数が EMQX に標準搭載されており、ルールエンジン内で Sparkplug B メッセージのエンコードおよびデコードを簡単に行えます。

:::: tip

以前の `sparkplug_encode` および `sparkplug_decode` 関数は、`bytes_value` の扱いが Sparkplug 仕様と互換性がなかったため非推奨となっています。  
代わりに、更新された `spb_encode` および `spb_decode` 関数をご利用ください。

::::

## Sparkplug B 関数

EMQX は Sparkplug B データのエンコードおよびデコード用に、ルールエンジン SQL 関数として `spb_encode` と `spb_decode` の2つを提供しています。  
[実用例](#practical-examples) セクションでは、これらの関数をさまざまなシナリオで使用する方法を紹介しています。

Sparkplug B のエンコードおよびデコード関数は、ルールエンジンとその `jq` 関数の柔軟性により、多様なタスクに活用可能です。ルールエンジンおよび `jq` 関数の詳細は以下のページをご参照ください。

* [ルールの作成](./rule-get-started.md)
* [ルールエンジン SQL 言語](./rule-sql-syntax.md)
* [ルールエンジンの JQ 関数](./rule-sql-jq.md)
* [JQ プログラミング言語の完全な説明](https://stedolan.github.io/jq/manual/)

### spb_decode

`spb_decode` 関数は Sparkplug B メッセージをデコードするために使用します。例えば、Sparkplug B エンコードされたメッセージの内容に基づいて特定のトピックにメッセージを転送したり、メッセージを何らかの形で変更したい場合に利用します。生の Sparkplug B エンコード済みペイロードを、さらに処理や解析がしやすい形式に変換します。

使用例：

```sql
select
  spb_decode(payload) as decoded
from t
```

上記の例では、`payload` はデコードしたい生の Sparkplug B メッセージを指します。

[Sparkplug B Protobuf スキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto) を参照すると、メッセージの構造についてさらに理解が深まります。

### spb_encode

`spb_encode` 関数はデータを Sparkplug B メッセージにエンコードするために使用します。これは、Sparkplug B メッセージを MQTT クライアントやシステムの他のコンポーネントに送信する際に特に役立ちます。

使用例：

```sql
select
  spb_encode(json_decode(payload)) as encoded
from t
```

上記の例では、`payload` は Sparkplug B メッセージにエンコードしたいデータを指します。

## 実用例

このセクションでは、`spb_decode` および `spb_encode` 関数を用いた Sparkplug B メッセージ処理の実用例を紹介します。ここで示す例は、可能な操作のごく一部に過ぎません。

以下の構造を持つ Sparkplug B エンコード済みメッセージを例に考えます。

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

デバイスからトピック `my/sparkplug/topic` でメッセージを受信し、その中の `counter_group1/counter1_run` メトリクスだけを抽出して、JSON 形式のメッセージとして別のトピック `interesting_counters/counter1_run_updates` に転送したい場合を考えます。以下の手順は、EMQX ダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/) クライアントツールで動作を確認する方法を示しています。

#### ダッシュボードでのルール作成

1. EMQX ダッシュボードにアクセスし、左側ナビゲーションメニューから **Integration** -> **Rules** をクリックします。**+ Create** をクリックして **Create Rule** ページに入ります。

2. **SQL Editor** に以下の SQL 文を入力します。

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

   ここで、`jq` 関数はメトリクス配列を反復処理し、名前が `"counter_group1/counter1_run"` のものだけを抽出しています。

   ::: tip

   Sparkplug B 仕様では、データは変化があった場合のみ送信することが推奨されているため、ペイロードにはメトリクスの一部のみが含まれることがあります。指定した名前のメトリクスが配列に存在しない場合、このルールは何も出力しません。

   :::

3. ページ右側の **+ Add Action** をクリックし、**Action** ドロップダウンリストから `Republish` を選択します。再パブリッシュ先のトピックに `interesting_counters/counter1_run_updates` を入力し、ペイロード欄には `${item}` を入力します。**Add** をクリックします。

4. **Create Rule** ページに戻り、**Create** をクリックします。ルール一覧に作成したルールが表示されます。

#### ルールのテスト

MQTTX クライアントツールを使用して、Sparkplug B メッセージをトピック `my/sparkplug/topic` にパブリッシュし、メッセージが JSON 形式に変換されてトピック `interesting_counters/counter1_run_updates` に転送されることを確認します。

1. MQTTX クライアントを起動し、EMQX ブローカーに接続します。MQTTX の詳細は [MQTTX クライアント](../../get-started/messaging/publish-and-subscribe.md) を参照してください。

2. 新規サブスクリプションを作成し、トピック `interesting_counters/counter1_run_updates` をサブスクライブします。

3. 画面右下のメッセージ送信エリアにトピック名 `my/sparkplug/topic` を入力し、ペイロードタイプを `Base64` に設定します。

4. 以下の Base64 エンコード済み Sparkplug B メッセージをコピーしてペイロード欄に貼り付けます。これは前述の Sparkplug メッセージ例をエンコードしたものです。

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

5. 送信ボタンをクリックします。

   正常に動作していれば、以下のような JSON 形式のメッセージが受信できます。

   ```json
   {
       "timestamp":1678094561525,
       "name":"counter_group1/counter1_run",
       "int_value":1,
       "datatype":5
   }
   ```

### データ更新

誤ったメトリクス `counter_group1/counter1_run` を発見し、Sparkplug B エンコード済みペイロードから削除してからメッセージを転送したい場合を考えます。

[データ抽出](#データ抽出) の例と同様に、EMQX ダッシュボードで以下のルールを作成し、再パブリッシュアクションを設定します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 削除対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_delete |
   # $to_delete と異なる名前のメトリクスだけを抽出
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # 新しいメトリクス配列でペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

このルールでは、`spb_decode` でメッセージをデコードし、`jq` で名前が `counter_group1/counter1_run` のメトリクスを除外しています。`DO` 節の `spb_encode` で再度メッセージをエンコードしています。

再パブリッシュアクションのペイロードには `${updated_payload}` を指定してください。これは更新後の Sparkplug B エンコード済みメッセージの名前です。

同様に、`spb_decode` と `spb_encode` を使ってメトリクスの値を更新することも可能です。例えば、`counter_group1/counter1_run` の値を 0 に更新したい場合は、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 更新対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_update |
   # 名前が $to_update のメトリクスの値を更新
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

また、新しいメトリクス `counter_group1/counter1_new` を値 42 で追加したい場合は、以下のルールを使用します。

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
   # ペイロードを新しいメトリクス配列で更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

### メッセージのフィルタリング

メトリクス `counter_group1/counter1_run` の値が 0 より大きいメッセージのみを転送したい場合、以下のルールを使用します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # 値が 0 以下のメッセージは除外
   if $value > 0 then $payload else empty end
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

上記ルールでは、`jq` 関数がメトリクス `counter_group1/counter1_run` の値が 0 以下の場合に空の配列を出力します。これにより、値が 0 以下のメッセージはルールに接続されたアクションに転送されません。

### メッセージの分割

Sparkplug B エンコード済みメッセージを複数のメッセージに分割し、メトリクス配列内の各メトリクスを個別の Sparkplug B エンコード済みメッセージとして再パブリッシュしたい場合、以下のルールで実現できます。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1つのメッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスだけを含むメトリクス配列に置き換え
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

上記ルールでは、`jq` 関数が複数のアイテムを含む配列を出力します（メトリクス配列に複数の要素がある場合）。ルールに接続されたすべてのアクションは配列内の各アイテムごとにトリガーされます。再パブリッシュアクションのペイロードには `${output_payload}` を指定してください。これは `DO` 節で Sparkplug B エンコード済みメッセージに割り当てた名前です。

### メッセージの分割と内容に基づくトピック送信

Sparkplug B エンコード済みメッセージを分割しつつ、メトリクス名に基づいて異なるトピックにメッセージを送信したい場合を考えます。例えば、出力トピック名を `"my_metrics/"` とメトリクス名を連結して構成したい場合、以下のように少し修正したコードで実現できます。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1つのメッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスだけを含むメトリクス配列に置き換え
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO
spb_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

再パブリッシュアクションの設定では、トピック名に `${output_topic}` を指定します。これは `DO` 節で出力トピック名として割り当てた名前です。ペイロードには `${output_payload}` を指定してください。

`jq` 関数の呼び出しは `DO` 節内で `first` 関数でラップされており、最初の（かつ唯一の）出力オブジェクトを取得しています。
