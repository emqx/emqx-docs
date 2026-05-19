# Sparkplug B

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse Foundation の TAHU プロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソースの仕様であり、MQTT における明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用 IoT 分野における相互運用性と一貫性の実現です。

Sparkplug エンコーディングスキームのバージョン B（Sparkplug B）は、監視制御およびデータ収集（SCADA）システム、リアルタイム制御システム、およびデバイス向けの MQTT ネームスペースを定義します。メトリクス、プロセス変数、デバイス状態情報を含む構造化データフォーマットを簡潔かつ処理しやすい形式でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug B を利用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTT ネットワーク内のデバイス間でシームレスな通信を実現できます。

本ページでは、EMQX における Sparkplug B の実装方法について、データフォーマット、機能、および実用例を交えて解説します。

## Sparkplug B データフォーマット

Sparkplug B は、データ通信の標準化のために明確に定義されたペイロード構造を利用します。コアには [Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers) を用いて Sparkplug メッセージを構造化し、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQX は [スキーマレジストリ](./schema-registry.md) 機能を通じて Sparkplug B を高度にサポートしています。スキーマレジストリを利用することで、Sparkplug B を含むさまざまなデータフォーマットに対してカスタムエンコーダーおよびデコーダーを作成可能です。レジストリに [適切な Sparkplug B スキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto) を定義することで、EMQX のルールエンジン内で `schema_decode` および `schema_encode` 関数を使い、指定されたフォーマットに準拠したデータのアクセスや操作が可能になります。

さらに、EMQX は Sparkplug B に対して組み込みサポートを提供しており、この特定フォーマットに関してはスキーマレジストリを使用する必要がありません。`spb_encode` および `spb_decode` 関数が EMQX に標準搭載されており、ルールエンジン内での Sparkplug B メッセージのエンコード・デコードを簡素化しています。

:::: tip

以前の `sparkplug_encode` および `sparkplug_decode` 関数は、`bytes_value` の扱いが Sparkplug 仕様と互換性がなかったため非推奨となりました。  
代わりに、更新された `spb_encode` および `spb_decode` 関数をご利用ください。

::::

## Sparkplug B 関数

EMQX は Sparkplug B データのエンコードおよびデコードのために、ルールエンジン SQL 関数として `spb_encode` と `spb_decode` の2つを提供しています。  
[実用例](#examples-for-using-spb_decode-and-spb_encode)では、これらの関数をさまざまなシナリオでどのように使うかを解説しています。

Sparkplug B のエンコード・デコード関数は、ルールエンジンとその `jq` 関数の柔軟性により、多様な処理に活用可能です。ルールエンジンおよび `jq` 関数の詳細は以下のページをご参照ください。

* [ルールの作成](./rule-get-started.md)
* [ルールエンジン SQL 言語](./rule-sql-syntax.md)
* [ルールエンジンの JQ 関数](./rule-sql-jq.md)
* [JQ プログラミング言語の完全な説明](https://stedolan.github.io/jq/manual/)

### spb_decode

`spb_decode` 関数は Sparkplug B メッセージをデコードするために使用します。たとえば、Sparkplug B エンコード済みメッセージの内容に基づいて特定のトピックにメッセージを転送したり、メッセージを何らかの形で変更したい場合に利用します。生の Sparkplug B エンコードペイロードを、より扱いやすい形式に変換し、さらに処理や解析が可能になります。

使用例：

```sql
select
  spb_decode(payload) as decoded
from t
```

上記の例では、`payload` はデコードしたい生の Sparkplug B メッセージを指します。

[Sparkplug B Protobuf スキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto) を参照すると、メッセージ構造の詳細が確認できます。

### spb_encode

`spb_encode` 関数はデータを Sparkplug B メッセージにエンコードするために使用します。MQTT クライアントやシステム内の他のコンポーネントに Sparkplug B メッセージを送信したい場合に特に有用です。

使用例：

```sql
select
  spb_encode(json_decode(payload)) as encoded
from t
```

上記の例では、`payload` は Sparkplug B メッセージにエンコードしたいデータを指します。

## Sparkplug B エイリアスマッピング

Sparkplug B 仕様では、デバイスがオンラインになる際（NBIRTH / DBIRTH メッセージ送信時）に、各メトリクスに対して数値の `alias` を割り当てることが許可されています。その後のデータ更新（NDATA / DDATA メッセージとして送信）では、メッセージサイズとネットワークオーバーヘッドを減らすために、デバイスは完全なメトリクス名（`name`）の代わりに `alias` のみをパブリッシュすることができます。

これらのエイリアスのみの更新を正しく解釈するためには、受信側が Sparkplug B セッション状態を追跡し、各エイリアスを元のメトリクス名に解決できる必要があります。

実際には、EMQX は Sparkplug B データの中央処理および配信ハブとして機能します。ルールエンジンを用いて、EMQX はデコード済みデータを Sparkplug B 非対応のクライアント（標準 MQTT クライアントやデータプラットフォームなど）に転送します。これらの下流システムは通常 Sparkplug B の状態管理を実装していないため、エイリアスのみのデータを扱うのが困難です。

EMQX 6.0.2 以降、`spb_decode` 関数は Sparkplug B エイリアスマッピングをサポートするように強化されました。この機能により、EMQX はデコード時にメトリクス名を自動的に復元し、下流システムが扱いやすいデータを生成できます。

### Sparkplug B エイリアスマッピングの仕組み

エイリアスマッピングが有効な場合、EMQX は以下のように Sparkplug B メッセージを処理します。

1. **NBIRTH / DBIRTH メッセージの処理**

   クライアントが NBIRTH または DBIRTH メッセージをパブリッシュすると、EMQX はペイロード内のメトリクスを調査し、`alias` と `name` の両方が定義されているメトリクスについてエイリアスから名前へのマッピングを記録します。

2. **セッション単位でのマッピング管理**

   エイリアスマッピングは MQTT クライアントのセッション単位で管理され、Sparkplug B のセマンティクスに従います。

   - ノードレベルメトリクス（NBIRTH / NDATA）とデバイスレベルメトリクス（DBIRTH / DDATA）は別々に管理されます。
   - 異なるクライアント間のマッピングは完全に分離され、相互に干渉しません。

3. **`spb_decode` 出力の強化**

   ルールエンジンが NDATA または DDATA メッセージに対して `spb_decode` を呼び出し、かつメトリクスに `alias` はあるが `name` がない場合、EMQX は記録済みマッピングを用いて対応するメトリクス名を自動復元します。

   その結果、デコード済みメッセージには常に明確で読みやすいメトリクス名が含まれ、ルール処理、変換、転送に適した形となります。

4. **セッション終了時のクリーンアップ**

   クライアントが切断されると、そのセッションに関連付けられたエイリアスマッピングは削除されます。EMQX はセッション終了後に Sparkplug B 状態を保持または復元しません。

### エイリアスマッピングの設定

エイリアスマッピングはデフォルトで有効です。EMQX による Sparkplug B メトリクスエイリアスの追跡および復元を無効にしたい場合は、設定ファイルで以下のように無効化できます。

```hocon
schema_registry {
  sparkplugb {
    enable_alias_mapping = false
  }
}
```

> **注意**:
>
> - エイリアスマッピングは、マッピング有効時に受信した NBIRTH / DBIRTH メッセージからのみ作成されます。
> - クライアントがすでにバースメッセージを送信済みの場合、エイリアスマッピングを適用するには再接続して NBIRTH / DBIRTH を再送信する必要があります。

### エイリアスマッピングの例

この例では、EMQX ダッシュボードと MQTTX を使って、エイリアスのみの DDATA メッセージをフルメトリクス名を含む JSON データに変換し、Sparkplug B 非対応クライアントに転送する方法を示します。

#### 目的

- **Sparkplug B デバイス**：DBIRTH で `name + alias` を宣言し、DDATA では `alias` のみをパブリッシュ。
- **EMQX**：`spb_decode` を使ってメトリクス名を自動復元。
- **下流サブスクライバー**：Sparkplug B の知識不要で標準 JSON メッセージを受信。

#### 前提条件

- EMQX 6.0.2 以降、Sparkplug B エイリアスマッピング有効（`enable_alias_mapping = true`）
- [MQTTX](https://mqttx.app/)

#### ステップ 1: EMQX ダッシュボードでルール作成

1. ダッシュボードの左メニューから **Integration** -> **Rules** をクリック。

2. **+ Create** をクリックして新規ルール作成画面へ。

3. **SQL Editor** に以下を入力：

   ```sql
   SELECT
     spb_decode(payload) AS decoded
   FROM "spBv1.0/+/DDATA/+/+"
   ```

   > **補足**：
   >
   > - このルールはすべての Sparkplug B DDATA メッセージにマッチします。
   > - `spb_decode(payload)` はペイロードをデコードし、エイリアスマッピング有効時にはエイリアスからメトリクス名を自動復元します。

4. **+ Add Action** をクリックしてアクションを追加。

5. アクションタイプに **Republish** を選択。

6. アクション設定：

   - **Topic**: `decoded/sparkplug/data`
   - **Payload**: `${decoded}`

7. **Add** をクリック。

8. **Save** をクリックしてルール作成完了。

   ![sparkplugb_alias_mapping_create_rule](./assets/sparkplugb_alias_mapping_create_rule.png)

#### ステップ 2: MQTTX でサブスクライバー準備

1. MQTTX を開き、EMQX ブローカーへの新規接続を作成。

2. トピック `decoded/sparkplug/data` をサブスクライブ。

このサブスクライバーは Sparkplug B 非対応のクライアントを想定し、プレーンな JSON データを受信します。

#### ステップ 3: MQTTX で Sparkplug B デバイスをシミュレート

以下のペイロードは可読性のため論理的に JSON 形式で示しています。実際のメッセージ送信時は Sparkplug B Protobuf エンコード（Base64）を使用してください。

1. DBIRTH（エイリアス宣言）をトピック `spBv1.0/group1/DBIRTH/eon1/device1` に送信。

   **論理ペイロード例**

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

   > **補足**：
   >
   > - Sparkplug B では `datatype` は符号なし整数で定義されており、値 `9` は Sparkplug B 仕様で Float 型を表します。
   > - EMQX はこの時点でエイリアスから名前へのマッピングを記録します。
   > - この手順は DDATA 送信前に必ず実施してください。

2. DDATA（エイリアスのみ）をトピック `spBv1.0/group1/DDATA/eon1/device1` に送信。

   **論理ペイロード例**

   ```json
   {
     "metrics": [
       { "alias": 0, "value": 73.1 },
       { "alias": 1, "value": 100.9 }
     ]
   }
   ```

#### ステップ 4: デコード結果の確認

MQTTX で `decoded/sparkplug/data` をサブスクライブしていると、以下のようなメッセージを受信します。

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

以下の点が確認できます。

- 元の DDATA メッセージには `name` が含まれていません。
- `spb_decode` が自動的に以下を復元しています。
  - `"Device/Temperature"`
  - `"Device/Pressure"`
- 下流のサブスクライバーは Sparkplug B の状態管理やエイリアス解釈を行う必要がありません。

## `spb_decode` と `spb_encode` の使用例

このセクションでは、`spb_decode` および `spb_encode` 関数を用いた Sparkplug B メッセージ処理の実用例を紹介します。ここで示す例は可能な操作の一部に過ぎません。

以下のような構造の Sparkplug B エンコード済みメッセージを受け取るケースを想定します。

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

デバイスからトピック `my/sparkplug/topic` でメッセージを受け取り、`counter_group1/counter1_run` メトリクスのみを JSON 形式で `interesting_counters/counter1_run_updates` トピックに転送したい場合の手順です。EMQX ダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/) クライアントツールで動作確認します。

#### ダッシュボードでルール作成

1. EMQX ダッシュボードの左ナビゲーションメニューから **Integration** -> **Rules** を選択し、**+ Create** をクリックしてルール作成画面へ。

2. **SQL Editor** に以下の SQL 文を入力。

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

   ここで、`jq` 関数はメトリクス配列を反復処理し、`"counter_group1/counter1_run"` という名前のメトリクスのみを抽出しています。

   ::: tip

   Sparkplug B 仕様では、データは変化時のみ送信することが推奨されているため、ペイロードにはメトリクスの一部しか含まれない場合があります。指定した名前のメトリクスが存在しない場合、このルールは何も出力しません。

   :::

3. ページ右側の **+ Add Action** をクリックし、アクションタイプから `Republish` を選択。  
   再パブリッシュ先トピックに `interesting_counters/counter1_run_updates` を指定し、ペイロードには `${item}` を入力。**Add** をクリック。

4. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成。

#### ルールのテスト

MQTTX クライアントツールを使って、Sparkplug B メッセージをトピック `my/sparkplug/topic` にパブリッシュし、変換された JSON メッセージが `interesting_counters/counter1_run_updates` に転送されることを確認します。

1. MQTTX クライアントを起動し、EMQX ブローカーに接続。詳細は [MQTTX クライアント](../messaging/publish-and-subscribe.md) を参照。

2. 新規サブスクリプションを作成し、トピック `interesting_counters/counter1_run_updates` をサブスクライブ。

3. メッセージ送信欄にトピック `my/sparkplug/topic` を入力し、ペイロードタイプを `Base64` に設定。

4. 以下の Base64 エンコード済み Sparkplug B メッセージをペイロード欄に貼り付け。これは前述の Sparkplug メッセージ例をエンコードしたものです。

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

5. 送信ボタンをクリック。

   正常に動作していれば、以下のような JSON メッセージを受信できます。

   ```json
   {
       "timestamp":1678094561525,
       "name":"counter_group1/counter1_run",
       "int_value":1,
       "datatype":5
   }
   ```

### データ更新

誤ったメトリクス `counter_group1/counter1_run` を Sparkplug B エンコード済みペイロードから削除してから転送したい場合の例です。

[データ抽出](#データ抽出)の例と同様に、EMQX ダッシュボードで以下のルールを作成し、再パブリッシュアクションを設定します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 削除対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_delete |
   # $to_delete と異なるメトリクスのみ抽出
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # 新しいメトリクス配列でペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

このルールでは、`spb_decode` でメッセージをデコードし、`jq` で指定したメトリクス名のものを除外しています。その後、`DO` 節で `spb_encode` により再エンコードしています。

再パブリッシュアクションでは、ペイロードに `${updated_payload}` を指定してください。これは更新済み Sparkplug B メッセージの変数名です。

同様に、メトリクスの値を更新したい場合も `spb_decode` と `spb_encode` を使って実現可能です。例えば、`counter_group1/counter1_run` の値を 0 に更新するルールは以下の通りです。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 更新対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_update |
   # 指定メトリクスの値を更新
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

また、新しいメトリクス `counter_group1/counter1_new` を値 42 で追加したい場合は以下のようにします。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 既存メトリクスを保存
   $payload | .metrics as $old_metrics |
   # 追加する新しいメトリクス
   {
     "name": "counter_group1/counter1_new",
     "int_value": 42,
     "datatype": 5
   } as $new_value |
   # 新しいメトリクス配列を作成
   ($old_metrics + [ $new_value ]) as $updated_metrics |
   # ペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

### メッセージフィルタリング

メトリクス `counter_group1/counter1_run` の値が 0 より大きいメッセージのみを転送したい場合の例です。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # 値が 0 以下の場合は出力しない
   if $value > 0 then $payload else empty end
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

このルールでは、`jq` 関数がフィルタ条件に合わない場合に空配列を返すため、ルールに接続されたアクションはトリガーされません。

### メッセージ分割

Sparkplug B エンコード済みメッセージを複数のメッセージに分割し、メトリクス配列の各メトリクスを個別の Sparkplug B エンコードメッセージとして再パブリッシュしたい場合は以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1メッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスのみを配列にセット
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

このルールでは、`jq` 関数が複数のアイテムを含む配列を出力し、ルールに接続されたすべてのアクションが各アイテムごとにトリガーされます。  
再パブリッシュアクションのペイロードには `${output_payload}` を指定してください。これは `DO` 節で割り当てた Sparkplug B エンコード済みメッセージの変数名です。

### メッセージ分割と内容に基づくトピック振り分け

Sparkplug B エンコード済みメッセージを分割しつつ、例えばメトリクス名に基づいて異なるトピックに送信したい場合の例です。出力トピック名は `"my_metrics/"` とメトリクス名を連結して構築するとします。

以下のようにコードを少し修正して実現可能です。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1メッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスのみを配列にセット
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO
spb_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

再パブリッシュアクションの設定では、トピック名に `${output_topic}` を指定し、ペイロードに `${output_payload}` を設定してください。  
`jq` 関数の呼び出しは `DO` 節内で `first` 関数でラップし、最初の（かつ唯一の）出力オブジェクトを取得しています。
