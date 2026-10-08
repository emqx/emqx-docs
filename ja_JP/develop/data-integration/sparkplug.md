# Sparkplug B

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse Foundation の TAHU プロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソース仕様であり、MQTT のための明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用IoT分野における相互運用性と一貫性の実現です。

Sparkplug エンコーディングスキームのバージョン B（Sparkplug B）は、監視制御およびデータ収集（SCADA）システム、リアルタイム制御システム、およびデバイス向けの MQTT ネームスペースを定義します。メトリクス、プロセス変数、デバイスの状態情報を含む構造化データ形式を簡潔かつ処理しやすい形でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug B を使用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTT ネットワーク内のデバイス間でシームレスな通信を可能にします。

本ページでは、EMQX における Sparkplug B の実装方法について、データ形式、機能、および実践的な例を交えて解説します。

## Sparkplug B データ形式

Sparkplug B は、データ通信を標準化するために明確に定義されたペイロード構造を利用します。コア部分では、[Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers) を用いて Sparkplug メッセージを構造化し、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQX は [スキーマレジストリ](./schema-registry.md) 機能を通じて Sparkplug B を高度にサポートしています。スキーマレジストリを利用することで、Sparkplug B を含むさまざまなデータ形式のカスタムエンコーダーおよびデコーダーを作成できます。レジストリに[適切な Sparkplug B スキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)を定義することで、EMQX のルールエンジン内で `schema_decode` と `schema_encode` 関数を使い、指定した形式に準拠したデータのアクセスや操作が可能です。

さらに、EMQX は Sparkplug B をネイティブにサポートしており、この特定の形式に対してはスキーマレジストリを使う必要がありません。`spb_encode` と `spb_decode` 関数が EMQX に標準搭載されており、ルールエンジン内で Sparkplug B メッセージのエンコードおよびデコードを簡単に行えます。

:::: tip

以前の `sparkplug_encode` および `sparkplug_decode` 関数は、`bytes_value` の扱いが Sparkplug 仕様と互換性がなかったため非推奨となりました。  
代わりに、更新された `spb_encode` と `spb_decode` 関数をご利用ください。

::::

## Sparkplug B 関数

EMQX は Sparkplug B データのエンコードおよびデコード用に、ルールエンジン SQL 関数 `spb_encode` と `spb_decode` を提供しています。  
[実践例](#examples-for-using-spb_decode-and-spb_encode)では、これらの関数をさまざまなシナリオでどのように使うかを解説しています。

Sparkplug B のエンコード・デコード関数は、ルールエンジンとその `jq` 関数の柔軟性により、多様な処理に利用可能です。ルールエンジンおよび `jq` 関数の詳細は以下のページを参照してください。

* [ルールの作成](./rule-get-started.md)
* [ルールエンジン SQL 言語](./rule-sql-syntax.md)
* [ルールエンジンの JQ 関数](./rule-sql-jq.md)
* [JQ プログラミング言語の完全な説明](https://stedolan.github.io/jq/manual/)

### spb_decode

`spb_decode` 関数は Sparkplug B メッセージをデコードするために使用します。たとえば、Sparkplug B エンコードされたメッセージの内容に基づいて特定のトピックへ転送したり、メッセージを何らかの形で変更したい場合に利用します。生の Sparkplug B エンコードペイロードを、より扱いやすい形式に変換し、さらなる処理や解析を可能にします。

使用例:

```sql
select
  spb_decode(payload) as decoded
from t
```

上記の例では、`payload` はデコードしたい生の Sparkplug B メッセージを指します。

[Sparkplug B Protobuf スキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto) を参照すると、メッセージの構造についてさらに理解が深まります。

### spb_encode

`spb_encode` 関数はデータを Sparkplug B メッセージにエンコードするために使用します。これは、Sparkplug B メッセージを MQTT クライアントやシステムの他のコンポーネントに送信する際に特に有用です。

使用例:

```sql
select
  spb_encode(json_decode(payload)) as encoded
from t
```

上記の例では、`payload` は Sparkplug B メッセージにエンコードしたいデータを指します。

## Sparkplug B エイリアスマッピング

`alias` は Sparkplug B メトリクスの数値識別子です。デバイスがオンラインになると、NBIRTH または DBIRTH メッセージで各メトリクスの `name` と `alias` を宣言します。その後の NDATA または DDATA メッセージでは、デバイスは完全なメトリクス名の代わりに `alias` のみを送信でき、メッセージサイズとネットワークオーバーヘッドを削減します。

エイリアスは Sparkplug B セッション内でのみ意味を持つため、受信側はエイリアスのみのデータを解釈するためにエイリアスと名前のマッピングを必要とします。このマッピングは、対応する NBIRTH または DBIRTH メッセージで宣言されたメトリクス名と各エイリアスを関連付けます。

EMQX は Sparkplug B データをデコードしてルール処理を行い、その結果を標準 MQTT クライアントやデータプラットフォームなどの非 Sparkplug B クライアントに転送できます。これらの下流システムは通常 Sparkplug B セッション状態を保持しないため、エイリアスのみのメトリクスを自力で解決できません。EMQX 6.0.2 以降ではエイリアスマッピングをサポートしており、現在の MQTT クライアントセッションに対応するマッピングがある場合、`spb_decode` はデコード時にエイリアスのみのメトリクスに不足しているメトリクス名を追加します。

::: warning 重要なお知らせ

EMQX 6.0.4 以降、EMQX は MQTT クライアントが直接パブリッシュしたメッセージのみエイリアスマッピングを保持します。MQTT ブリッジや他の内部経路を通じて取り込まれたメッセージはエイリアスマッピングを作成・使用しません。そのため、これらの経路経由で受信したエイリアスのみの NDATA または DDATA メッセージに対しては、`spb_decode` はメトリクス名を復元しません。

:::

### Sparkplug B エイリアスマッピングの動作

エイリアスマッピングが有効な場合、EMQX は以下のように Sparkplug B メッセージを処理します。

1. **NBIRTH / DBIRTH メッセージの処理**

   MQTT クライアントが直接 NBIRTH または DBIRTH メッセージをパブリッシュすると、EMQX はペイロード内のメトリクスを調べ、両方のフィールド（`name` と `alias`）を定義しているメトリクスのエイリアスと名前のマッピングを記録します。

2. **セッションごとのマッピング管理**

   エイリアスマッピングは MQTT クライアントセッションごとに管理され、Sparkplug B の意味論に従います。

   - ノードレベルのメトリクス（NBIRTH / NDATA）とデバイスレベルのメトリクス（DBIRTH / DDATA）は別々に追跡されます。
   - 異なるクライアントのマッピングは完全に分離され、相互に干渉しません。

3. **`spb_decode` 出力の強化**

   ルールエンジンが NDATA または DDATA メッセージに対して `spb_decode` を呼び出し、メトリクスに `alias` はあるが `name` がない場合、EMQX は現在の MQTT クライアントセッションで記録されたマッピングを使って対応するメトリクス名を復元します。

   現在のセッションに対応するマッピングがない場合、`spb_decode` はメトリクス名を追加せずにメッセージをデコードします。

4. **セッション終了時のクリーンアップ**

   クライアントが切断されると、そのセッションに関連付けられたエイリアスマッピングは削除されます。EMQX はセッション終了後に Sparkplug B の状態を保持または復元しません。

### エイリアスマッピングの設定

エイリアスマッピングはデフォルトで有効になっています。EMQX による Sparkplug B メトリクスのエイリアス追跡および復元を無効にしたい場合は、設定ファイルで以下のように設定してください。

```hocon
schema_registry {
  sparkplugb {
    enable_alias_mapping = false
  }
}
```

> **注意**:
>
> - エイリアスマッピングは、エイリアスマッピングが有効な状態で MQTT クライアントが直接パブリッシュした NBIRTH / DBIRTH メッセージからのみ作成されます。
> - クライアントがすでにバースメッセージを送信済みの場合、エイリアスマッピングを適用するには再接続して NBIRTH / DBIRTH を再度パブリッシュする必要があります。

### エイリアスマッピングの例

この例では、EMQX ダッシュボードと MQTTX を使って、エイリアスのみの DDATA メッセージをフルメトリクス名を含む JSON データに変換し、その結果を非 Sparkplug B クライアントに転送する方法を示します。

#### 目的

- **Sparkplug B デバイス**：DBIRTH で `name + alias` を宣言し、DDATA では `alias` のみをパブリッシュする。
- **EMQX**：`spb_decode` を使ってメトリクス名を自動的に復元する。
- **下流のサブスクライバー**：Sparkplug B の知識なしに標準的な JSON メッセージを受信する。

#### 前提条件

- EMQX 6.0.2 以降で Sparkplug B エイリアスマッピングが有効（`enable_alias_mapping = true`）
- DBIRTH と DDATA メッセージのパブリッシュに同一の直接 MQTT クライアント接続を使用
- [MQTTX](https://mqttx.app/) の利用

#### ステップ 1: EMQX ダッシュボードでルールを作成

1. ダッシュボードの左メニューから **Integration** -> **Rules** をクリック。

2. **+ Create** をクリックして新規ルール作成画面へ。

3. **SQL Editor** に以下を入力。

   ```sql
   SELECT
     spb_decode(payload) AS decoded
   FROM "spBv1.0/+/DDATA/+/+"
   ```

   > **補足**:
   >
   > - このルールはすべての Sparkplug B DDATA メッセージにマッチします。
   > - `spb_decode(payload)` は Sparkplug B ペイロードをデコードし、エイリアスマッピングが有効な場合はエイリアスからメトリクス名を自動復元します。

4. **+ Add Action** をクリックしてアクションを追加。

5. アクションタイプに **Republish** を選択。

6. アクション設定：

   - **Topic**: `decoded/sparkplug/data`
   - **Payload**: `${decoded}`

7. **Add** をクリック。

8. **Save** をクリックしてルール作成を完了。

   ![sparkplugb_alias_mapping_create_rule](./assets/sparkplugb_alias_mapping_create_rule.png)

#### ステップ 2: MQTTX でサブスクライバーを準備

1. MQTTX を開き、EMQX ブローカーへの新規接続を作成。

2. トピック `decoded/sparkplug/data` をサブスクライブ。

このサブスクライバーは、プレーンな JSON データを期待する**非 Sparkplug B クライアント**を表します。

#### ステップ 3: MQTTX で Sparkplug B デバイスをシミュレート

以下のペイロードは読みやすさのため論理的な JSON 形式で示しています。実際にメッセージをパブリッシュする際は Sparkplug B Protobuf エンコード（Base64）を使用してください。

1. DBIRTH（エイリアス宣言）をトピック `spBv1.0/group1/DBIRTH/eon1/device1` に送信。

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
   > - Sparkplug B では `datatype` は符号なし整数で定義されており、値 `9` は Sparkplug B 仕様で Float データ型を表します。
   > - EMQX はこの時点でエイリアスと名前のマッピングを記録します。
   > - このステップは DDATA 送信の前に必ず実行してください。

2. DDATA（エイリアスのみ）をトピック `spBv1.0/group1/DDATA/eon1/device1` に送信。

   **論理ペイロード（例）**

   ```json
   {
     "metrics": [
       { "alias": 0, "value": 73.1 },
       { "alias": 1, "value": 100.9 }
     ]
   }
   ```

#### ステップ 4: デコード結果を確認

MQTTX の `decoded/sparkplug/data` サブスクライバーは以下を受信します。

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

確認できる点は以下の通りです。

- 元の DDATA メッセージには `name` が含まれていませんでした。
- `spb_decode` により自動的に以下が復元されました。
  - `"Device/Temperature"`
  - `"Device/Pressure"`
- 下流のサブスクライバーは Sparkplug B の状態を保持したりエイリアスを解釈したりする必要がありません。

## `spb_decode` と `spb_encode` の使用例

このセクションでは、`spb_decode` と `spb_encode` 関数を用いた Sparkplug B メッセージ処理の実践例を紹介します。ここに示す例は可能な操作の一部に過ぎません。

以下の構造を持つ Sparkplug B エンコードメッセージを想定します。

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

デバイスからトピック `my/sparkplug/topic` でメッセージを受け取り、`counter_group1/counter1_run` メトリクスだけを JSON 形式で別トピック `interesting_counters/counter1_run_updates` に転送したい場合の手順を示します。EMQX ダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/) クライアントでテストします。

#### ダッシュボードでルール作成

1. EMQX ダッシュボードを開き、左ナビゲーションメニューから **Integration** -> **Rules** をクリック。**+ Create** をクリックしてルール作成画面へ。

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

   ここで `jq` 関数はメトリクス配列を反復処理し、名前が `"counter_group1/counter1_run"` のメトリクスだけを抽出しています。

   ::: tip

   Sparkplug B 仕様では、データは変化時のみ送信されることが推奨されているため、ペイロードにメトリクスの一部しか含まれない場合があります。指定した名前のアイテムが配列に存在しない場合、このルールは何も出力しません。

   :::

3. ページ右側の **+ Add Action** をクリック。アクションの種類から `Republish` を選択。リパブリッシュ先トピックに `interesting_counters/counter1_run_updates` を入力し、ペイロードには `${item}` を設定。**Add** をクリック。

4. **Create Rule** ページに戻り、**Create** をクリック。ルール一覧に作成したルールが表示されます。

#### ルールのテスト

MQTTX クライアントツールを使って Sparkplug B メッセージをトピック `my/sparkplug/topic` にパブリッシュし、メッセージが JSON 形式に変換されてトピック `interesting_counters/counter1_run_updates` に転送されることを確認します。

1. MQTTX クライアントを開き、EMQX ブローカーに接続します。MQTTX の詳細は [MQTTX クライアント](../../get-started/messaging/publish-and-subscribe.md) を参照してください。

2. 新規サブスクリプションを作成し、トピック `interesting_counters/counter1_run_updates` をサブスクライブ。

3. メッセージ送信エリアでトピックに `my/sparkplug/topic` を入力し、ペイロードタイプを `Base64` に設定。

4. 以下の Base64 エンコードされた Sparkplug B メッセージをペイロード欄に貼り付け。これは前述の Sparkplug メッセージ例をエンコードしたものです。

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

5. 送信ボタンをクリック。

   正常に動作すれば、以下のような JSON 形式のメッセージが受信されます。

   ```json
   {
       "timestamp":1678094561525,
       "name":"counter_group1/counter1_run",
       "int_value":1,
       "datatype":5
   }
   ```

### データ更新

誤ったメトリクス `counter_group1/counter1_run` を発見し、Sparkplug B エンコードペイロードから削除してからメッセージを転送したい場合を考えます。

[データ抽出](#データ抽出)の例と同様に、EMQX ダッシュボードで以下のルールを作成し、リパブリッシュアクションを設定します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 削除対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_delete |
   # $to_delete と名前が異なるメトリクスだけを抽出
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # 新しいメトリクス配列でペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

このルールでは、`spb_decode` でメッセージをデコードし、`jq` で指定した名前のメトリクスを除外しています。`DO` 節の `spb_encode` で再度メッセージをエンコードします。

リパブリッシュアクションでは、ペイロードに `${updated_payload}` を設定してください。これは更新された Sparkplug B エンコードメッセージの名前です。

同様に、メトリクス `counter_group1/counter1_run` の値を 0 に更新したい場合は、以下のルールを使えます。

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

また、`counter_group1/counter1_new` という名前で値 42 の新しいメトリクスを追加したい場合は、以下のルールを使えます。

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
   # 新しいメトリクス配列でペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

### メッセージのフィルタリング

メトリクス `counter_group1/counter1_run` の値が 0 より大きいメッセージのみ転送したい場合は、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # 値が 0 以下のメッセージを除外
   if $value > 0 then $payload else empty end
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

このルールでは、`jq` 関数が指定メトリクスの値が 0 以下の場合に空の配列を出力します。つまり、値が 0 以下のメッセージはルールに接続されたアクションに転送されません。

### メッセージの分割

Sparkplug B エンコードメッセージを複数のメッセージに分割し、メトリクス配列の各メトリクスを個別の Sparkplug B エンコードメッセージとしてリパブリッシュしたい場合は、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとにメッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスだけを含むメトリクス配列に置き換え
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

このルールでは、`jq` 関数が複数のアイテムを含む配列を出力します（メトリクス配列に複数アイテムがある場合）。ルールに接続されたすべてのアクションは配列の各アイテムに対してトリガーされます。リパブリッシュアクションのペイロードには `${output_payload}` を設定してください。`output_payload` は `DO` 節で Sparkplug B エンコードメッセージに割り当てた名前です。

### メッセージの分割と内容に応じたトピック送信

Sparkplug B エンコードメッセージを分割しつつ、各メッセージをメトリクス名に基づいて異なるトピックに送信したい場合を考えます。たとえば、出力トピック名を `"my_metrics/"` とメトリクス名の連結で構成したい場合、以下のように少し修正したコードを使えます。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとにメッセージを出力
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

リパブリッシュアクションの設定では、トピック名に `${output_topic}` を指定してください。これは `DO` 節で出力トピック名として割り当てた名前です。ペイロードには `${output_payload}` を設定します。

`jq` 関数の呼び出しは `DO` 節内で `first` 関数でラップしており、最初の（かつ唯一の）出力オブジェクトを取得しています。
