# Sparkplug B

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse Foundation の TAHU プロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソース仕様であり、MQTT における明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用 IoT 分野における相互運用性と一貫性の実現です。

Sparkplug エンコーディングスキームのバージョン B（Sparkplug B）は、監視制御およびデータ取得（SCADA）システム、リアルタイム制御システム、およびデバイス向けの MQTT ネームスペースを定義します。メトリクス、プロセス変数、デバイスステータス情報を含む構造化データ形式を簡潔かつ処理しやすいフォーマットでカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug B を使用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTT ネットワーク内のデバイス間でシームレスな通信を可能にします。

本ページでは、EMQX における Sparkplug B の実装方法について、データ形式、機能、および実践的な例を交えて解説します。

## Sparkplug B データ形式

Sparkplug B は、データ通信の標準化のために明確に定義されたペイロード構造を利用します。コア部分では、[Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers) を用いて Sparkplug メッセージを構造化し、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQX は [スキーマレジストリ](./schema-registry.md) 機能を通じて Sparkplug B を高度にサポートしています。スキーマレジストリを利用することで、Sparkplug B を含むさまざまなデータ形式のカスタムエンコーダーおよびデコーダーを作成できます。レジストリに[適切な Sparkplug B スキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)を定義することで、EMQX のルールエンジン内で `schema_decode` および `schema_encode` 関数を使用し、指定された形式に準拠したデータにアクセス・操作が可能です。

さらに、EMQX は Sparkplug B に対する組み込みサポートも提供しており、この特定の形式に対してはスキーマレジストリを使用する必要がありません。`spb_encode` および `spb_decode` 関数が EMQX に標準搭載されており、ルールエンジン内での Sparkplug B メッセージのエンコードおよびデコードを簡素化します。

:::: tip

以前の `sparkplug_encode` および `sparkplug_decode` 関数は、`bytes_value` の取り扱いが Sparkplug 仕様と互換性がなかったため非推奨となりました。  
代わりに、更新された `spb_encode` および `spb_decode` 関数をご利用ください。

::::

## Sparkplug B 関数

EMQX は Sparkplug B データのエンコードおよびデコード用に、ルールエンジン SQL 関数として `spb_encode` と `spb_decode` の2つを提供しています。[実践例](#examples-for-using-spb_decode-and-spb_encode)では、これらの関数をさまざまなシナリオでどのように使うかを解説しています。

Sparkplug B のエンコード・デコード関数は、ルールエンジンとその `jq` 関数の柔軟性により、多様な処理に利用可能です。ルールエンジンおよび `jq` 関数の詳細は以下のページをご参照ください。

* [ルールの作成](./rule-get-started.md)
* [ルールエンジン SQL 言語](./rule-sql-syntax.md)
* [ルールエンジンの JQ 関数](./rule-sql-jq.md)
* [JQ プログラミング言語の完全な説明](https://stedolan.github.io/jq/manual/)

### spb_decode

`spb_decode` 関数は Sparkplug B メッセージのデコードに使用します。例えば、Sparkplug B エンコードされたメッセージの内容に基づいて特定のトピックにメッセージを転送したり、メッセージを何らかの形で変更したい場合に利用します。生の Sparkplug B エンコードペイロードを、より扱いやすい形式に変換し、さらに処理や解析が可能になります。

使用例:

```sql
select
  spb_decode(payload) as decoded
from t
```

上記の例では、`payload` はデコードしたい生の Sparkplug B メッセージを指します。

[Sparkplug B Protobuf スキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto) を参照すると、メッセージ構造の詳細がわかります。

### spb_encode

`spb_encode` 関数はデータを Sparkplug B メッセージにエンコードするために使用します。これは、Sparkplug B メッセージを MQTT クライアントやシステムの他のコンポーネントに送信する必要がある場合に特に有用です。

使用例:

```sql
select
  spb_encode(json_decode(payload)) as encoded
from t
```

上記の例では、`payload` は Sparkplug B メッセージにエンコードしたいデータを指します。

## Sparkplug B エイリアスマッピング

Sparkplug B 仕様では、デバイスがオンラインになる際（NBIRTH / DBIRTH メッセージ送信時）に、各メトリクスに対して数値の `alias` を割り当てることが許可されています。その後のデータ更新（NDATA / DDATA メッセージとして送信）では、メッセージサイズとネットワークオーバーヘッドを削減するために、完全なメトリクス名（`name`）の代わりに `alias` のみをパブリッシュすることが可能です。

これらのエイリアスのみの更新を正しく解釈するには、受信側が Sparkplug B セッション状態を追跡し、各エイリアスを元のメトリクス名に解決できる必要があります。

実際には、EMQX は Sparkplug B データの中央処理および配信ハブとして機能します。ルールエンジンを利用して、EMQX は標準 MQTT クライアントやデータプラットフォームなど、Sparkplug B 非対応のクライアントへデコード済みデータを転送します。これらの下流システムは通常 Sparkplug B の状態管理を実装していないため、エイリアスのみのデータは扱いにくい状況です。

EMQX 6.0.2 以降、`spb_decode` 関数は Sparkplug B のエイリアスマッピングをサポートするよう強化されました。この強化により、EMQX はデコード時にメトリクス名を自動的に復元できるため、下流システムがより扱いやすいデータを受け取れます。

### Sparkplug B エイリアスマッピングの仕組み

エイリアスマッピングが有効な場合、EMQX は Sparkplug B メッセージを以下のように処理します。

1. **NBIRTH / DBIRTH メッセージの処理**

   クライアントが NBIRTH または DBIRTH メッセージをパブリッシュすると、EMQX はペイロード内のメトリクスを調査し、`alias` と `name` の両方が定義されているメトリクスについてエイリアスから名前へのマッピングを記録します。

2. **セッションごとのマッピング維持**

   エイリアスマッピングは MQTT クライアントのセッションごとに管理され、Sparkplug B のセマンティクスに従います。

   - ノードレベルメトリクス（NBIRTH / NDATA）とデバイスレベルメトリクス（DBIRTH / DDATA）は別々に追跡されます。
   - 異なるクライアントからのマッピングは完全に分離され、相互に干渉しません。

3. **`spb_decode` 出力の強化**

   ルールエンジンが NDATA または DDATA メッセージに対して `spb_decode` を呼び出し、かつメトリクスに `alias` はあるが `name` がない場合、EMQX は事前に記録されたマッピングを使って対応するメトリクス名を自動的に復元します。

   その結果、デコード済みメッセージには常に明確で読みやすいメトリクス名が含まれ、ルール処理、変換、転送に適した形式となります。

4. **セッション終了時のクリーンアップ**

   クライアントが切断されると、そのクライアントに関連付けられたエイリアスマッピングは削除されます。EMQX はセッション終了後に Sparkplug B 状態を保持または復元しません。

### エイリアスマッピングの設定

エイリアスマッピングはデフォルトで有効です。EMQX による Sparkplug B メトリクスエイリアスの追跡および復元を無効にしたい場合は、設定ファイルで以下のように設定してください。

```hocon
schema_registry {
  sparkplugb {
    enable_alias_mapping = false
  }
}
```

> **注意**:
>
> - エイリアスマッピングは、エイリアスマッピングが有効な状態で受信した NBIRTH / DBIRTH メッセージからのみ作成されます。
> - クライアントがすでにバースメッセージを送信済みの場合、エイリアスマッピングを適用するには再接続して NBIRTH / DBIRTH を再送信する必要があります。

### エイリアスマッピングの例

この例では、EMQX ダッシュボードと MQTTX を使用して、エイリアスのみの DDATA メッセージを完全なメトリクス名を含む JSON データに変換し、その結果を Sparkplug B 非対応のクライアントに転送する方法を示します。

#### 目的

- **Sparkplug B デバイス**：DBIRTH で `name + alias` を宣言し、DDATA では `alias` のみをパブリッシュ。
- **EMQX**：`spb_decode` を使ってメトリクス名を自動復元。
- **下流サブスクライバー**：Sparkplug B の知識なしに標準的な JSON メッセージを受信。

#### 前提条件

- EMQX 6.0.2 以降で、Sparkplug B エイリアスマッピングが有効（`enable_alias_mapping = true`）
- [MQTTX](https://mqttx.app/)

#### ステップ 1: EMQX ダッシュボードでルールを作成

1. ダッシュボード左メニューの **Integration** -> **Rules** をクリック。

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
   > - `spb_decode(payload)` はペイロードをデコードし、エイリアスマッピングが有効な場合はエイリアスからメトリクス名を自動復元します。

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

このサブスクライバーは、プレーンな JSON データを期待する **Sparkplug B 非対応クライアント** を表します。

#### ステップ 3: MQTTX で Sparkplug B デバイスをシミュレート

以下のペイロードは可読性のため論理的な JSON 形式で示しています。実際のメッセージ送信時は Sparkplug B Protobuf エンコード（Base64）を使用してください。

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
   > - Sparkplug B では `datatype` は符号なし整数で定義されており、値 `9` は Float データ型を示します（Sparkplug B 仕様による）。
   > - この時点で EMQX はエイリアスから名前へのマッピングを記録します。
   > - このステップは DDATA 送信前に必ず実行してください。

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

#### ステップ 4: デコード結果の確認

MQTTX でトピック `decoded/sparkplug/data` をサブスクライブしていると、以下のようなメッセージを受信します。

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
- `spb_decode` により以下が自動復元されました。
  - `"Device/Temperature"`
  - `"Device/Pressure"`
- 下流のサブスクライバーは Sparkplug B の状態管理やエイリアス解釈を行う必要がありません。

## `spb_decode` と `spb_encode` の使用例

このセクションでは、`spb_decode` と `spb_encode` 関数を用いた Sparkplug B メッセージ処理の実践例を示します。ここに示す例は可能な処理の一部に過ぎません。

以下のような構造の Sparkplug B エンコードメッセージを受け取るシナリオを想定します。

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

デバイスからトピック `my/sparkplug/topic` でメッセージを受け取り、`counter_group1/counter1_run` メトリクスのみを JSON 形式でトピック `interesting_counters/counter1_run_updates` に転送したい場合の手順です。EMQX ダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/) クライアントツールで動作確認します。

#### ダッシュボードでルール作成

1. EMQX ダッシュボードにアクセスし、左メニューの **Integration** -> **Rules** をクリック。**+ Create** をクリックしてルール作成画面へ。

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

   ここでは `jq` 関数を使い、メトリクス配列を走査して名前が "`counter_group1/counter1_run`" のものだけを抽出しています。

   ::: tip

   Sparkplug B 仕様では、値が変化した時のみデータを送信することが推奨されているため、ペイロードにはメトリクスの一部しか含まれない場合があります。指定した名前のメトリクスが存在しない場合、このルールは何も出力しません。

   :::

3. 画面右側の **+ Add Action** をクリックし、アクションタイプから `Republish` を選択。  
   トピックに `interesting_counters/counter1_run_updates` を入力し、ペイロードには `${item}` を設定。**Add** をクリック。

4. ルール作成画面に戻り、**Create** をクリック。ルール一覧に作成したルールが表示されます。

#### ルールのテスト

MQTTX クライアントツールを使って、Sparkplug B メッセージをトピック `my/sparkplug/topic` にパブリッシュし、メッセージが変換されてトピック `interesting_counters/counter1_run_updates` に JSON 形式で転送されることを確認します。

1. MQTTX クライアントを起動し、EMQX ブローカーに接続します。MQTTX の詳細は [MQTTX クライアント](../../get-started/messaging/publish-and-subscribe.md) を参照してください。

2. 新規サブスクリプションを作成し、トピック `interesting_counters/counter1_run_updates` をサブスクライブ。

3. 画面右下のメッセージ送信エリアで、トピックに `my/sparkplug/topic` を入力。ペイロードタイプは `Base64` を選択。

4. 以下の Base64 エンコード済み Sparkplug B メッセージをペイロード欄に貼り付け。これは前述の Sparkplug メッセージ例をエンコードしたものです。

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

5. 送信ボタンをクリック。

正常に動作していれば、以下のような JSON 形式のメッセージが受信されます。

```json
{
    "timestamp":1678094561525,
    "name":"counter_group1/counter1_run",
    "int_value":1,
    "datatype":5
}
```

### データ更新

誤ったメトリクス `counter_group1/counter1_run` を発見し、転送前に Sparkplug B エンコードペイロードから削除したい場合の例です。

[データ抽出](#データ抽出) の例と同様に、EMQX ダッシュボードで以下のルールを作成し、リパブリッシュアクションを追加します。

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

このルールでは、`spb_decode` でメッセージをデコードし、`jq` で指定した名前のメトリクスを除外しています。その後、`DO` 節で `spb_encode` を使って再エンコードしています。

リパブリッシュアクションでは、更新済みの Sparkplug B エンコードメッセージとして `${updated_payload}` をペイロードに指定してください。

同様に、メトリクスの値を更新することも可能です。例えば、`counter_group1/counter1_run` の値を 0 に更新したい場合は以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 更新対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_update |
   # 指定した名前のメトリクスの値を更新
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

また、新しいメトリクス `counter_group1/counter1_new` を値 42 で追加したい場合は以下のルールを使います。

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
   # ペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

### メッセージのフィルタリング

メトリクス `counter_group1/counter1_run` の値が 0 より大きいメッセージのみを転送したい場合、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # 値が 0 以下の場合は空にしてメッセージを除外
   if $value > 0 then $payload else empty end
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

このルールでは、`jq` 関数が指定メトリクスの値が 0 以下の場合に空の配列を出力します。結果として、値が 0 以下のメッセージはルールに接続されたアクションへは転送されません。

### メッセージの分割

Sparkplug B エンコードメッセージを複数のメッセージに分割し、メトリクス配列の各メトリクスを個別の Sparkplug B エンコードメッセージとしてリパブリッシュしたい場合、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとにメッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスのみを含むメトリクス配列に置き換え
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

このルールでは、`jq` 関数が複数のアイテムを含む配列を出力します。ルールに接続されたすべてのアクションは配列の各アイテムごとにトリガーされます。リパブリッシュアクションのペイロードには `${output_payload}` を設定してください。これは `DO` 節で Sparkplug B エンコードメッセージに割り当てた名前です。

### メッセージ分割とコンテンツに基づくトピック振り分け

Sparkplug B エンコードメッセージを分割しつつ、例えばメトリクス名に基づいて異なるトピックに送信したい場合の例です。出力トピック名は `"my_metrics/"` とメトリクス名を連結して構築するとします。以下のようにコードを少し変更します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとにメッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスのみを含むメトリクス配列に置き換え
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO
spb_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

リパブリッシュアクションの設定では、トピック名に `${output_topic}` を指定します。これは `DO` 節で出力トピック名として割り当てたものです。ペイロードには `${output_payload}` を設定してください。

`jq` 関数の呼び出しは `DO` 節内で `first` 関数にラップされており、最初の（唯一の）出力オブジェクトを取得しています。
