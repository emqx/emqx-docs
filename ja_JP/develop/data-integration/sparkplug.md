# Sparkplug B

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse Foundation の TAHU プロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソース仕様で、MQTT のための明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用IoT分野における相互運用性と一貫性の実現です。

Sparkplug エンコーディングスキームのバージョンB（Sparkplug B）は、監視制御およびデータ取得（SCADA）システム、リアルタイム制御システム、およびデバイス向けの MQTT ネームスペースを定義します。メトリクス、プロセス変数、デバイス状態情報を含む構造化データ形式を簡潔かつ処理しやすい形式でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug B を使用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTT ネットワーク内のデバイス間でシームレスな通信を実現できます。

このページでは、EMQX における Sparkplug B の実装方法について、データ形式、機能、および実用例を含めて解説します。

## Sparkplug B データ形式

Sparkplug B は、データ通信を標準化するために明確に定義されたペイロード構造を利用します。その中核として、Sparkplug メッセージの構造化に [Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers) を用いており、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQX は [スキーマレジストリ](./schema-registry.md) 機能を通じて Sparkplug B を高度にサポートしています。スキーマレジストリを利用することで、Sparkplug B を含む様々なデータ形式のカスタムエンコーダーおよびデコーダーを作成可能です。レジストリに [適切な Sparkplug B スキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto) を定義することで、EMQX のルールエンジン内で `schema_decode` および `schema_encode` 関数を使い、指定された形式に準拠したデータのアクセスや操作が行えます。

さらに、EMQX は Sparkplug B に対して組み込みサポートを提供しており、この特定の形式に関してはスキーマレジストリを使う必要がありません。`spb_encode` と `spb_decode` 関数が EMQX に標準搭載されており、ルールエンジン内での Sparkplug B メッセージのエンコードおよびデコードを簡素化しています。

:::: tip

以前の `sparkplug_encode` および `sparkplug_decode` 関数は、`bytes_value` の扱いが Sparkplug 仕様と互換性がなかったため非推奨となりました。  
代わりに、更新された `spb_encode` および `spb_decode` 関数をご利用ください。

::::

## Sparkplug B 関数

EMQX は Sparkplug B データのエンコードおよびデコード用に、ルールエンジンSQL関数として `spb_encode` と `spb_decode` の2つを提供しています。  
[実用例](#examples-for-using-spb_decode-and-spb_encode)では、これらの関数を様々なシナリオでどのように使うかを解説しています。

Sparkplug B のエンコード・デコード関数は、ルールエンジンとその `jq` 関数の柔軟性により、多様な処理に利用可能です。ルールエンジンと `jq` 関数の詳細は以下のページをご参照ください。

* [ルールの作成](./rule-get-started.md)
* [ルールエンジンSQL言語](./rule-sql-syntax.md)
* [ルールエンジンのJQ関数](./rule-sql-jq.md)
* [JQプログラミング言語の完全な説明](https://stedolan.github.io/jq/manual/)

### spb_decode

`spb_decode` 関数は Sparkplug B メッセージのデコードに使用します。例えば、Sparkplug B エンコードされたメッセージの内容に基づいて特定のトピックに転送したり、メッセージを何らかの形で変更したい場合に利用します。生の Sparkplug B エンコードペイロードを、より扱いやすい形式に変換し、さらなる処理や解析を可能にします。

使用例：

```sql
select
  spb_decode(payload) as decoded
from t
```

上記の例では、`payload` はデコードしたい生の Sparkplug B メッセージを指します。

[Sparkplug B Protobuf スキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto) を参照するとメッセージ構造の詳細が理解できます。

### spb_encode

`spb_encode` 関数はデータを Sparkplug B メッセージにエンコードするために使用します。MQTT クライアントやシステムの他のコンポーネントに Sparkplug B メッセージを送信する際に特に有用です。

使用例：

```sql
select
  spb_encode(json_decode(payload)) as encoded
from t
```

上記の例では、`payload` は Sparkplug B メッセージにエンコードしたいデータを指します。

## Sparkplug B エイリアスマッピング

`alias` は Sparkplug B メトリクスの数値識別子です。デバイスがオンラインになると、NBIRTH または DBIRTH メッセージで各メトリクスの `name` と `alias` を宣言します。以降の NDATA または DDATA メッセージでは、完全なメトリクス名の代わりに `alias` のみを送信でき、メッセージサイズとネットワークオーバーヘッドを削減します。

エイリアスは Sparkplug B セッション内でのみ意味を持つため、受信側はエイリアスのみのデータを解釈するためにエイリアスから名前へのマッピングを必要とします。このマッピングは、対応する NBIRTH または DBIRTH メッセージで宣言されたメトリクス名に各エイリアスを関連付けます。

EMQX は Sparkplug B データをデコードしてルール処理を行い、標準 MQTT クライアントやデータプラットフォームなどの非 Sparkplug B クライアントに結果を転送可能です。これらの下流システムは通常 Sparkplug B セッション状態を保持しないため、エイリアスのみのメトリクスを単独で解決できません。EMQX 6.0.2 以降、EMQX はエイリアスマッピングをサポートしています。現在の MQTT クライアントセッションに対応するマッピングがある場合、`spb_decode` はデコード時にエイリアスのみのメトリクスに欠落しているメトリクス名を追加します。

::: warning 重要なお知らせ

EMQX 6.0.4 以降、EMQX は MQTT クライアントが直接パブリッシュしたメッセージのみエイリアスマッピングを維持します。MQTT ブリッジやその他の内部経路を通じて取り込まれたメッセージはエイリアスマッピングを作成・使用しません。したがって、`spb_decode` はそれらの経路で受信したエイリアスのみの NDATA または DDATA メッセージに対してメトリクス名を復元しません。

:::

### Sparkplug B エイリアスマッピングの動作

エイリアスマッピングが有効な場合、EMQX は以下のように Sparkplug B メッセージを処理します。

1. **NBIRTH / DBIRTH メッセージの処理**

   MQTT クライアントが NBIRTH または DBIRTH メッセージを直接パブリッシュすると、EMQX はペイロード内のメトリクスを調べ、両方のフィールド（`name` と `alias`）を定義しているメトリクスのエイリアスマッピングを記録します。

2. **セッションごとのマッピング管理**

   エイリアスマッピングは MQTT クライアントセッションごとに管理され、Sparkplug B の意味論に従います。

   - ノードレベルメトリクス（NBIRTH / NDATA）とデバイスレベルメトリクス（DBIRTH / DDATA）は別々に追跡されます。
   - 異なるクライアントのマッピングは完全に分離され、互いに干渉しません。

3. **`spb_decode` 出力の強化**

   ルールエンジンが NDATA または DDATA メッセージに対して `spb_decode` を呼び出し、メトリクスに `alias` はあるが `name` がない場合、EMQX は現在の MQTT クライアントセッションで記録されたマッピングを使って対応するメトリクス名を復元します。

   現在のセッションに対応するマッピングがない場合、`spb_decode` はメトリクス名を追加せずにメッセージをデコードします。

4. **セッション終了時のクリーンアップ**

   クライアントが切断されると、そのセッションに関連付けられたエイリアスマッピングは削除されます。EMQX はセッション終了後に Sparkplug B の状態を保持または復元しません。

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
> - エイリアスマッピングは、エイリアスマッピングが有効な状態で MQTT クライアントが直接パブリッシュした NBIRTH / DBIRTH メッセージからのみ作成されます。
> - クライアントがすでにバースメッセージを送信済みの場合、エイリアスマッピングを適用するには再接続して NBIRTH / DBIRTH を再度パブリッシュする必要があります。

### エイリアスマッピングの例

この例では、EMQX ダッシュボードと MQTTX を使って、エイリアスのみの DDATA メッセージをフルメトリクス名を含む JSON データに変換し、非 Sparkplug B クライアントに転送する方法を示します。

#### 目的

- **Sparkplug B デバイス**：DBIRTH で `name + alias` を宣言し、DDATA では `alias` のみをパブリッシュ
- **EMQX**：`spb_decode` を使ってメトリクス名を自動復元
- **下流サブスクライバー**：Sparkplug B の知識なしに標準的な JSON メッセージを受信

#### 前提条件

- EMQX 6.0.2 以降、Sparkplug B エイリアスマッピング有効（`enable_alias_mapping = true`）
- DBIRTH と DDATA メッセージを同じ直接 MQTT クライアント接続でパブリッシュ
- [MQTTX](https://mqttx.app/) の利用

#### ステップ1：EMQX ダッシュボードでルール作成

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

4. **+ Add Action** をクリックし、アクションを追加。
5. アクションタイプに **Republish** を選択。
6. アクション設定：

   - **Topic**: `decoded/sparkplug/data`
   - **Payload**: `${decoded}`

7. **Add** をクリック。
8. **Save** をクリックしてルール作成完了。

   ![sparkplugb_alias_mapping_create_rule](./assets/sparkplugb_alias_mapping_create_rule.png)

#### ステップ2：MQTTX でサブスクライバー準備

1. MQTTX を開き、EMQX ブローカーへの新規接続を作成。
2. トピック `decoded/sparkplug/data` をサブスクライブ。

このサブスクライバーは、プレーンな JSON データを期待する非 Sparkplug B クライアントを表します。

#### ステップ3：MQTTX で Sparkplug B デバイスをシミュレート

以下のペイロードは読みやすさのため論理的に JSON 表示しています。実際のメッセージ送信時は Sparkplug B Protobuf エンコード（Base64）を使用してください。

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
   > - Sparkplug B 仕様では `datatype` は符号なし整数で定義され、値 `9` は Float データ型を表します。
   > - この時点で EMQX はエイリアスから名前へのマッピングを記録します。
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

#### ステップ4：デコード結果の確認

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

- 元の DDATA メッセージには `name` が含まれていませんでした。
- `spb_decode` が自動的に以下を復元しました：
  - `"Device/Temperature"`
  - `"Device/Pressure"`
- 下流のサブスクライバーは Sparkplug B 状態を保持したりエイリアスを解釈したりする必要がありません。

## `spb_decode` と `spb_encode` の使用例

このセクションでは、`spb_decode` と `spb_encode` 関数を使った Sparkplug B メッセージ処理の実用例を紹介します。例は可能な操作の一部に過ぎません。

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

デバイスからトピック `my/sparkplug/topic` でメッセージを受け取り、`counter_group1/counter1_run` メトリクスのみを JSON 形式でトピック `interesting_counters/counter1_run_updates` に転送したい場合の手順です。EMQX ダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/) クライアントでテストします。

#### ダッシュボードでルール作成

1. EMQX ダッシュボードの左ナビゲーションメニューから **Integration** -> **Rules** を開き、**+ Create** をクリックしてルール作成画面へ。
2. **SQL Editor** に以下を入力。

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

   ここで `jq` 関数はメトリクス配列を反復処理し、名前が "`counter_group1/counter1_run`" のものだけを抽出しています。

   ::: tip

   Sparkplug B 仕様ではデータは変化時のみ送信することが推奨されており、ペイロードに含まれるメトリクスは部分的な場合があります。指定した名前のアイテムが配列に存在しない場合、このルールは何も出力しません。

   :::

3. 右側の **+ Add Action** をクリックし、アクションタイプから `Republish` を選択。  
   再パブリッシュトピックに `interesting_counters/counter1_run_updates` を入力し、ペイロードに `${item}` を設定。  
   **Add** をクリック。
4. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成。

#### ルールのテスト

MQTTX クライアントツールを使って Sparkplug B メッセージをトピック `my/sparkplug/topic` にパブリッシュし、メッセージが JSON 形式に変換されてトピック `interesting_counters/counter1_run_updates` に転送されることを確認します。

1. MQTTX クライアントを開き、EMQX ブローカーに接続。詳細は [MQTTX クライアント](../../get-started/messaging/publish-and-subscribe.md) を参照。
2. 新規サブスクリプションを作成し、トピック `interesting_counters/counter1_run_updates` をサブスクライブ。
3. 画面右下のメッセージ送信欄にトピック `my/sparkplug/topic` を入力。ペイロードタイプは `Base64` を選択。
4. 以下の Base64 エンコード済み Sparkplug B メッセージをコピーしてペイロード欄に貼り付け。これは前述の Sparkplug メッセージ例のエンコード版です。

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

5. 送信ボタンをクリック。

   正常に動作していれば、以下のような JSON 形式のメッセージを受信します。

   ```json
   {
       "timestamp":1678094561525,
       "name":"counter_group1/counter1_run",
       "int_value":1,
       "datatype":5
   }
   ```

### データ更新

誤ったメトリクス `counter_group1/counter1_run` を発見し、Sparkplug B エンコードペイロードから削除してからメッセージを転送したい場合の例です。

[データ抽出](#データ抽出)の例と同様に、EMQX ダッシュボードで以下のルールを作成し、再パブリッシュアクションを設定します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 削除対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_delete |
   # $to_delete と異なるメトリクスだけを抽出
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # 新しいメトリクス配列でペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

このルールでは、`spb_decode` でメッセージをデコードし、`jq` で指定したメトリクス名を除外しています。`DO` 節の `spb_encode` で再エンコードしています。

再パブリッシュアクションのペイロードには `${updated_payload}` を指定してください。これは更新後の Sparkplug B エンコードメッセージの名前です。

同様に、メトリクスの値を更新することも可能です。例えば、`counter_group1/counter1_run` の値を 0 に更新したい場合は以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 更新対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_update |
   # 指定したメトリクスの値を更新
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
   # ペイロードを新しいメトリクス配列で更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

### メッセージのフィルタリング

`counter_group1/counter1_run` メトリクスの値が 0 より大きいメッセージのみ転送したい場合、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # 値が 0 以下なら空出力（転送しない）
   if $value > 0 then $payload else empty end
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

このルールでは、`jq` 関数が指定メトリクスの値が 0 以下の場合は空配列を出力し、ルールに接続されたアクションは何もトリガーされません。

### メッセージの分割

Sparkplug B エンコードメッセージを複数のメッセージに分割し、メトリクス配列の各メトリクスを個別の Sparkplug B エンコードメッセージとして再パブリッシュしたい場合、以下のルールを使います。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1メッセージ出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスのみをメトリクス配列に設定
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

このルールでは、`jq` 関数が複数のアイテムを含む配列を出力し、ルールに接続されたすべてのアクションが配列内の各アイテムに対してトリガーされます。  
再パブリッシュアクションのペイロードは `${output_payload}` に設定してください。これは `DO` 節でエンコードした Sparkplug B メッセージの名前です。

### メッセージを分割し、内容に応じてトピックに送信

Sparkplug B エンコードメッセージを分割し、例えばメトリクス名に基づいて各メッセージを異なるトピックに送信したい場合の例です。出力トピック名は `"my_metrics/"` とメトリクス名を連結して構築します。以下のようにコードを少し変更します。

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとに1メッセージ出力
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

再パブリッシュアクションの設定では、トピック名を `${output_topic}` に、ペイロードを `${output_payload}` に設定してください。  
`jq` 関数の呼び出しは `DO` 節内で `first` 関数にラップされており、最初の（かつ唯一の）出力オブジェクトを取得しています。
