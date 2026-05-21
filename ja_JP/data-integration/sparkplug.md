# Sparkplug B

<<<<<<< HEAD
[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse Foundation の TAHU プロジェクト](https://www.eclipse.org/tahu/)によって開発されたオープンソースの仕様で、MQTT のための明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用 IoT 分野における相互運用性と一貫性の実現です。

Sparkplug エンコーディングスキームのバージョン B（Sparkplug B）は、監視制御およびデータ収集（SCADA）システム、リアルタイム制御システム、およびデバイス向けの MQTT ネームスペースを定義します。メトリクス、プロセス変数、デバイスの状態情報を含む構造化データ形式を簡潔かつ処理しやすい形でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug B を使用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTT ネットワーク内のデバイス間でシームレスな通信を可能にします。

本ページでは、EMQX における Sparkplug B の実装方法について、データ形式、機能、実践例を交えて解説します。
=======
[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) は、[Eclipse Foundation の TAHU プロジェクト](https://www.eclipse.org/tahu/) によって開発されたオープンソース仕様であり、MQTT における明確に定義されたペイロードおよび状態管理システムを提供することを目的としています。主な目的は、産業用 IoT 分野における相互運用性と一貫性の実現です。

Sparkplug エンコーディングスキームのバージョン B（Sparkplug B）は、監視制御およびデータ収集（SCADA）システム、リアルタイム制御システム、およびデバイス向けの MQTT ネームスペースを定義します。メトリクス、プロセス変数、デバイスの状態情報を含む構造化データ形式を簡潔かつ処理しやすい形でカプセル化することで、標準化されたデータ伝送を保証します。Sparkplug B を使用することで、組織は運用効率を向上させ、データのサイロ化を回避し、MQTT ネットワーク内のデバイス間でシームレスな通信を実現できます。

本ページでは、EMQX における Sparkplug B の実装方法について、データ形式、機能、および実用例を含めて解説します。
>>>>>>> origin/release-5.10

## Sparkplug Bのデータ形式

<<<<<<< HEAD
Sparkplug B は、データ通信の標準化のために明確に定義されたペイロード構造を利用します。その中核には、Sparkplug メッセージの構造化に [Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers) を採用しており、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQX は [スキーマレジストリ](./schema-registry.md) 機能を通じて Sparkplug B を高度にサポートしています。スキーマレジストリを使うことで、Sparkplug B を含む様々なデータ形式のカスタムエンコーダーおよびデコーダーを作成できます。レジストリに[適切な Sparkplug B スキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)を定義することで、EMQX のルールエンジン内で `schema_decode` および `schema_encode` 関数を用いて、指定フォーマットに準拠したデータのアクセスや操作が可能になります。

さらに、EMQX は Sparkplug B に対する組み込みサポートも提供しており、この特定のフォーマットに関してはスキーマレジストリを使用する必要がありません。`spb_encode` および `spb_decode` 関数がルールエンジン内で利用可能で、Sparkplug B メッセージのエンコードおよびデコードを簡素化しています。

:::: tip

以前の `sparkplug_encode` および `sparkplug_decode` 関数は、`bytes_value` の扱いが Sparkplug 仕様と互換性がなかったため非推奨となりました。  
代わりに、更新された `spb_encode` および `spb_decode` 関数をご利用ください。

::::
=======
Sparkplug B は、データ通信の標準化のために明確に定義されたペイロード構造を利用します。コア部分では、[Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers) を用いて Sparkplug メッセージを構造化し、軽量で効率的かつ柔軟なデータ交換を実現しています。

EMQX は [スキーマレジストリ](./schema-registry.md) 機能を通じて Sparkplug B を高度にサポートしています。スキーマレジストリを利用することで、Sparkplug B を含むさまざまなデータ形式のカスタムエンコーダーおよびデコーダーを作成可能です。レジストリに[適切な Sparkplug B スキーマ](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)を定義することで、EMQX のルールエンジン内で `schema_decode` および `schema_encode` 関数を用いて、指定した形式に準拠したデータのアクセスや操作が行えます。

さらに、EMQX は Sparkplug B に対して組み込みサポートを提供しており、この特定の形式に関してはスキーマレジストリを使用する必要がありません。`spb_encode` および `spb_decode` 関数が EMQX に標準搭載されており、ルールエンジン内での Sparkplug B メッセージのエンコードおよびデコードを簡素化します。
>>>>>>> origin/release-5.10

:::: tip

<<<<<<< HEAD
EMQX は Sparkplug B データのエンコードおよびデコード用に、ルールエンジン SQL 関数 `spb_encode` と `spb_decode` を提供しています。  
[実践例](#examples-for-using-spb_decode-and-spb_encode)では、これらの関数を様々なシナリオでどのように使うかを解説しています。

Sparkplug B のエンコード・デコード関数は、ルールエンジンとその `jq` 関数の柔軟性により、多様な処理を実現できます。ルールエンジンおよび `jq` 関数の詳細は以下のページをご参照ください。
=======
以前の `sparkplug_encode` および `sparkplug_decode` 関数は、`bytes_value` の取り扱いが Sparkplug 仕様と互換性がなかったため非推奨となっています。  
代わりに、更新された `spb_encode` および `spb_decode` 関数をご利用ください。

::::

:::: tip

EMQX は Sparkplug B データのエンコードおよびデコード用に、ルールエンジンの SQL 関数として `spb_encode` と `spb_decode` の2つを提供しています。これらの関数の使い方は、[実用例](#practical-examples) セクションでさまざまなシナリオを通じて解説しています。

Sparkplug B のエンコード・デコード関数は、ルールエンジンの柔軟性と `jq` 関数の組み合わせにより、多様な処理に利用可能です。ルールエンジンおよび `jq` 関数の詳細については、以下のページをご参照ください。
>>>>>>> origin/release-5.10

* [ルールの作成](./rule-get-started.md)
* [ルールエンジン SQL 言語](./rule-sql-syntax.md)
* [ルールエンジンの JQ 関数](./rule-sql-jq.md)
* [JQ プログラミング言語の完全な説明](https://stedolan.github.io/jq/manual/)

### spb_decode

<<<<<<< HEAD
`spb_decode` 関数は Sparkplug B メッセージをデコードするために使用します。例えば、Sparkplug B エンコードされたメッセージの内容に基づいて特定のトピックにメッセージを転送したり、メッセージを何らかの形で変更したい場合に利用します。生の Sparkplug B エンコードされたペイロードを、より扱いやすい形式に変換し、さらに処理や解析を行いやすくします。
=======
`spb_decode` 関数は Sparkplug B メッセージをデコードするために使用します。たとえば、Sparkplug B エンコードされたメッセージの内容に基づいて特定のトピックにメッセージを転送したり、メッセージを何らかの形で変更したい場合に利用されます。生の Sparkplug B エンコード済みペイロードを、さらに処理や解析がしやすい形式に変換します。
>>>>>>> origin/release-5.10

使用例:

```sql
select
  spb_decode(payload) as decoded
from t
```

上記の例では、`payload` はデコードしたい生の Sparkplug B メッセージを指します。

<<<<<<< HEAD
[Sparkplug B Protobuf スキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto)はメッセージ構造の理解に役立ちます。

### spb_encode

`spb_encode` 関数はデータを Sparkplug B メッセージにエンコードするために使用します。MQTT クライアントやシステムの他のコンポーネントに Sparkplug B メッセージを送信する際に特に有用です。
=======
[Sparkplug B Protobuf スキーマ](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto) を参照すると、メッセージの構造についてさらに理解が深まります。

### spb_encode

`spb_encode` 関数はデータを Sparkplug B メッセージにエンコードするために使用します。これは、Sparkplug B メッセージを MQTT クライアントやシステムの他のコンポーネントに送信する際に特に有用です。
>>>>>>> origin/release-5.10

使用例:

```sql
select
  spb_encode(json_decode(payload)) as encoded
from t
```

上記の例では、`payload` は Sparkplug B メッセージにエンコードしたいデータを指します。

## Sparkplug B エイリアスマッピング

<<<<<<< HEAD
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

   ```sql
   SELECT
     spb_decode(payload) AS decoded
   FROM "spBv1.0/+/DDATA/+/+"
   ```

   > **補足**:
   >
   > - このルールはすべての Sparkplug B DDATA メッセージにマッチします。
   > - `spb_decode(payload)` はペイロードをデコードし、エイリアスマッピング有効時はエイリアスからメトリクス名を自動復元します。

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

このサブスクライバーは、プレーンな JSON データを期待する非 Sparkplug B クライアントを表します。

#### ステップ 3: MQTTX で Sparkplug B デバイスをシミュレート

以下のペイロードは可読性のため論理的な JSON で示しています。実際のメッセージ送信時は Sparkplug B Protobuf エンコード（Base64）を使用してください。

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
   > - Sparkplug B 仕様により、`datatype` は符号なし整数で定義され、値 `9` は Float データ型を表します。
   > - EMQX はこの時点でエイリアスから名前へのマッピングを記録します。
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

以下のことが確認できます。

- 元の DDATA メッセージには `name` が含まれていませんでした。
- `spb_decode` が自動的に以下を復元しました。
  - `"Device/Temperature"`
  - `"Device/Pressure"`
- 下流のサブスクライバーは Sparkplug B 状態を保持したりエイリアスを解釈したりする必要がありません。

## `spb_decode` と `spb_encode` の使用例

このセクションでは、`spb_decode` と `spb_encode` 関数を使った Sparkplug B メッセージの処理例を紹介します。ここで示す例は可能な処理の一部に過ぎません。

以下のような構造の Sparkplug B エンコード済みメッセージを受け取るシナリオを想定します。
=======
このセクションでは、`spb_decode` および `spb_encode` 関数を用いた Sparkplug B メッセージの処理に関する実用的な例を紹介します。ここで示す例は、可能な操作の一部に過ぎません。

以下のような構造を持つ Sparkplug B エンコード済みメッセージを想定します。
>>>>>>> origin/release-5.10

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
デバイスからトピック `my/sparkplug/topic` でメッセージを受け取り、`counter_group1/counter1_run` メトリクスのみを JSON 形式でトピック `interesting_counters/counter1_run_updates` に転送したい場合の手順を示します。EMQX ダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/) クライアントツールで動作を確認します。
=======
デバイスからトピック `my/sparkplug/topic` にメッセージが届き、その中の `counter_group1/counter1_run` メトリクスだけを抽出して、JSON 形式のメッセージとして別のトピック `interesting_counters/counter1_run_updates` に転送したい場合を考えます。以下の手順は、EMQX ダッシュボードでルールを作成し、[MQTTX](https://mqttx.app/) クライアントツールでルールをテストする方法を示します。
>>>>>>> origin/release-5.10

#### ダッシュボードでのルール作成

<<<<<<< HEAD
1. EMQX ダッシュボードの左メニューから **Integration** -> **Rules** を選択し、**+ Create** をクリックしてルール作成画面へ。

2. **SQL Editor** に以下の SQL 文を入力。
=======
1. EMQX ダッシュボードにアクセスし、左側ナビゲーションメニューの **Integration** -> **Rules** をクリックします。**+ Create** をクリックして **Create Rule** ページに入ります。

2. **SQL Editor** に以下の SQL 文を入力します。
>>>>>>> origin/release-5.10

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
=======
   ここで、`jq` 関数はメトリクス配列を反復処理し、名前が "`counter_group1/counter1_run`" のメトリクスだけを抽出しています。

   ::: tip

   Sparkplug B 仕様では、データは変化したときのみ送信することが推奨されているため、ペイロードに含まれるメトリクスは一部のみの場合があります。指定した名前のメトリクスが配列に存在しない場合、このルールは何も出力しません。

   :::

3. ページ右側の **+ Add Action** をクリックし、**Action** ドロップダウンリストから `Republish` を選択します。リパブリッシュ先のトピックに `interesting_counters/counter1_run_updates` を入力し、**Payload** フィールドに `${item}` を入力してアクションを追加します。

4. **Create Rule** ページに戻り、**Create** をクリックします。ルール一覧に作成したルールが表示されます。

#### ルールのテスト

MQTTX クライアントツールを使って、Sparkplug B メッセージをトピック `my/sparkplug/topic` にパブリッシュし、メッセージが JSON 形式に変換されてトピック `interesting_counters/counter1_run_updates` に転送されることを確認できます。

1. MQTTX クライアントを起動し、EMQX ブローカーに接続します。MQTTX の詳細は [MQTTX クライアント](../messaging/publish-and-subscribe.md) を参照してください。

2. 新規サブスクリプションを作成し、トピック`interesting_counters/counter1_run_updates`をサブスクライブします。

3. 画面右下のメッセージ送信エリアでトピックに `my/sparkplug/topic` を入力し、ペイロードタイプを `Base64` に設定します。

4. 以下の Base64 エンコード済み Sparkplug B メッセージをコピーしてペイロード欄に貼り付けます。これは前述の Sparkplug メッセージ例のエンコード版です。
>>>>>>> origin/release-5.10

   ```
   CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA
   ```

<<<<<<< HEAD
5. 送信ボタンをクリック。

   正常に動作すれば、以下のような JSON 形式のメッセージを受信します。
=======
5. 送信ボタンをクリックします。

   正常に動作すれば、以下のような JSON 形式のメッセージが受信できます。
>>>>>>> origin/release-5.10

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
誤ったメトリクス `counter_group1/counter1_run` を検出し、転送前に Sparkplug B エンコード済みペイロードから削除したい場合を考えます。

[データ抽出](#データ抽出)の例と同様に、EMQX ダッシュボードで以下のルールを作成し、再パブリッシュアクションを設定します。
=======
誤ったメトリクス `counter_group1/counter1_run` を Sparkplug B エンコード済みペイロードから削除してからメッセージを転送したい場合を考えます。

[データ抽出](#データ抽出) の例と同様に、EMQX ダッシュボードで以下のルールを作成し、リパブリッシュアクションを設定します。
>>>>>>> origin/release-5.10

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 削除するメトリクス名を保存
   "counter_group1/counter1_run" as $to_delete |
<<<<<<< HEAD
   # $to_delete 以外のメトリクスを抽出
=======
   # $to_delete と異なる名前のメトリクスだけを抽出
>>>>>>> origin/release-5.10
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # 新しいメトリクスでペイロードを更新
   $payload | .metrics = $updated_metrics
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

<<<<<<< HEAD
このルールでは、`spb_decode` でメッセージをデコードし、`jq` で指定した名前のメトリクスを除外しています。`DO` 節で `spb_encode` を使い再エンコードしています。

再パブリッシュアクションでは、ペイロードに `${updated_payload}` を指定してください。これは更新済みの Sparkplug B エンコード済みメッセージの変数名です。

同様に、メトリクスの値を更新することも可能です。例えば、`counter_group1/counter1_run` の値を 0 に更新したい場合は以下のルールを使用します。
=======
このルールでは、`spb_decode` でメッセージをデコードし、`jq` で名前が `counter_group1/counter1_run` のメトリクスを除外しています。その後、`DO` 節で `spb_encode` を使って再度メッセージをエンコードしています。

リパブリッシュアクションのペイロードには `${updated_payload}` を指定してください。これは更新済みの Sparkplug B エンコードメッセージの変数名です。

同様に、メトリクスの値を更新する場合も `spb_decode` と `spb_encode` を利用可能です。たとえば、`counter_group1/counter1_run` の値を 0 に更新したい場合は、以下のルールを使用します。
>>>>>>> origin/release-5.10

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 更新対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_update |
<<<<<<< HEAD
   # 指定メトリクスの値を更新
=======
   # 名前が $to_update のメトリクスの値を更新
>>>>>>> origin/release-5.10
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
また、新しいメトリクス `counter_group1/counter1_new` を値 42 で追加したい場合は以下のルールを使用します。
=======
また、名前が `counter_group1/counter1_new`、値が 42 の新しいメトリクスを追加したい場合は、以下のルールを使用します。
>>>>>>> origin/release-5.10

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
`counter_group1/counter1_run` メトリクスの値が 0 より大きいメッセージのみを転送したい場合、以下のルールを使用します。
=======
メトリクス `counter_group1/counter1_run` の値が 0 より大きいメッセージのみを転送したい場合は、以下のルールを使用します。
>>>>>>> origin/release-5.10

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # フィルタ対象のメトリクス名を保存
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
<<<<<<< HEAD
   # メトリクス値が 0 以下の場合は空にする
=======
   # $to_filter の値が 0 以下ならメッセージを除外
>>>>>>> origin/release-5.10
   if $value > 0 then $payload else empty end
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS item
FROM "my/sparkplug/topic"
```

<<<<<<< HEAD
このルールでは、`jq` 関数が条件に合わない場合に空配列を出力するため、値が 0 以下のメッセージはルールに接続されたアクションに転送されません。

### メッセージの分割

Sparkplug B エンコード済みメッセージを複数のメッセージに分割し、メトリクス配列の各メトリクスを別々の Sparkplug B エンコード済みメッセージとして再パブリッシュしたい場合、以下のルールを使用します。
=======
上記ルールでは、`jq` 関数が `counter_group1/counter1_run` の値が 0 以下の場合に空配列を出力します。これにより、値が 0 以下のメッセージはルールに接続されたアクションに転送されません。

### メッセージの分割

Sparkplug B エンコード済みメッセージを複数のメッセージに分割し、メトリクス配列内の各メトリクスを個別の Sparkplug B エンコード済みメッセージとしてリパブリッシュしたい場合は、以下のルールで実現可能です。
>>>>>>> origin/release-5.10

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとにメッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスだけを含む配列に置き換え
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO spb_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

<<<<<<< HEAD
このルールでは、`jq` 関数が複数のアイテムを出力し、ルールに接続されたすべてのアクションが各アイテムごとにトリガーされます。再パブリッシュアクションのペイロードには `${output_payload}` を指定してください。これは `DO` 節で割り当てた Sparkplug B エンコード済みメッセージの変数名です。

### メッセージを分割し、内容に応じてトピックに送信

Sparkplug B エンコード済みメッセージを分割しつつ、例えばメトリクス名に基づいて各メッセージを異なるトピックに送信したい場合を考えます。出力トピック名は `"my_metrics/"` とメトリクス名を連結して作成するとします。以下のようにコードを少し修正して実現可能です。
=======
上記ルールでは、`jq` 関数が複数のアイテムを含む配列を出力します（メトリクス配列に複数の要素がある場合）。ルールに接続されたすべてのアクションは、配列内の各アイテムごとにトリガーされます。リパブリッシュアクションのペイロードには `${output_payload}` を指定してください。これは `DO` 節で Sparkplug B エンコード済みメッセージに割り当てた名前です。

### メッセージを分割し、内容に応じてトピックに送信

Sparkplug B エンコード済みメッセージを分割し、さらにメトリクス名に基づいて異なるトピックに送信したい場合を考えます。たとえば、出力トピック名を `"my_metrics/"` とメトリクス名の連結で構成したい場合、以下のように少し修正したコードで実現可能です。
>>>>>>> origin/release-5.10

```sql
FOREACH
jq('
   # ペイロードを保存
   . as $payload |
   # 各メトリクスごとにメッセージを出力
   .metrics[] |
        . as $metric |
        # 現在のメトリクスだけを含む配列に置き換え
        $payload | .metrics = [ $metric ]
   ',
   spb_decode(payload)) AS item
DO
spb_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

<<<<<<< HEAD
再パブリッシュアクションの設定では、トピック名に `${output_topic}` を指定し、ペイロードに `${output_payload}` を設定してください。`${output_topic}` は `DO` 節で出力トピック名として割り当てた変数です。

`jq` 関数呼び出しは `DO` 節内で `first` 関数でラップされ、最初で唯一の出力オブジェクトを取得しています。
=======
リパブリッシュアクションの設定では、トピック名に `${output_topic}` を指定してください。これは `DO` 節で出力トピック名として割り当てた変数名です。ペイロードには `${output_payload}` を指定します。

`jq` 関数の呼び出しは `DO` 節内で `first` 関数でラップされており、最初の1つだけの出力オブジェクトを取得しています。
>>>>>>> origin/release-5.10
