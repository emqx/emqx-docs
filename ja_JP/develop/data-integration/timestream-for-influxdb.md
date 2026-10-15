# AWS Timestream for InfluxDB への MQTT データ取り込み

[AWS Timestream for InfluxDB](https://docs.aws.amazon.com/timestream/latest/developerguide/timestream-for-influxdb.html) は、InfluxDB 2.x ワークロードを AWS 上で実行できるフルマネージドの時系列データベースサービスであり、データ取り込みの簡素化とリアルタイム分析を可能にします。EMQX 6.1 以降、EMQX は既存の InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterprise のサポートに加え、Amazon Timestream for InfluxDB とのネイティブ統合を提供しています。

本ページでは、EMQX と Amazon Timestream for InfluxDB 間のデータ統合について包括的に解説し、設定およびデータフローの検証手順を実践的に説明します。

## 動作概要

Amazon Timestream for InfluxDB 統合は、EMQX のリアルタイムデータ処理およびルーティング機能を基盤とし、Timestream のフルマネージドかつ高性能な InfluxDB エンジンと組み合わせています。

組み込みの [ルールエンジン](./rules.md) と Timestream for InfluxDB Sink を通じて、EMQX は MQTT メッセージを変換し、カスタムアプリケーションコードを必要とせずに直接 Timestream for InfluxDB DB インスタンスへ書き込みます。

以下の図は、エネルギー貯蔵シナリオにおける EMQX と Amazon Timestream for InfluxDB 間の典型的なデータ統合アーキテクチャを示しています。

![timestream_for_influxdb](./assets/timestream_for_influxdb.png)

この統合は、リアルタイムのエネルギーモニタリングおよび分析のためのスケーラブルな IoT データパイプラインを提供します。EMQX はデバイス接続とデータルーティングを担う IoT メッセージングレイヤーとして機能し、Timestream for InfluxDB はマネージドな時系列ストレージとクエリ機能を提供します。ワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：デバイスは MQTT 経由で EMQX に接続し、テレメトリ（例：電力使用量、充放電メトリクス）をパブリッシュします。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージ処理**：ルールエンジンはトピックをマッチングし、フィルタリング、フィールド抽出、データエンリッチメントなどの変換を適用し、ターゲットの Timestream for InfluxDB バケットへの取り込み用にペイロードを整形します。
3. **Timestream へのデータ取り込み**：ルールが Amazon Timestream Sink をトリガーすると、EMQX は InfluxDB ラインプロトコルを用いてデータを書き込みます。テンプレートは MQTT フィールドの測定値、タグ、フィールドへのマッピングを定義します。

Timestream for InfluxDB に保存後は、Flux/InfluxQL クエリ、InfluxUI、Grafana などのツールを用いて電力メトリクスを可視化したり、監視やアラートのために業務システムと連携できます。

## 特長と利点

Amazon Timestream for InfluxDB 統合は以下の特長と利点を提供します：

- **効率的なデータ処理**：EMQX は大規模な IoT 接続と高スループットの MQTT データを処理し、Timestream for InfluxDB は高速な取り込みとミリ秒単位のクエリ性能でリアルタイム分析を実現します。
- **メッセージ変換**：EMQX のルールは MQTT メッセージの柔軟なフィルタリング、抽出、変換を提供し、構造化された JSON マッピングまたはカスタム InfluxDB ラインプロトコルテンプレートとしてフォーマットしてから Timestream に書き込みます。
- **マネージドスケーラビリティ**：EMQX は大規模 IoT 展開向けの水平クラスタリングをサポートし、Timestream for InfluxDB はマネージドインスタンスのスケーリング、自動バックアップ、シームレスなバージョンアップデートを提供します。
- **豊富なクエリ機能**：Timestream for InfluxDB は Flux や InfluxQL を含む InfluxDB 2.x のクエリエコシステムを完全にサポートし、強力な時系列分析と下流ツールとの統合を可能にします。
- **最適化されたストレージ**：Timestream for InfluxDB は AWS 管理のストレージを使用し、事前設定された IOPS とスループット階層で時系列データワークロードに対し効率的かつコスト最適化されたパフォーマンスを提供します。

## はじめる前に

このセクションでは、データ統合作成前に必要な準備として、Amazon Timestream for InfluxDB 環境のセットアップおよび接続パラメータの取得方法を説明します。

### 前提条件

統合設定前に以下を確認してください：

- EMQX が Timestream for InfluxDB への書き込みに使用する [InfluxDB ラインプロトコル](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/) の理解。
- EMQX のデータ統合における [ルール](./rules.md) とルールエンジンによる MQTT メッセージの変換およびルーティングの理解。
- EMQX の [データ統合](./data-bridges.md) の基本知識、特に Sink の設定とトリガー方法。

### Amazon Timestream for InfluxDB の準備

EMQX から Timestream for InfluxDB インスタンスへデータ送信を可能にするため、AWS 上で以下の準備を行います。

::: tip 前提条件

Timestream for InfluxDB リソースの作成および管理権限を持つ AWS アカウントを用意してください。

:::

#### Timestream for InfluxDB DB インスタンスの作成

1. AWS マネジメントコンソールにサインインし、[Amazon Timestream for InfluxDB コンソール](https://console.aws.amazon.com/timestream/) を開きます。

2. 右上のリージョン選択で、DB インスタンスを作成したい AWS リージョンを選択します。

3. ナビゲーションペインで **InfluxDB Databases** を選択します。

4. **Create InfluxDB database** をクリックします。

5. **Engine settings** で、デプロイする InfluxDB エンジンのバージョンを選択します。

   ::: tip 注意

   エンジンバージョンは後述の EMQX コネクターで必要な認証情報の取得方法に影響します。ワークロードと統合要件に合ったバージョンを選択してください。

   :::

   <img src="./assets/timestream_engine_settings.png" alt="timestream_engine_settings" style="zoom:67%;" />

6. 残りの設定（デプロイ設定、ストレージオプション、ネットワーキング、ログ設定など）を要件に応じて完了します。各オプションの詳細は以下を参照してください：[Create an InfluxDB DB Instance](https://docs.aws.amazon.com/timestream/latest/developerguide/timestream-for-influx-getting-started-creating-db-instance.html#timestream-for-influx-getting-started-creating-db-instance-step2)。

7. データベース作成後、インスタンス詳細ページを開き、AWS が生成したエンドポイント（例：`c5vasdqn0b-3ksj4dla5nfjhi.timestream-influxdb.us-east-1.on.aws`）を取得します。このエンドポイントは EMQX コネクター設定時に必要です。

#### ネットワークおよびセキュリティグループの設定

EMQX が Timestream for InfluxDB インスタンスに接続できるよう、インスタンスの VPC セキュリティグループで TCP ポート 8086 への着信接続を許可してください。設定例は以下の通りです：

- **プロトコル**：TCP
- **ポート**：8086（Timestream for InfluxDB が使用する InfluxDB API ポート）
- **送信元**：EMQX が展開されている環境の IP アドレス範囲またはセキュリティグループ

EMQX が Timestream for InfluxDB と同じ VPC 内にある場合は、VPC 内のプライベートネットワーク経路を通じて接続可能です。EMQX が AWS 外部にある場合は、セキュリティグループが EMQX の外部ネットワークからの接続を許可していることを確認してください。また、EMQX から Timestream エンドポイントへの HTTPS/TCP 8086 トラフィックをブロックするアウトバウンドファイアウォールルールがないかもご確認ください。

接続要件やセキュリティ上の注意点の詳細は AWS ドキュメントを参照してください：[Connecting to an Amazon Timestream for InfluxDB DB instance](https://docs.aws.amazon.com/timestream/latest/developerguide/timestream-for-influx-db-connecting.html)。

#### InfluxDB トークン、Organization、Bucket の取得

認証トークンおよび資格情報の取得方法は、Timestream for InfluxDB インスタンス作成時に選択した **InfluxDB エンジンバージョン** に依存します。

##### InfluxDB v2 DB インスタンスの Influx UI へのアクセス

1. DB インスタンスのエンドポイントを使い、**Influx UI** にアクセスします：

   ```
   https://<endpoint>:8086
   ```

   > DB インスタンスがパブリックアクセス不可の場合は、同一 VPC 内のホスト（バスチオンホストや SSM ポートフォワーディング経由など）からアクセスしてください。詳細は [AWS ドキュメント](https://docs.aws.amazon.com/timestream/latest/developerguide/timestream-for-influx-getting-started-creating-db-instance.html) を参照。

2. DB インスタンス作成時に設定したマスターユーザー資格情報でログインします。

3. 対象バケットへの書き込み権限を持つパーソナルアクセストークンを生成または取得します。

   これが EMQX が Timestream for InfluxDB へ認証するために使用するトークンです。

   ::: tip 注意

   新規作成したトークンは一度しか表示されません。必ずコピーして保存してください。

   :::

4. インスタンスで設定された **Organization** と **Bucket** の値を確認してください。これらは EMQX 設定時に正確に一致させる必要があります。

詳細な手順は AWS 公式ドキュメントを参照してください：[Access the InfluxDB UI](https://docs.aws.amazon.com/timestream/latest/developerguide/timestream-for-influx-getting-started-creating-db-instance.html#timestream-for-influx-getting-started-creating-db-instance-step-3)。

##### **InfluxDB v3** DB インスタンスの認証トークン取得

InfluxDB v3 は InfluxDB UI から API トークンを発行しません。代わりに、DB インスタンス作成時に AWS が認証パラメータ（API トークンを含む）を **AWS Secrets Manager** に保存します。

1. Timestream コンソールの DB クラスター詳細ページを開き、**Authentication properties Secret manager ARN** というフィールドを探します。

   ![timestream_secret_arn](./assets/timestream_secret_arn.png)

   この ARN は EMQX が使用する資格情報を含む Secrets Manager エントリを指します。

2. **AWS Secrets Manager** の **Secrets** で該当するシークレット名（例：`READONLY-InfluxDB-auth-parameters-<cluster-id>`）を検索します。

3. シークレットを開き、**Plaintext** 表示に切り替えてシークレット内容を取得します。

   ![timestream_secret_value](./assets/timestream_secret_value.png)

### 必要な接続パラメータ

EMQX で Amazon Timestream for InfluxDB コネクターを設定する際は、Timestream インスタンスで使用している InfluxDB エンジンバージョンに応じて以下のパラメータを指定してください：

| パラメータ           | 説明                                                         |
| -------------------- | ------------------------------------------------------------ |
| **Endpoint**         | InfluxDB インスタンスの AWS 生成エンドポイント例：`xxxxxxx-yyyyyyyy.timestream-influxdb.<region>.on.aws` |
| **Port**             | 常に **8086**。InfluxDB API エンドポイントのポート番号。     |
| **Database Name**    | （**InfluxDB v3**）v3 DB インスタンス作成時に指定したデータベース名。 |
| **Organization**     | （**InfluxDB v2**）InfluxDB UI で設定された Organization 名。 |
| **Bucket**           | （**InfluxDB v2**）EMQX がテレメトリデータを書き込む Bucket。 |
| **Token**            | EMQX が使用する認証トークン：<br />**InfluxDB v2:** InfluxDB UI で作成したパーソナルアクセストークン<br />**InfluxDB v3:** AWS Secrets Manager から取得したトークン（`token` フィールド） |

## コネクターの作成

このセクションでは、Sink を AWS Timestream for InfluxDB DB インスタンスに接続するコネクターの作成方法を説明します。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、**Data Persistence** タイプから **Amazon Timestream** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の項目を設定します：
   - **Connector Name**：英数字で始まり、英数字、ハイフン、アンダースコアが使用可能な名前を入力します。例：`my_timestream`。
   - **Server Host**：Timestream for InfluxDB インスタンスのエンドポイントとポートを入力します。例：`<instance-endpoint>:8086`。
   - **Version of InfluxDB**：Timestream for InfluxDB インスタンスの設定に合うバージョンを選択します。
     - `v2`（デフォルト）：[InfluxDB トークン、Organization、Bucket の取得](#obtain-influxdb-token-organization-and-bucket) で取得したパーソナルアクセストークン、Organization 名、Bucket 名を設定します。これらは InfluxDB 設定と完全に一致させる必要があります。
     - `v3`：DB インスタンス作成時に指定したデータベース名と、[InfluxDB v3 DB インスタンスのシークレット値取得](#retrieve-authentication-token-for-influxdb-v3-db-instances) で取得したシークレット値を入力します。
   - **TLS**（任意）：Timestream for InfluxDB エンドポイントが HTTPS を要求する場合は TLS を有効にします（推奨）。TLS 接続オプションの詳細は [TLS for External Resource Access](../../guides/network/overview.md#enabling-tls-for-external-resource-access) を参照してください。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Timestream InfluxDB DB インスタンスに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** を選択できます。ルールと Sink を作成して Timestream for InfluxDB へ転送するデータを指定する手順は [Create a Rule with Amazon Timestream Sink](#create-a-rule-with-amazon-timestream-sink) を参照してください。

## Amazon Timestream Sink を用いたルールの作成

このセクションでは、EMQX でソース MQTT トピック `t/#` からメッセージを処理し、設定済みの Sink を通じて AWS Timestream for InfluxDB に送信するルールの作成方法を説明します。

### ルール SQL の定義

1. EMQX ダッシュボードの左ナビゲーションメニューから **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Rule** ページで、ルール ID に `my_rule` を入力します。

4. **SQL Editor** にて、トピック `t/#` 以下のすべてのメッセージを転送するために以下の SQL 文を設定します。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   カスタム SQL を記述する場合、`SELECT` 句のフィールドは後で Sink のデータフォーマットで参照する変数をすべて含むようにしてください。

   :::

   > 初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストを行ってください。

### ルールにアクション（Sink）を追加

ルール SQL 定義後、Amazon Timestream Sink をトリガーするアクションを作成します。このアクションにより、ルールで処理されたデータが Timestream for InfluxDB に送信されます。

#### 基本設定の構成

1. **Create Rule** ページで + **Add Action** をクリックし、ルールの出力を定義します。

2. **Type of Action** ドロップダウンから `Amazon Timestream` を選択します。

   **Action** ドロップダウンはデフォルトの `Create Action` のままにします。

   > 既存の Sink を選択することも可能ですが、本例では新規作成します。

3. **Name** と任意で **Description** を入力します。

4. **Connector** ドロップダウンから先に作成した `my_timestream` を選択します。必要に応じて新規コネクターも作成可能です。詳細は [Create a Connector](#create-a-connector) を参照してください。

5. **Time Precision** を指定します（デフォルトは `millisecond`）。

#### データフォーマットの設定

EMQX が Timestream for InfluxDB に書き込む前にデータをシリアライズする方法として、**Data Format** を `JSON` または `Line Protocol` から選択します。

##### JSON フォーマット

構造化された設定フィールドを好む場合は **JSON** フォーマットを使用します。EMQX は定義された構造化フィールドを自動的に InfluxDB ラインプロトコルに変換します。

- **Measurement**：測定名を指定します。例：`sensor_data`。

  プレースホルダーも利用可能です。例：

  - `${topic}`
  - `${payload.measurement}`

- **Timestamp**：（任意）数値またはプレースホルダーのタイムスタンプ。省略時は EMQX のサーバー時刻が使用されます。

  例：

  - `${timestamp}`
  - `${payload.ts}`

- **Fields**：各フィールドはキーと値のペアです。すべての値は変数またはプレースホルダーにでき、[InfluxDB ラインプロトコル](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/) に準じて設定可能です。

  例：

  | キー    | 値                   |
  | ------- | -------------------- |
  | temp    | `${payload.temp}`     |
  | hum     | `${payload.hum}`      |
  | count   | `${payload.count}i`  |

  > **バッチ設定：**
  > 数百フィールドの大規模リストの場合は CSV からのインポートが可能です。詳細は [Batch Setting](#batch-setting) を参照してください。

- **Tags**：タグは常に文字列で、インデックスや高速クエリに使用されます。

  例：

  | キー     | 値               |
  | -------- | ---------------- |
  | device   | `${clientid}`    |
  | region   | `us-east`        |

##### ラインプロトコル

最終的な書き込み構文を完全に制御したい場合はラインプロトコルを使用します。**Write Syntax** ボックスに以下の [InfluxDB ラインプロトコル](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) の構文でテンプレートを入力します：

```
<measurement>[,<tag-key>=<tag-value>...] <field-key>=<field-value>[,<field-key>=<field-value>...] <timestamp>
```

例：

```bash
sensor_data,device=${clientid},region=us-east temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
```

**この例の意味：**

- `sensor_data` は測定名
- `device` と `region` はタグ
- `temp`、`hum`、`precip` はフィールド
- `${timestamp}` はタイムスタンプで、実行時に置換されます

::: tip

- InfluxDB 1.x または 2.x に符号付き整数型値を書き込むには、プレースホルダーの後に `i` を付けます。例：`${payload.int}i`。詳細は [InfluxDB 1.8 整数値の書き込み](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb) を参照してください。
- 符号なし整数型値の場合は `u` を付けます。例：`${payload.int}u`。詳細は同上リンクを参照してください。

:::

##### バッチ設定

InfluxDB ではデータエントリに数百フィールドが含まれることが多く、データフォーマット設定が複雑になる場合があります。これを補うため、EMQX はフィールドのバッチ設定機能を提供しています。

JSON 形式でデータフォーマットを設定する際、CSV ファイルからフィールドのキー・バリューを一括インポート可能です。

1. **Fields** テーブルの **Batch Setting** ボタンをクリックし、**Import Batch Setting** ポップアップを開きます。

2. 指示に従い、まずテンプレートファイルをダウンロードし、フィールドのキー・バリューを入力します。テンプレートのデフォルト内容は以下の通りです：

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値に `i` を付けて整数として InfluxDB に保存する。 |

   - **Field**：フィールドキー。定数または `${var}` 形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、ラインプロトコルに従い型識別子を付加可能。
   - **備考**：CSV 内のメモ用で、EMQX へのインポート対象外。

   CSV ファイルのバッチ設定データは 2048 行を超えないようにしてください。

3. 記入済みテンプレートを保存し、**Import Batch Setting** ポップアップにアップロード後、**Import** をクリックしてバッチ設定を完了します。

4. インポート後、**Fields** 設定テーブルでキー・バリューをさらに調整可能です。

#### アクション作成の完了

1. **Fallback Actions** と **Advanced Settings**（任意）を設定します：
   - **Fallback Actions**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink が処理に失敗した場合にトリガーされます。詳細は [Fallback Actions](./data-bridges.md#fallback-actions) を参照してください。
   - **Advanced settings**：詳細は [Advanced Configurations](#advanced-configurations) を参照してください。
2. **Add Action** ペイン下部の **Test Connectivity** をクリックし、Sink が Timestream for InfluxDB インスタンスに接続できるか確認します。
3. **Create** をクリックしてアクション作成を完了します。保存後、ルールページの **Action Outputs** に Sink が表示されます。

### ルール作成の完了

**Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。

これでルールが正常に作成され、**Rule** ページに新規ルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Amazon Timestream Sink が確認できます。

また、**Integration** -> **Flow Designer** を開くとトポロジーが表示され、トピック `t/#` 以下のメッセージがルール `my_rule` で解析され、Amazon Timestream に送信・保存されていることが確認できます。

## ルールのテスト

統合作成後、EMQX が MQTT メッセージを Timestream for InfluxDB インスタンスに正常に転送しているか検証できます。

### テスト MQTT メッセージのパブリッシュ

[MQTTX](https://mqttx.app/)（または任意の MQTT クライアント）を使い、ルールにマッチするトピック `t/1` にメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "temp": "36.5", "hum": "70", "precip": "12" }'
```

このメッセージはルールをトリガーし、設定済みの Timestream for InfluxDB Sink に送信されます。

### EMQX での Sink 配信状況の確認

EMQX ダッシュボードでルール名をクリックし、ルール詳細ページを開きます。受信メッセージ数が 1、正常に配信されたメッセージ数も 1 であることを確認してください。

### Timestream for InfluxDB でのデータ確認

#### InfluxDB v2 インスタンスの場合

InfluxDB UI を使用します：

1. InfluxDB UI にアクセス：`https://<endpoint>:8086`

2. **Data Explorer** に移動します。

3. EMQX Sink で設定した **Bucket** を選択します。

4. 最近のデータポイントをクエリまたは参照します。

   選択した測定値に以下のフィールドを含む新しいポイントが表示されるはずです。

   - `temp`
   - `hum`
   - `precip`

#### InfluxDB v3 インスタンスの場合

InfluxDB v3 は UI によるデータ閲覧を提供しません。InfluxDB v3 SQL クエリ API を使用して取り込んだデータを検証します。

例：

```bash
curl -G -k "https://<endpoint>:8181/api/v3/query_sql" \
  --header "Authorization: Bearer <your-token>" \
  --data-urlencode "db=<your-database-name>" \
  --data-urlencode "q=SELECT * FROM sensor_data" \
  --data-urlencode "format=jsonl"
```

期待される出力例：

```json
{"temp":36.5,"hum":70,"precip":12,"device":"myclient","region":"us-east", ... }
```

正常なレスポンスは JSONL 形式で挿入されたデータを返します。

詳細なクエリ例は InfluxDB [API ドキュメント](https://docs.influxdata.com/influxdb3/core/api/v3/#tag/Quick-start) を参照してください。

## 高度な設定

本セクションでは、Amazon Timestream コネクターおよび Sink の高度な設定オプションについて詳述します。ダッシュボードでコネクターや Sink を設定する際、**Advanced Settings** にて以下のパラメータを調整し、要件に合わせて最適化できます。

| **項目**               | **説明**                                                                                     | **推奨値** |
| ---------------------- | -------------------------------------------------------------------------------------------- | ---------- |
| Start Timeout          | コネクター起動時にターゲットリソース（例：Timestream for InfluxDB インスタンス）が正常になるまで待機する最大時間（秒）。この時間内に準備できない場合、作成リクエストは失敗します。 | `5`        |
| Buffer Pool Size       | Timestream for InfluxDB へ送信する前にアウトゴーイングデータを処理するバッファワーカープロセス数。高負荷時のスループット改善に寄与します。受信のみのシナリオでは `0` に設定可能。 | `4`        |
| Request TTL            | バッファ内にある書き込みリクエストが有効とみなされる最大時間（秒）。この期間内に送信またはアックされない場合、期限切れとして破棄されます。 | `45`       |
| Health Check Interval  | Sink が Timestream for InfluxDB エンドポイントの接続性と正常性をチェックする間隔（秒）。 | `15`       |
| Max Buffer Queue Size  | バッファワーカーが送信待ちで保持可能な最大データ量（バイト）。データのバーストによる一時的なバックプレッシャーが発生する場合は増加を検討してください。 | `1`        |
| Max Batch Size         | 1 回の書き込みリクエストで送信する最大レコード数。バッチサイズが大きいほどスループットは向上しますが、レイテンシが増加する可能性があります。`1` に設定するとバッチ処理を無効にし、個別送信となります。 | `100`      |
| Query Mode             | 書き込み処理を非同期または同期で実行するかを制御します。`Async` モードでは Timestream への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを受信した時点で Timestream への書き込みが完了していない可能性があります。 | `Async`    |
| Inflight Window        | 同時に処理可能な書き込みリクエストの最大数。**Query Mode** が `Async` の場合の並行処理数を制御します。同一 MQTT クライアントからのメッセージの厳密な順序保証が必要な場合は `1` に設定してください。 | `100`      |
