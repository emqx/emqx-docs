# MQTTデータをDatalayersに取り込む

Datalayersは、産業用IoT、IoV、エネルギーなどの分野向けに設計されたマルチモーダルでハイパーコンバージドなデータベースです。高いデータスループットと安定したパフォーマンスを備えており、IoTアプリケーションに最適です。EMQXは現在、Sinkを通じてDatalayersにメッセージやデータを保存することをサポートしており、データ分析や可視化を容易にしています。

本ページでは、EMQXとDatalayersのデータ統合の詳細な概要を説明し、ルールとSinkの作成方法について実践的なガイドを提供します。

## 動作の仕組み

Datalayersのデータ統合はEMQXの標準機能であり、EMQXのデバイス接続およびメッセージ送信機能とDatalayersのデータ保存・分析機能を組み合わせています。簡単な設定でシームレスなMQTTデータ統合が実現可能です。EMQXはルールエンジンとSinkを利用してデバイスデータをDatalayersに転送し、保存および分析を行います。Datalayersは分析結果をレポートやチャートなどの形で生成し、Datalayersの可視化ツールを通じてユーザーに表示します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとDatalayersのデータ統合の典型的なアーキテクチャを示しています。

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

EMQXとDatalayersは、リアルタイムでエネルギー消費データを効率的に収集・分析するためのスケーラブルなIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがデバイス接続、メッセージ送信、データルーティングを担当するIoTプラットフォームとして機能し、Datalayersがデータ保存および分析プラットフォームとして機能します。具体的なワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：MQTTプロトコルで正常に接続された後、エネルギー貯蔵デバイスは電力、入力、出力に関するエネルギー消費データを定期的にパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジンでマッチングを行います。
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、トピックマッチングに基づいて特定のソースからのメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。例えば、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などが行われます。
3. **Datalayersへの書き込み**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをDatalayersに書き込むアクションを実行します。Datalayers SinkはSQLテンプレートを提供しており、書き込むデータ形式を柔軟に定義できます。これにより、メッセージの特定フィールドをDatalayersの対応するテーブルやカラムに保存できます。

エネルギー貯蔵データがDatalayersに書き込まれた後は、[line protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を活用して柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続してチャートを生成し、エネルギー貯蔵データを表示する。
- 業務システムに接続してエネルギー貯蔵デバイスの状態監視やアラートを行う。

## 特徴と利点

Datalayersのデータ統合は以下の特徴と利点を提供します。

- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを処理でき、Datalayersはデータの書き込み、保存、クエリに優れており、IoTシナリオのデータ処理要件をシステムに負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXのルール内で広範囲に処理・変換されてからDatalayersに書き込まれます。
- **スケーラビリティ**：EMQXとDatalayersはどちらもクラスタリング機能を備えており、ビジネスの成長に応じて柔軟な水平スケーリングが可能です。
- **豊富なクエリ機能**：Datalayersはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから価値ある洞察を抽出します。
- **効率的なストレージ**：Datalayersは高圧縮エンコーディング方式を採用し、ストレージコストを大幅に削減します。また、不要なデータがストレージを占有しないようにカスタマイズ可能なデータ保持期間を設定できます。

## はじめる前に

このセクションでは、EMQXでDatalayers Sinkを作成する前に必要な準備、Datalayersのインストールおよびセットアップについて説明します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解
- Datalayers Sinkでのデータ書き込みに使用される[Datalayers Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)の理解

### Datalayersのインストールとセットアップ

1. Dockerを使用してDatalayersをインストールし起動します。詳細な手順は[Install Datalayers](https://docs.datalayers.cn/datalayers/latest/getting-started/docker.html)を参照してください。

   ```bash
   # Datalayersコンテナを起動
   docker run -d --name datalayers -p 8360:8360 -p 8361:8361 datalayers/datalayers:latest
   ```

2. Datalayersサービス起動後、デフォルトのユーザー名とパスワード `admin`/`public` でDatalayers CLIに入ります。Datalayers CLIでデータベースを作成する手順は以下の通りです。

   - Datalayersコンテナにアクセス：

     ```bash
     docker exec -it datalayers bash
     ```

   - Datalayers CLIに入る：

     ```bash
     dlsql -u admin -p public
     ```

   - データベースを作成：

     ```sql
     create database mqtt
     ```

## コネクターの作成

このセクションでは、SinkをDatalayersサーバーに接続するためのコネクター作成方法を示します。

以下の手順はEMQXとDatalayersがローカルで動作していることを前提としています。リモートで動作している場合は設定を適宜調整してください。

1. EMQXダッシュボードで、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Datalayers** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下を設定します。
   - コネクター名を入力します。大文字・小文字の英数字の組み合わせで例：`my_datalayers`
   - Datalayersサーバーの接続情報を入力します。
     - サーバーアドレス：`127.0.0.1:8361`
     - [Datalayersのインストールとセットアップ](#datalayersのインストールとセットアップ)で設定した**Username**、**Password**、**Database**を入力
   - TLSを有効にするか設定します。TLS接続オプションの詳細は[外部リソースアクセスのTLS暗号化有効化](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。
5. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがDatalayersサーバーに接続できるかテストできます。
6. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログでは**Back to Connector List**をクリックするか、**Create Rule**をクリックしてルールおよびSinkの作成に進むことができます。詳細は[Create Datalayers Sink Rules](#create-a-rule-with-datalayers-sink)を参照してください。

## Datalayers Sinkを使ったルールの作成

このセクションでは、EMQXでソースMQTTトピック `t/#` のメッセージを処理し、処理結果を設定済みのDatalayers Sinkを通じてDatalayersに送信するルールの作成方法を示します。

1. ダッシュボードの左メニューから **Data Integration** -> **Rules** をクリックします。

2. ルールページ右上の **Create** ボタンをクリックします。

3. ルールIDに `my_rule` を入力します。

4. SQLエディタに、`t/#` トピックのMQTTメッセージをDatalayersに保存するルールを入力します。例として以下のSQL文を使用します。

   ::: tip 補足

   独自のSQLルールを指定する場合は、ルールで選択するフィールド（SELECT部分）がSinkで指定したデータ書き込みフォーマットに含まれるすべての変数を含んでいることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   SQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてルールSQLの学習やテストが可能です。

   :::

5. ルールがトリガーされた際のアクションを指定するため、右側の **Add Action** ボタンをクリックします。これにより、EMQXはルールで処理したデータをDatalayersに転送します。

6. **Action** のドロップダウンリストから `Datalayers` を選択し、**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既存のDatalayers Sinkを選択することも可能です。本デモでは新しいSinkを作成します。

7. Sinkの名前を入力します。名前は大文字・小文字の英数字の組み合わせにしてください。

8. **Connector** のドロップダウンリストから先に作成した `my_datalayers` を選択します。隣のボタンをクリックして新しいコネクターを作成することも可能です。設定パラメーターは[コネクターの作成](#コネクターの作成)を参照してください。

9. **Time Precision** はデフォルトでミリ秒に設定します。

10. データ解析を定義し、Datalayersに解析・書き込みする**Data Format**と内容を指定します。`JSON` と `InfluxDB Line Protocol` のフォーマットがサポートされています。

    - JSONフォーマットの場合、**Measurement**、**Timestamp**、**Fields**、**Tags**を含むデータ解析方法を定義します。すべてのキー値は変数またはプレースホルダーにできます。また[InfluxDB line protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)に従って設定可能です。**Fields**はCSVファイルによる一括設定もサポートしています。詳細は[バッチ設定](#batch-settings)を参照してください。

    - Line Protocolフォーマットの場合、データポイントのテーブル、フィールド、タイムスタンプ、タグをステートメントで指定します。キーと値は定数またはプレースホルダー変数をサポートし、[InfluxDB line protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)に従って設定可能です。

      ::: tip

      Datalayersに書き込むデータはInfluxDB v1のline protocolと完全互換のため、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)を参照してデータフォーマットを設定できます。

      例えば、符号付き整数値を入力する場合は、プレースホルダーの後に型指定子として `i` を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8で整数値を書き込む方法](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。

      :::

      ここではLine Protocolフォーマットを使用し、以下のように設定できます。

      ```sql
      devices,clientid=${clientid} temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
      ```

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **Advanced Settings** を展開し、必要に応じて高度なオプションを設定します（任意）。詳細は[高度な設定](#advanced-settings)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがDatalayersサーバーに接続できるかテストできます。

14. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

15. **Create Rule**ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。

これでルールの作成が完了しました。**Rules**ページで新しいルールを確認できます。**Actions (Sink)**タブをクリックすると新しいDatalayers Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認できます。`t/#`トピックのメッセージが`my_rule`という名前のルールで処理され、結果がDatalayersに保存されていることがわかります。

### バッチ設定

Datalayersでは、1つのデータエントリに数百のフィールドが含まれることが多く、データフォーマットの設定が複雑になる場合があります。この問題を解決するため、EMQXはバッチフィールド設定機能を提供しています。

JSONフォーマットでデータ形式を設定する際、CSVファイルからフィールドのキー・バリューを一括インポートできます。

1. **Fields**テーブルの**Batch Settings**ボタンをクリックし、**Import Batch Settings**ポップアップを開きます。

2. 指示に従ってバッチ設定テンプレートファイルをダウンロードし、テンプレートにフィールドのキー・バリューを記入します。デフォルトのテンプレート内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値の後に `i` を付けると、Datalayersは整数型として保存します。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、line protocolに従い型指定子を付けることも可能。
   - **備考**：CSVファイル内のフィールドに関するコメント用で、EMQXへのインポートには含まれません。

   バッチ設定CSVファイルは2048行を超えないようにしてください。

3. 記入したテンプレートファイルを保存し、**Import Batch Settings**ポップアップにアップロード後、**Import**をクリックしてバッチ設定を完了します。

4. インポート後、**Fields**設定テーブル内でフィールドのキー・バリューをさらに調整できます。

## ルールとSinkのテスト

MQTTXを使って `t/1` トピックにメッセージをパブリッシュします。この操作はオンライン・オフラインイベントもトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "temp": "23.5", "hum": "62", "precip": 2}'
```

両方のSinkの稼働統計を確認してください。ヒット数と送信成功数がそれぞれ1ずつ増加しているはずです。

Datalayers CLIに入り、以下のコマンドを実行してデータが正常にデータベースに書き込まれているか確認します。

1. Datalayersコンソールに入る：

   ```bash
   docker exec -it datalayers bash
   dlsql -u admin -p public
   ```

2. データを確認するSQLクエリを実行：

   ```sql
   use mqtt;
   select * from devices;
   ```

## 高度な設定

このセクションでは、DatalayersコネクターおよびSinkの高度な設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**を展開して、ニーズに応じて以下のパラメーターを調整できます。

| フィールド名               | 説明                                                                                                                         | デフォルト値    |
| -------------------------- | ---------------------------------------------------------------------------------------------------------------------------- | -------------- |
| Startup Timeout            | コネクターが自動起動したリソースの正常状態を待機する最大時間（秒）を指定します。この設定により、Datalayersのデータベースインスタンスなどの接続先リソースが完全に稼働し、データ処理可能になるまでコネクターが操作を進めないようにします。 | `5`            |
| Buffer Pool Size           | バッファワーカープロセスの数を指定します。これらのプロセスはEMQXとDatalayersのEgressタイプSink間のデータフローを管理し、データを一時的に保存・処理してから送信します。パフォーマンス最適化とスムーズなデータ伝送に重要です。Ingressのみを扱うブリッジでは`0`に設定可能です。 | `4`            |
| Request Timeout            | 「Request TTL」（有効期限）設定は、リクエストがバッファに入ってから有効とみなされる最大時間（秒）を指定します。このタイマーはリクエストがバッファに入った時点で開始し、TTLを超えるかDatalayersからの応答・アックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`           |
| Health Check Interval      | SinkがDatalayersとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。                                           | `15`           |
| Max Buffer Queue Size      | Datalayers Sinkの各バッファワーカープロセスがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、効率的なデータストリーム処理を行います。システム性能やデータ伝送要件に応じて調整してください。 | `1`            |
| Max Batch Request Size    | EMQXからDatalayersへ一度に転送するデータバッチの最大サイズを指定します。このサイズを調整することでデータ転送の効率やパフォーマンスを最適化できます。<br />`1`に設定すると、データレコードはバッチ化されず個別に送信されます。 | `100`          |
| Request Mode              | メッセージ送信を最適化するために、`synchronous`（同期）または`asynchronous`（非同期）のリクエストモードを選択できます。非同期モードではDatalayersへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがDatalayersに到達する前にメッセージを受信する可能性があります。 | `Asynchronous` |
| Inflight Queue Window     | 「Inflight queue requests」とは、送信済みだがまだ応答やアックを受け取っていないリクエストのことです。この設定はSinkとDatalayers間の通信で同時に存在可能なインフライトリクエストの最大数を制御します。<br/>**Request Mode**が`asynchronous`の場合、このパラメーターは特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理する必要がある場合は、値を`1`に設定してください。 | `100`          |
