# TimescaleDBへのMQTTデータ取り込み

[TimescaleDB](https://www.timescale.com/)（Timescale）は、時系列データの保存と分析に特化したデータベースです。優れたデータスループットと信頼性の高いパフォーマンスにより、IoT（モノのインターネット）分野に最適であり、IoTアプリケーション向けに効率的かつスケーラブルなデータ保存と分析ソリューションを提供します。

本ページでは、EMQXとTimescaleDB間のデータ統合について、実践的な手順を交えて包括的に紹介します。

## 動作概要

TimescaleDBデータ統合はEMQXに組み込まれた機能であり、EMQXのリアルタイムデータキャプチャと送信能力をTimescaleDBのデータ保存および分析能力と組み合わせます。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからTimescaleDBへのデータ取り込みが簡素化され、複雑なコーディングなしで保存と分析が可能です。

以下の図は、産業用IoTにおけるEMQXとTimescaleDBのデータ統合の典型的なアーキテクチャを示しています。

![MQTT to Timescale](./assets/mqtt-to-timescaledb.jpg)

EMQXとTimescaleDBは、エネルギー消費データをリアルタイムに効率的に収集・分析するためのスケーラブルなIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがデバイスアクセス、メッセージ送信、データルーティングを担うIoTプラットフォームとして機能し、TimescaleDBがデータ保存および分析プラットフォームとしてデータの保存と分析を担当します。

EMQXはルールエンジンとSinkを介してデバイスデータをTimescaleDBに転送します。TimescaleDBはSQL文を用いてデータを分析し、レポートやチャートなどの分析結果を生成し、TimescaleDBの可視化ツールを通じてユーザーに表示します。ワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ラインの識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、トピックマッチングに基づいて特定のソースからのメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などが含まれます。
3. **TimescaleDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをTimescaleDBに書き込む操作が実行されます。TimescaleDB SinkはSQLテンプレートを提供し、メッセージの特定フィールドをTimescaleDBの対応するテーブルやカラムに柔軟に書き込むことが可能です。

エネルギー消費データがTimescaleDBに書き込まれた後は、SQL文を用いて柔軟にデータ分析が行えます。例えば：

- Grafanaなどの可視化ツールに接続してチャートを生成し、エネルギー消費データを表示する。
- ERPなどのアプリケーションシステムに接続して生産分析や生産計画の調整を行う。
- 業務システムに接続してリアルタイムのエネルギー使用分析を実施し、データ駆動型のエネルギー管理を支援する。

## 特長と利点

EMQXのTimescaleDBデータ統合は、以下の特長と利点をビジネスにもたらします：

- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを効率的に処理できます。TimescaleDBはデータの書き込み、保存、クエリに優れており、IoTシナリオのデータ処理要件をシステムに負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXのルール内で豊富な処理や変換を経てからTimescaleDBに書き込まれます。
- **効率的な保存とスケーラビリティ**：EMQXとTimescaleDBは共にクラスターのスケールアウト機能を備えており、ビジネスの成長に応じて柔軟に水平スケーリングが可能です。
- **高度なクエリ機能**：TimescaleDBは時刻データの効率的なクエリと分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから正確な洞察を抽出できます。

## はじめる前に

このセクションでは、TimescaleDBデータ統合の作成を始める前に必要な準備、TimescaleDBのインストールおよびデータテーブルの作成について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Timescaleのインストールとデータテーブルの作成

EMQXはセルフホストのTimescaleDBまたはクラウド上のTimescale Serviceとの統合をサポートしています。Timescale Serviceをクラウドサービスとして利用するか、Dockerを用いてTimescaleDBインスタンスをデプロイできます。

:::: tabs 
::: tab Timescale Service

1. Tiger Cloudアカウントをお持ちでない場合は、[Create your Tiger Cloud account](https://www.tigerdata.com/docs/getting-started/latest/services#create-your-timescale-account)を参照してアカウントを作成してください。

2. Tiger Dataポータルにログインし、[Create a Tiger Cloud service](https://www.tigerdata.com/docs/getting-started/latest/services#create-your-first-service)からサービスを作成します。サービスのパスワードを保存してください。

3. サービス概要ページから接続情報を取得します。EMQXが必要とする項目は、**Database name**、**Host**、**Port**、**Username**です。

4. `psql client`で[サービスに接続](https://www.tigerdata.com/docs/getting-started/latest/services#connect-to-your-service)します。

   ```bash
   # サービスURLを使って接続
   psql "postgres://tsdbadmin@xxxxx.xxxxx.tsdb.cloud.timescale.com:32541/tsdb?sslmode=require"
   # 前ステップで保存したパスワードを使用
   Password for user tsdbadmin:
   ```

5. クライアントからのメッセージデータを保存するために、`sensor_data`テーブルを作成します。

   ```sql
   CREATE TABLE sensor_data (
       time        TIMESTAMPTZ       NOT NULL,
       location    TEXT              NOT NULL,
       temperature DOUBLE PRECISION  NULL,
       humidity    DOUBLE PRECISION  NULL
   );
   
   SELECT create_hypertable('sensor_data', 'time');
   ```

テーブルが正常に作成されると、Servicesの**Explorer**タブで`sensor_data`テーブルの情報を確認できます。

![Timescale Explorer table](./assets/timescale-explorer-table.png)

:::

::: tab TimescaleDB Docker

1. Docker環境がない場合は、[Install Docker](https://docs.docker.com/install/)を参照してください。

2. DockerでTimescaleDBコンテナを作成し、`POSTGRES_PASSWORD`環境変数でデータベースのパスワードを設定します。

   ```bash
   docker run -d --name timescaledb \
       -p 5432:5432 \
       -e POSTGRES_PASSWORD=public \
       timescale/timescaledb:latest-pg13
   ```

3. クライアントデータを保存するためのデータベースを作成します。

   ```bash
   docker exec -it timescaledb psql -U postgres
   
   ## tsdbデータベースを作成
   > CREATE database tsdb;
   
   > \c tsdb;
   ```

4. クライアントからのメッセージデータを保存するために、`sensor_data`テーブルを作成します。

   ```sql
   CREATE TABLE sensor_data (
       time        TIMESTAMPTZ       NOT NULL,
       location    TEXT              NOT NULL,
       temperature DOUBLE PRECISION  NULL,
       humidity    DOUBLE PRECISION  NULL
   );
   
   SELECT create_hypertable('sensor_data', 'time');
   ```

:::
::::

## コネクターの作成

TimescaleDB Sinkを作成する前に、TimescaleDBサービスに接続するためのTimescaleDBコネクターを作成する必要があります。

以下の手順は、EMQXとTimescaleDB（セルフホストの場合）をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、左側ナビゲーションメニューから **Integration** -> **Connector** をクリックします。
2. 画面右上の **Create** をクリックします。
3. コネクター一覧から **TimescaleDB** を選択し、**Next** をクリックします。
4. **Connector Name** に名前を入力します（例：`my-timescale`）。名前は英数字の組み合わせで指定してください。
5. TimescaleDBのデプロイ方法に応じて接続情報を入力します。Dockerでデプロイしている場合は、**Server Host** に `127.0.0.1:5432`、**Database Name** に `tsdb`、**Username** に `postgres`、**Password** に `public` を入力します。
6. 高度な設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。
7. **Create**をクリックする前に、**Test Connectivity**を押してコネクターがTimescaleDBサーバーに接続できるかテストできます。
8. **Create**ボタンをクリックしてコネクター作成を完了します。

これでTimescaleDBコネクターが作成されました。次に、ルールとSinkを作成してTimescaleDBに書き込むデータを指定します。

## TimescaleDB Sinkを使ったルールの作成

このセクションでは、Dashboard上でMQTTトピック `t/#` からのメッセージを処理し、処理結果を設定済みのTimescaleDB Sink経由でTimescaleDBに送信するルールの作成方法を示します。

1. EMQXダッシュボードにアクセスし、左側ナビゲーションメニューから **Integration** -> **Rules** をクリックします。

2. 画面右上の **+ Create** をクリックします。

3. ルール作成ページで、ルールIDに `my_rule` を入力します。

4. **SQL Editor** に以下のSQLルールを入力し、トピック `t/#` のMQTTメッセージをTimescaleDBに保存します：

   ```sql
   SELECT
     payload.temp as temp,
     payload.humidity as humidity,
     payload.location as location
   FROM
       "t/#"
   ```

   注：初心者の方は、**SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストが可能です。

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンから `TimescaleDB` を選択すると、EMQXはルールで処理したデータをTimescaleDBに送信します。

   **Action** ドロップダウンは `Create Action` のままにするか、既に作成済みのTimescaleDBアクションを選択できます。本例では新しいSinkを作成してルールに追加します。

6. Sinkの名前と説明を **Name** と **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-timescale` を選択します。新しいコネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. データ挿入用の**SQL Template**を以下のSQL文で設定します。

   注：これは前処理済みのSQLのため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
     INSERT INTO
    sensor_data (time, location, temperature, humidity)
     VALUES
      (NOW(), ${location}, ${temp}, ${humidity})
   ```

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **高度な設定（任意）**：詳細は[高度な設定](#高度な設定)を参照してください。

11. **Add** ボタンをクリックしてSinkの設定を完了します。ルール作成ページの**Action Outputs**タブに新しいSinkが表示されます。

12. ルール作成ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status** は `connected` となっているはずです。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると新しいTimescaleDB Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析されTimescaleDBに送信・保存されていることがわかります。

### ルールのテスト

MQTTXを使ってトピック `t/1` にメッセージを送信し、同時にオンライン/オフラインイベントをトリガーします：

```bash
mqttx pub -i emqx_c -t t/1 -m '{"temp":24,"humidity":30,"location":"hangzhou"}'
```

Sinkの稼働状況を確認すると、1件の新しいMatchedと1件のSent Successfullyメッセージがあるはずです。

TimescaleDBの`sensor_data`テーブルを確認すると、新しいレコードが挿入されています：

```bash
tsdb=# select * from sensor_data;
             time              | location | temperature | humidity 
-------------------------------+----------+-------------+----------
 2023-07-10 08:28:48.813988+00 | hangzhou |          24 |       30
 2023-07-10 08:28:57.737768+00 | hangzhou |          24 |       30
 2023-07-10 08:28:58.599537+00 | hangzhou |          24 |       30
(3 rows)
```

## 高度な設定

このセクションでは、TimescaleDB Sinkの高度な設定オプションについて詳述します。DashboardでSinkを設定する際、**Advanced Settings** タブで以下のパラメータをニーズに合わせて調整できます。

| **項目**                  | **説明**                                                                                             | **推奨値**           |
| ------------------------- | -------------------------------------------------------------------------------------------------- | -------------------- |
| **Application Name**      | PostgreSQL接続時のアプリケーション名を指定します。PostgreSQLのアクティビティビューやログに表示されます。1～63バイトの印刷可能なASCII文字のみ使用可能で、ゼロバイトは不可です。 | `emqx`               |
| **Connection Pool Size**  | Timescaleサービスとの接続プールに保持できる同時接続数を指定します。EMQXとTimescaleDB間のアクティブ接続数を制御し、スケーラビリティとパフォーマンスを管理します。<br/>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、アプリケーションの負荷に依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の可能性があります。 | `8`                  |
| **Start Timeout**         | コネクターが自動起動したリソース（例：TimescaleDBインスタンス）が正常状態になるまで待機する最大秒数です。リソースが完全に稼働しデータトランザクションを処理可能になるまで操作を進めないようにします。 | `5`                  |
| **Buffer Pool Size**      | EMQXとTimescaleDB間の送信タイプSinkでデータフロー管理に割り当てるバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前に一時的にデータを保持・処理します。受信のみのSinkには「0」を設定可能です。 | `16`                 |
| **Request TTL**           | バッファに入ったリクエストが有効とみなされる最大秒数です。TTLを超えてバッファに滞留するか、送信後にTimescaleDBからの応答やアックが得られない場合、リクエストは期限切れとみなされます。 | `45`                 |
| **Health Check Interval** | SinkがTimescaleDB接続の自動ヘルスチェックを行う間隔（秒）を指定します。                                     | `15`                 |
| **Max Buffer Queue Size** | TimescaleDB Sinkの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータ送信前に一時的にデータを保持し、データフローを効率化します。システム性能やデータ転送要件に応じて調整してください。 | `256`                |
| **Max Batch Size**        | EMQXからTimescaleDBへ単一転送操作で送信するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率とパフォーマンスを最適化できます。<br />「1」に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1`                  |
| **Query Mode**            | `asynchronous` または `synchronous` のクエリモードを選択し、要件に応じてメッセージ送信を最適化します。非同期モードではTimescaleDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージをTimescaleDB到着前に受信する可能性があります。 | `Async`              |
| **Inflight Window**       | 「インフライトクエリ」とは開始済みで応答やアックをまだ受け取っていないクエリのことです。SinkがTimescaleDBと通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async` の場合、このパラメータは特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は1に設定してください。 | `100`                |

## さらに詳しく

以下のリンクもご参照ください：

**ブログ**：

[MQTTパフォーマンスベンチマークテスト：EMQX-TimescaleDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-series-emqx-timescaledb-integration)

[MQTTとTimescaleで産業用エネルギー監視のIoT時系列データアプリケーションを構築](https://www.emqx.com/en/blog/build-an-iot-time-series-data-application-for-energy-storage-with-mqtt-and-timescale)
