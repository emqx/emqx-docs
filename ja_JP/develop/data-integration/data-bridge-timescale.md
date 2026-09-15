# TimescaleDBへのMQTTデータ取り込み

[TimescaleDB](https://www.timescale.com/)（Timescale）は時系列データの保存と分析に特化したデータベースです。優れたデータスループットと信頼性の高いパフォーマンスにより、IoT（モノのインターネット）分野に最適であり、IoTアプリケーション向けに効率的かつスケーラブルなデータ保存と分析ソリューションを提供します。

本ページでは、EMQXとTimescaleDB間のデータ統合について包括的に紹介し、データ統合の作成と検証に関する実践的な手順を説明します。

## 動作の仕組み

TimescaleDBデータ統合は、EMQXに組み込まれた機能であり、EMQXのリアルタイムデータキャプチャと送信機能とTimescaleDBのデータ保存・分析機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからTimescaleDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、産業用IoTにおけるEMQXとTimescaleDBのデータ統合の典型的なアーキテクチャを示しています。

![MQTT to Timescale](./assets/mqtt-to-timescaledb.jpg)

EMQXとTimescaleDBは、エネルギー消費データをリアルタイムに効率的に収集・分析するためのスケーラブルなIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがデバイスの接続、メッセージ送信、データルーティングを担当するIoTプラットフォームとして機能し、TimescaleDBがデータ保存および分析プラットフォームとしてデータの保存と分析を担当します。

EMQXはルールエンジンとSinkを介してデバイスデータをTimescaleDBに転送します。TimescaleDBはSQL文を用いてデータを分析し、レポートやチャートなどの分析結果を生成し、TimescaleDBの可視化ツールを通じてユーザーに表示します。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ライン識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などが含まれます。  
3. **TimescaleDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをTimescaleDBに書き込む操作が実行されます。TimescaleDB SinkはSQLテンプレートを提供し、特定のメッセージフィールドをTimescaleDBの対応するテーブルやカラムに柔軟に書き込む定義が可能です。

エネルギー消費データがTimescaleDBに書き込まれた後は、SQL文を用いて柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールと連携してチャートを生成し、エネルギー消費データを表示する。
- ERPなどのアプリケーションシステムと連携し、生産分析や生産計画の調整を行う。
- 業務システムと連携してリアルタイムのエネルギー使用分析を実施し、データ駆動型のエネルギー管理を促進する。

## 特長とメリット

EMQXのTimescaleDBデータ統合は、以下の特長とメリットをビジネスにもたらします。

- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを効率的に処理可能です。TimescaleDBはデータの書き込み、保存、クエリに優れており、IoTシナリオのデータ処理要件をシステムに過負荷をかけずに満たします。  
- **メッセージ変換**：メッセージはEMQXのルール内で豊富な処理や変換を経てからTimescaleDBに書き込まれます。  
- **効率的な保存とスケーラビリティ**：EMQXとTimescaleDBは共にクラスターのスケールアウト機能を備えており、ビジネスの成長に合わせて柔軟に水平スケーリングが可能です。  
- **高度なクエリ機能**：TimescaleDBはタイムスタンプデータの効率的なクエリと分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから精緻なインサイトを抽出できます。

## はじめる前に

TimescaleDBデータ統合の作成を開始する前に必要な準備について説明します。TimescaleDBのインストールとデータテーブルの作成が含まれます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Timescaleのインストールとデータテーブルの作成

EMQXはセルフホストのTimescaleDBまたはクラウド上のTimescaleサービスとの統合をサポートしています。Timescaleサービスをクラウドサービスとして利用するか、Dockerを使ってTimescaleDBインスタンスをデプロイできます。

:::: tabs 
::: tab Timescale Service

1. Tiger Cloudアカウントをお持ちでない場合は、[Tiger Cloudアカウントの作成](https://www.tigerdata.com/docs/getting-started/latest/services#create-your-timescale-account)を参照してアカウントを作成してください。

2. Tiger Dataポータルにログインし、[Tiger Cloudサービスの作成](https://www.tigerdata.com/docs/getting-started/latest/services#create-your-first-service)を行います。サービスのパスワードを保存してください。

3. サービス概要ページから接続情報を取得します。EMQXが必要とする項目は、**データベース名**、**ホスト**、**ポート**、**ユーザー名**です。

4. `psql client`で[サービスに接続](https://www.tigerdata.com/docs/getting-started/latest/services#connect-to-your-service)します。

   ```bash
   # サービスURLで接続
   psql "postgres://tsdbadmin@xxxxx.xxxxx.tsdb.cloud.timescale.com:32541/tsdb?sslmode=require"
   # 前のステップで保存したパスワードを使用
   Password for user tsdbadmin:
   ```

5. クライアントからのメッセージデータを保存するためのテーブル`sensor_data`を作成します。

   ```sql
   CREATE TABLE sensor_data (
       time        TIMESTAMPTZ       NOT NULL,
       location    TEXT              NOT NULL,
       temperature DOUBLE PRECISION  NULL,
       humidity    DOUBLE PRECISION  NULL
   );
   
   SELECT create_hypertable('sensor_data', 'time');
   ```

テーブル作成後、サービスの**Explorer**タブで`sensor_data`テーブルの情報を確認できます。

![Timescale Explorer table](./assets/timescale-explorer-table.png)

:::

::: tab TimescaleDB Docker

1. Docker環境がない場合は、[Dockerのインストール](https://docs.docker.com/install/)を参照してください。

2. DockerでTimescaleDBコンテナを作成し、`POSTGRES_PASSWORD`環境変数でデータベースのパスワードを設定します。

   ```bash
   docker run -d --name timescaledb \
       -p 5432:5432 \
       -e POSTGRES_PASSWORD=public \
       timescale/timescaledb:latest-pg13
   ```

3. クライアントデータを保存するデータベースを作成します。

   ```bash
   docker exec -it timescaledb psql -U postgres
   
   ## tsdbデータベースを作成
   > CREATE database tsdb;
   
   > \c tsdb;
   ```

4. クライアントからのメッセージデータを保存するテーブル`sensor_data`を作成します。

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

以下の手順は、EMQXとTimescaleDB（セルフホストの場合）がローカルマシンで動作していることを前提としています。リモートで稼働している場合は設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、左側のナビゲーションメニューから **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックします。
3. コネクター一覧から **TimescaleDB** を選択し、**Next** をクリックします。
4. **Connector Name** に名前を入力します。例：`my-timescale`。名前は英数字の大文字・小文字を組み合わせてください。
5. TimescaleDBのデプロイ方法に応じて接続情報を入力します。Dockerでデプロイした場合は、**Server Host** に `127.0.0.1:5432`、**Database Name** に `tsdb`、**Username** に `postgres`、**Password** に `public` を入力します。
6. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。
7. **Create**をクリックする前に、**Test Connectivity**を押してコネクターがTimescaleDBサーバーに接続できるかテストできます。
8. **Create**ボタンをクリックしてコネクターの作成を完了します。

これでTimescaleDBコネクターが作成されました。次に、ルールとSinkを作成し、TimescaleDBデータベースに書き込むデータを指定します。

## TimescaleDB Sinkを用いたルールの作成

このセクションでは、ダッシュボードでルールを作成し、ソースMQTTトピック`t/#`からのメッセージを処理して、設定したSinkを通じてTimescaleDBに送信する方法を示します。

1. EMQXダッシュボードにアクセスし、左側のナビゲーションメニューから **Integration** -> **Rules** をクリックします。

2. ページ右上の **+ Create** をクリックします。

3. ルール作成ページでルールIDに `my_rule` を入力します。

4. **SQL Editor** に以下のSQLルールを入力し、トピック`t/#`のMQTTメッセージをTimescaleDBに保存します。

   ```sql
   SELECT
     payload.temp as temp,
     payload.humidity as humidity,
     payload.location as location
   FROM
       "t/#"
   ```

   注：初心者の方は、**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールの学習とテストが可能です。

5. **+ Add Action** ボタンをクリックして、ルールにトリガーされるアクションを定義します。**Type of Action**のドロップダウンから`TimescaleDB`を選択すると、EMQXはルールで処理されたデータをTimescaleDBに送信します。

   **Action**ドロップダウンは`Create Action`のままにするか、既存のTimescaleDBアクションを選択できます。本例では新しいSinkを作成してルールに追加します。

6. Sinkの**Name**と**Description**に名前と説明を入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-timescale`を選択します。新しいコネクターはドロップダウン横のボタンから作成可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. 以下のSQL文を用いて**SQL Template**を設定します。

   注：これは前処理済みのSQLのため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
     INSERT INTO
    sensor_data (time, location, temperature, humidity)
     VALUES
      (NOW(), ${location}, ${temp}, ${humidity})
   ```

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configurations)を参照してください。

11. **Add**ボタンをクリックしてSinkの設定を完了します。ルール作成ページの**Action Outputs**タブに新しいSinkが表示されます。

12. ルール作成ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status**は`connected`となっているはずです。

これでルールの作成が完了し、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいTimescaleDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認でき、トピック`t/#`のメッセージがルール`my_rule`で解析されTimescaleDBに送信・保存されていることがわかります。

### ルールのテスト

MQTTXを使ってトピック`t/1`にメッセージを送信し、同時にオンライン/オフラインイベントをトリガーします：

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

## 詳細設定

本セクションでは、TimescaleDB Sinkの詳細設定オプションについて説明します。ダッシュボードでSinkを設定する際、**Advanced Settings**にて以下のパラメータをニーズに合わせて調整できます。

| **項目**                  | **説明**                                                                                          | **推奨値**            |
| ------------------------- | ------------------------------------------------------------------------------------------------ | --------------------- |
| **Connection Pool Size**  | Timescaleサービスとの接続プールで維持可能な同時接続数を指定します。この設定はEMQXとTimescaleDB間のアクティブな接続数を制御し、アプリケーションのスケーラビリティとパフォーマンス管理に役立ちます。<br/>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、アプリケーションのワークロードに依存します。大きすぎるとリソース枯渇を招き、小さすぎるとスループットが制限されます。 | `8`                   |
| **Start Timeout**         | コネクターが自動起動したリソースの正常状態到達を待機する最大秒数を指定します。この設定により、TimescaleDBなどの接続先リソースが完全に稼働しデータ処理準備が整うまで操作を進めないようにします。 | `5`                   |
| **Buffer Pool Size**      | EMQXとTimescaleDB間のエグレス型Sinkでデータフローを管理するバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前の一時保存と処理を担当し、パフォーマンス最適化とスムーズなデータ転送を支えます。イングレスのみのSinkでは無効で、"0"に設定可能です。 | `16`                  |
| **Request TTL**           | バッファに入ったリクエストが有効とみなされる最大秒数を指定します。TTLはリクエストがバッファに入った時点からカウントされ、TTLを超えるかTimescaleDBからの応答やアックがタイムリーに得られない場合、リクエストは期限切れと見なされます。 | `45`                  |
| **Health Check Interval** | SinkがTimescaleDB接続のヘルスチェックを自動的に行う間隔（秒）を指定します。 | `15`                  |
| **Max Buffer Queue Size** | TimescaleDB Sinkの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータ送信前の一時保存を担い、システム性能やデータ転送要件に応じて調整してください。 | `256`                 |
| **Max Batch Size**        | EMQXからTimescaleDBへ一度に転送するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送の効率とパフォーマンスを最適化できます。<br />"1"に設定すると、データはバッチ化せず個別に送信されます。 | `1`                   |
| **Query Mode**            | メッセージ送信の最適化のため、`asynchronous`（非同期）または`synchronous`（同期）クエリモードを選択できます。非同期モードではTimescaleDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージをTimescaleDB到着前に受信する可能性があります。 | `Async`               |
| **Inflight Window**       | "インフライトクエリ"とは開始済みで応答やアックをまだ受け取っていないクエリを指します。SinkがTimescaleDBと通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode**が`async`の場合、この設定は特に重要です。同じMQTTクライアントからのメッセージを厳密に順序処理したい場合は、この値を1に設定してください。 | `100`                 |

## 参考情報

以下のリンクもご参照ください：

**ブログ**：

[MQTTパフォーマンスベンチマークテスト：EMQX-TimescaleDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-series-emqx-timescaledb-integration)

[MQTTとTimescaleを使った産業用エネルギー監視向けIoT時系列データアプリケーションの構築](https://www.emqx.com/en/blog/build-an-iot-time-series-data-application-for-energy-storage-with-mqtt-and-timescale)
