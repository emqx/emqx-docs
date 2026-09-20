# ClickHouseへのMQTTデータ取り込み

[ClickHouse](https://clickhouse.com/) は、高性能なカラム指向のSQLデータベース管理システム（DBMS）であり、オンライン分析処理（OLAP）に特化しています。大量のデータを最小限のレイテンシで処理・分析することに優れており、優れたクエリ性能、柔軟なデータモデル、スケーラブルな分散アーキテクチャを備えているため、さまざまなデータ分析シナリオに適しています。EMQXはClickHouseとの統合をサポートしており、MQTTメッセージやイベントデータをClickHouseに取り込んで、さらなる分析や処理を行うことが可能です。

## 動作概要

ClickHouseとのデータ統合は、EMQXに標準搭載された機能であり、MQTTのリアルタイムデータ取得・送信機能とClickHouseの強力なデータ処理機能を組み合わせることを目的としています。組み込みの[ルールエンジン](https://docs.emqx.com/en/enterprise/v5.1/data-integration/rules.html)コンポーネントにより、EMQXからClickHouseへのデータ取り込みを簡素化し、複雑なコーディングを不要にしています。

以下の図は、EMQXとClickHouse間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/clickhouse_architecture.png" alt="clickhouse_architecture" style="zoom:67%;" />

MQTTデータをClickHouseに取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、機械、センサー、製造ラインの稼働状態や計測値、トリガーイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールに基づいて処理されます。ルールは事前に定義された条件に基づき、どのメッセージをClickHouseにルーティングするかを決定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **ClickHouseへのデータ取り込み**：ルールエンジンがClickHouseへの保存対象メッセージを特定すると、メッセージの転送アクションをトリガーします。処理済みデータはClickHouseデータベースのコレクションにシームレスに書き込まれます。
4. **データの保存と活用**：データがClickHouseに保存されることで、企業はそのクエリ性能を活用してさまざまなユースケースに対応可能です。例えば、物流やサプライチェーン管理分野では、GPSトラッカー、温度センサー、在庫管理システムなどのIoTデバイスからのデータを監視・分析し、リアルタイム追跡、ルート最適化、需要予測、効率的な在庫管理に役立てることができます。

## 特長と利点

ClickHouseとのデータ統合は、効率的なデータ送信、保存、活用を実現するための多彩な特長と利点を提供します：

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからClickHouseへの効率的かつ信頼性の高いデータ送信を保証します。即時の洞察やアクションが求められるユースケースに最適です。
- **高性能かつスケーラブル**：EMQXの分散アーキテクチャとClickHouseのカラムナストレージ形式により、データ量の増加に応じてシームレスにスケール可能です。大量データでも一貫した性能と応答性を維持します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、ClickHouseに保存する前にデータの前処理が可能です。フィルタリング、ルーティング、集約、強化など多様な変換機能をサポートし、ニーズに合わせたデータ整形を実現します。
- **簡単なデプロイと管理**：EMQXはデータソースの設定、前処理ルール、ClickHouse保存設定をユーザーフレンドリーなインターフェースで提供し、データ統合プロセスの構築と運用を簡素化します。
- **高度な分析機能**：ClickHouseの強力なSQLクエリ言語と複雑な分析関数のサポートにより、IoTデータから価値ある洞察を得られ、予測分析や異常検知などが可能になります。

## はじめる前に

このセクションでは、EMQXダッシュボードでClickHouseデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- UNIXターミナルと基本コマンドの知識

### ClickHouseサーバーの起動

ここでは、[Docker](https://www.docker.com/)を使ってClickHouseサーバーを起動する方法を紹介します。

1. 以下の初期化SQL文を含む`init.sql`ファイルを作成します。このファイルはコンテナ起動時にデータベースを初期化するために使用されます。

   ```bash
   cat >init.sql <<SQL_INIT
   CREATE DATABASE IF NOT EXISTS mqtt_data;
   CREATE TABLE IF NOT EXISTS mqtt_data.messages (
      data String,
      arrived TIMESTAMP
   ) ENGINE = MergeTree()
   ORDER BY arrived;
   SQL_INIT
   ```

2. 以下のコマンドでClickHouseサーバーを起動します。このコマンドではデータベース名、ポート番号、ユーザー名、パスワードを指定し、カレントディレクトリの`init.sql`をDockerコンテナ内にマウントします。

   ```bash
   docker run \
   --rm \
   -e CLICKHOUSE_DB=mqtt_data \
   -e CLICKHOUSE_USER=emqx \
   -e CLICKHOUSE_DEFAULT_ACCESS_MANAGEMENT=1 \
   -e CLICKHOUSE_PASSWORD=public \
   -p 18123:8123 \
   -p 19000:9000 \
   --ulimit nofile=262144:262144 \
   -v $pwd/init.sql:/docker-entrypoint-initdb.d/init.sql \
   clickhouse/clickhouse-server
   ```

DockerでのClickHouse実行に関する詳細は[dockerhub](https://hub.docker.com/r/clickhouse/clickhouse-server)をご参照ください。

## コネクターの作成

このセクションでは、SinkをClickHouseサーバーに接続するためのコネクター作成方法を説明します。

以下の手順はEMQXとClickHouseをローカルマシンで実行していることを前提としています。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **ClickHouse** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下を設定します：
   - **Connector name**：コネクター名を入力します。英数字の大文字・小文字の組み合わせで、例：`my_clickhouse`
   - **Server URL**：`http://127.0.0.1:18123`
   - **Database Name**：`mqtt_data`
   - **Username**：`emqx`
   - **Password**：`public`
5. 高度な設定（任意）：[Advanced Configurations](#advanced-configurations)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity** をクリックしてClickHouseサーバーへの接続をテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップで **Back to Connector List** または **Create Rule** を選択可能です。ルールとSinkの作成については[Create a Rule with ClickHouse Sink](#create-a-rule-with-clickhouse-sink)をご覧ください。

## ClickHouse Sinkを使ったルールの作成

このセクションでは、DashboardでMQTTのソーストピック `t/#` からのメッセージを処理し、処理結果を設定済みのSink経由でClickHouseに転送するルールの作成方法を説明します。

1. EMQXダッシュボードで、左側メニューの **Integration** -> **Rules** をクリックします。
2. ページ右上の **Create** をクリックします。
3. ルールIDを入力します。例：`my_rule`
4. SQLエディタに以下の文を入力します。これはトピックパターン `t/#` にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT 
     payload as data,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

   注：初心者の方は **SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストが可能です。

5. + **Add Action** ボタンをクリックし、ルールによりトリガーされるアクションを定義します。このアクションによりEMQXはルールで処理したデータをClickHouseに送信します。
6. **Type of Action** のドロップダウンリストから `ClickHouse` を選択します。**Action** はデフォルトの `Create Action` のままにします。既存のClickHouse Sinkがあれば選択可能ですが、ここでは新規Sinkを作成します。
7. Sinkの名前を入力します。英数字の大文字・小文字の組み合わせで指定してください。
8. **Connector** のドロップダウンから先ほど作成した `my_clickhouse` を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメータは[Create a Connector](#create-a-connector)を参照してください。
9. **Batch Value Separator** は複数入力項目を区切るための文字列で、デフォルトの `,` のままにします。この設定は[バッチモード](./data-bridges.md)を有効にし、ClickHouseのFORMAT構文で別のフォーマットを指定する場合にのみ変更が必要です。
10. SQLテンプレートに以下の文を入力します（[ルールエンジン](./rules.md)を利用して、SQLインジェクション対策のために入力SQLの文字列を適切にエスケープしてください）：

    ```sql
    INSERT INTO messages(data, arrived) VALUES ('${data}', ${timestamp})
    ```

    ここで `${data}` と `${timestamp}` はメッセージ内容とタイムスタンプを表し、後でルールで設定されます。EMQXは転送前に対応する内容に置換します。

    SQLテンプレート内でプレースホルダー変数が未定義の場合、**Undefined Vars as Null** スイッチをSQLテンプレート上部で切り替えられます：

    - **無効（デフォルト）**：ルールエンジンは文字列 `undefined` をDBに挿入します。
    - **有効**：変数未定義時にルールエンジンが `NULL` をDBに挿入します。

      ::: tip

      可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

      :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。
12. 高度な設定（任意）：[Advanced Configurations](#advanced-configurations)を参照してください。
13. **Create** をクリックする前に、**Test Connectivity** ボタンでClickHouseサーバーへの接続確認ができます。
14. **Create** ボタンをクリックしてSinkの設定を完了します。**Create Rule** ページの **Action Outputs** タブに新しいSinkが表示されます。
15. **Create Rule** ページで設定内容を確認し、**Create** をクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status** は接続済みとなります。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいClickHouse Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、ClickHouseに送信・保存されている様子が確認できます。

## ルールのテスト

EMQXダッシュボードに組み込まれたWebSocketクライアントを使い、ルールが期待通りに動作するかテストできます。

ダッシュボード左メニューの **Diagnose** -> **WebSocket Client** をクリックしてWebSocketクライアントにアクセスし、以下の手順でWebSocketクライアントを設定し、トピック `t/test` にメッセージを送信します：

1. 現在のEMQXインスタンスの接続情報を入力します。EMQXをローカルで実行している場合、デフォルト値を使用可能です（認証設定を変更している場合はユーザー名・パスワードを入力してください）。
2. **Connect** をクリックしてクライアントをEMQXに接続します。
3. ページ下部のパブリッシュエリアに以下を入力します：
   - **Topic**：`t/test`
   - **Payload**：`Hello World Clickhouse from EMQX`
   - **QoS**：2
4. **Publish** をクリックしてメッセージを送信します。ClickHouseサーバーのデータベース `mqtt_data` のテーブル `messages` にエントリが挿入されているはずです。ターミナルから以下のコマンドで確認できます：

   ```bash
   curl -u emqx:public -X POST -d "SELECT * FROM mqtt_data.messages" http://localhost:18123
   ```

5. 正常に動作していれば、以下のような出力が得られます（タイムスタンプは異なります）：

   ```
   Hello World Clickhouse from EMQX        2024-01-17 09:40:06
   ```

## 高度な設定

このセクションでは、EMQX ClickHouseコネクターの高度な設定オプションについて詳述します。ダッシュボードでコネクターを設定する際、**Advanced Settings** にて以下のパラメータをニーズに合わせて調整可能です。

| **項目**                   | **説明**                                                                                         | **推奨値** |
| -------------------------- | ------------------------------------------------------------------------------------------------ | ---------- |
| **Connection Pool Size**   | ClickHouseサービスとの接続プールで維持可能な同時接続数を指定します。この設定はEMQXとClickHouse間のアクティブ接続数を制御し、アプリケーションのスケーラビリティと性能管理に役立ちます。<br/>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、アプリケーションの負荷に依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の原因となります。 | `8`        |
| **Clickhouse Timeout**     | ClickHouseサーバーへの接続確立時にコネクターが待機する最大時間（秒）を指定します。<br/>**注意**：システム性能とリソース利用のバランスを取るため、ネットワーク状況に応じた最適なタイムアウト値をテストすることが推奨されます。 | `15`       |
| **Start Timeout**          | 自動起動したリソースが正常状態になるまでコネクターが待機する最大時間（秒）を指定します。これにより、ClickHouseのデータベースインスタンスなどが完全に稼働し、データ処理可能になるまで操作を進めないようにします。 | `5`        |
| **Buffer Pool Size**       | EMQXとClickHouse間のegressタイプSinkにおけるデータフロー管理用のバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前の一時保管と処理を担当し、パフォーマンス最適化とスムーズなデータ送信を支えます。ingressのみのブリッジではこの値を「0」に設定可能です。 | `16`       |
| **Request TTL**            | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこのTTLを超えてバッファに滞留するか、ClickHouseからの応答やアックが遅延した場合、リクエストは期限切れと見なされます。 | `45`       |
| **Health Check Interval**  | コネクターがClickHouse接続の自動ヘルスチェックを行う間隔（秒）を指定します。 | `15`       |
| **Max Buffer Queue Size**  | ClickHouseコネクターの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはClickHouseへのデータ送信前の一時保管を担い、システム性能やデータ転送要件に応じて調整してください。 | `256`      |
| **Max Batch Size**         | EMQXからClickHouseへの単一転送操作で送信可能なデータバッチの最大サイズを指定します。サイズ調整により転送効率と性能を最適化可能です。<br />「1」に設定すると、データはバッチ化せず個別に送信されます。 | `1`        |
| **Query Mode**             | メッセージ送信要件に応じて `asynchronous` または `synchronous` のクエリモードを選択可能です。非同期モードではClickHouseへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがClickHouseへの到着前にメッセージを受信する可能性があります。 | `Async`    |
| **Inflight Window**        | 「インフライトクエリ」とは開始済みで応答やアックをまだ受け取っていないクエリを指します。ClickHouseとの通信時に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async` の場合、この設定は特に重要です。同一MQTTクライアントからのメッセージを厳密な順序で処理する必要がある場合は、この値を1に設定してください。 | `100`      |

## さらに詳しく

以下のリンクから詳細情報をご覧いただけます：

**ブログ**：

- [EMQX + ClickHouseによるIoTデータ収集と分析の実現](https://www.emqx.com/en/blog/emqx-and-clickhouse-for-iot-data-access-and-analysis)
- [MQTTからClickHouse統合：リアルタイムIoTデータ分析の加速](https://www.emqx.com/en/blog/mqtt-to-clickhouse-integration)
