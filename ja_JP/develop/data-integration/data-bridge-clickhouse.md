# ClickHouseへのMQTTデータ取り込み

[ClickHouse](https://clickhouse.com/)は、高性能なカラム指向SQLデータベース管理システム（DBMS）であり、オンライン分析処理（OLAP）に適しています。大量のデータを低レイテンシで処理・分析することに優れており、優れたクエリ性能、柔軟なデータモデル、スケーラブルな分散アーキテクチャを備えているため、さまざまなデータ分析シナリオに適しています。EMQXはClickHouseとの統合をサポートしており、MQTTメッセージやイベントデータをClickHouseに取り込んで、さらなる分析や処理を行うことが可能です。

## 動作概要

ClickHouseとのデータ統合は、EMQXに標準搭載された機能であり、MQTTのリアルタイムデータ取得・送信機能とClickHouseの強力なデータ処理機能を組み合わせることができます。組み込みの[ルールエンジン](https://docs.emqx.com/en/enterprise/v5.1/data-integration/rules.html)コンポーネントにより、EMQXからClickHouseへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとClickHouse間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/clickhouse_architecture.png" alt="clickhouse_architecture" style="zoom:67%;" />

MQTTデータのClickHouseへの取り込みは以下のように行われます：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、機械、センサー、製品ラインの稼働状態、計測値、またはトリガーされたイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
3. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づき、ClickHouseにルーティングすべきメッセージを決定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
4. **ClickHouseへのデータ取り込み**：ルールエンジンがClickHouseへの保存対象メッセージを特定すると、メッセージをClickHouseに転送するアクションをトリガーします。処理済みデータはClickHouseデータベースのコレクションにシームレスに書き込まれます。
5. **データの保存と活用**：データがClickHouseに保存されることで、企業はそのクエリ機能を活用してさまざまなユースケースに対応できます。たとえば、物流やサプライチェーン管理分野では、GPSトラッカー、温度センサー、在庫管理システムなどのIoTデバイスからのデータをリアルタイムに監視・分析し、追跡、ルート最適化、需要予測、効率的な在庫管理に役立てることが可能です。

## 特長とメリット

ClickHouseとのデータ統合は、効率的なデータ送信、保存、活用を実現するために以下のような特長とメリットを提供します：

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからClickHouseへの効率的かつ信頼性の高いデータ送信を保証します。即時の洞察やアクションが求められるユースケースに最適です。
- **高性能かつスケーラブル**：EMQXの分散アーキテクチャとClickHouseのカラムナーストレージ形式により、データ量の増加に応じてシームレスにスケール可能です。大規模データセットでも一貫した性能と応答性を維持します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、ClickHouseに保存する前にデータを前処理できます。フィルタリング、ルーティング、集約、強化など多様なデータ変換機能をサポートし、ニーズに応じてデータを整形可能です。
- **簡単なデプロイと管理**：EMQXはデータソース設定、前処理ルール、ClickHouse保存設定をユーザーフレンドリーなインターフェースで提供し、データ統合プロセスのセットアップと運用管理を簡素化します。
- **高度な分析機能**：ClickHouseの強力なSQLクエリ言語と複雑な分析関数のサポートにより、IoTデータから価値ある洞察を得ることができ、予測分析や異常検知などに活用可能です。

## はじめる前に

このセクションでは、EMQXダッシュボードでClickHouseデータ統合を作成する前に準備すべき事項を説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- UNIXターミナルおよびコマンドの基本知識

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

2. 以下のコマンドでClickHouseサーバーを起動します。このコマンドはデータベース名、ポート番号、ユーザー名、パスワードを指定し、カレントディレクトリの`init.sql`ファイルをDockerコンテナ内にマウントします。

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

DockerでのClickHouseの起動に関する詳細は[dockerhub](https://hub.docker.com/r/clickhouse/clickhouse-server)をご覧ください。

## コネクターの作成

このセクションでは、SinkをClickHouseサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとClickHouseをローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**ClickHouse**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の大文字・小文字の組み合わせが推奨されます。例：`my_clickhouse`
   - **Server URL**：`http://127.0.0.1:18123`
   - **Database Name**：`mqtt_data`
   - **Username**：`emqx`
   - **Password**：`public`
5. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがClickHouseサーバーに接続できるかテストできます。
7. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてルールおよびSinkの作成を続行できます。詳細は[ClickHouse Sink付きルールの作成](#create-a-rule-with-clickhouse-sink)を参照してください。

## ClickHouse Sink付きルールの作成

このセクションでは、ダッシュボードでMQTTのソーストピック`t/#`からのメッセージを処理し、処理結果を設定済みのSink経由でClickHouseに転送するルールの作成方法を説明します。

1. EMQXダッシュボードで左側メニューの**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. SQLエディターに以下の文を入力します。これはトピックパターン`t/#`にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT 
     payload as data,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

   注：初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールの学習とテストが可能です。

5. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをClickHouseに送信します。

6. **Type of Action**ドロップダウンから`ClickHouse`を選択します。**Action**はデフォルトの`Create Action`のままにします。既存のClickHouse Sinkがあれば選択可能ですが、この例では新規Sinkを作成します。

7. Sinkの名前を入力します。英数字の大文字・小文字の組み合わせが推奨されます。

8. **Connector**ドロップダウンから先ほど作成した`my_clickhouse`を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメーターは[コネクターの作成](#create-a-connector)を参照してください。

9. **Batch Value Separator**にはデフォルトの`,`を保持します。これは複数の入力項目を区切るための設定で、[バッチモード](./data-bridges.md)を有効にし、ClickHouseのFORMAT構文で別の形式を指定する場合のみ変更が必要です。

10. SQLテンプレートに以下の文を入力します（[ルールエンジン](./rules.md)を使って、SQLインジェクション対策のために入力SQL内の文字列が適切にエスケープされていることを確認してください）：

    ```sql
    INSERT INTO messages(data, arrived) VALUES ('${data}', ${timestamp})
    ```

    ここで`${data}`と`${timestamp}`はメッセージ内容とタイムスタンプを表し、ルールでのメッセージ転送時にEMQXが対応する内容に置き換えます。

    SQLテンプレート内でプレースホルダー変数が未定義の場合は、**SQL template**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を切り替えられます：

    - **Disabled**（デフォルト）：ルールエンジンは文字列`undefined`をデータベースに挿入します。
    - **Enabled**：変数が未定義の場合、`NULL`を挿入します。

      ::: tip

      可能な限りこのオプションは有効にすることを推奨します。無効にするのは後方互換性確保のためのみです。

      :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した際にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**ボタンでClickHouseサーバーへの接続確認が可能です。

14. **Create**ボタンをクリックしてSink設定を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

15. **Create Rule**ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status**は`connected`となっているはずです。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいClickHouse Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認でき、トピック`t/#`のメッセージがルール`my_rule`で解析されてClickHouseに送信・保存されている様子が見られます。

## ルールのテスト

EMQXダッシュボードに組み込まれたWebSocketクライアントを使って、ルールが期待通りに動作するかテストできます。

ダッシュボード左メニューの**Diagnose** -> **WebSocket Client**をクリックしてWebSocketクライアントにアクセスし、以下の手順でWebSocketクライアントを設定してトピック`t/test`にメッセージを送信します：

1. 現在のEMQXインスタンスの接続情報を入力します。ローカルでEMQXを実行している場合は、デフォルト値を使用できます（認証設定を変更している場合はユーザー名とパスワードの入力が必要です）。

2. **Connect**をクリックしてクライアントをEMQXインスタンスに接続します。

3. ページ下部のパブリッシュエリアに以下を入力します：
   * **Topic**：`t/test`
   * **Payload**：`Hello World Clickhouse from EMQX`
   * **QoS**：2

4. **Publish**をクリックしてメッセージを送信します。ClickHouseサーバーの`mqtt_data`データベース内の`messages`テーブルにエントリが挿入されているはずです。ターミナルから以下のコマンドで確認できます：

   ```bash
   curl -u emqx:public -X POST -d "SELECT * FROM mqtt_data.messages" http://localhost:18123
   ```

5. 正常に動作していれば、以下のような出力が得られます（タイムスタンプは異なります）：

   ```
   Hello World Clickhouse from EMQX        2024-01-17 09:40:06
   ```

## 詳細設定

このセクションでは、EMQX ClickHouseコネクターの詳細設定オプションについて説明します。ダッシュボードでコネクターを設定する際、**Advanced Settings**に移動して以下のパラメーターをニーズに合わせて調整できます。

| **項目**                   | **説明**                                                                                          | **推奨値** |
| -------------------------- | ------------------------------------------------------------------------------------------------ | ---------- |
| **Connection Pool Size**   | ClickHouseサービスとの接続プールで維持可能な同時接続数を指定します。この設定はEMQXとClickHouse間のアクティブ接続数を制御し、アプリケーションのスケーラビリティと性能に影響します。<br/>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、アプリケーションの負荷に依存します。大きすぎるとリソース枯渇、小さすぎるとスループット低下の原因となります。 | `8`        |
| **Clickhouse Timeout**     | ClickHouseサーバーへの接続確立時にコネクターが待機する最大時間（秒）を指定します。<br/>**注意**：適切なタイムアウト設定はシステム性能とリソース利用のバランスに重要です。ネットワーク状況に応じて最適値を検証してください。 | `15`       |
| **Start Timeout**          | 自動起動されたリソースが正常状態になるまでコネクターが待機する最大時間（秒）を指定します。これにより、ClickHouseのデータベースインスタンスなど接続先リソースが完全に稼働準備できるまで操作を進めないようにします。 | `5`        |
| **Buffer Pool Size**       | EMQXとClickHouse間の送信（egress）タイプSinkでデータフローを管理するバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前の一時保存と処理を担当します。ingress（受信）専用のブリッジではこの値を`0`に設定可能です。 | `16`       |
| **Request TTL**            | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこの時間を超えてバッファ内にあるか、送信後にClickHouseからの応答やアックを受け取れない場合、リクエストは期限切れとみなされます。 | `45`       |
| **Health Check Interval**  | コネクターがClickHouse接続のヘルスチェックを自動的に行う間隔（秒）を指定します。 | `15`       |
| **Max Buffer Queue Size**  | ClickHouseコネクターの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはClickHouseへのデータ送信前にデータを一時保存し、データフローを効率化します。システム性能やデータ転送要件に応じて調整してください。 | `256`      |
| **Max Batch Size**         | EMQXからClickHouseへ一度に転送可能なデータバッチの最大サイズを指定します。サイズ調整によりデータ転送の効率と性能を最適化できます。<br />`1`に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1`        |
| **Query Mode**             | メッセージ送信の要件に応じて`asynchronous`または`synchronous`のクエリモードを選択できます。非同期モードではClickHouseへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがClickHouseへの到達前にメッセージを受信する可能性があります。 | `Async`    |
| **Inflight Window**        | 「インフライトクエリ」とは、開始されたがまだ応答やアックを受け取っていないクエリを指します。コネクターがClickHouseと通信中に同時存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode**が`async`の場合、このパラメーターは特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`      |

## さらに詳しく

以下のリンクから詳細情報を参照できます：

**ブログ**：

- [EMQX + ClickHouseによるIoTデータ収集と分析の実現](https://www.emqx.com/en/blog/emqx-and-clickhouse-for-iot-data-access-and-analysis)
- [MQTTからClickHouse統合：リアルタイムIoTデータ分析の推進](https://www.emqx.com/en/blog/mqtt-to-clickhouse-integration)
