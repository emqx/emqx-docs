# ClickHouseへのMQTTデータ取り込み

[ClickHouse](https://clickhouse.com/)は、高性能なカラム指向のSQLデータベース管理システム（DBMS）であり、オンライン分析処理（OLAP）に優れています。大量のデータを低レイテンシで処理・分析することに優れており、優れたクエリ性能、柔軟なデータモデル、スケーラブルな分散アーキテクチャを備えているため、さまざまなデータ分析シナリオに適しています。EMQXはClickHouseとの統合をサポートしており、MQTTメッセージやイベントデータをClickHouseに取り込んで、さらなる分析や処理を行うことが可能です。

## 動作概要

ClickHouseデータ統合は、EMQXに標準搭載された機能であり、MQTTのリアルタイムデータ収集・送信機能とClickHouseの強力なデータ処理機能を組み合わせることを目的としています。組み込みの[ルールエンジン](https://docs.emqx.com/en/enterprise/v5.1/data-integration/rules.html)コンポーネントにより、EMQXからClickHouseへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとClickHouse間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/clickhouse_architecture.png" alt="clickhouse_architecture" style="zoom:67%;" />

MQTTデータをClickHouseに取り込む流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、機械、センサー、製造ラインの稼働状態、計測値、トリガーイベントに基づくリアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前に定義された条件に基づき、ClickHouseにルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの拡充などが適用されます。
3. **ClickHouseへのデータ取り込み**：ルールエンジンがClickHouseへの保存対象メッセージを特定すると、メッセージをClickHouseに転送するアクションをトリガーします。処理済みデータはClickHouseデータベースのコレクションにシームレスに書き込まれます。
4. **データの保存と活用**：データがClickHouseに保存されることで、企業はそのクエリ機能を活用し、さまざまなユースケースに対応できます。例えば、物流やサプライチェーン管理分野では、GPSトラッカー、温度センサー、在庫管理システムなどのIoTデバイスからのデータをリアルタイムで監視・分析し、追跡、ルート最適化、需要予測、効率的な在庫管理に役立てることが可能です。

## 特長とメリット

ClickHouseとのデータ統合は、効率的なデータ転送、保存、活用を実現するための多彩な機能とメリットを提供します。

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからClickHouseへの効率的かつ信頼性の高いデータ転送を実現します。即時の洞察とアクションが必要なユースケースに最適です。
- **高性能かつスケーラブル**：EMQXの分散アーキテクチャとClickHouseのカラムナストレージ形式により、データ量の増加に応じてシームレスにスケール可能です。大量データでも一貫した性能と応答性を維持します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、ClickHouseに保存する前にデータの前処理が可能です。フィルタリング、ルーティング、集約、拡充など多様なデータ変換機能をサポートし、ニーズに応じたデータ整形を実現します。
- **簡単なデプロイと管理**：EMQXはデータソースの設定、前処理ルール、ClickHouse保存設定をユーザーフレンドリーなインターフェースで提供し、データ統合プロセスのセットアップと運用管理を簡素化します。
- **高度な分析**：ClickHouseの強力なSQLクエリ言語と複雑な分析関数のサポートにより、IoTデータから価値ある洞察を得ることができ、予測分析や異常検知などを実現します。

## はじめる前に

このセクションでは、EMQXダッシュボードでClickHouseデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- UNIXターミナルおよび基本コマンドの基礎知識

### ClickHouseサーバーの起動

このセクションでは、[Docker](https://www.docker.com/)を使ってClickHouseサーバーを起動する方法を紹介します。

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

2. 以下のコマンドでClickHouseサーバーを起動します。このコマンドはデータベース名、ポート番号、ユーザー名、パスワードを設定し、カレントディレクトリの`init.sql`ファイルをDockerコンテナ内にマウントします。

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

DockerでのClickHouseの実行に関する詳細は、[Docker Hub](https://hub.docker.com/r/clickhouse/clickhouse-server)をご覧ください。

## コネクターの作成

このセクションでは、SinkをClickHouseサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとClickHouseをローカルマシンで実行していることを前提としています。リモート環境で実行している場合は、設定を適宜調整してください。

1. EMQXダッシュボードに入り、左メニューから **Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **ClickHouse** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の大文字・小文字の組み合わせで、例：`my_clickhouse`
   - **Server URL**：`http://127.0.0.1:18123`
   - **Database Name**：`mqtt_data`
   - **Username**：`emqx`
   - **Password**：`public`
5. 詳細設定（任意）：[詳細設定](#advanced-configurations)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがClickHouseサーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** をクリックして、ルールやSinkの作成を続行できます。詳細は[ClickHouse Sink付きルールの作成](#create-a-rule-with-clickhouse-sink)を参照してください。

## ClickHouse Sink付きルールの作成

このセクションでは、ソースMQTTトピック`t/#`からのメッセージを処理し、処理結果を設定済みのClickHouse Sink経由でClickHouseに転送するルールをDashboardで作成する方法を説明します。

1. EMQXダッシュボードの左メニューから **Integration** -> **Rules** をクリックします。
2. ページ右上の **Create** をクリックします。
3. ルールIDを入力します。例：`my_rule`
4. SQLエディタに以下の文を入力します。これはトピックパターン`t/#`にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT 
     payload as data,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

   注：初心者の場合は、**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールの学習とテストが可能です。

5. + **Add Action** ボタンをクリックして、ルール発動時にトリガーされるアクションを定義します。このアクションでEMQXはルールで処理したデータをClickHouseに送信します。
6. **Type of Action** ドロップダウンから `ClickHouse` を選択します。**Action** はデフォルトの `Create Action` のままにします。既存のClickHouse Sinkを選択することも可能ですが、ここでは新規Sinkを作成します。
7. Sink名を入力します。英数字の大文字・小文字の組み合わせで指定してください。
8. **Connector** ドロップダウンから先ほど作成した `my_clickhouse` を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメーターは[コネクター作成](#create-a-connector)を参照してください。
9. **Batch Value Separator** 設定は互換性のため残っていますが、EMQX 6.3.1以降は無視されます。バッチモードが有効な場合、EMQXはSQLフォーマットから区切り文字を自動判別します。
10. **SQL Template** に以下のコマンドを入力します。

    ::: warning 重要なお知らせ

    EMQX 6.3.1以降、Sink作成時にSQLテンプレートの解析を行い、SQLコンテキストに基づいてプレースホルダーのエスケープ処理を行い、サポートされていない構文は拒否されます。テンプレートは単一のClickHouse `INSERT` 文で、`VALUES`、`FORMAT Values`、または`FORMAT JSONCompactEachRow`を使用する必要があります。プレースホルダーは値の位置および文字列リテラル内でのみサポートされます。SQLコメント、追加ステートメント、識別子内のプレースホルダーはサポートされません。

    この検証により、以前のバージョンで受け入れられていたテンプレートが拒否される場合があります。アップグレード前に互換性のないテンプレートを修正してください。

    :::

    ```sql
    INSERT INTO messages(data, arrived) VALUES ('${data}', ${timestamp})
    ```

    ここで`${data}`と`${timestamp}`はそれぞれメッセージ内容とタイムスタンプを表し、後述のルール設定でメッセージ転送時にEMQXが対応する内容に置換します。

    SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL Template**上部の **Undefined Vars as Null** スイッチでルールエンジンの動作を切り替えられます。

    - **無効（デフォルト）**：ルールエンジンは文字列`undefined`をデータベースに挿入します。
    - **有効**：変数が未定義の場合、ルールエンジンは`NULL`を挿入します。

      ::: tip

      可能な限りこのオプションは有効にすべきであり、無効化は後方互換性確保のためのみ推奨されます。

      :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
12. 詳細設定（任意）：[詳細設定](#advanced-configurations)を参照してください。
13. **Create** をクリックする前に、**Test Connectivity** ボタンでClickHouseサーバーへの接続確認が可能です。
14. **Create** ボタンをクリックしてSink設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しいSinkが表示されます。
15. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status**は`connected`となります。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいClickHouse Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを確認できます。トピック`t/#`のメッセージがルール`my_rule`で解析され、ClickHouseに送信・保存されていることが分かります。

## ルールのテスト

EMQXダッシュボードに組み込まれたWebSocketクライアントを使って、ルールが期待通りに動作するかテストできます。

ダッシュボード左メニューの **Diagnose** -> **WebSocket Client** をクリックしてWebSocketクライアントにアクセスし、以下の手順でセットアップしてトピック`t/test`にメッセージを送信します。

1. 現在のEMQXインスタンスの接続情報を入力します。ローカルでEMQXを実行している場合、デフォルト値を使用できます（認証設定を変更している場合はユーザー名・パスワードを入力してください）。
2. **Connect** をクリックしてクライアントをEMQXに接続します。
3. 下にスクロールしてパブリッシュエリアに以下を入力します：
   - **Topic**：`t/test`
   - **Payload**：`Hello World Clickhouse from EMQX`
   - **QoS**：2
4. **Publish** をクリックしてメッセージを送信します。ClickHouseサーバーのデータベース`mqtt_data`のテーブル`messages`にエントリが挿入されているはずです。以下のコマンドをターミナルで実行して確認できます。

   ```bash
   curl -u emqx:public -X POST -d "SELECT * FROM mqtt_data.messages" http://localhost:18123
   ```

5. 正常に動作していれば、以下のような出力が得られます（タイムスタンプは異なります）。

   ```
   Hello World Clickhouse from EMQX        2024-01-17 09:40:06
   ```

## 詳細設定

このセクションでは、EMQX ClickHouseコネクターの詳細設定オプションについて説明します。ダッシュボードでコネクターを設定する際、**Advanced Settings** に移動して以下のパラメーターをニーズに合わせて調整してください。

| **項目**                   | **説明**                                                                                      | **推奨値**           |
| -------------------------- | --------------------------------------------------------------------------------------------- | -------------------- |
| **Connection Pool Size**   | ClickHouseサービスとの接続プールに保持可能な同時接続数を指定します。システムのスケーラビリティと性能管理に役立ちます。<br/>**注意**：適切なプールサイズはシステムリソース、ネットワークレイテンシ、アプリケーションの負荷に依存します。大きすぎるとリソース枯渇、小さすぎるとスループット低下の原因となります。 | `8`                  |
| **Clickhouse Timeout**     | ClickHouseサーバーへの接続確立時にコネクターが待機する最大時間（秒）を指定します。<br/>**注意**：パフォーマンスとリソース利用のバランスを取るため、ネットワーク環境を考慮して最適な値を設定してください。 | `15`                 |
| **Start Timeout**          | 自動起動したリソースが正常状態になるまでコネクターが待機する最大時間（秒）を指定します。接続先リソース（例：ClickHouseのデータベースインスタンス）が完全に稼働し、データ処理可能になるまで処理を進めないようにします。 | `5`                  |
| **Buffer Pool Size**       | EMQXとClickHouse間の送信（egress）タイプSinkでデータフロー管理に割り当てるバッファワーカープロセス数を指定します。これらのワーカーはデータを一時的に保持し処理します。受信（ingress）専用のブリッジでは不要なため「0」に設定可能です。 | `16`                 |
| **Request TTL**            | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。TTLを超えるか、ClickHouseからの応答・アックがタイムアウトした場合、リクエストは期限切れと判断されます。 | `45`                 |
| **Health Check Interval**  | コネクターがClickHouse接続のヘルスチェックを自動実行する間隔（秒）を指定します。 | `15`                 |
| **Max Buffer Queue Size**  | ClickHouseコネクターの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータ転送前の一時保管を担い、システム性能やデータ転送要件に応じて調整してください。 | `256`                |
| **Max Batch Size**         | EMQXからClickHouseへ一度に転送可能なデータバッチの最大サイズを指定します。サイズ調整により転送効率や性能を最適化できます。<br />「1」に設定すると、データはバッチ化せず個別に送信されます。 | `1`                  |
| **Query Mode**             | メッセージ送信要件に応じて、`asynchronous`（非同期）または`synchronous`（同期）クエリモードを選択できます。非同期モードではClickHouseへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがClickHouse到着前にメッセージを受信する可能性があります。 | `Async`              |
| **Inflight Window**        | 「インフライトクエリ」とは、開始されたが応答やアックをまだ受け取っていないクエリを指します。コネクターがClickHouseと通信する際に同時に存在可能な最大インフライトクエリ数を制御します。<br/>**Query Mode**が`async`の場合、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合はこの値を1に設定してください。 | `100`                |

## 参考情報

以下のリンクもご参照ください。

**ブログ**：

- [EMQX + ClickHouseによるIoTデータ収集と分析の実現](https://www.emqx.com/en/blog/emqx-and-clickhouse-for-iot-data-access-and-analysis)
- [MQTTからClickHouseへの統合：リアルタイムIoTデータ分析の推進](https://www.emqx.com/en/blog/mqtt-to-clickhouse-integration)
