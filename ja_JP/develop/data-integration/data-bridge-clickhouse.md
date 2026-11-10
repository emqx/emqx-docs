# ClickHouse に MQTT データを取り込む

[ClickHouse](https://clickhouse.com/) は、高性能なカラム指向の SQL データベース管理システム（DBMS）であり、オンライン分析処理（OLAP）に優れています。大量のデータを低レイテンシで処理・分析することに優れており、優れたクエリ性能、柔軟なデータモデル、スケーラブルな分散アーキテクチャを備えているため、さまざまなデータ分析シナリオに適しています。EMQX は ClickHouse との統合をサポートしており、MQTT メッセージやイベントデータを ClickHouse に取り込んで、さらなる分析や処理を行うことが可能です。

## 動作概要

ClickHouse とのデータ統合は、EMQX に標準搭載された機能であり、MQTT のリアルタイムデータキャプチャおよび送信機能と ClickHouse の強力なデータ処理機能を組み合わせることを目的としています。組み込みの[ルールエンジン](https://docs.emqx.com/en/enterprise/v5.1/data-integration/rules.html)コンポーネントにより、EMQX から ClickHouse へのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQX と ClickHouse 間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/clickhouse_architecture.png" alt="clickhouse_architecture" style="zoom:67%;" />

MQTT データを ClickHouse に取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用 IoT デバイスは MQTT プロトコルを通じて EMQX に正常に接続し、機械やセンサー、製品ラインの稼働状態、計測値、トリガーされたイベントに基づくリアルタイムの MQTT データを EMQX にパブリッシュします。EMQX がこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQX に定義されたルールによって処理されます。ルールは事前定義された条件に基づき、どのメッセージを ClickHouse にルーティングするかを決定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **ClickHouse へのデータ取り込み**：ルールエンジンが ClickHouse への保存対象メッセージを特定すると、メッセージを ClickHouse に転送するアクションがトリガーされます。処理済みデータは ClickHouse データベースのコレクションにシームレスに書き込まれます。
4. **データの保存と活用**：データが ClickHouse に保存された後、企業はそのクエリ性能を活用してさまざまなユースケースに利用できます。例えば、物流やサプライチェーン管理の分野では、GPS トラッカー、温度センサー、在庫管理システムなどの IoT デバイスからのデータをリアルタイムで監視・分析し、追跡、ルート最適化、需要予測、効率的な在庫管理に役立てることが可能です。

## 特長と利点

ClickHouse とのデータ統合は、効率的なデータ送信、保存、活用を実現するための多彩な特長と利点を提供します：

- **リアルタイムデータストリーミング**：EMQX はリアルタイムデータストリームの処理に最適化されており、ソースシステムから ClickHouse への効率的かつ信頼性の高いデータ送信を保証します。即時の洞察とアクションが求められるユースケースに最適です。
- **高性能かつスケーラブル**：EMQX の分散アーキテクチャと ClickHouse のカラム型ストレージ形式により、データ量の増加に応じてシームレスにスケール可能です。大規模データセットでも一貫した性能と応答性を維持します。
- **柔軟なデータ変換**：EMQX は強力な SQL ベースのルールエンジンを提供し、ClickHouse に保存する前にデータを事前処理できます。フィルタリング、ルーティング、集約、強化など多様なデータ変換機能をサポートし、ニーズに合わせてデータを整形可能です。
- **簡単なデプロイと管理**：EMQX はデータソースの設定、データ前処理ルール、ClickHouse の保存設定をユーザーフレンドリーなインターフェースで提供し、データ統合プロセスのセットアップと継続的な管理を簡素化します。
- **高度な分析**：ClickHouse の強力な SQL ベースのクエリ言語と複雑な分析関数のサポートにより、IoT データから価値ある洞察を得られ、予測分析や異常検知などが可能です。

## はじめる前に

このセクションでは、EMQX ダッシュボードで ClickHouse データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- UNIX ターミナルおよびコマンドの基本知識

### ClickHouse サーバーの起動

このセクションでは、[Docker](https://www.docker.com/) を使って ClickHouse サーバーを起動する方法を紹介します。

1. 以下の初期化 SQL 文を含む `init.sql` ファイルを作成します。このファイルはコンテナ起動時にデータベースを初期化するために使用されます。

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

2. 以下のコマンドで ClickHouse サーバーを起動します。このコマンドはデータベース名、ポート番号、ユーザー名、パスワードを指定し、カレントディレクトリの `init.sql` ファイルを Docker 内の対応ディレクトリにマウントします。

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

Docker 上での ClickHouse の実行に関する詳細は、[Docker Hub](https://hub.docker.com/r/clickhouse/clickhouse-server) を参照してください。

## コネクターの作成

このセクションでは、Sink を ClickHouse サーバーに接続するためのコネクターの作成方法を説明します。

以下の手順は、EMQX と ClickHouse の両方をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQX ダッシュボードにログインし、左メニューから **Integration** -> **Connectors** をクリックします。
2. 画面右上の **Create** をクリックします。
3. **Create Connector** ページで **ClickHouse** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の大文字・小文字の組み合わせが推奨されます。例：`my_clickhouse`
   - **Server URL**：`http://127.0.0.1:18123`
   - **Database Name**：`mqtt_data`
   - **Username**：`emqx`
   - **Password**：`public`
   - **Enable TLS**：EMQX 6.3.3 以降で利用可能です。ClickHouse サーバーが HTTPS を使用している場合は有効にします。EMQX はデフォルトでサーバー証明書を検証します。プライベート CA による署名の場合は対応する CA 証明書を設定してください。詳細は[外部リソースアクセスの TLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。
5. 高度な設定（任意）：[高度な設定](#advanced-configurations)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが ClickHouse サーバーに接続できるかテストできます。
7. 画面下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックしてルールと Sink の作成を続行できます。詳細は[ClickHouse Sink を使ったルールの作成](#create-a-rule-with-clickhouse-sink)を参照してください。

## ClickHouse Sink を使ったルールの作成

このセクションでは、Dashboard 上でソース MQTT トピック `t/#` からのメッセージを処理し、処理結果を設定済みの Sink 経由で ClickHouse に転送するルールの作成方法を説明します。

1. EMQX ダッシュボードにアクセスし、左メニューから **Integration** -> **Rules** をクリックします。
2. 画面右上の **Create** をクリックします。
3. ルール ID を入力します。例：`my_rule`
4. SQL エディターに以下のステートメントを入力します。これはトピックパターン `t/#` にマッチする MQTT メッセージを転送します。

   ```sql
   SELECT 
     payload as data,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

   注：初心者の方は **SQL Examples** をクリックし、**Enable Test** を有効にして SQL ルールの学習とテストを行うことができます。

5. + **Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを ClickHouse に送信します。
6. **Type of Action** ドロップダウンリストから `ClickHouse` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既存の ClickHouse Sink を選択することも可能ですが、この例では新規 Sink を作成します。
7. Sink の名前を入力します。英数字の大文字・小文字の組み合わせが推奨されます。
8. **Connector** ドロップダウンから先ほど作成した `my_clickhouse` を選択します。新規コネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメーターの詳細は[コネクターの作成](#create-a-connector)を参照してください。
9. **Batch Value Separator** 設定は互換性のために残されていますが、EMQX 6.3.1 以降は無視されます。バッチモードが有効な場合、EMQX は SQL フォーマットから区切り文字を自動判別します。
10. **SQL Template** に以下のコマンドを入力します：

    ::: warning 重要なお知らせ

    EMQX 6.3.1 以降、Sink 作成時に SQL テンプレートの解析を行い、SQL コンテキストに基づいてプレースホルダーをエスケープし、サポートされていない構文は拒否されます。テンプレートは単一の ClickHouse `INSERT` 文で、`VALUES`、`FORMAT Values`、または `FORMAT JSONCompactEachRow` を使用する必要があります。プレースホルダーは値の位置および文字列リテラル内でのみサポートされます。SQL コメント、追加ステートメント、識別子内のプレースホルダーはサポートされません。

    この検証により、以前のバージョンで受け入れられていたテンプレートが拒否される場合があります。アップグレード前に互換性のないテンプレートを修正してください。

    :::

    ```sql
    INSERT INTO messages(data, arrived) VALUES ('${data}', ${timestamp})
    ```

    ここで `${data}` と `${timestamp}` はそれぞれメッセージの内容とタイムスタンプを表し、後述のルール設定でメッセージ転送時に対応する内容に置き換えられます。

    SQL テンプレート内でプレースホルダー変数が未定義の場合、**SQL Template** 上部の **Undefined Vars as Null** スイッチを切り替えてルールエンジンの動作を設定できます：

    - **無効**（デフォルト）：ルールエンジンは文字列 `undefined` をデータベースに挿入します。
    - **有効**：変数が未定義の場合、ルールエンジンは `NULL` を挿入します。

      ::: tip

      可能な限りこのオプションは常に有効にすべきです。無効にするのは後方互換性を確保する場合のみです。

      :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
12. 高度な設定（任意）：[高度な設定](#advanced-configurations)を参照してください。
13. **Create** をクリックする前に、**Test Connectivity** ボタンをクリックして ClickHouse サーバーへの接続を確認できます。
14. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページに戻ると、新しい Sink が **Action Outputs** タブに表示されます。
15. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを作成します。作成したルールはルール一覧に表示され、**status** は接続済み（connected）となります。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい ClickHouse Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、ClickHouse に送信・保存されている様子が見えます。

## ルールのテスト

EMQX ダッシュボードに組み込まれている WebSocket クライアントを使って、ルールが期待通りに動作するかテストできます。

ダッシュボードの左メニューから **Diagnose** -> **WebSocket Client** をクリックして WebSocket クライアントを開き、以下の手順で設定してトピック `t/test` にメッセージを送信します：

1. 現在の EMQX インスタンスへの接続情報を入力します。ローカルで EMQX を実行している場合、デフォルト値のままで問題ありません（認証を設定している場合はユーザー名とパスワードを入力してください）。
2. **Connect** をクリックしてクライアントを EMQX に接続します。
3. 下にスクロールしてパブリッシュエリアに以下を入力します：
   * **Topic**：`t/test`
   * **Payload**：`Hello World Clickhouse from EMQX`
   * **QoS**：2
4. **Publish** をクリックしてメッセージを送信します。ClickHouse サーバーのデータベース `mqtt_data` のテーブル `messages` にエントリが挿入されているはずです。以下のコマンドをターミナルで実行して確認できます：

   ```bash
   curl -u emqx:public -X POST -d "SELECT * FROM mqtt_data.messages" http://localhost:18123
   ```

5. 正常に動作していれば、以下のような出力が得られます（タイムスタンプは異なります）：

   ```
   Hello World Clickhouse from EMQX        2024-01-17 09:40:06
   ```

## 高度な設定

このセクションでは、EMQX ClickHouse コネクターの高度な設定オプションについて詳しく説明します。ダッシュボードでコネクターを設定する際、**Advanced Settings** に移動して以下のパラメーターをニーズに合わせて調整できます。

| **項目**                   | **説明**                                                                                         | **推奨値**          |
| -------------------------- | ------------------------------------------------------------------------------------------------ | ------------------- |
| **Connection Pool Size**   | ClickHouse サービスとのインターフェース時に維持される接続プールの同時接続数を指定します。この設定は EMQX と ClickHouse 間のアクティブ接続数を制御し、アプリケーションのスケーラビリティと性能管理に役立ちます。<br/>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、アプリケーションのワークロードに依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の原因となります。 | `8`                 |
| **Clickhouse Timeout**     | ClickHouse サーバーへの接続確立を試みる際の最大待機時間（秒）を指定します。<br/>**注意**：適切なタイムアウト設定はシステム性能とリソース利用のバランスに重要です。ネットワーク条件を考慮して最適値を検証してください。 | `15`                |
| **Start Timeout**          | 自動起動されたリソースが正常状態に達するまでの最大待機時間（秒）を指定します。これにより、ClickHouse のデータベースインスタンスなどのリソースが完全に稼働し、データトランザクションを処理可能になるまで操作を進めないようにします。 | `5`                 |
| **Buffer Pool Size**       | EMQX と ClickHouse 間の egress タイプ Sink でデータフローを管理するバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前の一時保管と処理を担当します。ingress（受信）データのみを扱うブリッジではこの値を "0" に設定可能です。 | `16`                |
| **Request TTL**            | バッファに入ったリクエストの有効期限（秒）を指定します。リクエストがこの TTL を超えてバッファ内に滞留するか、ClickHouse からタイムリーな応答やアックが得られない場合、そのリクエストは期限切れとみなされます。 | `45`                |
| **Health Check Interval**  | ClickHouse への接続の自動ヘルスチェックを行う間隔（秒）を指定します。 | `15`                |
| **Max Buffer Queue Size**  | ClickHouse コネクターの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータ送信前の一時保管を担い、システム性能とデータ転送要件に応じて調整してください。 | `256`               |
| **Max Batch Size**         | EMQX から ClickHouse へ一度に転送可能なデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率と性能を最適化できます。<br />"1" に設定すると、データレコードはバッチ化されず個別に送信されます。 | `1`                 |
| **Query Mode**             | メッセージ送信の要件に応じて `asynchronous` または `synchronous` のクエリモードを選択できます。非同期モードでは ClickHouse への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージ到着前に受信する可能性があります。 | `Async`             |
| **Inflight Window**        | 「インフライトクエリ」とは、開始されたがまだ応答やアックを受け取っていないクエリを指します。この設定は ClickHouse との通信時に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async` の場合、このパラメーターは特に重要です。同一 MQTT クライアントからのメッセージを厳密な順序で処理する必要がある場合は、値を 1 に設定してください。 | `100`               |

## さらに詳しく

以下のリンクから詳細情報を確認できます：

**ブログ**：

- [EMQX + ClickHouse による IoT データ収集と分析の実装](https://www.emqx.com/en/blog/emqx-and-clickhouse-for-iot-data-access-and-analysis)
- [MQTT と ClickHouse の統合：リアルタイム IoT データ分析の加速](https://www.emqx.com/en/blog/mqtt-to-clickhouse-integration)
