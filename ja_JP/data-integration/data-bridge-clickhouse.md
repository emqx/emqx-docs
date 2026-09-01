# ClickHouseへのMQTTデータ取り込み

[ClickHouse](https://clickhouse.com/)は、高性能なカラム指向のSQLデータベース管理システム（DBMS）であり、オンライン分析処理（OLAP）に優れています。大量のデータを低レイテンシで処理・分析することに長けており、優れたクエリ性能、柔軟なデータモデル、スケーラブルな分散アーキテクチャを備えているため、さまざまなデータ分析シナリオに適しています。EMQXはClickHouseとの統合をサポートしており、MQTTメッセージやイベントデータをClickHouseに取り込んで、さらなる分析や処理を行うことが可能です。

## 動作概要

ClickHouseとのデータ統合は、EMQXに標準搭載された機能であり、MQTTのリアルタイムデータキャプチャおよび送信機能とClickHouseの強力なデータ処理機能を組み合わせることを目的としています。組み込みの[ルールエンジン](https://docs.emqx.com/en/enterprise/v5.1/data-integration/rules.html)コンポーネントにより、EMQXからClickHouseへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとClickHouse間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/clickhouse_architecture.png" alt="clickhouse_architecture" style="zoom:67%;" />

MQTTデータのClickHouseへの取り込みは以下のように動作します。

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、機械、センサー、製品ラインの稼働状態、計測値、またはトリガーされたイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータ処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づいて、どのメッセージをClickHouseにルーティングするかを決定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **ClickHouseへのデータ取り込み**：ルールエンジンがClickHouseへの保存対象メッセージを特定すると、メッセージをClickHouseに転送するアクションをトリガーします。処理済みデータはClickHouseデータベースのコレクションにシームレスに書き込まれます。
4. **データの保存と活用**：データがClickHouseに保存されることで、企業はそのクエリ性能を活用してさまざまなユースケースに対応できます。例えば、物流やサプライチェーン管理分野では、GPSトラッカー、温度センサー、在庫管理システムなどのIoTデバイスからのデータをリアルタイムで監視・分析し、追跡、ルート最適化、需要予測、効率的な在庫管理に役立てることが可能です。

## 特長と利点

ClickHouseとのデータ統合は、効率的なデータ送信、保存、活用を実現するために以下のような特長と利点を提供します。

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからClickHouseへの効率的かつ信頼性の高いデータ送信を保証します。これにより、即時のインサイトやアクションが必要なユースケースに最適です。
- **高性能かつスケーラブル**：EMQXの分散アーキテクチャとClickHouseのカラムナーストレージ形式により、データ量の増加に応じてシームレスにスケール可能です。大規模データセットでも一貫した性能と応答性を維持します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、ClickHouseに保存する前にデータを前処理できます。フィルタリング、ルーティング、集計、強化など多様なデータ変換機能をサポートし、ニーズに応じたデータ整形が可能です。
- **簡単なデプロイと管理**：EMQXはデータソースの設定、前処理ルール、ClickHouse保存設定のためのユーザーフレンドリーなインターフェースを提供し、データ統合プロセスのセットアップと継続的な管理を簡素化します。
- **高度な分析機能**：ClickHouseの強力なSQLクエリ言語と複雑な分析関数のサポートにより、IoTデータから価値あるインサイトを得ることができ、予測分析や異常検知などに活用可能です。

## はじめる前に

このセクションでは、EMQXダッシュボードでClickHouseデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- UNIXターミナルとコマンドの基本知識

### ClickHouseサーバーの起動

このセクションでは、[Docker](https://www.docker.com/)を使用してClickHouseサーバーを起動する方法を紹介します。

1. 以下の初期化SQLステートメントを含む`init.sql`ファイルを作成します。このファイルはコンテナ起動時にデータベースを初期化するために使用されます。

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

2. 以下のコマンドでClickHouseサーバーを起動します。このコマンドはデータベース名、ポート番号、ユーザー名、パスワードを定義し、カレントディレクトリの`init.sql`ファイルをDockerコンテナ内にマウントします。

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

Docker上でのClickHouseの実行に関する詳細は、[dockerhub](https://hub.docker.com/r/clickhouse/clickhouse-server)をご参照ください。

## コネクターの作成

このセクションでは、SinkをClickHouseサーバーに接続するためのコネクターの作成方法を説明します。

以下の手順は、EMQXとClickHouseの両方をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、左メニューから **Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **ClickHouse** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_clickhouse`
   - **Server URL**：`http://127.0.0.1:18123`
   - **Database Name**：`mqtt_data`
   - **Username**：`emqx`
   - **Password**：`public`
5. 詳細設定（任意）：[高度な設定](#advanced-configurations)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがClickHouseサーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** を選択して、ルールとSinkの作成を続けることができます。詳細は[ClickHouse Sink付きルールの作成](#create-a-rule-with-clickhouse-sink)を参照してください。

## ClickHouse Sink付きルールの作成

このセクションでは、ソースMQTTトピック `t/#` からのメッセージを処理し、処理結果を設定済みのSinkを介してClickHouseに転送するルールの作成方法を説明します。

1. EMQXダッシュボードで、左メニューから **Integration** -> **Rules** をクリックします。
2. ページ右上の **Create** をクリックします。
3. ルールIDを入力します。例：`my_rule`
4. SQLエディタに以下のステートメントを入力します。これはトピックパターン `t/#` にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT 
     payload as data,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

   ※ 初心者の方は、**SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストが可能です。

5. + **Add Action** ボタンをクリックして、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをClickHouseに送信します。
6. **Type of Action** ドロップダウンから `ClickHouse` を選択します。**Action** はデフォルトの `Create Action` のままにします。既存のClickHouse Sinkを選択することもできますが、この例では新しいSinkを作成します。
7. Sink名を入力します。大文字・小文字の英数字の組み合わせで指定してください。
8. **Connector** ドロップダウンから先ほど作成した `my_clickhouse` を選択します。隣のボタンから新規コネクターを作成することも可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。
9. **Batch Value Separator** はデフォルトの `,` のままにします。これは複数の入力項目を区切るための設定で、[バッチモード](./data-bridges.md)を有効にし、ClickHouseのFORMAT構文で別の形式を指定する場合のみ変更が必要です。
10. SQLテンプレートに以下のコマンドを入力します（[ルールエンジン](./rules.md)を使って、SQLインジェクションを防ぐために入力SQLの文字列が適切にエスケープされていることを確認してください）：

    ```sql
    INSERT INTO messages(data, arrived) VALUES ('${data}', ${timestamp})
    ```

    ここで `${data}` と `${timestamp}` はメッセージ内容とタイムスタンプを表し、後でルールでメッセージ転送用に設定されます。EMQXは転送前にこれらを対応する内容に置換します。

    SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL template** 上部の **Undefined Vars as Null** スイッチを切り替えてルールエンジンの動作を設定できます：

    - **無効（デフォルト）**：ルールエンジンは文字列 `undefined` をデータベースに挿入します。
    - **有効**：変数が未定義の場合、ルールエンジンは `NULL` を挿入します。

      ::: tip

      可能な限りこのオプションは有効にしてください。無効にするのは後方互換性を保つ場合のみです。

      :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
12. 詳細設定（任意）：[高度な設定](#advanced-configurations)を参照してください。
13. **Create** をクリックする前に、**Test Connectivity** ボタンを押してClickHouseサーバーへの接続を確認できます。
14. **Create** ボタンをクリックしてSink設定を完了します。**Create Rule** ページの **Action Outputs** タブに新しいSinkが表示されます。
15. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status** は `connected` となります。

これでルールの作成が完了し、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいClickHouse Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、ClickHouseに送信・保存されていることがわかります。

## ルールのテスト

EMQXダッシュボードに組み込まれたWebSocketクライアントを使って、ルールが期待通りに動作するかテストできます。

ダッシュボードの左メニューから **Diagnose** -> **WebSocket Client** をクリックしてWebSocketクライアントにアクセスし、以下の手順でクライアントを設定し、トピック `t/test` にメッセージを送信します。

1. 現在のEMQXインスタンスの接続情報を入力します。ローカルでEMQXを実行している場合、デフォルト値を使用できます（認証設定を変更している場合はユーザー名とパスワードを入力してください）。
2. **Connect** をクリックしてクライアントをEMQXに接続します。
3. 下にスクロールしてパブリッシュエリアに以下を入力します：
   - **Topic**：`t/test`
   - **Payload**：`Hello World Clickhouse from EMQX`
   - **QoS**：2
4. **Publish** をクリックしてメッセージを送信します。ClickHouseサーバーのデータベース `mqtt_data` のテーブル `messages` にエントリが挿入されているはずです。以下のコマンドで確認できます。

   ```bash
   curl -u emqx:public -X POST -d "SELECT * FROM mqtt_data.messages" http://localhost:18123
   ```

5. 正常に動作していれば、上記コマンドは以下のような出力を返します（タイムスタンプは異なります）。

   ```
   Hello World Clickhouse from EMQX        2024-01-17 09:40:06
   ```

## 高度な設定

このセクションでは、EMQX ClickHouseコネクターの高度な設定オプションについて詳しく説明します。ダッシュボードでコネクターを設定する際、**Advanced Settings** に移動して以下のパラメータをニーズに合わせて調整してください。

| **項目**                   | **説明**                                                                                         | **推奨値**           |
| -------------------------- | ------------------------------------------------------------------------------------------------ | -------------------- |
| **Connection Pool Size**   | ClickHouseサービスと接続する際に維持できる同時接続数を指定します。このオプションはEMQXとClickHouse間のアクティブな接続数を制限または増加させることで、アプリケーションのスケーラビリティと性能を管理します。<br/>**注意**：適切な接続プールサイズは、システムリソース、ネットワークレイテンシ、アプリケーションの特定のワークロードに依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の原因となります。 | `8`                  |
| **Clickhouse Timeout**     | ClickHouseサーバーへの接続確立を試みる際の最大待機時間（秒）を指定します。<br/>**注意**：パフォーマンスとリソース利用のバランスを取るために適切なタイムアウト設定が重要です。ネットワーク環境に応じて最適値をテストしてください。 | `15`                 |
| **Start Timeout**          | 自動起動されたリソースが正常状態になるまで待機する最大時間（秒）を指定します。これにより、ClickHouseのデータベースインスタンスなどの接続リソースが完全に稼働し、データ処理準備が整うまで操作を進めないようにします。 | `5`                  |
| **Buffer Pool Size**       | EMQXとClickHouse間のエグレス型Sinkでデータフローを管理するために割り当てられるバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前に一時的にデータを保持・処理します。インゲレス（受信）データのみを扱うブリッジではこの値を `0` に設定できます。 | `16`                 |
| **Request TTL**            | バッファに入ったリクエストが有効とみなされる最大期間（秒）を指定します。リクエストがこの期間を超えてバッファに滞留するか、ClickHouseからの応答やアックがタイムリーに得られない場合、リクエストは期限切れと見なされます。 | `45`                 |
| **Health Check Interval**  | ClickHouseへの接続状態を自動的に監視するヘルスチェックの実行間隔（秒）を指定します。 | `15`                 |
| **Max Buffer Queue Size**  | ClickHouseコネクターの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータ送信前に一時的にデータを保持し、データフローを効率化します。システム性能やデータ転送要件に応じて調整してください。 | `256`                |
| **Max Batch Size**         | EMQXからClickHouseへ一度に転送可能なデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率と性能を最適化できます。<br />`1` に設定すると、データレコードはバッチ化されず個別に送信されます。 | `1`                  |
| **Query Mode**             | メッセージ送信の要件に応じて `asynchronous` または `synchronous` のクエリモードを選択できます。非同期モードではClickHouseへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがClickHouseへの到着前にメッセージを受信する可能性があります。 | `Async`              |
| **Inflight Window**        | 「インフライトクエリ」とは、開始されたがまだ応答やアックを受け取っていないクエリを指します。この設定はClickHouseと通信中に同時に存在できるインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async` の場合、このパラメータは特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は、この値を `1` に設定してください。 | `100`                |

## 参考情報

以下のリンクからさらに詳しく学べます：

**ブログ**：

- [EMQX + ClickHouseによるIoTデータ収集と分析の実装](https://www.emqx.com/en/blog/emqx-and-clickhouse-for-iot-data-access-and-analysis)
- [MQTTからClickHouseへの統合：リアルタイムIoTデータ分析を加速](https://www.emqx.com/en/blog/mqtt-to-clickhouse-integration)
