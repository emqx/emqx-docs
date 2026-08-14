# MQTTデータをCouchbaseに取り込む

[Couchbase](https://couchbase.com/) は、SQLやACIDトランザクションなどリレーショナルデータベースの強みと、JSONの柔軟性を兼ね備えた多目的分散データベースです。高いパフォーマンスとスケーラビリティを基盤に構築されており、ユーザープロファイル、動的な製品カタログ、生成AIアプリケーション、ベクター検索、高速キャッシュなど、さまざまな業界で広く利用されています。

## 動作概要

Couchbaseとのデータ統合は、EMQXに標準搭載された機能で、MQTTのリアルタイムデータ収集・送信能力とCouchbaseの強力なデータ処理機能を組み合わせます。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからCouchbaseへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

下図は、EMQXとCouchbase間の典型的なデータ統合アーキテクチャを示しています。

![couchbase_architecture](./assets/couchbase_architecture.png)

MQTTデータをCouchbaseに取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに接続し、機械やセンサー、製品ラインの稼働状況、計測値、トリガーイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXで定義されたルールに基づいて処理されます。ルールは事前定義された条件により、どのメッセージをCouchbaseにルーティングするかを決定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Couchbaseへのデータ取り込み**：ルールエンジンがCouchbaseへの保存対象メッセージを特定すると、メッセージをCouchbaseに転送するアクションをトリガーします。処理済みデータはCouchbaseデータセットにシームレスに書き込まれます。
4. **データの保存と活用**：データがCouchbaseに保存されることで、企業はクエリ機能を活用し、動的製品カタログの管理やリアルタイム在庫更新、パーソナライズされたレコメンデーションの提供など、多様なユースケースに活用できます。これにより購買体験が向上し、売上増加に寄与します。

## 特長とメリット

Couchbaseとのデータ統合は、効率的なデータ送信、保存、活用を実現するための多彩な特長とメリットを提供します：

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからCouchbaseへの効率的かつ信頼性の高いデータ送信を保証します。即時のインサイトやアクションが必要なユースケースに適しています。
- **高性能とスケーラビリティ**：EMQXの分散アーキテクチャとCouchbaseのカラム型ストレージ形式により、データ量増加に伴うスムーズなスケーラビリティを実現します。大規模データセットでも一貫したパフォーマンスと応答性を維持します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、Couchbaseに保存する前にデータを前処理できます。フィルタリング、ルーティング、集約、強化など多様な変換機能により、ニーズに応じたデータ整形が可能です。
- **簡単なデプロイと管理**：EMQXはデータソース設定、前処理ルール、Couchbase保存設定をユーザーフレンドリーなインターフェースで提供し、データ統合のセットアップと運用管理を簡素化します。
- **高度な分析機能**：Couchbaseの強力なSQLベースクエリ言語と複雑な分析機能により、IoTデータから価値あるインサイトを得られます。予測分析や異常検知なども可能です。

## はじめる前に

このセクションでは、EMQXダッシュボードでCouchbaseデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)についての知識
- [データ統合](./data-bridges.md)の知識
- UNIXターミナルとコマンドの基本知識

### Couchbaseサーバーの起動

このセクションでは、[Docker](https://www.docker.com/)を使ってCouchbaseサーバーを起動する方法を紹介します。

1. 以下のコマンドでCouchbaseサーバーを起動します。

   ```bash
   docker run -t --name db -p 8091-8096:8091-8096 -p 11210-11211:11210-11211 couchbase/server:enterprise-7.2.0
   ```
   
   コマンド実行時にDockerがCouchbase Serverをダウンロード・インストールします。Docker仮想環境でCouchbase Serverが起動すると、以下のメッセージが表示されます：
   
   ```
   Starting Couchbase Server -- Web UI available at http://<ip>:8091
   and logs available in /opt/couchbase/var/lib/couchbase/logs
   ```
   
2. ブラウザで `http://localhost:8091` にアクセスし、Couchbase Webコンソールを開きます。

<img src="./assets/couchbase-consoleSetup.png" alt="Couchbaseコンソールセットアップ" style="zoom:67%;" />

3. **Setup New Cluster** をクリックし、クラスター名を入力します。初期設定として、管理者のユーザー名とパスワードをそれぞれ `admin` と `password` に設定します。

<img src="./assets/couchbase-consoleNewCluster.png" alt="Couchbase新規クラスター作成" style="zoom:67%;" />

4. 利用規約に同意し、**Finish with Defaults** をクリックしてデフォルト値で設定を完了します。

5. 設定入力後、右下の **Save & Finish** ボタンをクリックします。これによりサーバーが設定され、Couchbase Webコンソールのダッシュボードが表示されます。左側のナビゲーションパネルで **Buckets** を選択し、**ADD BUCKET** ボタンをクリックします。

   <img src="./assets/couchbase-consoleBuckets.png" alt="Couchbaseバケット一覧" style="zoom:67%;" />

6. バケット名を入力します（例：`emqx`）し、**Create** をクリックしてバケットを作成します。

7. デフォルトコレクションに対してプライマリインデックスを作成します：

    ```
    docker exec -t db /opt/couchbase/bin/cbq -u admin -p password -engine=http://127.0.0.1:8091/ -script "create primary index on default:emqx_data._default._default;"
    ```

DockerでのCouchbase実行に関する詳細は、[公式ドキュメントページ](https://docs.couchbase.com/server/current/getting-started/do-a-quick-install.html)をご参照ください。

## コネクターの作成

このセクションでは、SinkをCouchbaseサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとCouchbaseをローカルマシンで実行していることを前提としています。リモート環境で実行している場合は、設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Couchbase** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_couchbase`
   - **Server Host**：`127.0.0.1`
   - **Username**：`admin`
   - **Password**：`password`
5. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations) を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてCouchbaseサーバーへの接続確認ができます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** を選択できます。ルールとSinkの作成手順は、[Create a Rule with Couchbase Sink](#create-a-rule-with-couchbase-sink) を参照してください。

## Couchbase Sinkを使ったルールの作成

このセクションでは、EMQXダッシュボードで、ソースMQTTトピック `t/#` からのメッセージを処理し、処理結果を設定済みのSink経由でCouchbaseに転送するルールの作成方法を説明します。

1. EMQXダッシュボードで、左ナビゲーションメニューから **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します（例：`my_rule`）。

4. SQLエディターのステートメントはそのままにしておきます。これはトピックパターン `t/#` にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

5. + **Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。このアクションでEMQXはルールで処理したデータをCouchbaseに送信します。

6. **Type of Action** ドロップダウンリストから `Couchbase` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既存のCouchbase Sinkがあれば選択可能です。この例では新規Sinkを作成します。

7. Sinkの名前を入力します。大文字・小文字の英数字の組み合わせで指定してください。

8. **Connector** ドロップダウンから先ほど作成した `my_couchbase` を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[Create a Connector](#create-a-connector)を参照してください。

9. SQLテンプレートに以下を入力します：

    ```sql
    insert into emqx_data (key, value) values (${.id}, ${.payload})
    ```

    ここで `${.id}` と `${.payload}` はそれぞれMQTTメッセージのIDとペイロードを表し、EMQXが転送前に対応する内容に置き換えます。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

11. **詳細設定（任意）**：[Advanced Configurations](#advanced-configurations) を参照してください。

12. **Create** をクリックする前に、**Test Connectivity** ボタンでCouchbaseサーバーへの接続確認が可能です。

13. **Create** ボタンをクリックしてSinkの設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しいSinkが表示されます。

14. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status** は接続済みとなります。

これでルールの作成が完了し、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいCouchbase Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Couchbaseに送信・保存されている様子が見られます。

## ルールのテスト

EMQXダッシュボード内蔵のWebSocketクライアントを使って、ルールが期待通り動作するかテストできます。

ダッシュボード左ナビゲーションメニューの **Diagnose** -> **WebSocket Client** をクリックしてWebSocketクライアントを開き、以下の手順でトピック `t/test` にメッセージを送信します：

1. 現在のEMQXインスタンスの接続情報を入力します。ローカルでEMQXを実行している場合は、デフォルト値のままで問題ありません（認証設定を変更している場合はユーザー名・パスワードを入力してください）。

2. **Connect** をクリックしてクライアントをEMQXに接続します。

3. 下にスクロールしてパブリッシュエリアに以下を入力します：
   * **Topic**：`t/test`
   * **Payload**：`Hello World Couchbase from EMQX`
   * **QoS**：2
   
4. **Publish** をクリックしてメッセージを送信します。Couchbaseサーバーの `emqx_data` バケットにアイテムが挿入されているはずです。以下のコマンドをターミナルで実行して確認できます：

   ```bash
   docker exec -t db /opt/couchbase/bin/cbq -u admin -p password -engine=http://127.0.0.1:8091/ -script "SELECT * FROM emqx_data._default._default LIMIT 5;"
   ```

5. 正常に動作していれば、以下のような結果が表示されます（`requestID` やメトリクスは異なる場合があります）：

    ```
    {
        "requestID": "88be238c-5b63-453d-ac16-c0368a5be2bc",
        "signature": {
            "*": "*"
        },
       "results": [
       {
           "_default": "Hello World Couchbase from EMQX"
       }
       ],
       "status": "success",
       "metrics": {
           "elapsedTime": "3.189125ms",
           "executionTime": "3.098709ms",
           "resultCount": 1,
           "resultSize": 61,
           "serviceLoad": 2
       }
   }
   ```

## 詳細設定

このセクションでは、EMQX Couchbaseコネクターの詳細設定オプションについて説明します。ダッシュボードでコネクターを設定する際、**Advanced Settings** にて以下のパラメータを用途に応じて調整できます。

| **項目**                   | **説明**                                                                                          | **推奨値**             |
| -------------------------- | ------------------------------------------------------------------------------------------------ | ---------------------- |
| **HTTP Pipelining**        | サーバーに対して個別の応答を待たずに連続して送信可能なHTTPリクエストの最大数を指定します。<br />`1`に設定すると、従来のリクエスト-レスポンスモデルとなり、各リクエスト送信後に応答を待ってから次のリクエストを送信します。より大きな値は複数リクエストをバッチ送信し、ネットワークリソースを効率的に活用し、往復時間を短縮します。 | `100`                  |
| **Connection Pool Size**   | Couchbaseサービスとの接続プールで維持可能な同時接続数を指定します。<br/>システムリソースやネットワークレイテンシ、アプリケーションの負荷に応じて適切な値を設定してください。大きすぎるとリソース枯渇の恐れがあり、小さすぎるとスループットが制限されます。 | `8`                    |
| **Connect Timeout**        | Couchbaseサーバーへの接続確立を試みる際の最大待機時間（秒）を指定します。<br/>システム性能とリソース利用のバランスを考慮し、ネットワーク状況に応じて最適な値を設定してください。 | `15`                   |
| **Start Timeout**          | 自動起動したリソースが正常な状態になるまで待機する最大時間（秒）を指定します。これにより、Couchbaseのデータベースインスタンスなど接続先リソースが完全に稼働してから処理を進めることが保証されます。 | `5`                    |
| **Health Check Interval**  | Couchbaseとの接続状態を自動的に監視するヘルスチェックの実行間隔（秒）を指定します。 | `15`                   |
