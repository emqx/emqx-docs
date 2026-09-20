# CouchbaseへのMQTTデータ取り込み

[Couchbase](https://couchbase.com/)は、SQLやACIDトランザクションなどリレーショナルデータベースの強みと、JSONの柔軟性を兼ね備えた多目的分散データベースです。高いパフォーマンスとスケーラビリティを基盤に構築されており、ユーザープロファイル、動的な製品カタログ、生成AIアプリケーション、ベクター検索、高速キャッシュなど、さまざまな業界で広く利用されています。

## 動作概要

Couchbaseとのデータ統合は、EMQXに標準搭載された機能であり、MQTTのリアルタイムデータ収集・送信能力とCouchbaseの強力なデータ処理機能を組み合わせます。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからCouchbaseへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとCouchbase間のデータ統合の典型的なアーキテクチャを示しています。

![couchbase_architecture](./assets/couchbase_architecture.png)

CouchbaseへのMQTTデータ取り込みは以下のように動作します：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、機械、センサー、製品ラインの稼働状態、計測値、またはトリガーイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
3. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールに従って処理されます。ルールは事前定義された条件に基づき、どのメッセージをCouchbaseにルーティングするかを決定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
4. **Couchbaseへのデータ取り込み**：ルールエンジンがCouchbaseへの保存対象メッセージを特定すると、Couchbaseへの転送アクションがトリガーされます。処理済みデータはCouchbaseデータベースのデータセットにシームレスに書き込まれます。
5. **データの保存と活用**：データがCouchbaseに保存されることで、企業は様々なユースケースに対してクエリ機能を活用できます。例えば動的な製品カタログの文脈では、Couchbaseを用いて製品情報の効率的な管理・取得、リアルタイム在庫更新のサポート、顧客へのパーソナライズされた推奨の提供などが可能となり、購買体験の向上と売上増加に寄与します。

## 特長と利点

Couchbaseとのデータ統合は、効率的なデータ送信、保存、活用を実現するための多彩な特長と利点を提供します：

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に特化しており、ソースシステムからCouchbaseへの効率的かつ信頼性の高いデータ送信を保証します。即時の洞察やアクションを必要とするユースケースに最適です。
- **高性能かつスケーラブル**：EMQXの分散アーキテクチャとCouchbaseのカラムナストレージ形式により、データ量の増加に応じてシームレスにスケール可能です。大規模データセットでも一貫したパフォーマンスと応答性を維持します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、Couchbaseに保存する前のデータ前処理を可能にします。フィルタリング、ルーティング、集約、強化など多様な変換機能をサポートし、ニーズに応じてデータを最適化できます。
- **簡単なデプロイと管理**：EMQXはデータソース設定、データ前処理ルール、Couchbase保存設定のためのユーザーフレンドリーなインターフェースを提供し、データ統合プロセスのセットアップと運用管理を容易にします。
- **高度な分析機能**：Couchbaseの強力なSQLベースクエリ言語と複雑な分析関数のサポートにより、IoTデータから価値あるインサイトを得ることができ、予測分析や異常検知などの高度な分析が可能です。

## はじめる前に

このセクションでは、EMQXダッシュボードでCouchbaseデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXのデータ統合[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- UNIXターミナルとコマンドの基本知識

### Couchbaseサーバーの起動

このセクションでは、[Docker](https://www.docker.com/)を使用してCouchbaseサーバーを起動する方法を紹介します。

1. 以下のコマンドでCouchbaseサーバーを起動します。

   ```bash
   docker run -t --name db -p 8091-8096:8091-8096 -p 11210-11211:11210-11211 couchbase/server:enterprise-7.2.0
   ```
   
   コマンド実行時にDockerがCouchbase Serverをダウンロードおよびインストールします。Docker仮想環境でCouchbase Serverが起動すると、以下のメッセージが表示されます：
   
   ```
   Starting Couchbase Server -- Web UI available at http://<ip>:8091
   and logs available in /opt/couchbase/var/lib/couchbase/logs
   ```
   
2. ブラウザで `http://localhost:8091` にアクセスし、Couchbase Webコンソールを開きます。

<img src="./assets/couchbase-consoleSetup.png" alt="Couchbaseコンソールセットアップ" style="zoom:67%;" />

3. **Setup New Cluster** をクリックし、クラスター名を入力します。初期設定のため、管理者のユーザー名とパスワードをそれぞれ `admin` と `password` に設定してください。

<img src="./assets/couchbase-consoleNewCluster.png" alt="Couchbase新規クラスター作成" style="zoom:67%;" />

4. 利用規約に同意し、**Finish with Defaults** をクリックしてデフォルト値で設定を完了します。

5. 設定入力が完了したら、右下の **Save & Finish** ボタンをクリックします。これによりサーバーが設定され、Couchbase Webコンソールのダッシュボードが表示されます。左側のナビゲーションパネルで **Buckets** を選択し、**ADD BUCKET** ボタンをクリックします。

   <img src="./assets/couchbase-consoleBuckets.png" alt="Couchbaseバケット設定" style="zoom:67%;" />

7. バケット名（例：`emqx`）を入力し、**Create** をクリックしてバケットを作成します。

8. デフォルトコレクションに対してプライマリインデックスを作成します：

    ```
    docker exec -t db /opt/couchbase/bin/cbq -u admin -p password -engine=http://127.0.0.1:8091/ -script "create primary index on default:emqx_data._default._default;"
    ```

DockerでのCouchbase実行に関する詳細は、[公式ドキュメントページ](https://docs.couchbase.com/server/current/getting-started/do-a-quick-install.html)をご参照ください。

## コネクターの作成

このセクションでは、SinkをCouchbaseサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとCouchbaseの両方をローカルマシンで実行していることを前提としています。リモートで実行している場合は、設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Couchbase** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_couchbase`
   - **Server Host**：`127.0.0.1`
   - **Username**：`admin`
   - **Password**：`password`
5. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがCouchbaseサーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** を選択できます。ルールとSinkの作成手順は[Create a Rule with Couchbase Sink](#create-a-rule-with-couchbase-sink)を参照してください。

## Couchbase Sinkを使ったルールの作成

このセクションでは、ソースMQTTトピック `t/#` からのメッセージを処理し、処理結果を設定済みのSink経由でCouchbaseに転送するルールをダッシュボードで作成する方法を説明します。

1. EMQXダッシュボードで、左側ナビゲーションメニューから **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します（例：`my_rule`）。

4. SQLエディタのステートメントはそのままにしておきます。これはトピックパターン `t/#` にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

5. + **Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをCouchbaseに送信します。

6. **Type of Action** ドロップダウンリストから `Couchbase` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みのCouchbase Sinkがあれば選択可能です。この例では新規Sinkを作成します。

7. Sinkの名前を入力します。名前は大文字・小文字の英数字の組み合わせにしてください。

8. **Connector** ドロップダウンから先ほど作成した `my_couchbase` を選択します。新規コネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータは[Create a Connector](#create-a-connector)を参照してください。

9. SQLテンプレートに以下のコマンドを入力します：

    ```sql
    insert into emqx_data (key, value) values (${.id}, ${.payload})
    ```

    ここで `${.id}` と `${.payload}` はそれぞれMQTTメッセージのIDとペイロードを表し、EMQXが転送前に対応する内容に置き換えます。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

11. **詳細設定（任意）**：[Advanced Configurations](#advanced-configurations)を参照してください。

12. **Create** をクリックする前に、**Test Connectivity** ボタンを押してCouchbaseサーバーへの接続を確認できます。

13. **Create** ボタンをクリックしてSink設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しいSinkが表示されます。

14. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status** は接続済みとなります。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいCouchbase Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Couchbaseに送信・保存されている様子が確認できます。

## ルールのテスト

EMQXダッシュボードに組み込まれたWebSocketクライアントを使って、ルールが期待通りに動作するかテストできます。

ダッシュボードの左側ナビゲーションメニューから **Diagnose** -> **WebSocket Client** をクリックしてWebSocketクライアントを開き、以下の手順でセットアップし、トピック `t/test` にメッセージを送信します：

1. 現在のEMQXインスタンスへの接続情報を入力します。ローカルでEMQXを実行している場合、デフォルト値を使用できます（ただし認証設定を変更している場合はユーザー名・パスワードの入力が必要です）。

2. **Connect** をクリックしてクライアントをEMQXインスタンスに接続します。

3. 下にスクロールしてパブリッシュエリアに以下を入力します：
   * **Topic**：`t/test`
   * **Payload**：`Hello World Couchbase from EMQX`
   * **QoS**：2
   
4. **Publish** をクリックしてメッセージを送信します。Couchbaseサーバーの `emqx_data` バケットにアイテムが挿入されているはずです。以下のコマンドをターミナルで実行して確認できます：

   ```bash
   docker exec -t db /opt/couchbase/bin/cbq -u admin -p password -engine=http://127.0.0.1:8091/ -script "SELECT * FROM emqx_data._default._default LIMIT 5;"
   ```

5. 正常に動作していれば、上記コマンドは以下のような結果を表示します（`requestID`やメトリクスは異なる場合があります）：

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

このセクションでは、EMQX Couchbaseコネクターの詳細設定オプションについて説明します。ダッシュボードでコネクターを設定する際、**Advanced Settings** に進み、以下のパラメータをニーズに合わせて調整してください。

| **項目**                  | **説明**                                                                                         | **推奨値**           |
| ------------------------- | ------------------------------------------------------------------------------------------------ | --------------------- |
| **HTTP Pipelining**       | サーバーに対して、個別のレスポンスを待たずに連続して送信可能なHTTPリクエストの最大数を指定します。<br />値は正の整数で、`1`の場合は従来のリクエスト-レスポンスモデルとなり、各リクエスト送信後にレスポンスを待ちます。値を大きくすることで複数リクエストをバッチ送信でき、ネットワークリソースの効率的利用とラウンドトリップ時間の短縮が可能です。 | `100`              |
| **Connection Pool Size**  | Couchbaseサービスとの接続プールで維持可能な同時接続数を指定します。<br />システムリソース、ネットワークレイテンシ、アプリケーションのワークロードに応じて適切な値を設定してください。大きすぎるとリソース枯渇、小さすぎるとスループット制限の原因となります。 | `8`                   |
| **Connect Timeout**       | Couchbaseサーバーへの接続確立を試みる際の最大待機時間（秒）を指定します。<br />システムパフォーマンスとリソース利用のバランスを考慮し、ネットワーク環境に応じて最適な値を設定してください。 | `15`                  |
| **Start Timeout**         | 自動起動したリソースが正常状態になるまで待機する最大時間（秒）を指定します。これにより、データベースインスタンスなどの接続先リソースが完全に稼働し、トランザクション処理可能になるまで処理を進めないようにします。 | `5`                   |
| **Health Check Interval** | Couchbaseへの接続状態を自動的に監視する間隔（秒）を指定します。                                         | `15`                  |
