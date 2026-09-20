# MQTTデータをCouchbaseに取り込む

[Couchbase](https://couchbase.com/)は、多目的で分散型のデータベースであり、SQLやACIDトランザクションなどリレーショナルデータベースの強みと、JSONの柔軟性を兼ね備えています。高いパフォーマンスとスケーラビリティを基盤として構築されており、ユーザープロファイル、動的な製品カタログ、生成AIアプリケーション、ベクター検索、高速キャッシュなど、さまざまな業界で広く利用されています。

## 動作概要

Couchbaseとのデータ統合は、EMQXに標準搭載された機能であり、MQTTのリアルタイムデータ取得・送信機能とCouchbaseの強力なデータ処理機能を組み合わせることができます。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからCouchbaseへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとCouchbase間のデータ統合の典型的なアーキテクチャを示しています。

![couchbase_architecture](./assets/couchbase_architecture.png)

MQTTデータをCouchbaseに取り込む流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、機械、センサー、製品ラインの稼働状態や計測値、トリガーイベントに基づくリアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが届くと、ルールエンジンを通過し、EMQXで定義されたルールにより処理されます。ルールはあらかじめ定義された条件に基づき、どのメッセージをCouchbaseにルーティングするかを決定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Couchbaseへのデータ取り込み**：ルールエンジンがCouchbaseへの保存対象メッセージを特定すると、メッセージをCouchbaseに転送するアクションをトリガーします。処理済みのデータはCouchbaseデータベースのデータセットにシームレスに書き込まれます。
4. **データの保存と活用**：データがCouchbaseに保存された後、企業はそのクエリ機能を活用してさまざまなユースケースに対応できます。例えば、動的な製品カタログの文脈では、Couchbaseを用いて製品情報の効率的な管理・取得、リアルタイムの在庫更新、顧客へのパーソナライズされた推奨の提供が可能となり、購買体験の向上と売上増加に寄与します。

## 特長とメリット

Couchbaseとのデータ統合は、効率的なデータ送信、保存、活用を実現するための多彩な機能とメリットを提供します。

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからCouchbaseへの効率的かつ信頼性の高いデータ送信を保証します。即時のインサイトとアクションが求められるユースケースに最適です。
- **高パフォーマンスとスケーラビリティ**：EMQXの分散アーキテクチャとCouchbaseのカラムナーストレージ形式により、データ量の増加に応じてシームレスにスケール可能です。大規模データセットでも一貫したパフォーマンスと応答性を維持します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、Couchbaseに保存する前にデータの前処理が可能です。フィルタリング、ルーティング、集約、強化など多様なデータ変換機能により、ニーズに応じたデータ整形が行えます。
- **簡単なデプロイと管理**：EMQXはデータソースの設定、前処理ルール、Couchbase保存設定のためのユーザーフレンドリーなインターフェースを提供し、データ統合プロセスのセットアップと運用管理を簡素化します。
- **高度な分析機能**：Couchbaseの強力なSQLベースのクエリ言語と複雑な分析関数のサポートにより、IoTデータから価値あるインサイトを得られ、予測分析や異常検知などが可能です。

## はじめる前に

このセクションでは、EMQXダッシュボードでCouchbaseデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXのデータ統合[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- UNIXターミナルおよびコマンドの基本知識

### Couchbaseサーバーの起動

このセクションでは、[Docker](https://www.docker.com/)を使用してCouchbaseサーバーを起動する方法を紹介します。

1. 以下のコマンドでCouchbaseサーバーを起動します。

   ```bash
   docker run -t --name db -p 8091-8096:8091-8096 -p 11210-11211:11210-11211 couchbase/server:enterprise-7.2.0
   ```
   
   コマンド実行時にDockerがCouchbase Serverをダウンロードおよびインストールします。Docker仮想環境でCouchbase Serverが起動すると、以下のようなメッセージが表示されます。
   
   ```
   Starting Couchbase Server -- Web UI available at http://<ip>:8091
   and logs available in /opt/couchbase/var/lib/couchbase/logs
   ```
   
2. ブラウザで `http://localhost:8091` にアクセスし、Couchbase Webコンソールを開きます。

<img src="./assets/couchbase-consoleSetup.png" alt="Couchbaseコンソールセットアップ" style="zoom:67%;" />

3. **Setup New Cluster** をクリックし、クラスター名を入力します。初期設定として、管理者のユーザー名とパスワードをそれぞれ `admin` と `password` に設定してください。

<img src="./assets/couchbase-consoleNewCluster.png" alt="Couchbaseコンソール新規クラスター" style="zoom:67%;" />

4. 利用規約に同意し、**Finish with Defaults** をクリックしてデフォルト値で設定を完了します。

5. 設定入力が完了したら、右下の **Save & Finish** ボタンをクリックします。これによりサーバーが設定され、Couchbase Webコンソールのダッシュボードが表示されます。左側のナビゲーションパネルで **Buckets** を選択し、**ADD BUCKET** ボタンをクリックします。

   <img src="./assets/couchbase-consoleBuckets.png" alt="Couchbaseコンソールバケット" style="zoom:67%;" />

6. バケット名（例：`emqx`）を入力し、**Create** をクリックしてバケットを作成します。

7. デフォルトコレクションに対してプライマリインデックスを作成します。

    ```
    docker exec -t db /opt/couchbase/bin/cbq -u admin -p password -engine=http://127.0.0.1:8091/ -script "create primary index on default:emqx_data._default._default;"
    ```

DockerでのCouchbase実行に関する詳細は、[公式ドキュメントページ](https://docs.couchbase.com/server/current/getting-started/do-a-quick-install.html)をご参照ください。

## コネクターの作成

このセクションでは、SinkをCouchbaseサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとCouchbaseをローカルマシンで実行していることを前提としています。リモート環境で実行している場合は、設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Couchbase** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の大文字・小文字の組み合わせで、例：`my_couchbase`
   - **Server Host**：`127.0.0.1`
   - **Username**：`admin`
   - **Password**：`password`
5. 詳細設定（任意）：[高度な設定](#advanced-configurations)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** を押してコネクターがCouchbaseサーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** を選択できます。ルールとSinkの作成を続ける場合は後者を選択してください。詳細は[Create a Rule with Couchbase Sink](#create-a-rule-with-couchbase-sink)を参照してください。

## Couchbase Sinkを使ったルールの作成

このセクションでは、EMQXダッシュボードでソースMQTTトピック `t/#` からのメッセージを処理し、処理結果を設定済みのSink経由でCouchbaseに転送するルールの作成方法を説明します。

1. EMQXダッシュボードの左ナビゲーションメニューから **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. SQLエディターのステートメントはそのままにします。これはトピックパターン `t/#` にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

5. + **Add Action** ボタンをクリックして、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをCouchbaseに送信します。

6. **Type of Action** ドロップダウンリストから `Couchbase` を選択します。**Action** はデフォルトの `Create Action` のままにします。既に作成済みのCouchbase Sinkがあれば選択可能です。この例では新規Sinkを作成します。

7. Sink名を入力します。英数字の大文字・小文字の組み合わせで入力してください。

8. **Connector** ドロップダウンから先ほど作成した `my_couchbase` を選択します。新規コネクターを作成する場合は、ドロップダウン横のボタンから作成可能です。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

9. SQLテンプレートに以下を入力します。

    ```sql
    insert into emqx_data (key, value) values (${.id}, ${.payload})
    ```

    ここで `${.id}` と `${.payload}` はそれぞれMQTTメッセージのIDとペイロードを表し、EMQXが転送前に対応する内容に置き換えます。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **詳細設定（任意）**：[高度な設定](#advanced-configurations)を参照してください。

12. **Create** をクリックする前に、**Test Connectivity** ボタンでCouchbaseサーバーへの接続を確認できます。

13. **Create** ボタンをクリックしてSink設定を完了します。**Create Rule** ページに戻ると、新しいSinkが **Action Outputs** タブに表示されます。

14. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status** は `connected` となります。

これでルールの作成が完了し、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいCouchbase Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` により解析され、Couchbaseに送信・保存されている様子が確認できます。

## ルールのテスト

EMQXダッシュボードに内蔵されたWebSocketクライアントを使って、ルールが期待通りに動作するかテストできます。

ダッシュボード左ナビゲーションメニューの **Diagnose** -> **WebSocket Client** をクリックしてWebSocketクライアントを開き、以下の手順でトピック `t/test` にメッセージを送信します。

1. 現在のEMQXインスタンスの接続情報を入力します。ローカルでEMQXを実行している場合、デフォルト値を使用できます（認証設定を変更している場合はユーザー名・パスワードを入力してください）。

2. **Connect** をクリックしてクライアントをEMQXに接続します。

3. ページ下部のパブリッシュエリアに以下を入力します：
   * **Topic**：`t/test`
   * **Payload**：`Hello World Couchbase from EMQX`
   * **QoS**：2
   
4. **Publish** をクリックしてメッセージを送信します。Couchbaseサーバーの `emqx_data` バケットにアイテムが挿入されているはずです。以下のコマンドをターミナルで実行して確認できます。

   ```bash
   docker exec -t db /opt/couchbase/bin/cbq -u admin -p password -engine=http://127.0.0.1:8091/ -script "SELECT * FROM emqx_data._default._default LIMIT 5;"
   ```

5. 正常に動作していれば、上記コマンドは以下のような結果を出力します（`requestID`やメトリクスは異なる場合があります）。

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

## 高度な設定

このセクションでは、EMQX Couchbaseコネクターの高度な設定オプションについて詳しく説明します。ダッシュボードでコネクターを設定する際、**Advanced Settings** に移動し、以下のパラメータをニーズに合わせて調整できます。

| **項目**                  | **説明**                                                                                                  | **推奨値**          |
| ------------------------- | --------------------------------------------------------------------------------------------------------- | ------------------- |
| **HTTP Pipelining**       | サーバーに対してレスポンスを待たずに連続して送信できるHTTPリクエストの最大数を指定します。<br />`1`に設定すると、従来のリクエスト-レスポンスモデルとなり、各リクエスト送信後にレスポンスを待ちます。<br />より大きな値に設定すると、複数のリクエストをバッチ送信でき、ネットワークリソースの効率的な利用とラウンドトリップ時間の短縮が可能です。 | `100`               |
| **Connection Pool Size**  | Couchbaseサービスとの接続プールで維持する同時接続数を指定します。<br />システムリソース、ネットワークレイテンシ、アプリケーションの負荷に応じて適切な値を設定してください。大きすぎるとリソース枯渇、小さすぎるとスループットが制限される可能性があります。 | `8`                 |
| **Connect Timeout**       | Couchbaseサーバーへの接続確立を試みる際に待機する最大時間（秒）を指定します。<br />システムパフォーマンスとリソース利用のバランスを考慮し、ネットワーク環境に応じて最適な値をテストして設定してください。 | `15`                |
| **Start Timeout**         | 自動起動されたリソースが正常な状態になるまで待機する最大時間（秒）を指定します。<br />これにより、データベースインスタンスなどのリソースが完全に稼働準備できるまで処理を進めないようにします。 | `5`                 |
| **Health Check Interval** | Couchbaseへの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。                                         | `15`                |
