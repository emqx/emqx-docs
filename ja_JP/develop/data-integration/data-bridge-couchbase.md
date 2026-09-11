# CouchbaseへのMQTTデータ取り込み

[Couchbase](https://couchbase.com/)は、SQLやACIDトランザクションを含むリレーショナルデータベースの強みとJSONの柔軟性を兼ね備えた、多目的で分散型のデータベースです。高いパフォーマンスとスケーラビリティを基盤として構築されており、ユーザープロファイル、動的な製品カタログ、生成AIアプリケーション、ベクトル検索、高速キャッシュなど、さまざまな業界で広く利用されています。

## 動作概要

Couchbaseとのデータ統合は、EMQXに標準搭載された機能であり、MQTTのリアルタイムデータ取得・送信機能とCouchbaseの強力なデータ処理機能を組み合わせることができます。組み込みの[ルールエンジン](./rules.md)コンポーネントを活用することで、EMQXからCouchbaseへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとCouchbase間の典型的なデータ統合アーキテクチャを示しています。

![couchbase_architecture](./assets/couchbase_architecture.png)

CouchbaseへのMQTTデータ取り込みの流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、機械、センサー、製品ラインの稼働状態、計測値、トリガーイベントに基づくリアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXに定義されたルールによって処理されます。ルールは事前に定義された条件に基づき、Couchbaseにルーティングすべきメッセージを判別します。ペイロードの変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Couchbaseへのデータ取り込み**：ルールエンジンがCouchbaseへの保存対象メッセージを特定すると、メッセージをCouchbaseに転送するアクションをトリガーします。処理済みデータはCouchbaseデータベースのデータセットにシームレスに書き込まれます。
4. **データの保存と活用**：データがCouchbaseに保存されることで、企業はそのクエリ機能を活用して様々なユースケースに対応できます。例えば、動的な製品カタログにおいては、Couchbaseを用いて製品情報の効率的な管理・取得、リアルタイム在庫更新のサポート、顧客へのパーソナライズされた推奨の提供が可能となり、購買体験の向上と売上増加に寄与します。

## 特長とメリット

Couchbaseとのデータ統合は、効率的なデータ送信、保存、活用を実現するための多彩な機能とメリットを提供します。

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからCouchbaseへの効率的かつ信頼性の高いデータ送信を保証します。即時のインサイトやアクションが必要なユースケースに適しています。
- **高いパフォーマンスとスケーラビリティ**：EMQXの分散アーキテクチャとCouchbaseのカラムナー形式のストレージにより、データ量の増加に応じたシームレスなスケーラビリティを実現します。大規模データセットでも一貫した性能と応答性を維持します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、Couchbaseに保存する前にデータを前処理できます。フィルタリング、ルーティング、集約、強化など多様な変換機能により、ニーズに応じてデータを整形可能です。
- **簡単なデプロイと管理**：EMQXはデータソースの設定、前処理ルール、Couchbase保存設定のためのユーザーフレンドリーなインターフェースを提供し、データ統合プロセスのセットアップと継続的な管理を容易にします。
- **高度な分析機能**：Couchbaseの強力なSQLベースのクエリ言語と複雑な分析関数のサポートにより、IoTデータから価値あるインサイトを得られ、予測分析や異常検知などに活用できます。

## はじめる前に

このセクションでは、EMQXダッシュボードでCouchbaseデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- UNIXターミナルと基本コマンドの知識

### Couchbaseサーバーの起動

ここでは、[Docker](https://www.docker.com/)を使用してCouchbaseサーバーを起動する方法を紹介します。

1. 以下のコマンドでCouchbaseサーバーを起動します。

   ```bash
   docker run -t --name db -p 8091-8096:8091-8096 -p 11210-11211:11210-11211 couchbase/server:enterprise-7.2.0
   ```
   
   コマンド実行時にDockerがCouchbase Serverをダウンロード・インストールします。Docker仮想環境内でCouchbase Serverが起動すると、以下のメッセージが表示されます。
   
   ```
   Starting Couchbase Server -- Web UI available at http://<ip>:8091
   and logs available in /opt/couchbase/var/lib/couchbase/logs
   ```
   
2. ブラウザで `http://localhost:8091` にアクセスし、Couchbase Webコンソールを開きます。

<img src="./assets/couchbase-consoleSetup.png" alt="Couchbaseコンソールセットアップ" style="zoom:67%;" />

3. **Setup New Cluster** をクリックし、クラスター名を入力します。初期設定として、管理者アカウントのユーザー名とパスワードをそれぞれ `admin` と `password` に設定してください。

<img src="./assets/couchbase-consoleNewCluster.png" alt="Couchbase新規クラスター設定" style="zoom:67%;" />

4. 利用規約に同意し、**Finish with Defaults** をクリックしてデフォルト値で設定を完了します。

5. 設定内容を入力し終えたら、右下の **Save & Finish** ボタンをクリックします。これによりサーバーが設定され、Couchbase Webコンソールのダッシュボードが表示されます。左側のナビゲーションパネルで **Buckets** を選択し、**ADD BUCKET** ボタンをクリックします。

   <img src="./assets/couchbase-consoleBuckets.png" alt="Couchbaseバケット管理" style="zoom:67%;" />

6. バケット名（例：`emqx`）を入力し、**Create** をクリックしてバケットを作成します。

7. デフォルトコレクションに対してプライマリインデックスを作成します。

    ```
    docker exec -t db /opt/couchbase/bin/cbq -u admin -p password -engine=http://127.0.0.1:8091/ -script "create primary index on default:emqx_data._default._default;"
    ```

DockerでのCouchbase実行に関する詳細は、[公式ドキュメントページ](https://docs.couchbase.com/server/current/getting-started/do-a-quick-install.html)をご参照ください。

## コネクターの作成

このセクションでは、SinkをCouchbaseサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとCouchbaseの両方をローカルマシンで実行していることを前提としています。リモート環境で実行している場合は、適宜設定を調整してください。

1. EMQXダッシュボードにアクセスし、左メニューから **Integration** -> **Connectors** をクリックします。
2. 画面右上の **Create** をクリックします。
3. **Create Connector** ページで **Couchbase** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の大文字・小文字を組み合わせてください。例：`my_couchbase`
   - **Server Host**：`127.0.0.1`
   - **Username**：`admin`
   - **Password**：`password`
5. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** を押してCouchbaseサーバーへの接続確認ができます。
7. 画面下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** を選択できます。ルールとSinkの作成を続ける場合は、[Create a Rule with Couchbase Sink](#create-a-rule-with-couchbase-sink)をご覧ください。

## Couchbase Sinkを用いたルールの作成

このセクションでは、EMQXダッシュボードでMQTTのソーストピック `t/#` からメッセージを処理し、処理結果をCouchbaseに転送するルールの作成方法を説明します。

1. EMQXダッシュボードにアクセスし、左メニューから **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. SQLエディターのステートメントはそのままにしておきます。これはトピックパターン `t/#` にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

5. + **Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをCouchbaseに送信します。

6. **Type of Action** ドロップダウンから `Couchbase` を選択します。**Action** はデフォルトの `Create Action` のままにします。既に作成済みのCouchbase Sinkを選択することも可能です。この例では新規Sinkを作成します。

7. Sinkの名前を入力します。英数字の大文字・小文字を組み合わせてください。

8. **Connector** ドロップダウンから先ほど作成した `my_couchbase` を選択します。新規コネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータは[Create a Connector](#create-a-connector)を参照してください。

9. SQLテンプレートに以下のコマンドを入力します。

    ```sql
    insert into emqx_data (key, value) values (${.id}, ${.payload})
    ```

    ここで `${.id}` と `${.payload}` はそれぞれMQTTメッセージのIDとペイロードを表し、EMQXが転送前に対応する内容に置き換えます。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)をご覧ください。

11. **詳細設定（任意）**：[Advanced Configurations](#advanced-configurations)を参照してください。

12. **Create** をクリックする前に、**Test Connectivity** ボタンでCouchbaseサーバーへの接続確認が可能です。

13. **Create** ボタンをクリックしてSink設定を完了します。**Create Rule** ページに戻ると、新しいSinkが **Action Outputs** タブに表示されます。

14. **Create Rule** ページで設定内容を確認し、**Create** をクリックしてルールを作成します。作成したルールはルール一覧に表示され、**status** は `connected` となります。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると新しいCouchbase Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Couchbaseに送信・保存されていることが確認できます。

## ルールのテスト

EMQXダッシュボードに組み込まれたWebSocketクライアントを使って、ルールが期待通りに動作するかテストできます。

ダッシュボードの左メニューから **Diagnose** -> **WebSocket Client** をクリックしてWebSocketクライアントにアクセスし、以下の手順でWebSocketクライアントを設定し、トピック `t/test` にメッセージを送信します。

1. 現在のEMQXインスタンスの接続情報を入力します。ローカルでEMQXを実行している場合は、デフォルト値を使用できます（認証設定を変更している場合はユーザー名・パスワードの入力が必要です）。

2. **Connect** をクリックしてクライアントをEMQXインスタンスに接続します。

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

## 詳細設定

このセクションでは、EMQX Couchbaseコネクターの詳細設定オプションについて説明します。ダッシュボードでコネクターを設定する際、**Advanced Settings** にて以下のパラメータをニーズに合わせて調整できます。

| **項目**                  | **説明**                                                                                   | **推奨値**          |
| ------------------------- | ------------------------------------------------------------------------------------------ | ------------------- |
| **HTTP Pipelining**       | サーバーに対してレスポンスを待たずに連続して送信可能なHTTPリクエストの最大数を指定します。<br />`1`に設定すると従来のリクエスト-レスポンスモデルとなり、1リクエスト送信後にレスポンスを待ちます。値を大きくすることで複数リクエストをバッチ送信し、ネットワークリソースの効率的利用とラウンドトリップ時間の短縮が可能です。 | `100`               |
| **Connection Pool Size**  | Couchbaseサービスとの接続プール内で維持可能な同時接続数を指定します。<br />システムリソース、ネットワークレイテンシ、アプリケーションの負荷に応じて適切な値を設定してください。大きすぎるとリソース枯渇、小さすぎるとスループット制限の原因となります。 | `8`                 |
| **Connect Timeout**       | Couchbaseサーバーへの接続確立時に待機する最大時間（秒）を指定します。<br />システム性能とリソース利用のバランスを考慮し、ネットワーク条件に応じて最適な値を設定してください。 | `15`                |
| **Start Timeout**         | 自動起動されたリソースが正常状態になるまで待機する最大時間（秒）を指定します。<br />これにより、Couchbaseのデータベースインスタンスなど接続先リソースが完全に稼働し、データ処理可能になるまで処理を進めないようにします。 | `5`                 |
| **Health Check Interval** | Couchbaseとの接続状態を自動的に監視する間隔（秒）を指定します。                                       | `15`                |
