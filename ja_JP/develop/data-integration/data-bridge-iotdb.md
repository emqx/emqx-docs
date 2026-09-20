# Apache IoTDBへのMQTTデータ取り込み

[Apache IoTDB](https://iotdb.apache.org/)は、多種多様なIoTデバイスやシステムから生成される大量の時系列データを効率的に処理するために設計された高性能かつスケーラブルな時系列データベースです。

EMQXはApache IoTDBとのシームレスなデータ統合を提供し、EMQXで受信したリアルタイムのMQTTメッセージを[REST API V2](https://iotdb.apache.org/UserGuide/latest/API/RestServiceV2.html)を通じてIoTDBに転送できます。この統合は一方向のデータフローをサポートし、MQTTデータをIoTDBに書き込むことで効率的な時系列ストレージと分析を実現します。

本ページでは、EMQXとApache IoTDBの統合方法を紹介し、統合の作成および検証手順をステップバイステップで説明します。

## 動作概要

Apache IoTDBデータ統合はEMQXの組み込み機能であり、追加のコーディングなしにMQTTベースの時系列データをApache IoTDBに取り込むことが可能です。EMQXの組み込み[ルールエンジン](./rules.md)を活用することで、データのフィルタリング、変換、転送を簡素化し、IoTDBでの効率的な保存とクエリを実現します。

以下の図は、EMQXとIoTDB間の典型的なデータ統合アーキテクチャを示しています。<!-- この画像はIoTDB専用に修正が必要です -->

<img src="./assets/IoTDB_bridge_architecture.png" alt="IoTDB_bridge_architecture" style="zoom:67%;" />

データ統合のワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：デバイスがMQTT経由でEMQXに接続し、テレメトリデータ、ステータス更新、イベント情報を含むメッセージをパブリッシュします。ルールエンジンが受信メッセージを評価します。
2. **ルールベースの処理**：定義されたルールに一致するメッセージが選択され、必要に応じてフィールドのフィルタリング、データ形式の変換、ペイロードの拡充などの変換が適用されます。
3. **データのバッファリング**：IoTDBが一時的に利用できない場合に備え、EMQXはメッセージをメモリにバッファリングします。必要に応じてメモリ圧迫を避けるためにディスクにオフロード可能です。統合またはEMQXノードの再起動時にはバッファデータは保持されません。
4. **IoTDBへのデータ取り込み**：ルールに一致した場合、EMQXはIoTDBシンクをトリガーし、処理済みデータをIoTDBに時系列データとして書き込みます。
5. **データの保存と活用**：IoTDBに保存されたデータは、デバイス監視、資産追跡、予知保全、運用最適化などの下流アプリケーションでクエリや分析に利用可能です。

## 特長とメリット

IoTDBとのデータ統合は、効果的なデータ処理と保存を実現するための多様な機能とメリットを提供します：

- **ノーコードのIoTデータパイプライン**

  組み込みのルールとシンクを利用して、EMQXとApache IoTDB間のMQTTから時系列データへの完全なパイプラインをカスタムコードや外部サービスなしで構築可能です。

- **MQTTからIoTDBモデルへの柔軟なマッピング**

  ツリーモデルとテーブルモデルの両方をサポートし、デバイスのモデリングやクエリ要件に合わせた構造でMQTTデータをIoTDBに書き込めます。

- **取り込みと保存の分離**

  EMQXはバースト的かつ高頻度のMQTTトラフィックを吸収し、IoTDBは耐久性の高い時系列ストレージに専念することで、システムの安定性とレジリエンスを向上させます。

- **本番対応のスケーラビリティ**

  デバイス数やデータ量に応じて水平スケール可能で、大規模なIoT、IIoT、エネルギー分野のシナリオに適しています。

- **分析に最適化された時系列データ**

  IoTDBに書き込まれたデータは直接クエリ、集計、分析でき、ビッグデータエンジンと連携して高度な分析や長期的なインサイト取得も可能です。

## はじめる前に

このセクションでは、EMQXダッシュボードでApache IoTDBデータ統合を作成する前に準備すべき事項を説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Apache IoTDBサーバーの起動

ここでは[Docker](https://www.docker.com/)を使ったApache IoTDBサーバーの起動方法を紹介します。IoTDBの設定で`enable_rest_service=true`が有効になっていることを確認してください。

以下のコマンドでRESTインターフェースが有効なApache IoTDBサーバーを起動します：

```bash
docker run -d --name iotdb-service \
              --hostname iotdb-service \
              -p 6667:6667 \
              -p 18080:18080 \
              -e enable_rest_service=true \
              -e cn_internal_address=iotdb-service \
              -e cn_target_config_node_list=iotdb-service:10710 \
              -e cn_internal_port=10710 \
              -e cn_consensus_port=10720 \
              -e dn_rpc_address=iotdb-service \
              -e dn_internal_address=iotdb-service \
              -e dn_target_config_node_list=iotdb-service:10710 \
              -e dn_mpp_data_exchange_port=10740 \
              -e dn_schema_region_consensus_port=10750 \
              -e dn_data_region_consensus_port=10760 \
              -e dn_rpc_port=6667 \
              apache/iotdb:2.0.5-standalone
```

詳細は[Docker HubのIoTDB実行情報](https://hub.docker.com/r/apache/iotdb)をご覧ください。

### データベースの作成

IoTDBはツリーモデルとテーブルモデルの2つのデータモデルをサポートしています。データベース作成前に、コネクターとシンクで使用する**SQL方言**（TreeまたはTable）を確認し、それに応じてデータベースを作成してください。

- **ツリーモデル**の場合はデータベースのみ作成すれば十分です。
- **テーブルモデル**の場合は、データベース作成後にテーブルを作成する必要があります。

詳細な手順はIoTDBユーザーガイドをご参照ください：

- [ツリーモデル用データベース作成](https://iotdb.apache.org/UserGuide/latest/Basic-Concept/Operate-Metadata_apache.html#_1-1-create-database)
- [テーブルモデル用データベース作成](https://iotdb.apache.org/UserGuide/latest-Table/Basic-Concept/Database-Management_apache.html#_1-1-create-a-database)
- [テーブルモデル用テーブル作成](https://iotdb.apache.org/UserGuide/latest-Table/Basic-Concept/Table-Management_apache.html#_1-1-create-a-table)

## IoTDBコネクターの作成

Apache IoTDBデータ統合を作成するには、Apache IoTDBシンクとApache IoTDBサーバーを接続するコネクターを作成する必要があります。

EMQXはREST APIまたはThriftプロトコルを介してIoTDBと通信をサポートしています。

1. EMQXダッシュボードで **Integrations** -> **Connectors** に移動します。

2. 右上の **Create** をクリックします。

3. **Create Connector** ページで **Apache IoTDB** を選択します。

4. コネクターの設定を行います：

   - **Connector Name**：コネクターの一意の名前を入力します。大文字・小文字の英数字の組み合わせを使用してください。例：`my_iotdb`
   - **Description**：（任意）コネクターの簡単な説明
   - **Driver**：IoTDB接続に使用するプロトコルを選択します。
     - `REST API`：IoTDB RESTサービスのエンドポイント（例：`http://localhost:18080`）を**IoTDB REST Service Base URL**に入力します。
     - `Thrift Protocol`：IoTDB Thriftサーバーのアドレスを**Server Host**に入力します。

   - **SQL Dialect**：EMQXがデバイスデータをIoTDBに書き込む方法を決定するIoTDBのデータモデルを選択します。
     - `Tree Model`：階層的な時系列パスとしてデータを書き込み、パスベースのデバイス・測定管理に適しています。
     - `Table Model`：リレーショナルテーブルにデータを書き込み、デバイス種別やカテゴリごとの管理に適しています。
   - **Database Name**：`Table Model`を選択した場合、接続するデータベース名を指定する必要があります。
   - **Username** と **Password**：EMQXがApache IoTDBサーバーに認証するための資格情報を入力します。
   - **IoTDB Version**：Apache IoTDBのバージョンを選択します。
   - **Enable TLS**：Apache IoTDBサーバーへの暗号化接続を有効にします。詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。
   - オプションの調整は[高度な設定](#advanced-configurations)の**Advanced Settings**をご覧ください。

5. （任意）**Test Connectivity**をクリックして、コネクターがApache IoTDBサーバーに正常に接続できるか検証します。

6. **Create**をクリックしてコネクターの作成を完了します。

   表示されるダイアログで、**Back to Connector List** または **Create Rule** を選択して、ルールとApache IoTDBシンクの設定を続行できます。詳細は[ルールとApache IoTDBシンクの作成](#create-a-rule-and-apache-iotdb-sink)をご覧ください。

## Apache IoTDBシンクを使ったルールの作成

このセクションでは、EMQXでルールを作成し、ソースMQTTトピック`root/#`からのメッセージを処理して、設定済みのApache IoTDBシンクを通じて時系列データをApache IoTDBに保存する方法を示します。

### SQLを定義したルールの作成

1. EMQXダッシュボードで **Integration** -> **Rules** に移動します。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. **SQLエディター**に以下のステートメントを入力します。これはトピックパターン`root/#`に一致するMQTTメッセージを転送します：

   ```sql
   SELECT
     *
   FROM
     "root/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習やテストが可能です。

   :::

5. 処理結果をIoTDBに書き込むためにApache IoTDBシンクをルールに追加します。詳細は[Apache IoTDBシンクの追加](#add-an-apache-iotdb-sink)をご覧ください。

6. **Create Rule**ページで設定内容を確認し、**Save**をクリックしてルールを作成します。

作成したルールは**Rules**一覧に表示されます。**Actions (Sink)**タブをクリックすると、このルールに関連付けられたIoTDBシンクを確認できます。

また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示すると、トピック`root/#`からのメッセージが`my_rule`ルールで処理されIoTDBに書き込まれる様子を視覚的に確認できます。

### Apache IoTDBシンクの追加

1. ルールの右側にある**Add Action**ボタンをクリックし、ルールに一致した際にトリガーされるアクションを定義します。このアクションは処理済みデータをIoTDBに転送します。

2. **Type of Action**ドロップダウンで`Apache IoTDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既存のIoTDBシンクを選択することも可能ですが、ここでは新規作成を想定しています。

3. シンクの名前と説明を入力します。

4. **Connector**ドロップダウンで先ほど作成したコネクター`my_iotdb`を選択します。利用可能なコネクターがない場合は隣のボタンから新規作成できます。詳細は[IoTDBコネクターの作成](#create-an-iotdb-connector)をご覧ください。

5. シンクの設定を行います：

      * **SQL Dialect**：Apache IoTDBシンクがIoTDBにデータを書き込む方法を選択します。コネクターで選択したSQL方言と一致させる必要があります。

        * `Tree Model`：IoTDBの時系列パスとしてデータを書き込みます。各シンクレコードはデバイスパスに挿入され、測定値はそのデバイス下の個別時系列として書き込まれます。このモデルを選択した場合、**Device ID**フィールドを指定可能です。
        * `Table Model`：IoTDBのリレーショナルテーブルにデータを書き込みます。各シンクレコードは指定テーブルの行として挿入され、フィールドはテーブル列にマッピングされます。このモデルを選択した場合、**Table**フィールドの指定が必須です。

      * **Device ID**（任意）：IoTDBインスタンスに時系列データを書き込む際のデバイス名として使用する特定のデバイスIDを入力します。

        :::tip

        空欄の場合でも、パブリッシュされたメッセージ内やルール内でデバイスIDを指定可能です。例えば、JSONエンコードされたメッセージに`device_id`フィールドが含まれていれば、その値が出力デバイスIDとなります。ルールエンジンでこの情報を抽出するには、以下のようなSQLを使用できます：

        ```sql
        SELECT
         payload,
         `my_device` as payload.device_id
        ```

        ただし、このフィールドに固定で設定したデバイスIDが優先されます。

        :::

      - **Table**：データを書き込むIoTDBテーブル名

      - **Align Timeseries**：デフォルトで無効。これを有効にすると、グループ化されたアラインド時系列のタイムスタンプ列がIoTDBに一度だけ保存され、グループ内の各時系列での重複保存を防ぎます。詳細は[アラインド時系列](https://iotdb.apache.org/UserGuide/V1.1.x/Data-Concept/Data-Model-and-Terminology.html#aligned-timeseries)を参照してください。

      - **Write Data**の設定で、MQTTメッセージからIoTDBデータを生成する方法を指定します。

        **Write Data**セクションでは、必要な数だけアイテムを含むテンプレートを定義でき、各行に必要なコンテキスト情報を指定します。このテンプレートを用いてMQTTメッセージに適用し、IoTDBデータを生成します。書き込みテンプレートはCSVファイルによる一括設定もサポートしています。詳細は[一括設定](#batch-setting)をご覧ください。

        例として以下のテンプレートを考えます：

        ::: tip 補足

        **Column Category**はSQL方言で`Table Model`を選択した場合のみ表示されます。

        :::

        | Column Category | Timestamp | Measurement | Data Type | Value    |
        | --------------- | --------- | ----------- | --------- | -------- |
        | field           |           | index       | INT32     | ${index} |
        |                 |           | temperature | FLOAT     | ${temp}  |

        `Timestamp`と`Value`はプレースホルダー構文をサポートし、変数で埋められます。`Timestamp`が省略された場合は、現在のシステム時刻（ミリ秒単位）で自動補完されます。

        この場合、MQTTメッセージは以下のように構成できます：

          ```json
        {
          "index": "42",
          "temp": "32.67"
          }
          ```

6. **フォールバックアクション**：（任意）メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。これらはプライマリシンクがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)をご覧ください。

7. **高度な設定**：（任意）[高度な設定](#advanced-configurations)を参照してください。

8. （任意）**Test Connectivity**をクリックして、シンクがApache IoTDBサーバーに接続できるかテストします。

### 一括設定

Apache IoTDBでは、数百件のデータを同時に書き込む際にダッシュボード上での設定が難しい場合があります。これを解決するため、EMQXはデータ書き込みの一括設定機能を提供しています。

**Write Data**の設定時に、一括設定機能を使ってCSVファイルから挿入操作のフィールドをインポートできます。

1. **Write Data**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、テンプレートにデータ書き込み設定を記入します。デフォルトのテンプレート内容は以下の通りです：

   ::: tip 補足

   以下は`Table Model`用のデフォルトテンプレートです。`Tree Model`では**Column Category**列はありません。

   :::

   | Column Category | Timestamp | Measurement | Data Type | Value             | 備考（任意）                                               |
   | --------------- | --------- | ----------- | --------- | ----------------- | ---------------------------------------------------------- |
   | tag             | now       | clientid    | text      | ${clientid}       |                                                            |
   | field           | now       | temp        | float     | ${payload.temp}   | フィールド、値、データ型は必須。利用可能なデータ型はboolean, int32, int64, float, double, text |
   | attribute       | now       | hum         | text      | ${payload.hum}    |                                                            |
   | attribute       | now       | status      | text      | ${payload.status} |                                                            |

   - **Column Category**：列のデータモデル。`tag`、`field`、`attribute`がサポートされます。`tag`は文字列である必要があり、`field`または`attribute`の使用が推奨されます。
   - **Timestamp**：`${var}`形式のプレースホルダーをサポートし、タイムスタンプ形式が必要です。以下の特殊文字でシステム時刻を挿入可能です：
     - now：現在のミリ秒タイムスタンプ
     - now_ms：現在のミリ秒タイムスタンプ
     - now_us：現在のマイクロ秒タイムスタンプ
     - now_ns：現在のナノ秒タイムスタンプ
   - **Measurement**：フィールド名
   - **Data Type**：データ型。boolean、int32、int64、float、double、textが選択可能
   - **Value**：書き込みデータ値。定数または`${var}`形式のプレースホルダーをサポートし、データ型と一致する必要があります。
   - **備考**：CSVファイル内の注釈用で、EMQXにはインポートされません。

   1MB未満かつ2000行以内のCSVファイルのみサポートされます。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロード後、**Import**をクリックして一括設定を完了します。

4. インポート後、**Write Data**テーブルでさらにデータを調整可能です。

## ルールのテスト

EMQXダッシュボード内蔵のWebSocketクライアントを使って、Apache IoTDBシンクとルールの動作をテストできます。

1. ダッシュボード左メニューの **Diagnose** -> **WebSocket Client** をクリックします。

2. 現在のEMQXインスタンスへの接続情報を入力します。

   - ローカルでEMQXを実行している場合はデフォルト値を使用可能です。
   - 認証設定を変更している場合は、ユーザー名とパスワードを入力してください。

3. **Connect**をクリックしてクライアントをEMQXインスタンスに接続します。

4. 下にスクロールしてパブリッシュエリアに移動し、メッセージにデバイスIDを指定して以下を入力します：

   - **Topic**：`root/sg27`

     :::tip

     トピックが`root`で始まらない場合、自動的に`root`がプレフィックスされます。例えば、`test/sg27`にメッセージをパブリッシュすると、デバイス名は`root.test.sg27`となります。ルールとトピックの設定が正しく、該当トピックのメッセージがシンクに転送されることを確認してください。

     :::

   - **Payload**：

     ```json
      {
       "value": "37.6",
       "device_id": "root.sg27"
      }
     ```

      ::: tip

      `Write Data`テンプレート例：

     ```
      now, "temp", float, "${payload.value}"
     ```

      :::

   - **QoS**：`2`

7. **Publish**をクリックしてメッセージを送信します。

   シンクとルールが正常に作成されていれば、メッセージは指定したApache IoTDBサーバーの時系列テーブルにパブリッシュされているはずです。

8. IoTDBのコマンドラインインターフェースでメッセージを確認します。上記のDocker環境の場合、以下のコマンドでサーバーに接続できます：

   ```shell
       $ docker exec -ti iotdb-service /iotdb/sbin/start-cli.sh -h iotdb-service
   ```

9. コンソールで以下を入力します：

   ```sql
   IoTDB> select * from root.sg27
   ```

   以下のようにデータが表示されるはずです：

   ```
   +------------------------+--------------+
   |                    Time|root.sg27.temp|
   +------------------------+--------------+
   |2023-05-05T14:26:44.743Z|          37.6|
   +------------------------+--------------+
   ```

## 高度な設定

このセクションでは、コネクターのパフォーマンスを最適化し、特定のシナリオに合わせて動作をカスタマイズするための高度な設定オプションを説明します。コネクター作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                     | 説明                                                         | 推奨値             |
| ------------------------ | ------------------------------------------------------------ | ------------------ |
| HTTP Pipelining          | サーバーに対して個別のレスポンスを待たずに連続して送信可能なHTTPリクエスト数を指定します。正の整数値で、`1`は従来のリクエスト-レスポンスモデル（1リクエスト送信後にレスポンス待ち）を示します。値を大きくすると複数リクエストをバッチ送信でき、ラウンドトリップ時間を削減しネットワークリソースを効率的に利用可能です。 | `100`              |
| Pool Type                | EMQXとApache IoTDB間のコネクション管理・分配のアルゴリズム戦略を定義します。<br>`random`は利用可能な接続プールからランダムに選択し、単純で均等な分配を提供します。<br>`hash`はハッシュアルゴリズムを用いてリクエストを一貫して特定の接続にマッピングし、クライアントIDやトピック名に基づくロードバランシングなど決定的な分配が必要な場合に適します。<br>**注意**：適切なプールタイプはユースケースや求める分配特性に依存します。 | `random`           |
| Connection Pool Size     | Apache IoTDBサービスとの接続プールで維持可能な同時接続数を指定します。システムのスケーラビリティとパフォーマンス管理に役立ちます。<br>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、ワークロードに依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の可能性があります。 | `8`                |
| Connect Timeout          | EMQXがApache IoTDB HTTPサーバーへの接続確立を試みる最大待機時間（秒）を指定します。<br>**注意**：適切なタイムアウト設定はシステム性能とリソース利用のバランスに重要です。様々なネットワーク環境でテストして最適値を見つけてください。 | `15`               |
| HTTP Request Max Retries | EMQXとApache IoTDB間の通信でHTTPリクエストが失敗した場合の最大再試行回数を指定します。 | `2`                |
| Start Timeout            | 自動起動されたリソースが正常状態になるまで待機する最大時間（秒）を指定します。これにより、Apache IoTDBのデータベースインスタンスなどの接続リソースが完全に稼働し、データトランザクションを処理可能になるまで操作を進めないようにします。 | `5`                |
| Buffer Pool Size         | EMQXとApache IoTDB間のイグレス型ブリッジでデータフロー管理に割り当てるバッファワーカープロセス数を指定します。これらのワーカーは送信前のデータを一時保存・処理します。イングレス（受信）専用のブリッジでは`0`に設定可能です。 | `18`               |
| Request TTL              | バッファに入ったリクエストが有効とみなされる最大期間（秒）を指定します。TTLを超えたリクエスト、または送信後にApache IoTDBからの応答やアックがタイムリーに得られない場合、リクエストは期限切れと判断されます。 | `45`               |
| Health Check Interval    | コネクターがApache IoTDBへの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`               |
| Max Buffer Queue Size    | Apache IoTDBデータ統合で各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはIoTDBへのデータ送信前に一時保存を行い、データフローを効率化します。システム性能やデータ転送要件に応じて調整してください。 | `265`              |
| Query Mode               | メッセージ送信要件に応じて`asynchronous`（非同期）または`synchronous`（同期）クエリモードを選択します。非同期モードではIoTDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがIoTDB到着前にメッセージを受信する可能性があります。 | `Async`            |
| Inflight Window          | 「インフライトクエリ」とは開始済みで応答やアックをまだ受け取っていないクエリのことです。コネクターがApache IoTDBと通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br>`query_mode`が`async`の場合、この設定が特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は、この値を1に設定してください。 | `100`              |

## さらに詳しく

EMQXはApache IoTDBとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細をご覧ください：

**ブログ：**

[IoT向け時系列データベース（TSDB）：欠けていたピース](https://www.emqx.com/en/blog/time-series-database-for-iot-the-missing-piece)
