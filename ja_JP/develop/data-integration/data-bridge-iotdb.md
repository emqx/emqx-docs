# Apache IoTDB に MQTT データを取り込む

[Apache IoTDB](https://iotdb.apache.org/) は、多種多様な IoT デバイスやシステムから生成される膨大な時系列データを効率的に処理するために設計された、高性能かつスケーラブルな時系列データベースです。

EMQX は Apache IoTDB とのシームレスなデータ統合を提供し、EMQX がリアルタイムで受信した MQTT メッセージを [REST API V2](https://iotdb.apache.org/UserGuide/latest/API/RestServiceV2.html) を通じて IoTDB に転送します。この統合は一方向のデータフローをサポートし、MQTT データを効率的な時系列ストレージおよび分析のために IoTDB に書き込みます。

本ページでは、EMQX と Apache IoTDB の統合方法を紹介し、統合の作成および検証の手順をステップバイステップで説明します。

## 動作概要

Apache IoTDB データ統合は、EMQX に組み込まれた機能であり、追加のコーディングなしに MQTT ベースの時系列データを Apache IoTDB に取り込むことを可能にします。EMQX の組み込み [ルールエンジン](./rules.md) を活用することで、データのフィルタリング、変換、転送を簡素化し、IoTDB での効率的な保存とクエリを実現します。

以下の図は、EMQX と IoTDB 間の典型的なデータ統合アーキテクチャを示しています。<!-- この画像は IoTDB 専用に修正が必要です -->

<img src="./assets/IoTDB_bridge_architecture.png" alt="IoTDB_bridge_architecture" style="zoom:67%;" />

データ統合のワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：デバイスは MQTT を介して EMQX に接続し、テレメトリデータ、ステータス更新、イベント情報を含むメッセージをパブリッシュします。ルールエンジンが受信メッセージを評価します。
2. **ルールベースの処理**：定義されたルールにマッチしたメッセージが選択され、必要に応じてフィールドのフィルタリング、データ形式の変換、ペイロードの付加などの変換が適用されます。
3. **データのバッファリング**：信頼性向上のため、IoTDB が一時的に利用不可の場合は EMQX がメッセージをメモリにバッファリングします。必要に応じて、メモリ圧迫を避けるためにバッファデータをディスクにオフロード可能です。統合または EMQX ノードの再起動時にはバッファデータは保持されません。
4. **IoTDB へのデータ取り込み**：マッチしたルールに対して、EMQX は IoTDB Sink をトリガーし、処理済みデータを IoTDB に時系列データとして書き込みます。
5. **データの保存と活用**：IoTDB に保存されたデータは、デバイス監視、資産追跡、予知保全、運用最適化などの下流アプリケーションでクエリや分析に利用できます。

## 特長とメリット

IoTDB とのデータ統合は、効果的なデータ処理と保存を実現するための多彩な機能と利点を提供します：

- **ノーコード IoT データパイプライン**

  組み込みのルールとシンクを使い、カスタムコードや外部サービスなしで EMQX と Apache IoTDB 間の完全な MQTT から時系列データへのパイプラインを構築できます。

- **MQTT から IoTDB モデルへの柔軟なマッピング**

  ツリーモデルとテーブルモデルの両方をサポートし、デバイスモデリングやクエリ要件に合った構造で MQTT データを IoTDB に書き込めます。

- **取り込みと保存の分離**

  EMQX はバースト的で高頻度な MQTT トラフィックを吸収し、IoTDB は耐久性のある時系列ストレージに専念することで、システムの安定性とレジリエンスを向上させます。

- **本番対応のスケーラビリティ**

  統合はデバイス数とデータ量に応じて水平スケール可能で、大規模な IoT、IIoT、エネルギーシナリオに適しています。

- **分析対応の時系列データ**

  IoTDB に書き込まれたデータは直接クエリ、集計、分析できるほか、ビッグデータエンジンと連携して高度な分析や長期的な洞察を得られます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Apache IoTDB データ統合を作成する前に必要な準備を説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Apache IoTDB サーバーの起動

ここでは [Docker](https://www.docker.com/) を使って Apache IoTDB サーバーを起動する方法を紹介します。IoTDB の設定で `enable_rest_service=true` が有効になっていることを確認してください。

以下のコマンドを実行すると、REST インターフェースが有効な Apache IoTDB サーバーが起動します：

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

詳細は [Docker Hub の IoTDB 実行方法](https://hub.docker.com/r/apache/iotdb) をご参照ください。

### データベースの作成

IoTDB はツリーモデルとテーブルモデルの2つのデータモデルをサポートしています。データベース作成前に、コネクターおよびシンクで使用する **SQL Dialect**（Tree または Table）を確認し、それに応じてデータベースを作成してください。

- **ツリーモデル**の場合はデータベースのみ作成すればよいです。
- **テーブルモデル**の場合は、まずデータベースを作成し、その後データ取り込み用のテーブルを作成する必要があります。

詳細な手順は IoTDB ユーザーガイドをご参照ください：

- [ツリーモデル用データベース作成](https://iotdb.apache.org/UserGuide/latest/Basic-Concept/Operate-Metadata_apache.html#_1-1-create-database)
- [テーブルモデル用データベース作成](https://iotdb.apache.org/UserGuide/latest-Table/Basic-Concept/Database-Management_apache.html#_1-1-create-a-database)
- [テーブルモデル用テーブル作成](https://iotdb.apache.org/UserGuide/latest-Table/Basic-Concept/Table-Management_apache.html#_1-1-create-a-table)

## IoTDB コネクターの作成

Apache IoTDB データ統合を作成するには、Apache IoTDB Sink を Apache IoTDB サーバーに接続するためのコネクターを作成する必要があります。

EMQX は REST API または Thrift プロトコルを介した IoTDB との通信をサポートしています。

1. EMQX ダッシュボードで **Integrations** -> **Connectors** に移動します。

2. 右上の **Create** をクリックします。

3. **Create Connector** ページで **Apache IoTDB** を選択します。

4. コネクターの設定を行います：

   - **Connector Name**：コネクターの一意の名前を入力します。大文字・小文字の英数字の組み合わせを使用してください（例：`my_iotdb`）。
   - **Description**：（任意）コネクターの簡単な説明を入力します。
   - **Driver**：IoTDB への接続に使用するプロトコルを選択します。
     - `REST API`：IoTDB REST サービスのエンドポイント（例：`http://localhost:18080`）を **IoTDB REST Service Base URL** に入力します。
     - `Thrift Protocol`：IoTDB Thrift サーバーのアドレスを **Server Host** に入力します。

   - **SQL Dialect**：EMQX が IoTDB にデバイスデータを書き込む際のデータモデルを選択します。
     - `Tree Model`：階層的な時系列パスとしてデータを書き込み、パスベースのデバイス・計測管理に適しています。
     - `Table Model`：リレーショナルテーブルにデータを書き込み、デバイスタイプやカテゴリ別の管理に適しています。
   - **Database Name**：**SQL Dialect** が `Table Model` の場合、接続先のデータベース名を指定する必要があります。
   - **Username** と **Password**：EMQX が Apache IoTDB サーバーに認証するための資格情報を入力します。
   - **IoTDB Version**：使用している Apache IoTDB のバージョンを選択します。
   - **Enable TLS**：Apache IoTDB サーバーへの暗号化接続を有効にする場合はチェックします。詳細は [外部リソースアクセスの TLS](../../guides/network/overview.md#tls-for-external-resource-access) を参照してください。
   - 任意の調整は [高度な設定](#advanced-configurations) の **Advanced Settings** を参照してください。

5. （任意）**Test Connectivity** をクリックして、コネクターが Apache IoTDB サーバーに正常に接続できるかを確認します。

6. **Create** をクリックしてコネクターの作成を完了します。

   表示されるダイアログで、**Back to Connector List** または **Create Rule** を選択して、ルールおよび Apache IoTDB Sink の設定を続行できます。詳細は [ルールと Apache IoTDB Sink の作成](#create-a-rule-and-apache-iotdb-sink) を参照してください。

## Apache IoTDB Sink を使ったルールの作成

このセクションでは、EMQX でソース MQTT トピック `root/#` からのメッセージを処理し、処理結果を設定済みの Apache IoTDB Sink を通じて Apache IoTDB に時系列データとして保存するルールの作成方法を示します。

### SQL 定義付きルールの作成

1. EMQX ダッシュボードで **Integration** -> **Rules** に移動します。

2. ページ右上の **Create** をクリックします。

3. ルール ID を入力します（例：`my_rule`）。

4. **SQL editor** に以下のステートメントを入力します。これはトピックパターン `root/#` にマッチする MQTT メッセージを転送します：

   ```sql
   SELECT
     *
   FROM
     "root/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストができます。

   :::

5. 処理結果を IoTDB に書き込むために、ルールに Apache IoTDB Sink を追加します。詳細は [Apache IoTDB Sink の追加](#add-an-apache-iotdb-sink) を参照してください。

6. **Create Rule** ページで設定を確認し、**Save** をクリックしてルールを作成します。

ルールが作成されると、**Rules** リストに表示されます。**Actions (Sink)** タブをクリックすると、このルールに関連付けられた IoTDB Sink を確認できます。

また、**Integrations** -> **Flow Designer** に移動すると、トポロジーグラフが表示され、トピック `root/#` からのメッセージが `my_rule` ルールで処理され IoTDB に書き込まれている様子が確認できます。

### Apache IoTDB Sink の追加

1. 右側の **Add Action** ボタンをクリックし、ルールにマッチした際にトリガーされるアクションを定義します。このアクションは処理済みデータを IoTDB に転送します。

2. **Type of Action** ドロップダウンで `Apache IoTDB` を選択します。**Action** はデフォルトの `Create Action` のままにします。既存の IoTDB Sink を選択することも可能ですが、ここでは新規作成を想定しています。

3. Sink の名前と説明を入力します。

4. **Connector** ドロップダウンで、先ほど作成したコネクター `my_iotdb` を選択します。利用可能なコネクターがない場合は、隣のボタンから新規作成できます。詳細は [IoTDB コネクターの作成](#create-an-iotdb-connector) を参照してください。

5. Sink の設定を行います：

      * **SQL Dialect**：Apache IoTDB Sink が IoTDB にデータを書き込む方法を選択します。コネクターで選択した SQL Dialect と一致させる必要があります。

        * `Tree Model`：IoTDB の時系列パスとしてデータを書き込みます。各 Sink レコードはデバイスパスに挿入され、そのデバイスの下に個別の時系列として計測値が書き込まれます。このモデルを選択した場合、**Device ID** フィールドを指定できます。
        * `Table Model`：IoTDB のリレーショナルテーブルにデータを書き込みます。各 Sink レコードは指定テーブルの行として挿入され、フィールドはテーブルのカラムにマッピングされます。このモデルを選択した場合、**Table** フィールドの指定が必須です。
        
      * **Device ID**（任意）：IoTDB インスタンスに時系列データを転送・挿入する際のデバイス名として使用する特定のデバイス ID を入力します。

        :::tip

        空欄の場合でも、パブリッシュされたメッセージ内やルール内でデバイス ID を指定可能です。例えば、JSON エンコードされたメッセージに `device_id` フィールドがあれば、その値が出力デバイス ID になります。ルールエンジンでこの情報を抽出するには、以下のような SQL を使用できます：

        ```sql
        SELECT
         payload,
         `my_device` as payload.device_id
        ```

        ただし、このフィールドで固定指定したデバイス ID が優先されます。

        :::

      - **Table**：データを書き込む IoTDB のテーブル名を指定します。

      - **Align Timeseries**：デフォルトは無効です。有効にすると、グループ化されたアラインド時系列のタイムスタンプ列が IoTDB に一度だけ保存され、グループ内の各時系列で重複保存されなくなります。詳細は [Aligned timeseries](https://iotdb.apache.org/UserGuide/V1.1.x/Data-Concept/Data-Model-and-Terminology.html#aligned-timeseries) を参照してください。

      - **Write Data** の設定で、MQTT メッセージから IoTDB データを生成する方法を指定します。

        **Write Data** セクションでは、必要な数だけ項目を含むテンプレートを定義でき、各行に必要なコンテキスト情報を記述します。このテンプレートを基に MQTT メッセージから IoTDB データが生成されます。データ書き込みテンプレートは CSV ファイルによる一括設定もサポートしています。詳細は [バッチ設定](#batch-setting) を参照してください。

        例として以下のテンプレートを考えます：

        ::: tip 補足
        
        **Column Category** は SQL Dialect で `Table Model` を選択した場合のみ表示されます。
        
        :::

        | Column Category | Timestamp | Measurement | Data Type | Value    |
        | --------------- | --------- | ----------- | --------- | -------- |
        | field           |           | index       | INT32     | ${index} |
        |                 |           | temperature | FLOAT     | ${temp}  |

        `Timestamp` と `Value` はプレースホルダー構文をサポートし、変数で埋められます。`Timestamp` を省略すると、システムの現在時刻（ミリ秒単位）が自動的に設定されます。

        例えば、MQTT メッセージは以下のように構成できます：

          ```json
        {
          "index": "42",
          "temp": "32.67"
          }
          ```

6. **Fallback Actions**：（任意）メッセージ配信失敗時の信頼性向上のため、プライマリ Sink が処理に失敗した場合にトリガーされるフォールバックアクションを1つ以上定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

7. **Advanced settings**：（任意）[高度な設定](#advanced-configurations) を参照してください。

8. （任意）**Test Connectivity** をクリックして、Sink が Apache IoTDB サーバーに接続できるかをテストします。

### バッチ設定

Apache IoTDB では、ダッシュボード上で数百件のデータを同時に書き込む設定は困難な場合があります。これを解決するため、EMQX はデータ書き込みのバッチ設定機能を提供しています。

**Write Data** の設定時に、CSV ファイルから挿入操作用のフィールドを一括インポートできます。

1. **Write Data** テーブルの **Batch Setting** ボタンをクリックし、**Import Batch Setting** ポップアップを開きます。

2. 指示に従ってバッチ設定テンプレートファイルをダウンロードし、テンプレートにデータ書き込み設定を記入します。デフォルトのテンプレート内容は以下の通りです：

   ::: tip 補足

   以下は `Table Model` 用のデフォルトテンプレートです。`Tree Model` では **Column Category** 列はありません。

   :::

   | Column Category | Timestamp | Measurement | Data Type | Value             | 備考（任意）                                               |
   | --------------- | --------- | ----------- | --------- | ----------------- | ---------------------------------------------------------- |
   | tag             | now       | clientid    | text      | ${clientid}       |                                                            |
   | field           | now       | temp        | float     | ${payload.temp}   | フィールド、値、データ型は必須。データ型は boolean, int32, int64, float, double, text が利用可能 |
   | attribute       | now       | hum         | text      | ${payload.hum}    |                                                            |
   | attribute       | now       | status      | text      | ${payload.status} |                                                            |

   - **Column Category**：カラムのデータモデル。`tag`、`field`、`attribute` のいずれか。`tag` は文字列である必要があり、`field` または `attribute` の使用が推奨されます。
   - **Timestamp**：`${var}` 形式のプレースホルダーをサポートし、タイムスタンプ形式で指定します。以下の特殊文字も使用可能：
     - now：現在のミリ秒タイムスタンプ
     - now_ms：現在のミリ秒タイムスタンプ
     - now_us：現在のマイクロ秒タイムスタンプ
     - now_ns：現在のナノ秒タイムスタンプ
   - **Measurement**：フィールド名
   - **Data Type**：データ型（boolean, int32, int64, float, double, text）
   - **Value**：書き込むデータ値。定数または `${var}` 形式のプレースホルダーをサポートし、データ型と一致する必要があります。
   - **備考**：CSV ファイル内のメモ用で、EMQX にはインポートされません。

   ファイルサイズは 1MB 以下、行数は 2000 行以下の CSV のみサポートされます。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting** ポップアップにアップロード後、**Import** をクリックしてバッチ設定を完了します。

4. インポート後、**Write Data** テーブルでさらにデータを調整できます。

## ルールのテスト

EMQX ダッシュボード内蔵の WebSocket クライアントを使って、Apache IoTDB Sink とルールの動作をテストできます。

1. ダッシュボード左メニューの **Diagnose** -> **WebSocket Client** をクリックします。

2. 現在の EMQX インスタンスの接続情報を入力します。

   - ローカルで EMQX を実行している場合はデフォルト値を使用できます。
   - 認証設定などでデフォルト設定を変更している場合は、ユーザー名やパスワードを入力してください。

3. **Connect** をクリックしてクライアントを EMQX インスタンスに接続します。

4. 下にスクロールしてパブリッシュエリアに移動し、メッセージ内にデバイス ID を指定して以下を入力します：

   - **Topic**：`root/sg27`

     :::tip

     トピックが `root` で始まらない場合、自動的に `root` がプレフィックスされます。例えば `test/sg27` にメッセージをパブリッシュすると、デバイス名は `root.test.sg27` になります。ルールとトピックの設定が正しく、該当トピックのメッセージが Sink に転送されるようにしてください。

     :::

   - **Payload**：

     ```json
      {
       "value": "37.6",
       "device_id": "root.sg27"
      }
     ```
     
      ::: tip
     
      `Write Data` テンプレートは以下の通りです：
     
     ```
      now, "temp", float, "${payload.value}"
     ```
     
      :::

   - **QoS**：`2`

7. **Publish** をクリックしてメッセージを送信します。

   Sink とルールが正常に作成されていれば、メッセージは指定された時系列テーブルにパブリッシュされているはずです。

8. IoTDB のコマンドラインインターフェースでメッセージを確認します。上記の Docker 環境を使用している場合は、以下のコマンドでサーバーに接続できます：

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

このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じたカスタマイズが可能な高度な設定オプションを説明します。コネクター作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                     | 説明                                                         | 推奨値             |
| ------------------------ | ------------------------------------------------------------ | ------------------ |
| HTTP Pipelining          | サーバーに対して連続して応答を待たずに送信可能な HTTP リクエスト数を指定します。<br />正の整数値で最大パイプライン数を表します。<br />`1` は従来のリクエスト-レスポンスモデルで、1リクエスト送信後に応答を待ってから次を送信します。<br />値を大きくすると複数リクエストをバッチ送信でき、ネットワークリソースの効率的利用と往復時間の短縮が可能です。 | `100`              |
| Pool Type                | EMQX と Apache IoTDB 間のコネクション管理・分配のアルゴリズム戦略を定義します。<br />`random` は利用可能なコネクションプールからランダムに選択し、シンプルかつ均等な分配を実現します。<br />`hash` はハッシュアルゴリズムでリクエストを一貫して特定コネクションにマッピングし、クライアント ID やトピック名に基づく負荷分散など決定的な分配が必要な場合に適します。<br />**注意**：適切なプールタイプはユースケースや分配特性によります。 | `random`           |
| Connection Pool Size     | Apache IoTDB サービスとのコネクションプール内で維持可能な同時接続数を指定します。<br />システムのスケーラビリティとパフォーマンス管理に寄与します。<br />**注意**：適切なサイズはシステムリソース、ネットワークレイテンシ、ワークロードに依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の恐れがあります。 | `8`                |
| Connect Timeout          | EMQX が Apache IoTDB HTTP サーバーへの接続確立を試みる最大待機時間（秒）を指定します。<br />**注意**：適切なタイムアウト設定はシステム性能とリソース利用のバランスに重要です。様々なネットワーク条件下でテストし最適値を決定してください。 | `15`               |
| HTTP Request Max Retries | EMQX と Apache IoTDB 間の通信で HTTP リクエストが失敗した場合の最大再試行回数を指定します。 | `2`                |
| Start Timeout            | 自動起動されたリソースが正常状態になるまで待機する最大時間（秒）を指定します。<br />この設定により、Apache IoTDB のデータベースインスタンスなど接続先リソースが完全に稼働しデータ処理準備が整うまで操作を進めないようにします。 | `5`                |
| Buffer Pool Size         | EMQX と Apache IoTDB 間のイーグレス型ブリッジでデータフロー管理に割り当てるバッファワーカープロセス数を指定します。<br />これらのワーカーはデータを一時的に保存・処理し、ターゲットサービスへの送信を円滑にします。イングレスのみのブリッジでは「0」に設定可能です。 | `18`               |
| Request TTL              | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。<br />リクエストがこの TTL を超えてバッファに滞留するか、送信後に Apache IoTDB からの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`               |
| Health Check Interval    | Apache IoTDB への接続の自動ヘルスチェックを行う間隔（秒）を指定します。 | `15`               |
| Max Buffer Queue Size    | Apache IoTDB データ統合における各バッファワーカーがバッファリング可能な最大バイト数を指定します。<br />バッファワーカーはデータを一時保存し、効率的なデータフローを実現します。システム性能やデータ転送要件に応じて調整してください。 | `265`              |
| Query Mode               | メッセージ送信の最適化のため、`asynchronous`（非同期）または `synchronous`（同期）クエリモードを選択できます。<br />非同期モードでは IoTDB への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントが IoTDB 到着前にメッセージを受信する可能性があります。 | `Async`            |
| Inflight Window          | 「インフライトクエリ」とは、開始されたがまだ応答やアックを受け取っていないクエリを指します。<br />この設定は、Connector が Apache IoTDB と通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br />`query_mode` が `async` の場合、このパラメータは特に重要です。同一 MQTT クライアントからのメッセージを厳密な順序で処理したい場合は、この値を 1 に設定してください。 | `100`              |

## さらに詳しく

EMQX は Apache IoTDB とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます：

**ブログ：**

[Time-Series Database (TSDB) for IoT: The Missing Piece](https://www.emqx.com/en/blog/time-series-database-for-iot-the-missing-piece)
