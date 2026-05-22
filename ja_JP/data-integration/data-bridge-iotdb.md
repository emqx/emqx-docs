# Apache IoTDB への MQTT データ取り込み

<<<<<<< HEAD
[Apache IoTDB](https://iotdb.apache.org/)は、多種多様なIoTデバイスやシステムから生成される大量の時系列データを処理するために設計された、高性能かつスケーラブルな時系列データベースです。

EMQXはApache IoTDBとのシームレスなデータ統合を提供しており、EMQXで受信したリアルタイムのMQTTメッセージを[REST API V2](https://iotdb.apache.org/UserGuide/latest/API/RestServiceV2.html)を介してIoTDBに転送できます。この統合は一方向のデータフローをサポートし、MQTTデータを効率的な時系列ストレージおよび分析のためにIoTDBに書き込みます。

本ページでは、EMQXとApache IoTDBの統合方法を紹介し、統合の作成および検証手順を段階的に説明します。
=======
[Apache IoTDB](https://iotdb.apache.org/)は、多種多様なIoTデバイスやシステムから生成される膨大な時系列データを効率的に処理するために設計された高性能かつスケーラブルな時系列データベースです。

EMQXはApache IoTDBとのシームレスなデータ統合を提供しており、EMQXで受信したリアルタイムのMQTTメッセージを[REST API V2](https://iotdb.apache.org/UserGuide/latest/API/RestServiceV2.html)を通じてIoTDBに転送できます。この統合は一方向のデータフローをサポートし、MQTTデータをIoTDBに書き込むことで効率的な時系列の保存と分析を実現します。

本ページでは、EMQXとApache IoTDBの統合方法を紹介し、統合の作成および検証手順をステップバイステップで説明します。
>>>>>>> origin/release-6.1

## 動作概要

Apache IoTDBデータ統合はEMQXの組み込み機能であり、MQTTベースの時系列データを追加のコーディングなしでApache IoTDBに取り込むことを可能にします。EMQXの組み込み[ルールエンジン](./rules.md)を活用することで、データのフィルタリング、変換、転送を簡素化し、IoTDBでの効率的な保存とクエリを実現します。

以下の図は、EMQXとIoTDB間の典型的なデータ統合アーキテクチャを示しています。<!-- この画像はIoTDB専用に修正が必要です -->

<img src="./assets/IoTDB_bridge_architecture.png" alt="IoTDB_bridge_architecture" style="zoom:67%;" />

データ統合のワークフローは以下の通りです：

<<<<<<< HEAD
1. **メッセージのパブリッシュと受信**：デバイスはMQTT経由でEMQXに接続し、テレメトリデータ、ステータス更新、イベント情報を含むメッセージをパブリッシュします。ルールエンジンが受信メッセージを評価します。
2. **ルールベースの処理**：定義されたルールにマッチするメッセージが選択され、必要に応じてフィールドのフィルタリング、データ形式の変換、ペイロードの拡充などの変換が適用されます。
3. **データバッファリング**：信頼性向上のため、IoTDBが一時的に利用できない場合にEMQXはメッセージをメモリにバッファします。必要に応じてメモリ圧迫を避けるためディスクにオフロード可能です。統合やEMQXノードの再起動時にはバッファデータは保持されません。
4. **IoTDBへのデータ取り込み**：マッチしたルールに対して、EMQXはIoTDBシンクをトリガーし、処理済みデータをIoTDBに時系列データとして書き込みます。
5. **データの保存と活用**：IoTDBに保存されたデータは、デバイス監視、資産追跡、予知保全、運用最適化などの下流アプリケーションでクエリや分析に利用可能です。

## 特長とメリット

IoTDBとのデータ統合は、効果的なデータ処理と保存を実現するための以下の特長とメリットを提供します：

- **ノーコードのIoTデータパイプライン**

  組み込みのルールとシンクを利用して、EMQXとApache IoTDB間の完全なMQTTから時系列データへのパイプラインをカスタムコードや外部サービスなしで構築可能です。

- **MQTTからIoTDBモデルへの柔軟なマッピング**

  TreeモデルとTableモデルの両方をサポートし、デバイスモデリングやクエリ要件に合った構造でMQTTデータをIoTDBに書き込めます。

- **取り込みと保存の分離**

  EMQXはバースト的な高頻度MQTTトラフィックを吸収し、IoTDBは耐久性のある時系列ストレージに専念することで、システムの安定性とレジリエンスを向上させます。

- **本番対応のスケーラビリティ**

  デバイス数やデータ量に応じて水平スケール可能で、大規模なIoT、IIoT、エネルギー分野のシナリオに適しています。

- **分析対応の時系列データ**

  IoTDBに書き込まれたデータは直接クエリ、集計、分析が可能であり、ビッグデータエンジンとの統合による高度な分析や長期的なインサイト取得にも対応します。

## はじめる前に

このセクションでは、EMQXダッシュボードでApache IoTDBデータ統合を作成する前に完了すべき準備事項を説明します。
=======
1. **メッセージのパブリッシュと受信**：デバイスはMQTTでEMQXに接続し、テレメトリデータ、ステータス更新、イベント情報を含むメッセージをパブリッシュします。ルールエンジンが受信メッセージを評価します。
2. **ルールベースの処理**：定義されたルールにマッチしたメッセージが選択され、必要に応じてフィールドのフィルタリング、データ形式の変換、ペイロードの強化などの変換が適用されます。
3. **データバッファリング**：信頼性向上のため、IoTDBが一時的に利用不可の場合はEMQXがメッセージをメモリにバッファします。必要に応じてメモリ圧迫を避けるためにディスクにオフロードできます。統合またはEMQXノードが再起動するとバッファデータは保持されません。
4. **IoTDBへのデータ取り込み**：マッチしたルールに対して、EMQXはIoTDBシンクをトリガーし、処理済みデータをIoTDBに時系列データとして書き込みます。
5. **データの保存と活用**：IoTDBに保存されたデータは、デバイス監視、資産追跡、予知保全、運用最適化などの下流アプリケーションでクエリや分析に利用できます。

## 特長とメリット

IoTDBとのデータ統合は、効果的なデータ処理と保存を実現するために以下の特長とメリットを提供します：

- **ノーコードのIoTデータパイプライン**

  組み込みのルールとシンクを使い、カスタムコードや外部サービスなしでEMQXとApache IoTDB間の完全なMQTTから時系列データへのパイプラインを構築可能です。

- **MQTTからIoTDBモデルへの柔軟なマッピング**

  ツリーモデルとテーブルモデルの両方をサポートし、デバイスモデリングやクエリ要件に合わせた構造でMQTTデータをIoTDBに書き込めます。

- **取り込みと保存の分離**

  EMQXはバースト的かつ高頻度のMQTTトラフィックを吸収し、IoTDBは耐久性のある時系列保存に専念することで、システムの安定性とレジリエンスを向上させます。

- **本番対応のスケーラビリティ**

  デバイス数やデータ量に応じて水平スケール可能で、大規模なIoT、IIoT、エネルギー分野に適しています。

- **分析に適した時系列データ**

  IoTDBに書き込まれたデータは直接クエリ、集計、分析できるほか、ビッグデータエンジンと連携して高度な分析や長期的な洞察を得られます。

## はじめる前に

このセクションでは、EMQXダッシュボードでApache IoTDBデータ統合を作成する前に完了すべき準備について説明します。
>>>>>>> origin/release-6.1

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Apache IoTDB サーバーの起動

<<<<<<< HEAD
ここでは[Docker](https://www.docker.com/)を使ったApache IoTDBサーバーの起動方法を紹介します。IoTDBの設定で`enable_rest_service=true`が有効になっていることを確認してください。
=======
ここでは[Docker](https://www.docker.com/)を使ってApache IoTDBサーバーを起動する方法を紹介します。IoTDBの設定で`enable_rest_service=true`が有効になっていることを確認してください。
>>>>>>> origin/release-6.1

以下のコマンドを実行して、RESTインターフェースが有効なApache IoTDBサーバーを起動します：

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

<<<<<<< HEAD
詳細は[Docker HubのIoTDB実行情報](https://hub.docker.com/r/apache/iotdb)をご覧ください。

### データベースの作成

IoTDBはTreeモデルとTableモデルの2つのデータモデルをサポートしています。データベース作成前に、コネクターとシンクで使用する**SQL方言**（TreeまたはTable）を確認し、それに応じてデータベースを作成してください。

- **Treeモデル**の場合はデータベースのみ作成すれば十分です。
- **Tableモデル**の場合は、データベースを作成後、データ取り込み用のテーブルを作成する必要があります。

詳細な手順はIoTDBユーザーガイドをご参照ください：

- [Treeモデル用のデータベース作成](https://iotdb.apache.org/UserGuide/latest/Basic-Concept/Operate-Metadata_apache.html#_1-1-create-database)
- [Tableモデル用のデータベース作成](https://iotdb.apache.org/UserGuide/latest-Table/Basic-Concept/Database-Management_apache.html#_1-1-create-a-database)
- [Tableモデル用のテーブル作成](https://iotdb.apache.org/UserGuide/latest-Table/Basic-Concept/Table-Management_apache.html#_1-1-create-a-table)

## IoTDBコネクターの作成

Apache IoTDBデータ統合を作成するには、Apache IoTDBシンクをApache IoTDBサーバーに接続するためのコネクターを作成する必要があります。
=======
詳細は[Docker HubのIoTDB実行方法](https://hub.docker.com/r/apache/iotdb)をご参照ください。

### データベースの作成

IoTDBはツリーモデルとテーブルモデルの2つのデータモデルをサポートしています。データベース作成前に、コネクターとシンクで使用する**SQL方言**（TreeまたはTable）を確認し、それに応じたデータベースを作成してください。
>>>>>>> origin/release-6.1

- **ツリーモデル**の場合はデータベースのみ作成すればよいです。
- **テーブルモデル**の場合は、まずデータベースを作成し、その後データ取り込み用のテーブルを作成する必要があります。

<<<<<<< HEAD
1. EMQXダッシュボードで、**Integrations** -> **Connectors** に移動します。

2. 右上の**Create**をクリックします。

3. **Create Connector**ページで**Apache IoTDB**を選択します。

4. コネクターを設定します：

   - **Connector Name**：コネクターの一意な名前を入力します。大文字・小文字の英数字の組み合わせを推奨します。例：`my_iotdb`
   - **Description**：（任意）コネクターの簡単な説明
   - **Driver**：IoTDB接続に使用するプロトコルを選択します。
     - `REST API`：IoTDB RESTサービスのエンドポイント（例：`http://localhost:18080`）を**IoTDB REST Service Base URL**に入力します。
     - `Thrift Protocol`：IoTDB Thriftサーバーのアドレスを**Server Host**に入力します。

   - **SQL Dialect**：EMQXがデバイスデータをIoTDBに書き込む際のデータモデルを選択します。
     - `Tree Model`：階層的な時系列パスとしてデータを書き込み、パスベースのデバイス・測定管理に適します。
     - `Table Model`：リレーショナルテーブルにデータを書き込み、デバイスタイプやカテゴリ別の管理に適します。
   - **Database Name**：**SQL Dialect**が`Table Model`の場合、接続するデータベース名を指定します。
   - **Username** と **Password**：EMQXがApache IoTDBサーバーに認証するための資格情報を入力します。
   - **IoTDB Version**：Apache IoTDBのバージョンを選択します。
   - **Enable TLS**：Apache IoTDBサーバーへの暗号化接続を有効にします。詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。
   - 任意のチューニングは[高度な設定](#advanced-configurations)の**Advanced Settings**を参照してください。

5. （任意）**Test Connectivity**をクリックして、コネクターがApache IoTDBサーバーに正常に接続できるか確認します。

6. **Create**をクリックしてコネクター作成を完了します。

   表示されるダイアログで、**Back to Connector List**または**Create Rule**を選択して、ルールおよびApache IoTDBシンクの設定を続行できます。詳細は[ルールとApache IoTDBシンクの作成](#create-a-rule-and-apache-iotdb-sink)を参照してください。

## Apache IoTDBシンクを用いたルールの作成

このセクションでは、EMQXでソースMQTTトピック`root/#`からのメッセージを処理し、処理結果を設定済みのApache IoTDBシンク経由でApache IoTDBに時系列データとして保存するルールの作成方法を示します。

### SQLを定義したルールの作成

1. EMQXダッシュボードで、**Integration** -> **Rules**に移動します。
=======
詳細な手順はIoTDBユーザーガイドをご参照ください：

- [ツリーモデル用のデータベース作成](https://iotdb.apache.org/UserGuide/latest/Basic-Concept/Operate-Metadata_apache.html#_1-1-create-database)
- [テーブルモデル用のデータベース作成](https://iotdb.apache.org/UserGuide/latest-Table/Basic-Concept/Database-Management_apache.html#_1-1-create-a-database)
- [テーブルモデル用のテーブル作成](https://iotdb.apache.org/UserGuide/latest-Table/Basic-Concept/Table-Management_apache.html#_1-1-create-a-table)

## IoTDBコネクターの作成

Apache IoTDBデータ統合を作成するには、Apache IoTDBシンクとApache IoTDBサーバーを接続するコネクターを作成する必要があります。

EMQXはREST APIまたはThriftプロトコルを通じてIoTDBと通信をサポートしています。

1. EMQXダッシュボードで**Integrations** -> **Connectors**に移動します。

2. 右上の**Create**をクリックします。

3. **Create Connector**ページで**Apache IoTDB**を選択します。

4. コネクターを設定します：

   - **Connector Name**：コネクターの一意な名前を入力します。大文字・小文字の英数字の組み合わせを使用してください。例：`my_iotdb`
   - **Description**：（任意）コネクターの簡単な説明
   - **Driver**：IoTDBへの接続に使用するプロトコルを選択します。
     - `REST API`：IoTDB RESTサービスのエンドポイント（例：`http://localhost:18080`）を**IoTDB REST Service Base URL**に入力
     - `Thrift Protocol`：IoTDB Thriftサーバーのアドレスを**Server Host**に入力

   - **SQL Dialect**：EMQXがデバイスデータをIoTDBに書き込む際のIoTDBデータモデルを選択します。
     - `Tree Model`：階層的な時系列パスとしてデータを書き込み、パスベースのデバイス・測定管理に適しています。
     - `Table Model`：リレーショナルテーブルにデータを書き込み、デバイス種別やカテゴリごとの管理に適しています。
   - **Database Name**：**SQL Dialect**が`Table Model`の場合、接続するデータベース名を指定する必要があります。
   - **Username**および**Password**：EMQXがApache IoTDBサーバーに認証するための資格情報を入力します。
   - **IoTDB Version**：Apache IoTDBのバージョンを選択します。
   - **Enable TLS**：有効にするとApache IoTDBサーバーへの暗号化接続を確立します。詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)をご参照ください。
   - 任意のチューニングは[高度な設定](#advanced-configurations)の**Advanced Settings**を参照してください。

5. （任意）**Test Connectivity**をクリックして、コネクターがApache IoTDBサーバーに正常に接続できるか検証します。

6. **Create**をクリックしてコネクター作成を完了します。

   表示されるダイアログで**Back to Connector List**または**Create Rule**を選択し、ルールとApache IoTDBシンクの設定を続けられます。詳細は[ルールとApache IoTDBシンクの作成](#create-a-rule-and-apache-iotdb-sink)をご覧ください。

## Apache IoTDBシンクを用いたルールの作成

このセクションでは、EMQXでソースMQTTトピック`root/#`からメッセージを処理し、処理結果を設定済みのApache IoTDBシンク経由でApache IoTDBに時系列データとして保存するルールの作成方法を示します。

### SQLを定義したルールの作成

1. EMQXダッシュボードで**Integration** -> **Rules**に移動します。
>>>>>>> origin/release-6.1

2. ページ右上の**Create**をクリックします。

3. ルールIDを入力します（例：`my_rule`）。

<<<<<<< HEAD
4. **SQL editor**に以下のステートメントを入力します。これはトピックパターン`root/#`にマッチするMQTTメッセージを転送します：
=======
4. **SQLエディター**に以下の文を入力し、トピックパターン`root/#`にマッチするMQTTメッセージを転送します：
>>>>>>> origin/release-6.1

   ```sql
   SELECT
     *
   FROM
     "root/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習やテストが可能です。

   :::

<<<<<<< HEAD
5. 処理結果をIoTDBに書き込むためにApache IoTDBシンクをルールに追加します。詳細は[Apache IoTDBシンクの追加](#add-an-apache-iotdb-sink)を参照してください。
=======
5. 処理結果をIoTDBに書き込むため、ルールにApache IoTDBシンクを追加します。詳細は[Apache IoTDBシンクの追加](#add-an-apache-iotdb-sink)を参照してください。
>>>>>>> origin/release-6.1

6. **Create Rule**ページで設定内容を確認し、**Save**をクリックしてルールを作成します。

ルール作成後、**Rules**一覧に表示されます。**Actions (Sink)**タブをクリックすると、このルールに関連付けられたIoTDBシンクを確認できます。

<<<<<<< HEAD
また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示できます。ここでは、トピック`root/#`のメッセージが`my_rule`ルールで処理され、IoTDBに書き込まれる様子が可視化されます。

### Apache IoTDBシンクの追加

1. ルール画面右側の**Add Action**ボタンをクリックし、ルールにマッチした際にトリガーされるアクションを定義します。このアクションは処理済みデータをIoTDBに転送します。

2. **Type of Action**ドロップダウンで`Apache IoTDB`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のIoTDBシンクを選択することも可能ですが、本例では新規作成を想定しています。

3. シンクの名前と説明を入力します。

4. **Connector**ドロップダウンで先ほど作成したコネクター`my_iotdb`を選択します。利用可能なコネクターがない場合は隣のボタンから作成できます。詳細は[IoTDBコネクターの作成](#create-an-iotdb-connector)を参照してください。
=======
また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示可能です。`root/#`トピックのメッセージが`my_rule`ルールで処理されIoTDBに書き込まれる様子が確認できます。

### Apache IoTDBシンクの追加

1. ルールの右側にある**Add Action**ボタンをクリックし、ルールマッチ時にトリガーされるアクションを定義します。このアクションは処理済みデータをIoTDBに転送します。

2. **Type of Action**ドロップダウンで`Apache IoTDB`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のIoTDBシンクを選択することも可能ですが、ここでは新規作成を想定しています。

3. シンクの名前と説明を入力します。

4. **Connector**ドロップダウンで先ほど作成したコネクター`my_iotdb`を選択します。利用可能なコネクターがない場合は隣のボタンから作成可能です。[IoTDBコネクターの作成](#create-an-iotdb-connector)を参照してください。
>>>>>>> origin/release-6.1

5. シンクの設定を行います：

      * **SQL Dialect**：Apache IoTDBシンクがIoTDBにデータを書き込む方法を選択します。コネクターで選択したSQL方言と一致させる必要があります。

        * `Tree Model`：IoTDBの時系列パスとしてデータを書き込みます。各シンクレコードはデバイスパスに挿入され、その下に測定値が個別の時系列として書き込まれます。このモデル選択時は**Device ID**フィールドを指定可能です。
<<<<<<< HEAD
        * `Table Model`：IoTDBのリレーショナルテーブルにデータを書き込みます。各シンクレコードは指定テーブルの行として挿入され、フィールドはテーブルのカラムにマッピングされます。このモデル選択時は**Table**フィールドを指定必須です。
=======
        * `Table Model`：IoTDBのリレーショナルテーブルにデータを書き込みます。各シンクレコードは指定テーブルの行として挿入され、フィールドはテーブルのカラムにマッピングされます。このモデル選択時は**Table**フィールドの指定が必須です。
>>>>>>> origin/release-6.1
        
      * **Device ID**（任意）：IoTDBインスタンスに時系列データを転送・挿入する際のデバイス名として使用する特定のデバイスIDを入力します。

        :::tip

<<<<<<< HEAD
        空欄の場合でも、パブリッシュされたメッセージ内やルール内でデバイスIDを指定可能です。例えば、JSONエンコードされたメッセージに`device_id`フィールドがあれば、その値が出力デバイスIDとなります。ルールエンジンで抽出するSQL例は以下の通りです：
=======
        空欄の場合でも、パブリッシュされたメッセージ内やルール内でデバイスIDを指定可能です。例えば、JSONエンコードされたメッセージに`device_id`フィールドが含まれていれば、その値が出力デバイスIDになります。ルールエンジンでこの情報を抽出するには、以下のようなSQLを使用できます：
>>>>>>> origin/release-6.1

        ```sql
        SELECT
         payload,
         `my_device` as payload.device_id
        ```

<<<<<<< HEAD
        ただし、このフィールドに固定で設定したデバイスIDが優先されます。
=======
        ただし、このフィールドで固定したデバイスIDが優先されます。
>>>>>>> origin/release-6.1

        :::

      - **Table**：データを書き込むIoTDBテーブル名を指定します。

<<<<<<< HEAD
      - **Align Timeseries**：デフォルトは無効。これを有効にすると、グループ化されたアラインド時系列のタイムスタンプ列がIoTDB内で一度だけ保存され、グループ内の各時系列での重複保存が回避されます。詳細は[Aligned timeseries](https://iotdb.apache.org/UserGuide/V1.1.x/Data-Concept/Data-Model-and-Terminology.html#aligned-timeseries)を参照してください。

      - **Write Data**を設定し、MQTTメッセージからIoTDBデータを生成する方法を指定します。

        **Write Data**セクションでは、必要なだけ複数の項目を含むテンプレートを定義可能で、各行に必要なコンテキスト情報を含めます。このテンプレートに基づきMQTTメッセージからIoTDBデータが生成されます。書き込みテンプレートはCSVファイルによる一括設定もサポートしています。詳細は[バッチ設定](#batch-setting)を参照してください。

        例として以下のテンプレートを考えます：
=======
      - **Align Timeseries**：デフォルトで無効。これを有効にすると、グループ化されたアラインド時系列のタイムスタンプ列がIoTDBに一度だけ保存され、グループ内の各時系列で重複保存されなくなります。詳細は[Aligned timeseries](https://iotdb.apache.org/UserGuide/V1.1.x/Data-Concept/Data-Model-and-Terminology.html#aligned-timeseries)を参照してください。

      - **Write Data**を設定し、MQTTメッセージからIoTDBデータを生成する方法を指定します。

        **Write Data**セクションでは、必要な数だけ項目を含むテンプレートを定義可能で、各行に必要なコンテキスト情報を記述します。このテンプレートに基づき、MQTTメッセージからIoTDBデータが生成されます。書き込みテンプレートはCSVファイルによる一括設定もサポートしています。詳細は[バッチ設定](#batch-setting)を参照してください。

        例えば、以下のテンプレートを考えます：
>>>>>>> origin/release-6.1

        ::: tip 注意
        
        **Column Category**はSQL方言で`Table Model`を選択した場合のみ表示されます。
        
        :::

        | Column Category | Timestamp | Measurement | Data Type | Value    |
        | --------------- | --------- | ----------- | --------- | -------- |
        | field           |           | index       | INT32     | ${index} |
        |                 |           | temperature | FLOAT     | ${temp}  |

<<<<<<< HEAD
        `Timestamp`と`Value`はプレースホルダー構文をサポートし、変数で埋められます。`Timestamp`を省略すると、現在のシステム時刻（ミリ秒単位）が自動入力されます。

        その場合、MQTTメッセージは以下のように構成されます：
=======
        `Timestamp`と`Value`はプレースホルダー構文をサポートし、変数で埋められます。`Timestamp`を省略すると、現在のシステム時刻（ミリ秒単位）で自動補完されます。

        その場合、MQTTメッセージは以下のように構成できます：
>>>>>>> origin/release-6.1

          ```json
        {
          "index": "42",
          "temp": "32.67"
          }
          ```

<<<<<<< HEAD
6. **フォールバックアクション**：（任意）メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリシンクがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

7. **高度な設定**：（任意）[高度な設定](#advanced-configurations)を参照してください。

8. （任意）**Test Connectivity**をクリックしてシンクがApache IoTDBサーバーに接続可能かテストします。

### バッチ設定

Apache IoTDBでは、ダッシュボード上で数百件のデータを同時に書き込む設定は困難です。これを解決するため、EMQXは書き込みのバッチ設定機能を提供しています。

**Write Data**の設定時に、CSVファイルから挿入操作用のフィールドを一括インポートできます。
=======
6. **フォールバックアクション**：（任意）メッセージ配信失敗時の信頼性向上のため、プライマリシンクが処理に失敗した場合にトリガーされる1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
>>>>>>> origin/release-6.1

7. **高度な設定**：（任意）[高度な設定](#advanced-configurations)を参照してください。

<<<<<<< HEAD
2. 指示に従ってバッチ設定テンプレートファイルをダウンロードし、テンプレートに書き込み設定を記入します。デフォルトのテンプレート内容は以下の通りです：

   ::: tip 注意

   以下は`Table Model`用のデフォルトテンプレートです。`Tree Model`では**Column Category**列はありません。

   :::

   | Column Category | Timestamp | Measurement | Data Type | Value             | 備考（任意）                                               |
   | --------------- | --------- | ----------- | --------- | ----------------- | ---------------------------------------------------------- |
   | tag             | now       | clientid    | text      | ${clientid}       |                                                            |
   | field           | now       | temp        | float     | ${payload.temp}   | フィールド、値、データ型は必須。データ型はboolean, int32, int64, float, double, textが利用可能 |
   | attribute       | now       | hum         | text      | ${payload.hum}    |                                                            |
   | attribute       | now       | status      | text      | ${payload.status} |                                                            |

   - **Column Category**：カラムのデータモデル。`tag`、`field`、`attribute`がサポートされます。`tag`は文字列である必要があり、`field`または`attribute`の使用が推奨されます。
=======
8. （任意）**Test Connectivity**をクリックして、シンクがApache IoTDBサーバーに接続可能かテストします。

### バッチ設定

Apache IoTDBでは、ダッシュボード上で数百件のデータを同時に書き込む設定は困難な場合があります。これを解決するため、EMQXは書き込みのバッチ設定機能を提供しています。

**Write Data**の設定時に、CSVファイルから挿入操作用のフィールドを一括インポートできます。

1. **Write Data** テーブルの **Batch Setting** ボタンをクリックし、**Import Batch Setting** ポップアップを開きます。

2. 指示に従いバッチ設定テンプレートファイルをダウンロードし、テンプレートにデータ書き込み設定を記入します。デフォルトテンプレートの内容は以下の通りです：

   ::: tip 注意

   以下は`Table Model`用のデフォルトテンプレートです。`Tree Model`では**Column Category**列はありません。

   :::

   | Column Category | Timestamp | Measurement | Data Type | Value             | 備考（任意）                                               |
   | --------------- | --------- | ----------- | --------- | ----------------- | ---------------------------------------------------------- |
   | tag             | now       | clientid    | text      | ${clientid}       |                                                            |
   | field           | now       | temp        | float     | ${payload.temp}   | フィールド、値、データ型は必須。利用可能なデータ型はboolean, int32, int64, float, double, text |
   | attribute       | now       | hum         | text      | ${payload.hum}    |                                                            |
   | attribute       | now       | status      | text      | ${payload.status} |                                                            |

   - **Column Category**：カラムのデータモデル。`tag`、`field`、`attribute`がサポートされます。`tag`は文字列でなければならず、`field`または`attribute`が推奨されます。
>>>>>>> origin/release-6.1
   - **Timestamp**：`${var}`形式のプレースホルダーをサポートし、タイムスタンプ形式が必要です。以下の特殊文字でシステム時刻を挿入可能です：
     - now：現在のミリ秒タイムスタンプ
     - now_ms：現在のミリ秒タイムスタンプ
     - now_us：現在のマイクロ秒タイムスタンプ
     - now_ns：現在のナノ秒タイムスタンプ
   - **Measurement**：フィールド名
<<<<<<< HEAD
   - **Data Type**：データ型（boolean, int32, int64, float, double, text）
   - **Value**：書き込むデータ値。定数または`${var}`形式のプレースホルダーをサポートし、データ型と一致する必要があります。
   - **備考**：CSVファイル内のメモ用で、EMQXにはインポートされません。

   1MB未満かつ2000行以内のCSVファイルのみサポートされます。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックしバッチ設定を完了します。

4. インポート後、**Write Data**テーブル内でさらにデータを調整可能です。
=======
   - **Data Type**：データ型。boolean, int32, int64, float, double, textが選択可能
   - **Value**：書き込むデータ値。定数または`${var}`形式のプレースホルダーをサポートし、データ型と一致する必要があります。
   - **備考**：CSVファイル内の注釈用で、EMQXにはインポートされません。

   ファイルサイズは1MB以下、行数は2000行以下のCSVファイルのみ対応しています。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロード後、**Import**をクリックしてバッチ設定を完了します。

4. インポート後、**Write Data** テーブル内でさらにデータを調整できます。
>>>>>>> origin/release-6.1

## ルールのテスト

EMQXダッシュボード内蔵のWebSocketクライアントを使って、Apache IoTDBシンクとルールの動作をテストできます。
<<<<<<< HEAD

1. ダッシュボード左メニューの**Diagnose** -> **WebSocket Client**をクリックします。

2. 現在のEMQXインスタンスへの接続情報を入力します。

   - ローカルでEMQXを実行している場合はデフォルト値を使用可能です。
   - 認証設定などでEMQXのデフォルト構成を変更している場合は、ユーザー名やパスワードの入力が必要です。

3. **Connect**をクリックしてクライアントをEMQXに接続します。

4. ページ下部のパブリッシュエリアにスクロールし、メッセージ内にデバイスIDを指定して以下を入力します：
=======

1. ダッシュボード左メニューの**Diagnose** -> **WebSocket Client**をクリックします。

2. 現在のEMQXインスタンスの接続情報を入力します。

   - ローカルでEMQXを実行している場合はデフォルト値を使用可能です。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要です。

3. **Connect**をクリックしてクライアントをEMQXに接続します。

4. 下にスクロールしてパブリッシュエリアに移動し、メッセージ内にデバイスIDを指定して以下を入力します：
>>>>>>> origin/release-6.1

   - **Topic**：`root/sg27`

     :::tip

<<<<<<< HEAD
     トピックが`root`で始まらない場合、自動的に`root`がプレフィックスされます。例えば`test/sg27`にメッセージをパブリッシュすると、デバイス名は`root.test.sg27`となります。ルールとトピックの設定が正しく、該当トピックのメッセージがシンクに転送されるようにしてください。
=======
     トピックが`root`で始まらない場合、自動的に`root.`がプレフィックスされます。例えば、`test/sg27`にメッセージをパブリッシュすると、デバイス名は`root.test.sg27`になります。ルールとトピックの設定が正しく、該当トピックのメッセージがシンクに転送されるようにしてください。
>>>>>>> origin/release-6.1

     :::

   - **Payload**：

     ```json
      {
       "value": "37.6",
       "device_id": "root.sg27"
      }
     ```
     
      ::: tip
     
      `Write Data`テンプレートは以下の通りです：
     
     ```
      now, "temp", float, "${payload.value}"
     ```
     
      :::

   - **QoS**：`2`

7. **Publish**をクリックしてメッセージを送信します。

   シンクとルールが正常に作成されていれば、メッセージは指定したApache IoTDBサーバーの時系列テーブルにパブリッシュされているはずです。

<<<<<<< HEAD
8. IoTDBのコマンドラインインターフェースでメッセージを確認します。上記のようにDockerで起動している場合は、以下のコマンドでサーバーに接続できます：
=======
8. IoTDBのコマンドラインインターフェースを使ってメッセージを確認します。上記のDocker環境の場合、以下のコマンドでサーバーに接続できます：
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じた動作カスタマイズのための高度な設定オプションを説明します。コネクター作成時に**Advanced Settings**を展開し、ビジネス要件に応じて以下の設定を行えます。

| 項目                     | 説明                                                         | 推奨値             |
| ------------------------ | ------------------------------------------------------------ | ------------------ |
| HTTP Pipelining          | サーバーに対して個別のレスポンスを待たずに連続して送信可能なHTTPリクエスト数を指定します。正の整数値で最大パイプライン数を表します。<br />`1`の場合は従来のリクエスト-レスポンスモデルで、各リクエスト送信後にレスポンスを待ちます。値を大きくすると複数リクエストをバッチ送信でき、ラウンドトリップ時間を削減しネットワークリソースを効率的に活用します。 | `100`              |
| Pool Type                | EMQXとApache IoTDB間のコネクション管理・分配戦略を定義します。<br />`random`は利用可能なコネクションプールからランダムに選択し、シンプルで均等な分散を実現します。<br />`hash`はハッシュアルゴリズムを用いてリクエストを一貫して特定のコネクションにマッピングします。クライアントIDやトピック名に基づくロードバランシングなど、決定論的分散が必要な場合に適します。<br />**注意**：適切なプールタイプはユースケースや分散特性に依存します。 | `random`           |
| Connection Pool Size     | Apache IoTDBサービスとの接続プールで維持可能な同時接続数を指定します。システムのスケーラビリティとパフォーマンス管理に役立ちます。<br />**注意**：適切なサイズはシステムリソース、ネットワークレイテンシ、ワークロードに依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の可能性があります。 | `8`                |
| Connect Timeout          | EMQXがApache IoTDB HTTPサーバーへの接続確立を試みる最大待機時間（秒）を指定します。<br />**注意**：適切なタイムアウト設定はシステム性能とリソース利用のバランスに重要です。ネットワーク状況に応じて最適値を検証してください。 | `15`               |
| HTTP Request Max Retries | EMQXとApache IoTDB間の通信でHTTPリクエストが失敗した場合に再試行する最大回数を指定します。 | `2`                |
| Start Timeout            | 自動起動されたリソースが正常状態になるまで待機する最大時間（秒）を指定します。リソース作成リクエストに対する応答前に、接続先リソース（例：Apache IoTDBのデータベースインスタンス）が完全に稼働していることを確認するための設定です。 | `5`                |
| Buffer Pool Size         | EMQXとApache IoTDB間のイグレス型ブリッジでデータフロー管理に割り当てるバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前に一時的にデータを保持・処理します。イングレス（インバウンド）専用のブリッジでは不要なため`0`に設定可能です。 | `18`               |
| Request TTL              | バッファに入ったリクエストが有効とみなされる最大期間（秒）を指定します。リクエストがTTLを超えてバッファに滞留するか、送信後にApache IoTDBからの応答やアックが期限内に得られない場合、リクエストは期限切れとみなされます。 | `45`               |
| Health Check Interval    | コネクターがApache IoTDBとの接続状態を自動的に監視する間隔（秒）を指定します。 | `15`               |
| Max Buffer Queue Size    | Apache IoTDBデータ統合における各バッファワーカーがバッファ可能な最大バイト数を指定します。バッファワーカーはデータ送信前の一時保管を担当し、システム性能やデータ転送要件に応じて調整してください。 | `265`              |
| Query Mode               | メッセージ送信要件に応じて`asynchronous`または`synchronous`のクエリモードを選択可能です。非同期モードではIoTDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがIoTDB到達前にメッセージを受信する可能性があります。 | `Async`            |
| Inflight Window          | 「インフライトクエリ」とは、開始されたがまだ応答やアックを受け取っていないクエリを指します。コネクターがApache IoTDBと通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br />`query_mode`が`async`の場合、このパラメータは特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は、この値を1に設定してください。 | `100`              |
=======
このセクションでは、コネクターのパフォーマンスを最適化し、特定のシナリオに応じた動作をカスタマイズするための高度な設定オプションを説明します。コネクター作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                     | 説明                                                         | 推奨値             |
| ------------------------ | ------------------------------------------------------------ | ------------------ |
| HTTP Pipelining          | サーバーに対して連続してレスポンスを待たずに送信可能なHTTPリクエスト数を指定します。正の整数値で、`1`の場合は従来のリクエスト-レスポンスモデルとなり、各リクエスト送信後にレスポンスを待ちます。値を大きくすると複数リクエストをバッチ送信でき、ラウンドトリップ時間を削減しネットワークリソースを効率化します。 | `100`              |
| Pool Type                | EMQXとApache IoTDB間のコネクション管理・分配アルゴリズムを定義します。<br />`random`は利用可能な接続プールからランダムに接続を選択し、シンプルでバランスの取れた分配を実現します。<br />`hash`はハッシュアルゴリズムを用いてリクエストを一貫して接続にマッピングし、クライアントIDやトピック名に基づくロードバランシングなど決定的な分配が必要な場合に適します。<br />**注意**：適切なプールタイプはユースケースや求める分配特性によります。 | `random`           |
| Connection Pool Size     | Apache IoTDBサービスとの接続プールに保持可能な同時接続数を指定します。システムのスケーラビリティとパフォーマンス管理に役立ちます。<br />**注意**：適切なプールサイズはシステムリソース、ネットワークレイテンシ、ワークロードに依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の原因となります。 | `8`                |
| Connect Timeout          | EMQXがApache IoTDB HTTPサーバーへの接続確立を試みる最大待機時間（秒）を指定します。<br />**注意**：適切なタイムアウト設定はシステムパフォーマンスとリソース利用のバランスに重要です。ネットワーク状況を考慮して最適値をテストしてください。 | `15`               |
| HTTP Request Max Retries | EMQXとApache IoTDB間の通信でHTTPリクエストが失敗した場合の最大再試行回数を指定します。 | `2`                |
| Start Timeout            | 自動起動されたリソースが正常状態になるまで待機する最大時間（秒）を指定します。これにより、Apache IoTDBのデータベースインスタンスなど接続リソースが完全に稼働し、データ処理可能になるまで統合処理を進めないようにします。 | `5`                |
| Buffer Pool Size         | EMQXとApache IoTDB間のイグレス（送信）タイプのブリッジでデータフロー管理に割り当てるバッファワーカープロセス数を指定します。これらのワーカーはターゲットサービスに送信する前のデータを一時的に保持・処理します。イングレス（受信）専用のブリッジでは`0`に設定可能です。 | `18`               |
| Request TTL              | リクエストがバッファに入ってから有効とみなされる最大時間（秒）を指定します。この時間を超えてバッファ内にあるか、送信後にApache IoTDBから適時のレスポンスやアックが得られない場合、リクエストは期限切れとみなされます。 | `45`               |
| Health Check Interval    | Apache IoTDBへの接続の自動ヘルスチェックを行う間隔（秒）を指定します。 | `15`               |
| Max Buffer Queue Size    | Apache IoTDBデータ統合における各バッファワーカーがバッファ可能な最大バイト数を指定します。バッファワーカーはデータ送信前の一時保管を担い、システム性能やデータ転送要件に応じて調整してください。 | `265`              |
| Query Mode               | メッセージ送信要件に応じて`asynchronous`または`synchronous`のクエリモードを選択可能です。非同期モードではIoTDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがIoTDB到着前にメッセージを受信する可能性があります。 | `Async`            |
| Inflight Window          | 「インフライトクエリ」とは開始されたがレスポンスやアックをまだ受け取っていないクエリを指します。ConnectorがApache IoTDBと通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br />`query_mode`が`async`の場合、このパラメータは特に重要です。同一MQTTクライアントからのメッセージを厳密な順序で処理する必要がある場合は、この値を1に設定してください。 | `100`              |
>>>>>>> origin/release-6.1

## 参考情報

EMQXはApache IoTDBとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細をご覧ください：

**ブログ：**

[IoT向け時系列データベース（TSDB）：欠けていたピース](https://www.emqx.com/en/blog/time-series-database-for-iot-the-missing-piece)
