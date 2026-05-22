# LindormへのMQTT取り込み

[Alibaba Cloud Lindorm](https://cn.aliyun.com/product/apsaradb/lindorm?from_alibabacloud=)は、高スループット、高圧縮率、スケーラビリティを備えたクラウドネイティブのマルチモデルデータベースです。時系列（TSDB）、ワイドテーブル、ベクターデータモデルをサポートし、IoTテレメトリ、産業監視、コネクテッドカーなどのシナリオで広く利用されています。

<<<<<<< HEAD
EMQXは専用のLindormシンクを提供していませんが、LindormはMySQL互換のインターフェースを備えています。ユーザーはEMQXのデータ統合にあるMySQLシンクを利用して、デバイスデータをLindormに書き込むことが可能です。本ページでは、EMQXのデータ統合を用いてLindormにMQTTデータを抽出・変換・格納し、安定かつ効率的なIoTデータパイプラインを構築する方法を説明します。

## Lindorm

Lindormのバックエンドは複数のデータエンジンをサポートしています。その中でTSDBノードは時系列データに最適化されており、高圧縮率、高同時実行性、効率的なクエリを提供します。MQTTメッセージングプラットフォームとしてのEMQXは、ルールエンジンとデータ統合機能を活用し、複雑なコーディングなしにMQTTメッセージを効率的にLindorm（通常はTSDBノード）へ書き込みます。これにより、デバイスのテレメトリデータを構造化して収集・処理・保存することが可能です。
=======
EMQXは専用のLindorm Sinkを提供していませんが、LindormはMySQL互換のインターフェースを備えています。ユーザーはEMQXのデータ統合にあるMySQL Sinkを利用して、デバイスデータをLindormに書き込むことが可能です。本ページでは、EMQXのデータ統合とLindormを用いてMQTTデータを抽出・変換・格納し、安定かつ効率的なIoTデータパイプラインを構築する方法を説明します。

## Lindorm

Lindormのバックエンドは複数のデータエンジンをサポートしています。その中でTSDBノードは時系列データに最適化されており、高圧縮、高同時実行、効率的なクエリを実現しています。MQTTメッセージングプラットフォームであるEMQXは、ルールエンジンとデータ統合機能を活用し、複雑なコーディングなしでMQTTメッセージを効率的にLindorm（通常はTSDBノード）に書き込むことができます。これにより、デバイスのテレメトリデータを構造化して収集・処理・保存できます。
>>>>>>> origin/release-6.1

![lindorm_architecture](./assets/lindorm_architecture.png)

ワークフローは以下の通りです：

- **デバイスがEMQXに接続**：IoTデバイスがEMQXとMQTT接続を確立します。
<<<<<<< HEAD
- **デバイスメッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリや状態データをパブリッシュし、EMQXのルールエンジンがこれを受信・マッチングします。
- **ルールエンジンによるメッセージ処理**：トピックに基づいてメッセージをマッチングし、データ変換、フィルタリング、コンテキスト付加などのアクションを実行します。
- **Lindormへの書き込み**：トリガーされたルールはMySQLシンクを使い、LindormのMySQL互換インターフェースを呼び出します。
- **Lindormバックエンドの保存・最適化**：Lindormはスキーマ定義に基づきデータを時系列またはワイドテーブル形式に整理し、圧縮、インデックス作成、集約を行います。
- **外部アプリケーションによるクエリ・分析**：ビジネスシステムやQuickBI、DataVなどの可視化ツールがSQLクエリでデバイス状態監視、指標追跡、傾向分析を実施します。
=======
- **デバイスメッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリや状態データをパブリッシュし、EMQXのルールエンジンが受信・マッチングします。
- **ルールエンジンによるメッセージ処理**：トピックに基づいてメッセージをマッチさせ、データ変換、フィルタリング、コンテキスト付加などのアクションを実行します。
- **Lindormへの書き込み**：トリガーされたルールはMySQL Sinkを使い、LindormのMySQL互換インターフェースを呼び出します。
- **Lindormバックエンドの保存と最適化**：Lindormはスキーマ定義に基づき、時系列またはワイドテーブル形式でデータを整理し、圧縮、インデックス作成、集約処理を行います。
- **外部アプリケーションによるクエリと分析**：業務システムや可視化ツール（QuickBI、DataVなど）がSQLクエリを通じてデバイス状態監視、指標追跡、傾向分析を実施します。
>>>>>>> origin/release-6.1

## 特長と利点

<<<<<<< HEAD
LindormとEMQXの統合により、以下のメリットがあります：

- **高同時書き込み性能**：LindormのTSDBノードは高同時実行シナリオに最適化されており、大量のデバイステレメトリ取り込みをサポート。産業監視やスマートシティに最適です。
- **メッセージ変換**：EMQXルールでメッセージを処理・変換してからLindormに書き込むため、保存や利用が簡素化されます。
- **柔軟なフィールドマッピングとルール処理**：EMQXルールエンジンはメッセージフィールドの動的抽出・変換を可能にし、カスタマイズ可能なSQLテンプレートで正確なデータ構造制御が可能です。
- **効率的な圧縮と永続化ストレージ**：Lindormは時系列および構造化データの保存を最適化し、高頻度書き込みシナリオでのストレージコスト削減と長期保存を両立します。
- **ランタイムメトリクス**：各シンクの総メッセージ数、成功・失敗数、現在の処理レートなどのランタイムメトリクスを確認できます。
=======
LindormとEMQXの統合により、以下の利点があります：

- **高同時書き込み性能**：Lindorm TSDBノードは高同時実行シナリオ向けに設計されており、大量のデバイステレメトリ取り込みに対応。産業監視やスマートシティに最適です。
- **メッセージ変換**：EMQXのルールでメッセージを処理・変換してからLindormに書き込むため、保存や利用が簡素化されます。
- **柔軟なフィールドマッピングとルール処理**：EMQXルールエンジンはメッセージフィールドの動的抽出・変換を可能にし、カスタマイズ可能なSQLテンプレートで精密なデータ構造制御ができます。
- **効率的な圧縮と永続化ストレージ**：Lindormは時系列および構造化データの保存を最適化し、高頻度書き込み時のストレージコストを効果的に削減しつつ、長期保存をサポートします。
- **ランタイムメトリクス**：各Sinkの総メッセージ数、成功・失敗数、現在の処理レートなどのランタイムメトリクスを確認可能です。
>>>>>>> origin/release-6.1

EMQXの豊富なメッセージ変換機能とLindormの保存・クエリ機能を組み合わせ、多様なビジネスニーズに対応する信頼性の高いスケーラブルなIoTデータパイプラインを構築できます。

## はじめに

<<<<<<< HEAD
このセクションでは、EMQXでLindormデータ統合を作成する前に必要な準備（Lindormインスタンス作成、接続設定、テーブル作成）について説明します。
=======
このセクションでは、EMQXでLindormデータ統合を作成する前に必要な準備として、Lindormインスタンスの作成、接続設定、テーブル作成について説明します。
>>>>>>> origin/release-6.1

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解

### Lindormインスタンスの作成と接続

<<<<<<< HEAD
統合前にLindormインスタンスを作成し、ネットワークアクセスを設定してください：

1. Alibaba Cloudコンソールにログインし、[Lindormインスタンスを作成](https://www.alibabacloud.com/help/en/lindorm/getting-started/create-an-instance)します。
2. [ホワイトリストアクセスを設定](https://www.alibabacloud.com/help/en/lindorm/getting-started/configure-a-whitelist)して、EMQXホストのIPからのアクセスを許可します。
3. EMQXのデプロイ方法に応じて、適切なLindorm接続方法を選択します：
   - EMQXがAlibaba Cloud ECSまたはVPC上にある場合は、Lindormの内部VPCアクセスアドレスを使用し、安定性と低レイテンシを確保します。
   - EMQXがローカルデータセンターや他クラウドにある場合：
=======
統合前にLindormインスタンスを作成し、ネットワークアクセスを設定してください。

1. Alibaba Cloudコンソールにログインし、[Lindormインスタンスを作成](https://www.alibabacloud.com/help/en/lindorm/getting-started/create-an-instance)します。
2. EMQXホストIPのアクセスを許可するために、[ホワイトリスト設定](https://www.alibabacloud.com/help/en/lindorm/getting-started/configure-a-whitelist)を行います。
3. EMQXのデプロイ方法に応じて、適切なLindorm接続方法を選択します：
   - EMQXがAlibaba Cloud ECSまたはVPC上にデプロイされている場合は、Lindormの内部VPCアクセスアドレスを使用し、安定性と低レイテンシを確保します。
   - EMQXがローカルデータセンターや他クラウドにデプロイされている場合：
>>>>>>> origin/release-6.1
     - Lindormのパブリックアクセスを有効化します。
     - パブリックSQLエンドポイント（通常ポート`33060`）を使用します。
     - EMQXホストのパブリックIPをLindormのホワイトリストに追加します。

詳細は[公式接続ガイド](https://www.alibabacloud.com/help/en/lindorm/getting-started/connect-to-an-instance)および[TSDBエンジンのJDBC接続](https://www.alibabacloud.com/help/en/lindorm/user-guide/use-the-jdbc-driver-for-lindorm-to-connect-to-and-use-lindormtsdb)を参照してください。

### データベースとテーブルの作成

```sql
CREATE DATABASE emqx_data;

CREATE TABLE demo_sensor (
  device_id VARCHAR(255) COMMENT 'TAG',
  time BIGINT,
  msg VARCHAR(255),
  PRIMARY KEY (device_id, time)
);
```

このテーブル構造は時系列データに適しており、`device_id`をタグ、`time`をタイムスタンプ、`msg`を業務データとして使用します。

## コネクターの作成

<<<<<<< HEAD
MySQLプロトコル経由でLindormシンクを作成する前に、EMQXでMySQLコネクターを作成し、Lindormとの接続を確立する必要があります。
=======
MySQLプロトコル経由でLindorm Sinkを作成する前に、EMQXでMySQLコネクターを作成してLindormとの接続を確立します。
>>>>>>> origin/release-6.1

1. ダッシュボードの **Integration** -> **Connectors** に移動し、**Create** をクリックします。

2. コネクタータイプとして **MySQL** を選択し、**Next** をクリックします。

3. 以下を設定します：
   - **Connector Name**：英数字で例：`my_lindorm`
   - **Server Host**：
<<<<<<< HEAD
     - EMQXがAlibaba Cloud VPCネットワーク（ECSなど）内にある場合は、Lindormインスタンスの内部SQLアドレスを入力します。形式は通常Lindormが提供する内部ドメインで、例：`ld-xxxx-proxy-sql-lindorm.lindorm.rds.aliyuncs.com:33060`。
     - EMQXがローカルデータセンターや非Alibaba Cloud環境の場合は、Lindormコンソールでパブリックアクセスを有効にし、割り当てられたパブリックSQLアドレスを入力します。形式は通常：`ld-xxxx-proxy-sql-public.lindorm.rds.aliyuncs.com:33060`。
=======
>>>>>>> origin/release-6.1
     
     - EMQXがAlibaba Cloud VPCネットワーク（ECSインスタンスなど）内にある場合は、Lindormインスタンスの内部SQLアドレスを入力します。通常はLindormが提供する内部ドメイン形式で、例：`ld-xxxx-proxy-sql-lindorm.lindorm.rds.aliyuncs.com:33060`
     - EMQXがローカルデータセンターやAlibaba Cloud以外の環境にある場合は、Lindormコンソールでパブリックアクセスを有効にし、割り当てられたパブリックSQLアドレスを入力します。形式は通常：`ld-xxxx-proxy-sql-public.lindorm.rds.aliyuncs.com:33060`
     
     EMQXがデプロイされているホストのIPアドレスがLindormのアクセスホワイトリストに追加されていることを確認してください。
     
   - **Database Name**：`emqx_data`
   - **Username**：`root`
   - **Password**：`public`

<<<<<<< HEAD
4. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations)を参照。
=======
4. 詳細設定（任意）：[高度な設定](#advanced-configurations)を参照してください。
>>>>>>> origin/release-6.1

5. **Create**をクリックする前に、**Test Connectivity**を押してコネクターがLindormに接続できるかテストできます。

<<<<<<< HEAD
6. 下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択し、シンクを指定したルール作成を続行できます。
=======
6. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択し、Sinkを指定したルール作成を続行できます。
>>>>>>> origin/release-6.1

## Lindormシンクルールの作成

ここでは、トピック`#`のMQTTメッセージを処理し、Lindormの`demo_sensor`テーブルに書き込むルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** に移動します。

2. **Create**をクリックし、ルールIDに`my_rule`を入力します。

<<<<<<< HEAD
3. ルールID `my_rule`を入力し、SQLエディターにルールを記述します。この例では、トピック`#`のMQTTメッセージをLindormに保存します。**SELECT**句で指定するフィールドはSQLテンプレートで使用する変数をすべて含めてください。ルールSQLは以下の通りです：
=======
3. ルールIDを`my_rule`とし、SQLエディターに以下のルールを入力します。例として、トピック`#`のMQTTメッセージをLindormに保存します。**SELECT**句で選択するフィールドはSQLテンプレートで使用するすべての変数を含めてください。
>>>>>>> origin/release-6.1

   ```sql
   SELECT
     clientid AS device_id,
     timestamp AS time,
     payload.msg AS msg
   FROM
     "#"
   ```

   ::: tip

   初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習・テストが可能です。

   :::

4. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをLindormに送信します。

<<<<<<< HEAD
5. **Type of Action**ドロップダウンから`MySQL`を選択します。**Action**はデフォルトの`Create Action`のままにします。既にシンクを作成している場合は選択も可能です。本例では新規シンクを作成します。

6. シンク名を入力します。英数字の組み合わせで指定してください。

7. **Connector**ドロップダウンから先ほど作成した`my_lindorm`を選択します。新規作成も可能です。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. 機能に応じて**SQLテンプレート**を設定します：

   注意：これは前処理済みSQLのため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。
=======
5. **Type of Action**ドロップダウンから`MySQL`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能ですが、ここでは新規Sinkを作成します。

6. Sinkの名前を入力します。名前は英数字の組み合わせにしてください。

7. **Connector**ドロップダウンから先ほど作成した`my_lindorm`を選択します。新規コネクターを作成する場合は隣のボタンから可能です。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. 利用する機能に応じて**SQLテンプレート**を設定します：

   注意：これは事前処理されたSQLのため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。
>>>>>>> origin/release-6.1

   ```sql
   INSERT INTO demo_sensor(device_id, time, msg) VALUES (
     ${device_id},
     ${time},
     ${msg}
   )
   ```

<<<<<<< HEAD
   SQLテンプレート内でプレースホルダー変数が未定義の場合は、**SQLテンプレート**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を設定できます：

   - **無効（デフォルト）**：ルールエンジンは文字列`undefined`をデータベースに挿入します。
   - **有効**：変数が未定義の場合、`NULL`を挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。
=======
   SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQLテンプレート**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を切り替えられます：

   - **無効**（デフォルト）：ルールエンジンは文字列`undefined`をデータベースに挿入します。
   - **有効**：変数未定義時に`NULL`を挿入します。

     ::: tip

     可能な限りこのオプションは常に有効にしてください。無効化は後方互換性確保のためのみ推奨されます。
>>>>>>> origin/release-6.1

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

<<<<<<< HEAD
10. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations)を参照。

11. **Create**ボタンをクリックし、シンク設定を完了します。新しいシンクが**Action Outputs**に追加されます。

12. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでルールの作成が完了しました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると新しいMySQLシンクが表示されます。

また、**Integration** -> **Flow Designer**を開くとトポロジーが表示され、トピック`#`のメッセージがMySQLに送信・保存されていることが確認できます。
=======
10. 詳細設定（任意）：[高度な設定](#advanced-configurations)を参照してください。

11. **Create**ボタンをクリックし、Sink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

12. **Create Rule**画面に戻り、設定内容を確認後、**Create**をクリックしてルールを生成します。

これでルールが正常に作成されました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブをクリックすると、新しいMySQL Sinkが表示されます。

また、**Integration** -> **Flow Designer**でトポロジーを確認でき、トピック`#`のメッセージがMySQLに送信・保存されていることがわかります。
>>>>>>> origin/release-6.1

## ルールのテスト

MQTTXを使ってトピック`sensor/1`にメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_test -t sensor/1 -m '{ "msg": "hello lindorm" }'
```

<<<<<<< HEAD
シンクの稼働状況を確認すると、1件の新規インカミングメッセージと1件の新規アウトゴーイングメッセージがあるはずです。

APIを使ってLindormにデータが正常に書き込まれているか確認します：
=======
Sinkの稼働状況を確認すると、新規の受信メッセージ数と送信メッセージ数が1件ずつ増えているはずです。

APIを使ってLindormにデータが正常に書き込まれたか確認します：
>>>>>>> origin/release-6.1

```bash
curl -X POST http://${LINDORM_SERVER}:8242/api/v2/sql?database=emqx_data \
  -H "Content-Type: text/plain" \
  -d 'SELECT * FROM demo_sensor'
```

## 詳細設定

<<<<<<< HEAD
MySQLコネクターおよびシンク（Lindorm）向けの詳細設定オプションの説明：

| 項目                      | 説明                                                         | デフォルト |
| ------------------------- | ------------------------------------------------------------ | ---------- |
| **Connection Pool Size**  | MySQLサービス通信のためにプールで維持する同時接続数。システムリソースや負荷に応じて調整してください。 | `8`        |
| **Start Timeout**         | 作成後にリソース準備完了を待つ最大時間（秒）。Lindorm接続の健全性を確認してからデータ処理を開始します。 | `5s`       |
| **Buffer Pool Size**      | Lindorm送信前にデータフローを管理するワーカープロセス数。インジェストのみの場合は`0`に設定可能。 | `16`       |
| **Request TTL**           | バッファリングされたリクエストのTTL（秒）。この時間を超えたリクエストは期限切れとみなされます。 | `45s`      |
| **Health Check Interval** | Lindorm接続の自動健全性チェックの間隔（秒）。                         | `15s`      |
| **Max Buffer Queue Size** | バッファーワーカーがLindormにフラッシュする前に保持可能な最大バイト数。 | `256MB`    |
| **Max Batch Size**        | Lindormに送信する1バッチあたりの最大レコード数。単一レコード転送の場合は`1`に設定。 | `1`        |
| **Query Mode**            | `sync`または`async`モードを選択。非同期モードはMQTTメッセージのパブリッシュをブロックしませんが、厳密な順序性に影響する可能性があります。 | `async`    |
| **In-flight Window**      | 応答待ちのインフライトリクエストの最大数。同一クライアントからの厳密なメッセージ順序が必要な場合は`1`に設定してください。 | `100`      |
=======
MySQLコネクターおよびSink（Lindorm）向けの高度な設定オプションの詳細説明：

| フィールド                 | 説明                                                                 | デフォルト |
| ------------------------- | ------------------------------------------------------------------ | --------- |
| **Connection Pool Size**  | MySQLサービス通信で維持する同時接続数。システムリソースや負荷に応じて調整してください。 | `8`       |
| **Start Timeout**         | 作成後にリソース準備が整うまでの最大待機時間（秒）。Lindorm接続の正常性確認に使用します。 | `5s`      |
| **Buffer Pool Size**      | Lindorm送信前にデータフローを管理するワーカープロセス数。Ingressのみの場合は`0`に設定。 | `16`      |
| **Request TTL**           | バッファリングされたリクエストのTTL（秒）。これを超えたリクエストは期限切れとみなされます。 | `45s`     |
| **Health Check Interval** | Lindorm接続の自動ヘルスチェック間隔（秒）。                             | `15s`     |
| **Max Buffer Queue Size** | バッファワーカーがデータをLindormにフラッシュする前に保持できる最大バイト数。      | `256MB`   |
| **Max Batch Size**        | Lindormに送信するバッチの最大レコード数。単一レコード送信時は`1`に設定。          | `1`       |
| **Query Mode**            | `sync`または`async`モードを選択。非同期モードはMQTTメッセージのパブリッシュをブロックしませんが、厳密な順序性に影響する可能性があります。 | `async`   |
| **In-flight Window**      | 応答待ちのリクエスト最大数。同一クライアントからの厳密なメッセージ順序が必要な場合は`1`に設定。 | `100`     |
>>>>>>> origin/release-6.1
