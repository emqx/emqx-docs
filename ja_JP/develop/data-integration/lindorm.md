# LindormへのMQTT取り込み

[Alibaba Cloud Lindorm](https://cn.aliyun.com/product/apsaradb/lindorm?from_alibabacloud=)は、高スループット、高圧縮率、スケーラビリティを備えたクラウドネイティブのマルチモデルデータベースです。時系列（TSDB）、ワイドテーブル、ベクトルデータモデルをサポートし、IoTテレメトリ、産業監視、コネクテッドカーなどのシナリオで広く利用されています。

EMQXは専用のLindorm Sinkを提供していませんが、LindormはMySQL互換のインターフェースを備えています。ユーザーはEMQXのデータ統合にあるMySQL Sinkを利用して、デバイスデータをLindormに書き込むことが可能です。本ページでは、EMQXのデータ統合機能を用いてLindormにMQTTデータを抽出・変換・格納し、安定かつ効率的なIoTデータパイプラインを構築する方法を解説します。

## Lindorm

Lindormのバックエンドは複数のデータエンジンをサポートしています。その中でTSDBノードは時系列データに最適化されており、高圧縮、高同時実行、効率的なクエリを実現しています。MQTTメッセージングプラットフォームであるEMQXは、ルールエンジンとデータ統合機能を活用し、複雑なコーディングなしにMQTTメッセージをLindorm（通常はTSDBノード）へ効率的に書き込むことができます。これにより、デバイスのテレメトリデータを構造化して収集・処理・保存することが可能です。

![lindorm_architecture](./assets/lindorm_architecture.png)

ワークフローは以下の通りです：

- **デバイスがEMQXに接続**：IoTデバイスがEMQXにMQTT接続を確立します。
- **デバイスメッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリやステータスデータをパブリッシュし、EMQXのルールエンジンが受信・マッチングします。
- **ルールエンジンによるメッセージ処理**：トピックに基づいてメッセージをマッチングし、データ変換、フィルタリング、コンテキスト付加などのアクションを実行します。
- **Lindormへの書き込み**：トリガーされたルールはMySQL Sinkを使い、LindormのMySQL互換インターフェースを呼び出します。
- **Lindormのバックエンド保存・最適化**：Lindormはスキーマ定義に基づき、時系列またはワイドテーブル形式でデータを整理し、圧縮、インデックス、集計を行います。
- **外部アプリケーションによるクエリ・分析**：業務システムや可視化ツール（QuickBI、DataVなど）がSQLクエリを通じてデバイスの状態監視、指標追跡、傾向分析を行います。

## 特長とメリット

LindormとEMQXの統合により、以下の利点があります：

- **高同時書き込み性能**：Lindorm TSDBノードは高同時実行シナリオに設計されており、大量のデバイステレメトリ取り込みに対応。産業監視やスマートシティなどに最適です。
- **メッセージ変換**：EMQXルールでメッセージを処理・変換してからLindormに書き込むため、保存や利用が容易になります。
- **柔軟なフィールドマッピングとルール処理**：EMQXルールエンジンはメッセージフィールドの動的抽出・変換を可能にし、カスタマイズ可能なSQLテンプレートで正確なデータ構造制御を実現します。
- **効率的な圧縮と永続化ストレージ**：Lindormは時系列および構造化データのストレージを最適化し、高頻度書き込みシナリオでのストレージコスト削減と長期データ保持をサポートします。
- **ランタイムメトリクス**：各Sinkの総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスを確認できます。

EMQXの豊富なメッセージ変換機能とLindormの保存・クエリ機能を組み合わせることで、多様なビジネスニーズに応える信頼性とスケーラビリティの高いIoTデータパイプラインを構築できます。

## はじめる前に

このセクションでは、EMQXでLindormデータ統合を作成する前に必要な準備（Lindormインスタンスの作成、接続設定、テーブル作成）について説明します。

### 前提条件

- [ルール](./rules.md)の基本知識
- [データ統合](./data-bridges.md)の基本知識

### Lindormインスタンスの作成と接続

統合前に、Lindormインスタンスを作成し、ネットワークアクセスを設定してください：

1. Alibaba Cloudコンソールにログインし、[Lindormインスタンスを作成](https://www.alibabacloud.com/help/en/lindorm/getting-started/create-an-instance)します。
2. EMQXホストIPのアクセスを許可するために、[ホワイトリスト設定](https://www.alibabacloud.com/help/en/lindorm/getting-started/configure-a-whitelist)を行います。
3. EMQXのデプロイ方法に応じて、適切なLindorm接続方法を選択してください：
   - EMQXがAlibaba Cloud ECSやVPC上にデプロイされている場合、Lindormの内部VPCアクセスアドレスを使用すると安定性と低レイテンシが得られます。
   - EMQXがローカルデータセンターや他クラウドにデプロイされている場合：
     - Lindormのパブリックアクセスを有効化します。
     - パブリックSQLエンドポイント（通常ポート`33060`）を使用します。
     - EMQXホストのパブリックIPをLindormのホワイトリストに追加します。

詳細は[公式接続ガイド](https://www.alibabacloud.com/help/en/lindorm/getting-started/connect-to-an-instance)および[TSDBエンジンのJDBC接続](https://www.alibabacloud.com/help/en/lindorm/user-guide/use-the-jdbc-driver-for-lindorm-to-connect-to-and-use-lindormtsdb)をご参照ください。

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

Lindorm Sink（MySQLプロトコル経由）を作成する前に、EMQXでMySQLコネクターを作成してLindormとの接続を確立する必要があります。

1. ダッシュボードの **Integration** -> **Connectors** に移動し、**Create** をクリックします。

2. コネクタータイプで **MySQL** を選択し、**Next** をクリックします。

3. 以下を設定します：
   - **Connector Name**：英数字で例：`my_lindorm`
   - **Server Host**：
     - EMQXがAlibaba Cloud VPCネットワーク（ECSなど）内にある場合、Lindormインスタンスの内部SQLアドレスを入力します。通常はLindormが提供する内部ドメイン形式で、例：`ld-xxxx-proxy-sql-lindorm.lindorm.rds.aliyuncs.com:33060`
     - EMQXがローカルデータセンターや非Alibaba Cloud環境にある場合、Lindormコンソールでパブリックアクセスを有効にし、割り当てられたパブリックSQLアドレスを入力します。形式は通常：`ld-xxxx-proxy-sql-public.lindorm.rds.aliyuncs.com:33060`
     
     EMQXが稼働するホストのIPアドレスがLindormのホワイトリストに追加されていることを確認してください。
   - **Database Name**：`emqx_data`
   - **Username**：`root`
   - **Password**：`public`

4. 詳細設定（任意）：[高度な設定](#advanced-configurations)を参照してください。

5. **Create**をクリックする前に、**Test Connectivity**でLindormへの接続が成功するか確認できます。

6. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを指定したルール作成を続行できます。

## Lindorm Sinkルールの作成

このセクションでは、トピック`#`のMQTTメッセージを処理し、Lindormの`demo_sensor`テーブルに書き込むルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** に移動します。

2. **Create**をクリックし、ルールIDに`my_rule`を入力します。

3. ルールID`my_rule`を入力し、SQLエディターに以下のルールSQLを記述します。例として、トピック`#`のMQTTメッセージをLindormに保存します。**SELECT**句で指定するフィールドはSQLテンプレートで使用する変数をすべて含めてください。

   ```sql
   SELECT
     clientid AS device_id,
     timestamp AS time,
     payload.msg AS msg
   FROM
     "#"
   ```

   ::: tip

   初心者の方は、**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習やテストが可能です。

   :::

4. + **Add Action**ボタンをクリックして、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをLindormに送信します。

5. **Type of Action**のドロップダウンリストから`MySQL`を選択します。**Action**はデフォルトの`Create Action`のままにします。既にSinkを作成している場合はそれを選択することも可能です。この例では新規Sinkを作成します。

6. Sinkの名前を入力します。名前は英数字の組み合わせで指定してください。

7. **Connector**ドロップダウンから先ほど作成した`my_lindorm`を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメータは[コネクター作成](#コネクターの作成)を参照してください。

8. 利用する機能に応じて**SQLテンプレート**を設定します：

   注意：これは前処理済みのSQLなので、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   INSERT INTO demo_sensor(device_id, time, msg) VALUES (
     ${device_id},
     ${time},
     ${msg}
   )
   ```

   SQLテンプレート内のプレースホルダー変数が未定義の場合、**SQLテンプレート**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を切り替えられます：

   - **無効**（デフォルト）：未定義変数は文字列`undefined`としてデータベースに挿入されます。
   - **有効**：未定義変数は`NULL`として挿入されます。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効にするのは後方互換性確保時のみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. 詳細設定（任意）：[高度な設定](#advanced-configurations)を参照してください。

11. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

12. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでルールが正常に作成されました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいMySQL Sinkが表示されます。

また、**Integration** -> **Flow Designer**を開くとトポロジーが表示され、トピック`#`のメッセージがMySQLに送信・保存されていることが確認できます。

## ルールのテスト

MQTTクライアントMQTTXを使い、`sensor/1`トピックにメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_test -t sensor/1 -m '{ "msg": "hello lindorm" }'
```

Sinkの稼働状況を確認すると、1件の新規受信メッセージと1件の新規送信メッセージがあるはずです。

APIを使ってLindormにデータが正常に書き込まれているか確認します：

```bash
curl -X POST http://${LINDORM_SERVER}:8242/api/v2/sql?database=emqx_data \
  -H "Content-Type: text/plain" \
  -d 'SELECT * FROM demo_sensor'
```

## 高度な設定

MySQLコネクターおよびSink（Lindorm）向けの高度な設定オプションの詳細：

| フィールド                   | 説明                                                                                   | デフォルト |
| ---------------------------- | -------------------------------------------------------------------------------------- | ---------- |
| **Connection Pool Size**      | MySQLサービス通信のためにプールで維持する同時接続数。システムリソースや負荷に応じて調整。 | `8`        |
| **Start Timeout**             | 作成後のリソース準備完了までの最大待機時間（秒）。Lindorm接続の健全性を確保。             | `5s`       |
| **Buffer Pool Size**          | Lindorm送信前にデータフローを管理するワーカー数。Ingressのみの場合は`0`に設定可能。       | `16`       |
| **Request TTL**               | バッファリングされたリクエストのTTL（秒）。これを超えるとリクエストは期限切れとみなされる。 | `45s`      |
| **Health Check Interval**     | Lindorm接続の自動健全性チェック間隔（秒）。                                         | `15s`      |
| **Max Buffer Queue Size**     | バッファワーカーがLindormにフラッシュする前に保持可能な最大バイト数。                   | `256MB`    |
| **Max Batch Size**            | Lindormに送信する1バッチあたりの最大レコード数。単一レコード転送は`1`に設定。           | `1`        |
| **Query Mode**                | `sync`または`async`モードを選択。AsyncはMQTTメッセージパブリッシュのブロックを回避するが、厳密な順序性に影響。 | `async`    |
| **In-flight Window**          | 応答待ちの最大リクエスト数。同一クライアントからの厳密なメッセージ順序が必要な場合は`1`に設定。 | `100`      |
