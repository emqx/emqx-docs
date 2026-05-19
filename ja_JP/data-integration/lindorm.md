# LindormへのMQTT取り込み

[Alibaba Cloud Lindorm](https://cn.aliyun.com/product/apsaradb/lindorm?from_alibabacloud=) は、高スループット、高圧縮率、スケーラビリティを備えたクラウドネイティブのマルチモデルデータベースです。時系列（TSDB）、ワイドテーブル、ベクトルデータモデルをサポートし、IoTテレメトリ、産業モニタリング、コネクテッドカーなどのシナリオで広く利用されています。

EMQXは専用のLindorm Sinkを提供していませんが、LindormはMySQL互換のインターフェースを備えています。ユーザーはEMQXのデータ統合にあるMySQL Sinkを利用して、デバイスデータをLindormに書き込むことが可能です。本ページでは、EMQXのデータ統合を用いてMQTTデータを抽出・変換・格納し、安定かつ効率的なIoTデータパイプラインを構築する方法を説明します。

## Lindorm

Lindormのバックエンドは複数のデータエンジンをサポートしています。その中でTSDBノードは時系列データに最適化されており、高圧縮、高同時接続、効率的なクエリを実現しています。MQTTメッセージプラットフォームであるEMQXは、ルールエンジンとデータ統合機能を活用し、複雑なコーディングなしにMQTTメッセージをLindorm（通常はTSDBノード）へ効率的に書き込みます。これにより、デバイスのテレメトリデータを構造化して収集・処理・保存できます。

![lindorm_architecture](./assets/lindorm_architecture.png)

ワークフローは以下の通りです：

- **デバイスがEMQXに接続**：IoTデバイスがEMQXとMQTT接続を確立します。
- **デバイスのメッセージパブリッシュと受信**：デバイスは特定のトピックにテレメトリや状態データをパブリッシュし、EMQXのルールエンジンが受信・マッチングします。
- **ルールエンジンによるメッセージ処理**：トピックに基づいてメッセージをマッチングし、データ変換、フィルタリング、コンテキスト付加などのアクションを実行します。
- **Lindormへの書き込み**：トリガーされたルールはMySQL Sinkを使い、LindormのMySQL互換インターフェースを呼び出します。
- **Lindormのバックエンド保存と最適化**：Lindormはスキーマ定義に基づき、時系列またはワイドテーブル形式でデータを整理し、圧縮、インデックス付け、集約を行います。
- **外部アプリケーションによるクエリと分析**：業務システムや可視化ツール（QuickBI、DataVなど）がSQLクエリを通じてデバイス状態監視、指標追跡、トレンド分析を行います。

## 特長と利点

LindormとEMQXの統合により、以下のメリットがあります：

- **高同時書き込み性能**：Lindorm TSDBノードは高同時接続シナリオ向けに設計されており、大量のデバイステレメトリ取り込みに対応。産業モニタリングやスマートシティに最適です。
- **メッセージ変換**：EMQXルールでメッセージを処理・変換してからLindormに書き込むため、保存や利用が簡単になります。
- **柔軟なフィールドマッピングとルール処理**：EMQXルールエンジンはメッセージフィールドの動的抽出・変換を可能にし、カスタマイズ可能なSQLテンプレートで正確なデータ構造制御ができます。
- **効率的な圧縮と永続化ストレージ**：Lindormは時系列および構造化データのストレージを最適化し、高頻度書き込みでもストレージコストを削減しつつ長期保存をサポートします。
- **ランタイムメトリクス**：各Sinkの総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスを確認可能です。

EMQXの豊富なメッセージ変換機能とLindormのストレージ・クエリ機能を組み合わせることで、多様なビジネスニーズに対応した信頼性の高いスケーラブルなIoTデータパイプラインを構築できます。

## はじめる前に

本節では、EMQXでLindormデータ統合を作成する前に必要な準備、Lindormインスタンスの作成、接続設定、テーブル作成について説明します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解

### Lindormインスタンスの作成と接続

統合前にLindormインスタンスを作成し、ネットワークアクセスを設定してください：

1. Alibaba Cloudコンソールにログインし、[Lindormインスタンスを作成](https://www.alibabacloud.com/help/en/lindorm/getting-started/create-an-instance)します。
2. EMQXホストIPのアクセスを許可するために、[ホワイトリストを設定](https://www.alibabacloud.com/help/en/lindorm/getting-started/configure-a-whitelist)します。
3. EMQXのデプロイ方法に応じて、適切なLindorm接続方法を選択します：
   - EMQXがAlibaba Cloud ECSまたはVPC内にデプロイされている場合は、Lindormの内部VPCアクセスアドレスを使用し、安定性と低レイテンシを確保します。
   - EMQXがローカルデータセンターや他クラウドにデプロイされている場合：
     - Lindormのパブリックアクセスを有効化します。
     - パブリックSQLエンドポイント（通常ポート`33060`）を使用します。
     - EMQXホストのパブリックIPをLindormのホワイトリストに追加します。

詳細は[公式接続ガイド](https://www.alibabacloud.com/help/en/lindorm/getting-started/connect-to-an-instance)および[TSDBエンジン用JDBC接続](https://www.alibabacloud.com/help/en/lindorm/user-guide/use-the-jdbc-driver-for-lindorm-to-connect-to-and-use-lindormtsdb)を参照してください。

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

Lindorm Sink（MySQLプロトコル経由）を作成する前に、EMQXでMySQLコネクターを作成し、Lindormとの接続を確立する必要があります。

1. ダッシュボードの **Integration** -> **Connectors** に移動し、**Create** をクリックします。

2. コネクタータイプで **MySQL** を選択し、**Next** をクリックします。

3. 以下を設定します：
   - **Connector Name**：英数字で、例：`my_lindorm`。

   - **Server Host**：
     
     - EMQXがAlibaba Cloud VPCネットワーク（ECSインスタンスなど）内にデプロイされている場合は、Lindormインスタンスの内部SQLアドレスを入力します。形式は通常、Lindormが提供する内部ドメインで例：`ld-xxxx-proxy-sql-lindorm.lindorm.rds.aliyuncs.com:33060`。
     - EMQXがローカルデータセンターや非Alibaba Cloud環境にデプロイされている場合は、Lindormコンソールでパブリックアクセスを有効にし、割り当てられたパブリックSQLアドレスを入力します。形式は通常：`ld-xxxx-proxy-sql-public.lindorm.rds.aliyuncs.com:33060`。
     
     EMQXがデプロイされているホストのIPアドレスがLindormのアクセスホワイトリストに追加されていることを確認してください。
     
   - **Database Name**：`emqx_data`。

   - **Username**：`root`。

   - **Password**：`public`。

4. 詳細設定（任意）：[詳細設定](#advanced-configurations)を参照してください。

5. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがLindormに接続できるかテストします。

6. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSink付きのルール作成に進めます。

## Lindorm Sinkルールの作成

ここでは、トピック`#`のMQTTメッセージを処理し、Lindormの`demo_sensor`テーブルに書き込むルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** に移動します。

2. **Create**をクリックし、ルールIDに`my_rule`を入力します。

3. ルールID`my_rule`を入力し、SQLエディターにルールを記述します。本例ではトピック`#`のMQTTメッセージをLindormに保存します。**SELECT**句で選択するフィールドはSQLテンプレートで使用する変数をすべて含めてください。ルールSQLは以下の通りです：

   ```sql
   SELECT
     clientid AS device_id,
     timestamp AS time,
     payload.msg AS msg
   FROM
     "#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストが可能です。

   :::

4. + **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをLindormに送信します。

5. **Type of Action**ドロップダウンから`MySQL`を選択します。**Action**はデフォルトの`Create Action`のままにします。既にSinkを作成している場合は選択も可能です。本例では新規Sinkを作成します。

6. Sinkの名前を入力します。名前は英数字の組み合わせにしてください。

7. **Connector**ドロップダウンから先ほど作成した`my_lindorm`を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

8. 使用する機能に応じて**SQL Template**を設定します：

   注意：これは前処理済みのSQLなので、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   INSERT INTO demo_sensor(device_id, time, msg) VALUES (
     ${device_id},
     ${time},
     ${msg}
   )
   ```

   SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL template**上部の**Undefined Vars as Null**スイッチでルールエンジンの挙動を切り替えられます：

   - **Disabled**（デフォルト）：ルールエンジンは`undefined`という文字列をデータベースに挿入します。

   - **Enabled**：変数が未定義の場合、`NULL`を挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. 詳細設定（任意）：[詳細設定](#advanced-configurations)を参照してください。

11. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

12. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでルールの作成が完了しました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると新しいMySQL Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`#`のメッセージがMySQLに送信・保存されていることを確認できます。

## ルールのテスト

MQTTXを使って`sensor/1`トピックにメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_test -t sensor/1 -m '{ "msg": "hello lindorm" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージ1件と送信メッセージ1件があるはずです。

APIを使ってLindormにデータが正常に書き込まれているか確認します：

```bash
curl -X POST http://${LINDORM_SERVER}:8242/api/v2/sql?database=emqx_data \
  -H "Content-Type: text/plain" \
  -d 'SELECT * FROM demo_sensor'
```

## 詳細設定

MySQLコネクターおよびSink（Lindorm）の詳細設定オプションの説明：

| 項目                      | 説明                                                         | デフォルト |
| ------------------------- | ------------------------------------------------------------ | ---------- |
| **Connection Pool Size**  | MySQLサービス通信で維持する同時接続数。システムリソースや負荷に応じて調整してください。 | `8`        |
| **Start Timeout**         | 作成後にリソース準備が整うまでの最大待機時間（秒）。Lindorm接続の健全性を確保します。 | `5s`       |
| **Buffer Pool Size**      | Lindorm送信前にデータフローを管理するワーカープロセス数。ingressのみのシナリオは`0`に設定。 | `16`       |
| **Request TTL**           | バッファリングされたリクエストのTTL（秒）。この時間を超えたリクエストは期限切れとみなされます。 | `45s`      |
| **Health Check Interval** | Lindorm接続の自動健全性チェックの頻度（秒）。                          | `15s`      |
| **Max Buffer Queue Size** | バッファワーカーがLindormにデータをフラッシュする前に保持できる最大バイト数。 | `256MB`    |
| **Max Batch Size**        | Lindormに送信するバッチあたりの最大レコード数。単一レコード転送は`1`に設定。 | `1`        |
| **Query Mode**            | `sync`または`async`モードを選択。非同期モードはMQTTメッセージパブリッシュのブロックを回避しますが、厳密な順序性に影響する場合があります。 | `async`    |
| **In-flight Window**      | 応答待ちのリクエスト最大数。同一クライアントからの厳密なメッセージ順序が必要な場合は`1`に設定。 | `100`      |
