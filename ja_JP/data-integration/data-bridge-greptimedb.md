# GreptimeDBへのMQTTデータ取り込み

[GreptimeDB](https://github.com/GreptimeTeam/greptimedb)は、スケーラビリティ、分析機能、効率性に特化したオープンソースの時系列データベースです。クラウド時代のインフラ上で動作するよう設計されており、ユーザーはその弾力性と汎用ストレージの利点を享受できます。EMQXは現在、GreptimeDBの主流バージョンであるGreptimeCloudやGreptimeDB Enterpriseとの接続をサポートしています。

本ページでは、EMQXとGreptimeDB間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作の仕組み

GreptimeDBデータ統合は、EMQXに組み込まれた機能であり、EMQXのリアルタイムデータキャプチャおよび送信機能とGreptimeDBのデータ保存・分析機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからGreptimeDBへのデータ取り込みが簡素化され、複雑なコーディングを不要にします。ワークフローは以下の通りです。

以下の図は、EMQXとGreptimeDB間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration GreptimeDB](./assets/emqx-integration-greptimedb.png)

1. **メッセージのパブリッシュと受信**：産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ラインの識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、トピックマッチングに基づいて特定のソースからのメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などが含まれます。
3. **GreptimeDBへのデータ取り込み**：ルールエンジンで定義されたルールは、メッセージをGreptimeDBに書き込む操作をトリガーします。GreptimeDB SinkはLine Protocolテンプレートを提供し、特定のメッセージフィールドをGreptimeDBの対応するテーブルやカラムに柔軟に書き込むデータフォーマットの定義を可能にします。

エネルギー消費データがGreptimeDBに書き込まれた後は、SQL文やPrometheusクエリ言語を用いて柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー消費データのグラフを生成・表示する。
- ERPなどのアプリケーションシステムに接続し、生産分析や生産計画の調整を行う。
- ビジネスシステムに接続し、リアルタイムのエネルギー使用分析を実施してデータ駆動型のエネルギー管理を支援する。

## 特長と利点

GreptimeDBとのデータ統合は、以下の特長と利点をビジネスにもたらします。

- **使いやすさ**：EMQXとGreptimeDBは共に開発者に優しい設計を提供します。EMQXは標準的なMQTTプロトコルに加え、多様な認証・認可・クラスタリング機能を備えています。GreptimeDBは時系列テーブルやスキーマレスアーキテクチャなどユーザーフレンドリーな設計を持ちます。両者の統合により、ビジネス統合と開発のスピードアップが期待できます。
- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを効率的に処理可能です。GreptimeDBはデータの書き込み、保存、クエリに優れており、IoTシナリオのデータ処理要件をシステムに過負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXのルール内で豊富な処理・変換を経てからGreptimeDBに書き込まれます。
- **効率的なストレージとスケーラビリティ**：EMQXとGreptimeDBは共にクラスター拡張機能を持ち、ビジネスの成長に応じて柔軟な水平スケーリングが可能です。
- **高度なクエリ機能**：GreptimeDBはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから精緻な洞察を引き出せます。

## はじめる前に

このセクションでは、GreptimeDBデータ統合の作成を開始する前に完了すべき準備事項を説明します。GreptimeDBサーバーのインストール方法も含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GreptimeDBサーバーのインストール

1. Docker経由で[GreptimeDB](https://greptime.com/download)をインストールし、Dockerイメージを起動します。

   ```bash
   # GreptimeDBのDockerイメージを起動する
   docker run -p 127.0.0.1:4000-4003:4000-4003 \
     -v "$(pwd)/greptimedb_data:/greptimedb_data" \
     --name greptime --rm \
     greptime/greptimedb:latest standalone start \
     --http-addr 0.0.0.0:4000 \
     --rpc-bind-addr 0.0.0.0:4001 \
     --mysql-addr 0.0.0.0:4002 \
     --postgres-addr 0.0.0.0:4003 \
     --user-provider=static_user_provider:cmd:greptime_user=greptime_pwd
   ```

2. `user-provider`パラメータはGreptimeDBの認証を設定します。ファイルによる設定も可能です。詳細は[ドキュメント](https://docs.greptime.com/user-guide/deployments-administration/authentication/static/)を参照してください。
3. GreptimeDBが起動したら、[http://localhost:4000/dashboard](http://localhost:4000/dashboard)にアクセスしてダッシュボードを利用できます。ユーザー名とパスワードはそれぞれ`greptime_user`と`greptime_pwd`です。

## コネクターの作成

このセクションでは、SinkをGreptimeDBサーバーに接続するためのコネクター作成方法を示します。

以下の手順は、EMQXとGreptimeDBをローカルマシンで実行していることを前提としています。リモート環境で実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**GreptimeDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します：
   - コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_greptimedb`
   - **Server Host**：`127.0.0.1:4001`を入力します。GreptimeCloudに接続する場合はポートを443にして`{url}:443`と入力してください。
   - **Database**：`public`を入力します。GreptimeCloudに接続する場合はサービス名を入力します。
   - **Username**と**Password**：`greptime_user`と`greptime_pwd`を入力します（[GreptimeDBサーバーのインストール](#greptimedbサーバーのインストール)で設定したもの）。GreptimeCloudの場合はサービスのユーザー名とパスワードを入力してください。
5. **Advanced Settings**を展開し、必要に応じて詳細設定を行います（任意）。詳細は[高度な設定](#高度な設定)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがGreptimeDBサーバーに接続できるかテストできます。
7. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてGreptimeDB Sinkを用いたルール作成に進むことができます。詳細は[GreptimeDB Sinkを用いたルール作成](#greptimedb-sinkを用いたルール作成)を参照してください。

## GreptimeDB Sinkを用いたルール作成

このセクションでは、EMQXでソースMQTTトピック`t/#`からのメッセージを処理し、設定済みのSinkを通じてGreptimeDBに送信するルールの作成方法を示します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**でルールを設定します。ここではトピック`t/#`のMQTTメッセージをGreptimeDBに保存するため、以下のSQL文を使用します。

   注意：独自のSQL文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストできます。

   :::

4. + **Add Action**ボタンをクリックし、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをGreptimeDBに送信します。

5. **Type of Action**ドロップダウンリストから`GreptimeDB`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既にSinkを作成していれば選択可能です。本例では新規Sinkを作成します。

6. Sinkの名前を入力します。名前は大文字・小文字の英数字の組み合わせにしてください。

7. **Connector**ドロップダウンから先に作成した`my_greptimedb`を選択します。新規コネクターはドロップダウン横のボタンから作成可能です。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. **Write Syntax**を設定します。データポイントのmeasurement、タグ、フィールド、タイムスタンプを指定するテキストベースのフォーマットをInfluxDBのLine Protocol仕様に準拠して指定します。GreptimeDBはInfluxDB互換のデータフォーマットをサポートしています。

   ::: tip

   - GreptimeDBに符号付き整数型の値を書き込む場合は、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`
   - 符号なし整数型の場合は`u`を付けます。例：`${payload.int}u`

   :::

9. **Time Precision**を指定します。デフォルトは`millisecond`です。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **高度な設定（任意）**：同期（sync）または非同期（async）クエリモードの選択、キューやバッチの有効化設定が可能です。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

12. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがGreptimeDBサーバーに接続できるかテストできます。

13. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

14. **Create Rule**ページに戻り、設定内容を確認します。**Create**ボタンをクリックしてルールを生成します。

これで、GreptimeDB Sinkを通じてデータを転送するルールの作成が完了しました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると、新しいGreptimeDB Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`によって解析され、GreptimeDBに送信・保存されていることが確認できます。

## ルールのテスト

MQTTXを使用してトピック`t/1`にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello GreptimeDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

GreptimeDBダッシュボードで`SQL`を用いて、メッセージがGreptimeDBに書き込まれていることを確認できます。

## 高度な設定

このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じたカスタマイズに役立つ高度な設定オプションを説明します。コネクター作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

| フィールド名                     | 説明                                                        | デフォルト値    |
| ------------------------------ | ----------------------------------------------------------- | -------------- |
| Time-To-Live (TTL)             | GreptimeDBで自動作成されるテーブルの有効期限設定。            | -              |
| Custom Timestamp Column Name   | 定義すると、クエリ時に表示されるカスタムタイムスタンプカラム名を指定。 | -              |
| Start Timeout                  | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。リソース作成要求に応答する前に、接続先リソースが完全に稼働しデータ処理準備が整っていることを保証するための設定。 | `5`秒          |
| Health Check Interval          | コネクターの稼働状態をチェックする間隔。                      | `15`秒         |
| Health Check Timeout           | GreptimeDBサーバーとの接続に対する自動ヘルスチェックのタイムアウト時間。 | `60`秒         |
