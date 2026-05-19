# GreptimeDB に MQTT データを取り込む

[GreptimeDB](https://github.com/GreptimeTeam/greptimedb) は、スケーラビリティ、分析機能、効率性に特化したオープンソースの時系列データベースです。クラウド時代のインフラ上で動作するよう設計されており、ユーザーはその弾力性と汎用ストレージの恩恵を受けられます。EMQX は現在、GreptimeDB、GreptimeCloud、GreptimeDB Enterprise の主流バージョンへの接続をサポートしています。

このページでは、EMQX と GreptimeDB 間のデータ統合について、作成方法と検証手順を含む実践的な解説を提供します。

## 動作の仕組み

GreptimeDB データ統合は、EMQX に組み込まれた機能であり、EMQX のリアルタイムデータキャプチャおよび送信機能と、GreptimeDB のデータ保存・分析機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQX から GreptimeDB へのデータ取り込みが簡素化され、複雑なコーディングを不要にします。ワークフローは以下の通りです。

以下の図は、EMQX と GreptimeDB 間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration GreptimeDB](./assets/emqx-integration-greptimedb.png)

1. **メッセージのパブリッシュと受信**：産業用デバイスは MQTT プロトコルを介して EMQX に正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ライン識別子やエネルギー消費値が含まれます。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、トピックマッチングに基づいて特定のソースからのメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報の付加などが含まれます。
3. **GreptimeDB へのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、メッセージを GreptimeDB に書き込む操作を実行します。GreptimeDB Sink は Line Protocol のテンプレートを提供し、メッセージの特定フィールドを GreptimeDB の対応するテーブル・カラムに柔軟に書き込むデータ形式を定義可能です。

エネルギー消費データが GreptimeDB に書き込まれた後は、SQL 文や Prometheus クエリ言語を用いて柔軟に分析できます。例えば：

- Grafana などの可視化ツールに接続し、エネルギー消費データのグラフを生成・表示する。
- ERP などのアプリケーションシステムに接続し、生産分析や生産計画の調整を行う。
- 業務システムに接続し、リアルタイムのエネルギー使用分析を実施してデータ駆動型のエネルギー管理を支援する。

## 特長とメリット

GreptimeDB とのデータ統合は、以下の特長と利点をビジネスにもたらします。

- **使いやすさ**：EMQX と GreptimeDB はともに開発者に優しい設計です。EMQX は標準の MQTT プロトコルと各種認証・認可・クラスタリング機能を提供し、GreptimeDB は時系列テーブルやスキーマレスアーキテクチャなどユーザーフレンドリーな設計を備えています。両者の統合により、ビジネス統合と開発のスピードが加速します。
- **効率的なデータ処理**：EMQX は多数の IoT デバイス接続とメッセージスループットを効率的に処理可能です。GreptimeDB はデータの書き込み、保存、クエリに優れており、IoT シナリオのデータ処理要件をシステムに負荷をかけずに満たします。
- **メッセージ変換**：メッセージは EMQX のルール内で多彩な処理・変換を経てから GreptimeDB に書き込まれます。
- **効率的なストレージとスケーラビリティ**：EMQX と GreptimeDB はともにクラスターのスケールアウト機能を持ち、ビジネスの成長に応じて柔軟に水平スケーリングが可能です。
- **高度なクエリ機能**：GreptimeDB はタイムスタンプデータの効率的なクエリと分析のために最適化された関数、演算子、インデックス技術を提供し、IoT 時系列データから正確な洞察を抽出できます。

## はじめる前に

このセクションでは、GreptimeDB データ統合の作成を始める前に必要な準備、特に GreptimeDB サーバーのインストール方法について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GreptimeDB サーバーのインストール

1. Docker を使って [GreptimeDB](https://greptime.com/download) をインストールし、Docker イメージを起動します。

   ```bash
   # GreptimeDB Docker イメージの起動
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

2. `user-provider` パラメータは GreptimeDB の認証を設定します。ファイルによる設定も可能です。詳細は[ドキュメント](https://docs.greptime.com/user-guide/deployments-administration/authentication/static/)を参照してください。
3. GreptimeDB が起動したら、[http://localhost:4000/dashboard](http://localhost:4000/dashboard) にアクセスしてダッシュボードを利用できます。ユーザー名とパスワードはそれぞれ `greptime_user` と `greptime_pwd` です。

## コネクターの作成

このセクションでは、Sink を GreptimeDB サーバーに接続するためのコネクター作成手順を説明します。

以下の手順は、EMQX と GreptimeDB をローカルマシンで実行していることを前提としています。リモート環境の場合は設定を適宜調整してください。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **GreptimeDB** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します。
   - コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_greptimedb`
   - **Server Host**：`127.0.0.1:4001` を入力します。GreptimeCloud に接続する場合はポートを 443 にして `{url}:443` と入力してください。
   - **Database**：`public` を入力します。GreptimeCloud に接続する場合はサービス名を入力してください。
   - **Username** と **Password**：`greptime_user` と `greptime_pwd` を入力します（[GreptimeDB サーバーのインストール](#greptimedb-サーバーのインストール)で設定したもの）。GreptimeCloud の場合はサービスのユーザー名とパスワードを入力してください。
5. **Advanced Settings** を展開し、必要に応じて詳細設定を行います（任意）。詳細は[高度な設定](#advanced-configuration)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GreptimeDB サーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** を選択できます。後者を選ぶと GreptimeDB Sink を使ったルール作成に進めます。詳細は[GreptimeDB Sink を使ったルール作成](#create-a-rule-with-greptimedb-sink)を参照してください。

## GreptimeDB Sink を使ったルール作成

このセクションでは、EMQX で MQTT トピック `t/#` からのメッセージを処理し、処理結果を設定済み Sink 経由で GreptimeDB に送信するルール作成手順を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力し、**SQL Editor** にルールを設定します。ここではトピック `t/#` の MQTT メッセージを GreptimeDB に保存するため、以下の SQL 文を使用します。

   注意：独自の SQL 文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールの学習とテストが可能です。

   :::

4. + **Add Action** ボタンをクリックし、ルールによりトリガーされるアクションを定義します。このアクションで EMQX はルールで処理したデータを GreptimeDB に送信します。

5. **Type of Action** ドロップダウンリストから `GreptimeDB` を選択します。**Action** はデフォルトの `Create Action` のままにします。既に Sink を作成済みの場合はそれを選択可能です。この例では新規 Sink を作成します。

6. Sink の名前を入力します。名前は大文字・小文字の英数字の組み合わせにしてください。

7. **Connector** ドロップダウンから先ほど作成した `my_greptimedb` を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. **Write Syntax** を設定します。これはデータポイントのメジャメント、タグ、フィールド、タイムスタンプをテキスト形式で指定するもので、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) の構文に準拠したプレースホルダーをサポートします。GreptimeDB は InfluxDB 互換のデータ形式をサポートしています。

   ::: tip

   - GreptimeDB に符号付き整数型の値を書き込む場合は、プレースホルダーの後に `i` を付けます。例：`${payload.int}i`
   - 符号なし整数型の場合は `u` を付けます。例：`${payload.int}u`

   :::

9. **Time Precision** を指定します。デフォルトは `millisecond` です。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **高度な設定（任意）**：同期・非同期クエリモードの選択やキュー・バッチの有効化を設定できます。詳細は[Sink の機能](./data-bridges.md#features-of-sink)を参照してください。

12. **Create** をクリックする前に、**Test Connectivity** をクリックして Sink が GreptimeDB サーバーに接続できるかテスト可能です。

13. **Create** ボタンをクリックして Sink 設定を完了します。新しい Sink が **Action Outputs** に追加されます。

14. **Create Rule** ページに戻り、設定内容を確認して **Create** をクリックしルールを生成します。

これで GreptimeDB Sink 経由でデータを転送するルールが正常に作成されました。**Integration** -> **Rules** ページで新規ルールを確認できます。**Actions(Sink)** タブをクリックすると新しい GreptimeDB Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され GreptimeDB に送信・保存されている様子を確認できます。

## ルールのテスト

MQTTX を使ってトピック `t/1` にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello GreptimeDB" }'
```

Sink の稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

GreptimeDB ダッシュボードで `SQL` を使い、メッセージが GreptimeDB に書き込まれていることを確認できます。

## 高度な設定

このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じたカスタマイズのための高度な設定オプションを説明します。コネクター作成時に **Advanced Settings** を展開し、ビジネス要件に応じて以下の設定を行えます。

| 項目名                       | 説明                                                         | デフォルト値     |
| ---------------------------- | ------------------------------------------------------------ | --------------- |
| Time-To-Live (TTL)           | GreptimeDB で自動作成されるテーブルの有効期限設定。          | -               |
| Custom Timestamp Column Name | 定義すると、クエリ時に表示されるカスタムタイムスタンプ列名。 | -               |
| Start Timeout                | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。リソース作成要求に応答する前に、接続先リソースが完全に稼働しデータ処理可能であることを確認するための設定。 | `5` 秒          |
| Health Check Interval        | コネクターの稼働状態をチェックする間隔。                     | `15` 秒         |
| Health Check Timeout         | GreptimeDB サーバーとの接続に対する自動ヘルスチェックのタイムアウト時間。 | `60` 秒         |
