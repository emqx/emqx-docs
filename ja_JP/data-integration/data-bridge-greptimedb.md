# GreptimeDBへのMQTTデータ取り込み

[GreptimeDB](https://github.com/GreptimeTeam/greptimedb)は、スケーラビリティ、分析機能、効率性に特化したオープンソースの時系列データベースです。クラウド時代のインフラ上での動作を想定して設計されており、ユーザーはその弾力性と汎用ストレージの恩恵を受けられます。EMQXは現在、GreptimeDBの主流バージョンであるGreptimeCloudやGreptimeDB Enterpriseとの接続をサポートしています。

本ページでは、EMQXとGreptimeDB間のデータ連携について包括的に紹介し、データ連携の作成および検証方法を実践的に解説します。

## 動作概要

GreptimeDBデータ連携はEMQXに組み込まれた機能で、EMQXのリアルタイムデータキャプチャと転送能力と、GreptimeDBのデータ保存・分析能力を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからGreptimeDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。ワークフローは以下の通りです。

以下の図は、EMQXとGreptimeDB間の典型的なデータ連携アーキテクチャを示しています。

![EMQX Integration GreptimeDB](./assets/emqx-integration-greptimedb.png)

1. **メッセージのパブリッシュと受信**：産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ラインの識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、トピックマッチングに基づき特定のソースからのメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報付加などが含まれます。
3. **GreptimeDBへのデータ取り込み**：ルールエンジンで定義されたルールにより、メッセージをGreptimeDBに書き込む操作がトリガーされます。GreptimeDB SinkはLine Protocolテンプレートを提供し、特定のメッセージフィールドをGreptimeDBの対応するテーブルやカラムに柔軟に書き込むデータ形式を定義できます。

エネルギー消費データがGreptimeDBに書き込まれた後は、SQL文やPrometheusクエリ言語を用いて柔軟に分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続してチャートを生成し、エネルギー消費データを表示する。
- ERPなどの業務システムに接続し、生産分析や生産計画の調整に活用する。
- ビジネスシステムに接続し、リアルタイムのエネルギー使用分析を行い、データ駆動型のエネルギー管理を実現する。

## 特長とメリット

GreptimeDBとのデータ連携は、以下の特長と利点をビジネスにもたらします。

- **使いやすさ**：EMQXとGreptimeDBはともに開発者に優しい設計です。EMQXは標準のMQTTプロトコルに加え、多様な認証・認可・クラスタリング機能を提供します。GreptimeDBは時系列テーブルやスキーマレス構造などユーザーフレンドリーな設計を持ちます。両者の連携により、ビジネス統合と開発を加速できます。
- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを効率的に処理可能です。GreptimeDBはデータ書き込み、保存、クエリに優れており、IoTシナリオのデータ処理要件をシステムに負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXルール内で豊富な処理・変換を経てからGreptimeDBに書き込まれます。
- **効率的な保存とスケーラビリティ**：EMQXとGreptimeDBはともにクラスタースケール機能を持ち、ビジネスの成長に応じて柔軟に水平スケールが可能です。
- **高度なクエリ機能**：GreptimeDBはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから精緻な洞察を引き出せます。

## はじめる前に

このセクションでは、GreptimeDBデータ連携の作成を始める前に必要な準備、特にGreptimeDBサーバーのインストール方法について説明します。

### 前提条件

- EMQXデータ連携の[ルール](./rules.md)に関する知識
- [データ連携](./data-bridges.md)に関する知識

### GreptimeDBサーバーのインストール

1. Dockerを使って[GreptimeDB](https://greptime.com/download)をインストールし、Dockerイメージを起動します。

   ```bash
   # GreptimeDBのDockerイメージを起動する
   docker run -p 4000-4004:4000-4004 \
   -p 4242:4242 -v "$(pwd)/greptimedb:/tmp/greptimedb" \
   --name greptime --rm \
   greptime/greptimedb standalone start \
   --http-addr 0.0.0.0:4000 \
   --rpc-addr 0.0.0.0:4001 \
   --mysql-addr 0.0.0.0:4002 \
   --user-provider=static_user_provider:cmd:greptime_user=greptime_pwd
   ```

2. `user-provider`パラメータはGreptimeDBの認証を設定します。ファイルによる設定も可能です。詳細は[ドキュメント](https://docs.greptime.com/user-guide/deployments/authentication/static)を参照してください。
3. GreptimeDBが起動したら、[http://localhost:4000/dashboard](http://localhost:4000/dashboard)にアクセスしてGreptimeDBダッシュボードを利用できます。ユーザー名とパスワードはそれぞれ`greptime_user`と`greptime_pwd`です。

## コネクターの作成

このセクションでは、SinkをGreptimeDBサーバーに接続するためのコネクター作成方法を説明します。

以下の手順はEMQXとGreptimeDBをローカルマシンで動作させていることを前提としています。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **GreptimeDB** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下を設定します：
   - コネクター名を入力します。英数字の組み合わせで、例：`my_greptimedb`。
   - **Server Host**：`127.0.0.1:4001` と入力します。GreptimeCloudに接続する場合はポートを443にして `{url}:443` と入力してください。
   - **Database**：`public` と入力します。GreptimeCloudの場合はサービス名を入力します。
   - **Username** と **Password**：`greptime_user` と `greptime_pwd` を入力します（[GreptimeDBサーバーのインストール](#greptimedbサーバーのインストール)で設定したもの）。GreptimeCloudの場合はサービスのユーザー名とパスワードを入力してください。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがGreptimeDBサーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックしてGreptimeDB Sinkを使ったルール作成に進めます。詳細は[GreptimeDB Sinkを使ったルール作成](#create-a-rule-with-greptimedb-sink)を参照してください。

## GreptimeDB Sinkを使ったルール作成

このセクションでは、EMQXでルールを作成し、ソースMQTTトピック `t/#` からのメッセージを処理して、設定済みのSinkを通じてGreptimeDBに送信する方法を説明します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` を入力し、**SQL Editor** にルールを設定します。ここではトピック `t/#` のMQTTメッセージをGreptimeDBに保存するため、以下のSQL文を使用します。

   注意：独自のSQL文を指定する場合は、Sinkが必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストができます。

   :::

4. + **Add Action** ボタンをクリックして、ルールに紐づくアクションを定義します。このアクションにより、EMQXはルールで処理したデータをGreptimeDBに送信します。

5. **Type of Action** ドロップダウンから `GreptimeDB` を選択します。**Action** はデフォルトの `Create Action` のままにします。既にSinkを作成している場合はそれを選択可能ですが、ここでは新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせで指定してください。

7. **Connector** ドロップダウンから先ほど作成した `my_greptimedb` を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックします。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. **Write Syntax** を設定します。これはテキストベースのフォーマットで、データポイントのmeasurement、tags、fields、timestampを指定し、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に準拠したプレースホルダーをサポートします。GreptimeDBはInfluxDB互換のデータフォーマットをサポートしています。

   ::: tip

   - GreptimeDBに符号付き整数型の値を書き込む場合は、プレースホルダーの後に `i` を付けます。例：`${payload.int}i`
   - 符号なし整数型の値を書く場合は、プレースホルダーの後に `u` を付けます。例：`${payload.int}u`

   :::

9. **Time Precision** を指定します。デフォルトは `millisecond` です。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のために、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **詳細設定（任意）**：**sync** または **async** クエリモードの選択、キューやバッチの有効化を設定します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

12. **Create** をクリックする前に、**Test Connectivity** をクリックしてSinkがGreptimeDBサーバーに接続できるかテストできます。

13. **Create** ボタンをクリックしてSinkの設定を完了します。新しいSinkが **Action Outputs** に追加されます。

14. **Create Rule** ページに戻り、設定内容を確認して **Create** をクリックしルールを生成します。

これでGreptimeDB Sinkを通じたデータ転送ルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると新しいGreptimeDB Sinkが表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` により解析されGreptimeDBに送信・保存されている様子を確認できます。

## ルールのテスト

MQTTXを使ってトピック `t/1` にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello GreptimeDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

GreptimeDBダッシュボードでSQLを使い、メッセージがGreptimeDBに書き込まれていることを確認できます。
