# GreptimeDBへのMQTTデータ取り込み

<<<<<<< HEAD
[GreptimeDB](https://github.com/GreptimeTeam/greptimedb)は、スケーラビリティ、分析機能、効率性に特化したオープンソースの時系列データベースです。クラウド時代のインフラ上での動作を想定して設計されており、ユーザーはその弾力性と汎用ストレージの恩恵を受けられます。EMQXは現在、GreptimeDBの主流バージョン、GreptimeCloud、またはGreptimeDB Enterpriseへの接続をサポートしています。

本ページでは、EMQXとGreptimeDB間のデータ統合について、実践的な作成および検証手順を含めて包括的に紹介します。

## 動作概要

GreptimeDBデータ統合はEMQXに組み込まれた機能であり、EMQXのリアルタイムデータキャプチャと送信能力と、GreptimeDBのデータ保存および分析能力を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからGreptimeDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。ワークフローは以下の通りです。

下図はEMQXとGreptimeDB間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration GreptimeDB](./assets/emqx-integration-greptimedb.png)

1. **メッセージのパブリッシュおよび受信**：産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ライン識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などが含まれます。
3. **GreptimeDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをGreptimeDBに書き込む操作が実行されます。GreptimeDB SinkはLine Protocolテンプレートを提供し、特定のメッセージフィールドをGreptimeDBの対応テーブルおよびカラムに柔軟に書き込むデータ形式を定義できます。

エネルギー消費データがGreptimeDBに書き込まれた後は、SQL文やPrometheusクエリ言語を用いて柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー消費データのグラフを生成・表示する。
- ERPなどのアプリケーションシステムに接続し、生産分析や生産計画の調整を行う。
- 業務システムに接続し、リアルタイムのエネルギー使用分析を行い、データ駆動型のエネルギー管理を支援する。

## 特長と利点

GreptimeDBとのデータ統合は、以下の特長とメリットをビジネスにもたらします。

- **使いやすさ**：EMQXとGreptimeDBは共に開発者に優しい設計です。EMQXは標準のMQTTプロトコルに加え、多様な認証、認可、クラスタリング機能を標準で提供します。GreptimeDBは時系列テーブルやスキーマレスアーキテクチャなどユーザーフレンドリーな設計を持ちます。両者の統合により、ビジネス統合と開発のスピードアップが可能です。
- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを効率的に処理できます。GreptimeDBはデータ書き込み、保存、クエリに優れており、IoTシナリオのデータ処理ニーズをシステム負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXのルール内で豊富な処理・変換を経てからGreptimeDBに書き込まれます。
- **効率的なストレージとスケーラビリティ**：EMQXとGreptimeDBは共にクラスターのスケールアウト機能を持ち、ビジネス成長に応じて柔軟に水平スケーリングが可能です。
- **高度なクエリ機能**：GreptimeDBはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから精緻な洞察を引き出せます。

## はじめる前に

このセクションでは、GreptimeDBデータ統合の作成に先立って準備すべき事項を説明します。GreptimeDBサーバーのインストール方法も含みます。
=======
[GreptimeDB](https://github.com/GreptimeTeam/greptimedb) は、スケーラビリティ、分析機能、効率性に特化したオープンソースの時系列データベースです。クラウド時代のインフラ上で動作するよう設計されており、その弾力性と汎用ストレージの利点をユーザーに提供します。EMQXは現在、GreptimeDBの主流バージョンであるGreptimeCloudやGreptimeDB Enterpriseとの接続をサポートしています。

本ページでは、EMQXとGreptimeDB間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作概要

GreptimeDBデータ統合は、EMQXに組み込まれた機能であり、EMQXのリアルタイムデータキャプチャと送信能力と、GreptimeDBのデータ保存および分析能力を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからGreptimeDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。ワークフローは以下の通りです。

以下の図は、EMQXとGreptimeDB間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration GreptimeDB](./assets/emqx-integration-greptimedb.png)

1. **メッセージのパブリッシュと受信**: 産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ライン識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**: 組み込みのルールエンジンは、トピックマッチングに基づいて特定のソースからのメッセージを処理します。メッセージ到着時にルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などが含まれます。
3. **GreptimeDBへのデータ取り込み**: ルールエンジンで定義されたルールは、メッセージをGreptimeDBに書き込む操作をトリガーします。GreptimeDB SinkはLine Protocolテンプレートを提供し、特定のメッセージフィールドをGreptimeDBの対応するテーブルやカラムに柔軟に書き込むデータフォーマットを定義できます。

エネルギー消費データがGreptimeDBに書き込まれた後は、SQL文やPrometheusクエリ言語を用いて柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、チャートを生成してエネルギー消費データを表示。
- ERPなどのアプリケーションシステムに接続し、生産分析や生産計画の調整に活用。
- 業務システムに接続し、リアルタイムのエネルギー使用分析を行い、データ駆動型のエネルギー管理を支援。

## 特長と利点

GreptimeDBとのデータ統合は、以下の特長と利点をビジネスにもたらします。

- **使いやすさ**: EMQXとGreptimeDBは共に開発者に優しい設計です。EMQXは標準のMQTTプロトコルに加え、多様な認証、認可、クラスタリング機能を提供します。GreptimeDBは時系列テーブルやスキーマレスアーキテクチャなどのユーザーフレンドリーな設計を特徴とします。両者の統合により、ビジネス統合と開発のスピードアップが期待できます。
- **効率的なデータ処理**: EMQXは大量のIoTデバイス接続とメッセージスループットを効率的に処理可能です。GreptimeDBはデータの書き込み、保存、クエリに優れており、IoTシナリオのデータ処理ニーズをシステムに過度な負荷をかけずに満たします。
- **メッセージ変換**: メッセージはEMQXのルール内で豊富に処理・変換されてからGreptimeDBに書き込まれます。
- **効率的なストレージとスケーラビリティ**: EMQXとGreptimeDBは共にクラスターのスケールアウト機能を備えており、ビジネスの成長に応じて柔軟に水平スケーリングが可能です。
- **高度なクエリ機能**: GreptimeDBはタイムスタンプデータの効率的なクエリと分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから精緻な洞察を引き出せます。

## はじめる前に

このセクションでは、GreptimeDBデータ統合を作成する前に必要な準備、特にGreptimeDBサーバーのインストール方法について説明します。
>>>>>>> origin/release-5.9

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GreptimeDBサーバーのインストール

1. Docker経由で[GreptimeDB](https://greptime.com/download)をインストールし、Dockerイメージを起動します。

   ```bash
   # GreptimeDBのDockerイメージを起動するコマンド
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

2. `user-provider`パラメータはGreptimeDBの認証を設定します。ファイルによる設定も可能です。詳細は[ドキュメント](https://docs.greptime.com/user-guide/deployments/authentication/static)を参照してください。
3. GreptimeDBが起動したら、[http://localhost:4000/dashboard](http://localhost:4000/dashboard) にアクセスしてGreptimeDBダッシュボードを利用できます。ユーザー名とパスワードはそれぞれ `greptime_user` と `greptime_pwd` です。

## コネクターの作成

このセクションでは、SinkをGreptimeDBサーバーに接続するためのコネクターの作成方法を示します。

<<<<<<< HEAD
以下の手順は、EMQXとGreptimeDBの両方をローカルマシンで実行していることを前提としています。リモート環境で実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**GreptimeDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します：
   - コネクター名を入力します。英数字の大文字・小文字の組み合わせで、例：`my_greptimedb`。
   - **Server Host**：`127.0.0.1:4001`を入力します。GreptimeCloudに接続する場合はポートを443にして`{url}:443`と入力します。
   - **Database**：`public`を入力します。GreptimeCloudの場合はサービス名を入力してください。
   - **Username**と**Password**：`greptime_user`と`greptime_pwd`を入力します（[GreptimeDBサーバーのインストール](#greptimedbサーバーのインストール)で設定したもの）。GreptimeCloudの場合はサービスのユーザー名とパスワードを入力します。
5. **Advanced Settings**を展開し、必要に応じて詳細設定を行います（任意）。詳細は[高度な設定](#高度な設定)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがGreptimeDBサーバーに接続できるかテストできます。
7. ページ下部の**Create**ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログでは**Back to Connector List**をクリックしてコネクター一覧に戻るか、**Create Rule**をクリックしてGreptimeDB Sinkを使ったルール作成に進めます。詳細は[GreptimeDB Sinkを使ったルール作成](#greptimedb-sinkを使ったルール作成)を参照してください。
=======
以下の手順は、EMQXとGreptimeDBをローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **GreptimeDB** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下を設定します：
   - コネクター名を入力します。英数字の組み合わせで、例：`my_greptimedb`。
   - **Server Host**: `127.0.0.1:4001` を入力します。GreptimeCloudに接続する場合はポートを443にして `{url}:443` と入力してください。
   - **Database**: `public` を入力します。GreptimeCloudの場合はサービス名を入力してください。
   - **Username** と **Password**: [GreptimeDBサーバーのインストール](#greptimedbサーバーのインストール)で設定した `greptime_user` と `greptime_pwd` を入力します。GreptimeCloudの場合はサービスのユーザー名とパスワードを入力してください。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがGreptimeDBサーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** をクリックして、GreptimeDB Sinkを使ったルール作成に進めます。詳細は[GreptimeDB Sinkでルールを作成](#create-a-rule-with-greptimedb-sink)を参照してください。
>>>>>>> origin/release-5.9

## GreptimeDB Sinkでルールを作成

<<<<<<< HEAD
このセクションでは、EMQXでソースMQTTトピック`t/#`からのメッセージを処理し、処理結果を設定済みのSink経由でGreptimeDBに送信するルールの作成方法を示します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。
=======
このセクションでは、EMQXでMQTTトピック `t/#` からのメッセージを処理し、設定済みのSinkを通じてGreptimeDBに送信するルールの作成方法を説明します。

1. EMQXダッシュボードで **Integration** -> **Rules** をクリックします。
>>>>>>> origin/release-5.9

2. ページ右上の**Create**をクリックします。

<<<<<<< HEAD
3. ルールIDに`my_rule`を入力し、**SQL Editor**でルールを設定します。ここでは、トピック`t/#`のMQTTメッセージをGreptimeDBに保存したいので、以下のSQL構文を使用します。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドが`SELECT`部分に含まれていることを確認してください。
=======
3. ルールIDに `my_rule` を入力し、**SQL Editor** にルールを設定します。ここではトピック `t/#` のMQTTメッセージをGreptimeDBに保存するため、以下のSQL構文を使用します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを `SELECT` 部分に含めていることを確認してください。
>>>>>>> origin/release-5.9

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

<<<<<<< HEAD
   初心者の方は、**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールの学習とテストを行うことを推奨します。

   :::

4. + **Add Action**ボタンをクリックして、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをGreptimeDBに送信します。

5. **Type of Action**ドロップダウンリストから`GreptimeDB`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既に作成済みのSinkを選択することも可能ですが、ここでは新規Sinkを作成します。
=======
   初心者の方は **SQL Examples** をクリックし、**Enable Test** を有効にしてSQLルールの学習とテストが可能です。

   :::

4. + **Add Action** ボタンをクリックし、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをGreptimeDBに送信します。

5. **Type of Action** ドロップダウンリストから `GreptimeDB` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みのSinkがあれば選択可能です。本デモでは新しいSinkを作成します。
>>>>>>> origin/release-5.9

6. Sink名を入力します。英数字の大文字・小文字の組み合わせで指定してください。

<<<<<<< HEAD
7. **Connector**ドロップダウンから、先に作成した`my_greptimedb`を選択します。ドロップダウン横のボタンから新規コネクター作成も可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. **Write Syntax**を設定します。測定値、タグ、フィールド、タイムスタンプを指定するテキストベースのフォーマットを、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に準拠して指定します。GreptimeDBはInfluxDB互換のデータ形式をサポートしています。

   ::: tip

   - GreptimeDBに符号付き整数型の値を書き込む場合は、プレースホルダーの後に`i`を型識別子として付加します。例：`${payload.int}i`
   - 符号なし整数型の場合は`u`を付加します。例：`${payload.int}u`
=======
7. **Connector** ドロップダウンから先ほど作成した `my_greptimedb` を選択します。隣のボタンをクリックして新規コネクターを作成することも可能です。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. **Write Syntax** を設定します。測定値、タグ、フィールド、タイムスタンプをテキストベースで指定するフォーマットを設定します。プレースホルダーは[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に準拠してサポートされます。GreptimeDBはInfluxDB互換のデータフォーマットをサポートしています。

   ::: tip

   - GreptimeDBに符号付き整数型の値を書き込む場合は、プレースホルダーの後に `i` を付けます。例：`${payload.int}i`
   - 符号なし整数型の場合は `u` を付けます。例：`${payload.int}u`
>>>>>>> origin/release-5.9

   :::

9. **Time Precision**を指定します。デフォルトは`millisecond`です。

<<<<<<< HEAD
10. **Fallback Actions（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した際にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **高度な設定（任意）**：同期・非同期クエリモードの選択やキュー・バッチの有効化設定が可能です。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

12. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがGreptimeDBサーバーに接続できるかテスト可能です。
=======
10. **Fallback Actions（オプション）**: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した際にトリガーされます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

11. **詳細設定（オプション）**: **sync** または **async** クエリモードの選択、キューやバッチの有効化を設定できます。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

12. **Create** をクリックする前に、**Test Connectivity** をクリックしてSinkがGreptimeDBサーバーに接続できるかテストします。
>>>>>>> origin/release-5.9

13. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

<<<<<<< HEAD
14. **Create Rule**ページに戻り、設定内容を確認後、**Create**をクリックしてルールを生成します。

これでGreptimeDB Sink経由でデータを転送するルールの作成が完了しました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると新しいGreptimeDB Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認でき、トピック`t/#`のメッセージがルール`my_rule`で解析されGreptimeDBに送信・保存されている様子が見られます。
=======
14. **Create Rule** ページに戻り、設定内容を確認後、**Create** ボタンをクリックしてルールを生成します。

これで、GreptimeDB Sinkを通じてデータを転送するルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しいGreptimeDB Sinkが表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、GreptimeDBに送信・保存されていることが確認できます。
>>>>>>> origin/release-5.9

## ルールのテスト

MQTTクライアントのMQTTXを使い、トピック`t/1`にメッセージを送信してオンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello GreptimeDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

<<<<<<< HEAD
GreptimeDBダッシュボードで`SQL`を使い、メッセージがGreptimeDBに書き込まれていることを確認できます。

## 高度な設定

このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じたカスタマイズのための高度な設定オプションを説明します。コネクター作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

| フィールド名                   | 説明                                                         | デフォルト値     |
| ---------------------------- | ------------------------------------------------------------ | --------------- |
| Time-To-Live (TTL)           | GreptimeDBで自動作成されるテーブルの有効期限設定。           | -               |
| Custom Timestamp Column Name | 定義した場合、クエリ時に表示されるカスタムタイムスタンプカラム名を指定。 | -               |
| Start Timeout                | コネクターが自動起動したリソースの正常状態到達を待機する最大秒数。リソース作成要求に応答する前に、接続先リソースが完全に稼働しデータ処理可能であることを確認するための設定。 | `5`秒           |
| Health Check Interval        | コネクターの稼働状況をチェックする間隔時間。                 | `15`秒          |
| Health Check Timeout         | GreptimeDBサーバーとの接続に対する自動ヘルスチェックのタイムアウト時間。 | `60`秒          |
=======
GreptimeDBダッシュボードで `SQL` を使い、メッセージがGreptimeDBに書き込まれているかを確認できます。
>>>>>>> origin/release-5.9
