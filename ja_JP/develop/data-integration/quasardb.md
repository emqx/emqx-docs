# QuasarDBへのMQTTデータ取り込み

[QuasarDB](https://www.quasardb.net/)は、大量の時系列データの保存とクエリに特化した高性能なカラム指向時系列データベースです。EMQXはQuasarDBとの連携をサポートしており、MQTTメッセージやクライアントイベントをQuasarDBに保存できます。これにより、IoTテレメトリの管理および分析のためのデータパイプラインや解析プロセスの構築が容易になります。

本ページでは、EMQXとQuasarDB間のデータ統合の概要と、実際の作成および検証手順について詳しく説明します。

## 動作概要

QuasarDBデータ統合はEMQXに標準搭載された機能であり、EMQXのデバイス接続およびメッセージ送信機能と、QuasarDBの高性能な時系列ストレージを組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントとSinkを利用して、MQTTメッセージやクライアントイベントをQuasarDBに保存できます。この統合により、EMQXからQuasarDBへのデータ取り込みが簡素化され、複雑なコーディングなしでデータの保存と管理が可能になります。

以下の図は、EMQXとQuasarDB間の典型的なデータ統合アーキテクチャを示しています：

![quasardb_integration](./assets/quasardb_integration.png)

QuasarDBへのMQTTデータ取り込みは以下のように動作します：

1. **メッセージのパブリッシュと受信**：IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、リアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理が開始されます。
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXで定義されたルールに従って処理されます。ルールは事前定義された条件に基づいて、どのメッセージをQuasarDBにルーティングするかを決定します。ペイロード変換が指定されている場合は、データ形式の変換や特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **QuasarDBへのデータ取り込み**：ルールによりメッセージのQuasarDBへの書き込みがトリガーされます。SQLテンプレートを利用して、ルール処理結果からデータを抽出しSQLを構築、QuasarDBに送信して実行することで、メッセージの特定フィールドを対応するテーブルに書き込みます。
4. **データの保存と活用**：データがQuasarDBに保存されることで、企業はその時系列クエリ機能を活用し、分析、監視、運用用途に利用できます。

## 特長とメリット

QuasarDBとのデータ統合は以下の特長とメリットを提供します：

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからQuasarDBへの効率的かつ信頼性の高いデータ伝送を実現します。即時の洞察やアクションを必要とするユースケースに最適です。
- **高性能な時系列ストレージ**：QuasarDBのカラムエンジンは時系列ワークロードに最適化されており、大量のタイムスタンプ付きデータに対して高速な取り込みスループットと効率的な範囲クエリを提供します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、QuasarDBに保存する前にデータを前処理できます。フィルタリング、ルーティング、集約、強化など多様なデータ変換が可能です。
- **バッチ処理対応**：QuasarDB Sinkはバッチ書き込みをサポートし、往復回数を削減して全体の取り込みスループットを向上させます。

## はじめる前に

このセクションでは、QuasarDBデータ統合を作成する前に必要な準備、ODBCドライバーの設定やQuasarDBのインストール方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### ODBCドライバーのインストールと設定

QuasarDBコネクターはODBCを使用してデータベースに接続します。EMQXが稼働するホストにQuasarDB ODBCドライバーをインストールおよび設定する必要があります。

完全なインストール手順は[QuasarDB ODBCドキュメント](https://doc.quasar.ai/master/user-guide/integration/odbc.html)を参照してください。以下はDebian系システムでドライバー3.14.1を使用した典型的なセットアップ例です。

1. QuasarDB C APIパッケージとODBCドライバーをダウンロードしてインストールします：

   ```bash
   curl -fsSL -O https://download.quasar.ai/quasardb/3.14/3.14.1/api/c/qdb-api_3.14.1.deb
   curl -fsSL -O https://download.quasar.ai/quasardb/3.14/3.14.1/api/odbc/qdb-3.14.1-linux-64bit-odbc-driver.tar.gz
   apt-get install -yqq ./qdb-api_3.14.1.deb
   tar -C /tmp/qdb_odbc_driver -xf qdb-3.14.1-linux-64bit-odbc-driver.tar.gz
   ```

2. `/etc/odbcinst.ini`にドライバーを登録します：

   ```ini
   [qdb_odbc_driver]
   Description=Quasardb ODBC Driver
   Driver=/tmp/qdb_odbc_driver/lib/libqdb_odbc_driver.so
   Setup=/tmp/qdb_odbc_driver/lib/libqdb_odbc_driver.so
   ```

3. `/etc/odbc.ini`にデータソース名（DSN）エントリを作成します：

   ```ini
   [qdb]
   Driver = qdb_odbc_driver
   Description = QuasarDB ODBC Data Source
   #URI = qdb://172.100.239.30:2836
   #UID = user_name
   #PWD = user_key
   #KEY = cluster_public_key
   ```

ここで設定したDSN名（例：`qdb`）は、コネクター作成時の**ODBC Data Source Name**フィールドに入力します。

### QuasarDBのインストールと接続

このセクションでは、Dockerを使ったQuasarDBインスタンスの起動方法を説明します。

1. QuasarDBのDockerイメージをプルして起動します：

   ```bash
   docker run -d --name qdb \
     -p 2836:2836 \
     bureau14/qdb:3.14.1
   ```

   ::: tip

   QuasarDBはホスト名ではなく**IPアドレス**での接続が必要です。URIには`127.0.0.1`（または実際のホストIP）を使用してください。ホスト名ベースの接続はサポートされていません。

   :::

2. QuasarDBシェルで接続し、インスタンスが起動していることを確認します：

   ```bash
   docker run -it --rm bureau14/qdbsh --cluster qdb://127.0.0.1:2836
   ```

ユーザー認証やクラスタキー認証を有効にする場合は、[QuasarDBセキュリティドキュメント](https://doc.quasar.ai/)を参照してください。

### テーブルの作成

QuasarDBに取り込むデータを受け取るテーブルを作成します。以下は温度と湿度の読み取り値を保存するテーブル作成例です：

```sql
CREATE TABLE temp_hum (temp DOUBLE, hum DOUBLE);
```

::: tip

QuasarDBのテーブルには常に暗黙の`$timestamp`インデックス列が含まれます。テーブル作成時に宣言する必要はありませんが、INSERT文で参照可能です。

:::

## コネクターの作成

このセクションでは、EMQXとQuasarDBを接続するコネクターの作成方法を説明します。

1. EMQXダッシュボードで、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**ページで**QuasarDB**を選択し、**Next**をクリックします。

4. コネクター名を入力します。英数字の組み合わせで、例として`my_quasardb`などを指定します。

5. 接続情報を設定します：

   - **Server URI**：IPアドレスを使ったQuasarDBクラスタのURIを入力します（例：`qdb://127.0.0.1:2836`）。
   - **ODBC Data Source Name**：`/etc/odbc.ini`で定義したDSN名を入力します（例：`qdb`）。
   - **Username**：ユーザー名（ある場合）。
   - **Password**：ユーザーのシークレットキー（ある場合）。
   - **Cluster Public Key**：クラスタの公開鍵（ある場合）。

6. 詳細設定（任意）：詳細は[高度な設定](#advanced-configuration)を参照してください。

7. **Create**をクリックする前に、**Test Connectivity**をクリックしてEMQXからQuasarDBへの接続を検証できます。

8. **Create**ボタンをクリックしてコネクターの作成を完了します。作成成功のダイアログが表示され、ルールを今すぐ作成するか尋ねられます。**Create Rule**をクリックするとコネクターが事前選択された状態でルール作成画面に進みます。**Back To Connector List**をクリックすると戻って後でルールを作成できます。

## QuasarDB Sinkを使ったルールの作成

このセクションでは、ソースMQTTトピック`t/#`からのメッセージを処理し、QuasarDBの`temp_hum`テーブルに保存するルールをダッシュボードで作成する方法を説明します。

1. もし前のステップで**Create Rule**をクリックしていれば、**Add Action**パネルが自動で開き、**Type of Action**が`QuasarDB`、コネクターが事前選択されています。ステップ5に進んでください。

   そうでなければ、EMQXダッシュボードで**Integration** -> **Rules**をクリックし、右上の**Create**をクリック、続いて**+ Add Action**をクリックします。

2. 左側の**SQL Editor**にルールIDと以下のSQLを入力し、トピック`t/#`のメッセージにマッチさせます：

   注意：独自のSQL構文を指定する場合は、Sinkが必要とする全フィールドが`SELECT`句に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストできます。

   :::

3. 右側の**Add Action**パネルで、**Type of Action**ドロップダウンから`QuasarDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。

4. **Connectors**ドロップダウンから先ほど作成した`my_quasardb`コネクターを選択します。新しいコネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

5. Sinkの名前と任意の説明を入力します。

6. QuasarDBへの書き込み方法を定義する**SQL Template**を設定します。

   ::: tip 注意

   SQLテンプレートは**INSERT**文のみ対応しています。UPDATEやDELETEなど他の文はサポートされていません。

   :::

   SQLテンプレートは`${clientid}`のようなプレースホルダー変数をサポートします。QuasarDBは暗黙のタイムスタンプインデックス列として`$timestamp`を使用し、`now()`で現在のサーバー時刻を挿入できます。

   ::: tip 注意

   QuasarDB ODBCドライバーはプリペアドステートメントをサポートしていません。`STRING`や`BLOB`型に解決される値は、SQLテンプレート内で手動でシングルクォート（`'`）で囲む必要があります。

   :::

   ```sql
   insert into temp_hum($timestamp, temp, hum)
   values (now(), ${.temp}, ${.hum})
   ```

7. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のために1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

8. **詳細設定（任意）**：詳細は[Sinkの高度な設定](#sink-advanced-settings)を参照してください。

9. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがQuasarDBに接続できるかテストできます。

10. **Create**ボタンをクリックしてSinkの設定を完了します。新しいSinkが**Action Outputs**に追加されます。

11. **Create Rule**ページに戻り、設定内容を確認して**Save**ボタンをクリックしルールを生成します。

これでルールが正常に作成されました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると新しいQuasarDB Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で処理された後にQuasarDBに転送されていることを確認できます。

## ルールのテスト

MQTTXを使ってトピック`t/1`にメッセージを送信し、ルールをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "temp": "27.5", "hum": "41.8" }'
```

QuasarDB Sinkの実行統計を確認してください。マッチしたメッセージ数と送信済みメッセージ数がそれぞれ1増えているはずです。QuasarDBの`temp_hum`テーブルにデータが書き込まれていることを検証してください。

## 高度な設定

このセクションでは、QuasarDBコネクターおよびSinkの高度な設定オプションについて説明します。ダッシュボードで設定する際は、**Advanced Settings**を展開して、用途に応じて以下のパラメータを調整できます。

### コネクターの高度な設定

| フィールド名               | 説明                                                                                     | デフォルト値   |
|----------------------------|------------------------------------------------------------------------------------------|---------------|
| Connection Pool Size       | プール内で維持する同時接続数。大きすぎるとシステムリソースを枯渇させ、小さすぎるとスループットが制限されます。 | `8`           |
| Connect Timeout            | QuasarDBへの接続確立時に待機する最大時間。                                             | `5`秒         |
| Start Timeout              | 自動起動リソースが正常になるまでコネクターが待機する最大時間。                           | `5`秒         |
| Health Check Interval      | QuasarDB接続の自動ヘルスチェックを実行する間隔。                                       | `15`秒        |
| Health Check Timeout       | 各ヘルスチェックの完了に許容される最大時間。                                           | `60`秒        |

### Sinkの高度な設定

| フィールド名               | 説明                                                                                     | デフォルト値   |
|----------------------------|------------------------------------------------------------------------------------------|---------------|
| Buffer Pool Size           | EMQXとQuasarDB間のデータフローを処理するバッファワーカーの数。負荷が高い場合は増やすとスループットが向上します。 | `16`          |
| Request TTL                | バッファ内でリクエストが有効な最大時間。キュー内や未アックのままこの時間を超えたリクエストは破棄されます。 | `45`秒        |
| Health Check Interval      | SinkがQuasarDB接続の自動ヘルスチェックを実行する間隔。                                 | `15`秒        |
| Health Check Interval Jitter | 複数ノードが同時にヘルスチェックを行わないように間隔にランダム遅延を加えます。同じコネクターを共有する複数のアクションやソースがある場合に有効です。 | `0`ミリ秒     |
| Health Check Timeout       | 各Sinkヘルスチェックの完了に許容される最大時間。                                       | `60`秒        |
| Max Buffer Queue Size      | 各バッファワーカーが保持可能な最大バイト数。負荷のバーストが多い場合は増やしてください。 | `256`MB       |
| Batch Size                 | 1回の操作でQuasarDBに送信する最大レコード数。`1`に設定するとバッチ処理を無効にし、個別送信になります。 | `100`         |
| Query Mode                 | `async`はQuasarDBの書き込み確認を待たずにパブリッシュを継続し、`sync`は確認を待ってから進行します。非同期はスループットが高い反面、順序が前後する可能性があります。 | `Async`       |
| Inflight Window            | 同時に未アックのリクエスト数の最大値。**Query Mode**が`async`の場合、クライアントごとのメッセージ順序を保証するには`1`に設定してください。 | `100`         |
