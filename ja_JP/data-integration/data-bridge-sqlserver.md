# Microsoft SQL Server への MQTT データ取り込み

[SQL Server](https://www.microsoft.com/en-us/sql-server/) は、企業や組織の規模や種類を問わず広く利用されている主要な商用リレーショナルデータベースソリューションの一つです。EMQX は SQL Server との統合をサポートしており、MQTT メッセージやクライアントイベントを SQL Server に保存することが可能です。これにより、複雑なデータパイプラインや分析プロセスの構築、データ管理・分析、デバイス接続管理、さらには ERP、CRM、BI などの他の企業システムとの統合が容易になります。

本ページでは、EMQX と Microsoft SQL Server 間のデータ統合について詳細に解説し、データ統合の作成および検証方法を実践的に説明します。

::: tip

Microsoft SQL Server とのデータ統合は EMQX Enterprise 5.0.3 以降でサポートされています。

:::

## 動作概要

Microsoft SQL Server とのデータ統合は EMQX の標準機能として提供されており、EMQX のデバイス接続およびメッセージ送受信機能と Microsoft SQL Server の強力なデータ保存機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントと Sink を利用して、MQTT メッセージやクライアントイベントを Microsoft SQL Server に保存できます。さらに、イベントをトリガーとして Microsoft SQL Server 内のデータ更新や削除を行うことも可能で、デバイスのオンライン状態や接続履歴の記録などに活用できます。この統合により、EMQX から SQL Server へのデータ取り込みが簡素化され、複雑なコーディングなしでデータの保存・管理が実現します。

以下の図は、EMQX と SQL Server 間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration SQL Server](./assets/emqx-integration-sql_server.png)

Microsoft SQL Server への MQTT データ取り込みは以下のように動作します：

1. **メッセージのパブリッシュと受信**：産業用 IoT デバイスは MQTT プロトコルを介して EMQX に正常に接続し、機械、センサー、製造ラインの稼働状態や計測値、トリガーイベントに基づくリアルタイム MQTT データを EMQX にパブリッシュします。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQX に定義されたルールで処理されます。ルールは事前定義された条件に基づき、Microsoft SQL Server にルーティングすべきメッセージを判定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、ペイロードの付加情報による拡充などの処理が適用されます。
3. **SQL Server へのデータ取り込み**：ルールはメッセージの Microsoft SQL Server への書き込みをトリガーします。SQL テンプレートを活用してルール処理結果からデータを抽出し、SQL を構築して SQL Server に送信・実行します。これにより、メッセージの特定フィールドをデータベースの対応するテーブル・カラムに書き込んだり更新したりできます。
4. **データの保存と活用**：Microsoft SQL Server にデータが保存された後、企業はそのクエリ機能を活用して様々なユースケースに対応できます。

## 特長とメリット

Microsoft SQL Server とのデータ統合は、効率的なデータ送信・保存・活用を実現するために以下の特長とメリットを備えています：

- **リアルタイムデータストリーミング**：EMQX はリアルタイムデータストリームの処理に最適化されており、ソースシステムから Microsoft SQL Server への効率的かつ信頼性の高いデータ送信を実現します。即時の洞察やアクションが必要なユースケースに最適です。
- **高性能かつスケーラブル**：EMQX と Microsoft SQL Server は共に拡張性と信頼性を備え、大規模な IoT データの処理に対応可能です。需要の増加に応じて水平・垂直方向の拡張を継続的に行え、IoT アプリケーションの継続性と信頼性を確保します。
- **柔軟なデータ変換**：EMQX の強力な SQL ベースのルールエンジンにより、Microsoft SQL Server に保存する前にデータの前処理が可能です。フィルタリング、ルーティング、集約、拡充など多様なデータ変換機能をサポートし、ニーズに応じたデータ整形を実現します。
- **高度な分析機能**：Microsoft SQL Server は Analysis Services による多次元データモデル構築など強力な分析機能を提供し、複雑なデータ分析やデータマイニングを支援します。Reporting Services によりレポート作成・公開も可能で、IoT データの洞察や分析結果を関係者に提示できます。

## はじめる前に

本セクションでは、Microsoft SQL Server とのデータ統合を作成する前に必要な準備として、ODBC ドライバーのインストールと設定、Microsoft SQL Server のインストールと接続、データベースおよびデータテーブルの作成方法を説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### ODBC ドライバーのインストールと設定

Microsoft SQL Server データベースにアクセスするために ODBC ドライバーの設定が必要です。ODBC ドライバーとしては、FreeTDS または Microsoft 提供の msodbcsql18 ドライバーを使用できます。

EMQX は `odbcinst.ini` 設定で指定された DSN 名を参照してドライバーの動的ライブラリのパスを特定します。以下の例では DSN 名を `ms-sql` としています。詳細は [Connection Properties](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/connection-string-keywords-and-data-source-names-dsns?view=sql-server-ver16#connection-properties) を参照してください。

::: tip 補足

DSN 名は任意に設定可能ですが、英字のみの使用を推奨します。また、DSN 名は大文字小文字を区別します。

:::

#### msodbcsql18 ドライバーのインストールと設定

<!-- TODO: コマンドや Dockerfile のタグバージョンを更新してください -->

msodbcsql18 ドライバーを ODBC ドライバーとして使用する場合は、Microsoft の公式手順を参照してください：

- [Microsoft ODBC ドライバーのインストール（Linux）](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver16&tabs=alpine18-install%2Calpine17-install%2Cdebian8-install%2Credhat7-13-install%2Crhel7-offline)
- [Microsoft ODBC ドライバーのインストール（macOS）](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/install-microsoft-odbc-driver-sql-server-macos?view=sql-server-ver16)

Microsoft の EULA 条項により、EMQX が提供する Docker イメージには msodbcsql18 ドライバーは含まれていません。Docker や Kubernetes で使用する場合は、[EMQX Enterprise](https://hub.docker.com/r/emqx/emqx-enterprise) が提供するイメージをベースに ODBC ドライバーをインストールした新しいイメージを作成する必要があります。新しいイメージを使用することは、[Microsoft SQL Server EULA](https://go.microsoft.com/fwlink/?linkid=857698) に同意したことを意味します。

以下の手順で新しいイメージをビルドします：

1. 以下の Dockerfile を使用して新しいイメージをビルドします。

   この例のベースイメージバージョンは `emqx/emqx-enterprise:5.8.1` です。必要な EMQX Enterprise バージョンに応じてビルドするか、最新バージョンの `emqx/emqx-enterprise:latest` を使用してください。

```dockerfile
FROM emqx/emqx-enterprise:5.8.1

USER root

RUN apt-get -qq update && apt-get install -yqq curl gpg && \
    . /etc/os-release && \
    curl -fsSL https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor -o /usr/share/keyrings/microsoft-prod.gpg && \
    curl -fsSL "https://packages.microsoft.com/config/${ID}/${VERSION_ID}/prod.list" > /etc/apt/sources.list.d/mssql-release.list && \
    apt-get -qq update && \
    ACCEPT_EULA=Y apt-get install -yqq msodbcsql18 unixodbc-dev && \
    sed -i 's/ODBC Driver 18 for SQL Server/ms-sql/g' /etc/odbcinst.ini && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

USER emqx
```

2. コマンド `docker build -t emqx/emqx-enterprise:5.8.1-msodbc` を実行して新しいイメージをビルドします。

3. ビルド後、`docker image ls` でローカルイメージ一覧を確認できます。イメージのアップロードや保存も可能です。

::: tip 補足

この例で msodbcsql18 ドライバーをインストールした場合、`odbcinst.ini` の DSN 名は `ms-sql` になっていることを確認してください。必要に応じて DSN 名は変更可能です。

:::

#### FreeTDS を ODBC ドライバーとしてインストール・設定

ここでは主流のディストリビューションで FreeTDS を ODBC ドライバーとしてインストール・設定する方法を紹介します。

MacOS での FreeTDS ODBC ドライバーのインストールと設定：

```bash
$ brew install unixodbc freetds
$ vim /usr/local/etc/odbcinst.ini
# 以下の内容を追加
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/local/lib/libtdsodbc.so
Setup       = /usr/local/lib/libtdsodbc.so
FileUsage   = 1
```

CentOS での FreeTDS ODBC ドライバーのインストールと設定：

```bash
$ yum install unixODBC unixODBC-devel freetds freetds-devel perl-DBD-ODBC perl-local-lib
$ vim /etc/odbcinst.ini
# 以下の内容を追加
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib64/libtdsodbc.so
Setup       = /usr/lib64/libtdsS.so.2
Driver64    = /usr/lib64/libtdsodbc.so
Setup64     = /usr/lib64/libtdsS.so.2
FileUsage   = 1
```

Ubuntu での FreeTDS ODBC ドライバーのインストールと設定（Ubuntu 20.04 を例に、他バージョンは公式 ODBC ドキュメントを参照）：

```bash
$ apt-get install unixodbc unixodbc-dev tdsodbc freetds-bin freetds-common freetds-dev libdbd-odbc-perl liblocal-lib-perl
$ vim /etc/odbcinst.ini
# 以下の内容を追加
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libtdsS.so
FileUsage   = 1
```

### Microsoft SQL Server のインストールと接続

このセクションでは、Linux/MacOS 上で Docker イメージを使って Microsoft SQL Server 2019 を起動し、`sqlcmd` で接続する方法を説明します。その他のインストール方法は [Microsoft SQL Server インストールガイド](https://learn.microsoft.com/en-us/sql/database-engine/install-windows/install-sql-server?view=sql-server-ver16) を参照してください。

1. Docker で Microsoft SQL Server をインストールし、以下のコマンドで Docker イメージを起動します。パスワードは `mqtt_public1` を使用します。Microsoft SQL Server のパスワードポリシーは [Password Complexity](https://learn.microsoft.com/en-us/sql/relational-databases/security/password-policy?view=sql-server-ver16#password-complexity) を参照してください。

   注意：環境変数 `ACCEPT_EULA=Y` を設定して Docker コンテナを起動することで、Microsoft EULA の条件に同意したことになります。詳細は [End-User Licensing Agreement](https://go.microsoft.com/fwlink/?linkid=857698) をご覧ください。

   ```bash
   # Microsoft SQL Server Docker イメージを起動し、パスワードを `mqtt_public1` に設定
   $ docker run --name sqlserver -p 1433:1433 -e ACCEPT_EULA=Y -e MSSQL_SA_PASSWORD=mqtt_public1 -d mcr.microsoft.com/mssql/server:2022-CU15-ubuntu-22.04
   ```

2. コンテナにアクセスします。

   ```bash
   docker exec -it sqlserver bash
   ```

3. コンテナ内で設定したパスワードを入力してサーバーに接続します。パスワード入力時は文字が表示されません。入力後はそのまま `Enter` を押してください。

   ```bash
   $ /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P mqtt_public1 -N -C
   1>
   ```

   ::: tip

   Microsoft が提供する Microsoft SQL Server コンテナには `mssql-tools18` パッケージがインストールされていますが、実行ファイルは `$PATH` に含まれていません。そのため、`sqlcmd` を実行する際はパスを指定する必要があります。本例の Docker 環境では `/opt` 配下にあります。

   `mssql-tools18` の使い方は [sqlcmd-utility](https://learn.microsoft.com/en-us/sql/tools/sqlcmd/sqlcmd-utility?view=sql-server-ver16) を参照してください。

   :::

これで Microsoft SQL Server 2022 インスタンスのデプロイと接続が完了しました。

### データベースとデータテーブルの作成

前節で作成した接続を利用し、以下の SQL 文でデータテーブルを作成します。

::: tip

ODBC インターフェースの制限により、CJK 文字や絵文字などの Unicode 文字を挿入する場合は、挿入前にバイナリ形式に変換する関数を使用する必要があります。テーブル作成時には Unicode 文字を格納するカラムの型を `NVARCHAR` に設定してください。

:::

- MQTT メッセージを保存するためのデータテーブルを作成します。メッセージ ID、トピック、QoS、ペイロード、パブリッシュ時刻を含みます。

  ```sql
  CREATE TABLE dbo.t_mqtt_msg (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                               msgid   VARCHAR(64) NULL,
                               topic   VARCHAR(100) NULL,
                               qos     tinyint NOT NULL DEFAULT 0,
                               payload VARCHAR(100) NULL,
                               arrived DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
  GO
  ```

- クライアントのオンライン／オフライン状態を記録するためのデータテーブルを作成します。

  ```sql
  CREATE TABLE dbo.t_mqtt_events (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                                  clientid VARCHAR(255) NULL,
                                  event_type VARCHAR(255) NULL,
                                  event_time DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
  GO
  ```

## コネクターの作成

このセクションでは、Sink を Microsoft SQL Server に接続するためのコネクターの作成方法を示します。

以下の手順は、EMQX と Microsoft SQL Server の両方をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQX ダッシュボードにアクセスし、**Integration** -> **Connectors** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Microsoft SQL Server** を選択し、**Next** をクリックします。

4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_sqlserver`
   
   - **Server Host**：`127.0.0.1:1433` または Microsoft SQL Server がリモートの場合はその URL を入力します。
   
     ::: tip
   
     Named Instance を使用している場合は、インスタンスが稼働するポート番号を明示的に指定する必要があります。ドライバーは指定ポートでインスタンスに接続し、EMQX はヘルスチェック時にインスタンス名を推測します。
   
     Server Host にインスタンス名のみ（例：`MYSERVER\SQL2022`）を指定しても正しいインスタンスに接続できる保証はありません。必ずポート設定を確認してください。
   
     :::
   
   - **Database Name**：`master` を入力します。
   
   - **Username**：`sa` を入力します。
   
   - **Password**：設定したパスワード `mqtt_public1` または実際のパスワードを入力します。
   
   - **SQL Server Driver Name**：`ms-sql` を入力します。これは `odbcinst.ini` で設定した DSN 名です。
   
5. 高度な設定（任意）：詳細は [Features of Sink](./data-bridges.md#features-of-sink) を参照してください。

6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Microsoft SQL Server に接続できるかテストできます。

7. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして Sink を利用したルールの作成に進めます。ルール作成の詳細は [Create a Rule with Microsoft SQL Server Sink for Message Storage](#create-a-rule-with-microsoft-sql-server-sink-for-message-storage) および [Create a Rule with Microsoft SQL Server Sink for Events Recording](#create-a-rule-with-microsoft-sql-server-sink-for-events-recording) を参照してください。

## Microsoft SQL Server Sink を使ったメッセージ保存ルールの作成

このセクションでは、ソース MQTT トピック `t/#` からのメッセージを処理し、処理済みデータを設定済みの Sink 経由で Microsoft SQL Server のテーブル `dbo.t_mqtt_msg` に保存するルールをダッシュボードで作成する方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力します。メッセージ保存用ルールを作成するため、**SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` 以下の MQTT メッセージを Microsoft SQL Server に保存することを意味します。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   ODBC インターフェースの制限により、CJK 文字や絵文字などの Unicode 文字を挿入する場合は、挿入前にバイナリ形式に変換する関数を使用してください。

   ルール作成時に組み込み関数を使って文字列を UTF-16 リトルエンディアンエンコードのバイナリ文字列に変換できます。例：

   ```sql
   SELECT
     sqlserver_bin2hexstr(str_utf16_le(payload)) as payload,
     *
   FROM
     "t/#"
   ```

   :::

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストを行ってください。

   :::

4. + **Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQX はルール処理済みデータを Microsoft SQL Server に送信します。

5. **Type of Action** ドロップダウンリストから `Microsoft SQL Server` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みの Microsoft SQL Server Sink を選択することもできますが、ここでは新規 Sink を作成します。

6. Sink 名を入力します。英数字の組み合わせで指定してください。

7. **Connector** ドロップダウンから先に作成した `my_sqlserver` を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは [Create a Connector](#create-a-connector) を参照してください。

8. メッセージ保存用の **SQL Template** を以下の SQL 文で設定します。

   注意：これは前処理済みの SQL なので、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   insert into dbo.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )
   ```

   ::: tip

   ODBC インターフェースの制限により、CJK 文字や絵文字などの Unicode 文字を挿入する場合は、挿入前にバイナリ形式に変換する関数を使用してください。

   Microsoft SQL Server 側で対応するバイナリデータを文字列に変換するには、SQL テンプレート内で `CONVERT` 関数を使用できます。

   ```sql
   insert into dbo.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, CONVERT(NVARCHAR(100), ${payload}) )
   ```

   :::

   SQL テンプレート内でプレースホルダー変数が未定義の場合、**SQL template** 上部の **Undefined Vars as Null** スイッチでルールエンジンの動作を切り替えられます：

   - **Disabled**（デフォルト）：ルールエンジンは文字列 `undefined` をデータベースに挿入します。

   - **Enabled**：変数が未定義の場合、ルールエンジンは `NULL` を挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. フォールバックアクション（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [Fallback Actions](./data-bridges.md#fallback-actions) を参照してください。

10. 高度な設定（任意）：詳細は [Features of Sink](./data-bridges.md#features-of-sink) を参照してください。

11. **Create** をクリックする前に、**Test Connectivity** をクリックして Sink が Microsoft SQL Server に接続できるかテストできます。

12. **Create** ボタンをクリックして Sink の設定を完了します。新しい Sink が **Action Outputs** に追加されます。

13. **Create Rule** ページに戻り、設定内容を確認して **Create** ボタンをクリックし、ルールを生成します。

これで Microsoft SQL Server Sink 用のルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Microsoft SQL Server Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` 以下のメッセージがルール `my_rule` によって解析され Microsoft SQL Server に送信・保存される様子を確認できます。

## Microsoft SQL Server Sink を使ったイベント記録ルールの作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済み Sink 経由で Microsoft SQL Server のテーブル `dbo.t_mqtt_events` に保存するルールの作成方法を示します。

手順は [Microsoft SQL Server Sink を使ったメッセージ保存ルールの作成](#microsoft-sql-server-sink-を使ったメッセージ保存ルールの作成) とほぼ同様ですが、SQL テンプレートと SQL ルール文が異なります。

オンライン／オフライン状態記録用のルール SQL 文は以下の通りです。

```sql
SELECT
  *,
  floor(timestamp / 1000) as s_shift,
  timestamp div 1000 as ms_shift
FROM
  "$events/client_connected", "$events/client_disconnected"
```

イベント記録用の SQL テンプレートは以下の通りです。

```sql
insert into dbo.t_mqtt_events(clientid, event_type, event_time) values ( ${clientid}, ${event}, DATEADD(MS, ${ms_shift}, DATEADD(S, ${s_shift}, '19700101 00:00:00:000') ) )
```

## ルールのテスト

MQTT X を使ってトピック `t/1` にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello SQL Server" }'
```

Microsoft SQL Server Sink の稼働状況を確認します。

- メッセージ保存用 Sink では、新たに 1 件のマッチングと 1 件の送信済みメッセージがあるはずです。`dbo.t_mqtt_msg` テーブルにデータが書き込まれているか確認してください。

```bash
1> SELECT * from dbo.t_mqtt_msg
2> GO
id          msgid                                                            topic                                                                                                qos payload                                                                                              arrived
----------- ---------------------------------------------------------------- ---------------------------------------------------------------------------------------------------- --- ---------------------------------------------------------------------------------------------------- -----------------------
 1000000001 0005F995096D9466F442000010520002                                 t/1                                                                                                    0 { "msg": "Hello SQL Server" }                                                                        2023-04-18 04:49:47.170

(1 rows affected)
1>
```

- オンライン／オフライン状態記録用 Sink では、新たに 2 件のイベント（クライアント接続、切断）が記録されているはずです。`dbo.t_mqtt_events` テーブルに状態記録が書き込まれているか確認してください。

```bash
1> SELECT * from dbo.t_mqtt_events
2> GO
id          clientid                                                         event_type                                                                                                                                                                                                    event_time
----------- ---------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- -----------------------
 1000000001 emqx_c                                                           client.connected                                                                                                                                                                                              2023-04-18 04:49:47.140
 1000000002 emqx_c                                                           client.disconnected                                                                                                                                                                                           2023-04-18 04:49:47.180

(2 rows affected)
1>
```
