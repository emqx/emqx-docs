# Microsoft SQL ServerへのMQTTデータ取り込み

[SQL Server](https://www.microsoft.com/en-us/sql-server/)は、企業や組織の規模や種類を問わず広く利用されている主要なリレーショナル商用データベースソリューションの一つです。EMQXはSQL Serverとの統合をサポートしており、MQTTメッセージやクライアントイベントをSQL Serverに保存することが可能です。これにより、複雑なデータパイプラインや分析処理の構築、データ管理・分析、デバイス接続管理、ERP、CRM、BIなどの他の企業システムとの連携が容易になります。

本ページでは、EMQXとMicrosoft SQL Server間のデータ統合について詳細に解説し、実際の作成および検証手順を紹介します。

::: tip

Microsoft SQL Serverとのデータ統合は、EMQX Enterprise 5.0.3以降でサポートされています。

:::

## 動作概要

Microsoft SQL Serverとのデータ統合はEMQXの標準機能であり、EMQXのデバイス接続およびメッセージ送信機能とMicrosoft SQL Serverの強力なデータ保存機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントとSinkを通じて、MQTTメッセージやクライアントイベントをMicrosoft SQL Serverに保存できます。さらに、イベントによりMicrosoft SQL Server内のデータ更新や削除をトリガーでき、デバイスのオンライン状態や接続履歴などの情報を記録可能です。この統合により、EMQXからSQL Serverへのデータ取り込みが簡素化され、複雑なコーディングが不要になります。

以下の図は、EMQXとSQL Server間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration SQL Server](./assets/emqx-integration-sql_server.png)

Microsoft SQL ServerへのMQTTデータ取り込みの流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、機械、センサー、製造ラインの稼働状態、計測値、トリガーイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXで定義されたルールに従って処理されます。ルールは事前定義された条件に基づき、どのメッセージをMicrosoft SQL Serverへルーティングするかを決定します。ペイロード変換を指定するルールがあれば、データ形式の変換、特定情報のフィルタリング、ペイロードの付加情報による強化などが適用されます。
3. **SQL Serverへのデータ取り込み**：ルールがメッセージのMicrosoft SQL Serverへの書き込みをトリガーします。SQLテンプレートを用いて、ルール処理結果からデータを抽出しSQLを構築し、SQL Serverで実行することで、メッセージの特定フィールドを対応するテーブルやカラムに書き込んだり更新したりします。
4. **データの保存と活用**：Microsoft SQL Serverに保存されたデータを活用し、様々なビジネスユースケースにおけるクエリ処理が可能になります。

## 特長と利点

Microsoft SQL Serverとのデータ統合は、効率的なデータ送信、保存、活用を実現するための多様な特長と利点を備えています。

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからMicrosoft SQL Serverへの効率的かつ信頼性の高いデータ送信を実現します。即時の洞察やアクションが求められるユースケースに最適です。
- **高性能かつスケーラブル**：EMQXとMicrosoft SQL Serverは共に拡張性と信頼性を備え、大規模なIoTデータの処理に適しています。需要の増加に応じて水平・垂直の拡張が途切れることなく可能であり、IoTアプリケーションの継続性と信頼性を確保します。
- **柔軟なデータ変換**：EMQXの強力なSQLベースのルールエンジンにより、Microsoft SQL Serverに保存する前にデータを前処理できます。フィルタリング、ルーティング、集約、強化など多様なデータ変換機構をサポートし、組織のニーズに応じたデータ整形が可能です。
- **高度な分析機能**：Microsoft SQL ServerはAnalysis Servicesによる多次元データモデル構築など強力な分析機能を提供し、複雑なデータ分析やデータマイニングを支援します。また、Reporting Servicesを通じてIoTデータの洞察や分析結果をレポート化し、関係者に提示できます。

## はじめる前に

本節では、Microsoft SQL Serverデータ統合の作成を開始する前に必要な準備について説明します。ODBCドライバーのインストールと設定、Microsoft SQL Serverのインストールと接続、データベースおよびデータテーブルの作成方法を含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### ODBCドライバーのインストールと設定

Microsoft SQL ServerデータベースにアクセスするためにODBCドライバーを設定する必要があります。ODBCドライバーとしては、FreeTDSまたはMicrosoft提供のmsodbcsql18ドライバーのいずれかを使用できます。

EMQXは`odbcinst.ini`設定で指定されたDSN名を用いてドライバーの動的ライブラリのパスを判別します。以下の例ではDSN名を`ms-sql`としています。詳細は[接続プロパティ](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/connection-string-keywords-and-data-source-names-dsns?view=sql-server-ver16#connection-properties)を参照してください。

::: tip 注意

DSN名は任意に設定可能ですが、英字のみの使用を推奨します。また、DSN名は大文字・小文字を区別します。

:::

#### msodbcsql18ドライバーのODBCドライバーとしてのインストールと設定

<!-- TODO: コマンドやDockerfileのタグバージョンを更新 -->

msodbcsql18ドライバーをODBCドライバーとして使用する場合は、Microsoftの手順を参照してください。

- [Microsoft ODBCドライバーのインストール（Linux）](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver16&tabs=alpine18-install%2Calpine17-install%2Cdebian8-install%2Credhat7-13-install%2Crhel7-offline)
- [Microsoft ODBCドライバーのインストール（macOS）](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/install-microsoft-odbc-driver-sql-server-macos?view=sql-server-ver16)

MicrosoftのEULA条件により、EMQXが提供するDockerイメージにはmsodbcsql18ドライバーが含まれていません。DockerやKubernetesで使用する場合は、[EMQX Enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)が提供するイメージをベースにODBCドライバーをインストールした新しいイメージを作成する必要があります。新しいイメージを使用することは、[Microsoft SQL Server EULA](https://go.microsoft.com/fwlink/?linkid=857698)に同意したことを意味します。

以下の手順で新しいイメージをビルドしてください。

1. 以下のDockerfileを使用して新しいイメージをビルドします。

   この例のベースイメージバージョンは`emqx/emqx-enterprise:5.8.1`です。必要なEMQX Enterpriseバージョンに応じてビルドするか、最新バージョンの`emqx/emqx-enterprise:latest`を使用してください。

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

2. 以下のコマンドで新しいイメージをビルドします。

   ```bash
   docker build -t emqx/emqx-enterprise:5.8.1-msodbc .
   ```

3. ビルド後、`docker image ls`でローカルイメージの一覧を確認できます。イメージをアップロードまたは保存して後で使用することも可能です。

::: tip 注意

この例でmsodbcsql18ドライバーをインストールした場合、`odbcinst.ini`のDSN名は`ms-sql`になっていることを確認してください。必要に応じてDSN名を変更できます。

:::

#### FreeTDSのODBCドライバーとしてのインストールと設定

ここでは、主要なディストリビューションでFreeTDSをODBCドライバーとしてインストール・設定する方法を紹介します。

MacOSでFreeTDS ODBCドライバーをインストール・設定する例：

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

CentOSでFreeTDS ODBCドライバーをインストール・設定する例：

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

Ubuntu（例としてUbuntu 20.04）の場合のFreeTDS ODBCドライバーインストール・設定例（他バージョンは公式ODBCドキュメントを参照）：

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

### Microsoft SQL Serverのインストールと接続

この節では、Dockerイメージを使ってLinux/MacOS上でMicrosoft SQL Server 2019を起動し、`sqlcmd`で接続する方法を説明します。その他のインストール方法については[Microsoft SQL Serverインストールガイド](https://learn.microsoft.com/en-us/sql/database-engine/install-windows/install-sql-server?view=sql-server-ver16)を参照してください。

1. Docker経由でMicrosoft SQL Serverをインストールし、以下のコマンドでDockerイメージを起動します。パスワードは`mqtt_public1`を使用します。Microsoft SQL Serverのパスワードポリシーは[パスワードの複雑さ](https://learn.microsoft.com/en-us/sql/relational-databases/security/password-policy?view=sql-server-ver16#password-complexity)を参照してください。

   注意：環境変数`ACCEPT_EULA=Y`を指定してDockerコンテナを起動することで、MicrosoftのEULAに同意したことになります。詳細は[エンドユーザー使用許諾契約](https://go.microsoft.com/fwlink/?linkid=857698)を参照してください。

   ```bash
   # Microsoft SQL Server Dockerイメージを起動し、パスワードを`mqtt_public1`に設定
   $ docker run --name sqlserver -p 1433:1433 -e ACCEPT_EULA=Y -e MSSQL_SA_PASSWORD=mqtt_public1 -d mcr.microsoft.com/mssql/server:2022-CU15-ubuntu-22.04
   ```

2. コンテナにアクセスします。

   ```bash
   docker exec -it sqlserver bash
   ```

3. コンテナ内で設定したパスワードを入力してサーバーに接続します。パスワード入力時は文字が表示されません。入力後はEnterキーを押してください。

   ```bash
   $ /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P mqtt_public1 -N -C
   1>
   ```

   ::: tip

   Microsoft SQL Serverコンテナには`mssql-tools18`パッケージがインストールされていますが、実行ファイルは`$PATH`に含まれていません。そのため、`sqlcmd`を使用する際は実行ファイルのパスを指定する必要があります。ここでは`/opt`配下にあります。

   `mssql-tools18`の使い方の詳細は[sqlcmdユーティリティ](https://learn.microsoft.com/en-us/sql/tools/sqlcmd/sqlcmd-utility?view=sql-server-ver16)を参照してください。

   :::

これでMicrosoft SQL Server 2022インスタンスがデプロイされ、接続可能になりました。

### データベースとデータテーブルの作成

前節で作成した接続を用いて、以下のSQL文でデータテーブルを作成します。

::: tip

ODBCインターフェースの制限により、CJK文字や絵文字などのUnicode文字を書き込む場合は、挿入前にバイナリ形式に変換する関数を使用する必要があります。テーブル作成時はUnicode文字を格納するカラムの型を`NVARCHAR`に設定してください。

:::

- MQTTメッセージを保存するためのデータテーブルを作成します。メッセージID、トピック、QoS、ペイロード、パブリッシュ時刻を含みます。

  ```sql
  CREATE TABLE dbo.t_mqtt_msg (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                               msgid   VARCHAR(64) NULL,
                               topic   VARCHAR(100) NULL,
                               qos     tinyint NOT NULL DEFAULT 0,
                               payload VARCHAR(100) NULL,
                               arrived DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
  GO
  ```

- クライアントのオンライン/オフライン状態を記録するためのデータテーブルを作成します。

  ```sql
  CREATE TABLE dbo.t_mqtt_events (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                                  clientid VARCHAR(255) NULL,
                                  event_type VARCHAR(255) NULL,
                                  event_time DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
  GO
  ```

## コネクターの作成

この節では、SinkをMicrosoft SQL Serverに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとMicrosoft SQL Serverの両方がローカルマシンで動作していることを前提としています。リモートで動作している場合は設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**ページで**Microsoft SQL Server**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下を設定します。
   - **Connector name**：コネクター名を入力します。英数字の組み合わせが望ましく、例：`my_sqlserver`
   - **Server Host**：`127.0.0.1:1433`またはMicrosoft SQL Serverがリモートの場合はそのURLを入力します。

     ::: tip

     Named Instanceを使用している場合は、インスタンスが動作するポート番号を明示的に指定する必要があります。ドライバーは指定されたポートを使ってインスタンスに接続し、ヘルスチェック時にEMQXがインスタンス名を推測します。

     Server Host欄にインスタンス名のみ（例：`MYSERVER\SQL2022`）を指定しても正しいインスタンスに接続できる保証はありません。必ずポート設定を確認してください。

     :::

   - **Database Name**：`master`を入力します。
   - **Username**：`sa`を入力します。
   - **Password**：設定したパスワード`mqtt_public1`または実際のパスワードを入力します。
   - **SQL Server Driver Name**：`ms-sql`を入力します。これは`odbcinst.ini`で設定したDSN名です。

5. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてMicrosoft SQL Serverへの接続確認が可能です。

7. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを用いたルール作成を続行できます。ルール作成の詳細は[メッセージ保存用Microsoft SQL Server Sinkのルール作成](#create-a-rule-with-microsoft-sql-server-sink-for-message-storage)および[イベント記録用Microsoft SQL Server Sinkのルール作成](#create-a-rule-with-microsoft-sql-server-sink-for-events-recording)を参照してください。

## メッセージ保存用Microsoft SQL Server Sinkのルール作成

この節では、DashboardでソースMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みSink経由でMicrosoft SQL Serverのテーブル`dbo.t_mqtt_msg`に保存するルールの作成方法を示します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. 画面右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力します。メッセージ保存用ルールを作成するため、**SQL Editor**に以下の文を入力します。これはトピック`t/#`配下のMQTTメッセージをMicrosoft SQL Serverに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドが`SELECT`句に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   ODBCインターフェースの制限により、CJK文字や絵文字などのUnicode文字を書き込む場合は、挿入前にバイナリ形式に変換する関数を使用する必要があります。

   ルール作成時に組み込み関数を使って文字列をUTF-16リトルエンディアンエンコードのバイナリ文字列に変換できます。例：

   ```sql
   SELECT
     sqlserver_bin2hexstr(str_utf16_le(payload)) as payload,
     *
   FROM
     "t/#"
   ```

   :::

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールの学習とテストを行うことができます。

   :::

4. + **Add Action**ボタンをクリックし、ルールトリガー時に実行されるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをMicrosoft SQL Serverに送信します。

5. **Type of Action**ドロップダウンリストから`Microsoft SQL Server`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既に作成済みのMicrosoft SQL Server Sinkを選択することも可能です。この例では新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせにしてください。

7. **Connector**ドロップダウンから先に作成した`my_sqlserver`を選択します。ドロップダウン横のボタンから新規コネクターを作成することも可能です。設定パラメーターは[コネクターの作成](#create-a-connector)を参照してください。

8. メッセージ保存用の**SQL Template**を以下のSQL文で設定します。

   ::: warning 重要

   EMQX 6.3.1以降、Sink作成時にSQLテンプレートを解析し、SQLコンテキストに基づいてプレースホルダー値をエスケープし、サポートされない構文を拒否します。テンプレートはMicrosoft SQL Serverの単一の`INSERT INTO ... VALUES`文で、1行のみ設定可能です。プレースホルダーは値の位置および文字列リテラル内でサポートされます。SQLコメント、追加文、複数行設定、識別子内のプレースホルダーはサポートされません。

   この検証により、以前のバージョンで許容されたテンプレートが拒否される場合があります。アップグレード前に互換性のないテンプレートを修正してください。

   :::

   ```sql
   insert into dbo.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )
   ```

   ::: tip

   ODBCインターフェースの制限により、CJK文字や絵文字などのUnicode文字を書き込む場合は、挿入前にバイナリ形式に変換する関数を使用する必要があります。

   SQLテンプレート内で`CONVERT`関数を用いてMicrosoft SQL Server側で対応するバイナリデータを文字列に変換できます。

   ```sql
   insert into dbo.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, CONVERT(NVARCHAR(100), ${payload}) )
   ```

   :::

   SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL template**上部の**Undefined Vars as Null**スイッチを切り替えてルールエンジンの動作を設定できます。

   - **無効**（デフォルト）：ルールエンジンは文字列`undefined`をデータベースに挿入します。
   - **有効**：変数が未定義の場合、ルールエンジンは`NULL`を挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがMicrosoft SQL Serverに接続できるか確認できます。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでMicrosoft SQL Server Sink用のルールが作成されました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいMicrosoft SQL Server Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されMicrosoft SQL Serverに送信・保存されていることが確認できます。

## イベント記録用Microsoft SQL Server Sinkのルール作成

この節では、クライアントのオンライン/オフライン状態を記録し、イベントデータを設定済みSink経由でMicrosoft SQL Serverのテーブル`dbo.t_mqtt_events`に保存するルールの作成方法を示します。

手順は[メッセージ保存用Microsoft SQL Server Sinkのルール作成](#create-a-rule-with-microsoft-sql-server-sink-for-message-storage)とほぼ同様で、SQLテンプレートおよびSQLルールのみ異なります。

前節で説明したSQLテンプレートの制限もこのテンプレートに適用されます。

オンライン/オフライン状態記録用のルールSQL文は以下の通りです。

```sql
SELECT
  *,
  floor(timestamp / 1000) as s_shift,
  timestamp div 1000 as ms_shift
FROM
  "$events/client_connected", "$events/client_disconnected"
```

イベント記録用のSQLテンプレートは以下の通りです。

```sql
insert into dbo.t_mqtt_events(clientid, event_type, event_time) values ( ${clientid}, ${event}, DATEADD(MS, ${ms_shift}, DATEADD(S, ${s_shift}, '19700101 00:00:00:000') ) )
```

## ルールのテスト

MQTT Xを使ってトピック`t/1`にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello SQL Server" }'
```

Microsoft SQL Server Sinkの実行統計を確認します。

- メッセージ保存用Sinkでは、新たに1件のマッチングと1件の送信済みメッセージがあるはずです。`dbo.t_mqtt_msg`テーブルにデータが書き込まれているか確認してください。

```bash
1> SELECT * from dbo.t_mqtt_msg
2> GO
id          msgid                                                            topic                                                                                                qos payload                                                                                              arrived
----------- ---------------------------------------------------------------- ---------------------------------------------------------------------------------------------------- --- ---------------------------------------------------------------------------------------------------- -----------------------
 1000000001 0005F995096D9466F442000010520002                                 t/1                                                                                                    0 { "msg": "Hello SQL Server" }                                                                        2023-04-18 04:49:47.170

(1 rows affected)
1>
```

- オンライン/オフライン状態記録用Sinkでは、新たに2件のイベント（クライアント接続、切断）が記録されているはずです。`dbo.t_mqtt_events`テーブルに状態記録が書き込まれているか確認してください。

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
