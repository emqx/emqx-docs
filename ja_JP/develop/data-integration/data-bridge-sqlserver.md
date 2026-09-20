# Microsoft SQL ServerへのMQTTデータ取り込み

[SQL Server](https://www.microsoft.com/en-us/sql-server/)は、企業や組織の規模や種類を問わず広く利用されている主要な商用リレーショナルデータベースソリューションの一つです。EMQXはSQL Serverとの統合をサポートしており、MQTTメッセージやクライアントイベントをSQL Serverに保存できます。これにより、複雑なデータパイプラインや分析処理の構築、データ管理や分析、デバイス接続管理、ERP、CRM、BIなど他の企業システムとの連携が容易になります。

本ページでは、EMQXとMicrosoft SQL Server間のデータ統合について、実践的な作成手順と検証方法を含めて詳しく解説します。

::: tip

Microsoft SQL Serverとのデータ統合は、EMQX Enterprise 5.0.3以降でサポートされています。

:::

## 動作の仕組み

Microsoft SQL Serverとのデータ統合は、EMQXに標準搭載された機能であり、EMQXのデバイス接続およびメッセージ送受信機能とMicrosoft SQL Serverの強力なデータ保存機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントとSinkを利用して、MQTTメッセージやクライアントイベントをMicrosoft SQL Serverに保存できます。さらに、イベントに応じてMicrosoft SQL Server内のデータ更新や削除をトリガーでき、デバイスのオンライン状態や接続履歴などの情報を記録可能です。この統合により、EMQXからSQL Serverへのデータ取り込みが簡素化され、複雑なコーディングを必要としません。

以下の図は、EMQXとSQL Server間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration SQL Server](./assets/emqx-integration-sql_server.png)

Microsoft SQL ServerへのMQTTデータ取り込みは以下のように動作します：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、機械、センサー、製造ラインの稼働状態、計測値、トリガーイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールに従って処理されます。ルールは事前定義された条件に基づき、どのメッセージをMicrosoft SQL Serverにルーティングするかを判断します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、ペイロードへの追加コンテキスト付与などの変換が適用されます。
3. **SQL Serverへのデータ取り込み**：ルールはメッセージのMicrosoft SQL Serverへの書き込みをトリガーします。SQLテンプレートを用いて、ルール処理結果からデータを抽出しSQLを構築、SQL Serverで実行することで、メッセージの特定フィールドを対応するテーブルやカラムに書き込みまたは更新します。
4. **データの保存と活用**：データがMicrosoft SQL Serverに保存されることで、企業はそのクエリ機能を活用し、多様なユースケースに対応可能となります。

## 特長と利点

Microsoft SQL Serverとのデータ統合は、効率的なデータ送信、保存、活用を実現するための多彩な特長と利点を提供します：

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリーム処理に最適化されており、ソースシステムからMicrosoft SQL Serverへの効率的かつ信頼性の高いデータ送信を保証します。即時の洞察やアクションが必要なユースケースに理想的です。
- **高性能かつスケーラブル**：EMQXとMicrosoft SQL Serverは共に拡張性と信頼性を備え、大規模なIoTデータの処理に適しています。需要の増加に応じて水平・垂直の拡張が途切れることなく可能で、IoTアプリケーションの継続性と信頼性を確保します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、Microsoft SQL Serverに保存する前にデータの前処理が可能です。フィルタリング、ルーティング、集約、エンリッチメントなど多様なデータ変換機能をサポートし、ニーズに応じたデータ整形を実現します。
- **高度な分析機能**：Microsoft SQL ServerはAnalysis Servicesによる多次元データモデル構築など強力な分析機能を備え、複雑なデータ分析やデータマイニングを支援します。また、Reporting Servicesを通じてIoTデータの洞察や分析結果をレポートとして作成・公開できます。

## はじめる前に

本節では、Microsoft SQL Serverデータ統合の作成を始める前に必要な準備について説明します。ODBCドライバーのインストールと設定、Microsoft SQL Serverのインストールと接続、データベースおよびデータテーブルの作成方法を含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### ODBCドライバーのインストールと設定

Microsoft SQL ServerデータベースにアクセスするためにODBCドライバーの設定が必要です。ODBCドライバーとしては、FreeTDSまたはMicrosoftが提供するmsodbcsql18ドライバーのいずれかを利用できます。

EMQXは`odbcinst.ini`設定ファイルで指定されたDSN名を用いてドライバーの動的ライブラリのパスを判別します。以下の例ではDSN名は`ms-sql`です。詳細は[接続プロパティ](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/connection-string-keywords-and-data-source-names-dsns?view=sql-server-ver16#connection-properties)を参照してください。

::: tip 注意

DSN名は任意に設定可能ですが、英字のみの使用を推奨します。また、DSN名は大文字・小文字を区別します。

:::

#### msodbcsql18ドライバーのインストールと設定

<!-- TODO: コマンドやDockerfileのタグバージョンを更新 -->

msodbcsql18ドライバーをODBCドライバーとして使用する場合は、Microsoftの公式手順を参照してください：

- [Microsoft ODBCドライバーのインストール（Linux）](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver16&tabs=alpine18-install%2Calpine17-install%2Cdebian8-install%2Credhat7-13-install%2Crhel7-offline)
- [Microsoft ODBCドライバーのインストール（macOS）](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/install-microsoft-odbc-driver-sql-server-macos?view=sql-server-ver16)

MicrosoftのEULA条件により、EMQXが提供するDockerイメージにはmsodbcsql18ドライバーは含まれていません。DockerやKubernetesで利用する場合は、[EMQX Enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)のイメージをベースにODBCドライバーをインストールした新しいイメージを作成する必要があります。新しいイメージを使用することは、[Microsoft SQL ServerのEULA](https://go.microsoft.com/fwlink/?linkid=857698)に同意したことを意味します。

以下の手順で新しいイメージをビルドしてください：

1. 以下のDockerfileを使用して新しいイメージをビルドします。

   この例のベースイメージは`emqx/emqx-enterprise:5.8.1`です。必要なEMQX Enterpriseのバージョンに基づいてビルドするか、最新の`emqx/emqx-enterprise:latest`を使用してください。

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

2. `docker build -t emqx/emqx-enterprise:5.8.1-msodbc`コマンドで新しいイメージをビルドします。

3. ビルド後、`docker image ls`でローカルイメージ一覧を確認できます。必要に応じてイメージのアップロードや保存も可能です。

::: tip 注意

この例でmsodbcsql18ドライバーをインストールした場合、`odbcinst.ini`のDSN名は`ms-sql`になっていることを確認してください。必要に応じてDSN名は変更可能です。

:::

#### FreeTDSのODBCドライバーとしてのインストールと設定

ここでは主要なディストリビューションでのFreeTDSをODBCドライバーとしてインストール・設定する方法を紹介します。

MacOSでのFreeTDS ODBCドライバーのインストールと設定：

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

CentOSでのFreeTDS ODBCドライバーのインストールと設定：

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

UbuntuでのFreeTDS ODBCドライバーのインストールと設定（Ubuntu20.04を例に、他のバージョンは公式ODBCドキュメントを参照）：

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

本節では、Dockerイメージを用いてLinux/MacOS上でMicrosoft SQL Server 2019を起動し、`sqlcmd`で接続する方法を説明します。その他のインストール方法については[Microsoft SQL Serverインストールガイド](https://learn.microsoft.com/en-us/sql/database-engine/install-windows/install-sql-server?view=sql-server-ver16)を参照してください。

1. DockerでMicrosoft SQL Serverをインストールし、以下のコマンドでDockerイメージを起動します。パスワードは`mqtt_public1`を使用します。Microsoft SQL Serverのパスワードポリシーは[パスワードの複雑さ](https://learn.microsoft.com/en-us/sql/relational-databases/security/password-policy?view=sql-server-ver16#password-complexity)を参照してください。

   注意：環境変数`ACCEPT_EULA=Y`を指定してDockerコンテナを起動することで、MicrosoftのEULAに同意したことになります。詳細は[エンドユーザー使用許諾契約](https://go.microsoft.com/fwlink/?linkid=857698)を参照してください。

   ```bash
   # Microsoft SQL Server Dockerイメージを起動し、パスワードをmqtt_public1に設定
   $ docker run --name sqlserver -p 1433:1433 -e ACCEPT_EULA=Y -e MSSQL_SA_PASSWORD=mqtt_public1 -d mcr.microsoft.com/mssql/server:2022-CU15-ubuntu-22.04
   ```

2. コンテナにアクセスします。

   ```bash
   docker exec -it sqlserver bash
   ```

3. コンテナ内で設定したパスワードを入力してサーバーに接続します。パスワード入力時は文字が表示されません。入力後はそのまま`Enter`を押してください。

   ```bash
   $ /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P mqtt_public1 -N -C
   1>
   ```

   ::: tip

   Microsoftが提供するMicrosoft SQL Serverコンテナには`mssql-tools18`パッケージがインストールされていますが、実行ファイルは`$PATH`に含まれていません。そのため、`sqlcmd`を実行する際はフルパスを指定する必要があります。この例のDocker環境ではパスは`/opt`です。

   `mssql-tools18`の使い方の詳細は[sqlcmdユーティリティ](https://learn.microsoft.com/en-us/sql/tools/sqlcmd/sqlcmd-utility?view=sql-server-ver16)を参照してください。

   :::

これでMicrosoft SQL Server 2022インスタンスのデプロイと接続が完了しました。

### データベースとデータテーブルの作成

前節で作成した接続を用いて、以下のSQL文でデータテーブルを作成します。

::: tip

ODBCインターフェースの制限により、CJK文字やEmojiなどのUnicode文字を書き込む場合は、挿入前にバイナリ形式に変換する関数を使用する必要があります。テーブル作成時にはUnicode文字を格納するカラムの型を`NVARCHAR`に設定してください。

:::

- MQTTメッセージを保存するためのデータテーブルを作成します。メッセージID、トピック、QoS、ペイロード、パブリッシュ時間を含みます。

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

本節では、SinkをMicrosoft SQL Serverに接続するためのコネクター作成手順を示します。

以下の手順は、EMQXとMicrosoft SQL Serverの両方をローカルマシンで実行している前提です。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**Microsoft SQL Server**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_sqlserver`
   
   - **Server Host**：`127.0.0.1:1433`またはMicrosoft SQL Serverがリモートの場合はそのURLを入力します。
   
     ::: tip
   
     Named Instanceを使用する場合は、インスタンスが動作するポート番号を明示的に指定する必要があります。ドライバーは指定されたポートでインスタンスに接続し、EMQXはヘルスチェック時にインスタンス名を推測します。
   
     Server Host欄にインスタンス名のみ（例：`MYSERVER\SQL2022`）を指定しても正しいインスタンスに接続できる保証はありません。必ずポート設定を確認してください。
   
     :::
   
   - **Database Name**：`master`を入力します。
   
   - **Username**：`sa`を入力します。
   
   - **Password**：事前設定したパスワード`mqtt_public1`または実際のパスワードを入力します。
   
   - **SQL Server Driver Name**：`ms-sql`（`odbcinst.ini`で設定したDSN名）を入力します。
   
5. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてMicrosoft SQL Serverへの接続確認を行えます。

7. ページ下部の**Create**ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを用いたルール作成を続行できます。ルール作成の詳細は[メッセージ保存用Microsoft SQL Server Sinkのルール作成](#create-a-rule-with-microsoft-sql-server-sink-for-message-storage)および[イベント記録用Microsoft SQL Server Sinkのルール作成](#create-a-rule-with-microsoft-sql-server-sink-for-events-recording)を参照してください。

## メッセージ保存用Microsoft SQL Server Sinkのルール作成

本節では、ダッシュボード上でMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みSink経由でMicrosoft SQL Serverのテーブル`dbo.t_mqtt_msg`に保存するルールの作成方法を示します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力します。メッセージ保存用のルールを作成するため、**SQL Editor**に以下のステートメントを入力します。これはトピック`t/#`配下のMQTTメッセージをMicrosoft SQL Serverに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要な全フィールドを`SELECT`句に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   ODBCインターフェースの制限により、CJK文字やEmojiなどのUnicode文字を書き込む場合は、挿入前にバイナリ形式に変換する関数を使用する必要があります。

   ルール作成時に組み込み関数を使い、文字列をUTF-16リトルエンディアンエンコードのバイナリ文字列に変換可能です。例：

   ```sql
   SELECT
     sqlserver_bin2hexstr(str_utf16_le(payload)) as payload,
     *
   FROM
     "t/#"
   ```

   :::

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストが可能です。

   :::

4. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをMicrosoft SQL Serverに送信します。

5. **Type of Action**ドロップダウンから`Microsoft SQL Server`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既に作成済みのMicrosoft SQL Server Sinkがあれば選択可能です。本デモでは新規Sinkを作成します。

6. Sink名を入力します。英数字の組み合わせで指定してください。

7. **Connector**ドロップダウンから前に作成した`my_sqlserver`を選択します。新規コネクターはドロップダウン横のボタンから作成可能です。設定パラメーターは[コネクター作成](#create-a-connector)を参照してください。

8. メッセージ保存用の**SQL Template**を以下のSQL文で設定します。

   注意：これは前処理済みSQLのため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   insert into dbo.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )
   ```

   ::: tip

   ODBCインターフェースの制限により、Unicode文字（CJK文字やEmojiなど）を書き込む場合は、挿入前にバイナリ形式に変換する関数を使用してください。

   SQLテンプレート内で`CONVERT`関数を使い、Microsoft SQL Server側で対応するバイナリデータを文字列に変換可能です。

   ```sql
   insert into dbo.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, CONVERT(NVARCHAR(100), ${payload}) )
   ```

   :::

   SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL template**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を切り替えられます：

   - **Disabled**（デフォルト）：未定義変数は文字列`undefined`としてデータベースに挿入されます。

   - **Enabled**：未定義変数の場合、ルールエンジンは`NULL`をデータベースに挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. フォールバックアクション（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**でSinkがMicrosoft SQL Serverに接続可能かテストできます。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでMicrosoft SQL Server Sink用のルール作成が完了しました。**Integration** -> **Rules**ページで新規作成ルールを確認できます。**Actions(Sink)**タブで新しいMicrosoft SQL Server Sinkを確認可能です。

また、**Integration** -> **Flow Designer**を開くとトポロジーが表示され、トピック`t/#`配下のメッセージがルール`my_rule`で解析されMicrosoft SQL Serverに送信・保存されていることが確認できます。

## イベント記録用Microsoft SQL Server Sinkのルール作成

本節では、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みSink経由でMicrosoft SQL Serverのテーブル`dbo.t_mqtt_events`に保存するルール作成方法を示します。

手順は[メッセージ保存用Microsoft SQL Server Sinkのルール作成](#create-a-rule-with-microsoft-sql-server-sink-for-message-storage)とほぼ同様ですが、SQLテンプレートとSQLルールが異なります。

オンライン／オフライン状態記録用のルールSQLステートメントは以下の通りです。

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

MQTT Xを使ってトピック`t/1`にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello SQL Server" }'
```

Microsoft SQL Server Sinkの実行統計を確認します。

- メッセージ保存用Sinkでは、新たに1件のマッチングと1件の送信済みメッセージがあるはずです。`dbo.t_mqtt_msg`データテーブルにデータが書き込まれているか確認してください。

```bash
1> SELECT * from dbo.t_mqtt_msg
2> GO
id          msgid                                                            topic                                                                                                qos payload                                                                                              arrived
----------- ---------------------------------------------------------------- ---------------------------------------------------------------------------------------------------- --- ---------------------------------------------------------------------------------------------------- -----------------------
 1000000001 0005F995096D9466F442000010520002                                 t/1                                                                                                    0 { "msg": "Hello SQL Server" }                                                                        2023-04-18 04:49:47.170

(1 rows affected)
1>
```

- オンライン／オフライン状態記録用Sinkでは、新たに2件のイベント（クライアント接続・切断）が記録されているはずです。`dbo.t_mqtt_events`データテーブルに状態記録が書き込まれているか確認してください。

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
