# Microsoft SQL ServerへのMQTTデータ取り込み

[SQL Server](https://www.microsoft.com/en-us/sql-server/)は、企業や組織の規模や種類を問わず広く利用されている主要なリレーショナル商用データベースソリューションの一つです。EMQXはSQL Serverとの連携をサポートしており、MQTTメッセージやクライアントイベントをSQL Serverに保存できます。これにより、複雑なデータパイプラインや分析プロセスの構築、データ管理・解析、デバイス接続管理、ERPやCRM、BIなどの他の企業システムとの統合が容易になります。

本ページでは、EMQXとMicrosoft SQL Server間のデータ統合について、作成および検証の実践的な手順を含めて詳細に解説します。

::: tip

Microsoft SQL Serverとのデータ統合は、EMQX Enterprise 5.0.3以降でサポートされています。

:::

## 動作概要

Microsoft SQL Serverとのデータ統合はEMQXの標準機能であり、EMQXのデバイス接続およびメッセージ伝送機能とMicrosoft SQL Serverの強力なデータ保存機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントとSinkを利用して、MQTTメッセージやクライアントイベントをMicrosoft SQL Serverに保存できます。さらに、イベントに応じてMicrosoft SQL Server内のデータ更新や削除をトリガーでき、デバイスのオンライン状態や接続履歴などの情報を記録可能です。この統合により、EMQXからSQL Serverへのデータ取り込みが簡素化され、複雑なコーディングなしでデータの保存・管理が実現します。

以下の図は、EMQXとSQL Server間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration SQL Server](./assets/emqx-integration-sql_server.png)

Microsoft SQL ServerへのMQTTデータ取り込みの流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、機械やセンサー、製品ラインの稼働状態や計測値、トリガーイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXに定義されたルールで処理されます。ルールは事前定義された条件に基づき、どのメッセージをMicrosoft SQL Serverにルーティングするかを決定します。ペイロード変換を指定したルールがあれば、データ形式の変換や特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **SQL Serverへのデータ取り込み**：ルールによりメッセージのMicrosoft SQL Serverへの書き込みがトリガーされます。SQLテンプレートを用いて、ルール処理結果からデータを抽出しSQLを構築、SQL Serverで実行することで、メッセージの特定フィールドを対応するデータベースのテーブルやカラムに書き込んだり更新したりします。
4. **データの保存と活用**：データがMicrosoft SQL Serverに保存された後、企業はSQL Serverのクエリ機能を活用して様々なユースケースに利用できます。

## 特長とメリット

Microsoft SQL Serverとのデータ統合は、効率的なデータ伝送、保存、活用を実現するための多彩な特長とメリットを備えています。

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリーム処理に最適化されており、ソースシステムからMicrosoft SQL Serverへの効率的かつ信頼性の高いデータ伝送を実現します。即時の洞察やアクションが必要なユースケースに適しています。
- **高性能かつスケーラブル**：EMQXとMicrosoft SQL Serverは共に拡張性と信頼性を備え、大規模なIoTデータ処理に対応可能です。需要増加に応じて水平・垂直の無停止拡張が可能で、IoTアプリケーションの継続性と信頼性を確保します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、Microsoft SQL Serverに保存する前にデータを前処理できます。フィルタリング、ルーティング、集約、強化など多様なデータ変換機構をサポートし、組織のニーズに合わせてデータを整形可能です。
- **高度な分析機能**：Microsoft SQL ServerはAnalysis Servicesによる多次元データモデル構築など強力な分析機能を提供し、複雑なデータ分析やデータマイニングを支援します。また、Reporting Servicesを通じてレポート作成・公開が可能で、IoTデータの洞察や分析結果を関係者に提示できます。

## はじめる前に

本セクションでは、Microsoft SQL Serverデータ統合の作成を開始する前に必要な準備について説明します。ODBCドライバーのインストールと設定、Microsoft SQL Serverのインストールと接続、データベースおよびデータテーブルの作成方法を含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### ODBCドライバーのインストールと設定

Microsoft SQL ServerデータベースにアクセスするためにODBCドライバーの設定が必要です。ODBCドライバーとしては、FreeTDSまたはMicrosoft提供のmsodbcsql18ドライバーのいずれかを使用できます。

EMQXは`odbcinst.ini`設定内のDSN名を参照してドライバーの動的ライブラリのパスを特定します。以下の例ではDSN名を`ms-sql`としています。詳細は[接続プロパティ](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/connection-string-keywords-and-data-source-names-dsns?view=sql-server-ver16#connection-properties)を参照してください。

::: tip 注意

DSN名は任意に設定可能ですが、英字のみの使用が推奨されます。またDSN名は大文字小文字を区別します。

:::

#### msodbcsql18ドライバーのインストールと設定

<!-- TODO: コマンドおよびDockerfileのタグバージョン更新 -->

msodbcsql18ドライバーをODBCドライバーとして使用する場合は、Microsoftの手順を参照してください：

- [Microsoft ODBCドライバーのインストール（Linux）](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver16&tabs=alpine18-install%2Calpine17-install%2Cdebian8-install%2Credhat7-13-install%2Crhel7-offline)
- [Microsoft ODBCドライバーのインストール（macOS）](https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/install-microsoft-odbc-driver-sql-server-macos?view=sql-server-ver16)

MicrosoftのEULA条件により、EMQXが提供するDockerイメージにはmsodbcsql18ドライバーは含まれていません。DockerやKubernetesで使用する場合は、[EMQX Enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)が提供するイメージをベースにODBCドライバーをインストールした新しいイメージを作成する必要があります。新しいイメージを使用することで、[Microsoft SQL Server EULA](https://go.microsoft.com/fwlink/?linkid=857698)に同意したものとみなされます。

以下の手順で新しいイメージをビルドしてください。

1. 以下のDockerfileを使用して新しいイメージをビルドします。

   この例のベースイメージバージョンは`emqx/emqx-enterprise:5.8.1`です。必要なEMQX Enterpriseバージョンに応じてビルドするか、最新の`emqx/emqx-enterprise:latest`を使用してください。

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

3. ビルド後、`docker image ls`でローカルイメージ一覧を確認できます。必要に応じてイメージをアップロードまたは保存してください。

::: tip 注意

この例でmsodbcsql18ドライバーをインストールした場合、`odbcinst.ini`のDSN名は`ms-sql`になっていることを確認してください。必要に応じてDSN名は変更可能です。

:::

#### FreeTDSのインストールと設定

ここでは、主要なディストリビューションでのFreeTDSをODBCドライバーとしてインストールおよび設定する方法を紹介します。

MacOSでのFreeTDS ODBCドライバーのインストールと設定：

```bash
$ brew install unixodbc freetds
$ vim /usr/local/etc/odbcinst.ini
# 以下を追加
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
# 以下を追加
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib64/libtdsodbc.so
Setup       = /usr/lib64/libtdsS.so.2
Driver64    = /usr/lib64/libtdsodbc.so
Setup64     = /usr/lib64/libtdsS.so.2
FileUsage   = 1
```

Ubuntu（例：Ubuntu20.04）でのFreeTDS ODBCドライバーのインストールと設定：

```bash
$ apt-get install unixodbc unixodbc-dev tdsodbc freetds-bin freetds-common freetds-dev libdbd-odbc-perl liblocal-lib-perl
$ vim /etc/odbcinst.ini
# 以下を追加
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libtdsS.so
FileUsage   = 1
```

### Microsoft SQL Serverのインストールと接続

このセクションでは、Dockerイメージを使ってLinux/MacOS上でMicrosoft SQL Server 2019を起動し、`sqlcmd`で接続する方法を説明します。その他のインストール方法については[Microsoft SQL Serverインストールガイド](https://learn.microsoft.com/en-us/sql/database-engine/install-windows/install-sql-server?view=sql-server-ver16)を参照してください。

1. DockerでMicrosoft SQL Serverをインストールし、以下のコマンドでDockerイメージを起動します。パスワードは`mqtt_public1`を使用します。Microsoft SQL Serverのパスワードポリシーは[パスワードの複雑性](https://learn.microsoft.com/en-us/sql/relational-databases/security/password-policy?view=sql-server-ver16#password-complexity)を参照してください。

   注意：環境変数`ACCEPT_EULA=Y`を指定してDockerコンテナを起動することで、MicrosoftのEULAに同意したものとみなされます。詳細は[エンドユーザー使用許諾契約](https://go.microsoft.com/fwlink/?linkid=857698)を参照してください。

   ```bash
   # Microsoft SQL ServerのDockerイメージを起動し、パスワードを`mqtt_public1`に設定
   $ docker run --name sqlserver -p 1433:1433 -e ACCEPT_EULA=Y -e MSSQL_SA_PASSWORD=mqtt_public1 -d mcr.microsoft.com/mssql/server:2022-CU15-ubuntu-22.04
   ```

2. コンテナにアクセスします。

   ```bash
   docker exec -it sqlserver bash
   ```

3. コンテナ内で設定したパスワードを入力してサーバーに接続します。パスワード入力時は文字が表示されません。入力後にEnterを押してください。

   ```bash
   $ /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P mqtt_public1 -N -C
   1>
   ```

   ::: tip

   Microsoftが提供するMicrosoft SQL Serverコンテナには`mssql-tools18`パッケージがインストールされていますが、実行ファイルは`$PATH`に含まれていません。そのため、`sqlcmd`を使用する際は実行ファイルのパスを指定する必要があります。この例のDocker環境ではパスは`/opt`です。

   `mssql-tools18`の使用方法の詳細は[sqlcmdユーティリティ](https://learn.microsoft.com/en-us/sql/tools/sqlcmd/sqlcmd-utility?view=sql-server-ver16)を参照してください。

   :::

これでMicrosoft SQL Server 2022のインスタンスがデプロイされ、接続可能になりました。

### データベースとデータテーブルの作成

前節で作成した接続を使い、以下のSQL文でデータテーブルを作成します。

::: tip

ODBCインターフェースの制約により、CJK文字や絵文字などのUnicode文字を書き込む場合は、挿入前にバイナリ形式に変換する関数を使用する必要があります。テーブル作成時はUnicode文字を格納するカラムの型を`NVARCHAR`に設定してください。

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

このセクションでは、SinkをMicrosoft SQL Serverに接続するためのコネクターの作成方法を説明します。

以下の手順は、EMQXとMicrosoft SQL Serverの両方をローカルマシンで実行していることを前提としています。リモート環境で実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**Microsoft SQL Server**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下を設定します：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせが望ましく、例：`my_sqlserver`
   
   - **Server Host**：`127.0.0.1:1433`（ローカル実行の場合）またはリモートのMicrosoft SQL ServerのURLを入力します。
   
     ::: tip
   
     Named Instanceを使用する場合は、インスタンスが稼働するポート番号を明示的に指定する必要があります。ドライバーは指定されたポートを使ってインスタンスに接続し、ヘルスチェック時にEMQXはインスタンス名を推測します。
   
     Server Host欄にインスタンス名（例：`MYSERVER\SQL2022`）のみを指定しても正しいインスタンスに接続できる保証はありません。必ずポート設定を確認してください。
   
     :::
   
   - **Database Name**：`master`を入力します。
   
   - **Username**：`sa`を入力します。
   
   - **Password**：事前設定したパスワード`mqtt_public1`または実際のパスワードを入力します。
   
   - **SQL Server Driver Name**：`ms-sql`（`odbcinst.ini`で設定したDSN名）を入力します。
   
5. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**を押してMicrosoft SQL Serverへの接続テストが可能です。

7. ページ下部の**Create**をクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択可能です。後者を選ぶと、Sinkを指定してMicrosoft SQL Serverに転送するデータやクライアントイベントの記録を行うルール作成に進めます。詳細は[メッセージ保存用Microsoft SQL Server Sinkのルール作成](#create-a-rule-with-microsoft-sql-server-sink-for-message-storage)および[イベント記録用Microsoft SQL Server Sinkのルール作成](#create-a-rule-with-microsoft-sql-server-sink-for-events-recording)を参照してください。

## メッセージ保存用Microsoft SQL Server Sinkのルール作成

このセクションでは、ダッシュボードでMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みSink経由でMicrosoft SQL Serverのテーブル`dbo.t_mqtt_msg`に保存するルールの作成方法を示します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力します。メッセージ保存用のルールを作成するため、**SQL Editor**に以下の文を入力します。これはトピック`t/#`配下のMQTTメッセージをMicrosoft SQL Serverに保存することを意味します。

   注意：独自のSQL文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`句に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   ODBCインターフェースの制約により、CJK文字や絵文字などのUnicode文字を書き込む場合は、挿入前にバイナリ形式に変換する関数を使用する必要があります。

   ルール作成時に組み込み関数を使って文字列をUTF-16リトルエンディアンエンコードのバイナリ文字列に変換可能です。例：

   ```sql
   SELECT
     sqlserver_bin2hexstr(str_utf16_le(payload)) as payload
     *
   FROM
     "t/#"
   ```

   :::

   ::: tip

   初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習やテストが可能です。

   :::

4. + **Add Action**ボタンをクリックし、ルール発動時にトリガーされるアクションを定義します。このアクションによりEMQXはルール処理済みデータをMicrosoft SQL Serverに送信します。

5. **Type of Action**ドロップダウンから`Microsoft SQL Server`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのMicrosoft SQL Server Sinkを選択することも可能ですが、ここでは新規Sinkを作成します。

6. Sink名を入力します。英数字の組み合わせが望ましいです。

7. **Connector**ドロップダウンから先に作成した`my_sqlserver`を選択します。新規コネクターはドロップダウン横のボタンから作成可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

8. メッセージ保存用の**SQL Template**を以下のSQL文で設定します。

   注意：これは前処理済みSQLのため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   insert into dbo.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )
   ```

   ::: tip

   ODBCインターフェースの制約により、Unicode文字（CJK文字や絵文字など）を書き込む場合は、挿入前にバイナリ形式に変換する関数を使用してください。

   SQLテンプレート内で`CONVERT`関数を使い、Microsoft SQL Server側で対応するバイナリデータを文字列に変換可能です。

   ```sql
   insert into dbo.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, CONVERT(NVARCHAR(100), ${payload}) )
   ```

   :::

   SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL template**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を切り替えられます：

   - **無効**（デフォルト）：ルールエンジンは文字列`undefined`をデータベースに挿入します。

   - **有効**：変数未定義時に`NULL`を挿入可能にします。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**でSinkがMicrosoft SQL Serverに接続可能かテストできます。

12. **Create**をクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでMicrosoft SQL Server Sink用のルールが正常に作成されました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいMicrosoft SQL Server Sinkが表示されます。

また、**Integration** -> **Flow Designer**を開くとトポロジーを確認でき、トピック`t/#`配下のメッセージがルール`my_rule`で解析されMicrosoft SQL Serverに送信・保存されていることがわかります。

## イベント記録用Microsoft SQL Server Sinkのルール作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みSink経由でMicrosoft SQL Serverのテーブル`dbo.t_mqtt_events`に保存するルールの作成方法を示します。

手順は[メッセージ保存用Microsoft SQL Server Sinkのルール作成](#create-a-rule-with-microsoft-sql-server-sink-for-message-storage)とほぼ同様で、SQLテンプレートとSQL文のみ異なります。

オンライン／オフライン状態記録用のルールSQL文は以下の通りです。

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

Microsoft SQL Server Sinkの稼働状況を確認します。

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

- オンライン／オフライン状態記録用Sinkでは、新たに2件のイベント（クライアント接続および切断）が記録されているはずです。`dbo.t_mqtt_events`データテーブルに状態記録が書き込まれているか確認してください。

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
