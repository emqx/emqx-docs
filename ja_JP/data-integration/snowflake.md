# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/)は、クラウドベースのデータプラットフォームであり、高いスケーラビリティと柔軟性を備えたデータウェアハウジング、分析、セキュアなデータ共有のソリューションを提供します。構造化データおよび半構造化データの処理に優れており、大量のデータを格納しつつ高速なクエリ性能と様々なツールやサービスとのシームレスな統合を実現するよう設計されています。

本ページでは、EMQXとSnowflake間のデータ統合について詳細に解説し、ルールおよびSinkの作成方法について実践的なガイダンスを提供します。

## 動作概要

EMQXにおけるSnowflakeデータ統合は、複雑なビジネス開発に対応可能なすぐに使える機能として提供されています。典型的なIoTアプリケーションにおいて、EMQXはデバイス接続およびメッセージ送受信を担うIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、格納、分析を行うデータストレージおよび処理プラットフォームとして役割を果たします。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを活用してデバイスイベントやデータをSnowflakeへ転送します。エンドユーザーやアプリケーションはSnowflakeのテーブル内のデータにアクセス可能です。具体的なワークフローは以下の通りです。

1. **デバイスのEMQX接続**：IoTデバイスはMQTTプロトコルで正常に接続されるとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレス、その他のプロパティ情報が含まれます。
2. **デバイスのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュします。EMQXはメッセージを受信し、ルールエンジン内で照合します。
3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールにマッチしたメッセージやイベントは、データ形式変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
4. **Snowflakeへの書き込み**：ルールがトリガーされると、メッセージはSnowflakeのStageに書き込まれ、そこからSnowflakeテーブルにロードされます。

イベントおよびメッセージデータがSnowflakeに書き込まれた後は、以下のような多様なビジネスおよび技術的用途に活用可能です。

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの利用を保証します。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を行い、予知保全、運用インサイト、デバイス性能評価を実現します。

## 特長と利点

EMQXでSnowflakeデータ統合を利用することで、以下の特長と利点が得られます。

- **メッセージ変換**：メッセージはSnowflakeへの書き込み前にEMQXルールで高度な処理や変換が可能であり、後続の格納や利用を容易にします。
- **柔軟なデータ操作**：Snowflake Sinkは、Snowflakeに書き込む特定フィールドの選択を可能にし、ビジネスニーズに合わせた効率的かつ動的なストレージ構成を実現します。
- **統合されたビジネスプロセス**：Snowflake SinkによりデバイスデータをSnowflakeの豊富なエコシステムアプリケーションと連携でき、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージインフラは、従来のデータベースに比べて低コストで長期データ保存に最適なソリューションを提供し、大量のIoTデータの保存に適しています。

これらの特長により、効率的で信頼性が高くスケーラブルなIoTアプリケーションの構築と、ビジネス意思決定や最適化の恩恵を受けることが可能です。

## はじめに

ここでは、EMQXでSnowflake Sinkを作成する前の準備について説明します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し、効率的にデータ転送を行うためには、Snowflake Open Database Connectivity（ODBC）ドライバーのインストールと設定が必要です。これは通信の橋渡し役を担い、データが適切にフォーマットされ、認証され、転送されることを保証します。

詳細は公式の[ODBC Driver](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### Linux

EMQXはDebian系システム（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な展開と必要なシステム設定を行うための[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法の推奨ではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`スクリプトをローカルマシンにコピーし、`chmod a+x`で実行権限を付与してから、`sudo`で実行します。

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリに自動ダウンロードし、ドライバーをインストール後、以下のシステム設定ファイルを更新します。

- `/etc/odbc.ini`：Snowflakeデータソース設定を追加
- `/etc/odbcinst.ini`：Snowflakeドライバーのパスを登録

**設定例**

`/etc/odbc.ini`の設定内容を確認するには以下を実行します。

```
emqx@emqx-0:~$ cat /etc/odbc.ini 

[snowflake]
Description=SnowflakeDB
Driver=SnowflakeDSIIDriver
Locale=en-US
PORT=443
SSL=on

[ODBC Data Sources]
snowflake = SnowflakeDSIIDriver
```

`/etc/odbcinst.ini`の設定内容を確認するには以下を実行します。

```
emqx@emqx-0:~$ cat /etc/odbcinst.ini 

[ODBC Driver 18 for SQL Server]
Description=Microsoft ODBC Driver 18 for SQL Server
Driver=/opt/microsoft/msodbcsql18/lib64/libmsodbcsql-18.5.so.1.1
UsageCount=1

[ODBC Driver 17 for SQL Server]
Description=Microsoft ODBC Driver 17 for SQL Server
Driver=/opt/microsoft/msodbcsql17/lib64/libmsodbcsql-17.10.so.6.1
UsageCount=1

[SnowflakeDSIIDriver]
APILevel=1
ConnectFunctions=YYY
Description=Snowflake DSII
Driver=/usr/lib/snowflake/odbc/lib/libSnowflake.so
DriverODBCVer=03.52
SQLLevel=1
UsageCount=1
```

#### macOS

macOSでSnowflake ODBCドライバーをインストールおよび設定する手順は以下の通りです。

1. unixODBCをインストールします。例：

   ```
   brew install unixodbc
   ```

2. [iODBCをダウンロードしてインストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)します。

3. [Snowflake ODBCドライバーをダウンロードしてインストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)します。

4. 詳細なインストールおよび設定手順は[macOS向けODBCドライバーのインストールと設定](https://docs.snowflake.com/en/developer-guide/odbc/odbc-mac)を参照してください。

5. インストール後、以下の設定ファイルを更新します。

   - Snowflake ODBCドライバーの権限と設定を更新：

     ```bash
     chown $(id -u):$(id -g) /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     echo 'ODBCInstLib=libiodbcinst.dylib' >> /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     ```

   - ODBC接続設定のため、`~/.odbc.ini`ファイルを作成または更新：

     ```
     cat << EOF > ~/.odbc.ini
     [ODBC]
     Trace=no
     TraceFile=
     
     [ODBC Drivers]
     Snowflake = Installed
     
     [ODBC Data Sources]
     snowflake = Snowflake
     
     [Snowflake]
     Driver = /opt/snowflake/snowflakeodbc/lib/universal/libSnowflake.dylib
     EOF
     ```

### ユーザーアカウントとデータベースの作成

Snowflake ODBCドライバーのインストールが完了したら、データ取り込み用のユーザーアカウント、データベース、および関連リソースを設定する必要があります。以下の認証情報は後でEMQXのコネクターおよびSinkの設定に使用します。

| 項目名                   | 値                                               |
| ------------------------ | ------------------------------------------------ |
| データソース名（DSN）    | `snowflake`                                      |
| ユーザー名               | `snowpipeuser`                                   |
| パスワード               | `Snowpipeuser99`                                 |
| データベース名           | `testdatabase`                                   |
| スキーマ                 | `public`                                         |
| ステージ                 | `emqx`                                           |
| パイプ                   | `emqx`                                           |
| パイプユーザー           | `snowpipeuser`                                   |
| プライベートキー         | `file://<path to snowflake_rsa_key.private.pem>` |

#### RSA鍵ペアの生成

Snowflakeへの安全な接続のため、以下のコマンドで認証用のRSA鍵ペアを生成します。

```bash
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

詳細は[鍵ペア認証と鍵ペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLによるSnowflakeリソースの設定

ODBCドライバーの設定とRSA鍵ペアの生成が完了したら、Snowflakeのリソースを設定します。SQLコマンドを用いて必要なデータベース、テーブル、ステージ、パイプを作成します。

1. SnowflakeコンソールのSQLワークシートを開き、以下のSQLを実行してデータベース、テーブル、ステージ、パイプを作成します。

   ```sql
   USE ROLE accountadmin;
   
   CREATE DATABASE IF NOT EXISTS testdatabase;
   
   CREATE OR REPLACE TABLE testdatabase.public.emqx (
       clientid STRING,
       topic STRING,
       payload STRING,
       publish_received_at TIMESTAMP_LTZ
   );
   
   CREATE STAGE IF NOT EXISTS testdatabase.public.emqx
   FILE_FORMAT = (TYPE = CSV PARSE_HEADER = TRUE FIELD_OPTIONALLY_ENCLOSED_BY = '"')
   COPY_OPTIONS = (ON_ERROR = CONTINUE PURGE = TRUE);
   
   CREATE PIPE IF NOT EXISTS testdatabase.public.emqx AS
   COPY INTO testdatabase.public.emqx
   FROM @testdatabase.public.emqx
   MATCH_BY_COLUMN_NAME = CASE_INSENSITIVE;
   ```

2. 新しいユーザーを作成し、そのユーザーにRSA公開鍵を設定します。

   ```sql
   CREATE USER IF NOT EXISTS snowpipeuser
       PASSWORD = 'Snowpipeuser99'
       MUST_CHANGE_PASSWORD = FALSE;
   
   ALTER USER snowpipeuser SET RSA_PUBLIC_KEY = '
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_1>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_2>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_3>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_4>
   ';
   ```

   ::: tip

   PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`の行は削除し、改行を保持したまま残りの内容を含めてください。

   :::

3. 必要なロールを作成し、ユーザーに割り当てます。

   ```sql
   CREATE OR REPLACE ROLE snowpipe;
   
   GRANT USAGE ON DATABASE testdatabase TO ROLE snowpipe;
   GRANT USAGE ON SCHEMA testdatabase.public TO ROLE snowpipe;
   GRANT INSERT, SELECT ON testdatabase.public.emqx TO ROLE snowpipe;
   GRANT READ, WRITE ON STAGE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT ROLE snowpipe TO USER snowpipeuser;
   ALTER USER snowpipeuser SET DEFAULT_ROLE = snowpipe;
   ```

## コネクターの作成

Snowflake Sinkを追加する前に、EMQXでSnowflakeとの接続を確立するためのコネクターを作成する必要があります。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプとして **Snowflake** を選択し、次へ進みます。

4. コネクター名を入力します。英数字の大文字・小文字の組み合わせで、ここでは `my-snowflake` と入力します。

5. 接続情報を入力します。
   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切ったものを入力します。これはSnowflakeプラットフォームにアクセスするURLの一部で、Snowflakeコンソールで確認可能です。
   
   - **Server Host**：SnowflakeのエンドポイントURLで、通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式です。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はご自身のSnowflakeインスタンス固有のサブドメインに置き換えてください。
   
   - **データソース名（DSN）**：ODBCドライバー設定時に`.odbc.ini`ファイルで設定した`snowflake`を入力します。
   
   - **ユーザー名**：前述の設定で作成した`snowpipeuser`を入力します。
   
   - **パスワード**：ODBCのユーザー名/パスワード認証でSnowflakeに接続するためのパスワードです。任意入力項目です。
   
     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、
     - `/etc/odbc.ini`に設定するか、
     - 鍵ペア認証を使用する場合は空欄のままにします。
   
     ::: tip
   
     認証にはパスワードかプライベートキーのいずれか一方を使用してください。両方設定しない場合は、適切な認証情報が`/etc/odbc.ini`に設定されていることを確認してください。
   
     :::
   
   - **プライベートキーパス**：SnowflakeへのODBC認証に使用するRSA秘密鍵の絶対パスです。クラスターの全ノードで同一パスである必要があります。パスは`file://`で始まる必要があります。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`
   
   - **プライベートキーパスワード**：秘密鍵ファイルが暗号化されている場合の復号パスワードです。OpenSSLの`-nocrypt`オプションで生成した場合は空欄のままにします。
   
   - **プロキシ**：HTTPプロキシサーバー経由でSnowflakeに接続する場合の設定です。HTTPSプロキシはサポートされていません。デフォルトはプロキシ未使用です。プロキシ利用時は`Enable Proxy`を選択し、以下を入力します。
   
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス
     - **Proxy Port**：プロキシサーバーのポート番号
   
6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルスイッチをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがSnowflakeに接続可能かテストできます。

9. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、次にルールとSinkを作成してSnowflakeへのデータ書き込み方法を指定できます。

## Snowflake Sinkを用いたルールの作成

このセクションでは、EMQXでMQTTトピック`t/#`からのメッセージを処理し、処理結果を設定済みのSnowflake Sinkを通じてSnowflakeに書き込むルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. ルールIDに`my_rule`を入力し、SQLエディターに以下のルールSQLを入力します。

   ```sql
   SELECT
     clientid,
     unix_ts_to_rfc3339(publish_received_at, 'millisecond') as publish_received_at,
     topic,
     payload
   FROM
       "t/#"
   ```

   ::: tip

   SQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてルールSQLの学習やテストが可能です。

   :::
   ::: tip
   
   Snowflake連携では、選択するフィールドがSnowflakeで定義したテーブルのカラム数および名前と正確に一致することが重要です。余分なフィールドを追加したり`*`で全選択することは避けてください。
   
   :::

4. アクションを追加し、**Action Type**ドロップダウンから`Snowflake`を選択します。アクションドロップダウンはデフォルトの`create action`のままにするか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成してルールに追加します。

5. Sink名（例：`snowflake_sink`）と簡単な説明を入力します。

6. 先ほど作成した`my-snowflake`コネクターをコネクタードロップダウンから選択します。ドロップダウン横の作成ボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。必要な設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

7. 以下の設定を行います。

   - **データベース名**：`testdatabase`を入力。EMQXデータ格納用に作成したSnowflakeデータベースです。
   - **スキーマ**：`public`を入力。`testdatabase`内のデータテーブルが存在するスキーマです。
   - **ステージ**：`emqx`を入力。Snowflakeでデータをテーブルにロードする前に保持するステージです。
   - **パイプ**：`emqx`を入力。ステージからテーブルへのロード処理を自動化するパイプです。
   - **パイプユーザー**：`snowpipeuser`を入力。パイプ管理権限を持つSnowflakeユーザーです。
   - **プライベートキー**：RSA秘密鍵のパス（例：`file://<path to snowflake_rsa_key.private.pem>`）またはRSA秘密鍵ファイルの内容を入力します。これは安全な認証に必要で、クラスタの全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。

8. **アップロードモード**を選択します。現在は`Aggregated Upload`のみサポートされています。この方式は複数のルールトリガー結果を単一ファイル（例：CSVファイル）にまとめてSnowflakeにアップロードし、ファイル数を減らして書き込み効率を向上させます。

9. **集約タイプ**を選択します。現在は`csv`のみサポートされています。データはカンマ区切りのCSV形式でSnowflakeにステージされます。

   - **カラム順序**：ドロップダウンからカラムの並び順を選択します。生成されるCSVファイルは選択したカラム順にソートされ、未選択カラムはアルファベット順に並びます。

   - **最大レコード数**：集約をトリガーする最大レコード数を設定します。例えば`1000`に設定すると、1000件のレコード収集後にアップロードされます。最大レコード数に達すると単一ファイルの集約が完了しアップロードされ、時間間隔がリセットされます。

   - **時間間隔**：集約を行う時間間隔（秒）を設定します。例えば`60`に設定すると、最大レコード数に達していなくても60秒ごとにデータがアップロードされ、最大レコード数がリセットされます。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。プライマリSinkがメッセージ処理に失敗した場合にこれらのアクションがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **詳細設定**を展開し、必要に応じて高度な設定オプションを構成します（任意）。詳細は[詳細設定](#advanced-settings)を参照してください。

12. 残りの設定はデフォルト値のままにし、**Create**ボタンをクリックしてSink作成を完了します。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

13. ルール作成画面にて、**Create**ボタンをクリックしてルール作成全体を完了します。

これでルールの作成が完了しました。**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブに新規Snowflake Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認可能です。トポロジーは、トピック`t/#`のメッセージがルール`my_rule`で解析され、Snowflakeに書き込まれる流れを視覚的に示します。

## ルールのテスト

ここでは設定したルールのテスト方法を示します。

### テストメッセージのパブリッシュ

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返してテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeにデータが正常に書き込まれたかを確認します。

1. SnowflakeのWebインターフェースを開き、認証情報でSnowflakeコンソールにログインします。

2. Snowflakeコンソールで以下のSQLクエリを実行し、ルールによって書き込まれた`emqx`テーブルのデータを表示します。

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   これにより、`emqx`テーブルにアップロードされたすべてのレコードが表示され、`clientid`、`topic`、`payload`、`publish_received_at`フィールドを確認できます。

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）やトピック、タイムスタンプなどのメタデータが表示されるはずです。

## 詳細設定

このセクションでは、Snowflake Sinkの詳細設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、用途に応じて以下のパラメータを調整可能です。

| 項目名                   | 説明                                                         | デフォルト値   |
| ------------------------ | ------------------------------------------------------------ | -------------- |
| **最大リトライ回数**     | アップロード失敗時の最大リトライ回数を設定します。例：`3`で3回までリトライ可能。 | `3`            |
| **バッファプールサイズ** | EMQXとSnowflake間のデータフローを管理するバッファワーカープロセスの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化しスムーズなデータ伝送を実現します。 | `16`           |
| **リクエストTTL**        | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこのTTLを超えてバッファ内にあるか、送信後にSnowflakeからの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 |                |
| **ヘルスチェック間隔**   | Snowflakeとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`           |
| **最大バッファキューサイズ** | Snowflake Sinkの各バッファワーカーが一時的に保持可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、効率的にデータストリームを処理します。システム性能やデータ伝送要件に応じて調整してください。 | `256`          |
| **クエリモード**          | `synchronous`（同期）または`asynchronous`（非同期）モードを選択し、メッセージ伝送を最適化します。非同期モードではSnowflakeへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがメッセージをSnowflake到着前に受信する可能性があります。 | `Asynchronous` |
| **バッチサイズ**          | EMQXからSnowflakeへ一度に転送するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送の効率と性能を最適化可能です。<br />`1`に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1`            |
| **インフライトウィンドウ** | 送信済みだが応答やアックがまだ得られていない「インフライト」状態のリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合に特に重要で、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`          |
| **接続タイムアウト**     | Snowflakeへの接続確立を待つ最大時間（秒）を指定します。例：`30`秒。タイムアウト時はリトライ（最大リトライ回数に基づく）またはエラーを返します。ネットワークレイテンシや接続信頼性管理に有効です。 | `15`           |
| **HTTPパイプライニング** | 応答待ちをせずに送信可能なHTTPリクエストの最大数を指定します。 | `100`          |
| **接続プールサイズ**     | EMQXがSnowflakeに対して同時に維持可能な接続数を定義します。大きいほど高負荷時に多くの同時リクエストを処理可能ですが、システムリソース消費も増加します。 | `8`            |
