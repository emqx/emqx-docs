# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/) は、クラウドベースのデータプラットフォームであり、高いスケーラビリティと柔軟性を備えたデータウェアハウジング、分析、および安全なデータ共有のソリューションを提供します。構造化データおよび半構造化データの処理に優れており、大量のデータを高速なクエリ性能で保存し、さまざまなツールやサービスとのシームレスな統合を実現するよう設計されています。

本ページでは、EMQXとSnowflake間のデータ統合について詳細に紹介し、ルールおよびSinkの作成方法について実践的なガイダンスを提供します。

## 動作概要

EMQXにおけるSnowflakeデータ統合はすぐに利用可能な機能であり、複雑なビジネス開発にも簡単に設定できます。典型的なIoTアプリケーションでは、EMQXがデバイス接続およびメッセージ送信を担当するIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、保存、分析を行うデータストレージおよび処理プラットフォームとして動作します。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブル内のデータにアクセスできます。具体的なワークフローは以下の通りです：

1. **デバイスのEMQXへの接続**：IoTデバイスはMQTTプロトコルで正常に接続されるとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレス、その他のプロパティ情報が含まれます。
2. **デバイスメッセージのパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリおよびステータスデータをパブリッシュします。EMQXはメッセージを受信し、ルールエンジン内で比較処理を行います。
3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールをマッチングし、データフォーマット変換、特定情報のフィルタリング、メッセージへのコンテキスト情報付加などの処理を行います。
4. **Snowflakeへの書き込み**：ルールはメッセージをSnowflakeのStageに書き込み、そこからSnowflakeテーブルにロードするアクションをトリガーします。

イベントおよびメッセージデータがSnowflakeに書き込まれた後、以下のようなビジネスおよび技術目的で利用可能です：

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの利用を保証します。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を行い、予知保全、運用インサイト、デバイス性能評価を実現します。

## 特徴と利点

EMQXでSnowflakeデータ統合を利用することで、以下の特徴と利点が得られます：

- **メッセージ変換**：メッセージはSnowflakeに書き込まれる前にEMQXルール内で大規模な処理や変換が可能であり、後続の保存や利用を容易にします。
- **柔軟なデータ操作**：Snowflake Sinkは、書き込むフィールドを選択可能であり、ビジネスニーズに応じた効率的かつ動的なストレージ構成を実現します。
- **統合されたビジネスプロセス**：Snowflake Sinkにより、デバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせることができ、データ分析やアーカイブなど多様なビジネスシナリオを可能にします。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージインフラは、従来のデータベースよりも低コストで長期データ保持に最適なソリューションを提供し、大量のIoTデータ保存に適しています。

これらの特徴により、効率的で信頼性が高くスケーラブルなIoTアプリケーションを構築し、ビジネスの意思決定や最適化に役立てることができます。

## はじめる前に

ここでは、EMQXでSnowflake Sinkを作成する前の準備について説明します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送を行うためには、SnowflakeのOpen Database Connectivity（ODBC）ドライバーをインストールおよび設定する必要があります。これは通信の橋渡し役となり、データの適切なフォーマット、認証、転送を保証します。

詳細は公式の[ODBC Driver](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### Linux

EMQXはDebian系（Ubuntuなど）システム向けにSnowflake ODBCドライバーの迅速な展開と必要なシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法の推奨ではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`スクリプトをローカルマシンにコピーし、`chmod a+x`で実行権限を付与してから、`sudo`で実行します：

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリに自動ダウンロードし、ドライバーをインストール、以下のシステム設定ファイルを更新します：

- `/etc/odbc.ini`：Snowflakeデータソース設定を追加
- `/etc/odbcinst.ini`：Snowflakeドライバーパスを登録

**設定例**

`/etc/odbc.ini`の設定を確認するコマンド：

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

`/etc/odbcinst.ini`の設定を確認するコマンド：

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

macOSでSnowflake ODBCドライバーをインストールおよび設定する手順は以下の通りです：

1. unixODBCをインストールします。例：

   ```
   brew install unixodbc
   ```

2. [iODBCをダウンロードしてインストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)します。

3. [Snowflake ODBCドライバーをダウンロードしてインストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)します。

4. 詳細なインストールおよび設定手順は[macOS向けODBCドライバーのインストールと設定](https://docs.snowflake.com/en/developer-guide/odbc/odbc-mac)を参照してください。

5. インストール後、以下の設定ファイルを更新します：

   - Snowflake ODBCドライバーの権限と設定を更新：

     ```bash
     chown $(id -u):$(id -g) /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     echo 'ODBCInstLib=libiodbcinst.dylib' >> /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     ```

   - ODBC接続設定のために`~/.odbc.ini`ファイルを作成または更新：

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

Snowflake ODBCドライバーのインストール後、データ取り込み用のユーザーアカウント、データベース、および関連リソースを設定する必要があります。以下の認証情報は後でEMQXのコネクターおよびSinkの設定時に必要となります：

| 項目                   | 値                                               |
| ---------------------- | ------------------------------------------------ |
| データソース名（DSN）  | `snowflake`                                      |
| ユーザー名             | `snowpipeuser`                                   |
| パスワード             | `Snowpipeuser99`                                 |
| データベース名         | `testdatabase`                                   |
| スキーマ               | `public`                                         |
| ステージ               | `emqx`                                           |
| パイプ                 | `emqx`                                           |
| パイプユーザー         | `snowpipeuser`                                   |
| プライベートキー       | `file://<path to snowflake_rsa_key.private.pem>` |

#### RSA鍵ペアの生成

Snowflakeへの安全な接続のため、以下のコマンドでRSA鍵ペアを生成します：

```bash
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

詳細は[鍵ペア認証と鍵ペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLによるSnowflakeリソースの設定

ODBCドライバーの設定とRSA鍵ペアの生成が完了したら、SnowflakeコンソールのSQLワークシートで以下のSQLコマンドを実行し、データベース、テーブル、ステージ、パイプを作成します：

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

次に、新規ユーザーを作成し、そのユーザーにRSA公開鍵を設定します：

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

PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`の行は削除し、残りの内容を改行を保持したまま記載してください。

:::

最後に、Snowflakeリソース管理用のロールを作成し、ユーザーに割り当てます：

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

4. コネクター名を入力します。英数字の組み合わせで、ここでは `my-snowflake` と入力します。

5. 接続情報を入力します。
   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切って入力します。これはSnowflakeプラットフォームにアクセスするURLの一部で、Snowflakeコンソールで確認できます。
   
   - **Server Host**：SnowflakeのエンドポイントURLで、通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式です。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はご自身のSnowflakeインスタンスに合わせて置き換えてください。
   
   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`ファイルで設定した`snowflake`を入力します。
   
   - **Username**：前述の設定で作成した`snowpipeuser`を入力します。

   - **Password**：ODBC経由でユーザー名/パスワード認証を行う場合のパスワードです。任意入力です：

     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、
     - `/etc/odbc.ini`に設定するか、
     - 鍵ペア認証を使用する場合は空欄のままにします。

     ::: tip

     認証にはパスワードまたはプライベートキーのいずれかを使用してください。両方を設定しないでください。どちらも設定しない場合は、適切な認証情報が`/etc/odbc.ini`に設定されていることを確認してください。

     :::

   - **Private Key Path**：ODBC経由で鍵ペア認証を行うためのプライベートRSA鍵の絶対ファイルパスを入力します。クラスタの全ノードで同一パスである必要があります。パスは`file://`で始まる必要があります。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`
   
   - **Private Key Password**：プライベートRSA鍵ファイルが暗号化されている場合の復号パスワードです。OpenSSLの`-nocrypt`オプションで生成した鍵の場合は空欄のままにします。
   
   - **Proxy**：HTTPプロキシサーバー経由でSnowflakeに接続する場合の設定です。HTTPSプロキシはサポートされていません。デフォルトはプロキシなしです。プロキシを有効にするには`Enable Proxy`を選択し、以下を入力します：
   
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス
     - **Proxy Port**：プロキシサーバーのポート番号
   
6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルスイッチをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。

7. 高度な設定（任意）：[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがSnowflakeに接続できるかテストできます。

9. 最後に、画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクターの作成が完了し、次にルールとSinkを作成してSnowflakeへのデータ書き込み方法を指定できます。

## Snowflake Sinkを使ったルールの作成

ここでは、EMQXでMQTTのソーストピック `t/#` からメッセージを処理し、処理結果を設定済みのSnowflake Sinkを通じてSnowflakeに書き込むルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. ルールIDに `my_rule` を入力し、SQLエディタに以下のルールSQLを入力します：

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

   Snowflake連携では、選択するフィールドはSnowflakeのテーブルのカラム数および名前と完全に一致させる必要があります。余分なフィールドを追加したり`*`で全選択するのは避けてください。

   :::

4. アクションを追加し、**Action Type**のドロップダウンから`Snowflake`を選択します。アクションのドロップダウンはデフォルトの`create action`のままにするか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成してルールに追加します。

5. Sinkの名前（例：`snowflake_sink`）と簡単な説明を入力します。

6. 先に作成した`my-snowflake`コネクターをコネクタードロップダウンから選択します。ドロップダウン横の作成ボタンからポップアップで新規コネクター作成も可能です。必要な設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

7. 以下の設定を行います：

   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース名。
   - **Schema**：`public`。`testdatabase`内のデータテーブルが存在するスキーマ名。
   - **Stage**：`emqx`。Snowflakeでデータをテーブルにロードする前に保持するステージ名。
   - **Pipe**：`emqx`。ステージからテーブルへのロード処理を自動化するパイプ名。
   - **Pipe User**：`snowpipeuser`。パイプ管理権限を持つSnowflakeユーザー名。
   - **Private Key**：プライベートRSA鍵のパス（例：`file://<path to snowflake_rsa_key.private.pem>`）またはRSAプライベート鍵ファイルの内容。安全な認証に使用され、Snowflakeパイプへの安全なアクセスに必要です。ファイルパスを使用する場合はクラスタ全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。

8. **Upload Mode**を選択します。現在は`Aggregated Upload`のみサポートされています。この方式は複数のルールトリガー結果を1つのファイル（例：CSVファイル）にまとめてSnowflakeにアップロードし、ファイル数を減らし書き込み効率を向上させます。

9. **Aggregation Type**を選択します。現在は`csv`のみサポートされており、データはカンマ区切りのCSV形式でSnowflakeにステージされます。

   - **Column Order**：ドロップダウンから列の並び順を選択します。生成されるCSVファイルは選択した列を優先的にソートし、選択されていない列はアルファベット順にソートされます。

   - **Max Records**：集約がトリガーされる最大レコード数を設定します。例：`1000`に設定すると1000レコード収集後にアップロードされます。最大レコード数に達すると単一ファイルの集約が完了しアップロードされ、時間間隔がリセットされます。

   - **Time Interval**：集約が行われる時間間隔（秒）を設定します。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにデータがアップロードされ、最大レコード数がリセットされます。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **Advanced Settings**を展開し、必要に応じて高度な設定を行います（任意）。詳細は[Advanced Settings](#advanced-settings)を参照してください。

12. 残りの設定はデフォルト値のままにし、**Create**ボタンをクリックしてSink作成を完了します。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

13. ルール作成画面で**Create**ボタンをクリックし、ルール作成全体を完了します。

これでルールの作成が完了しました。**Rules**ページで新規作成したルールを確認でき、**Actions (Sink)**タブで新しいSnowflake Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析され、Snowflakeに書き込まれる流れを視覚的に確認できます。

## ルールのテスト

ここでは設定したルールのテスト方法を説明します。

### テストメッセージのパブリッシュ

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返して複数のテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeにデータが正常に書き込まれたかを以下の手順で確認できます。

1. SnowflakeのWebインターフェースにログインします。

2. Snowflakeコンソールで以下のSQLクエリを実行し、ルールで書き込まれた`emqx`テーブルのデータを表示します：

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）およびトピックやタイムスタンプなどのメタデータが表示されるはずです。

## 高度な設定

ここではSnowflake Sinkで利用可能な高度な設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、必要に応じて以下のパラメータを調整できます。

| 項目名                   | 説明                                                                                          | デフォルト値  |
| ------------------------ | --------------------------------------------------------------------------------------------- | ------------ |
| **Max Retries**           | アップロード失敗時の最大リトライ回数を設定します。例：`3`で3回までリトライ可能。                   | `3`          |
| **Buffer Pool Size**      | EMQXとSnowflake間のデータフローを管理するバッファワーカープロセス数を指定します。これらのワーカーはデータを一時的に保持・処理し、性能最適化とスムーズなデータ転送に重要です。 | `16`         |
| **Request TTL**           | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。TTLを超えるか、Snowflakeからの応答やアックがタイムリーに得られない場合、リクエストは期限切れと判断されます。 |              |
| **Health Check Interval** | Snowflakeとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。                          | `15`         |
| **Max Buffer Queue Size** | Snowflake Sinkの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保持し、効率的なデータストリーム処理を担います。システム性能やデータ転送要件に応じて調整してください。 | `256`        |
| **Query Mode**            | 同期（`synchronous`）または非同期（`asynchronous`）のリクエストモードを選択できます。非同期モードではSnowflakeへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受信する可能性があります。 | `Asynchronous` |
| **Batch Size**            | EMQXからSnowflakeへ一度に送信するデータバッチの最大サイズを指定します。サイズ調整により転送効率や性能を最適化可能です。<br />`1`に設定すると、データはバッチ化せず個別に送信されます。 | `1`          |
| **Inflight Window**       | 送信済みで応答やアックをまだ受け取っていない「インフライト」キューリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合、同一MQTTクライアントからのメッセージを厳密に順次処理したい場合はこの値を`1`に設定してください。 | `100`        |
| **Connect Timeout**       | Snowflakeへの接続確立を待つ最大時間（秒）を指定します。例：`30`秒。タイムアウト時はリトライ（Max Retriesに基づく）またはエラーを返します。ネットワークレイテンシや接続信頼性管理に有用です。 | `15`         |
| **HTTP Pipelining**       | 応答待ちをせずに送信可能なHTTPリクエストの最大数を指定します。                                   | `100`        |
| **Connection Pool Size**  | EMQXがSnowflakeに同時に維持できる接続数を定義します。大きいほど同時リクエスト数が増え高負荷に対応可能ですが、システムリソース消費も増加します。 | `8`          |
