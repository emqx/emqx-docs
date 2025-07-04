# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/)は、クラウドベースのデータプラットフォームであり、データウェアハウジング、分析、および安全なデータ共有に対して高いスケーラビリティと柔軟性を提供します。構造化データおよび半構造化データの処理に優れており、大量のデータを高速なクエリ性能で保存し、さまざまなツールやサービスとのシームレスな統合を実現するよう設計されています。

本ページでは、EMQXとSnowflake間のデータ統合について詳細に紹介し、ルールおよびSinkの作成に関する実践的なガイドを提供します。

## 動作概要

EMQXにおけるSnowflakeデータ統合はすぐに使える機能であり、複雑なビジネス開発にも簡単に設定可能です。典型的なIoTアプリケーションでは、EMQXがデバイス接続とメッセージ送信を担うIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、保存、分析を行うデータストレージおよび処理プラットフォームとして利用されます。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブルに格納されたデータにアクセス可能です。具体的なワークフローは以下の通りです。

1. **デバイスのEMQXへの接続**：IoTデバイスはMQTTプロトコルで正常に接続されるとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレス、その他のプロパティ情報が含まれます。
2. **デバイスのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリおよびステータスデータをパブリッシュします。EMQXはメッセージを受信し、ルールエンジン内で比較処理を行います。
3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールにマッチしたメッセージやイベントに対して、データ形式変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
4. **Snowflakeへの書き込み**：ルールはSnowflake Stageへのメッセージ書き込みアクションをトリガーし、Snowflakeテーブルにロードします。

イベントおよびメッセージデータがSnowflakeに書き込まれた後、以下のようなビジネスおよび技術的用途で活用可能です。

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの利用を保証します。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を行い、予知保全、運用インサイト、デバイス性能評価を実現します。

## 特長とメリット

EMQXにおけるSnowflakeデータ統合を利用することで、以下の特長とメリットが得られます。

- **メッセージ変換**：メッセージはSnowflakeへの書き込み前にEMQXルール内で高度な処理・変換が可能であり、その後の保存や利用を容易にします。
- **柔軟なデータ操作**：Snowflake Sinkは書き込むフィールドを選択可能で、ビジネスニーズに応じた効率的かつ動的なストレージ構成を実現します。
- **統合されたビジネスプロセス**：Snowflake SinkによりデバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせ、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は従来のデータベースに比べ低コストで長期データ保持に最適であり、大量のIoTデータ保存に適しています。

これらの特長により、効率的で信頼性が高くスケーラブルなIoTアプリケーション構築と、ビジネス意思決定や最適化に貢献します。

## はじめる前に

本節では、EMQXでSnowflake Sinkを作成する前の準備について説明します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送を行うためには、SnowflakeのOpen Database Connectivity（ODBC）ドライバーをインストールおよび設定する必要があります。これは通信の橋渡し役を担い、データの正しいフォーマット化、認証、転送を保証します。

詳細は公式の[ODBC Driver](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### Linux

EMQXはDebian系システム（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な導入と必要なシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト目的のものであり、本番環境でのODBCドライバー設定方法の推奨ではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`スクリプトをローカルにコピーし、`chmod a+x`で実行権限を付与後、`sudo`で実行します。

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリに自動ダウンロードし、ドライバーをインストール、以下のシステム設定ファイルを更新します。

- `/etc/odbc.ini`：Snowflakeデータソース設定を追加
- `/etc/odbcinst.ini`：Snowflakeドライバーのパスを登録

**設定例**

以下のコマンドで`/etc/odbc.ini`の設定を確認できます。

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

以下のコマンドで`/etc/odbcinst.ini`の設定を確認できます。

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

1. unixODBCをインストール（例）：

   ```
   brew install unixodbc
   ```

2. [iODBCのダウンロードとインストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)。

3. [Snowflake ODBCドライバーのダウンロードとインストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)。

4. 詳細なインストールおよび設定手順は[macOS向けODBCドライバーのインストールと設定](https://docs.snowflake.com/en/developer-guide/odbc/odbc-mac)を参照。

5. インストール後、以下の設定ファイルを更新：

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

Snowflake ODBCドライバーのインストール後、データ取り込み用のユーザーアカウント、データベース、関連リソースを設定する必要があります。これらの認証情報は後でEMQXのコネクターおよびSink設定時に使用します。

| 項目                    | 値                                               |
| ----------------------- | ------------------------------------------------ |
| データソース名 (DSN)    | `snowflake`                                      |
| ユーザー名              | `snowpipeuser`                                   |
| パスワード              | `Snowpipeuser99`                                 |
| データベース名          | `testdatabase`                                   |
| スキーマ                | `public`                                         |
| ステージ                | `emqx`                                           |
| パイプ                  | `emqx`                                           |
| パイプユーザー          | `snowpipeuser`                                   |
| プライベートキー        | `file://<path to snowflake_rsa_key.private.pem>` |

#### RSA鍵ペアの生成

Snowflakeへの安全な接続のため、以下のコマンドでRSA鍵ペアを生成します。

```bash
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

詳細は[鍵ペア認証と鍵ペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLによるSnowflakeリソースの設定

ODBCドライバーの設定とRSA鍵ペアの生成が完了したら、SQLコマンドでSnowflakeのリソース（データベース、テーブル、ステージ、パイプ）を作成します。

1. SnowflakeコンソールでSQLワークシートを開き、以下のSQLを実行してデータベース、テーブル、ステージ、パイプを作成します。

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

2. 新規ユーザーを作成し、そのユーザーにRSA公開鍵を設定します。

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

   PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`の行は削除し、残りの内容は改行を保持して含めてください。

   :::

3. Snowflakeリソース管理用のロールを作成し、ユーザーに割り当てます。

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

4. コネクター名を英数字の組み合わせで入力します。ここでは `my-snowflake` と入力します。

5. 接続情報を入力します。
   - **Server Host**：SnowflakeのエンドポイントURLで、通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式です。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換えてください。
   
   - **Data Source Name(DSN)**：ODBCドライバー設定時に`.odbc.ini`ファイルで設定した`snowflake`を入力します。
   
   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切って入力します。これはSnowflakeプラットフォームへのアクセスURLの一部で、Snowflakeコンソールで確認可能です。
   
   - **Username**：前述の設定で作成した`snowpipeuser`を入力します。
   
   - **Password**：ODBC経由でのユーザー名/パスワード認証に使用するパスワードです。任意入力項目です。
   
     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、
     - `/etc/odbc.ini`で設定するか、
     - キーペア認証を使用する場合は空欄のままにします。
   
     ::: tip
   
     認証にはパスワードかプライベートキーのいずれか一方を使用してください。両方同時には設定しないでください。ここでどちらも設定しない場合は、適切な認証情報が`/etc/odbc.ini`に設定されていることを確認してください。
   
     :::
   
   - **Proxy**：HTTPプロキシサーバー経由でSnowflakeに接続する場合の設定です。HTTPSプロキシはサポートされていません。デフォルトではプロキシは使用しません。プロキシを有効にするには`Enable Proxy`を選択し、以下を入力します。
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス
     - **Proxy Port**：プロキシサーバーのポート番号
   - **Private Key Path**：ODBC経由でSnowflake認証に使用するRSA秘密鍵の絶対ファイルパスです。クラスターの全ノードで同一パスである必要があります。パスは`file://`で始まる必要があります（例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`）。
   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワードです。OpenSSLの`-nocrypt`オプションで生成した暗号化なしの鍵の場合は空欄にします。

6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルスイッチをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがSnowflakeに接続できるかテストできます。

9. 下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、次にルールとSinkを作成してデータのSnowflakeへの書き込み方法を指定できます。

## Snowflake Sinkを用いたルールの作成

本節では、EMQXでソースMQTTトピック`t/#`からのメッセージを処理し、処理結果を設定済みのSnowflake Sinkを通じてSnowflakeに書き込むルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. ルールIDに `my_rule` を入力し、SQLエディターに以下のルールSQLを入力します。

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
   
   Snowflake連携では、選択するフィールドがSnowflakeで定義したテーブルの列数および名前と完全に一致することが重要です。余分なフィールドの追加や`*`による全選択は避けてください。
   
   :::

4. アクションを追加し、**Action Type**のドロップダウンから`Snowflake`を選択します。アクションドロップダウンはデフォルトの`create action`のままにするか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成してルールに追加します。

5. Sinkの名前（例：`snowflake_sink`）と簡単な説明を入力します。

6. コネクターのドロップダウンから先ほど作成した`my-snowflake`を選択します。ドロップダウン横の作成ボタンを押すとポップアップで新規コネクター作成も可能です。必要な設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

7. 以下の設定を行います。

   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース名です。
   - **Schema**：`public`。`testdatabase`内のデータテーブルがあるスキーマ名です。
   - **Stage**：`emqx`。Snowflakeでデータをテーブルにロードする前に保持するステージ名です。
   - **Pipe**：`emqx`。ステージからテーブルへのロードを自動化するパイプ名です。
   - **Pipe User**：`snowpipeuser`。パイプ管理権限を持つSnowflakeユーザー名です。
   - **Private Key**：RSA秘密鍵のパス（例：`file://<path to snowflake_rsa_key.private.pem>`）またはRSA秘密鍵ファイルの内容を入力します。これはSnowflakeパイプへの安全なアクセスに必要な認証鍵です。ファイルパスを使用する場合は、クラスター全ノードで同一かつEMQXアプリケーションユーザーがアクセス可能である必要があります。

8. **Upload Mode**を選択します。現在は`Aggregated Upload`のみサポートされています。この方式は複数のルールトリガー結果を1つのファイル（例：CSVファイル）にまとめてSnowflakeにアップロードし、ファイル数を減らして書き込み効率を向上させます。

9. **Aggregation Type**を選択します。現在は`csv`のみサポートされており、データはカンマ区切りCSV形式でSnowflakeにステージングされます。

   - **Column Order**：ドロップダウンから列の並び順を選択します。生成されるCSVファイルは選択された列順に並び、未選択列はアルファベット順に続きます。

   - **Max Records**：集約がトリガーされる最大レコード数を設定します。例：`1000`と設定すると1000レコード収集後にアップロードされ、時間間隔がリセットされます。

   - **Time Interval**：集約が行われる時間間隔（秒）を設定します。例：`60`と設定すると最大レコード数に達していなくても60秒ごとにデータがアップロードされ、最大レコード数がリセットされます。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **詳細設定**を展開し、必要に応じて高度な設定オプションを構成します（任意）。詳細は[詳細設定](#advanced-settings)を参照してください。

12. 残りの設定はデフォルト値のままにし、**Create**ボタンをクリックしてSink作成を完了します。作成成功後、ルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

13. ルール作成画面で**Create**ボタンをクリックし、ルール作成全体を完了します。

これでルールの作成が完了しました。**Rules**ページで新規作成したルールを確認でき、**Actions (Sink)**タブで新しいSnowflake Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示されます。トポロジーはトピック`t/#`のメッセージがルール`my_rule`で解析され、Snowflakeに書き込まれる様子を視覚的に示します。

## ルールのテスト

本節では設定したルールのテスト方法を示します。

### テストメッセージのパブリッシュ

MQTTXを使い、トピック`t/1`にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返して複数のテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeにデータが正常に書き込まれたかをSnowflakeインスタンスにアクセスして確認します。

1. SnowflakeのWebインターフェースを開き、認証情報でSnowflakeコンソールにログインします。

2. Snowflakeコンソールで以下のSQLクエリを実行し、ルールによって書き込まれた`emqx`テーブルのデータを確認します。

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   これにより、`clientid`、`topic`、`payload`、`publish_received_at`フィールドを含む`emqx`テーブルにアップロードされた全レコードが表示されます。

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）やトピック、タイムスタンプなどのメタデータが確認できるはずです。

## 詳細設定

本節ではSnowflake Sinkの詳細設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、以下のパラメータをニーズに応じて調整可能です。

| 項目名                   | 説明                                                                 | デフォルト値  |
| ------------------------ | -------------------------------------------------------------------- | ------------ |
| **Max Retries**           | アップロード失敗時の最大リトライ回数を設定します。例：`3`で3回までリトライ可能。 | `3`          |
| **Buffer Pool Size**      | バッファワーカープロセスの数を指定します。これらのワーカーはEMQXとSnowflake間のデータフローを管理し、一時的にデータを保存・処理して送信します。パフォーマンス最適化とスムーズなデータ送信に重要です。 | `16`         |
| **Request TTL**           | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。TTLを超えるか、送信後にSnowflakeからの応答やアックがタイムリーに得られない場合、リクエストは期限切れと判断されます。 |              |
| **Health Check Interval** | SinkがSnowflakeとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`         |
| **Max Buffer Queue Size** | 各バッファワーカーがSnowflake Sinkでバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、効率的なデータストリーム処理を担います。システム性能やデータ送信要件に応じて調整してください。 | `256`        |
| **Query Mode**            | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードではSnowflakeへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがSnowflake到着前にメッセージを受信する可能性があります。 | `Asynchronous` |
| **Batch Size**            | EMQXからSnowflakeへ一度に送信するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送効率と性能を微調整可能です。<br />`1`に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1`          |
| **Inflight Window**       | 送信済みで応答やアックをまだ受け取っていない「インフライトキューリクエスト」の最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合、このパラメータは特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`        |
| **Connect Timeout**       | Snowflakeへの接続試行がタイムアウトするまでの待機時間（秒）を指定します。例：`30`秒。接続できない場合は**Max Retries**に基づきリトライまたはエラーを返します。ネットワークレイテンシや接続信頼性管理に有用です。 | `15`         |
| **HTTP Pipelining**       | 応答待ち前に送信可能なHTTPリクエストの最大数を指定します。 | `100`        |
| **Connection Pool Size**  | EMQXがSnowflakeに同時に維持可能な接続数を定義します。大きいほど同時リクエスト数が増え高負荷時に有利ですが、システムリソース消費も増加します。 | `8`          |
