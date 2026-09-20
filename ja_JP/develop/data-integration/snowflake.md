# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/)は、クラウドベースのデータプラットフォームであり、データウェアハウジング、分析、および安全なデータ共有のための高いスケーラビリティと柔軟性を提供します。構造化データおよび半構造化データの処理能力で知られ、膨大なデータを保存しつつ高速なクエリ性能とさまざまなツールやサービスとのシームレスな統合を実現するよう設計されています。

本ページでは、EMQXとSnowflake間のデータ統合について詳しく紹介し、ルールおよびSinkの作成方法について実践的なガイドを提供します。

## 動作概要

EMQXにおけるSnowflakeデータ統合はすぐに使える機能で、複雑なIoTビジネスワークフローを簡単にサポートできるように構成可能です。典型的なIoTアプリケーションでは、EMQXがデバイス接続とメッセージ送信を担うIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、保存、分析を行うデータストレージおよび処理プラットフォームとして機能します。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスのイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブル内のデータにアクセス可能です。具体的なワークフローは以下の通りです。

1. **デバイスのEMQXへの接続**：IoTデバイスはMQTTプロトコルで正常に接続されるとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレス、その他識別情報が含まれます。

2. **デバイスのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジン内で比較処理を行います。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールとマッチし、データ形式変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。

4. **Snowflakeへの書き込み**：ルールはメッセージデータをSnowflakeに書き込むアクションをトリガーします。メッセージをファイルにバッチングしStageとPipe経由でロードする方法（集約モード）か、Snowpipe Streaming APIを使って直接ストリーミングする方法（ストリーミングモード）があります。

イベントやメッセージデータがSnowflakeに書き込まれた後は、以下のようなビジネスおよび技術目的で活用可能です。

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの利用を保証。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を実施。予知保全、運用インサイト、デバイス性能評価などを実現。

## 特長と利点

EMQXのSnowflakeデータ統合を利用することで、以下の特長と利点をビジネスにもたらします。

- **メッセージ変換**：メッセージはSnowflakeへの書き込み前にEMQXルール内で高度な処理や変換が可能で、後続の保存や利用を容易にします。
- **柔軟なデータ操作**：Snowflake Sinkは書き込むフィールドを選択可能で、ビジネスニーズに応じた効率的かつ動的なストレージ構成を実現します。
- **統合されたビジネスプロセス**：Snowflake SinkによりデバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせ、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は従来のデータベースより低コストで長期データ保持に最適で、大量のIoTデータ保存に適しています。

これらの特長により、効率的で信頼性が高くスケーラブルなIoTアプリケーションを構築し、ビジネスの意思決定や最適化に役立てられます。

## はじめる前に

このセクションでは、EMQXでSnowflake Sinkを作成する前に必要な準備について説明します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の概念理解
- 管理者権限を持つ稼働中のSnowflakeアカウント

### アップロードモードの選択

::: tip

モードを最初に選択してください。これによりEMQXとSnowflake環境の両方の設定方法が決まります。

:::

EMQXはSnowflakeへのデータ送信に以下の2つのモードをサポートします。

| モード       | 説明                                                         | ODBC必要性    |
| ---------- | ------------------------------------------------------------ | ------------- |
| 集約（Aggregated） | EMQXはMQTTメッセージをローカルファイルにバッファリングし、Snowflakeのステージにアップロードします。`COPY INTO`文を設定したパイプが自動的にステージファイルをターゲットテーブルにロードします。詳細は[Snowflake Snowpipeドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)を参照してください。 | 必須          |
| ストリーミング（Streaming） | Snowpipe Streaming APIを介してリアルタイムにデータを送信し、行を直接Snowflakeテーブルに書き込みます。 | 必須          |

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送を行うためには、Snowflake Open Database Connectivity（ODBC）ドライバーのインストールと設定が必要です。このドライバーはEMQXがSnowflakeのステージにデータを書き込むための通信ブリッジとして機能し、データの適切なフォーマット、認証、転送を保証します。

詳細は公式の[ODBCドライバー](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な導入と必要なシステム設定を行うための[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法の推奨ではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`をローカルマシンにコピーし、`chmod a+x`で実行権限を付与してから`sudo`で実行します。

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`パッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリにダウンロードし、ドライバーをインストール後、以下のシステム設定ファイルを更新します。

- `/etc/odbc.ini`：Snowflakeデータソース設定を追加
- `/etc/odbcinst.ini`：Snowflakeドライバーパスを登録

**設定例**

`/etc/odbc.ini`の内容確認：

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

`/etc/odbcinst.ini`の内容確認：

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

#### macOSでのSnowflake ODBCドライバー初期化

macOSでSnowflake ODBCドライバーをインストール・設定する手順は以下の通りです。

1. unixODBCをインストール（例）：

   ```
   brew install unixodbc
   ```

2. [iODBCをダウンロード・インストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)。

3. [Snowflake ODBCドライバーをダウンロード・インストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)。

4. 詳細なインストール・設定手順は[macOS向けODBCドライバーのインストールと設定](https://docs.snowflake.com/en/developer-guide/odbc/odbc-mac)を参照。

5. インストール後、以下の設定ファイルを更新：

   - Snowflake ODBCドライバーの権限と設定を更新：

     ```bash
     chown $(id -u):$(id -g) /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     echo 'ODBCInstLib=libiodbcinst.dylib' >> /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     ```

   - `~/.odbc.ini`ファイルを作成または更新し、ODBC接続を設定：

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

### ユーザーアカウント作成とSnowflakeリソース設定

アップロードモードに関わらず、Snowflake環境にユーザーアカウント、データベース、関連リソースを設定し、データ取り込み用の準備を行う必要があります。以下の認証情報は後でEMQXのコネクターおよびSink設定に使用します。

| 項目名                  | 値                                               | 説明                                                         |
| ---------------------- | ------------------------------------------------ | ------------------------------------------------------------ |
| データソース名（DSN）  | `snowflake`（集約モードのみ）                     | `/etc/odbc.ini`で設定したODBC DSN。集約アップロード用。     |
| ユーザー名             | `snowpipeuser`                                   | Snowflake接続認証に使用するユーザー。適切な権限を持つ必要あり。 |
| パスワード             | `Snowpipeuser99`                                 | キーペア認証時は省略可能。                                   |
| データベース名         | `testdatabase`                                   | 対象テーブルが存在するSnowflakeデータベース。               |
| スキーマ               | `public`                                         | データベース内のスキーマ。テーブルやパイプが含まれる。       |
| ステージ（集約モード） | `emqx`                                           | データ取り込み前にファイルを保持するSnowflakeステージ。     |
| パイプ（集約モード）   | `emqx`                                           | ステージからテーブルへデータをロードするパイプ。             |
| パイプ（ストリーミング） | `emqxstreaming`                                  | Snowpipe Streaming API用に`DATA_SOURCE(TYPE => 'STREAMING')`で作成したパイプ。 |
| プライベートキー       | `file://<path to snowflake_rsa_key.private.pem>` | API認証用JWT署名に使用するRSAプライベートキー。              |

#### RSAキーペアの生成（集約モードは任意）

Snowflakeは複数の認証方法をサポートしており、EMQXではアップロードモードと接続設定に応じて選択します。

| アップロードモード | 認証オプション                                               | キーペア必須か |
| ----------------- | ------------------------------------------------------------ | ------------- |
| ストリーミング（HTTPS） | RSAキーペア＋JWT（唯一サポートされる方法）                  | 必須          |
| 集約（ODBC）       | ユーザー名/パスワード（DSNまたはEMQX経由）<br />RSAキーペア＋JWT（任意、EMQXのみ設定） | 任意          |

キーペア認証はストリーミングモードで必須であり、EMQXはJWTを署名してSnowflake Streaming APIへ安全に認証します。

集約モードではユーザー名/パスワードまたはRSAキーペアのいずれかを使用可能です。認証情報の提供方法は以下のいずれかです。

- ダッシュボードのEMQXコネクター設定にユーザー名とパスワードを直接入力
- キーペア認証を使う場合はプライベートRSAキーのパスを指定
- EMQXに指定がなければ、システムのODBC DSN（Linuxなら`/etc/odbc.ini`、macOSなら`~/.odbc.ini`）に正しく設定されている必要あり

::: tip

認証にはパスワードかプライベートキーのいずれかを使用してください。両方は使いません。

EMQXにどちらも設定されていない場合は、`/etc/odbc.ini`の認証情報を参照します。

:::

**例：ユーザー名/パスワードを使った`/etc/odbc.ini`設定**

```ini
[snowflake]
Driver=SnowflakeDSIIDriver
Server=<account>.snowflakecomputing.com
UID=snowpipeuser
PWD=Snowpipeuser99
Database=testdatabase
Schema=public
Warehouse=compute_wh
Role=snowpipe
```

> この方法により、EMQXは設定内で認証情報を直接含めずに`DSN`（`snowflake`）を参照できます。

**キーペア認証を使う場合**

RSAキーペア認証を選択または要求される場合（例：ストリーミングモード）、以下のコマンドで鍵を生成し設定します。

```bash
# 秘密鍵の生成
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt

# 公開鍵の生成
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

EMQXがキーペア認証を使用する場合（集約・ストリーミング両モード対応）：

- EMQXは秘密鍵でJWTに署名し、安全かつ検証可能なIDトークンとして利用
- Snowflakeは公開鍵で署名を検証

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLでSnowflakeリソースを設定

RSAキーペア生成後、集約またはストリーミングの取り込み用に必要なSnowflakeオブジェクトをSQLで作成します。

対象は以下を含みます。

- データベースとテーブルの作成
- ステージとパイプの作成（集約モード）
- ストリーミングパイプの作成（ストリーミングモード）
- ユーザーとロールの作成および権限付与

1. SnowflakeコンソールでSQLワークシートを開き、以下のSQLを実行してデータベース、テーブル、ステージ、パイプを作成します。

   ```sql
   USE ROLE accountadmin;

   -- データ保存用データベース作成（存在しない場合）
   CREATE DATABASE IF NOT EXISTS testdatabase;

   -- MQTTデータ受け取り用テーブル作成
   CREATE OR REPLACE TABLE testdatabase.public.emqx (
       clientid STRING,
       topic STRING,
       payload STRING,
       publish_received_at TIMESTAMP_LTZ
   );

   -- ファイルアップロード用Snowflakeステージ作成（集約モードのみ）
   CREATE STAGE IF NOT EXISTS testdatabase.public.emqx
   FILE_FORMAT = (TYPE = CSV PARSE_HEADER = TRUE FIELD_OPTIONALLY_ENCLOSED_BY = '"')
   COPY_OPTIONS = (ON_ERROR = CONTINUE PURGE = TRUE);

   -- ステージからロードする集約モード用パイプ作成
   CREATE PIPE IF NOT EXISTS testdatabase.public.emqx AS
   COPY INTO testdatabase.public.emqx
   FROM @testdatabase.public.emqx
   MATCH_BY_COLUMN_NAME = CASE_INSENSITIVE;

   -- ストリーミングモード用パイプ作成（直接取り込み）
   CREATE PIPE IF NOT EXISTS testdatabase.public.emqxstreaming AS
   COPY INTO testdatabase.public.emqx (
       clientid,
       topic,
       payload,
       publish_received_at
   )
   FROM (
       SELECT
           $1:clientid::STRING,
           $1:topic::STRING,
           $1:payload::STRING,
           $1:publish_received_at::TIMESTAMP_LTZ
       FROM TABLE(DATA_SOURCE(TYPE => 'STREAMING'))
   );

   ```

   - パイプ内の`COPY INTO`により、Snowflakeはステージまたはストリームされたデータを自動的にテーブルにロードします。
   - ストリーミングパイプの`$1:field`構文はEMQX経由で取り込んだJSONペイロードからフィールドを抽出します。

2. EMQX認証用の専用ユーザー（例：`snowpipeuser`）を作成し、そのユーザーにRSA公開鍵をバインドします。

   ```sql
   -- ユーザーアカウント作成
   CREATE USER IF NOT EXISTS snowpipeuser
       PASSWORD = 'Snowpipeuser99'
       MUST_CHANGE_PASSWORD = FALSE;

   -- RSA公開鍵をユーザーに設定
   ALTER USER snowpipeuser SET RSA_PUBLIC_KEY = '
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_1>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_2>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_3>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_4>
   ';
   ```

   ::: tip

   PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`行は削除し、残りの内容を改行を保持したまま記述してください。

   :::

   このキーはSnowflakeユーザーにアップロードされ、Snowflake内に保存されます。

3. ユーザーに必要なロールを作成し、Snowflakeリソース管理権限を付与します。

   ```sql
   CREATE OR REPLACE ROLE snowpipe;
   
   -- データベースとスキーマの使用権限
   GRANT USAGE ON DATABASE testdatabase TO ROLE snowpipe;
   GRANT USAGE ON SCHEMA testdatabase.public TO ROLE snowpipe;
   GRANT INSERT, SELECT ON testdatabase.public.emqx TO ROLE snowpipe;
   
   -- 集約モード用にステージとパイプの権限付与
   GRANT READ, WRITE ON STAGE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqx TO ROLE snowpipe;
   
   -- ストリーミングモード用にストリーミングパイプの権限付与
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqxstreaming TO ROLE snowpipe;
   
   -- ユーザーにロールを割り当て、デフォルトに設定
   GRANT ROLE snowpipe TO USER snowpipeuser;
   ALTER USER snowpipeuser SET DEFAULT_ROLE = snowpipe;
   ```

## 集約モード用Snowflakeコネクターの作成

Snowflake Sinkで集約アップロードモードを使用する場合は、Snowflake環境との接続を確立するためにSnowflakeコネクターを作成する必要があります。このコネクターはODBC（DSN経由）を使用してステージを介して接続します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプで **Snowflake** を選択し、次へ進みます。

4. コネクター名を入力します。英数字の組み合わせで、ここでは `my-snowflake` と入力します。

5. 接続情報を入力します。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は`<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com`形式です。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>`はSnowflakeインスタンス固有のサブドメインに置き換えてください。

   - **Account**：Snowflakeの組織IDとアカウント名をダッシュ（`-`）で区切って入力します。SnowflakeコンソールのURLの一部です。

   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`で設定した`snowflake`を入力します。

   - **Username**：前述のセットアップで定義した`snowpipeuser`を入力します。

   - **Password**：ODBC経由でユーザー名/パスワード認証を行う場合のパスワード。任意入力です。

     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、

     - `/etc/odbc.ini`に設定するか、

     - キーペア認証を使う場合は空欄にします。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用してください。両方は使いません。ここに設定がなければ`/etc/odbc.ini`の認証情報を参照します。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使うRSAプライベートキーの絶対パス。クラスター内の全ノードで同じパスである必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス

     - **Proxy Port**：プロキシサーバーのポート番号

6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへのデータ書き込みを指定できます。

## Snowflakeストリーミングコネクターの作成

Snowflake Sinkでストリーミングアップロードモードを使う場合は、Snowflake環境との接続を確立するためにSnowflakeストリーミングコネクターを作成します。このコネクターはHTTPSとSnowpipe Streaming REST APIを使用します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプで **Snowflake Streaming** を選択し、次へ進みます。

4. コネクター名を入力します。英数字の組み合わせで、ここでは `my-snowflake-streaming` と入力します。

5. 接続情報を入力します。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は`<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com`形式です。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>`はSnowflakeインスタンス固有のサブドメインに置き換えてください。

   - **Account**：Snowflakeの組織IDとアカウント名をダッシュ（`-`）で区切って入力します。SnowflakeコンソールのURLの一部です。

   - **Pipe User**：対象パイプの操作権限を持つSnowflakeユーザー名。例：`snowpipeuser`。少なくとも`OPERATE`と`MONITOR`権限が必要です。

   - **Private Key Path**：EMQXがJWT署名に使用するRSAプライベートキー。PEM形式のフルテキストを文字列として貼り付けるか、`file://`で始まるファイルパスを指定可能。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス

     - **Proxy Port**：プロキシサーバーのポート番号

6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへのデータ書き込みを指定できます。

## Snowflake Sinkを使ったルールの作成

このセクションでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sink経由で書き込む方法を示します。

### SQLを定義したルールの作成

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

   Snowflake統合では、選択するフィールドはSnowflakeのテーブル定義のカラム数と名前に正確に一致させる必要があります。余分なフィールドを追加したり`*`を使って全選択するのは避けてください。

   :::

4. ルールにアクションを追加し、Sinkを設定します。

   - 集約アップロードモードでSnowflakeに書き込む場合は、[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照してください。

   - ストリーミングアップロードモードでSnowflakeに書き込む場合は、[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照してください。

5. アクション追加後、**Action Outputs**セクションに新規Sinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックしてルール作成を完了します。

これでルール作成が完了し、**Rules**ページに新規ルールが表示され、**Actions (Sink)**タブに新しいSnowflake Sinkが追加されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認でき、トピック`t/#`のメッセージがルール`my_rule`で解析されSnowflakeに書き込まれる様子が視覚的に表示されます。

### 集約アップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、処理結果を集約アップロードモードでSnowflakeに書き込む方法を示します。このモードは複数のルールトリガー結果を単一ファイル（例：CSVファイル）にまとめてSnowflakeにアップロードし、ファイル数を減らし書き込み効率を高めます。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加します。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成してルールに追加します。

3. Sink名（例：`snowflake_sink`）と簡単な説明を入力します。

4. **Connectors**ドロップダウンから先に作成した`my-snowflake`コネクターを選択します。ドロップダウン横の作成ボタンをクリックしてポップアップで新規コネクターを作成することも可能です。必要な設定パラメーターは[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照してください。

5. 集約アップロードモードの設定を行います。

   - **Database Name**：`testdatabase`を入力。EMQXデータ保存用に作成したSnowflakeデータベース。

   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルが存在するスキーマ。

   - **Stage**：`emqx`を入力。データをロード前に保持するSnowflakeステージ。

   - **Pipe**：`emqx`を入力。ステージからテーブルへ自動ロードするパイプ。

   - **Pipe User**：`snowpipeuser`を入力。パイプ管理権限を持つSnowflakeユーザー。

   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSAプライベートキー。以下のいずれかの形式で提供可能：

     - **プレーンテキスト**：PEM形式のプライベートキー全文を文字列として直接貼り付け。

     - **ファイルパス**：`file://`で始まるプライベートキーファイルのパスを指定。クラスター内すべてのノードで同じパスであり、EMQXアプリケーションユーザーがアクセス可能である必要があります。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄。

   - **Aggregation Upload Format**：現在は`csv`のみサポート。データはカンマ区切りCSV形式でSnowflakeにステージングされます。

   - **Column Order**：ドロップダウンから列の並び順を選択。生成されるCSVファイルは選択列を優先して並べ、未選択列はアルファベット順に並びます。

   - **Max Records**：集約トリガーとなる最大レコード数。例：`1000`に設定すると1000レコード収集後にアップロードされ、時間間隔がリセットされます。

   - **Time Interval**：集約が発生する時間間隔（秒）。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにアップロードされ、最大レコード数がリセットされます。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス

     - **Proxy Port**：プロキシサーバーのポート番号

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

7. **詳細設定**を展開し、必要に応じて高度な設定を行います（任意）。詳細は[詳細設定](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSinkがSnowflakeサーバーに接続可能かテストできます。

9. **Create**ボタンをクリックしてSink作成を完了します。作成成功後、ページはルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

### ストリーミングアップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、処理結果をSnowpipe Streaming APIを使ったストリーミングアップロードモードでSnowflakeに書き込む方法を示します。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加します。

2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成してルールに追加します。

3. Sink名（例：`snowflake_sink_streaming`）と簡単な説明を入力します。

4. コネクタードロップダウンから先に作成した`my-snowflake-streaming`コネクターを選択します。ドロップダウン横の作成ボタンをクリックしてポップアップで新規コネクターを作成することも可能です。必要な設定パラメーターは[ストリーミングコネクターの作成](#create-a-snowflake-streaming-connector)を参照してください。

5. ストリーミングアップロードモードの設定を行います。

   - **Database Name**：`testdatabase`を入力。EMQXデータ保存用に作成したSnowflakeデータベース。

   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルが存在するスキーマ。

   - **Pipe**：`emqxstreaming`を入力。SQL文で作成したSnowflakeストリーミングパイプ名。Snowflake側で定義した名前と正確に一致させる必要があります。

   - **HTTP Pipelining**：応答を待たずに送信可能なHTTPリクエストの最大数。デフォルトは`100`。

   - **Connect Timeout**：Snowflakeへの接続確立のタイムアウト秒数。デフォルトは`15`秒。

   - **Connection Pool Size**：EMQXがこのSink用にSnowflakeと維持可能な同時接続数の最大値。デフォルトは`8`。

   - **Max Inactive**：アイドル状態の接続が閉じられるまでの最大時間（秒）。デフォルトは`10`秒。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

7. **詳細設定**を展開し、必要に応じて高度な設定を行います（任意）。詳細は[詳細設定](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSinkがSnowflakeサーバーに接続可能かテストできます。

9. **Create**ボタンをクリックしてSink作成を完了します。作成成功後、ページはルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

## ルールのテスト

このセクションでは、設定したルールのテスト方法を示します。

### テストメッセージのパブリッシュ

MQTTXを使ってトピック`t/1`にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返して複数のテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeにデータが正常に書き込まれたかをSnowflakeインスタンスにアクセスしてターゲットテーブルをクエリして確認します。

1. SnowflakeのWebインターフェースを開き、認証情報でSnowflakeコンソールにログインします。

2. Snowflakeコンソールで以下のSQLを実行し、ルールで書き込まれた`emqx`テーブルのデータを表示します。

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   これにより、`emqx`テーブルにアップロードされたすべてのレコードが表示され、`clientid`、`topic`、`payload`、`publish_received_at`フィールドが含まれます。

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）やトピック、タイムスタンプなどのメタデータが確認できるはずです。

## 詳細設定

このセクションでは、Snowflake Sinkの詳細設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、ニーズに応じて以下のパラメーターを調整できます。

| 項目名                         | 説明                                                         | デフォルト値   |
| ------------------------------ | ------------------------------------------------------------ | ------------- |
| **Buffer Pool Size**            | EMQXとSnowflake間のデータフローを管理するバッファワーカープロセスの数を指定します。これらのワーカーはデータを一時的に保存・処理し、ターゲットサービスへの送信を最適化しスムーズなデータ転送を保証します。 | `16`          |
| **Request TTL**                 | リクエストTTL（Time To Live）は、リクエストがバッファに入ってから有効とみなされる最大秒数を指定します。この時間を超えてバッファに滞留するか、Snowflakeからの応答やアック（ACK）がタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`          |
| **Health Check Interval**       | SinkがSnowflakeとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`          |
| **Health Check Interval Jitter**| 複数ノードが同時にヘルスチェックを開始する可能性を減らすため、基本間隔に加える一様ランダム遅延（ミリ秒）です。複数のアクションやソースが同じコネクターを共有する場合に有効です。 | `0`           |
| **Health Check Timeout**        | コネクターがSnowflakeとの接続ヘルスチェックを行う際のタイムアウト時間（秒）を指定します。 | `60`          |
| **Max Buffer Queue Size**       | Snowflake Sinkの各バッファワーカーが保持可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、Snowflakeへの送信を効率化します。システム性能やデータ転送要件に応じて調整してください。 | `256` MB      |
| **Query Mode**                  | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードではSnowflakeへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受信する可能性があります。 | `Asynchronous`|
| **Batch Size**                  | EMQXからSnowflakeへ一度に転送するデータバッチの最大サイズを指定します。サイズを調整することで転送効率や性能を最適化可能です。<br />`1`に設定するとバッチ化せず個別に送信します。 | `100`         |
| **Inflight Window**             | 送信済みだが応答やアック（ACK）をまだ受け取っていない「インフライト」キューリクエストの最大数を指定します。<br/>`Request Mode`が`asynchronous`の場合に特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`         |
