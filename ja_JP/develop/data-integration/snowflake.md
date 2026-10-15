# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/) は、クラウドベースのデータプラットフォームであり、高いスケーラビリティと柔軟性を備えたデータウェアハウジング、分析、セキュアなデータ共有のソリューションを提供します。構造化データおよび半構造化データの処理に優れ、大量のデータを格納しつつ高速なクエリ性能とさまざまなツールやサービスとのシームレスな統合を実現するよう設計されています。

本ページでは、EMQXとSnowflake間のデータ統合について詳細に紹介し、ルールとSinkの作成方法について実践的なガイダンスを提供します。

## 動作の仕組み

EMQXにおけるSnowflakeデータ統合はすぐに利用可能な機能で、複雑なIoTビジネスワークフローを簡単にサポートできるよう設定可能です。典型的なIoTアプリケーションでは、EMQXがデバイス接続とメッセージ送信を担うIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、格納、分析を行うデータストレージおよび処理プラットフォームとして役割を果たします。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブル内のデータにアクセス可能です。具体的なワークフローは以下の通りです。

1. **デバイスのEMQXへの接続**：IoTデバイスはMQTTプロトコルを介して正常に接続されるとオンラインイベントをトリガーします。イベントにはデバイスID、送信元IPアドレス、その他識別情報が含まれます。

2. **デバイスのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュします。EMQXはメッセージを受信し、ルールエンジン内で比較処理を行います。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールをマッチさせ、データフォーマット変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などを実施します。

4. **Snowflakeへの書き込み**：ルールはメッセージデータをSnowflakeに書き込むアクションをトリガーします。メッセージをファイルにバッチングしてStageとPipe経由でロードする（集約モード）、またはSnowpipe Streaming APIを使って直接ストリーミングする（ストリーミングモード）方法があります。

イベントおよびメッセージデータがSnowflakeに書き込まれた後は、以下のようなビジネスおよび技術的用途で活用可能です。

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの利用を保証します。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を行い、予知保全、運用インサイト、デバイス性能評価を実現します。

## 特長とメリット

EMQXのSnowflakeデータ統合を利用することで、以下の特長とメリットをビジネスにもたらします。

- **メッセージ変換**：メッセージはEMQXルール内で高度な処理や変換を経てSnowflakeに書き込まれ、後続の格納や利用を容易にします。
- **柔軟なデータ操作**：Snowflake Sinkは書き込むフィールドを選択可能で、ビジネスニーズに応じた効率的かつ動的なストレージ構成を実現します。
- **統合されたビジネスプロセス**：Snowflake SinkによりデバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせ、多様なビジネスシナリオ（データ分析やアーカイブなど）を可能にします。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は従来のデータベースより低コストで長期データ保持に最適で、大量のIoTデータ保存に適しています。

これらの特長により、効率的で信頼性の高いスケーラブルなIoTアプリケーションを構築し、ビジネスの意思決定や最適化に役立てることができます。

## はじめる前に

このセクションでは、EMQXでSnowflake Sinkを作成する前の準備について説明します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の基本概念の理解。
- 管理者権限を持つ動作中のSnowflakeアカウント。

### アップロードモードの選択

::: tip

まずモードを選択してください。これによりEMQXおよびSnowflake環境の設定方法が決まります。

:::

EMQXはSnowflakeへのデータ送信に以下の2つのモードをサポートしています。

| モード       | 説明                                                         | ODBC 必須か    |
| ---------- | ------------------------------------------------------------ | ------------- |
| 集約モード | EMQXはMQTTメッセージをローカルファイルにバッファリングし、Snowflakeのステージにアップロードします。`COPY INTO`文で設定されたパイプが自動的にステージファイルをターゲットテーブルにロードします。詳細は[Snowflake Snowpipeドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)を参照してください。 | はい          |
| ストリーミングモード | Snowpipe Streaming APIを使いリアルタイムにデータを送信し、行を直接Snowflakeテーブルに書き込みます。 | はい          |

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送するためには、Snowflake Open Database Connectivity (ODBC)ドライバーのインストールと設定が必要です。このドライバーはEMQXがSnowflakeのステージにデータを書き込むための通信ブリッジとして機能し、データの適切なフォーマット化、認証、転送を保証します。

詳細は公式の[ODBCドライバー](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な導入と必要なシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法の推奨ではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`スクリプトをローカルにコピーし、`chmod a+x`で実行可能にしてから`sudo`で実行します。

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリにダウンロードし、ドライバーをインストール後、以下のシステム設定ファイルを更新します。

- `/etc/odbc.ini`：Snowflakeデータソース設定を追加
- `/etc/odbcinst.ini`：Snowflakeドライバーパスを登録

**設定例**

`/etc/odbc.ini`の内容を確認するには以下を実行します。

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

`/etc/odbcinst.ini`の内容を確認するには以下を実行します。

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

macOSでSnowflake ODBCドライバーをインストール・設定するには以下の手順を実施してください。

1. unixODBCをインストールします（例）:

   ```
   brew install unixodbc
   ```

2. [iODBCをダウンロード・インストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)します。

3. [Snowflake ODBCドライバーをダウンロード・インストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)します。

4. 詳細なインストール・設定手順は[macOS向けODBCドライバーのインストールと設定](https://docs.snowflake.com/en/developer-guide/odbc/odbc-mac)を参照してください。

5. インストール後、以下の設定ファイルを更新します。

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

### ユーザーアカウントとSnowflakeリソースの設定

アップロードモードにかかわらず、Snowflake環境にユーザーアカウント、データベース、関連リソースを設定し、データ取り込みに必要な準備を行う必要があります。以下の認証情報は後でEMQXのコネクターおよびSink設定時に使用します。

| 項目名                  | 値                                                  | 説明                                                         |
| ---------------------- | -------------------------------------------------- | ------------------------------------------------------------ |
| データソース名 (DSN)   | `snowflake`（集約モードのみ）                      | `/etc/odbc.ini`に設定したODBC DSN。集約アップロードで使用。 |
| ユーザー名             | `snowpipeuser`                                     | Snowflake接続認証に使用するユーザー。適切な権限が必要。     |
| パスワード             | `Snowpipeuser99`                                   | キーペア認証使用時は省略可能。                               |
| データベース名         | `testdatabase`                                     | 対象テーブルが存在するSnowflakeデータベース。               |
| スキーマ               | `public`                                           | データベース内のテーブルおよびパイプが存在するスキーマ。     |
| ステージ（集約モード） | `emqx`                                             | データ取り込み前にファイルを保持するSnowflakeステージ。       |
| パイプ（集約モード）   | `emqx`                                             | ステージからテーブルへデータをロードするパイプ。             |
| パイプ（ストリーミング）| `emqxstreaming`                                    | Snowpipe Streaming API用に`DATA_SOURCE(TYPE => 'STREAMING')`で作成したパイプ。 |
| プライベートキー       | `file://<path to snowflake_rsa_key.private.pem>`  | API認証のJWT署名に使用するRSAプライベートキー。              |

#### RSAキーペアの生成（集約モードは任意）

Snowflakeは複数の認証方式をサポートしています。EMQXでの認証方式はアップロードモードと接続設定に依存します。

| アップロードモード       | 認証オプション                                                | キーペア必須か   |
| ----------------- | ------------------------------------------------------------ | --------------- |
| ストリーミング（HTTPS） | RSAキーペア＋JWT（唯一のサポート方式）                       | 必須            |
| 集約（ODBC）       | ユーザー名/パスワード（DSNまたはEMQX経由）<br />RSAキーペア＋JWT（任意、EMQXのみで設定） | 任意            |

キーペア認証はストリーミングモードでのみ必須で、EMQXはJWTに署名しSnowflakeのStreaming APIに安全に認証します。

集約モードではユーザー名/パスワードまたはRSAキーペアのいずれかで認証可能です。認証情報は以下のいずれかで提供します。

- ダッシュボードのEMQXコネクター設定にユーザー名とパスワードを直接入力。
- キーペア認証時はプライベートRSAキーのパスを指定。
- どちらも指定しない場合は、Linuxの`/etc/odbc.ini`やmacOSの`~/.odbc.ini`などシステムのODBC DSNに正しく設定されていることを確認。

::: tip

認証にはパスワードかプライベートキーのいずれかを使用してください。両方は使いません。

EMQXでどちらも設定されていない場合、コネクターは`/etc/odbc.ini`の認証情報を使用します。

:::

**例：ユーザー名/パスワードを使った`/etc/odbc.ini`**

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

**キーペア認証を使用する場合**

RSAキーペア認証を選択または必須となる場合（例：ストリーミングモード）、以下のコマンドで鍵を生成します。

```bash
# 秘密鍵の生成
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt

# 公開鍵の生成
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

EMQXがキーペア認証を使用する場合（集約・ストリーミング両モード対応）：

- EMQXは秘密鍵でJWTに署名し、安全かつ検証可能なIDトークンとして利用。
- Snowflakeは公開鍵で署名を検証。

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLでSnowflakeリソースを設定

RSAキーペア生成後、`aggregated`または`streaming`の取り込み用に必要なSnowflakeオブジェクトをSQLで作成します。

対象は以下を含みます。

- データベースとテーブルの作成
- ステージとパイプの作成（集約モード）
- ストリーミングパイプの作成（ストリーミングモード）
- ユーザーとロールの作成および権限付与

1. SnowflakeコンソールのSQLワークシートを開き、以下SQLを実行してデータベース、テーブル、ステージ、パイプを作成します。

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

   - パイプ内の`COPY INTO`により、Snowflakeはステージまたはストリーミングされたデータを自動的にテーブルにロードします。
   - ストリーミングパイプの`$1:field`構文はEMQX経由で取り込まれたJSONペイロードからフィールドを抽出します。

2. EMQX認証用の専用ユーザー（例：`snowpipeuser`）を作成し、RSA公開鍵をバインドします。

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

   PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`行は削除し、残りの内容を改行を保持して含めてください。

   :::

   この鍵はSnowflakeユーザーにアップロードされ、Snowflake内に保存されます。

3. 必要なロールを作成し、ユーザーに割り当ててSnowflakeリソースの管理権限を付与します。

   ```sql
   CREATE OR REPLACE ROLE snowpipe;
   
   -- データベースとスキーマの使用権限付与
   GRANT USAGE ON DATABASE testdatabase TO ROLE snowpipe;
   GRANT USAGE ON SCHEMA testdatabase.public TO ROLE snowpipe;
   GRANT INSERT, SELECT ON testdatabase.public.emqx TO ROLE snowpipe;
   
   -- 集約モード用にステージとパイプの権限付与
   GRANT READ, WRITE ON STAGE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqx TO ROLE snowpipe;
   
   -- ストリーミングモード用パイプの権限付与
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqxstreaming TO ROLE snowpipe;
   
   -- ユーザーにロールを割り当て、デフォルトに設定
   GRANT ROLE snowpipe TO USER snowpipeuser;
   ALTER USER snowpipeuser SET DEFAULT_ROLE = snowpipe;
   ```

## 集約モード用Snowflakeコネクターの作成

Snowflake Sinkで集約アップロードモードを使用する場合、Snowflake環境との接続を確立するためにSnowflakeコネクターを作成する必要があります。このコネクターはODBC（DSN経由）を使いステージを通じて接続します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプで **Snowflake** を選択し、次へ進みます。

4. コネクター名を入力します。英数字の組み合わせで、ここでは `my-snowflake` とします。

5. 接続情報を入力します。

   - **Server Host**：SnowflakeのエンドポイントURL。通常 `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式です。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換えてください。

   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切って入力します。SnowflakeコンソールのURLに含まれています。

   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`に設定した`snowflake`を入力します。

   - **Username**：前述の設定で作成した`snowpipeuser`を入力します。

   - **Password**：ODBC経由でユーザー名/パスワード認証する場合のパスワード。任意入力です。

     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、

     - `/etc/odbc.ini`に設定するか、

     - キーペア認証を使う場合は空欄にします。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用してください。両方は使いません。ここでどちらも設定しない場合は`/etc/odbc.ini`の認証情報を利用します。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使うRSAプライベートキーの絶対パス。クラスタ内の全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄にします。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシは非対応です。デフォルトはプロキシなし。プロキシを有効にするには`Enable Proxy`を選択し、以下を入力します。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を確立する場合は **Enable TLS** トグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 高度な設定（任意）：[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへの書き込み方法を指定できます。

## Snowflakeストリーミングコネクターの作成

Snowflake Sinkでストリーミングアップロードモードを使用する場合、Snowflake環境との接続を確立するためにSnowflakeストリーミングコネクターを作成します。このコネクターはHTTPSおよびSnowpipe Streaming REST APIを使用します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプで **Snowflake Streaming** を選択し、次へ進みます。

4. コネクター名を入力します。英数字の組み合わせで、ここでは `my-snowflake-streaming` とします。

5. 接続情報を入力します。

   - **Server Host**：SnowflakeのエンドポイントURL。通常 `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式です。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換えてください。

   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切って入力します。SnowflakeコンソールのURLに含まれています。

   - **Pipe User**：ターゲットPipeを操作できる権限を持つSnowflakeユーザー名（例：`snowpipeuser`）。少なくとも`OPERATE`および`MONITOR`権限が必要です。

   - **Private Key Path**：EMQXがJWT署名に使うRSAプライベートキー。PEM形式の秘密鍵全文を文字列として貼り付けるか、`file://`で始まるファイルパスを指定します。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄にします。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシは非対応です。デフォルトはプロキシなし。プロキシを有効にするには`Enable Proxy`を選択し、以下を入力します。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を確立する場合は **Enable TLS** トグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 高度な設定（任意）：[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへの書き込み方法を指定できます。

## Snowflake Sinkを使ったルールの作成

このセクションでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sinkを通じてSnowflakeに書き込む方法を示します。

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

   Snowflake連携では、選択フィールドがSnowflakeのテーブル定義のカラム数および名前と完全に一致することが重要です。不要なフィールド追加や`*`選択は避けてください。

   :::

4. ルールにアクションを追加し、Sinkを設定します。

   - 集約アップロードモードでSnowflakeに書き込む場合は[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照してください。

   - ストリーミングアップロードモードでSnowflakeに書き込む場合は[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照してください。

5. アクション追加後、**Action Outputs**セクションに新しいSinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックしてルール作成を完了します。

これでルール作成が完了し、**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブに新しいSnowflake Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されSnowflakeに書き込まれる流れを視覚的に確認できます。

### 集約アップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、集約アップロードモードで処理結果をSnowflakeに書き込む方法を示します。このモードは複数のルールトリガー結果を単一ファイル（例：CSVファイル）にまとめてアップロードし、ファイル数を減らして書き込み効率を向上させます。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加します。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**ドロップダウンはデフォルトの`Create Action`のままにするか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成しルールに追加します。

3. Sink名（例：`snowflake_sink`）と簡単な説明を入力します。

4. **Connectors**ドロップダウンから先に作成した`my-snowflake`コネクターを選択します。隣の作成ボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。必要な設定パラメータは[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照してください。

5. 集約アップロードモードの設定を行います。

   - **Database Name**：`testdatabase`を入力。EMQXデータ保存用に作成したSnowflakeデータベースです。

   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルがあるスキーマです。

   - **Stage**：`emqx`を入力。Snowflakeでデータロード前にファイルを保持するステージです。

   - **Pipe**：`emqx`を入力。ステージからテーブルへのロードを自動化するパイプです。

   - **Pipe User**：`snowpipeuser`を入力。パイプ管理権限を持つSnowflakeユーザーです。

   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSAプライベートキー。以下いずれかの形式で指定可能です。

     - **プレーンテキスト**：PEM形式の秘密鍵全文を文字列として直接貼り付け。

     - **ファイルパス**：`file://`で始まる秘密鍵ファイルのパスを指定。クラスタ内全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵が暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄にします。

   - **Aggregation Upload Format**：現在は`csv`のみサポート。SnowflakeにCSV形式でステージングされます。

   - **Column Order**：ドロップダウンからカラムの並び順を選択。生成されるCSVファイルは選択したカラム順にソートされ、未選択カラムはアルファベット順に続きます。

   - **Max Records**：集約トリガーとなる最大レコード数。例：`1000`に設定すると1000件集まった時点でファイルをアップロードし、時間間隔をリセットします。

   - **Time Interval**：集約が発生する時間間隔（秒）。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにアップロードし、最大レコード数をリセットします。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシは非対応です。デフォルトはプロキシなし。プロキシを有効にするには`Enable Proxy`を選択し、以下を入力します。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて高度な設定を行います（任意）。詳細は[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSinkがSnowflakeサーバーに接続できるかテスト可能です。

9. **Create**ボタンをクリックしてSink作成を完了します。作成成功後、ルール作成画面に戻り、新しいSinkがルールアクションに追加されます。

### ストリーミングアップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、ストリーミングアップロードモードで処理結果をSnowflakeに書き込む方法を示します。このモードはSnowpipe Streaming APIを使いリアルタイム取り込みを可能にします。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加します。

2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**ドロップダウンはデフォルトの`Create Action`のままにするか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成しルールに追加します。

3. Sink名（例：`snowflake_sink_streaming`）と簡単な説明を入力します。

4. コネクタードロップダウンから先に作成した`my-snowflake-streaming`コネクターを選択します。隣の作成ボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。必要な設定パラメータは[Snowflakeストリーミングコネクターの作成](#create-a-snowflake-streaming-connector)を参照してください。

5. ストリーミングアップロードモードの設定を行います。

   - **Database Name**：`testdatabase`を入力。EMQXデータ保存用に作成したSnowflakeデータベースです。

   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルがあるスキーマです。

   - **Pipe**：`emqxstreaming`を入力。SQL文で作成したSnowflakeストリーミングパイプの名前で、Snowflake側で定義した名前と完全一致する必要があります。

   - **HTTP Pipelining**：レスポンスを待たずに送信可能な最大HTTPリクエスト数。デフォルトは`100`。

   - **Connect Timeout**：Snowflakeへの接続確立のタイムアウト秒数。デフォルトは`15`秒。

   - **Connection Pool Size**：このSinkがSnowflakeと維持可能な最大同時接続数。デフォルトは`8`。

   - **Max Inactive**：HTTPクライアントが再接続を試みるまでの最大非アクティブ時間（秒）。EMQX 6.0.4以降、リクエスト待ち状態で60秒間送信がない場合に再接続します。60秒より長い値を設定すると優先されます。レスポンスが60秒以上かかる可能性がある場合は、最大応答時間以上に設定してください。デフォルトは`10`秒。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて高度な設定を行います（任意）。詳細は[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSinkがSnowflakeサーバーに接続できるかテスト可能です。

9. **Create**ボタンをクリックしてSink作成を完了します。作成成功後、ルール作成画面に戻り、新しいSinkがルールアクションに追加されます。

## ルールのテスト

このセクションでは、設定したルールのテスト方法を示します。

### テストメッセージのパブリッシュ

MQTTXを使い、トピック`t/1`にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返してテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeにデータが正常に書き込まれたかをSnowflakeインスタンスにアクセスし、対象テーブルをクエリして確認できます。

1. SnowflakeのWebインターフェースを開き、認証情報でSnowflakeコンソールにログインします。

2. Snowflakeコンソールで以下SQLを実行し、ルールが書き込んだ`emqx`テーブルのデータを確認します。

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   これにより`emqx`テーブルにアップロードされたすべてのレコード（`clientid`、`topic`、`payload`、`publish_received_at`フィールド）が表示されます。

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）やトピック、タイムスタンプなどのメタデータが確認できるはずです。

## 高度な設定

このセクションでは、Snowflake Sinkの高度な設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、用途に応じて以下のパラメータを調整可能です。

| 項目名                         | 説明                                                         | デフォルト値   |
| ------------------------------ | ------------------------------------------------------------ | ------------- |
| **Buffer Pool Size**            | EMQXとSnowflake間のデータフローを管理するバッファワーカーのプロセス数を指定します。これらのワーカーはデータを一時的に保持・処理し、送信前のパフォーマンス最適化とスムーズなデータ転送に重要です。 | `16`          |
| **Request TTL**                 | リクエストTTL（Time To Live）は、リクエストがバッファに入ってから有効とみなされる最大秒数を指定します。TTLを超えたリクエストや、送信後にSnowflakeから適時の応答やアック（ACK）が得られない場合、リクエストは期限切れと見なされます。 | `45`          |
| **Health Check Interval**       | SinkがSnowflakeとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`          |
| **Health Check Interval Jitter**| 複数ノードが同時にヘルスチェックを開始するのを防ぐため、基本間隔に加える一様ランダム遅延です。複数のアクションやソースが同一コネクターを共有する場合、ジッターを有効にするとヘルスチェック開始時刻が分散されます。 | `0` ミリ秒    |
| **Health Check Timeout**        | Snowflake接続の自動ヘルスチェックのタイムアウト時間を指定します。 | `60`          |
| **Max Buffer Queue Size**       | Snowflake Sinkの各バッファワーカーが保持可能な最大バイト数を指定します。バッファワーカーはデータを一時的に保持し、効率的なデータストリーム処理を行います。システム性能やデータ転送要件に応じて調整してください。 | `256` MB      |
| **Query Mode**                  | 同期（`synchronous`）または非同期（`asynchronous`）のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードではSnowflakeへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受信する可能性があります。 | `Asynchronous`|
| **Batch Size**                  | EMQXからSnowflakeへ一度に送信するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送効率や性能を最適化可能です。<br />`Batch Size`を`1`に設定すると、データレコードはバッチ化せず個別に送信されます。 | `100`         |
| **Inflight Window**             | 「インフライトキューリクエスト」とは、送信済みで応答やアック待ちのリクエストを指します。この設定はSnowflakeとの通信中に同時に存在可能なインフライトリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`         |
