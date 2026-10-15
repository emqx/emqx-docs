# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/)は、クラウドベースのデータプラットフォームであり、高いスケーラビリティと柔軟性を備えたデータウェアハウジング、分析、セキュアなデータ共有のソリューションを提供します。構造化データおよび半構造化データの取り扱いに優れ、大量のデータを高速なクエリ性能で保存し、さまざまなツールやサービスとのシームレスな統合を実現しています。

本ページでは、EMQXとSnowflake間のデータ統合について詳しく解説し、ルールおよびSinkの作成方法について実践的なガイダンスを提供します。

## 動作概要

EMQXにおけるSnowflakeデータ統合は、複雑なIoTビジネスワークフローを簡単にサポートできる使いやすい機能です。典型的なIoTアプリケーションでは、EMQXがデバイス接続とメッセージ送受信を担当するIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、保存、分析を行うデータストレージおよび処理プラットフォームとして役割を果たします。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスのイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブルに格納されたデータにアクセスできます。具体的なワークフローは以下の通りです。

1. **デバイスのEMQXへの接続**：IoTデバイスはMQTTプロトコルで正常に接続するとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレスなどの識別情報が含まれます。

2. **デバイスのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジン内で処理します。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。

4. **Snowflakeへの書き込み**：ルールはメッセージデータを書き込むアクションをトリガーします。メッセージをファイルにバッチングしてStageとPipe経由でロードする（集約モード）、またはSnowpipe Streaming APIを用いて直接ストリーミングする（ストリーミングモード）方法があります。

イベントやメッセージデータがSnowflakeに書き込まれた後は、以下のようなビジネスおよび技術的用途で活用できます。

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの保持を実現。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を行い、予知保全、運用インサイト、デバイス性能評価を可能にします。

## 特長とメリット

EMQXのSnowflakeデータ統合を利用することで、以下の特長とメリットをビジネスにもたらします。

- **メッセージ変換**：EMQXのルール内でメッセージを多様に処理・変換でき、Snowflakeへの書き込み前にデータの整形や加工が可能です。
- **柔軟なデータ操作**：Snowflake Sinkは書き込むフィールドを選択できるため、ビジネスニーズに応じた効率的かつ動的なストレージ構成が可能です。
- **統合されたビジネスプロセス**：Snowflake SinkはデバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせることができ、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は、従来のデータベースに比べ低コストで長期データ保持に最適なソリューションを提供します。

これらの特長により、効率的で信頼性が高くスケーラブルなIoTアプリケーションを構築し、ビジネスの意思決定や最適化に役立てることができます。

## はじめる前に

このセクションでは、EMQXでSnowflake Sinkを作成する前の準備について説明します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の基本概念の理解。
- 管理者権限を持つ動作中のSnowflakeアカウント。

### アップロードモードの選択

::: tip

モードの選択は、EMQXおよびSnowflake環境の設定方法に影響するため、最初に決定してください。

:::

EMQXはSnowflakeへのデータ送信に以下の2つのモードをサポートしています。

| モード       | 説明                                                         | ODBC 必須 |
| ------------ | ------------------------------------------------------------ | --------- |
| 集約モード   | EMQXはMQTTメッセージをローカルファイルにバッファリングし、Snowflakeのステージにアップロードします。`COPY INTO`文で設定されたパイプが自動的にステージファイルをターゲットテーブルにロードします。詳細は[Snowflake Snowpipeドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)を参照してください。 | はい      |
| ストリーミングモード | Snowpipe Streaming APIを介してリアルタイムにデータを送信し、行を直接Snowflakeテーブルに書き込みます。 | はい      |

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送を行うためには、SnowflakeのOpen Database Connectivity（ODBC）ドライバーをインストールおよび設定する必要があります。このドライバーは、EMQXがSnowflakeのステージにデータを書き込むための通信橋渡し役を担い、データの適切なフォーマット、認証、転送を保証します。

詳細は公式の[ODBCドライバー](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な導入と必要なシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法の推奨ではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`スクリプトをローカルにコピーし、`chmod a+x`で実行権限を付与後、`sudo`で実行します。

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリにダウンロードし、ドライバーをインストール、以下のシステム設定ファイルを更新します。

- `/etc/odbc.ini`：Snowflakeデータソース設定を追加
- `/etc/odbcinst.ini`：Snowflakeドライバーのパスを登録

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

macOSでSnowflake ODBCドライバーをインストールおよび設定するには、以下の手順に従ってください。

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

   - ODBC接続設定用に`~/.odbc.ini`ファイルを作成または更新：

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

### ユーザーアカウント作成およびSnowflakeリソースの設定

アップロードモードにかかわらず、Snowflake環境においてユーザーアカウント、データベース、関連リソースを設定し、データ取り込みの準備を行う必要があります。以下の認証情報は後にEMQXのコネクターおよびSink設定で使用します。

| 項目                     | 値                                                  | 説明                                                         |
| ------------------------ | --------------------------------------------------- | ------------------------------------------------------------ |
| データソース名（DSN）    | `snowflake`（集約モードのみ）                        | `/etc/odbc.ini`に設定されたODBC DSN。集約アップロード用。     |
| ユーザー名               | `snowpipeuser`                                      | Snowflake接続認証に使用するユーザー。適切な権限が必要。       |
| パスワード               | `Snowpipeuser99`                                    | キーペア認証使用時は省略可能。                                |
| データベース名           | `testdatabase`                                      | 対象テーブルが存在するSnowflakeデータベース。                 |
| スキーマ                 | `public`                                            | データベース内のスキーマ。テーブルやパイプが存在する。         |
| ステージ（集約モード）   | `emqx`                                              | データ取り込み前にファイルを保持するSnowflakeステージ。       |
| パイプ（集約モード）     | `emqx`                                              | ステージからテーブルへデータをロードするパイプ。               |
| パイプ（ストリーミング） | `emqxstreaming`                                     | Snowpipe Streaming API経由でデータを取り込むためのストリーミングパイプ。 |
| プライベートキー         | `file://<path to snowflake_rsa_key.private.pem>`   | API認証用JWT署名に使用するRSAプライベートキーのパス。          |

#### RSAキーペアの生成（集約モードは任意）

Snowflakeは複数の認証方法をサポートしており、EMQXではアップロードモードおよび接続設定に応じて認証方法を選択します。

| アップロードモード       | 認証方法                                                     | キーペア必須 |
| ----------------------- | ------------------------------------------------------------ | ----------- |
| ストリーミング（HTTPS） | RSAキーペア + JWT（唯一サポートされる方法）                  | 必須        |
| 集約（ODBC）            | ユーザー名/パスワード（DSNまたはEMQX経由）<br />RSAキーペア + JWT（任意、EMQX内で設定） | 任意        |

キーペア認証はストリーミングモードで必須であり、EMQXはJWTを署名してSnowflakeのStreaming APIに安全に認証します。

集約モードではユーザー名/パスワードまたはRSAキーペアのいずれかを使用可能です。認証情報は以下のいずれかの方法で提供します。

- ダッシュボードのEMQXコネクター設定にユーザー名とパスワードを直接入力。
- プライベートRSAキーのパスを指定（キーペア認証時）。
- いずれも指定しない場合は、システムのODBC DSN（Linuxの`/etc/odbc.ini`やmacOSの`~/.odbc.ini`）に認証情報を正しく設定。

::: tip

認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に設定しないでください。

EMQXにいずれも設定されていない場合は、`/etc/odbc.ini`の認証情報を使用します。

:::

**例：ユーザー名/パスワードを使った`/etc/odbc.ini`の設定**

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

RSAキーペア認証（例：ストリーミングモード）を使用する場合、以下のコマンドで鍵を生成し設定します。

```bash
# プライベートキー生成
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt

# パブリックキー生成
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

EMQXがキーペア認証を使用する場合（集約・ストリーミング両モード対応）：

- EMQXはプライベートRSAキーでJWTに署名し、安全かつ検証可能なIDトークンとして利用。
- Snowflakeはパブリックキーで署名を検証。

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLによるSnowflakeリソースのセットアップ

RSAキーペア生成後、`aggregated`または`streaming`の取り込み用に必要なSnowflakeオブジェクトをSQLで作成します。

対象は以下の通りです。

- データベースとテーブルの作成
- ステージとパイプの作成（`aggregated`用）
- ストリーミングパイプの作成（`streaming`用）
- ユーザーとロールの作成および権限付与

1. SnowflakeコンソールのSQLワークシートで以下を実行し、データベース、テーブル、ステージ、パイプを作成します。

   ```sql
   USE ROLE accountadmin;

   -- データ保存用のデータベース作成（存在しない場合）
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

   -- ステージからのロードを行う集約モード用パイプ作成
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
   - ストリーミングパイプの`$1:field`構文は、EMQX経由で取り込まれたJSONペイロードからフィールドを抽出します。

2. EMQX認証用の専用ユーザー（例：`snowpipeuser`）を作成し、RSA公開鍵をユーザーにバインドします。

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

   PEMファイルの`-----BEGIN PUBLIC KEY-----`と`-----END PUBLIC KEY-----`の行は削除し、残りの内容を改行を保持したまま含めてください。

   :::

   この鍵はSnowflakeユーザーにアップロードされ、Snowflake内部に保存されます。

3. ユーザーに必要な権限を付与するロールを作成し、割り当てます。

   ```sql
   CREATE OR REPLACE ROLE snowpipe;

   -- データベースとスキーマの使用権限付与
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

Snowflake Sinkで集約アップロードモードを使用する場合は、Snowflake環境との接続を確立するためにODBC（DSN経由）を使ったSnowflakeコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake** を選択し、次へ。

4. コネクター名を英数字の組み合わせで入力。ここでは `my-snowflake` とします。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>`はSnowflakeインスタンス固有のサブドメインに置き換えます。

   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切ったもの。SnowflakeコンソールのURLに含まれています。

   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`で設定した`snowflake`。

   - **Username**：前述の設定で作成した`snowpipeuser`。

   - **Password**：ODBC経由でユーザー名/パスワード認証を行う場合のパスワード。任意入力です。

     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、

     - `/etc/odbc.ini`に設定するか、

     - キーペア認証を使う場合は空欄にします。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に設定しないでください。ここに設定がない場合は`/etc/odbc.ini`の認証情報が使われます。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使うRSAプライベートキーの絶対ファイルパス。クラスター内の全ノードで同一パスである必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`。

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を行う場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**で接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへの書き込みを指定できます。

## ストリーミングモード用Snowflakeコネクターの作成

Snowflake Sinkでストリーミングアップロードモードを使用する場合は、HTTPSおよびSnowpipe Streaming REST APIを利用するSnowflakeストリーミングコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake Streaming** を選択し、次へ。

4. コネクター名を英数字の組み合わせで入力。ここでは `my-snowflake-streaming` とします。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>`はSnowflakeインスタンス固有のサブドメインに置き換えます。

   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切ったもの。SnowflakeコンソールのURLに含まれています。

   - **Pipe User**：対象パイプの操作権限を持つSnowflakeユーザー名（例：`snowpipeuser`）。少なくとも`OPERATE`と`MONITOR`権限が必要です。

   - **Private Key Path**：EMQXがJWT署名に使用するRSAプライベートキー。PEM形式のキー全文を文字列で貼り付けるか、`file://`で始まるファイルパスを指定可能。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`。

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。`-nocrypt`オプションで生成した場合は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を行う場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**で接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへの書き込みを指定できます。

## Snowflake Sinkを使ったルールの作成

このセクションでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sink経由でSnowflakeに書き込む方法を示します。

### SQLを定義したルールの作成

1. ダッシュボードの **Integration** -> **Rules** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. ルールIDに `my_rule` を入力し、SQLエディターに以下のルールSQLを入力。

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

   SQL初心者の場合は、**SQL Examples**や**Enable Debug**をクリックして、ルールSQLの学習やテストが可能です。

   :::

   ::: tip

   Snowflake連携では、選択するフィールドがSnowflakeのテーブルのカラム数および名前と完全に一致することが重要です。余分なフィールドを追加したり`*`で選択しないようにしてください。

   :::

4. ルールにアクションを追加し、Sinkを設定します。

   - 集約アップロードモードでSnowflakeに書き込む場合は、[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照。

   - ストリーミングアップロードモードでSnowflakeに書き込む場合は、[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照。

5. アクション追加後、**Action Outputs**セクションに新しいSinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックしてルール作成を完了します。

これでルール作成が完了し、**Rules**ページに新規ルールが表示され、**Actions (Sink)**タブに新しいSnowflake Sinkが追加されます。

また、**Integration** -> **Flow Designer**でトポロジーを確認できます。トポロジーは、トピック`t/#`のメッセージがルール`my_rule`で解析され、Snowflakeに書き込まれる流れを視覚的に示します。

### 集約アップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加して、処理結果を集約アップロードモードでSnowflakeに書き込む方法を示します。このモードでは複数のルールトリガー結果を1つのファイル（例：CSV）にまとめてアップロードし、ファイル数を減らして書き込み効率を向上させます。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sink名（例：`snowflake_sink`）と簡単な説明を入力。

4. **Connectors**ドロップダウンから先に作成した`my-snowflake`コネクターを選択。ドロップダウン横の作成ボタンで新規コネクターをポップアップから素早く作成可能。設定パラメータは[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照。

5. 集約アップロードモードの設定を行う。

   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース。

   - **Schema**：`public`。`testdatabase`内のデータテーブルがあるスキーマ。

   - **Stage**：`emqx`。Snowflakeで作成した、データロード前にファイルを保持するステージ。

   - **Pipe**：`emqx`。ステージからテーブルへのロードを自動化するパイプ。

   - **Pipe User**：`snowpipeuser`。パイプ管理権限を持つSnowflakeユーザー。

   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSAプライベートキー。以下のいずれかの形式で指定可能。

     - **プレーンテキスト**：PEM形式のプライベートキー全文を文字列として直接貼り付け。

     - **ファイルパス**：`file://`で始まるプライベートキーファイルのパス。クラスター内の全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要あり。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`。

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄。

   - **Aggregation Upload Format**：現在は`csv`のみ対応。Snowflakeにカンマ区切りCSV形式でステージングされます。

   - **Column Order**：ドロップダウンから列の並び順を選択。生成されるCSVファイルは選択した列を優先的に並べ、未選択列はアルファベット順に並びます。

   - **Max Records**：集約をトリガーする最大レコード数。例：`1000`に設定すると1000件集まった時点でアップロードし、時間間隔をリセット。

   - **Time Interval**：集約が発生する時間間隔（秒）。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにアップロードし、最大レコード数をリセット。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeサーバーへの接続テストが可能。

9. **Create**ボタンをクリックしてSink作成を完了。成功するとルール作成画面に戻り、新しいSinkがルールアクションに追加されます。

### ストリーミングアップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加して、処理結果をストリーミングアップロードモードでSnowflakeに書き込む方法を示します。このモードはSnowpipe Streaming APIを利用したリアルタイム取り込みを可能にします。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sink名（例：`snowflake_sink_streaming`）と簡単な説明を入力。

4. コネクターのドロップダウンから先に作成した`my-snowflake-streaming`を選択。ドロップダウン横の作成ボタンで新規コネクターをポップアップから素早く作成可能。設定パラメータは[ストリーミングモード用Snowflakeコネクターの作成](#create-a-snowflake-streaming-connector)を参照。

5. ストリーミングアップロードモードの設定を行う。

   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース。

   - **Schema**：`public`。`testdatabase`内のデータテーブルがあるスキーマ。

   - **Pipe**：`emqxstreaming`。SQLで作成したSnowflakeストリーミングパイプの名前。Snowflakeで定義した名前と完全一致する必要あり。

   - **HTTP Pipelining**：レスポンスを待たずに送信可能な最大HTTPリクエスト数。デフォルト：`100`。

   - **Connect Timeout**：Snowflakeへの接続確立のタイムアウト秒数。デフォルト：`15`秒。

   - **Connection Pool Size**：EMQXがこのSink用にSnowflakeへ維持可能な同時接続数の最大値。デフォルト：`8`。

   - **Max Inactive**：HTTPクライアントが再接続を試みるまでの最大非アクティブ時間。EMQX 6.0.4以降は、リクエスト待ちで60秒間送信がない場合に再接続。60秒より長い値を設定すると優先されます。応答に60秒以上かかる場合は最大応答時間以上に設定してください。デフォルト：`10`秒。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeサーバーへの接続テストが可能。

9. **Create**ボタンをクリックしてSink作成を完了。成功するとルール作成画面に戻り、新しいSinkがルールアクションに追加されます。

## ルールのテスト

このセクションでは、設定したルールのテスト方法を示します。

### テストメッセージのパブリッシュ

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返してテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeにデータが正常に書き込まれたか確認します。

1. SnowflakeのWebインターフェースにログイン。

2. Snowflakeコンソールで以下のSQLを実行し、ルールで書き込まれた`emqx`テーブルのデータを表示。

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）や、トピック、タイムスタンプなどのメタデータが表示されるはずです。

## 詳細設定

このセクションでは、Snowflake Sinkの詳細設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、用途に応じて以下のパラメータを調整できます。

| 項目名                         | 説明                                                         | デフォルト値   |
| ------------------------------ | ------------------------------------------------------------ | ------------- |
| **Buffer Pool Size**            | EMQXとSnowflake間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保持・処理し、送信前のパフォーマンス最適化とスムーズなデータ転送を支えます。 | `16`          |
| **Request TTL**                 | バッファに入ったリクエストが有効とみなされる最大時間（秒）です。TTLを超えてバッファに滞留するか、Snowflakeからの応答やアックが期限内に得られない場合、リクエストは期限切れと判断されます。 | `45`          |
| **Health Check Interval**       | Snowflakeとの接続状態を自動的にチェックする間隔（秒）を指定します。 | `15`          |
| **Health Check Interval Jitter**| 複数ノードが同時にヘルスチェックを開始するのを防ぐため、基本間隔に加えるランダムな遅延（ミリ秒）です。複数のアクションやソースが同じコネクターを共有する場合に有効です。 | `0`           |
| **Health Check Timeout**        | Snowflakeとの接続状態チェックのタイムアウト時間（秒）を指定します。 | `60`          |
| **Max Buffer Queue Size**       | Snowflake Sinkの各バッファワーカーが保持可能な最大バイト数です。バッファワーカーはデータを一時保存し、効率的なデータストリーム処理を行います。システム性能やデータ転送要件に応じて調整してください。 | `256` MB      |
| **Query Mode**                  | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信の最適化を図ります。非同期モードではSnowflakeへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受信する可能性があります。 | `Asynchronous`|
| **Batch Size**                  | EMQXからSnowflakeへ一度に転送するデータバッチの最大サイズです。サイズ調整により転送効率や性能を最適化できます。<br />`1`に設定するとバッチングせず個別送信となります。 | `100`         |
| **Inflight Window**             | 送信済みだが応答やアックをまだ受け取っていない「インフライト」キューリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合に重要で、同一MQTTクライアントのメッセージを厳密に順序処理する必要がある場合は`1`に設定してください。 | `100`         |
