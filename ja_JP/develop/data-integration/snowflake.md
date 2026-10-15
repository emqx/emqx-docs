# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/)は、クラウドベースのデータプラットフォームであり、データウェアハウジング、分析、および安全なデータ共有のための高いスケーラビリティと柔軟性を提供します。構造化データおよび半構造化データの処理に優れ、大量のデータを高速なクエリ性能とさまざまなツールやサービスとのシームレスな統合を実現するよう設計されています。

本ページでは、EMQXとSnowflake間のデータ統合について詳細に紹介し、ルールとSinkの作成方法について実践的なガイダンスを提供します。

## 動作概要

EMQXにおけるSnowflakeデータ統合はすぐに利用可能な機能であり、複雑なIoTビジネスワークフローを簡単にサポートできるように構成可能です。典型的なIoTアプリケーションでは、EMQXがデバイス接続とメッセージ送信を担うIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、保存、分析を行うデータストレージおよび処理プラットフォームとして利用されます。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブル内のデータにアクセスできます。具体的なワークフローは以下の通りです。

1. **デバイスのEMQXへの接続**：IoTデバイスはMQTTプロトコルを介して正常に接続されるとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレスなどの識別情報が含まれます。

2. **デバイスからのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリおよびステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジン内で比較処理を行います。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、トピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールをマッチさせ、データ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などを実行します。

4. **Snowflakeへの書き込み**：ルールはメッセージデータをSnowflakeに書き込むアクションをトリガーします。メッセージをファイルにバッチングしてStageとPipe経由でロードする（集約モード）、またはSnowpipe Streaming APIを使ってリアルタイムにストリーミングする（ストリーミングモード）方法があります。

イベントやメッセージデータがSnowflakeに書き込まれた後は、以下のようなビジネスや技術的な目的で利用可能です。

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの保持を実現。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を実行。予知保全、運用インサイト、デバイス性能評価などを可能にします。

## 特徴と利点

EMQXのSnowflakeデータ統合を利用することで、以下の特徴と利点が得られます。

- **メッセージ変換**：メッセージはEMQXのルール内で高度に処理・変換されてからSnowflakeに書き込まれるため、後続の保存や利用が容易になります。
- **柔軟なデータ操作**：Snowflake Sinkは、Snowflakeに書き込む特定フィールドの選択を可能にし、ビジネスニーズに応じた効率的かつ動的なストレージ構成を実現します。
- **統合されたビジネスプロセス**：Snowflake Sinkにより、デバイスデータをSnowflakeの豊富なエコシステムアプリケーションと連携でき、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストな長期保存**：Snowflakeのスケーラブルなストレージ基盤は、従来のデータベースよりも低コストで大容量のIoTデータの長期保存に最適です。

これらの特徴により、効率的で信頼性が高くスケーラブルなIoTアプリケーションを構築し、ビジネスの意思決定や最適化に役立てることができます。

## はじめる前に

このセクションでは、EMQXでSnowflake Sinkを作成する前の準備について説明します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の基本概念の理解
- 管理者権限を持つSnowflakeアカウント

### アップロードモードの選択

::: tip

モードを最初に選択してください。これにより、EMQXおよびSnowflake環境の設定方法が決まります。

:::

EMQXはSnowflakeへのデータ送信に以下の2つのモードをサポートしています。

| モード       | 説明                                                         | ODBC 必要性   |
| ---------- | ------------------------------------------------------------ | ------------- |
| 集約モード | EMQXはMQTTメッセージをローカルファイルにバッファリングし、それをSnowflakeのStageにアップロードします。`COPY INTO`文を設定したPipeが自動的にステージファイルをターゲットテーブルにロードします。詳細は[Snowflake Snowpipe ドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)を参照してください。 | 必須          |
| ストリーミングモード | Snowpipe Streaming APIを介してリアルタイムにデータを送信し、行単位でSnowflakeテーブルに直接書き込みます。 | 必須          |

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送を行うためには、SnowflakeのOpen Database Connectivity（ODBC）ドライバーをインストールおよび設定する必要があります。このドライバーはEMQXがSnowflakeのStageにデータを書き込むための通信橋渡しを行い、データの適切なフォーマット、認証、転送を保証します。

詳細は公式の[ODBCドライバー](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な展開と必要なシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法の推奨ではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`スクリプトをローカルマシンにコピーし、`chmod a+x`で実行権限を付与してから`sudo`で実行します。

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリに自動ダウンロードし、ドライバーをインストールします。その後、以下のシステム設定ファイルを更新します。

- `/etc/odbc.ini`：Snowflakeのデータソース設定を追加
- `/etc/odbcinst.ini`：Snowflakeドライバーのパスを登録

**設定例**

`/etc/odbc.ini`の内容を確認するコマンド：

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

`/etc/odbcinst.ini`の内容を確認するコマンド：

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

macOSでSnowflake ODBCドライバーをインストールおよび設定するには、以下の手順に従います。

1. unixODBCをインストール（例）：

   ```
   brew install unixodbc
   ```

2. [iODBCのダウンロードとインストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)。

3. [Snowflake ODBCドライバーのダウンロードとインストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)。

4. 詳細なインストールおよび設定手順は[macOS向けODBCドライバーのインストールと設定](https://docs.snowflake.com/en/developer-guide/odbc/odbc-mac)を参照してください。

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

### ユーザーアカウント作成とSnowflakeリソース設定

アップロードモードに関わらず、Snowflake環境でユーザーアカウント、データベース、関連リソースを設定し、データ取り込み準備を行う必要があります。以下の認証情報は後でEMQXのコネクターおよびSink設定時に使用します。

| 項目                    | 値                                               | 説明                                                         |
| ---------------------- | ------------------------------------------------ | ------------------------------------------------------------ |
| データソース名（DSN）   | `snowflake`（集約モードのみ）                    | `/etc/odbc.ini`に設定したODBC DSN。集約アップロードで使用。 |
| ユーザー名              | `snowpipeuser`                                   | Snowflake接続認証に使用するユーザー。適切な権限を持つ必要あり。 |
| パスワード              | `Snowpipeuser99`                                 | キーペア認証使用時は省略可能。                               |
| データベース名          | `testdatabase`                                   | 対象テーブルが存在するSnowflakeのデータベース名。           |
| スキーマ                | `public`                                         | データベース内のスキーマ名。テーブルやパイプが存在する場所。 |
| ステージ（集約モード）  | `emqx`                                           | データ取り込み前にファイルを保持するSnowflakeステージ名。   |
| パイプ（集約モード）    | `emqx`                                           | ステージからテーブルへデータをロードするパイプ名。           |
| パイプ（ストリーミング）| `emqxstreaming`                                  | Snowpipe Streaming API経由でデータを取り込むためのパイプ名。 |
| プライベートキー        | `file://<path to snowflake_rsa_key.private.pem>` | API認証用JWTの署名に使うRSA秘密鍵のパス。                    |

#### RSAキーペアの生成（集約モードは任意）

Snowflakeは複数の認証方法をサポートしています。EMQXでの認証方法はアップロードモードと接続設定によって異なります。

| アップロードモード | 認証オプション                                               | キーペア必須か |
| ----------------- | ------------------------------------------------------------ | ------------- |
| ストリーミング（HTTPS） | RSAキーペア＋JWT（唯一のサポート方式）                         | 必須          |
| 集約（ODBC）       | ユーザー名／パスワード（DSNまたはEMQX経由）<br />RSAキーペア＋JWT（任意、EMQXのみ設定） | 任意          |

キーペア認証はストリーミングモードで必須であり、EMQXはJWTに署名してSnowflakeのStreaming APIへ安全に認証します。

集約モードではユーザー名／パスワードまたはRSAキーペアのいずれかで認証可能です。認証情報は以下のいずれかの方法で提供できます。

- ダッシュボードのEMQXコネクター設定にユーザー名とパスワードを直接入力
- キーペア認証の場合は秘密鍵のパスを指定
- EMQXでいずれも指定しない場合は、システムのODBC DSN（Linuxの`/etc/odbc.ini`やmacOSの`~/.odbc.ini`）に正しく設定されていることを確認

::: tip

認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。

EMQXでどちらも設定されていない場合は、`/etc/odbc.ini`の認証情報が使用されます。

:::

**例：ユーザー名／パスワードを使った`/etc/odbc.ini`**

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

> この方法により、EMQXは設定内で`DSN`（`snowflake`）を参照し、認証情報を直接含める必要がなくなります。

**キーペア認証を使う場合**

RSAキーペア認証を使用または必須とする場合（例：ストリーミングモード）、以下のコマンドで鍵を生成し設定します。

```bash
# 秘密鍵生成
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt

# 公開鍵生成
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

EMQXがキーペア認証を使用する場合（集約モード・ストリーミングモード両対応）：

- EMQXは秘密鍵でJWTに署名し、安全で検証可能なIDトークンとして使用
- Snowflakeは公開鍵で署名を検証

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLでSnowflakeリソースを設定

RSAキーペア生成後、`aggregated`または`streaming`の取り込み用に必要なSnowflakeオブジェクトをSQLで作成します。

- データベースとテーブルの作成
- ステージとパイプの作成（集約モード）
- ストリーミングパイプの作成（ストリーミングモード）
- ユーザーとロールの作成および権限付与

1. SnowflakeコンソールのSQLワークシートで以下のSQLを実行し、データベース、テーブル、ステージ、パイプを作成します。

   ```sql
   USE ROLE accountadmin;

   -- データベース作成（存在しない場合）
   CREATE DATABASE IF NOT EXISTS testdatabase;

   -- MQTTデータ受け取り用テーブル作成
   CREATE OR REPLACE TABLE testdatabase.public.emqx (
       clientid STRING,
       topic STRING,
       payload STRING,
       publish_received_at TIMESTAMP_LTZ
   );

   -- ファイルアップロード用ステージ作成（集約モードのみ）
   CREATE STAGE IF NOT EXISTS testdatabase.public.emqx
   FILE_FORMAT = (TYPE = CSV PARSE_HEADER = TRUE FIELD_OPTIONALLY_ENCLOSED_BY = '"')
   COPY_OPTIONS = (ON_ERROR = CONTINUE PURGE = TRUE);

   -- ステージからのロード用パイプ作成（集約モード）
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
   - ストリーミングパイプの`$1:field`構文は、EMQX経由で取り込まれるJSONペイロードからフィールドを抽出します。

2. EMQX認証用の専用ユーザー（例：`snowpipeuser`）を作成し、RSA公開鍵をバインドします。

   ```sql
   -- ユーザー作成
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

   PEMファイルの`-----BEGIN PUBLIC KEY-----`と`-----END PUBLIC KEY-----`の行は削除し、残りの内容を改行を保持して記載してください。

   :::

   この鍵はSnowflakeユーザーにアップロードされ、Snowflake内に保存されます。

3. ユーザーに必要なロールを作成し、権限を付与します。

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
   
   -- ユーザーにロールを付与しデフォルトに設定
   GRANT ROLE snowpipe TO USER snowpipeuser;
   ALTER USER snowpipeuser SET DEFAULT_ROLE = snowpipe;
   ```

## 集約モード用Snowflakeコネクターの作成

Snowflake Sinkで集約アップロードモードを使用する場合、Snowflake環境との接続を確立するためにSnowflakeコネクターを作成する必要があります。このコネクターはODBC（DSN経由）を利用してStageに接続します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプで **Snowflake** を選択し、次へ進みます。

4. コネクター名を入力します。英数字の組み合わせで、ここでは `my-snowflake` とします。

5. 接続情報を入力します。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式です。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換えてください。

   - **Account**：Snowflake組織IDとアカウント名をハイフン（`-`）で区切って入力します。これはSnowflakeコンソールのURLにも含まれています。

   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`で設定した`snowflake`を入力します。

   - **Username**：前述の設定で作成した`snowpipeuser`を入力します。

   - **Password**：ODBC経由でユーザー名／パスワード認証を行う場合のパスワード。任意入力です。

     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、
     - `/etc/odbc.ini`に設定するか、
     - キーペア認証を使う場合は空欄にします。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。ここでどちらも設定しない場合は`/etc/odbc.ini`の認証情報が使用されます。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使うRSA秘密鍵の絶対パス。クラスター内の全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`。

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄にします。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシはサポートされていません。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力します。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス
     - **Proxy Port**：プロキシサーバーのポート番号

6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルスイッチをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 高度な設定（任意）：[高度な設定](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**を押してSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、次にルールとSinkを作成してSnowflakeへのデータ書き込みを指定できます。

## Snowflakeストリーミングコネクターの作成

Snowflake Sinkでストリーミングアップロードモードを使用する場合、Snowflake環境との接続を確立するためにSnowflakeストリーミングコネクターを作成します。このコネクターはHTTPSおよびSnowpipe Streaming REST APIを利用します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。  
2. 右上の **Create** ボタンをクリックします。  
3. コネクタータイプで **Snowflake Streaming** を選択し、次へ進みます。  
4. コネクター名を英数字で入力します。ここでは `my-snowflake-streaming` とします。  
5. 接続情報を入力します。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式です。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換えてください。

   - **Account**：Snowflake組織IDとアカウント名をハイフン（`-`）で区切って入力します。SnowflakeコンソールのURLにも含まれています。

   - **Pipe User**：対象パイプを操作する権限を持つSnowflakeユーザー名（例：`snowpipeuser`）。少なくとも`OPERATE`と`MONITOR`権限を持つ必要があります。

   - **Private Key Path**：EMQXがJWT署名に使うRSA秘密鍵。PEM形式の秘密鍵全文を文字列として直接貼り付けるか、`file://`で始まる秘密鍵ファイルのパスを指定します。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`。

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄にします。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシはサポートされていません。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力します。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス  
     - **Proxy Port**：プロキシサーバーのポート番号  

6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルスイッチをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 高度な設定（任意）：[高度な設定](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**を押してSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、次にルールとSinkを作成してSnowflakeへのデータ書き込みを指定できます。

## Snowflake Sinkを使ったルールの作成

このセクションでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sinkを通じてSnowflakeに書き込む方法を示します。

### SQLを指定したルールの作成

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

   SQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてルールSQLの結果を学習・テストできます。

   :::

   ::: tip

   Snowflake連携では、選択するフィールドがSnowflakeのテーブルのカラム数および名前と完全に一致することが重要です。余分なフィールドの追加や`*`での選択は避けてください。

   :::

4. ルールにアクションを追加し、Sinkを設定します。

   - 集約アップロードモードでSnowflakeに書き込む場合は、[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照してください。
   - ストリーミングアップロードモードでSnowflakeに書き込む場合は、[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照してください。

5. アクション追加後、**Action Outputs**セクションに新しいSinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックしてルール作成を完了します。

これでルール作成が完了し、**Rules**ページに新しいルールが表示され、**Actions (Sink)**タブに新しいSnowflake Sinkが追加されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを視覚的に確認でき、トピック`t/#`のメッセージがルール`my_rule`で解析されてSnowflakeに書き込まれる流れを把握できます。

### 集約アップロードモードでSnowflake Sinkを追加

このセクションでは、集約アップロードモードを使って処理結果をSnowflakeに書き込むSinkをルールに追加する方法を示します。このモードは複数のルールトリガー結果を単一ファイル（例：CSVファイル）にまとめてSnowflakeにアップロードし、ファイル数を減らして書き込み効率を向上させます。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加します。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成してルールに追加します。

3. Sink名（例：`snowflake_sink`）と簡単な説明を入力します。

4. 先に作成した`my-snowflake`コネクターを**Connectors**ドロップダウンから選択します。ドロップダウン横の作成ボタンで新規コネクターをポップアップで素早く作成することも可能です。必要な設定パラメーターは[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照してください。

5. 集約アップロードモードの設定を行います。

   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース名。
   - **Schema**：`public`。`testdatabase`内のデータテーブルが存在するスキーマ名。
   - **Stage**：`emqx`。Snowflakeでデータをテーブルにロードする前に保持するステージ名。
   - **Pipe**：`emqx`。ステージからテーブルへのロードを自動化するパイプ名。
   - **Pipe User**：パイプ操作権限を持つSnowflakeユーザー名。例：`snowpipeuser`。
   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSA秘密鍵。以下のいずれかの形式で指定可能です。
     - **プレーンテキスト**：PEM形式の秘密鍵全文を文字列として貼り付け。
     - **ファイルパス**：`file://`で始まる秘密鍵ファイルのパス。クラスター内全ノードで同一パスかつEMQXアプリユーザーがアクセス可能である必要があります。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`。
   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄にします。

   - **Aggregation Upload Format**：現在は`csv`のみ対応。データはカンマ区切りCSV形式でSnowflakeにステージされます。
   - **Column Order**：ドロップダウンからカラムの並び順を選択。生成されるCSVファイルは選択したカラム順にソートされ、未選択カラムはアルファベット順に続きます。
   - **Max Records**：集約をトリガーする最大レコード数。例：`1000`に設定すると1000件集まった時点でファイルをアップロードし、時間間隔をリセットします。
   - **Time Interval**：集約を行う時間間隔（秒）。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにデータをアップロードし、最大レコード数をリセットします。
   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシはサポートされていません。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力します。
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス
     - **Proxy Port**：プロキシサーバーのポート番号

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

7. **高度な設定**を展開し、必要に応じて詳細設定を行います（任意）。詳細は[高度な設定](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**を押してSinkがSnowflakeサーバーに接続できるかテスト可能です。

9. **Create**ボタンをクリックしてSink作成を完了します。作成成功後はルール作成画面に戻り、新しいSinkがルールアクションに追加されます。

### ストリーミングアップロードモードでSnowflake Sinkを追加

このセクションでは、ストリーミングアップロードモードを使って処理結果をSnowflakeに書き込むSinkをルールに追加する方法を示します。このモードはSnowpipe Streaming APIを利用したリアルタイム取り込みを可能にします。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加します。  
2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成してルールに追加します。  
3. Sink名（例：`snowflake_sink_streaming`）と簡単な説明を入力します。  
4. 先に作成した`my-snowflake-streaming`コネクターをコネクタードロップダウンから選択します。ドロップダウン横の作成ボタンで新規コネクターをポップアップで素早く作成することも可能です。必要な設定パラメーターは[ストリーミングコネクターの作成](#create-a-snowflake-streaming-connector)を参照してください。  
5. ストリーミングアップロードモードの設定を行います。  
   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース名。  
   - **Schema**：`public`。`testdatabase`内のデータテーブルが存在するスキーマ名。  
   - **Pipe**：`emqxstreaming`。SQLで作成したSnowflakeストリーミングパイプ名。Snowflakeで定義した名前と完全に一致させる必要があります。  
   - **HTTP Pipelining**：応答を待たずに送信可能な最大HTTPリクエスト数。デフォルトは`100`。  
   - **Connect Timeout**：Snowflakeへの接続確立のタイムアウト秒数。デフォルトは`15`秒。  
   - **Connection Pool Size**：EMQXがSnowflakeに対して同時に維持可能な接続数の最大値。デフォルトは`8`。  
   - **Max Inactive**：HTTPクライアントが再接続を試みるまでの最大非アクティブ時間。EMQX 6.0.4以降は、リクエスト待ち状態で60秒間送信がない場合に再接続します。60秒より長い値を設定すると優先されます。レスポンスが60秒以上かかる可能性がある場合は、最大応答時間以上に設定してください。デフォルトは`10`秒。  
6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。  
7. **高度な設定**を展開し、必要に応じて詳細設定を行います（任意）。詳細は[高度な設定](#advanced-settings)を参照してください。  
8. **Create**をクリックする前に、**Test Connectivity**を押してSinkがSnowflakeサーバーに接続できるかテスト可能です。  
9. **Create**ボタンをクリックしてSink作成を完了します。作成成功後はルール作成画面に戻り、新しいSinkがルールアクションに追加されます。

## ルールのテスト

このセクションでは、設定したルールのテスト方法を示します。

### テストメッセージのパブリッシュ

MQTTXを使ってトピック`t/1`にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返して複数のテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeにデータが正常に書き込まれているか確認します。

1. SnowflakeのWebインターフェースにログインします。

2. Snowflakeコンソールで以下のSQLを実行し、ルールで書き込まれた`emqx`テーブルのデータを確認します。

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   これにより、`clientid`、`topic`、`payload`、`publish_received_at`フィールドを含む全レコードが表示されます。

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）やトピック、タイムスタンプなどのメタデータが確認できるはずです。

## 高度な設定

このセクションでは、Snowflake Sinkの高度な設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、必要に応じて以下のパラメーターを調整できます。

| 項目名                          | 説明                                                         | デフォルト値    |
| ------------------------------ | ------------------------------------------------------------ | --------------- |
| **Buffer Pool Size**            | EMQXとSnowflake間のデータフローを管理するバッファワーカープロセスの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化しスムーズなデータ伝送を保証します。 | `16`           |
| **Request TTL**                 | バッファに入ったリクエストが有効とみなされる最大秒数です。リクエストがこのTTLを超えてバッファに残るか、送信後にSnowflakeからの応答やアックがタイムリーに得られない場合、リクエストは期限切れと判断されます。 | `45`秒         |
| **Health Check Interval**       | SinkがSnowflakeとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`秒         |
| **Health Check Interval Jitter**| 複数ノードが同時にヘルスチェックを開始するのを防ぐため、基本間隔に加える一様ランダム遅延です。複数のアクションやソースが同一コネクターを共有する場合、ジッターを有効にするとヘルスチェック開始時刻が分散されます。 | `0`ミリ秒      |
| **Health Check Timeout**        | Snowflakeとの接続ヘルスチェックのタイムアウト時間（秒）を指定します。 | `60`秒         |
| **Max Buffer Queue Size**       | Snowflake Sinkの各バッファワーカーがバッファリング可能な最大バイト数です。バッファワーカーはデータを一時的に保持し、Snowflakeへの送信を効率化します。システム性能やデータ伝送要件に応じて調整してください。 | `256`MB        |
| **Query Mode**                  | 同期（`synchronous`）または非同期（`asynchronous`）のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードではSnowflakeへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受信する可能性があります。 | `Asynchronous` |
| **Batch Size**                  | EMQXからSnowflakeへ一度に送信するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送効率と性能を微調整可能です。<br />`1`に設定するとレコードを個別に送信し、バッチングしません。 | `100`          |
| **Inflight Window**             | 送信済みでまだ応答やアックを受け取っていない「インフライト」キューリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合に特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`          |
