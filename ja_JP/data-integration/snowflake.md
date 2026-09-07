# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/)は、クラウドベースのデータプラットフォームであり、データウェアハウジング、分析、および安全なデータ共有のための高いスケーラビリティと柔軟性を提供します。構造化データおよび半構造化データの処理に優れており、大量のデータを格納しつつ、高速なクエリ性能とさまざまなツールやサービスとのシームレスな統合を実現するよう設計されています。

本ページでは、EMQXとSnowflake間のデータ統合について詳しく紹介し、ルールとSinkの作成方法について実践的なガイダンスを提供します。

## 動作の仕組み

EMQXにおけるSnowflakeデータ統合は、複雑なIoTビジネスワークフローをサポートするために簡単に設定可能な即利用可能な機能です。典型的なIoTアプリケーションでは、EMQXがデバイス接続とメッセージ送信を担うIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、格納、分析を行うデータストレージおよび処理プラットフォームとして役割を果たします。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスのイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブル内のデータにアクセスできます。具体的なワークフローは以下の通りです。

1. **デバイスのEMQXへの接続**：IoTデバイスはMQTTプロトコルで正常に接続するとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレスなどの識別情報が含まれます。

2. **デバイスのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジン内で比較処理を行います。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールにマッチしたメッセージやイベントに対して、データフォーマット変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。

4. **Snowflakeへの書き込み**：ルールはメッセージデータをSnowflakeに書き込むアクションをトリガーします。メッセージをファイルにバッチングしてStageとPipe経由でロードする（集約モード）か、Snowpipe Streaming APIを使って直接ストリーミングする（ストリーミングモード）方法があります。

イベントやメッセージデータがSnowflakeに書き込まれた後は、以下のようなビジネスや技術的用途に活用できます。

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの利用を保証します。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を行い、予知保全、運用インサイト、デバイス性能評価を可能にします。

## 特徴と利点

EMQXのSnowflakeデータ統合を利用することで、以下の特徴と利点が得られます。

- **メッセージ変換**：メッセージはEMQXのルール内で高度な処理や変換を経てからSnowflakeに書き込まれるため、後続の格納や利用が容易になります。
- **柔軟なデータ操作**：Snowflake Sinkは、Snowflakeに書き込む特定フィールドを選択可能で、ビジネスニーズに応じた効率的かつ動的なストレージ構成を実現します。
- **統合されたビジネスプロセス**：Snowflake Sinkにより、デバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせることができ、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は、従来のデータベースに比べて低コストで長期データ保持に最適であり、大量のIoTデータ保存に適しています。

これらの特徴により、効率的で信頼性の高いスケーラブルなIoTアプリケーションの構築と、ビジネスの意思決定や最適化に役立てることが可能です。

## はじめる前に

このセクションでは、EMQXでSnowflake Sinkを作成する前に必要な準備について説明します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の基本概念の理解。
- 管理者権限を持つ動作中のSnowflakeアカウント。

### アップロードモードの選択

::: tip

モードの選択は、EMQXおよびSnowflake環境の設定方法に影響するため、最初に決定してください。

:::

EMQXはSnowflakeへのデータ送信に以下2つのモードをサポートしています。

| モード       | 説明                                                         | ODBC必要性   |
| ---------- | ------------------------------------------------------------ | ----------- |
| 集約（Aggregated） | EMQXはMQTTメッセージをローカルファイルにバッファリングし、それをSnowflakeのステージにアップロードします。`COPY INTO`文で設定されたパイプが自動的にステージファイルをターゲットテーブルにロードします。詳細は[Snowflake Snowpipeドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)を参照してください。 | 必須         |
| ストリーミング（Streaming） | Snowpipe Streaming APIを介してリアルタイムにデータを送信し、行を直接Snowflakeテーブルに書き込みます。 | 必須         |

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送を行うためには、SnowflakeのOpen Database Connectivity（ODBC）ドライバーのインストールと設定が必要です。このドライバーはEMQXがSnowflakeのステージにデータを書き込むための通信橋渡し役を果たし、データの適切なフォーマット化、認証、転送を保証します。

詳細は公式の[ODBCドライバー](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページと[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な展開と必要なシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法として推奨するものではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`スクリプトをローカルにコピーし、`chmod a+x`で実行権限を付与して、`sudo`で実行します。

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリに自動ダウンロードし、ドライバーをインストールして以下のシステム設定ファイルを更新します。

- `/etc/odbc.ini`：Snowflakeのデータソース設定を追加
- `/etc/odbcinst.ini`：Snowflakeドライバーのパスを登録

**設定例**

`/etc/odbc.ini`の内容確認コマンド例：

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

`/etc/odbcinst.ini`の内容確認コマンド例：

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

### ユーザーアカウント作成とSnowflakeリソースのセットアップ

アップロードモードに関わらず、Snowflake環境においてユーザーアカウント、データベース、関連リソースを設定し、データ取り込みに必要な環境を整備する必要があります。以下の認証情報は後でEMQXのコネクターやSink設定時に使用します。

| 項目名                  | 値                                                  | 説明                                                         |
| ---------------------- | -------------------------------------------------- | ------------------------------------------------------------ |
| データソース名（DSN）  | `snowflake`（集約モードのみ）                      | `/etc/odbc.ini`に設定されたODBC DSN。集約アップロード用。    |
| ユーザー名              | `snowpipeuser`                                     | Snowflake接続認証に使用するユーザー。適切な権限が必要。       |
| パスワード              | `Snowpipeuser99`                                   | キーペア認証時は省略可能。                                   |
| データベース名          | `testdatabase`                                     | 対象テーブルが存在するSnowflakeのデータベース名。             |
| スキーマ                | `public`                                           | データベース内のスキーマ名。テーブルやパイプが存在する場所。   |
| ステージ（集約モード）  | `emqx`                                             | データ取り込み前にファイルを保持するSnowflakeステージ名。      |
| パイプ（集約モード）    | `emqx`                                             | ステージからテーブルへデータをロードするパイプ名。             |
| パイプ（ストリーミング）| `emqxstreaming`                                    | Snowpipe Streaming API経由でデータ取り込みするためのパイプ名。 |
| プライベートキー        | `file://<path to snowflake_rsa_key.private.pem>`  | API認証用JWTの署名に使うRSAプライベートキーのパス。            |

#### RSAキーペアの生成（集約モードは任意）

Snowflakeは複数の認証方式をサポートしており、EMQXではアップロードモードや接続設定に応じて認証方法を選択します。

| アップロードモード   | 認証方式                                                         | キーペア必須か |
| ------------------- | ---------------------------------------------------------------- | ------------- |
| ストリーミング（HTTPS） | RSAキーペア＋JWT（唯一サポートされる方式）                      | 必須          |
| 集約（ODBC）         | ユーザー名/パスワード（DSNまたはEMQX経由）<br />RSAキーペア＋JWT（任意、EMQX設定のみ） | 任意          |

キーペア認証はストリーミングモードで必須であり、EMQXがJWTを署名してSnowflakeのStreaming APIに安全に認証します。

集約モードではユーザー名/パスワードまたはRSAキーペアのいずれかで認証可能です。認証情報は以下のいずれかで提供します。

- ダッシュボードのEMQXコネクター設定で直接ユーザー名とパスワードを入力。
- キーペア認証を使う場合はプライベートRSAキーのパスを指定。
- どちらも指定しない場合は、システムのODBC DSN（Linuxの`/etc/odbc.ini`やmacOSの`~/.odbc.ini`）に正しく設定されていることを確認。

::: tip

認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。

EMQXでどちらも設定されていない場合は、`/etc/odbc.ini`の認証情報が使用されます。

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

> この方法により、EMQXは設定内で`DSN`（`snowflake`）を参照し、認証情報を直接含める必要がなくなります。

**キーペア認証を使う場合**

RSAキーペア認証を使う（または使う必要がある）場合（例：ストリーミングモード）、以下のコマンドで鍵を生成します。

```bash
# 秘密鍵の生成
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt

# 公開鍵の生成
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

EMQXがキーペア認証を使う場合（集約・ストリーミング両モード対応）：

- EMQXは秘密鍵でJWTに署名し、安全かつ検証可能なIDトークンとして利用。
- Snowflakeは公開鍵で署名を検証。

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLでSnowflakeリソースをセットアップ

RSAキーペア生成後、集約またはストリーミング取り込み用に必要なSnowflakeオブジェクトをSQLで作成します。

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

   -- ステージからのコピーを行うパイプ作成（集約モード）
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

   - パイプ内の`COPY INTO`文により、Snowflakeがステージまたはストリーミングされたデータを自動的にテーブルにロードします。
   - ストリーミングパイプの`$1:field`構文はEMQX経由で取り込まれるJSONペイロードからフィールドを抽出します。

2. EMQXが認証に使う専用ユーザー（例：`snowpipeuser`）を作成し、そのユーザーにRSA公開鍵をバインドします。

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

   PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`行は削除し、改行を保持したまま中身のみを記載してください。

   :::

   この鍵はSnowflakeユーザーにアップロードされ、Snowflake内部に保存されます。

3. ユーザーにSnowflakeリソース管理権限を付与するロールを作成し、割り当てます。

   ```sql
   CREATE OR REPLACE ROLE snowpipe;
   
   -- データベースとスキーマの使用権限付与
   GRANT USAGE ON DATABASE testdatabase TO ROLE snowpipe;
   GRANT USAGE ON SCHEMA testdatabase.public TO ROLE snowpipe;
   GRANT INSERT, SELECT ON testdatabase.public.emqx TO ROLE snowpipe;
   
   -- 集約モード用にステージとパイプへのアクセス権限付与
   GRANT READ, WRITE ON STAGE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqx TO ROLE snowpipe;
   
   -- ストリーミングモード用にストリーミングパイプへの権限付与
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqxstreaming TO ROLE snowpipe;
   
   -- ユーザーにロールを割り当て、デフォルトロールに設定
   GRANT ROLE snowpipe TO USER snowpipeuser;
   ALTER USER snowpipeuser SET DEFAULT_ROLE = snowpipe;
   ```

## 集約モード用Snowflakeコネクターの作成

Snowflake Sinkで集約アップロードモードを使う場合は、Snowflake環境との接続を確立するためにSnowflakeコネクターを作成する必要があります。このコネクターはODBC（DSN経由）を使い、ステージを通じて接続します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake** を選択し、次へ。

4. コネクター名を入力（英数字の組み合わせ）。ここでは `my-snowflake` と入力。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換える。

   - **Account**：Snowflake組織IDとアカウント名をハイフン（`-`）で区切って入力。SnowflakeコンソールのURLに含まれる。

   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`で設定した`snowflake`を入力。

   - **Username**：セットアップ時に作成した`snowpipeuser`を入力。

   - **Password**：ODBC経由でユーザー名/パスワード認証する場合のパスワード。任意入力。

     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、

     - `/etc/odbc.ini`に設定するか、

     - キーペア認証を使う場合は空欄のままにする。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。ここにどちらも設定しない場合は、`/etc/odbc.ini`の認証情報が使われます。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使うRSA秘密鍵の絶対ファイルパス。クラスター内の全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能。

9. **Create**ボタンをクリックしてコネクター作成を完了。

これでコネクターが作成され、ルールとSinkの作成に進めます。

## ストリーミングモード用Snowflakeコネクターの作成

Snowflake Sinkでストリーミングアップロードモードを使う場合は、Snowflake環境との接続を確立するためにSnowflakeストリーミングコネクターを作成します。このコネクターはHTTPSおよびSnowpipe Streaming REST APIを利用します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake Streaming** を選択し、次へ。

4. コネクター名を入力（英数字の組み合わせ）。ここでは `my-snowflake-streaming` と入力。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換える。

   - **Account**：Snowflake組織IDとアカウント名をハイフン（`-`）で区切って入力。SnowflakeコンソールのURLに含まれる。

   - **Pipe User**：対象パイプを操作可能な権限を持つSnowflakeユーザー名。例：`snowpipeuser`。少なくとも`OPERATE`と`MONITOR`権限が必要。

   - **Private Key Path**：EMQXがJWT署名に使うRSA秘密鍵。PEM形式の鍵全文を文字列として貼り付けるか、`file://`で始まる秘密鍵ファイルのパスを指定可能。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能。

9. **Create**ボタンをクリックしてコネクター作成を完了。

これでコネクターが作成され、ルールとSinkの作成に進めます。

## Snowflake Sinkを使ったルールの作成

このセクションでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sink経由でSnowflakeに書き込む方法を説明します。

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

   SQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてルールSQLの結果を学習・テストできます。

   :::
   ::: tip

   Snowflake統合では、選択するフィールドがSnowflakeのテーブルの列数および列名と完全に一致することが重要です。余計なフィールドを追加したり、`*`で選択しないようにしてください。

   :::

4. ルールにアクションを追加し、Sinkを設定します。

   - 集約アップロードモードでSnowflakeに書き込む場合は、[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照。

   - ストリーミングアップロードモードでSnowflakeに書き込む場合は、[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照。

5. アクション追加後、**Action Outputs**セクションに新規Sinkが表示されます。**Create Rule**ページで**Save**をクリックし、ルール作成を完了。

これでルールが作成されました。**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブで新規Snowflake Sinkを確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されてSnowflakeに書き込まれる様子を視覚的に確認できます。

### 集約アップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、集約アップロードモードで処理結果をSnowflakeに書き込む方法を説明します。このモードは複数のルールトリガー結果を単一ファイル（例：CSVファイル）にまとめてアップロードし、ファイル数を減らして書き込み効率を向上させます。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sink名（例：`snowflake_sink`）と簡単な説明を入力。

4. **Connectors**ドロップダウンから先に作成した`my-snowflake`コネクターを選択。隣の作成ボタンをクリックしてポップアップで新規コネクターを作成することも可能。必要な設定パラメーターは[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照。

5. 集約アップロードモードの設定を行う。

   - **Database Name**：`testdatabase`を入力。EMQXデータ格納用のSnowflakeデータベース。

   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルがあるスキーマ。

   - **Stage**：`emqx`を入力。Snowflakeで作成したデータアップロード用ステージ。

   - **Pipe**：`emqx`を入力。ステージからテーブルへの自動ロードを行うパイプ。

   - **Pipe User**：`snowpipeuser`を入力。パイプ管理権限を持つSnowflakeユーザー。

   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSA秘密鍵。以下いずれかの形式で指定可能。

     - **プレーンテキスト**：PEM形式の秘密鍵全文を文字列として直接貼り付け。

     - **ファイルパス**：`file://`で始まる秘密鍵ファイルのパスを指定。クラスター内全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要あり。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄。

   - **Aggregation Upload Format**：現在は`csv`のみ対応。データはカンマ区切りCSV形式でSnowflakeにステージされる。

   - **Column Order**：ドロップダウンから列の並び順を選択。生成されるCSVファイルは選択した列順にソートされ、未選択列はアルファベット順にソートされる。

   - **Max Records**：集約をトリガーする最大レコード数。例：`1000`に設定すると1000レコード収集後にアップロード。最大レコード数到達時に1ファイルの集約が完了しアップロードされ、時間間隔がリセットされる。

   - **Time Interval**：集約を行う時間間隔（秒）。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにデータをアップロードし、最大レコード数がリセットされる。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeサーバーへの接続テストが可能。

9. **Create**ボタンをクリックしSink作成を完了。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加される。

### ストリーミングアップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、ストリーミングアップロードモードで処理結果をSnowflakeに書き込む方法を説明します。このモードはSnowpipe Streaming APIを使いリアルタイム取り込みを可能にします。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sink名（例：`snowflake_sink_streaming`）と簡単な説明を入力。

4. コネクタードロップダウンから先に作成した`my-snowflake-streaming`コネクターを選択。隣の作成ボタンをクリックしてポップアップで新規コネクターを作成することも可能。必要な設定パラメーターは[ストリーミングモード用Snowflakeコネクターの作成](#create-a-snowflake-streaming-connector)を参照。

5. ストリーミングアップロードモードの設定を行う。

   - **Database Name**：`testdatabase`を入力。EMQXデータ格納用のSnowflakeデータベース。

   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルがあるスキーマ。

   - **Pipe**：`emqxstreaming`を入力。SQLで作成したSnowflakeストリーミングパイプ名。Snowflakeで定義した名前と完全一致させる必要あり。

   - **HTTP Pipelining**：応答を待たずに送信可能なHTTPリクエストの最大数。デフォルト：`100`。

   - **Connect Timeout**：Snowflakeへの接続確立のタイムアウト時間（秒）。デフォルト：`15`秒。

   - **Connection Pool Size**：このSinkがSnowflakeに維持可能な同時接続数の最大値。デフォルト：`8`。

   - **Max Inactive**：アイドル状態の接続を閉じるまでの最大待機時間（秒）。デフォルト：`10`秒。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeサーバーへの接続テストが可能。

9. **Create**ボタンをクリックしSink作成を完了。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加される。

## ルールのテスト

このセクションでは、設定済みルールのテスト方法を示します。

### テストメッセージのパブリッシュ

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返してテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeにデータが正常に書き込まれたかをSnowflakeインスタンスにアクセスして確認します。

1. SnowflakeのWebインターフェースを開き、認証情報でログイン。

2. Snowflakeコンソールで以下のSQLを実行し、ルールで書き込まれた`emqx`テーブルのデータを確認。

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   `emqx`テーブルにアップロードされたすべてのレコード（`clientid`、`topic`、`payload`、`publish_received_at`フィールドを含む）が表示されます。

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）や、トピック、タイムスタンプなどのメタデータが確認できるはずです。

## 詳細設定

このセクションでは、Snowflake Sinkの詳細設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、用途に応じて以下のパラメーターを調整可能です。

| 項目名                         | 説明                                                         | デフォルト値   |
| ------------------------------ | ------------------------------------------------------------ | ------------- |
| **Buffer Pool Size**            | EMQXとSnowflake間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保持・処理し、送信前のパフォーマンス最適化とスムーズなデータ伝送を支えます。 | `16`          |
| **Request TTL**                 | バッファに入ったリクエストが有効とみなされる最大時間（秒）です。TTLを超えてバッファ内に滞留するか、Snowflakeからの応答・アックがタイムリーに得られない場合、リクエストは期限切れと判断されます。 | `45`          |
| **Health Check Interval**       | SinkがSnowflakeとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`          |
| **Health Check Interval Jitter**| 複数ノードが同時にヘルスチェックを開始するのを避けるため、基本間隔に加える一様ランダム遅延です。複数のアクションやソースが同一コネクターを共有する場合に有効です。 | `0` ミリ秒    |
| **Health Check Timeout**        | Snowflakeとの接続ヘルスチェックのタイムアウト時間（秒）を指定します。 | `60`          |
| **Max Buffer Queue Size**       | Snowflake Sinkの各バッファワーカーが一時的に保持可能な最大バイト数を指定します。バッファワーカーはデータ送信前の中継役として機能し、システム性能やデータ伝送要件に応じて調整可能です。 | `256` MB      |
| **Query Mode**                  | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信の最適化を図ります。非同期モードではSnowflakeへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受け取る可能性があります。 | `Asynchronous`|
| **Batch Size**                  | EMQXからSnowflakeへ一度に送信するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送効率と性能を最適化可能です。<br />`1`に設定するとバッチ化せず単一レコードずつ送信します。 | `100`         |
| **Inflight Window**             | 送信済みだが応答やアックをまだ受け取っていない「インフライト」キューリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の際に重要で、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定します。 | `100`         |
