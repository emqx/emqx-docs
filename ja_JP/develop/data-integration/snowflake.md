# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/) は、クラウドベースのデータプラットフォームであり、高いスケーラビリティと柔軟性を備えたデータウェアハウジング、分析、セキュアなデータ共有のソリューションを提供します。構造化データおよび半構造化データの処理に優れており、大量のデータを格納しつつ高速なクエリ性能と多様なツールやサービスとのシームレスな統合を実現しています。

本ページでは、EMQXとSnowflake間のデータ統合について詳しく紹介し、ルールとSinkの作成方法について実践的なガイダンスを提供します。

## 動作概要

EMQXにおけるSnowflakeデータ統合はすぐに使える機能であり、複雑なIoTビジネスワークフローを簡単にサポートするよう設定可能です。典型的なIoTアプリケーションでは、EMQXがデバイス接続とメッセージ送受信を担当するIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、格納、分析を行うデータストレージおよび処理プラットフォームとして機能します。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスのイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブル内のデータにアクセス可能です。具体的なワークフローは以下の通りです：

1. **デバイスのEMQX接続**：IoTデバイスはMQTTプロトコルで正常に接続されるとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレスなどの識別情報が含まれます。

2. **デバイスからのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジン内で比較処理を行います。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールにマッチしたメッセージやイベントに対し、データフォーマット変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。

4. **Snowflakeへの書き込み**：ルールがトリガーされると、メッセージデータをSnowflakeに書き込みます。書き込み方法は、メッセージをファイルにバッチングしてStageとPipe経由でロードする（集約モード）、またはSnowpipe Streaming APIを使って直接ストリーミングする（ストリーミングモード）方法があります。

イベントやメッセージデータがSnowflakeに書き込まれた後は、以下のような多様なビジネス・技術用途に活用できます：

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの利用を保証します。
- **データ分析**：Snowflakeのデータウェアハウジングと分析機能を活用し、リアルタイムまたはバッチ分析を行い、予知保全、運用インサイト、デバイス性能評価を実現します。

## 特長と利点

EMQXのSnowflakeデータ統合を利用することで、以下の特長と利点をビジネスにもたらします：

- **メッセージ変換**：メッセージはEMQXのルール内で多様な処理や変換を経てからSnowflakeに書き込まれるため、後続の保存や利用が容易になります。
- **柔軟なデータ操作**：Snowflake Sinkは書き込むフィールドを選択可能であり、ビジネスニーズに応じた効率的かつ動的なストレージ構成が可能です。
- **統合されたビジネスプロセス**：Snowflake Sinkにより、デバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせ、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は従来のデータベースに比べ低コストで長期データ保持に最適であり、大量のIoTデータ保存に適しています。

これらの特長により、効率的で信頼性が高くスケーラブルなIoTアプリケーションを構築し、ビジネス上の意思決定や最適化に役立てることが可能です。

## はじめる前に

このセクションでは、EMQXでSnowflake Sinkを作成する前に必要な準備について説明します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の基本概念の理解
- 管理者権限を持つSnowflakeアカウントの用意

### アップロードモードの選択

::: tip

最初にモードを選択してください。これによりEMQXおよびSnowflake環境の設定方法が決まります。

:::

EMQXはSnowflakeへのデータ送信に以下の2つのモードをサポートしています：

| モード       | 説明                                                         | ODBC必要性    |
| ------------ | ------------------------------------------------------------ | ------------ |
| 集約モード   | EMQXはMQTTメッセージをローカルファイルにバッファリングし、SnowflakeのStageにアップロードします。`COPY INTO`文で設定されたPipeが自動的にステージファイルをターゲットテーブルにロードします。詳細は[Snowflake Snowpipeドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)を参照してください。 | 必要         |
| ストリーミングモード | Snowpipe Streaming APIを介してリアルタイムにデータを送信し、行を直接Snowflakeテーブルに書き込みます。 | 必要         |

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送を行うためには、SnowflakeのODBCドライバーをインストールおよび設定する必要があります。このドライバーはEMQXがSnowflakeのStageにデータを書き込むための通信ブリッジとして機能し、データの適切なフォーマット、認証、転送を保証します。

詳細は公式の[ODBCドライバー](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な導入と必要なシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法として推奨するものではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`をローカルにコピーし、`chmod a+x`で実行可能にしてから`sudo`で実行します：

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリにダウンロードし、ドライバーをインストール後、以下のシステム設定ファイルを更新します：

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

macOSでSnowflake ODBCドライバーをインストール・設定する手順は以下の通りです：

1. unixODBCをインストール（例）：

   ```
   brew install unixodbc
   ```

2. [iODBCのダウンロードとインストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)

3. [Snowflake ODBCドライバーのダウンロードとインストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)

4. 詳細なインストール・設定手順は[macOS向けODBCドライバーのインストールと設定](https://docs.snowflake.com/en/developer-guide/odbc/odbc-mac)を参照

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

アップロードモードに関わらず、Snowflake環境においてユーザーアカウント、データベース、関連リソースの設定が必要です。以下の認証情報は後にEMQXのコネクターおよびSink設定で使用します：

| 項目名                  | 値                                                    | 説明                                                         |
| ----------------------- | ----------------------------------------------------- | ------------------------------------------------------------ |
| Data Source Name (DSN)  | `snowflake`（集約モードのみ）                         | `/etc/odbc.ini`に設定したODBC DSN。集約アップロードで使用。 |
| ユーザー名              | `snowpipeuser`                                        | Snowflake接続認証に使用するユーザー。適切な権限が必要。     |
| パスワード              | `Snowpipeuser99`                                      | キーペア認証を使う場合は省略可能。                           |
| データベース名          | `testdatabase`                                        | 対象テーブルが存在するSnowflakeデータベース。               |
| スキーマ                | `public`                                              | データベース内のスキーマ。テーブルやパイプが存在する場所。  |
| ステージ（集約モード）  | `emqx`                                                | データ取り込み前にファイルを保持するSnowflakeステージ。     |
| パイプ（集約モード）    | `emqx`                                                | ステージからテーブルへデータをロードするパイプ。             |
| パイプ（ストリーミング）| `emqxstreaming`                                       | Snowpipe Streaming APIでデータ取り込み用に作成したパイプ。  |
| プライベートキー        | `file://<path to snowflake_rsa_key.private.pem>`     | API認証用JWTの署名に使うRSAプライベートキーのパス。          |

#### RSAキーペアの生成（集約モードは任意）

Snowflakeは複数の認証方式をサポートしています。EMQXでの認証方式はアップロードモードや接続設定に依存します：

| アップロードモード      | 認証オプション                                              | キーペア必須か  |
| ----------------------- | ----------------------------------------------------------- | -------------- |
| ストリーミング（HTTPS） | RSAキーペア＋JWT（唯一のサポート方式）                      | 必須           |
| 集約（ODBC）            | ユーザー名/パスワード（DSNまたはEMQX経由）<br />RSAキーペア＋JWT（任意、EMQX設定のみ） | 任意           |

キーペア認証はストリーミングモードでのみ必須であり、EMQXはJWTに署名してSnowflake Streaming APIに安全に認証します。

集約モードではユーザー名/パスワードまたはRSAキーペアのいずれかで認証可能です。認証情報は以下のいずれかで指定します：

- ダッシュボードのEMQXコネクター設定にユーザー名とパスワードを直接入力
- キーペア認証を使う場合はプライベートRSAキーのパスを指定
- EMQXにいずれも設定しない場合は、システムのODBC DSN（Linuxなら`/etc/odbc.ini`、macOSなら`~/.odbc.ini`）に正しく設定されていることを確認

::: tip

認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。

EMQXにどちらも設定されていない場合、コネクターは`/etc/odbc.ini`の認証情報を使用します。

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

**キーペア認証を使う場合**

RSAキーペア認証を使う（例：ストリーミングモード）場合は、以下のコマンドで鍵を生成し設定します：

```bash
# 秘密鍵の生成
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt

# 公開鍵の生成
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

EMQXがキーペア認証を使う場合（集約・ストリーミング両モード対応）：

- EMQXは秘密鍵でJWTに署名し、安全で検証可能なIDトークンとして利用
- Snowflakeは公開鍵でトークンの署名を検証

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLでSnowflakeリソースをセットアップ

RSAキーペア生成後、`aggregated`または`streaming`取り込み用のSnowflakeオブジェクトをSQLで作成します。

対象は以下を含みます：

- データベースとテーブルの作成
- ステージとパイプの作成（集約モード）
- ストリーミングパイプの作成（ストリーミングモード）
- ユーザーとロールの作成および権限付与

1. SnowflakeコンソールのSQLワークシートで以下SQLを実行し、データベース、テーブル、ステージ、パイプを作成：

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

   -- ステージからのロード用パイプ作成（集約モード）
   CREATE PIPE IF NOT EXISTS testdatabase.public.emqx AS
   COPY INTO testdatabase.public.emqx
   FROM @testdatabase.public.emqx
   MATCH_BY_COLUMN_NAME = CASE_INSENSITIVE;

   -- ストリーミング用パイプ作成（直接取り込み）
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
   - ストリーミングパイプの`$1:field`構文は、EMQX経由で取り込まれたJSONペイロードからフィールドを抽出します。

2. EMQX認証用の専用ユーザー（例：`snowpipeuser`）を作成し、RSA公開鍵をバインド：

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

   PEMファイルの`-----BEGIN PUBLIC KEY-----`と`-----END PUBLIC KEY-----`の行は削除し、残りの内容を改行を保持して記載してください。

   :::

   この鍵はSnowflakeユーザーにアップロードされ、Snowflake内に保存されます。

3. ユーザーに必要なロールを作成し、権限を付与：

   ```sql
   CREATE OR REPLACE ROLE snowpipe;

   -- データベースとスキーマの使用権限
   GRANT USAGE ON DATABASE testdatabase TO ROLE snowpipe;
   GRANT USAGE ON SCHEMA testdatabase.public TO ROLE snowpipe;
   GRANT INSERT, SELECT ON testdatabase.public.emqx TO ROLE snowpipe;

   -- 集約モード用にステージとパイプへのアクセス権限
   GRANT READ, WRITE ON STAGE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqx TO ROLE snowpipe;

   -- ストリーミングモード用にストリーミングパイプへの権限
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqxstreaming TO ROLE snowpipe;

   -- ユーザーにロールを割り当て、デフォルトに設定
   GRANT ROLE snowpipe TO USER snowpipeuser;
   ALTER USER snowpipeuser SET DEFAULT_ROLE = snowpipe;
   ```

## 集約モード用Snowflakeコネクターの作成

Snowflake Sinkで集約アップロードモードを使う場合、Snowflake環境との接続を確立するためにODBC（DSN経由）を使ったSnowflakeコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake** を選択し、次へ。

4. コネクター名を英数字の組み合わせで入力。ここでは `my-snowflake` とします。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` 部分はご自身のSnowflakeインスタンス固有のサブドメインに置き換えます。

   - **Account**：Snowflake組織IDとアカウント名をハイフン（`-`）で区切って入力。SnowflakeコンソールのURLから確認可能。

   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`で設定した`snowflake`を入力。

   - **Username**：前述の設定で作成した`snowpipeuser`を入力。

   - **Password**：ODBC経由でユーザー名/パスワード認証する場合のパスワード。任意入力：

     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、

     - `/etc/odbc.ini`に設定するか、

     - キーペア認証を使う場合は空欄にします。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。ここに設定がない場合は`/etc/odbc.ini`の認証情報を使用します。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使うRSA秘密鍵の絶対パス。クラスター内の全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。暗号化していない場合は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス

     - **Proxy Port**：プロキシサーバーのポート番号

6. 暗号化接続を行う場合は **Enable TLS** トグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックし、コネクター作成を完了。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへの書き込みを指定できます。

## Snowflakeストリーミングコネクターの作成

Snowflake Sinkでストリーミングアップロードモードを使う場合、HTTPSとSnowpipe Streaming REST APIを使うSnowflakeストリーミングコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake Streaming** を選択し、次へ。

4. コネクター名を英数字の組み合わせで入力。ここでは `my-snowflake-streaming` とします。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` 部分はご自身のSnowflakeインスタンス固有のサブドメインに置き換えます。

   - **Account**：Snowflake組織IDとアカウント名をハイフン（`-`）で区切って入力。SnowflakeコンソールのURLから確認可能。

   - **Pipe User**：対象パイプを操作する権限を持つSnowflakeユーザー名。例：`snowpipeuser`。`OPERATE`および`MONITOR`権限が必要。

   - **Private Key Path**：EMQXがJWT署名に使うRSA秘密鍵。PEM形式の秘密鍵全文を文字列として貼り付けるか、`file://`で始まる秘密鍵ファイルのパスを指定可能。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。暗号化していない場合は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス

     - **Proxy Port**：プロキシサーバーのポート番号

6. 暗号化接続を行う場合は **Enable TLS** トグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックし、コネクター作成を完了。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへの書き込みを指定できます。

## Snowflake Sinkを使ったルールの作成

このセクションでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sink経由で書き込む方法を示します。

### SQLを指定したルールの作成

1. ダッシュボードの **Integration** -> **Rules** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. ルールIDに `my_rule` を入力し、SQLエディターに以下のルールSQLを入力：

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

   Snowflake連携では、選択するフィールドがSnowflakeのテーブルのカラム数と名前に厳密に一致する必要があるため、余分なフィールド追加や`*`選択は避けてください。

   :::

4. ルールにアクションを追加し、Sinkを設定します。
   - 集約アップロードモードでSnowflakeに書き込む場合は、[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照。
   - ストリーミングアップロードモードでSnowflakeに書き込む場合は、[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照。
5. アクション追加後、**Action Outputs**セクションに新規Sinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックし、ルール作成を完了。

これでルール作成が完了し、**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブに新規Snowflake Sinkが表示されます。

また、**Integration** -> **Flow Designer**でトポロジーを確認できます。トポロジーはトピック`t/#`のメッセージがルール`my_rule`で解析され、Snowflakeに書き込まれる流れを視覚的に示します。

### 集約アップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、集約アップロードモードで処理結果をSnowflakeに書き込む方法を示します。このモードは複数のルールトリガー結果を単一ファイル（例：CSV）にまとめてアップロードし、ファイル数を減らし書き込み効率を向上させます。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成しルールに追加。

3. Sink名（例：`snowflake_sink`）と簡単な説明を入力。

4. **Connectors**ドロップダウンから先に作成した`my-snowflake`コネクターを選択。隣の作成ボタンで新規コネクターをポップアップで素早く作成可能。必要な設定は[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照。

5. 集約アップロードモードの設定を行う。

   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース名。

   - **Schema**：`public`。`testdatabase`内のデータテーブルが存在するスキーマ。

   - **Stage**：`emqx`。Snowflakeでデータロード前にファイルを保持するステージ名。

   - **Pipe**：`emqx`。ステージからテーブルへのロードを自動化するパイプ名。

   - **Pipe User**：`snowpipeuser`。パイプ管理権限を持つSnowflakeユーザー名。

   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSA秘密鍵。以下いずれかの形式で指定可能：

     - **プレーンテキスト**：PEM形式の秘密鍵全文を文字列として直接貼り付け。

     - **ファイルパス**：`file://`で始まる秘密鍵ファイルのパス。クラスター内の全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要あり。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。暗号化していない場合は空欄。

   - **Aggregation Upload Format**：現在は`csv`のみサポート。データはカンマ区切りCSV形式でSnowflakeにステージングされます。

   - **Column Order**：ドロップダウンから列の順序を選択。生成されるCSVファイルは選択列を先に並べ、未選択列はアルファベット順に並びます。

   - **Max Records**：集約をトリガーする最大レコード数。例：`1000`に設定すると1000件集まった時点でアップロードし、時間間隔をリセット。

   - **Time Interval**：集約を行う時間間隔（秒）。例：`60`に設定すると最大レコード数に達しなくても60秒ごとにアップロードし、最大レコード数をリセット。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス

     - **Proxy Port**：プロキシサーバーのポート番号

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeサーバーへの接続テストが可能。

9. **Create**ボタンをクリックし、Sink作成を完了。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

### ストリーミングアップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、ストリーミングアップロードモードで処理結果をSnowflakeに書き込む方法を示します。このモードはSnowpipe Streaming APIを使ったリアルタイム取り込みを可能にします。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成しルールに追加。

3. Sink名（例：`snowflake_sink_streaming`）と簡単な説明を入力。

4. コネクタードロップダウンから先に作成した`my-snowflake-streaming`コネクターを選択。隣の作成ボタンで新規コネクターをポップアップで素早く作成可能。必要な設定は[ストリーミングコネクターの作成](#create-a-snowflake-streaming-connector)を参照。

5. ストリーミングアップロードモードの設定を行う。

   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース名。

   - **Schema**：`public`。`testdatabase`内のデータテーブルが存在するスキーマ。

   - **Pipe**：`emqxstreaming`。SQL文で作成したSnowflakeストリーミングパイプ名。Snowflakeで定義した名前と完全一致させる必要あり。

   - **HTTP Pipelining**：レスポンスを待たずに送信可能な最大HTTPリクエスト数。デフォルト：`100`

   - **Connect Timeout**：Snowflakeへの接続確立のタイムアウト秒数。デフォルト：`15`

   - **Connection Pool Size**：EMQXがこのSink用にSnowflakeへ維持可能な最大同時接続数。デフォルト：`8`

   - **Max Inactive**：アイドル状態の接続を閉じるまでの最大待機時間（秒）。デフォルト：`10`

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeサーバーへの接続テストが可能。

9. **Create**ボタンをクリックし、Sink作成を完了。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

## ルールのテスト

このセクションでは、設定したルールのテスト方法を示します。

### テストメッセージのパブリッシュ

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返し、複数のテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeに正常にデータが書き込まれているかをSnowflakeインスタンスにアクセスし、対象テーブルをクエリして確認します。

1. SnowflakeのWebインターフェースを開き、認証情報でログイン。

2. Snowflakeコンソールで以下SQLを実行し、ルールで書き込まれた`emqx`テーブルのデータを表示：

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   これにより、`clientid`、`topic`、`payload`、`publish_received_at`フィールドを含む全レコードが表示されます。

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）やトピック、タイムスタンプなどのメタデータが確認できるはずです。

## 詳細設定

このセクションでは、Snowflake Sinkの詳細設定オプションについて説明します。ダッシュボードでSink設定時に**Advanced Settings**を展開し、用途に応じて以下のパラメーターを調整可能です。

| 項目名                         | 説明                                                         | デフォルト値    |
| ------------------------------ | ------------------------------------------------------------ | -------------- |
| **Buffer Pool Size**            | EMQXとSnowflake間のデータフローを管理するバッファワーカー数を指定します。これらのワーカーはデータを一時的に保持・処理し、送信前のパフォーマンス最適化とスムーズなデータ転送を支えます。 | `16`           |
| **Request TTL**                 | バッファに入ったリクエストが有効とみなされる最大時間（秒）です。TTLを超えるか、送信後にSnowflakeから応答やアックが得られない場合、リクエストは期限切れと判定されます。 | `45`           |
| **Health Check Interval**       | Snowflakeとの接続状態を自動チェックする間隔（秒）を指定します。 | `15`           |
| **Health Check Interval Jitter**| 複数ノードが同時にヘルスチェックを開始しないように、基本間隔に加える一様ランダム遅延（ミリ秒）です。複数のActionやSourceが同じConnectorを共有する場合に有効です。 | `0`            |
| **Health Check Timeout**        | Snowflake接続の自動ヘルスチェックのタイムアウト時間（秒）を指定します。 | `60`           |
| **Max Buffer Queue Size**       | Snowflake Sinkの各バッファワーカーが保持可能な最大バイト数です。ワーカーはデータを一時的に保持し、効率的なデータストリーム処理を行います。システム性能やデータ転送要件に応じて調整してください。 | `256` MB       |
| **Query Mode**                  | 同期（`synchronous`）または非同期（`asynchronous`）のリクエストモードを選択可能です。非同期モードではSnowflakeへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受け取る可能性があります。 | `Asynchronous` |
| **Batch Size**                  | EMQXからSnowflakeへ一度に送信するデータバッチの最大サイズです。サイズ調整により転送効率と性能を最適化可能です。<br />`1`に設定するとバッチングせず個別送信となります。 | `100`          |
| **Inflight Window**             | 送信済みだが応答やアックをまだ受け取っていない「インフライト」キューリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`          |
