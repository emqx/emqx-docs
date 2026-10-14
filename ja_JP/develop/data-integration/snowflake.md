# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/)は、クラウドベースのデータプラットフォームであり、非常にスケーラブルかつ柔軟なデータウェアハウジング、分析、セキュアなデータ共有ソリューションを提供します。構造化データおよび半構造化データの処理に優れ、大量のデータを格納しつつ高速なクエリ性能と多様なツールやサービスとのシームレスな統合を実現しています。

本ページでは、EMQXとSnowflake間のデータ統合について詳しく解説し、ルールとSinkの作成方法について実践的なガイダンスを提供します。

## 動作概要

EMQXにおけるSnowflakeデータ統合は、複雑なIoTビジネスワークフローを簡単にサポートできる使い勝手の良い機能です。典型的なIoTアプリケーションでは、EMQXがデバイス接続とメッセージ送受信を担うIoTプラットフォームとして機能し、一方Snowflakeはメッセージデータの取り込み、格納、分析を行うデータストレージおよび処理プラットフォームとして役割を果たします。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブルに格納されたデータにアクセス可能です。具体的なワークフローは以下の通りです。

1. **デバイスのEMQXへの接続**：IoTデバイスはMQTTプロトコルで正常に接続するとオンラインイベントをトリガーします。このイベントにはデバイスIDや送信元IPアドレスなどの識別情報が含まれます。

2. **デバイスからのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジン内で処理します。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールにマッチしたメッセージは、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。

4. **Snowflakeへの書き込み**：ルールはメッセージデータをSnowflakeに書き込むアクションをトリガーします。メッセージをファイルにバッチングしてStageとPipe経由でロードする（集約モード）か、Snowpipe Streaming APIを使ってリアルタイムにストリーミングする（ストリーミングモード）方法があります。

イベントやメッセージデータがSnowflakeに書き込まれた後は、以下のようなビジネス・技術的用途に活用できます。

- **データアーカイブ**：IoTデータを安全にSnowflakeに長期保存し、コンプライアンスや履歴データの利用を保証します。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を実施。予知保全、運用インサイト、デバイス性能評価などを可能にします。

## 特長とメリット

EMQXのSnowflakeデータ統合を利用することで、以下の特長とメリットをビジネスにもたらします。

- **メッセージ変換**：Snowflakeへの書き込み前にEMQXルールでメッセージを柔軟に処理・変換でき、後続の保存や利用を容易にします。
- **柔軟なデータ操作**：Snowflake Sinkでは書き込み対象のフィールドを選択可能で、ビジネスニーズに応じた効率的かつ動的なストレージ構成が可能です。
- **統合されたビジネスプロセス**：Snowflake Sinkにより、デバイスデータをSnowflakeの豊富なエコシステムアプリケーションと連携でき、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は、従来のデータベースに比べて低コストでの長期データ保持に最適で、大量のIoTデータ保存に適しています。

これらの特長により、効率的で信頼性が高くスケーラブルなIoTアプリケーションの構築と、ビジネス意思決定や最適化に貢献します。

## はじめる前に

ここでは、EMQXでSnowflake Sinkを作成する前に必要な準備を紹介します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の基本概念の理解。
- 管理者権限を持つSnowflakeアカウント。

### アップロードモードの選択

::: tip

モードは最初に選択してください。EMQXおよびSnowflake環境の設定方法がこれに依存します。

:::

EMQXはSnowflakeへのデータ送信に以下の2つのモードをサポートしています。

| モード       | 説明                                                         | ODBC 必要性   |
| ------------ | ------------------------------------------------------------ | ------------ |
| 集約（Aggregated） | EMQXがMQTTメッセージをローカルファイルにバッファし、SnowflakeのStageにアップロード。`COPY INTO`文で設定されたPipeが自動的にファイルをテーブルにロードします。詳細は[Snowflake Snowpipeドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)参照。 | 必須         |
| ストリーミング（Streaming） | Snowpipe Streaming APIを通じてリアルタイムにデータを送信し、行単位でSnowflakeテーブルに書き込みます。 | 必須         |

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送を行うためには、Snowflake Open Database Connectivity（ODBC）ドライバーのインストールと設定が必要です。このドライバーはEMQXがSnowflakeのStageにデータを書き込む際の通信ブリッジとして機能し、データの適切なフォーマット、認証、転送を保証します。

詳細は公式の[ODBCドライバー](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な導入とシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法として推奨するものではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`をローカルにコピーし、`chmod a+x`で実行権限を付与してから`sudo`で実行します。

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリにダウンロードし、ドライバーをインストールします。その後、以下のシステム設定ファイルを更新します。

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

macOSでのSnowflake ODBCドライバーのインストールと設定は以下の手順に従ってください。

1. unixODBCをインストール（例）：

   ```
   brew install unixodbc
   ```

2. [iODBCのダウンロードとインストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)。

3. [Snowflake ODBCドライバーのダウンロードとインストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)。

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

アップロードモードに関わらず、Snowflake環境においてユーザーアカウント、データベース、関連リソースを設定し、データ取り込みの準備を行う必要があります。以下の認証情報は後でEMQXのコネクターおよびSink設定時に使用します。

| 項目                    | 値                                              | 説明                                                         |
| ----------------------- | ------------------------------------------------ | ------------------------------------------------------------ |
| データソース名（DSN）   | `snowflake`（集約モードのみ）                    | `/etc/odbc.ini`に設定されたODBC DSN。集約アップロードで使用。 |
| ユーザー名              | `snowpipeuser`                                   | Snowflake接続認証に使用するユーザー。モードに応じて適切な権限が必要。 |
| パスワード              | `Snowpipeuser99`                                 | キーペア認証を使う場合は省略可能。                             |
| データベース名          | `testdatabase`                                   | 対象テーブルが存在するSnowflakeのデータベース名。             |
| スキーマ                | `public`                                         | データベース内のスキーマ名。テーブルやパイプが存在する場所。   |
| ステージ（集約モード）  | `emqx`                                           | ファイル取り込み前にデータを保持するSnowflakeのステージ。       |
| パイプ（集約モード）    | `emqx`                                           | ステージからテーブルへデータをロードするパイプ。               |
| パイプ（ストリーミング）| `emqxstreaming`                                  | Snowpipe Streaming API経由でデータを取り込むためのストリーミングパイプ。 |
| プライベートキー        | `file://<path to snowflake_rsa_key.private.pem>` | API認証用JWT署名に使うRSA秘密鍵のファイルパス。                |

#### RSAキーペアの生成（集約モードは任意）

Snowflakeは複数の認証方式をサポートしており、EMQXでの認証方法はアップロードモードと接続設定に依存します。

| アップロードモード | 認証方式                                                  | キーペア必須か |
| ------------------ | --------------------------------------------------------- | ------------- |
| ストリーミング（HTTPS） | RSAキーペア＋JWT（唯一のサポート方式）                     | 必須          |
| 集約（ODBC）        | ユーザー名/パスワード（DSNまたはEMQX経由）<br />RSAキーペア＋JWT（任意、EMQX内で設定） | 任意          |

キーペア認証はストリーミングモードで必須であり、EMQXがJWTを署名してSnowflake Streaming APIに安全に認証します。

集約モードではユーザー名/パスワードまたはRSAキーペアのいずれかで認証可能です。認証情報は以下のいずれかの方法で提供します。

- ダッシュボードのコネクター設定でユーザー名とパスワードを直接入力
- キーペア認証を使う場合は秘密鍵ファイルのパスを指定
- EMQXでどちらも指定しない場合は、システムのODBC DSN（Linuxなら`/etc/odbc.ini`、macOSなら`~/.odbc.ini`）に正しく設定されていることを確認

::: tip

認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。

EMQXでどちらも設定されていない場合、コネクターは`/etc/odbc.ini`の認証情報を使用します。

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

RSAキーペア認証を使う（または必須の）場合は、以下のコマンドで鍵を生成し設定します。

```bash
# 秘密鍵生成
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt

# 公開鍵生成
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

EMQXがキーペア認証を使う場合（集約・ストリーミング両モード対応）：

- EMQXは秘密鍵でJWTに署名し、安全かつ検証可能なIDトークンを生成します。
- Snowflakeは公開鍵でトークンの署名を検証します。

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLでSnowflakeリソースをセットアップ

RSAキーペア生成後、`aggregated`または`streaming`のいずれかの取り込み用に必要なSnowflakeオブジェクトをSQLで作成します。

対象は以下を含みます。

- データベースとテーブルの作成
- ステージとパイプの作成（`aggregated`用）
- ストリーミングパイプの作成（`streaming`用）
- ユーザーとロールの作成および権限付与

1. SnowflakeコンソールのSQLワークシートで以下を実行し、データベース、テーブル、ステージ、パイプを作成します。

   ```sql
   USE ROLE accountadmin;

   -- データ格納用データベース作成（存在しない場合）
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

   - パイプ内の`COPY INTO`により、Snowflakeはステージまたはストリーミングで取り込んだデータを自動的にテーブルにロードします。
   - ストリーミングパイプの`$1:field`構文はEMQXからJSON形式で取り込んだペイロードのフィールドを抽出します。

2. EMQX認証用の専用ユーザー（例：`snowpipeuser`）を作成し、RSA公開鍵をユーザーに紐付けます。

   ```sql
   -- ユーザー作成
   CREATE USER IF NOT EXISTS snowpipeuser
       PASSWORD = 'Snowpipeuser99'
       MUST_CHANGE_PASSWORD = FALSE;

   -- RSA公開鍵の紐付け
   ALTER USER snowpipeuser SET RSA_PUBLIC_KEY = '
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_1>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_2>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_3>
   <YOUR_PUBLIC_KEY_CONTENTS_LINE_4>
   ';
   ```

   ::: tip

   PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`の行は削除し、残りの内容を改行を保持したまま貼り付けてください。

   :::

   この鍵はSnowflakeユーザーにアップロードされ、Snowflake内に保存されます。

3. ユーザーに必要なロールを作成し、Snowflakeリソース管理権限を付与します。

   ```sql
   CREATE OR REPLACE ROLE snowpipe;

   -- データベースとスキーマの使用権限付与
   GRANT USAGE ON DATABASE testdatabase TO ROLE snowpipe;
   GRANT USAGE ON SCHEMA testdatabase.public TO ROLE snowpipe;
   GRANT INSERT, SELECT ON testdatabase.public.emqx TO ROLE snowpipe;

   -- 集約モード用にステージとパイプへのアクセス権限付与
   GRANT READ, WRITE ON STAGE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqx TO ROLE snowpipe;

   -- ストリーミングモード用パイプへの権限付与
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqxstreaming TO ROLE snowpipe;

   -- ユーザーにロールを割り当て、デフォルトに設定
   GRANT ROLE snowpipe TO USER snowpipeuser;
   ALTER USER snowpipeuser SET DEFAULT_ROLE = snowpipe;
   ```

## 集約モード用Snowflakeコネクターの作成

Snowflake Sinkで集約アップロードモードを利用する場合、Snowflake環境との接続を確立するためにODBC（DSN経由）を使ったSnowflakeコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake** を選択し、次へ。

4. コネクター名を英数字で入力。ここでは `my-snowflake` と入力。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>`はSnowflake環境固有のサブドメインに置き換える。

   - **Account**：Snowflakeコンソールで確認できる組織IDとアカウント名をハイフン（`-`）で区切って入力。

   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`で設定した`snowflake`を入力。

   - **Username**：前述の設定で作成した`snowpipeuser`を入力。

   - **Password**：SnowflakeへのODBC認証用パスワード。任意入力。

     - ここにパスワード（例：`Snowpipeuser99`）を入力するか、

     - `/etc/odbc.ini`に設定するか、

     - キーペア認証を使う場合は空欄にする。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。ここで未設定の場合は`/etc/odbc.ini`の認証情報が使われます。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使うRSA秘密鍵の絶対ファイルパス。クラスタ内の全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄でよい。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシは非対応。デフォルトは無効。利用する場合は`Enable Proxy`を選択し、以下を設定。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を有効にする場合は **Enable TLS** をオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[Advanced Settings](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能。

9. **Create**ボタンをクリックしてコネクター作成を完了。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへの書き込みを指定できます。

## Snowflakeストリーミングコネクターの作成

Snowflake Sinkでストリーミングアップロードモードを利用する場合、HTTPSおよびSnowpipe Streaming REST APIを使ったSnowflakeストリーミングコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake Streaming** を選択し、次へ。

4. コネクター名を英数字で入力。ここでは `my-snowflake-streaming` と入力。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。環境固有のサブドメインに置き換える。

   - **Account**：Snowflakeコンソールで確認できる組織IDとアカウント名をハイフン（`-`）で区切って入力。

   - **Pipe User**：対象パイプの操作権限を持つSnowflakeユーザー名。例：`snowpipeuser`。`OPERATE`および`MONITOR`権限が必要。

   - **Private Key Path**：EMQXがJWT署名に使うRSA秘密鍵。PEM形式の秘密鍵全文を文字列で貼り付けるか、`file://`で始まる秘密鍵ファイルのパスを指定可能。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄でよい。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシは非対応。デフォルトは無効。利用する場合は`Enable Proxy`を選択し、以下を設定。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を有効にする場合は **Enable TLS** をオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 詳細設定（任意）：[Advanced Settings](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能。

9. **Create**ボタンをクリックしてコネクター作成を完了。

これでコネクター作成が完了し、ルールとSinkを作成してSnowflakeへの書き込みを指定できます。

## Snowflake Sink付きルールの作成

ここでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sink経由でSnowflakeに書き込む方法を示します。

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

   SQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてルールSQLの学習やテストが可能です。

   :::

   ::: tip

   Snowflake連携では、選択するフィールドはSnowflakeのテーブルのカラム数と名前に厳密に一致させる必要があります。余分なフィールド追加や`*`での選択は避けてください。

   :::

4. ルールにアクションを追加し、Sinkを設定します。

   - 集約アップロードモードでSnowflakeに書き込む場合は[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照。

   - ストリーミングアップロードモードでSnowflakeに書き込む場合は[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照。

5. アクション追加後、**Action Outputs**に新規Sinkが表示されます。**Create Rule**ページで**Save**をクリックし、ルール作成を完了。

これでルール作成が完了し、**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブに新規Snowflake Sinkが表示されます。

また、**Integration** -> **Flow Designer**でトポロジーを確認可能です。トポロジーはトピック`t/#`のメッセージがルール`my_rule`で処理され、Snowflakeに書き込まれる流れを視覚的に示します。

### 集約アップロードモードでSnowflake Sinkを追加

ここでは、ルールにSinkを追加し、処理結果を集約アップロードモードでSnowflakeに書き込む方法を示します。このモードは複数のルールトリガー結果を1ファイル（例：CSV）にまとめてアップロードし、ファイル数を減らして書き込み効率を向上させます。

1. **Create Rule**ページの**Action Outputs**で**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sinkの名前（例：`snowflake_sink`）と簡単な説明を入力。

4. **Connectors**ドロップダウンから先に作成した`my-snowflake`コネクターを選択。隣の作成ボタンでポップアップから新規コネクター作成も可能。必要な設定は[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照。

5. 集約アップロードモードの設定を行う。

   - **Database Name**：`testdatabase`。EMQXデータ格納用に作成したSnowflakeデータベース名。

   - **Schema**：`public`。`testdatabase`内のデータテーブルがあるスキーマ名。

   - **Stage**：`emqx`。Snowflakeでデータを一時保持するステージ名。

   - **Pipe**：`emqx`。ステージからテーブルへの自動ロードを行うパイプ名。

   - **Pipe User**：`snowpipeuser`。パイプ管理権限を持つSnowflakeユーザー名。

   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSA秘密鍵。以下いずれかの形式で指定可能。

     - **プレーンテキスト**：PEM形式の秘密鍵全文を文字列として直接貼り付け。

     - **ファイルパス**：`file://`で始まる秘密鍵ファイルのパス。クラスタ内全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄でよい。

   - **Aggregation Upload Format**：現状`csv`のみサポート。SnowflakeにCSV形式でステージングされます。

   - **Column Order**：ドロップダウンからカラムの並び順を選択。生成されるCSVファイルは選択したカラム順にソートされ、未選択カラムはアルファベット順に続きます。

   - **Max Records**：集約トリガーとなる最大レコード数。例：`1000`に設定すると1000件集まった時点でファイルをアップロードし、時間間隔をリセット。

   - **Time Interval**：集約を行う時間間隔（秒）。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにアップロードし、最大レコード数をリセット。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する場合の設定。HTTPSプロキシは非対応。デフォルトは無効。利用する場合は`Enable Proxy`を選択し、以下を設定。

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。

     - **Proxy Port**：プロキシサーバーのポート番号。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **Advanced Settings**を展開し、必要に応じて詳細設定を行う（任意）。詳細は[Advanced Settings](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能。

9. **Create**ボタンをクリックしてSink作成を完了。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

### ストリーミングアップロードモードでSnowflake Sinkを追加

ここでは、ルールにSinkを追加し、処理結果をSnowpipe Streaming APIを使ったストリーミングアップロードモードでSnowflakeに書き込む方法を示します。

1. **Create Rule**ページの**Action Outputs**で**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sinkの名前（例：`snowflake_sink_streaming`）と簡単な説明を入力。

4. コネクタードロップダウンから先に作成した`my-snowflake-streaming`コネクターを選択。隣の作成ボタンでポップアップから新規コネクター作成も可能。必要な設定は[Snowflakeストリーミングコネクターの作成](#create-a-snowflake-streaming-connector)を参照。

5. ストリーミングアップロードモードの設定を行う。

   - **Database Name**：`testdatabase`。EMQXデータ格納用に作成したSnowflakeデータベース名。

   - **Schema**：`public`。`testdatabase`内のデータテーブルがあるスキーマ名。

   - **Pipe**：`emqxstreaming`。SQLで作成したSnowflakeストリーミングパイプ名。Snowflake側で定義した名前と完全一致する必要あり。

   - **HTTP Pipelining**：レスポンスを待たずに送信可能なHTTPリクエストの最大数。デフォルトは`100`。

   - **Connect Timeout**：Snowflakeへの接続確立のタイムアウト秒数。デフォルトは`15`秒。

   - **Connection Pool Size**：このSinkがSnowflakeと維持可能な最大同時接続数。デフォルトは`8`。

   - **Max Inactive**：HTTPクライアントが再接続を試みるまでの最大非アクティブ時間。EMQX 6.0.4以降は60秒間リクエスト送信がない場合に再接続。60秒より長い値を設定するとそちらが優先。レスポンスが60秒以上かかる場合は最大応答時間以上に設定推奨。デフォルトは`10`秒。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **Advanced Settings**を展開し、必要に応じて詳細設定を行う（任意）。詳細は[Advanced Settings](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能。

9. **Create**ボタンをクリックしてSink作成を完了。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

## ルールのテスト

ここでは、設定したルールの動作確認方法を示します。

### テストメッセージのパブリッシュ

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返してテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeに正常にデータが書き込まれたかを確認します。

1. SnowflakeのWebインターフェースにログイン。

2. Snowflakeコンソールで以下のSQLクエリを実行し、ルールで書き込まれた`emqx`テーブルのデータを確認。

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）や、トピック、タイムスタンプなどのメタデータが表示されるはずです。

## 詳細設定

ここでは、Snowflake Sinkの詳細設定項目について説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、用途に応じて以下のパラメーターを調整可能です。

| 項目名                          | 説明                                                         | デフォルト値   |
| ------------------------------- | ------------------------------------------------------------ | -------------- |
| **Buffer Pool Size**             | EMQXとSnowflake間のデータフローを管理するバッファワーカーの数。これらのワーカーはデータを一時的に保持・処理し、送信前のパフォーマンス最適化とスムーズなデータ転送を担います。 | `16`           |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）。バッファに滞留する時間がこのTTLを超えたり、送信後にSnowflakeからの応答やアックが遅延した場合、リクエストは期限切れと判断されます。 | `45`           |
| **Health Check Interval**        | Snowflakeとの接続状態を自動的にチェックする間隔（秒）。     | `15`           |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを開始するのを防ぐために、基本間隔に加えるランダム遅延（ミリ秒）。複数のアクションやソースが同じコネクターを共有する場合、ジッター有効化でチェック開始時間をずらせます。 | `0`            |
| **Health Check Timeout**         | Snowflake接続のヘルスチェックがタイムアウトするまでの時間（秒）。 | `60`           |
| **Max Buffer Queue Size**        | Snowflake Sinkの各バッファワーカーが保持可能な最大バイト数。バッファワーカーはデータを一時保持し、効率的なデータストリーム処理を実現します。システム性能やデータ転送要件に応じて調整してください。 | `256` MB       |
| **Query Mode**                   | `synchronous`または`asynchronous`のリクエストモードを選択可能。非同期モードではSnowflakeへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがメッセージを受信してからSnowflakeに到達するまで時間差が生じる可能性があります。 | `Asynchronous` |
| **Batch Size**                   | EMQXからSnowflakeへ一度に送信するデータバッチの最大サイズ。サイズ調整により転送効率とパフォーマンスを最適化可能。<br />`1`に設定するとバッチ化せずに個別送信となります。 | `100`          |
| **Inflight  Window**             | 送信済みで応答待ちのリクエスト数の最大値。<br />`Request Mode`が`asynchronous`の場合、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`          |
