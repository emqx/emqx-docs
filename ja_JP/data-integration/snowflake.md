# SnowflakeへのMQTTデータ取り込み

[Snowflake](https://www.snowflake.com/en/)は、クラウドベースのデータプラットフォームであり、高いスケーラビリティと柔軟性を備えたデータウェアハウジング、分析、セキュアなデータ共有のソリューションを提供します。構造化データおよび半構造化データの処理に優れ、大量のデータを格納しつつ高速なクエリ性能と多様なツールやサービスとのシームレスな統合を実現する設計となっています。

本ページでは、EMQXとSnowflake間のデータ統合について詳細に解説し、ルールおよびSinkの作成方法を実践的に案内します。

## 動作概要

EMQXにおけるSnowflakeデータ統合はすぐに利用可能な機能であり、複雑なIoTビジネスワークフローを簡単にサポートできるよう設定可能です。典型的なIoTアプリケーションでは、EMQXがデバイスの接続管理およびメッセージ送受信を担うIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、格納、分析を行うデータストレージおよび処理プラットフォームとして役割を果たします。

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスイベントやデータをSnowflakeへ転送します。エンドユーザーやアプリケーションはSnowflakeのテーブルに格納されたデータにアクセス可能です。具体的なワークフローは以下の通りです：

1. **デバイスのEMQX接続**：IoTデバイスはMQTTプロトコルを介して正常に接続されるとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレスなどの識別情報が含まれます。

2. **デバイスメッセージのパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリおよびステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジン内で処理します。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールをマッチさせ、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。

4. **Snowflakeへの書き込み**：ルールはメッセージデータをSnowflakeに書き込むアクションをトリガーします。メッセージをファイルにバッチ化してStageおよびPipe経由でロードする（集約モード）、またはSnowpipe Streaming APIを使って直接ストリーミングする（ストリーミングモード）方法があります。

イベントおよびメッセージデータがSnowflakeに書き込まれた後は、以下のようなビジネスおよび技術的用途で活用可能です：

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの保持を確保。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を実施。予知保全、運用インサイト、デバイス性能評価などを実現。

## 特長と利点

EMQXのSnowflakeデータ統合を利用することで、以下の特長と利点をビジネスにもたらします：

- **メッセージ変換**：メッセージはEMQXルール内で高度に処理・変換されてからSnowflakeに書き込まれるため、その後の保管や活用が容易になります。
- **柔軟なデータ操作**：Snowflake Sinkは書き込むフィールドを選択可能であり、ビジネスニーズに応じた効率的かつ動的なストレージ構成が可能です。
- **統合されたビジネスプロセス**：Snowflake Sinkにより、デバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせることができ、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は従来のデータベースに比べて低コストで長期データ保持に最適であり、大量のIoTデータの保存に適しています。

これらの特長により、効率的で信頼性が高くスケーラブルなIoTアプリケーション構築と、ビジネス意思決定や最適化の恩恵を受けることができます。

## はじめる前に

このセクションでは、EMQXでSnowflake Sinkを作成する前に必要な準備について説明します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の概念理解
- 管理者権限を持つSnowflakeアカウント

### アップロードモードの選択

::: tip

まずモードを選択してください。これによりEMQXおよびSnowflake環境の設定方法が決まります。

:::

EMQXはSnowflakeへのデータ送信に以下の2つのモードをサポートしています：

| モード       | 説明                                                                                          | ODBC必要性      |
| ----------- | --------------------------------------------------------------------------------------------- | -------------- |
| 集約（Aggregated） | EMQXはMQTTメッセージをローカルファイルにバッファリングし、SnowflakeのStageにアップロードします。`COPY INTO`文を設定したPipeが自動的にファイルをターゲットテーブルにロードします。詳細は[Snowflake Snowpipeドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)参照。 | 必須           |
| ストリーミング（Streaming） | Snowpipe Streaming APIを介してリアルタイムにデータを送信し、行単位でSnowflakeテーブルに直接書き込みます。 | 必須           |

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送するためには、SnowflakeのODBCドライバーをインストール・設定する必要があります。このドライバーはEMQXがSnowflakeのStageにデータを書き込むための通信ブリッジとして機能し、データの適切なフォーマット、認証、転送を保証します。

詳細は公式の[ODBC Driver](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーを迅速に導入するための[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法の推奨ではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`をローカルにコピーし、実行権限を付与してsudoで実行します：

```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリにダウンロードし、ドライバーをインストール、以下の設定ファイルを更新します：

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

macOSでのSnowflake ODBCドライバーのインストール・設定手順は以下の通りです：

1. unixODBCをインストール（例）：

   ```
   brew install unixodbc
   ```

2. [iODBCのダウンロードとインストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)

3. [Snowflake ODBCドライバーのダウンロードとインストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)

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

### ユーザーアカウント作成およびSnowflakeリソース設定

アップロードモードに関わらず、Snowflake環境にユーザーアカウント、データベース、関連リソースを設定し、データ取り込みの準備を行う必要があります。以下の認証情報は後でEMQXのコネクターおよびSink設定時に必要です：

| 項目                     | 値                                             | 説明                                                         |
| ------------------------ | ---------------------------------------------- | ------------------------------------------------------------ |
| データソース名（DSN）    | `snowflake`（集約モードのみ）                   | `/etc/odbc.ini`で設定したODBC DSN。集約アップロードに使用。    |
| ユーザー名               | `snowpipeuser`                                 | Snowflake接続認証に使用するユーザー。モードに応じた権限が必要。 |
| パスワード               | `Snowpipeuser99`                               | キーペア認証使用時は省略可能。                                |
| データベース名           | `testdatabase`                                 | 対象テーブルが存在するSnowflakeデータベース。                |
| スキーマ                 | `public`                                       | データベース内のスキーマ名。テーブルやパイプが存在する。       |
| ステージ（集約モード）   | `emqx`                                         | データ取り込み前にファイルを保持するSnowflakeステージ。       |
| パイプ（集約モード）     | `emqx`                                         | ステージからテーブルへデータをロードするパイプ。               |
| パイプ（ストリーミング） | `emqxstreaming`                                | Snowpipe Streaming API経由でデータを取り込むためのパイプ。     |
| プライベートキー         | `file://<path to snowflake_rsa_key.private.pem>` | API認証用JWT署名に使うRSA秘密鍵のパス。                        |

#### RSAキーペア生成（集約モードは任意）

Snowflakeは複数の認証方法をサポートしており、EMQXではアップロードモードと接続設定に応じて選択します：

| アップロードモード | 認証オプション                                               | キーペア必須       |
| ----------------- | ------------------------------------------------------------ | ----------------- |
| ストリーミング（HTTPS） | RSAキーペア + JWT（唯一のサポート方式）                       | 必須              |
| 集約（ODBC）       | ユーザー名/パスワード（DSNまたはEMQX経由）<br>RSAキーペア + JWT（任意、EMQXのみ設定） | 任意              |

キーペア認証はストリーミングモードで必須であり、EMQXはJWTを署名してSnowflakeのStreaming APIに安全に認証します。

集約モードではユーザー名/パスワードまたはRSAキーペアのいずれかで認証可能です。認証情報の提供方法は以下のいずれかです：

- ダッシュボードのEMQXコネクター設定でユーザー名とパスワードを直接入力
- キーペア認証を使う場合は秘密鍵のパスを指定
- EMQXに設定がない場合は、システムのODBC DSN（Linuxなら`/etc/odbc.ini`、macOSなら`~/.odbc.ini`）に正しく設定されていることを確認

::: tip

認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に設定しないでください。

EMQXにどちらも設定されていない場合、コネクターは`/etc/odbc.ini`の認証情報を参照します。

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

> この方法により、EMQXは認証情報を直接含めずに`DSN`（`snowflake`）を参照できます。

**キーペア認証を使う場合**

RSAキーペア認証を使う場合（例：ストリーミングモード）、以下のコマンドで鍵を生成し設定します：

```bash
# 秘密鍵生成
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt

# 公開鍵生成
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

EMQXがキーペア認証を使う場合（集約・ストリーミング両モード対応）：

- EMQXは秘密鍵でJWTに署名し、安全かつ検証可能なIDトークンとして利用
- Snowflakeは公開鍵で署名を検証

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLによるSnowflakeリソース設定

RSAキーペア生成後、以下のSQLコマンドで`aggregated`または`streaming`取り込み用のSnowflakeオブジェクトを作成します。

対象は：

- データベースとテーブルの作成
- ステージとパイプの作成（集約モード）
- ストリーミングパイプの作成（ストリーミングモード）
- ユーザーとロールの作成および権限付与

1. SnowflakeコンソールのSQLワークシートで以下を実行：

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
   - ストリーミングパイプの`$1:field`構文はEMQXから取り込まれたJSONペイロードのフィールド抽出を行います。

2. EMQX認証用の専用ユーザー（例：`snowpipeuser`）を作成し、RSA公開鍵をバインド：

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

   PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`行は削除し、改行を保持したまま中身のみを貼り付けてください。

   :::

   この鍵はSnowflakeユーザーにアップロードされ、Snowflake内に保存されます。

3. 必要なロールを作成し、ユーザーに割り当てて権限を付与：

   ```sql
   CREATE OR REPLACE ROLE snowpipe;
   
   -- データベースおよびスキーマの使用権限付与
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

Snowflake Sinkで集約アップロードモードを使う場合は、Snowflake環境との接続を確立するためにODBC（DSN経由）を使ったSnowflakeコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake** を選択し、次へ。

4. コネクター名を英数字の組み合わせで入力。ここでは `my-snowflake` と入力。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常 `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換え。

   - **Account**：Snowflake組織IDとアカウント名をハイフン（`-`）で区切って入力。SnowflakeコンソールのURLから確認可能。

   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`で設定した`snowflake`を入力。

   - **Username**：前述の設定で作成した`snowpipeuser`を入力。

   - **Password**：ODBC経由でユーザー名/パスワード認証に使うパスワード。任意入力。
     - ここにパスワードを入力（例：`Snowpipeuser99`）、
     - または`/etc/odbc.ini`に設定、
     - キーペア認証を使う場合は空欄のままにする。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に設定しないでください。ここに設定がない場合は`/etc/odbc.ini`の認証情報を参照します。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使うRSA秘密鍵の絶対パス。クラスター内の全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続するための設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス
     - **Proxy Port**：プロキシサーバーのポート番号

6. 暗号化接続を有効にする場合は **Enable TLS** をオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のため必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**で接続テスト可能。

9. **Create**をクリックしてコネクター作成完了。

これでコネクター作成が完了し、Snowflakeへの書き込み方法を指定するルールおよびSinkの作成に進めます。

## Snowflakeストリーミングコネクターの作成

Snowflake Sinkでストリーミングアップロードモードを使う場合は、HTTPSおよびSnowpipe Streaming REST APIを利用するSnowflakeストリーミングコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. コネクタータイプで **Snowflake Streaming** を選択し、次へ。

4. コネクター名を英数字の組み合わせで入力。ここでは `my-snowflake-streaming` と入力。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常 `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。Snowflakeインスタンス固有のサブドメインに置き換え。

   - **Account**：Snowflake組織IDとアカウント名をハイフン（`-`）で区切って入力。SnowflakeコンソールのURLから確認可能。

   - **Pipe User**：対象パイプを操作可能な権限を持つSnowflakeユーザー名。例：`snowpipeuser`。少なくとも`OPERATE`および`MONITOR`権限が必要。

   - **Private Key Path**：EMQXがJWT署名に使うRSA秘密鍵。PEM形式の秘密鍵全文を文字列として直接貼り付けるか、`file://`で始まる秘密鍵ファイルのパスを指定可能。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。`-nocrypt`オプションで生成した場合は空欄。

   - **Proxy**：HTTPプロキシ経由接続設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシ利用時は`Enable Proxy`を選択し、以下を入力：
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス
     - **Proxy Port**：プロキシサーバーのポート番号

6. 暗号化接続を有効にする場合は **Enable TLS** をオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のため必須です。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**で接続テスト可能。

9. **Create**をクリックしてコネクター作成完了。

これでコネクター作成が完了し、Snowflakeへの書き込み方法を指定するルールおよびSinkの作成に進めます。

## Snowflake Sinkを用いたルールの作成

このセクションでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sinkを通じてSnowflakeに書き込む方法を示します。

### SQLを定義したルールの作成

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

   SQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックして学習・テスト可能です。

   :::
   ::: tip

   Snowflake連携では、選択するフィールドがSnowflakeテーブルのカラム数および名前と完全に一致することが重要です。余分なフィールドを追加したり`*`で全選択することは避けてください。

   :::

4. ルールにアクションとしてSinkを追加します。
   - 集約アップロードモードでSnowflakeに書き込む場合は、[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照。
   - ストリーミングアップロードモードでSnowflakeに書き込む場合は、[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照。

5. アクション追加後、**Action Outputs**セクションに新規Sinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックし、ルール作成を完了。

これでルール作成が完了し、**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブに新規Snowflake Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されてSnowflakeに書き込まれる流れを視覚的に確認できます。

### 集約アップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加して集約アップロードモードで処理結果をSnowflakeに書き込む方法を示します。このモードは複数のルールトリガー結果を1つのファイル（例：CSV）にまとめてアップロードし、ファイル数を減らして書き込み効率を向上させます。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**はデフォルトの`Create Action`のまま、または既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sink名（例：`snowflake_sink`）と簡単な説明を入力。

4. **Connectors**ドロップダウンから先に作成した`my-snowflake`コネクターを選択。隣の作成ボタンでポップアップから新規コネクター作成も可能。設定パラメータは[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照。

5. 集約アップロードモードの設定を行う。

   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース名。
   - **Schema**：`public`。`testdatabase`内のデータテーブルが存在するスキーマ名。
   - **Stage**：`emqx`。Snowflakeで作成したデータアップロード用ステージ名。
   - **Pipe**：`emqx`。ステージからテーブルへ自動ロードするパイプ名。
   - **Pipe User**：`snowpipeuser`。パイプ操作権限を持つSnowflakeユーザー名。
   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSA秘密鍵。以下いずれかの形式で指定可能：
     - **プレーンテキスト**：PEM形式の秘密鍵全文を文字列として直接貼り付け。
     - **ファイルパス**：`file://`で始まる秘密鍵ファイルのパス。クラスター全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要あり。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`
   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄。

   - **Aggregation Upload Format**：現在は`csv`のみサポート。データはカンマ区切りCSV形式でSnowflakeにステージングされる。
   - **Column Order**：ドロップダウンからカラムの並び順を選択。生成されるCSVファイルは選択したカラム順にソートされ、未選択カラムはアルファベット順に続く。
   - **Max Records**：集約トリガーとなる最大レコード数。例：`1000`に設定すると1000レコード集まった時点でファイルをアップロードし、時間間隔をリセット。
   - **Time Interval**：集約処理の時間間隔（秒）。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにデータをアップロードし、最大レコード数をリセット。
   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシ利用時は`Enable Proxy`を選択し、以下を入力：
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス
     - **Proxy Port**：プロキシサーバーのポート番号

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeサーバーへの接続テストが可能。

9. **Create**をクリックしてSink作成完了。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加される。

### ストリーミングアップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加してストリーミングアップロードモードで処理結果をSnowflakeに書き込む方法を示します。このモードはSnowpipe Streaming APIを使ったリアルタイム取り込みを可能にします。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**はデフォルトの`Create Action`のまま、または既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sink名（例：`snowflake_sink_streaming`）と簡単な説明を入力。

4. コネクターのドロップダウンから先に作成した`my-snowflake-streaming`を選択。隣の作成ボタンでポップアップから新規コネクター作成も可能。設定パラメータは[ストリーミングコネクターの作成](#create-a-snowflake-streaming-connector)を参照。

5. ストリーミングアップロードモードの設定を行う。

   - **Database Name**：`testdatabase`。EMQXデータ保存用に作成したSnowflakeデータベース名。
   - **Schema**：`public`。`testdatabase`内のデータテーブルが存在するスキーマ名。
   - **Pipe**：`emqxstreaming`。SQL文で作成したSnowflakeストリーミングパイプ名。Snowflakeで定義した名前と完全一致させる必要あり。
   - **HTTP Pipelining**：応答を待たずに送信可能なHTTPリクエストの最大数。デフォルト：`100`。
   - **Connect Timeout**：Snowflakeへの接続確立のタイムアウト秒数。デフォルト：`15`秒。
   - **Connection Pool Size**：EMQXがこのSink用にSnowflakeと維持可能な同時接続数の最大値。デフォルト：`8`。
   - **Max Inactive**：アイドル状態の接続を閉じるまでの最大待機時間（秒）。デフォルト：`10`秒。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeサーバーへの接続テストが可能。

9. **Create**をクリックしてSink作成完了。作成成功後はルール作成画面に戻り、新規Sinkがルールアクションに追加される。

## ルールのテスト

このセクションでは、設定したルールのテスト方法を示します。

### テストメッセージのパブリッシュ

MQTTXを使い、トピック`t/1`にメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

複数回繰り返して複数のテストメッセージを生成してください。

### Snowflake内のデータ検証

テストメッセージ送信後、Snowflakeにデータが正常に書き込まれたかを以下の手順で確認します。

1. SnowflakeのWebインターフェースにログイン。

2. Snowflakeコンソールで以下のSQLクエリを実行し、ルールで書き込まれた`emqx`テーブルのデータを表示：

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   `clientid`、`topic`、`payload`、`publish_received_at`フィールドを含む全レコードが表示されます。

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）およびトピックやタイムスタンプなどのメタデータが確認できます。

## 詳細設定

このセクションでは、Snowflake Sinkの詳細設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、以下のパラメータをニーズに応じて調整可能です。

| 項目名                         | 説明                                                                                         | デフォルト値    |
| ------------------------------ | -------------------------------------------------------------------------------------------- | -------------- |
| **Buffer Pool Size**            | EMQXとSnowflake間のデータフローを管理するバッファワーカープロセスの数を指定します。これらのワーカーはデータを一時的に保存・処理し、送信前のパフォーマンス最適化とスムーズなデータ転送を支えます。 | `16`           |
| **Request TTL**                 | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。TTLを超えたリクエストや、送信後にSnowflakeから応答やアックを受け取れなかったリクエストは期限切れと判定されます。 | `45`           |
| **Health Check Interval**       | SinkがSnowflakeとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。               | `15`           |
| **Health Check Interval Jitter**| 複数ノードが同時にヘルスチェックを開始するのを避けるため、基本間隔に加える一様ランダム遅延時間（ミリ秒）です。複数のアクションやソースが同一コネクターを共有する場合に有効です。 | `0`            |
| **Health Check Timeout**        | Snowflakeとの接続ヘルスチェックのタイムアウト時間（秒）を指定します。                        | `60`           |
| **Max Buffer Queue Size**       | Snowflake Sinkの各バッファワーカーがバッファリング可能な最大バイト数を指定します。ワーカーはデータを一時的に保存し、効率的なデータストリーム処理を実現します。システム性能やデータ転送要件に応じて調整してください。 | `256` MB       |
| **Query Mode**                  | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードではSnowflakeへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受信する可能性があります。 | `Asynchronous` |
| **Batch Size**                  | EMQXからSnowflakeへ一度に転送するデータバッチの最大サイズを指定します。サイズ調整により転送効率や性能を最適化可能です。<br />`1`に設定するとバッチ化せず個別に送信します。 | `100`          |
| **Inflight Window**             | 送信済みだが応答やアックをまだ受け取っていない「インフライト」キューリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合に重要で、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`          |
