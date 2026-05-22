# SnowflakeへのMQTTデータ取り込み

<<<<<<< HEAD
[Snowflake](https://www.snowflake.com/en/)は、クラウドベースのデータプラットフォームであり、高いスケーラビリティと柔軟性を備えたデータウェアハウジング、分析、セキュアなデータ共有のソリューションを提供します。構造化データおよび半構造化データの処理に優れ、大量のデータを高速なクエリ性能で保存し、さまざまなツールやサービスとシームレスに統合できるよう設計されています。

本ページでは、EMQXとSnowflake間のデータ統合について詳細に紹介し、ルールおよびSinkの作成方法について実践的なガイドを提供します。
=======
[Snowflake](https://www.snowflake.com/en/) は、クラウドベースのデータプラットフォームであり、高いスケーラビリティと柔軟性を備えたデータウェアハウジング、分析、セキュアなデータ共有のソリューションを提供します。構造化データおよび半構造化データの処理に優れ、大量のデータを高速なクエリ性能で保存し、さまざまなツールやサービスとシームレスに統合できるよう設計されています。

本ページでは、EMQXとSnowflake間のデータ統合について詳しく紹介し、ルールとSinkの作成方法について実践的なガイダンスを提供します。
>>>>>>> origin/release-6.1

## 動作の仕組み

<<<<<<< HEAD
EMQXにおけるSnowflakeデータ統合はすぐに使える機能であり、複雑なIoTビジネスワークフローを簡単にサポートできるよう構成可能です。典型的なIoTアプリケーションでは、EMQXがデバイス接続およびメッセージ送受信を担うIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、保存、分析を行うデータストレージおよび処理プラットフォームとして役割を果たします。
=======
EMQXにおけるSnowflakeデータ統合はすぐに使える機能であり、複雑なIoTビジネスワークフローを簡単にサポートできるように構成可能です。典型的なIoTアプリケーションでは、EMQXがデバイスの接続とメッセージ送受信を担うIoTプラットフォームとして機能し、Snowflakeはメッセージデータの取り込み、保存、分析を行うデータストレージおよび処理プラットフォームとして役割を果たします。
>>>>>>> origin/release-6.1

![snowflake-architecture](./assets/snowflake-architecture.png)

EMQXはルールエンジンとSinkを利用してデバイスのイベントやデータをSnowflakeに転送します。エンドユーザーやアプリケーションはSnowflakeのテーブル内のデータにアクセス可能です。具体的なワークフローは以下の通りです：

1. **デバイスのEMQXへの接続**：IoTデバイスはMQTTプロトコルで正常に接続されるとオンラインイベントをトリガーします。このイベントにはデバイスID、送信元IPアドレスなどの識別情報が含まれます。

<<<<<<< HEAD
2. **デバイスからのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリおよびステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジン内で比較処理を行います。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールをマッチングし、データフォーマット変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などの処理を行います。

4. **Snowflakeへの書き込み**：ルールはメッセージデータをSnowflakeに書き込むアクションをトリガーします。メッセージをファイルにバッチングしてStageおよびPipe経由でロードする（集約モード）、またはSnowpipe Streaming APIを用いて直接ストリーミングする（ストリーミングモード）方法があります。

イベントやメッセージデータがSnowflakeに書き込まれた後は、以下のようなビジネスや技術的な目的で活用可能です：

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの利用を保証します。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を行い、予知保全、運用インサイト、デバイス性能評価を実現します。

## 特長と利点

EMQXのSnowflakeデータ統合を利用することで、以下の特長と利点をビジネスにもたらします：

- **メッセージ変換**：メッセージはEMQXのルール内で高度に処理・変換されてからSnowflakeに書き込まれるため、後続の保存や活用が容易になります。
- **柔軟なデータ操作**：Snowflake Sinkは書き込むフィールドを選択可能であり、ビジネスニーズに応じた効率的かつ動的なストレージ構成を実現します。
- **統合されたビジネスプロセス**：Snowflake SinkによりデバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせ、多様なビジネスシナリオ（データ分析やアーカイブなど）を可能にします。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は従来のデータベースより低コストで長期データ保持に最適であり、大量のIoTデータ保存に適しています。

これらの特長により、効率的で信頼性が高くスケーラブルなIoTアプリケーションを構築し、ビジネスの意思決定や最適化に役立てることができます。

## はじめる前に

このセクションでは、EMQXでSnowflake Sinkを作成する前に必要な準備について説明します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の基本概念の理解
- 管理者権限を持つ動作中のSnowflakeアカウント

### アップロードモードの選択

::: tip

最初にモードを選択してください。これによりEMQXとSnowflake環境の両方の設定方法が決まります。

:::

EMQXはSnowflakeへのデータ送信に以下の2つのモードをサポートしています：

| モード       | 説明                                                         | ODBC 必須か    |
| ------------ | ------------------------------------------------------------ | ------------- |
| 集約モード   | EMQXはMQTTメッセージをローカルファイルにバッファリングし、Snowflakeのステージにアップロードします。`COPY INTO`文で設定されたパイプが自動的にこれらのファイルをターゲットテーブルにロードします。詳細は[Snowflake Snowpipeドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)を参照してください。 | 必須          |
| ストリーミングモード | Snowpipe Streaming APIを介してリアルタイムにデータを送信し、行を直接Snowflakeテーブルに書き込みます。 | 必須          |
=======
2. **デバイスのメッセージパブリッシュと受信**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジン内で比較処理を行います。

3. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージやイベントを処理します。対応するルールをマッチさせ、データフォーマット変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を実施します。

4. **Snowflakeへの書き込み**：ルールはメッセージデータをSnowflakeに書き込むアクションをトリガーします。メッセージをファイルにバッチングしてStageとPipe経由でロードする（集約モード）か、Snowpipe Streaming APIを使って直接ストリーミングする（ストリーミングモード）かのいずれかです。

イベントやメッセージデータがSnowflakeに書き込まれた後は、以下のようなビジネスおよび技術的な目的で利用可能です：

- **データアーカイブ**：IoTデータをSnowflakeに安全に長期保存し、コンプライアンスや履歴データの利用を保証します。
- **データ分析**：Snowflakeのデータウェアハウジングおよび分析機能を活用し、リアルタイムまたはバッチ分析を行い、予知保全、運用インサイト、デバイス性能評価を可能にします。

## 特長と利点

EMQXのSnowflakeデータ統合を利用することで、以下の特長と利点が得られます：

- **メッセージ変換**：Snowflakeに書き込む前に、EMQXのルールでメッセージを多様に処理・変換でき、後続の保存や利用を容易にします。
- **柔軟なデータ操作**：Snowflake Sinkは書き込むフィールドを選択可能で、ビジネスニーズに応じた効率的かつ動的なストレージ構成を実現します。
- **統合されたビジネスプロセス**：Snowflake SinkによりデバイスデータをSnowflakeの豊富なエコシステムアプリケーションと組み合わせられ、データ分析やアーカイブなど多様なビジネスシナリオを実現します。
- **低コストの長期保存**：Snowflakeのスケーラブルなストレージ基盤は、従来のデータベースより低コストで長期データ保持に最適であり、大量のIoTデータ保存に適しています。

これらの特長により、効率的で信頼性が高くスケーラブルなIoTアプリケーションを構築し、ビジネスの意思決定や最適化に役立てられます。

## はじめる前に

このセクションでは、EMQXでSnowflake Sinkを作成する前の準備について説明します。

### 前提条件

- EMQXの[ルール](./rules.md)および[データ統合](./data-bridges.md)の基本概念の理解。
- 管理者権限を持つ有効なSnowflakeアカウント。
>>>>>>> origin/release-6.1

### アップロードモードの選択

<<<<<<< HEAD
EMQXがSnowflakeと通信し効率的にデータを転送するために、SnowflakeのOpen Database Connectivity（ODBC）ドライバーをインストールおよび設定する必要があります。このドライバーはEMQXがSnowflakeのステージにデータを書き込むための通信ブリッジとして機能し、データの適切なフォーマット、認証、転送を保証します。

詳細は公式の[ODBCドライバー](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な展開と必要なシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法の推奨ではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh`スクリプトをローカルにコピーし、`chmod a+x`で実行権限を付与してから`sudo`で実行します：

=======
::: tip

最初にモードを選択してください。これはEMQXとSnowflake環境の両方の設定方法を決定します。

:::

EMQXはSnowflakeへのデータ送信に以下の2つのモードをサポートしています：

| モード       | 説明                                                         | ODBC必要性    |
| ---------- | ------------------------------------------------------------ | ------------- |
| 集約（Aggregated） | EMQXがMQTTメッセージをローカルファイルにバッファリングし、SnowflakeのStageにアップロードします。`COPY INTO`文で設定されたPipeが自動的にステージファイルをターゲットテーブルにロードします。詳細は[Snowflake Snowpipeドキュメント](https://docs.snowflake.com/en/user-guide/data-load-snowpipe-intro)を参照。 | 必須          |
| ストリーミング（Streaming） | Snowpipe Streaming APIを介してリアルタイムにデータを送信し、行を直接Snowflakeテーブルに書き込みます。 | 必須          |

### Snowflake ODBCドライバーの初期化

EMQXがSnowflakeと通信し効率的にデータ転送を行うために、SnowflakeのOpen Database Connectivity（ODBC）ドライバーをインストールおよび設定する必要があります。このドライバーはEMQXがSnowflakeのStageにデータを書き込むための通信橋渡し役を果たし、データの適切なフォーマット、認証、転送を保証します。

詳細は公式の[ODBCドライバー](https://docs.snowflake.com/en/developer-guide/odbc/odbc)ページおよび[ライセンス契約](https://sfc-repo.snowflakecomputing.com/odbc/Snowflake_ODBC_Driver_License_Agreement.pdf)を参照してください。

#### LinuxでのSnowflake ODBCドライバー初期化

EMQXはDebian系（Ubuntuなど）向けにSnowflake ODBCドライバーの迅速な導入と必要なシステム設定を行う[インストールスクリプト](https://github.com/emqx/emqx/blob/master/scripts/install-snowflake-driver.sh)を提供しています。

::: tip 注意

このスクリプトはテスト用であり、本番環境でのODBCドライバー設定方法として推奨するものではありません。公式の[Linux向けインストール手順](https://docs.snowflake.com/en/developer-guide/odbc/odbc-linux)を参照してください。

:::

**インストールスクリプトの実行**

`scripts/install-snowflake-driver.sh` スクリプトをローカルマシンにコピーし、`chmod a+x`で実行権限を付与後、`sudo`で実行します：

>>>>>>> origin/release-6.1
```bash
chmod a+x scripts/install-snowflake-driver.sh
sudo ./scripts/install-snowflake-driver.sh
```

<<<<<<< HEAD
スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリに自動ダウンロードし、ドライバーをインストールして以下のシステム設定ファイルを更新します：
=======
スクリプトはSnowflake ODBCの`.deb`インストールパッケージ（例：`snowflake-odbc-3.4.1.x86_64.deb`）をカレントディレクトリに自動ダウンロードし、ドライバーをインストール、以下のシステム設定ファイルを更新します：
>>>>>>> origin/release-6.1

- `/etc/odbc.ini`：Snowflakeデータソース設定を追加
- `/etc/odbcinst.ini`：Snowflakeドライバーパスを登録

**設定例**

<<<<<<< HEAD
`/etc/odbc.ini`の内容を確認するコマンド：
=======
`/etc/odbc.ini`の設定を確認するコマンド例：
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
`/etc/odbcinst.ini`の内容を確認するコマンド：
=======
`/etc/odbcinst.ini`の設定を確認するコマンド例：
>>>>>>> origin/release-6.1

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

macOSでSnowflake ODBCドライバーをインストールおよび設定する手順は以下の通りです：

1. unixODBCをインストール（例）：

   ```
   brew install unixodbc
   ```

<<<<<<< HEAD
2. [iODBCをダウンロードしてインストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)。

3. [Snowflake ODBCドライバーをダウンロードしてインストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)。
=======
2. [iODBCをダウンロードおよびインストール](https://github.com/openlink/iODBC/releases/download/v3.52.16/iODBC-SDK-3.52.16-macOS11.dmg)。

3. [Snowflake ODBCドライバーをダウンロードおよびインストール](https://sfc-repo.snowflakecomputing.com/odbc/macuniversal/3.3.2/snowflake_odbc_mac_64universal-3.3.2.dmg)。
>>>>>>> origin/release-6.1

4. 詳細なインストールおよび設定手順は[macOS向けODBCドライバーのインストールと設定](https://docs.snowflake.com/en/developer-guide/odbc/odbc-mac)を参照してください。

5. インストール後、以下の設定ファイルを更新します。

   - Snowflake ODBCドライバーの権限と設定を更新：

     ```bash
     chown $(id -u):$(id -g) /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     echo 'ODBCInstLib=libiodbcinst.dylib' >> /opt/snowflake/snowflakeodbc/lib/universal/simba.snowflake.ini
     ```

<<<<<<< HEAD
   - ODBC接続設定のため`~/.odbc.ini`ファイルを作成または更新：
=======
   - ODBC接続設定のために`~/.odbc.ini`ファイルを作成または更新：
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
アップロードモードに関わらず、Snowflake環境でユーザーアカウント、データベース、関連リソースを設定し、データ取り込み用の準備を行う必要があります。以下の認証情報は後でEMQXのコネクターおよびSink設定時に必要となります：

| 項目名                 | 値                                               | 説明                                                         |
| ---------------------- | ------------------------------------------------ | ------------------------------------------------------------ |
| データソース名 (DSN)   | `snowflake`（集約モードのみ）                    | `/etc/odbc.ini`で設定したODBC DSN。集約アップロード時に使用。 |
| ユーザー名             | `snowpipeuser`                                   | Snowflake接続認証に使用するユーザー。適切な権限を持つ必要あり。 |
| パスワード             | `Snowpipeuser99`                                 | キーペア認証使用時は省略可能。                               |
| データベース名         | `testdatabase`                                   | 対象テーブルが存在するSnowflakeデータベース名。              |
| スキーマ               | `public`                                         | データベース内のスキーマ名。テーブルやパイプが存在する場所。  |
| ステージ（集約モード） | `emqx`                                           | データ取り込み前にファイルを保持するSnowflakeステージ名。    |
| パイプ（集約モード）   | `emqx`                                           | ステージからテーブルへデータをロードするパイプ名。            |
| パイプ（ストリーミング） | `emqxstreaming`                                  | Snowpipe Streaming API経由でデータを取り込むためのパイプ名。  |
| プライベートキー       | `file://<path to snowflake_rsa_key.private.pem>` | API認証用JWT署名に使用するRSAプライベートキーのパス。          |
=======
アップロードモードに関わらず、Snowflake環境においてユーザーアカウント、データベース、関連リソースを設定し、データ取り込み用の準備を行う必要があります。以下の認証情報は後でEMQXのコネクターおよびSink設定時に必要となります：

| 項目名                   | 値                                               | 説明                                                         |
| ---------------------- | ------------------------------------------------ | ------------------------------------------------------------ |
| データソース名（DSN）    | `snowflake`（集約モードのみ）                     | `/etc/odbc.ini`に設定されたODBC DSN。集約アップロード用。    |
| ユーザー名               | `snowpipeuser`                                   | Snowflake接続認証に使用するユーザー。適切な権限が必要。       |
| パスワード               | `Snowpipeuser99`                                 | キーペア認証使用時は省略可能。                                |
| データベース名           | `testdatabase`                                   | 対象テーブルが存在するSnowflakeデータベース。                 |
| スキーマ                 | `public`                                         | データベース内のスキーマ。テーブルおよびパイプが存在する場所。 |
| ステージ（集約モード）    | `emqx`                                           | データ取り込み前にファイルを保持するSnowflakeステージ。       |
| パイプ（集約モード）      | `emqx`                                           | ステージからテーブルへデータをロードするパイプ。               |
| パイプ（ストリーミング）  | `emqxstreaming`                                  | Snowpipe Streaming API用に`DATA_SOURCE(TYPE => 'STREAMING')`で作成したパイプ。 |
| プライベートキー          | `file://<path to snowflake_rsa_key.private.pem>` | API認証用JWT署名に使用するRSA秘密鍵のパス。                    |
>>>>>>> origin/release-6.1

#### RSAキーペアの生成（集約モードは任意）

Snowflakeは複数の認証方式をサポートしており、EMQXではアップロードモードや接続設定に応じて選択します：

<<<<<<< HEAD
| アップロードモード     | 認証オプション                                               | キーペア必須か   |
| --------------------- | ------------------------------------------------------------ | --------------- |
| ストリーミング（HTTPS） | RSAキーペア + JWT（唯一のサポート方式）                      | 必須            |
| 集約（ODBC）           | ユーザー名/パスワード（DSNまたはEMQX経由）<br />RSAキーペア + JWT（任意、EMQX設定のみ） | 任意            |

キーペア認証はストリーミングモードで必須であり、EMQXはJWTを署名してSnowflakeのStreaming APIに安全に認証します。

集約モードではユーザー名/パスワードまたはRSAキーペアのいずれかで認証可能です。認証情報は以下のいずれかの方法で提供できます：

- ダッシュボードのEMQXコネクター設定でユーザー名とパスワードを直接入力
- キーペア認証を使う場合はプライベートRSAキーのパスを指定
- EMQXでいずれも指定しない場合は、システムのODBC DSN（Linuxの`/etc/odbc.ini`やmacOSの`~/.odbc.ini`）に正しく設定されていることを確認
=======
| アップロードモード       | 認証オプション                                               | キーペア必要性    |
| ----------------- | ------------------------------------------------------------ | ----------------- |
| ストリーミング（HTTPS） | RSAキーペア＋JWT（唯一のサポート方式）                        | 必須              |
| 集約（ODBC）         | ユーザー名/パスワード（DSNまたはEMQX経由）<br />RSAキーペア＋JWT（任意、EMQXのみ設定） | 任意              |

キーペア認証はストリーミングモードで必須であり、EMQXはJWTに署名してSnowflakeのStreaming APIに安全に認証します。

集約モードではユーザー名/パスワードまたはRSAキーペアのいずれかを使用可能です。認証情報の提供方法は以下のいずれかです：

- ダッシュボードのEMQXコネクター設定にユーザー名とパスワードを直接入力。
- キーペア認証の場合は秘密鍵のパスを指定。
- EMQXにいずれも設定しない場合は、Linuxの`/etc/odbc.ini`やmacOSの`~/.odbc.ini`などのシステムDSNに正しく設定されていることを確認。
>>>>>>> origin/release-6.1

::: tip

認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。

<<<<<<< HEAD
EMQXでどちらも設定されていない場合、コネクターは`/etc/odbc.ini`の認証情報を参照します。

:::

**例：ユーザー名/パスワードを含む`/etc/odbc.ini`**
=======
EMQXにどちらも設定しない場合は、`/etc/odbc.ini`の認証情報が使用されます。

:::

**例：ユーザー名/パスワードを使った`/etc/odbc.ini`**
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
> この方法により、EMQXは設定で直接認証情報を含めずに`DSN`（`snowflake`）を参照できます。

**キーペア認証を利用する場合**

RSAキーペア認証を使用する場合（例：ストリーミングモード）、以下のコマンドで鍵を生成し設定します：
=======
> この方法により、EMQXは設定内で直接認証情報を含めずにDSN（`snowflake`）を参照できます。

**キーペア認証を使用する場合**

RSAキーペア認証を使用または必須とする（例：ストリーミングモード）場合、以下のコマンドで鍵を生成し設定します：
>>>>>>> origin/release-6.1

```bash
# 秘密鍵の生成
openssl genrsa 2048 | openssl pkcs8 -topk8 -inform PEM -out snowflake_rsa_key.private.pem -nocrypt

# 公開鍵の生成
openssl rsa -in snowflake_rsa_key.private.pem -pubout -out snowflake_rsa_key.public.pem
```

EMQXがキーペア認証を使用する場合（集約・ストリーミング両モード対応）：

<<<<<<< HEAD
- EMQXはプライベートRSAキーでJWTに署名し、安全で検証可能なIDトークンとして利用
- Snowflakeは公開鍵で署名を検証

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLでSnowflakeリソースをセットアップ
=======
- EMQXは秘密鍵でJWTに署名し、安全かつ検証可能なIDトークンとして利用。
- Snowflakeは公開鍵で署名を検証。

詳細は[キーペア認証とキーペアローテーション](https://docs.snowflake.com/en/user-guide/key-pair-auth)を参照してください。

#### SQLによるSnowflakeリソースのセットアップ
>>>>>>> origin/release-6.1

RSAキーペア生成後、`aggregated`または`streaming`の取り込み用に必要なSnowflakeオブジェクトをSQLで作成します。

対象は以下を含みます：

- データベースとテーブルの作成
<<<<<<< HEAD
- ステージとパイプの作成（集約モード）
- ストリーミングパイプの作成（ストリーミングモード）
=======
- ステージとパイプの作成（`aggregated`用）
- ストリーミングパイプの作成（`streaming`用）
>>>>>>> origin/release-6.1
- ユーザーとロールの作成および権限付与

1. SnowflakeコンソールのSQLワークシートで以下のSQLを実行し、データベース、テーブル、ステージ、パイプを作成します：

   ```sql
   USE ROLE accountadmin;

<<<<<<< HEAD
   -- データ保存用データベース作成（存在しない場合）
=======
   -- データ格納用データベース作成（存在しない場合）
>>>>>>> origin/release-6.1
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

<<<<<<< HEAD
   -- ステージからコピーする集約モード用パイプ作成
=======
   -- ステージからロードする集約モード用パイプ作成
>>>>>>> origin/release-6.1
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

<<<<<<< HEAD
   - パイプ内の`COPY INTO`文により、Snowflakeはステージまたはストリーミングされたデータを自動的にテーブルにロードします。
=======
   - パイプ内の`COPY INTO`により、Snowflakeはステージまたはストリーミングされたデータを自動的にテーブルにロードします。
>>>>>>> origin/release-6.1
   - ストリーミングパイプの`$1:field`構文は、EMQX経由で取り込まれたJSONペイロードからフィールドを抽出します。

2. EMQXが認証に使用する専用ユーザー（例：`snowpipeuser`）を作成し、RSA公開鍵をバインドします：

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

<<<<<<< HEAD
   PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`行は削除し、残りの内容を改行を保持したまま含めてください。
=======
   PEMファイルの`-----BEGIN PUBLIC KEY-----`および`-----END PUBLIC KEY-----`の行は削除し、改行を保持したまま残りの内容を貼り付けてください。
>>>>>>> origin/release-6.1

   :::

   この鍵はSnowflakeユーザーにアップロードされ、Snowflake内に保存されます。

<<<<<<< HEAD
3. 必要なロールを作成し、ユーザーに割り当ててSnowflakeリソースを管理できるようにします：
=======
3. ユーザーに必要なロールを作成し、Snowflakeリソース管理権限を付与します：
>>>>>>> origin/release-6.1

   ```sql
   CREATE OR REPLACE ROLE snowpipe;
   
<<<<<<< HEAD
   -- データベースとスキーマの使用権限付与
=======
   -- データベースおよびスキーマの使用権限付与
>>>>>>> origin/release-6.1
   GRANT USAGE ON DATABASE testdatabase TO ROLE snowpipe;
   GRANT USAGE ON SCHEMA testdatabase.public TO ROLE snowpipe;
   GRANT INSERT, SELECT ON testdatabase.public.emqx TO ROLE snowpipe;
   
   -- 集約モード用にステージとパイプの権限付与
   GRANT READ, WRITE ON STAGE testdatabase.public.emqx TO ROLE snowpipe;
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqx TO ROLE snowpipe;
   
   -- ストリーミングモード用にストリーミングパイプの権限付与
   GRANT OPERATE, MONITOR ON PIPE testdatabase.public.emqxstreaming TO ROLE snowpipe;
   
<<<<<<< HEAD
   -- ユーザーにロールを割り当て、デフォルトに設定
=======
   -- ユーザーにロールを付与しデフォルトに設定
>>>>>>> origin/release-6.1
   GRANT ROLE snowpipe TO USER snowpipeuser;
   ALTER USER snowpipeuser SET DEFAULT_ROLE = snowpipe;
   ```

## 集約モード用Snowflakeコネクターの作成

<<<<<<< HEAD
Snowflake Sinkで集約アップロードモードを使用する場合、Snowflake環境との接続を確立するためにSnowflakeコネクターを作成する必要があります。このコネクターはODBC（DSN経由）を使ってステージに接続します。
=======
Snowflake Sinkで集約アップロードモードを使用する場合は、Snowflake環境との接続を確立するためにSnowflakeコネクターを作成する必要があります。このコネクターはODBC（DSN経由）を使用してステージを通じて接続します。
>>>>>>> origin/release-6.1

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

<<<<<<< HEAD
3. コネクタータイプで **Snowflake** を選択し、次へ進みます。

4. コネクター名を入力します。英数字の組み合わせで、ここでは `my-snowflake` とします。
=======
3. コネクタータイプで **Snowflake** を選択し、次へ。

4. コネクター名を入力（英数字の組み合わせ）。例として `my-snowflake` と入力。
>>>>>>> origin/release-6.1

5. 接続情報を入力。

<<<<<<< HEAD
   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式で、実際にはSnowflakeインスタンス固有のサブドメインに置き換えます。

   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切って入力します。SnowflakeコンソールのURLに含まれています。
=======
   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換え。

   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切って入力。SnowflakeコンソールのURLの一部。
>>>>>>> origin/release-6.1

   - **Data Source Name (DSN)**：ODBCドライバー設定時に`.odbc.ini`で設定した`snowflake`を入力。

   - **Username**：前述の設定で作成した`snowpipeuser`を入力。

<<<<<<< HEAD
   - **Password**：SnowflakeへのODBC認証用パスワード。任意入力です。

     - ここに`Snowpipeuser99`などを入力してもよいです。

     - または`/etc/odbc.ini`に設定している場合は空欄でも構いません。

     - キーペア認証を使う場合は空欄にします。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。ここで設定しない場合は`/etc/odbc.ini`の認証情報を参照します。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使用するRSAプライベートキーの絶対パス。クラスター内の全ノードで共通のパスである必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。暗号化なし（OpenSSLの`-nocrypt`オプション）で生成した場合は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシはサポートされていません。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`をオンにし、以下を入力：

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス

     - **Proxy Port**：プロキシサーバーのポート番号

6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 高度な設定（任意）：[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクターの作成が完了し、ルールとSinkを作成してSnowflakeへのデータ書き込みを指定できます。

## ストリーミングモード用Snowflakeコネクターの作成

Snowflake Sinkでストリーミングアップロードモードを使用する場合、Snowflake環境との接続を確立するためにSnowflakeストリーミングコネクターを作成します。このコネクターはHTTPSおよびSnowpipe Streaming REST APIを使用します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプで **Snowflake Streaming** を選択し、次へ進みます。

4. コネクター名を入力します。英数字の組み合わせで、ここでは `my-snowflake-streaming` とします。

5. 接続情報を入力します。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式で、実際にはSnowflakeインスタンス固有のサブドメインに置き換えます。

   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切って入力します。SnowflakeコンソールのURLに含まれています。

   - **Pipe User**：対象パイプの操作権限を持つSnowflakeユーザー名（例：`snowpipeuser`）。`OPERATE`および`MONITOR`権限が必要です。

   - **Private Key Path**：EMQXがJWT署名に使用するRSAプライベートキー。PEM形式のキー全文を文字列として貼り付けるか、`file://`で始まるファイルパスを指定可能。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`

   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。暗号化なしで生成した場合は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシはサポートされていません。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`をオンにし、以下を入力：

     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス

     - **Proxy Port**：プロキシサーバーのポート番号

6. 暗号化接続を確立したい場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。ストリーミングモードではHTTPS通信のためTLS必須です。

7. 高度な設定（任意）：[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続テストが可能です。

9. **Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクターの作成が完了し、ルールとSinkを作成してSnowflakeへのデータ書き込みを指定できます。
=======
   - **Password**：ODBC経由のユーザー名/パスワード認証用パスワード。任意入力：
     - ここに`Snowpipeuser99`などを入力可能。
     - または`/etc/odbc.ini`に設定。
     - キーペア認証の場合は空欄に。

       ::: tip

       認証にはパスワードかプライベートキーのいずれかを使用し、両方を同時に使わないでください。ここに設定がない場合は`/etc/odbc.ini`の認証情報が使用されます。

       :::

   - **Private Key Path**：ODBC経由でSnowflake認証に使用するRSA秘密鍵の絶対パス。クラスター内の全ノードで同一パスである必要があります。例：`/etc/emqx/certs/snowflake_rsa_key.private.pem`。

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号用パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。
     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を有効にする場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のためTLS必須。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続確認が可能。

9. **Create**ボタンをクリックしてコネクター作成を完了。

これでコネクターの作成が完了し、ルールとSinkを作成してSnowflakeへの書き込みを指定できます。
>>>>>>> origin/release-6.1

## Snowflakeストリーミングコネクターの作成

<<<<<<< HEAD
このセクションでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sinkを通じて書き込む方法を示します。

### SQLを定義したルールの作成
=======
Snowflake Sinkでストリーミングアップロードモードを使用する場合は、Snowflake環境との接続を確立するためにSnowflakeストリーミングコネクターを作成します。このコネクターはHTTPSとSnowpipe Streaming REST APIを使用します。
>>>>>>> origin/release-6.1

1. ダッシュボードの **Integration** -> **Connector** ページに移動。

2. 右上の **Create** ボタンをクリック。

<<<<<<< HEAD
3. ルールIDに `my_rule` を入力し、SQLエディターに以下のルールSQLを入力します：
=======
3. コネクタータイプで **Snowflake Streaming** を選択し、次へ。

4. コネクター名を入力（英数字の組み合わせ）。例として `my-snowflake-streaming` と入力。

5. 接続情報を入力。

   - **Server Host**：SnowflakeのエンドポイントURL。通常は `<Your Snowflake Organization ID>-<Your Snowflake Account Name>.snowflakecomputing.com` の形式。`<Your Snowflake Organization ID>-<Your Snowflake Account Name>` はSnowflakeインスタンス固有のサブドメインに置き換え。

   - **Account**：Snowflakeの組織IDとアカウント名をハイフン（`-`）で区切って入力。SnowflakeコンソールのURLの一部。

   - **Pipe User**：対象パイプを操作できる権限を持つSnowflakeユーザー名。例：`snowpipeuser`。ロールには少なくとも`OPERATE`と`MONITOR`権限が必要。

   - **Private Key Path**：EMQXがJWT署名に使用するRSA秘密鍵。PEM形式の秘密鍵全文を文字列として貼り付けるか、`file://`で始まる秘密鍵ファイルのパスを指定可能。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`。

   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号用パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄。

   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。
     - **Proxy Port**：プロキシサーバーのポート番号。

6. 暗号化接続を有効にする場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照。ストリーミングモードではHTTPS通信のためTLS必須。

7. 詳細設定（任意）：[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSnowflakeへの接続確認が可能。

9. **Create**ボタンをクリックしてコネクター作成を完了。

これでコネクターの作成が完了し、ルールとSinkを作成してSnowflakeへの書き込みを指定できます。

## Snowflake Sinkを用いたルールの作成

このセクションでは、EMQXでルールを作成し、メッセージ（例：ソースMQTTトピック`t/#`）を処理して、処理結果を設定済みのSnowflake Sink経由でSnowflakeに書き込む方法を示します。

### SQLを定義したルールの作成

1. ダッシュボードの **Integration** -> **Rules** ページに移動。

2. 右上の **Create** ボタンをクリック。

3. ルールIDに `my_rule` を入力し、SQLエディターに以下のルールSQLを入力：
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
   Snowflake連携では、選択するフィールドがSnowflakeのテーブルの列数および名前と完全に一致することが重要です。余分なフィールドを追加したり`*`で全選択するのは避けてください。

   :::

4. ルールにアクションを追加してSinkを設定します。
   - 集約アップロードモードでSnowflakeに書き込む場合は、[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照してください。
   - ストリーミングアップロードモードでSnowflakeに書き込む場合は、[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照してください。

5. アクション追加後、**Action Outputs**セクションに新規追加されたSinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックしてルール作成を完了します。

これでルールが正常に作成され、**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブに新しいSnowflake Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析され、Snowflakeに書き込まれる流れを視覚的に確認できます。

### 集約アップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、集約アップロードモードで処理結果をSnowflakeに書き込む方法を示します。このモードは複数のルールトリガー結果を単一ファイル（例：CSVファイル）にまとめてSnowflakeにアップロードし、ファイル数を削減して書き込み効率を向上させます。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加します。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成してルールに追加します。

3. Sinkの名前（例：`snowflake_sink`）と簡単な説明を入力します。

4. **Connectors**ドロップダウンから先に作成した`my-snowflake`コネクターを選択します。ドロップダウン横の作成ボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。必要な設定パラメーターは[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照してください。

5. 集約アップロードモードの設定を行います。

   - **Database Name**：`testdatabase`を入力。EMQXデータ保存用に作成したSnowflakeデータベースです。
   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルが存在するスキーマ名です。
   - **Stage**：`emqx`を入力。Snowflakeで作成したデータアップロード前のステージ名です。
   - **Pipe**：`emqx`を入力。ステージからテーブルへのロードを自動化するパイプ名です。
   - **Pipe User**：`snowpipeuser`を入力。パイプ管理権限を持つSnowflakeユーザーです。
   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSAプライベートキー。以下のいずれかの形式で指定可能：
     - **プレーンテキスト**：PEM形式のプライベートキー全文を文字列として直接貼り付け
     - **ファイルパス**：`file://`で始まるプライベートキーファイルのパス。クラスター内の全ノードで共通かつEMQXアプリケーションユーザーがアクセス可能である必要があります。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`
   - **Private Key Password**：プライベートキーが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した場合は空欄にします。

   - **Aggregation Upload Format**：現状は`csv`のみサポート。データはカンマ区切りCSV形式でSnowflakeにステージされます。
   - **Column Order**：ドロップダウンから列の並び順を選択。生成されるCSVファイルは選択した列順にソートされ、未選択列はアルファベット順にソートされます。
   - **Max Records**：集約がトリガーされる最大レコード数。例：`1000`に設定すると1000件収集後にアップロードされ、時間間隔はリセットされます。
   - **Time Interval**：集約が発生する時間間隔（秒）。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにアップロードされ、最大レコード数はリセットされます。
   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシはサポートされていません。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`をオンにし、以下を入力：
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス
     - **Proxy Port**：プロキシサーバーのポート番号

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて高度な設定を行います（任意）。詳細は[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSinkがSnowflakeサーバーに接続できるかテスト可能です。

9. **Create**ボタンをクリックしてSink作成を完了します。作成成功後、ページはルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

### ストリーミングアップロードモードでSnowflake Sinkを追加

このセクションでは、ルールにSinkを追加し、ストリーミングアップロードモードで処理結果をSnowflakeに書き込む方法を示します。このモードはSnowpipe Streaming APIを使ったリアルタイム取り込みを可能にします。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加します。

2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択します。ここでは新規Sinkを作成してルールに追加します。

3. Sinkの名前（例：`snowflake_sink_streaming`）と簡単な説明を入力します。

4. コネクタードロップダウンから先に作成した`my-snowflake-streaming`コネクターを選択します。ドロップダウン横の作成ボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。必要な設定パラメーターは[ストリーミングモード用Snowflakeコネクターの作成](#create-a-snowflake-streaming-connector)を参照してください。

5. ストリーミングアップロードモードの設定を行います。

   - **Database Name**：`testdatabase`を入力。EMQXデータ保存用に作成したSnowflakeデータベースです。
   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルが存在するスキーマ名です。
   - **Pipe**：`emqxstreaming`を入力。SQL文で作成したSnowflakeストリーミングパイプの名前で、Snowflakeで定義した名前と完全に一致する必要があります。
   - **HTTP Pipelining**：レスポンスを待たずに送信可能な最大HTTPリクエスト数。デフォルトは`100`。
   - **Connect Timeout**：Snowflakeへの接続確立タイムアウト秒数。デフォルトは`15`秒。
   - **Connection Pool Size**：このSinkがSnowflakeに対して維持可能な最大同時接続数。デフォルトは`8`。
   - **Max Inactive**：アイドル状態の接続が閉じられるまでの最大時間（秒）。デフォルトは`10`秒。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて高度な設定を行います（任意）。詳細は[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSinkがSnowflakeサーバーに接続できるかテスト可能です。

9. **Create**ボタンをクリックしてSink作成を完了します。作成成功後、ページはルール作成画面に戻り、新規Sinkがルールアクションに追加されます。
=======
   Snowflake統合では、選択するフィールドはSnowflakeのテーブルのカラム数および名前と完全に一致させることが重要です。余分なフィールドを追加したり、`*`で選択することは避けてください。

   :::

4. ルールにアクションとしてSinkを追加します。
   - 集約アップロードモードでSnowflakeに書き込む場合は、[集約アップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-aggregated-upload-mode)を参照。
   - ストリーミングアップロードモードでSnowflakeに書き込む場合は、[ストリーミングアップロードモードでSnowflake Sinkを追加](#add-snowflake-sink-with-streaming-upload-mode)を参照。

5. アクション追加後、**Action Outputs**セクションに新しいSinkが表示されます。**Create Rule**ページで**Save**をクリックし、ルール作成を完了します。

これでルールの作成が完了し、**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブで新しいSnowflake Sinkも確認可能です。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で処理されSnowflakeに書き込まれる流れを視覚的に確認できます。

### 集約アップロードモードでSnowflake Sinkを追加

このセクションでは、集約アップロードモードを使って処理結果をSnowflakeに書き込むSinkをルールに追加する方法を示します。このモードは複数のルールトリガー結果を1つのファイル（例：CSVファイル）にまとめてアップロードし、ファイル数を減らして書き込み効率を向上させます。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sink名（例：`snowflake_sink`）と簡単な説明を入力。

4. **Connectors**ドロップダウンから先に作成した`my-snowflake`コネクターを選択。隣の作成ボタンで新規コネクターをポップアップで素早く作成可能。必要な設定は[集約モード用Snowflakeコネクターの作成](#create-a-snowflake-connector-for-aggregated-mode)を参照。

5. 集約アップロードモードの設定を行う。

   - **Database Name**：`testdatabase`を入力。EMQXデータ保存用に作成したSnowflakeデータベース。
   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルがあるスキーマ。
   - **Stage**：`emqx`を入力。Snowflakeで作成した、ロード前のファイルを保持するステージ。
   - **Pipe**：`emqx`を入力。ステージからテーブルへ自動ロードするパイプ。
   - **Pipe User**：`snowpipeuser`を入力。パイプ管理権限を持つSnowflakeユーザー。
   - **Private Key**：パイプユーザーがSnowflakeパイプに安全にアクセスするためのRSA秘密鍵。以下いずれかの形式で指定可能：
     - **プレーンテキスト**：PEM形式の秘密鍵全文を文字列として貼り付け。
     - **ファイルパス**：`file://`で始まる秘密鍵ファイルのパス。クラスター全ノードで同一パスかつEMQXアプリケーションユーザーがアクセス可能である必要あり。例：`file:///etc/emqx/certs/snowflake_rsa_key.private.pem`。
   - **Private Key Password**：秘密鍵ファイルが暗号化されている場合の復号パスワード。OpenSSLの`-nocrypt`オプションで生成した鍵は空欄。

   - **Aggregation Upload Format**：現在は`csv`のみ対応。Snowflakeにカンマ区切りCSV形式でステージング。
   - **Column Order**：ドロップダウンからカラムの並び順を選択。生成されるCSVは選択したカラム順にソートされ、未選択カラムはアルファベット順に並びます。
   - **Max Records**：集約をトリガーする最大レコード数。例：`1000`に設定すると1000件収集後にアップロード。最大レコード数到達時に1ファイルの集約が完了しアップロードされ、時間間隔がリセットされます。
   - **Time Interval**：集約を行う時間間隔（秒）。例：`60`に設定すると最大レコード数に達していなくても60秒ごとにアップロードされ、最大レコード数がリセットされます。
   - **Proxy**：HTTPプロキシ経由でSnowflakeに接続する設定。HTTPSプロキシは非対応。デフォルトはプロキシなし。プロキシを使う場合は`Enable Proxy`を選択し、以下を入力：
     - **Proxy Host**：プロキシサーバーのホスト名またはIPアドレス。
     - **Proxy Port**：プロキシサーバーのポート番号。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSinkがSnowflakeに接続可能かテスト可能。

9. **Create**ボタンをクリックしてSink作成を完了。作成成功後はルール作成画面に戻り、新しいSinkがルールアクションに追加されます。

### ストリーミングアップロードモードでSnowflake Sinkを追加

このセクションでは、ストリーミングアップロードモードを使って処理結果をSnowflakeに書き込むSinkをルールに追加する方法を示します。このモードはSnowpipe Streaming APIを利用したリアルタイム取り込みを可能にします。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールにアクションを追加。

2. **Action Type**ドロップダウンから`Snowflake Streaming`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のSnowflakeアクションを選択。ここでは新規Sinkを作成してルールに追加。

3. Sink名（例：`snowflake_sink_streaming`）と簡単な説明を入力。

4. コネクターのドロップダウンから先に作成した`my-snowflake-streaming`を選択。隣の作成ボタンで新規コネクターをポップアップで素早く作成可能。必要な設定は[Snowflakeストリーミングコネクターの作成](#create-a-snowflake-streaming-connector)を参照。

5. ストリーミングアップロードモードの設定を行う。

   - **Database Name**：`testdatabase`を入力。EMQXデータ保存用に作成したSnowflakeデータベース。
   - **Schema**：`public`を入力。`testdatabase`内のデータテーブルがあるスキーマ。
   - **Pipe**：`emqxstreaming`を入力。SQLで作成したSnowflakeストリーミングパイプ名。Snowflakeで定義した名前と完全一致させる必要あり。
   - **HTTP Pipelining**：レスポンスを待たずに送信可能な最大HTTPリクエスト数。デフォルト：`100`。
   - **Connect Timeout**：Snowflakeへの接続確立のタイムアウト秒数。デフォルト：`15`秒。
   - **Connection Pool Size**：EMQXがこのSink用にSnowflakeと維持可能な同時接続数の最大値。デフォルト：`8`。
   - **Max Inactive**：アイドル状態の接続を閉じるまでの最大待機時間（秒）。デフォルト：`10`秒。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

7. **詳細設定**を展開し、必要に応じて高度な設定を行う（任意）。詳細は[詳細設定](#advanced-settings)を参照。

8. **Create**をクリックする前に、**Test Connectivity**でSinkがSnowflakeに接続可能かテスト可能。

9. **Create**ボタンをクリックしてSink作成を完了。作成成功後はルール作成画面に戻り、新しいSinkがルールアクションに追加されます。
>>>>>>> origin/release-6.1

## ルールのテスト

このセクションでは、設定したルールのテスト方法を示します。

### テストメッセージのパブリッシュ

<<<<<<< HEAD
MQTTXを使ってトピック`t/1`にメッセージをパブリッシュします：
=======
MQTTクライアントMQTTXを使ってトピック`t/1`にメッセージをパブリッシュします：
>>>>>>> origin/release-6.1

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Snowflake" }'
```

<<<<<<< HEAD
複数回繰り返してテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeに正常にデータが書き込まれたかをSnowflakeインスタンスにアクセスしてターゲットテーブルをクエリし確認します。

1. SnowflakeのWebインターフェースを開き、認証情報でSnowflakeコンソールにログインします。

2. Snowflakeコンソールで以下のSQLを実行し、ルールで書き込まれた`emqx`テーブルのデータを表示します：
=======
複数回繰り返して複数のテストメッセージを生成してください。

### Snowflake内のデータ確認

テストメッセージ送信後、Snowflakeに正常にデータが書き込まれたかを確認します。

1. SnowflakeのWebインターフェースにログイン。

2. Snowflakeコンソールで以下のSQLクエリを実行し、ルールで書き込まれた`emqx`テーブルのデータを確認：
>>>>>>> origin/release-6.1

   ```
   SELECT * FROM testdatabase.public.emqx;
   ```

   これにより、`clientid`、`topic`、`payload`、`publish_received_at`などのフィールドを含む全レコードが表示されます。

3. 送信したテストメッセージ（例：`{ "msg": "Hello Snowflake" }`）やトピック、タイムスタンプなどのメタデータが確認できるはずです。

## 高度な設定

<<<<<<< HEAD
このセクションでは、Snowflake Sinkの高度な設定オプションについて説明します。ダッシュボードでSinkを設定する際、**Advanced Settings**を展開して以下のパラメーターをニーズに応じて調整できます。

| 項目名                       | 説明                                                         | デフォルト値   |
| ---------------------------- | ------------------------------------------------------------ | ------------- |
| **Buffer Pool Size**          | EMQXとSnowflake間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化しスムーズなデータ伝送を保証します。 | `16`          |
| **Request TTL**               | バッファに入ったリクエストが有効とみなされる最大秒数です。リクエストがこのTTLを超えてバッファに滞留するか、送信後にSnowflakeから適時の応答やアックを受け取れなかった場合、リクエストは期限切れとみなされます。 | `45`          |
| **Health Check Interval**     | SinkがSnowflakeとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`          |
| **Health Check Interval Jitter** | ヘルスチェック間隔に加える一様ランダム遅延（ミリ秒）です。複数のノードが同時にヘルスチェックを開始する確率を減らします。複数のアクションやソースが同一コネクターを共有する場合に有効です。 | `0`           |
| **Health Check Timeout**      | SinkがSnowflakeとの接続状態をヘルスチェックする際のタイムアウト秒数を指定します。 | `60`          |
| **Max Buffer Queue Size**     | Snowflake Sinkの各バッファワーカーがバッファリング可能な最大バイト数です。バッファワーカーはデータを一時的に保持し、Snowflakeへの送信を効率化します。システム性能やデータ伝送要件に応じて調整可能です。 | `256` MB      |
| **Query Mode**                | 同期（`synchronous`）または非同期（`asynchronous`）のリクエストモードを選択し、メッセージ伝送を最適化します。非同期モードではSnowflakeへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受信する可能性があります。 | `Asynchronous` |
| **Batch Size**                | EMQXからSnowflakeへ一度に転送するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送の効率と性能を微調整可能です。<br />`Batch Size`を`1`に設定すると、データはバッチ化されず個別に送信されます。 | `100`         |
| **Inflight Window**           | 送信済みだが応答やアックをまだ受け取っていない「インフライト」キューリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合、このパラメーターが重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`         |
=======
このセクションでは、Snowflake Sinkの詳細な設定オプションについて説明します。ダッシュボードでSink設定時に**Advanced Settings**を展開し、用途に応じて以下のパラメーターを調整可能です。

| 項目名                         | 説明                                                         | デフォルト値     |
| ------------------------------ | ------------------------------------------------------------ | --------------- |
| **Buffer Pool Size**            | EMQXとSnowflake間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保持・処理し、パフォーマンス最適化とスムーズなデータ送信を支えます。 | `16`            |
| **Request TTL**                 | バッファに入ってからリクエストが有効とみなされる最大秒数です。TTLを超えたリクエストや、送信済みでSnowflakeからの応答やアックがタイムリーに得られない場合は期限切れと判断されます。 | `45` 秒         |
| **Health Check Interval**       | Snowflakeとの接続状態を自動でチェックする間隔（秒）を指定します。 | `15` 秒         |
| **Health Check Interval Jitter**| 複数ノードが同時にヘルスチェックを開始するのを避けるために、基本間隔に加算される一様ランダム遅延です。複数のアクションやソースが同じコネクターを共有する場合に有効です。 | `0` ミリ秒      |
| **Health Check Timeout**        | Snowflake接続の自動ヘルスチェックのタイムアウト時間（秒）を指定します。 | `60` 秒         |
| **Max Buffer Queue Size**       | Snowflake Sinkの各バッファワーカーが一時的に保持可能な最大バイト数を指定します。バッファワーカーはデータ送信前の中継役として機能し、システム性能やデータ送信要件に応じて調整してください。 | `256` MB        |
| **Query Mode**                  | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードではSnowflakeへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがSnowflake到達前にメッセージを受信する可能性があります。 | `Asynchronous`  |
| **Batch Size**                  | EMQXからSnowflakeへ一度に送信するデータバッチの最大サイズを指定します。サイズ調整により転送効率やパフォーマンスを最適化可能です。<br />`1`に設定するとバッチングせず個別送信となります。 | `100`           |
| **Inflight Window**             | 送信済みで応答やアックをまだ受け取っていない「インフライト」リクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合に重要で、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`           |
>>>>>>> origin/release-6.1
