# MQTTデータを達夢データベースへ書き込む

[達夢データベース（DM8）](https://www.dameng.com/) は、政府・金融・電力・通信などの業界で広く使われている大型の汎用リレーショナルデータベースです。EMQXは達夢データベースとの統合により、MQTTメッセージやクライアントイベントを達夢データベースに保存し、データパイプラインの構築や分析、デバイス接続管理、システム統合に利用できます。

本ページでは、EMQXと達夢データベースのデータ統合について説明し、ルールとSink（アクション）の作成ガイドを提供します。

::: tip

達夢SinkはEMQX 6.x以降でサポートされています。統合にはODBCドライバが必要です。EMQXを実行するマシンにunixODBCと達夢ODBCドライバを設定してください（下記参照）。

:::

## 動作の仕組み

達夢データ統合は、EMQXのデバイス接続・メッセージ転送機能と達夢データベースのデータ保存機能を組み合わせた機能です。内蔵の[ルールエンジン](./rules.md)とSinkにより、MQTTメッセージやクライアントイベントを達夢データベースに保存したり、イベントに応じて更新・削除処理を行えます。

MQTTデータを達夢データベースへ取り込む流れ：

1. **メッセージの公開と受信**：産業IoTデバイスがMQTTでEMQXに接続し、リアルタイムデータを公開します。EMQXがメッセージを受信すると、ルールエンジンがマッチング処理を開始します。
2. **メッセージ処理**：メッセージはルールで処理され、達夢データベースにルーティングするメッセージを決定します。ペイロード変換の指定があれば適用されます。
3. **達夢データベースへの書き込み**：ルールが達夢データベースへ書き込むアクションをトリガーします。SQLテンプレートを使ってルール結果からデータを抽出し、ODBC経由で達夢データベースに送信し、対応するテーブルとカラムに書き込みます。
4. **保存と活用**：データは達夢データベースに保存され、企業のさまざまなユースケースに利用できます。

## 特徴と利点

- **リアルタイムデータストリーム**：EMQXはリアルタイムストリーム向けに構築されており、ソースシステムから達夢データベースへの効率的で信頼性の高い転送を実現します。
- **高性能とスケーラビリティ**：EMQXと達夢データベースはどちらも拡張性と信頼性を備え、大規模なIoTデータに対応します。
- **柔軟なデータ変換**：EMQXのSQLベースのルールエンジンは、達夢データベースへの保存前処理を可能にします。
- **バッチ書き込み**：`odbc:param_query`によるバッチのパラメータ化書き込みをサポートし、ネットワーク往復とデータベースのオーバーヘッドを大幅に削減します。

## 事前準備

達夢データ統合を作成する前に必要な準備（ODBCドライバのインストールと設定、達夢サーバのセットアップ、データベース・テーブルの作成）について説明します。

### 前提条件

- [ルール](./rules.md)を理解していること。
- [データ統合](./data-bridges.md)を理解していること。

### ODBCドライバのインストールと設定

達夢データベースにアクセスするには、EMQXを実行するマシンにunixODBCと達夢ODBCドライバをインストール・設定する必要があります。

::: tip 重要

EMQXはErlang/OTPの`odbc`アプリケーションを使用して達夢に接続します。`odbc`の`odbcserver`ポートプログラムは、**`/etc/`**（`/usr/local/etc/`ではない）からODBC設定を読み取ります。したがって、**`odbcinst.ini`と`odbc.ini`は`/etc/`に配置**するか`/etc/`にシンボリックリンクする必要があります。そうしないと`DSN=`/ドライバ名の解決に失敗します。

:::

1. unixODBCと達夢ODBCドライバをインストールします（達夢のインストール先は`/opt/dmdbms`、ドライバは`/opt/dmdbms/bin/libdodbc.so`）。
2. `/etc/odbcinst.ini`を編集して達夢ドライバを追加します：
   ```
   [DM8 ODBC DRIVER]
   Description = ODBC DRIVER FOR DM8
   Driver = /opt/dmdbms/bin/libdodbc.so
   ```
3. `/etc/odbc.ini`を編集してデータソース（DSN）を設定します：
   ```
   [dm8]
   Description = DM ODBC DSN
   Driver = DM8 ODBC DRIVER
   SERVER = 192.168.1.10
   UID = SYSDBA
   PWD = あなたのパスワード
   TCP_PORT = 5237
   ```
   ::: tip
   DSNを使わず、コネクタで`driver`（ドライバ名または`.so`の絶対パス）と`server/port/username/password`を直接指定することもできます（フィールド接続文字列形式）。
   :::
4. 接続を確認します：`odbcinst -j`でunixODBCとドライバを確認し、`isql dm8 SYSDBA あなたのパスワード`で`select 1`を実行します。

## コネクタの作成

EMQXで以下のコマンドを実行して達夢コネクタを作成します：

```bash
curl -XPOST http://localhost:18083/api/v5/connectors \
  -d '{
    "type": "dameng",
    "name": "dameng",
    "enable": true,
    "server": "192.168.1.10",
    "port": 5237,
    "username": "SYSDBA",
    "password": "あなたのパスワード",
    "driver": "DM8 ODBC DRIVER",
    "database": "DM8",
    "charset": "utf8",
    "pool_size": 8,
    "resource_opts": {"health_check_interval": "20s"}
  }'
```

主な接続パラメータ：

| パラメータ | 説明 |
| --- | --- |
| `server` | 達夢ホスト（`host`または`host:port`）。 |
| `port` | 達夢ポート、デフォルト`5236`。 |
| `username` / `password` | ログインユーザー（デフォルト`SYSDBA`）とパスワード。 |
| `driver` | ODBCドライバ名（例: `DM8 ODBC DRIVER`、`/etc/odbcinst.ini`の設定が必要）またはドライバの`.so`絶対パス（例: `/opt/dmdbms/bin/libdodbc.so`）。 |
| `dsn` | 任意。`/etc/odbc.ini`にDSNがある場合は`DSN=<名前>`での直接接続が可能。 |
| `database` | データベース名（ODBC接続文字列の`Database=`に渡される、例: `DM8`）。 |
| `charset` | 文字セット（`Charset=`に渡される、例: `utf8`）。 |
| `pool_size` | 接続プールサイズ、デフォルト`8`。 |

## アクション（ルールSink）の作成

EMQXで以下のコマンドを実行して達夢アクションを作成します：

```bash
curl -XPOST http://localhost:18083/api/v5/actions \
  -d '{
    "type": "dameng",
    "name": "dameng",
    "parameters": {
      "connector": "dameng",
      "sql": "insert into SYSDBA.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )",
      "undefined_vars_as_null": false
    },
    "resource_opts": {"batch_size": 100, "batch_time": "100ms", "query_mode": "sync"}
  }'
```

アクションパラメータ：

| パラメータ | 説明 |
| --- | --- |
| `sql` | 挿入SQLテンプレート（プレースホルダ`${...}`はルール結果から取得）。**INSERTはカラムを明示的に列挙する必要があります**。コネクタはアクション作成時に`describe_table`でテーブルのカラム型を調べます。 |
| `undefined_vars_as_null` | `true`の場合、未マッチの変数は`null`として書き込まれます。`false`（デフォルト）の場合、未マッチはエラーとして扱われます。 |
| `resource_opts.batch_size` | バッチ行数、デフォルト`100`。 |
| `resource_opts.batch_time` | バッチ集約時間、デフォルト`100ms`。 |

### SQLテンプレート例

```sql
insert into SYSDBA.t_mqtt_msg(msgid, topic, qos, payload)
values ( ${id}, ${topic}, ${qos}, ${payload} )
```

- `${id}`、`${topic}`、`${qos}`、`${payload}`はルールの出力フィールドから取得されます。
- バッチ書き込みはパラメータ化クエリ（`odbc:param_query`）を使用するため、文字列エスケープは不要です。

## ルールの作成

EMQXでルールを作成し、`dameng`アクションを選択すると、一致したメッセージが達夢データベースに書き込まれます。

```bash
curl -XPOST http://localhost:18083/api/v5/rules \
  -d '{
    "name": "write to dameng",
    "sql": "SELECT * FROM \"t/#\"",
    "actions": [{"function": "dameng:dameng"}]
  }'
```

## 例

一致したトピックへメッセージを公開すると、達夢データベースにデータが書き込まれます：

```bash
mosquitto_pub -t 't/1' -m 'hello dameng'
```

達夢テーブルを検索：

```sql
SELECT * FROM SYSDBA.t_mqtt_msg;
```

## 注意事項

- 達夢の接続文字列は`DSN=`形式とフィールド形式の両方をサポートします。フィールド形式では`Driver`にドライバ名または`.so`絶対パスを指定できます。
- DSN/ドライバ名を使用する場合は、`/etc/odbcinst.ini`と`/etc/odbc.ini`が正しく設定されていることを確認してください（Erlangの`odbcserver`は`/etc/`を読み取ります）。
- バッチ書き込みは`odbc:param_query`に依存し、すべての行のカラム数が一致している必要があります。
- ルール出力フィールドが対象カラムと一致しない場合は、`undefined_vars_as_null: true`を設定して欠落フィールドを`null`で埋められます。
