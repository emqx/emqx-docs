# 設定ファイル

<<<<<<< HEAD
ユーザーは設定ファイルまたは環境変数を使ってEMQXを設定できます。本節では主にEMQXの設定ファイルを紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目と解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定および実行時データを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、実行時に生成または動的に更新される設定ファイルを格納します。
=======
ユーザーは設定ファイルまたは環境変数を使って EMQX を設定できます。本章では主に EMQX の設定ファイルについて紹介し、EMQX で最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目と説明については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) を参照してください。

## 設定ディレクトリ

EMQX をインストールすると、設定およびランタイムデータを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ (`etc`)**：読み取り専用で、変更されない静的な設定ファイルを格納します。
- **動的設定ディレクトリ (`data/configs`)**：書き込み可能で、ランタイムに生成または動的に更新される設定ファイルを格納します。
>>>>>>> origin/release-5.10

### 静的設定ディレクトリ（`etc`）

<<<<<<< HEAD
`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイ時やアップグレード時に変更され、実行時は安定性を保つために読み取り専用となります。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                           | パス               |
| ------------------------------------------ | ------------------ |
| RPMまたはDEBパッケージでインストール       | `/etc/emqx`        |
| Dockerコンテナで実行                       | `/opt/emqx/etc`    |
| ポータブル圧縮パッケージから展開           | `./etc`            |
=======
`etc` ディレクトリには EMQX の初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイやアップグレード時に編集され、ランタイム中は安定性を保つために読み取り専用となっています。`etc` ディレクトリの場所はインストール方法によって異なります。

| インストール方法                          | パス               |
| ---------------------------------------- | ------------------ |
| RPM または DEB パッケージでインストール | `/etc/emqx`        |
| Docker コンテナで実行                    | `/opt/emqx/etc`    |
| ポータブル圧縮パッケージから展開        | `./etc`            |
>>>>>>> origin/release-5.10

### 動的設定ディレクトリ（`data/configs`）

<<<<<<< HEAD
実行時には、ダッシュボード、REST API、CLIを通じて動的に設定を変更できます。これらの変更は`data/configs`ディレクトリに保存され、セッションをまたいで永続化されます。このディレクトリの場所もインストール方法により異なります。

| インストール方法                           | パス                     |
| ------------------------------------------ | ------------------------ |
| RPMまたはDEBパッケージでインストール       | `/var/lib/emqx/configs`  |
| Dockerコンテナで実行                       | `/opt/emqx/data/configs` |
| ポータブル圧縮パッケージから展開           | `./data/configs`         |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することで、データディレクトリのパスを変更可能です。ただし、クラスター運用時は全ノードで同じディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、重複した場合は事前定義された上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションに詳細なリファレンスがありますが、設定例はEMQXの設定理解や適用に役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合、`etc/emqx/examples`ディレクトリに設定例があります。
- DockerコンテナでEMQXを実行している場合、`opt/emqx/etc/examples`ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれており、実行時により上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、その後ダッシュボードUIからより複雑な設定で上書きすることができます。

`node`や`cluster`などの不変設定は`base.hocon`に設定することは**推奨されません**。詳細は[Immutable Configurations File](#immutable-configuration-file)をご参照ください。
=======
ランタイム中、EMQX はダッシュボード、REST API、CLI を通じて動的な再設定を許可しています。これらのツールで行われた変更は `data/configs` ディレクトリに保存され、セッションをまたいで永続化されます。このディレクトリの場所もインストール方法に依存します。

| インストール方法                          | パス                    |
| ---------------------------------------- | ----------------------- |
| RPM または DEB パッケージでインストール | `/var/lib/emqx/configs` |
| Docker コンテナで実行                    | `/opt/emqx/data/configs`|
| ポータブル圧縮パッケージから展開        | `./data/configs`        |

::: tip
`node.data_dir` 設定や環境変数 `EMQX_NODE__DATA_DIR` を変更することで、データディレクトリの場所を変更可能です。ただし、クラスター運用時は全ノードで同一のディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、もし重複があった場合は事前定義された上書きルールに従って解決されます。詳細は [Config Override Rules](#config-override-rules) を参照してください。

## 設定例

[Schema](#schema) セクションで詳細なリファレンスを提供していますが、設定例は EMQX の設定を理解し適用する際に役立ちます。

- RPM または DEB パッケージでインストールした場合、設定例は `etc/emqx/examples` ディレクトリにあります。
- Docker コンテナで実行している場合は、`opt/emqx/etc/examples` ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4 以降、`etc` ディレクトリに `base.hocon` というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれ、ランタイムで上位の設定ファイルによって上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、その後ダッシュボード UI からより複雑な設定に上書きすることができます。

ただし、`node` や `cluster` のような不変設定は `base.hocon` に設定することは**推奨されません**。詳細は [Immutable Configurations File](#immutable-configuration-file) を参照してください。
>>>>>>> origin/release-5.10

::: tip
`base.hocon` ファイルはクラスター間で同期されず、配置されているノードにのみ適用されます。
:::

## 設定上書きファイル

<<<<<<< HEAD
`data/configs`ディレクトリの`cluster.hocon`ファイルにはクラスター全体の設定項目が含まれます。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、ノードは自動的に他のノードから`cluster.hocon`をコピーして適用します。そのため、このファイルを手動で編集することは推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層の詳細は[Config Override Rules](#config-override-rules)をご覧ください。

EMQX 5.1以降、クラスター設定の変更があると`cluster.hocon`ファイルは上書き前にバックアップされます。バックアップファイルはノードのローカル時間でタイムスタンプが付けられ、最大10ファイルまで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは依然として`node`や`cluster`設定など重要なシステム設定の主要な設定ファイルとして残っています。このファイルは`base.hocon`や`cluster.hocon`より優先度が高いですが、環境変数よりは低い優先度です。

設定の上書きに関する詳細は[Config Override Rules](#config-override-rules)をご参照ください。

## 設定パス

EMQXでは設定値をドット区切りのパスで参照できます。これはツリー構造に似ており、ルート（常にStruct）から始まり、各セグメントはフィールド名またはMapのキーを指します。配列要素の場合は1始まりのインデックスを使用します。
=======
`data/configs` ディレクトリ内の `cluster.hocon` ファイルはクラスター全体の設定項目を含みます。ダッシュボード、REST API、CLI からの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、ノードは自動的に他のノードから `cluster.hocon` をコピーして適用します。このため、手動での編集は推奨されません。

このファイルの設定は `base.hocon` の設定の上に適用されます。設定の上書き階層の詳細は [Config Override Rules](#config-override-rules) を参照してください。

EMQX 5.1 以降、クラスター設定の変更があると、上書き前に `cluster.hocon` のバックアップが作成されます。バックアップファイルはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf` ファイルは `node` や `cluster` 設定を含む重要なシステム設定の主要な設定ファイルとして残っています。このファイルは `base.hocon` や `cluster.hocon` より優先度が高いですが、環境変数よりは低い優先度です。

設定の上書きに関する詳細は [Config Override Rules](#config-override-rules) を参照してください。

## 設定パス

EMQX では設定値をドット区切りのパスで参照できます。ルート（常に Struct）から始まり、各セグメントはフィールド名または Map のキーを示します。配列要素は1始まりのインデックスで指定します。
>>>>>>> origin/release-5.10

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON 設定フォーマット

<<<<<<< HEAD
EMQX v5.0以降、設定ファイルフォーマットとして[Human-Optimized Config Object Notation（HOCON）](https://github.com/emqx/hocon)を採用しています。

HOCONは人間が読みやすいデータ形式であり、JSONのスーパーセットです。継承、結合、引用符などの機能により、設定作業をさらに簡素化します。

**HOCON構文例：**

JSONに似たオブジェクトとして表現可能です。
=======
EMQX v5.0 から、設定ファイルフォーマットとして [Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon) を採用しています。

HOCON は人間に読みやすいデータフォーマットであり、JSON のスーパーセットです。継承や結合、引用符などの機能により、設定作業をより簡単にします。

**HOCON の構文例：**

JSON に似たオブジェクト形式で表現可能です。
>>>>>>> origin/release-5.10

```bash
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

<<<<<<< HEAD
またはフラット形式で：
=======
またはフラット形式でも記述可能です。
>>>>>>> origin/release-5.10

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

<<<<<<< HEAD
このフラット形式は以前のEMQXバージョンとの互換性を保ちつつ、使い方が異なります。

HOCONでは文字列の両端に引用符を付けることを推奨しています。特殊文字を含まない文字列は引用符なしでも構いません（例：`foo`、`foo_bar`）。一方、フラット形式は`=`の右側のすべてを値として扱います。
=======
このフラット形式は過去の EMQX バージョンとの互換性がありますが、使い方に違いがあります。

HOCON では文字列は両端に引用符を付けることを推奨しています。特殊文字を含まない文字列は引用符なしでも構いません（例：`foo`、`foo_bar`）。一方、フラット形式は `=` の右側のすべてを値として扱います。
>>>>>>> origin/release-5.10

HOCON の詳細な構文については、[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md) を参照してください。

## 環境変数

<<<<<<< HEAD
設定ファイルのほかに、環境変数を使ってEMQXを設定することも可能です。

例えば、環境変数`EMQX_NODE__NAME=emqx2@127.0.0.1`は以下の設定を上書きします。
=======
設定ファイルのほかに、環境変数を使って EMQX を設定することも可能です。

例えば、環境変数 `EMQX_NODE__NAME=emqx2@127.0.0.1` は以下の設定を上書きします。
>>>>>>> origin/release-5.10

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数の変換ルールは以下の通りです。

<<<<<<< HEAD
1. 設定ファイルの`.`区切りは環境変数に使えないため、EMQXは区切りに`__`（ダブルアンダースコア）を使用します。
2. 他の環境変数と区別するため、環境変数には`EMQX_`プレフィックスが付けられます。
3. 環境変数の値はHOCONの値として解析されるため、複雑なデータ型の値も渡せます。ただし、`:`や`=`などの特殊文字はダブルクォート`"`で囲む必要があります。
=======
1. 設定ファイルの区切り文字 `.` は環境変数に使えないため、EMQX では代わりにダブルアンダースコア `__` を区切り文字として使用します。
2. 他の環境変数と区別するため、環境変数名の先頭に `EMQX_` を付加します。
3. 環境変数の値は HOCON 値として解析されるため、複雑なデータ型を渡すことが可能です。`:`、`=`、`#` などの HOCON 特殊文字を含む値は、ダブルクォート `"` で囲む必要があります。特に `#` は HOCON の行コメントの開始文字なので、引用符なしでは `#` 以降がコメントとして無視されます。
>>>>>>> origin/release-5.10

変換例：

```bash
# 環境変数

## localhost:1883 は構造体 {"localhost": 1883} として解析されるため、ダブルクォートで囲む必要があります
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

<<<<<<< HEAD
## HOCON配列を文字列として直接渡す
=======
## HOCON 配列を文字列で直接渡す
>>>>>>> origin/release-5.10
export EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CIPHERS='["TLS_AES_256_GCM_SHA384"]'


# 設定ファイル
listeners.ssl.default {
    ...
    bind = "127.0.0.1:8883"
    ssl_options {
      ciphers = ["TLS_AES_256_GCM_SHA384"]
    }
  }
}
```

::: warning `#`、`:`、`=` を含む値について

パスワードなどに `#` を含む場合、`#` は HOCON の行コメント開始文字なので、以下のようにすると `#` 以降がコメントとして無視されます。

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

この場合、パスワードは `MQtt` として解析され、`#123` は破棄されます。リテラルとして渡すには、**HOCONレベルの**ダブルクォートで囲み、パーサーに `"MQtt#123"` と認識させる必要があります。

```bash
# 正しい例 — HOCON パーサーに渡される値は "MQtt#123"
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# 同じ効果、シェル用に内側の引用符をエスケープ
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

`:` や `=` を含む値も同様です。URLエンコード（例：`%23`）は無効で、EMQX は環境変数値の URL デコードを行いません。

:::

::: tip なぜ一部の引用なし値は通り、一部は通らないのか

EMQX は内部で環境変数値を `fake_key=<value>` として HOCON パースを試みます。成功すればパース結果を使用し、失敗すれば生の文字列を使います。例えば、`EMQX_..._PASSWORD="abc#def"` は有効な HOCON として `#def` がコメント扱いされ `abc` になりますが、`EMQX_..._PASSWORD=".abc#def"` は無効な HOCON なので生の文字列 `.abc#def` が使われます。HOCON の引用符で囲むことで挙動を明確にできます。

:::

::: tip

<<<<<<< HEAD
EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。`UNKNOWN_ROOT`は事前定義されたルートパスではないためです。

既知のルートパスに未知のフィールド名が設定された場合、起動時に`warning`ログが出力されます。例えば、`enable`を誤って`enabled`と設定した場合、以下のように出力されます。
=======
EMQX は未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。既知のルートパスに対して未知のフィールド名が設定された場合、起動時に `warning` ログを出力します。例えば、`enable` を誤って `enabled` と設定すると以下のように警告が出ます。
>>>>>>> origin/release-5.10

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 設定上書きルール

EMQX では設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が前の値を上書きします。
<<<<<<< HEAD
- 上位の設定ファイルは下位の設定を置き換えます。
=======
- 上位レベルの設定が下位レベルの設定を置き換えます。
>>>>>>> origin/release-5.10

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

<<<<<<< HEAD
つまり、`base.hocon`の設定は最も優先度が低く、より優先度の高いファイルの設定で上書きされます。`EMQX_`で始まる環境変数が最も優先度が高いです。

::: tip
バージョン5.8.4以前は`base.hocon`ファイルが存在しませんでした。優先順位は同じですが、`base.hocon`は含まれません。
:::

EMQXダッシュボードUI、HTTP API、CLIからの変更は実行時に`cluster.hocon`に永続化され、即時反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、`emqx.conf`と`cluster.hocon`間で設定の重複は**避けてください**。

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`ファイルが存在し、設定の優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. 5.0.2/v5.0.22以前から最新バージョンにアップグレードする場合、優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`機構はバージョン5.1で削除されました。
=======
つまり、`base.hocon` の設定は最も優先度が低く、上位のファイルで上書きされます。`EMQX_` で始まる環境変数は最も優先度が高いです。

::: tip
バージョン 5.8.4 以前は `base.hocon` ファイルが存在しませんでした。優先順位は同じですが、`base.hocon` は含まれません。
:::

EMQX ダッシュボード UI、HTTP API、CLI で行われた変更はランタイム中に `cluster.hocon` に永続化され、即時反映されます。ただし、`emqx.conf` や環境変数で同じ設定項目が異なる値で設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、`emqx.conf` と `cluster.hocon` の設定を重複させることは**避けてください**。

::: tip
1. 古い EMQX バージョン（例：5.0.2/v5.0.22 以前）では、`cluster-override.conf` ファイルが存在し、設定の優先順位は `emqx.conf < ENV < HTTP API (cluster-override.conf)` でした。
2. これらの古いバージョンから最新バージョンにアップグレードしても優先順位は変わらず、互換性維持のため `cluster.hocon` は作成されません。
3. `cluster-override.conf` 機構はバージョン 5.1 で廃止されました。
>>>>>>> origin/release-5.10
:::

### 上書き例

<<<<<<< HEAD
以下の設定では、最後の行で定義された`level`の`debug`が以前の`error`を上書きしますが、`enable`フィールドは変更されません。
=======
以下の設定では、最後の行で `level` の値が `error` から `debug` に上書きされますが、`enable` フィールドは変更されません。
>>>>>>> origin/release-5.10

```bash
log {
  console {
    enable = true
    level = error
  }
}

<<<<<<< HEAD
## コンソールログの出力レベルをdebugに設定し、他の設定は維持
log.console.level = debug
```

パケットサイズ制限は最初に1MBに設定され、その後10MBに上書きされました。
=======
## コンソールログのレベルを debug に設定し、他の設定は維持
log.console.level = debug
```

パケットサイズ制限は最初に 1MB に設定され、その後 10MB に上書きされています。
>>>>>>> origin/release-5.10

```bash
zones {
  zone1 {
    mqtt.max_packet_size = 1M
  }
}
zones.zone1.mqtt.max_packet_size = 10M
```

### 配列要素の上書き

EMQX の配列は以下の2つの表現方法があります。

- リスト形式（例：`[1, 2, 3]`）
<<<<<<< HEAD
- Map形式（サブスクライブ形式、例：`{"1"=1, "2"=2, "3"=3}`）
=======
- マップ形式（サブスクリプション用、例：`{"1"=1, "2"=2, "3"=3}`）
>>>>>>> origin/release-5.10

以下の3つの表現は同等です。

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

<<<<<<< HEAD
この特徴を利用して配列の要素を簡単に上書きできます。例：
=======
この特徴を利用して、配列内の特定要素の値を簡単に上書きできます。
>>>>>>> origin/release-5.10

```bash
authentication  = [
  {
    enable = true,
    backend = "built_in_database",
    mechanism = "password_based"
  }
]

<<<<<<< HEAD
# 1番目の要素の`enable`フィールドを以下のように上書き可能
=======
# 1番目の要素の `enable` フィールドを以下のように上書き可能
>>>>>>> origin/release-5.10
authentication.1.enable = false
```

::: tip

リスト形式の配列は完全に上書きされ、元の値は保持されません。例えば：

```bash
authentication = [
  {
    enable = true
    backend = "built_in_database"
    mechanism="password_based"
  }
]

<<<<<<< HEAD
## 以下の方法では、1番目の要素の`enable`以外のフィールドは失われます。
=======
## 以下の設定では、1番目の要素の `enable` 以外の全フィールドが失われます。
>>>>>>> origin/release-5.10
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

<<<<<<< HEAD
EMQXのゾーンは設定をグループ化する概念です。リスナーの`zone`フィールドにゾーン名を設定することで、ゾーンに関連付けられたリスナーに接続するMQTTクライアントは、そのゾーンの設定を継承し、グローバル設定を上書きできます。
=======
EMQX のゾーンは設定をグループ化する概念です。ゾーンはリスナーの `zone` フィールドに設定することで関連付けられます。ゾーンに関連付けられたリスナーに接続する MQTT クライアントは、そのゾーンの設定を継承し、グローバル設定を上書きする場合があります。
>>>>>>> origin/release-5.10

::: tip
デフォルトではリスナーは `default` という名前のゾーンに紐づいています。`default` ゾーンは論理的なグループであり、設定ファイルには存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

<<<<<<< HEAD
- `mqtt`：MQTT接続およびセッション設定。特定ゾーンでMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：MQTTセッションの永続化設定。特定ゾーンで永続ストレージを有効化など。

EMQXバージョン5のデフォルト設定ファイルにはゾーンは含まれていません。バージョン4では`internal`と`external`の2つのデフォルトゾーンがありました。
=======
- `mqtt`：MQTT 接続やセッション設定。特定ゾーンで MQTT メッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlang プロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検知。
- `durable_sessions`：セッション永続化設定。特定ゾーンで MQTT セッションの永続ストレージを有効化など。

EMQX バージョン5のデフォルト設定ファイルにはゾーンは含まれていません。バージョン4では `internal` と `external` の2つのデフォルトゾーンが存在していました。
>>>>>>> origin/release-5.10

ゾーンを作成するには、設定ファイルで以下のように定義します。

```bash
zones {
  # 複数のゾーンを定義可能
  my_zone1 {
    # ゾーンはグローバル設定と同じスキーマを共有
    mqtt {
      # このゾーンの接続でより大きなパケットサイズを許可
      max_packet_size = 10M
    }
    force_shutdown {
      # このゾーン固有の設定
      ...
    }
    durable_sessions {
      # このゾーンでセッション永続化を有効化
      enable = true
      ...
    }
  }
  my_zone2 {
    ...
  }
}
```

<<<<<<< HEAD
リスナーで`zone`フィールドを設定し、作成済みのゾーンに関連付けます。
=======
リスナーの `zone` フィールドに作成済みのゾーン名を設定して紐づけます。
>>>>>>> origin/release-5.10

```bash
listeners.tcp.default {
    bind = 1883
    zone = my_zone1
    ...
}
```

## スキーマ

<<<<<<< HEAD
HOCONオブジェクトの型安全性を確保するため、EMQXはスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などを可能にします。
=======
HOCON オブジェクトの型安全性を高めるため、EMQX はスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などに利用されます。
>>>>>>> origin/release-5.10

[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) はこのスキーマから生成されています。

::: tip
<<<<<<< HEAD
ゾーン設定スキーマは各グループで共通のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
=======
ゾーンの設定スキーマは各グループで共通のため、設定マニュアルには含まれていません。例えば `zones.my_zone1.mqtt {...}` は `mqtt {...}` と同じスキーマです。
>>>>>>> origin/release-5.10
:::

### プリミティブデータ型

<<<<<<< HEAD
設定マニュアルに登場するプリミティブ型は基本的に自明であり、最小限の説明で十分です。以下に主な型を列挙します。

#### Integer

整数値を表します。例：`42`、`-3`、`0`。

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf`は`1`から正の無限大までの整数を意味し、正の整数のみ許容します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義済みのシンボルのいずれかのみを取ります。例：`Enum(debug,info,warning,error)`はログレベルを定義。
=======
設定マニュアルに登場するプリミティブ型は概ね直感的で、詳細な説明は不要です。以下に代表的な型を示します。

#### Integer

整数。例：`42`、`-3`、`0`

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf` は 1 以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義済みのシンボルのいずれかのみ許容。例：`Enum(debug,info,warning,error)` はログレベルの指定。
>>>>>>> origin/release-5.10

#### String

文字列型で、用途に応じて複数の表現形式があります。

<<<<<<< HEAD
- **非引用文字列**：特殊文字を含まない単純な識別子や名前に適しています（詳細は下記参照）。
- **引用文字列**：特殊文字や空白を含む場合はダブルクォート`"`で囲み、必要に応じてバックスラッシュ`\`でエスケープします。例：`"line1\nline2"`。
- **三重引用文字列**：`"""`で囲み、`\`以外のエスケープ不要で複雑な内容を簡単に記述可能。三重引用符の隣接する引用符はエスケープが必要です。
- **インデント付き三重引用文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを許容し、複数行や整形済みテキストに適しています。

**非引用文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、および空白を含みません。
- `//`で始めない（コメントと誤認されるため）。
- `true`、`false`、`null`で始まらない（ブール値やnullと誤解されるため）。

**三重引用文字列のガイドライン：**

- 三重引用符の隣にクォート文字を含める場合はエスケープするか`~`区切りを使います。
- 複数行文字列はスペース（タブ不可）によるインデントをサポートし、最小のインデント幅が基準になります。
=======
- **無引用符**：特殊文字を含まない単純な識別子や名前に適します（詳細は後述）。
- **引用符付き文字列**：特殊文字や空白を含む場合はダブルクォート `"` で囲み、必要に応じてバックスラッシュ `\` でエスケープします。例：`"line1\nline2"`
- **三重引用符文字列**：`"""` で囲み、エスケープ不要（`\` は例外）。複雑な内容の記述に便利です。三重引用符に隣接するクォートはエスケープが必要です。
- **インデント付き三重引用符文字列**：`"""~` と `~"""` で囲み、EMQX 5.6 以降で導入。設定ファイル内でインデントを保持しつつ複数行や整形済みテキストを記述可能。

**無引用符文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、空白
- `//` で始めない（コメント開始と誤認されるため）
- `true`、`false`、`null` で始まらない（ブールや null と誤解されるため）

**三重引用符文字列のガイドライン：**

- 三重引用符に隣接するクォートはエスケープするか、`~` 区切りを使う。
- 複数行文字列はスペース（タブ不可）でインデント可能。インデントレベルは最小の先頭スペース数で決定。
>>>>>>> origin/release-5.10

例：

```
rule_xlu4 {
  sql = """~
    SELECT
      *
    FROM
      "t/#"
  ~"""
}
```

<<<<<<< HEAD
HOCONの文字列引用規則の詳細は[HOCON仕様](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings)をご参照ください。

EMQX独自のインデント付き三重引用文字列の詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)にあります。

#### String("constant")

定数文字列で、単一値の列挙型（`Enum`）のように振る舞います。特定の設定やモードなど静的な値の定義に使います。

#### Boolean

`true`または`false`のいずれか（大文字小文字は区別されます）。

#### Float

浮動小数点数。例：`3.14`、`-0.001`。

#### Duration

人間が読みやすい形式の時間の長さを表します。書式の例と説明があります。

#### Duration(s)

秒単位の精度を持つ`Duration`型です。詳細と例があります。

#### Secret

パスワードやトークンなど機密情報向けの型です。用途と重要性の説明があります。

### 複合データ型

EMQXのHOCON設定における複合型は、他の複合型やプリミティブ値を含む階層的なデータ構造を表現可能です。

#### Struct `Struct(name)`

波括弧`{}`で囲まれたフィールドを持つ構造体です。`name`はスキーマ内の構造体のフィールド名と型を示します。

#### Map `Map($name->Type)`

`Struct`に似ていますが、フィールド名が事前定義されていません。

`$name`はドット`.`を含まない任意の文字列キーを表し、エンティティや属性名を示します。`Type`はMap内のすべての値が同じ型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを取るユニオン型です。ある構造体のフィールドが複数の型のうちどれか一つを許容する場合に使います。例：`String(infinity)`または`Duration`のどちらか。

#### Array `Array(Type)`

指定された`Type`の要素からなる配列を定義します。
=======
HOCON の文字列引用規則の詳細は [HOCON specification](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings) を参照してください。

EMQX 独自のインデント付き三重引用符の詳細は [emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats) を参照してください。

#### String("constant")

定数文字列。単一値の列挙型（`Enum`）として機能します。特定の設定やモードの固定値に使います。

#### Boolean

`true` または `false`（大文字小文字を区別）

#### Float

浮動小数点数。例：`3.14`、`-0.001`

#### Duration

人間に読みやすい時間の長さを表現。フォーマット例と説明あり。

#### Duration(s)

秒単位の精度を持つ `Duration` 型。詳細と例あり。

#### Secret

パスワードやトークンなどの機密情報用型。用途と重要性の説明あり。

### 複合データ型

EMQX の HOCON 設定では、複合データ型を用いて複雑な構造体やプリミティブ値を階層的に表現できます。

#### Struct `Struct(name)`

波括弧 `{}` で囲まれた構造体。`name` はスキーマで定義されたフィールド名と型の参照です。

#### Map `Map($name->Type)`

Struct に似ていますが、フィールド名が事前定義されていないキー・バリューの集合です。

`$name` は文字列（ただしドット `.` を含まない）で、エンティティや属性名を表します。`Type` はすべての値が同じ型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを許容するユニオン型。構造体のフィールドが複数の型のどれかを取る場合に使います。例：`String(infinity)` または `Duration` のどちらか。

#### Array `Array(Type)`

指定型の要素からなる配列。
>>>>>>> origin/release-5.10

::: tip

Map のフィールド名が正の整数の場合、`Array` の別表現として解釈されます。例：

```bash
myarray.1 = 74
myarray.2 = 75
```

は `myarray = [74, 75]` と解釈され、配列要素の上書きに便利です。

:::

### Variform 式

<<<<<<< HEAD
Variformは文字列操作や実行時評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内に埋め込んで動的に文字列操作を行うための専門的なツールです。

::: tip
Variform式は特定の設定項目でのみ使用可能です。指定がない限り使用しないでください。
:::

::: tip NULL値：
Variform式では、値バインディング参照や部分式の評価が未定義値となる場合、空文字列（`''`）として扱われます。

JSONデコードされたフィールドが`null`の場合は未定義値（空文字列）として扱われ、文字列`"null"`とは異なります。
=======
Variform は文字列操作やランタイム評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQX の設定内で動的に文字列操作を行うために埋め込まれます。

::: tip
Variform 式は特定の設定項目でのみ使用可能です。指定がない限り使用しないでください。
:::

::: tip NULL 値について
Variform 式では値のバインディング参照や部分式の評価結果が未定義の場合、空文字列 (`''`) として扱われます。

JSON で `null` のフィールドは未定義値（空文字列）として扱われ、文字列 `"null"` とは異なります。
>>>>>>> origin/release-5.10
:::

#### 構文例

```js
function_call(clientid, another_function_call(username))
```

<<<<<<< HEAD
この式は`clientid`と`username`を組み合わせて新しい文字列値を生成します。

Variformは以下のリテラルをサポートします。

- ブール値：`true`または`false`
- 整数：例`42`
- 浮動小数点数：例`3.14`
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲まれたASCII文字列
- 配列：`[`と`]`で囲まれ、カンマ`,`で区切られた要素
=======
この式は `clientid` と `username` を組み合わせて新しい文字列値を生成します。

Variform は以下のリテラルをサポートします。

- ブール値：`true` または `false`
- 整数：例 `42`
- 浮動小数点数：例 `3.14`
- 文字列：シングルクォート `'` またはダブルクォート `"` で囲まれた ASCII 文字
- 配列：`[` と `]` で囲まれ、カンマ `,` 区切りの要素
>>>>>>> origin/release-5.10
- 変数：事前定義された値の参照（例：`clientid`）
- 関数：事前定義された関数（例：`concat([...])`）

Variform は以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
<<<<<<< HEAD
- 文字列リテラル内のエスケープシーケンス（特殊文字のアンエスケープは`unescape`関数を呼び出す必要があります）

以下はVariform式を埋め込んだ設定例です。
=======
- 文字列リテラル内のエスケープシーケンス（特殊文字のアンエスケープは `unescape` 関数を使用）

以下は Variform 式を埋め込んだ設定例です。
>>>>>>> origin/release-5.10

```js
mqtt {
    client_attrs_init = [
        {
<<<<<<< HEAD
            # client IDの最初の`-`までのプレフィックスを抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.groupに設定
=======
            # clientid の最初の `-` までのプレフィックスを抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.group に設定
>>>>>>> origin/release-5.10
            set_as_attr = group
        }
    ]
}
```

::: tip
<<<<<<< HEAD
式内で`unescape`関数を使う場合、HOCON設定で三重引用符`"""`文字列を使うと二重エスケープ不要で便利です。
=======
式内でアンエスケープ関数が必要な場合、HOCON 設定で三重引用符文字列 (`"""`) を使うと二重エスケープが不要です。
>>>>>>> origin/release-5.10

例：

```
#### 複数行の clientid の場合、最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

<<<<<<< HEAD
EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を提供します。これらは抽出データの操作や整形に使えます。例として`lower()`、`upper()`、`concat()`は文字列のフォーマット調整に、`hash()`や`hash_to_range()`はハッシュや範囲マッピングに利用します。

使用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`は任意の中間非文字列値を文字列に変換
- **配列関数**：[nth/2](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
=======
EMQX にはルールエンジンの文字列関数に類似した豊富な文字列、配列、乱数、ハッシュ関数が用意されています。これらは抽出データの操作や整形に使えます。例：`lower()`、`upper()`、`concat()`、`hash()`、`hash_to_range()` など。

利用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数 `any_to_string/1`（任意の中間値を文字列に変換）
- **配列関数**：`nth/2` など（[参照](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)）
>>>>>>> origin/release-5.10
- **乱数関数**：`rand_str`、`rand_int`
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_decode(Data, 'no_padding')](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)  (6.0.2以降)
  - [base64_decode(Data, 'no_padding', 'urlsafe')](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)  (6.0.2以降)
  - [base64_encode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
<<<<<<< HEAD
  - [base64_encode(Data, 'no_padding')](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string) (6.0.2以降)
  - [base64_encode(Data, 'no_padding', 'urlsafe')](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string) (6.0.2以降)
  - `int2hexstr(Integer)`：整数を16進文字列にエンコード（例：15は`F`（大文字））
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：`Algorithm`は`md4`、`md5`、`sha`（または`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`のいずれか
  - `hash_to_range(Input, Min, Max)`：`sha256`でハッシュ化し、`Min`から`Max`までの整数にマッピング（`Min <= X <= Max`）
  - `map_to_rage(Input, Min, Max)`：入力を`Min`から`Max`までの整数にマッピング（`Min <= X <= Max`）
- **比較関数**：
  - `num_eq(A, B)`：2つの数値が同じなら`true`、そうでなければ`false`
  - `num_neq(A, B)`：2つの数値が異なれば`true`、そうでなければ`false`
  - `num_gt(A, B)`：`A > B`なら`true`、そうでなければ`false`
  - `num_gte(A, B)`：`A >= B`なら`true`、そうでなければ`false`
  - `num_lt(A, B)`：`A < B`なら`true`、そうでなければ`false`
  - `num_lte(A, B)`：`A <= B`なら`true`、そうでなければ`false`
  - `str_eq(A, B)`：2つの文字列が同じなら`true`、そうでなければ`false`
  - `str_neq(A, B)`：2つの文字列が異なれば`true`、そうでなければ`false`
  - `str_gt(A, B)`：辞書順で`A`が`B`より後なら`true`、そうでなければ`false`
  - `str_gte(A, B)`：辞書順で`A`が`B`より前でないなら`true`、そうでなければ`false`
  - `str_lt(A, B)`：辞書順で`A`が`B`より前なら`true`、そうでなければ`false`
  - `str_lte(A, B)`：辞書順で`A`が`B`より後でないなら`true`、そうでなければ`false`
  - `is_empty_var(V)`：変数が空かどうかを判定。Variformの空は未定義値（`undefined`）、JSONの`null`（文字列`"null"`は含まない）、または空文字列`""`を意味します。
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`を返します。文字列も受け入れ、入力が文字列の場合は出力も文字列です。

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返します。以下の制約があります。
    - OS環境変数を読み取る際に`EMQXVAR_`プレフィックスが付加されます。例：`getenv('FOO_BAR')`は`EMQXVAR_FOO_BAR`を読み取ります。
    - OS環境変数から読み込んだ値は不変です。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスでネスト構造を辿って値を抽出。例：`username`がJSONオブジェクトの場合、`json_value(username, 'shop.floor')`でフィールドにアクセス可能。
  - `jwt_value(Data, Path)`：JWTトークンのペイロードをデコードし、ドット区切りパスでクレーム値を抽出。例：`password`がカスタムクレームを持つJWTの場合、`jwt_value(password, 'client_attrs.unitid')`でネスト値にアクセス可能。

#### 条件式

Variform式は包括的な制御構造を持ちませんが、以下の関数で基本的な返却値制御が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、そうでなければ`ElseExpression`を返します。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返します。
- `coalesce([Element1, Element2, ...])`：最初の空でない要素を返します。

#### エラー処理

Bashなどのスクリプト環境のデフォルト動作と同様に、Variform式は未束縛変数や実行時例外が発生した場合、空文字列（`""`）を返すよう設計されています。

- 未束縛変数：定義されていない変数を参照すると空文字列になります。
- 実行時例外：関数の誤用や型不一致などの例外が発生すると空文字列を返します。例：配列のインデックス範囲外。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclient IDのプレフィックスを抽出。
- `strlen(username, 0, 5)`：部分的なusernameを抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclient IDから数字を抽出。空文字列なら`'000'`を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid`が`foo.`で始まる場合`foo`、そうでなければ`bar`を返す。
=======
  - `int2hexstr(Integer)`：整数を16進文字列に変換（例：15 → 'F' 大文字）
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：`Algorithm` は md4 | md5 | sha (sha1) | sha224 | sha256 | sha384 | sha512 | sha3_224 | sha3_256 | sha3_384 | sha3_512 | shake128 | shake256 | blake2b | blake2s のいずれか
  - `hash_to_range(Input, Min, Max)`：sha256 でハッシュ化し、`Min` から `Max` の範囲の整数にマッピング（`Min <= X <= Max`）
  - `map_to_rage(Input, Min, Max)`：入力を `Min` から `Max` の範囲の整数にマッピング（`Min <= X <= Max`）
- **比較関数**：
  - `num_eq(A, B)`：数値が等しければ `true`、そうでなければ `false`
  - `num_neq(A, B)`：数値が異なれば `true`、そうでなければ `false`
  - `num_gt(A, B)`：`A > B` なら `true`、そうでなければ `false`
  - `num_gte(A, B)`：`A >= B` なら `true`、そうでなければ `false`
  - `num_lt(A, B)`：`A < B` なら `true`、そうでなければ `false`
  - `num_lte(A, B)`：`A <= B` なら `true`、そうでなければ `false`
  - `str_eq(A, B)`：文字列が等しければ `true`、そうでなければ `false`
  - `str_neq(A, B)`：文字列が異なれば `true`、そうでなければ `false`
  - `str_gt(A, B)`：辞書順で `A` が `B` より後なら `true`、そうでなければ `false`
  - `str_gte(A, B)`：辞書順で `A` が `B` より前でないなら `true`、そうでなければ `false`
  - `str_lt(A, B)`：辞書順で `A` が `B` より前なら `true`、そうでなければ `false`
  - `str_lte(A, B)`：辞書順で `A` が `B` より後でないなら `true`、そうでなければ `false`
  - `is_empty_var(V)`：変数が空か判定。Variform での空は未定義（`undefined`）、JSON の `null`（文字列 `"null"` ではない）、空文字列 `""` を含む
  - `not(Bool)`：`Bool` が `false` なら `true`、`true` なら `false`。文字列も受け付け、入力が文字列なら出力も文字列

- **システム関数**：
  - `getenv(Name)`：環境変数 `Name` の値を返す。ただし OS 環境変数の読み取り時は `EMQXVAR_` プレフィックスが付加される（例：`getenv('FOO_BAR')` は `EMQXVAR_FOO_BAR` を読み込む）。値は読み込み後不変。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON 文字列からドット区切りパスで値を抽出。例：`username` が JSON オブジェクトなら `json_value(username, 'shop.floor')` でフィールドにアクセス。
  - `jwt_value(Data, Path)`：JWT トークンのペイロードからクレーム値をドット区切りパスで抽出。例：`password` がカスタムクレームを持つ JWT の場合、`jwt_value(password, 'client_attrs.unitid')` でネスト値にアクセス。

#### 条件式

Variform 式は包括的な制御構造を持ちませんが、以下の関数で基本的な制御が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition` が `true` または空でない文字列なら `ThenExpression` を返し、そうでなければ `ElseExpression` を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：配列の最初の空でない要素を返す。

#### エラー処理

Bash のようなスクリプト環境のデフォルト動作として、Variform 式は未定義変数参照や実行時例外が発生した場合に空文字列 `""` を返します。

- 未定義変数：定義されていない変数参照は空文字列として評価されます。
- 実行時例外：関数の誤用や型不整合、範囲外アクセスなどの例外は空文字列を返します。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りの clientid のプレフィックスを抽出。
- `strlen(username, 0, 5)`：username の部分文字列を抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現で clientid から数字を抽出。空文字列なら `'000'` を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true` を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false` を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true` を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid` が `foo.` で始まれば `foo`、そうでなければ `bar` を返す。
>>>>>>> origin/release-5.10
