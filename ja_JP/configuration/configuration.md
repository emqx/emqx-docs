# 設定ファイル

ユーザーは設定ファイルまたは環境変数で EMQX を設定できます。本節では主に EMQX の設定ファイルについて紹介し、EMQX で最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目や説明については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) を参照してください。

## 設定ディレクトリ

EMQX をインストールすると、設定および実行時データを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ (`etc`)**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ (`data/configs`)**：書き込み可能で、実行時に生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ (`etc`)

`etc` ディレクトリには EMQX の初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイやアップグレード時に変更され、実行時は安定性を確保するため読み取り専用となります。`etc` ディレクトリの場所はインストール方法によって異なります。

| インストール方法                        | パス              |
| ------------------------------------- | ----------------- |
| RPM または DEB パッケージでインストール | `/etc/emqx`       |
| Docker コンテナで実行                   | `/opt/emqx/etc`   |
| ポータブル圧縮パッケージから展開       | `./etc`           |

### 動的設定ディレクトリ (`data/configs`)

実行時には、Dashboard、REST API、CLI を通じて動的に設定を変更できます。これらの変更は `data/configs` ディレクトリに保存され、セッションをまたいで永続化されます。このディレクトリの場所もインストール方法によって異なります。

| インストール方法                        | パス                      |
| ------------------------------------- | ------------------------- |
| RPM または DEB パッケージでインストール | `/var/lib/emqx/configs`   |
| Docker コンテナで実行                   | `/opt/emqx/data/configs`  |
| ポータブル圧縮パッケージから展開       | `./data/configs`          |

::: tip
`node.data_dir` 設定や環境変数 `EMQX_NODE__DATA_DIR` を変更することでデータディレクトリを変更可能です。ただし、クラスター運用時はすべてのノードで同じディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、重複があった場合は事前に定められた上書きルールに従って解決されます。詳細は [Config Override Rules](#config-override-rules) を参照してください。

## 設定例

[Schema](#schema) セクションで詳細なリファレンスを提供していますが、設定例は EMQX の設定理解や適用に役立ちます。

- RPM または DEB パッケージでインストールした場合、`etc/emqx/examples` ディレクトリに設定例があります。
- Docker コンテナで実行している場合は、`opt/emqx/etc/examples` ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4 以降、`etc` ディレクトリに `base.hocon` というベース設定ファイルがあります。このファイルにはデフォルト設定が含まれており、実行時により上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、その後 Dashboard UI からより複雑な設定で上書きすることができます。

`node` や `cluster` のような不変設定は `base.hocon` に設定することは推奨されません。詳細は [Immutable Configurations File](#immutable-configuration-file) を参照してください。

::: tip
`base.hocon` ファイルはクラスター間で同期されず、配置されたノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs` ディレクトリの `cluster.hocon` ファイルにはクラスター全体の設定項目が含まれています。Dashboard、REST API、CLI からの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、ノードは自動的に他のノードから `cluster.hocon` をコピーして適用します。そのため、このファイルを手動で編集することは推奨されません。

このファイルの設定は `base.hocon` の設定の上に適用されます。設定の上書き階層については [Config Override Rules](#config-override-rules) を参照してください。

EMQX 5.1 以降、クラスター設定の変更時に `cluster.hocon` ファイルのバックアップが作成されます。バックアップはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf` ファイルは `node` や `cluster` 設定など重要なシステム設定の主要な設定ファイルとして残っています。このファイルは `base.hocon` や `cluster.hocon` より優先度が高いですが、環境変数よりは低い優先度です。

設定の上書き詳細は [Config Override Rules](#config-override-rules) を参照してください。

## 設定パス

EMQX では設定値をドット区切りのパスで参照できます。これはツリー構造のようなもので、ルート（常に Struct）から始まり、各セグメントはフィールド名または Map のキーを表します。配列要素の場合は1始まりのインデックスを使います。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON 設定フォーマット

EMQX v5.0 からは設定ファイルフォーマットとして [Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon) を採用しています。

HOCON は人間に読みやすいデータ形式で、JSON のスーパーセットです。継承や結合、引用符などの機能により設定作業を簡素化します。

**HOCON 構文例：**

JSON風のオブジェクトとして表現可能です。

```bash
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

またはフラットに書くこともできます。

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このフラット形式は旧バージョンの EMQX と互換性がありますが、使い方が異なります。

HOCON では文字列の両端に引用符を付けることを推奨しています。特殊文字を含まない文字列は引用符なしでも構いません（例：`foo`、`foo_bar`）。一方、フラット形式は `=` の右側のすべてを値として扱います。

HOCON 構文の詳細は [HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md) を参照してください。

## 環境変数

設定ファイルのほかに環境変数でも EMQX を設定できます。

例えば、環境変数 `EMQX_NODE__NAME=emqx2@127.0.0.1` は以下の設定を上書きします。

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数は以下のルールで変換されます。

1. 設定ファイルの区切り文字 `.` は環境変数で使えないため、EMQX では区切りにダブルアンダースコア `__` を使います。
2. 他の環境変数と区別するため、環境変数名の先頭に `EMQX_` を付けます。
3. 環境変数の値は HOCON 値として解析されるため、複雑なデータ型も渡せます。値に `:`, `=`, `#` などの HOCON 特殊文字が含まれる場合は、ダブルクォート `"` で囲む必要があります。特に `#` は HOCON の行コメントを開始するため、引用符なしだと `#` 以降がコメントとして無視されます。

変換例：

```bash
# 環境変数

## localhost:1883 は構造体 {"localhost": 1883} と解釈されるため、ダブルクォートで囲む必要がある
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

## HOCON 配列を文字列として直接渡す
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

::: warning `#`, `:`, `=` を含む値について

パスワードなどの文字列に `#` を含む場合、`#` は HOCON の行コメント開始文字のため、以下のように設定するとパスワードが途中で切れてしまいます。

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

この場合、パスワードは `MQtt` と解釈され、`#123` はコメントとして無視されます。文字列をそのまま渡すには、**HOCONレベルの**ダブルクォート（シェルのクォートではなく）で囲み、パーサーに `"MQtt#123"` と認識させる必要があります。

```bash
# 正しい例 — HOCON パーサーに渡される値は "MQtt#123"
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# 同じ効果、シェル用に内側のクォートをエスケープ
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

`:` や `=` を含む値も同様です。URLエンコード（例：`%23`）は無効で、EMQX は環境変数値を URL デコードしません。

:::

::: tip なぜ一部の引用なし値は通り、一部は通らないのか

EMQX は環境変数の値を内部的に `fake_key=<値>` として HOCON パースを試みます。成功すればパース結果を使い、失敗すれば生の文字列を使います。そのため、

`EMQX_..._PASSWORD="abc#def"` は有効な HOCON で `#def` はコメント扱いされるため `abc` となりますが、

`EMQX_..._PASSWORD=".abc#def"` は無効な HOCON なので生文字列 `.abc#def` が使われます。

HOCON の引用符で囲むと挙動が明確になります。

:::

::: tip

EMQX は未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。`UNKNOWN_ROOT` は事前定義されたルートパスではないためです。

既知のルートパスに未知のフィールド名が設定された場合、起動時に `warning` ログを出力します。例えば `enable` を誤って `enabled` と設定すると以下のように出力されます。

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 設定上書きルール

EMQX では設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が前の値を上書きします。
- 上位の設定ファイルが下位の設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり `base.hocon` の設定は最も優先度が低く、より優先度の高いファイルで上書きされます。`EMQX_` で始まる環境変数が最も優先されます。

::: tip
バージョン 5.8.4 より前は `base.hocon` ファイルは存在しませんでした。優先順位は同じですが `base.hocon` は含まれません。
:::

Dashboard UI、HTTP API、CLI からの変更は実行時に `cluster.hocon` に永続化され即時反映されます。ただし、`emqx.conf` や環境変数で同じ設定項目が異なる値で設定されていると、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、`emqx.conf` と `cluster.hocon` で設定を重複させることは**避けてください**。

::: tip
1. 古い EMQX バージョン（例：5.0.2/v5.0.22 以前）では `cluster-override.conf` ファイルが存在し、優先順位は `emqx.conf < ENV < HTTP API (cluster-override.conf)` でした。
2. 5.0.2/v5.0.22 以前から最新バージョンにアップグレードする場合、優先順位は変わらず、互換性維持のため `cluster.hocon` は作成されません。
3. `cluster-override.conf` 機構はバージョン 5.1 で廃止されました。
:::

### 上書き例

以下の設定では、最後の行で定義された `level` の `debug` が前の `error` を上書きしますが、`enable` フィールドは変更されません。

```bash
log {
  console {
    enable = true
    level = error
  }
}

## コンソールログの出力レベルを debug に設定し、他の設定は維持
log.console.level = debug
```

パケットサイズ制限は最初に 1MB に設定され、その後 10MB に上書きされています。

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
- マップ形式（サブスクライブ用、例：`{"1"=1, "2"=2, "3"=3}`）

以下の3つの書き方は同等です。

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

この特徴を利用して、配列の要素の値を簡単に上書きできます。

```bash
authentication  = [
  {
    enable = true,
    backend = "built_in_database",
    mechanism = "password_based"
  }
]

# 1番目の要素の `enable` フィールドを以下のように上書き可能
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

## 以下の設定で、1番目の要素の `enable` 以外のフィールドはすべて失われる
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQX のゾーンは設定をグループ化する概念です。ゾーンはリスナーの `zone` フィールドに設定することで紐づけられ、そのゾーンに関連付けられたリスナーに接続する MQTT クライアントはゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは `default` という名前のゾーンに紐づいています。`default` ゾーンは論理的なグループであり、設定ファイル上には存在しません。
:::

ゾーンレベルで上書き可能な設定項目：

- `mqtt`：MQTT 接続やセッション設定。特定ゾーンの MQTT メッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlang プロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検知。
- `durable_sessions`：セッション永続化設定。特定ゾーンで MQTT セッションの永続ストレージを有効化など。

EMQX バージョン 5 のデフォルト設定ファイルにはゾーンは含まれていません。バージョン 4 では `internal` と `external` の2つのデフォルトゾーンがありました。

ゾーンを作成するには、設定ファイルに以下のように定義します。

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
      # このゾーンでセッションの永続化を有効化
      enable = true
      ...
    }
  }
  my_zone2 {
    ...
  }
}
```

リスナーで `zone` フィールドに作成済みのゾーン名を設定して紐づけます。

```bash
listeners.tcp.default {
    bind = 1883
    zone = my_zone1
    ...
}
```

## スキーマ

HOCON オブジェクトの型安全性を高めるため、EMQX ではスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などに利用されます。

[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) はこのスキーマから生成されています。

::: tip
ゾーンの設定スキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば `zones.my_zone1.mqtt {...}` は `mqtt {...}` と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルのプリミティブ型はほぼ自明であり、簡潔な説明で十分です。以下は主なプリミティブ型の一覧です。

#### Integer（整数）

整数値。例：`42`、`-3`、`0`。

#### Integer(Min..Max)

指定された範囲内の整数。例：`1..+inf` は 1 以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義されたシンボルのいずれかのみを取ることができます。例：`Enum(debug,info,warning,error)` はログレベルの定義。

#### String（文字列）

文字列型は複数の形式をサポートしています。

- **引用符なし**：特殊文字を含まない単純な識別子や名前に適しています（後述の禁止文字を参照）。
- **引用符付き文字列**：特殊文字や空白を含む場合はダブルクォート（`"`）で囲み、必要に応じてバックスラッシュ（`\`）でエスケープします。例：`"line1\nline2"`。
- **三重引用符文字列**：三重引用符（`"""`）で囲み、`\` 以外のエスケープ不要で複雑な内容を表現可能。三重引用符の隣接するクォートはエスケープが必要です。
- **インデント付き三重引用符文字列**：`"""~` と `~"""` で囲み、EMQX 5.6 以降で導入。複数行や整形テキストのインデントを許容します。

**引用符なし文字列の注意点：**

- 禁止文字：`$`, `"`, `{`, `}`, `[`, `]`, `:`, `=`, `,`, `+`, `#`, `` ` ``, `^`, `?`, `!`, `*`, `&`, `\`、および空白を含まないこと。
- `//` で始まらないこと（コメント開始と誤認されるため）。
- `true`、`false`、`null` で始まらないこと（ブール値や null と誤認されるため）。

**トリプルクォート文字列のガイドライン：**

- 三重引用符の隣にクォートを含めたい場合はエスケープするか、`~` 区切りを使います。
- 複数行文字列はスペース（タブ不可）によるインデントが可能で、最小のインデント幅に合わせて調整されます。

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

HOCON の文字列引用規則の詳細は [HOCON specification](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings) を参照してください。

EMQX 独自のインデント付き三重引用符文字列の詳細は [emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats) をご覧ください。

#### String("constant")

定数文字列値で、単一値の列挙型（`Enum`）のように振る舞います。特定の設定やモードの静的値に使います。

#### Boolean（真偽値）

`true` または `false`（大文字小文字区別あり）。

#### Float（浮動小数点数）

小数点を含む数値。例：`3.14`、`-0.001`。

#### Duration（期間）

人間に読みやすい形式の時間の長さを表します。フォーマットの例と説明があります。

#### Duration(s)

秒単位の精度を持つ `Duration` 型。詳細と例があります。

#### Secret（シークレット）

パスワードやトークンなど機密情報用の型。使用方法と重要性について説明があります。

### 複合データ型

EMQX の HOCON 設定で使われる複合型は、他の複合型やプリミティブ型を含むデータ構造を表現します。階層的で柔軟なデータ表現を可能にします。

#### Struct `Struct(name)`

波括弧 `{}` で囲まれたフィールドを持つ構造体。`name` は構造体のフィールドと型を定義したスキーマの参照名です。

#### Map `Map($name->Type)`

`Struct` に似ていますが、フィールド名が事前定義されておらず、任意の文字列（`.` を含まない）をキーに持つキー・バリューの集合です。すべての値は同じ型 `Type` でなければなりません。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを取るユニオン型。あるフィールドが複数の型のいずれかを許容する場合に使います。例：`String(infinity)` または `Duration` のどちらか。

#### Array `Array(Type)`

指定された型 `Type` の要素からなる配列。

::: tip

Map のフィールド名が正の整数の場合、`Array` の別表現として解釈されます。例えば：

```bash
myarray.1 = 74
myarray.2 = 75
```

は `myarray = [74, 75]` と解釈され、配列要素の上書きに便利です。

:::

### Variform 式

Variform は文字列操作や実行時評価に特化した軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQX の設定内に埋め込んで動的に文字列操作を行うためのツールです。

::: tip
Variform 式は特定の設定項目にのみ適用されます。明記されていない限り使用しないでください。
:::

::: tip NULL 値について

Variform 式では値のバインディング参照や部分式の評価結果が未定義の場合、空文字列（`''`）として扱われます。

JSON でデコードしたフィールドが `null` の場合も未定義値（`''`）として扱われ、文字列 `"null"` とは異なります。

:::

#### 構文

例：

```js
function_call(clientid, another_function_call(username))
```

これは `clientid` と `username` を組み合わせて新しい文字列を生成します。

Variform は以下のリテラルをサポートします。

- ブール値：`true` または `false`
- 整数：例 `42`
- 浮動小数点数：例 `3.14`
- 文字列：シングルクォート `'` またはダブルクォート `"` で囲む ASCII 文字
- 配列：`[` と `]` で囲み、カンマ `,` 区切り
- 変数：事前定義された値の参照（例：`clientid`）
- 関数：組み込み関数（例：`concat([...])`）

Variform は以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のアンエスケープは `unescape` 関数を使用）

以下は Variform 式を埋め込んだ設定例です。

```js
mqtt {
    client_attrs_init = [
        {
            # clientid の最初の `-` までのプレフィックスを抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.group に設定
            set_as_attr = group
        }
    ]
}
```

::: tip
アンエスケープ関数が必要な場合は、HOCON 設定で三重引用符（`"""`）文字列を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行の client ID の場合、最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 組み込み関数

EMQX はルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を提供します。これらは抽出データの操作や整形に使えます。例：`lower()`、`upper()`、`concat()`、`hash()`、`hash_to_range()` など。

利用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数 `any_to_string/1`：任意の中間値を文字列に変換
- **配列関数**：`nth/2` など [nth/2](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
- **乱数関数**：`rand_str`, `rand_int`
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_encode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - `int2hexstr(Integer)`：整数を16進文字列に変換（例：15 → 'F'（大文字））
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：Algorithm は `md4 | md5, sha (or sha1) | sha224 | sha256 | sha384 | sha512 | sha3_224 | sha3_256 | sha3_384 | sha3_512 | shake128 | shake256 | blake2b | blake2s` のいずれか
  - `hash_to_range(Input, Min, Max)`：sha256 でハッシュ化し、Min から Max の範囲の整数にマッピング（Min <= X <= Max）
  - `map_to_rage(Input, Min, Max)`：入力を Min から Max の範囲の整数にマッピング（Min <= X <= Max）
- **比較関数**：
  - `num_eq(A, B)`：数値が等しい場合は `true`、それ以外は `false`
  - `num_neq(A, B)`：数値が等しくない場合は `true`、それ以外は `false`
  - `num_gt(A, B)`：A が B より大きい場合は `true`、それ以外は `false`
  - `num_gte(A, B)`：A が B 以上の場合は `true`、それ以外は `false`
  - `num_lt(A, B)`：A が B より小さい場合は `true`、それ以外は `false`
  - `num_lte(A, B)`：A が B 以下の場合は `true`、それ以外は `false`
  - `str_eq(A, B)`：文字列が等しい場合は `true`、それ以外は `false`
  - `str_neq(A, B)`：文字列が等しくない場合は `true`、それ以外は `false`
  - `str_gt(A, B)`：辞書順で A が B の後の場合は `true`、それ以外は `false`
  - `str_gte(A, B)`：辞書順で A が B の前でない場合は `true`、それ以外は `false`
  - `str_lt(A, B)`：辞書順で A が B の前の場合は `true`、それ以外は `false`
  - `str_lte(A, B)`：辞書順で A が B の後でない場合は `true`、それ以外は `false`
  - `is_empty_var(V)`：変数が空かどうかを判定。Variform の空は未定義（`undefined`）、JSON の `null`（文字列 `"null"` は除く）、または空文字列 `""`。
  - `not(Bool)`：`Bool` が `false` の場合は `true`、`true` の場合は `false`。文字列も受け付け、入力が文字列なら出力も文字列。

- **システム関数**：
  - `getenv(Name)`：環境変数 `Name` の値を返す。ただし OS 環境変数の読み込み時に `EMQXVAR_` プレフィックスが付加される（例：`getenv('FOO_BAR')` は `EMQXVAR_FOO_BAR` を読み込む）。読み込んだ値は不変。

#### 条件式

Variform 式には包括的な制御フローはありませんが、基本的な値の選択に使える関数があります。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition` が `true` または空でない文字列なら `ThenExpression` を返し、そうでなければ `ElseExpression` を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：配列の最初の空でない要素を返す。

#### エラー処理

Bash などのスクリプト環境と同様に、Variform 式はエラー発生時（未定義変数参照や実行時例外など）に空文字列（`""`）を返す設計です。

- 未定義変数：未定義またはスコープ外の変数参照は空文字列として評価されます。
- 実行時例外：関数の誤用や型不一致などの例外発生時も空文字列を返します。例：配列のインデックスが範囲外など。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りの clientid のプレフィックスを抽出。
- `strlen(username, 0, 5)`：username の一部を抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現で clientid から数字を抽出。空文字列なら `'000'` を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true` を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false` を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true` を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：clientid が `foo.` で始まれば `foo`、そうでなければ `bar` を返す。
