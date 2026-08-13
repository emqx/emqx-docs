# 設定ファイル

ユーザーは設定ファイルまたは環境変数を使ってEMQXを設定できます。本章では主にEMQXの設定ファイルについて紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目と解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定および実行時データを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、実行時に生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ（`etc`）

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイやアップグレード時に変更され、実行時は安定性を保つために読み取り専用となります。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                          | パス               |
| ---------------------------------------- | ------------------ |
| RPMまたはDEBパッケージでインストール     | `/etc/emqx`        |
| Dockerコンテナで実行                      | `/opt/emqx/etc`    |
| ポータブル圧縮パッケージから展開         | `./etc`            |

### 動的設定ディレクトリ（`data/configs`）

実行時には、ダッシュボード、REST API、CLIを通じて動的に設定を変更できます。これらの変更は`data/configs`ディレクトリに保存され、セッション間で永続化されます。このディレクトリの場所もインストール方法によって異なります。

| インストール方法                          | パス                     |
| ---------------------------------------- | ------------------------ |
| RPMまたはDEBパッケージでインストール     | `/var/lib/emqx/configs`  |
| Dockerコンテナで実行                      | `/opt/emqx/data/configs` |
| ポータブル圧縮パッケージから展開         | `./data/configs`         |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することで、データディレクトリの場所を変更可能です。ただし、クラスター運用時はすべてのノードで同一のディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、重複があった場合は事前定義された上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションで詳細なリファレンスを提供していますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合、設定例は`etc/emqx/examples`ディレクトリにあります。
- DockerコンテナでEMQXを実行している場合は、`opt/emqx/etc/examples`ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが追加されました。このファイルにはデフォルト設定が含まれ、実行時により上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、後からダッシュボードUIでより複雑な設定に上書きすることができます。

`node`や`cluster`のような不変設定は`base.hocon`に設定することは**推奨されません**。詳細は[Immutable Configurations File](#immutable-configuration-file)をご参照ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、配置されたノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリ内の`cluster.hocon`ファイルはクラスター全体の設定項目を含みます。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新規ノードが追加された場合、対象ノードは自動的に他のノードから`cluster.hocon`をコピーして適用します。そのため、このファイルを手動で編集することは推奨されません。

このファイルの設定は`base.hocon`の設定に上書きされる形で適用されます。設定の上書き階層については[Config Override Rules](#config-override-rules)を参照してください。

EMQX 5.1以降、クラスタ設定の変更時に`cluster.hocon`のバックアップが作成されます。バックアップはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`設定を含む重要なシステム設定の主要な設定ファイルとして残っています。このファイルは`base.hocon`や`cluster.hocon`よりも優先度が高いですが、環境変数よりは低い優先度です。

設定の上書きに関する詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定パス

EMQXでは設定値をドット区切りのパスで参照できます。これはツリー構造に似ており、ルート（常にStruct）から始まり、各セグメントはフィールド名またはMapのキーを指します。配列要素の場合は1ベースのインデックスを使用します。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0以降、設定ファイルフォーマットとして[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間に読みやすいデータ形式であり、JSONのスーパーセットです。継承、結合、引用符などの機能により設定作業をさらに簡素化します。

**HOCON構文例：**

HOCON値はJSONに似たオブジェクトとして表現可能です。

```bash
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

またはフラット化して：

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このフラット化形式は過去のEMQXバージョンとの互換性がありますが、使い方が異なります。

HOCONでは文字列の両端に引用符を付けることを推奨します。特殊文字を含まない文字列は引用符なしでも構いません（例：`foo`、`foo_bar`）。一方、フラット形式は`=`の右側のすべてを値として扱います。

HOCON構文の詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)をご覧ください。

## 環境変数

設定ファイルのほかに、環境変数を使ってEMQXを設定することも可能です。

例えば、環境変数`EMQX_NODE__NAME=emqx2@127.0.0.1`は以下の設定を上書きします。

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数の変換ルールは以下の通りです。

1. 設定ファイルの`.`区切りは環境変数で使えないため、EMQXは`__`（ダブルアンダースコア）を区切りとして使用します。
2. 他の環境変数と区別するために、環境変数名の先頭に`EMQX_`を付けます。
3. 環境変数の値はHOCON値として解析されるため、複雑なデータ型も渡せます。`:`、`=`、`#`などのHOCON特殊文字を含む値は必ずダブルクォート`"`で囲み、文字列として扱われるようにしてください。特に`#`はHOCONの行コメントを開始するため、引用符なしでは`#`以降がコメントとして無視されます。

変換例：

```bash
# 環境変数

## localhost:1883は構造体{"localhost": 1883}として解析されるため、ダブルクォートで囲む必要があります
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

## HOCON配列を文字列として直接渡す
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

::: warning `#`、`:`、`=`を含む値について

パスワードなどの文字列に`#`を含む場合、`#`はHOCONの行コメントを開始するため、

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

はパスワードが`MQtt`として解析され、`#123`はコメントとして無視されます。文字列として正しく渡すには、**HOCONレベルの**ダブルクォート（シェルの引用符ではなく）で囲み、パーサーに`"MQtt#123"`として認識させます。

```bash
# 正しい例 — HOCONパーサーに渡される値は "MQtt#123"
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# 同じ効果で、シェル用に内側の引用符をエスケープ
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

`:`や`=`を含む値も同様です。URLエンコード（例：`%23`）は動作しません。EMQXは環境変数の値をURLデコードしません。

:::

::: tip なぜ一部の未引用値は通り、一部は通らないのか

EMQXは環境変数の値を`fake_key=<value>`としてHOCONパースを試みます。成功すれば解析結果を使用し、失敗すれば生の文字列として扱います。したがって、

`EMQX_..._PASSWORD="abc#def"`は有効なHOCONで`#def`がコメントとなり`abc`になる一方、

`EMQX_..._PASSWORD=".abc#def"`は無効なHOCONのため生の文字列`.abc#def`が使われます。HOCONの引用符で囲むと動作が安定します。

:::

::: tip

EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。

既知のルートパスで未知のフィールド名が設定された場合、起動時に`warning`ログを出力します。例えば、`enable`を誤って`enabled`と設定すると以下のように出力されます。

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 設定上書きルール

EMQXでは設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が先の値を上書きします。
- 上位の設定ファイルが下位の設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`  

つまり、`base.hocon`の設定は最も優先度が低く、より優先度の高いファイルで上書き可能です。`EMQX_`で始まる環境変数が最も優先されます。

::: tip
バージョン5.8.4以前は`base.hocon`ファイルが存在しませんでした。優先順位は同じですが、`base.hocon`は含まれません。
:::

EMQXダッシュボードUI、HTTP API、CLIからの変更は実行時に`cluster.hocon`に永続化され即時反映されます。ただし、ノード再起動後に`emqx.conf`や環境変数で異なる設定がある場合、変更が元に戻ることがあります。

混乱を避けるため、`emqx.conf`と`cluster.hocon`で設定を重複させることは**避けてください**。

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`が存在し、優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. 5.0.2/v5.0.22以前から最新バージョンにアップグレードする場合、優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`機構は5.1で廃止されました。
:::

### 上書き例

以下の設定では、最後の行で定義された`level`の`debug`が先の`error`を上書きしますが、`enable`フィールドは変更されません。

```bash
log {
  console {
    enable = true
    level = error
  }
}

## コンソールログのレベルをdebugに設定し、他の設定は維持
log.console.level = debug
```

パケットサイズ制限は最初に1MBに設定され、その後10MBに上書きされています。

```bash
zones {
  zone1 {
    mqtt.max_packet_size = 1M
  }
}
zones.zone1.mqtt.max_packet_size = 10M
```

### 配列要素の上書き

EMQXの配列は以下の2つの表現方法があります。

- リスト形式（例：`[1, 2, 3]`）
- マップ形式（サブスクライブ用、例：`{"1"=1, "2"=2, "3"=3}`）

以下の3つは同等です。

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

この特徴を利用し、配列の特定要素の値を簡単に上書きできます。

```bash
authentication  = [
  {
    enable = true,
    backend = "built_in_database",
    mechanism = "password_based"
  }
]

# 1番目の要素のenableフィールドを上書き
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

## 以下の方法では1番目の要素のenable以外のフィールドが失われます
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。リスナーは`zone`フィールドにゾーン名を設定して紐づけられます。ゾーンに紐づくリスナーに接続したMQTTクライアントは、そのゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐づいています。`default`ゾーンは論理的なグループであり、設定ファイル上には存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続やセッション設定。特定ゾーンでMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検知。
- `durable_sessions`：MQTTセッションの永続化設定。特定ゾーンで永続化を有効化など。

EMQX 5系のデフォルト設定ファイルにはゾーンは含まれていません。これはEMQX 4系の`internal`と`external`の2つのデフォルトゾーンとは異なります。

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

リスナーで`zone`フィールドに作成済みのゾーン名を設定して紐づけます。

```bash
listeners.tcp.default {
    bind = 1883
    zone = my_zone1
    ...
}
```

## スキーマ

HOCONオブジェクトの型安全性を高めるため、EMQXはスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などに利用されます。

[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)はこのスキーマから生成されています。

::: tip
ゾーン設定のスキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルのプリミティブ型はほぼ自明で、詳細な説明は不要です。以下に全リストを示します。

#### Integer

整数値。例：`42`、`-3`、`0`。

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義されたシンボルのいずれかのみ許容。例：`Enum(debug,info,warning,error)`はログレベルを定義。

#### String

文字列型で、様々な用途に対応する複数の形式をサポートします。

- **未引用文字列**：特殊文字を含まない単純な識別子や名前に適します（詳細は後述）。
- **引用文字列**：特殊文字や空白を含む場合はダブルクォート`"`で囲み、必要に応じてバックスラッシュ`\`でエスケープします。例：`"line1\nline2"`。
- **三重引用文字列**：`"""`で囲み、`\`以外のエスケープ不要で複雑な内容を含められます。三重引用符に隣接するクォートはエスケープが必要です。
- **インデント付き三重引用文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを保持でき、多行や整形テキストに適します。

**未引用文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、空白
- `//`で始めない（コメントと誤認されるため）
- `true`、`false`、`null`で始めない（ブールやnullと誤認されるため）

**三重引用文字列のガイドライン：**

- 三重引用符に隣接するクォートはエスケープまたは`~`区切りを使う
- 複数行文字列はスペース（タブ不可）でインデント可能。インデントレベルは最小の先頭スペース数で決定

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

HOCONの文字列引用規則の詳細は[HOCON仕様](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings)を参照してください。

EMQX独自のインデント付き三重引用文字列の詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)をご覧ください。

#### String("constant")

定数文字列値。単一値の列挙型（`Enum`）のように動作し、特定の静的値やモードを定義するのに使います。

#### Boolean

`true`または`false`（大文字小文字を区別）。

#### Float

浮動小数点数。例：`3.14`、`-0.001`。

#### Duration

人間が読みやすい形式の時間間隔。フォーマット例と説明あり。

#### Duration(s)

秒単位の精度を持つ`Duration`型。詳細と例あり。

#### Secret

パスワードやトークンなどの機密情報用型。用途と重要性の説明あり。

### 複合データ型

EMQXのHOCON設定における複合型は、プリミティブ型や他の複合型を含む階層的なデータ構造を表現します。

#### Struct `Struct(name)`

波括弧`{}`で囲まれたフィールドを持つ構造体。`name`は構造体のフィールド名と型を定義するスキーマ名。

#### Map `Map($name->Type)`

`Struct`に似ていますが、フィールド名が事前定義されていないキー・バリューの集合です。

`$name`は任意の文字列（`.`を含まない）で、エンティティや属性名を表します。`Type`はすべての値が同一型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを取れるユニオン型。あるフィールドが複数型のうちどれか1つを受け入れることを示します。例：`String(infinity)`または`Duration`。

#### Array `Array(Type)`

指定型の要素からなる配列。

::: tip

Mapのフィールド名が正の整数の場合、`Array`の別表現として解釈されます。例：

```bash
myarray.1 = 74
myarray.2 = 75
```

は`myarray = [74, 75]`と解釈され、配列要素の上書きに便利です。

:::

### Variform式

Variformは文字列操作や実行時評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQX設定内で文字列操作を動的に行うために埋め込まれます。

::: tip
Variform式は特定の設定項目でのみ使用可能です。指定がない限り使用しないでください。
:::

::: tip NULL値について

Variform式では値のバインディング参照や部分式の評価結果が未定義の場合、空文字列（`''`）として扱われます。

JSONで`null`のフィールドは未定義値（`''`）として扱われ、文字列`"null"`とは異なります。

:::

#### 構文例

```js
function_call(clientid, another_function_call(username))
```

これは`clientid`と`username`を組み合わせて新しい文字列を生成する例です。

Variformがサポートするリテラル：

- ブール値：`true`または`false`
- 整数：例`42`
- 浮動小数点数：例`3.14`
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲むASCII文字
- 配列：`[`と`]`で囲み、カンマ`,`で区切る要素
- 変数：事前定義された値の参照（例：`clientid`）
- 関数：組み込み関数（例：`concat([...])`）

Variformは以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のアンエスケープは`unescape`関数を呼び出す）

以下はVariform式を埋め込んだ設定例です。

```js
mqtt {
    client_attrs_init = [
        {
            # clientidの最初の'-'までのプレフィックスを抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.groupとして設定
            set_as_attr = group
        }
    ]
}
```

::: tip
アンエスケープ関数を使う場合、HOCON設定で三重引用符`"""`を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行のclient IDのうち最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 組み込み関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を備えています。これらは抽出したデータの操作や整形に使えます。例えば、`lower()`、`upper()`、`concat()`は文字列のフォーマット調整に、`hash()`や`hash_to_range()`はハッシュ値や範囲マッピングに利用可能です。

利用可能な関数の例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`は任意の中間非文字列値を文字列に変換します。
- **配列関数**：[nth/2](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
- **乱数関数**：rand_str、rand_int
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_decode(Data, 'no_padding')](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)（6.0.2以降）
  - [base64_decode(Data, 'no_padding', 'urlsafe')](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)（6.0.2以降）
  - [base64_encode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - [base64_encode(Data, 'no_padding')](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)（6.0.2以降）
  - [base64_encode(Data, 'no_padding', 'urlsafe')](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)（6.0.2以降）
  - `int2hexstr(Integer)`：整数を16進文字列に変換（例：15 → 'F'（大文字））
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：アルゴリズムはmd4、md5、sha（sha1）、sha224、sha256、sha384、sha512、sha3_224、sha3_256、sha3_384、sha3_512、shake128、shake256、blake2b、blake2sのいずれか
  - `hash_to_range(Input, Min, Max)`：sha256でハッシュし、MinからMaxの範囲にマッピング（Min <= X <= Max）
  - `map_to_rage(Input, Min, Max)`：入力をMinからMaxの範囲にマッピング（Min <= X <= Max）
- **比較関数**：
  - `num_eq(A, B)`：2つの数値が等しければ`true`、そうでなければ`false`
  - `num_neq(A, B)`：2つの数値が異なれば`true`、そうでなければ`false`
  - `num_gt(A, B)`：AがBより大きければ`true`、そうでなければ`false`
  - `num_gte(A, B)`：AがB以上なら`true`、そうでなければ`false`
  - `num_lt(A, B)`：AがBより小さければ`true`、そうでなければ`false`
  - `num_lte(A, B)`：AがB以下なら`true`、そうでなければ`false`
  - `str_eq(A, B)`：2つの文字列が同じなら`true`、そうでなければ`false`
  - `str_neq(A, B)`：2つの文字列が異なれば`true`、そうでなければ`false`
  - `str_gt(A, B)`：辞書順でAがBの後なら`true`、そうでなければ`false`
  - `str_gte(A, B)`：辞書順でAがBの前でなければ`true`、そうでなければ`false`
  - `str_lt(A, B)`：辞書順でAがBの前なら`true`、そうでなければ`false`
  - `str_lte(A, B)`：辞書順でAがBの後でなければ`true`、そうでなければ`false`
  - `is_empty_var(V)`：変数が空か判定。Variformの空は未定義（`undefined`）、JSONの`null`（文字列`"null"`は除く）、空文字列`""`を含む
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`を返す。文字列も受け付け、入力が文字列なら出力も文字列

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。OS環境変数読み込み時に`EMQXVAR_`を接頭辞として付加。読み込み後は値は不変。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスでネスト構造の値を抽出。例：`username`がJSONオブジェクトの場合、`json_value(username, 'shop.floor')`でフィールドにアクセス。
  - `jwt_value(Data, Path)`：JWTトークンのペイロードをデコードし、ドット区切りパスでクレーム値を抽出。例：`password`がカスタムクレームを持つJWTの場合、`jwt_value(password, 'client_attrs.unitid')`でネスト値を取得。

#### 条件式

Variform式は包括的な制御構造を持ちませんが、以下の関数で基本的な条件制御が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、そうでなければ`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の空でない要素を返す。

#### エラー処理

Bashなどのスクリプト環境と同様に、Variform式は未バインド変数や実行時例外が発生した場合に空文字列（`""`）を返す設計です。

- 未バインド変数：定義されていない変数参照は空文字列として評価されます。
- 実行時例外：関数の誤用や型不一致などの例外発生時も空文字列を返します。例：配列のインデックス範囲外。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclientidのプレフィックスを抽出。
- `strlen(username, 0, 5)`：usernameの部分文字列を抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclientidから数字を抽出。空文字列なら`'000'`を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：clientidが`foo.`で始まれば`foo`、そうでなければ`bar`を返す。
