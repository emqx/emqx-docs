# 設定ファイル

ユーザーは設定ファイルまたは環境変数でEMQXを設定できます。本節では主にEMQXの設定ファイルについて紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目や解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

## 設定ディレクトリ

EMQXをインストールすると、設定や実行時データを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つに分類されます。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、実行時に生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ（`etc`）

`etc`ディレクトリには、EMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイ時やアップグレード時に変更され、実行時には安定性を保つため読み取り専用となっています。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                             | パス              |
| ------------------------------------------ | ----------------- |
| RPMまたはDEBパッケージでインストール       | `/etc/emqx`       |
| Dockerコンテナで実行                        | `/opt/emqx/etc`   |
| ポータブル圧縮パッケージを展開             | `./etc`           |

### 動的設定ディレクトリ（`data/configs`）

実行時には、ダッシュボード、REST API、CLIを通じて動的に設定を変更できます。これらの変更は`data/configs`ディレクトリに保存され、セッションをまたいで永続化されます。このディレクトリの場所もインストール方法によって異なります。

| インストール方法                             | パス                      |
| ------------------------------------------ | ------------------------- |
| RPMまたはDEBパッケージでインストール       | `/var/lib/emqx/configs`   |
| Dockerコンテナで実行                        | `/opt/emqx/data/configs`  |
| ポータブル圧縮パッケージを展開             | `./data/configs`          |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することで、データディレクトリの場所を変更可能です。ただし、クラスターを構成する場合は、すべてのノードで同じディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、万が一重複した場合は事前に定義された上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)を参照してください。

## 設定例

[Schema](#schema)セクションで詳細なリファレンスを提供していますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合は、`etc/emqx/examples`ディレクトリに設定例があります。
- DockerコンテナでEMQXを実行している場合は、`opt/emqx/etc/examples`ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルがあります。このファイルにはデフォルト設定が含まれており、実行時により上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、後からダッシュボードUIでより複雑な設定に上書きすることができます。

`node`や`cluster`のような不変の設定は、`base.hocon`ファイルに設定することは**推奨されません**。詳細は[Immutable Configurations File](#immutable-configuration-file)を参照してください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、そのファイルが存在するノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリの`cluster.hocon`ファイルにはクラスター全体の設定項目が含まれています。ダッシュボード、REST API、CLIから行った設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり、新しいノードが追加された場合、そのノードは自動的に他のノードから`cluster.hocon`ファイルをコピーして適用します。そのため、このファイルを手動で編集することは推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層の詳細は[Config Override Rules](#config-override-rules)を参照してください。

EMQX 5.1以降、クラスター設定の変更があると、`cluster.hocon`ファイルの上書き前にバックアップが作成されます。バックアップファイルはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`の重要なシステム設定の主要な設定ファイルとして残っています。このファイルは`base.hocon`や`cluster.hocon`より優先度が高いですが、環境変数よりは低い優先度です。

設定の上書きに関する詳細は[Config Override Rules](#config-override-rules)を参照してください。

## 設定パス

EMQXでは、設定値をドット区切りのパスで参照できます。これはツリー構造に似ており、ルート（常にStruct）から始まり、各セグメントがフィールド名またはMapのキーを指します。配列要素の場合は1始まりのインデックスを使用します。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0以降、設定ファイルフォーマットとして[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間に読みやすいデータフォーマットで、JSONのスーパーセットです。継承や結合、引用符などの機能により設定作業がさらに簡単になります。

**HOCON構文例：**

JSONに似たオブジェクト形式で表現可能です。

```bash
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

またはフラット形式でも記述できます。

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このイカのようなフラット形式は旧バージョンのEMQXとの互換性がありますが、使い方が異なります。

HOCONでは文字列の両端に引用符を付けることを推奨します。特殊文字を含まない文字列は引用符なしでも構いません（例：`foo`、`foo_bar`）。一方、イカ形式は`=`の右側の全てを値として扱います。

HOCON構文の詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)を参照してください。

## 環境変数

設定ファイルのほかに、環境変数でもEMQXを設定できます。

例えば、環境変数`EMQX_NODE__NAME=emqx2@127.0.0.1`は以下の設定を上書きします。

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数の変換ルールは以下の通りです。

1. 設定ファイルの`.`区切りは環境変数では使えないため、EMQXは`__`（ダブルアンダースコア）を区切りに使います。
2. 他の環境変数と区別するため、環境変数名の先頭に`EMQX_`を付けます。
3. 環境変数の値はHOCON値として解析されるため、複雑なデータ型も渡せます。`:`, `=`, `#`などのHOCON特殊文字を含む値は、ダブルクォート`"`で囲む必要があります。特に`#`はHOCONの行コメント開始文字なので、引用符なしだと`#`以降がコメントとして無視されます。

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

::: warning `#`, `:`, `=`を含む値について

パスワードなどに`#`を含む場合、`#`はHOCONの行コメント開始文字なので、以下のようにするとパスワードは`MQtt`として解析され、`#123`はコメントとして無視されます。

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

リテラルとして渡すには、**HOCONレベルの**ダブルクォート（シェルのクォートではなく）で囲み、パーサーに`"MQtt#123"`として認識させます。

```bash
# 正しい例 — HOCONパーサーが受け取る値は "MQtt#123"
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# 同じ効果、シェル用に内側のクォートをエスケープ
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

`:`や`=`を含む値も同様です。URLエンコード（例：`%23`）は無効で、EMQXは環境変数値をURLデコードしません。

:::

::: tip なぜ一部の引用なし値は通り、一部は通らないのか

EMQXは環境変数の値を`fake_key=<value>`としてHOCONパースを試みます。成功すれば解析結果を使い、失敗すれば生の文字列を使います。例えば`EMQX_..._PASSWORD="abc#def"`は有効なHOCONで`#def`がコメント扱いされ`abc`となりますが、`EMQX_..._PASSWORD=".abc#def"`は無効なHOCONなので生文字列`.abc#def`が使われます。値をHOCONのクォートで囲むと動作が安定します。

:::

::: tip

EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。

既知のルートパスに対して未知のフィールド名を設定すると、起動時に警告ログを出力します。例えば`enable`を誤って`enabled`と設定した場合、以下のように出力されます。

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 設定上書きルール

EMQXでは設定値は階層的に適用され、以下の上書きルールがあります。

- 同じファイル内では後に定義された値が前の値を上書きします。
- 上位の設定ファイルが下位の設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり、`base.hocon`の設定は最も優先度が低く、上位のファイルで上書きされます。`EMQX_`で始まる環境変数が最も優先されます。

::: tip
バージョン5.8.4以前は`base.hocon`ファイルが存在しませんでした。優先順位は同じですが、`base.hocon`は含まれません。
:::

EMQXダッシュボードUI、HTTP API、CLIからの変更は実行時に`cluster.hocon`に永続化され、即時反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、`emqx.conf`と`cluster.hocon`間で設定を重複させることは**避けてください**。

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`ファイルが存在し、優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. これらの古いバージョンから最新バージョンにアップグレードしても優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`機構はバージョン5.1で廃止されました。
:::

### 上書き例

以下の設定では、最後の行で`level`の値が`error`から`debug`に上書きされますが、`enable`フィールドは変更されません。

```bash
log {
  console {
    enable = true
    level = error
  }
}

## コンソールログの出力レベルをdebugに設定し、その他の設定は維持
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

この特徴を利用して、配列の特定要素の値を簡単に上書きできます。

```bash
authentication  = [
  {
    enable = true,
    backend = "built_in_database",
    mechanism = "password_based"
  }
]

# 1番目の要素の`enable`フィールドを以下のように上書き可能
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

## 以下の設定では1番目の要素の`enable`以外の全フィールドが失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。リスナーの`zone`フィールドにゾーン名を設定すると、そのゾーンに紐づいたリスナーに接続したMQTTクライアントはゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐づいています。`default`ゾーンは論理的なグループであり、設定ファイルには存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続やセッション設定。特定ゾーンでより大きな最大パケットサイズを許可するなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：セッション永続化設定。特定ゾーンでMQTTセッションの永続化を有効にするなど。

EMQX 5系のデフォルト設定ファイルにはゾーンは含まれていません。これは4系の`internal`と`external`という2つのデフォルトゾーンとは異なります。

ゾーンを作成するには設定ファイルに定義します。例：

```bash
zones {
  # 複数のゾーンを定義可能
  my_zone1 {
    # ゾーンはグローバル設定と同じスキーマを共有
    mqtt {
      # このゾーンの接続に対してより大きなパケットサイズを許可
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

リスナーで`zone`フィールドに作成済みのゾーン名を設定します。

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
ゾーンの設定スキーマは各グループで共通のため、設定マニュアルには含まれていません。例えば、`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルのプリミティブ型は自明なものが多く、簡単な説明で十分です。以下は主なプリミティブ型の一覧です。

#### Integer

整数値。例：`42`、`-3`、`0`。

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf`は1以上の正の整数。

#### Enum(symbol1, symbol2, ...)

列挙型。定義済みのシンボルのいずれかのみ許容。例：`Enum(debug,info,warning,error)`はログレベル。

#### String

文字列型。複数の形式があります。

- **無引用**：特殊文字を含まない単純な識別子や名前向け（詳細は後述）。
- **引用文字列**：特殊文字や空白を含む場合はダブルクォート`"`で囲み、必要に応じてバックスラッシュ`\`でエスケープ。例：`"line1\nline2"`。
- **三重引用文字列**：`"""`で囲み、`\`以外のエスケープ不要。複雑な内容を扱いやすい。三重引用符に隣接するクォートはエスケープが必要。
- **インデント付き三重引用文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを保持しつつ複数行や整形テキストを記述可能。

**無引用文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、空白文字。
- `//`で始めない（コメント開始と誤認されるため）。
- `true`、`false`、`null`で始めない（ブールやnullと誤認されるため）。

**三重引用文字列のガイドライン：**

- 三重引用符に隣接するクォートはエスケープするか`~`区切りを使う。
- 複数行文字列はスペース（タブ不可）でインデント可能。インデントレベルは最小の先頭スペース数で決定。

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

EMQX独自のインデント付き三重引用文字列の詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)を参照してください。

#### String("constant")

定数文字列。単一値の列挙（Enum）として機能し、変更されない静的値の定義に使います。

#### Boolean

`true`または`false`（大文字小文字区別あり）。

#### Float

浮動小数点数。例：`3.14`、`-0.001`。

#### Duration

人間に読みやすい時間の長さを表現。フォーマット例と説明あり。

#### Duration(s)

秒単位の精度を持つ`Duration`型。詳細と例あり。

#### Secret

パスワードやトークンなど機密情報用の型。用途と重要性の説明あり。

### 複合データ型

EMQXのHOCON設定で使われる複合型は、他の複合型やプリミティブ値を含むデータ構造を表現します。柔軟で階層的なデータ表現を可能にします。

#### Struct `Struct(name)`

フィールドを持つ構造体。波括弧`{}`で囲みます。`name`はスキーマで定義された構造体のフィールド名と型を指します。

#### Map `Map($name->Type)`

`Struct`に似ていますが、フィールド名が事前定義されていないキー・バリューの集合です。

`$name`は任意の文字列キー（`.`を含まない）を表し、`Type`はすべての値が同じ型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを取るユニオン型。あるフィールドが複数の型のうちどれか1つを取れることを示します。例：`String(infinity)`または`Duration`。

#### Array `Array(Type)`

指定型の要素からなる配列。

::: tip

Mapのフィールド名が正の整数の場合、`Array`の別表現として解釈されます。例えば：

```bash
myarray.1 = 74
myarray.2 = 75
```

は`myarray = [74, 75]`として扱われ、配列要素の上書きに便利です。

:::

### Variform式

Variformは文字列操作や実行時評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQX設定内に埋め込んで動的な文字列操作を行うための専門ツールです。

::: tip
Variform式は特定の設定項目にのみ適用されます。明示されていない限り使用しないでください。
:::

::: tip NULL値について

Variform式では、値バインディング参照や部分式の評価結果が未定義の場合、空文字列（`''`）として扱われます。

JSONで`null`がデコードされた場合も未定義値（`''`）として扱われ、文字列`"null"`とは異なります。

:::

#### 構文例

```js
function_call(clientid, another_function_call(username))
```

この式は`clientid`と`username`を組み合わせて新しい文字列を生成します。

Variformは以下のリテラルをサポートします。

- ブール値：`true`または`false`
- 整数：例`42`
- 浮動小数点数：例`3.14`
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲むASCII文字
- 配列：`[`と`]`で囲み、カンマ`,`で区切る
- 変数：事前定義された値への参照（例：`clientid`）
- 関数：事前定義された関数（例：`concat([...])`）

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
            # client IDの最初の'-'までのプレフィックスを抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.groupとして設定
            set_as_attr = group
        }
    ]
}
```

::: tip
アンエスケープ関数を使う場合、HOCON設定で三重引用符`"""`文字列を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行のclient IDの1行目を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を提供します。これらは抽出したデータの操作や整形に使えます。例：`lower()`、`upper()`、`concat()`、`hash()`、`hash_to_range()`など。

利用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../../develop/data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`：任意の中間値を文字列に変換
- **配列関数**：`nth/2`など（[詳細](../../develop/data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)）
- **乱数関数**：`rand_str`、`rand_int`
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_encode(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - `int2hexstr(Integer)`：整数を16進文字列に変換（例：15 → 'F'）
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：`Algorithm`は`md4`、`md5`、`sha`（`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`のいずれか
  - `hash_to_range(Input, Min, Max)`：`sha256`でハッシュし、`Min`〜`Max`の範囲の整数にマッピング（`Min <= X <= Max`）
  - `map_to_rage(Input, Min, Max)`：入力を`Min`〜`Max`の範囲の整数にマッピング
- **比較関数**：
  - `num_eq(A, B)`：2数値が等しいなら`true`、そうでなければ`false`
  - `num_neq(A, B)`：2数値が異なれば`true`、そうでなければ`false`
  - `num_gt(A, B)`：`A > B`なら`true`、そうでなければ`false`
  - `num_gte(A, B)`：`A >= B`なら`true`、そうでなければ`false`
  - `num_lt(A, B)`：`A < B`なら`true`、そうでなければ`false`
  - `num_lte(A, B)`：`A <= B`なら`true`、そうでなければ`false`
  - `str_eq(A, B)`：2文字列が等しいなら`true`、そうでなければ`false`
  - `str_neq(A, B)`：2文字列が異なれば`true`、そうでなければ`false`
  - `str_gt(A, B)`：辞書順で`A > B`なら`true`、そうでなければ`false`
  - `str_gte(A, B)`：辞書順で`A >= B`なら`true`、そうでなければ`false`
  - `str_lt(A, B)`：辞書順で`A < B`なら`true`、そうでなければ`false`
  - `str_lte(A, B)`：辞書順で`A <= B`なら`true`、そうでなければ`false`
  - `is_empty_var(V)`：変数が空か判定。Variformでの空は未定義（`undefined`）、JSONの`null`（文字列`"null"`は除く）、空文字列`""`
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`。文字列も受け付け、入力が文字列の場合は出力も文字列。

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。ただしOS環境変数は`EMQXVAR_`プレフィックス付きで読み込み、一度読み込んだ値は不変。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスで値を抽出。例：`username`がJSONオブジェクトなら`json_value(username, 'shop.floor')`でフィールド取得。
  - `jwt_value(Data, Path)`：JWTトークンのペイロードからクレーム値をドット区切りパスで抽出。例：`password`がカスタムクレームを持つJWTなら`jwt_value(password, 'client_attrs.unitid')`で取得。

#### 条件式

Variform式は包括的な制御構造を持ちませんが、以下の関数で基本的な条件制御が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、そうでなければ`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の空でない要素を返す。

#### エラー処理

Bashなどのスクリプト環境と同様に、Variform式は未束縛変数や実行時例外が発生した場合、空文字列（`""`）を返す設計です。

- 未束縛変数：定義されていない変数を参照すると空文字列となる。
- 実行時例外：関数の誤用や型不一致、配列インデックス範囲外などの例外発生時は空文字列を返す。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclient IDのプレフィックスを抽出。
- `strlen(username, 0, 5)`：usernameの部分文字列を抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclient IDから数字を抽出。空文字列なら`'000'`を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid`が`foo.`で始まれば`foo`、そうでなければ`bar`を返す。
