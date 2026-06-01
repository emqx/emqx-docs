# 設定ファイル

ユーザーは設定ファイルまたは環境変数でEMQXを設定できます。本節では主にEMQXの設定ファイルを紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目や解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定およびランタイムデータを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ (`etc`)**：読み取り専用で、不変または静的な設定ファイルが格納されます。
- **動的設定ディレクトリ (`data/configs`)**：書き込み可能で、ランタイム中に生成または動的に更新される設定ファイルが格納されます。

### 静的設定ディレクトリ (`etc`)

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されています。これらのファイルは通常、デプロイ時やアップグレード時に変更され、ランタイム中は安定性を保つために読み取り専用となっています。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                          | パス              |
| ---------------------------------------- | ----------------- |
| RPMまたはDEBパッケージでインストール    | `/etc/emqx`       |
| Dockerコンテナで実行                     | `/opt/emqx/etc`   |
| ポータブル圧縮パッケージから展開        | `./etc`           |

### 動的設定ディレクトリ (`data/configs`)

ランタイム中、EMQXはダッシュボード、REST API、CLIを通じて動的に再設定が可能です。これらのツールで行われた変更は`data/configs`ディレクトリに保存され、セッションをまたいで永続化されます。このディレクトリの場所もインストール方法によって異なります。

| インストール方法                          | パス                      |
| ---------------------------------------- | ------------------------- |
| RPMまたはDEBパッケージでインストール    | `/var/lib/emqx/configs`   |
| Dockerコンテナで実行                     | `/opt/emqx/data/configs`  |
| ポータブル圧縮パッケージから展開        | `./data/configs`          |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することで、データディレクトリの場所を変更可能です。ただし、クラスターを構成する場合は、すべてのノードで同じディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、もし重複があった場合は事前に定められた上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションは詳細なリファレンスを提供しますが、設定例はEMQXの設定を理解し適用するのに役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合は、`etc/emqx/examples`ディレクトリに設定例があります。
- DockerコンテナでEMQXを実行している場合は、`opt/emqx/etc/examples`ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルがあります。このファイルにはデフォルト設定が含まれており、ランタイム中により上位の設定ファイルで上書き可能です。

たとえば、基本的な認証設定でデプロイを開始し、後からダッシュボードUIでより複雑な設定に上書きすることができます。

`node`や`cluster`のような不変設定は`base.hocon`に設定することは**推奨されません**。詳細は[Immutable Configurations File](#immutable-configuration-file)をご覧ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、そのノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリ内の`cluster.hocon`ファイルにはクラスター全体の設定項目が含まれています。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、そのノードは自動的に他のノードから`cluster.hocon`をコピーして適用します。そのため、このファイルを手動で編集することは推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層については[Config Override Rules](#config-override-rules)をご参照ください。

EMQX 5.1以降、クラスター設定の変更時に`cluster.hocon`のバックアップが作成されます。バックアップファイルはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`などの重要なシステム設定の主要な設定ファイルとして残っています。このファイルは`base.hocon`や`cluster.hocon`より優先度が高いですが、環境変数よりは低い優先度です。

設定の上書きについては[Config Override Rules](#config-override-rules)を参照してください。

## 設定パス

EMQXでは設定値をドット区切りのパスで参照できます。これはツリー構造のようなもので、ルート（常にStruct）から始まり、各セグメントはフィールド名またはMapのキーを指します。配列要素の場合は1始まりのインデックスを使用します。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0以降、設定ファイルフォーマットとして[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間に読みやすいデータ形式で、JSONのスーパーセットです。継承や結合、クォートなどの機能により設定作業をより簡単にします。

**HOCONの構文例：**

JSON風のオブジェクト表現：

```bash
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

またはフラットな形式：

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このフラット形式は以前のEMQXバージョンとの後方互換性がありますが、使い方が異なります。

HOCONでは文字列は両端にクォートを付けることが推奨されます。特殊文字を含まない文字列はクォートなしでも構いません（例：`foo`、`foo_bar`）。一方、フラット形式では`=`の右側はすべて値として扱われます。

HOCON構文の詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)をご参照ください。

## 環境変数

設定ファイルのほかに、環境変数でもEMQXを設定できます。

例として、環境変数`EMQX_NODE__NAME=emqx2@127.0.0.1`は以下の設定を上書きします。

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数の変換ルールは以下の通りです。

1. 設定ファイルの`.`区切りは環境変数では使えないため、EMQXは`__`（ダブルアンダースコア）を区切り文字として使用します。
2. 他の環境変数と区別するため、`EMQX_`というプレフィックスを付けます。
3. 環境変数の値はHOCONの値として解析されるため、複雑なデータ型も渡せます。ただし、`:`や`=`などの特殊文字はダブルクォート`"`で囲む必要があります。

変換例：

```bash
# 環境変数

## localhost:1883は構造体{"localhost": 1883}として解析されるため、ダブルクォートで囲む必要があります
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

## HOCONの配列を文字列として直接渡す
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

::: tip

EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。`UNKNOWN_ROOT`は事前定義されたルートパスではないためです。

既知のルートパスに未知のフィールド名が設定された場合、起動時に`warning`ログを出力します。例えば、`enable`を誤って`enabled`と設定すると以下のように出力されます。

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 設定上書きルール

EMQXでは設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が前の値を上書きします。
- 上位の設定ファイルが下位の設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり、`base.hocon`の設定は最も優先度が低く、上位の設定ファイルで上書き可能です。`EMQX_`で始まる環境変数が最も優先度が高いです。

::: tip
5.8.4以前のバージョンでは`base.hocon`ファイルは存在しません。優先順位は同じですが、`base.hocon`はありません。
:::

EMQXダッシュボードUI、HTTP API、CLIからの変更はランタイム中に`cluster.hocon`に永続化され、即座に反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、`emqx.conf`と`cluster.hocon`間で設定を重複させることは**避けてください**。

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`ファイルが存在し、設定優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. これらの古いバージョンから最新バージョンにアップグレードする場合、優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`の仕組みは5.1で廃止されました。
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

パケットサイズ制限は最初に1MBに設定され、その後10MBに上書きされます。

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

以下の3つの形式は同等です。

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

この特徴により、配列の要素を簡単に上書きできます。例：

```bash
authentication  = [
  {
    enable = true,
    backend = "built_in_database",
    mechanism = "password_based"
  }
]

# 1番目の要素の`enable`フィールドを上書き
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

## 以下の設定では、1番目の要素の`enable`以外のフィールドは失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。ゾーンはリスナーの`zone`フィールドに設定することで関連付けられます。ゾーンに関連付けられたリスナーに接続するMQTTクライアントは、そのゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトでは、リスナーは`default`という名前のゾーンにリンクされています。`default`ゾーンは論理的なグループであり、設定ファイルには存在しません。
:::

ゾーンレベルで上書き可能な設定項目：

- `mqtt`：MQTT接続やセッション設定。特定ゾーンでより大きな最大パケットサイズを許可するなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：MQTTセッションの永続化設定。特定ゾーンで永続化を有効にするなど。

EMQX 5系のデフォルト設定ファイルにはゾーンは含まれていません。これはEMQX 4系の`internal`と`external`という2つのデフォルトゾーンとは異なります。

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

リスナーの`zone`フィールドに作成済みのゾーン名を設定して関連付けます。

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
ゾーン設定のスキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば、`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルに登場するプリミティブ型はほぼ自明であり、詳細な説明は不要です。以下に主な型を列挙します。

#### Integer

整数値。例：`42`、`-3`、`0`。

#### Integer(Min..Max)

指定された範囲内の整数。例：`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、事前定義されたシンボルのいずれかのみを取ります。例：`Enum(debug,info,warning,error)`はログレベルの指定に使われます。

#### String

文字列型で、複数の形式をサポートします。

- **クォートなし**：特殊文字を含まない単純な識別子や名前に適します（詳細は下記参照）。
- **クォート付き文字列**：特殊文字や空白を含む場合はダブルクォート`"`で囲み、必要に応じてバックスラッシュ`\`でエスケープします。例：`"line1\nline2"`。
- **トリプルクォート文字列**：`"""`で囲み、エスケープ不要（`\`を除く）で複雑な内容を含められます。トリプルクォートに隣接するクォートはエスケープが必要です。
- **インデント付きトリプルクォート文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。複数行や整形テキストのインデントを許容します。

**クォートなし文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、空白
- `//`で始めない（コメントと誤認されるため）
- `true`、`false`、`null`で始めない（ブール値やnullと誤認されるため）

**トリプルクォート文字列のガイドライン：**

- クォート文字をトリプルクォートに隣接させる場合はエスケープするか`~`区切りを使う。
- 複数行文字列はスペースによるインデントをサポート（タブ不可）。
- インデントレベルは最小の先頭スペース数で決定。

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

HOCONの文字列クォート規約の詳細は[HOCON仕様](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings)を参照してください。

EMQX独自のインデント付きトリプルクォートの詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)をご覧ください。

#### String("constant")

定数文字列で、単一値の列挙型（`Enum`）のように振る舞います。特定の固定値やモードを定義する際に使います。

#### Boolean

`true`または`false`（大文字小文字を区別）。

#### Float

小数を含む浮動小数点数。例：`3.14`、`-0.001`。

#### Duration

人間に読みやすい時間の長さを表します。フォーマットの例と説明があります。

#### Duration(s)

秒単位の精度を持つ`Duration`型。詳細と例があります。

#### Secret

パスワードやトークンなどの機密情報用の型。使用方法と重要性の説明があります。

### 複合データ型

EMQXのHOCON設定で複合データ型は、他の複合型やプリミティブ型を含むデータ構造を表現します。階層的で柔軟なデータ表現を可能にします。

#### Struct `Struct(name)`

波括弧`{}`で囲まれたフィールドを持つ構造体。`name`はスキーマ参照で、構造体のフィールド名と型を指定します。

#### Map `Map($name->Type)`

`Struct`に似ていますが、フィールド名が事前定義されていないキーと値のペアの集合です。

`$name`はドット`.`を含まない任意の文字列キーを表し、`Type`はすべての値が同じ型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のうちいずれか1つを取るユニオン型。あるフィールドが複数の型のいずれかを許容する場合に使います。例：`String(infinity)`または`Duration`。

#### Array `Array(Type)`

指定された型の要素からなる配列。

::: tip

Mapのフィールド名が正の整数の場合、`Array`の別表現として解釈されます。例：

```bash
myarray.1 = 74
myarray.2 = 75
```

は`myarray = [74, 75]`と解釈され、配列要素の上書きに便利です。

:::

### Variform式

Variformは文字列操作やランタイム評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内で文字列操作を動的に行うために埋め込まれます。

::: tip
Variform式は特定の設定項目でのみ使用可能です。明示されていない限り使用しないでください。
:::

::: tip NULL値について
Variform式では、値バインディング参照や部分式の評価結果が未定義の場合、空文字列(`''`)として扱われます。

JSONで`null`のフィールドは未定義値（空文字列）として扱われ、文字列`"null"`とは異なります。
:::

#### 構文

例：

```js
function_call(clientid, another_function_call(username))
```

これは`clientid`と`username`を組み合わせて新しい文字列を生成します。

Variformがサポートするリテラル：

- ブール値：`true`または`false`
- 整数：例`42`
- 浮動小数点数：例`3.14`
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲むASCII文字
- 配列：`[`と`]`で囲み、カンマ`,`で区切る
- 変数：事前定義された値への参照（例：`clientid`）
- 関数：事前定義関数（例：`concat([...])`）

Variformは以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のアンエスケープには`unescape`関数を使用）

Variform式を埋め込んだ設定例：

```js
mqtt {
    client_attrs_init = [
        {
            # clientidの最初の'-'までの接頭辞を抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.groupとして設定
            set_as_attr = group
        }
    ]
}
```

::: tip
アンエスケープ関数が必要な場合、HOCON設定でトリプルクォート`"""`文字列を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行のclient IDの最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を提供しています。抽出データの操作や整形に利用可能です。

以下の関数が使用可能です。

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`：任意の中間値を文字列に変換
- **配列関数**：[nth/2](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
- **乱数関数**：`rand_str`、`rand_int`
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_encode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - `int2hexstr(Integer)`：整数を16進文字列に変換（例：15 → 'F'（大文字））
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：Algorithmは`md4`、`md5`、`sha`（`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`のいずれか
  - `hash_to_range(Input, Min, Max)`：`sha256`でハッシュし、`Min`から`Max`までの整数にマップ（`Min <= X <= Max`）
  - `map_to_rage(Input, Min, Max)`：入力を`Min`から`Max`までの整数にマップ（`Min <= X <= Max`）
- **比較関数**：
  - `num_eq(A, B)`：数値が同じなら`true`、そうでなければ`false`
  - `num_neq(A, B)`：数値が異なれば`true`、そうでなければ`false`
  - `num_gt(A, B)`：`A > B`なら`true`、そうでなければ`false`
  - `num_gte(A, B)`：`A >= B`なら`true`、そうでなければ`false`
  - `num_lt(A, B)`：`A < B`なら`true`、そうでなければ`false`
  - `num_lte(A, B)`：`A <= B`なら`true`、そうでなければ`false`
  - `str_eq(A, B)`：文字列が同じなら`true`、そうでなければ`false`
  - `str_neq(A, B)`：文字列が異なれば`true`、そうでなければ`false`
  - `str_gt(A, B)`：辞書順で`A`が`B`より後なら`true`、そうでなければ`false`
  - `str_gte(A, B)`：辞書順で`A`が`B`より前でないなら`true`、そうでなければ`false`
  - `str_lt(A, B)`：辞書順で`A`が`B`より前なら`true`、そうでなければ`false`
  - `str_lte(A, B)`：辞書順で`A`が`B`より後でないなら`true`、そうでなければ`false`
  - `is_empty_var(V)`：変数が空か判定。Variformの空は未定義（`undefined`）、JSONの`null`（文字列`"null"`は除く）、空文字列`""`を含む
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`。文字列も受け付け、入力が文字列なら出力も文字列

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。ただしOS環境変数は`EMQXVAR_`プレフィックスを付けて読み込む。例：`getenv('FOO_BAR')`は`EMQXVAR_FOO_BAR`を読み込む。読み込み後は値は不変。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスで値を抽出。例：`username`がJSONオブジェクトなら`json_value(username, 'shop.floor')`でフィールドにアクセス。
  - `jwt_value(Data, Path)`：JWTトークンのペイロードからドット区切りパスでクレーム値を抽出。例：`password`がカスタムクレームを持つJWTなら`jwt_value(password, 'client_attrs.unitid')`でネスト値にアクセス。

#### 条件式

Variform式は包括的な制御フローを持ちませんが、以下の関数で基本的な条件分岐が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、そうでなければ`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の空でない要素を返す。

#### エラー処理

Bashなどのスクリプト環境と同様に、Variform式は未束縛変数や実行時例外が発生した場合、空文字列`""`を返す設計です。

- 未束縛変数：定義されていない変数参照は空文字列として評価されます。
- 実行時例外：関数の誤用や型不整合などの例外は空文字列を返します。例：配列インデックスの範囲外。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclientidの接頭辞を抽出。
- `strlen(username, 0, 5)`：usernameの部分文字列を抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclientidから数字を抽出。空文字列なら`'000'`を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid`が`foo.`で始まれば`foo`、そうでなければ`bar`を返す。
