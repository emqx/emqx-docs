# 設定ファイル

ユーザーは設定ファイルまたは環境変数を使ってEMQXを設定できます。本節では主にEMQXの設定ファイルについて紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目と解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定およびランタイムデータを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、ランタイムで生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ（`etc`）

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイやアップグレード時に変更され、ランタイム中は安定性を保つために読み取り専用となっています。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                           | パス              |
| ------------------------------------------ | ----------------- |
| RPMまたはDEBパッケージでインストール       | `/etc/emqx`       |
| Dockerコンテナで実行                       | `/opt/emqx/etc`   |
| ポータブル圧縮パッケージから展開           | `./etc`           |

### 動的設定ディレクトリ（`data/configs`）

ランタイム中、EMQXはダッシュボード、REST API、CLIを通じて動的な再設定を許可しています。これらのツールで行われた変更は永続化のために`data/configs`ディレクトリに保存されます。このディレクトリの場所もインストール方法によって異なります。

| インストール方法                           | パス                      |
| ------------------------------------------ | ------------------------- |
| RPMまたはDEBパッケージでインストール       | `/var/lib/emqx/configs`   |
| Dockerコンテナで実行                       | `/opt/emqx/data/configs`  |
| ポータブル圧縮パッケージから展開           | `./data/configs`          |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することで、データディレクトリを変更可能です。ただし、クラスターを構成する場合は、すべてのノードで同じディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、重複した場合はあらかじめ定められた上書きルールで解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションでは詳細なリファレンスを提供していますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合、設定例は`etc/emqx/examples`ディレクトリにあります。
- DockerコンテナでEMQXを実行している場合、設定例は`opt/emqx/etc/examples`ディレクトリにあります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれており、ランタイムでより上位の設定ファイルによって上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、その後ダッシュボードUIからより複雑な設定で上書きすることができます。

`node`や`cluster`のような不変設定は`base.hocon`に設定することは**推奨されません**。詳細は[Immutable Configuration File](#immutable-configuration-file)をご参照ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、そのノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリ内の`cluster.hocon`ファイルにはクラスター全体の設定項目が含まれています。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、ノードは自動的に他のノードから`cluster.hocon`ファイルをコピーして適用します。このため、手動での編集は推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層については[Config Override Rules](#config-override-rules)をご覧ください。

EMQX 5.1以降、クラスター設定の変更時に`cluster.hocon`ファイルのバックアップが上書き前に作成されます。バックアップはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`設定など重要なシステム設定の主要な設定ファイルとして残っています。このファイルは`base.hocon`および`cluster.hocon`より優先度が高いですが、環境変数よりは低い優先度です。

設定の上書きについては[Config Override Rules](#config-override-rules)をご参照ください。

## 設定パス

EMQXでは設定値をツリー構造のようにドット区切りのパスで参照できます。ルートは常にStructであり、各セグメントはフィールド名またはMapのキーを指します。配列要素の場合は1始まりのインデックスを使用します。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0以降、設定ファイルフォーマットとして[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間に読みやすいデータフォーマットで、JSONのスーパーセットです。継承、結合、引用符などの機能により設定作業をさらに簡素化します。

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

またはフラット形式で：

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このイカのようなフラット形式は以前のEMQXバージョンとの互換性がありますが、使い方が異なります。

HOCONでは文字列の両端に引用符を付けることを推奨します。特殊文字を含まない文字列は引用符なしでもよく、例として`foo`や`foo_bar`があります。一方、イカ形式では`=`の右側のすべての文字を値として扱います。

HOCON構文の詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)をご参照ください。

## 環境変数

設定ファイルのほかに環境変数でもEMQXを設定可能です。

例えば、環境変数`EMQX_NODE__NAME=emqx2@127.0.0.1`は以下の設定を上書きします。

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数の変換ルールは以下の通りです。

1. 設定ファイルの`.`区切りは環境変数に使えないため、EMQXは区切りに`__`（ダブルアンダースコア）を使用します。
2. 他の環境変数と区別するため、環境変数には`EMQX_`プレフィックスが付加されます。
3. 環境変数の値はHOCONの値として解析されるため、複雑なデータ型も渡せます。ただし、`:`や`=`などの特殊文字はダブルクォート`"`で囲む必要があります。

変換例：

```bash
# 環境変数

## localhost:1883は構造体{"localhost": 1883}として解析されるため、ダブルクォートで囲む必要があります
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

## HOCON配列を文字列として直接渡す例
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

既知のルートパスに未知のフィールド名が設定された場合、起動時に`warning`ログを出力します。例えば`enable`を誤って`enabled`と設定すると、以下のように出力されます。

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 設定上書きルール

EMQXでは設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が前の値を上書きします。
- 上位の設定ファイルは下位の設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり、`base.hocon`の設定は最も優先度が低く、上位のファイルで上書き可能です。`EMQX_`で始まる環境変数が最も優先されます。

::: tip
バージョン5.8.4以前は`base.hocon`ファイルが存在しませんでした。優先順位は同じですが、`base.hocon`は含まれません。
:::

EMQXダッシュボードUI、HTTP API、CLIでの変更はランタイム中に`cluster.hocon`に永続化され、即時反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、**`emqx.conf`と`cluster.hocon`で設定を重複させないでください。**

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`ファイルが存在し、設定優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. 5.0.2/v5.0.22以前から最新バージョンにアップグレードする場合、優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`機構はバージョン5.1で削除されました。
:::

### 上書き例

以下の設定では、最後の行で定義された`level`の`debug`が以前の`error`を上書きしますが、`enable`フィールドは変更されません。

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

- リスト形式、例：`[1, 2, 3]`
- マップ形式（サブスクライブ用）、例：`{"1"=1, "2"=2, "3"=3}`

以下の3つの形式は同等です。

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

この特徴を利用して、配列の要素を簡単に上書きできます。

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

## 以下の方法では1番目の要素の`enable`以外のフィールドは失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。リスナーの`zone`フィールドにゾーン名を設定することで、ゾーンに関連付けられたリスナーに接続するMQTTクライアントはそのゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐づいています。`default`ゾーンは論理的なグループであり、設定ファイルには存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続およびセッション設定。特定ゾーンのMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：セッション永続化設定。特定ゾーンでMQTTセッションの永続ストレージを有効化など。

EMQXバージョン5のデフォルト設定ファイルにはゾーンは含まれていません。バージョン4では`internal`と`external`の2つのデフォルトゾーンがありました。

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
      # このゾーンのセッション永続化を有効化
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

HOCONオブジェクトの型安全性を確保するため、EMQXはスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などに利用されます。

[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)はこのスキーマから生成されています。

::: tip
ゾーンの設定スキーマは各グループで共通のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同一のスキーマです。
:::

### プリミティブデータ型

設定マニュアルのプリミティブ型はほぼ自明であり、最小限の説明で十分です。以下は代表的なプリミティブ型の一覧です。

#### Integer

整数値。例：`42`、`-3`、`0`。

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、あらかじめ定義されたシンボルのいずれかのみを取ります。例：`Enum(debug,info,warning,error)`はログレベルの指定に使われます。

#### String

文字列型で、複数の形式をサポートします。

- **非引用文字列**：特殊文字を含まない識別子や名前に適します（詳細は下記参照）。
- **引用文字列**：特殊文字や空白を含む場合はダブルクォート`"`で囲み、必要に応じてバックスラッシュ`\`でエスケープします。例：`"line1\nline2"`。
- **三重引用文字列**：`"""`で囲み、`\`以外のエスケープ不要で複雑な内容を簡単に記述可能です。三重引用符の隣接するクォートはエスケープが必要です。
- **インデント付き三重引用文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを保持でき、多行や整形テキストに適しています。

**非引用文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、空白などを含めない。
- `//`で始めない（コメントと誤認されるため）。
- `true`、`false`、`null`で始まらない（ブール値やnullと誤認されるため）。

**三重引用文字列のガイドライン：**

- 三重引用符の隣接するクォートを含める場合はエスケープまたは`~`区切りを使う。
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

EMQX独自のインデント付き三重引用文字列の詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)をご覧ください。

#### String("constant")

定数文字列で、単一値の列挙型（Enum）として機能します。特定の設定やモードの固定値に使われます。

#### Boolean

`true`または`false`（大文字小文字区別あり）。

#### Float

小数を含む浮動小数点数。例：`3.14`、`-0.001`。

#### Duration

人間に読みやすい時間の長さを表します。フォーマットの例と説明あり。

#### Duration(s)

秒単位の精度を持つDuration型。詳細と例あり。

#### Secret

パスワードやトークンなど機密情報用の型。用途と重要性の説明あり。

### 複合データ型

EMQXのHOCON設定における複合データ型は、他の複合型やプリミティブ値を含むデータ構造を表現可能です。階層的で柔軟なデータ表現を実現します。

#### Struct `Struct(name)`

波括弧`{}`で囲まれたフィールドを持つ構造体。`name`は構造体のフィールドと型を定義するスキーマの参照名です。

#### Map `Map($name->Type)`

Structに似ていますが、フィールド名が事前定義されていないキーと値のペアの集合です。

`$name`はドット`.`を含まない任意の文字列キーを表し、エンティティや属性名を示します。`Type`はマップ内のすべての値が同じ型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のうちいずれか1つを取るユニオン型。例えば、設定項目が`String(infinity)`または`Duration`のいずれかであることを示せます。

#### Array `Array(Type)`

指定型の要素からなる配列。

::: tip

Mapのフィールド名が正の整数の場合、配列の別表現として解釈されます。例：

```bash
myarray.1 = 74
myarray.2 = 75
```

は`myarray = [74, 75]`と解釈され、配列要素の上書きに便利です。

:::

### Variform式

Variformは文字列操作やランタイム評価に特化した軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内に埋め込んで動的な文字列操作を行うためのツールです。

::: tip
Variform式は特定の設定項目にのみ適用されます。指定がない限り使用しないでください。
:::

::: tip NULL値について
Variform式では値のバインディング参照や部分式の評価結果が未定義の場合、空文字列(`''`)として扱われます。

JSONデコードしたフィールドが`null`の場合は未定義値（空文字列）として扱い、文字列`"null"`とは区別されます。
:::

#### 構文例

```js
function_call(clientid, another_function_call(username))
```

この式は`clientid`と`username`を組み合わせて新しい文字列値を生成します。

Variformは以下のリテラルをサポートします。

- ブール値：`true`または`false`
- 整数：例`42`
- 浮動小数点数：例`3.14`
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲まれたASCII文字列
- 配列：`[`と`]`で囲まれ、カンマ`,`区切りの要素
- 変数：事前定義された値の参照例`clientid`
- 関数：事前定義された関数例`concat([...])`

Variformは以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のエスケープは`unescape`関数を呼び出してください）

Variform式を含む設定例：

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
式内で`unescape`関数を使う場合、HOCON設定で三重引用符`"""`文字列を使うと二重エスケープが不要で便利です。

例：

```
#### 複数行のclient IDの1行目を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を備えています。これらは抽出データの操作や整形に利用できます。例えば`lower()`、`upper()`、`concat()`は文字列のフォーマット調整に、`hash()`や`hash_to_range()`はハッシュ化や範囲マッピングに使います。

利用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`は任意の中間値を文字列に変換します。
- **配列関数**：`nth/2`など
- **乱数関数**：`rand_str`、`rand_int`
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_decode(Data, 'no_padding')](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)（6.0.2以降）
  - [base64_decode(Data, 'no_padding', 'urlsafe')](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)（6.0.2以降）
  - [base64_encode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - [base64_encode(Data, 'no_padding')](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)（6.0.2以降）
  - [base64_encode(Data, 'no_padding', 'urlsafe')](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)（6.0.2以降）
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスで値を抽出。例：`username`がJSONオブジェクトの場合、`json_value(username, 'shop.floor')`でフィールドにアクセス可能（6.0.2以降）。
  - `jwt_value(Data, Path)`：JWTトークンのペイロードをデコードし、ドット区切りパスでクレーム値を抽出。例：`password`がJWTでカスタムクレームを含む場合、`jwt_value(password, 'client_attrs.unitid')`でネスト値にアクセス可能（6.0.2以降）。
  - `int2hexstr(Integer)`：整数を16進文字列に変換。例：15は`'F'`（大文字）。
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：アルゴリズムは`md4`、`md5`、`sha`（`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`から選択可能。
  - `hash_to_range(Input, Min, Max)`：`sha256`でハッシュ化し、`Min`から`Max`までの整数にマッピング（`Min <= X <= Max`）。
  - `map_to_rage(Input, Min, Max)`：入力を`Min`から`Max`までの整数にマッピング（`Min <= X <= Max`）。
- **比較関数**：
  - `num_eq(A, B)`：数値が等しい場合`true`、それ以外は`false`。
  - `num_neq(A, B)`：数値が等しくない場合`true`、それ以外は`false`。
  - `num_gt(A, B)`：`A > B`なら`true`、それ以外は`false`。
  - `num_gte(A, B)`：`A >= B`なら`true`、それ以外は`false`。
  - `num_lt(A, B)`：`A < B`なら`true`、それ以外は`false`。
  - `num_lte(A, B)`：`A <= B`なら`true`、それ以外は`false`。
  - `str_eq(A, B)`：文字列が等しい場合`true`、それ以外は`false`。
  - `str_neq(A, B)`：文字列が等しくない場合`true`、それ以外は`false`。
  - `str_gt(A, B)`：辞書順で`A`が`B`より後なら`true`、それ以外は`false`。
  - `str_gte(A, B)`：辞書順で`A`が`B`より前でないなら`true`、それ以外は`false`。
  - `str_lt(A, B)`：辞書順で`A`が`B`より前なら`true`、それ以外は`false`。
  - `str_lte(A, B)`：辞書順で`A`が`B`より後でないなら`true`、それ以外は`false`。
  - `is_empty_var(V)`：変数が空か判定。Variformの空は値が存在しない（未定義）、JSONの`null`（文字列`"null"`は除く）、空文字列`""`を指す。
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`を返す。文字列も受け入れ、入力が文字列の場合は出力も文字列。

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。以下の制約あり：
    - OS環境変数を読む際に`EMQXVAR_`プレフィックスが付加される。例：`getenv('FOO_BAR')`は`EMQXVAR_FOO_BAR`を読む。
    - OS環境変数から読み込んだ値は不変。

#### 条件式

Variform式は包括的な制御フローを持ちませんが、基本的な値選択に役立つ関数があります。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、そうでなければ`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の空でない配列要素を返す。

#### エラー処理

Bashなどのスクリプト環境のデフォルト動作と同様に、Variform式は未束縛変数や実行時例外が発生した場合に空文字列（`""`）を返します。

- 未束縛変数：定義されていない変数を参照すると空文字列になる。
- 実行時例外：関数の誤用や型不一致などの例外発生時も空文字列を返す。例：配列インデックス範囲外。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclientidのプレフィックスを抽出。
- `strlen(username, 0, 5)`：usernameの一部を抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclientidから数字を抽出。空文字列なら`'000'`を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：clientidが`foo.`で始まれば`foo`、そうでなければ`bar`を返す。
