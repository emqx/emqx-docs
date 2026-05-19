# 設定ファイル

ユーザーは設定ファイルまたは環境変数を使用してEMQXを設定できます。本節では主にEMQXの設定ファイルについて紹介し、EMQXで最も一般的に使用される機能の基本的な設定方法を説明します。詳細な設定項目と説明については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定およびランタイムデータを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、ランタイムで生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ（`etc`）

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイメントやアップグレード時に変更され、ランタイム中は安定性を保つため読み取り専用となっています。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                          | パス               |
| ---------------------------------------- | ------------------ |
| RPMまたはDEBパッケージでインストール    | `/etc/emqx`        |
| Dockerコンテナで実行                     | `/opt/emqx/etc`    |
| ポータブル圧縮パッケージから展開        | `./etc`            |

### 動的設定ディレクトリ（`data/configs`）

ランタイム中、EMQXはダッシュボード、REST API、CLIを通じて動的な再設定を可能にします。これらのツールで行われた変更は永続化のため`data/configs`ディレクトリに保存されます。このディレクトリの場所もインストール方法に依存します。

| インストール方法                          | パス                     |
| ---------------------------------------- | ------------------------ |
| RPMまたはDEBパッケージでインストール    | `/var/lib/emqx/configs`  |
| Dockerコンテナで実行                     | `/opt/emqx/data/configs` |
| ポータブル圧縮パッケージから展開        | `./data/configs`         |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することでデータディレクトリを変更可能です。ただし、クラスター運用時は全ノードで同じディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、もし重複があった場合は事前定義された上書きルールに従い解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションで詳細なリファレンスを提供していますが、設定例はEMQXの設定理解と適用に役立ちます。

- RPMまたはDEBパッケージでインストールした場合、`etc/emqx/examples`ディレクトリに設定例があります。
- Dockerコンテナで実行している場合、`opt/emqx/etc/examples`ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれており、ランタイム中に上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、ダッシュボードUIからより複雑な設定に上書きすることが可能です。

ただし、`node`や`cluster`のような不変設定は`base.hocon`に設定することは**推奨されません**。詳細は[Immutable Configurations File](#immutable-configuration-file)をご参照ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、そのノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリ内の`cluster.hocon`ファイルはクラスター全体の設定項目を含みます。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新規ノードが追加された場合、ノードは自動的に他のノードから`cluster.hocon`をコピーして適用します。そのため、このファイルを手動で編集することは推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層については[Config Override Rules](#config-override-rules)をご覧ください。

EMQX 5.1以降、クラスター設定の変更時には`cluster.hocon`が上書きされる前にバックアップが作成されます。バックアップファイルはノードのローカルタイムスタンプ付きで最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`などの重要なシステム設定の主要な設定ファイルとして残っています。このファイルは`base.hocon`や`cluster.hocon`よりも優先度が高いですが、環境変数よりは低い優先度です。

設定の上書きについての詳細は[Config Override Rules](#config-override-rules)をご参照ください。

## 設定パス

EMQXでは設定値をドット区切りのパスで参照できます。これはツリー構造に似ており、ルート（常にStruct）から始まり、各セグメントはフィールド名またはMapのキーを示します。配列要素の場合は1始まりのインデックスを使用します。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0以降、設定ファイルフォーマットとして[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間が読みやすいデータ形式であり、JSONのスーパーセットです。継承や結合、クォートなどの機能により、設定作業がさらに簡素化されています。

**HOCON構文例：**

JSONに似たオブジェクト表現：

```bash
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

またはフラット化した表現：

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このフラット化形式は従来のEMQXバージョンと互換性がありますが、使い方が異なります。

HOCONでは文字列は両端にクォートを付けることが推奨されます。特殊文字を含まない文字列はクォートなしでも構いません（例：`foo`、`foo_bar`）。一方、フラット化形式は`=`の右側のすべてを値として扱います。

詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)をご参照ください。

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

1. 設定ファイルの`.`区切りは環境変数では使えないため、EMQXは`__`（ダブルアンダースコア）を区切り文字として使用します。
2. 他の環境変数と区別するため、環境変数名の先頭に`EMQX_`を付けます。
3. 環境変数の値はHOCONの値として解析されるため、複雑なデータ型の値も渡せます。ただし、`:`や`=`などの特殊文字はダブルクォート`"`で囲む必要があります。

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
- 上位の設定ファイルは下位の設定を置き換えます。

優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり、`base.hocon`の設定は最も優先度が低く、より優先度の高いファイルの設定で上書き可能です。`EMQX_`で始まる環境変数が最も優先されます。

::: tip
バージョン5.8.4以前は`base.hocon`ファイルが存在しませんでした。優先順位は同じですが`base.hocon`は含まれません。
:::

EMQXダッシュボードUI、HTTP API、CLIでの変更はランタイム中に`cluster.hocon`に永続化され即時反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、**`emqx.conf`と`cluster.hocon`間で設定の重複は避けてください。**

::: tip
1. 古いバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`ファイルが存在し、設定優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. 5.0.2/v5.0.22以前から最新バージョンにアップグレードする場合、優先順位は変わらず互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`機構はバージョン5.1で廃止されました。
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

## コンソールログの出力レベルをdebugに設定し、他の設定は維持
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

以下の3つの形式は同等です。

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

この特徴を利用し、配列の要素を簡単に上書きできます。

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

## 以下の設定では、1番目の要素の`enable`以外のフィールドがすべて失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。リスナーの`zone`フィールドにゾーン名を設定することで紐付けられます。ゾーンに紐づくリスナーに接続したMQTTクライアントは、そのゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐付けられています。`default`ゾーンは論理的なグループであり、設定ファイル上には存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続・セッション設定。特定ゾーンでMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：セッション永続化設定。特定ゾーンでMQTTセッションの永続ストレージを有効化など。

EMQXバージョン5のデフォルト設定ファイルにはゾーンは含まれていません。バージョン4では`internal`と`external`の2つのデフォルトゾーンが存在していました。

ゾーンを作成するには設定ファイルで定義します。例：

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

リスナーで`zone`フィールドを設定し、作成済みのゾーンに紐付けます。

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
ゾーン設定スキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

プリミティブデータ型は基本的に自明であり、最小限の説明で十分です。以下に代表的な型を示します。

#### Integer

整数値。例：`42`、`-3`、`0`

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、あらかじめ定義されたシンボルのいずれかのみを許容します。例：`Enum(debug,info,warning,error)`はログレベルを定義。

#### String

文字列型で、複数の形式をサポートします。

- **クォートなし**：特殊文字を含まない簡単な識別子や名前に適しています（詳細は下記参照）。
- **クォート付き文字列**：特殊文字や空白を含む場合はダブルクォート`"`で囲み、必要に応じてバックスラッシュ`\`でエスケープします。例：`"line1\nline2"`。
- **トリプルクォート文字列**：`"""`で囲み、エスケープ不要（`\`は例外）。複雑な内容を簡単に記述可能。トリプルクォート隣接のクォートはエスケープが必要。
- **インデント付きトリプルクォート文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを保持でき、複数行や整形テキストに適します。

**クォートなし文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、および空白
- `//`で始めない（コメントと誤認されるため）
- `true`、`false`、`null`で始めない（ブール値やnullと誤認されるため）

**トリプルクォート文字列のガイドライン：**

- トリプルクォート隣接のクォートを含める場合はエスケープまたは`~`区切りを使用
- 複数行文字列はスペース（タブ不可）によるインデントをサポート。インデントレベルは最小の先頭スペース数で決定

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

HOCONの文字列クォート規則の詳細は[HOCON仕様](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings)を参照してください。

EMQX独自のインデント付きトリプルクォート文字列の詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)をご覧ください。

#### String("constant")

定数文字列値。単一値の列挙型（Enum）として機能し、特定の設定やモードの固定値に使用されます。

#### Boolean

`true`または`false`（大文字小文字を区別）。

#### Float

浮動小数点数。例：`3.14`、`-0.001`

#### Duration

人間が読みやすい形式の時間間隔。フォーマットの例と説明。

#### Duration(s)

秒単位の精度を持つDuration型。詳細と例。

#### Secret

パスワードやトークンなど機密情報用の型。使用方法と重要性の説明。

### 複合データ型

EMQXのHOCON設定における複合データ型は、他の複合型やプリミティブ値を含むことができるデータ構造を表現します。柔軟で階層的なデータ表現を可能にします。

#### Struct `Struct(name)`

中括弧`{}`で囲まれたフィールドを持つ構造体。`name`はフィールドと型を定義するスキーマの参照名。

#### Map `Map($name->Type)`

Structに似ていますが、フィールド名が事前定義されていないキー・バリューの集合です。

`$name`は文字列（`.`を含まない）で任意のキーを示し、`Type`はすべての値が同じ型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを取るユニオン型。1つのフィールドが複数の型のいずれかを許容します。例：`String(infinity)`または`Duration`のいずれか。

#### Array `Array(Type)`

指定された`Type`の要素からなる配列。

::: tip

Mapのフィールド名が正の整数の場合、`Array`の別表現として解釈されます。例えば：

```bash
myarray.1 = 74
myarray.2 = 75
```

は`myarray = [74, 75]`と解釈され、配列要素の上書きに便利です。

:::

### Variform式

Variformは文字列操作やランタイム評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内で文字列操作を動的に行うための埋め込みツールです。

::: tip
Variform式は特定の設定項目にのみ適用されます。明示されていない限り使用しないでください。
:::

::: tip NULL値について
Variform式では値のバインディング参照や部分式の評価が未定義値となる場合があり、これは空文字列（`''`）で表現されます。

JSONで`null`がデコードされた場合も未定義値（`''`）として扱われ、文字列`"null"`とは異なります。
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
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲まれたASCII文字
- 配列：`[`と`]`で囲まれ、`,`で区切られた要素
- 変数：事前定義された値の参照（例：`clientid`）
- 関数：事前定義関数（例：`concat([...])`）

Variformは以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のアンエスケープは`unescape`関数を使用）

以下はVariform式を埋め込んだ設定例です。

```js
mqtt {
    client_attrs_init = [
        {
            # client IDの最初の'-'前の接頭辞を抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.groupとして設定
            set_as_attr = group
        }
    ]
}
```

::: tip
アンエスケープ関数を使う場合、HOCON設定でトリプルクォート`"""`文字列を使うと二重エスケープが不要で便利です。

例：

```
#### 複数行のclient IDの最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

EMQXはルールエンジンの文字列関数に類似した豊富な文字列、配列、乱数、ハッシュ関数を提供しています。これらは抽出データの操作やフォーマットに利用可能です。例えば`lower()`、`upper()`、`concat()`は文字列の整形に、`hash()`や`hash_to_range()`はハッシュ化や範囲マッピングに使えます。

利用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`は任意の中間非文字列値を文字列に変換
- **配列関数**：`nth/2`など（[詳細](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)）
- **乱数関数**：`rand_str`、`rand_int`
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_encode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - `int2hexstr(Integer)`：整数を16進文字列にエンコード（例：15は`F`（大文字））
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：アルゴリズムは`md4`、`md5`、`sha`（`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`のいずれか
  - `hash_to_range(Input, Min, Max)`：`sha256`でハッシュ化し、`Min`から`Max`までの整数にマッピング（`Min <= X <= Max`）
  - `map_to_rage(Input, Min, Max)`：入力を`Min`から`Max`までの整数にマッピング（`Min <= X <= Max`）
- **比較関数**：
  - `num_eq(A, B)`：数値が等しい場合`true`、それ以外は`false`
  - `num_neq(A, B)`：数値が等しくない場合`true`、それ以外は`false`
  - `num_gt(A, B)`：`A > B`なら`true`、それ以外は`false`
  - `num_gte(A, B)`：`A >= B`なら`true`、それ以外は`false`
  - `num_lt(A, B)`：`A < B`なら`true`、それ以外は`false`
  - `num_lte(A, B)`：`A <= B`なら`true`、それ以外は`false`
  - `str_eq(A, B)`：文字列が等しい場合`true`、それ以外は`false`
  - `str_neq(A, B)`：文字列が等しくない場合`true`、それ以外は`false`
  - `str_gt(A, B)`：辞書順で`A`が`B`より後なら`true`、それ以外は`false`
  - `str_gte(A, B)`：辞書順で`A >= B`なら`true`、それ以外は`false`
  - `str_lt(A, B)`：辞書順で`A < B`なら`true`、それ以外は`false`
  - `str_lte(A, B)`：辞書順で`A <= B`なら`true`、それ以外は`false`
  - `is_empty_var(V)`：変数が空か判定。Variformの空は未定義（`undefined`）、JSONの`null`（文字列`"null"`は除く）、空文字列`""`を含む
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`。文字列も受け入れ、入力が文字列なら出力も文字列

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。以下の制約あり：
    - OS環境変数を読み取る際に`EMQXVAR_`プレフィックスが付加される。例：`getenv('FOO_BAR')`は`EMQXVAR_FOO_BAR`を読み込む
    - OS環境変数から読み込んだ値は不変

#### 条件式

Variform式には包括的な制御フローはありませんが、以下の関数で基本的な値の選択が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、それ以外は`ElseExpression`を返す
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す
- `coalesce([Element1, Element2, ...])`：最初の空でない要素を返す

#### エラー処理

Bashなどのスクリプト環境と同様に、Variform式は未バインド変数や実行時例外発生時に空文字列（`""`）を返す設計です。

- 未バインド変数：定義されていない変数参照は空文字列として評価されます。
- 実行時例外：関数の誤用や無効なデータ型、配列インデックス範囲外などの例外は空文字列を返します。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclient IDの接頭辞を抽出
- `strlen(username, 0, 5)`：usernameの部分文字列を抽出
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclient IDから数字を抽出。空文字列なら`'000'`を返す
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid`が`foo.`で始まれば`foo`、そうでなければ`bar`を返す
