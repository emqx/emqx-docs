# 設定ファイル

ユーザーは設定ファイルまたは環境変数を使ってEMQXを設定できます。本節では主にEMQXの設定ファイルについて紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目や解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定や実行時データを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、実行時に生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ（`etc`）

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイ時やアップグレード時に変更され、実行時は安定性を保つため読み取り専用となります。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                          | パス              |
| ---------------------------------------- | ----------------- |
| RPMまたはDEBパッケージでインストール    | `/etc/emqx`       |
| Dockerコンテナで実行                     | `/opt/emqx/etc`   |
| ポータブル圧縮パッケージから展開        | `./etc`           |

### 動的設定ディレクトリ（`data/configs`）

実行時には、ダッシュボード、REST API、CLIを通じて動的な再設定が可能です。これらのツールで行われた変更は`data/configs`ディレクトリに保存され、セッションを跨いで永続化されます。このディレクトリの場所もインストール方法によって異なります。

| インストール方法                          | パス                     |
| ---------------------------------------- | ------------------------ |
| RPMまたはDEBパッケージでインストール    | `/var/lib/emqx/configs`  |
| Dockerコンテナで実行                     | `/opt/emqx/data/configs` |
| ポータブル圧縮パッケージから展開        | `./data/configs`         |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することで、データディレクトリの場所を変更可能です。ただし、クラスター運用時はすべてのノードで同じディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、もし重複があった場合は事前定義された上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションで詳細なリファレンスを提供していますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでインストールした場合は、`etc/emqx/examples`ディレクトリに設定例があります。
- Dockerコンテナで実行している場合は、`opt/emqx/etc/examples`ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれており、実行時により上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、ダッシュボードUIからより複雑な設定に上書きすることができます。

`node`や`cluster`のような不変設定は`base.hocon`に設定することは**推奨されません**。詳細は[Immutable Configurations File](#immutable-configuration-file)をご参照ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、そのノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリ内の`cluster.hocon`ファイルにはクラスター全体の設定項目が含まれています。ダッシュボード、REST API、CLIから行われた設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、そのノードは自動的に他のノードから`cluster.hocon`ファイルをコピーして適用します。このため、手動での編集は推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層については[Config Override Rules](#config-override-rules)をご覧ください。

EMQX 5.1以降、クラスター設定の変更時に`cluster.hocon`ファイルのバックアップが作成されます。バックアップはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`設定を含む重要なシステム設定の主要な設定ファイルとして残っています。このファイルは`base.hocon`や`cluster.hocon`よりも優先度が高いですが、環境変数よりは低い優先度です。

設定の上書き詳細については[Config Override Rules](#config-override-rules)をご参照ください。

## 設定パス

EMQXでは設定値をドット区切りのパスで参照できます。これはツリー構造のようなもので、ルート（常にStruct）から始まり、各セグメントがフィールド名またはMapのキーを指します。配列要素の場合は1始まりのインデックスを使います。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0以降、設定ファイルフォーマットとして[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間に読みやすいデータフォーマットでJSONのスーパーセットです。継承や結合、クォートなどの機能により設定作業をさらに簡素化します。

**HOCON構文例：**

JSONに似たオブジェクト表現が可能です。

```bash
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

またはフラットな形式でも記述可能です。

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このフラット形式は過去のEMQXバージョンとの互換性を保ちつつ、使い方が異なります。

HOCONでは文字列の両端にクォートを付けることを推奨しています。特殊文字を含まない文字列はクォートなしでも構いません（例：`foo`、`foo_bar`）。一方、フラット形式は`=`の右側のすべてを値として扱います。

HOCON構文の詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)をご参照ください。

## 環境変数

設定ファイルのほかに、環境変数を使ってEMQXを設定することもできます。

例えば、環境変数`EMQX_NODE__NAME=emqx2@127.0.0.1`は以下の設定を上書きします。

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数の変換ルールは以下の通りです。

1. 設定ファイルの`.`区切りは環境変数で使えないため、EMQXでは`__`（アンダースコア2つ）を区切り文字として使います。
2. 他の環境変数と区別するため、環境変数名の先頭に`EMQX_`を付けます。
3. 環境変数の値はHOCON値として解析されるため、複雑なデータ型も渡せます。`:`、`=`、`#`などのHOCON特殊文字を含む値は、ダブルクォート`"`で囲む必要があります。特に`#`はHOCONの行コメントの開始文字なので、クォートなしだと`#`以降が無視されます。

変換例：

```bash
# 環境変数

## localhost:1883 は構造体 {"localhost": 1883} と解釈されるため、ダブルクォートで囲む必要があります
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

::: warning `#`、`:`、`=`を含む値について

パスワードなどに`#`を含む場合、`#`はHOCONの行コメントとして扱われるため、以下のようにするとパスワードが途中で切れてしまいます。

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

この場合、パスワードは`MQtt`と解釈され、`#123`はコメントとして無視されます。文字列として正しく渡すには、**HOCONレベルのダブルクォート**（シェルのクォートとは別）で囲む必要があります。

```bash
# 正しい例 — HOCONパーサーが受け取る値は "MQtt#123"（クォート込み）
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# 同じ効果、シェル用に内側のクォートをエスケープ
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

`:`や`=`を含む値も同様です。URLエンコード（例：`%23`）は使えません。EMQXは環境変数の値をURLデコードしません。

:::

::: tip なぜ一部のクォートなし値は通り、一部は通らないのか

EMQXは環境変数の値を`fake_key=<値>`という形式にラップしてHOCONとして解析します。解析成功すればその値を使い、失敗すれば文字列としてそのまま扱います。

そのため、`EMQX_..._PASSWORD="abc#def"`は`abc`（`#def`はコメント）となり、`EMQX_..._PASSWORD=".abc#def"`はHOCON構文エラーで文字列のまま扱われます。HOCONクォートで囲むと動作が明確になります。

:::

::: tip

EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。

既知のルートパスに対して未知のフィールド名が設定された場合、起動時に`warning`ログを出力します。例えば`enable`を誤って`enabled`と設定した場合、以下のように出力されます。

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 設定の上書きルール

EMQXでは設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が前の値を上書きします。
- 上位の設定ファイルが下位の設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり`base.hocon`の設定は最も優先度が低く、`EMQX_`で始まる環境変数が最も優先されます。

::: tip
5.8.4以前のバージョンには`base.hocon`ファイルは存在しませんでした。優先順位は同じですが、`base.hocon`は含まれません。
:::

EMQXダッシュボードUI、HTTP API、CLI経由での変更は実行時に`cluster.hocon`に永続化され、即時反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が戻ることがあります。

混乱を避けるため、`emqx.conf`と`cluster.hocon`で設定を重複させることは**避けてください**。

::: tip
1. 古いバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`ファイルが存在し、優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. これらの古いバージョンから最新バージョンにアップグレードする場合、優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`機構は5.1で廃止されました。
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
- マップ形式（サブスクライブ用）（例：`{"1"=1, "2"=2, "3"=3}`）

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

リスト形式の配列は完全に上書きされ、元の値を保持できません。例えば：

```bash
authentication = [
  {
    enable = true
    backend = "built_in_database"
    mechanism="password_based"
  }
]

## 以下の方法では1番目の要素の`enable`以外のフィールドがすべて失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。ゾーンはリスナーの`zone`フィールドに設定することで関連付けられ、そのゾーンに紐づくリスナーに接続したMQTTクライアントはゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐づいています。`default`ゾーンは論理的なグループであり、設定ファイルには存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続やセッション設定。特定ゾーンのMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検知。
- `durable_sessions`：MQTTセッションの永続化設定。特定ゾーンで永続化を有効にするなど。

EMQX 5系のデフォルト設定ファイルにはゾーンは含まれていません。これはEMQX 4系の`internal`と`external`の2つのデフォルトゾーンとは異なります。

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
ゾーン設定のスキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルに登場するプリミティブデータ型はほぼ自明で、最小限の説明で十分です。以下に主要な型を示します。

#### Integer

整数値。例：`42`、`-3`、`0`

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義済みのシンボルのいずれかのみ許容されます。例：`Enum(debug,info,warning,error)`はログレベルを定義。

#### String

文字列型で、複数の表現形式があります。

- **クォートなし**：特殊文字を含まない単純な識別子や名前に適します（詳細は下記参照）。
- **ダブルクォート文字列**：特殊文字や空白を含む場合に`"`で囲み、必要に応じて`\`でエスケープします。例：`"line1\nline2"`。
- **トリプルクォート文字列**：`"""`で囲み、`\`以外のエスケープ不要で複雑な内容を簡単に記述可能。トリプルクォートに隣接するクォートはエスケープが必要です。
- **インデント付きトリプルクォート文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを維持可能で、複数行や整形テキストに適します。

**クォートなし文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、および空白。
- `//`で始めない（コメント開始のため）。
- `true`、`false`、`null`で始めない（ブール値やnullと誤解されるため）。

**トリプルクォート文字列のガイドライン：**

- トリプルクォートに隣接するクォートはエスケープするか、`~`区切りを使う。
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

HOCONの文字列クォート規則の詳細は[HOCON仕様](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings)を参照してください。

EMQX独自のインデント付きトリプルクォート文字列の詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)をご覧ください。

#### String("constant")

定数文字列値で、単一値の列挙型（`Enum`）のように振る舞います。特定の設定やモードの静的値に利用。

#### Boolean

`true`または`false`（大文字小文字区別あり）。

#### Float

浮動小数点数。例：`3.14`、`-0.001`

#### Duration

人間に読みやすい時間の長さを表します。フォーマットの例と説明。

#### Duration(s)

秒単位の精度を持つ`Duration`型。詳細と例。

#### Secret

パスワードやトークンなど機密情報用の型。使用方法と重要性の説明。

### 複合データ型

EMQXのHOCON設定における複合データ型は、他の複合型やプリミティブ値を含むデータ構造を表現し、柔軟かつ階層的なデータ表現を可能にします。

#### Struct `Struct(name)`

波括弧`{}`で囲まれたフィールドを持つ構造体。`name`は構造体のフィールド名や型を定義するスキーマの参照。

#### Map `Map($name->Type)`

`Struct`に似ていますが、フィールド名が事前定義されていないキーと値のペアの集合です。

`$name`は任意の文字列（`.`を含まない）で、エンティティや属性名を表します。`Type`はすべての値が同じ型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを取るユニオン型。ある構造体フィールドが複数の型のどれかを許容する場合に使います。例：`String(infinity)`または`Duration`。

#### Array `Array(Type)`

指定された`Type`の要素からなる配列。

::: tip

Mapのフィールド名が正の整数の場合、`Array`の別表現として解釈されます。例：

```bash
myarray.1 = 74
myarray.2 = 75
```

は`myarray = [74, 75]`と解釈され、配列要素の上書きに便利です。

:::

### Variform式

Variformは文字列操作や実行時評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内に埋め込んで文字列操作を動的に行うための専門ツールです。

::: tip
Variform式は特定の設定項目でのみ使用可能です。明示されていない限り使用しないでください。
:::

::: tip NULL値について

Variform式では、値バインディング参照や部分式の評価結果が未定義の場合、空文字列（`''`）として扱われます。

JSONで`null`のフィールドは未定義値（`''`）として扱われ、文字列`"null"`とは異なります。

:::

#### 構文

例：

```js
function_call(clientid, another_function_call(username))
```

これは`clientid`と`username`を組み合わせて新しい文字列を生成します。

Variformは以下のリテラルをサポートします。

- ブール値：`true`または`false`
- 整数：例`42`
- 浮動小数点数：例`3.14`
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲まれたASCII文字
- 配列：`[`と`]`で囲み、カンマ`,`で区切った要素
- 変数：事前定義された値への参照（例：`clientid`）
- 関数：事前定義された関数（例：`concat([...])`）

Variformは以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のエスケープは`unescape`関数を使用）

Variform式を埋め込んだ設定例：

```js
mqtt {
    client_attrs_init = [
        {
            # client IDの最初の`-`までのプレフィックスを抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.groupとして設定
            set_as_attr = group
        }
    ]
}
```

::: tip
エスケープ解除関数が必要な場合、HOCON設定でトリプルクォート`"""`文字列を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行のclient IDの最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を備えています。これらは抽出データの操作やフォーマットに使えます。例：`lower()`、`upper()`、`concat()`、`hash()`、`hash_to_range()`など。

利用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../../develop/data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`：任意の中間値を文字列に変換
- **配列関数**：`nth/2`など
- **乱数関数**：`rand_str`、`rand_int`
- **スキーマレスエンコード/デコード関数**：
  - `bin2hexstr(Data)`
  - `hexstr2bin(Data)`
  - `base64_decode(Data)`
  - `base64_encode(Data)`
  - `int2hexstr(Integer)`：整数を16進文字列に変換（例：15 → 'F'）
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：Algorithmは`md4`、`md5`、`sha`（`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`のいずれか
  - `hash_to_range(Input, Min, Max)`：`sha256`でハッシュ化し、`Min`から`Max`の範囲にマップ（`Min <= X <= Max`）
  - `map_to_rage(Input, Min, Max)`：入力を`Min`から`Max`の範囲にマップ
- **比較関数**：
  - `num_eq(A, B)`：数値が等しい場合`true`、それ以外は`false`
  - `num_neq(A, B)`：数値が等しくない場合`true`、それ以外は`false`
  - `num_gt(A, B)`：`A > B`なら`true`、それ以外は`false`
  - `num_gte(A, B)`：`A >= B`なら`true`、それ以外は`false`
  - `num_lt(A, B)`：`A < B`なら`true`、それ以外は`false`
  - `num_lte(A, B)`：`A <= B`なら`true`、それ以外は`false`
  - `str_eq(A, B)`：文字列が等しい場合`true`、それ以外は`false`
  - `str_neq(A, B)`：文字列が等しくない場合`true`、それ以外は`false`
  - `str_gt(A, B)`：辞書順で`A > B`なら`true`、それ以外は`false`
  - `str_gte(A, B)`：辞書順で`A >= B`なら`true`、それ以外は`false`
  - `str_lt(A, B)`：辞書順で`A < B`なら`true`、それ以外は`false`
  - `str_lte(A, B)`：辞書順で`A <= B`なら`true`、それ以外は`false`
  - `is_empty_var(V)`：変数が空か判定。Variformでの空は未定義（`undefined`）、JSONの`null`（文字列`"null"`は除く）、空文字列`""`を含む
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`。文字列も受け入れ、入力が文字列の場合は出力も文字列

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。以下の制約あり：
    - OS環境変数は`EMQXVAR_`をプレフィックスとして読み込む。例：`getenv('FOO_BAR')`は`EMQXVAR_FOO_BAR`を読み込む
    - OS環境変数から読み込んだ値は不変

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスで値を抽出。例：`username`がJSONオブジェクトなら`json_value(username, 'shop.floor')`
  - `jwt_value(Data, Path)`：JWTトークンのペイロードからクレーム値をドット区切りパスで抽出。例：`password`がJWTなら`jwt_value(password, 'client_attrs.unitid')`

#### 条件式

Variform式は包括的な制御フローを持ちませんが、以下の関数で基本的な条件制御が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、そうでなければ`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の空でない要素を返す。

#### エラー処理

Bashなどのスクリプト環境と同様に、Variform式は未束縛変数や実行時例外が発生した場合、空文字列（`""`）を返す設計です。

- 未束縛変数：定義されていない変数参照は空文字列として評価されます。
- 実行時例外：関数の誤用や型エラーなどの例外発生時も空文字列を返します。例：配列インデックスの範囲外。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclient IDのプレフィックスを抽出
- `strlen(username, 0, 5)`：ユーザー名の一部を抽出
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclient IDから数字を抽出。空文字列なら`'000'`を返す
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid`が`foo.`で始まる場合`foo`、そうでなければ`bar`を返す
