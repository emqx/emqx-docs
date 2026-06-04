# 設定ファイル

ユーザーは設定ファイルまたは環境変数を用いてEMQXを設定できます。本節では主にEMQXの設定ファイルを紹介し、EMQXで最も一般的に使用される機能の基本的な設定方法を説明します。詳細な設定項目と解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定およびランタイムデータを管理するための一連のディレクトリが作成されます。これらのディレクトリは大きく2つのカテゴリに分かれています。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、ランタイムに生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ（`etc`）

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイやアップグレード時に変更され、ランタイム中は安定性を確保するため読み取り専用となります。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                          | パス               |
| ---------------------------------------- | ------------------ |
| RPMまたはDEBパッケージでインストール    | `/etc/emqx`        |
| Dockerコンテナで実行                     | `/opt/emqx/etc`    |
| ポータブル圧縮パッケージから展開        | `./etc`            |

### 動的設定ディレクトリ（`data/configs`）

ランタイム中、EMQXはダッシュボード、REST API、CLIを通じて動的な再設定を許可します。これらのツールで行われた変更は`data/configs`ディレクトリに保存され、セッションを跨いで永続化されます。このディレクトリの場所もインストール方法によって異なります。

| インストール方法                          | パス                    |
| ---------------------------------------- | ----------------------- |
| RPMまたはDEBパッケージでインストール    | `/var/lib/emqx/configs` |
| Dockerコンテナで実行                     | `/opt/emqx/data/configs`|
| ポータブル圧縮パッケージから展開        | `./data/configs`        |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することでデータディレクトリを変更可能です。ただし、クラスター運用時は全ノードで同一のディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、重複があった場合はあらかじめ定められた上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションでは詳細なリファレンスを提供していますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合、設定例は`etc/emqx/examples`ディレクトリにあります。
- DockerコンテナでEMQXを実行している場合は、`opt/emqx/etc/examples`ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが追加されました。このファイルにはデフォルト設定が含まれ、ランタイム中により上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、その後ダッシュボードUIからより複雑な設定に上書きすることができます。

`node`や`cluster`のような不変設定は`base.hocon`に設定することは**推奨されません**。詳細は[Immutable Configurations File](#immutable-configuration-file)をご参照ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、配置されているノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリ内の`cluster.hocon`ファイルにはクラスター全体の設定項目が含まれています。ダッシュボード、REST API、CLIから行われた設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新規ノードが追加された場合、対象ノードは自動的に他のノードから`cluster.hocon`ファイルをコピーして適用します。このため、手動での編集は推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層については[Config Override Rules](#config-override-rules)をご参照ください。

EMQX 5.1以降、クラスター設定の変更時に`cluster.hocon`ファイルのバックアップが上書き前に自動作成されます。バックアップファイルはノードのローカル時間でタイムスタンプが付与され、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`設定を含む重要なシステム設定の主要な設定ファイルとして残されています。このファイルは`base.hocon`や`cluster.hocon`より優先度が高いですが、環境変数よりは低い優先度です。

設定の上書きについては[Config Override Rules](#config-override-rules)をご参照ください。

## 設定パス

EMQXでは設定値をドット区切りのパスで参照できます。これはツリー構造に似ており、ルート（常にStruct）から始まり、各セグメントはフィールド名またはMapのキーを指します。配列要素の場合は1始まりのインデックスを使用します。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0以降、設定ファイルフォーマットとして[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間に読みやすいデータフォーマットであり、JSONのスーパーセットです。継承、結合、引用符などの機能により設定作業をさらに簡素化します。

**HOCON構文例：**

JSONに似たオブジェクト形式で記述可能です。

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

このフラット形式は従来のEMQXバージョンとの後方互換性がありますが、使用方法が異なります。

HOCONでは文字列は両端に引用符を付けることが推奨されます。特殊文字を含まない文字列は引用符なしでも構いません（例：`foo`、`foo_bar`）。一方、フラット形式では`=`の右側のすべてを値とみなします。

詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)をご参照ください。

## 環境変数

設定ファイルに加え、環境変数でもEMQXを設定可能です。

例えば、環境変数`EMQX_NODE__NAME=emqx2@127.0.0.1`は以下の設定を上書きします。

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数の変換ルールは以下の通りです。

1. 設定ファイルの`.`区切りは環境変数で使えないため、EMQXは区切りに`__`（ダブルアンダースコア）を使用します。
2. 他の環境変数と区別するため、環境変数名の先頭に`EMQX_`を付加します。
3. 環境変数の値はHOCON値として解析されるため、複雑なデータ型も渡せます。`:`、`=`、`#`などのHOCON特殊文字を含む値は必ずダブルクォート`"`で囲み、リテラル文字列として扱われるようにします。特に`#`はHOCONの行コメント開始文字なので、引用符なしでは`#`以降がコメントとして無視されます。

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

パスワードなどに`#`を含む場合、`#`はHOCONの行コメントとして扱われるため、以下のように

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

パスワードは`MQtt`として解析され、`#123`はコメントとして無視されます。リテラルとして渡すには**HOCONレベルの**ダブルクォートで囲み、パーサーに`"MQtt#123"`として認識させる必要があります。

```bash
# 正しい例 — HOCONパーサーに渡される値は "MQtt#123"
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# 同様の効果、シェル用に内側の引用符をエスケープ
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

`:`や`=`を含む値も同様です。URLエンコード（例：`%23`）は無効で、EMQXは環境変数値をURLデコードしません。

:::

::: tip なぜ一部の引用なし値は通り、一部は通らないのか

EMQXは内部的に各環境変数値を`fake_key=<value>`としてHOCON解析します。解析成功すればその値を使い、失敗すれば生の文字列を使います。

そのため、`EMQX_..._PASSWORD="abc#def"`は有効なHOCONで`#def`がコメント扱いされ`abc`となりますが、`EMQX_..._PASSWORD=".abc#def"`は無効なHOCONで生の文字列`.abc#def`が使われます。HOCONの引用符で囲むと動作が明確になります。

:::

::: tip

EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。これは`UNKNOWN_ROOT`が事前定義されたルートパスでないためです。

既知のルートパスに未知のフィールド名が設定されると、起動時に`warning`ログを出力します。例えば、`enable`を誤って`enabled`と設定すると以下のように出力されます。

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 設定上書きルール

EMQXでは設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が前の値を上書きします。
- 上位レベルの設定が下位レベルの設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり、`base.hocon`の設定は最も優先度が低く、上位の設定で上書き可能です。`EMQX_`で始まる環境変数が最も優先されます。

::: tip
5.8.4以前のバージョンでは`base.hocon`ファイルは存在しません。優先順位は同じですが`base.hocon`は含まれません。
:::

EMQXダッシュボードUI、HTTP API、CLIからの変更はランタイム中に`cluster.hocon`に永続化され即時反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、`emqx.conf`と`cluster.hocon`間で設定を重複させることは**避けてください**。

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`ファイルが存在し、設定優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. これらのバージョンから最新バージョンにアップグレードしても優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`の仕組みは5.1で廃止されました。
:::

### 上書き例

以下の設定では、最後の行で`level`の`debug`値が以前の`error`を上書きしますが、`enable`フィールドは変更されません。

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

この特徴を利用し、配列の要素を簡単に上書きできます。

```bash
authentication  = [
  {
    enable = true,
    backend = "built_in_database",
    mechanism = "password_based"
  }
]

# 最初の要素の`enable`フィールドを以下のように上書き可能
authentication.1.enable = false
```

::: tip

リスト形式の配列は完全に上書きされ、元の値は保持できません。例えば：

```bash
authentication = [
  {
    enable = true
    backend = "built_in_database"
    mechanism="password_based"
  }
]

## 以下の設定では最初の要素の`enable`以外の全フィールドが失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。ゾーンはリスナーの`zone`フィールドに設定することで関連付けられ、ゾーンに紐づくリスナーに接続したMQTTクライアントはそのゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐づいています。`default`ゾーンは論理的なグループであり、設定ファイル上には存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続およびセッション設定。特定ゾーンでMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：セッション永続化設定。特定ゾーンでMQTTセッションの永続ストレージを有効化など。

EMQX 5系のデフォルト設定ファイルにはゾーンは含まれていません。これは4系の`internal`と`external`という2つのデフォルトゾーンとは異なります。

ゾーンを作成するには設定ファイルに以下のように定義します。

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

リスナーで`zone`フィールドを設定し、作成済みのゾーンに紐づけます。

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
ゾーンの設定スキーマは各グループで共通のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルに登場するプリミティブ型は基本的に自明であり、詳細な説明は不要です。以下に代表的な型を列挙します。

#### Integer

整数値。例：`42`、`-3`、`0`。

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義されたシンボルのいずれかのみ許容。例：`Enum(debug,info,warning,error)`はログレベルを定義。

#### String

文字列型で、様々な形式をサポートします。

- **引用符なし**：特殊文字を含まない単純な識別子や名前に適します（詳細は後述）。
- **引用符付き文字列**：特殊文字や空白を含む場合はダブルクォート`"`で囲み、必要に応じてバックスラッシュ`\`でエスケープします。例：`"line1\nline2"`。
- **トリプルクォート文字列**：`"""`で囲み、`\`以外のエスケープ不要で複雑な内容を含められます。トリプルクォート隣接のクォートはエスケープが必要です。
- **インデント付きトリプルクォート文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを保持でき、多行や整形テキストに適します。

**引用符なし文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、空白。
- `//`で始めない（コメント開始と誤認されるため）。
- `true`、`false`、`null`で始めない（ブールやnullと誤認されるため）。

**トリプルクォート文字列のガイドライン：**

- トリプルクォート隣接のクォートはエスケープまたは`~`区切りを使用。
- 複数行文字列はスペース（タブ不可）でインデント可能。インデントは最小の先頭スペース数に合わせて除去。

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

EMQX独自のインデント付きトリプルクォート文字列の詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)をご覧ください。

#### String("constant")

定数文字列値で、単一値の列挙型（`Enum`）のように機能します。特定の設定やモードの静的値に利用。

#### Boolean

`true`または`false`（大文字小文字区別あり）。

#### Float

浮動小数点数。例：`3.14`、`-0.001`。

#### Duration

人間に読みやすい時間の長さを表現。形式の例や説明あり。

#### Duration(s)

秒単位の精度を持つ`Duration`型。詳細と例あり。

#### Secret

パスワードやトークンなどの機密情報用型。使用方法と重要性の説明あり。

### 複合データ型

EMQXのHOCON設定における複合型は、他の複合型やプリミティブ値を含むデータ構造を表現します。階層的で柔軟なデータ表現を可能にします。

#### Struct `Struct(name)`

中括弧`{}`で囲まれたフィールドを持つ構造体。`name`は構造体のフィールド名と型を定義するスキーマ参照。

#### Map `Map($name->Type)`

`Struct`に似ていますが、フィールド名が事前定義されていません。

`$name`はドット`.`を含まない任意の文字列キーを示し、エンティティや属性名を表します。`Type`はマップ内のすべての値の型を指定し、均一なデータコレクションを可能にします。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを許容するユニオン型。構造体のフィールドが複数の型のいずれかとなることを示します。例：`String(infinity)`または`Duration`のどちらか。

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

Variformは文字列操作やランタイム評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内に埋め込んで動的な文字列操作を実現します。

::: tip
Variform式は特定の設定項目にのみ適用されます。指定がない限り使用しないでください。
:::

::: tip NULL値について
Variform式では、値バインディング参照や部分式の評価結果が未定義の場合、空文字列（`''`）として扱われます。

JSONで`null`のフィールドは未定義値（`''`）として扱われ、文字列`"null"`とは異なります。
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
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲むASCII文字
- 配列：`[`と`]`で囲み、カンマ`,`区切り
- 変数：事前定義値参照（例：`clientid`）
- 関数：事前定義関数（例：`concat([...])`）

Variformは以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のアンエスケープには`unescape`関数を使用）

以下はVariform式を埋め込んだ設定例です。

```js
mqtt {
    client_attrs_init = [
        {
            # client IDの最初の`-`までのプレフィックスを抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.groupに設定
            set_as_attr = group
        }
    ]
}
```

::: tip
アンエスケープ関数が必要な場合、HOCON設定でトリプルクォート`"""`文字列を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行のclient IDのうち最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

EMQXはルールエンジンの文字列関数に類似した豊富な文字列、配列、乱数、ハッシュ関数を備えています。これらは抽出したデータの操作や整形に使えます。例：`lower()`、`upper()`、`concat()`は文字列のフォーマット調整に、`hash()`や`hash_to_range()`はハッシュ化や範囲マッピングに利用可能です。

利用可能な関数の例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`：任意の中間非文字列値を文字列に変換
- **配列関数**：`nth/2`など（[参照](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)）
- **乱数関数**：`rand_str`、`rand_int`
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_decode(Data, 'no_padding')](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string) (6.0.2以降)
  - [base64_decode(Data, 'no_padding', 'urlsafe')](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string) (6.0.2以降)
  - [base64_encode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - [base64_encode(Data, 'no_padding')](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string) (6.0.2以降)
  - [base64_encode(Data, 'no_padding', 'urlsafe')](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string) (6.0.2以降)
  - `int2hexstr(Integer)`：整数を16進文字列に変換（例：15 → 'F'（大文字））
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：`Algorithm`は`md4`、`md5`、`sha`（または`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`のいずれか
  - `hash_to_range(Input, Min, Max)`：`sha256`で`Input`をハッシュ化し、`Min`から`Max`までの整数にマッピング（`Min <= X <= Max`）
  - `map_to_rage(Input, Min, Max)`：`Input`を`Min`から`Max`までの整数にマッピング（`Min <= X <= Max`）
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
  - `str_gte(A, B)`：辞書順で`A`が`B`より前でないなら`true`、それ以外は`false`
  - `str_lt(A, B)`：辞書順で`A`が`B`より前なら`true`、それ以外は`false`
  - `str_lte(A, B)`：辞書順で`A`が`B`より後でないなら`true`、それ以外は`false`
  - `is_empty_var(V)`：変数が空か判定。Variformの空は未定義（`undefined`）、JSONの`null`（文字列`"null"`は含まない）、空文字列`""`を指す
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`を返す。文字列も受け入れ、入力が文字列なら出力も文字列

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。以下の制約あり：
    - OS環境変数の読み取り時に`EMQXVAR_`プレフィックスを付加。例：`getenv('FOO_BAR')`は`EMQXVAR_FOO_BAR`を読み取る。
    - OS環境変数から読み込んだ値は不変。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスで値を抽出。例：`username`がJSONオブジェクトなら`json_value(username, 'shop.floor')`でフィールド取得。
  - `jwt_value(Data, Path)`：JWTトークンのペイロードからクレーム値をドット区切りパスで抽出。例：`password`がカスタムクレームを持つJWTなら`jwt_value(password, 'client_attrs.unitid')`で値取得。

#### 条件式

Variform式は包括的な制御構造を持ちませんが、以下の関数で基本的な返却値制御が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、それ以外は`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の空でない要素を返す。

#### エラー処理

Bashなどのスクリプト環境同様、Variform式は未バインド変数や実行時例外時に空文字列（`""`）を返す設計です。

- 未バインド変数：定義されていない変数参照は空文字列として評価。
- 実行時例外：関数誤用や型不正などの例外発生時も空文字列を返す。例：配列インデックス範囲外など。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclient IDのプレフィックスを抽出。
- `strlen(username, 0, 5)`：usernameの部分文字列を抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclient IDから数字を抽出。空なら`'000'`を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid`が`foo.`で始まれば`foo`、そうでなければ`bar`を返す。
