# 設定ファイル

ユーザーは設定ファイルまたは環境変数を使用してEMQXを設定できます。本節では主にEMQXの設定ファイルについて紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目や説明については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定およびランタイムデータを管理するためのディレクトリ群が作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、ランタイムで生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ（`etc`）

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイやアップグレード時に変更され、ランタイム中は安定性を確保するため読み取り専用となります。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                         | パス              |
| -------------------------------------- | ----------------- |
| RPMまたはDEBパッケージでインストール   | `/etc/emqx`       |
| Dockerコンテナで実行                    | `/opt/emqx/etc`   |
| ポータブル圧縮パッケージから展開       | `./etc`           |

### 動的設定ディレクトリ（`data/configs`）

ランタイム中、EMQXはダッシュボード、REST API、CLIを通じて動的な再設定を許可しています。これらのツールで行われた変更は`data/configs`ディレクトリに保存され、セッションをまたいで永続化されます。このディレクトリの場所もインストール方法により異なります。

| インストール方法                         | パス                      |
| -------------------------------------- | ------------------------- |
| RPMまたはDEBパッケージでインストール   | `/var/lib/emqx/configs`   |
| Dockerコンテナで実行                    | `/opt/emqx/data/configs`  |
| ポータブル圧縮パッケージから展開       | `./data/configs`          |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することで、データディレクトリの場所を変更可能です。ただし、クラスター運用時は全ノードで同一のディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、重複した場合はあらかじめ定められた上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)をご参照ください。

## 設定例

[Schema](#schema)セクションで詳細なリファレンスを提供していますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合、設定例は`etc/emqx/examples`ディレクトリにあります。
- DockerコンテナでEMQXを実行している場合、設定例は`opt/emqx/etc/examples`ディレクトリにあります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれており、ランタイムでより上位の設定ファイルによって上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、後からダッシュボードUIからより複雑な設定で上書きすることができます。

`node`や`cluster`のような不変の設定については、デプロイ固有でランタイム中に変更すべきでない値の場合、環境変数を使用することも可能です。詳細は[Environment Variables](#environment-variables)および[Config Override Rules](#config-override-rules)をご参照ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、そのノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリ内の`cluster.hocon`ファイルはクラスター全体の設定項目を含みます。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新規ノードが追加された場合、ノードは自動的に他のノードから`cluster.hocon`ファイルをコピーして適用します。このため、手動での編集は推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層については[Config Override Rules](#config-override-rules)をご参照ください。

EMQX 5.1以降、クラスタ設定の変更時に`cluster.hocon`ファイルのバックアップが作成されます。バックアップはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`の重要なシステム設定に対して引き続き利用可能です。このファイルは`base.hocon`や`cluster.hocon`より優先度が高いですが、環境変数よりは低い優先度です。意図的にこの優先度を利用し、パッケージのアップグレードでこのファイルのデフォルトが更新される可能性を理解している場合を除き、変更は避けてください。

設定の上書きに関する詳細は[Config Override Rules](#config-override-rules)をご参照ください。

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

HOCONは人間に読みやすいデータ形式であり、JSONのスーパーセットです。継承や結合、引用符などの機能により設定作業をさらに簡素化します。

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

またはフラット化して記述も可能です。

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このイカのようなフラット化形式は従来のEMQXバージョンとの互換性を保ちつつ、使い分けられています。

HOCONでは文字列の両端に引用符を付けることを推奨します。特殊文字を含まない文字列は引用符なしでも構いません（例：`foo`、`foo_bar`）。一方、イカ形式は`=`の右側のすべてを値として扱います。

HOCON構文の詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)をご参照ください。

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

1. 設定ファイルの`.`区切りは環境変数で使えないため、EMQXは`__`（ダブルアンダースコア）を区切り文字として使用します。
2. 他の環境変数と区別するため、環境変数名の先頭に`EMQX_`を付加します。
3. 環境変数の値はHOCONの値として解析されるため、複雑なデータ型も渡せます。ただし、`:`や`=`などの特殊文字はダブルクォート`"`で囲む必要があります。

変換例：

```bash
# 環境変数

## localhost:1883は構造体{"localhost": 1883}として解析されるため、ダブルクォートで囲む必要があります
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

## HOCON配列を文字列で直接渡す例
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

EMQXでは設定値は階層的に適用され、以下の上書きルールが適用されます。

- 同一ファイル内では後に定義された値が先の値を上書きします。
- 上位の設定が下位の設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり、`base.hocon`の設定は最も優先度が低く、上位のファイルの設定で上書き可能です。`EMQX_`で始まる環境変数は最も優先されます。

::: tip
バージョン5.8.4以前は`base.hocon`ファイルが存在しませんでした。優先順位は同じですが、`base.hocon`はありません。
:::

ダッシュボードUI、HTTP API、CLIからの変更はランタイムで`cluster.hocon`に永続化され、即時反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、`emqx.conf`と`cluster.hocon`間で設定の重複は避けてください。

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`ファイルが存在し、設定優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. これらのバージョンから最新にアップグレードしても優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`機構は5.1で削除されました。
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

パケットサイズの上限は最初に1MBに設定され、その後10MBに上書きされています。

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

- リスト形式例：`[1, 2, 3]`
- マップ形式（サブスクライブ形式）例：`{"1"=1, "2"=2, "3"=3}`

以下の3つの形式は同等です。

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

この特徴を利用して、配列内の特定要素の値を簡単に上書きできます。

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

## 以下の設定では1番目の要素の`enable`以外のフィールドは失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXにおけるゾーンは設定をグループ化する概念です。リスナーに`zone`フィールドでゾーン名を設定すると、そのリスナーに接続するMQTTクライアントはゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐づいています。`default`ゾーンは論理的なグループであり、設定ファイル上には存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続およびセッション設定。特定ゾーンでMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：セッション永続化設定。特定ゾーンでMQTTセッションの永続ストレージを有効化など。

EMQX 5系のデフォルト設定ファイルにはゾーンは含まれていません。これは4系の`internal`および`external`という2つのデフォルトゾーンとは異なります。

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

リスナーの`zone`フィールドに作成済みのゾーン名を設定します。

```bash
listeners.tcp.default {
    bind = 1883
    zone = my_zone1
    ...
}
```

## 設定コード管理のベストプラクティス

EMQXの設定をソース管理や自動化システムで管理する場合、以下のルールを推奨します。

- 設定コードは`base.hocon`に記述する。
- `cluster.hocon`を手動編集したり、自分でマウントしたりしない。
- `emqx.conf`は設定階層での優先度やアップグレード影響を理解していない限り変更しない。
- ダッシュボード、API、CLIで変更しない単純な上書きは環境変数で行う。

設定コードの真実のソースは`base.hocon`です。ノード起動時に静的設定ディレクトリから読み込まれ、パッケージングやイメージビルド、構成管理、GitOpsワークフローで管理可能です。ランタイムの変更はダッシュボード、REST API、CLIから`cluster.hocon`に永続化され、`base.hocon`の上に重ねられます。

例えば、リスナー、ログ、認証、認可、データ統合のベースライン設定を`base.hocon`に保持できます。

```bash
# base.hocon
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}

log.console {
  enable = true
  level = warning
}

authentication = [
  {
    mechanism = password_based
    backend = built_in_database
    user_id_type = username
  }
]
```

`cluster.hocon`は設定コードの真実のソースとして使わないでください。EMQXがランタイムで管理し、ダッシュボード、REST API、CLIが書き換え、上書き前にバックアップを作成し、クラスター内でノード間コピーも行います。手動編集やマウントはランタイム更新と競合し、上書きされる恐れがあります。

`emqx.conf`は配布パッケージにベースライン設定として同梱されています。変更しなければアップグレード時に新しいEMQXバージョンの保守的なデフォルト変更を取り込みやすくなります。`emqx.conf`で設定した項目は`base.hocon`や`cluster.hocon`より優先度が高いため、ランタイムで同じ項目を変更してもノード再起動後に元に戻ることがあります。意図的にその挙動が必要な場合のみ使用してください。

環境変数は最も優先度が高く、特にデプロイ固有の単純な値やランタイムで変更すべきでない値に適しています。

```bash
export EMQX_NODE__NAME='emqx@node1.example.net'
export EMQX_NODE__COOKIE='mysecret'
export EMQX_CLUSTER__DISCOVERY_STRATEGY='static'
export EMQX_CLUSTER__STATIC__SEEDS='["emqx@node1.example.net", "emqx@node2.example.net"]'
```

環境変数はすべての設定ファイルを上書きするため、オペレーターが後でダッシュボード、API、CLIから調整することが想定される設定には使用を避けてください。

## スキーマ

HOCONオブジェクトを型安全にするため、EMQXはスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などを可能にします。

[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)はこのスキーマから生成されています。

::: tip
ゾーンの設定スキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルに登場するプリミティブ型はほぼ自明であり、最小限の説明で十分です。以下に代表的な型を示します。

#### Integer

整数値を表します。例：`42`、`-3`、`0`

#### Integer(Min..Max)

指定された範囲内の整数。例：`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義されたシンボルのいずれかのみを取ります。例：`Enum(debug,info,warning,error)`はログレベルの指定。

#### String

文字列型で、複数の形式をサポートします。

- **引用符なし**：特殊文字を含まない単純な識別子や名前に適します（詳細は下記）。
- **引用符付き文字列**：特殊文字や空白を含む場合はダブルクォート`"`で囲み、必要に応じてバックスラッシュ`\`でエスケープします。例：`"line1\nline2"`
- **三重引用符文字列**：`"""`で囲み、`\`以外のエスケープ不要で複雑な内容を簡単に記述可能。三重引用符に隣接するクォートはエスケープが必要です。
- **インデント付き三重引用符文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを保持でき、多行や整形テキストに適します。

**引用符なし文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、および空白
- `//`で始めない（コメントと誤認されるため）
- `true`、`false`、`null`で始めない（ブールやnullと誤解されるため）

**三重引用符文字列のガイドライン：**

- 三重引用符に隣接するクォートはエスケープまたは`~`区切りを使う
- 複数行文字列はスペース（タブ不可）でインデント可能。インデントレベルは最小の先頭スペース数で判定

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

EMQX独自のインデント付き三重引用符文字列の詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)を参照してください。

#### String("constant")

定数文字列で、単一値の列挙（Enum）として機能します。特定の設定やモードなど静的値に利用されます。

#### Boolean

`true`または`false`（大文字小文字区別あり）

#### Float

浮動小数点数。例：`3.14`、`-0.001`

#### Duration

人間に読みやすい時間の長さを表します。フォーマットの例と説明があります。

#### Duration(s)

秒単位の精度を持つDuration型。詳細と例があります。

#### Secret

パスワードやトークンなど機密情報用の型。利用方法と重要性の説明があります。

### 複合データ型

EMQXのHOCON設定で使われる複合型は、他の複合型やプリミティブ型を含む階層的なデータ構造を表現可能です。

#### Struct `Struct(name)`

中括弧`{}`で囲まれたフィールドを持つ構造体。`name`はスキーマ内の構造体定義を参照します。

#### Map `Map($name->Type)`

Structに似ていますが、フィールド名が事前定義されていないキーと値のペアの集合です。

`$name`はドットを含まない任意の文字列キーを表し、`Type`はすべての値が同じ型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを取るユニオン型。例えば`String(infinity)`または`Duration`のどちらかを許容する設定項目に使います。

#### Array `Array(Type)`

指定された型の要素からなる配列。

::: tip

Mapのフィールド名が正の整数の場合、`Array`の別表現として解釈されます。例えば：

```bash
myarray.1 = 74
myarray.2 = 75
```

は`myarray = [74, 75]`と解釈され、配列要素の上書きに便利です。

:::

### Variform式

Variformは文字列操作やランタイム評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内に埋め込んで動的に文字列操作を行うための専門ツールです。

::: tip
Variform式は特定の設定項目にのみ適用されます。明示されていない限り使用しないでください。
:::

::: tip NULL値について
Variform式では値のバインディング参照や部分式の評価が未定義値となる場合があり、これは空文字列（`''`）で表現されます。

JSONで`null`のフィールドは未定義値（空文字列）として扱われ、文字列`"null"`とは異なります。
:::

#### 構文例

```js
function_call(clientid, another_function_call(username))
```

これは`clientid`と`username`を組み合わせて新しい文字列値を生成する式です。

Variformがサポートするリテラル：

- ブール値：`true`または`false`
- 整数：例`42`
- 浮動小数点数：例`3.14`
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲むASCII文字列
- 配列：`[`と`]`で囲み、カンマ`,`で区切る
- 変数：事前定義された値の参照（例：`clientid`）
- 関数：事前定義された関数（例：`concat([...])`）

Variformは以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のエスケープは`unescape`関数を使用）

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
式内で`unescape`関数が必要な場合、HOCON設定で三重引用符`"""`文字列を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行のclient IDの最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を提供しています。これらは抽出データの操作や整形に使えます。例えば`lower()`、`upper()`、`concat()`は文字列のフォーマット調整に、`hash()`や`hash_to_range()`はハッシュ化や範囲マッピングに利用可能です。

利用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`は任意の中間非文字列値を文字列に変換します。
- **配列関数**：[nth/2](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
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
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスで値を抽出。例：`username`がJSONオブジェクトの場合、`json_value(username, 'shop.floor')`でフィールド取得（6.0.2以降）
  - `jwt_value(Data, Path)`：JWTトークンのペイロードからドット区切りパスでクレーム値を抽出。例：`password`がカスタムクレームを持つJWTの場合、`jwt_value(password, 'client_attrs.unitid')`でネスト値取得（6.0.2以降）
  - `int2hexstr(Integer)`：整数を16進文字列にエンコード（例：15 → 'F'大文字）
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：アルゴリズムは`md4`、`md5`、`sha`（`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`から選択可能
  - `hash_to_range(Input, Min, Max)`：`sha256`でハッシュ化し、`Min`から`Max`の整数範囲にマッピング（`Min <= X <= Max`）
  - `map_to_rage(Input, Min, Max)`：入力を`Min`から`Max`の整数範囲にマッピング（`Min <= X <= Max`）
- **比較関数**：
  - `num_eq(A, B)`：数値が等しい場合`true`、そうでなければ`false`
  - `num_neq(A, B)`：数値が等しくない場合`true`、そうでなければ`false`
  - `num_gt(A, B)`：`A > B`なら`true`、そうでなければ`false`
  - `num_gte(A, B)`：`A >= B`なら`true`、そうでなければ`false`
  - `num_lt(A, B)`：`A < B`なら`true`、そうでなければ`false`
  - `num_lte(A, B)`：`A <= B`なら`true`、そうでなければ`false`
  - `str_eq(A, B)`：文字列が等しい場合`true`、そうでなければ`false`
  - `str_neq(A, B)`：文字列が等しくない場合`true`、そうでなければ`false`
  - `str_gt(A, B)`：辞書順で`A > B`なら`true`、そうでなければ`false`
  - `str_gte(A, B)`：辞書順で`A >= B`なら`true`、そうでなければ`false`
  - `str_lt(A, B)`：辞書順で`A < B`なら`true`、そうでなければ`false`
  - `str_lte(A, B)`：辞書順で`A <= B`なら`true`、そうでなければ`false`
  - `is_empty_var(V)`：変数が空か判定。Variformの空は未定義値（`undefined`）、JSONの`null`（文字列`"null"`は含まない）、空文字列`""`を含む
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`を返す。文字列パラメータも受け付け、入力が文字列なら出力も文字列

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。ただしOS環境変数は`EMQXVAR_`プレフィックスを付けて読み込む。例：`getenv('FOO_BAR')`は`EMQXVAR_FOO_BAR`を読み込み。読み込み後は値は不変。

#### 条件式

Variform式には包括的な制御フローはありませんが、以下の関数で基本的な値の返却制御が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、そうでなければ`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の空でない配列要素を返す。

#### エラー処理

Bashなどのスクリプト環境と同様、Variform式は未バインド変数や実行時例外が発生した場合は空文字列（`""`）を返す設計です。

- 未バインド変数：定義されていない変数参照は空文字列として評価されます。
- 実行時例外：関数の誤用や無効なデータ型などの例外は空文字列を返します。例：配列インデックス範囲外。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclientidのプレフィックスを抽出
- `strlen(username, 0, 5)`：usernameの一部を抽出
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclientidから数字を抽出。空文字列なら`'000'`を返す
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：clientidが`foo.`で始まるなら`foo`、そうでなければ`bar`を返す
