# 設定ファイル

ユーザーは設定ファイルまたは環境変数を使ってEMQXを設定できます。本節では主にEMQXの設定ファイルについて紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目と解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定およびランタイムデータを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、ランタイムで生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ（`etc`）

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイやアップグレード時に変更され、ランタイム中は安定性を保つため読み取り専用となっています。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                           | パス              |
| ------------------------------------------ | ----------------- |
| RPMまたはDEBパッケージでインストール      | `/etc/emqx`       |
| Dockerコンテナで実行                       | `/opt/emqx/etc`   |
| ポータブル圧縮パッケージから展開          | `./etc`           |

### 動的設定ディレクトリ（`data/configs`）

ランタイム中、EMQXはダッシュボード、REST API、CLIを通じて動的な再設定を可能にします。これらのツールで行われた変更は`data/configs`ディレクトリに保存され、セッションをまたいで永続化されます。このディレクトリの場所もインストール方法によって異なります。

| インストール方法                           | パス                     |
| ------------------------------------------ | ------------------------ |
| RPMまたはDEBパッケージでインストール      | `/var/lib/emqx/configs`  |
| Dockerコンテナで実行                       | `/opt/emqx/data/configs` |
| ポータブル圧縮パッケージから展開          | `./data/configs`         |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することで、データディレクトリの場所を変更可能です。ただし、クラスター運用時は全ノードで同じディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、もし重複があった場合はあらかじめ定められた上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションで詳細なリファレンスを提供していますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合は、`etc/emqx/examples`ディレクトリに設定例があります。
- DockerコンテナでEMQXを実行している場合は、`opt/emqx/etc/examples`ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれており、ランタイム中に上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、その後ダッシュボードUIからより複雑な設定に上書きすることができます。

`node`や`cluster`など不変の設定については、値がデプロイ固有でランタイム中に変更しない場合、環境変数を使うことも可能です。詳細は[Environment Variables](#environment-variables)および[Config Override Rules](#config-override-rules)をご参照ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、そのノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリの`cluster.hocon`ファイルにはクラスター全体の設定項目が含まれています。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、ノードは自動的に他のノードから`cluster.hocon`ファイルをコピーして適用します。そのため、このファイルを手動で編集することは推奨されません。

このファイルの設定は`base.hocon`の設定に上書きされる形で適用されます。設定の上書き階層については[Config Override Rules](#config-override-rules)をご覧ください。

EMQX 5.1以降、クラスター設定の変更があると、上書き前に`cluster.hocon`ファイルのバックアップが作成されます。バックアップファイルはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`の重要なシステム設定に引き続き利用可能です。このファイルは`base.hocon`や`cluster.hocon`より優先度が高いですが、環境変数よりは低い優先度です。意図的にこの優先度を利用し、パッケージアップグレードでこのファイルのデフォルトが更新されることを理解している場合を除き、変更は避けてください。

設定の上書きについての詳細は[Config Override Rules](#config-override-rules)をご参照ください。

## 設定パス

EMQXでは設定値をツリー構造のようにドット区切りのパスで参照できます。ルートは常にStructで、パスの各セグメントはフィールド名またはMapのキーを指します。配列要素の場合は1始まりのインデックスを使用します。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0からは設定ファイルフォーマットとして[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間が読みやすいデータフォーマットでJSONのスーパーセットです。継承、結合、クォートなどの機能により設定作業をさらに簡素化します。

**HOCONの構文例：**

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

またはフラット形式でも記述可能です。

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このフラット形式は旧EMQXバージョンとの後方互換性がありますが、使い方に違いがあります。

HOCONでは文字列の両端にクォートを付けることを推奨します。特殊文字を含まない文字列はクォートなしでもよく、例えば`foo`や`foo_bar`のように記述できます。一方、フラット形式では`=`の右側のすべてを値として扱います。

HOCON構文の詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)をご参照ください。

## 環境変数

設定ファイルのほかに、環境変数を使ってEMQXを設定することも可能です。

例として、環境変数`EMQX_NODE__NAME=emqx2@127.0.0.1`は以下の設定を上書きします。

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数の変換ルールは以下の通りです。

1. 設定ファイルの`.`区切りは環境変数で使えないため、EMQXでは`__`（ダブルアンダースコア）を区切りとして使用します。
2. 他の環境変数と区別するため、環境変数名の先頭に`EMQX_`を付けます。
3. 環境変数の値はHOCON値として解析されるため、複雑なデータ型も渡せます。`:`、`=`、`#`などのHOCON特殊文字を含む値は必ずダブルクォート`"`で囲み、リテラル文字列として扱われるようにしてください。特に`#`はHOCONの行コメント開始文字なので、クォートなしだと`#`以降がコメントとして無視されます。

変換例：

```bash
# 環境変数

## localhost:1883は構造体{"localhost": 1883}として解析されるため、ダブルクォートで囲む必要がある
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

パスワードなどに`#`が含まれる場合、`#`はHOCONの行コメント開始文字なので、以下のようにすると`MQtt`だけがパースされ、`#123`はコメントとして無視されます。

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

リテラル値として渡すには、**HOCONレベルの**ダブルクォート（シェルのクォートではない）で囲み、パーサーに`"MQtt#123"`として認識させます。

```bash
# 正しい例 — HOCONパーサーに渡される値は "MQtt#123"
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# 同様の効果で、シェルの中で内側のクォートをエスケープ
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

`:`や`=`を含む値も同様です。URLエンコード（例：`%23`）は機能しません。EMQXは環境変数の値をURLデコードしません。

:::

::: tip なぜクォートなしの値が通る場合と通らない場合があるのか

EMQXは環境変数の値を`fake_key=<値>`という形にラップしてHOCONとして解析します。解析に成功すればその値を使い、失敗すれば生の文字列を使います。

例えば、`EMQX_..._PASSWORD="abc#def"`は有効なHOCONで`#def`がコメント扱いされるため`abc`となりますが、`EMQX_..._PASSWORD=".abc#def"`は無効なHOCONなので生の文字列`.abc#def`が使われます。

値をHOCONクォートで囲むと動作が確定的になります。

:::

::: tip

EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。`UNKNOWN_ROOT`は事前定義されたルートパスではないためです。

既知のルートパスに対して未知のフィールド名を設定すると、起動時に以下のような`warning`ログを出力します。例えば`enable`を誤って`enabled`とした場合：

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 設定上書きルール

EMQXでは設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が前の値を上書きします。
- 上位の設定が下位の設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり、`base.hocon`の設定は最も優先度が低く、上位の設定で上書きされます。`EMQX_`で始まる環境変数が最も優先されます。

::: tip
バージョン5.8.4以前は`base.hocon`ファイルが存在しませんでした。優先順位は同じですが、`base.hocon`はありません。
:::

ダッシュボードUI、HTTP API、CLIからの変更はランタイム中に`cluster.hocon`に永続化され即時反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、**`emqx.conf`と`cluster.hocon`で設定を重複させないでください。**

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では、`cluster-override.conf`ファイルが存在し、設定優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. これらの古いバージョンから最新バージョンにアップグレードする際、優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`の仕組みはバージョン5.1で廃止されました。
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

以下の3つは同等です。

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

この特徴を利用し、配列の要素を簡単に上書きできます。例えば：

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

## 以下の方法では、1番目の要素の`enable`以外のフィールドはすべて失われる
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。ゾーンはリスナーの`zone`フィールドに設定することで関連付けられ、そのゾーンに紐づくリスナーに接続したMQTTクライアントはゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐づいています。`default`ゾーンは論理的なグループであり、設定ファイル上には存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続やセッション設定。特定ゾーンでMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：MQTTセッションの永続化設定。特定ゾーンで永続化を有効にするなど。

EMQXバージョン5のデフォルト設定ファイルにはゾーンは含まれていません。バージョン4では`internal`と`external`の2つのデフォルトゾーンが存在していました。

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

## 設定コード管理のベストプラクティス

EMQXの設定をソース管理や自動化システムで管理する場合、以下のルールを推奨します。

- 設定コードは`base.hocon`に置く。
- `cluster.hocon`を手動で編集したり、自分の`cluster.hocon`をマウントしない。
- `emqx.conf`は優先度が高いこととアップグレード時の影響を理解した上でのみ変更する。
- ランタイム中にダッシュボード、API、CLIから変更しない単純な上書きは環境変数で行う。

設定コードの真実の情報源は`base.hocon`です。ノード起動時に静的設定ディレクトリから読み込まれ、パッケージングやイメージビルド、構成管理、GitOpsワークフローで管理可能です。ダッシュボード、REST API、CLIからのランタイム変更は`cluster.hocon`に永続化され、`base.hocon`の上に重ねられます。

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

`cluster.hocon`は設定コードの真実の情報源として使わないでください。EMQXがランタイムで管理するファイルであり、ダッシュボード、REST API、CLIが書き換え、EMQXは上書き前にバックアップを作成し、クラスター内のノード間でコピーされます。手動編集やマウントはランタイムの更新と競合したり上書きされる可能性があります。

`emqx.conf`は配布パッケージに同梱されるベースライン設定ファイルです。変更しなければアップグレード時に新しいEMQXバージョンの保守的なデフォルト変更を取り込めます。`emqx.conf`で設定した項目は`base.hocon`や`cluster.hocon`より優先度が高いため、ランタイム変更が反映されているように見えてもノード再起動後に元に戻ることがあります。意図的にこの動作が必要な場合のみ使用してください。

環境変数は最も優先度が高く、デプロイ固有の単純な値やランタイム中に変更すべきでない値に適しています。

```bash
export EMQX_NODE__NAME='emqx@node1.example.net'
export EMQX_NODE__COOKIE='mysecret'
export EMQX_CLUSTER__DISCOVERY_STRATEGY='static'
export EMQX_CLUSTER__STATIC__SEEDS='["emqx@node1.example.net", "emqx@node2.example.net"]'
```

環境変数はすべての設定ファイルを上書きするため、オペレーターが後からダッシュボード、API、CLIで調整することが想定される設定には使用を避けてください。

## スキーマ

HOCONオブジェクトの型安全性を高めるため、EMQXはスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などに利用されます。

[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)はこのスキーマから生成されています。

::: tip
ゾーン設定のスキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルに登場するプリミティブ型はほぼ自明で、最小限の説明で十分です。以下は全プリミティブ型の一覧です。

#### Integer

整数を表します。例：`42`、`-3`、`0`

#### Integer(Min..Max)

指定された範囲内の整数。例：`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義されたシンボルのいずれかのみを取れます。例：`Enum(debug,info,warning,error)`はログレベルの指定に使います。

#### String

文字列型で、複数の形式をサポートします。

- **クォートなし**：特殊文字を含まない単純な識別子や名前向け（詳細は後述）。
- **ダブルクォート文字列**：特殊文字や空白を含む場合に`"`で囲み、`\`でエスケープ可能。例：`"line1\nline2"`。
- **トリプルクォート文字列**：`"""`で囲み、エスケープ不要（`\`は例外）。複雑な内容を含む場合に便利。トリプルクォートに隣接するクォートはエスケープが必要。
- **インデント付きトリプルクォート文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを保持でき、多行や整形テキストに適しています。

**クォートなし文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、空白
- `//`で始めない（コメント扱いになるため）
- `true`、`false`、`null`で始めない（ブール値やnullと誤認される可能性あり）

**トリプルクォート文字列のガイドライン：**

- トリプルクォートに隣接するクォートはエスケープするか`~`区切りを使う
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

HOCONの文字列クォート規則の詳細は[HOCON仕様](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings)を参照してください。

EMQXのトリプルクォート文字列の拡張については[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)を参照してください。

#### String("constant")

定数文字列で、単一値の列挙型（Enum）として機能します。特定の設定やモードの静的値に使います。

#### Boolean

`true`または`false`（大文字・小文字区別あり）

#### Float

浮動小数点数。例：`3.14`、`-0.001`

#### Duration

人間が読みやすい形式の時間間隔。例やフォーマットの説明あり。

#### Duration(s)

秒単位の精度を持つDuration型。詳細と例あり。

#### Secret

パスワードやトークンなど機密情報用の型。用途と重要性の説明あり。

### 複合データ型

EMQXのHOCON設定で使われる複合型は、他の複合型やプリミティブ値を含むデータ構造を表現します。階層的かつ柔軟なデータ表現を可能にします。

#### Struct `Struct(name)`

中括弧`{}`で囲まれたフィールドを持つ構造体。`name`はスキーマで指定されたフィールド名と型を参照します。

#### Map `Map($name->Type)`

Structに似ていますが、フィールド名が事前定義されていません。

`$name`はドット`.`を含まない任意の文字列キーを表し、エンティティや属性名を示します。`Type`はマップ内のすべての値が同一型であることを示し、均質なデータ集合を表現します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを取るユニオン型。構造体のフィールドが複数の型のいずれかであることを示します。例：`String(infinity)`または`Duration`のどちらか。

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

Variformは文字列操作やランタイム評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内に埋め込んで動的に文字列操作を行うための専用ツールです。

::: tip
Variform式は特定の設定項目にのみ適用されます。指定がない限り使用しないでください。
:::

::: tip NULL値について
Variform式では値のバインディング参照や部分式の評価が未定義値になることがあり、これは空文字列（`''`）で表されます。

JSONで`null`のフィールドは未定義値（`''`）として扱われ、文字列`"null"`とは異なります。
:::

#### 構文

例：

```js
function_call(clientid, another_function_call(username))
```

これは`clientid`と`username`を組み合わせて新しい文字列値を生成します。

Variformは以下のリテラルをサポートします。

- ブール値：`true`または`false`
- 整数：例`42`
- 浮動小数点数：例`3.14`
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲むASCII文字列
- 配列：`[`と`]`で囲み、カンマ`,`で区切る要素
- 変数：事前定義された値の参照（例：`clientid`）
- 関数：事前定義関数（例：`concat([...])`）

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
            # client IDの最初の'-'より前の部分を抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.groupとして設定
            set_as_attr = group
        }
    ]
}
```

::: tip
式内で`unescape`関数を使う場合、HOCON設定でトリプルクォート`"""`文字列を使うと二重エスケープを避けられます。

例：

```
#### 複数行のclient IDの最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を備えています。これらは抽出したデータの操作や整形に使えます。例えば`lower()`、`upper()`、`concat()`は文字列のフォーマット調整に、`hash()`や`hash_to_range()`はハッシュ化や範囲マッピングに使います。

利用可能な関数一覧：

- **文字列関数**：
  - [文字列操作関数](../../develop/data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`は任意の中間非文字列値を文字列に変換します。
- **配列関数**：`nth/2`など（[参照](../../develop/data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)）
- **乱数関数**：`rand_str`、`rand_int`
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_decode(Data, 'no_padding')](../../develop/data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)（6.0.2以降）
  - [base64_decode(Data, 'no_padding', 'urlsafe')](../../develop/data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)（6.0.2以降）
  - [base64_encode(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - [base64_encode(Data, 'no_padding')](../../develop/data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)（6.0.2以降）
  - [base64_encode(Data, 'no_padding', 'urlsafe')](../../develop/data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)（6.0.2以降）
  - `int2hexstr(Integer)`：整数を16進文字列に変換。例：15は`F`（大文字）。
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：`Algorithm`は`md4`、`md5`、`sha`（`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`のいずれか。
  - `hash_to_range(Input, Min, Max)`：`sha256`でハッシュ化し、`Min`から`Max`の整数範囲にマッピング（`Min <= X <= Max`）。
  - `map_to_range(Input, Min, Max)`：入力を`Min`から`Max`の整数範囲にマッピング。
- **比較関数**：
  - `num_eq(A, B)`：2つの数値が同じなら`true`、そうでなければ`false`
  - `num_neq(A, B)`：2つの数値が異なれば`true`、そうでなければ`false`
  - `num_gt(A, B)`：`A > B`なら`true`、そうでなければ`false`
  - `num_gte(A, B)`：`A >= B`なら`true`、そうでなければ`false`
  - `num_lt(A, B)`：`A < B`なら`true`、そうでなければ`false`
  - `num_lte(A, B)`：`A <= B`なら`true`、そうでなければ`false`
  - `str_eq(A, B)`：2つの文字列が同じなら`true`、そうでなければ`false`
  - `str_neq(A, B)`：2つの文字列が異なれば`true`、そうでなければ`false`
  - `str_gt(A, B)`：辞書順で`A > B`なら`true`、そうでなければ`false`
  - `str_gte(A, B)`：辞書順で`A >= B`なら`true`、そうでなければ`false`
  - `str_lt(A, B)`：辞書順で`A < B`なら`true`、そうでなければ`false`
  - `str_lte(A, B)`：辞書順で`A <= B`なら`true`、そうでなければ`false`
  - `is_empty_var(V)`：変数が空か判定。Variformで空とは未定義値（`undefined`）、JSONの`null`（文字列`"null"`は除く）、空文字列`""`を指す
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`を返す。文字列も受け付け、入力が文字列なら出力も文字列になる。

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。ただしOS環境変数は`EMQXVAR_`プレフィックスを付けて読み込む。例：`getenv('FOO_BAR')`は`EMQXVAR_FOO_BAR`を読み込む。読み込み後は値は不変。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスでネスト構造を辿って値を抽出。例：`username`がJSONオブジェクトなら`json_value(username, 'shop.floor')`でフィールドにアクセス。
  - `jwt_value(Data, Path)`：JWTトークンのペイロードをデコードし、ドット区切りパスでクレーム値を抽出。例：`password`がカスタムクレームを持つJWTなら`jwt_value(password, 'client_attrs.unitid')`でネスト値にアクセス。
  - `is_jwt(Data)`（6.2.3以降）：`Data`がJWSコンパクト形式のJWT構造か判定。3つのドット区切りBase64URLデコード可能なセグメントを持ち、ヘッダーJSONに`alg`フィールドがある場合のみ`true`。署名検証やペイロード検査は行わず、未定義、null、空文字列、5セグメントのJWE、破損値は`false`。

#### 条件式

Variform式は包括的な制御フローを持ちませんが、以下の関数で基本的な値の選択が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または非空文字列なら`ThenExpression`を返し、それ以外は`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の非空引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の非空要素を返す。

#### エラー処理

Bashなどのスクリプト環境のデフォルト動作と同様、Variform式は未束縛変数参照や実行時例外が発生した場合は空文字列`""`を返す設計です。

- 未束縛変数：定義されていない変数参照は空文字列として評価される。
- 実行時例外：関数の誤用や型不一致などの例外は空文字列を返す。例：配列インデックス範囲外。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclient IDのプレフィックスを抽出。
- `strlen(username, 0, 5)`：usernameの部分文字列を抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclient IDから数字を抽出。空文字列なら`'000'`を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid`が`foo.`で始まれば`foo`、そうでなければ`bar`を返す。
