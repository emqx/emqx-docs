# 設定ファイル

ユーザーは設定ファイルまたは環境変数を用いてEMQXを設定できます。本章では主にEMQXの設定ファイルについて紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目や解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXのインストール後、設定およびランタイムデータを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つに分類されます。

- **静的設定ディレクトリ（`etc`）**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ（`data/configs`）**：書き込み可能で、ランタイム中に生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ（`etc`）

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイやアップグレード時に編集され、安定性を確保するためにランタイムでは読み取り専用となります。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                           | パス               |
| ---------------------------------------- | ------------------ |
| RPMまたはDEBパッケージでインストール     | `/etc/emqx`        |
| Dockerコンテナで実行                      | `/opt/emqx/etc`    |
| ポータブル圧縮パッケージから展開         | `./etc`            |

### 動的設定ディレクトリ（`data/configs`）

ランタイム中、EMQXはダッシュボード、REST API、CLIを通じて動的な再設定を許可しています。これらのツールで行われた変更は、セッションを超えて永続化されるように`data/configs`ディレクトリに保存されます。このディレクトリの場所もインストール方法により異なります。

| インストール方法                           | パス                     |
| ---------------------------------------- | ------------------------ |
| RPMまたはDEBパッケージでインストール     | `/var/lib/emqx/configs`  |
| Dockerコンテナで実行                      | `/opt/emqx/data/configs` |
| ポータブル圧縮パッケージから展開         | `./data/configs`         |

::: tip
設定の`node.data_dir`を変更するか、環境変数`EMQX_NODE__DATA_DIR`を設定することで、データディレクトリの場所を変更可能です。ただし、クラスター運用時は全ノードで同じディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、もし重複した場合は事前に定められた上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションでは詳細なリファレンスを提供していますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合、設定例は`etc/emqx/examples`ディレクトリにあります。
- DockerコンテナでEMQXを実行している場合、設定例は`opt/emqx/etc/examples`ディレクトリにあります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれ、ランタイム中により上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、後からダッシュボードUIでより複雑な設定に上書きすることができます。

`node`や`cluster`のような不変の設定については、デプロイ固有の値でランタイム中に変更しない場合、環境変数を使用することも可能です。詳細は[Environment Variables](#environment-variables)および[Config Override Rules](#config-override-rules)をご参照ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、そのノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリ内の`cluster.hocon`ファイルはクラスター全体の設定項目を含みます。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、ノードは自動的に他のノードから`cluster.hocon`をコピーして適用します。このため、手動での編集は推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層の詳細は[Config Override Rules](#config-override-rules)をご覧ください。

EMQX 5.1以降、クラスター設定の変更時に`cluster.hocon`ファイルのバックアップが作成されます。バックアップはノードのローカル時間でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`の重要なシステム設定に引き続き使用可能です。このファイルは`base.hocon`や`cluster.hocon`より優先度が高いですが、環境変数よりは低い優先度です。意図的にこの優先度を利用し、パッケージのアップグレードでこのファイルのデフォルトが更新されることを理解している場合を除き、変更は避けてください。

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

HOCONは人間が読みやすいデータフォーマットであり、JSONのスーパーセットです。継承や結合、引用符などの機能により、設定作業をさらに簡素化します。

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

このイカのようなフラット形式は旧バージョンとの互換性を保ちつつ、使い方が異なります。

HOCONでは文字列の両端に引用符を付けることを推奨しています。特殊文字を含まない文字列は引用符なしでも構いません（例：`foo`、`foo_bar`）。一方、フラット形式では`=`の右側のすべての文字を値として扱います。

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

設定項目と環境変数は以下のルールで変換されます。

1. 設定ファイルの`.`区切りは環境変数で使えないため、EMQXでは`__`（ダブルアンダースコア）を区切りに使用します。
2. 他の環境変数と区別するため、`EMQX_`というプレフィックスを付けます。
3. 環境変数の値はHOCON値として解析されるため、複雑なデータ型も渡せます。`:`、`=`、`#`などのHOCON特殊文字を含む値はダブルクォートで囲む必要があります。特に`#`はHOCONの行コメントを開始するため、クォートしないと`#`以降がコメントとして無視されます。

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

パスワードなどに`#`を含む場合、`#`はHOCONの行コメント開始文字なので、以下のようにすると`#`以降がコメントとして無視されます。

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

この場合、パスワードは`MQtt`として解析され、`#123`は削除されます。文字列として正しく渡すには、**HOCONレベルの**ダブルクォートで囲み、パーサーに`"MQtt#123"`として認識させます。

```bash
# 正しい例 — HOCONパーサーには "MQtt#123" として渡される
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# シェル用に内側のクォートをエスケープした例
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

`:`や`=`を含む値も同様です。URLエンコード（例：`%23`）は無効で、EMQXは環境変数の値をURLデコードしません。

:::

::: tip なぜ一部の非引用値は通り、一部は通らないのか

EMQXは環境変数の値を`fake_key=<value>`としてHOCONパーサーに渡します。パース成功すれば解析結果を使い、失敗すれば生文字列を使います。例えば、`EMQX_..._PASSWORD="abc#def"`は有効なHOCONで`#def`がコメントになるため`abc`となりますが、`EMQX_..._PASSWORD=".abc#def"`は無効なHOCONなので生文字列`.abc#def`が使われます。HOCONクォートで囲むと動作が確定的になります。

:::

::: tip

EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。既知のルートパスに未知のフィールド名が設定されると、起動時に以下のような`warning`ログを出力します。例えば、`enable`を誤って`enabled`と設定した場合：

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

つまり、`base.hocon`の設定は最も優先度が低く、上位のファイルで上書き可能です。`EMQX_`で始まる環境変数は最も優先度が高いです。

::: tip
5.8.4以前のバージョンでは`base.hocon`ファイルは存在しませんでした。優先順位は同じですが`base.hocon`は含まれません。
:::

ダッシュボードUI、HTTP API、CLIからの変更はランタイム中に`cluster.hocon`に永続化され即時反映されます。ただし、`emqx.conf`や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、**`emqx.conf`と`cluster.hocon`で設定を重複させないでください**。

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`が存在し、設定優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。
2. これらのバージョンから最新にアップグレードしても優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。
3. `cluster-override.conf`機構はバージョン5.1で廃止されました。
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

### リスト要素の上書き

EMQXの配列表現は以下の2種類があります。

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

# 1番目の要素の`enable`フィールドだけを上書き
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

## 以下の書き方では1番目の要素の`enable`以外のフィールドがすべて失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定のグループ化を表す概念です。リスナーに`zone`フィールドでゾーン名を設定すると、そのリスナーに接続したMQTTクライアントはゾーンの設定を継承し、グローバル設定を上書きできます。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐づいています。`default`ゾーンは論理的なグループであり、設定ファイルには存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続やセッション設定。特定ゾーンでMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検知。
- `durable_sessions`：セッション永続化設定。特定ゾーンでMQTTセッションの永続化を有効化など。

EMQX 5系のデフォルト設定ファイルにはゾーンは含まれていません。これは4系の`internal`と`external`という2つのデフォルトゾーンと異なります。

ゾーンを作成するには設定ファイルに以下のように定義します。

```bash
zones {
  # 複数のゾーンを定義可能
  my_zone1 {
    # ゾーンはグローバル設定と同じスキーマを共有
    mqtt {
      # このゾーンの接続に対して大きなパケットサイズを許可
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

リスナーの`zone`フィールドに作成済みゾーン名を設定して紐づけます。

```bash
listeners.tcp.default {
    bind = 1883
    zone = my_zone1
    ...
}
```

## Configuration-as-Codeのベストプラクティス

EMQXの設定をソース管理や自動化システムで管理する場合、以下の原則を推奨します。

- Configuration-as-Codeの設定は`base.hocon`に記述する。
- `cluster.hocon`を手動で編集したり、自分の`cluster.hocon`をマウントしない。
- `emqx.conf`は優先度が高くアップグレード時の影響があるため、理解した上で変更する。
- ダッシュボード、API、CLIで変更しない単純な上書きは環境変数で行う。

Configuration-as-Codeの真実の情報源は`base.hocon`です。ノード起動時に静的設定ディレクトリから読み込まれ、パッケージングやイメージビルド、構成管理、GitOpsワークフローで管理可能です。ダッシュボード、REST API、CLIからのランタイム変更は`cluster.hocon`に永続化され、`base.hocon`の上に重ねられます。

例えば、リスナー、ログ、認証、認可、データ統合のベースラインは`base.hocon`に保持できます。

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

`cluster.hocon`はConfiguration-as-Codeの真実の情報源として使わないでください。EMQXがランタイムで管理し、ダッシュボード、REST API、CLIが書き換え、バックアップを作成し、クラスター内でコピーされます。手動編集やマウントはランタイム更新と競合し、上書きされる恐れがあります。

`emqx.conf`は配布パッケージに同梱されるベースライン設定ファイルです。変更しないことでアップグレードが容易になり、新しいEMQXバージョンの保守的なデフォルト変更を取り込みやすくなります。`emqx.conf`で設定した項目は`base.hocon`や`cluster.hocon`より優先度が高いため、ランタイム変更が反映されているように見えてもノード再起動後に元に戻ることがあります。意図的にその動作を利用する場合のみ使用してください。

環境変数は最も優先度が高く、単純なデプロイ固有の値やランタイムで変更すべきでない値に適しています。

```bash
export EMQX_NODE__NAME='emqx@node1.example.net'
export EMQX_NODE__COOKIE='mysecret'
export EMQX_CLUSTER__DISCOVERY_STRATEGY='static'
export EMQX_CLUSTER__STATIC__SEEDS='["emqx@node1.example.net", "emqx@node2.example.net"]'
```

環境変数はすべての設定ファイルを上書きするため、オペレーターが後からダッシュボードやAPI、CLIで調整することが想定される設定には使わないでください。

## スキーマ

HOCONオブジェクトの型安全性を高めるため、EMQXはスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などに利用されます。

[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)はスキーマから生成されています。

::: tip
ゾーンの設定スキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルに登場するプリミティブ型はほぼ自明で、簡単な説明で十分です。以下は代表的な型の一覧です。

#### Integer（整数）

整数値を表します。例：`42`、`-3`、`0`

#### Integer(Min..Max)

指定範囲内の整数。例えば`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義済みのシンボルのいずれかのみ許容します。例：`Enum(debug,info,warning,error)`はログレベルを表します。

#### String（文字列）

文字の連続を表し、複数の形式をサポートします。

- **非引用文字列**：特殊文字を含まない単純な識別子や名前に適します（詳細は後述）。
- **引用文字列**：特殊文字や空白を含む場合はダブルクォート（`"`）で囲み、必要に応じてバックスラッシュ（`\`）でエスケープします。例：`"line1\nline2"`。
- **三重引用文字列**：`"""`で囲み、エスケープ不要（`\`を除く）で複雑な内容を含められます。三重引用符に隣接するクォートはエスケープが必要です。
- **インデント付き三重引用文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを許容し、複数行や整形テキストに適します。

**非引用文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、空白
- `//`で始めない（コメント開始と誤認されるため）
- `true`、`false`、`null`で始めない（ブールやnullと誤認されるため）

**三重引用文字列のガイドライン：**

- 三重引用符に隣接するクォートはエスケープするか、`~`区切りを使う
- 複数行文字列はスペースによるインデントを許容（タブ不可）
- インデントレベルは最小の先頭スペース数で決定

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

HOCONの文字列引用規則の詳細は[HOCON仕様](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings)をご覧ください。

EMQX独自のインデント付き三重引用文字列の詳細は[emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats)を参照してください。

#### String("constant")

定数文字列で、単一値の列挙型（Enum）として機能します。特定の設定やモードの静的値に使います。

#### Boolean（真偽値）

`true`または`false`（大文字小文字区別あり）

#### Float（浮動小数点数）

小数を含む数値。例：`3.14`、`-0.001`

#### Duration（期間）

人間が読みやすい形式の時間間隔。例やフォーマットの説明。

#### Duration(s)

秒単位の精度を持つDuration型。詳細と例。

#### Secret（シークレット）

パスワードやトークンなど機密情報用の型。使用方法と重要性の説明。

### 複合データ型

EMQXのHOCON設定で使われる複合型は、他の複合型やプリミティブ型を含むデータ構造を表現します。階層的で柔軟なデータ表現を可能にします。

#### Struct `Struct(name)`

波括弧`{}`で囲まれたフィールドを持つ構造体。`name`はスキーマで定義されたフィールド名と型の参照です。

#### Map `Map($name->Type)`

Structに似ていますが、フィールド名が事前定義されていません。

`$name`は任意の文字列（`.`を含まない）をキーとして表し、`Type`はすべての値が同じ型であることを示します。均一なデータコレクションを作れます。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを許容するユニオン型。1つのフィールドが複数の型のいずれかを取れることを示します。例：`String(infinity)`または`Duration`。

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

Variformは文字列操作やランタイム評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQX設定内に埋め込んで文字列操作を動的に行うための専門ツールです。

::: tip
Variform式は特定の設定項目にのみ適用可能です。指定がない限り使用しないでください。
:::

::: tip NULL値について
Variform式では、値のバインディング参照や部分式の評価が未定義値になることがあり、これは空文字列（`''`）で表されます。

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
- 配列：`[`と`]`で囲み、カンマ`,`で区切る
- 変数：事前定義された値（例：`clientid`）
- 関数：組み込み関数（例：`concat([...])`）

Variformは以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のアンエスケープは`unescape`関数を呼び出す）

以下はVariform式を含む設定例です。

```js
mqtt {
    client_attrs_init = [
        {
            # clientidの最初の`-`までのプレフィックスを抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.groupに設定
            set_as_attr = group
        }
    ]
}
```

::: tip
アンエスケープ関数が必要な場合、HOCON設定で三重引用符（`"""`）文字列を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行のclient IDの1行目を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 組み込み関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を提供します。これらは抽出データの操作や整形に使えます。例：`lower()`、`upper()`、`concat()`、`hash()`、`hash_to_range()`など。

利用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../../develop/data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`（任意の中間値を文字列に変換）
- **配列関数**：[nth/2](../../develop/data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
- **乱数関数**：`rand_str`、`rand_int`
- **スキーマレスエンコード/デコード関数**：
  - [bin2hexstr(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_decode(Data, 'no_padding')](../../develop/data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)  (6.0.2以降)
  - [base64_decode(Data, 'no_padding', 'urlsafe')](../../develop/data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)  (6.0.2以降)
  - [base64_encode(Data)](../../develop/data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - [base64_encode(Data, 'no_padding')](../../develop/data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string) (6.0.2以降)
  - [base64_encode(Data, 'no_padding', 'urlsafe')](../../develop/data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string) (6.0.2以降)
  - `int2hexstr(Integer)`：整数を16進文字列に変換（例：15 → 'F'）
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：`Algorithm`は`md4`、`md5`、`sha`（`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`のいずれか
  - `hash_to_range(Input, Min, Max)`：`sha256`でハッシュ化し、`Min`〜`Max`の整数にマッピング（`Min <= X <= Max`）
  - `map_to_range(Input, Min, Max)`：入力を`Min`〜`Max`の整数にマッピング
- **比較関数**：
  - `num_eq(A, B)`：数値が等しいなら`true`、そうでなければ`false`
  - `num_neq(A, B)`：数値が異なれば`true`、そうでなければ`false`
  - `num_gt(A, B)`：`A > B`なら`true`、そうでなければ`false`
  - `num_gte(A, B)`：`A >= B`なら`true`、そうでなければ`false`
  - `num_lt(A, B)`：`A < B`なら`true`、そうでなければ`false`
  - `num_lte(A, B)`：`A <= B`なら`true`、そうでなければ`false`
  - `str_eq(A, B)`：文字列が等しいなら`true`、そうでなければ`false`
  - `str_neq(A, B)`：文字列が異なれば`true`、そうでなければ`false`
  - `str_gt(A, B)`：辞書順で`A > B`なら`true`、そうでなければ`false`
  - `str_gte(A, B)`：辞書順で`A >= B`なら`true`、そうでなければ`false`
  - `str_lt(A, B)`：辞書順で`A < B`なら`true`、そうでなければ`false`
  - `str_lte(A, B)`：辞書順で`A <= B`なら`true`、そうでなければ`false`
  - `is_empty_var(V)`：変数が空か判定。Variformの空は未定義値（`undefined`）、JSONの`null`（文字列`"null"`ではない）、空文字列`""`を含む
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`を返す。文字列も受け付け、入力が文字列なら出力も文字列

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。OS環境変数は`EMQXVAR_`プレフィックス付きで読み込み、読み込み後は不変。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りのパスで値を抽出。例：`username`がJSONオブジェクトなら`json_value(username, 'shop.floor')`
  - `jwt_value(Data, Path)`：JWTトークンのペイロードからクレーム値を抽出。例：`password`がカスタムクレームを持つJWTなら`jwt_value(password, 'client_attrs.unitid')`
  - `is_jwt(Data)`（6.2.3以降）：`Data`がJWSコンパクト形式のJWTか判定。3つのドット区切りBase64URLデコード可能なセグメントを持ち、ヘッダーJSONに`alg`フィールドがあれば`true`。署名検証やペイロード検査は行わず、未定義、`null`、空文字列、5セグメントのJWE、破損値は`false`。

#### 条件式

Variform式は包括的な制御構造を持ちませんが、以下の関数で基本的な条件分岐が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または空でない文字列なら`ThenExpression`を返し、そうでなければ`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の空でない配列要素を返す。

#### エラー処理

Bashなどのスクリプト環境と同様に、Variform式は未定義変数参照や実行時例外時に空文字列（`""`）を返す設計です。

- 未定義変数：定義されていない変数参照は空文字列として評価される。
- 実行時例外：関数の誤用や型エラーなどの例外発生時は空文字列を返す。例：配列インデックス範囲外。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclientidのプレフィックスを抽出
- `strlen(username, 0, 5)`：usernameの部分文字列を抽出
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclientidから数字を抽出。空なら`'000'`を返す
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：clientidが`foo.`で始まれば`foo`、そうでなければ`bar`を返す
