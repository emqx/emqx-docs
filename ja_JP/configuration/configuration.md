# 設定ファイル

ユーザーは設定ファイルまたは環境変数を使ってEMQXを設定できます。本節では主にEMQXの設定ファイルについて紹介し、EMQXで最もよく使われる機能の基本的な設定方法を説明します。詳細な設定項目や解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定およびランタイムデータを管理するためのディレクトリ群が作成されます。これらのディレクトリは大きく2つのカテゴリに分かれています。

- **静的設定ディレクトリ (`etc`)**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ (`data/configs`)**：書き込み可能で、ランタイムで生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ (`etc`)

`etc`ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイやアップグレード時に変更され、ランタイム中は安定性を確保するため読み取り専用となります。`etc`ディレクトリの場所はインストール方法によって異なります。

| インストール方法                        | パス               |
| ------------------------------------- | ------------------ |
| RPMまたはDEBパッケージでインストール | `/etc/emqx`        |
| Dockerコンテナで実行                  | `/opt/emqx/etc`    |
| ポータブル圧縮パッケージから展開     | `./etc`            |

### 動的設定ディレクトリ (`data/configs`)

ランタイム中、EMQXはダッシュボード、REST API、CLIを通じて動的な再設定を許可しています。これらのツールで行われた変更は永続化のため`data/configs`ディレクトリに保存されます。このディレクトリの場所もインストール方法に依存します。

| インストール方法                        | パス                     |
| ------------------------------------- | ------------------------ |
| RPMまたはDEBパッケージでインストール | `/var/lib/emqx/configs`  |
| Dockerコンテナで実行                  | `/opt/emqx/data/configs` |
| ポータブル圧縮パッケージから展開     | `./data/configs`         |

::: tip
`node.data_dir`設定や環境変数`EMQX_NODE__DATA_DIR`を変更することで、データディレクトリの場所を変更可能です。ただし、クラスター運用時は全ノードで同一のディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することは推奨されませんが、万が一重複した場合はあらかじめ定義された上書きルールに従って解決されます。詳細は[Config Override Rules](#config-override-rules)をご覧ください。

## 設定例

[Schema](#schema)セクションに詳細なリファレンスがありますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでインストールした場合は、`etc/emqx/examples`ディレクトリに設定例があります。
- Dockerコンテナで実行している場合は、`opt/emqx/etc/examples`ディレクトリに設定例があります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc`ディレクトリに`base.hocon`というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれており、ランタイム中に上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、その後ダッシュボードUIからより複雑な設定で上書きすることができます。

`node`や`cluster`のような不変設定については、デプロイ固有でランタイム中に変更すべきでない場合、環境変数を使うことも可能です。詳細は[Environment Variables](#environment-variables)および[Config Override Rules](#config-override-rules)をご参照ください。

::: tip
`base.hocon`ファイルはクラスター間で同期されず、そのノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs`ディレクトリ内の`cluster.hocon`ファイルにはクラスター全体の設定項目が含まれています。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、ノードは自動的にクラスター内の他のノードから`cluster.hocon`をコピーして適用します。そのため、このファイルを手動で編集することは推奨されません。

このファイルの設定は`base.hocon`の設定の上に適用されます。設定の上書き階層の詳細は[Config Override Rules](#config-override-rules)をご覧ください。

EMQX 5.1以降、クラスター設定に変更があると、`cluster.hocon`の上書き前にバックアップが作成されます。バックアップはノードのローカル時刻でタイムスタンプが付けられ、最大10個まで保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf`ファイルは`node`や`cluster`の重要なシステム設定に利用可能なままです。このファイルは`base.hocon`や`cluster.hocon`より優先度が高いですが、環境変数よりは低い優先度です。意図的にこの優先度を利用し、パッケージアップグレード時にこのファイルのデフォルトが更新されることを理解している場合を除き、変更は避けてください。

設定の上書きについての詳細は[Config Override Rules](#config-override-rules)をご参照ください。

## 設定パス

EMQXでは設定値をドット区切りのパスで参照できます。これはツリー構造のようなもので、ルート（常にStruct）から始まり、各セグメントはフィールド名またはMapのキーを指します。配列要素の場合は1始まりのインデックスを使用します。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0以降、設定ファイルのフォーマットとして[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間に読みやすいデータフォーマットでJSONのスーパーセットです。継承や結合、引用符などの機能により設定作業をさらに簡素化します。

**HOCON構文例：**

JSONに似たオブジェクトとして表現可能です。

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

このフラット形式は以前のEMQXバージョンとの後方互換性がありますが、使い方が異なります。

HOCONでは文字列の両端に引用符を付けることを推奨します。特殊文字を含まない文字列は引用符なしでもよく、例えば`foo`や`foo_bar`などです。一方、フラット形式では`=`の右側の全てを値として扱います。

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

1. 設定ファイルの`.`区切りは環境変数では使えないため、EMQXは区切りに`__`（ダブルアンダースコア）を使用します。
2. 他の環境変数と区別するため、環境変数名の先頭に`EMQX_`を付加します。
3. 環境変数の値はHOCON値として解析されるため、複雑なデータ型も渡せます。`:`、`=`、`#`などのHOCON特殊文字を含む値は、パーサーが文字列として扱うように必ずダブルクォート`"`で囲む必要があります。特に`#`はHOCONの行コメントを開始するため、引用符なしでは`#`以降がコメントとして無視されます。

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

::: warning `#`, `:`, `=`を含む値

パスワードなどに`#`を含む場合、`#`はHOCONの行コメント開始文字のため、以下のようにすると`#`以降が削除されます。

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

この場合、パスワードは`MQtt`として解釈され、`#123`はコメントとして無視されます。リテラルとして渡すには、**HOCONレベルの**ダブルクォート（シェルのクォートではなく）で囲み、パーサーに`"MQtt#123"`として認識させる必要があります。

```bash
# 正しい例 — HOCONパーサーに渡される値は "MQtt#123"
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# シェル用に内側のクォートをエスケープした例
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

`:`や`=`を含む値も同様です。URLエンコード（例：`%23`）は無効で、EMQXは環境変数値をURLデコードしません。

:::

::: tip なぜ一部の非引用値は通り、一部は通らないのか

EMQXは環境変数値を`fake_key=<value>`としてHOCONパースを試みます。成功すれば解析結果を使い、失敗すれば生の文字列を使います。例えば、`EMQX_..._PASSWORD="abc#def"`は有効なHOCONで`#def`がコメント扱いされ`abc`となりますが、`EMQX_..._PASSWORD=".abc#def"`は無効なHOCONなので生文字列`.abc#def`が使われます。HOCONクォートで囲むと動作が決定的になります。

:::

::: tip

EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。  

既知のルートパスに未知のフィールド名が設定された場合、起動時に`warning`ログを出力します。例えば`enable`を誤って`enabled`と設定すると以下のように出力されます。

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

::: tip
EMQX 6.3.0以降、`EMQX_FEATURES`は[feature gates](../deploy/feature-gates.md)用の特別な起動環境変数です。HOCON設定パスに対応せず、`cluster.hocon`にも保存されず、EMQX起動時のみ解決されます。
:::

### ブート時環境変数

ほとんどの`EMQX_`プレフィックス環境変数は`emqx.conf`の設定を上書きしますが、設定ファイル解析前にEMQX自体の動作を制御する変数もあります。

- `EMQX_FEATURES`：ノードが起動するアプリケーションセットを選択（例：`FULL`や`ESSENTIAL`）。
- `EMQX_SECURITY_PROFILE`：ノード全体のセキュリティプロファイルを選択（`legacy`または`hardened`）。

EMQX 6.3.0以降、`emqx`コマンドは実行時に`etc/emqx.env`から環境変数を読み込みます。サービス起動、フォアグラウンド起動、`emqx ctl`実行時も含みます。RPM/DEBインストールでは`/etc/emqx/emqx.env`にあります。システムdユニットを編集する代わりにこのファイルでブート時環境変数を設定してください。

- ファイル内の設定は環境から継承した変数を上書きします。
- パッケージアップグレード時も編集内容は保持されます。
- 配布ファイルにはコメントアウトされたブート時変数の例が記載されています（例：`#KEY="${KEY:-default}"`）。式を変更せずにアンコメントすると、変数が空または未設定の場合にデフォルト値が使われます。既存値を上書きするには`KEY=value`の形で書き換えてください。
- ブート時環境変数を変更したらEMQXノードを再起動してください。
- 通常の`EMQX_`プレフィックス変数（例：`EMQX_NODE__COOKIE`）もこのファイルに設定可能です。

## 設定上書きルール

EMQXでは設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が前の値を上書きします。
- 上位レベルの設定が下位レベルの設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり、`base.hocon`の設定は最も優先度が低く、上位のファイルで上書き可能です。`EMQX_`で始まる環境変数が最も高い優先度を持ちます。

::: tip
5.8.4以前のバージョンでは`base.hocon`は存在しませんでした。優先順位は同じですが`base.hocon`はありません。
:::

EMQXダッシュボードUI、HTTP API、CLIからの変更はランタイム中に`cluster.hocon`に永続化され即時反映されます。しかし、同じ設定項目が`emqx.conf`や環境変数で異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、**`emqx.conf`と`cluster.hocon`で設定を重複させないでください**。

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では`cluster-override.conf`ファイルが存在し、設定優先順位は`emqx.conf < ENV < HTTP API (cluster-override.conf)`でした。  
2. これらのバージョンから最新にアップグレードしても優先順位は変わらず、互換性維持のため`cluster.hocon`は作成されません。  
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
- マップ形式（サブスクライブ用、例：`{"1"=1, "2"=2, "3"=3}`）

以下の3つの形式は同等です。

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

# 最初の要素の`enable`フィールドを以下のように上書き可能
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

## 以下の方法では最初の要素の`enable`以外の全フィールドが失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。リスナーに`zone`フィールドでゾーン名を設定すると、そのゾーンに紐づく設定が適用されます。ゾーンに紐づくリスナーに接続したMQTTクライアントは、そのゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは`default`という名前のゾーンに紐づいています。`default`ゾーンは論理的なグループであり、設定ファイル上には存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続やセッション設定。特定ゾーンでMQTTメッセージの最大パケットサイズを大きく許可するなど。
- `force_shutdown`：強制シャットダウンポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：セッション永続化設定。特定ゾーンでMQTTセッションの永続ストレージを有効化など。

EMQX 5系のデフォルト設定ファイルにはゾーンは含まれておらず、4系の`internal`と`external`の2つのデフォルトゾーンとは異なります。

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

リスナーで`zone`フィールドを設定し、作成済みのゾーンに紐づけます。

```bash
listeners.tcp.default {
    bind = 1883
    zone = my_zone1
    ...
}
```

## Configuration-as-Codeのベストプラクティス

ソース管理や自動化システムでEMQX設定を管理する場合、以下のルールを推奨します。

- Configuration-as-Codeの設定は`base.hocon`に記述する。
- `cluster.hocon`は手動編集や独自マウントをしない。
- `emqx.conf`は優先度が高くアップグレード影響もあるため、理解した上でのみ変更する。
- ダッシュボード、API、CLIでランタイム変更しない単純な上書きは環境変数で行う。

Configuration-as-Codeの真の情報源は`base.hocon`です。ノード起動時に静的設定ディレクトリから読み込まれ、パッケージングやイメージビルド、構成管理、GitOpsワークフローで管理可能です。ダッシュボード、REST API、CLIからのランタイム変更は`cluster.hocon`に永続化され、`base.hocon`の上に重ねられます。

例として、リスナー、ログ、認証、認可、データ統合のベースラインを`base.hocon`に保持できます。

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

`cluster.hocon`はランタイムにEMQXが管理するファイルであり、ダッシュボード、REST API、CLIが書き換え、バックアップを作成し、クラスター内でコピーされます。手動編集やマウントはランタイム更新と競合し、上書きされる可能性があります。

`emqx.conf`は配布パッケージに同梱されるベースライン設定ファイルです。変更せずにおくと、新しいEMQXバージョンの保守的なデフォルト変更を取り込みやすくなります。`emqx.conf`で設定した項目は`base.hocon`や`cluster.hocon`より優先度が高いため、ランタイム変更が反映されてもノード再起動後に元に戻ることがあります。意図的にこの挙動を利用する場合のみ使用してください。

環境変数は最も優先度が高く、特にデプロイ固有の単純な値やランタイムに変更すべきでない値に適しています。

```bash
export EMQX_NODE__NAME='emqx@node1.example.net'
export EMQX_NODE__COOKIE='mysecret'
export EMQX_CLUSTER__DISCOVERY_STRATEGY='static'
export EMQX_CLUSTER__STATIC__SEEDS='["emqx@node1.example.net", "emqx@node2.example.net"]'
```

環境変数はすべての設定ファイルを上書きするため、オペレーターが後でダッシュボード、API、CLIから調整することが想定される設定には使わないでください。

## スキーマ

HOCONオブジェクトの型安全性を高めるため、EMQXはスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などに利用されます。

[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)はスキーマから生成されています。

::: tip
ゾーンの設定スキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば`zones.my_zone1.mqtt {...}`は`mqtt {...}`と同じスキーマです。
:::

### プリミティブデータ型

プリミティブデータ型はほぼ自明であり、最小限の説明で十分です。以下は代表的な型の一覧です。

#### Integer

整数を表します。例：`42`、`-3`、`0`。

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf`は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型で、定義されたシンボルのいずれかのみ許容します。例：`Enum(debug,info,warning,error)`はログレベルを定義。

#### String

文字列型で、複数の形式をサポートします。

- **非引用文字列**：特殊文字を含まない単純な識別子や名前に適します（詳細は後述）。
- **引用文字列**：特殊文字や空白を含む場合にダブルクォート`"`で囲み、必要に応じてバックスラッシュ`\`でエスケープします。例：`"line1\nline2"`。
- **三重引用文字列**：`"""`で囲み、`\`以外のエスケープ不要で複雑な内容を含められます。三重引用符に隣接するクォートはエスケープが必要です。
- **インデント付き三重引用文字列**：`"""~`と`~"""`で囲み、EMQX 5.6以降で導入。設定ファイル内でインデントを保持でき、複数行や整形テキストに適します。

**非引用文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、および空白を含めない。
- `//`で始めない（コメント開始と誤認されるため）。
- `true`、`false`、`null`で始めない（ブール値やnullと誤認されるため）。

**三重引用文字列のガイドライン：**

- 三重引用符に隣接するクォートはエスケープするか`~`区切りを使う。
- 複数行文字列はスペース（タブ不可）によるインデントをサポート。インデントレベルは最小の先頭スペース数で決定。

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

定数文字列値で、単一値列挙型（Enum）として機能します。特定の設定やモードの静的値定義に使います。

#### Boolean

`true`または`false`（大文字小文字区別あり）。

#### Float

小数点を含む浮動小数点数。例：`3.14`、`-0.001`。

#### Duration

人間に読みやすい時間の長さを表します。例やフォーマットの説明。

#### Duration(s)

秒単位の精度を持つDuration型。詳細と例。

#### Secret

パスワードやトークンなどの機密情報用型。使用方法と重要性の説明。

### 複合データ型

EMQXのHOCON設定における複合データ型は、他の複合型やプリミティブ値を含むデータ構造を表現します。柔軟で階層的なデータ表現を可能にします。

#### Struct `Struct(name)`

波括弧`{}`で囲まれたフィールドを持つ構造体を表します。`name`は構造体のフィールド名や型を指定するスキーマ参照です。

#### Map `Map($name->Type)`

`Struct`に似ていますが、フィールド名が事前定義されていません。

`$name`はドット`.`を含まない任意の文字列キーを表し、エンティティや属性名を示します。`Type`はMap内のすべての値が同一型であることを示し、均一なデータコレクションを可能にします。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを取れるユニオン型を定義します。構造体のフィールドが複数の型のいずれかを許容する場合に使います。例：`String(infinity)`または`Duration`のどちらか。

#### Array `Array(Type)`

指定型の要素からなる配列を定義します。

::: tip

Mapのフィールド名が正の整数の場合、`Array`の代替表現として解釈されます。例：

```bash
myarray.1 = 74
myarray.2 = 75
```

は`myarray = [74, 75]`と解釈され、配列要素の上書きに便利です。

:::

### Variform式

Variformは文字列操作やランタイム評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内に埋め込んで動的に文字列操作を行うための専門ツールです。

::: tip
Variform式は特定の設定項目でのみ使用可能です。明示されていない限り使用しないでください。
:::

::: tip NULL値について
Variform式では値のバインディング参照や部分式の評価が未定義値となる場合、空文字列（`''`）として扱います。

JSONで`null`のフィールドは未定義値（空文字列）として扱われ、文字列`"null"`とは異なります。
:::

#### 構文例

```js
function_call(clientid, another_function_call(username))
```

これは`clientid`と`username`を組み合わせて新しい文字列値を生成する例です。

Variformは以下のリテラルをサポートします。

- ブール値：`true`または`false`
- 整数：例`42`
- 浮動小数点数：例`3.14`
- 文字列：シングルクォート`'`またはダブルクォート`"`で囲むASCII文字列
- 配列：`[`と`]`で囲み、カンマ`,`区切りの要素
- 変数：事前定義された値の参照（例：`clientid`）
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
            # client_attrs.groupとして設定
            set_as_attr = group
        }
    ]
}
```

::: tip
アンエスケープ関数が必要な場合、HOCON設定で三重引用符`"""`文字列を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行のclient IDの最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 事前定義関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を備えています。これらは抽出データの操作や整形に使えます。例えば`lower()`、`upper()`、`concat()`は文字列のフォーマット調整に、`hash()`や`hash_to_range()`はハッシュ化や範囲マッピングに利用可能です。

使用可能な関数例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数`any_to_string/1`は任意の中間非文字列値を文字列に変換します。
- **配列関数**：`nth/2`など（[参照](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)）
- **トピック関数**：
  - `topic_join(Words)`：配列のトピックレベルを`/`で結合しMQTTトピックやフィルターを生成。例：`topic_join(['devices', clientid, '#'])`は`devices/<clientid>/#`を生成。
  - `topic_join(Parent, Word)`：`Parent`トピックに`Word`を追加。`Parent`が`/`で終わる場合は区切り文字追加なし。
  - `topic_match(Topic, Filter)`：MQTTトピックがフィルターにマッチするか判定し、`true`または`false`を返す。例：`topic_match(topic, topic_join(['devices', clientid, '#']))`はクライアント固有のトピックフィルターと比較。
  - `topic_split(Topic)`：MQTTトピックを`/`で分割しトピックレベルの配列を返す。
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
  - `int2hexstr(Integer)`：整数を16進文字列にエンコード。例：15は`'F'`（大文字）。
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：`Algorithm`は`md4`、`md5`、`sha`（または`sha1`）、`sha224`、`sha256`、`sha384`、`sha512`、`sha3_224`、`sha3_256`、`sha3_384`、`sha3_512`、`shake128`、`shake256`、`blake2b`、`blake2s`のいずれか。
  - `hash_to_range(Input, Min, Max)`：`sha256`で`Input`をハッシュし、`Min`〜`Max`の範囲の整数にマッピング（`Min <= X <= Max`）。
  - `map_to_range(Input, Min, Max)`：`Input`を`Min`〜`Max`の範囲の整数にマッピング（`Min <= X <= Max`）。
- **比較関数**：
  - `num_eq(A, B)`：数値が同じなら`true`、そうでなければ`false`。
  - `num_neq(A, B)`：数値が異なれば`true`、そうでなければ`false`。
  - `num_gt(A, B)`：`A > B`なら`true`、そうでなければ`false`。
  - `num_gte(A, B)`：`A >= B`なら`true`、そうでなければ`false`。
  - `num_lt(A, B)`：`A < B`なら`true`、そうでなければ`false`。
  - `num_lte(A, B)`：`A <= B`なら`true`、そうでなければ`false`。
  - `str_eq(A, B)`：文字列が同じなら`true`、そうでなければ`false`。
  - `str_neq(A, B)`：文字列が異なれば`true`、そうでなければ`false`。
  - `str_gt(A, B)`：辞書順で`A`が`B`より後なら`true`、そうでなければ`false`。
  - `str_gte(A, B)`：辞書順で`A`が`B`より前でないなら`true`、そうでなければ`false`。
  - `str_lt(A, B)`：辞書順で`A`が`B`より前なら`true`、そうでなければ`false`。
  - `str_lte(A, B)`：辞書順で`A`が`B`より後でないなら`true`、そうでなければ`false`。
  - `is_empty_var(V)`：変数が空か判定。Variformの空は未定義値（`undefined`）、JSONの`null`（文字列`"null"`ではない）、空文字列`""`を含む。
  - `not(Bool)`：`Bool`が`false`なら`true`、`true`なら`false`を返す。文字列も受け入れ、文字列入力時は文字列出力。

- **システム関数**：
  - `getenv(Name)`：環境変数`Name`の値を返す。OS環境変数読み取り時は`EMQXVAR_`を接頭辞として付加。読み込み後は値は不変。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスでネスト構造の値を抽出。例：`username`がJSONオブジェクトなら`json_value(username, 'shop.floor')`でフィールド取得。
  - `jwt_value(Data, Path)`：JWTトークンのペイロードからクレーム値をドット区切りパスで抽出。例：`password`がJWTでカスタムクレームがある場合、`jwt_value(password, 'client_attrs.unitid')`でネスト値取得。
  - `is_jwt(Data)`（6.2.3以降）：`Data`がJWSコンパクト形式のJWT構造か判定。3つのドット区切りBase64URLデコード可能なセグメントがあり、ヘッダーJSONに`alg`フィールドがあれば`true`。署名検証やペイロード検査は行わず、未定義、null、空文字列、5セグメントのJWEトークン、破損値は`false`。

#### 条件式

Variform式は包括的な制御構造を持ちませんが、以下の関数で基本的な返却値制御が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition`が`true`または非空文字列なら`ThenExpression`を返し、そうでなければ`ElseExpression`を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の非空引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の非空要素を返す。

#### エラー処理

Bashなどのスクリプト環境と同様に、Variform式は未定義変数参照や実行時例外が発生した場合、空文字列`""`を返す設計です。

- 未定義変数：定義されていない変数を参照すると空文字列となる。
- 実行時例外：関数の誤用や型不整合、配列インデックス範囲外などの例外は空文字列を返す。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りのclient IDのプレフィックスを抽出。
- `strlen(username, 0, 5)`：usernameの部分文字列を抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclient IDから数字を抽出。空文字列なら`'000'`を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true`を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false`を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true`を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid`が`foo.`で始まれば`foo`、そうでなければ`bar`を返す。
