# 設定ファイル

ユーザーは設定ファイルまたは環境変数を使ってEMQXを設定できます。本節では主にEMQXの設定ファイルを紹介し、EMQXで最も一般的に使用される機能の基本的な設定方法を説明します。詳細な設定項目と解説については、[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

## 設定ディレクトリ

EMQXをインストールすると、設定およびランタイムデータを管理するための一連のディレクトリが作成されます。これらのディレクトリは主に以下の2つのカテゴリに分かれています。

- **静的設定ディレクトリ (`etc`)**：読み取り専用で、不変または静的な設定ファイルを格納します。
- **動的設定ディレクトリ (`data/configs`)**：書き込み可能で、ランタイムで生成または動的に更新される設定ファイルを格納します。

### 静的設定ディレクトリ (`etc`)

`etc` ディレクトリにはEMQXの初期設定を定義する設定ファイルが格納されます。これらのファイルは通常、デプロイ時やアップグレード時に変更され、ランタイム中は安定性を確保するため読み取り専用となっています。`etc` ディレクトリの場所はインストール方法によって異なります。

| インストール方法                          | パス              |
| ---------------------------------------- | ----------------- |
| RPMまたはDEBパッケージでインストール    | `/etc/emqx`       |
| Dockerコンテナで実行                     | `/opt/emqx/etc`   |
| ポータブル圧縮パッケージから展開        | `./etc`           |

EMQX 6.3.0以降、RPMおよびDEBインストールでは `/opt/emqx/etc` が `/etc/emqx` へのシンボリックリンクとして作成されます。

### 動的設定ディレクトリ (`data/configs`)

ランタイム中、EMQXはダッシュボード、REST API、CLIを通じて動的な再設定を可能にしています。これらのツールで行われた変更は永続化のため `data/configs` ディレクトリに保存されます。このディレクトリの場所もインストール方法によって異なります。

| インストール方法                          | パス                      |
| ---------------------------------------- | ------------------------- |
| RPMまたはDEBパッケージでインストール    | `/var/lib/emqx/configs`   |
| Dockerコンテナで実行                     | `/opt/emqx/data/configs`  |
| ポータブル圧縮パッケージから展開        | `./data/configs`          |

EMQX 6.3.0以降、RPMおよびDEBインストールでは `/opt/emqx/data` が `/var/lib/emqx` へのシンボリックリンクとして作成されるため、`/opt/emqx/data/configs` は `/var/lib/emqx/configs` に解決されます。カスタムのデータディレクトリを設定してもこのシンボリックリンクは更新されません。

::: tip
`node.data_dir` 設定や環境変数 `EMQX_NODE__DATA_DIR` を変更することでデータディレクトリを変更可能です。ただし、クラスター運用時はすべてのノードで同一のディレクトリパスを使用する必要があります。
:::

設定ファイルの内容が重複することもありますが推奨されません。重複が発生した場合は、[Config Override Rules](#config-override-rules) に定義された優先ルールに従って解決されます。

## 設定例

[Schema](#schema) セクションで詳細なリファレンスを提供していますが、設定例はEMQXの設定を理解し適用する際に役立ちます。

- RPMまたはDEBパッケージでEMQXをインストールした場合、設定例は `etc/emqx/examples` ディレクトリにあります。
- DockerコンテナでEMQXを実行している場合、設定例は `opt/emqx/etc/examples` ディレクトリにあります。

## ベース設定ファイル

EMQX 5.8.4以降、`etc` ディレクトリに `base.hocon` というベース設定ファイルが存在します。このファイルにはデフォルト設定が含まれ、ランタイム中に上位の設定ファイルで上書き可能です。

例えば、基本的な認証設定でデプロイを開始し、その後ダッシュボードUIからより複雑な設定で上書きすることができます。

`node` や `cluster` のような不変の設定については、デプロイ固有でランタイム中に変更しない値の場合、環境変数を使用することも可能です。詳細は [Environment Variables](#environment-variables) および [Config Override Rules](#config-override-rules) を参照してください。

::: tip
`base.hocon` ファイルはクラスター間で同期されず、そのノードにのみ適用されます。
:::

## 設定書き換えファイル

`data/configs` ディレクトリの `cluster.hocon` ファイルにはクラスター全体の設定項目が含まれています。ダッシュボード、REST API、CLIからの設定変更はこのファイルに永続化されます。

クラスター内のノードが再起動されたり新しいノードが追加された場合、そのノードは自動的に他のノードから `cluster.hocon` をコピーして適用します。このため、手動でのファイル編集は推奨されません。

このファイルの設定は `base.hocon` の設定の上に適用されます。設定の上書き階層については [Config Override Rules](#config-override-rules) をご覧ください。

EMQX 5.1以降、クラスター設定の変更時に `cluster.hocon` ファイルのバックアップが上書き前に作成されます。バックアップはノードのローカル時間でタイムスタンプが付けられ、最大10個のバックアップファイルが保持されます。

## 不変設定ファイル

後方互換性のため、`emqx.conf` ファイルは `node` および `cluster` の重要なシステム設定に引き続き利用可能です。このファイルは `base.hocon` と `cluster.hocon` より優先度が高いですが、環境変数よりは低い優先度です。意図的にこの優先度を利用し、パッケージのアップグレードでこのファイルのデフォルトが更新されることを理解している場合を除き、変更は避けてください。

設定の上書きについては [Config Override Rules](#config-override-rules) を参照してください。

## 設定パス

EMQXでは設定値をドット区切りのパスで参照できます。これはツリー構造に似ており、ルート（常にStruct）から始まり、各セグメントはフィールド名またはMapのキーを指します。配列要素の場合は1始まりのインデックスを使用します。

設定パスの例：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON設定フォーマット

EMQX v5.0以降、設定ファイル形式として[Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon)を採用しています。

HOCONは人間に読みやすいデータ形式で、JSONのスーパーセットです。継承や結合、引用符などの機能により設定作業をさらに簡素化します。

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

またはフラット形式：

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

このイカのようなフラット形式は以前のEMQXバージョンとの互換性がありますが、使い方は異なります。

HOCONでは文字列の両端に引用符を付けることを推奨します。特殊文字を含まない文字列は引用符なしでも構いません（例：`foo`、`foo_bar`）。一方、イカ形式は`=`の右側のすべてを値として扱います。

HOCON構文の詳細は[HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md)をご覧ください。

## 環境変数

設定ファイルに加え、環境変数でもEMQXを設定できます。

例えば、環境変数 `EMQX_NODE__NAME=emqx2@127.0.0.1` は以下の設定を上書きします。

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

設定項目と環境変数の変換ルールは以下の通りです。

1. 設定ファイルの区切り文字 `.` は環境変数で使えないため、EMQXは区切りにダブルアンダースコア `__` を使用します。
2. 他の環境変数と区別するため、環境変数名の先頭に `EMQX_` プレフィックスを付けます。
3. 環境変数の値はHOCON値として解析されるため、複雑なデータ型も渡せます。`:`、`=`、`#` などのHOCON特殊文字を含む値はダブルクォート `"` で囲む必要があります。特に `#` はHOCONの行コメント開始文字なので、クォートなしでは `#` 以降がコメントとして無視されます。

変換例：

```bash
# 環境変数

## localhost:1883 は構造体 {"localhost": 1883} として解析されるため、ダブルクォートで囲む必要があります
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

## HOCON配列を文字列で直接渡す
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

::: warning `#`、`:`、`=` を含む値について

パスワードなどに `#` を含む場合、`#` はHOCONの行コメント開始文字なので、以下のようにするとパスワードが途中で切れてしまいます。

```bash
export EMQX_DASHBOARD__DEFAULT_PASSWORD="MQtt#123"
```

この場合、パスワードは `MQtt` として解釈され、`#123` はコメントとして無視されます。リテラルとして渡すには、**HOCONレベルの**ダブルクォートで囲み、パーサーに `"MQtt#123"` と認識させる必要があります。

```bash
# 正しい例 — HOCONパーサーが受け取る値は "MQtt#123"
export EMQX_DASHBOARD__DEFAULT_PASSWORD='"MQtt#123"'

# 同じ効果、シェル用に内側のクォートをエスケープ
export EMQX_DASHBOARD__DEFAULT_PASSWORD="\"MQtt#123\""
```

同様に `:` や `=` を含む値も同様の扱いです。URLエンコード（例：`%23`）は無効で、EMQXは環境変数の値をURLデコードしません。

:::

::: tip なぜ一部の非引用値は通るが他は通らないのか

EMQXは環境変数の値を `fake_key=<値>` の形でHOCONとして解析します。解析成功すればその値を使い、失敗すれば生の文字列を使います。

例えば、`EMQX_..._PASSWORD="abc#def"` は有効なHOCONで `#def` はコメント扱いされるため、値は `abc` になります。一方、`EMQX_..._PASSWORD=".abc#def"` は無効なHOCONなので生の文字列 `.abc#def` が使われます。

HOCONの引用符で囲むと動作が確定的になります。

:::

::: tip

EMQXは未定義のルートパス（例：`EMQX_UNKNOWN_ROOT__FOOBAR`）を無視します。`UNKNOWN_ROOT` は事前定義されたルートパスではないためです。

既知のルートパスに未知のフィールド名を設定した場合、起動時に警告ログを出力します。例えば、`enable` を誤って `enabled` と設定すると以下のように出力されます。

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

::: tip
EMQX 6.3.0以降、`EMQX_FEATURES` は[feature gates](../deploy/feature-gates.md)用の特別な起動環境変数です。HOCON設定パスにはマッピングされず、`cluster.hocon` に保存されず、EMQX起動時のみ解決されます。
:::

### ブート時環境変数

多くの `EMQX_` プレフィックス付き環境変数は `emqx.conf` の設定を上書きしますが、設定ファイル解析前にEMQX自体の動作を制御する変数もあります。これらは `emqx.conf` に対応する設定がありません。

- `EMQX_FEATURES`：ノードが起動するアプリケーションセットを選択（例：`FULL`、`ESSENTIAL`）。
- `EMQX_SECURITY_PROFILE`：ノード全体のセキュリティプロファイルを選択（`legacy` または `hardened`）。

EMQX 6.3.0以降、`emqx` コマンドは実行時に `etc/emqx.env` から環境変数を読み込みます。サービス起動、フォアグラウンド起動、`emqx ctl` 実行時も含みます。RPM/DEBインストールでは `/etc/emqx/emqx.env` にあります。システムdユニットを編集する代わりにこのファイルでブート時環境変数を設定してください。

- ファイル内の設定は環境から継承した変数を上書きします。
- パッケージアップグレード時もファイルの編集内容は保持されます。
- ファイルにはコメントアウトされたブート時変数の例が記載されています（例：`#KEY="${KEY:-default}"`）。コメント解除して式を変更しなければ、既存の環境値を保持するか未設定時はデフォルトを使います。値を上書きする場合は式を `KEY=value` に置き換えます。
- ブート時環境変数を変更したらEMQXノードを再起動してください。
- 通常の `EMQX_` プレフィックス付き上書き変数（例：`EMQX_NODE__COOKIE`）もこのファイルで設定可能です。

## 設定上書きルール

EMQXでは設定値は階層的に適用され、以下の上書きルールがあります。

- 同一ファイル内では後に定義された値が前の値を上書きします。
- 上位レベルの設定が下位レベルの設定を置き換えます。

設定の優先順位は以下の通りです。

`base.hocon < cluster.hocon < emqx.conf < 環境変数`

つまり、`base.hocon` の設定は最も優先度が低く、上位の設定で上書きされます。`EMQX_` で始まる環境変数が最も優先度が高いです。

::: tip
バージョン5.8.4以前は `base.hocon` ファイルが存在しませんでした。優先順位は同じですが、`base.hocon` は含まれません。
:::

ダッシュボードUI、HTTP API、CLIからの変更はランタイムで `cluster.hocon` に永続化され即時反映されます。ただし、`emqx.conf` や環境変数で同じ設定項目が異なる値に設定されている場合、ノード再起動後に変更が元に戻ることがあります。

混乱を避けるため、**`emqx.conf` と `cluster.hocon` で設定を重複させないでください。**

::: tip
1. 古いEMQXバージョン（例：5.0.2/v5.0.22以前）では `cluster-override.conf` ファイルが存在し、設定優先順位は `emqx.conf < ENV < HTTP API (cluster-override.conf)` でした。
2. 5.0.2/v5.0.22以前から最新バージョンにアップグレードする場合、優先順位は変わらず互換性維持のため `cluster.hocon` は作成されません。
3. `cluster-override.conf` 機構はバージョン5.1で廃止されました。
:::

### 上書き例

以下の設定では、最後の行で定義された `level` の値 `debug` が先の `error` を上書きしますが、`enable` フィールドは変更されません。

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
- マップ形式（サブスクリプション形式、例：`{"1"=1, "2"=2, "3"=3}`）

以下の3つの形式は同等です。

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

この特徴により、配列の要素値を簡単に上書きできます。例えば：

```bash
authentication  = [
  {
    enable = true,
    backend = "built_in_database",
    mechanism = "password_based"
  }
]

# 1番目の要素の `enable` フィールドを以下のように上書き可能
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

## 以下の設定では1番目の要素の `enable` 以外のフィールドが失われます。
authentication = [{ enable = true }]
```

:::

### ゾーンの上書き

EMQXのゾーンは設定をグループ化する概念です。リスナーは `zone` フィールドにゾーン名を設定して関連付けられます。ゾーンに関連付けられたリスナーに接続するMQTTクライアントは、そのゾーンの設定を継承し、グローバル設定を上書きすることがあります。

::: tip
デフォルトではリスナーは `default` という名前のゾーンに紐づいています。`default` ゾーンは論理的なグループであり、設定ファイル上には存在しません。
:::

ゾーンレベルで上書き可能な設定項目は以下の通りです。

- `mqtt`：MQTT接続およびセッション設定。特定ゾーンでのMQTTメッセージの最大パケットサイズを大きくするなど。
- `force_shutdown`：強制シャットダウンのポリシー。
- `force_gc`：Erlangプロセスのガベージコレクションの微調整。
- `flapping_detect`：クライアントのフラッピング検出。
- `durable_sessions`：セッション永続化設定。特定ゾーンでMQTTセッションの永続ストレージを有効化など。

EMQXバージョン5のデフォルト設定ファイルにはゾーンは含まれていません。バージョン4では `internal` と `external` の2つのデフォルトゾーンがありました。

ゾーンを作成するには設定ファイルで定義します。例：

```bash
zones {
  # 複数のゾーンを定義可能
  my_zone1 {
    # ゾーンはグローバル設定と同じスキーマを共有
    mqtt {
      # このゾーン内の接続でより大きなパケットサイズを許可
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

リスナーで `zone` フィールドに作成済みのゾーン名を設定して関連付けます。

```bash
listeners.tcp.default {
    bind = 1883
    zone = my_zone1
    ...
}
```

## Configuration-as-Code のベストプラクティス

EMQXの設定をソース管理や自動化システムで管理する場合、以下のルールを推奨します。

- Configuration-as-Codeの設定は `base.hocon` に記述する。
- `cluster.hocon` を手動編集したり、自分の `cluster.hocon` ファイルをマウントしない。
- `emqx.conf` は優先度が高いこととアップグレード時の影響を理解している場合以外は変更しない。
- ダッシュボード、API、CLIでランタイムに変更しない単純な上書きは環境変数で行う。

Configuration-as-Codeの真実の情報源は `base.hocon` です。ノード起動時に静的設定ディレクトリから読み込まれ、パッケージング、イメージビルド、構成管理、GitOpsワークフローで管理できます。ダッシュボード、REST API、CLIからのランタイム変更は `cluster.hocon` に永続化され、`base.hocon` の上にレイヤーされます。

例えば、デプロイはリスナー、ログ、認証、認可、データ統合のベースラインを `base.hocon` に保持できます。

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

`cluster.hocon` はConfiguration-as-Codeの真実の情報源として使わないでください。EMQXがランタイムで管理し、ダッシュボード、REST API、CLIが書き換え、上書き前にバックアップを作成し、クラスター内のノード間でコピーされます。手動編集やマウントはランタイムの更新と競合し、上書きされる恐れがあります。

`emqx.conf` はディストリビューションパッケージにベースライン設定ファイルとして同梱されています。変更せずにおくと、新しいEMQXバージョンで提供される保守的なデフォルト変更を取り込みやすくなります。`emqx.conf` で設定した項目は `base.hocon` と `cluster.hocon` より優先度が高いため、同じ項目のランタイム変更は動作しているように見えてもノード再起動後に元に戻ることがあります。意図的にその挙動が必要な場合のみ使用してください。

環境変数は最も優先度が高く、特にランタイムに変更しないデプロイ固有の単純な値や、すでにランタイム環境が提供する値に適しています。

```bash
export EMQX_NODE__NAME='emqx@node1.example.net'
export EMQX_NODE__COOKIE='mysecret'
export EMQX_CLUSTER__DISCOVERY_STRATEGY='static'
export EMQX_CLUSTER__STATIC__SEEDS='["emqx@node1.example.net", "emqx@node2.example.net"]'
```

環境変数はすべての設定ファイルを上書きするため、オペレーターが後でダッシュボード、API、CLIで調整することが想定される設定には使わないでください。

## スキーマ

HOCONオブジェクトの型安全性を高めるため、EMQXはスキーマを導入しています。このスキーマはデータ型、フィールド名、メタデータを定義し、設定値の検証などを可能にします。

[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) はこのスキーマから生成されています。

::: tip
ゾーンの設定スキーマは各グループで同一のため、設定マニュアルには含まれていません。例えば `zones.my_zone1.mqtt {...}` は `mqtt {...}` と同じスキーマです。
:::

### プリミティブデータ型

設定マニュアルのプリミティブ型はほぼ自明で、詳細な説明は不要です。以下に代表的な型を列挙します。

#### Integer

整数。例：`42`、`-3`、`0`

#### Integer(Min..Max)

指定範囲内の整数。例：`1..+inf` は1以上の正の整数を意味します。

#### Enum(symbol1, symbol2, ...)

列挙型。定義されたシンボルのいずれかのみ許容。例：`Enum(debug,info,warning,error)` はログレベルを示します。

#### String

文字列型。複数の形式をサポートします。

- **非引用文字列**：特殊文字を含まない単純な識別子や名前に適します（詳細は下記）。
- **引用文字列**：特殊文字や空白を含む場合はダブルクォート `"` で囲み、必要に応じてバックスラッシュ `\` でエスケープします。例：`"line1\nline2"`。
- **三重引用文字列**：`"""` で囲まれ、エスケープ不要（`\` は例外）。複雑な内容を含む場合に便利です。三重引用符に隣接するクォートはエスケープが必要です。
- **インデント付き三重引用文字列**：`"""~` と `~"""` で囲み、EMQX 5.6以降で導入。設定ファイル内でのインデントを許容し、複数行や整形済みテキストに適します。

**非引用文字列の注意点：**

- 禁止文字：`$`、`"`、`{`、`}`、`[`、`]`、`:`、`=`、`,`、`+`、`#`、`` ` ``、`^`、`?`、`!`、`*`、`&`、`\`、空白
- `//` で始めない（コメントになるため）
- `true`、`false`、`null` で始まらない（ブールやnullと誤認されるため）

**三重引用文字列のガイドライン：**

- 三重引用符に隣接するクォートはエスケープするか、`~` 区切りを使う
- 複数行文字列は空白インデントを許容（タブ不可）。インデントレベルは最小の先頭空白数で決定

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

定数文字列。単一値の列挙型（`Enum`）のように振る舞います。特定の設定やモードの静的値に使います。

#### Boolean

`true` または `false`（大文字小文字区別あり）

#### Float

浮動小数点数。例：`3.14`、`-0.001`

#### Duration

人間に読みやすい時間の長さを表します。例やフォーマットの説明。

#### Duration(s)

秒単位の精度を持つ `Duration` 型。詳細と例。

#### Secret

パスワードやトークンなどの機密情報用型。使用方法と重要性の説明。

### 複合データ型

EMQXのHOCON設定の複合型は、他の複合型やプリミティブ値を含むデータ構造を表現します。柔軟かつ階層的なデータ表現を可能にします。

#### Struct `Struct(name)`

波括弧 `{}` で囲まれたフィールドを持つ構造体。`name` はスキーマで定義されたフィールド名と型を参照します。

#### Map `Map($name->Type)`

`Struct` に似ていますが、フィールド名が事前定義されていません。

`$name` はドット `.` を含まない任意の文字列キーを表し、エンティティや属性の名前を示します。`Type` はマップ内のすべての値が同一の型であることを示します。

#### OneOf `OneOf(Type1, Type2, ...)`

複数の型のいずれかを許容するユニオン型。1つのフィールドが複数の型のいずれかを取る場合に使います。例：`String(infinity)` または `Duration`。

#### Array `Array(Type)`

指定型の要素からなる配列。

::: tip

Mapのフィールド名が正の整数の場合、`Array` の別表現として解釈されます。例：

```bash
myarray.1 = 74
myarray.2 = 75
```

は `myarray = [74, 75]` と解釈され、配列要素の上書きに便利です。

:::

### Variform式

Variformは文字列操作とランタイム評価のための軽量で表現力豊かな言語です。完全なプログラミング言語ではなく、EMQXの設定内に埋め込んで動的に文字列操作を行うための専門ツールです。

::: tip
Variform式は特定の設定項目にのみ適用されます。指定がない限り使用しないでください。
:::

::: tip NULL値について
Variform式では値バインディング参照や部分式の評価結果が未定義値になる場合があり、これは空文字列 (`''`) として表現されます。

JSONでデコードしたフィールドが `null` の場合、未定義値（空文字列）として扱われ、文字列 `"null"` とは異なります。
:::

#### 構文例

```js
function_call(clientid, another_function_call(username))
```

この式は `clientid` と `username` を組み合わせて新しい文字列値を生成します。

Variformは以下のリテラルをサポートします。

- ブール値：`true` または `false`
- 整数：例 `42`
- 浮動小数点数：例 `3.14`
- 文字列：シングルクォート `'` またはダブルクォート `"` で囲むASCII文字
- 配列：`[` と `]` で囲み、カンマ `,` 区切り
- 変数：事前定義された値への参照（例：`clientid`）
- 関数：組み込み関数（例：`concat([...])`）

Variformは以下をサポートしません。

- 算術演算
- ループ
- ユーザー定義変数
- ユーザー定義関数
- 例外処理やエラー回復
- 文字列リテラル内のエスケープシーケンス（特殊文字のアンエスケープは `unescape` 関数を呼び出す）

以下はVariform式を埋め込んだ設定例です。

```js
mqtt {
    client_attrs_init = [
        {
            # clientidの最初の `-` までのプレフィックスを抽出
            expression = "nth(1, tokens(clientid, '-'))"
            # client_attrs.group に設定
            set_as_attr = group
        }
    ]
}
```

::: tip
アンエスケープ関数を使う場合、HOCON設定で三重引用符 (`"""`) を使うと二重エスケープ不要で便利です。

例：

```
#### 複数行のclient IDの最初の行を取得
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### 組み込み関数

EMQXはルールエンジンの文字列関数に似た豊富な文字列、配列、乱数、ハッシュ関数を提供します。これらは抽出したデータの操作や整形に使えます。例えば、`lower()`、`upper()`、`concat()` は文字列のフォーマット調整に、`hash()`、`hash_to_range()` はハッシュや範囲マップに使います。

使用可能な関数の例：

- **文字列関数**：
  - [文字列操作関数](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - 新関数 `any_to_string/1`：任意の中間非文字列値を文字列に変換
- **配列関数**：`nth/2` など ([詳細](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any))
- **トピック関数**：
  - `topic_join(Words)`：配列のトピックレベルを `/` で結合しMQTTトピックやフィルターを生成。例：`topic_join(['devices', clientid, '#'])` は `devices/<clientid>/#` を生成。
  - `topic_join(Parent, Word)`：`Parent` トピックに `Word` を追加。`Parent` が `/` で終わっていれば区切り文字は追加しない。
  - `topic_match(Topic, Filter)`：MQTTトピックがフィルターにマッチするか判定。`true` または `false` を返す。例：`topic_match(topic, topic_join(['devices', clientid, '#']))` はクライアント固有のトピックフィルターにマッチするか判定。
  - `topic_split(Topic)`：MQTTトピックを `/` で分割しトピックレベルの配列を返す。
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
  - `int2hexstr(Integer)`：整数を16進文字列にエンコード。例：15は 'F'（大文字）
- **ハッシュ関数**：
  - `hash(Algorithm, Data)`：`Algorithm` は md4 | md5 | sha (sha1) | sha224 | sha256 | sha384 | sha512 | sha3_224 | sha3_256 | sha3_384 | sha3_512 | shake128 | shake256 | blake2b | blake2s のいずれか
  - `hash_to_range(Input, Min, Max)`：sha256でハッシュし、`Min` から `Max` の範囲にマップ（`Min <= X <= Max`）
  - `map_to_range(Input, Min, Max)`：入力を `Min` から `Max` の範囲にマップ（`Min <= X <= Max`）
- **比較関数**：
  - `num_eq(A, B)`：2つの数値が同じなら `true`、そうでなければ `false`
  - `num_neq(A, B)`：2つの数値が異なれば `true`、そうでなければ `false`
  - `num_gt(A, B)`：`A > B` なら `true`、そうでなければ `false`
  - `num_gte(A, B)`：`A >= B` なら `true`、そうでなければ `false`
  - `num_lt(A, B)`：`A < B` なら `true`、そうでなければ `false`
  - `num_lte(A, B)`：`A <= B` なら `true`、そうでなければ `false`
  - `str_eq(A, B)`：2つの文字列が同じなら `true`、そうでなければ `false`
  - `str_neq(A, B)`：2つの文字列が異なれば `true`、そうでなければ `false`
  - `str_gt(A, B)`：辞書順で `A > B` なら `true`、そうでなければ `false`
  - `str_gte(A, B)`：辞書順で `A >= B` なら `true`、そうでなければ `false`
  - `str_lt(A, B)`：辞書順で `A < B` なら `true`、そうでなければ `false`
  - `str_lte(A, B)`：辞書順で `A <= B` なら `true`、そうでなければ `false`
  - `is_empty_var(V)`：変数が空か判定。Variformの空は未定義値（`undefined`）、JSONの `null`（文字列 `"null"` ではない）、空文字列 `""` を含む
  - `not(Bool)`：`Bool` が `false` なら `true`、`true` なら `false`。文字列も受け付け、入力が文字列なら出力も文字列

- **システム関数**：
  - `getenv(Name)`：環境変数 `Name` の値を返す。OS環境変数読み込み時は `EMQXVAR_` プレフィックスが付加される（例：`getenv('FOO_BAR')` は `EMQXVAR_FOO_BAR` を読み込む）。読み込み後は値は不変。

- **データ抽出関数**：
  - `json_value(Data, Path)`：JSON文字列からドット区切りパスで値を抽出。例：`username` がJSONオブジェクトなら `json_value(username, 'shop.floor')` でフィールドにアクセス。
  - `jwt_value(Data, Path)`：JWTトークンのペイロードからドット区切りパスでクレーム値を抽出。例：`password` がカスタムクレームを持つJWTなら `jwt_value(password, 'client_attrs.unitid')` でネスト値にアクセス。
  - `is_jwt(Data)`（6.2.3以降）：`Data` がJWSコンパクト形式のJWT構造か判定。3つのドット区切りBase64URLデコード可能なセグメントを持ち、ヘッダーJSONに `alg` フィールドがある場合のみ `true`。署名検証やペイロード検査は行わず、未定義、null、空文字列、JWEトークン（5セグメント）、不正値は `false`。

#### 条件式

Variform式は包括的な制御フローを持ちませんが、以下の関数で基本的な条件分岐が可能です。

- `iif(Condition, ThenExpression, ElseExpression)`：`Condition` が `true` または空でない文字列なら `ThenExpression`、そうでなければ `ElseExpression` を返す。
- `coalesce(Arg1, Arg2, ...)`：最初の空でない引数を返す。
- `coalesce([Element1, Element2, ...])`：最初の空でない配列要素を返す。

#### エラー処理

Bashなどのスクリプト環境と同様、Variform式は未束縛変数参照やランタイム例外発生時に空文字列 (`""`) を返す設計です。

- 未束縛変数：定義されていない変数参照は空文字列として評価されます。
- ランタイム例外：関数の誤用や型不整合などの例外は空文字列を返します。例：配列インデックスが範囲外。

#### 式の例

- `nth(1, tokens(clientid, '.'))`：ドット区切りclient IDのプレフィックス抽出。
- `strlen(username, 0, 5)`：usernameの一部抽出。
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`：正規表現でclient IDから数字抽出。空なら `'000'` を返す。
- `iif(true, "Value if true", "Value if false")`：`Value if true` を返す。
- `iif("", "Value if true", "Value if false")`：`Value if false` を返す。
- `iif("hello", "Value if true", "Value if false")`：`Value if true` を返す。
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`：`clientid` が `foo.` で始まれば `foo`、そうでなければ `bar` を返す。
