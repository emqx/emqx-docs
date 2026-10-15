# MQTT ブリッジ（ディスクキュー付き）

このプラグインを使用すると、ローカルの MQTT メッセージを別の MQTT ブローカーに転送し、ディスクバッファを利用してレジリエンスを向上させることができます。

## 特長

- ブリッジごとのディスクバッファリング。
- リモートブローカーが利用できない場合の自動リトライ。
- `${topic}` を使ったトピック書き換え対応。
- 1つのプラグインで複数のブリッジを管理可能。
- 設定の更新はブリッジ単位で適用（変更のないブリッジは継続稼働）。

## 動作概要

1. ローカルのパブリッシュが各ブリッジの `filter_topic` にマッチするか判定。
2. マッチしたメッセージをディスクキューのパーティションに追記。
3. キューに溜まったメッセージをリモートブローカーにパブリッシュ。
4. ネットワークや接続の問題でパブリッシュに失敗した場合は自動でリトライ。
5. キューパーティションのサイズが `queue.max_total_bytes` を超えると、最も古いレコードから破棄される。

## 設定方法

EMQX ダッシュボード（推奨）またはプラグイン設定ファイルから設定可能です。

本番環境では、まず1つのブリッジでトラフィックを検証し、その後スケールアウトしてください。

### 設定ファイルの場所

関連する設定ファイルの場所は2つあります：

- インストールされたプラグインパッケージ内のデフォルトファイル：
  - docker インストール例（バージョン `0.2.0`）：
    `/opt/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`
  - deb/rpm インストール例（バージョン `0.2.0`）：
    `/usr/lib/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`

- ダッシュボードや API 経由で設定保存後に EMQX が管理する永続化されたプラグイン設定ファイル：
  - docker：
    `/opt/emqx/data/plugins/emqx_bridge_mqtt_dq/config.hocon`
  - deb/rpm：
    `/var/lib/emqx/plugins/emqx_bridge_mqtt_dq/config.hocon`

`priv/config.hocon` はパッケージに含まれるデフォルトテンプレートであり、`data/plugins/.../config.hocon` は EMQX が設定変更後に永続化して使用するファイルです。

### クイックスタート（ダッシュボード）

1. プラグインを有効化します。
2. `remotes` に再利用可能なリモートブローカーを1つ追加します。
3. `bridges` にブリッジを1つ追加します。
4. `remote`、`filter_topic`、`remote_topic` を設定します。
5. 保存してリモートへの配信を検証します。
6. ベースライン検証後にキューやプールの設定を調整してください。

### 設定例

```hocon
bridges {
  to-cloud {
    enable = true
    remote = cloud
    proto_ver = "v4"
    keepalive_s = 60
    pool_size = 4
    filter_topic = "devices/#"
    remote_topic = "fwd/${topic}"
    remote_qos = "${qos}"
    remote_retain = "${retain}"
    queue {
      seg_bytes = "100MB"
      max_total_bytes = "1GB"
    }
  }
}

remotes {
  cloud {
    server = "cloud-broker.example.com:8883"
    username = "bridge_user"
    password = "secret"
    ssl {
      enable = true
      verify = verify_none
      # cacertfile = "/path/to/ca.pem"
      # certfile = "/path/to/client-cert.pem"
      # keyfile = "/path/to/client-key.pem"
    }
  }
}
```

### 環境変数の置換

設定ファイル内の任意の文字列値は、`${EMQXDQ_*}` 形式で OS 環境変数を参照できます。`EMQXDQ_` プレフィックスが付いた変数のみが解決され、それ以外の `${...}` パターン（例：`remote_topic` 内の `${topic}`）はそのまま残ります。値全体がプレースホルダーである必要があり、部分的な文字列補間（例：`"prefix-${EMQXDQ_VAR}-suffix"`）はサポートされません。

**制限:** `${EMQXDQ_*}` の置換は文字列型のフィールド（例：`server`、`username`、`password`）にのみ有効であり、ブール型（`enable`）、整数型（`pool_size`、`keepalive_s`）には使用できません。

例：

```hocon
remotes {
  cloud {
    server = "${EMQXDQ_REMOTE_SERVER}"
    username = "${EMQXDQ_REMOTE_USER}"
    password = "${EMQXDQ_REMOTE_PASSWORD}"
  }
}
```

環境変数が設定されていない場合、プラグインはエラーをログに出力し、元の `${EMQXDQ_...}` 文字列をリテラル値として保持します。これにより接続失敗（例：`"${EMQXDQ_REMOTE_SERVER}"` に接続しようとする）が発生し、ログやステータス API で設定ミスが明示されます。

> **警告 — 動的設定更新とノードローカル環境変数**
>
> 環境変数は設定を解析するノードでのみ解決されます。EMQX ダッシュボード、REST API、CLI でプラグイン設定を更新すると、設定テキストが永続化され、クラスター内の全ノードで再解析されます。異なるノードで環境変数の値が異なる（または未設定）場合、ノードごとに異なる実効設定になります。
>
> そのため、**ダッシュボード、API、CLI での設定更新時に `${EMQXDQ_...}` の置換は避けてください**。クラスター内の全ノードで同一の環境変数が設定されている場合のみ使用してください。ノードローカルなシークレットは、設定ファイルを直接編集してプラグインをリロードするか、Kubernetes ConfigMaps/Secrets のように全ノードで一貫してマウントされる仕組みを利用してください。

### 設定リファレンス

#### トップレベル

| フィールド | 型   | デフォルト | 説明                                  |
|------------|------|------------|-------------------------------------|
| `bridges`  | map  | `{}`       | ブリッジ名をキーとしたブリッジ設定のマップ。 |
| `remotes`  | map  | `{}`       | 再利用可能なリモートブローカー定義のマップ。 |

#### ブリッジ（`bridges.<name>`）

| フィールド             | 型       | デフォルト               | 説明                                                                                  |
|-----------------------|----------|-------------------------|---------------------------------------------------------------------------------------|
| `enable`              | boolean  | `true`                  | このブリッジを有効または無効にします。                                               |
| `remote`              | string   | —                       | `remotes` 配下のリモートブローカー定義名。                                           |
| `proto_ver`           | string   | `"v4"`                  | MQTT プロトコルバージョン：`v3`、`v4`、`v5` のいずれか。                             |
| `clientid_prefix`     | string   | `"emqx-dq-<name>-"`     | 自動生成される MQTT クライアントIDのプレフィックス。各接続はユニークなインデックスを付加（例：`emqx-dq-mybridge-0`）。省略可。 |
| `keepalive_s`         | integer  | `60`                    | MQTT キープアライブ間隔（秒）。                                                      |
| `pool_size`           | integer  | `4`                     | リモートブローカーへの MQTT 接続数。                                                |
| `buffer_pool_size`    | integer  | `4`                     | ブリッジごとのディスクキューバッファワーカー数。以下の注意点を参照。                 |
| `filter_topic`        | string   | —                       | ローカルトピックのフィルターパターン。`+` と `#` のワイルドカードをサポート。         |
| `remote_topic`        | string   | —                       | 転送先トピックのテンプレート。元のトピックは `${topic}` で参照可能。                 |
| `enqueue_timeout_ms`  | integer  | `5000`                  | ディスクキューへの書き込み確認待ちの最大ブロック時間（ms）。QoS > 0 の場合のみ適用。QoS 0 は常に非同期。 |
| `max_inflight`        | integer  | `32`                    | リモートブローカーへの未アックメッセージの最大数。ディスクキューからのバッチポップサイズと emqtt の送信ウィンドウを制御。 |
| `remote_qos`          | string   | `"${qos}"`              | リモートブローカーへのパブリッシュ時の QoS レベル（`"0"`、`"1"`、`"2"`）。デフォルトの `"${qos}"` は元メッセージの QoS を保持。 |
| `remote_retain`       | string   | `"${retain}"`           | リモートブローカーへのパブリッシュ時のリテインフラグ（`"true"`、`"false"`）。デフォルトの `"${retain}"` は元のリテインフラグを保持。 |
| `max_publish_retries` | integer  | `-1`                    | メッセージごとのパブリッシュリトライ回数。`-1` は無限リトライ。失敗した PUBACK や接続切断ごとに1回消費。 |

#### リモート（`remotes.<name>`）

| フィールド          | 型       | デフォルト       | 説明                                         |
|--------------------|----------|-----------------|----------------------------------------------|
| `server`           | string   | —               | リモート MQTT ブローカーのアドレス（`host:port`）。 |
| `username`         | string   | `""`            | リモートブローカー認証用のユーザー名。         |
| `password`         | string   | `""`            | リモートブローカー認証用のパスワード。         |
| `ssl.enable`       | boolean  | `false`         | リモートブローカー接続に SSL/TLS を使用するか。 |
| `ssl.verify`       | string   | `verify_none`   | TLS 検証モード。`verify_none`、`verify_peer` をサポート。 |
| `ssl.sni`          | string   | サーバーホスト名 | TLS Server Name Indication。サーバーホスト名がデフォルト。`"disable"` で SNI 無効化。 |
| `ssl.cacertfile`   | string   | —               | リモートブローカー証明書検証用の CA 証明書ファイル。 |
| `ssl.certfile`     | string   | —               | 相互 TLS 認証用のクライアント証明書ファイル。     |
| `ssl.keyfile`      | string   | —               | 相互 TLS 認証用のクライアント秘密鍵ファイル。     |

#### キュー

| フィールド               | 型     | デフォルト                      | 説明                                                                                   |
|-------------------------|--------|-------------------------------|----------------------------------------------------------------------------------------|
| `queue.base_dir`        | string | `"emqx_bridge_mqtt_dq"`        | ディスクキューのセグメントファイルのベースディレクトリ。ブリッジ名とパーティションインデックスが自動付加される（例：`<base_dir>/<bridge_name>/<index>`）。相対パスは EMQX の `data_dir` 基準で解決。絶対パスはそのまま使用。 |
| `queue_seg_bytes`       | string | `"100MB"`                     | キューセグメントファイルの最大サイズ。                                                  |
| `queue.max_total_bytes` | string | `"1GB"`                       | パーティションごとの最大ディスクキューサイズ。ブリッジは `buffer_pool_size` 個のパーティションを使うため、最大ディスク使用量は `buffer_pool_size` × この値。超過時は最古のメッセージが破棄される。 |

## トピックテンプレート

`remote_topic` フィールドは `${topic}` プレースホルダーをサポートし、転送時に元のパブリッシュトピックに置換されます。

例：
- `remote_topic = "${topic}"` — 元のトピックをそのまま転送。
- `remote_topic = "forwarded/${topic}"` — プレフィックスを付加。
- `remote_topic = "region1/${topic}"` — リージョンのネームスペースを付加。

`remote_topic` はキューからメッセージを送信する際に適用されます。このフィールドを変更した後は、該当ブリッジを再起動すると新しいテンプレートが適用されます。

## REST API

プラグインは EMQX プラグイン API ベースパス配下に4つのエンドポイントを公開しています：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics` — Prometheus テキスト形式
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats` — JSON ダッシュボードスナップショット
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>` — 単一ブリッジ情報
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status` — プラグイン／クラスターのヘルスサマリー

すべての JSON エンドポイントは `application/json; charset=utf-8` を返します。

JSON API はクラスター集約型です。集約中にノードが利用不可またはタイムアウトした場合でも、API はベストエフォートでデータを返しますが、レスポンスにはクラスターの完全性メタデータが含まれます。

例：

```bash
curl -u admin:public \
  http://127.0.0.1:18083/api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics
```

```bash
curl -u admin:public \
  http://127.0.0.1:18083/api/v5/plugin_api/emqx_bridge_mqtt_dq/stats
```

### `/stats` レスポンス構造

`/stats` のレスポンスボディには以下が含まれます：

- `cluster`: クラスターの完全性と失敗ノード情報
- `uptime_seconds`: 応答したノードの中で観測された最大プラグイン稼働時間（秒）
- `summary`: 全ブリッジの合計値
- `bridges`: 設定された各ブリッジの情報

例：

```json
{
  "cluster": {
    "complete": true,
    "responded_nodes": ["emqx@127.0.0.1"],
    "failed_nodes": [],
    "timeout_ms": 5000
  },
  "uptime_seconds": 123,
  "summary": {
    "bridge_count": 1,
    "running_bridge_count": 1,
    "buffered": 12,
    "backlog": 3,
    "inflight": 8,
    "enqueue": 1000,
    "dequeue": 995,
    "publish": 990,
    "drop": 5
  },
  "bridges": [
    {
      "name": "to-cloud",
      "config_state": "enabled",
      "runtime_state": "running",
      "status": "ok",
      "status_reason": null,
      "enqueue": 1000,
      "dequeue": 995,
      "publish": 990,
      "drop": 5,
      "retried_by_reason": {
        "connect_failed": 2,
        "reason_code": 3
      },
      "buffered": 12,
      "backlog": 3,
      "inflight": 8,
      "buffers": [
        {
          "bridge": "to-cloud",
          "index": 0,
          "status": "running",
          "buffered": 12
        }
      ],
      "connectors": [
        {
          "bridge": "to-cloud",
          "index": 0,
          "status": "connected",
          "backlog": 3,
          "inflight": 8
        }
      ]
    }
  ]
}
```

`GET /stats/<bridge>` は以下を返します：

```json
{
  "cluster": {
    "complete": true,
    "responded_nodes": ["emqx@127.0.0.1"],
    "failed_nodes": [],
    "timeout_ms": 5000
  },
  "bridge": {
    "name": "to-cloud",
    "config_state": "enabled",
    "runtime_state": "running",
    "status": "ok"
  }
}
```

指定したブリッジが現在の設定に存在しない場合、API は `404` を返します。

`GET /status` は簡潔なヘルスビューを返します：

```json
{
  "plugin": "emqx_bridge_mqtt_dq",
  "cluster": {
    "complete": true,
    "responded_nodes": ["emqx@127.0.0.1"],
    "failed_nodes": [],
    "timeout_ms": 5000
  },
  "status": "ok",
  "bridge_count": 1
}
```

`/metrics` エンドポイントはクラスター集約された Prometheus テキスト形式のメトリクスを返します。例：

- `emqx_bridge_mqtt_dq_uptime_seconds`
- `emqx_bridge_mqtt_dq_bridge_enqueue_total{bridge="..."}`
- `emqx_bridge_mqtt_dq_bridge_dequeue_total{bridge="..."}`
- `emqx_bridge_mqtt_dq_bridge_publish_total{bridge="..."}`
- `emqx_bridge_mqtt_dq_bridge_drop_total{bridge="..."}`
- `emqx_bridge_mqtt_dq_bridge_status{bridge="...",status="..."}`
- `emqx_bridge_mqtt_dq_bridge_retry_reason_total{bridge="...",reason="..."}`
- `emqx_bridge_mqtt_dq_buffer_buffered{bridge="...",index="..."}`
- `emqx_bridge_mqtt_dq_connector_backlog{bridge="...",index="..."}`
- `emqx_bridge_mqtt_dq_connector_inflight{bridge="...",index="..."}`

### メトリクスの意味

#### ブリッジメトリクス

- `enqueue`: ブリッジのエンキュー経路に受け入れられたローカルメッセージ数
- `dequeue`: ローカルキューから耐久的に削除されたメッセージ数
- `publish`: リモートブローカーに正常にパブリッシュされたメッセージ数
- `drop`: キューに溜まったメッセージのうち破棄された数
- `retried_by_reason`: リトライ理由別の試行回数
- `config_state`: 設定からの望ましいブリッジ状態（`enabled` または `disabled`）
- `runtime_state`: 実際のワーカー／ストレージ状態（`running`、`degraded`、`purged`）
- `status`: オペレーター向けのブリッジヘルス状態（`ok`、`partial`、`disconnected`、`disabled`、`error`）

現在のリトライ理由は以下を含みます：

- `reason_code`: リモートブローカーが非成功の MQTT 理由コードを返しリトライされた
- `connect_failed`: 接続またはパブリッシュ失敗によるリトライ
- `timeout`: タイムアウトによるリトライ分類
- `connection_lost`: クライアントプロセス終了に伴いインフライトメッセージがリトライ用に回収された
- `other`: 未分類のリトライ理由のフォールバック

ブリッジが完全にドレインされた後は、以下の関係が成り立ちます：

- `enqueue = dequeue = publish + drop`

#### バッファメトリクス

- `buffered`: その耐久キューパーティションに現在格納されているメッセージ数
- バッファ行の `status`: ワーカーが存在する場合は `running`、そうでなければ `missing`

このゲージは `replayq:open/1` の直後に更新されるため、新しいトラフィックが来る前でも永続化されたディスク上のメッセージが見えます。

#### コネクタメトリクス

- `backlog`: コネクタのバックログキューに溜まっている、`emqtt` への送信待ちメッセージ数
- `inflight`: すでに `emqtt` に渡されて完了待ちのメッセージ数
- コネクタ行の `status`: `connected`、`disconnected`、`partial`、`missing`、`unknown` のいずれか

## 設定変更時の挙動

設定更新はブリッジ単位で適用されます：

- 変更されたブリッジは再起動されます。
- 削除されたブリッジは停止します。
- 無効化されたブリッジは停止し、キューディレクトリをパージします。
- 新規ブリッジは起動します。
- 変更のないブリッジは継続稼働します。

プラグイン全体は設定更新ごとに再起動されません。ただし、再起動したブリッジには短時間の引き継ぎウィンドウがあり、その間にマッチするメッセージが破棄される可能性があります。ブリッジに影響する変更はトラフィックの少ない時間帯に適用してください。

### 設定変更前の注意

1. 影響を受けるブリッジを特定する。
2. トラフィックの少ない時間帯に適用する。
3. ダッシュボードのステータスやログで再起動／再接続エラーを監視する。
4. 重要なパイプラインの場合は変更後にエンドツーエンドの配信検証を行う。

### `queue.base_dir` の変更

有効なブリッジで `queue.base_dir` を変更すると、新しいディレクトリでブリッジが再起動します。実際のキューパスは `<base_dir>/<bridge_name>/<index>` です。古いディレクトリは自動的に削除されず、ディスク上に孤立したデータとして残ります。不要な場合は、ブリッジが新しいパスで稼働していることを確認した後に手動で削除してください。

### `buffer_pool_size` の変更

`buffer_pool_size` はブリッジごとのディスクキューパーティション数を制御します。メッセージは `erlang:phash2(Topic, buffer_pool_size)` でパーティションに割り当てられます。変更には以下の副作用があります：

1. **プール縮小**（例：8 → 4）  
   新しいサイズ以上のインデックスのパーティションは消費されなくなります。古いファイルは `queue.base_dir` 配下に残り、手動でのクリーンアップが必要です。

2. **プール拡大**（例：4 → 8）  
   ハッシュ空間が変わるため、以前はパーティション N に割り当てられていたトピックがパーティション M に割り当てられる可能性があります。既存の古いパーティション内のメッセージは順序を保って配信されますが、新しいメッセージは別パーティションに行くため、トピック単位の順序が一時的に崩れます。

3. **ブリッジ単位のドロップウィンドウ**  
   `buffer_pool_size` の変更によりブリッジが再起動するため、引き継ぎ時にインフライトのマッチメッセージが破棄される可能性があります。

## メッセージ配信保証

このプラグインは通常動作時に **at-least-once** 配信を提供し、持続的な障害時には **ベストエフォート** 配信となります。以下の状況でメッセージが失われる可能性があります：

### ディスクキューのオーバーフロー

キューパーティションが `queue.max_total_bytes` を超えると、そのパーティションの最も古いメッセージが静かに破棄され、新しいデータの領域を確保します。警告ログ（`mqtt_dq_buffer_overflow`）が周期的に出力されます（メッセージごとではありません）。

**対策**：`queue.max_total_bytes` を増やす、`buffer_pool_size` を増やしてパーティションを分散させる、またはメッセージスループットを減らす。

### リモートブローカーがパブリッシュを拒否

リモートブローカーが PUBACK（QoS 1）や PUBREC（QoS 2）で非成功の MQTT 理由コードを返すと、コネクターは最大3回までリトライします。すべてのリトライが失敗するとメッセージは破棄され、警告ログ（`mqtt_dq_publish_dropped`）が出力されます。

主な拒否理由コード：

| コード | 意味（MQTT 5.0）               |
|--------|-------------------------------|
| 16     | サブスクライバーなし           |
| 128    | 未指定エラー                   |
| 131    | 実装固有のエラー               |
| 135    | 認可されていない               |
| 144    | トピック名が無効               |
| 145    | パケット識別子が使用中         |
| 151    | クォータ超過                   |

注：理由コード 0（成功）と 16（サブスクライバーなし）は成功扱いでリトライは行いません。

**対策**：リモートブローカーの ACL やトピックポリシーを確認し、ログで具体的な理由コードを調査してください。

### 接続障害の繰り返し

リモートブローカーへの接続が切断されるたびに、未アックのメッセージはリトライ回数を1回消費します。3回連続で接続障害が発生し成功配信がない場合、メッセージは破棄されます。

例：

1. ネットワーク障害中にメッセージがローカルキューに入り（リトライカウンタ=3）。
2. リモートが再接続しメッセージ送信 → ACK 前に切断（リトライカウンタ=2）。
3. 再接続し再送 → 切断（リトライカウンタ=1）。
4. 再接続し再送 → 拒否または切断（リトライカウンタ=0）。
5. メッセージ破棄、警告ログ出力。

**対策**：リモートブローカーが繰り返し到達不能になる原因を調査してください。短時間のネットワーク断は透明に処理されますが、持続的な不安定さは問題です。

### エンキュー時のバックプレッシャー（QoS > 0 のローカルパブリッシュ）

QoS 1 または 2 のクライアントがブリッジにマッチするメッセージをパブリッシュすると、プラグインはバッファワーカーのメールボックスにメッセージを送信し、その後最大 `enqueue_timeout_ms`（デフォルト 5000 ms）までディスク書き込み完了を待ってパブリッシュセッションプロセスをブロックします。

このタイムアウトが発生してもメッセージ自体は失われません。すでにバッファワーカーの Erlang メールボックスに入っており、最終的にディスクキューに書き込まれます。タイムアウトはローカルパブリッシュ経路のブロック時間を制御するだけです。

重要な理由は、`message.publish` フックが MQTT セッションプロセス内で動作し、フックがブロック中はそのクライアントからの他メッセージ処理が停止するためです。バッファワーカーが遅い（ディスクI/O遅延やメールボックスのバックログ増大）場合、タイムアウトにより1つの遅いブリッジがクライアントセッションを無期限に停滞させるのを防ぎます。

タイムアウト時の挙動：

1. セッションプロセスは待機をやめて通常処理を継続。
2. クライアントには通常通り PUBACK/PUBREC が返され、エラーは通知されない。
3. 警告ログ（`mqtt_dq_enqueue_timeout`）が出力される。
4. メッセージはバッファワーカーのメールボックスに残り、ワーカーが追いついた時点でディスクキューに書き込まれる。

リスクは間接的で、バッファワーカーが持続的に遅延するとメールボックスが無制限に増大しメモリ使用量が増えることです。これはブリッジが受信メッセージレートに追いつけていない兆候です。

**対策**：`buffer_pool_size` を増やして負荷分散、`queue.base_dir` に高速ストレージを使用、またはマッチするトピックのメッセージレートを下げてください。

注：QoS 0 のローカルパブリッシュはブロックせず、非同期にキューイングされます。

### ブリッジ再起動時のウィンドウ

ブリッジが再起動（設定変更、プラグインリロード、有効／無効切替）すると、マッチするメッセージが一時的に捕捉されない短いウィンドウが発生します。

**対策**：設定変更はトラフィックの少ない時間帯に行ってください。

### QoS 0 の TCP レベル配信

リモートブローカーへの QoS 0 パブリッシュは、メッセージがローカルの TCP 送信バッファに到達した時点で配信成功とみなされます。もしリモートブローカーが TCP スタックで受け入れた後にクラッシュした場合、メッセージはコネクターにエラーを返さずに失われる可能性があります。

これは MQTT QoS 0 の仕様上の特性であり、本プラグイン固有の問題ではありません。

## 運用上の注意

### 永続化

バッファされたメッセージは以下の状況で保持されます：

- EMQX ノードの再起動
- プラグインのリロードやアップグレード
- リモートブローカーへの一時的なネットワーク障害

### キュー制限

パーティションのキュー使用量が `queue.max_total_bytes` を超えると、最も古いメッセージが破棄されます。警告ログが出力されます。

### プールサイズ設定

各バッファワーカーは `BufferIndex rem pool_size` により1つのコネクターに割り当てられます。負荷分散のために：

- `buffer_pool_size` は `pool_size` 以上であるべきです。
- `buffer_pool_size` は `pool_size` の倍数であるべきです（`buffer_pool_size mod pool_size = 0`）。

良い例：`pool_size = 4, buffer_pool_size = 4`（1:1）、`pool_size = 4, buffer_pool_size = 8`（2:1）。

悪い例：`pool_size = 4, buffer_pool_size = 5` — コネクター0が2つのバッファを担当し、他は1つでスループットが不均一になります。

コネクターが切断すると、割り当てられたバッファワーカーは一時停止し、再接続時に自動で再開します。

### 順序性

安定したブリッジ設定下ではトピック単位の順序は保持されます。`buffer_pool_size` を変更すると、前述の通り一時的に順序が乱れる可能性があります。

### パブリッシャーのアック挙動（QoS 1/2）

ブリッジにマッチするメッセージについて：

- クライアントへの `PUBACK`（QoS 1）および `PUBREC`（QoS 2）は、EMQX がディスクキューへのエンキュー確認（`enqueue_timeout_ms`）を待つ間遅延する可能性があります。
- その待機がタイムアウトしても、EMQX はクライアントのパブリッシュフローを完了します。ディスクキューのエンキュータイムアウトによるパブリッシュエラーはクライアントに通知されません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースの tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.2.0 | 0.5.1 | [emqx_bridge_mqtt_dq-0.5.1.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.0/emqx_bridge_mqtt_dq-0.5.1.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.0/emqx_bridge_mqtt_dq-0.5.1.sha256)) |
| 6.2.1 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.2.2 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.2.3 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_bridge_mqtt_dq-0.5.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
