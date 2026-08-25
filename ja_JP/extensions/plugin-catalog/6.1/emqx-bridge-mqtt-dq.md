# MQTT ブリッジ（ディスクキュー付き）

このプラグインを使用すると、ローカルの MQTT メッセージを別の MQTT ブローカーに転送し、ディスクバッファを利用してレジリエンスを向上させることができます。

## 特長

- ブリッジごとのディスクバッファリング。
- リモートブローカーが利用できない場合の自動リトライ。
- `${topic}` を使ったトピック書き換え対応。
- 1つのプラグインで複数のブリッジを管理可能。
- 設定変更はブリッジ単位で適用（変更のないブリッジは継続稼働）。

## 動作概要

1. ローカルのパブリッシュが各ブリッジの `filter_topic` とマッチするか判定。
2. マッチしたメッセージをディスクキューのパーティションに追加。
3. キューに溜まったメッセージをリモートブローカーにパブリッシュ。
4. ネットワークや接続障害でパブリッシュに失敗した場合は自動リトライ。
5. キューパーティションが `queue.max_total_bytes` を超えた場合、最も古いレコードが破棄される。

## 設定

EMQX ダッシュボード（推奨）またはプラグイン設定ファイルから設定可能です。

本番環境では、まず1つのブリッジでトラフィックを検証し、その後スケールアウトしてください。

### 設定ファイルの場所

関連する設定ファイルの場所は以下の2つです：

- インストール済みプラグインパッケージ内のデフォルトファイル：
  - Docker インストール例（バージョン `0.2.0`）：
    `/opt/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`
  - deb/rpm インストール例（バージョン `0.2.0`）：
    `/usr/lib/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`

- ダッシュボードや API から設定保存後に EMQX が管理する永続化されたプラグイン設定ファイル：
  - Docker：
    `/opt/emqx/data/plugins/emqx_bridge_mqtt_dq/config.hocon`
  - deb/rpm：
    `/var/lib/emqx/plugins/emqx_bridge_mqtt_dq/config.hocon`

`priv/config.hocon` はパッケージに含まれるデフォルトテンプレートであり、`data/plugins/.../config.hocon` は設定変更後に EMQX が使用する永続化設定ファイルです。

### クイックスタート（ダッシュボード）

1. プラグインを有効化します。
2. `remotes` にリユース可能なリモート設定を1つ追加します。
3. `bridges` にブリッジを1つ追加します。
4. `remote`、`filter_topic`、`remote_topic` を設定します。
5. 保存してリモートへの配信を確認します。
6. ベースライン検証後にキューやプール設定を調整してください。

### 例

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

設定ファイル内の任意の文字列値は、`${EMQXDQ_*}` 形式で OS 環境変数を参照できます。`EMQXDQ_` プレフィックスのついた変数のみが解決され、それ以外の `${...}`（例：`remote_topic` の `${topic}`）はそのまま残ります。値全体がプレースホルダーである必要があり、部分的な埋め込み（例：`"prefix-${EMQXDQ_VAR}-suffix"`）はサポートされません。

**制限事項:** `${EMQXDQ_*}` の置換は文字列型フィールド（例：`server`、`username`、`password`）にのみ有効で、ブール型（`enable`）や整数型（`pool_size`、`keepalive_s`）には使用できません。

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
> 環境変数は設定をパースするノードで解決されます。EMQX ダッシュボード、REST API、CLI でプラグイン設定を更新すると、生の設定テキストが永続化され、クラスター内の全ノードで再パースされます。ノード間で参照される環境変数の値が異なる（または未設定）場合、ノードごとに異なる実効設定となります。
>
> そのため、**クラスター内の全ノードで同一の環境変数が設定されている場合を除き、ダッシュボード、API、CLI での `${EMQXDQ_...}` 置換の使用は避けてください**。ノードローカルのシークレットは、設定ファイルを直接編集してプラグインをリロードするか、Kubernetes ConfigMaps/Secrets のような一貫したシークレット注入機構を利用してください。

### 設定リファレンス

#### トップレベル

| フィールド | 型   | デフォルト | 説明                                      |
|------------|------|------------|-------------------------------------------|
| `bridges`  | map  | `{}`       | ブリッジ名をキーとしたブリッジ設定のマップ。 |
| `remotes`  | map  | `{}`       | 再利用可能なリモートブローカー定義のマップ。 |

#### ブリッジ (`bridges.<name>`)

| フィールド           | 型       | デフォルト               | 説明                                                                                  |
|---------------------|----------|-------------------------|---------------------------------------------------------------------------------------|
| `enable`            | boolean  | `true`                  | このブリッジを有効または無効にします。                                               |
| `remote`            | string   | —                       | `remotes` 配下のリモートブローカー定義名。                                          |
| `proto_ver`         | string   | `"v4"`                  | MQTT プロトコルバージョン：`v3`、`v4`、`v5`。                                      |
| `clientid_prefix`   | string   | `"emqx-dq-<name>-"`     | 自動生成される MQTT クライアントIDのプレフィックス。各接続に一意のインデックスが付与されます（例：`emqx-dq-mybridge-0`）。省略可能。 |
| `keepalive_s`       | integer  | `60`                    | MQTT のキープアライブ間隔（秒）。                                                    |
| `pool_size`         | integer  | `4`                     | リモートブローカーへの MQTT 接続数。                                                |
| `buffer_pool_size`  | integer  | `4`                     | ブリッジごとのディスクキューバッファーワーカー数。以下の注意点を参照してください。     |
| `filter_topic`      | string   | —                       | ローカルのトピックフィルタパターン。`+` と `#` ワイルドカードをサポート。             |
| `remote_topic`      | string   | —                       | 転送先トピックのテンプレート。元のトピックは `${topic}` で参照可能。                  |
| `enqueue_timeout_ms`| integer  | `5000`                  | ディスクキュー書き込み確認待ちの最大ブロック時間（ms）。QoS > 0 の場合のみ適用。QoS 0 は常に非同期。 |
| `max_inflight`      | integer  | `32`                    | リモートブローカーへの未アックメッセージの最大数。ディスクキューからのバッチポップサイズと emqtt の送信ウィンドウを制御。 |
| `remote_qos`        | string   | `"${qos}"`              | リモートブローカーへのパブリッシュ時の QoS レベル（`"0"`、`"1"`、`"2"`）。デフォルトの `"${qos}"` は元のメッセージの QoS を保持。 |
| `remote_retain`     | string   | `"${retain}"`           | リモートブローカーへのパブリッシュ時のリテインフラグ（`"true"`、`"false"`）。デフォルトの `"${retain}"` は元のメッセージのリテインフラグを保持。 |
| `max_publish_retries` | integer | `-1`                   | メッセージごとのパブリッシュリトライ回数。`-1` は無限リトライ。失敗した PUBACK または接続切断ごとに1回消費。 |

#### リモート (`remotes.<name>`)

| フィールド         | 型       | デフォルト       | 説明                                               |
|-------------------|----------|-----------------|----------------------------------------------------|
| `server`          | string   | —               | リモート MQTT ブローカーのアドレス（`host:port`）。 |
| `username`        | string   | `""`            | リモートブローカー認証用のユーザー名。              |
| `password`        | string   | `""`            | リモートブローカー認証用のパスワード。              |
| `ssl.enable`      | boolean  | `false`         | リモートブローカー接続に SSL/TLS を有効化。         |
| `ssl.verify`      | string   | `verify_none`   | TLS 検証モード。サポート値：`verify_none`、`verify_peer`。 |
| `ssl.sni`         | string   | サーバーホスト名 | TLS Server Name Indication。デフォルトはサーバーホスト名。`"disable"` に設定すると SNI 無効化。 |
| `ssl.cacertfile`  | string   | —               | リモートブローカー証明書検証に使用する CA 証明書ファイル。 |
| `ssl.certfile`    | string   | —               | 相互 TLS 認証用のクライアント証明書ファイル。         |
| `ssl.keyfile`     | string   | —               | 相互 TLS 認証用のクライアント秘密鍵ファイル。         |

#### キュー

| フィールド             | 型     | デフォルト                 | 説明                                                                                   |
|-----------------------|--------|---------------------------|----------------------------------------------------------------------------------------|
| `queue.base_dir`       | string | `"emqx_bridge_mqtt_dq"`   | ディスクキューのセグメントファイルのベースディレクトリ。ブリッジ名とパーティションインデックスが自動付加されます（例：`<base_dir>/<bridge_name>/<index>`）。相対パスは EMQX の `data_dir` に対して解決され、絶対パスはそのまま使用されます。 |
| `queue_seg_bytes`      | string | `"100MB"`                 | キューセグメントファイルの最大サイズ。                                                  |
| `queue.max_total_bytes`| string | `"1GB"`                   | パーティションごとの最大ディスクキューサイズ。各ブリッジは `buffer_pool_size` 個のパーティションを使用するため、最大合計使用量は `buffer_pool_size` × この値となります。超過時は最も古いメッセージが破棄されます。 |

## トピックテンプレート

`remote_topic` フィールドは `${topic}` プレースホルダーをサポートしており、転送時に元のパブリッシュトピックに置換されます。

例：
- `remote_topic = "${topic}"` — 元のトピックを変更せずに転送。
- `remote_topic = "forwarded/${topic}"` — プレフィックスを付加。
- `remote_topic = "region1/${topic}"` — リージョンのネームスペースを追加。

`remote_topic` はキューからメッセージを送信する際に適用されます。このフィールドを変更した場合、該当ブリッジの再起動後にキュー内のメッセージは新しいテンプレートを使用します。

## REST API

プラグインは EMQX プラグイン API ベースパス配下に4つのエンドポイントを公開します：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics` — Prometheus テキスト形式
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats` — JSON ダッシュボードスナップショット
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>` — 特定ブリッジのみ
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status` — プラグイン／クラスターのヘルスサマリー

すべての JSON エンドポイントは `application/json; charset=utf-8` を返します。

JSON API はクラスター集約型です。ノードが利用不可またはタイムアウトした場合でもベストエフォートでデータを返しますが、レスポンスにはクラスターの完全性メタデータが含まれます。

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

`/stats` のレスポンスボディは以下を含みます：

- `cluster`: クラスターの完全性と失敗ノード情報
- `uptime_seconds`: 応答したノード間で観測された最大プラグインアップタイム
- `summary`: 全ブリッジの合計値
- `bridges`: 設定された各ブリッジのエントリ

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
- `dequeue`: ローカルキューから永続的に削除されたメッセージ数
- `publish`: リモートブローカーに正常にパブリッシュされたメッセージ数
- `drop`: キュー内で破棄されたメッセージ数
- `retried_by_reason`: リトライ理由別の試行回数
- `config_state`: 設定上のブリッジ状態（`enabled` または `disabled`）
- `runtime_state`: 実際のワーカー／ストレージ状態（`running`、`degraded`、`purged`）
- `status`: オペレーター向けのブリッジヘルス状態（`ok`、`partial`、`disconnected`、`disabled`、`error`）

現在のリトライ理由：

- `reason_code`: リモートブローカーが非成功 MQTT リーズンコードを返しリトライされた場合
- `connect_failed`: 接続またはパブリッシュ失敗によるリトライ
- `timeout`: タイムアウトによるリトライ分類
- `connection_lost`: 関連クライアントプロセス終了によりインフライトメッセージがリトライ用に回収された場合
- `other`: 未分類のリトライ理由のフォールバック

ブリッジが完全にドレインされた後は以下の関係が成り立ちます：

- `enqueue = dequeue = publish + drop`

#### バッファメトリクス

- `buffered`: その永続キューパーティションに現在格納されているメッセージ数
- バッファ行の `status`: ワーカーが存在すれば `running`、そうでなければ `missing`

このゲージは `replayq:open/1` 後すぐに更新されるため、新規トラフィック到着前でも永続化されたディスク上のメッセージが見えます。

#### コネクタメトリクス

- `backlog`: `emqtt` に送信待ちのコネクタバックログキュー内メッセージ数
- `inflight`: すでに `emqtt` に渡され完了待ちのメッセージ数
- コネクタ行の `status`: `connected`、`disconnected`、`partial`、`missing`、`unknown` のいずれか

## 設定変更時の挙動

設定変更はブリッジ単位で適用されます：

- 変更されたブリッジは再起動。
- 削除されたブリッジは停止。
- 無効化されたブリッジは停止し、キューディレクトリをパージ。
- 新規ブリッジは起動。
- 変更のないブリッジは継続稼働。

プラグイン全体は設定更新ごとに再起動されません。ただし、再起動したブリッジは短時間の切り替えウィンドウがあり、その間にマッチするメッセージが破棄される可能性があります。トラフィックが少ない時間帯に影響のある変更を適用してください。

### 設定変更前の注意

1. 影響を受けるブリッジを特定。
2. トラフィックが少ない時間帯に適用。
3. ダッシュボードのステータスとログを監視し、再起動や再接続エラーを確認。
4. 重要なパイプラインでは変更後にエンドツーエンドの配信検証を実施。

### `queue.base_dir` の変更

有効なブリッジで `queue.base_dir` を変更すると、そのブリッジは新しいディレクトリで再起動します。実際のキューパスは `<base_dir>/<bridge_name>/<index>` です。古いディレクトリは自動で削除されず、オーファンデータとしてディスクに残ります。不要な場合はブリッジが新パスで稼働していることを確認後、手動で削除してください。

### `buffer_pool_size` の変更

`buffer_pool_size` はブリッジごとのディスクキューパーティション数を制御します。メッセージは `erlang:phash2(Topic, buffer_pool_size)` でパーティションに割り当てられます。変更による影響は以下の通りです：

1. **プール縮小**（例：8 → 4）：新サイズ以上のインデックスのパーティションは消費されなくなります。古いファイルは `queue.base_dir` 配下に残り、手動でのクリーンアップが必要です。

2. **プール拡大**（例：4 → 8）：ハッシュ空間が変わるため、以前パーティション N に割り当てられていたトピックがパーティション M に変わる可能性があります。古いパーティション内のメッセージは順序を保って配信されますが、新しいメッセージは異なるパーティションに入るため、トピック単位のエンドツーエンド順序が一時的に崩れます。

3. **ブリッジ単位のドロップウィンドウ**：`buffer_pool_size` の変更によりブリッジが再起動するため、切り替え時にインフライトのマッチメッセージが破棄される可能性があります。

## メッセージ配信保証

このプラグインは通常動作下で **少なくとも1回** 配信を保証し、持続的な障害時は **ベストエフォート** 配信となります。以下の場合にメッセージが失われる可能性があります。

### ディスクキューのオーバーフロー

キューパーティションが `queue.max_total_bytes` を超えると、そのパーティションの最も古いメッセージが静かに破棄され、新しいデータのための空きを作ります。警告ログ（`mqtt_dq_buffer_overflow`）が定期的に出力されます（メッセージ単位ではありません）。

**対策**：`queue.max_total_bytes` を増やす、`buffer_pool_size` を増やして負荷分散する、またはメッセージスループットを減らす。

### リモートブローカーによるパブリッシュ拒否

リモートブローカーが PUBACK（QoS 1）や PUBREC（QoS 2）で非成功の MQTT リーズンコードを返した場合、コネクターは最大3回までリトライします。すべてのリトライが失敗するとメッセージは破棄され、警告ログ（`mqtt_dq_publish_dropped`）が出力されます。

主な拒否理由コード：

| コード | 意味（MQTT 5.0）                |
|--------|--------------------------------|
| 16     | サブスクライバーなし            |
| 128    | 未指定のエラー                  |
| 131    | 実装固有のエラー                |
| 135    | 認可されていない                |
| 144    | トピック名が無効              |
| 145    | パケット識別子が使用中         |
| 151    | クォータ超過                   |

注：コード 0（成功）と 16（サブスクライバーなし）は成功扱いでリトライしません。

**対策**：リモートブローカーの ACL とトピックポリシーを確認し、特定の理由コードに関するログを調査してください。

### 繰り返す接続障害

リモートブローカーへの接続が切断されるたびに、未アックのメッセージはリトライ回数を1回消費します。3回連続で接続障害が発生し成功配信がない場合、メッセージは破棄されます。

例：
1. ネットワーク障害中にメッセージがローカルキューに追加される（リトライカウンター=3）。
2. リモートが再接続しメッセージ送信するも、ACK 前に切断（リトライカウンター=2）。
3. 再接続し再送信するも切断（リトライカウンター=1）。
4. 再接続し再送信するも拒否または切断（リトライカウンター=0）。
5. メッセージ破棄、警告ログ出力。

**対策**：リモートブローカーが繰り返し到達不能になる原因を調査してください。短期的なネットワーク断は透過的に処理されますが、持続的な不安定さが問題です。

### エンキューのバックプレッシャー（QoS > 0 ローカルパブリッシュ）

QoS 1 または 2 のクライアントがブリッジにマッチするメッセージをパブリッシュすると、プラグインはバッファワーカーのメールボックスにメッセージを送信し、ディスク書き込み確認まで最大 `enqueue_timeout_ms`（デフォルト5000ms）ブロックします。

このタイムアウトが発生してもメッセージ自体は失われません。すでにバッファワーカーの Erlang メールボックスに存在し、遅れてディスクキューに書き込まれます。タイムアウトはローカルパブリッシュ処理のブロック時間を制御するだけです。

理由は、`message.publish` フックは MQTT セッションプロセス内で動作し、フックがブロック中はそのクライアントからの他メッセージ処理ができません。バッファワーカーが遅い（ディスク I/O ストールやメールボックスのバックログ増大）場合、タイムアウトがないとクライアントセッションが無期限に停止します。

タイムアウト時の挙動：
1. セッションプロセスは待機をやめて通常処理を継続。
2. クライアントには通常通り PUBACK/PUBREC が返され、エラーは通知されない。
3. 警告ログ（`mqtt_dq_enqueue_timeout`）が出力される。
4. メッセージはバッファワーカーのメールボックスに残り、追いついた時点でディスクキューに書き込まれる。

リスクは間接的で、バッファワーカーが継続的に遅延するとメールボックスが無制限に増加しメモリ使用量が増えることです。これはブリッジが受信メッセージレートに追いついていない兆候です。

**対策**：`buffer_pool_size` を増やして負荷分散、`queue.base_dir` に高速ストレージを使用、またはマッチするトピックのメッセージレートを下げてください。

注：QoS 0 のローカルパブリッシュはブロックせず、非同期にエンキューされます。

### ブリッジ再起動時のウィンドウ

ブリッジが再起動（設定変更、プラグインリロード、有効/無効切替）すると、マッチするメッセージが一時的にキャプチャされない可能性があります。

**対策**：トラフィックが少ない時間帯に設定変更を適用してください。

### QoS 0 の TCP レベル配信

QoS 0 のリモートブローカーへのパブリッシュは、メッセージがローカル TCP 送信バッファに到達した時点で成功と見なされます。リモートブローカーが TCP スタック受理後にクラッシュした場合、メッセージは失われる可能性がありますが、コネクターにはエラーが返りません。

これは MQTT QoS 0 の仕様であり、本プラグイン固有の問題ではありません。

## 運用上の注意

### 永続化

バッファされたメッセージは以下を耐えます：

- EMQX ノードの再起動
- プラグインのリロードおよびアップグレード
- リモートブローカーへの一時的なネットワーク障害

### キュー制限

キュー使用量がパーティションごとの `queue.max_total_bytes` を超えると、最も古いメッセージが破棄されます。警告ログが出力されます。

### プールサイズ設定

各バッファワーカーは `BufferIndex rem pool_size` によって1つのコネクターに割り当てられます。負荷分散のために：

- `buffer_pool_size` は `pool_size` 以上に設定してください。
- `buffer_pool_size` は `pool_size` の倍数であるべきです（`buffer_pool_size mod pool_size = 0`）。

良い例：`pool_size = 4, buffer_pool_size = 4`（1:1）、`pool_size = 4, buffer_pool_size = 8`（2:1）。

悪い例：`pool_size = 4, buffer_pool_size = 5` — コネクター0が2つのバッファを担当し、他は1つのためスループットが不均一になります。

コネクターが切断されると、割り当てられたバッファワーカーは一時停止し、再接続時に自動で再開します。

### 順序保証

安定したブリッジ設定下ではトピック単位の順序は保持されます。`buffer_pool_size` を変更すると、前述の通り一時的に順序が乱れる可能性があります。

### パブリッシャーのアック挙動（QoS 1/2）

ブリッジにマッチするメッセージについて：

- クライアントへの `PUBACK`（QoS 1）および `PUBREC`（QoS 2）は、EMQX がディスクキューへのエンキュー確認（`enqueue_timeout_ms`）を待つ間、遅延することがあります。
- その待機がタイムアウトしても、EMQX はクライアントのパブリッシュ処理を完了します。ディスクキューエンキューのタイムアウトによるエラーはクライアントに通知されません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースに対応するプラグインパッケージ:

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.2 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.1.3 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.1.4 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_bridge_mqtt_dq-0.5.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
