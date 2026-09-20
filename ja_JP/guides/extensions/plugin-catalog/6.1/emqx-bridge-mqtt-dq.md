# MQTT ブリッジ（ディスクキュー付き）

このプラグインは、ローカルの MQTT メッセージを別の MQTT ブローカーに転送する際に、ディスクバッファを用いてレジリエンスを向上させます。

## 特長

- ブリッジごとのディスクバッファリング。
- リモートブローカーが利用不可の場合の自動リトライ。
- `${topic}` を使ったトピック書き換え対応。
- 1つのプラグインで複数のブリッジを管理可能。
- 設定変更はブリッジ単位で適用（変更のないブリッジは継続稼働）。

## 動作概要

1. ローカルのパブリッシュが各ブリッジの `filter_topic` とマッチするか判定。
2. マッチしたメッセージをディスクキューのパーティションに追記。
3. キューに溜まったメッセージをリモートブローカーにパブリッシュ。
4. ネットワークや接続障害でパブリッシュに失敗した場合は自動リトライ。
5. キューパーティションが `queue.max_total_bytes` を超えた場合は、そのパーティション内の最も古いレコードから破棄。

## 設定方法

EMQX ダッシュボード（推奨）またはプラグイン設定ファイルで設定可能です。

本番環境では、まず1つのブリッジでトラフィックを検証し、その後スケールアウトしてください。

### 設定ファイルの場所

関連する設定ファイルは以下の2種類です：

- インストールされたプラグインパッケージ内のデフォルトファイル：
  - docker インストール例（バージョン `0.2.0`）：
    `/opt/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`
  - deb/rpm インストール例（バージョン `0.2.0`）：
    `/usr/lib/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`

- ダッシュボードや API で設定保存後に EMQX が管理する永続化されたプラグイン設定ファイル：
  - docker：
    `/opt/emqx/data/plugins/emqx_bridge_mqtt_dq/config.hocon`
  - deb/rpm：
    `/var/lib/emqx/plugins/emqx_bridge_mqtt_dq/config.hocon`

`priv/config.hocon` はパッケージに含まれるデフォルトテンプレートです。  
`data/plugins/.../config.hocon` は設定変更後に EMQX が使用する永続化された設定ファイルです。

### クイックスタート（ダッシュボード）

1. プラグインを有効化します。  
2. `remotes` に再利用可能なリモートを1つ追加します。  
3. `bridges` にブリッジを1つ追加します。  
4. `remote`、`filter_topic`、`remote_topic` を設定します。  
5. 保存してリモートへの配信を検証します。  
6. ベースライン検証後にキューやプールの設定を調整します。

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

設定ファイルの文字列値は `${EMQXDQ_*}` 形式で OS 環境変数を参照できます。  
`EMQXDQ_` プレフィックスの付いた変数のみ解決され、それ以外の `${...}`（例：`remote_topic` の `${topic}`）はそのまま残ります。  
値全体がプレースホルダーでなければなりません（部分的な文字列補間は不可）。

**制限:** `${EMQXDQ_*}` は文字列型フィールド（例：`server`、`username`、`password`）のみ対応し、  
boolean（`enable`）、整数（`pool_size`、`keepalive_s`）には使えません。

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

環境変数が設定されていない場合、プラグインはエラーをログに記録し、元の `${EMQXDQ_...}` 文字列をそのまま値として使用します。  
これにより接続失敗（例：`"${EMQXDQ_REMOTE_SERVER}"` に接続しようとする）が発生し、ログやステータス API で誤設定が明示されます。

> **警告 — 動的設定更新とノードローカル環境変数**
>
> 環境変数は設定を解析するノードで解決されます。  
> EMQX ダッシュボード、REST API、CLI でプラグイン設定を更新すると、設定テキストが永続化され、クラスター内のすべてのノードで再解析されます。  
> ノードごとに環境変数の値が異なる（または未設定）場合、ノードごとに異なる実効設定となります。  
>
> そのため、**クラスター内のすべてのノードで同一の環境変数が設定されている場合を除き、ダッシュボードや API、CLI での `${EMQXDQ_...}` 置換は避けてください。**  
> ノードローカルなシークレットは、設定ファイルを直接編集してプラグインをリロードするか、Kubernetes ConfigMaps/Secrets などの一貫したシークレット注入機構を利用してください。

### 設定リファレンス

#### トップレベル

| フィールド | 型   | デフォルト | 説明                                  |
|------------|------|------------|-------------------------------------|
| `bridges`  | map  | `{}`       | ブリッジ名をキーとしたブリッジ設定のマップ。 |
| `remotes`  | map  | `{}`       | 再利用可能なリモートブローカー定義のマップ。 |

#### ブリッジ (`bridges.<name>`)

| フィールド             | 型       | デフォルト               | 説明                                                                                      |
|-----------------------|----------|-------------------------|-------------------------------------------------------------------------------------------|
| `enable`              | boolean  | `true`                  | このブリッジを有効または無効にする。                                                     |
| `remote`              | string   | —                       | `remotes` 内のリモートブローカー定義名。                                                |
| `proto_ver`           | string   | `"v4"`                  | MQTT プロトコルバージョン：`v3`、`v4`、`v5`。                                          |
| `clientid_prefix`     | string   | `"emqx-dq-<name>-"`     | 自動生成される MQTT クライアントIDのプレフィックス。各接続にユニークなインデックスが付与される（例：`emqx-dq-mybridge-0`）。省略可。 |
| `keepalive_s`         | integer  | `60`                    | MQTT キープアライブ間隔（秒）。                                                           |
| `pool_size`           | integer  | `4`                     | リモートブローカーへの MQTT 接続数。                                                     |
| `buffer_pool_size`    | integer  | `4`                     | ブリッジごとのディスクキューバッファワーカー数。以下の警告を参照してください。           |
| `filter_topic`        | string   | —                       | ローカルトピックフィルターパターン。`+` と `#` ワイルドカード対応。                      |
| `remote_topic`        | string   | —                       | 転送先トピックのテンプレート。元のトピックは `${topic}` で参照可能。                    |
| `enqueue_timeout_ms`  | integer  | `5000`                  | ディスクキュー書き込み確認待ちの最大ブロック時間（ms）。QoS > 0 のみ適用。QoS 0 は常に非同期。 |
| `max_inflight`        | integer  | `32`                    | リモートブローカーごとの未アックメッセージ最大数。ディスクキューからのバッチポップサイズと emqtt 送信ウィンドウを制御。 |
| `remote_qos`          | string   | `"${qos}"`              | リモートブローカーへのパブリッシュ時の QoS レベル（`"0"`、`"1"`、`"2"`）。デフォルトの `"${qos}"` は元メッセージの QoS を保持。 |
| `remote_retain`       | string   | `"${retain}"`           | リモートブローカーへのパブリッシュ時のリテインフラグ（`"true"`、`"false"`）。デフォルトの `"${retain}"` は元メッセージのリテインフラグを保持。 |
| `max_publish_retries` | integer  | `-1`                    | メッセージごとのパブリッシュリトライ最大回数。`-1` は無限リトライ。失敗した PUBACK や接続断で1回分消費。 |

#### リモート (`remotes.<name>`)

| フィールド         | 型       | デフォルト       | 説明                                         |
|--------------------|----------|-----------------|----------------------------------------------|
| `server`           | string   | —               | リモート MQTT ブローカーのアドレス（`host:port`）。 |
| `username`         | string   | `""`            | リモートブローカー認証用ユーザー名。           |
| `password`         | string   | `""`            | リモートブローカー認証用パスワード。           |
| `ssl.enable`       | boolean  | `false`         | リモートブローカー接続に SSL/TLS を有効化。    |
| `ssl.verify`       | string   | `verify_none`   | TLS 検証モード。サポート値：`verify_none`、`verify_peer`。 |
| `ssl.sni`          | string   | サーバーホスト名 | TLS サーバーネームインジケーション。デフォルトはサーバーホスト名。`"disable"` で無効化。 |
| `ssl.cacertfile`   | string   | —               | リモートブローカー証明書検証用 CA 証明書ファイル。 |
| `ssl.certfile`     | string   | —               | 相互 TLS 認証用クライアント証明書ファイル。     |
| `ssl.keyfile`      | string   | —               | 相互 TLS 認証用クライアント秘密鍵ファイル。     |

#### キュー

| フィールド              | 型     | デフォルト               | 説明                                                                                   |
|------------------------|--------|-------------------------|----------------------------------------------------------------------------------------|
| `queue.base_dir`       | string | `"emqx_bridge_mqtt_dq"` | ディスクキューのセグメントファイルのベースディレクトリ。ブリッジ名とパーティションインデックスが自動付加される（例：`<base_dir>/<bridge_name>/<index>`）。相対パスは EMQX の `data_dir` 基準で解決。絶対パスはそのまま使用。 |
| `queue_seg_bytes`      | string | `"100MB"`               | キューセグメントファイルの最大サイズ。                                                  |
| `queue.max_total_bytes`| string | `"1GB"`                 | パーティションごとの最大ディスクキューサイズ。各ブリッジは `buffer_pool_size` 個のパーティションを使うため、最大総ディスク使用量は `buffer_pool_size` × この値。超過時は最古メッセージを破棄。 |

## トピックテンプレート

`remote_topic` フィールドは `${topic}` プレースホルダーをサポートし、転送時に元のパブリッシュトピックに置換されます。

例：  
- `remote_topic = "${topic}"` — 元のトピックをそのまま転送。  
- `remote_topic = "forwarded/${topic}"` — プレフィックスを付加。  
- `remote_topic = "region1/${topic}"` — リージョンのネームスペースを付加。

`remote_topic` はキューからメッセージ送信時に適用されます。変更後は対象ブリッジの再起動後にキュー内メッセージに新テンプレートが適用されます。

## REST API

プラグインは EMQX プラグイン API ベースパス以下に4つのエンドポイントを公開します：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics` — Prometheus テキスト形式  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats` — JSON ダッシュボードスナップショット  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>` — 特定ブリッジのみ  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status` — プラグイン／クラスターのヘルスサマリー

すべての JSON エンドポイントは `application/json; charset=utf-8` を返します。

JSON API はクラスター集約型です。ノードが利用不可またはタイムアウト時もベストエフォートでデータを返しますが、レスポンスにクラスターの完全性メタデータが含まれます。

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
- `uptime_seconds`: 応答したノードの中で最大のプラグイン稼働時間（秒）  
- `summary`: 全ブリッジ合計値  
- `bridges`: 設定された各ブリッジの情報配列

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

ブリッジが現在の設定に存在しない場合は `404` を返します。

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

`/metrics` エンドポイントはクラスター集約済みの Prometheus テキスト形式を返し、以下のようなメトリクスを含みます：

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

- `enqueue`: ブリッジのエンキュー経路に受け入れたローカルメッセージ数  
- `dequeue`: ローカルキューから耐久的に削除したメッセージ数  
- `publish`: リモートブローカーに正常にパブリッシュしたメッセージ数  
- `drop`: キュー内で破棄されたメッセージ数  
- `retried_by_reason`: リトライ理由別のリトライ試行回数  
- `config_state`: 設定上のブリッジ状態（`enabled` または `disabled`）  
- `runtime_state`: 実際のワーカー／ストレージ状態（`running`、`degraded`、`purged`）  
- `status`: オペレーター向けのブリッジヘルス（`ok`、`partial`、`disconnected`、`disabled`、`error`）

現在のリトライ理由：

- `reason_code`: リモートブローカーが MQTT 理由コードで非成功を返しリトライ  
- `connect_failed`: 接続またはパブリッシュ失敗でリトライ  
- `timeout`: タイムアウトによるリトライ分類  
- `connection_lost`: クライアントプロセス終了に伴いインフライトメッセージをリトライ用に回収  
- `other`: 分類不能なリトライ理由のフォールバック

ブリッジが完全にドレインした後は以下が成立：

- `enqueue = dequeue = publish + drop`

#### バッファメトリクス

- `buffered`: その耐久キューパーティションに現在格納されているメッセージ数  
- バッファ行の `status`: ワーカーが存在すれば `running`、そうでなければ `missing`

このゲージは `replayq:open/1` の直後に更新されるため、新しいトラフィックが来る前でも永続化済みのディスク上メッセージが見えます。

#### コネクタメトリクス

- `backlog`: コネクタのバックログキューに滞留し、`emqtt` に送信待ちのメッセージ数  
- `inflight`: すでに `emqtt` に渡され、完了待ちのメッセージ数  
- コネクタ行の `status`: `connected`、`disconnected`、`partial`、`missing`、`unknown`

## 設定変更時の挙動

設定更新はブリッジ単位で適用されます：  
- 変更されたブリッジは再起動。  
- 削除されたブリッジは停止。  
- 無効化されたブリッジは停止し、キューディレクトリをパージ。  
- 新規ブリッジは起動。  
- 変更のないブリッジは継続稼働。

プラグイン全体は設定更新ごとに再起動されません。  
ただし再起動される各ブリッジには短い引き継ぎ時間があり、その間にマッチするメッセージが破棄される可能性があります。  
トラフィックが少ない時間帯にブリッジ影響のある変更を適用してください。

### 設定変更前の注意

1. 影響を受けるブリッジを特定。  
2. トラフィックが少ない時間帯に適用。  
3. ダッシュボードのステータスやログで再起動・再接続エラーを監視。  
4. 重要なパイプラインは変更後にエンドツーエンド配信を検証。

### `queue.base_dir` の変更

有効なブリッジで `queue.base_dir` を変更すると、新しいディレクトリでブリッジが再起動されます。  
実際のキューパスは `<base_dir>/<bridge_name>/<index>` です。  
古いディレクトリは自動で削除されず、ディスク上に孤立したデータとして残ります。  
不要な場合は新しいパスでブリッジが稼働していることを確認後、手動で削除してください。

### `buffer_pool_size` の変更

`buffer_pool_size` はブリッジごとのディスクキューパーティション数を制御します。  
メッセージは `erlang:phash2(Topic, buffer_pool_size)` でパーティションに割り当てられます。  
この値を変更すると以下の副作用があります：

1. **プール縮小**（例：8 → 4）：新サイズ以上のインデックスのパーティションは消費されなくなります。古いファイルは `queue.base_dir` 配下に残り手動でのクリーンアップが必要です。  
2. **プール拡大**（例：4 → 8）：ハッシュ空間が変わるため、以前パーティション N に割り当てられていたトピックがパーティション M に変わる可能性があります。既存の古いパーティション内メッセージは順序を保って配信されますが、新しいメッセージは別パーティションに行くため、トピック単位の順序が一時的に崩れます。  
3. **ブリッジ単位のドロップウィンドウ**：`buffer_pool_size` の変更でブリッジが再起動されるため、引き継ぎ中にインフライトのマッチメッセージが破棄される可能性があります。

## メッセージ配信保証

このプラグインは通常動作時に **少なくとも1回以上の配信（at-least-once）** を提供し、持続的障害時は **ベストエフォート配信** となります。以下のシナリオでメッセージが失われる可能性があります。

### ディスクキューのオーバーフロー

キューパーティションが `queue.max_total_bytes` を超えた場合、そのパーティション内の最も古いメッセージが静かに破棄されます。  
警告ログ（`mqtt_dq_buffer_overflow`）が定期的に出力されます（メッセージごとではありません）。

**対策**：`queue.max_total_bytes` を増やす、`buffer_pool_size` を増やして負荷分散、またはメッセージスループットを減らす。

### リモートブローカーがパブリッシュを拒否

リモートブローカーが PUBACK（QoS 1）または PUBREC（QoS 2）で非成功の MQTT 理由コードを返した場合、コネクターは最大3回までリトライします。  
すべてのリトライが尽きるとメッセージは破棄され、警告ログ（`mqtt_dq_publish_dropped`）が出力されます。

主な拒否理由コード：

| コード | 意味（MQTT 5.0）               |
|--------|-------------------------------|
| 16     | サブスクライバーなし           |
| 128    | 未指定のエラー                 |
| 131    | 実装固有のエラー               |
| 135    | 認可されていない               |
| 144    | トピック名が無効              |
| 145    | パケット識別子が使用中        |
| 151    | クォータ超過                  |

注：理由コード 0（成功）と 16（サブスクライバーなし）は成功扱いでリトライしません。

**対策**：リモートブローカーの ACL やトピックポリシーを確認し、ログで具体的な理由コードを調査。

### 接続障害の繰り返し

リモートブローカーとの接続が切断されるたびに、未アックのメッセージはリトライ回数を1回消費します。  
成功配信なしに3回連続で接続障害が発生するとメッセージは破棄されます。

例：ネットワーク障害中のメッセージ  
1. ローカルキューに追加（リトライカウンター=3）  
2. リモート再接続、メッセージ送信 → ACK 前に切断（カウンター=2）  
3. 再接続、再送 → 切断（カウンター=1）  
4. 再接続、再送 → 拒否または切断（カウンター=0）  
5. メッセージ破棄、警告ログ出力

**対策**：リモートブローカーが繰り返し接続不可となる原因を調査。  
一時的なネットワーク障害は透過的に処理されますが、持続的な不安定さは問題です。

### エンキュー時のバックプレッシャー（QoS > 0 のローカルパブリッシュ）

QoS 1 または 2 のクライアントがブリッジにマッチするメッセージをパブリッシュすると、プラグインはバッファワーカーのメールボックスにメッセージを送信し、ディスク書き込み確認まで最大 `enqueue_timeout_ms`（デフォルト 5000 ms）ブロックします。

このタイムアウトが発生してもメッセージ自体は失われません。すでにバッファワーカーの Erlang メールボックスに入っており、最終的にディスクキューに書き込まれます。  
タイムアウトはローカルパブリッシュ経路のブロック時間を制御するだけです。

重要な理由：`message.publish` フックは MQTT セッションプロセス内で実行されます。  
フックがブロック中は、そのクライアントの他メッセージ処理が停止します。  
バッファワーカーが遅い（ディスク I/O ストールやメールボックスのバックログ増大）場合、タイムアウトがなければクライアントセッションが無期限に停止する恐れがあります。

タイムアウト発生時の挙動：  
1. セッションプロセスは待機を解除し通常処理を継続。  
2. クライアントは通常通り PUBACK/PUBREC を受信し、エラーは通知されない。  
3. 警告ログ（`mqtt_dq_enqueue_timeout`）を出力。  
4. メッセージはバッファワーカーのメールボックスに残り、追いついた時点でディスクキューに書き込まれる。

リスクは間接的です。バッファワーカーが継続的に遅延するとメールボックスが増大しメモリ使用量が増えるため、ブリッジがメッセージレートに追いつけていない兆候です。

**対策**：`buffer_pool_size` を増やして負荷分散、`queue.base_dir` に高速ストレージを使う、またはマッチするトピックのメッセージレートを下げる。

注：QoS 0 のローカルパブリッシュは非同期でエンキューされ、パブリッシュセッションにバックプレッシャーはかかりません。

### ブリッジ再起動時のウィンドウ

設定変更やプラグインリロード、有効／無効切り替えでブリッジが再起動すると、マッチするメッセージが一時的に捕捉されない可能性があります。

**対策**：トラフィックが少ない時間帯に設定変更を適用してください。

### QoS 0 の TCP レベル配信

リモートブローカーへの QoS 0 パブリッシュは、メッセージがローカル TCP 送信バッファに到達した時点で配信成功とみなされます。  
もしリモートブローカーが TCP スタック受理後にクラッシュすると、メッセージはコネクターにエラー通知されずに失われる可能性があります。

これは MQTT QoS 0 の仕様であり、本プラグイン固有の問題ではありません。

## 運用上の注意

### 永続化

バッファされたメッセージは以下の状況でも保持されます：  
- EMQX ノード再起動時  
- プラグインのリロードやアップグレード時  
- リモートブローカーへの一時的なネットワーク障害時

### キュー制限

キュー使用量が `queue.max_total_bytes` を超えたパーティションでは、最古メッセージが破棄され警告ログが出力されます。

### プールサイズ

各バッファワーカーは `BufferIndex rem pool_size` により1つのコネクターに割り当てられます。負荷分散のため：

- `buffer_pool_size` は `pool_size` 以上に設定してください。  
- `buffer_pool_size` は `pool_size` の倍数であるべきです（`buffer_pool_size mod pool_size = 0`）。

良い例：`pool_size = 4, buffer_pool_size = 4`（1:1）、`pool_size = 4, buffer_pool_size = 8`（2:1）。  
悪い例：`pool_size = 4, buffer_pool_size = 5` — コネクター0が2つのバッファを担当し他は1つで、スループットが不均一になります。

コネクターが切断されると、割り当てられたバッファワーカーは自動的に一時停止し、再接続後に再開します。

### 順序性

安定したブリッジ設定下ではトピック単位の順序性が保持されます。  
`buffer_pool_size` を変更すると、前述の通り一時的に順序が乱れる可能性があります。

### パブリッシャーの ACK 挙動（QoS 1/2）

ブリッジにマッチするメッセージでは：  
- EMQX はディスクキューへのエンキュー確認（`enqueue_timeout_ms`）を待つ間、パブリッシャーへの `PUBACK`（QoS 1）や `PUBREC`（QoS 2）を遅延させることがあります。  
- タイムアウトした場合でも、クライアントのパブリッシュフローは完了し、ディスクキューのエンキュータイムアウトによるエラーは通知されません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースの tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.2 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.1.3 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.1.4 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_bridge_mqtt_dq-0.5.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
