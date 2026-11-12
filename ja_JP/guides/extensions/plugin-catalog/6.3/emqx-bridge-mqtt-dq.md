# MQTT ブリッジ（ディスクキュー付き）

このプラグインは、ローカルの MQTT メッセージを別の MQTT ブローカーへ転送する際に、ディスクバッファを利用してレジリエンスを向上させるために使用します。

## 特長

- ブリッジごとのディスクバッファリング。
- リモートブローカーが利用できない場合の自動リトライ。
- `${topic}` を使ったトピック書き換え対応。
- 1つのプラグインで複数のブリッジを管理可能。
- 設定更新はブリッジ単位で適用（変更のないブリッジは継続稼働）。

## 動作概要

1. ローカルのパブリッシュが各ブリッジの `filter_topic` とマッチするか判定。
2. マッチしたメッセージをディスクキューのパーティションに追記。
3. キュー内のメッセージをリモートブローカーへパブリッシュ。
4. ネットワークや接続の問題でパブリッシュ失敗時は自動リトライ。
5. キューパーティションのサイズが `queue.max_total_bytes` を超えた場合、古いレコードから順に破棄。

## 設定

EMQX ダッシュボード（推奨）またはプラグイン設定ファイルから設定可能です。

本番環境では、まず1つのブリッジでトラフィックを検証し、その後スケールアウトしてください。

### 設定ファイルの場所

関連する設定ファイルの場所は2つあります：

- インストール済みプラグインパッケージ内のデフォルトファイル：
  - Docker インストール例（バージョン `0.2.0`）：
    `/opt/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`
  - deb/rpm インストール例（バージョン `0.2.0`）：
    `/usr/lib/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`

- ダッシュボードや API 経由で設定保存後に EMQX が管理する永続化プラグイン設定ファイル：
  - Docker：
    `/opt/emqx/data/plugins/emqx_bridge_mqtt_dq/config.hocon`
  - deb/rpm：
    `/var/lib/emqx/plugins/emqx_bridge_mqtt_dq/config.hocon`

`priv/config.hocon` はパッケージに含まれるデフォルトテンプレートです。  
`data/plugins/.../config.hocon` は設定変更後に EMQX が使用する永続化設定ファイルです。

### クイックスタート（ダッシュボード）

1. プラグインを有効化。
2. `remotes` に再利用可能なリモートを1つ追加。
3. `bridges` にブリッジを1つ追加。
4. `remote`、`filter_topic`、`remote_topic` を設定。
5. 保存してリモート配信を検証。
6. ベースライン検証後にキューやプール設定を調整。

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

設定ファイル内の任意の文字列値は `${EMQXDQ_*}` 形式で OS 環境変数を参照できます。  
`EMQXDQ_` プレフィックスの付いた変数のみ解決され、その他の `${...}` パターン（例：`remote_topic` 内の `${topic}`）はそのまま残ります。  
値全体がプレースホルダーである必要があり、部分的な埋め込み（例：`"prefix-${EMQXDQ_VAR}-suffix"`）はサポートされません。

**制限**：`${EMQXDQ_*}` 置換は文字列型フィールド（例：`server`、`username`、`password`）にのみ有効で、  
ブール型（`enable`）、整数型（`pool_size`、`keepalive_s`）には使用できません。

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

環境変数が設定されていない場合、プラグインはエラーをログに出力し、元の `${EMQXDQ_...}` 文字列をリテラル値として保持します。  
これにより接続失敗（例：`"${EMQXDQ_REMOTE_SERVER}"` に接続しようとする）が発生し、ログやステータス API で設定ミスが明示されます。

> **警告 — 動的設定更新とノードローカル環境変数**
>
> 環境変数は設定解析時に、そのノード上で解決されます。  
> EMQX ダッシュボード、REST API、CLI でプラグイン設定を更新すると、設定テキストが永続化され、クラスタ内のすべてのノードで再解析されます。  
> もしノードごとに環境変数の値が異なるか欠落していると、ノードごとに異なる実効設定となります。  
>
> そのため、**クラスタ内のすべてのノードで同一の環境変数が設定されている場合を除き、ダッシュボードや API、CLI での `${EMQXDQ_...}` 置換は避けてください。**  
> ノードローカルな秘密情報は、設定ファイルを直接編集してプラグインをリロードするか、Kubernetes ConfigMaps/Secrets のような一貫した秘密注入機構を利用してください。

### 設定リファレンス

#### トップレベル

| フィールド | 型   | デフォルト | 説明                                |
|------------|------|------------|-------------------------------------|
| `bridges`  | map  | `{}`       | ブリッジ名をキーとするブリッジ設定のマップ。 |
| `remotes`  | map  | `{}`       | 再利用可能なリモートブローカー定義のマップ。 |

#### ブリッジ（`bridges.<name>`）

| フィールド             | 型       | デフォルト               | 説明                                                                                   |
|-----------------------|----------|-------------------------|----------------------------------------------------------------------------------------|
| `enable`              | boolean  | `true`                  | このブリッジを有効または無効にする。                                                  |
| `remote`              | string   | —                       | `remotes` 内のリモートブローカー定義名。                                             |
| `proto_ver`           | string   | `"v4"`                  | MQTT プロトコルバージョン。`v3`、`v4`、`v5` のいずれか。                             |
| `clientid_prefix`     | string   | `"emqx-dq-<name>-"`     | 自動生成される MQTT クライアントIDのプレフィックス。各接続はユニークなインデックスを付加（例：`emqx-dq-mybridge-0`）。省略可。 |
| `keepalive_s`         | integer  | `60`                    | MQTT キープアライブ間隔（秒）。                                                       |
| `pool_size`           | integer  | `4`                     | リモートブローカーへの MQTT 接続数。                                                 |
| `buffer_pool_size`    | integer  | `4`                     | ブリッジごとのディスクキューバッファワーカー数。下記の警告を参照。                   |
| `filter_topic`        | string   | —                       | ローカルトピックのフィルタパターン。`+` と `#` ワイルドカード対応。                  |
| `remote_topic`        | string   | —                       | 転送先トピックのテンプレート。元のトピックは `${topic}` で参照可能。                 |
| `enqueue_timeout_ms`  | integer  | `5000`                  | ディスクキュー書き込み確認待ちの最大ブロック時間（ms）。QoS > 0 のみ適用。QoS 0 は常に非同期。 |
| `max_inflight`        | integer  | `32`                    | リモートブローカーへの未アックメッセージ最大数。ディスクキューからのバッチポップサイズや emqtt 送信ウィンドウを制御。 |
| `remote_qos`          | string   | `"${qos}"`              | リモートブローカーへのパブリッシュ時の QoS レベル（`"0"`、`"1"`、`"2"`）。デフォルトは元メッセージの QoS を保持。 |
| `remote_retain`       | string   | `"${retain}"`           | リモートブローカーへのパブリッシュ時のリテインフラグ（`"true"`、`"false"`）。デフォルトは元メッセージのリテインを保持。 |
| `max_publish_retries` | integer  | `-1`                    | メッセージごとのパブリッシュリトライ回数。`-1` は無限リトライ。失敗した PUBACK や接続切断で1回消費。 |

#### リモート（`remotes.<name>`）

| フィールド          | 型       | デフォルト       | 説明                                               |
|---------------------|----------|-----------------|----------------------------------------------------|
| `server`            | string   | —               | リモート MQTT ブローカーのアドレス（`host:port`）。 |
| `username`          | string   | `""`            | リモートブローカー認証用ユーザー名。               |
| `password`          | string   | `""`            | リモートブローカー認証用パスワード。               |
| `ssl.enable`        | boolean  | `false`         | リモートブローカー接続に SSL/TLS を使用するか。    |
| `ssl.verify`        | string   | `verify_none`   | TLS 検証モード。`verify_none`、`verify_peer` をサポート。 |
| `ssl.sni`           | string   | サーバーホスト名 | TLS Server Name Indication。デフォルトはサーバーホスト名。`"disable"` で無効化可能。 |
| `ssl.cacertfile`    | string   | —               | リモートブローカー証明書検証用の CA 証明書ファイル。 |
| `ssl.certfile`      | string   | —               | 相互 TLS 認証用のクライアント証明書ファイル。       |
| `ssl.keyfile`       | string   | —               | 相互 TLS 認証用のクライアント秘密鍵ファイル。       |

#### キュー

| フィールド               | 型     | デフォルト                    | 説明                                                                                     |
|-------------------------|--------|------------------------------|------------------------------------------------------------------------------------------|
| `queue.base_dir`         | string | `"emqx_bridge_mqtt_dq"`       | ディスクキューセグメントファイルのベースディレクトリ。ブリッジ名とパーティションインデックスが自動付加される（例：`<base_dir>/<bridge_name>/<index>`）。相対パスは EMQX の `data_dir` に対して解決。絶対パスはそのまま使用。 |
| `queue_seg_bytes`        | string | `"100MB"`                    | キューセグメントファイルの最大サイズ。                                                    |
| `queue.max_total_bytes`  | string | `"1GB"`                      | パーティションごとの最大ディスクキューサイズ。各ブリッジは `buffer_pool_size` 個のパーティションを使用するため、最大総ディスク使用量は `buffer_pool_size` × この値。超過時は古いメッセージを破棄。 |

## トピックテンプレート

`remote_topic` フィールドは `${topic}` プレースホルダーをサポートし、転送時に元のパブリッシュトピックに置換されます。

例：  
- `remote_topic = "${topic}"` — 元のトピックをそのまま転送。  
- `remote_topic = "forwarded/${topic}"` — プレフィックスを付加。  
- `remote_topic = "region1/${topic}"` — リージョンネームスペースを追加。

`remote_topic` はキューからメッセージを送出する際に適用されます。変更後は該当ブリッジの再起動後に新テンプレートが反映されます。

## REST API

プラグインは EMQX プラグイン API ベースパス配下に4つのエンドポイントを公開します：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics` — Prometheus テキストフォーマット  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats` — JSON ダッシュボードスナップショット  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>` — 特定ブリッジのみ  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status` — プラグイン／クラスターのヘルスサマリー

すべての JSON エンドポイントは `application/json; charset=utf-8` を返します。

JSON API はクラスタ集約型です。ノードが利用不可またはタイムアウトした場合でも、ベストエフォートのデータを返しますが、レスポンスにクラスタの完全性メタデータが含まれます。

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

- `cluster`：クラスタの完全性と失敗ノード情報  
- `uptime_seconds`：応答ノード間で観測された最大プラグインアップタイム  
- `summary`：全ブリッジ合計値  
- `bridges`：設定された各ブリッジのエントリ

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

ブリッジが現在の設定に存在しない場合、API は `404` を返します。

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

`/metrics` エンドポイントは Prometheus テキスト形式でクラスタ集約済みのメトリクスを返します。例：

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

- `enqueue`：ローカルでブリッジのエンキュー経路に受け入れたメッセージ数  
- `dequeue`：ローカルキューから耐久的に削除されたメッセージ数  
- `publish`：リモートブローカーへ正常にパブリッシュされたメッセージ数  
- `drop`：キュー内で破棄されたメッセージ数  
- `retried_by_reason`：リトライ理由別の試行回数  
- `config_state`：設定上のブリッジ状態（`enabled` または `disabled`）  
- `runtime_state`：実際のワーカー／ストレージ状態（`running`、`degraded`、`purged`）  
- `status`：運用者向けのブリッジヘルス（`ok`、`partial`、`disconnected`、`disabled`、`error`）

現在のリトライ理由：

- `reason_code`：リモートブローカーが非成功 MQTT リーズンコードを返しリトライ  
- `connect_failed`：接続またはパブリッシュ失敗によるリトライ  
- `timeout`：タイムアウトによるリトライ分類  
- `connection_lost`：関連クライアントプロセス終了に伴いインフライトメッセージをリトライ用に回収  
- `other`：分類不能なリトライ理由のフォールバック

ブリッジが完全にドレインした後は以下が成立：

- `enqueue = dequeue = publish + drop`

#### バッファメトリクス

- `buffered`：該当耐久キューパーティションに現在格納されているメッセージ数  
- バッファ行の `status`：ワーカーが存在すれば `running`、そうでなければ `missing`

このゲージは `replayq:open/1` 後すぐに更新されるため、永続化済みのディスク上メッセージも新規トラフィック到着前に可視化されます。

#### コネクタメトリクス

- `backlog`：`emqtt` へ送出待ちのコネクタバックログキュー内メッセージ数  
- `inflight`：すでに `emqtt` に渡され完了待ちのメッセージ数  
- コネクタ行の `status`：`connected`、`disconnected`、`partial`、`missing`、`unknown` のいずれか

## 設定変更時の挙動

設定更新はブリッジ単位で適用されます：

- 変更されたブリッジは再起動。  
- 削除されたブリッジは停止。  
- 無効化されたブリッジは停止しキューディレクトリをパージ。  
- 新規ブリッジは起動。  
- 変更のないブリッジは継続稼働。

プラグイン全体は設定更新ごとに再起動されません。  
ただし、再起動された各ブリッジには短時間の引き継ぎウィンドウがあり、その間にマッチするメッセージが破棄される可能性があります。  
トラフィックが少ない時間帯にブリッジに影響する変更を適用してください。

### 設定変更前の注意

1. 影響を受けるブリッジを特定。  
2. トラフィックが少ない時間帯に適用。  
3. ダッシュボードのステータスとログで再起動や再接続エラーを監視。  
4. 重要なパイプラインは変更後にエンドツーエンドの配信検証を実施。

### `queue.base_dir` の変更

有効なブリッジで `queue.base_dir` を変更すると、新しいディレクトリでブリッジが再起動します。  
実際のキューパスは `<base_dir>/<bridge_name>/<index>` です。  
古いディレクトリは自動的に削除されず、オーファンデータとしてディスクに残ります。  
不要な場合は、新しいパスでブリッジが稼働していることを確認後に手動で削除してください。

### `buffer_pool_size` の変更

`buffer_pool_size` はブリッジごとのディスクキューパーティション数を制御します。  
メッセージは `erlang:phash2(Topic, buffer_pool_size)` によってパーティションに割り当てられます。  
この値の変更には以下の重要な副作用があります：

1. **プール縮小**（例：8 → 4）：新サイズ以上のインデックスを持つパーティションは消費されなくなります。古いファイルは `queue.base_dir` 配下に残るため手動クリーンアップが必要。  
2. **プール拡大**（例：4 → 8）：ハッシュ空間が変わるため、以前パーティション N に割り当てられていたトピックがパーティション M に割り当てられる可能性があります。既存の古いパーティション内のメッセージは順序を保って配信されますが、新しいトピックのメッセージは別パーティションに送られ、トピック単位の完全な順序保証が一時的に破綻します。  
3. **ブリッジ単位のドロップウィンドウ**：`buffer_pool_size` の変更はブリッジを再起動させるため、引き継ぎ中にマッチするメッセージが破棄される可能性があります。

## メッセージ配信保証

このプラグインは通常動作下での **at-least-once** 配信を提供し、持続的な障害時は **ベストエフォート** 配信となります。以下のシナリオでメッセージが失われる可能性があります。

### ディスクキューのオーバーフロー

キューパーティションが `queue.max_total_bytes` を超えると、そのパーティション内の最も古いメッセージが静かに破棄され、新しいデータの領域を確保します。  
警告ログ（`mqtt_dq_buffer_overflow`）は定期的に出力されます（メッセージ単位ではありません）。

**対策**：`queue.max_total_bytes` を増やす、`buffer_pool_size` を増やして負荷を分散、またはメッセージスループットを減らす。

### リモートブローカーによるパブリッシュ拒否

リモートブローカーが PUBACK（QoS 1）または PUBREC（QoS 2）で非成功 MQTT リーズンコードを返した場合、コネクターは最大3回リトライします。  
リトライが尽きるとメッセージは破棄され、警告ログ（`mqtt_dq_publish_dropped`）が出力されます。

主な拒否理由コード：

| コード | 意味（MQTT 5.0）               |
|--------|-------------------------------|
| 16     | マッチするサブスクライバーなし |
| 128    | 未指定のエラー                 |
| 131    | 実装固有のエラー              |
| 135    | 認可されていない              |
| 144    | トピック名が無効             |
| 145    | パケット識別子が使用中       |
| 151    | クォータ超過                 |

注：コード 0（成功）および 16（マッチするサブスクライバーなし）は正常配信扱いでリトライされません。

**対策**：リモートブローカーの ACL やトピックポリシーを確認し、ログで具体的な理由コードを調査。

### 接続の繰り返し失敗

リモートブローカーへの接続が切断されるたびに、未アックのメッセージはリトライ回数を1回消費します。  
3回連続で接続失敗が発生し成功配信がない場合、メッセージは破棄されます。

例：ネットワーク障害中にパブリッシュされたメッセージ  
1. ローカルキューに格納（リトライカウンター=3）  
2. リモート再接続、メッセージ送出 — ACK 前に切断（リトライカウンター=2）  
3. 再接続、再送出 — 切断（リトライカウンター=1）  
4. 再接続、再送出 — 拒否または切断（リトライカウンター=0）  
5. メッセージ破棄、警告ログ出力

**対策**：リモートブローカーが繰り返し到達不能になる原因を調査。  
一時的なネットワーク断は透過的に処理されますが、このケースは持続的な不安定性が必要。

### エンキュー時のバックプレッシャー（QoS > 0 ローカルパブリッシュ）

QoS 1 または 2 のクライアントがブリッジにマッチするメッセージをパブリッシュすると、プラグインはバッファワーカーのメールボックスにメッセージを送信し、ディスク書き込み確認まで最大 `enqueue_timeout_ms`（デフォルト 5000 ms）までパブリッシュセッションをブロックします。

このタイムアウトが発生してもメッセージ自体は失われません。  
メッセージはすでにバッファワーカーの Erlang メールボックスにあり、後でディスクキューに書き込まれます。  
タイムアウトはローカルパブリッシュ経路のブロック時間を制御するだけです。

重要な理由：`message.publish` フックは MQTT セッションプロセス内で実行されます。  
フックがブロック中は、そのクライアントからの他メッセージ処理が停止します。  
バッファワーカーが遅い（例：ディスク I/O ストールやメールボックスのバックログ増大）場合、タイムアウトにより1つの遅いブリッジがクライアントセッションを無期限に停滞させるのを防ぎます。

タイムアウト時の挙動：  
1. セッションプロセスは待機を終了し通常処理を継続。  
2. クライアントには通常通り PUBACK/PUBREC が返され、エラーは発生しない。  
3. 警告ログ（`mqtt_dq_enqueue_timeout`）を出力。  
4. メッセージはバッファワーカーのメールボックスに残り、追いついた時点でディスクキューに書き込まれる。

リスクは間接的です。バッファワーカーが継続的に遅延するとメールボックスが無制限に増大し、メモリ使用量が増加します。これはブリッジが受信メッセージレートに追いつけていない兆候です。

**対策**：`buffer_pool_size` を増やして負荷分散、`queue.base_dir` に高速ストレージを使用、またはマッチするトピックのメッセージレートを減らす。

注：QoS 0 のローカルパブリッシュは非同期でエンキューされ、セッションにバックプレッシャーはかかりません。

### ブリッジ再起動ウィンドウ

設定変更、プラグインリロード、有効化／無効化切り替えによりブリッジが再起動すると、マッチするメッセージが一時的に捕捉されない可能性があります。

**対策**：トラフィックが少ない時間帯に設定変更を適用してください。

### QoS 0 の TCP レベル配信

QoS 0 のリモートブローカーへのパブリッシュは、メッセージがローカルの TCP 送信バッファに到達した時点で成功とみなされます。  
リモートブローカーが TCP スタックがデータを受け入れた後にクラッシュし、ブローカー処理前に停止した場合、メッセージはコネクターにエラーを返さずに失われる可能性があります。

これは MQTT QoS 0 の仕様上の特性であり、本プラグイン固有の問題ではありません。

## 運用上の注意

### 永続化

バッファされたメッセージは以下の状況でも保持されます：

- EMQX ノード再起動  
- プラグインのリロードやアップグレード  
- リモートブローカーへの一時的なネットワーク断

### キュー制限

パーティションのキュー使用量が `queue.max_total_bytes` を超えると、古いメッセージが破棄されます。警告ログが出力されます。

### プールサイズ

各バッファワーカーは `BufferIndex rem pool_size` により正確に1つのコネクターに割り当てられます。均等な負荷分散のため：

- `buffer_pool_size` は `pool_size` 以上であるべきです。  
- `buffer_pool_size` は `pool_size` の倍数であるべきです（`buffer_pool_size mod pool_size = 0`）。

良い例：`pool_size = 4, buffer_pool_size = 4`（1:1）、`pool_size = 4, buffer_pool_size = 8`（2:1）。  
悪い例：`pool_size = 4, buffer_pool_size = 5` — コネクター0が2つのバッファを担当し他は1つで不均等なスループットに。

コネクターが切断されると、割り当てられたバッファワーカーは一時停止し、再接続後に自動再開します。

### 順序保証

安定したブリッジ設定下ではトピック単位の順序が保持されます。  
`buffer_pool_size` を変更すると、前述の通り一時的に順序が乱れる可能性があります。

### パブリッシャーのアック挙動（QoS 1/2）

ブリッジにマッチするメッセージについて：

- クライアントへの `PUBACK`（QoS 1）および `PUBREC`（QoS 2）は、EMQX がディスクキューへのエンキュー確認（`enqueue_timeout_ms`）を待つ間、遅延することがあります。  
- エンキュー待ちがタイムアウトしても、EMQX はクライアントのパブリッシュ処理を完了します。ディスクキューエンキュータイムアウトによるエラーはクライアントに返されません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースの tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.3.0 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.3.1 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.3.2 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.2/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.2/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.3.3 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_bridge_mqtt_dq-0.5.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
