# MQTT ブリッジ（ディスクキュー付き）

このプラグインは、ローカルの MQTT メッセージを別の MQTT ブローカーに転送する際に、ディスクバッファを利用してレジリエンスを向上させます。

## 特長

- ブリッジごとのディスクバッファリング。
- リモートブローカーが利用不可の場合の自動リトライ。
- `${topic}` を使ったトピック書き換え対応。
- 1つのプラグインで複数のブリッジを管理可能。
- 設定更新はブリッジ単位で適用（変更のないブリッジは継続稼働）。

## 動作概要

1. ローカルのパブリッシュが各ブリッジの `filter_topic` とマッチするか判定。
2. マッチしたメッセージをディスクキューのパーティションに追記。
3. キュー内のメッセージをリモートブローカーにパブリッシュ。
4. ネットワークや接続障害でパブリッシュに失敗した場合は自動リトライ。
5. キューパーティションが `queue.max_total_bytes` を超えた場合、最も古いレコードから削除。

## 設定

EMQX ダッシュボード（推奨）またはプラグイン設定ファイルから設定可能です。

本番環境では、まず1つのブリッジでトラフィックを検証し、その後スケールアウトしてください。

### 設定ファイルの場所

関連する設定ファイルは以下の2種類です：

- インストール済みプラグインパッケージ内のデフォルトファイル：
  - docker インストール例（バージョン `0.2.0`）：
    `/opt/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`
  - deb/rpm インストール例（バージョン `0.2.0`）：
    `/usr/lib/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`

- ダッシュボードや API で設定保存後に EMQX が管理する永続化プラグイン設定ファイル：
  - docker：
    `/opt/emqx/data/plugins/emqx_bridge_mqtt_dq/config.hocon`
  - deb/rpm：
    `/var/lib/emqx/plugins/emqx_bridge_mqtt_dq/config.hocon`

`priv/config.hocon` はパッケージに含まれるデフォルトテンプレートであり、`data/plugins/.../config.hocon` は設定変更後に EMQX が使用する永続化設定ファイルです。

### クイックスタート（ダッシュボード）

1. プラグインを有効化。
2. `remotes` に再利用可能なリモートを1つ追加。
3. `bridges` にブリッジを1つ追加。
4. `remote`、`filter_topic`、`remote_topic` を設定。
5. 保存してリモートへの配信を検証。
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

設定ファイル内の任意の文字列値は `${EMQXDQ_*}` 形式で OS 環境変数を参照可能です。`EMQXDQ_` プレフィックスの付いた変数のみ解決され、それ以外の `${...}` パターン（例：`remote_topic` 内の `${topic}`）はそのまま残ります。値全体がプレースホルダーである必要があり、部分的な文字列補間（例：`"prefix-${EMQXDQ_VAR}-suffix"`）はサポートされません。

**制限:** `${EMQXDQ_*}` の置換は文字列型フィールド（例：`server`、`username`、`password`）のみ対応し、ブール型（`enable`）、整数型（`pool_size`、`keepalive_s`）には使用できません。

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

環境変数が設定されていない場合、プラグインはエラーをログに記録し、元の `${EMQXDQ_...}` 文字列をリテラル値として保持します。これにより接続失敗（例：`"${EMQXDQ_REMOTE_SERVER}"` に接続しようとする）が発生し、ログおよびステータス API で誤設定が明示されます。

> **警告: 動的設定更新とノードローカル環境変数について**
>
> 環境変数は設定を解析するノード上で解決されます。EMQX ダッシュボード、REST API、CLI からプラグイン設定を更新すると、設定テキストが永続化され、クラスタ内のすべてのノードで再解析されます。ノードごとに環境変数の値が異なる（または未設定）場合、ノードごとに異なる実効設定となります。
>
> そのため、**クラスタ内のすべてのノードで同一の環境変数が設定されていることが確実でない限り、ダッシュボード、API、CLI での設定更新に `${EMQXDQ_...}` を使うのは避けてください**。ノードローカルなシークレットは、設定ファイルを直接編集してプラグインをリロードするか、Kubernetes ConfigMaps/Secrets のような一貫したシークレット注入機構を利用してください。

### 設定リファレンス

#### トップレベル

| フィールド | 型   | デフォルト | 説明                              |
|------------|------|------------|----------------------------------|
| `bridges`  | map  | `{}`       | ブリッジ名をキーとしたブリッジ設定のマップ。 |
| `remotes`  | map  | `{}`       | 再利用可能なリモートブローカー定義のマップ。 |

#### ブリッジ (`bridges.<name>`)

| フィールド             | 型      | デフォルト               | 説明                                                                                   |
|-----------------------|---------|-------------------------|----------------------------------------------------------------------------------------|
| `enable`              | boolean | `true`                  | このブリッジを有効または無効にします。                                               |
| `remote`              | string  | —                       | `remotes` 内のリモートブローカー定義名。                                             |
| `proto_ver`           | string  | `"v4"`                  | MQTT プロトコルバージョン：`v3`、`v4`、`v5`。                                       |
| `clientid_prefix`     | string  | `"emqx-dq-<name>-"`     | 自動生成される MQTT クライアントIDのプレフィックス。各接続にユニークなインデックスが付加されます（例：`emqx-dq-mybridge-0`）。省略可。 |
| `keepalive_s`         | integer | `60`                    | MQTT のキープアライブ間隔（秒）。                                                    |
| `pool_size`           | integer | `4`                     | リモートブローカーへの MQTT 接続数。                                                |
| `buffer_pool_size`    | integer | `4`                     | ブリッジごとのディスクキューバッファワーカー数。以下の注意を参照してください。       |
| `filter_topic`        | string  | —                       | ローカルトピックフィルタパターン。`+` と `#` ワイルドカード対応。                    |
| `remote_topic`        | string  | —                       | 転送先トピックのテンプレート。元のトピックは `${topic}` で参照可能。                 |
| `enqueue_timeout_ms`  | integer | `5000`                  | ディスクキューへの書き込み確認待ちの最大ブロック時間（ms）。QoS > 0 のみ適用。QoS 0 は常に非同期。 |
| `max_inflight`        | integer | `32`                    | リモートブローカーへの未アックメッセージ最大数。ディスクキューからのバッチポップサイズと emqtt 送信ウィンドウを制御。 |
| `remote_qos`          | string  | `"${qos}"`              | リモートブローカーへのパブリッシュ時の QoS レベル（`"0"`、`"1"`、`"2"`）。デフォルトの `"${qos}"` は元メッセージの QoS を維持。 |
| `remote_retain`       | string  | `"${retain}"`           | リモートブローカーへのパブリッシュ時のリテインフラグ（`"true"`、`"false"`）。デフォルトの `"${retain}"` は元メッセージのリテインを維持。 |
| `max_publish_retries` | integer | `-1`                    | メッセージごとのパブリッシュリトライ回数。`-1` は無限リトライ。失敗した PUBACK や接続切断で1回消費。 |

#### リモート (`remotes.<name>`)

| フィールド         | 型      | デフォルト       | 説明                                               |
|--------------------|---------|-----------------|----------------------------------------------------|
| `server`           | string  | —               | リモート MQTT ブローカーのアドレス（`host:port`）。 |
| `username`         | string  | `""`            | リモートブローカー認証用ユーザー名。               |
| `password`         | string  | `""`            | リモートブローカー認証用パスワード。               |
| `ssl.enable`       | boolean | `false`         | リモートブローカー接続に SSL/TLS を使用するか。    |
| `ssl.verify`       | string  | `verify_none`   | TLS 検証モード。サポート値：`verify_none`、`verify_peer`。 |
| `ssl.sni`          | string  | サーバーホスト名 | TLS Server Name Indication。デフォルトはサーバーホスト名。`"disable"` で無効化可能。 |
| `ssl.cacertfile`   | string  | —               | リモートブローカー証明書検証用 CA 証明書ファイル。  |
| `ssl.certfile`     | string  | —               | 相互 TLS 認証用クライアント証明書ファイル。         |
| `ssl.keyfile`      | string  | —               | 相互 TLS 認証用クライアント秘密鍵ファイル。         |

#### キュー

| フィールド             | 型     | デフォルト               | 説明                                                                                   |
|-----------------------|--------|-------------------------|----------------------------------------------------------------------------------------|
| `queue.base_dir`       | string | `"emqx_bridge_mqtt_dq"` | ディスクキューセグメントファイルのベースディレクトリ。ブリッジ名とパーティションインデックスが自動付加されます（例：`<base_dir>/<bridge_name>/<index>`）。相対パスは EMQX の `data_dir` に対して解決され、絶対パスはそのまま使用されます。 |
| `queue_seg_bytes`      | string | `"100MB"`               | キューセグメントファイルの最大サイズ。                                                  |
| `queue.max_total_bytes`| string | `"1GB"`                 | パーティションごとの最大ディスクキューサイズ。ブリッジは `buffer_pool_size` 個のパーティションを使用するため、最大総ディスク使用量は `buffer_pool_size` × この値となります。超過時は古いメッセージから破棄されます。 |

## トピックテンプレート

`remote_topic` フィールドは `${topic}` プレースホルダーをサポートし、転送時に元のパブリッシュトピックに置換されます。

例：
- `remote_topic = "${topic}"`：元のトピックをそのまま転送。
- `remote_topic = "forwarded/${topic}"`：プレフィックスを付加。
- `remote_topic = "region1/${topic}"`：リージョンのネームスペースを追加。

`remote_topic` はキューからメッセージを送信する際に適用されます。このフィールドを変更した場合、影響を受けるブリッジを再起動後、新しいテンプレートがキュー内メッセージに適用されます。

## REST API

プラグインは EMQX プラグイン API ベースパス配下に4つのエンドポイントを公開しています：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics`：Prometheus テキスト形式
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats`：JSON ダッシュボードスナップショット
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>`：特定ブリッジのみ
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status`：プラグイン／クラスターのヘルスサマリー

すべての JSON エンドポイントは `application/json; charset=utf-8` を返します。

JSON API はクラスタ集約されており、ノードが利用不可またはタイムアウトした場合もベストエフォートでデータを返しますが、レスポンスにクラスタの完全性メタデータが含まれます。

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
- `summary`：すべての設定済みブリッジの合計値
- `bridges`：設定済みブリッジごとのエントリ

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

ブリッジが存在しない場合は `404` を返します。

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

`/metrics` エンドポイントは以下のようなクラスタ集約された Prometheus テキスト形式のメトリクスを返します：

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

- `enqueue`：ローカルでブリッジのエンキュー経路に受け入れられたメッセージ数
- `dequeue`：ローカルキューから永続的に削除されたメッセージ数
- `publish`：リモートブローカーに正常にパブリッシュされたメッセージ数
- `drop`：キュー内で破棄されたメッセージ数
- `retried_by_reason`：リトライ理由別のリトライ試行回数
- `config_state`：設定上のブリッジ状態（`enabled` または `disabled`）
- `runtime_state`：実際のワーカー／ストレージ状態（`running`、`degraded`、`purged`）
- `status`：運用者向けブリッジヘルス状態（`ok`、`partial`、`disconnected`、`disabled`、`error`）

現在のリトライ理由：

- `reason_code`：リモートブローカーが非成功 MQTT 理由コードを返しリトライした
- `connect_failed`：接続またはパブリッシュ失敗によるリトライ
- `timeout`：タイムアウトによるリトライ分類
- `connection_lost`：関連クライアントプロセス終了に伴いインフライトメッセージをリトライ用に回収
- `other`：分類不能なリトライ理由のフォールバック

ブリッジが完全にドレインされた後は以下が成立：

- `enqueue = dequeue = publish + drop`

#### バッファメトリクス

- `buffered`：その永続キューパーティションに現在格納されているメッセージ数
- バッファ行の `status`：ワーカーが存在する場合は `running`、それ以外は `missing`

このゲージは `replayq:open/1` 直後に更新されるため、新規トラフィック到着前でも永続化済みメッセージが可視化されます。

#### コネクタメトリクス

- `backlog`：`emqtt` への送信待ちでコネクタのバックログキューに溜まっているメッセージ数
- `inflight`：すでに `emqtt` に渡されて完了待ちのメッセージ数
- コネクタ行の `status`：`connected`、`disconnected`、`partial`、`missing`、`unknown`

## 設定変更時の挙動

設定更新はブリッジ単位で適用されます：

- 変更されたブリッジは再起動。
- 削除されたブリッジは停止。
- 無効化されたブリッジは停止し、キューディレクトリを削除。
- 新規ブリッジは起動。
- 変更のないブリッジは継続稼働。

プラグイン全体は設定更新ごとに再起動されません。ただし、再起動されるブリッジは短時間のハンドオーバー期間があり、その間にマッチするメッセージが破棄される可能性があります。ブリッジに影響する変更はトラフィックが少ない時間帯に適用してください。

### 設定変更前の注意

1. 影響を受けるブリッジを特定。
2. トラフィックが少ない時間帯に適用。
3. ダッシュボードのステータスやログで再起動・再接続エラーを監視。
4. 重要なパイプラインは変更後にエンドツーエンドの配信検証を実施。

### `queue.base_dir` の変更

有効なブリッジで `queue.base_dir` を変更すると、新しいディレクトリでブリッジが再起動します。実際のキューパスは `<base_dir>/<bridge_name>/<index>` です。古いディレクトリは自動削除されず、オーファンデータとしてディスクに残ります。不要な場合は新しいパスでブリッジが稼働していることを確認後、手動で削除してください。

### `buffer_pool_size` の変更

`buffer_pool_size` はブリッジごとのディスクキューパーティション数を制御します。メッセージは `erlang:phash2(Topic, buffer_pool_size)` でパーティションに割り当てられます。変更すると以下の影響があります：

1. **プール縮小**（例：8 → 4）：新サイズ以上のインデックスのパーティションは消費されなくなり、古いファイルは `queue.base_dir` 配下に残るため手動でクリーンアップが必要。

2. **プール拡大**（例：4 → 8）：ハッシュ空間が変わるため、以前パーティション N に割り当てられていたトピックがパーティション M に移動する可能性があります。古いパーティション内のメッセージは順序を保って配信されますが、新旧パーティション間でトピック単位の順序が一時的に崩れます。

3. **ブリッジ単位のドロップウィンドウ**：`buffer_pool_size` の変更によりブリッジが再起動し、ハンドオーバー期間中にインフライトのマッチメッセージが破棄される可能性があります。

## メッセージ配信保証

このプラグインは通常動作時に **少なくとも1回（at-least-once）** の配信を保証し、持続的障害時は **ベストエフォート** 配信となります。以下のシナリオでメッセージが失われる可能性があります。

### ディスクキューのオーバーフロー

キューパーティションが `queue.max_total_bytes` を超えると、そのパーティションの最も古いメッセージが新規データのために静かに破棄されます。警告ログ（`mqtt_dq_buffer_overflow`）が定期的に出力されます（メッセージ単位ではありません）。

**対策**：`queue.max_total_bytes` を増やす、`buffer_pool_size` を増やして負荷分散、またはメッセージスループットを減らす。

### リモートブローカーによるパブリッシュ拒否

リモートブローカーが PUBACK（QoS 1）または PUBREC（QoS 2）で非成功 MQTT 理由コードを返した場合、コネクターは最大3回までリトライします。すべてのリトライが失敗するとメッセージは破棄され、警告ログ（`mqtt_dq_publish_dropped`）が出力されます。

主な拒否理由コード：

| コード | 意味（MQTT 5.0）               |
|--------|-------------------------------|
| 16     | サブスクライバーなし           |
| 128    | 未指定のエラー                 |
| 131    | 実装固有のエラー               |
| 135    | 認可されていない               |
| 144    | トピック名が無効             |
| 145    | パケット識別子が使用中       |
| 151    | クォータ超過                 |

注：理由コード 0（成功）および 16（サブスクライバーなし）は成功扱いでリトライしません。

**対策**：リモートブローカーの ACL やトピックポリシーを確認し、ログの理由コードを調査してください。

### 接続障害の繰り返し

リモートブローカーとの接続が切断されるたびに、未アックのメッセージはリトライ回数を1回消費します。3回連続の接続障害で成功配信がない場合、メッセージは破棄されます。

例：
1. ネットワーク障害中にメッセージがローカルキューに入る（リトライカウンタ=3）。
2. リモート再接続、メッセージ送信：ACK 前に切断（リトライカウンタ=2）。
3. 再接続、再送信：切断（リトライカウンタ=1）。
4. 再接続、再送信：拒否または切断（リトライカウンタ=0）。
5. メッセージ破棄、警告ログ出力。

**対策**：リモートブローカーが繰り返し接続不能になる原因を調査。短時間のネットワーク断は透過的に処理されますが、持続的な不安定さは問題です。

### エンキュー時のバックプレッシャー（QoS > 0 ローカルパブリッシュ）

QoS 1 または 2 のクライアントがブリッジにマッチするメッセージをパブリッシュすると、プラグインはバッファワーカーのメールボックスにメッセージを送信し、ディスク書き込み確認のため最大 `enqueue_timeout_ms`（デフォルト5000ms）までパブリッシュセッションをブロックします。

このタイムアウトが発生してもメッセージ自体は失われません。メッセージはすでにバッファワーカーの Erlang メールボックスにあり、最終的にディスクキューに書き込まれます。タイムアウトはローカルパブリッシュ経路のブロック時間制御のみです。

重要な点は、`message.publish` フックが MQTT セッションプロセス内で動作するため、フックがブロック中はそのクライアントからの他メッセージ処理が停止することです。バッファワーカーが遅い場合（例：ディスク I/O ストールやメールボックスのバックログ増大）、タイムアウトによりクライアントセッションの無限停止を防止します。

タイムアウト発生時の挙動：
1. セッションプロセスは待機をやめ通常処理を継続。
2. クライアントは通常通り PUBACK/PUBREC を受信し、エラーは発生しません。
3. 警告ログ（`mqtt_dq_enqueue_timeout`）を出力。
4. メッセージはバッファワーカーのメールボックスに残り、追いついた時点でディスクキューに書き込み。

リスクは間接的で、バッファワーカーが継続的に遅延するとメールボックスが無制限に増加しメモリ使用量が増えることです。これはブリッジが受信メッセージレートに追いつけていない兆候です。

**対策**：`buffer_pool_size` を増やして負荷分散、`queue.base_dir` に高速ストレージを使用、またはマッチするトピックのメッセージレートを減らす。

注：QoS 0 のローカルパブリッシュは非同期でエンキューされ、セッションにバックプレッシャーはかかりません。

### ブリッジ再起動時のウィンドウ

ブリッジが再起動（設定変更、プラグインリロード、有効化／無効化切替）されると、マッチするメッセージが一時的にキャプチャされない可能性があります。

**対策**：トラフィックが少ない時間帯に設定変更を適用してください。

### QoS 0 の TCP レベル配信

リモートブローカーへの QoS 0 パブリッシュは、メッセージがローカルの TCP 送信バッファに到達した時点で成功とみなされます。リモートブローカーが TCP スタック受理後にクラッシュした場合、メッセージはコネクターにエラーを返さずに失われる可能性があります。

これは MQTT QoS 0 の仕様であり、本プラグイン固有の問題ではありません。

## 運用上の注意

### 永続化

バッファされたメッセージは以下の状況で保持されます：

- EMQX ノードの再起動
- プラグインのリロードやアップグレード
- リモートブローカーへの一時的なネットワーク障害

### キュー制限

キュー使用量がパーティションごとの `queue.max_total_bytes` を超えると、古いメッセージが破棄されます。警告ログが出力されます。

### プールサイズ設定

各バッファワーカーは `BufferIndex rem pool_size` でちょうど1つのコネクターに割り当てられます。負荷分散のために：

- `buffer_pool_size` は `pool_size` 以上に設定してください。
- `buffer_pool_size` は `pool_size` の倍数であるべきです（`buffer_pool_size mod pool_size = 0`）。

良い例：`pool_size = 4, buffer_pool_size = 4`（1:1）、`pool_size = 4, buffer_pool_size = 8`（2:1）。

悪い例：`pool_size = 4, buffer_pool_size = 5` は、コネクター0が2つのバッファを担当し他は1つでスループットが不均一になります。

コネクターが切断されると、割り当てられたバッファワーカーは一時停止し、再接続後に自動的に再開します。

### 順序性

安定したブリッジ設定下ではトピック単位の順序は維持されます。`buffer_pool_size` を変更すると一時的に順序が乱れる可能性があります（前述参照）。

### パブリッシャーの ACK 挙動（QoS 1/2）

ブリッジにマッチするメッセージについて：

- クライアントへの `PUBACK`（QoS 1）や `PUBREC`（QoS 2）は、EMQX がディスクキューへのエンキュー確認（`enqueue_timeout_ms`）を待つ間、遅延する場合があります。
- エンキュー待機がタイムアウトしても、EMQX はクライアントのパブリッシュフローを完了します。ディスクキューエンキュータイムアウトによるパブリッシュエラーはクライアントに通知されません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリース用の tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|-----------------|---------------------|------------|
| 6.2.0           | 0.5.1               | [emqx_bridge_mqtt_dq-0.5.1.tar.gz](https://packages.emqx.io/emqx-plugins/6.2.0/emqx_bridge_mqtt_dq-0.5.1.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
