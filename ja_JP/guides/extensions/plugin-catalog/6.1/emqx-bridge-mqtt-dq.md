# MQTT ブリッジ（ディスクキュー付き）

このプラグインは、ローカルの MQTT メッセージを別の MQTT ブローカーへ転送する際に、ディスクバッファを利用してレジリエンスを向上させるために使用します。

## 特長

- ブリッジごとのディスクバッファリング。
- リモートブローカーが利用できない場合の自動リトライ。
- `${topic}` を使ったトピック書き換え対応。
- 1つのプラグインで複数のブリッジを管理可能。
- 設定の更新はブリッジ単位で適用（変更のないブリッジは継続稼働）。

## 動作概要

1. ローカルのパブリッシュが各ブリッジの `filter_topic` とマッチするか判定。
2. マッチしたメッセージをディスクキューのパーティションに追記。
3. キューに溜まったメッセージをリモートブローカーへパブリッシュ。
4. ネットワークや接続障害でパブリッシュに失敗した場合は自動リトライ。
5. キューパーティションのサイズが `queue.max_total_bytes` を超えると、古いレコードから削除。

## 設定方法

EMQX ダッシュボード（推奨）またはプラグイン設定ファイルで設定可能です。

本番環境では、まず1つのブリッジで動作を検証し、その後スケールアウトしてください。

### 設定ファイルの場所

関連する設定ファイルは以下の2種類です：

- インストール済みプラグインパッケージ内のデフォルトファイル：
  - docker インストール例（バージョン `0.2.0`）：
    `/opt/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`
  - deb/rpm インストール例（バージョン `0.2.0`）：
    `/usr/lib/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`

- ダッシュボードや API から設定保存後に EMQX が管理する永続化プラグイン設定ファイル：
  - docker：
    `/opt/emqx/data/plugins/emqx_bridge_mqtt_dq/config.hocon`
  - deb/rpm：
    `/var/lib/emqx/plugins/emqx_bridge_mqtt_dq/config.hocon`

`priv/config.hocon` はパッケージに含まれるデフォルトテンプレートで、`data/plugins/.../config.hocon` は設定変更後に使用される永続化ファイルです。

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

設定ファイル内の任意の文字列は `${EMQXDQ_*}` 形式で OS 環境変数を参照できます。`EMQXDQ_` プレフィックスのついた変数のみ解決され、他の `${...}` パターン（例：`remote_topic` の `${topic}`）はそのまま残ります。値全体がプレースホルダーでなければなりません（部分的な埋め込みは不可）。

**制限**：`${EMQXDQ_*}` の置換は文字列型のフィールド（例：`server`、`username`、`password`）のみ対応し、ブール型（`enable`）、整数型（`pool_size`、`keepalive_s`）には使えません。

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

環境変数が設定されていない場合、プラグインはエラーをログに出力し、元の `${EMQXDQ_...}` 文字列をそのまま値として保持します。これにより接続失敗（例：`"${EMQXDQ_REMOTE_SERVER}"` に接続しようとする）が発生し、ログやステータス API で誤設定が明示されます。

> **警告 — 動的設定更新とノードローカル環境変数**
>
> 環境変数は設定をパースするノードで解決されます。EMQX ダッシュボード、REST API、CLI からプラグイン設定を更新すると、設定テキストが永続化され、クラスター内の全ノードで再パースされます。異なるノードで環境変数の値が異なる（または未設定）場合、ノードごとに異なる有効設定となります。
>
> そのため、**ダッシュボード、API、CLI からの設定更新で `${EMQXDQ_...}` を使うのは避けてください**。クラスター内の全ノードで同じ環境変数が設定されている場合のみ利用可能です。ノードローカルなシークレットは設定ファイルを直接編集しプラグインをリロードするか、Kubernetes ConfigMaps/Secrets など一貫したシークレット注入機構を使うことを推奨します。

### 設定リファレンス

#### トップレベル

| フィールド | 型   | デフォルト | 説明                                   |
|------------|------|------------|----------------------------------------|
| `bridges`  | map  | `{}`       | ブリッジ名をキーとしたブリッジ設定のマップ。 |
| `remotes`  | map  | `{}`       | 再利用可能なリモートブローカー定義のマップ。 |

#### ブリッジ（`bridges.<name>`）

| フィールド           | 型      | デフォルト               | 説明                                                                                   |
|---------------------|---------|-------------------------|----------------------------------------------------------------------------------------|
| `enable`            | boolean | `true`                  | このブリッジを有効化または無効化します。                                              |
| `remote`            | string  | —                       | `remotes` 内のリモートブローカー定義名。                                              |
| `proto_ver`         | string  | `"v4"`                  | MQTT プロトコルバージョン。`v3`、`v4`、`v5` のいずれか。                             |
| `clientid_prefix`   | string  | `"emqx-dq-<name>-"`     | 自動生成される MQTT クライアントIDのプレフィックス。各接続に一意のインデックスが付加されます（例：`emqx-dq-mybridge-0`）。省略可能。 |
| `keepalive_s`       | integer | `60`                    | MQTT キープアライブ間隔（秒）。                                                        |
| `pool_size`         | integer | `4`                     | リモートブローカーへの MQTT 接続数。                                                  |
| `buffer_pool_size`  | integer | `4`                     | ブリッジごとのディスクキューバッファワーカー数。以下の注意を参照。                     |
| `filter_topic`      | string  | —                       | ローカルトピックフィルター。`+` と `#` ワイルドカードをサポート。                      |
| `remote_topic`      | string  | —                       | 転送先トピックのテンプレート。元のトピックは `${topic}` で参照可能。                   |
| `enqueue_timeout_ms`| integer | `5000`                  | ディスクキュー書き込み確認待ちの最大ブロック時間（ms）。QoS > 0 の場合のみ適用。QoS 0 は常に非同期。 |
| `max_inflight`      | integer | `32`                    | リモートブローカーごとの未アックメッセージ最大数。ディスクキューからのバッチポップサイズと emqtt 送信ウィンドウを制御。 |
| `remote_qos`        | string  | `"${qos}"`              | リモートブローカーへのパブリッシュ時の QoS レベル（`"0"`、`"1"`、`"2"`）。デフォルトは元のメッセージの QoS を保持。 |
| `remote_retain`     | string  | `"${retain}"`           | リモートブローカーへのパブリッシュ時のリテインフラグ（`"true"`、`"false"`）。デフォルトは元のメッセージのリテインフラグを保持。 |
| `max_publish_retries` | integer | `-1`                   | メッセージごとの最大パブリッシュリトライ回数。`-1` は無限リトライ。失敗した PUBACK や接続切断ごとに1回消費。 |

#### リモート（`remotes.<name>`）

| フィールド         | 型      | デフォルト       | 説明                                               |
|--------------------|---------|-----------------|----------------------------------------------------|
| `server`           | string  | —               | リモート MQTT ブローカーのアドレス（`host:port`）。 |
| `username`         | string  | `""`            | リモートブローカー認証用ユーザー名。               |
| `password`         | string  | `""`            | リモートブローカー認証用パスワード。               |
| `ssl.enable`       | boolean | `false`         | リモートブローカー接続に SSL/TLS を有効化。        |
| `ssl.verify`       | string  | `verify_none`   | TLS 検証モード。`verify_none`、`verify_peer` をサポート。 |
| `ssl.sni`          | string  | サーバーホスト名 | TLS Server Name Indication。デフォルトはサーバーホスト名。`"disable"` で無効化可能。 |
| `ssl.cacertfile`   | string  | —               | リモートブローカー証明書検証用 CA 証明書ファイル。  |
| `ssl.certfile`     | string  | —               | 相互 TLS 認証用クライアント証明書ファイル。         |
| `ssl.keyfile`      | string  | —               | 相互 TLS 認証用クライアント秘密鍵ファイル。         |

#### キュー

| フィールド               | 型     | デフォルト                  | 説明                                                                                   |
|-------------------------|--------|----------------------------|----------------------------------------------------------------------------------------|
| `queue.base_dir`         | string | `"emqx_bridge_mqtt_dq"`    | ディスクキューのセグメントファイルのベースディレクトリ。ブリッジ名とパーティション番号が自動付加される（例：`<base_dir>/<bridge_name>/<index>`）。相対パスは EMQX の `data_dir` に対して解決。絶対パスはそのまま使用。 |
| `queue_seg_bytes`        | string | `"100MB"`                  | キューセグメントファイルの最大サイズ。                                                  |
| `queue.max_total_bytes`  | string | `"1GB"`                    | パーティションごとの最大ディスクキューサイズ。各ブリッジは `buffer_pool_size` 個のパーティションを使うため、最大総ディスク使用量は `buffer_pool_size` × この値。超過時は古いメッセージから破棄。 |

## トピックテンプレート

`remote_topic` フィールドは `${topic}` プレースホルダーをサポートし、転送時に元のパブリッシュトピックに置換されます。

例：
- `remote_topic = "${topic}"` — 元のトピックをそのまま転送。
- `remote_topic = "forwarded/${topic}"` — プレフィックスを付加。
- `remote_topic = "region1/${topic}"` — リージョンのネームスペースを追加。

`remote_topic` はキューからメッセージを送信する際に適用されます。このフィールドを変更した場合、影響を受けるブリッジの再起動後にキュー内メッセージは新しいテンプレートを使用します。

## REST API

プラグインは EMQX プラグイン API ベースパス以下に4つのエンドポイントを公開します：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics` — Prometheus テキスト形式
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats` — JSON ダッシュボードスナップショット
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>` — 特定ブリッジのみ
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status` — プラグイン／クラスターのヘルスサマリー

すべての JSON エンドポイントは `application/json; charset=utf-8` を返します。

JSON API はクラスター集約型です。ノードが利用不可またはタイムアウトした場合でも、ベストエフォートでデータを返しますが、レスポンスにクラスターの完全性メタデータが含まれます。

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

- `cluster`：クラスターの完全性および失敗ノード情報
- `uptime_seconds`：応答ノード間で観測された最大プラグイン稼働時間（秒）
- `summary`：全ブリッジの合計値
- `bridges`：設定された各ブリッジの情報

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

ブリッジが設定に存在しない場合は `404` が返されます。

`GET /status` はコンパクトなヘルスビューを返します：

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

- `enqueue`：ローカルでブリッジのエンキュー経路に受け入れられたメッセージ数
- `dequeue`：ローカルキューから耐久的に削除されたメッセージ数
- `publish`：リモートブローカーへ正常にパブリッシュされたメッセージ数
- `drop`：キュー内で破棄されたメッセージ数
- `retried_by_reason`：リトライ理由別の試行回数
- `config_state`：設定上のブリッジ状態（`enabled` または `disabled`）
- `runtime_state`：実際のワーカー／ストレージ状態（`running`、`degraded`、`purged`）
- `status`：オペレーター向けのブリッジヘルス状態（`ok`、`partial`、`disconnected`、`disabled`、`error`）

現在のリトライ理由：

- `reason_code`：リモートブローカーが非成功 MQTT リーズンコードを返しリトライした
- `connect_failed`：接続またはパブリッシュ失敗によるリトライ
- `timeout`：タイムアウトによるリトライ分類
- `connection_lost`：関連クライアントプロセス終了によりインフライトメッセージを再試行
- `other`：分類不能なリトライ理由のフォールバック

ブリッジが完全にドレインされた後は以下が成立：

- `enqueue = dequeue = publish + drop`

#### バッファメトリクス

- `buffered`：当該耐久キューパーティションに格納されているメッセージ数
- バッファ行の `status`：ワーカーが存在すれば `running`、存在しなければ `missing`

このゲージは `replayq:open/1` の直後に更新されるため、永続化されたディスク上のメッセージは新規トラフィック到着前でも可視化されます。

#### コネクタメトリクス

- `backlog`：コネクタのバックログキューに滞留し、`emqtt` への送信待ちメッセージ数
- `inflight`：すでに `emqtt` に渡され、完了待ちのメッセージ数
- コネクタ行の `status`：`connected`、`disconnected`、`partial`、`missing`、`unknown` のいずれか

## 設定変更時の挙動

設定更新はブリッジ単位で適用されます：

- 変更されたブリッジは再起動。
- 削除されたブリッジは停止。
- 無効化されたブリッジは停止し、キューディレクトリをパージ。
- 新規ブリッジは起動。
- 変更のないブリッジは継続稼働。

プラグイン全体は設定更新ごとに再起動されません。ただし、再起動したブリッジは短時間の引き継ぎウィンドウがあり、その間にマッチするメッセージが失われる可能性があります。トラフィックの少ない時間帯に変更を適用してください。

### 設定変更前の注意

1. 影響を受けるブリッジを特定。
2. トラフィックの少ない時間帯に適用。
3. ダッシュボードのステータスやログで再起動・再接続エラーを監視。
4. 重要なパイプラインは変更後にエンドツーエンドの配信検証を実施。

### `queue.base_dir` の変更

有効なブリッジで `queue.base_dir` を変更すると、新しいディレクトリでブリッジが再起動します。実際のキューパスは `<base_dir>/<bridge_name>/<index>` です。古いディレクトリは自動で削除されず、オーファンデータとして残ります。不要な場合は新パスでブリッジが稼働していることを確認後、手動で削除してください。

### `buffer_pool_size` の変更

`buffer_pool_size` はブリッジごとのディスクキューパーティション数を制御します。メッセージは `erlang:phash2(Topic, buffer_pool_size)` でパーティションに割り当てられます。変更には以下の副作用があります：

1. **プール縮小**（例：8 → 4）：新サイズ以上のインデックスのパーティションは消費されなくなります。古いファイルは `queue.base_dir` 以下に残り、手動でのクリーンアップが必要です。

2. **プール拡大**（例：4 → 8）：ハッシュ空間が変わるため、以前はパーティション N に割り当てられていたトピックがパーティション M に移動する可能性があります。古いパーティションにあるメッセージは順序を保って配信されますが、新旧で同じトピックのメッセージが異なるパーティションに分散するため、エンドツーエンドのトピック単位順序が一時的に崩れます。

3. **ブリッジ単位のドロップウィンドウ**：`buffer_pool_size` の変更によりブリッジが再起動するため、引き継ぎ時にマッチするメッセージが失われる可能性があります。

## メッセージ配信保証

このプラグインは通常時に **少なくとも1回** 配信を保証し、障害継続時は **ベストエフォート** 配信となります。以下のケースでメッセージが失われる可能性があります。

### ディスクキューのオーバーフロー

キューパーティションが `queue.max_total_bytes` を超えると、古いメッセージから順に破棄されます。破棄はサイレントに行われますが、警告ログ（`mqtt_dq_buffer_overflow`）が定期的に出力されます。

**対策**：`queue.max_total_bytes` の増加、`buffer_pool_size` の増加による負荷分散、またはメッセージスループットの削減。

### リモートブローカーによるパブリッシュ拒否

リモートブローカーが PUBACK（QoS 1）または PUBREC（QoS 2）で非成功の MQTT リーズンコードを返した場合、コネクターは最大3回リトライします。すべてのリトライが失敗するとメッセージは破棄され、警告ログ（`mqtt_dq_publish_dropped`）が出力されます。

主な拒否理由コード：

| コード | 意味（MQTT 5.0）                |
|--------|---------------------------------|
| 16     | マッチするサブスクライバーなし   |
| 128    | 未指定のエラー                  |
| 131    | 実装固有のエラー               |
| 135    | 認可されていない               |
| 144    | トピック名が無効              |
| 145    | パケット識別子が使用中        |
| 151    | クォータ超過                  |

注：理由コード 0（成功）および 16（マッチするサブスクライバーなし）は成功扱いでリトライされません。

**対策**：リモートブローカーの ACL やトピックポリシーを確認し、ログの理由コードを調査。

### 接続障害の繰り返し

リモートブローカーへの接続が切断されるたびに、未アックのメッセージはリトライ回数を1回消費します。3回の接続障害が成功配信なしに続くとメッセージは破棄されます。

例：
1. ネットワーク障害中にメッセージをキューイング（リトライカウンター=3）。
2. リモート再接続、メッセージ送信 → ACK 前に切断（カウンター=2）。
3. 再接続、再送 → 切断（カウンター=1）。
4. 再接続、再送 → 拒否または切断（カウンター=0）。
5. メッセージ破棄、警告ログ出力。

**対策**：リモートブローカーが繰り返し到達不能になる原因を調査。短期的なネットワーク断は透明に処理されますが、継続的な不安定状態は問題です。

### エンキュー時のバックプレッシャー（QoS > 0 のローカルパブリッシュ）

QoS 1 または 2 のクライアントがブリッジにマッチするメッセージをパブリッシュすると、プラグインはバッファワーカーのメールボックスにメッセージを送信し、ディスク書き込み確認まで最大 `enqueue_timeout_ms`（デフォルト 5000 ms）ブロックします。

このタイムアウトが発生してもメッセージ自体は失われません。すでにバッファワーカーの Erlang メールボックスに存在し、後でディスクキューに書き込まれます。タイムアウトはローカルパブリッシュ経路のブロック時間を制御します。

重要な理由：`message.publish` フックは MQTT セッションプロセス内で実行されます。フックがブロック中はそのクライアントの他メッセージ処理が停止します。バッファワーカーが遅い（ディスク I/O ストールやメールボックスのバックログ増大）場合、タイムアウトによりクライアントセッションが無期限に停止するのを防ぎます。

タイムアウト時の挙動：
1. セッションプロセスは待機を解除し通常処理継続。
2. クライアントには通常通り PUBACK/PUBREC が返され、エラーは発生しません。
3. 警告ログ（`mqtt_dq_enqueue_timeout`）が出力されます。
4. メッセージはバッファワーカーのメールボックスに残り、追いついた時点でディスクキューに書き込まれます。

リスクは間接的です。バッファワーカーが継続的に遅延するとメールボックスが増大し、メモリ使用量が増えます。これはブリッジが受信メッセージレートに追いつけていない兆候です。

**対策**：`buffer_pool_size` を増やして負荷分散、`queue.base_dir` に高速ストレージを使用、またはマッチするトピックのメッセージレートを下げる。

注：QoS 0 のローカルパブリッシュは常に非同期でエンキューされ、パブリッシュセッションにバックプレッシャーはかかりません。

### ブリッジ再起動時のウィンドウ

設定変更、プラグインリロード、有効化／無効化切り替え時にブリッジが再起動すると、マッチするメッセージが一時的に捕捉されない可能性があります。

**対策**：トラフィックの少ない時間帯に設定変更を適用してください。

### QoS 0 の TCP レベル配信

リモートブローカーへの QoS 0 パブリッシュは、メッセージがローカル TCP 送信バッファに到達した時点で配信成功とみなされます。リモートブローカーが TCP スタック受け入れ後にクラッシュすると、メッセージは失われる可能性がありますが、コネクターにはエラーが返りません。

これは MQTT QoS 0 の仕様であり、本プラグイン固有の問題ではありません。

## 運用上の注意

### 永続化

バッファされたメッセージは以下をまたいで保持されます：

- EMQX ノードの再起動
- プラグインのリロードやアップグレード
- リモートブローカーへの一時的なネットワーク障害

### キュー制限

キュー使用量がパーティションごとの `queue.max_total_bytes` を超えると、古いメッセージから破棄されます。警告ログが出力されます。

### プールサイズ設定

各バッファワーカーは `BufferIndex rem pool_size` により1つのコネクターに割り当てられます。負荷分散を均等にするには：

- `buffer_pool_size` は `pool_size` 以上に設定。
- `buffer_pool_size` は `pool_size` の倍数であること（`buffer_pool_size mod pool_size = 0`）。

良い例：`pool_size = 4, buffer_pool_size = 4`（1:1）、`pool_size = 4, buffer_pool_size = 8`（2:1）。

悪い例：`pool_size = 4, buffer_pool_size = 5` — コネクター0が2つのバッファを担当し、他は1つでスループットが不均一。

コネクターが切断すると、割り当てられたバッファワーカーは一時停止し、再接続後に自動再開します。

### 順序保証

安定したブリッジ設定下ではトピック単位の順序が保持されます。`buffer_pool_size` を変更すると、一時的に順序が乱れる可能性があります。

### パブリッシャーのアック挙動（QoS 1/2）

ブリッジにマッチするメッセージについて：

- パブリッシュクライアントへの `PUBACK`（QoS 1）や `PUBREC`（QoS 2）は、ディスクキューへのエンキュー確認待ち（`enqueue_timeout_ms`）で遅延する場合があります。
- エンキュー待ちがタイムアウトしても、EMQX はクライアントパブリッシュフローを完了します。クライアントにはディスクキューのエンキュータイムアウトによるエラーは通知されません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリース向けの tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.2 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.1.3 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.1.4 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.1.5 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.5/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.5/emqx_bridge_mqtt_dq-0.5.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
