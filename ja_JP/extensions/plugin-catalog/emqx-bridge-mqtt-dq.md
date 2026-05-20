# MQTT ブリッジ（ディスクキュー付き）

このプラグインを使用すると、ローカルの MQTT メッセージを別の MQTT ブローカーへ転送し、ディスクバッファを利用してレジリエンスを向上させることができます。

## 特徴

- ブリッジごとのディスクバッファリング。
- リモートブローカーが利用不可の場合の自動リトライ。
- `${topic}` を使ったトピック書き換え対応。
- 1つのプラグインで複数のブリッジを管理可能。
- 設定の更新はブリッジ単位で適用（変更のないブリッジは継続稼働）。

## 動作概要

1. ローカルのパブリッシュが各ブリッジの `filter_topic` とマッチするか判定。
2. マッチしたメッセージをディスクキューのパーティションに追記。
3. キューに溜まったメッセージをリモートブローカーへパブリッシュ。
4. ネットワークや接続障害でパブリッシュに失敗した場合は自動リトライ。
5. キューパーティションのサイズが `queue.max_total_bytes` を超えた場合、最も古いレコードを破棄。

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

`priv/config.hocon` はパッケージに含まれるデフォルトテンプレートです。  
`data/plugins/.../config.hocon` は設定変更後に EMQX が永続化して使用する設定ファイルです。

### クイックスタート（ダッシュボード）

1. プラグインを有効化します。  
2. `remotes` に再利用可能なリモートを1つ追加します。  
3. `bridges` にブリッジを1つ追加します。  
4. `remote`、`filter_topic`、`remote_topic` を設定します。  
5. 保存してリモートへの配信を検証します。  
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

設定ファイル内の任意の文字列値は `${EMQXDQ_*}` 形式で OS 環境変数を参照可能です。  
`EMQXDQ_` プレフィックスの付いた変数のみ解決され、それ以外の `${...}`（例：`remote_topic` の `${topic}`）はそのまま残ります。  
値全体がプレースホルダーである必要があり、部分的な埋め込み（例：`"prefix-${EMQXDQ_VAR}-suffix"`）はサポートされません。

**制限**：`${EMQXDQ_*}` は文字列型フィールド（例：`server`、`username`、`password`）でのみ機能し、  
ブール型（`enable`）、整数型（`pool_size`、`keepalive_s`）では使用できません。

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

環境変数が設定されていない場合、プラグインはエラーをログに出力し、元の `${EMQXDQ_...}` 文字列をそのまま値として保持します。  
通常は接続失敗（例：`"${EMQXDQ_REMOTE_SERVER}"` への接続試行）となり、ログやステータス API で誤設定が確認できます。

> **警告：動的設定更新とノードローカル環境変数について**  
> 環境変数は設定解析時にそのノードで解決されます。  
> EMQX ダッシュボード、REST API、CLI でプラグイン設定を更新すると、設定テキストは永続化され、クラスター内の全ノードで再解析されます。  
> ノードごとに環境変数の値が異なる、または設定されていない場合、ノードごとに異なる実効設定となります。  
> そのため、**クラスター全ノードで同一の環境変数が設定されている場合を除き、ダッシュボードや API、CLI で `${EMQXDQ_...}` を使うのは避けてください。**  
> ノードローカルなシークレットは設定ファイルを直接編集しプラグインをリロードするか、Kubernetes ConfigMaps/Secrets のように全ノードで同一にマウントされる仕組みを利用してください。

### 設定リファレンス

#### トップレベル

| フィールド | 型   | デフォルト | 説明                                   |
|------------|------|------------|----------------------------------------|
| `bridges`  | map  | `{}`       | ブリッジ名をキーとしたブリッジ設定のマップ。 |
| `remotes`  | map  | `{}`       | 再利用可能なリモートブローカー定義のマップ。 |

#### ブリッジ (`bridges.<name>`)

| フィールド             | 型       | デフォルト             | 説明                                                                                     |
|-----------------------|----------|-----------------------|------------------------------------------------------------------------------------------|
| `enable`              | boolean  | `true`                | このブリッジを有効化または無効化します。                                               |
| `remote`              | string   | —                     | `remotes` 内のリモートブローカー定義名。                                               |
| `proto_ver`           | string   | `"v4"`                | MQTT プロトコルバージョン。`v3`、`v4`、`v5` のいずれか。                              |
| `clientid_prefix`     | string   | `"emqx-dq-<name>-"`   | 自動生成される MQTT クライアントIDのプレフィックス。各接続にユニークなインデックスが付加されます（例：`emqx-dq-mybridge-0`）。空欄にするとデフォルトを使用。 |
| `keepalive_s`         | integer  | `60`                  | MQTT のキープアライブ間隔（秒）。                                                       |
| `pool_size`           | integer  | `4`                   | リモートブローカーへの MQTT 接続数。                                                   |
| `buffer_pool_size`    | integer  | `4`                   | ブリッジごとのディスクキューバッファワーカー数。以下の注意事項を参照してください。       |
| `filter_topic`        | string   | —                     | ローカルトピックのフィルターパターン。`+` と `#` ワイルドカード対応。                   |
| `remote_topic`        | string   | —                     | 転送先トピックのテンプレート。元のトピックは `${topic}` で参照可能。                   |
| `enqueue_timeout_ms`  | integer  | `5000`                | ディスクキュー書き込み確認の待機最大時間（ミリ秒）。QoS > 0 の場合に適用。QoS 0 は常に非同期。 |
| `max_inflight`        | integer  | `32`                  | リモートブローカーごとの未アックメッセージ最大数。ディスクキューからのバッチポップサイズと emqtt 送信ウィンドウを制御。 |
| `remote_qos`          | string   | `"${qos}"`            | リモートブローカーへのパブリッシュ時の QoS レベル（`"0"`、`"1"`、`"2"`）。デフォルトの `"${qos}"` は元メッセージの QoS を保持。 |
| `remote_retain`       | string   | `"${retain}"`         | リモートブローカーへのパブリッシュ時のリテインフラグ（`"true"`、`"false"`）。デフォルトの `"${retain}"` は元メッセージのリテインフラグを保持。 |
| `max_publish_retries` | integer  | `-1`                  | メッセージごとのパブリッシュリトライ最大回数。`-1` は無限リトライ。失敗した PUBACK や接続断は1回のリトライ消費。 |

#### リモート (`remotes.<name>`)

| フィールド           | 型       | デフォルト       | 説明                                                      |
|---------------------|----------|-----------------|-----------------------------------------------------------|
| `server`            | string   | —               | リモート MQTT ブローカーのアドレス（`host:port`）。       |
| `username`          | string   | `""`            | リモートブローカー認証用ユーザー名。                       |
| `password`          | string   | `""`            | リモートブローカー認証用パスワード。                       |
| `ssl.enable`        | boolean  | `false`         | リモートブローカー接続に SSL/TLS を有効化。                |
| `ssl.verify`        | string   | `verify_none`   | TLS 検証モード。サポート値：`verify_none`、`verify_peer`。  |
| `ssl.sni`           | string   | サーバーホスト名 | TLS Server Name Indication。デフォルトはサーバーホスト名。`"disable"` で SNI 無効化。 |
| `ssl.cacertfile`    | string   | —               | リモートブローカー証明書検証用 CA 証明書ファイル。          |
| `ssl.certfile`      | string   | —               | 相互 TLS 認証用クライアント証明書ファイル。                 |
| `ssl.keyfile`       | string   | —               | 相互 TLS 認証用クライアント秘密鍵ファイル。                 |

#### キュー

| フィールド              | 型      | デフォルト               | 説明                                                                                   |
|------------------------|---------|-------------------------|----------------------------------------------------------------------------------------|
| `queue.base_dir`        | string  | `"emqx_bridge_mqtt_dq"` | ディスクキューのセグメントファイルのベースディレクトリ。ブリッジ名とパーティションインデックスが自動付加されます（例：`<base_dir>/<bridge_name>/<index>`）。相対パスは EMQX の `data_dir` に対して解決され、絶対パスはそのまま使用。 |
| `queue_seg_bytes`       | string  | `"100MB"`               | キューセグメントファイルの最大サイズ。                                                |
| `queue.max_total_bytes` | string  | `"1GB"`                 | パーティションごとの最大ディスクキューサイズ。各ブリッジは `buffer_pool_size` 個のパーティションを使用するため、最大合計ディスク使用量は `buffer_pool_size` × この値。超過時は最古メッセージを破棄。 |

## トピックテンプレート

`remote_topic` フィールドは `${topic}` プレースホルダーをサポートし、転送時に元のパブリッシュトピックに置換されます。

例：  
- `remote_topic = "${topic}"`：元のトピックをそのまま転送。  
- `remote_topic = "forwarded/${topic}"`：プレフィックスを付加。  
- `remote_topic = "region1/${topic}"`：リージョンのネームスペースを追加。

`remote_topic` はキューからメッセージを送信する際に適用されます。  
このフィールドを変更した場合、該当ブリッジの再起動後にキュー内メッセージは新しいテンプレートを使用します。

## REST API

プラグインは EMQX プラグイン API ベースパス以下に4つのエンドポイントを公開しています：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics`：Prometheus テキスト形式  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats`：JSON ダッシュボードスナップショット  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>`：特定ブリッジのみ  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status`：プラグイン／クラスターのヘルスサマリー

すべての JSON エンドポイントは `application/json; charset=utf-8` を返します。

JSON API はクラスター集約済みです。ノードが利用不可またはタイムアウトした場合でも、ベストエフォートのデータを返しますが、レスポンスにクラスターの完全性メタデータが含まれます。

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

- `cluster`：クラスターの完全性と失敗ノード情報  
- `uptime_seconds`：応答ノード間で観測された最大プラグインアップタイム（秒）  
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

指定ブリッジが設定に存在しない場合、API は `404` を返します。

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

- `enqueue`：ローカルメッセージがブリッジのエンキュー経路に受け入れられた数  
- `dequeue`：ローカルキューから永続的に削除されたメッセージ数  
- `publish`：リモートブローカーに正常にパブリッシュされたメッセージ数  
- `drop`：キュー内で破棄されたメッセージ数  
- `retried_by_reason`：リトライ理由別の試行回数  
- `config_state`：設定上のブリッジ状態（`enabled` または `disabled`）  
- `runtime_state`：実際のワーカー／ストレージ状態（`running`、`degraded`、`purged`）  
- `status`：運用者向けのブリッジヘルス状態（`ok`、`partial`、`disconnected`、`disabled`、`error`）

現在のリトライ理由例：

- `reason_code`：リモートブローカーが MQTT の非成功リースンコードを返しリトライされた  
- `connect_failed`：接続またはパブリッシュ失敗によるリトライ  
- `timeout`：タイムアウトによるリトライ分類  
- `connection_lost`：関連クライアントプロセス終了によりインフライトメッセージをリトライ用に回収  
- `other`：分類不能なリトライ理由のフォールバック

ブリッジが完全にドレインした後は以下が成立します：

- `enqueue = dequeue = publish + drop`

#### バッファメトリクス

- `buffered`：その永続キューパーティションに現在格納されているメッセージ数  
- バッファ行の `status`：ワーカーが存在すれば `running`、存在しなければ `missing`

このゲージは `replayq:open/1` の直後に更新されるため、永続化済みのディスク上メッセージは新規トラフィック到着前から見えます。

#### コネクタメトリクス

- `backlog`：`emqtt` へ送信待ちのコネクタバックログキュー内メッセージ数  
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
ただし、再起動する各ブリッジには短い引き継ぎウィンドウがあり、その間にマッチするメッセージが破棄される可能性があります。  
ブリッジに影響する変更はトラフィックの少ないタイミングで適用してください。

### 設定変更前の注意

1. 影響を受けるブリッジを特定。  
2. トラフィックの少ない時間帯に適用。  
3. ダッシュボードのステータスやログで再起動・再接続エラーを監視。  
4. 重要なパイプラインは変更後にエンドツーエンドの配送検証を実施。

### `queue.base_dir` の変更

有効なブリッジで `queue.base_dir` を変更すると、新しいディレクトリでブリッジが再起動します。  
実際のキューパスは `<base_dir>/<bridge_name>/<index>` です。  
古いディレクトリは自動で削除されず、孤立データとしてディスクに残ります。  
不要な場合は新パスでの稼働を確認後に手動で削除してください。

### `buffer_pool_size` の変更

`buffer_pool_size` はブリッジごとのディスクキューパーティション数を制御します。  
メッセージは `erlang:phash2(Topic, buffer_pool_size)` でパーティションに割り当てられます。  
この値の変更は以下の副作用があります：

1. **プール縮小**（例：8 → 4）：新サイズ以上のインデックスのパーティションは消費されなくなります。古いファイルは `queue.base_dir` 下に残り手動でクリーンアップが必要。  
2. **プール拡大**（例：4 → 8）：ハッシュ空間が変わるため、以前はパーティション N に割り当てられていたトピックがパーティション M に変わる可能性があります。  
   既存の古いパーティション内のメッセージは順序を保って配信されますが、新しいメッセージは別パーティションに行くため、トピック単位の順序が一時的に崩れます。  
3. **ブリッジ単位のドロップウィンドウ**：`buffer_pool_size` の変更でブリッジが再起動するため、引き継ぎ中にインフライトのマッチメッセージが破棄される可能性があります。

## メッセージ配送保証

このプラグインは通常時に **at-least-once** 配送を提供し、持続的障害時には **ベストエフォート** 配送となります。  
以下のシナリオでメッセージが失われる可能性があります：

### ディスクキューのオーバーフロー

キューパーティションが `queue.max_total_bytes` を超えると、そのパーティション内の最古メッセージが静かに破棄され、新規データの領域を確保します。  
警告ログ（`mqtt_dq_buffer_overflow`）が定期的に出力されます（メッセージ単位ではありません）。

**対策**：`queue.max_total_bytes` を増やす、`buffer_pool_size` を増やして負荷分散、またはメッセージスループットを減らす。

### リモートブローカーによるパブリッシュ拒否

リモートブローカーが PUBACK（QoS 1）または PUBREC（QoS 2）で非成功の MQTT リースンコードを返すと、コネクタは最大3回リトライします。  
リトライ上限に達するとメッセージは破棄され、警告ログ（`mqtt_dq_publish_dropped`）が出力されます。

主な拒否理由コード：

| コード | 意味（MQTT 5.0）                  |
|--------|----------------------------------|
| 16     | マッチするサブスクライバーなし    |
| 128    | 未指定エラー                     |
| 131    | 実装固有エラー                   |
| 135    | 認可されていない                 |
| 144    | トピック名が無効                 |
| 145    | パケット識別子が使用中           |
| 151    | クォータ超過                     |

注：コード 0（成功）と 16（マッチするサブスクライバーなし）は成功扱いでリトライされません。

**対策**：リモートブローカーの ACL とトピックポリシーを確認し、ログで具体的なリースンコードを調査してください。

### 接続障害の繰り返し

リモートブローカーへの接続が切断されるたびに、未アックのメッセージはリトライ回数を1回消費します。  
3回の接続障害が成功配信なしで累積するとメッセージは破棄されます。

例：ネットワーク障害時のメッセージ処理  
1. ローカルキューに格納（リトライカウンター=3）  
2. リモート再接続、メッセージ送信：ACK前に切断（リトライカウンター=2）  
3. 再接続、再送信：切断（リトライカウンター=1）  
4. 再接続、再送信：拒否または切断（リトライカウンター=0）  
5. メッセージ破棄、警告ログ出力

**対策**：リモートブローカーが繰り返し接続不可になる原因を調査してください。  
一時的なネットワーク断は透明に処理されますが、持続的な不安定さは問題です。

### エンキュー時のバックプレッシャー（QoS > 0 ローカルパブリッシュ）

QoS 1 または 2 のクライアントがブリッジにマッチするメッセージをパブリッシュすると、プラグインはバッファワーカーのメールボックスにメッセージを送信し、ディスク書き込み確認まで最大 `enqueue_timeout_ms`（デフォルト 5000 ms）待機してパブリッシュセッションをブロックします。

このタイムアウト発生時もメッセージ自体は失われません。すでにバッファワーカーの Erlang メールボックスに存在し、最終的にディスクキューに書き込まれます。  
タイムアウトはローカルパブリッシュ経路のブロック時間を制御するだけです。

重要な理由：`message.publish` フックは MQTT セッションプロセス内で実行されます。  
フックがブロック中はそのクライアントの他メッセージ処理が停止します。  
バッファワーカーが遅い（ディスク I/O ストールやメールボックスのバックログ増大）場合、タイムアウトがあることでクライアントセッションの無限停止を防ぎます。

タイムアウト発生時の挙動：  
1. セッションプロセスは待機をやめ通常処理を継続。  
2. クライアントには通常通り PUBACK/PUBREC を返す（エラーは発生しない）。  
3. 警告ログ（`mqtt_dq_enqueue_timeout`）を出力。  
4. メッセージはバッファワーカーのメールボックスに残り、追いついた時点でディスクキューに書き込まれる。

リスクは間接的です。バッファワーカーが継続的に遅延するとメールボックスが無制限に増大し、メモリ使用量が増加します。  
これはブリッジが受信メッセージレートに追いつけていない兆候です。

**対策**：`buffer_pool_size` を増やして負荷分散、`queue.base_dir` に高速ストレージを使う、またはマッチするトピックのメッセージレートを減らしてください。

注：QoS 0 のローカルパブリッシュは非同期でエンキューされ、バックプレッシャーはかかりません。

### ブリッジ再起動時のウィンドウ

ブリッジが再起動（設定変更、プラグインリロード、有効/無効切替）される際、マッチするメッセージが捕捉されない短時間のウィンドウがあります。

**対策**：トラフィックの少ない時間帯に設定変更を適用してください。

### QoS 0 の TCP レベル配送

リモートブローカーへの QoS 0 パブリッシュは、メッセージがローカル TCP 送信バッファに到達した時点で配送成功とみなします。  
リモートブローカーが TCP スタックで受け入れた後にクラッシュし、ブローカーが処理する前に失われる可能性があります。  
これは MQTT QoS 0 の仕様であり、本プラグイン固有の問題ではありません。

## 運用上の注意

### 永続化

バッファされたメッセージは以下の状況でも保持されます：  
- EMQX ノードの再起動  
- プラグインのリロードやアップグレード  
- リモートブローカーへの一時的なネットワーク障害

### キュー制限

パーティションのキュー使用量が `queue.max_total_bytes` を超えた場合、最古メッセージが破棄され警告ログが出力されます。

### プールサイズの調整

各バッファワーカーは `BufferIndex rem pool_size` により1つのコネクターに割り当てられます。負荷を均等に分散するために：

- `buffer_pool_size` は `pool_size` 以上に設定してください。  
- `buffer_pool_size` は `pool_size` の倍数であるべきです（`buffer_pool_size mod pool_size = 0`）。

良い例：`pool_size = 4, buffer_pool_size = 4`（1:1）、`pool_size = 4, buffer_pool_size = 8`（2:1）。  
悪い例：`pool_size = 4, buffer_pool_size = 5` はコネクター0が2つのバッファを担当し他は1つのためスループットが不均一になります。

コネクターが切断されると、割り当てられたバッファワーカーは一時停止し、再接続後に自動再開します。

### 順序性

安定したブリッジ設定下ではトピック単位の順序性が保たれます。  
`buffer_pool_size` を変更すると、一時的に順序が乱れる可能性があります（前述の通り）。

### パブリッシャーのアック挙動（QoS 1/2）

ブリッジにマッチするメッセージについて：  
- クライアントへの `PUBACK`（QoS 1）や `PUBREC`（QoS 2）は、EMQX がディスクキューへのエンキュー確認（`enqueue_timeout_ms`）を待つ間、遅延する場合があります。  
- エンキュー待機がタイムアウトしても、EMQX はクライアントのパブリッシュ処理を完了させます。  
- クライアントはディスクキューエンキュータイムアウトによるエラーを受け取りません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースの tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.2.0 | 0.5.1 | [emqx_bridge_mqtt_dq-0.5.1.tar.gz](https://packages.emqx.io/emqx-plugins/6.2.0/emqx_bridge_mqtt_dq-0.5.1.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
