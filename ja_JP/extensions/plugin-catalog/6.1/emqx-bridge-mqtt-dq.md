# MQTT ブリッジ（ディスクキュー付き）

このプラグインは、ローカルの MQTT メッセージを別の MQTT ブローカーに転送する際に、ディスクバッファを利用してレジリエンスを向上させます。

## 特長

- ブリッジごとのディスクバッファリング。
- リモートブローカーが利用できない場合の自動リトライ。
- `${topic}` を使ったトピック書き換え対応。
- 1つのプラグインで複数のブリッジを管理可能。
- 設定変更はブリッジ単位で適用（変更なしのブリッジは継続稼働）。

## 動作概要

1. ローカルのパブリッシュが各ブリッジの `filter_topic` にマッチするか判定。
2. マッチしたメッセージをディスクキューのパーティションに追記。
3. キュー内のメッセージをリモートブローカーにパブリッシュ。
4. ネットワークや接続障害でパブリッシュに失敗した場合は自動リトライ。
5. キューパーティションのサイズが `queue.max_total_bytes` を超えた場合、最も古いレコードが破棄される。

## 設定

EMQX ダッシュボード（推奨）またはプラグイン設定ファイルから設定します。

本番環境ではまず1つのブリッジでトラフィックを検証し、その後スケールアウトしてください。

### 設定ファイルの場所

関連する設定ファイルの場所は以下の通りです。

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
`data/plugins/.../config.hocon` は EMQX が設定変更後に使用する永続化設定ファイルです。

### クイックスタート（ダッシュボード）

1. プラグインを有効化します。
2. `remotes` に再利用可能なリモートブローカーを1つ追加します。
3. `bridges` にブリッジを1つ追加します。
4. `remote`、`filter_topic`、`remote_topic` を設定します。
5. 保存してリモート配信を検証します。
6. ベースライン検証後にキューやプール設定を調整します。

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
`EMQXDQ_` プレフィックスの付いた変数のみ解決され、それ以外の `${...}`（例：`remote_topic` の `${topic}`）はそのまま残ります。  
値全体がプレースホルダーである必要があり、部分的な埋め込み（例：`"prefix-${EMQXDQ_VAR}-suffix"`）はサポートされません。

**制限:** `${EMQXDQ_*}` 置換は文字列型フィールド（例：`server`、`username`、`password`）のみ対応し、ブール型（`enable`）、整数型（`pool_size`、`keepalive_s`）には使えません。

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

環境変数が設定されていない場合、プラグインはエラーをログに記録し、元の `${EMQXDQ_...}` 文字列をリテラル値として保持します。  
これにより接続失敗（例：`"${EMQXDQ_REMOTE_SERVER}"` に接続しようとする）が発生し、ログやステータス API で誤設定が明示されます。

> **警告 — 動的設定更新とノードローカル環境変数**
>
> 環境変数は設定をパースするノードで解決されます。EMQX ダッシュボード、REST API、CLI でプラグイン設定を更新すると、設定テキストが永続化されクラスター内の全ノードで再パースされます。  
> もしノードごとに参照環境変数の値が異なる（または存在しない）場合、ノードごとに異なる実効設定になります。  
>
> そのため、**クラスター内の全ノードで同一の環境変数が設定されていることが確実でない限り、ダッシュボード、API、CLI の設定更新で `${EMQXDQ_...}` を使うのは避けてください。**  
> ノードローカルなシークレットは設定ファイルを直接編集してプラグインをリロードするか、Kubernetes ConfigMaps/Secrets のような全ノードで一貫したシークレット注入機構を利用してください。

### 設定リファレンス

#### トップレベル

| フィールド | 型   | デフォルト | 説明                                   |
|------------|------|------------|--------------------------------------|
| `bridges`  | map  | `{}`       | ブリッジ名をキーとしたブリッジ設定のマップ。 |
| `remotes`  | map  | `{}`       | 再利用可能なリモートブローカー定義のマップ。   |

#### ブリッジ (`bridges.<name>`)

| フィールド            | 型       | デフォルト               | 説明                                                                                 |
|----------------------|----------|-------------------------|-------------------------------------------------------------------------------------|
| `enable`             | boolean  | `true`                  | このブリッジを有効または無効にする。                                               |
| `remote`             | string   | —                       | `remotes` 内のリモートブローカー定義名。                                           |
| `proto_ver`          | string   | `"v4"`                  | MQTT プロトコルバージョン：`v3`、`v4`、`v5`。                                     |
| `clientid_prefix`    | string   | `"emqx-dq-<name>-"`     | 自動生成される MQTT クライアントIDのプレフィックス。各接続にユニークなインデックスが付与される（例：`emqx-dq-mybridge-0`）。省略可。 |
| `keepalive_s`        | integer  | `60`                    | MQTT のキープアライブ間隔（秒）。                                                  |
| `pool_size`          | integer  | `4`                     | リモートブローカーへの MQTT 接続数。                                              |
| `buffer_pool_size`   | integer  | `4`                     | ブリッジごとのディスクキューバッファワーカー数。以下の注意点を参照。              |
| `filter_topic`       | string   | —                       | ローカルトピックのフィルターパターン。`+` と `#` ワイルドカード対応。              |
| `remote_topic`       | string   | —                       | 転送先トピックのテンプレート。元のトピックは `${topic}` で参照可能。              |
| `enqueue_timeout_ms` | integer  | `5000`                  | ディスクキュー書き込み確認待ちの最大ブロック時間（ms）。QoS > 0 のみ適用。QoS 0 は常に非同期。 |
| `max_inflight`       | integer  | `32`                    | リモートブローカーごとの未アックメッセージ最大数。ディスクキューからのバッチ取得数と emqtt 送信ウィンドウを制御。 |
| `remote_qos`         | string   | `"${qos}"`              | リモートブローカーへのパブリッシュ時の QoS レベル（`"0"`、`"1"`、`"2"`）。デフォルトの `"${qos}"` は元メッセージの QoS を保持。 |
| `remote_retain`      | string   | `"${retain}"`           | リモートブローカーへのパブリッシュ時のリテインフラグ（`"true"`、`"false"`）。デフォルトの `"${retain}"` は元メッセージのリテインフラグを保持。 |
| `max_publish_retries`| integer  | `-1`                    | メッセージごとのパブリッシュリトライ最大回数。`-1` は無限リトライ。失敗した PUBACK や接続切断ごとに1回消費。 |

#### リモート (`remotes.<name>`)

| フィールド          | 型       | デフォルト        | 説明                                         |
|--------------------|----------|------------------|----------------------------------------------|
| `server`           | string   | —                | リモート MQTT ブローカーのアドレス（`host:port`）。 |
| `username`         | string   | `""`             | リモートブローカー認証用ユーザー名。           |
| `password`         | string   | `""`             | リモートブローカー認証用パスワード。           |
| `ssl.enable`       | boolean  | `false`          | リモートブローカー接続の SSL/TLS を有効化。   |
| `ssl.verify`       | string   | `verify_none`    | TLS 検証モード。`verify_none`、`verify_peer` をサポート。 |
| `ssl.sni`          | string   | サーバーホスト名 | TLS Server Name Indication。サーバーホスト名がデフォルト。`"disable"` で SNI 無効化。 |
| `ssl.cacertfile`   | string   | —                | リモートブローカー証明書検証用 CA 証明書ファイル。 |
| `ssl.certfile`     | string   | —                | 相互 TLS 認証用クライアント証明書ファイル。     |
| `ssl.keyfile`      | string   | —                | 相互 TLS 認証用クライアント秘密鍵ファイル。     |

#### キュー

| フィールド               | 型     | デフォルト                      | 説明                                                                                   |
|-------------------------|--------|-------------------------------|----------------------------------------------------------------------------------------|
| `queue.base_dir`         | string | `"emqx_bridge_mqtt_dq"`        | ディスクキューのセグメントファイルのベースディレクトリ。ブリッジ名とパーティションインデックスが自動付加される（例：`<base_dir>/<bridge_name>/<index>`）。相対パスは EMQX の `data_dir` に対して解決され、絶対パスはそのまま使用。 |
| `queue_seg_bytes`        | string | `"100MB"`                      | キューセグメントファイルの最大サイズ。                                                 |
| `queue.max_total_bytes`  | string | `"1GB"`                       | パーティションごとの最大ディスクキューサイズ。各ブリッジは `buffer_pool_size` 個のパーティションを使用するため、最大ディスク使用量は `buffer_pool_size` × この値。超過時は最古メッセージから破棄。 |

## トピックテンプレート

`remote_topic` フィールドは `${topic}` プレースホルダーをサポートし、転送時に元のパブリッシュトピックに置換されます。

例：
- `remote_topic = "${topic}"` — 元のトピックをそのまま転送。
- `remote_topic = "forwarded/${topic}"` — プレフィックスを付加。
- `remote_topic = "region1/${topic}"` — リージョンのネームスペースを追加。

`remote_topic` はキューからメッセージ送信時に適用されます。このフィールドを変更した場合、該当ブリッジを再起動後にキュー内メッセージは新しいテンプレートを使用します。

## REST API

プラグインは EMQX プラグイン API ベースパス配下に4つのエンドポイントを公開しています：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics` — Prometheus テキスト形式
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats` — JSON ダッシュボードスナップショット
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>` — 特定ブリッジのみ
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status` — プラグイン／クラスターのヘルスサマリー

すべての JSON エンドポイントは `application/json; charset=utf-8` を返します。

JSON API はクラスター集約済みです。ノードが利用不可またはタイムアウトした場合でもベストエフォートでデータを返しますが、レスポンスにはクラスターの完全性メタデータが含まれます。

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
- `uptime_seconds`: 応答ノード間で観測された最大プラグインアップタイム
- `summary`: 全ブリッジ合計値
- `bridges`: 各ブリッジごとのエントリ

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

`/metrics` エンドポイントはクラスター集約済みの Prometheus テキスト形式メトリクスを返します。例：

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

- `enqueue`: ローカルでブリッジのエンキュー経路に受け入れたメッセージ数
- `dequeue`: ローカルキューから耐久的に削除されたメッセージ数
- `publish`: リモートブローカーに正常にパブリッシュされたメッセージ数
- `drop`: キュー内で破棄されたメッセージ数
- `retried_by_reason`: リトライ理由別のリトライ回数
- `config_state`: 設定上のブリッジ状態（`enabled` または `disabled`）
- `runtime_state`: 実際のワーカー／ストレージ状態（`running`、`degraded`、`purged`）
- `status`: オペレーター向けブリッジのヘルス状態（`ok`、`partial`、`disconnected`、`disabled`、`error`）

現在のリトライ理由：

- `reason_code`: リモートブローカーが非成功 MQTT リーズンコードを返しリトライされた
- `connect_failed`: 接続またはパブリッシュ失敗によるリトライ
- `timeout`: タイムアウトによるリトライ分類
- `connection_lost`: 関連クライアントプロセス終了によりインフライトメッセージがリトライ用に回収された
- `other`: 未分類のリトライ理由のフォールバック

ブリッジが完全にドレインされた後は以下が成り立ちます：

- `enqueue = dequeue = publish + drop`

#### バッファメトリクス

- `buffered`: その耐久キューパーティションに現在格納されているメッセージ数
- バッファ行の `status`: ワーカーが存在する場合は `running`、そうでなければ `missing`

このゲージは `replayq:open/1` 後すぐに更新されるため、永続化済みのディスク上メッセージが新規トラフィック到着前でも可視化されます。

#### コネクタメトリクス

- `backlog`: コネクタのバックログキューに滞留し `emqtt` に送信待ちのメッセージ数
- `inflight`: 既に `emqtt` に渡され完了待ちのメッセージ数
- コネクタ行の `status`: `connected`、`disconnected`、`partial`、`missing`、`unknown`

## 設定変更時の挙動

設定変更はブリッジ単位で適用されます：

- 変更されたブリッジは再起動。
- 削除されたブリッジは停止。
- 無効化されたブリッジは停止しキューディレクトリをパージ。
- 新規ブリッジは起動。
- 変更なしのブリッジは継続稼働。

プラグイン全体は設定更新ごとに再起動されません。  
ただし再起動する各ブリッジは短時間のハンドオーバーウィンドウがあり、その間にマッチするメッセージが破棄される可能性があります。  
トラフィックが少ない時間帯にブリッジに影響する変更を適用してください。

### 設定変更前の注意

1. 影響を受けるブリッジを特定。
2. トラフィックが少ない時間帯に適用。
3. ダッシュボードのステータスとログで再起動や再接続エラーを監視。
4. 重要なパイプラインは変更後にエンドツーエンドの配信検証を実施。

### `queue.base_dir` の変更

有効なブリッジで `queue.base_dir` を変更すると新しいディレクトリでブリッジが再起動します。  
実際のキューパスは `<base_dir>/<bridge_name>/<index>` です。  
古いディレクトリは自動的に削除されず、オーファンデータとして残ります。  
不要になった古いディレクトリは、ブリッジが新パスで稼働していることを確認後に手動削除してください。

### `buffer_pool_size` の変更

`buffer_pool_size` はブリッジごとのディスクキューパーティション数を制御します。  
メッセージは `erlang:phash2(Topic, buffer_pool_size)` でパーティションに割り当てられます。  
この値を変更すると以下の副作用があります：

1. **プール縮小**（例：8 → 4）  
   新サイズ以上のインデックスのパーティションは消費されなくなります。  
   古いファイルは `queue.base_dir` 配下に残り手動でクリーンアップが必要です。

2. **プール拡大**（例：4 → 8）  
   ハッシュ空間が変わるため、以前はパーティション N に割り当てられていたトピックがパーティション M に変わる可能性があります。  
   旧パーティションにキューイング済みのメッセージは順序を保って配信されますが、新規メッセージは別パーティションに行くため、トピック単位のエンドツーエンド順序が一時的に崩れます。  
   古いメッセージが新しいメッセージより後に配信される場合があります。

3. **ブリッジ単位のドロップウィンドウ**  
   `buffer_pool_size` の変更はブリッジ再起動を伴うため、ハンドオーバー中にインフライトのマッチメッセージが破棄される可能性があります。

## メッセージ配信保証

このプラグインは通常動作時に **少なくとも1回以上（at-least-once）** 配信を提供し、持続的障害時は **ベストエフォート** 配信となります。  
以下のシナリオでメッセージが失われる可能性があります。

### ディスクキューオーバーフロー

キューパーティションが `queue.max_total_bytes` を超えると、そのパーティションの最古メッセージが新規データのために静かに破棄されます。  
警告ログ（`mqtt_dq_buffer_overflow`）が定期的に出力されます（メッセージ単位ではありません）。

**対策**：`queue.max_total_bytes` を増やす、`buffer_pool_size` を増やしてパーティションを分散、またはメッセージスループットを減らす。

### リモートブローカーがパブリッシュを拒否

リモートブローカーが PUBACK（QoS 1）または PUBREC（QoS 2）で非成功 MQTT リーズンコードを返すと、コネクターは最大3回リトライします。  
リトライ上限に達するとメッセージは破棄され、警告ログ（`mqtt_dq_publish_dropped`）が出力されます。

主な拒否理由コード：

| コード | 意味（MQTT 5.0）               |
|--------|-------------------------------|
| 16     | マッチするサブスクライバーなし |
| 128    | 未指定エラー                   |
| 131    | 実装固有エラー                |
| 135    | 認可されていない              |
| 144    | トピック名が無効             |
| 145    | パケット識別子が使用中       |
| 151    | クォータ超過                 |

注：コード 0（成功）と 16（マッチするサブスクライバーなし）は成功扱いでリトライされません。

**対策**：リモートブローカーの ACL とトピックポリシーを確認し、ログの理由コードを調査してください。

### 接続障害の繰り返し

リモートブローカーへの接続が切断されるたびに、未アックのメッセージはリトライ回数を1回消費します。  
3回連続で接続障害が発生し成功配信がない場合、メッセージは破棄されます。

例：ネットワーク障害中にパブリッシュされたメッセージの場合  
1. ローカルにキューイング（リトライカウンター = 3）  
2. リモート再接続、メッセージ送信 — ACK 前に切断（リトライカウンター = 2）  
3. 再接続、再送信 — 切断（リトライカウンター = 1）  
4. 再接続、再送信 — 拒否または切断（リトライカウンター = 0）  
5. メッセージ破棄、警告ログ出力

**対策**：リモートブローカーが繰り返し接続不能になる原因を調査してください。  
一時的なネットワーク障害は透明に処理されますが、このケースは持続的な不安定性が原因です。

### エンキュー時のバックプレッシャー（QoS > 0 ローカルパブリッシュ）

QoS 1 または 2 のクライアントがブリッジにマッチするメッセージをパブリッシュすると、プラグインはバッファワーカーのメールボックスにメッセージを送信し、ディスク書き込み確認のため最大 `enqueue_timeout_ms`（デフォルト 5000 ms）までパブリッシュセッションプロセスをブロックします。

このタイムアウト発生時でもメッセージ自体は **失われません**。既にバッファワーカーの Erlang メールボックスに存在し、最終的にディスクキューに書き込まれます。  
タイムアウトはローカルパブリッシュパスのブロック時間を制御するだけです。

理由：`message.publish` フックは MQTT セッションプロセス内で実行されます。フックがブロック中はそのクライアントの他メッセージ処理が停止します。  
バッファワーカーが遅い（ディスク I/O ストールやメールボックス遅延）場合、タイムアウトがなければ遅いブリッジがクライアントセッションを無期限に停止させる恐れがあります。

タイムアウト発生時の挙動：
1. セッションプロセスは待機を解除し通常処理を継続。
2. クライアントには通常通り PUBACK/PUBREC が返され、エラーは通知されない。
3. 警告ログ（`mqtt_dq_enqueue_timeout`）が出力される。
4. メッセージはバッファワーカーのメールボックスに残り、ワーカーが追いついた時にディスクキューに書き込まれる。

リスクは間接的です。バッファワーカーが継続的に遅延するとメールボックスが無制限に増加しメモリ使用量が増大します。これはブリッジが受信メッセージレートに追いついていない兆候です。

**対策**：`buffer_pool_size` を増やして負荷分散、`queue.base_dir` に高速ストレージを使用、またはマッチするトピックのメッセージレートを減らしてください。

注：QoS 0 のローカルパブリッシュは常に非同期でエンキューされ、パブリッシュセッションにバックプレッシャーはかかりません。

### ブリッジ再起動時のウィンドウ

ブリッジが再起動（設定変更、プラグインリロード、有効／無効切替）すると、マッチするメッセージが一時的にキャプチャされない短時間のウィンドウがあります。

**対策**：トラフィックが少ない時間帯に設定変更を適用してください。

### QoS 0 の TCP レベル配信

QoS 0 のリモートブローカーへのパブリッシュは、メッセージがローカルの TCP 送信バッファに到達した時点で成功と見なされます。  
リモートブローカーが TCP スタック受理後にクラッシュし、ブローカー処理前に停止した場合、メッセージはコネクターにエラーが返らず失われる可能性があります。

これは MQTT QoS 0 の特性であり、本プラグイン固有の問題ではありません。

## 運用上の注意

### 永続化

バッファされたメッセージは以下の状況で生存します：

- EMQX ノードの再起動
- プラグインのリロードやアップグレード
- リモートブローカーへの一時的なネットワーク障害

### キュー制限

キュー使用量がパーティションごとの `queue.max_total_bytes` を超えると、最古メッセージが破棄され警告ログが出力されます。

### プールサイズ設定

各バッファワーカーは `BufferIndex rem pool_size` によってちょうど1つのコネクターに割り当てられます。均等な負荷分散のために：

- `buffer_pool_size` は `pool_size` 以上に設定してください。
- `buffer_pool_size` は `pool_size` の倍数であるべきです（`buffer_pool_size mod pool_size = 0`）。

良い例：`pool_size = 4, buffer_pool_size = 4`（1:1）、`pool_size = 4, buffer_pool_size = 8`（2:1）。  
悪い例：`pool_size = 4, buffer_pool_size = 5` — コネクター 0 は2つのバッファを担当し他は1つでスループットが不均等。

コネクターが切断されると、割り当てられたバッファワーカーは一時停止し、再接続時に自動再開します。

### 順序性

安定したブリッジ設定下ではトピック単位の順序性は保持されます。  
`buffer_pool_size` を変更すると前述の通り一時的に順序が乱れる可能性があります。

### パブリッシャーのアック挙動（QoS 1/2）

ブリッジにマッチするメッセージについて：

- パブリッシュクライアントへの `PUBACK`（QoS 1）および `PUBREC`（QoS 2）は、EMQX がディスクキューのエンキュー確認（`enqueue_timeout_ms`）を待つ間、遅延することがあります。
- その待機がタイムアウトしても、EMQX はクライアントのパブリッシュ処理を完了します。  
  クライアントはディスクキューのエンキュータイムアウトによるエラーを受け取りません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリースの tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.2 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.1.3 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.1.4 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_bridge_mqtt_dq-0.5.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
