# MQTT ブリッジ（ディスクキュー付き）

このプラグインは、ローカルの MQTT メッセージを別の MQTT ブローカーへ転送する際に、ディスクバッファを利用してレジリエンスを向上させます。

## 特長

- ブリッジごとのディスクバッファリング。
- リモートブローカーが利用不可の場合の自動リトライ。
- `${topic}` を用いたトピック書き換え対応。
- 1つのプラグインで複数のブリッジを管理可能。
- 設定更新はブリッジ単位で適用（変更のないブリッジは継続稼働）。

## 動作概要

1. ローカルのパブリッシュが各ブリッジの `filter_topic` とマッチするか判定。
2. マッチしたメッセージをディスクキューのパーティションに追記。
3. キューに溜まったメッセージをリモートブローカーへパブリッシュ。
4. ネットワークや接続障害でパブリッシュに失敗した場合は自動リトライ。
5. キューパーティションのサイズが `queue.max_total_bytes` を超えた場合、最も古いレコードから削除。

## 設定

EMQX ダッシュボード（推奨）またはプラグイン設定ファイルから設定可能です。

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

`priv/config.hocon` はパッケージに含まれるデフォルトテンプレートです。  
`data/plugins/.../config.hocon` は設定変更後に EMQX が使用する永続化設定ファイルです。

### クイックスタート（ダッシュボード）

1. プラグインを有効化します。  
2. `remotes` に再利用可能なリモートを1つ追加します。  
3. `bridges` にブリッジを1つ追加します。  
4. `remote`、`filter_topic`、`remote_topic` を設定します。  
5. 保存してリモートへの配信を検証します。  
6. ベースライン検証後にキューやプールの設定を調整します。

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

設定ファイル内の文字列値は `${EMQXDQ_*}` 形式で OS の環境変数を参照できます。  
`EMQXDQ_` プレフィックス付きの変数のみ解決され、それ以外の `${...}`（例：`remote_topic` の `${topic}`）はそのまま残ります。  
値全体がプレースホルダーである必要があり、部分的な埋め込み（例：`"prefix-${EMQXDQ_VAR}-suffix"`）はサポートされません。

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

環境変数が設定されていない場合、プラグインはエラーをログに出力し、元の `${EMQXDQ_...}` 文字列をそのまま値として保持します。  
これにより接続失敗（例：`"${EMQXDQ_REMOTE_SERVER}"` への接続試行）が発生し、ログやステータス API で設定ミスが明示されます。

> **警告 — 動的設定更新とノードローカル環境変数**  
> 環境変数は設定をパースするノード上で解決されます。EMQX ダッシュボード、REST API、CLI でプラグイン設定を更新すると、設定テキストが永続化され、クラスタ内の全ノードで再パースされます。  
> もしノードごとに環境変数の値が異なる、または設定されていない場合、ノードごとに異なる実効設定となります。  
> そのため、**クラスタ全ノードで同一の環境変数が設定されている場合を除き、ダッシュボードや API、CLI での `${EMQXDQ_...}` 置換の使用は避けてください。**  
> ノードローカルなシークレットは、設定ファイルを直接編集してプラグインをリロードするか、Kubernetes の ConfigMap/Secret のように全ノードで一貫したシークレット注入機構を利用してください。

### 設定リファレンス

#### トップレベル

| フィールド | 型   | デフォルト | 説明                              |
|------------|------|------------|-----------------------------------|
| `bridges`  | map  | `{}`       | ブリッジ名をキーとしたブリッジ設定のマップ。 |
| `remotes`  | map  | `{}`       | 再利用可能なリモートブローカー定義のマップ。 |

#### ブリッジ（`bridges.<name>`）

| フィールド             | 型      | デフォルト               | 説明                                                                                 |
|-----------------------|---------|-------------------------|--------------------------------------------------------------------------------------|
| `enable`              | boolean | `true`                  | このブリッジを有効または無効にします。                                              |
| `remote`              | string  | —                       | `remotes` 内のリモートブローカー定義名。                                            |
| `proto_ver`           | string  | `"v4"`                  | MQTT プロトコルバージョン：`v3`、`v4`、`v5`。                                      |
| `clientid_prefix`     | string  | `"emqx-dq-<name>-"`     | 自動生成される MQTT クライアントIDのプレフィックス。各接続にユニークなインデックスが付加されます（例：`emqx-dq-mybridge-0`）。省略可。 |
| `keepalive_s`         | integer | `60`                    | MQTT のキープアライブ間隔（秒）。                                                    |
| `pool_size`           | integer | `4`                     | リモートブローカーへの MQTT 接続数。                                                |
| `buffer_pool_size`    | integer | `4`                     | ブリッジごとのディスクキューバッファワーカー数。後述の注意事項を参照してください。    |
| `filter_topic`        | string  | —                       | ローカルトピックのフィルタパターン。`+` と `#` のワイルドカードをサポート。          |
| `remote_topic`        | string  | —                       | 転送先トピックのテンプレート。元のトピックは `${topic}` で参照可能。                |
| `enqueue_timeout_ms`  | integer | `5000`                  | ディスクキュー書き込み確認待ちの最大ブロック時間（ms）。QoS > 0 の場合のみ適用。QoS 0 は常に非同期。 |
| `max_inflight`        | integer | `32`                    | リモートブローカーへの未アックメッセージ最大数。ディスクキューからのバッチポップサイズと emqtt の送信ウィンドウを制御。 |
| `remote_qos`          | string  | `"${qos}"`              | リモートブローカーへパブリッシュする際の QoS レベル（`"0"`、`"1"`、`"2"`）。デフォルトの `"${qos}"` は元メッセージの QoS を保持。 |
| `remote_retain`       | string  | `"${retain}"`           | リモートブローカーへパブリッシュする際のリテインフラグ（`"true"`、`"false"`）。デフォルトの `"${retain}"` は元メッセージのリテインフラグを保持。 |
| `max_publish_retries` | integer | `-1`                    | メッセージごとのパブリッシュリトライ最大回数。`-1` は無限リトライ。失敗した PUBACK や接続切断ごとに1回消費。 |

#### リモート（`remotes.<name>`）

| フィールド          | 型      | デフォルト       | 説明                                                     |
|--------------------|---------|-----------------|----------------------------------------------------------|
| `server`           | string  | —               | リモート MQTT ブローカーのアドレス（`host:port`）。      |
| `username`         | string  | `""`            | リモートブローカー認証用ユーザー名。                      |
| `password`         | string  | `""`            | リモートブローカー認証用パスワード。                      |
| `ssl.enable`       | boolean | `false`         | リモートブローカー接続に SSL/TLS を有効化。               |
| `ssl.verify`       | string  | `verify_none`   | TLS 検証モード。サポート値：`verify_none`、`verify_peer`。 |
| `ssl.sni`          | string  | サーバーホスト名 | TLS Server Name Indication。デフォルトはサーバーホスト名。`"disable"` で SNI 無効化。 |
| `ssl.cacertfile`   | string  | —               | リモートブローカー証明書検証用 CA 証明書ファイル。        |
| `ssl.certfile`     | string  | —               | 相互 TLS 認証用クライアント証明書ファイル。                |
| `ssl.keyfile`      | string  | —               | 相互 TLS 認証用クライアント秘密鍵ファイル。                |

#### キュー

| フィールド               | 型     | デフォルト                  | 説明                                                                                     |
|-------------------------|--------|----------------------------|------------------------------------------------------------------------------------------|
| `queue.base_dir`         | string | `"emqx_bridge_mqtt_dq"`    | ディスクキューのセグメントファイルのベースディレクトリ。ブリッジ名とパーティションインデックスが自動付加されます（例：`<base_dir>/<bridge_name>/<index>`）。相対パスは EMQX の `data_dir` 基準で解決。絶対パスはそのまま使用。 |
| `queue_seg_bytes`        | string | `"100MB"`                  | キューセグメントファイルの最大サイズ。                                                    |
| `queue.max_total_bytes`  | string | `"1GB"`                    | パーティションごとの最大ディスクキューサイズ。各ブリッジは `buffer_pool_size` 個のパーティションを使用するため、最大合計ディスク使用量は `buffer_pool_size` × この値となります。超過時は最古メッセージから破棄。 |

## トピックテンプレート

`remote_topic` フィールドは `${topic}` プレースホルダーをサポートし、転送時に元のパブリッシュトピックに置き換えられます。

例：  
- `remote_topic = "${topic}"` — 元のトピックをそのまま転送。  
- `remote_topic = "forwarded/${topic}"` — プレフィックスを付加。  
- `remote_topic = "region1/${topic}"` — リージョンのネームスペースを追加。

`remote_topic` はキューからメッセージ送出時に適用されます。このフィールドを変更した場合、影響を受けるブリッジを再起動後にキュー内のメッセージは新しいテンプレートを使用します。

## REST API

プラグインは EMQX プラグイン API ベースパス以下に4つのエンドポイントを公開しています：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics` — Prometheus テキスト形式  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats` — JSON ダッシュボードスナップショット  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>` — 特定ブリッジのみ  
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status` — プラグイン／クラスターのヘルスサマリー

すべての JSON エンドポイントは `application/json; charset=utf-8` を返します。

JSON API はクラスタ集約型です。ノードが利用不可またはタイムアウトした場合でもベストエフォートでデータを返しますが、レスポンスにクラスタの完全性メタデータが含まれます。

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

`/metrics` エンドポイントは Prometheus テキスト形式のクラスタ集約メトリクスを返します。例：

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

- `enqueue`: ブリッジのエンキュー経路で受け入れたローカルメッセージ数  
- `dequeue`: ローカルキューから耐久的に削除されたメッセージ数  
- `publish`: リモートブローカーへ正常にパブリッシュされたメッセージ数  
- `drop`: キュー内で破棄されたメッセージ数  
- `retried_by_reason`: リトライ理由別のリトライ試行回数  
- `config_state`: 設定からの望ましいブリッジ状態（`enabled` または `disabled`）  
- `runtime_state`: 実際のワーカー／ストレージ状態（`running`、`degraded`、`purged`）  
- `status`: オペレーター向けのブリッジヘルス（`ok`、`partial`、`disconnected`、`disabled`、`error`）

現在のリトライ理由例：

- `reason_code`: リモートブローカーが非成功 MQTT リーズンコードを返しリトライした  
- `connect_failed`: 接続またはパブリッシュ失敗でリトライした  
- `timeout`: タイムアウトによるリトライ分類  
- `connection_lost`: 関連クライアントプロセス終了でインフライトメッセージを救出しリトライ  
- `other`: 未分類のリトライ原因

ブリッジが完全にドレインした後は以下を満たします：

- `enqueue = dequeue = publish + drop`

#### バッファメトリクス

- `buffered`: その耐久キューパーティションに現在格納されているメッセージ数  
- バッファ行の `status`: ワーカーが存在する場合は `running`、それ以外は `missing`

このゲージは `replayq:open/1` の直後に更新されるため、新規トラフィック到着前でも永続化済みメッセージが見えます。

#### コネクタメトリクス

- `backlog`: コネクタのバックログキューに滞留し `emqtt` へ送出待ちのメッセージ数  
- `inflight`: 既に `emqtt` に渡され完了待ちのメッセージ数  
- コネクタ行の `status`: `connected`、`disconnected`、`partial`、`missing`、`unknown`

## 設定変更時の挙動

設定更新はブリッジ単位で適用されます：

- 変更されたブリッジは再起動。  
- 削除されたブリッジは停止。  
- 無効化されたブリッジは停止しキューディレクトリをパージ。  
- 新規ブリッジは起動。  
- 変更のないブリッジは継続稼働。

プラグイン全体は設定更新ごとに再起動されません。  
ただし、再起動したブリッジは短時間の引き継ぎウィンドウがあり、その間にマッチするメッセージが破棄される可能性があります。  
ブリッジに影響する変更はトラフィックが少ない時間帯に適用してください。

### 設定変更前の注意

1. 影響を受けるブリッジを特定。  
2. トラフィックが少ない時間帯に適用。  
3. ダッシュボードのステータスとログで再起動や再接続エラーを監視。  
4. 重要なパイプラインは変更後にエンドツーエンドの配信検証を実施。

### `queue.base_dir` の変更

有効なブリッジで `queue.base_dir` を変更すると、新しいディレクトリでブリッジが再起動します。  
実際のキューパスは `<base_dir>/<bridge_name>/<index>` です。  
古いディレクトリは自動で削除されず、オーファンデータとして残ります。  
不要になった場合は、ブリッジが新パスで動作していることを確認後に手動で削除してください。

### `buffer_pool_size` の変更

`buffer_pool_size` はブリッジごとのディスクキューパーティション数を制御します。  
メッセージは `erlang:phash2(Topic, buffer_pool_size)` でパーティションに割り当てられます。  
この値を変更すると以下の副作用があります：

1. **プール縮小**（例：8 → 4）  
   新サイズ以上のインデックスのパーティションは消費されなくなり、古いファイルは `queue.base_dir` 以下に残るため手動でクリーンアップが必要。  

2. **プール拡大**（例：4 → 8）  
   ハッシュ空間が変わるため、以前パーティション N に割り当てられていたトピックがパーティション M に割り当てられる可能性があります。  
   既存の古いパーティションのメッセージは順序を保って配信されますが、新しいメッセージは別パーティションに入るため、トピック単位の順序が一時的に乱れます。  

3. **ブリッジ単位のドロップウィンドウ**  
   `buffer_pool_size` の変更はブリッジ再起動を伴い、引き継ぎ中にマッチするメッセージが破棄される可能性があります。

## メッセージ配信保証

このプラグインは通常動作時に **少なくとも1回** 配信を保証し、持続的障害時は **ベストエフォート** 配信となります。以下のケースでメッセージが失われる可能性があります：

### ディスクキューのオーバーフロー

キューパーティションが `queue.max_total_bytes` を超えると、そのパーティション内の最古メッセージが静かに破棄され、新しいデータの領域を確保します。  
警告ログ（`mqtt_dq_buffer_overflow`）が定期的に出力されます（メッセージ単位ではありません）。

**対策**：`queue.max_total_bytes` を増やす、`buffer_pool_size` を増やして負荷分散、またはメッセージスループットを減らす。

### リモートブローカーがパブリッシュを拒否

リモートブローカーが PUBACK（QoS 1）や PUBREC（QoS 2）で非成功 MQTT リーズンコードを返すと、コネクターは最大3回リトライします。  
リトライが尽きるとメッセージは破棄され、警告ログ（`mqtt_dq_publish_dropped`）が出力されます。

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

注：コード 0（成功）と 16（サブスクライバーなし）は成功扱いでリトライしません。

**対策**：リモートブローカーの ACL やトピックポリシーを確認し、ログで具体的な理由コードを調査してください。

### 接続障害の繰り返し

リモートブローカーとの接続が切断されるたびに、未アックメッセージのリトライ回数が1回減ります。  
3回の接続障害が連続し成功配信がない場合、メッセージは破棄されます。

例：ネットワーク障害時のメッセージ処理  
1. ローカルキューに格納（リトライカウンター=3）  
2. リモート再接続、メッセージ送出 → 接続切断（リトライカウンター=2）  
3. 再接続、再送出 → 接続切断（リトライカウンター=1）  
4. 再接続、再送出 → 拒否または接続切断（リトライカウンター=0）  
5. メッセージ破棄、警告ログ出力

**対策**：リモートブローカーが繰り返し到達不能になる原因を調査してください。  
一時的なネットワーク断は透過的に処理されますが、持続的な不安定状態が問題です。

### エンキュー時のバックプレッシャー（QoS > 0 のローカルパブリッシュ）

QoS 1 または 2 のクライアントがブリッジにマッチするメッセージをパブリッシュすると、プラグインはバッファワーカーのメールボックスにメッセージを送信し、ディスク書き込み確認が完了するまで最大 `enqueue_timeout_ms`（デフォルト 5000 ms）までパブリッシュセッションをブロックします。

このタイムアウトが発生してもメッセージ自体は失われません。  
メッセージはすでにバッファワーカーの Erlang メールボックスに存在し、最終的にディスクキューに書き込まれます。  
タイムアウトはローカルパブリッシュ経路のブロック時間を制御するのみです。

重要な理由：`message.publish` フックは MQTT セッションプロセス内で動作し、このフックがブロック中はそのクライアントからの他メッセージ処理が停止します。  
バッファワーカーが遅い（ディスク I/O ストールやメールボックスのバックログ増加）場合、タイムアウトがないとクライアントセッションが無期限に停止する恐れがあります。

タイムアウト発生時の挙動：  
1. セッションプロセスは待機を解除し通常処理を継続。  
2. クライアントには通常通り PUBACK/PUBREC が返され、エラーは通知されません。  
3. 警告ログ（`mqtt_dq_enqueue_timeout`）が出力されます。  
4. メッセージはバッファワーカーのメールボックスに残り、ワーカーが追いついた時点でディスクキューに書き込み。

リスクは間接的で、バッファワーカーが継続的に遅延するとメールボックスが増大しメモリ使用量が増加します。  
これはブリッジが受信メッセージレートに追いつけていない兆候です。

**対策**：`buffer_pool_size` を増やして負荷分散、`queue.base_dir` に高速ストレージを使用、またはマッチするトピックのメッセージレートを下げてください。

注：QoS 0 のローカルパブリッシュは非同期でエンキューされるため、パブリッシュセッションにバックプレッシャーはかかりません。

### ブリッジ再起動時のウィンドウ

設定変更、プラグインリロード、有効化／無効化切り替えによるブリッジ再起動時に、マッチするメッセージが一時的に捕捉されない可能性があります。

**対策**：トラフィックが少ない時間帯に設定変更を適用してください。

### QoS 0 の TCP レベル配信

QoS 0 でリモートブローカーへパブリッシュする場合、コネクターはメッセージがローカルの TCP 送信バッファに到達した時点で配信成功とみなします。  
もしリモートブローカーが TCP スタックがデータを受け入れた後にクラッシュし、ブローカー処理前であれば、メッセージはエラーなしで失われる可能性があります。

これは MQTT QoS 0 の仕様上の特性であり、本プラグイン固有の問題ではありません。

## 運用上の注意

### 永続化

バッファされたメッセージは以下の状況でも保持されます：

- EMQX ノードの再起動  
- プラグインのリロードやアップグレード  
- リモートブローカーへの一時的なネットワーク断

### キュー制限

キュー使用量がパーティションごとの `queue.max_total_bytes` を超えると、最古メッセージから破棄されます。警告ログが出力されます。

### プールサイズ設定

各バッファワーカーは `BufferIndex rem pool_size` により1つのコネクターに割り当てられます。負荷分散のため：

- `buffer_pool_size` は `pool_size` 以上に設定してください。  
- `buffer_pool_size` は `pool_size` の倍数であるべきです（`buffer_pool_size mod pool_size = 0`）。

良い例：`pool_size = 4, buffer_pool_size = 4`（1:1）、`pool_size = 4, buffer_pool_size = 8`（2:1）。  
悪い例：`pool_size = 4, buffer_pool_size = 5` — コネクター0が2つのバッファを担当し他は1つでスループットが不均一。

コネクターが切断されると、割り当てられたバッファワーカーは一時停止し、再接続時に自動で再開します。

### 順序性

安定したブリッジ設定下ではトピック単位の順序性が保たれます。  
`buffer_pool_size` を変更すると前述の通り一時的に順序性が乱れる可能性があります。

### パブリッシャーのアック挙動（QoS 1/2）

ブリッジにマッチするメッセージについて：  
- クライアントへの `PUBACK`（QoS 1）や `PUBREC`（QoS 2）は、EMQX がディスクキューへのエンキュー確認（`enqueue_timeout_ms`）を待つ間、遅延することがあります。  
- エンキュー待ちがタイムアウトしても、EMQX はクライアントのパブリッシュ処理を完了させます。ディスクキューのエンキュータイムアウトによるパブリッシュエラーはクライアントに通知されません。

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
