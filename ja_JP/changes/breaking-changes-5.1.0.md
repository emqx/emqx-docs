# EMQX 4.4 と EMQX 5.1 間の互換性のない変更点

EMQX 5.0 シリーズでは、旧バージョンの EMQX との互換性に影響を与える可能性のある複数の変更が導入されています。これらの破壊的変更は EMQX 5.1 のリリースノートにも記載されています。

本ドキュメントは、EMQX 4.x から EMQX 5.1 へのアップグレードを計画しているユーザー向けに、発生しうる問題点を理解していただくためのものです。

::: tip

1. 5.1 へのアップグレードを行う前に、4.4 の最新バージョンへのアップグレードを推奨します。  
2. 5.0 シリーズのより高いバージョンへアップグレードする場合は、本ドキュメントに従いまず 5.1 へアップグレードし、その後に更に高いバージョンへアップグレードしてください。

:::

## 概要

EMQX 4.4 と比較して、EMQX 5.1 へのアップグレードでは、特に様々な概念や仕組みにおいて大幅な変更が加えられており、EMQX 2.x から 3.x、3.x から 4.x へのアップグレード時の変更を上回る規模となっています。

まとめると、以下の点に注意が必要です：

1. **設定ファイルおよび HTTP API** に大幅な変更があります。既存の設定やこれらのインターフェースに依存したコードは移行が必要です。  
2. **MQTT プロトコルのコア機能**（Pub/Sub、Retainer、Shared Subscription など）はクライアントプログラムとの互換性を維持していますが、管理インターフェースに若干の変更があります。  
3. 認証、認可、データ統合、プロトコルアクセスに関するその他の機能は、それぞれの機能に応じた移行が必要です。  
4. 一部の概念が変更されています。例えば、**プラグイン**の新バージョンが導入され、旧バージョンとは大きく異なります。旧バージョンの**モジュール**の概念は完全に廃止されました。  
5. クラスター探索のための `mcast` や、データ統合のリソースタイプとしての `EMQX Bridges` など、一部機能の**削除**に注意してください。

## HTTP API

以前はダッシュボードの **Applications** で API アクセス認証情報を管理していましたが、現在は **[API Key](../dashboard/system.md#api-key)** を使って認証情報を作成します。認証情報は API Key と Secret Key からなり、それぞれ HTTP Basic 認証のユーザー名とパスワードとして使用できます。Secret Key は作成時に一度だけ表示され、その後は取得できません。

- ポート 8081 は閉じられ、すべての API リクエストはポート 18083 を使用します。  
- HTTP API へのアクセスにユーザー名/パスワードは使えず、API Key の使用が**必須**です。  
- API の基本パスは `/api/v4` から `/api/v5` に変更されました。ポート 18083 とパス `/api/v5` を使って API を呼び出してください。  
- 時刻関連のフィールドは [RFC3339](https://datatracker.ietf.org/doc/html/rfc3339) 形式（タイムゾーン付き）を使用します。

### データフォーマットの変更

レスポンスが成功時には、ビジネスステータスコード `code` は返されなくなり、エラー発生時には対応する 4xx/5xx の HTTP ステータスコードとエラーメッセージが返されます。  
すべての可能なエラーコードは `GET /error_codes` で取得可能です。

::: details レスポンスフォーマットの比較例：

**成功時のレスポンス**

```shell
# 4.x
## HTTP StatusCode = 200
GET /api/v4/rules/my_rule
{ "code": 0, "data": { ... } }

# 5.1
## HTTP StatusCode = 200
GET /api/v5/rules/my_rule
{ ... }
```

**エラー時のレスポンス**

```bash
# 4.x
## HTTP StatusCode = 200
GET /api/v4/rules/my_rule
{ "code": 404, "message": "Not Found" }

# 5.1
## HTTP StatusCode = 404
GET /api/v5/rules/my_rule
{ "code": "NOT_FOUND", "message": "Rule Id Not Found" }
```

:::

### 主な API の変更点

API は大幅に変更され、一部は互換性を持たせています。以下はよく使われる API の変更比較表です。

::: tip 互換性の注意点

- 互換あり：旧 API パスとパラメータを使用可能、または旧 API を維持。  
- 部分的に互換あり：API パスは変わらないが、一部フィールドが変更。  
- 互換なし：API パスおよびフィールドが変更。

:::

::: details API 互換性表

| 4.x                              | 5.x                                         | 互換性               | 備考                         |
| -------------------------------- | ------------------------------------------- | -------------------- | ---------------------------- |
| **Publish/Subscribe**            |                                             |                      |                              |
| `POST /mqtt/publish`             | `POST /publish`                             | 互換あり             |                              |
| `POST /mqtt/publish_batch`       | `POST /publish/bulk`                        | 互換あり             |                              |
| `POST /mqtt/subscribe`           | `POST /clients/{clientid}/subscribe`        | 互換あり             |                              |
| `POST /mqtt/subscribe_batch`     | `POST /clients/{clientid}/subscribe/bulk`   | 互換あり             |                              |
| `POST /mqtt/unsubscribe`         | `POST /clients/{clientid}/unsubscribe`      | 互換あり             |                              |
| `POST /mqtt/unsubscribe_batch`   | `POST /clients/{clientid}/unsubscribe/bulk` | 互換あり             |                              |
| **Clients/Topics/Subscriptions** |                                             |                      |                              |
| `GET /clients`                   | `GET /clients`                              | 部分的に互換あり     |                              |
| `GET /routes{/topic}`            | `GET /topics{/topic}`                       | 互換なし             | `routes` が `topics` に名称変更 |
| `GET /subscriptions`             | `GET /subscriptions`                        | 部分的に互換あり     |                              |
| `GET /subscriptions/{clientid}`  | `GET /clients/{clientid}/subscriptions`     | 互換なし             |                              |
| **Node/Stats/Metrics**           |                                             |                      |                              |
| `GET /nodes`                     | `GET /nodes`                                | 部分的に互換あり     |                              |
| `GET /brokers`                   | -                                           | 互換なし             | `GET /nodes` に統合          |
| `GET /stats`                     | `GET /stats`                                | 部分的に互換あり     |                              |
| `GET /metrics`                   | `GET /metrics`                              | 部分的に互換あり     |                              |
| **Users/Alarms**                 |                                             |                      |                              |
| `GET /users`                     | `GET /users`                                | 部分的に互換あり     |                              |
| `GET /alarms{/activated}`        | `GET /alarms?activated={true,false}`        | 互換なし             |                              |
| `GET /alarms{/deactivated}`      | `GET /alarms?activated={true,false}`        | 互換なし             |                              |

:::

## 設定ファイル

- フォーマット：

  - EMQX 4.x：`path.to.key = value` のフラット形式。  
  - EMQX 5.1：`path{to{ key = value }}` のネスト形式をサポート。  
- ソース：  
  - EMQX 4.x：  
    - `emqx.conf`、`listeners.conf`、`zones.conf` など複数ファイル。  
    - 動的更新は Mnesia に保存。動的更新を有効にするとファイルによる設定変更は不可。  
  - EMQX 5.1：  
    - 静的設定は `emqx.conf`。  
    - 動的更新は `cluster.hocon`。

## デフォルトリスナーの変更

| 名称             | 説明                               | v4.4 ポート | v5.x 対応ポート                    |
| ---------------- | ---------------------------------- | ----------- | ---------------------------------- |
| MQTT-TCP         | 内部（バックプレーン）MQTTリスナー | 11883       | -（削除）                        |
| Management-HTTP  | REST API                          | 8081        | 18083（ダッシュボードポートと統合） |

## プラグイン

従来の公式プラグインは EMQX に組み込み機能として移行されました。4.x 用に開発されたカスタムプラグインは、EMQX 5.x で使用する前に適応が必要です。

::: details 公式プラグインと組み込み機能の比較表

| 4.x              | 5.x                                                       |
| ---------------- | --------------------------------------------------------- |
| emqx_auth_http   | 認証・認可 - HTTP データソース                            |
| emqx_auth_jwt    | 認証・認可 - JWT                                         |
| emqx_auth_mnesia | 認証・認可 - 組み込みデータベース                         |
| emqx_auth_mongo  | 認証・認可 - MongoDB データソース                         |
| emqx_auth_mysql  | 認証・認可 - MySQL データソース                           |
| emqx_auth_pgsql  | 認証・認可 - PostgreSQL データソース                      |
| emqx_auth_redis  | 認証・認可 - Redis データソース                           |
| emqx_sasl        | 認証・認可 - MQTT 5 強化認証                              |
| emqx_auth_ldap   | -                                                         |
| emqx_rule_engine | データ統合                                                |
| emqx_bridge_mqtt | データブリッジ - MQTT ブリッジ                            |
| emqx_web_hook    | データブリッジ - HTTP サーバー                            |
| emqx_coap        | CoAP ゲートウェイ                                        |
| emqx_dashboard   | ダッシュボード                                           |
| emqx_exhook      | ExHook                                                    |
| emqx_exproto     | ExProto ゲートウェイ                                     |
| emqx_lwm2m       | LwM2M ゲートウェイ                                       |
| emqx_sn          | MQTT-SN ゲートウェイ                                     |
| emqx_stomp       | STOMP ゲートウェイ                                       |
| emqx_lua_hook    | -                                                         |
| emqx_management  | ダッシュボード                                           |
| emqx_prometheus  | Prometheus                                                |
| emqx_psk_file    | 認証 - PSK (`psk_authentication.enable = true`)          |
| emqx_recon       | 旧機能は CLI の `emqx ctl observer` で利用可能          |
| emqx_retainer    | Retain                                                    |
| <!--             | emqx_telemetry                                            |

:::

## ディストリビューションとクラスター

- クラスター作成のための `mcast` 探索戦略は非推奨となり、削除予定です。  
- サービス探索の設定が変更され、`cluster.discovery` は **cluster.discovery_strategy** に変更されました。  
- 新機能：[cluster call](https://docs.emqx.com/en/enterprise/v5.0/configuration/configuration-manual.md#cluster-autodiscovery)。
- 内部 DB にオプションの [最終的整合性](../design/clustering.md#data-consistency) が追加されました。

## MQTT

- EMQX 5.0 では、最終的整合性のため、MQTT クライアントは EMQX クラスターを単一のブラックボックスとして認識できなくなりました。サブスクライブ確定後、他クライアントからのパブリッシュメッセージを受信するかは保証されません。  
- EMQX 5.0 では、キープアライブ（PING 受信）に対して、数バイトではなく完全な MQTT コントロールパケットが必要です。  
- EMQX 5.0 の TLS リスナーは `partial_chain` と `verify_peer_ext_key_usage` をサポートしません。  
- リトライ間隔は 5.0 で 30 秒ですが、4.4 では無効（0）です。4.4 のデフォルト設定ファイルではリトライ間隔は 30 秒となっています。

## MQTT over QUIC

MQTT over QUIC は 5.0 の新機能ですがデフォルトで無効です。OS によっては `libatomic` の動的リンクが必要となる場合があります。

## 認証／認可

完全な互換性レポートは [Authentication / Authorization v4.4 to v5.1 Compatibility](./auth-4.4-to-5.1-compatibility.md) を参照してください。

すべての認証／認可プロバイダーは、旧フォーマットの代わりにプレースホルダーを使用するようになりました。EMQX 5.x では `${clientid}` のようなプレースホルダーを使い、4.x の `%c` から変更されています。利用可能なプレースホルダーのセットも変わっています。

### **概念の変更**

Auth は **Authentication（認証）**、ACL は **Authorization（認可）** と呼ばれます。

### **データ移行**

4.x からの認証方式や対応データソースはほぼ維持されており、使い方に若干の変更があります。多くの Authenticator/Authorizer は 4.x のデータベースをそのまま 5.x で利用可能で、既存データの移行は不要です。

### 実行順序の固定化

複数の認証器や認可チェッカーが同時に有効な場合、起動順ではなく設定ファイルやダッシュボードで指定した固定の順序で実行されます。実行順序は設定可能です。

### 変数展開構文の変更

従来は Auth プラグインで `%u` のような構文を使い、SQL 文や Redis クエリ、HTTP リクエストにクライアント情報を動的に埋め込んでいました。  
現在は `${username}`、`${clientid}` のような `${}` 構文を使い、ルール SQL と統一されています。

対応プレースホルダーの詳細は以下を参照してください：

- [Authentication Placeholders](../access-control/authn/authn.md#authentication-placeholders)  
- [Authorization Placeholders](../access-control/authz/authz.md#placeholders-in-data-queries)  

::: details 使用例

```shell
# 4.x
# etc/emqx_auth_mysql.conf
auth.mysql.auth_query = select password from mqtt_user where username = '%u' limit 1

# 5.x
# emqx.conf
authentication = [
  {
    ...
    mechanism = "password_based"
    backend = "mysql"
    query = "SELECT password_hash, salt FROM mqtt_user where username = ${username} LIMIT 1"
  }
]
```

:::

### 認証の互換性のない点

- スーパーユーザーのクエリは廃止されました。ハッシュ化された資格情報と `is_superuser` フラグを返す単一のクエリが必要です。  
- HTTP 認証  
  - EMQX 4.x では HTTP ステータスコードのみを使用し、本文は破棄されていました（例：`200` は許可、`403` は拒否）。  
  - EMQX 5.x では HTTP 認証が再設計され、HTTP 本文を活用します。詳細は [HTTP サービス認証](../access-control/authn/http.md#http-request-and-response) を参照してください。  
- SCRAM 認証  
  - 4.4 で唯一利用可能だった SHA1 ハッシュモードは廃止され、SHA256/SHA512 ハッシュが使用されます。  
- 組み込みデータベース  
  - 資格情報を設定ファイルに直接記述できません。  
  - 資格情報テーブルはユーザー名かクライアントIDのいずれかのタイプのみ保持し、両方は保持しません。  
- Redis  
  - `HMGET` と `HGET` コマンドのみサポート。  
  - `query_timeout` は廃止。  
- PostgreSQL  
  - `query_timeout` は使用されません。  
  - `encoding` は使用されません。

### 認可

- ファイルベース  

  - ACL ルール `{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"\]}` は EMQX 5.1 で動作しません。詳細は issue [#10735](https://github.com/emqx/emqx/issues/10735) を参照してください。

- HTTP  

  - EMQX 4.x では HTTP ステータスコードのみを使用し、本文は破棄されていました（"ignore" ケースを除く）。例：`200` は許可、`403` は拒否。  
  - EMQX 5.0 では HTTP 認可が再設計され、HTTP 本文を活用します。詳細は [HTTP リクエストとレスポンス](../access-control/authz/http.md#http-request-and-response) を参照してください。

- MySQL、PostgreSQL  

  - ストレージスキーマが変更されました。  
  - EMQX 4.4 では、クエリは `[Allow, IpAddr, Username, ClientId, Access, Topic]` の順で列を取得する必要がありました（名前は任意）。  
  - EMQX 5.1 では、クエリは `permission, action, topic` の列を任意の順序で、かつ正確な名前で取得する必要があります。`IpAddr, Username, ClientId` などの「誰が」部分はクエリに含めることが推奨されます。

- MongoDB  

  - ストレージスキーマが変更されました。  
  - EMQX 4.4 では、結果のドキュメントは Redis や JWT と同様にアクションキーごとにトピックリストを含んでいました：

    ```
    {
      "publish": ["t1", "t2"],
      "subscribe": ["t3", "t4"],
      "pubsub": ["t5", "t6"]
    }
    ```

  - EMQX 5.1 では、ドキュメントは `permission, action, topics` フィールドを持つ個別のルールを含みます。`topics` はトピックの配列である必要があります。

## ルールエンジン

ルール SQL は EMQX 4.x の構文と完全互換ですが、ルール下のアクションは組み込みアクション（republish、console）とデータブリッジ（HTTP サーバー、MQTT ブリッジ）に分離されました。

## データ統合

EMQX 5.1 ではデータ統合に関して以下の概念的な改善が行われています：

- ルールと SQL テンプレートの完全互換を保証。  
- リソースやブリッジの設定項目名やフォーマットの多くが変更。  
- 旧来の **Rule** -> **Action** -> **Resources** の流れが **Rule** -> **Bridges** に変更。  
- **Modules/Message Publish** の機能は Bridges に移行。  
- [オフラインメッセージ保存](https://docs.emqx.com/en/enterprise/v4.4/rule/offline_msg_to_redis.html)、[サブスクリプション取得](https://docs.emqx.com/en/enterprise/v4.4/rule/get_subs_from_redis.html)、および EMQX ブリッジ機能は削除。  
- Tablestore、DolphinDB、Lindorm、SAP Event Mesh のデータブリッジは未対応。  
- MQTT ブリッジプラグイン（`emqx_bridge_mqtt`）は削除。代わりにデータ統合の組み込み MQTT データブリッジを使用。

完全な互換性レポートは [Data Integration Incompatibility Between EMQX 5.1 with EMQX 4.4](./data-integration-4.4-to-5.1-incompatibility.md) を参照してください。

## HTTP サーバー

WebHook プラグイン（`emqx_web_hook`）はネイティブ機能に変換され、「HTTP サーバー」ブリッジと呼ばれます。

## オフラインメッセージ

EMQX Enterprise 4.x で提供されていた [オフラインメッセージ](https://docs.emqx.com/en/enterprise/v4.4/rule/offline_msg_to_redis.html) は外部データベースに基づいています。EMQX は将来のバージョンで組み込みデータベースに基づくネイティブなオフラインメッセージ機能を提供予定であり、5.x では外部データベースのオフラインメッセージはサポートされません。

今後のネイティブオフラインメッセージ機能は性能向上と運用コスト削減を実現します。続報にご期待ください。

## 自動サブスクリプション（サーバーサイドサブスクリプション）

EMQX Enterprise 5.0.0 以降、外部データベースに基づく [自動サブスクリプション](https://docs.emqx.com/en/enterprise/v4.4/rule/get_subs_from_redis.html)（サーバーサイドサブスクリプション）は提供されていません。

## データ永続化

[MQTT メッセージ永続化](https://docs.emqx.com/en/enterprise/v4.4/backend/backend.html#mqtt-message-persistence) は EMQX 5.0 および 5.1 では未実装であり、将来のバージョンでの実装が予定されています。

## ゲートウェイ

EMQX 4.x では各種プロトコルを対応プラグインやモジュールで設定していましたが、EMQX 5.0 では新たにゲートウェイという概念を導入しました。

MQTT 以外のプロトコル（LwM2M、CoAP、STOMP、MQTT-SN）のクライアントは、ダッシュボードの **Connections** ページや `GET /clients` API には表示されず、**Management** -> **Gateways** または `GET /gateway/{name}/clients` API で確認できます。

- 設定および管理方法は **完全に互換性がありません**。EMQX 5.0 では新しい設定フォーマットと管理方法が採用されています。  
  - 新しい設定フォーマット。  
  - ゲートウェイおよびゲートウェイクライアント管理のための新 HTTP API 追加。  
  - 各ゲートウェイは独立した認証方式を持ちます。  
- JT/T 808、GB/T 32960、TCP、OCPP は EMQX 5.1 で未対応です。  
- STOMP、MQTT-SN、ExProto は 4.x と完全互換で、機能もさらに改善されています。  
- CoAP と LwM2M のゲートウェイは 5.1.0 で実装されていますが、設計・実装が未完成のため本番環境での使用は推奨されません。

完全な互換性レポートは [Gateway Incompatibility between e4.4 and e5.1](./gateway-4.4-to-5.1-incompatibility.md) を参照してください。

## ログファイルフォーマット

EMQX 5.1 のログファイルは、EMQX 4.4 と同様のフラットログ形式か、よりインデクサーに優しい構造化 JSON 形式のいずれかです。

また、多くのログフィールドは単語区切りにアンダースコアを使うようになり、検索しやすくなっています。例：

`2022-06-29T16:58:53.235042+02:00 [info] foo: bar, msg: msg_for_human_to_read_but_also_easy_to_index`

詳細は [Logs](../observability/log.md) を参照してください。

## Prometheus

プラグイン `emqx_statsd` は削除されました。旧プラグイン `emqx_prometheus` は 5.x でネイティブ機能に統合され、Prometheus のスクレイピングエンドポイントはデフォルトで有効化されており、認証なしでメトリクスを取得できます。

`curl` コマンドでメトリクスを確認できます：

```bash
curl -f "http://127.0.0.1:18083/api/v5/prometheus/stats"
```

プッシュゲートウェイを有効にする場合は [Integrate with Prometheus](../observability/prometheus.md) を参照してください。

::: details Prometheus メトリクスの変更点

| 4.4.x                                        | 5.x                                             | 説明       |
| -------------------------------------------- | ----------------------------------------------- | ---------- |
| emqx_client_auth_success_anonymous           | emqx_client_auth_anonymous                      | 名称変更   |
| emqx_client_check_acl                        | emqx_client_authorize counter                   | 名称変更   |
| -                                            | emqx_mria_last_intercepted_trans                | 新規       |
| -                                            | emqx_mria_replicants                            | 新規       |
| -                                            | emqx_mria_server_mql                            | 新規       |
| -                                            | emqx_mria_weight                                | 新規       |
| emqx_routes_count                            | emqx_topics_count                               | 名称変更   |
| emqx_routes_max                              | emqx_topics_max                                 | 名称変更   |
| emqx_session_takeovered                      | emqx_session_takenover                          | 名称変更   |
| erlang_vm_ets_tables                         | -                                               | 削除       |
| -                                            | erlang_vm_memory_dets_tables                    | 新規       |
| -                                            | erlang_vm_memory_ets_tables                     | 新規       |
| -                                            | erlang_vm_msacc_alloc_seconds_total             | 新規       |
| -                                            | erlang_vm_msacc_aux_seconds_total               | 新規       |
| -                                            | erlang_vm_msacc_bif_seconds_total               | 新規       |
| -                                            | erlang_vm_msacc_busy_wait_seconds_total         | 新規       |
| -                                            | erlang_vm_msacc_check_io_seconds_total          | 新規       |
| -                                            | erlang_vm_msacc_emulator_seconds_total          | 新規       |
| -                                            | erlang_vm_msacc_ets_seconds_total               | 新規       |
| -                                            | erlang_vm_msacc_gc_full_seconds_total           | 新規       |
| -                                            | erlang_vm_msacc_gc_seconds_total                | 新規       |
| -                                            | erlang_vm_msacc_nif_seconds_total               | 新規       |
| -                                            | erlang_vm_msacc_other_seconds_total             | 新規       |
| -                                            | erlang_vm_msacc_port_seconds_total              | 新規       |
| -                                            | erlang_vm_msacc_send_seconds_total              | 新規       |
| -                                            | erlang_vm_msacc_sleep_seconds_total             | 新規       |
| -                                            | erlang_vm_msacc_timers_seconds_total            | 新規       |
| -                                            | erlang_vm_statistics_dirty_cpu_run_queue_length | 新規       |
| -                                            | erlang_vm_statistics_dirty_io_run_queue_length  | 新規       |
| erlang_vm_statistics_run_queues_length_total | erlang_vm_statistics_run_queues_length          | 名称変更   |
| -                                            | erlang_vm_wordsize_bytes                        | 新規       |

:::
