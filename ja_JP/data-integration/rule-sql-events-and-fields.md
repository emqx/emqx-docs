# データソースとフィールド

EMQXのルールは、**MQTTメッセージ**、**MQTTイベント**、または**データブリッジ**など、さまざまなデータソースからのデータを処理できます。

[ルールエンジン構文](./rule-sql-syntax.md)のセクションで説明したように、`FROM`句でデータソースを指定し、対応するフィールドを`SELECT`句や`where`句で参照できます。本セクションでは、[MQTTメッセージ](#mqtt-message)、[MQTTイベント](#mqtt-events)、および[データブリッジ](#data-bridges)のフィールドについて紹介します。

## MQTTメッセージ

EMQXのルールを使ってメッセージのパブリッシュを処理する場合、`FROM`句でメッセージのトピックを指定する必要があります。

例えば、以下のステートメントでは、トピックパターン`t/#`にパブリッシュされたメッセージの`payload.msg`（`AS`句で`msg`に名前変更）、`clientid`、`username`、`payload`、`topic`、`qos`のフィールドを選択しています。

例：
```sql
SELECT
  payload.msg as msg,
  clientid,
  username,
  payload,
  topic,
  qos
FROM
  "t/#"
```

出力例：

```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "payload": "{\"msg\":\"hello\"}",
  "msg": "hello",
  "clientid": "c_emqx"
}
```

受信したMQTTメッセージから選択可能なフィールドは以下の通りです： <!--技術レビュー必要 @WIVWIV-->

| フィールド             | 説明                                                   |
| :-------------------- | :----------------------------------------------------- |
| `id`                  | MQTTメッセージID                                       |
| `clientid`            | パブリッシャーのクライアントID                         |
| `username`            | パブリッシャーのユーザー名                             |
| `payload`             | MQTTペイロード                                         |
| `peerhost`            | クライアントのIPアドレス                               |
| `topic`               | MQTTトピック                                           |
| `qos`                 | QoSレベル                                              |
| `flags`               | フラグ <!--追加説明は必要でしょうか？-->               |
| `headers`             | メッセージ処理に関連する内部データ                     |
| `pub_props`           | PUBLISHプロパティ（MQTT 5.0クライアントのみ）          |
| `timestamp`           | タイムスタンプ（単位：ミリ秒）                         |
| `publish_received_at` | PUBLISHメッセージがEMQXに到達した時間（単位：ミリ秒）  |
| `node`                | イベントが発生したノード                                |
| `client_attrs`        | [クライアント属性](../client-attributes/client-attributes.md) |

## MQTTイベント

EMQXのルールを使って、クライアントのオンライン・オフラインやサブスクリプションなどのイベント通知を取得するために、イベントトピックからデータを抽出できます。イベントトピックは`"$events/"`で始まり、例えば`"$events/client/connected"`などがあり、ルールの`FROM`句で指定可能です。

::: tip

デフォルトでは、クライアントはMQTTイベントメッセージを直接サブスクライブできません。このセクションではルールを使ってこれらのメッセージをサブスクライブする方法を説明します。MQTTイベントメッセージのデータは、[システムトピック](../observability/mqtt-system-topics.md)をサブスクライブすることでも取得可能です。

:::

以下の表はサポートされているイベントトピックの一覧です。

### イベントトピック一覧

| イベントトピック名                                             | 説明                           |
| -------------------------------------------------------------- | :------------------------------ |
| [$events/message/delivered](#message-delivery-event-events-message-delivered) | メッセージ配信完了             |
| [$events/message/acked](#message-acknowledged-event-events-message-acked) | メッセージ配信アック           |
| [$events/message/dropped](#message-dropped-when-routing-event-events-message-dropped) | ルーティング時のメッセージ破棄 |
| [$events/message/delivery_dropped](#message-dropped-when-delivering-event-events-delivery-dropped) | 配信時のメッセージ破棄         |
| [$events/client/connected](#connection-complete-event-events-client-connected) | 接続完了                      |
| [$events/client/disconnected](#disconnect-event-events-client-disconnected) | 切断                         |
| [$events/client/connack](#connection-acknowledge-event-events-client-connack) | 接続応答                     |
| [$events/auth/check_authz_complete](#authorization-check-complete-event-events-client-check-authz-complete) | 認可チェック完了              |
| [$events/auth/check_authn_complete](#authentication-check-complete-event-events-client-check-authn-complete) | 認証チェック完了              |
| [$events/session/subscribed](#subscriber-event-events-session-subscribed) | サブスクライブ完了            |
| [$events/session/unsubscribed](#unsubscribe-event-events-session-unsubscribed) | サブスクリプション解除完了    |
| [$events/sys/alarm_activated](#system-alarm-activated-event-events-sys-alarm-activated) | システムアラーム発動         |
| [$events/sys/alarm_deactivated](#system-alarm-deactivated-event-events-sys-alarm-deactivated) | システムアラーム解除         |
| [$events/client/ping](#client-keepalive-ping-event-events-client-ping) | `PINGREQ`パケット受信         |

::: tip

EMQX 5.10.0以降、イベントトピックにネームスペースが導入され、論理的かつ階層的な構造に再編成されました。これによりイベントトピックの明確化、フィルタリング、管理が向上しています。

後方互換性のため、旧イベントトピックも引き続きサポートされていますが、新規設定ではネームスペース付きの新しいトピックの使用を推奨します。以下の表は旧トピックと新トピックの対応です。

| 旧イベントトピック                      | 新イベントトピック                         |
|:----------------------------------------|:------------------------------------------|
| `$events/client_connected`              | `$events/client/connected`                 |
| `$events/client_disconnected`           | `$events/client/disconnected`              |
| `$events/client_connack`                | `$events/client/connack`                   |
| `$events/client_check_authz_complete`   | `$events/auth/check_authz_complete`       |
| `$events/client_check_authn_complete`   | `$events/auth/check_authn_complete`       |
| `$events/session_subscribed`            | `$events/session/subscribed`               |
| `$events/session_unsubscribed`          | `$events/session/unsubscribed`             |
| `$events/message_delivered`             | `$events/message/delivered`                |
| `$events/message_acked`                 | `$events/message/acked`                    |
| `$events/message_dropped`               | `$events/message/dropped`                  |
| `$events/delivery_dropped`              | `$events/message/delivery_dropped`        |
| `$events/message_transformation_failed` | `$events/message_transformation/failed`   |
| `$events/schema_validation_failed`      | `$events/schema_validation/failed`        |

:::

### メッセージ配信完了イベント ("$events/message/delivered")

このイベントトピックは、メッセージがクライアントに配信された際にルールをトリガーできます。

例えば、`"$events/message/delivered"`イベントトピックから、パブリッシャーのIDとユーザー名、メッセージトピック、メッセージQoS、イベントが発生したEMQXノード、イベント発生時刻のフィールドを抽出するには、以下のステートメントを使用します。

例：
```sql
SELECT
  from_clientid,
  from_username,
  topic,
  qos,
  node,
  timestamp
FROM
  "$events/message/delivered"
```
出力例：
```json
{
  "topic": "t/a",
  "timestamp": 1645002753259,
  "qos": 1,
  "node": "emqx@127.0.0.1",
  "from_username": "u_emqx_1",
  "from_clientid": "c_emqx_1"
}
```
各フィールドの詳細説明は以下の通りです。

| フィールド             | 説明                                                   |
| :-------------------- | :----------------------------------------------------- |
| `id`                  | MQTTメッセージID                                       |
| `from_clientid`       | パブリッシャーのクライアントID                         |
| `from_username`       | パブリッシャーのユーザー名                             |
| `clientid`            | サブスクライバーのクライアントID                       |
| `username`            | サブスクライバーのユーザー名                           |
| `payload`             | MQTTペイロード                                         |
| `peerhost`            | クライアントIPアドレス                                 |
| `topic`               | MQTTトピック                                           |
| `qos`                 | QoSレベル                                              |
| `flags`               | フラグ                                                 |
| `pub_props`           | PUBLISHプロパティ（MQTT 5.0クライアントのみ）          |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `publish_received_at` | PUBLISHメッセージがEMQXに到達した時間（単位：ミリ秒）  |
| `node`                | イベントが発生したEMQXノード                           |

### メッセージ配信アックイベント ("$events/message/acked")

このイベントトピックは、メッセージ配信がアックされた際にルールをトリガーできます。

::: tip

QoS 1およびQoS 2のメッセージのみ利用可能です。

:::

例えば、`"$events/message/acked"`イベントトピックから、パブリッシャーのIDとユーザー名、メッセージトピック、メッセージQoS、イベントが発生したEMQXノード、イベント発生時刻のフィールドを抽出するには、以下のステートメントを使用します。 <!--node部分の確認が必要です-->

例：
```sql
SELECT
  from_clientid,
  from_username,
  topic,
  qos,
  node,
  timestamp
FROM
  "$events/message/acked"
```

出力例：
```json
{
  "topic": "t/a",
  "timestamp": 1645002965664,
  "qos": 1,
  "node": "emqx@127.0.0.1",
  "from_username": "u_emqx_1",
  "from_clientid": "c_emqx_1"
}
```

各フィールドの詳細説明は以下の通りです。

| フィールド             | 説明                                                   |
| :-------------------- | :----------------------------------------------------- |
| `id`                  | MQTTメッセージID                                       |
| `from_clientid`       | パブリッシャーのクライアントID                         |
| `from_username`       | パブリッシャーのユーザー名                             |
| `clientid`            | サブスクライバーのクライアントID                       |
| `username`            | サブスクライバーのユーザー名                           |
| `payload`             | MQTTペイロード                                         |
| `peerhost`            | クライアントIPアドレス                                 |
| `topic`               | MQTTトピック                                           |
| `qos`                 | QoSレベル                                              |
| `flags`               | フラグ                                                 |
| `pub_props`           | PUBLISHプロパティ（MQTT 5.0のみ）                      |
| `puback_props`        | PUBACKプロパティ（MQTT 5.0のみ）                       |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `publish_received_at` | PUBLISHメッセージがEMQXに到達した時間（単位：ミリ秒）  |
| `node`                | イベントが発生したEMQXノード                           |

### ルーティング時のメッセージ破棄イベント ("$events/message/dropped")

このイベントトピックは、メッセージがルーティング中に破棄された際にルールをトリガーできます。

例えば、`"$events/message/dropped"`イベントトピックから、破棄理由、メッセージトピック、メッセージQoS、イベントが発生したEMQXノード、イベント発生時刻のフィールドを抽出するには、以下のステートメントを使用します。

例：
```sql
SELECT
  reason,
  topic,
  qos,
  node,
  timestamp
FROM
  "$events/message/dropped"
```
出力例：
```json
{
  "topic": "t/a",
  "timestamp": 1645003103004,
  "reason": "no_subscribers",
  "qos": 1,
  "node": "emqx@127.0.0.1"
}
```

| フィールド             | 説明                                                   |
| :-------------------- | :----------------------------------------------------- |
| `id`                  | MQTTメッセージID                                       |
| `reason`              | 破棄理由：<br/><br/>`no_subscribers`：トピックにサブスクライブするクライアントが存在しない<br/><br/>`receive_maximum_exceeded`：`awaiting_rel`キューが満杯<br/><br/>`packet_identifier_inuse`：未解放のパケットIDを持つQoS 2メッセージを受信した |
| `clientid`            | パブリッシャーのクライアントID                         |
| `username`            | パブリッシャーのユーザー名                             |
| `payload`             | MQTTペイロード                                         |
| `peerhost`            | クライアントIPアドレス                                 |
| `topic`               | MQTTトピック                                           |
| `qos`                 | QoSレベル                                              |
| `flags`               | フラグ                                                 |
| `pub_props`           | PUBLISHプロパティ（MQTT 5.0のみ）                      |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `publish_received_at` | PUBLISHメッセージがEMQXに到達した時間（単位：ミリ秒）  |
| `node`                | イベントが発生したノード                               |

### 配信時のメッセージ破棄イベント ("$events/message/delivery_dropped")

このイベントトピックは、メッセージが配信中に破棄された際にルールをトリガーできます。

例えば、`"$events/message/delivery_dropped"`イベントトピックから、パブリッシャーのIDとユーザー名、破棄理由、メッセージトピックとQoSのフィールドを抽出するには、以下のステートメントを使用します。

例：
```sql
SELECT
  from_clientid,
  from_username,
  reason,
  topic,
  qos
FROM "$events/message/delivery_dropped"
```
出力例：
```json
{
  "topic": "t/a",
  "reason": "queue_full",
  "qos": 1,
  "from_username": "u_emqx_1",
  "from_clientid": "c_emqx_1"
}
```
各フィールドの詳細説明は以下の通りです。

| フィールド             | 説明                                                   |
| :-------------------- | :----------------------------------------------------- |
| `id`                  | MQTTメッセージID                                       |
| `reason`              | 破棄理由：<br/><br/>`queue_full`：メッセージ（QoS>0）キューが満杯<br/><br/>`no_local`：クライアントが自身のパブリッシュしたメッセージを受信できない<br/><br/>`expired`：メッセージまたはセッションが期限切れ<br/><br/>`qos0_msg`：メッセージ（QoS 0）キューが満杯 |
| `from_clientid`       | パブリッシャーのクライアントID                         |
| `from_username`       | パブリッシャーのユーザー名                             |
| `clientid`            | サブスクライバーのクライアントID                       |
| `username`            | サブスクライバーのユーザー名                           |
| `payload`             | MQTTペイロード                                         |
| `peerhost`            | クライアントIPアドレス                                 |
| `topic`               | MQTTトピック                                           |
| `qos`                 | メッセージのQoS                                        |
| `flags`               | フラグ                                                 |
| `pub_props`           | PUBLISHプロパティ（MQTT 5.0クライアントのみ）          |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `publish_received_at` | PUBLISHメッセージがEMQXに到達した時間（単位：ミリ秒）  |
| `node`                | イベントが発生したEMQXノード                           |

### 接続完了イベント ("$events/client/connected")

このイベントトピックは、クライアントが正常に接続した際にルールをトリガーできます。

例えば、`"$events/client/connected"`イベントトピックから、クライアントID、ユーザー名、キープアライブ間隔、接続したMQTTクライアントがブリッジとして動作しているかどうかのフィールドを抽出するには、以下のステートメントを使用します。

例：
```sql
SELECT
  clientid,
  username,
  keepalive,
  is_bridge
FROM
  "$events/client/connected"
```
出力例：
```json
{
  "username": "u_emqx",
  "keepalive": 60,
  "is_bridge": false,
  "clientid": "c_emqx"
}
```

受信したMQTTメッセージから選択可能なフィールドは以下の通りです。

| フィールド             | 説明                                                   |
| :-------------------- | :----------------------------------------------------- |
| `clientid`            | クライアントID                                         |
| `username`            | クライアントのユーザー名                               |
| `mountpoint`          | ブリッジメッセージのマウントポイント                   |
| `peername`            | 端末のIPアドレスとポート <!--すべてポートを含むべきか？--> |
| `sockname`            | EMQXがリッスンしているIPアドレスとポート               |
| `proto_name`          | プロトコル名                                           |
| `proto_ver`           | プロトコルバージョン                                   |
| `keepalive`           | MQTTキープアライブ間隔                                 |
| `clean_start`         | MQTTのclean_startフラグ                                |
| `expiry_interval`     | MQTTセッションの有効期限                               |
| `is_bridge`           | クライアントがブリッジとして動作しているかどうか       |
| `connected_at`        | クライアント接続完了時刻（単位：ミリ秒）               |
| `conn_props`          | CONNECTプロパティ（MQTT 5.0クライアントのみ）          |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `node`                | イベントが発生したEMQXノード                           |
| `client_attrs`        | [クライアント属性](../client-attributes/client-attributes.md) |

### 切断イベント ("$events/client/disconnected")

このイベントトピックは、クライアントが切断された際にルールをトリガーできます。

例えば、`"$events/client/disconnected"`イベントトピックから、クライアントID、ユーザー名、切断理由、切断時刻、イベントが発生したEMQXノードのフィールドを抽出するには、以下のステートメントを使用します。

例：
```sql
SELECT
  clientid,
  username,
  reason,
  connected_at,
  disconnected_at,
  node
FROM
  "$events/client/disconnected"
```
出力例：
```json
{
  "username": "u_emqx",
  "reason": "normal",
  "node": "emqx@127.0.0.1",
  "connected_at": 1645003578036,
  "disconnected_at": 1645003578536,
  "clientid": "c_emqx"
}
```

| フィールド             | 説明                                                   |
| :-------------------- | :----------------------------------------------------- |
| `reason`              | 切断理由：<br/><br/>`normal`：クライアントが意図的に切断<br/><br/>`kicked`：REST APIによってEMQXが強制切断<br/><br/>`keepalive_timeout`：キープアライブ時間切れ<br/><br/>`not_authorized`：認可失敗<br/><br/>`tcp_closed`：ピアがネットワーク接続を閉じた<br/><br/>`discarded`：`clean_start`が`true`の別クライアントが同じClientIDで接続し、前の接続が切断された<br/><br/>`takenover`：`clean_start`が`false`の別クライアントが同じClientIDで接続し、前の接続を引き継いだ<br/><br/>`internal_error`：不正なメッセージ形式やその他不明なエラーが発生 |
| `clientid`            | クライアントID                                         |
| `username`            | クライアントのユーザー名                               |
| `peername`            | IPアドレスとポート番号                                 |
| `sockname`            | EMQXがリッスンしているIPアドレスとポート               |
| `connected_at`        | クライアント接続開始時刻（単位：ミリ秒）。このタイムスタンプは現在のセッションが確立された時刻を示し、切断イベントがどの接続セッションに属するかを識別するために使用されます。遅延した切断イベントが新しい接続状態を上書きしないようにします。 |
| `disconnected_at`     | クライアント切断完了時刻（単位：ミリ秒）               |
| `disconn_props`       | DISCONNECTプロパティ（MQTT 5.0クライアントのみ）       |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `node`                | イベントが発生したEMQXノード                           |
| `client_attrs`        | [クライアント属性](../client-attributes/client-attributes.md) |

### 接続応答イベント ("$events/client/connack")

このイベントトピックは、EMQXがクライアントに`CONNACK`パケットを送信した際にルールをトリガーできます。

例：

```sql
SELECT
  clientid,
  username,
  reason_code,
  node
FROM
  "$events/client/connack"
```

出力例：

```json
{
  "username": "u_emqx",
  "reason_code": "success",
  "node": "emqx@127.0.0.1",
  "connected_at": 1645003578536,
  "clientid": "c_emqx"
}
```

抽出可能なフィールドは以下の通りです。

| フィールド             | 説明                                                   |
| --------------------- | :----------------------------------------------------- |
| `reason_code`         | 理由コード*                                            |
| `clientid`            | パブリッシャーのクライアントID                         |
| `username`            | パブリッシャーのユーザー名                             |
| `peername`            | IPアドレスとポート                                     |
| `sockname`            | EMQXがリッスンしているIPアドレスとポート               |
| `proto_name`          | プロトコル名                                           |
| `proto_ver`           | プロトコルバージョン                                   |
| `keepalive`           | MQTTキープアライブ間隔                                 |
| `clean_start`         | MQTTのclean_startフラグ                                |
| `expiry_interval`     | MQTTセッションの有効期限                               |
| `conn_props`          | CONNECTプロパティ（MQTT 5.0クライアントのみ）          |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `node`                | イベントが発生したEMQXノード                           |

[^*]: MQTT v5.0プロトコルでは、リターンコードは理由コードに名称が変更され、より多くのエラータイプを示す理由コードが追加されています（[Reason code and ACK - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)）。

以下はMQTT v3.1.1およびMQTT v5.0の理由コード一覧です。

:::: tabs type:card

::: tab MQTT v3.1.1

| 理由コード                      | 説明                                                   |
| ------------------------------ | ------------------------------------------------------ |
| `connection_accepted`           | 接続が受理された                                     |
| `unacceptable_protocol_version` | EMQXがクライアントの要求するMQTTプロトコルをサポートしていない |
| `client_identifier_not_valid`   | クライアントIDがEMQXで許可されていない               |
| `server_unavaliable`            | ネットワーク接続は確立されたが、MQTTサービスが利用不可 |
| `malformed_username_or_password`| ユーザー名またはパスワードのデータ形式が不正          |
| `unauthorized_client`           | クライアントの接続が認可されていない                   |

:::

::: tab MQTT v5.0

| 理由コード                     | 説明                                                   |
| ----------------------------- | ------------------------------------------------------ |
| `success`                     | 接続成功                                              |
| `unspecified_error`           | 不明なエラー                                          |
| `malformed_packet`            | パケットが不正                                        |
| `protocol_error`              | プロトコルエラー                                      |
| `implementation_specific_error`| 実装固有のエラー                                      |
| `unsupported_protocol_version`| サポートされていないプロトコルバージョン              |
| `client_identifier_not_valid` | 無効なクライアントID                                  |
| `bad_username_or_password`    | 無効なユーザー名またはパスワード                      |
| `not_authorized`              | 認可されていない                                      |
| `server_unavailable`          | サーバー利用不可                                      |
| `server_busy`                 | サーバーがビジー状態                                  |
| `banned`                     | 接続禁止                                              |
| `bad_authentication_method`   | 無効な認証方式                                        |
| `topic_name_invalid`          | 無効なトピック名                                     |
| `packet_too_large`            | パケットが大きすぎる                                 |
| `quota_exceeded`              | クォータ超過                                          |
| `retain_not_supported`        | Retainメッセージ機能非対応                            |
| `qos_not_supported`           | サポートされていないQoSレベル                         |
| `use_another_server`          | 別のブローカーを使用してください                      |
| `server_moved`                | ブローカーが移動した                                  |
| `connection_rate_exceeded`    | 接続レート制限超過                                   |

:::

::::

### 認可チェック完了イベント ("$events/auth/check_authz_complete")

このイベントトピックは、クライアントの認可チェックが完了した際にルールをトリガーできます。

例：

```sql
SELECT
  clientid,
  username,
  topic,
  action,
  result,
  authz_source,
  node
FROM
  "$events/auth/check_authz_complete"
```

出力例：

```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "action": "publish",
  "result": "allow",
  "authz_source": "cache",
  "node": "emqx@127.0.0.1",
  "clientid": "c_emqx"
}
```

抽出可能なフィールドは以下の通りです。

| フィールド             | 説明                                                   |
| --------------------- | :----------------------------------------------------- |
| `clientid`            | クライアントID                                         |
| `username`            | ユーザー名                                             |
| `peerhost`            | クライアントIPアドレス                                 |
| `topic`               | MQTTトピック                                           |
| `action`              | パブリッシュまたはサブスクライブのアクション           |
| `result`              | アクセス制御チェックの結果                             |
| `authz_source`        | 認可のソース                                           |
| `timestamp`           | タイムスタンプ（単位：ミリ秒）                         |
| `node`                | イベントが発生したEMQXノード                           |
| `client_attrs`        | [クライアント属性](../client-attributes/client-attributes.md) |

### 認証チェック完了イベント ("$events/auth/check_authn_complete")

このイベントトピックは、クライアントの認証チェックが完了した際にルールをトリガーできます。

例：

```sql
SELECT
  clientid,
  username,
  reason_code,
  is_superuser,
  is_anonymous
FROM
  "$events/auth/check_authn_complete"
```

出力例：

```json
{
  "clientid": "c_emqx",
  "username": "u_emqx",
  "reason_code": "success",
  "is_superuser": true,
  "is_anonymous": false
}
```

抽出可能なフィールドは以下の通りです。

| フィールド             | 説明                                                   |
| --------------------- | :----------------------------------------------------- |
| `clientid`            | クライアントID                                         |
| `username`            | ユーザー名                                             |
| `peername`            | クライアントIPアドレス                                 |
| `reason_code`         | 認証結果                                               |
| `is_superuser`        | このクライアントがスーパーユーザーかどうか            |
| `is_anonymous`        | このクライアントが匿名ユーザーかどうか                |
| `client_attrs`        | [クライアント属性](../client-attributes/client-attributes.md) |

### サブスクライバーイベント ("$events/session/subscribed")

このイベントトピックは、クライアントが正常にサブスクライブした際にルールをトリガーできます。

例：

```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session/subscribed"
```

出力例：

```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "clientid": "c_emqx"
}
```

抽出可能なフィールドは以下の通りです。

| フィールド             | 説明                                                   |
| --------------------- | :----------------------------------------------------- |
| `clientid`            | クライアントID                                         |
| `username`            | クライアントのユーザー名                               |
| `peerhost`            | クライアントIPアドレス                                 |
| `topic`               | MQTTトピック                                           |
| `qos`                 | QoSレベル                                              |
| `sub_props`           | SUBSCRIBEプロパティ（MQTT 5.0クライアントのみ）        |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `node`                | イベントが発生したEMQXノード                           |
| `client_attrs`        | [クライアント属性](../client-attributes/client-attributes.md) |

### サブスクリプション解除イベント ("$events/session/unsubscribed")

このイベントトピックは、クライアントのサブスクリプション解除が成功した際にルールをトリガーできます。

例：
```sql
SELECT
  clientid,
  username,
  topic,
  qos
FROM
  "$events/session/unsubscribed"
```
出力例：
```json
{
  "username": "u_emqx",
  "topic": "t/a",
  "qos": 1,
  "clientid": "c_emqx"
}
```
抽出可能なフィールドは以下の通りです。

| フィールド             | 説明                                                   |
| --------------------- | :----------------------------------------------------- |
| `clientid`            | クライアントID                                         |
| `username`            | クライアントのユーザー名                               |
| `peerhost`            | クライアントIPアドレス                                 |
| `topic`               | MQTTトピック                                           |
| `qos`                 | QoSレベル                                              |
| `unsub_props`         | UNSUBSCRIBEプロパティ（MQTT 5.0クライアントのみ）      |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `node`                | イベントが発生したEMQXノード                           |
| `client_attrs`        | [クライアント属性](../client-attributes/client-attributes.md) |

### システムアラーム発動イベント ("$events/sys/alarm_activated")

このイベントトピックは、EMQXのシステムアラームが発動した際にルールをトリガーできます。

例えば、`"$events/sys/alarm_activated"`イベントトピックから、アラーム名、詳細、説明メッセージ、発動時刻のフィールドを抽出するには、以下のステートメントを使用します。

例：

```sql
SELECT
  name,
  details,
  message,
  activated_at,
  node
FROM
  "$events/sys/alarm_activated"
```

出力例：

```json
{
  "name": "too_many_processes",
  "details": {
    "usage": "99%",
    "high_watermark": "80%"
  },
  "message": "99% process usage",
  "activated_at": 1645003578536000,
  "node": "emqx@127.0.0.1"
}
```

抽出可能なフィールドは以下の通りです。

| フィールド             | 説明                                                   |
| --------------------- | :----------------------------------------------------- |
| `name`                | アラームの短い識別子（例："too_many_processes"）       |
| `details`             | アラームに関する追加詳細を含むJSONオブジェクト。スキーマは固定されていません（例：`{"usage": "99%", "high_watermark": "80%"}`） |
| `message`             | アラームの説明メッセージ（例："99% process usage"）    |
| `activated_at`        | アラーム発動時のUnixタイムスタンプ（マイクロ秒）       |
| `node`                | イベントが発生したEMQXノード                           |

### システムアラーム解除イベント ("$events/sys/alarm_deactivated")

このイベントトピックは、EMQXのシステムアラームが解除された際にルールをトリガーできます。

例えば、`"$events/sys/alarm_deactivated"`イベントトピックから、アラーム名、詳細、説明メッセージ、発動時刻、解除時刻のフィールドを抽出するには、以下のステートメントを使用します。

例：

```sql
SELECT
  name,
  details,
  message,
  activated_at,
  deactivated_at,
  node
FROM
  "$events/sys/alarm_deactivated"
```

出力例：

```json
{
  "name": "too_many_processes",
  "details": {
    "usage": "99%",
    "high_watermark": "80%"
  },
  "message": "99% process usage",
  "activated_at": 1645003578536000,
  "deactivated_at": 1645004000000000,
  "node": "emqx@127.0.0.1"
}
```

抽出可能なフィールドは以下の通りです。

| フィールド             | 説明                                                   |
| --------------------- | :----------------------------------------------------- |
| `name`                | アラームの短い識別子（例："too_many_processes"）       |
| `details`             | アラームに関する追加詳細を含むJSONオブジェクト。スキーマは固定されていません（例：`{"usage": "99%", "high_watermark": "80%"}`） |
| `message`             | アラームの説明メッセージ（例："99% process usage"）    |
| `activated_at`        | アラーム発動時のUnixタイムスタンプ（マイクロ秒）       |
| `deactivated_at`      | アラーム解除時のUnixタイムスタンプ（マイクロ秒）       |
| `node`                | イベントが発生したEMQXノード                           |

### クライアントキープアライブ（PING）イベント ("$events/client/ping")

このイベントトピックは、EMQXが接続されたMQTTクライアントから`PINGREQ`パケットを受信した際にルールをトリガーできます。これはクライアントのハートビート受信を意味します。

主に診断やトラブルシューティングに使用されます。`$events/client/disconnected`はクライアント切断の理由（例：`keepalive_timeout`）を説明しますが、`"$events/client/ping"`イベントはEMQXが実際にクライアントからハートビートパケットを受信した直接的な証拠を提供し、クライアント側やネットワーク側の問題とブローカー側の問題を区別するのに役立ちます。

例えば、`"$events/client/ping"`イベントトピックから、クライアントID、ユーザー名、ネゴシエートされたキープアライブ間隔、イベント発生時刻、イベントが発生したEMQXノードのフィールドを抽出するには、以下のステートメントを使用します。

例：

```sql
SELECT
  clientid,
  username,
  keepalive,
  timestamp,
  node
FROM
  "$events/client/ping"
```

出力例：

```json
{
  "clientid": "c_emqx",
  "username": "u_emqx",
  "keepalive": 60,
  "timestamp": 1645003800123,
  "node": "emqx@127.0.0.1"
}
```

クライアントPINGイベントから抽出可能なフィールドは以下の通りです。

| フィールド             | 説明                                                   |
| --------------------- | :----------------------------------------------------- |
| `clientid`            | クライアントID                                         |
| `clean_start`         | MQTTのclean_startフラグ                                |
| `username`            | クライアントのユーザー名                               |
| `peername`            | クライアントのIPアドレスとポート                       |
| `sockname`            | EMQXがリッスンしているIPアドレスとポート               |
| `proto_name`          | プロトコル名                                           |
| `proto_ver`           | プロトコルバージョン                                   |
| `keepalive`           | ネゴシエートされたMQTTキープアライブ間隔               |
| `timestamp`           | イベント発生時刻（単位：ミリ秒）                       |
| `node`                | イベントが発生したEMQXノード                           |
| `conn_props`          | CONNECTプロパティ（MQTT 5.0クライアントのみ）          |
| `expiry_interval`     | MQTTセッションの有効期限                               |

## データブリッジ

ルールは、`$bridges/`で始まるトピックを使用して、データブリッジによってトリガーされたメッセージやイベントを表現します。フォーマットは以下の通りです。

 `$bridges/<type>:<name>`

ここで、

- `<type>:<name>`はブリッジID、
- `<type>`はブリッジタイプ、
- `<name>`はブリッジ名です。

例えば、MQTTブリッジのイベントは`"$bridges/mqtt:*"`の形式で参照できます。MQTTデータブリッジ`my_mqtt_bridge`が送信するすべてのメッセージに対してルールを設定するには、以下のステートメントを使用します。

例：

```sql
SELECT
  *
FROM
  "$bridges/mqtt:my_mqtt_bridge"
```

出力例：

```json
{
  "id": "0005E27C1D24E44FF440000017520000",
  "server": "broker.emqx.io:1883",
  "payload": "hello",
  "topic": "t/a",
  "qos": 1,
  "dup": false,
  "retain": false,
  "pub_props": {
    "Message-Expiry-Interval": 30,
    "Payload-Format-Indicator": 0,
    "User-Property": {
      "foo": "bar"
    },
    "User-Property-Pairs": [
      {
        "key": "foo"
      },
      {
        "value": "bar"
      }
    ]
  },
  "message_received_at": 1645002753259
}
```

返される出力の各フィールドは以下の通りです。

| フィールド             | 説明                                                   |
| :-------------------- | :----------------------------------------------------- |
| `id`                  | MQTTメッセージID                                       |
| `server`              | リモートMQTTブローカーのサーバー名（例："broker.emqx.io:1883"） |
| `payload`             | MQTTペイロード                                         |
| `topic`               | MQTTトピック                                           |
| `qos`                 | MQTTのQoS                                             |
| `dup`                 | MQTTのDUPフラグ                                       |
| `retain`              | MQTTのRetainフラグ                                    |
| `pub_props`           | PUBLISHプロパティ（MQTT 5.0クライアントのみ）          |
| `message_received_at` | メッセージ受信時のタイムスタンプ（単位：ミリ秒）       |
