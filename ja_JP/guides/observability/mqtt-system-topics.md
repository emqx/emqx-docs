# システムトピック

EMQX は定期的に稼働状況、メッセージ統計、クライアントのオンライン・オフラインイベントを `$SYS/` で始まるシステムトピックにパブリッシュします。

`$SYS` トピックのパスは `$SYS/brokers/{node}/` で始まり、`{node}` はイベントやメッセージが生成されたノード名を示します。例：

```bash
$SYS/brokers/emqx@127.0.0.1/version
$SYS/brokers/emqx@127.0.0.1/uptime
```

`$SYS` システムメッセージのパブリッシュ間隔はダッシュボードの `Management/MQTT Setting/System Topic` で設定します。

::: tip
デフォルトでは、localhost 上の MQTT クライアントのみが `$SYS` トピックのサブスクライブを許可されています。  
パブリッシュおよびサブスクライブの ACL ルールを変更する場合は、[ファイル認可](../access-control/authz/file.md) を参照してください。

EMQX の `$SYS` トピックの多くのデータは、より疎結合な他の方法でも取得可能です。  
デバイスのオンライン・オフライン状態はルールエンジンでキャプチャして処理できます。  
:::

## クラスター状態情報

| トピック                          | 説明               |
| -------------------------------- | ------------------ |
| $SYS/brokers                     | クラスターのノード一覧 |
| $SYS/brokers/\${node}/version    | EMQX のバージョン   |
| $SYS/brokers/\${node}/uptime     | EMQX の起動時間     |
| $SYS/brokers/\${node}/datetime   | EMQX の現在時刻     |
| $SYS/brokers/\${node}/sysdescr   | EMQX の説明         |

## クライアントのオンライン・オフラインイベント

これらのイベントはデフォルトで有効です。無効にする場合は `sys_topics.sys_event_messages` を参照してください。

`$SYS` トピックのプレフィックス：`$SYS/brokers/${node}/clients/`

| トピック                    | 説明                                                       |
| --------------------------- | ---------------------------------------------------------- |
| ${clientid}/connected       | オンラインイベント。クライアントがオンラインになったときにパブリッシュされるメッセージ |
| ${clientid}/disconnected    | オフラインイベント。クライアントがオフラインになったときにパブリッシュされるメッセージ |

`connected` イベントメッセージのペイロードは JSON 形式で解析可能です：

```bash
{
    "username": "foo",
    "ts": 1625572213873,
    "sockport": 1883,
    "proto_ver": 4,
    "proto_name": "MQTT",
    "keepalive": 60,
    "ipaddress": "127.0.0.1",
    "expiry_interval": 0,
    "connected_at": 1625572213873,
    "connack": 0,
    "clientid": "emqtt-8348fe27a87976ad4db3",
    "clean_start": true
}
```

`disconnected` イベントメッセージのペイロードは JSON 形式で解析可能です：

```bash
{
    "username": "foo",
    "ts": 1625572213873,
    "sockport": 1883,
    "reason": "tcp_closed",
    "proto_ver": 4,
    "proto_name": "MQTT",
    "ipaddress": "127.0.0.1",
    "disconnected_at": 1625572213873,
    "clientid": "emqtt-8348fe27a87976ad4db3"
}
```

## クライアントのサブスクライブ・アンサブスクライブイベント

これらのイベントはデフォルトで無効です。有効にする場合は `sys_topics.sys_event_messages` を参照してください。

`$SYS` トピックのプレフィックス：`$SYS/brokers/${node}/clients/`

| トピック                    | 説明                                         |
| --------------------------- | -------------------------------------------- |
| ${clientid}/subscribed      | サブスクライブイベント。クライアントがトピックをサブスクライブしたときにパブリッシュされるメッセージ |
| ${clientid}/unsubscribed    | アン・サブスクライブイベント。クライアントがトピックのサブスクライブを解除したときにパブリッシュされるメッセージ |

`subscribed` イベントメッセージのペイロードは JSON 形式で解析可能です：

```bash
{
    "username":"foo",
    "ts":1625572213878,
    "topic":"/the/best/mqtt/broker/is/emqx",
    "subopts":{
        "sub_props":{},
        "rh":0,
        "rap":0,
        "qos":0,
        "nl":0,
        "is_new":true
    },
    "protocol":"mqtt",
    "clientid":"emqtt-8348fe27a87976ad4db3"
}
```

`unsubscribed` イベントメッセージのペイロードは JSON 形式で解析可能です：

```bash
{
    "username":"foo",
    "ts":1625572213899,
    "topic":"/the/best/mqtt/broker/is/emqx",
    "protocol":"mqtt",
    "clientid":"emqtt-8348fe27a87976ad4db3"
}
```

## 統計情報

システムトピックのプレフィックス：`$SYS/brokers/${node}/stats/`

### クライアント統計

| トピック               | 説明                         |
| ---------------------- | ---------------------------- |
| connections/count      | 現在のクライアント総数       |
| connections/max        | 最大クライアント数           |

### サブスクリプション統計

| トピック                      | 説明                                         |
| ----------------------------- | -------------------------------------------- |
| suboptions/count              | 現在のサブスクリプションオプション数       |
| suboptions/max                | 最大サブスクリプションオプション数         |
| subscribers/count             | 現在のサブスクライバー数                     |
| subscribers/max               | 最大サブスクライバー数                       |
| subscriptions/count           | 現在のサブスクリプション総数                 |
| subscriptions/max             | 最大サブスクリプション数                     |
| subscriptions/shared/count    | 現在の共有サブスクリプション総数             |
| subscriptions/shared/max      | 最大共有サブスクリプション数                 |

### トピック統計

| トピック           | 説明                     |
| ------------------ | ------------------------ |
| topics/count       | 現在のトピック総数       |
| topics/max         | 最大トピック数           |

### ルート統計

| トピック           | 説明                     |
| ------------------ | ------------------------ |
| routes/count       | 現在のルート総数         |
| routes/max         | 最大ルート数             |

`topics/count` と `topics/max` は `routes/count` と `routes/max` と数値的に同じです。

### スループット（バイト／パケット／メッセージ）統計

システムトピックのプレフィックス：`$SYS/brokers/${node}/metrics/`

### 送受信バイト統計

| トピック           | 説明                     |
| ------------------ | ------------------------ |
| bytes/received     | 累積受信バイト数         |
| bytes/sent         | 累積送信バイト数         |

### 送受信 MQTT パケット統計

| トピック                      | 説明                                         |
| ----------------------------- | -------------------------------------------- |
| packets/received             | 累積受信 MQTT パケット数                     |
| packets/sent                 | 累積送信 MQTT パケット数                     |
| packets/connect/received     | 累積受信 CONNECT パケット数                   |
| packets/connack/sent         | 累積送信 CONNACK パケット数                   |
| packets/publish/received     | 累積受信 PUBLISH パケット数                   |
| packets/publish/sent         | 累積送信 PUBLISH パケット数                   |
| packets/publish/error        | 累積処理中の PUBLISH エラーパケット数         |
| packets/publish/auth_error   | 累積拒否された PUBLISH パケット数             |
| packets/publish/dropped      | 累積ドロップされた PUBLISH パケット数         |
| packets/puback/received      | 累積受信 PUBACK パケット数                    |
| packets/puback/sent          | 累積送信 PUBACK パケット数                    |
| packets/puback/inuse         | 累積ドロップされた PUBACK パケット数          |
| packets/puback/missed        | 累積見逃された PUBACK パケット数              |
| packets/pubrec/received      | 累積受信 PUBREC パケット数                    |
| packets/pubrec/sent          | 累積送信 PUBREC パケット数                    |
| packets/pubrec/inuse         | 累積ドロップされた PUBREC パケット数          |
| packets/pubrec/missed        | 累積見逃された PUBREC パケット数              |
| packets/pubrel/received      | 累積受信 PUBREL パケット数                    |
| packets/pubrel/sent          | 累積送信 PUBREL パケット数                    |
| packets/pubrel/missed        | 累積見逃された PUBREL パケット数              |
| packets/pubcomp/received     | 累積受信 PUBCOMP パケット数                   |
| packets/pubcomp/sent         | 累積送信 PUBCOMP パケット数                   |
| packets/pubcomp/inuse        | 累積ドロップされた PUBCOMP パケット数         |
| packets/pubcomp/missed       | 累積見逃された PUBCOMP パケット数             |
| packets/subscribe/received   | 累積受信 SUBSCRIBE パケット数                 |
| packets/subscribe/error      | 累積処理中の SUBSCRIBE エラーパケット数       |
| packets/subscribe/auth_error | 累積拒否された SUBSCRIBE パケット数           |
| packets/suback/sent          | 累積送信 SUBACK パケット数                    |
| packets/unsubscribe/received | 累積受信 UNSUBSCRIBE パケット数               |
| packets/unsuback/sent        | 累積送信 UNSUBACK パケット数                  |
| packets/pingreq/received     | 累積受信 PINGREQ パケット数                   |
| packets/pingresp/sent        | 累積送信 PINGRESP パケット数                  |
| packets/disconnect/received  | 累積受信 DISCONNECT パケット数                |
| packets/disconnect/sent      | 累積送信 DISCONNECT パケット数                 |
| packets/auth/received        | 累積受信 AUTH パケット数                      |
| packets/auth/sent            | 累積送信 AUTH パケット数                      |

### MQTT 送受信メッセージ統計

| トピック                     | 説明                                         |
| ---------------------------- | -------------------------------------------- |
| messages/received           | 累積受信メッセージ数                         |
| messages/sent               | 累積送信メッセージ数                         |
| messages/qos0/received      | 累積受信 QoS 0 メッセージ数                   |
| messages/qos0/sent          | 累積送信 QoS 0 メッセージ数                   |
| messages/qos1/received      | 累積受信 QoS 1 メッセージ数                   |
| messages/qos1/sent          | 累積送信 QoS 1 メッセージ数                   |
| messages/qos2/received      | 累積受信 QoS 2 メッセージ数                   |
| messages/qos2/sent          | 累積送信 QoS 2 メッセージ数                   |
| messages/publish            | 累積 PUBLISH メッセージ数                     |
| messages/dropped            | ドロップされたメッセージ総数                   |
| messages/dropped/expired    | ドロップされたメッセージ総数（有効期限切れ）   |
| messages/dropped/no_subscribers | ドロップされたメッセージ総数（サブスクライバーなし） |
| messages/forward            | ノードによって転送されたメッセージ総数         |
| messages/retained           | 累積保持メッセージ数                           |
| messages/delayed            | 累積遅延メッセージ数                           |
| messages/delivered          | 累積配信済みメッセージ数                       |
| messages/acked              | 累積アック済みメッセージ数                     |

## アラーム - システムアラーム

システムトピックのプレフィックス：`$SYS/brokers/${node}/alarms/`

| トピック      | 説明                 |
| ------------- | -------------------- |
| activate      | 新規発生したアラーム |
| deactivate    | 解消されたアラーム   |

## Sysmon - システムモニタリング

システムトピックのプレフィックス：`$SYS/brokers/${node}/sysmon/`

| トピック          | 説明                                                                                     |
| ----------------- | ---------------------------------------------------------------------------------------- |
| long_gc           | ガベージコレクションに時間がかかりすぎている                                           |
| long_schedule     | プロセススケジューリングに時間がかかりすぎており、スケジューラーのタイムスライスを多く消費している |
| large_heap        | プロセスのメモリ使用量が多すぎる                                                       |
| busy_port         | プロセスがビジーポートにメッセージを送信し、プロセスがハングしている                     |
| busy_dist_port    | ノード間通信に使用される分散通信ポートがビジーで、プロセスがハングしている               |
