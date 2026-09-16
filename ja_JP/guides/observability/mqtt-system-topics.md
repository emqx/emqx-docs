# システムトピック

EMQXは定期的に稼働状況、メッセージ統計、クライアントのオンライン・オフラインイベントを、`$SYS/`で始まるシステムトピックにパブリッシュします。

`$SYS`トピックのパスは`$SYS/brokers/{node}/`で始まり、`{node}`はイベントやメッセージが生成されたノード名を示します。例：

```bash
$SYS/brokers/emqx@127.0.0.1/version
$SYS/brokers/emqx@127.0.0.1/uptime
```

`$SYS`システムメッセージのパブリッシュ間隔は、ダッシュボードの `Management/MQTT Setting/System Topic` で設定可能です。

::: tip
デフォルトでは、localhost上のMQTTクライアントのみが`$SYS`トピックのサブスクライブを許可されています。  
パブリッシュおよびサブスクライブのACLルールを変更する場合は、[File Authorization](../access-control/authz/file.md) をご参照ください。

EMQXの`$SYS`トピックの多くのデータは、より低い結合度で他の方法から取得可能です。  
デバイスのオンライン・オフライン状態はルールエンジンでキャプチャし処理できます。
:::

## クラスター状態情報

| トピック                          | 説明               |
| -------------------------------- | ------------------ |
| $SYS/brokers                     | クラスターのノード一覧 |
| $SYS/brokers/\${node}/version    | EMQXのバージョン    |
| $SYS/brokers/\${node}/uptime     | EMQXの起動時間      |
| $SYS/brokers/\${node}/datetime   | EMQXの時刻          |
| $SYS/brokers/\${node}/sysdescr   | EMQXの説明          |

## クライアントのオンライン・オフラインイベント

これらのイベントはデフォルトで有効です。無効化する場合は `sys_topics.sys_event_messages` をご参照ください。

`$SYS`トピックのプレフィックス：`$SYS/brokers/${node}/clients/`

| トピック                    | 説明                                         |
| -------------------------- | -------------------------------------------- |
| ${clientid}/connected      | オンラインイベント。クライアントがオンラインになった際にパブリッシュされるメッセージ |
| ${clientid}/disconnected   | オフラインイベント。クライアントがオフラインになった際にパブリッシュされるメッセージ |

`connected`イベントメッセージのペイロードはJSON形式で解析可能です：

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

`disconnected`イベントメッセージのペイロードもJSON形式で解析可能です：

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

これらのイベントはデフォルトで無効です。有効化する場合は `sys_topics.sys_event_messages` をご参照ください。

`$SYS`トピックのプレフィックス：`$SYS/brokers/${node}/clients/`

| トピック                    | 説明                                         |
| -------------------------- | -------------------------------------------- |
| ${clientid}/subscribed     | サブスクライブイベント。クライアントがトピックをサブスクライブした際にパブリッシュされるメッセージ |
| ${clientid}/unsubscribed   | アンザブスクライブイベント。クライアントがトピックのサブスクライブを解除した際にパブリッシュされるメッセージ |

`subscribed`イベントメッセージのペイロードはJSON形式で解析可能です：

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

`unsubscribed`イベントメッセージのペイロードもJSON形式で解析可能です：

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

| トピック             | 説明                          |
| ------------------- | ----------------------------- |
| connections/count   | 現在のクライアント総数         |
| connections/max     | クライアントの最大数           |

### サブスクリプション統計

| トピック                      | 説明                                    |
| ---------------------------- | --------------------------------------- |
| suboptions/count             | 現在のサブスクリプションオプション数     |
| suboptions/max               | 最大サブスクリプションオプション数       |
| subscribers/count            | 現在のサブスクライバー数                 |
| subscribers/max              | 最大サブスクライバー数                   |
| subscriptions/count          | 現在のサブスクリプション総数             |
| subscriptions/max            | 最大サブスクリプション数                 |
| subscriptions/shared/count   | 現在の共有サブスクリプション総数         |
| subscriptions/shared/max     | 最大共有サブスクリプション数             |

### トピック統計

| トピック        | 説明                          |
| -------------- | ----------------------------- |
| topics/count   | 現在のトピック総数             |
| topics/max     | 最大トピック数                 |

### ルート統計

| トピック        | 説明                          |
| -------------- | ----------------------------- |
| routes/count   | 現在のルート総数               |
| routes/max     | 最大ルート数                   |

`topics/count` と `topics/max` は数値的に `routes/count` と `routes/max` と同じです。

### スループット（バイト／パケット／メッセージ）統計

システムトピックのプレフィックス：`$SYS/brokers/${node}/metrics/`

### 送受信バイト統計

| トピック          | 説明                        |
| ---------------- | --------------------------- |
| bytes/received   | 累積受信バイト数             |
| bytes/sent       | 累積送信バイト数             |

### 送受信MQTTパケット統計

| トピック                        | 説明                                        |
| ------------------------------ | ------------------------------------------- |
| packets/received               | 累積受信MQTTパケット数                       |
| packets/sent                   | 累積送信MQTTパケット数                       |
| packets/connect/received       | 累積受信CONNECTパケット数                     |
| packets/connack/sent           | 累積送信CONNACKパケット数                     |
| packets/publish/received       | 累積受信PUBLISHパケット数                     |
| packets/publish/sent           | 累積送信PUBLISHパケット数                     |
| packets/publish/error          | PUBLISHエラーパケットの累積処理数             |
| packets/publish/auth_error     | PUBLISH拒否パケットの累積数                   |
| packets/publish/dropped        | PUBLISHドロップパケットの累積数               |
| packets/puback/received        | 累積受信PUBACKパケット数                      |
| packets/puback/sent            | 累積送信PUBACKパケット数                      |
| packets/puback/inuse           | 累積ドロップPUBACKパケット数                  |
| packets/puback/missed          | 累積見逃しPUBACKパケット数                    |
| packets/pubrec/received        | 累積受信PUBRECパケット数                      |
| packets/pubrec/sent            | 累積送信PUBRECパケット数                      |
| packets/pubrec/inuse           | 累積ドロップPUBRECパケット数                  |
| packets/pubrec/missed          | 累積見逃しPUBRECパケット数                    |
| packets/pubrel/received        | 累積受信PUBRELパケット数                      |
| packets/pubrel/sent            | 累積送信PUBRELパケット数                      |
| packets/pubrel/missed          | 累積見逃しPUBRELパケット数                    |
| packets/pubcomp/received       | 累積受信PUBCOMPパケット数                     |
| packets/pubcomp/sent           | 累積送信PUBCOMPパケット数                     |
| packets/pubcomp/inuse          | 累積ドロップPUBCOMPパケット数                 |
| packets/pubcomp/missed         | 累積見逃しPUBCOMPパケット数                   |
| packets/subscribe/received     | 累積受信SUBSCRIBEパケット数                   |
| packets/subscribe/error        | SUBSCRIBEエラーパケットの累積処理数           |
| packets/subscribe/auth_error   | SUBSCRIBE拒否パケットの累積数                 |
| packets/suback/sent            | 累積送信SUBACKパケット数                      |
| packets/unsubscribe/received   | 累積受信UNSUBSCRIBEパケット数                 |
| packets/unsuback/sent          | 累積送信UNSUBACKパケット数                    |
| packets/pingreq/received       | 累積受信PINGREQパケット数                      |
| packets/pingresp/sent          | 累積送信PINGRESPパケット数                     |
| packets/disconnect/received    | 累積受信DISCONNECTパケット数                   |
| packets/disconnect/sent        | 累積送信DISCONNECTパケット数                   |
| packets/auth/received          | 累積受信AUTHパケット数                         |
| packets/auth/sent              | 累積送信AUTHパケット数                         |

### MQTT送受信メッセージ統計

| トピック                         | 説明                                        |
| -------------------------------- | ------------------------------------------- |
| messages/received               | 累積受信メッセージ数                         |
| messages/sent                   | 累積送信メッセージ数                         |
| messages/qos0/received          | QoS 0の累積受信メッセージ数                   |
| messages/qos0/sent              | QoS 0の累積送信メッセージ数                   |
| messages/qos1/received          | QoS 1の累積受信メッセージ数                   |
| messages/qos1/sent              | QoS 1の累積送信メッセージ数                   |
| messages/qos2/received          | QoS 2の累積受信メッセージ数                   |
| messages/qos2/sent              | QoS 2の累積送信メッセージ数                   |
| messages/publish                | 累積PUBLISHメッセージ数                       |
| messages/dropped                | ドロップされたメッセージの総数                 |
| messages/dropped/expired        | ドロップされたメッセージの総数（期限切れ）       |
| messages/dropped/no_subscribers | ドロップされたメッセージの総数（サブスクライバーなし） |
| messages/forward                | ノードによって転送されたメッセージの総数         |
| messages/retained               | 累積保持メッセージ数                           |
| messages/delayed                | 累積遅延メッセージ数                           |
| messages/delivered              | 累積配信済みメッセージ数                       |
| messages/acked                  | 累積アック済みメッセージ数                      |

## アラーム - システムアラーム

システムトピックのプレフィックス：`$SYS/brokers/${node}/alarms/`

| トピック      | 説明               |
| ------------ | ------------------ |
| activate     | 新規発生したアラーム |
| deactivate   | 解消されたアラーム   |

## Sysmon - システムモニタリング

システムトピックのプレフィックス：`$SYS/brokers/${node}/sysmon/`

| トピック          | 説明                                                                                   |
| ---------------- | -------------------------------------------------------------------------------------- |
| long_gc          | ガベージコレクションに時間がかかりすぎている                                         |
| long_schedule    | プロセススケジューリングに時間がかかりすぎており、スケジューラーのタイムスライスを多く消費している |
| large_heap       | プロセスのメモリ使用量が多すぎる                                                     |
| busy_port        | プロセスがビジーポートにメッセージを送信し、プロセスがハングしている                   |
| busy_dist_port   | ノード間通信に使用される分散通信ポートがビジーで、プロセスがハングしている             |
