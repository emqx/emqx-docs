# システムトピック

EMQXは定期的に稼働状況、メッセージ統計、クライアントのオンライン・オフラインイベントを、`$SYS/`で始まるシステムトピックにパブリッシュします。

`$SYS`トピックのパスは`$SYS/brokers/{node}/`で始まり、`{node}`はイベントやメッセージが発生したノード名を示します。例：

```bash
$SYS/brokers/emqx@127.0.0.1/version
$SYS/brokers/emqx@127.0.0.1/uptime
```

`$SYS`システムメッセージのパブリッシュ間隔は、ダッシュボードの `Management/MQTT Setting/System Topic` で設定します。

::: tip
デフォルトでは、localhost上のMQTTクライアントのみが`$SYS`トピックのサブスクライブを許可されています。
パブリッシュおよびサブスクライブのACLルールを変更するには、[File Authorization](../access-control/authz/file.md) を参照してください。

EMQXの`$SYS`トピックの多くのデータは、より疎結合な他の方法でも取得可能です。
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

これらのイベントはデフォルトで有効です。無効にするには`sys_topics.sys_event_messages`を参照してください。

`$SYS`トピックのプレフィックス：`$SYS/brokers/${node}/clients/`

| トピック                    | 説明                                               |
| --------------------------- | -------------------------------------------------- |
| ${clientid}/connected       | オンラインイベント。クライアントがオンラインになるとパブリッシュされます |
| ${clientid}/disconnected    | オフラインイベント。クライアントがオフラインになるとパブリッシュされます |

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

`disconnected`イベントメッセージのペイロードはJSON形式で解析可能です：

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

これらのイベントはデフォルトで無効です。有効にするには`sys_topics.sys_event_messages`を参照してください。

`$SYS`トピックのプレフィックス：`$SYS/brokers/${node}/clients/`

| トピック                    | 説明                                               |
| --------------------------- | -------------------------------------------------- |
| ${clientid}/subscribed      | サブスクライブイベント。クライアントがトピックをサブスクライブするとパブリッシュされます |
| ${clientid}/unsubscribed    | アンザブスクライブイベント。クライアントがトピックのサブスクライブを解除するとパブリッシュされます |

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

`unsubscribed`イベントメッセージのペイロードはJSON形式で解析可能です：

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

| トピック               | 説明                      |
| ---------------------- | ------------------------- |
| connections/count      | 現在のクライアント総数    |
| connections/max        | 最大クライアント数        |

### サブスクリプション統計

| トピック                      | 説明                                      |
| ----------------------------- | ----------------------------------------- |
| suboptions/count              | 現在のサブスクリプションオプション数     |
| suboptions/max                | 最大サブスクリプションオプション数       |
| subscribers/count             | 現在のサブスクライバー数                   |
| subscribers/max               | 最大サブスクライバー数                     |
| subscriptions/count           | 現在のサブスクリプション総数               |
| subscriptions/max             | 最大サブスクリプション数                   |
| subscriptions/shared/count    | 現在の共有サブスクリプション総数           |
| subscriptions/shared/max      | 最大共有サブスクリプション数               |

### トピック統計

| トピック           | 説明                      |
| ------------------ | ------------------------- |
| topics/count       | 現在のトピック総数        |
| topics/max         | 最大トピック数            |

### ルート統計

| トピック           | 説明                      |
| ------------------ | ------------------------- |
| routes/count       | 現在のルート総数          |
| routes/max         | 最大ルート数              |

`topics/count` と `topics/max` は `routes/count` と `routes/max` と数値的に同じです。

### スループット（バイト／パケット／メッセージ）統計

システムトピックのプレフィックス：`$SYS/brokers/${node}/metrics/`

### 送受信バイト統計

| トピック           | 説明                      |
| ------------------ | ------------------------- |
| bytes/received     | 累積受信バイト数          |
| bytes/sent         | 累積送信バイト数          |

### 送受信MQTTパケット統計

| トピック                      | 説明                                              |
| ----------------------------- | ------------------------------------------------- |
| packets/received             | 累積受信MQTTパケット数                            |
| packets/sent                 | 累積送信MQTTパケット数                            |
| packets/connect/received     | CONNECTパケットの累積受信数                        |
| packets/connack/sent         | CONNACKパケットの累積送信数                        |
| packets/publish/received     | PUBLISHパケットの累積受信数                        |
| packets/publish/sent         | PUBLISHパケットの累積送信数                        |
| packets/publish/error        | PUBLISHエラーパケットの累積処理数                  |
| packets/publish/auth_error   | PUBLISH拒否パケットの累積数                        |
| packets/publish/dropped      | PUBLISHドロップパケットの累積数                    |
| packets/puback/received      | PUBACKパケットの累積受信数                         |
| packets/puback/sent          | PUBACKパケットの累積送信数                         |
| packets/puback/inuse         | PUBACKドロップパケットの累積数                     |
| packets/puback/missed        | PUBACKミスパケットの累積数                         |
| packets/pubrec/received      | PUBRECパケットの累積受信数                         |
| packets/pubrec/sent          | PUBRECパケットの累積送信数                         |
| packets/pubrec/inuse         | PUBRECドロップパケットの累積数                     |
| packets/pubrec/missed        | PUBRECミスパケットの累積数                         |
| packets/pubrel/received      | PUBRELパケットの累積受信数                         |
| packets/pubrel/sent          | PUBRELパケットの累積送信数                         |
| packets/pubrel/missed        | PUBRELミスパケットの累積数                         |
| packets/pubcomp/received     | PUBCOMPパケットの累積受信数                        |
| packets/pubcomp/sent         | PUBCOMPパケットの累積送信数                        |
| packets/pubcomp/inuse        | PUBCOMPドロップパケットの累積数                    |
| packets/pubcomp/missed       | PUBCOMPミスパケットの累積数                        |
| packets/subscribe/received   | SUBSCRIBEパケットの累積受信数                      |
| packets/subscribe/error      | SUBSCRIBEエラーパケットの累積処理数                |
| packets/subscribe/auth_error | SUBSCRIBE拒否パケットの累積数                      |
| packets/suback/sent          | SUBACKパケットの累積送信数                         |
| packets/unsubscribe/received | UNSUBSCRIBEパケットの累積受信数                    |
| packets/unsuback/sent        | UNSUBACKパケットの累積送信数                       |
| packets/pingreq/received     | PINGREQパケットの累積受信数                        |
| packets/pingresp/sent        | PINGRESPパケットの累積送信数                       |
| packets/disconnect/received  | DISCONNECTパケットの累積受信数                     |
| packets/disconnect/sent      | DISCONNECTパケットの累積送信数                      |
| packets/auth/received        | AUTHパケットの累積受信数                           |
| packets/auth/sent            | AUTHパケットの累積送信数                           |

### MQTT送受信メッセージ統計

| トピック                      | 説明                                              |
| ----------------------------- | ------------------------------------------------- |
| messages/received             | 累積受信メッセージ数                              |
| messages/sent                 | 累積送信メッセージ数                              |
| messages/qos0/received        | QoS 0の累積受信メッセージ数                        |
| messages/qos0/sent            | QoS 0の累積送信メッセージ数                        |
| messages/qos1/received        | QoS 1の累積受信メッセージ数                        |
| messages/qos1/sent            | QoS 1の累積送信メッセージ数                        |
| messages/qos2/received        | QoS 2の累積受信メッセージ数                        |
| messages/qos2/sent            | QoS 2の累積送信メッセージ数                        |
| messages/publish              | 累積PUBLISHメッセージ数                            |
| messages/dropped              | ドロップされたメッセージの総数                      |
| messages/dropped/expired      | ドロップされたメッセージの総数（期限切れ）          |
| messages/dropped/no_subscribers | ドロップされたメッセージの総数（サブスクライバーなし） |
| messages/forward              | ノードによって転送されたメッセージの総数            |
| messages/retained             | 累積保持メッセージ数                               |
| messages/delayed              | 累積遅延メッセージ数                               |
| messages/delivered            | 累積配信済みメッセージ数                           |
| messages/acked                | 累積アック済みメッセージ数                         |

## アラーム - システムアラーム

システムトピックのプレフィックス：`$SYS/brokers/${node}/alarms/`

| トピック      | 説明               |
| ------------- | ------------------ |
| activate      | 新規発生したアラーム |
| deactivate    | 解消されたアラーム   |

## Sysmon - システムモニタリング

システムトピックのプレフィックス：`$SYS/brokers/${node}/sysmon/`

| トピック          | 説明                                                                 |
| ----------------- | -------------------------------------------------------------------- |
| long_gc           | ガベージコレクションに時間がかかりすぎている                         |
| long_schedule     | プロセススケジューリングに時間がかかりすぎており、スケジューラーのタイムスライスを多く消費している |
| large_heap        | プロセスのメモリ使用量が多すぎる                                    |
| busy_port         | プロセスがビジーポートにメッセージを送信し、プロセスがハングしている  |
| busy_dist_port    | ノード間通信に使われる分散通信ポートがビジーで、プロセスがハングしている |
