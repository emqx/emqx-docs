# システムトピック

EMQXは定期的に稼働状況、メッセージ統計、クライアントのオンライン・オフラインイベントを`$SYS/`で始まるシステムトピックにパブリッシュします。

`$SYS`トピックのパスは`$SYS/brokers/{node}/`で始まり、`{node}`はイベントやメッセージが生成されたノード名を示します。例：

```bash
$SYS/brokers/emqx@127.0.0.1/version
$SYS/brokers/emqx@127.0.0.1/uptime
```

`$SYS`システムメッセージのパブリッシュ間隔はダッシュボードの`Management/MQTT Setting/System Topic`で設定します。

::: tip
デフォルトでは、ローカルホスト上のMQTTクライアントのみが`$SYS`トピックのサブスクライブを許可されています。  
パブリッシュおよびサブスクライブのACLルールを変更するには、[ファイル認可](../access-control/authz/file.md)を参照してください。

EMQXの`$SYS`トピックの多くのデータは、より低い結合度で他の方法から取得可能です。  
デバイスのオンライン・オフライン状態はルールエンジンでキャプチャおよび処理できます。  
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

| トピック                    | 説明                                                         |
| --------------------------- | ------------------------------------------------------------ |
| ${clientid}/connected       | オンラインイベント。クライアントがオンラインになるとパブリッシュされるメッセージ |
| ${clientid}/disconnected    | オフラインイベント。クライアントがオフラインになるとパブリッシュされるメッセージ |

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

## クライアントのサブスクライブ・アンインストールイベント

これらのイベントはデフォルトで無効です。有効にするには`sys_topics.sys_event_messages`を参照してください。

`$SYS`トピックのプレフィックス：`$SYS/brokers/${node}/clients/`

| トピック                    | 説明                                                     |
| --------------------------- | -------------------------------------------------------- |
| ${clientid}/subscribed      | サブスクライブイベント。クライアントがトピックをサブスクライブするとパブリッシュされるメッセージ |
| ${clientid}/unsubscribed    | アンインストールイベント。クライアントがトピックのサブスクライブを解除するとパブリッシュされるメッセージ |

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

| トピック               | 説明                           |
| ---------------------- | ------------------------------ |
| connections/count      | 現在のクライアント総数          |
| connections/max        | 最大クライアント数              |

### サブスクリプション統計

| トピック                     | 説明                                         |
| ---------------------------- | -------------------------------------------- |
| suboptions/count             | 現在のサブスクリプションオプション数          |
| suboptions/max               | 最大サブスクリプションオプション数            |
| subscribers/count            | 現在のサブスクライバー数                      |
| subscribers/max              | 最大サブスクライバー数                        |
| subscriptions/count          | 現在のサブスクリプション総数                  |
| subscriptions/max            | 最大サブスクリプション数                      |
| subscriptions/shared/count   | 現在の共有サブスクリプション総数              |
| subscriptions/shared/max     | 最大共有サブスクリプション数                  |

### トピック統計

| トピック           | 説明                       |
| ------------------ | -------------------------- |
| topics/count       | 現在のトピック総数          |
| topics/max         | 最大トピック数              |

### ルート統計

| トピック           | 説明                       |
| ------------------ | -------------------------- |
| routes/count       | 現在のルート総数            |
| routes/max         | 最大ルート数                |

`topics/count`と`topics/max`は`routes/count`と`routes/max`と数値的に同じです。

### スループット（バイト／パケット／メッセージ）統計

システムトピックのプレフィックス：`$SYS/brokers/${node}/metrics/`

### 送受信バイト統計

| トピック           | 説明                     |
| ------------------ | ------------------------ |
| bytes/received     | 累積受信バイト数          |
| bytes/sent         | 累積送信バイト数          |

### 送受信MQTTパケット統計

| トピック                      | 説明                                             |
| ----------------------------- | ------------------------------------------------ |
| packets/received              | 累積受信MQTTパケット数                            |
| packets/sent                  | 累積送信MQTTパケット数                            |
| packets/connect/received      | 累積受信CONNECTパケット数                         |
| packets/connack/sent          | 累積送信CONNACKパケット数                         |
| packets/publish/received      | 累積受信PUBLISHパケット数                         |
| packets/publish/sent          | 累積送信PUBLISHパケット数                         |
| packets/publish/error         | 累積処理中のPUBLISHエラーパケット数               |
| packets/publish/auth_error    | 累積拒否されたPUBLISHパケット数                   |
| packets/publish/dropped       | 累積ドロップされたPUBLISHパケット数               |
| packets/puback/received       | 累積受信PUBACKパケット数                          |
| packets/puback/sent           | 累積送信PUBACKパケット数                          |
| packets/puback/inuse          | 累積ドロップされたPUBACKパケット数                |
| packets/puback/missed         | 累積見逃されたPUBACKパケット数                    |
| packets/pubrec/received       | 累積受信PUBRECパケット数                          |
| packets/pubrec/sent           | 累積送信PUBRECパケット数                          |
| packets/pubrec/inuse          | 累積ドロップされたPUBRECパケット数                |
| packets/pubrec/missed         | 累積見逃されたPUBRECパケット数                    |
| packets/pubrel/received       | 累積受信PUBRELパケット数                          |
| packets/pubrel/sent           | 累積送信PUBRELパケット数                          |
| packets/pubrel/missed         | 累積見逃されたPUBRELパケット数                    |
| packets/pubcomp/received      | 累積受信PUBCOMPパケット数                         |
| packets/pubcomp/sent          | 累積送信PUBCOMPパケット数                         |
| packets/pubcomp/inuse         | 累積ドロップされたPUBCOMPパケット数               |
| packets/pubcomp/missed        | 累積見逃されたPUBCOMPパケット数                   |
| packets/subscribe/received    | 累積受信SUBSCRIBEパケット数                       |
| packets/subscribe/error       | 累積処理中のSUBSCRIBEエラーパケット数             |
| packets/subscribe/auth_error  | 累積拒否されたSUBSCRIBEパケット数                 |
| packets/suback/sent           | 累積送信SUBACKパケット数                          |
| packets/unsubscribe/received  | 累積受信UNSUBSCRIBEパケット数                     |
| packets/unsuback/sent         | 累積送信UNSUBACKパケット数                        |
| packets/pingreq/received      | 累積受信PINGREQパケット数                         |
| packets/pingresp/sent         | 累積送信PINGRESPパケット数                        |
| packets/disconnect/received   | 累積受信DISCONNECTパケット数                      |
| packets/disconnect/sent       | 累積送信DISCONNECTパケット数                      |
| packets/auth/received         | 累積受信AUTHパケット数                            |
| packets/auth/sent             | 累積送信AUTHパケット数                            |

### MQTT送受信メッセージ統計

| トピック                      | 説明                                             |
| ----------------------------- | ------------------------------------------------ |
| messages/received            | 累積受信メッセージ数                              |
| messages/sent                | 累積送信メッセージ数                              |
| messages/qos0/received       | 累積受信QoS 0メッセージ数                         |
| messages/qos0/sent           | 累積送信QoS 0メッセージ数                         |
| messages/qos1/received       | 累積受信QoS 1メッセージ数                         |
| messages/qos1/sent           | 累積送信QoS 1メッセージ数                         |
| messages/qos2/received       | 累積受信QoS 2メッセージ数                         |
| messages/qos2/sent           | 累積送信QoS 2メッセージ数                         |
| messages/publish             | 累積PUBLISHメッセージ数                           |
| messages/dropped             | ドロップされたメッセージ総数                       |
| messages/dropped/expired     | ドロップされたメッセージ総数（期限切れ）           |
| messages/dropped/no_subscribers | ドロップされたメッセージ総数（サブスクライバーなし） |
| messages/forward             | ノードによって転送されたメッセージ総数             |
| messages/retained            | 累積保持メッセージ数                              |
| messages/delayed             | 累積遅延メッセージ数                              |
| messages/delivered           | 累積配信済みメッセージ数                          |
| messages/acked               | 累積アック済みメッセージ数                        |

## アラーム - システムアラーム

システムトピックのプレフィックス：`$SYS/brokers/${node}/alarms/`

| トピック      | 説明                 |
| ------------- | -------------------- |
| activate      | 新規発生したアラーム |
| deactivate    | 解消されたアラーム   |

## Sysmon - システムモニタリング

システムトピックのプレフィックス：`$SYS/brokers/${node}/sysmon/`

| トピック          | 説明                                                                                  |
| ----------------- | ------------------------------------------------------------------------------------- |
| long_gc           | ガベージコレクションに時間がかかりすぎている                                         |
| long_schedule     | プロセススケジューリングに時間がかかりすぎており、スケジューラーのタイムスライスを多く消費している |
| large_heap        | プロセスのメモリ使用量が非常に多い                                                   |
| busy_port         | プロセスがビジーポートにメッセージを送信し、プロセスがハングしている                   |
| busy_dist_port    | ノード間通信に使用される分散通信ポートがビジーで、プロセスがハングしている             |
