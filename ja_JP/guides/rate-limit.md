# Rate Limit

EMQX は、接続速度およびメッセージ速度に制限を指定できる機能を提供しており、システムの入口での過負荷を回避するバックプレッシャー方式を採用し、予測可能なスループットでシステムの安定性を保証します。

## Listener-Level Limiters

リミッターはリスナー単位で動作させることが可能です。EMQX では以下の種類のリミッターを使用してレート制限を指定します。

| 種類           | ダッシュボードUI                                 | 説明                                                         | 過負荷時の動作                   |
| :------------- | ------------------------------------------------ | :------------------------------------------------------------ | :------------------------------ |
| bytes_rate     | Max Message Publishing Traffic (Per Client)       | 単一クライアントが1秒あたりにパブリッシュするメッセージのバイト数 | クライアントからのメッセージ受信を一時停止 |
| bytes_burst    | Max Message Publishing Traffic Burst (Per Client) | 通常の `Data Publishing Rate` に基づく単一クライアントのバースト送信可能バイト数 | クライアントからのメッセージ受信を一時停止 |
| messages_rate  | Max Message Publishing Rate (Per Client)          | 単一クライアントが1秒あたりにパブリッシュするメッセージ数       | クライアントからのメッセージ受信を一時停止 |
| messages_burst | Max Message Publishing Burst (Per Client)         | 通常の `Messages Publish Rate` に加えて単一クライアントがバースト送信可能なメッセージ数 | クライアントからのメッセージ受信を一時停止 |
| max_conn_rate  | Max Connection Rate (Listener)                    | 現在のリスナーに対する1秒あたりの接続数                       | 新規接続の受け入れを一時停止   |
| max_conn_burst | Max Connection Burst (Listener)                   | リスナーがバーストで受け入れ可能な最大接続数                   | 新規接続の受け入れを一時停止   |

### Listener-Level Limiters の設定

ダッシュボードの **Management** -> **Listeners** ページで各リスナーのレート制限を設定できます。

または、設定ファイルを通じて設定することも可能です。例えば、デフォルトの TCP リスナーにリミッターを設定する場合、`emqx.conf` ファイルに以下のように記述します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_conn_rate = "1000/s"
  max_conn_burst = "10000/60m"
  messages_rate = "1000/s"
  messages_burst = "10000/60m"
  bytes_rate = "1MB/s"
  bytes_burst = "100MB/60m"
}
```

この設定は以下を意味します：

- リスナーでの接続確立の最大レートは1秒あたり1000件。
- リスナーは60分間に最大10,000件の接続を受け入れ可能。
- クライアントごとのメッセージの最大パブリッシュレートは1秒あたり1000件。
- リスナーは60分間に最大10,000件のメッセージをバーストで許容。
- クライアントごとのデータの最大パブリッシュレートは1秒あたり1MB。
- リスナーは60分間に最大100MBのデータをバーストで許容。

## Node-Level Limiters

リミッターはノード単位でも動作し、各 EMQX ノードへの個々のクライアント接続の速度や、ノードへのメッセージ・データのパブリッシュ速度を制限します。EMQX ノードでは以下の種類のリミッターを使用してレート制限を指定します。

| 種類           | ダッシュボードUI             | 説明                                                         | 過負荷時の動作                                               |
| -------------- | ---------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| bytes_rate     | Data Publish Rate            | 単一クライアントが各 EMQX ノードに送信するデータ量（バイト単位） | 制限に達すると、EMQX は QoS 0 メッセージを破棄し、QoS 1 および QoS 2 メッセージを「Quota Exceeded」エラー（0x97）で拒否します。 |
| bytes_burst    | Data Publish Burst           | 通常の `data publish rate` に基づくクライアントごとのバースト許容量 | 制限に達すると、EMQX は QoS 0 メッセージを破棄し、QoS 1 および QoS 2 メッセージを「Quota Exceeded」エラー（0x97）で拒否します。 |
| messages_rate  | Message Publish Rate         | 単一クライアントが各 EMQX ノードに送信するメッセージのレート   | 制限に達すると、EMQX は QoS 0 メッセージを破棄し、QoS 1 および QoS 2 メッセージを「Quota Exceeded」エラー（0x97）で拒否します。 |
| messages_burst | Message Publish Burst        | 通常の `message publishing rate` に基づくノードごとのバースト許容量 | 制限に達すると、EMQX は QoS 0 メッセージを破棄し、QoS 1 および QoS 2 メッセージを「Quota Exceeded」エラー（0x97）で拒否します。 |
| max_conn_rate  | Maximum Connection Rate      | ノードごとに受け入れる新規接続のレート                       | 制限に達すると、EMQX は Accept キューでの接続処理を一時停止し、新規接続の遅延または拒否を行います。 |
| max_conn_burst | Maximum Connection Burst     | ノードがバーストで受け入れ可能な最大接続数                   | 新規接続の受け入れを一時停止                                 |

### Node-Level Limiters の設定

ダッシュボードの **Management** -> **MQTT Configuration** ページで各ノードのレート制限を設定できます。

または、設定ファイルを通じて設定することも可能です。例えば、`emqx.conf` に以下のように記述します。

```bash
mqtt.limiter {
  max_conn_rate = "1000/s"
  max_conn_burst = "10000/60m"
  messages_rate = "500/10s"
  messages_burst = "10000/60m"
  bytes_rate = "500KB/s"
  bytes_burst = "100MB/60m"
}
```

ゾーン単位のリミッターは `zone` セクション内に以下のように埋め込むことができます。

```bash
zones.my_zone.mqtt {
  limiter {...}
}
```

- ノードは10秒ごとに最大500件のメッセージを受信でき、それを超えると破棄または拒否されます。
- ノードは60分間に最大10,000件のメッセージをバーストで許容します。
- ノードは10秒ごとに最大500MBのデータを受信でき、それを超えると破棄または拒否されます。
- ノードは60分間に最大100MBのデータをバーストで許容します。

## Rate Unit

### 時間単位

レート値でサポートされる時間単位は以下の通りです。

- **s** : 秒
- **m** : 分
- **h** : 時間
- **d** : 日

時間単位は間隔値としても指定可能で、例えば `1000/10s` は「10秒ごとに1000件の制限」を意味します。

### サイズ単位

レート値でサポートされるサイズ単位は以下の通りです。

- **KB** : キロバイト
- **MB** : メガバイト
- **GB** : ギガバイト
