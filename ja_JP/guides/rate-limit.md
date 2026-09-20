# Rate Limit

EMQX は、接続速度およびメッセージ速度に制限を指定できる機能を提供しており、システムの入口での過負荷を回避するバックプレッシャースキームを用いて、予測可能なスループットでシステムの安定性を保証します。

## Listener-Level Limiters

リミッターはリスナー単位で動作させることが可能です。EMQX は以下の種類のリミッターを使用してレート制限を指定します。

| 種類                         | ダッシュボード UI                                      | 説明                                                        | 過負荷時の挙動                   |
| :--------------------------- | ------------------------------------------------------ | :----------------------------------------------------------- | :------------------------------ |
| bytes_rate                   | Max Message Publishing Traffic (Per Client)            | 単一クライアントが1秒あたりにパブリッシュするメッセージのバイト数。 | クライアントからのメッセージ受信を一時停止 |
| bytes_burst                  | Max Message Publishing Traffic Burst (Per Client)      | 通常の `Data Publishing Rate` に基づく、単一クライアントがバーストで送信可能なバイト数。 | クライアントからのメッセージ受信を一時停止 |
| messages_rate                | Max Message Publishing Rate (Per Client)               | 単一クライアントが1秒あたりにパブリッシュするメッセージ数。 | クライアントからのメッセージ受信を一時停止 |
| messages_burst               | Max Message Publishing Burst (Per Client)              | 通常の `Messages Publish Rate` に加えて、単一クライアントがバーストで送信可能なメッセージ数。 | クライアントからのメッセージ受信を一時停止 |
| max_conn_rate                | Max Connection Rate (Listener)                         | 現在のリスナーにおける1秒あたりの接続数。                   | 新規接続の受け入れを一時停止   |
| max_conn_burst               | Max Connection Burst (Listener)                        | リスナーがバーストで受け入れ可能な最大接続数。               | 新規接続の受け入れを一時停止   |

配信レートリミッターは同じくリスナー単位で動作しますが、過負荷時の挙動が異なります。詳細は[Delivery Rate Limiters](#delivery-rate-limiters)をご覧ください。

### Listener-Level Limiters の設定

ダッシュボードの **Management** -> **Listeners** ページで各リスナーのレート制限を設定できます。

または、設定ファイルを通じて設定可能です。例えば、デフォルトの TCP リスナーに対してリミッターを設定する場合、`emqx.conf` ファイルに以下のように記述します。

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

この設定は以下を意味します。

- リスナーの接続確立の最大レートは1秒あたり1000接続です。
- リスナーは60分間に最大10,000接続をバーストで受け入れ可能です。
- クライアントごとのメッセージパブリッシュの最大レートは1秒あたり1000メッセージです。
- リスナーは60分間に最大10,000メッセージのバーストを許容します。
- クライアントごとのデータパブリッシュの最大レートは1秒あたり1MBです。
- リスナーは60分間に最大100MBのバーストを許容します。

## Node-Level Limiters

リミッターはノード単位でも動作し、各 EMQX ノードに対する個々のクライアント接続の速度や、ノードにパブリッシュされるメッセージやデータのレートを制限します。EMQX ノードは以下の種類のリミッターを使用してレート制限を指定します。

| 種類           | ダッシュボード UI             | 説明                                                        | 過負荷時の挙動                                               |
| -------------- | ---------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| bytes_rate     | Data Publish Rate            | 単一クライアントが各 EMQX ノードにパブリッシュするデータ量（バイト単位）。 | 制限に達すると、QoS 0 メッセージは破棄され、QoS 1 および QoS 2 メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| bytes_burst    | Data Publish Burst           | 通常の `data publish rate` に基づく、クライアントごとのバースト許容量。 | 制限に達すると、QoS 0 メッセージは破棄され、QoS 1 および QoS 2 メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| messages_rate  | Message Publish Rate         | 単一クライアントが各 EMQX ノードにパブリッシュするメッセージのレート。 | 制限に達すると、QoS 0 メッセージは破棄され、QoS 1 および QoS 2 メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| messages_burst | Message Publish Burst        | 通常の `message publishing rate` に基づく、ノードごとのバースト許容量。 | 制限に達すると、QoS 0 メッセージは破棄され、QoS 1 および QoS 2 メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| max_conn_rate  | Maximum Connection Rate      | ノードごとに受け入れる新規接続のレート。                     | 制限に達すると、Accept キューでの接続処理が一時停止され、新規接続が遅延または拒否されます。 |
| max_conn_burst | Maximum Connection Burst     | ノードがバーストで受け入れ可能な最大接続数。                 | 新規接続の受け入れを一時停止                               |

### Node-Level Limiters の設定

ダッシュボードの **Management** -> **MQTT Configuration** ページで各ノードのレート制限を設定できます。

または、設定ファイルで設定可能です。例えば、`emqx.conf` に以下のように記述します。

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

ゾーン単位のリミッターは `zone` セクション内に以下のように埋め込めます。

```bash
zones.my_zone.mqtt {
  limiter {...}
}
```

- ノードは10秒あたり最大500メッセージを受信可能で、それを超えるメッセージは破棄または拒否されます。
- ノードは60分間に最大10,000メッセージのバーストを許容します。
- ノードは10秒あたり最大500KBのデータを受信可能で、それを超えるデータは破棄または拒否されます。
- ノードは60分間に最大100MBのバーストを許容します。

## Delivery Rate Limiters

上記のパブリッシュ側リミッターに加え、EMQX はサブスクライブ側の配信レート制限もサポートしています。これらのリミッターは、どのクライアントがパブリッシュしたかに関わらず、EMQX がサブスクライブクライアントにメッセージを配信する速度を制御します。

| 種類                    | ダッシュボード UI                              | 説明                                                        | 過負荷時の挙動                                               |
| ----------------------- | --------------------------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| delivery_messages_rate  | Max Message Delivery Rate (Per Client)        | ノードごとに単一サブスクライバーに配信されるメッセージの最大レート。 | QoS 0 メッセージは破棄されます。QoS 1 および QoS 2 メッセージは内部キューに入り、リミッター設定に基づく遅延後に再試行されます。 |
| delivery_messages_burst | Max Message Delivery Burst (Per Client)       | `delivery_messages_rate` に加えたバースト許容量。           | 上記と同様。                                               |
| delivery_bytes_rate     | Max Message Delivery Traffic (Per Client)     | ノードごとに単一サブスクライバーに配信されるデータの最大レート（バイト単位）。 | QoS 0 メッセージは破棄されます。QoS 1 および QoS 2 メッセージは内部キューに入り、リミッター設定に基づく遅延後に再試行されます。 |
| delivery_bytes_burst    | Max Message Delivery Traffic Burst (Per Client) | `delivery_bytes_rate` に加えたバースト許容量。               | 上記と同様。                                               |

パブリッシュ側リミッターとは異なり、配信リミッターはチャネル単位のみで適用されます。クライアント接続ごとに適用され、ゾーンやリスナーグループ間で共有されません。

::: tip
配信レートリミッターはメモリーセッション（`durable_sessions.enable = false`）でのみサポートされています。永続セッションが有効な場合は効果がありません。
:::

### Delivery Rate Limiters の設定

ダッシュボードの **Management** -> **Listeners** ページで各リスナーの配信レート制限を設定できます。

または、設定ファイルで設定可能です。例えば、デフォルトの TCP リスナーに対して配信レートリミッターを設定する場合、`emqx.conf` に以下のように記述します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  delivery_messages_rate = "100/s"
  delivery_messages_burst = "500/10s"
  delivery_bytes_rate = "1MB/s"
  delivery_bytes_burst = "10MB/10s"
}
```

この設定は以下を意味します。

- 各サブスクライバーは EMQX から1秒あたり最大100メッセージを受信します。超過した QoS 0 メッセージは破棄され、QoS 1/2 メッセージはキューに入り再試行されます。
- 各サブスクライバーは1秒あたり最大1MBのメッセージデータを受信します。超過時の挙動は上記と同様です。

未指定の場合、デフォルト値は `infinity` であり、配信側のレート制限を必要としない既存のデプロイとの互換性を維持します。

## Rate Unit

### 時間単位

レート値でサポートされる時間単位は以下の通りです。

- **s** : 秒
- **m** : 分
- **h** : 時間
- **d** : 日

時間単位はインターバル値としても指定可能で、例えば `1000/10s` は「10秒ごとに1000回の制限を設定する」ことを意味します。

### サイズ単位

レート値でサポートされるサイズ単位は以下の通りです。

- **KB** : キロバイト
- **MB** : メガバイト
- **GB** : ギガバイト
