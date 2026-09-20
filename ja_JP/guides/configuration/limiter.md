# レートリミッターの設定

リミッターはEMQX 5.0で導入された新機能で、クライアントまたはトピックが指定された時間内にパブリッシュまたはサブスクライブできるメッセージ数を制限する仕組みです。リミッターの詳細や動作については、[Rate Limit](../rate-limit.md)をご参照ください。

## リスナー単位のリミッター

リミッターはリスナー単位で動作させることができます。EMQXでは以下の種類のリミッターを用いてレート制限を指定します。

| 種類           | ダッシュボードUI                                 | 説明                                                         | オーバーロード時の動作               |
| :------------- | ------------------------------------------------ | :------------------------------------------------------------ | :---------------------------------- |
| bytes_rate     | Max Message Publishing Traffic (Per Client)       | 単一クライアントが1秒あたりにパブリッシュするメッセージのバイト数。 | クライアントからのメッセージ受信を一時停止 |
| bytes_burst    | Max Message Publishing Traffic Burst (Per Client) | 通常の`Data Publishing Rate`に基づく、単一クライアントがバーストで送信可能なバイト数。 | クライアントからのメッセージ受信を一時停止 |
| messages_rate  | Max Message Publishing Rate (Per Client)          | 単一クライアントが1秒あたりにパブリッシュするメッセージ数。 | クライアントからのメッセージ受信を一時停止 |
| messages_burst | Max Message Publishing Burst (Per Client)         | 通常の`Messages Publish Rate`に加えて、単一クライアントがバーストで送信可能なメッセージ数。 | クライアントからのメッセージ受信を一時停止 |
| max_conn_rate  | Max Connection Rate (Listener)                    | 現在のリスナーに対する1秒あたりの接続数。                   | 新規接続の受け入れを一時停止         |
| max_conn_burst | Max Connection Burst (Listener)                   | リスナーがバーストで受け入れ可能な最大接続数。               | 新規接続の受け入れを一時停止         |

配信レートリミッターはリスナー単位で動作しますが、オーバーロード時の動作が異なります。詳細は[配信レートリミッター](#delivery-rate-limiters)をご覧ください。

例として、デフォルトのTCPリスナーにリミッターを設定する場合、以下のように設定します。

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
- リスナーは60分間に最大10,000接続を受け入れ可能です。
- クライアントごとのメッセージの最大パブリッシュレートは1秒あたり1000メッセージです。
- リスナーは60分ごとに短時間で最大10,000メッセージのバーストを許容します。
- クライアントごとのデータの最大パブリッシュレートは1秒あたり1MBです。
- リスナーは60分ごとに短時間で最大100MBのバーストを許容します。

## ノード単位のリミッター

リミッターはノード単位でも動作し、各EMQXノードに対する個々のクライアント接続の速度や、ノードにパブリッシュされるメッセージやデータのレートを制限します。EMQXノードでは以下の種類のリミッターを用いてレート制限を指定します。

| 種類           | ダッシュボードUI             | 説明                                                         | オーバーロード時の動作                                                                 |
| -------------- | ---------------------------- | ------------------------------------------------------------ | -------------------------------------------------------------------------------------- |
| bytes_rate     | Data Publish Rate            | 単一クライアントが各EMQXノードにパブリッシュするデータ量（バイト単位）。 | 制限に達すると、QoS 0メッセージは破棄され、QoS 1およびQoS 2メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| bytes_burst    | Data Publish Burst           | 通常の`data publish rate`に基づく、クライアントごとのバースト許容量。 | 制限に達すると、QoS 0メッセージは破棄され、QoS 1およびQoS 2メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| messages_rate  | Message Publish Rate         | 単一クライアントが各EMQXノードにパブリッシュするメッセージのレート。 | 制限に達すると、QoS 0メッセージは破棄され、QoS 1およびQoS 2メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| messages_burst | Message Publish Burst        | 通常の`message publishing rate`に基づく、ノードごとのバースト許容量。 | 制限に達すると、QoS 0メッセージは破棄され、QoS 1およびQoS 2メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| max_conn_rate  | Maximum Connection Rate      | ノードごとに受け入れる新規接続のレート。                     | 制限に達すると、Acceptキューでの接続処理が一時停止され、新規接続の遅延または拒否が発生します。 |
| max_conn_burst | Maximum Connection Burst     | ノードがバーストで受け入れ可能な最大接続数。                 | 新規接続の受け入れを一時停止します。                                                   |

例として、EMQXノードにリミッターを設定する場合、`emqx.conf`に以下のように設定します。

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

ゾーン単位のリミッターは`zone`セクション内に以下のように埋め込むことができます。

```bash
zones.my_zone.mqtt {
  limiter {...}
}
```

- ノードは10秒ごとに最大500メッセージを受信でき、それを超えるメッセージは破棄または拒否されます。
- ノードは60分ごとに短時間で最大10,000メッセージのバーストを許容します。
- ノードは10秒ごとに最大500MBのデータを受信でき、それを超えるデータは破棄または拒否されます。
- ノードは60分ごとに短時間で最大100MBのバーストを許容します。

## 配信レートリミッター

上記のパブリッシュ側リミッターに加え、EMQXはサブスクライブ側の配信レート制限もサポートしています。これらのリミッターは、どのクライアントがメッセージをパブリッシュしたかに関わらず、EMQXがサブスクライブしているクライアントにメッセージを配信する速度を制御します。

| 種類                    | ダッシュボードUI                                | 説明                                                         | オーバーロード時の動作                                                                                   |
| ----------------------- | ----------------------------------------------- | ------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------- |
| delivery_messages_rate  | Max Message Delivery Rate (Per Client)          | ノードごとに単一サブスクライバーに配信されるメッセージの最大レート。 | QoS 0メッセージは破棄されます。QoS 1およびQoS 2メッセージは内部でキューイングされ、リミッター設定に基づく遅延後に再試行されます。 |
| delivery_messages_burst | Max Message Delivery Burst (Per Client)         | `delivery_messages_rate`に加えたバースト許容量。              | 上記と同様です。                                                                                         |
| delivery_bytes_rate     | Max Message Delivery Traffic (Per Client)       | ノードごとに単一サブスクライバーに配信されるデータの最大レート（バイト単位）。 | QoS 0メッセージは破棄されます。QoS 1およびQoS 2メッセージは内部でキューイングされ、リミッター設定に基づく遅延後に再試行されます。 |
| delivery_bytes_burst    | Max Message Delivery Traffic Burst (Per Client) | `delivery_bytes_rate`に加えたバースト許容量。                  | 上記と同様です。                                                                                         |

パブリッシュ側リミッターとは異なり、配信リミッターはチャネル単位でのみ適用され、ゾーンやリスナーグループ間で共有されません。

:::tip
配信レートリミッターはメモリーセッション（`durable_sessions.enable = false`）でのみサポートされます。永続化セッションが有効な場合は効果がありません。
:::

配信レート制限はダッシュボードの **Management** -> **Listeners** ページで各リスナーごとに設定できます。

または設定ファイルで設定することも可能です。例として、デフォルトのTCPリスナーに配信レート制限を設定する場合、`emqx.conf`に以下のように記述します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  delivery_messages_rate = "100/s"
  delivery_messages_burst = "500/10s"
  delivery_bytes_rate = "1MB/s"
  delivery_bytes_burst = "10MB/10s"
}
```

未指定の場合、デフォルト値は`infinity`であり、後方互換性が維持されます。

::: tip

EMQXはより細かいカスタマイズに対応するため、多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::
