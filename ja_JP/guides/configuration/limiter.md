# レートリミッターの設定

LimiterはEMQX 5.0で導入された新機能で、クライアントやトピックが指定された時間内にパブリッシュまたはサブスクライブできるメッセージ数を制限する仕組みです。Limiterの詳細や動作については、[Rate Limit](../rate-limit.md)をご参照ください。

## リスナー単位のリミッター

リミッターはリスナー単位で動作させることができます。EMQXでは以下の種類のリミッターを用いてレート制限を指定します。

| 種類           | ダッシュボードUI                                    | 説明                                                         | オーバーロード後の動作                 |
| :------------- | ------------------------------------------------- | :----------------------------------------------------------- | :------------------------------------ |
| bytes_rate     | Max Message Publishing Traffic (Per Client)       | 単一クライアントが1秒あたりにパブリッシュするメッセージのバイト数 | クライアントからのメッセージ受信を一時停止 |
| bytes_burst    | Max Message Publishing Traffic Burst (Per Client) | 通常の`Data Publishing Rate`に基づく、単一クライアントがバーストで送信できるバイト数 | クライアントからのメッセージ受信を一時停止 |
| messages_rate  | Max Message Publishing Rate (Per Client)          | 単一クライアントが1秒あたりにパブリッシュするメッセージ数   | クライアントからのメッセージ受信を一時停止 |
| messages_burst | Max Message Publishing Burst (Per Client)         | 通常の`Messages Publish Rate`に加えて、単一クライアントがバーストで送信できるメッセージ数 | クライアントからのメッセージ受信を一時停止 |
| max_conn_rate  | Max Connection Rate (Listener)                     | 現在のリスナーに対する1秒あたりの接続数                     | 新規接続の受け入れを一時停止           |
| max_conn_rate  | Max Connection Burst (Listener)                    | リスナーがバーストで受け入れ可能な最大接続数                 | 新規接続の受け入れを一時停止           |

例えば、デフォルトのTCPリスナーにリミッターを設定する場合、以下のように設定します。

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

- リスナーの接続確立の最大レートは1秒あたり1000件です。
- リスナーは60分間に最大10,000件の接続を受け入れられます。
- クライアントごとのメッセージパブリッシュの最大レートは1秒あたり1000件です。
- リスナーは60分ごとに短時間で最大10,000件のメッセージのバーストを許容します。
- クライアントごとのデータパブリッシュの最大レートは1秒あたり1MBです。
- リスナーは60分ごとに短時間で最大100MBのデータバーストを許容します。

## ノード単位のリミッター

リミッターはノード単位でも動作し、各EMQXノードへの個々のクライアント接続の速度や、ノードにパブリッシュされるメッセージやデータのレートを制限します。EMQXノードでは以下の種類のリミッターを用いてレート制限を指定します。

| 種類           | ダッシュボードUI             | 説明                                                         | オーバーロード後の動作                                                                                   |
| -------------- | ---------------------------- | ------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------- |
| bytes_rate     | Data Publish Rate            | 単一クライアントが各EMQXノードに送信するデータ量（バイト単位） | 制限に達した場合、QoS 0メッセージは破棄され、QoS 1およびQoS 2メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| bytes_burst    | Data Publish Burst           | 通常の`data publish rate`に基づく、クライアントごとに許可されるデータのバースト量 | 制限に達した場合、QoS 0メッセージは破棄され、QoS 1およびQoS 2メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| messages_rate  | Message Publish Rate         | 単一クライアントが各EMQXノードに送信するメッセージのレート | 制限に達した場合、QoS 0メッセージは破棄され、QoS 1およびQoS 2メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| messages_burst | Message Publish Burst        | 通常の`message publishing rate`に基づく、ノードごとに許可されるメッセージのバースト数 | 制限に達した場合、QoS 0メッセージは破棄され、QoS 1およびQoS 2メッセージは「Quota Exceeded」エラー（0x97）で拒否されます。 |
| max_conn_rate  | Maximum Connection Rate      | ノードごとに受け入れる新規接続のレート                       | 制限に達した場合、Acceptキューでの接続処理が一時停止され、新規接続の遅延または拒否が発生します。             |
| max_conn_burst | Maximum Connection Burst     | ノードがバーストで受け入れ可能な最大接続数                   | 新規接続の受け入れを一時停止します。                                                                       |

例えば、EMQXノードにリミッターを設定するには、`emqx.conf`に以下のように記述します。

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
- ノードは60分ごとに短時間で最大100MBのデータバーストを許容します。

::: tip

EMQXはカスタマイズニーズに応じたより詳細な設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご覧ください。

:::
