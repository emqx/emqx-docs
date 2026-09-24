# MQTT 設定

[MQTT](https://mqtt.org/) は、モノのインターネット（IoT）向けの標準的なメッセージングプロトコルです。非常に軽量なパブリッシュ／サブスクライブ型のメッセージングトランスポートとして設計されており、リモートデバイスを小さなコードフットプリントと最小限のネットワーク帯域幅で接続するのに最適です。

EMQX は 100% MQTT 5.0 および 3.x に準拠しています。本セクションでは、基本的な MQTT 設定項目について紹介し、基本的な MQTT 設定、サブスクリプション設定、セッション設定、強制シャットダウン設定、および強制ガベージコレクション設定などのトピックを扱います。

## 基本的な MQTT 設定

本セクションでは、パケットサイズ、クライアントIDの長さ、トピックレベル数、QoS（サービス品質）、トピックエイリアス、保持設定など、MQTT プロトコルの動作を決定する設定項目を紹介します。

:::tip

EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも対応する設定項目を確認できます。ダッシュボードで設定を行うと、設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定がある場合、ダッシュボードからの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

:::

**設定例:**

```bash
mqtt {
  max_packet_size = 1MB
  max_connect_packet_size = 1MB
  max_clientid_len = 65535
  max_topic_levels = 128
  max_qos_allowed = 2
  max_topic_alias = 65535
  retain_available = true
}  
```

各項目の説明は以下の通りです。

| **設定項目**               | ダッシュボード UI           | **説明**                                                    | **デフォルト値** | **設定可能値**          |
| ------------------------- | --------------------------- | ----------------------------------------------------------- | ---------------- | ----------------------- |
| `max_packet_size`         | Max Packet Size             | MQTT パケットは MQTT クライアントと EMQX 間でメッセージを送るために使用されます。<br /><br />許可される最大 MQTT パケットサイズを設定します。 | `1MB`            |                         |
| `max_connect_packet_size` | Max CONNECT Packet Size     | CONNECT パケットの最大サイズを設定します。CONNECT パケットは `max_connect_packet_size` と `max_packet_size` の両方の制限内である必要があります。どちらかの制限を超えた場合、EMQX は接続を切断します。 | `1MB`            |                         |
| `max_clientid_len`        | Max Client ID Length        | MQTT クライアントIDの最大長を設定します。<br /><br />過度に長いクライアントIDの使用を防止できます。 | `65535`          | `23` - `65535`          |
| `max_topic_levels`        | Max Topic Levels            | MQTT トピックはメッセージの整理・分類に使用されます。<br /><br />トピックに許可される最大レベル数を設定します。 | `128`            | `1` - `35`              |
| `max_qos_allowed`         | Max QoS                    | メッセージの信頼性と配信保証のレベルを決定する QoS レベルの最大値を設定します。 |                  |                         |
| `max_topic_alias`         | Max Topic Alias             | トピックエイリアスは、完全なトピック名の代わりに短いエイリアスを使用して MQTT パケットのサイズを削減する方法です。<br /><br />MQTT セッションで使用可能なトピックエイリアスの最大数を設定します。 | `65535`          | `1` - `65535`           |
| `retain_available`        | Retain Available            | 保持メッセージは、トピックに最後にパブリッシュされたメッセージを保存し、新しいサブスクライバーが最新のメッセージを受け取れるようにします。<br /><br />MQTT の保持メッセージ機能を有効にするかどうかを設定します。 | `true`           | `true`, `false`         |

## サブスクリプション設定

EMQX におけるサブスクリプションとは、クライアントが EMQX のトピックにサブスクライブするプロセスを指します。クライアントがトピックにサブスクライブすると、そのトピックにパブリッシュされたメッセージを受信したいことを示します。

本セクションでは、共有サブスクリプション、ワイルドカードサブスクリプション、排他サブスクリプションの設定方法を紹介します。

:::tip

EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも対応する設定項目を確認できます。ダッシュボードで設定を行うと、設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定がある場合、ダッシュボードからの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

:::

**設定例:**

```bash
mqtt {
	wildcard_subscription = true
  exclusive_subscription = false
  shared_subscription = true
  shared_subscription_strategy  =  round_robin
}
```

各項目の説明は以下の通りです。

| **設定項目**                  | ダッシュボード UI                | **説明**                                                    | **デフォルト値** | **設定可能値**                                              |
| ---------------------------- | ------------------------------- | ----------------------------------------------------------- | ---------------- | ------------------------------------------------------------ |
| `wildcard_subscription`      | Wildcard Subscription Available | ワイルドカードサブスクリプションは、`+` や `#` といったワイルドカードを使い、複数のトピックに対して単一のサブスクリプションでサブスクライブ可能にします。<br /><br />ワイルドカードサブスクリプションを有効にするかどうかを設定します。 | `true`           | `true`, `false`                                              |
| `exclusive_subscription`     | Exclusive Subscription          | 排他サブスクリプションは、1つのトピックに対し同時に1つの MQTT クライアントのみがサブスクライブ可能にします。<br /><br />排他サブスクリプションを有効にするかどうかを設定します。 | `true`           | `true`, `false`                                              |
| `shared_subscription`        | Shared Subscription Available   | 共有サブスクリプションは、複数の MQTT クライアントがトピックのサブスクリプションを共有可能にします。<br /><br />共有サブスクリプションを有効にするかどうかを設定します。 | `true`           | `true`, `false`                                              |
| `shared_subscription_strategy` |                               | 共有サブスクリプションを有効にした場合に、メッセージを共有する MQTT クライアント間でどのように配信するかの戦略を設定します。 | `round_robin`    | - `random`（ランダムにサブスクライバーへ配信）<br /><br />- `round_robin`（ラウンドロビン方式で順番に配信）<br /><br />- `sticky`（最後に選択されたサブスクライバーへ常に配信、切断されるまで継続）<br /><br />- `hash`（`clientIds` のハッシュでサブスクライバーを選択） |

## 遅延パブリッシュ設定

遅延パブリッシュ機能は、クライアントがメッセージのパブリッシュを指定した時間だけ遅延させることを可能にします。この機能は、特定の時間にメッセージをパブリッシュしたい場合や、特定の条件が満たされたときにメッセージを送信したい場合に有用です。

本セクションでは、遅延パブリッシュの有効化方法と、許可される遅延メッセージの最大数の設定方法を紹介します。

**設定例:**

```bash
delay {
  delayed_publish_enabled = true
  max_delayed_messages = 0
}
```

各項目の説明は以下の通りです。

- `delayed_publish_enabled` は EMQX の遅延パブリッシュ機能を有効にするかどうかを設定します。デフォルト値は `true`、設定可能値は `true` または `false` です。
- `max_delayed_messages` は許可される遅延メッセージの最大数を設定します。デフォルト値は `0` です。

## キープアライブ設定

キープアライブは 2 バイトの整数で、秒単位の時間間隔を示します。これは、MQTT クライアントと EMQX 間の接続がデータ送信がなくてもアクティブな状態を維持するための仕組みです。MQTT クライアントが EMQX に接続を確立する際、CONNECT パケットの可変ヘッダーのキープアライブ変数にゼロ以外の値を設定することで、両者間のキープアライブ機構を有効にできます。キープアライブの動作詳細については、[MQTT キープアライブパラメータとは？](https://www.emqx.com/en/blog/mqtt-keep-alive) をご参照ください。

MQTT 5.0 プロトコルによると、キープアライブが有効なクライアントに対して、サーバーがクライアントからキープアライブ時間の1.5倍以内に MQTT コントロールパケットを受信しなかった場合、ネットワーク接続を切断しなければなりません。  
そのため、EMQX ではクライアントのキープアライブタイムアウト状態を定期的にチェックするための設定 `keepalive_multiplier` を導入しています。デフォルト値は `1.5` です。

```bash
keepalive_multiplier = 1.5
```

タイムアウト計算式は以下の通りです。  
$$
\text{Keep Alive} \times \text{keepalive\_multiplier}
$$

## セッション設定

MQTT におけるセッションとは、クライアントとブローカー間の接続を指します。EMQX では、クライアントが接続するとセッションが確立され、トピックのサブスクライブやメッセージの受信、EMQX へのメッセージパブリッシュが可能になります。

本セクションでは、セッションの設定方法を紹介します。

**設定例:**

```bash
mqtt {
    max_subscriptions = infinity
    upgrade_qos = false
    max_inflight = 32
    retry_interval = 30s
    max_awaiting_rel = 100
    await_rel_timeout = 300s
    session_expiry_interval = 2h
    max_mqueue_len = 1000
    mqueue_priorities = disabled
    mqueue_default_priority = lowest
    mqueue_store_qos0 = true
    force_shutdown {
      max_mailbox_size = 1000
      max_heap_size = 32MB
    }
    force_gc {
      count  =  16000
      bytes  =  16MB
    }
  }
```

各項目の説明は以下の通りです。

| **設定項目**                      | ダッシュボード UI           | **説明**                                                    | **デフォルト値**                                            | **設定可能値**                 |
| -------------------------------- | --------------------------- | ----------------------------------------------------------- | ----------------------------------------------------------- | ----------------------------- |
| `max_subscriptions`               | Max Subscriptions           | クライアントが持つことが許可される最大サブスクリプション数を設定します。 | `infinity`                                                  | `1` - `infinity`              |
| `upgrade_qos`                    | Upgrade QoS                 | メッセージがパブリッシュされた後に、クライアントが QoS（サービス品質）レベルをアップグレードできるかどうかを設定します。 | `false`（無効）                                             | `true`, `false`               |
| `max_inflight`                   | Max Inflight                | QoS 1 および QoS 2 メッセージのうち、送信済みだがまだアック（ACK）を受け取っていないメッセージの最大数を設定します。 | `32`                                                        | `1` - `65535`                 |
| `retry_interval`                 | Retry Interval              | QoS 1 または QoS 2 メッセージの再送間隔を設定します。       | `30s`<br />単位: 秒                                         | --                            |
| `max_awaiting_rel`               | Max Awaiting PUBREL         | 各セッションで `PUBREL` を受信するまでまたはタイムアウトまで保留される QoS 2 メッセージの最大数を設定します。この制限に達すると、新しい QoS 2 `PUBLISH` リクエストはエラーコード `147(0x93)` で拒否されます。<br />MQTT における `PUBREL` は、QoS 2 メッセージフローで保証配信を行うための制御パケットです。 | `100`                                                       | `1` - `infinity`              |
| `await_rel_timeout`              | Max Awaiting PUBREL TIMEOUT | QoS 2 メッセージの `PUBREL` 受信を待つ最大時間を設定します。この制限に達すると、EMQX はパケットIDを解放し、警告レベルのログを生成します。<br />注意：EMQX は `PUBREL` を受信したかどうかに関わらず、受信した QoS 2 メッセージの転送を行います。 | `300s`<br />単位: 秒                                         | --                            |
| `session_expiry_interval`        | Session Expiry Interval     | セッションがアイドル状態で自動的に閉じられるまでの時間を設定します。MQTT 5.0 非対応クライアントのみ対象です。 | `2h`                                                        |                               |
| `max_mqueue_len`                 | Max Message Queue Length    | 永続化クライアントが切断された場合やインフライトウィンドウが満杯の場合に許可される最大キュー長を設定します。 | `1000`                                                      | `0` - `infinity`              |
| `mqueue_priorities`              | Topic Priorities            | トピック優先度を設定します。ここでの設定は `mqueue_default_priority` の設定を上書きします。 | `disabled` <br />セッションは `mqueue_default_priority` の優先度を使用します。 | `disabled`<br />または<br />`1` - `255` |
| `mqueue_default_priority`        | Default Topic Priorities    | デフォルトのトピック優先度を設定します。                    | `lowest`                                                    | `highest`, `lowest`           |
| `mqueue_store_qos0`              | Store QoS 0 Message         | 接続が切断されセッションが維持されている場合に、QoS 0 メッセージをメッセージキューに保存するかどうかを設定します。 | `true`                                                      | `true`, `false`               |
| `force_shutdown`                 | Enable Force Shutdown       | 強制シャットダウン機能を有効にするかどうかを設定します。メールボックスキュー長（`max_mailbox_size`）またはヒープサイズ（`max_heap_size`）が指定値に達すると、クライアント接続処理が強制的にシャットダウンされます。 | `true`                                                      | `true`, `false`               |
| `force_shutdown.max_mailbox_size` | Max Mailbox Size            | 強制シャットダウンをトリガーする最大メールボックスキュー長を設定します。 | `1000`                                                      | `1` - `infinity`              |
| `force_shutdown.max_heap_size`   | Max Heap Size               | 強制シャットダウンをトリガーする最大ヒープサイズを設定します。 | `32MB`                                                      | --                            |
| `force_gc`                     | --                          | 指定されたメッセージ数（`count`）または受信バイト数（`bytes`）に達した場合に強制ガベージコレクションを有効にするかどうかを設定します。 | `true`                                                      | `true`, `false`               |
| `force_gc.count`               | --                          | 強制ガベージコレクションをトリガーする受信メッセージ数を設定します。 | `16000`                                                     | `0` - `infinity`              |
| `force_gc.bytes`               | --                          | 強制ガベージコレクションをトリガーする受信バイト数を設定します。 | `16MB`<br />単位: `MB`                                      | --                            |

:::tip

MQTT 設定をダッシュボードで行うには、ダッシュボード左側のナビゲーションメニューから **Management** -> **MQTT Settings** をクリックしてください。ダッシュボードで設定を行うと、設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定がある場合、ダッシュボードからの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

:::

:::tip

EMQX はより詳細なカスタマイズニーズに対応するため、さらに多くの設定項目を提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご参照ください。

:::
