# MQTT 設定

[MQTT](https://mqtt.org/) は、モノのインターネット（IoT）向けの標準メッセージングプロトコルです。非常に軽量なパブリッシュ／サブスクライブ型のメッセージングトランスポートとして設計されており、リモートデバイスを小さなコードフットプリントと最小限のネットワーク帯域幅で接続するのに最適です。

EMQX は 100% MQTT 5.0 および 3.x に準拠しています。本セクションでは、基本的な MQTT 設定項目について紹介します。基本的な MQTT 設定、サブスクリプション設定、セッション設定、強制シャットダウン設定、強制ガベージコレクション設定などのトピックを扱います。

## 基本的な MQTT 設定

このセクションでは、パケットサイズ、クライアントIDの長さ、トピックレベル数、QoS（サービス品質）、トピックエイリアス、保持設定など、MQTT プロトコルの動作を決定する設定項目を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも確認できます。ダッシュボードで設定した場合、その設定は設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定すると、ダッシュボードからの変更が一時的なものとなり、EMQX 再起動時に失われるためです。

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

| **設定項目**                 | ダッシュボードUI           | **説明**                                                                                     | **デフォルト値** | **設定可能値**       |
| ---------------------------- | -------------------------- | -------------------------------------------------------------------------------------------- | ---------------- | -------------------- |
| `max_packet_size`            | Max Packet Size            | MQTT パケットは MQTT クライアントと EMQX 間でメッセージを送信するために使用されます。<br /><br />許可される最大 MQTT パケットサイズを設定します。 | `1MB`            |                      |
| `max_connect_packet_size`    | Max CONNECT Packet Size    | CONNECT パケットの最大サイズを設定します。CONNECT パケットは `max_connect_packet_size` と `max_packet_size` の両方の制限内である必要があります。いずれかの制限を超えると EMQX は接続を切断します。 | `1MB`            |                      |
| `max_clientid_len`           | Max Client ID Length       | MQTT クライアントIDの最大長を設定します。<br /><br />過度に長いクライアントIDの使用を防止できます。 | `65535`          | `23` - `65535`       |
| `max_topic_levels`           | Max Topic Levels           | MQTT トピックはメッセージの整理・分類に使用されます。<br /><br />トピックに許可される最大レベル数を設定します。 | `128`            | `1` - `35`           |
| `max_qos_allowed`            | Max QoS                   | メッセージの信頼性と配信保証のレベルを決定する QoS レベルの最大値を設定します。                     |                  |                      |
| `max_topic_alias`            | Max Topic Alias            | トピックエイリアスは、完全なトピック名の代わりに短いエイリアスを使うことで MQTT パケットのサイズを削減する方法です。<br /><br />MQTT セッションで使用可能なトピックエイリアスの最大数を設定します。 | `65535`          | `1` - `65535`        |
| `retain_available`           | Retain Available           | 保持メッセージは、トピックに最後にパブリッシュされたメッセージを保存し、新しいサブスクライバーが最新のメッセージを受信できるようにします。<br /><br />MQTT の保持メッセージ機能を有効にするかどうかを設定します。 | `true`           | `true`, `false`      |

## サブスクリプション設定

EMQX におけるサブスクリプションとは、クライアントが EMQX 上のトピックをサブスクライブするプロセスを指します。クライアントがトピックをサブスクライブすると、そのトピックにパブリッシュされたメッセージを受信したいことを示します。

このセクションでは、共有サブスクリプション、ワイルドカードサブスクリプション、排他サブスクリプションの設定方法を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも確認できます。ダッシュボードで設定した場合、その設定は設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定すると、ダッシュボードからの変更が一時的なものとなり、EMQX 再起動時に失われるためです。

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

| **設定項目**                   | ダッシュボードUI              | **説明**                                                                                     | **デフォルト値** | **設定可能値**                                                  |
| ------------------------------ | ----------------------------- | -------------------------------------------------------------------------------------------- | ---------------- | -------------------------------------------------------------- |
| `wildcard_subscription`        | Wildcard Subscription Available | ワイルドカードサブスクリプションは、`+` や `#` などのワイルドカードを使って複数トピックを一括サブスクライブ可能にします。<br /><br />ワイルドカードサブスクリプションを有効にするかどうかを設定します。 | `true`           | `true`, `false`                                                |
| `exclusive_subscription`       | Exclusive Subscription        | 排他サブスクリプションは、1つのトピックに対して同時に1つの MQTT クライアントのみがサブスクライブ可能にします。<br /><br />排他サブスクリプションを有効にするかどうかを設定します。 | `true`           | `true`, `false`                                                |
| `shared_subscription`          | Shared Subscription Available | 共有サブスクリプションは複数の MQTT クライアントがトピックのサブスクリプションを共有できます。<br /><br />共有サブスクリプションを有効にするかどうかを設定します。 | `true`           | `true`, `false`                                                |
| `shared_subscription_strategy` |                               | 共有サブスクリプションを持つ MQTT クライアント間でメッセージを配信する戦略を定義します。<br /><br />`shared_subscription` が `true` の場合のみ必要です。 | `round_robin`    | - `random`（ランダムにサブスクライバーへ配信）<br /><br />- `round_robin`（ラウンドロビン方式でサブスクライバーを選択）<br /><br />- `sticky`（最後に選択されたサブスクライバーに配信し続ける。サブスクライバー切断まで）<br /><br />- `hash`（`clientIds` のハッシュでサブスクライバーを選択） |

## 遅延パブリッシュ設定

遅延パブリッシュ機能は、クライアントがメッセージのパブリッシュを指定した時間だけ遅延させることを可能にします。この機能は、特定の時間にメッセージをパブリッシュしたい場合や、特定条件が満たされたときにメッセージを送信したい場合に有用です。

このセクションでは、遅延パブリッシュの有効化方法と遅延メッセージの最大数設定方法を紹介します。

**設定例:**

```bash
delay {
  delayed_publish_enabled = true
  max_delayed_messages = 0
}
```

| 設定項目                    | 説明                                                                                      | デフォルト値 | 設定可能値           |
| --------------------------- | ----------------------------------------------------------------------------------------- | ------------ | -------------------- |
| `delayed_publish_enabled`   | EMQX で遅延パブリッシュ機能を有効にするかどうかを設定します。                             | `true`       | `true`, `false`      |
| `max_delayed_messages`      | 許可される遅延メッセージの最大数を設定します。                                           | `0`          |                      |

## キープアライブ設定

キープアライブは 2 バイトの整数で、秒単位の時間間隔を表します。これは MQTT クライアントと EMQX 間の接続がデータ送信がなくてもアクティブな状態を維持する仕組みです。  
MQTT クライアントが EMQX へ接続を確立するとき、CONNECT パケットの可変ヘッダーのキープアライブ変数に 0 以外の値を設定すると、両者間でキープアライブ機構が有効になります。キープアライブの動作詳細は [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive) を参照してください。

MQTT 5.0 プロトコルによると、キープアライブが有効なクライアントに対し、サーバーがクライアントからキープアライブ時間の1.5倍以内に MQTT 制御パケットを受信しなかった場合、ネットワーク接続を切断しなければなりません。  
そのため EMQX では `keepalive_multiplier` という設定項目を導入し、クライアントのキープアライブタイムアウト状態を定期的にチェックします。デフォルト値は `1.5` です。

```bash
keepalive_multiplier = 1.5
```

タイムアウト計算式は以下の通りです。  
$$
\text{Keep Alive} \times \text{keepalive\_multiplier}
$$

## セッション設定

MQTT におけるセッションとは、クライアントとブローカー間の接続を指します。EMQX では、クライアントが接続するとセッションが確立され、トピックのサブスクライブやメッセージの受信、EMQX へのメッセージパブリッシュが可能になります。

このセクションでは、セッションの設定方法を紹介します。

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

| **設定項目**                      | ダッシュボードUI           | **説明**                                                                                     | **デフォルト値**           | **設定可能値**                 |
| --------------------------------- | -------------------------- | -------------------------------------------------------------------------------------------- | -------------------------- | ------------------------------ |
| `max_subscriptions`               | Max Subscriptions          | クライアントが保持可能な最大サブスクリプション数を設定します。                              | `infinity`                 | `1` - `infinity`               |
| `upgrade_qos`                    | Upgrade QoS                | パブリッシュ後にメッセージの QoS（サービス品質）レベルをクライアントがアップグレード可能かどうかを設定します。 | `false`（無効）            | `true`, `false`                |
| `max_inflight`                   | Max Inflight               | 同時にフライト中（送信済みだが未アック）の QoS 1 および QoS 2 メッセージの最大数を設定します。 | `32`                       | `1` - `65535`                  |
| `retry_interval`                 | Retry Interval             | QoS 1 または QoS 2 メッセージの再送間隔を設定します。                                      | `30s`<br />単位: 秒        | --                            |
| `max_awaiting_rel`               | Max Awaiting PUBREL        | 各セッションで `PUBREL` を受信するかタイムアウトするまで保留する QoS 2 メッセージの最大数を設定します。<br />この上限に達すると、新たな QoS 2 `PUBLISH` リクエストはエラーコード `147(0x93)` で拒否されます。<br />MQTT では `PUBREL` は QoS 2 メッセージフローの制御パケットで、メッセージ配信保証を提供します。 | `100`                      | `1` - `infinity`              |
| `await_rel_timeout`              | Max Awaiting PUBREL TIMEOUT | QoS 2 メッセージの `PUBREL` を受信するまでの最大待機時間を設定します。<br />この時間を超えると EMQX はパケットIDを解放し、警告ログを出力します。<br />注意：EMQX は `PUBREL` 受信の有無にかかわらず受信した QoS 2 メッセージの転送を行います。 | `300s`<br />単位: 秒        | --                            |
| `session_expiry_interval`        | Session Expiry Interval    | セッションがアイドル状態で自動的にクローズされるまでの時間を設定します。<br />注意：MQTT 5.0 以外のクライアントのみ対象です。 | `2h`                       |                                |
| `max_mqueue_len`                 | Max Message Queue Length   | 永続化クライアントが切断された場合やフライトウィンドウが満杯のときの最大キュー長を設定します。 | `1000`                     | `0` - `infinity`              |
| `mqueue_priorities`              | Topic Priorities           | トピック優先度を設定します。この設定は `mqueue_default_priority` の設定を上書きします。     | `disabled`<br />セッションは `mqueue_default_priority` の優先度を使用 | `disabled`<br />または<br />`1` - `255` |
| `mqueue_default_priority`        | Default Topic Priorities   | デフォルトのトピック優先度を設定します。                                                   | `lowest`                   | `highest`， `lowest`           |
| `mqueue_store_qos0`              | Store QoS 0 Message        | 接続が切断されセッションが維持されているときに QoS 0 メッセージをメッセージキューに保存するかどうかを設定します。 | `true`                     | `true`, `false`               |
| `force_shutdown`                 | Enable Force Shutdown      | 強制シャットダウン機能を有効にするかどうかを設定します。メールボックスキュー長（`max_mailbox_size`）またはヒープサイズ（`max_heap_size`）が指定値に達するとクライアント接続処理が強制終了されます。 | `true`                     | `true`, `false`               |
| `force_shutdown.max_mailbox_size` | Max Mailbox Size           | 強制シャットダウンをトリガーする最大メールボックスキュー長を設定します。                   | `1000`                     | `1` - `infinity`              |
| `force_shutdown.max_heap_size`   | Max Heap Size              | 強制シャットダウンをトリガーする最大ヒープサイズを設定します。                             | `32MB`                     | --                            |
| `force_gc`                      | --                         | 指定されたメッセージ数（`count`）または受信バイト数（`bytes`）に達した場合に強制ガベージコレクションを有効にするかどうかを設定します。 | `true`                     | `true`, `false`               |
| `force_gc.count`                | --                         | 強制ガベージコレクションをトリガーする受信メッセージ数を設定します。                       | `16000`                    | `0` - `infinity`              |
| `force_gc.bytes`                | --                         | 強制ガベージコレクションをトリガーする受信バイト数を設定します。                           | `16MB`<br />単位: MB       | --                            |

:::tip

ダッシュボードから MQTT 設定を行う場合は、ダッシュボード左のナビゲーションメニューから **Management** -> **MQTT Settings** をクリックしてください。ダッシュボードで設定した内容は設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定すると、ダッシュボードからの変更が一時的なものとなり、EMQX 再起動時に失われるためです。

:::

:::tip

EMQX はカスタマイズニーズに応じたより多くの設定項目を提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご参照ください。

:::
