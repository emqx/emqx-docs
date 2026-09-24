# MQTT 設定

[MQTT](https://mqtt.org/) は、モノのインターネット（IoT）向けの標準的なメッセージングプロトコルです。非常に軽量なパブリッシュ／サブスクライブ型メッセージトランスポートとして設計されており、リモートデバイスを小さなコードフットプリントと最小限のネットワーク帯域幅で接続するのに最適です。

EMQX は 100% MQTT 5.0 および 3.x に準拠しています。本セクションでは、基本的な MQTT 設定項目について紹介し、基本的な MQTT 設定、サブスクリプション設定、セッション設定、強制シャットダウン設定、強制ガベージコレクション設定などのトピックをカバーします。

## 基本的な MQTT 設定

このセクションでは、パケットサイズ、クライアントIDの長さ、トピックレベル数、QoS（サービス品質）、トピックエイリアス、保持設定など、MQTT プロトコルの動作を決定する設定項目を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボード（**管理** -> **MQTT 設定** -> **一般**）でも確認できます。ダッシュボードで設定した内容は、設定ファイル内の同じ設定項目を上書きします。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定があると、ダッシュボードでの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

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

各設定項目の説明は以下の通りです。

| **設定項目**                    | ダッシュボード表示名         | **説明**                                                    | **デフォルト値** | **選択可能な値**           |
| ------------------------------ | ---------------------------- | ----------------------------------------------------------- | ---------------- | -------------------------- |
| `max_packet_size`               | Max Packet Size              | MQTT クライアントと EMQX 間でメッセージを送受信するための MQTT パケットの最大サイズを設定します。 | `1MB`            |                            |
| `max_connect_packet_size`       | Max CONNECT Packet Size      | CONNECT パケットの最大サイズを設定します。CONNECT パケットは `max_connect_packet_size` と `max_packet_size` の両方の制限内である必要があります。いずれかの制限を超えると EMQX は接続を切断します。 | `1MB`            |                            |
| `max_clientid_len`              | Max Client ID Length         | MQTT クライアントIDの最大長を設定します。過度に長いクライアントIDの使用を防止できます。 | `65535`          | `23` - `65535`             |
| `max_topic_levels`              | Max Topic Levels             | MQTT トピックの階層レベル数の最大値を設定します。トピックは階層的にメッセージを整理・分類するために使用されます。 | `128`            | `1` - `35`                 |
| `max_qos_allowed`               | Max QoS                     | MQTT メッセージに許可される最大の QoS（サービス品質）レベルを設定します。 |                  |                            |
| `max_topic_alias`               | Max Topic Alias              | トピックエイリアスは、完全なトピック名の代わりに短いエイリアスを使うことで MQTT パケットサイズを削減する方法です。MQTT セッションで使用可能な最大トピックエイリアス数を設定します。 | `65535`          | `1` - `65535`              |
| `retain_available`              | Retain Available             | Retained メッセージは、トピックに最後にパブリッシュされたメッセージを保存し、新規サブスクライバーが最新メッセージを受け取れるようにします。MQTT の Retained メッセージ機能を有効化するかどうかを設定します。 | `true`           | `true`, `false`            |

## サブスクリプション設定

EMQX におけるサブスクリプションとは、クライアントが EMQX 上のトピックにサブスクライブするプロセスを指します。クライアントがトピックにサブスクライブすると、そのトピックにパブリッシュされたメッセージを受信したいことを示します。

このセクションでは、共有サブスクリプション、ワイルドカードサブスクリプション、排他サブスクリプションの設定方法を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボード（**管理** -> **MQTT 設定** -> **一般**）でも確認できます。ダッシュボードで設定した内容は、設定ファイル内の同じ設定項目を上書きします。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定があると、ダッシュボードでの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

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

各設定項目の説明は以下の通りです。

| **設定項目**                   | ダッシュボード表示名           | **説明**                                                    | **デフォルト値** | **選択可能な値**                                             |
| ----------------------------- | ------------------------------ | ----------------------------------------------------------- | ---------------- | ------------------------------------------------------------ |
| `wildcard_subscription`       | Wildcard Subscription Available | ワイルドカードサブスクリプションは、`+` や `#` といったワイルドカードを使い、複数のトピックに対して単一のサブスクリプションでサブスクライブ可能にします。 | `true`           | `true`, `false`                                              |
| `exclusive_subscription`      | Exclusive Subscription          | 排他サブスクリプションは、1つのトピックに対して同時に1つの MQTT クライアントのみがサブスクライブ可能にします。 | `true`           | `true`, `false`                                              |
| `shared_subscription`         | Shared Subscription Available   | 共有サブスクリプションは、複数の MQTT クライアントが1つのトピックのサブスクリプションを共有可能にします。 | `true`           | `true`, `false`                                              |
| `shared_subscription_strategy`|                                | 共有サブスクリプションを共有する MQTT クライアント間でメッセージを配信する戦略を定義します。<br />`shared_subscription` が `true` の場合にのみ必要です。 | `round_robin`    | - `random`（ランダムにサブスクライバーへ配信）<br /><br />- `round_robin`（ラウンドロビン方式でサブスクライバーを選択）<br /><br />- `sticky`（最後に選択されたサブスクライバーに配信し続け、切断されるまで維持）<br /><br />- `hash`（`clientIds` のハッシュでサブスクライバーを選択） |

## 遅延パブリッシュ設定

遅延パブリッシュ機能は、クライアントがメッセージのパブリッシュを指定した時間だけ遅延させることを可能にします。この機能は、特定の時間にメッセージをパブリッシュしたい場合や、特定条件が満たされたときにメッセージを送信したい場合に有用です。

このセクションでは、遅延パブリッシュの有効化方法と許可される遅延メッセージの最大数の設定方法を紹介します。

**設定例:**

```bash
delay {
  delayed_publish_enabled = true
  max_delayed_messages = 0
}
```

各設定項目の説明は以下の通りです。

- `delayed_publish_enabled` は EMQX における遅延パブリッシュ機能の有効化を設定します。デフォルト値は `true`、選択可能値は `true`、`false` です。  
- `max_delayed_messages` は許可される遅延メッセージの最大数を設定します。デフォルト値は `0` です。

## キープアライブ設定

キープアライブは 2 バイトの整数で、秒単位の時間間隔を示します。これは、MQTT クライアントと EMQX 間の接続をデータ送信がなくても維持するための仕組みです。MQTT クライアントが EMQX に接続を確立するとき、CONNECT パケットの可変ヘッダーのキープアライブ変数に 0 以外の値を設定すると、双方でキープアライブ機構が有効になります。キープアライブの動作詳細については、[MQTT キープアライブパラメータとは？](https://www.emqx.com/en/blog/mqtt-keep-alive) をご参照ください。

MQTT 5.0 プロトコルによると、キープアライブが有効なクライアントに対し、サーバーはキープアライブ時間の 1.5 倍の間クライアントから MQTT コントロールパケットを受信しない場合、ネットワーク接続を切断しなければなりません。  
そのため EMQX では、クライアントのキープアライブタイムアウト状態を定期的にチェックするための設定項目 `keepalive_multiplier` を導入しています。デフォルト値は `1.5` です。

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

各設定項目の説明は以下の通りです。

| **設定項目**                      | ダッシュボード表示名           | **説明**                                                    | **デフォルト値**                                            | **選択可能な値**                 |
| -------------------------------- | ------------------------------ | ----------------------------------------------------------- | ----------------------------------------------------------- | --------------------------------- |
| `max_subscriptions`               | Max Subscriptions              | クライアントが持てる最大サブスクリプション数を設定します。 | `infinity`                                                  | `1` - `infinity`                |
| `upgrade_qos`                    | Upgrade QoS                   | メッセージパブリッシュ後にクライアントが QoS（サービス品質）レベルをアップグレードできるかどうかを設定します。 | `false`（無効）                                            | `true`, `false`                 |
| `max_inflight`                   | Max Inflight                  | QoS 1 および QoS 2 メッセージのうち、送信済みでまだアック（ACK）を受け取っていないメッセージの最大数を設定します。 | `32`                                                       | `1` - `65535`                   |
| `retry_interval`                 | Retry Interval                | QoS 1 または QoS 2 メッセージの再送間隔を設定します。      | `30s`<br />単位: 秒                                        | --                              |
| `max_awaiting_rel`               | Max Awaiting PUBREL           | 各セッションで `PUBREL` を受信するまで保留される QoS 2 メッセージの最大数を設定します。上限に達すると、新規 QoS 2 `PUBLISH` リクエストはエラーコード `147(0x93)` で拒否されます。<br />MQTT における `PUBREL` は、QoS 2 メッセージの確実な配信を保証するための制御パケットです。 | `100`                                                      | `1` - `infinity`                |
| `await_rel_timeout`              | Max Awaiting PUBREL TIMEOUT   | QoS 2 メッセージの `PUBREL` 受信を待つ最大時間を設定します。タイムアウト後、EMQX はパケットIDを解放し、警告レベルのログを生成します。<br />注意: EMQX は `PUBREL` の有無にかかわらず受信した QoS 2 メッセージを転送します。 | `300s`<br />単位: 秒                                      | --                              |
| `session_expiry_interval`        | Session Expiry Interval       | セッションがアイドル状態のまま自動的に閉じられるまでの時間を設定します。非 MQTT 5.0 クライアントのみ対象です。 | `2h`                                                       |                                |
| `max_mqueue_len`                 | Max Message Queue Length      | 永続化クライアントが切断されている場合やインフライトウィンドウが満杯の場合に許可される最大キュー長を設定します。 | `1000`                                                     | `0` - `infinity`                |
| `mqueue_priorities`              | Topic Priorities              | トピック優先度を設定します。この設定は `mqueue_default_priority` の設定を上書きします。 | `disabled` <br />セッションは `mqueue_default_priority` の優先度を使用します。 | `disabled`<br />または<br />`1` - `255` |
| `mqueue_default_priority`        | Default Topic Priorities      | デフォルトのトピック優先度を設定します。                     | `lowest`                                                   | `highest`， `lowest`            |
| `mqueue_store_qos0`              | Store QoS 0 Message           | 接続が切断されていてもセッションが維持されている場合に、QoS 0 メッセージをメッセージキューに保存するかどうかを設定します。 | `true`                                                     | `true`, `false`                 |
| `force_shutdown`                 | Enable Force Shutdown         | 強制シャットダウン機能を有効にするかどうかを設定します。メールボックスキュー長（`max_mailbox_size`）またはヒープサイズ（`max_heap_size`）が指定値に達すると、クライアント接続プロセスは強制的にシャットダウンされます。 | `true`                                                     | `true`, `false`                 |
| `force_shutdown.max_mailbox_size` | Max Mailbox Size              | 強制シャットダウンをトリガーする最大メールボックスキュー長を設定します。 | `1000`                                                     | `1` - `infinity`                |
| `force_shutdown.max_heap_size`   | Max Heap Size                 | 強制シャットダウンをトリガーする最大ヒープサイズを設定します。 | `32MB`                                                     | --                              |
| `force_gc`                     | --                            | 指定されたメッセージ数（`count`）または受信バイト数（`bytes`）に達した場合に強制ガベージコレクションを有効にするかどうかを設定します。 | `true`                                                     | `true`, `false`                 |
| `force_gc.count`               | --                            | 強制ガベージコレクションをトリガーする受信メッセージ数を設定します。 | `16000`                                                    | `0` - `infinity`                |
| `force_gc.bytes`               | --                            | 強制ガベージコレクションをトリガーする受信バイト数を設定します。 | `16MB`<br />単位: `MB`                                    | --                              |

:::tip

MQTT 設定をダッシュボードで行う場合は、ダッシュボード左側のナビゲーションメニューから **管理** -> **MQTT 設定** をクリックしてください。ダッシュボードで設定した内容は設定ファイル内の同じ設定項目を上書きします。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定があると、ダッシュボードでの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

:::

:::tip

EMQX はより詳細なカスタマイズに対応する多くの設定項目を提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご参照ください。

:::
