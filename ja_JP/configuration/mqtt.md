# MQTT 設定

[MQTT](https://mqtt.org/) は、モノのインターネット（IoT）向けの標準的なメッセージングプロトコルです。非常に軽量なパブリッシュ／サブスクライブ型のメッセージトランスポートとして設計されており、リモートデバイスを小さなコードフットプリントと最小限のネットワーク帯域で接続するのに最適です。

EMQX は MQTT 5.0 および 3.x に 100% 準拠しています。本セクションでは、基本的な MQTT 設定項目について紹介します。基本的な MQTT 設定、サブスクリプション設定、セッション設定、強制シャットダウン設定、および強制ガベージコレクション設定などのトピックを扱います。

## 基本的な MQTT 設定

このセクションでは、パケットサイズ、クライアントID長、トピック階層数、QoS（サービス品質）、トピックエイリアス、保持設定など、MQTT プロトコルの動作を決定する設定項目を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも確認できます。ダッシュボードで設定を行うと、設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` を使用することを推奨します。  
`emqx.conf` に設定した場合、ダッシュボードからの変更は一時的なものとなり、EMQX の再起動時に失われるためです。

:::

**設定例:**

```bash
mqtt {
  max_packet_size = 1MB
  max_clientid_len = 65535
  max_topic_levels = 128
  max_qos_allowed = 2
  max_topic_alias = 65535
  retain_available = true
}  
```

各項目の説明は以下の通りです。

| **設定項目**             | ダッシュボードUI           | **説明**                                                                 | **デフォルト値** | **設定可能値**        |
|-------------------------|----------------------------|-------------------------------------------------------------------------|------------------|-----------------------|
| `max_packet_size`       | Max Packet Size            | MQTT クライアントと EMQX 間でメッセージ送信に使われる MQTT パケットの最大サイズを設定します。 | `1MB`            |                       |
| `max_clientid_len`      | Max Client ID Length       | MQTT クライアントIDの最大長を設定します。過度に長いクライアントIDの使用を防止できます。 | `65535`          | `23` - `65535`        |
| `max_topic_levels`      | Max Topic Levels           | MQTT トピックの階層数の最大値を設定します。トピックはメッセージの分類に使われます。 | `128`            | `1` - `35`            |
| `max_qos_allowed`       | Max QoS                    | MQTT メッセージに許可される最大の QoS（サービス品質）レベルを設定します。               |                  |                       |
| `max_topic_alias`       | Max Topic Alias            | トピックエイリアスは、トピック名の代わりに短いエイリアスを使うことで MQTT パケットサイズを削減します。セッションで使用可能な最大エイリアス数を設定します。 | `65535`          | `1` - `65535`         |
| `retain_available`      | Retain Available           | 保持メッセージ機能を有効にするかどうかを設定します。保持メッセージは、トピックに最後にパブリッシュされたメッセージを保存し、新規サブスクライバーが最新メッセージを受け取れるようにします。 | `true`           | `true`, `false`       |

## サブスクリプション設定

EMQX におけるサブスクリプションとは、クライアントが EMQX のトピックにサブスクライブするプロセスを指します。クライアントがトピックをサブスクライブすると、そのトピックにパブリッシュされたメッセージを受信したいことを示します。

このセクションでは、共有サブスクリプション、ワイルドカードサブスクリプション、および排他サブスクリプションの設定方法を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも確認できます。ダッシュボードで設定を行うと、設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` を使用することを推奨します。  
`emqx.conf` に設定した場合、ダッシュボードからの変更は一時的なものとなり、EMQX の再起動時に失われるためです。

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

| **設定項目**                 | ダッシュボードUI               | **説明**                                                                                         | **デフォルト値** | **設定可能値**                                                                                                   |
|-----------------------------|-------------------------------|-------------------------------------------------------------------------------------------------|------------------|-----------------------------------------------------------------------------------------------------------------|
| `wildcard_subscription`     | Wildcard Subscription Available | ワイルドカードサブスクリプションは、`+` や `#` などのワイルドカードを使い、複数トピックを一括でサブスクライブ可能にします。 | `true`           | `true`, `false`                                                                                                |
| `exclusive_subscription`    | Exclusive Subscription         | 排他サブスクリプションは、特定トピックに対して同時に1つのクライアントのみがサブスクライブ可能にします。 | `true`           | `true`, `false`                                                                                                |
| `shared_subscription`       | Shared Subscription Available  | 共有サブスクリプションは、複数のクライアントが同じトピックのサブスクリプションを共有できます。               | `true`           | `true`, `false`                                                                                                |
| `shared_subscription_strategy` |                             | 共有サブスクリプションでメッセージを複数クライアントに配信する際の分配戦略を設定します。<br />`shared_subscription` が `true` の場合にのみ必要です。 | `round_robin`    | - `random`（ランダムにサブスクライバーへ配信）<br /><br />- `round_robin`（ラウンドロビン方式で順番に配信）<br /><br />- `sticky`（最後に選択されたサブスクライバーへ継続配信、切断時まで）<br /><br />- `hash`（`clientIds` のハッシュでサブスクライバーを選択） |

## 遅延パブリッシュ設定

遅延パブリッシュ機能は、クライアントがメッセージを指定した時間だけ遅延してトピックにパブリッシュできる機能です。特定の時間にメッセージをパブリッシュしたい場合や、条件が整ったときにパブリッシュしたい場合に有用です。

このセクションでは、遅延パブリッシュの有効化方法と遅延メッセージの最大数設定を紹介します。

**設定例:**

```bash
delay {
  delayed_publish_enabled = true
  max_delayed_messages = 0
}
```

各項目の説明は以下の通りです。

- `delayed_publish_enabled` は遅延パブリッシュ機能を有効にするかどうかを設定します。デフォルト値は `true`、設定可能値は `true`、`false` です。  
- `max_delayed_messages` は許容される遅延メッセージの最大数を設定します。デフォルト値は `0` です。

## キープアライブ設定

キープアライブは 2 バイトの整数で、秒単位の時間間隔を表します。MQTT クライアントと EMQX の接続がデータ送信がなくても維持されていることを保証する仕組みです。MQTT クライアントが EMQX に接続する際、CONNECT パケットのヘッダーに非ゼロのキープアライブ値を設定すると、双方でキープアライブ機構が有効になります。キープアライブの動作詳細は [MQTT のキープアライブパラメータとは？](https://www.emqx.com/en/blog/mqtt-keep-alive) を参照してください。

MQTT 5.0 プロトコルに従い、キープアライブが有効なクライアントに対して、サーバーはキープアライブ時間の1.5倍以内にクライアントから MQTT コントロールパケットを受信しない場合、ネットワーク接続を切断しなければなりません。  
そのため、EMQX ではクライアントのキープアライブタイムアウト状態を定期的にチェックするための設定 `keepalive_multiplier` を導入しています。デフォルト値は `1.5` です。

```bash
keepalive_multiplier = 1.5
```

タイムアウトの計算式は以下の通りです。  
$$
\text{Keep Alive} \times \text{keepalive\_multiplier}
$$

## セッション設定

MQTT におけるセッションとは、クライアントとブローカー間の接続を指します。EMQX では、クライアントが接続するとセッションが確立され、トピックのサブスクライブやメッセージの受信、EMQX へのメッセージパブリッシュが可能になります。

このセクションではセッションの設定方法を紹介します。

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

| **設定項目**                      | ダッシュボードUI             | **説明**                                                                                                                         | **デフォルト値**                                               | **設定可能値**                     |
|----------------------------------|------------------------------|---------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------|-----------------------------------|
| `max_subscriptions`              | Max Subscriptions            | クライアントが持てる最大サブスクリプション数を設定します。                                                                       | `infinity`                                                    | `1` - `infinity`                  |
| `upgrade_qos`                    | Upgrade QoS                  | メッセージパブリッシュ後にクライアントが QoS（サービス品質）レベルをアップグレードできるかどうかを設定します。                   | `false`（無効）                                               | `true`, `false`                   |
| `max_inflight`                   | Max Inflight                 | QoS 1 および QoS 2 メッセージの送信済みだが未アック状態の最大数を設定します。                                                     | `32`                                                          | `1` - `65535`                     |
| `retry_interval`                 | Retry Interval               | QoS 1 または QoS 2 メッセージの再送間隔を設定します。単位は秒です。                                                               | `30s`                                                         | --                                |
| `max_awaiting_rel`               | Max Awaiting PUBREL          | セッションごとに PUBREL を受信するまで保留される QoS 2 メッセージの最大数を設定します。制限超過時は新規 QoS 2 PUBLISH リクエストをエラーコード `147(0x93)` で拒否します。<br />MQTT の PUBREL は QoS 2 メッセージフローの制御パケットで、メッセージ配信の保証に使われます。 | `100`                                                         | `1` - `infinity`                  |
| `await_rel_timeout`              | Max Awaiting PUBREL TIMEOUT  | QoS 2 メッセージの PUBREL 受信待機時間を設定します。タイムアウト時に EMQX はパケットIDを解放し、警告ログを出力します。<br />注：PUBREL の受信有無に関わらず、EMQX は受信した QoS 2 メッセージを転送します。 | `300s`                                                        | --                                |
| `session_expiry_interval`        | Session Expiry Interval      | セッションがアイドル状態のまま自動的にクローズされるまでの時間を設定します。MQTT 5.0 非対応クライアント向けです。               | `2h`                                                          |                                   |
| `max_mqueue_len`                 | Max Message Queue Length     | 永続化クライアントが切断された場合やインフライトウィンドウが満杯の場合のメッセージキューの最大長を設定します。                   | `1000`                                                        | `0` - `infinity`                  |
| `mqueue_priorities`              | Topic Priorities             | トピック優先度を設定します。ここでの設定は `mqueue_default_priority` の設定を上書きします。                                       | `disabled` <br />セッションは `mqueue_default_priority` の優先度を使用します。 | `disabled`<br />または<br />`1` - `255` |
| `mqueue_default_priority`        | Default Topic Priorities     | デフォルトのトピック優先度を設定します。                                                                                         | `lowest`                                                      | `highest`， `lowest`              |
| `mqueue_store_qos0`              | Store QoS 0 Message          | 接続が切断されセッションが維持されている場合に QoS 0 メッセージをメッセージキューに保存するかどうかを設定します。                 | `true`                                                        | `true`, `false`                   |
| `force_shutdown`                 | Enable Force Shutdown        | 強制シャットダウン機能を有効にするかどうかを設定します。メールボックスキュー長（`max_mailbox_size`）またはヒープサイズ（`max_heap_size`）が指定値に達するとクライアント接続処理が強制終了されます。 | `true`                                                        | `true`, `false`                   |
| `force_shutdown.max_mailbox_size` | Max Mailbox Size             | 強制シャットダウンをトリガーする最大メールボックスキュー長を設定します。                                                         | `1000`                                                        | `1` - `infinity`                  |
| `force_shutdown.max_heap_size`   | Max Heap Size                | 強制シャットダウンをトリガーする最大ヒープサイズを設定します。                                                                     | `32MB`                                                        | --                                |
| `force_gc`                     | --                           | 指定されたメッセージ数（`count`）または受信バイト数（`bytes`）に達した場合に強制ガベージコレクションを有効にするかどうかを設定します。 | `true`                                                        | `true`, `false`                   |
| `force_gc.count`               | --                           | 強制ガベージコレクションをトリガーする受信メッセージ数を設定します。                                                               | `16000`                                                       | `0` - `infinity`                  |
| `force_gc.bytes`               | --                           | 強制ガベージコレクションをトリガーする受信バイト数を設定します。単位は MB です。                                                   | `16MB`                                                        | --                                |

:::tip

ダッシュボードで MQTT 設定を行うには、左メニューの **Management** -> **MQTT Settings** をクリックしてください。ダッシュボードで設定した内容は設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` を使用することを推奨します。  
`emqx.conf` に設定した場合、ダッシュボードからの変更は一時的なものとなり、EMQX の再起動時に失われるためです。

:::

:::tip

EMQX はより詳細なカスタマイズに対応する多くの設定項目を提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご参照ください。

:::
