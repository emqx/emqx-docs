# MQTT 設定

[MQTT](https://mqtt.org/) は、モノのインターネット（IoT）デバイスを接続するための軽量なパブリッシュ／サブスクライブメッセージングプロトコルです。EMQX は MQTT 3.1、3.1.1、および 5.0 をサポートしています。

本ページでは、EMQX における MQTT プロトコルの動作設定について、パケット検証と制限、サブスクリプション、遅延パブリッシュ、キープアライブ処理、セッションなどの設定方法を説明します。

## 基本的な MQTT 設定

このセクションでは、パケットサイズ、クライアントIDの長さ、トピックレベル数、QoS（サービス品質）、トピックエイリアス、保持設定に関する MQTT プロトコルの動作を決定する設定項目を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも確認できます。ダッシュボードで設定した場合、その設定は設定ファイル内の同じ項目を上書きします。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定を記述すると、ダッシュボードでの変更が一時的なものとなり、EMQX 再起動時に失われるためです。

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
  strict_mode = true
}  
```

各項目の説明は以下の通りです。

| **設定項目**                 | ダッシュボード表示名           | **説明**                                                                 | **デフォルト値** | **選択可能な値**       |
| ---------------------------- | ------------------------------ | ------------------------------------------------------------------------ | ---------------- | ---------------------- |
| `max_packet_size`            | Max Packet Size                | MQTT クライアントと EMQX 間でメッセージ送信に使用される MQTT パケットの最大サイズを設定します。 | `1MB`            |                        |
| `max_connect_packet_size`    | Max CONNECT Packet Size        | CONNECT パケットの最大サイズを設定します。CONNECT パケットは `max_connect_packet_size` と `max_packet_size` の両方の制限内である必要があります。制限を超えると EMQX は接続を切断します。 | `1MB`            |                        |
| `max_clientid_len`           | Max Client ID Length           | MQTT クライアントIDの最大長を設定します。過度に長いクライアントIDの使用を防止できます。 | `65535`          | `23` - `65535`          |
| `max_topic_levels`           | Max Topic Levels               | MQTT トピックの階層レベルの最大数を設定します。                         | `128`            | `1` - `35`              |
| `max_qos_allowed`            | Max QoS                       | MQTT メッセージに許可される最大の QoS（サービス品質）レベルを設定します。 |                  |                        |
| `max_topic_alias`            | Max Topic Alias                | トピックエイリアスは、トピック名の代わりに短いエイリアスを使うことで MQTT パケットサイズを削減します。MQTT セッションで使用可能な最大トピックエイリアス数を設定します。 | `65535`          | `1` - `65535`           |
| `retain_available`           | Retain Available               | 保持メッセージ機能を有効にするかどうかを設定します。保持メッセージは、トピックに最後にパブリッシュされたメッセージを保存し、新規サブスクライバーが最新メッセージを受信できるようにします。 | `true`           | `true`, `false`         |
| `strict_mode`                | Strict Mode                   | 追加のプロトコル準拠チェックを受信 MQTT パケットに適用するかどうかを設定します。チェックに失敗したパケットはクライアント接続を切断します。 | `true`           | `true`, `false`         |

### 厳格な MQTT パケット検証

EMQX 6.3.0 以降、`strict_mode = true` がデフォルトで有効になっており、以下のような不正な MQTT パケットを拒否します。

- 無効な MQTT 固定ヘッダーフラグの組み合わせ
- MQTT 3.1.1 の CONNECT パケットにおいて、Username Flag なしに Password Flag がある場合
- クライアントID、トピック名、ユーザー名、パスワード、Will トピック、MQTT 5.0 の文字列プロパティなどに無効な UTF-8 文字列（ヌル文字や禁止制御文字を含む）
- MQTT プロトコルで非ゼロが要求されるパケット識別子がゼロの場合

不正なパケットを検出すると、EMQX はクライアント接続を切断し、`info` レベルで `frame_parse_error` ログを理由付きで記録します。MQTT 5.0 クライアントには可能な場合、理由コード `0x81`（Malformed Packet）を含む CONNACK または DISCONNECT パケットを送信します。MQTT 3.1 および 3.1.1 クライアントは理由コードなしで切断されます。

既存のクライアントがこれらのプロトコル要件に準拠していない場合、一時的に厳格モードのプロトコル準拠チェックを無効化できます。

```bash
mqtt.strict_mode = false
```

特定のレガシークライアントのみで厳格モードを無効にする場合は、ゾーンを設定し専用リスナーに紐付けます。

```bash
zones.legacy_clients {
  mqtt.strict_mode = false
}

listeners.tcp.legacy {
  bind = "0.0.0.0:1884"
  zone = legacy_clients
}
```

他のリスナー経由で接続するクライアントは引き続き厳格検証を使用します。ゾーンの詳細は [Zone Override](./configuration.md#zone-override) を参照してください。

## サブスクリプション設定

EMQX におけるサブスクリプションとは、クライアントが EMQX 上のトピックにサブスクライブする処理を指します。クライアントがトピックにサブスクライブすると、そのトピックにパブリッシュされたメッセージを受信したいことを示します。

本セクションでは、共有サブスクリプション、ワイルドカードサブスクリプション、および排他サブスクリプションの設定方法を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも確認できます。ダッシュボードで設定した場合、その設定は設定ファイル内の同じ項目を上書きします。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定を記述すると、ダッシュボードでの変更が一時的なものとなり、EMQX 再起動時に失われるためです。

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

| **設定項目**                   | ダッシュボード表示名               | **説明**                                                                 | **デフォルト値** | **選択可能な値**                                                                                                         |
| ------------------------------ | --------------------------------- | ------------------------------------------------------------------------ | ---------------- | ------------------------------------------------------------------------------------------------------------------------ |
| `wildcard_subscription`        | Wildcard Subscription Available   | ワイルドカードサブスクリプションを有効にするかどうかを設定します。`+` や `#` のワイルドカードを使い、複数トピックを一括サブスクライブ可能にします。 | `true`           | `true`, `false`                                                                                                           |
| `exclusive_subscription`       | Exclusive Subscription            | 排他サブスクリプションを有効にするかどうかを設定します。トピックに対し同時に1つのクライアントのみがサブスクライブ可能になります。 | `true`           | `true`, `false`                                                                                                           |
| `shared_subscription`          | Shared Subscription Available     | 共有サブスクリプションを有効にするかどうかを設定します。複数のクライアントが同一トピックのサブスクリプションを共有可能にします。 | `true`           | `true`, `false`                                                                                                           |
| `shared_subscription_strategy` |                                   | 共有サブスクリプションでメッセージを配信する際の分配戦略を設定します。`shared_subscription` が `true` の場合にのみ必要です。 | `round_robin`    | - `random`（ランダムにサブスクライバーへ配信）<br /><br />- `round_robin`（ラウンドロビン方式で順番に配信）<br /><br />- `sticky`（最後に選択されたサブスクライバーに固定して配信、切断時まで継続）<br /><br />- `hash`（`clientIds` のハッシュでサブスクライバーを選択） |

## 遅延パブリッシュ設定

遅延パブリッシュ機能は、クライアントがメッセージのパブリッシュを指定した時間だけ遅延させることを可能にします。この機能は、特定の時間にメッセージをパブリッシュしたい場合や、特定条件を満たしたときにメッセージを送信したい場合に有用です。

本セクションでは、遅延パブリッシュの有効化方法と遅延メッセージの最大数設定を紹介します。

**設定例:**

```bash
delay {
  delayed_publish_enabled = true
  max_delayed_messages = 0
}
```

- `delayed_publish_enabled` は遅延パブリッシュ機能の有効化設定です。デフォルトは `true`、選択可能値は `true`、`false` です。
- `max_delayed_messages` は許容される遅延メッセージの最大数を設定します。デフォルトは `0` です。

## キープアライブ設定

キープアライブは 2 バイトの整数で、秒単位の時間間隔を表します。これは、MQTT クライアントと EMQX 間の接続がデータ送信がなくてもアクティブな状態を維持するための仕組みです。MQTT クライアントが EMQX に接続する際、CONNECT パケットの可変ヘッダのキープアライブ値にゼロ以外を設定すると、双方でキープアライブ機能が有効になります。キープアライブの動作詳細は [MQTT キープアライブパラメータとは？](https://www.emqx.com/en/blog/mqtt-keep-alive) を参照してください。

MQTT 5.0 プロトコルによると、キープアライブが有効なクライアントに対して、サーバーはクライアントからキープアライブ時間の 1.5 倍以内に MQTT 制御パケットを受信しなければ、ネットワーク接続を切断しなければなりません。これに対応し、EMQX はクライアントのキープアライブタイムアウトを定期的にチェックするための設定 `keepalive_multiplier` を導入しています。デフォルト値は `1.5` です。

```bash
keepalive_multiplier = 1.5
```

タイムアウト計算式は以下の通りです。  
$$
\text{Keep Alive} \times \text{keepalive\_multiplier}
$$

### 動的キープアライブ調整

車載ネットワーク（T-Box）やモバイル IoT などのシナリオでは、MQTT クライアントは「アクティブ状態」（頻繁な通信）と「スリープ状態」（低消費電力のアイドル）を切り替える必要があります。単一の固定キープアライブ値では両方のニーズを満たせません。

- 短いキープアライブはアクティブ時の切断検知を高速化しますが、駐車中やアイドル時に過剰なハートビート通信とバッテリー消費を招きます。
- 長いキープアライブはスリープ時の通信量を減らしますが、アクティブ時の切断検知が遅れます。

EMQX は `$SETOPTS/` システムトピックを通じてクライアント単位で動的にキープアライブを調整可能です。クライアントは自身のブローカー側キープアライブ許容値を更新するためにこれらのトピックにパブリッシュできます。また、権限のあるバックエンドサービスは複数クライアントを一括更新できます。接続を切断したり MQTT 接続を再交渉したりせずに適用され、メモリ上のアクティブセッションにのみ反映され永続化されません。

::: warning リスナーのマウントポイントとの非互換性
動的キープアライブ調整は、[マウントポイント](./listener.md#mountpoint) が設定されたリスナー経由のクライアントには機能しません。EMQX はマウントポイントを `$SETOPTS/` プレフィックスの前に適用するため、更新はマウントされたリテラルトピックへの通常メッセージとしてルーティングされ、クライアントにはエラーが通知されません。
:::

#### 単一クライアント更新：`$SETOPTS/mqtt/keepalive`

クライアントはこのトピックにパブリッシュして自身のブローカー側キープアライブタイムアウトを更新します。EMQX は自動的にパブリッシュ元のセッションからクライアントIDを取得します。

**ペイロード:** 非負整数の文字列（秒単位）

```text
300
```

**有効範囲:** `0`～`65535` 秒。`0` はそのセッションのキープアライブチェックを無効化します。`65535` を超える値は `65535` にクランプされます。クライアントのゾーンに `mqtt.server_keepalive` が設定されている場合は、両者の最小値が有効値となります。

**利用例:** 車両が駐車状態になると T-Box クライアントが `$SETOPTS/mqtt/keepalive` に `300` をパブリッシュします。EMQX はブローカー側のキープアライブ許容値を 300 秒（デフォルトの `1.5×` 倍数で有効アイドルタイムアウトは 450 秒）に延長し、リモートコマンド配信のために接続を維持します。なお、これはブローカー側のタイムアウトのみ調整し、クライアントの実際の PINGREQ 間隔は変わりません。ハートビート通信量を減らすにはクライアント側もキープアライブ間隔を長くする必要があります。

#### 一括更新：`$SETOPTS/mqtt/keepalive-bulk`

バックエンドサービスはこのトピックにパブリッシュして複数クライアントのキープアライブを一括更新できます。

**ペイロード:** JSON 配列。各要素は以下を含みます。

| フィールド    | 型       | 必須 | 説明                         |
| ------------- | -------- | ---- | ---------------------------- |
| `clientid`    | 文字列   | 必須 | 対象 MQTT クライアント識別子 |
| `keepalive`   | 整数     | 必須 | 新しいキープアライブ間隔（秒単位、0～65535） |

```json
[
  { "clientid": "tbox-001", "keepalive": 300 },
  { "clientid": "tbox-002", "keepalive": 60 }
]
```

一括更新は非同期で処理され、クラスター対応です。EMQX は各対象クライアントをホストするノードを特定し、ノード間 RPC で更新を適用します。内部キューに 10 件を超える一括リクエストが溜まると追加リクエストは破棄され、警告ログが記録されます。

#### アクセス制御

2 つのトピックは細かい ACL をサポートするために分けられています。

- 認証済みクライアントに `$SETOPTS/mqtt/keepalive` へのパブリッシュを許可し、各デバイスが自身のキープアライブを調整可能にします。
- `$SETOPTS/mqtt/keepalive-bulk` は信頼できるバックエンドサービスのみに制限します。

:::tip
信頼できないクライアントに `$SETOPTS/mqtt/keepalive` へのパブリッシュ権限を与えないでください。キープアライブを `0` に設定するとそのセッションのキープアライブチェックが完全に無効化され、過度に大きな値は切断されない接続を長時間維持しブローカー資源を消費する恐れがあります。
:::

これらのトピックにパブリッシュされたメッセージは EMQX によってインターセプトされ消費され、サブスクライバーには配信されません。

## セッション設定

MQTT におけるセッションとは、クライアントとブローカー間の接続状態を指します。EMQX では、クライアントが接続するとセッションが確立され、トピックのサブスクライブやメッセージの受信、EMQX へのパブリッシュが可能になります。

本セクションではセッションの設定方法を紹介します。

**設定例:**

```hocon
mqtt {
    max_subscriptions = infinity
    upgrade_qos = false
    max_inflight = 32
    retry_interval = infinity
    max_awaiting_rel = 100
    await_rel_timeout = 300s
    session_expiry_interval = 2h
    max_session_expiry_interval = infinity
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

| **設定項目**                      | ダッシュボード表示名           | **説明**                                                                 | **デフォルト値**                   | **選択可能な値**                        |
| --------------------------------- | ------------------------------ | ------------------------------------------------------------------------ | --------------------------------- | ------------------------------------- |
| `max_subscriptions`               | Max Subscriptions              | クライアントが持てる最大サブスクリプション数を設定します。               | `infinity`                       | `1` - `infinity`                      |
| `upgrade_qos`                     | Upgrade QoS                   | パブリッシュ後にメッセージの QoS（サービス品質）レベルをアップグレード可能にするか設定します。 | `false`（無効）                  | `true`, `false`                      |
| `max_inflight`                    | Max Inflight                  | EMQX が同時に配信可能な未アック済み QoS 1 および QoS 2 メッセージの最大数を制限します。MQTT 5.0 クライアントではこの値とクライアントの `Receive Maximum` の小さい方が適用されます。詳細は [Message Queue Delivery Behavior](../../develop/design/inflight-window-and-message-queue.md#message-queue-delivery-behavior) を参照。 | `32`                             | `1` - `65535`                        |
| `retry_interval`                  | Retry Interval                | クライアント接続中に未アック済み QoS 1 および QoS 2 メッセージを再送する間隔を設定します。詳細は [Message Retransmission](../../develop/design/retransmission.md#basic-configuration) を参照。 | `infinity`                       | duration<br />または<br />`infinity` |
| `max_awaiting_rel`                | Max Awaiting PUBREL           | セッション内で `PUBREL` 受信待ちの QoS 2 メッセージの最大数を設定します。上限に達すると新規 QoS 2 `PUBLISH` はエラーコード `147(0x93)` で拒否されます。<br />MQTT の `PUBREL` は QoS 2 メッセージの配信制御パケットです。 | `100`                            | `1` - `infinity`                      |
| `await_rel_timeout`               | Max Awaiting PUBREL TIMEOUT   | QoS 2 メッセージの `PUBREL` 受信待ちタイムアウト時間を設定します。タイムアウト後、EMQX はパケットIDを解放し警告ログを出力します。<br />注意：`PUBREL` 受信の有無にかかわらず QoS 2 メッセージは転送されます。 | `300s`<br />単位: 秒              | --                                  |
| `session_expiry_interval`         | Session Expiry Interval       | クライアント切断後に EMQX がセッションを保持する期間を設定します。`Clean Session = false` で接続する MQTT 3.1 および 3.1.1 クライアントに適用されます。MQTT 5.0 クライアントは CONNECT プロパティ `Session-Expiry-Interval` で独自に設定します（`max_session_expiry_interval` 参照）。<br />デフォルトのインメモリセッションストアでは、切断されたセッションはこの期間メモリに保持されます。以下の警告も参照してください。 | `2h`                             | --                                  |
| `max_session_expiry_interval`     | Max Session Expiry Interval   | MQTT 5.0 クライアントが CONNECT および DISCONNECT パケットの `Session-Expiry-Interval` プロパティで要求できる最大セッション有効期限を制限します。クライアントがより長い値を要求した場合、EMQX はこの値にクランプし、CONNACK の同プロパティで返します。DISCONNECT パケットの長い値も同様にクランプされます。MQTT 3.1 および 3.1.1 クライアントには影響しません。EMQX 6.3.0 以降で利用可能。 | `infinity`（制限なし）           | duration<br />または<br />`infinity` |
| `max_mqueue_len`                  | Max Message Queue Length      | インメモリセッションのメッセージキュー内の最大メッセージ数を設定します。詳細は [Message Queue Delivery Behavior](../../develop/design/inflight-window-and-message-queue.md#message-queue-delivery-behavior) を参照。 | `1000`                          | `0` - `infinity`                    |
| `mqueue_priorities`               | Topic Priorities              | トピック優先度を設定します。ここでの設定は `mqueue_default_priority` の設定を上書きします。 | `disabled` <br />セッションは `mqueue_default_priority` の優先度を使用 | `disabled`<br />または<br />`1` - `255` |
| `mqueue_default_priority`         | Default Topic Priorities      | デフォルトのトピック優先度を設定します。                             | `lowest`                        | `highest`， `lowest`                |
| `mqueue_store_qos0`               | Store QoS 0 Message           | クライアント切断中または接続が混雑している間に、QoS 0 メッセージをインメモリセッションのメッセージキューに保存するか制御します。詳細は [Message Queue Delivery Behavior](../../develop/design/inflight-window-and-message-queue.md#message-queue-delivery-behavior) を参照。 | `true`                         | `true`, `false`                     |
| `force_shutdown`                  | Enable Force Shutdown         | 強制シャットダウン機能を有効にするか設定します。メールボックスキュー長（`max_mailbox_size`）またはヒープサイズ（`max_heap_size`）が指定値に達するとクライアント接続処理を強制終了します。 | `true`                         | `true`, `false`                     |
| `force_shutdown.max_mailbox_size` | Max Mailbox Size              | 強制シャットダウンをトリガーするメールボックスキューの最大長を設定します。 | `1000`                         | `1` - `infinity`                    |
| `force_shutdown.max_heap_size`    | Max Heap Size                 | 強制シャットダウンをトリガーするヒープサイズの最大値を設定します。       | `32MB`                         | --                                  |
| `force_gc`                        | --                            | 指定されたメッセージ数（`count`）または受信バイト数（`bytes`）に達した場合に強制ガベージコレクションを有効にするか設定します。 | `true`                         | `true`, `false`                     |
| `force_gc.count`                  | --                            | 強制ガベージコレクションをトリガーする受信メッセージ数を設定します。     | `16000`                        | `0` - `infinity`                    |
| `force_gc.bytes`                  | --                            | 強制ガベージコレクションをトリガーする受信バイト数を設定します。         | `16MB`<br />単位: `MB`          | --                                  |

:::warning 切断されたセッションのメモリコスト

デフォルトのインメモリセッションストアでは、有効期限がゼロより大きいセッションはクライアント切断時に削除されません。EMQX はクライアントが再接続するか有効期限が切れるまで、セッション、サブスクリプション、メッセージキューをメモリに保持します。クライアントがセッション有効期限前に再接続しない場合、ノード上の切断セッション数はおおよそクライアント切断率と有効期限の積になります。

切断後のセッション保持は永続セッションの意図した動作です。ワークロードに十分なメモリを割り当てるか、セッション状態をディスクに保存する [durable sessions](../durability/durability_introduction.md) を利用してください。

:::

:::tip

MQTT 設定をダッシュボードから行うには、ダッシュボード左のナビゲーションメニューで **Management** -> **MQTT Settings** をクリックしてください。ダッシュボードで設定した場合、その設定は設定ファイル内の同じ項目を上書きします。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定を記述すると、ダッシュボードでの変更が一時的なものとなり、EMQX 再起動時に失われるためです。

:::

::: tip

EMQX はカスタマイズニーズに対応するため、さらに多くの設定項目を提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) を参照してください。

:::
