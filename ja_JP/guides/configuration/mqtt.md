# MQTT 設定

[MQTT](https://mqtt.org/) は、Internet of Things（IoT）デバイスを接続するための軽量なパブリッシュ／サブスクライブメッセージングプロトコルです。EMQX は MQTT 3.1、3.1.1、および 5.0 をサポートしています。

このページでは、EMQX における MQTT プロトコルの動作設定について、パケット検証と制限、サブスクリプション、遅延パブリッシュ、キープアライブ処理、セッションなどの設定方法を説明します。

## 基本的な MQTT 設定

このセクションでは、パケットサイズ、クライアント ID の長さ、トピックレベル数、QoS（サービス品質）、トピックエイリアス、保持設定など、MQTT プロトコルの動作を決定する設定項目を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** にもあります。ダッシュボードで設定した場合、設定ファイルの同じ項目より優先されます。  
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
  strict_mode = true
}  
```

各項目の説明は以下の通りです。

| **設定項目**                  | ダッシュボード UI           | **説明**                                                                                              | **デフォルト値** | **設定可能値**          |
| ----------------------------- | --------------------------- | --------------------------------------------------------------------------------------------------- | ---------------- | ----------------------- |
| `max_packet_size`             | Max Packet Size             | MQTT クライアントと EMQX 間でメッセージ送信に使われる MQTT パケットの最大サイズを設定します。          | `1MB`            |                         |
| `max_connect_packet_size`     | Max CONNECT Packet Size     | CONNECT パケットの最大サイズを設定します。CONNECT パケットは `max_connect_packet_size` と `max_packet_size` の両方の制限内である必要があります。どちらかを超えると EMQX は接続を切断します。EMQX 6.3.2 以降で利用可能。 | `1MB`            |                         |
| `max_clientid_len`            | Max Client ID Length        | MQTT クライアント ID の最大長を設定します。過度に長いクライアント ID の使用を防止できます。             | `65535`          | `23` - `65535`          |
| `max_topic_levels`            | Max Topic Levels            | MQTT トピックの階層レベル数の最大値を設定します。                                                    | `128`            | `1` - `35`              |
| `max_qos_allowed`             | Max QoS                    | MQTT メッセージで許可される最大の QoS（サービス品質）レベルを設定します。                             |                  |                         |
| `max_topic_alias`             | Max Topic Alias             | トピックエイリアスは、トピック名の代わりに短いエイリアスを使うことで MQTT パケットのサイズを削減する仕組みです。MQTT セッションで使用可能な最大トピックエイリアス数を設定します。 | `65535`          | `1` - `65535`           |
| `retain_available`            | Retain Available            | 保持メッセージ機能を有効にするかどうかを設定します。保持メッセージは、トピックに最後にパブリッシュされたメッセージを保存し、新規サブスクライバーに最新メッセージを届けます。 | `true`           | `true`, `false`         |
| `strict_mode`                 | Strict Mode                 | 受信した MQTT パケットに対して追加のプロトコル準拠チェックを適用するかどうかを設定します。チェックに失敗したパケットはクライアント接続を切断します。 | `true`           | `true`, `false`         |

### 厳密な MQTT パケット検証

EMQX 6.3.0 以降、`strict_mode = true` がデフォルトで有効になっています。これにより、以下のような不正な MQTT パケットを EMQX が拒否します。

- 不正な MQTT 固定ヘッダーフラグの組み合わせ
- MQTT 3.1.1 の CONNECT パケットで、Username フラグなしに Password フラグが立っている場合
- クライアント ID、トピック名、ユーザー名、パスワード、Will トピック、MQTT 5.0 の文字列プロパティなどに不正な UTF-8 文字列（ヌル文字や禁止されている制御文字を含む）が含まれる場合
- MQTT プロトコルで非ゼロが要求されるパケット識別子がゼロの場合

不正なパケットを検出すると、EMQX はクライアント接続を切断し、`frame_parse_error` ログを `info` レベルで理由と共に記録します。MQTT 5.0 クライアントには可能な場合、理由コード `0x81`（Malformed Packet）を含む CONNACK または DISCONNECT パケットを送信します。MQTT 3.1 および 3.1.1 クライアントは理由コードなしで切断されます。

::: warning 厳密モードでは UTF-8 エンコードされたパスワードが必要です

MQTT 仕様では CONNECT パケットのパスワードフィールドはバイナリデータとして定義されており、任意のバイト列を含むことができます。しかし厳密モードが有効な場合、EMQX はこのフィールドを UTF-8 文字列として検証します。パスワードに不正な UTF-8 や制御文字が含まれると、CONNECT パケットは拒否され、ネットワーク接続が切断されます。

バイナリパスワード（生のハッシュバイトやバイナリトークンなど）を送信する必要があるクライアントには、以下の専用ゾーンとリスナー設定を用いて、そのクライアントのみ厳密モードを無効化することを推奨します。厳密モードを無効にすると、厳密モードでのみ適用される他のチェックも無効になります。

:::

既存のクライアントがこれらの MQTT プロトコル要件に準拠していない場合、厳密モードでのみ強制されるプロトコル準拠チェックを一時的に無効化できます。

```bash
mqtt.strict_mode = false
```

特定のレガシークライアントのみ厳密モードチェックを無効にするには、ゾーンを設定し専用リスナーに割り当てます。

```bash
zones.legacy_clients {
  mqtt.strict_mode = false
}

listeners.tcp.legacy {
  bind = "0.0.0.0:1884"
  zone = legacy_clients
}
```

他のリスナー経由で接続するクライアントは引き続き厳密な検証を使用します。ゾーンの詳細は [Zone Override](./configuration.md#zone-override) を参照してください。

## サブスクリプション設定

EMQX におけるサブスクリプションとは、クライアントが EMQX のトピックにサブスクライブするプロセスを指します。クライアントがトピックにサブスクライブすると、そのトピックにパブリッシュされたメッセージを受信したいことを示します。

このセクションでは、共有サブスクリプション、ワイルドカードサブスクリプション、排他サブスクリプションの設定方法を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** にもあります。ダッシュボードで設定した場合、設定ファイルの同じ項目より優先されます。  
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

各項目の説明は以下の通りです。

| **設定項目**                   | ダッシュボード UI               | **説明**                                                                                              | **デフォルト値** | **設定可能値**                                                                                              |
| ------------------------------ | ------------------------------- | --------------------------------------------------------------------------------------------------- | ---------------- | --------------------------------------------------------------------------------------------------------- |
| `wildcard_subscription`        | Wildcard Subscription Available | ワイルドカードサブスクリプションは、`+` や `#` といったワイルドカードを使い、複数トピックに対して一括でサブスクライブできます。 | `true`           | `true`, `false`                                                                                           |
| `exclusive_subscription`       | Exclusive Subscription          | 排他サブスクリプションは、あるトピックに対して同時にサブスクライブできるクライアントを1つに制限します。 | `true`           | `true`, `false`                                                                                           |
| `shared_subscription`          | Shared Subscription Available   | 共有サブスクリプションは、複数のクライアントが同じトピックのサブスクリプションを共有できます。         | `true`           | `true`, `false`                                                                                           |
| `shared_subscription_strategy` |                                 | 共有サブスクリプションを持つクライアント間でメッセージを配信する戦略を定義します。<br />`shared_subscription` が `true` の場合にのみ必要です。 | `round_robin`    | - `random`（ランダムにサブスクライバーを選択）<br /><br />- `round_robin`（ラウンドロビン方式で順番に選択）<br /><br />- `sticky`（最後に選択されたサブスクライバーに常に配信、切断まで継続）<br /><br />- `hash`（`clientIds` のハッシュで選択） |

## 遅延パブリッシュ設定

遅延パブリッシュ機能は、クライアントがメッセージを指定した時間だけ遅延してトピックにパブリッシュできる機能です。特定の時間にメッセージを送信したい場合や、条件を満たしたときに送信したい場合に有用です。

このセクションでは、遅延パブリッシュの有効化方法と遅延メッセージの最大数設定を紹介します。

**設定例:**

```bash
delay {
  delayed_publish_enabled = true
  max_delayed_messages = 0
}
```

- `delayed_publish_enabled` は遅延パブリッシュ機能の有効化設定です。デフォルトは `true`、設定可能値は `true` または `false` です。
- `max_delayed_messages` は許容される遅延メッセージの最大数を設定します。デフォルトは `0` です。

## キープアライブ設定

キープアライブは 2 バイト整数で、秒単位の時間間隔を示します。MQTT クライアントと EMQX 間の接続がデータ送信がなくても維持されていることを保証する仕組みです。クライアントが EMQX に接続する際、CONNECT パケットの可変ヘッダーのキープアライブに非ゼロ値を設定すると、双方でキープアライブ機能が有効になります。キープアライブの動作詳細は [MQTT キープアライブパラメータとは？](https://www.emqx.com/en/blog/mqtt-keep-alive) を参照してください。

MQTT 5.0 プロトコルによると、キープアライブが有効なクライアントに対し、サーバーがクライアントからキープアライブ時間の1.5倍以内に MQTT コントロールパケットを受信しなければ、ネットワーク接続を切断しなければなりません。  
そのため、EMQX ではクライアントのキープアライブタイムアウトを定期的にチェックするための設定 `keepalive_multiplier` を導入しています。デフォルト値は `1.5` です。

```bash
keepalive_multiplier = 1.5
```

タイムアウト計算式は以下の通りです。

$$
\text{Keep Alive} \times \text{keepalive\_multiplier}
$$

### 動的キープアライブ調整

車載ネットワーク（T-Box）やモバイル IoT などのシナリオでは、MQTT クライアントが **アクティブ状態**（頻繁な通信）と **スリープ状態**（低消費電力の待機）を切り替えます。固定のキープアライブ値では両方の要件を同時に満たせません。

- 短いキープアライブはアクティブ時の切断検知を速くしますが、駐車中や待機時にハートビート通信が過剰になりバッテリーを消耗します。
- 長いキープアライブはスリープ時の通信を減らしますが、アクティブ時の切断検知が遅れます。

EMQX は `$SETOPTS/` システムトピック群を使ったクライアント単位の動的キープアライブ調整をサポートします。クライアントはこれらのトピックにパブリッシュして自身のブローカー側キープアライブ許容値を更新できます。特権を持つバックエンドサービスは複数クライアントをまとめて更新可能です。MQTT 接続の切断や再交渉は不要です。調整はアクティブなセッションのメモリ上にのみ適用され、永続化されません。

::: warning リスナーのマウントポイントとは非互換

動的キープアライブ調整は、マウントポイントが設定されたリスナー経由のクライアントには機能しません。EMQX はマウントポイント適用後に `$SETOPTS/` プレフィックスを判定するため、更新メッセージは通常のリテラルトピックとして処理され、クライアントにエラーは通知されません。

:::

#### 単一クライアント更新：`$SETOPTS/mqtt/keepalive`

クライアントはこのトピックにパブリッシュして自身のブローカー側キープアライブタイムアウトを更新します。EMQX は自動的にパブリッシュ元のセッションからクライアント ID を取得します。

**ペイロード:** 非負整数の文字列（秒単位）

```text
300
```

**有効範囲:** `0`～`65535` 秒。`0` はそのセッションのキープアライブチェックを無効化します。`65535` を超える値は `65535` にクランプされます。クライアントのゾーンに `mqtt.server_keepalive` が設定されている場合、実効値は両者の最小値になります。

**利用例:** 車両が駐車状態に入った際、T-Box クライアントが `$SETOPTS/mqtt/keepalive` に `300` をパブリッシュします。EMQX はブローカー側のキープアライブ許容値を 300 秒（デフォルトの `1.5×` 倍数で実効アイドルタイムアウトは 450 秒）に延長し、リモートコマンド配信のために MQTT 接続を維持します。なお、これはブローカー側のタイムアウト調整のみであり、クライアントの実際の PINGREQ 間隔は変わりません。ハートビート通信を減らすにはクライアント側もキープアライブ間隔を長くする必要があります。

#### バッチ更新：`$SETOPTS/mqtt/keepalive-bulk`

バックエンドサービスはこのトピックにパブリッシュして複数クライアントのキープアライブを一括更新できます。

**ペイロード:** JSON 配列。各要素は以下を含みます。

| フィールド   | 型      | 必須 | 説明                     |
| ------------ | ------- | ---- | ------------------------ |
| `clientid`   | 文字列  | はい | 対象 MQTT クライアント ID |
| `keepalive`  | 整数    | はい | 新しいキープアライブ間隔（秒、0～65535） |

```json
[
  { "clientid": "tbox-001", "keepalive": 300 },
  { "clientid": "tbox-002", "keepalive": 60 }
]
```

バッチ更新は非同期に処理され、クラスター対応です。EMQX は各ターゲットクライアントが存在するノードを特定し、ノード間 RPC で更新を適用します。内部キューに 10 件以上のバッチ要求が溜まると追加要求は破棄され、警告ログが記録されます。

#### アクセス制御

2 つのトピックは細かい ACL をサポートするために分けられています。

- 認証済みクライアントに `$SETOPTS/mqtt/keepalive` へのパブリッシュを許可し、各デバイスが自身のキープアライブを調整可能にします。
- `$SETOPTS/mqtt/keepalive-bulk` は信頼できるバックエンドサービスのみに制限します。

:::tip
信頼できないクライアントに `$SETOPTS/mqtt/keepalive` へのパブリッシュ権限を与えないでください。キープアライブを `0` に設定すると、そのセッションのキープアライブチェックが完全に無効化され、過度に大きな値は切断検知を遅延させてブローカーリソースを浪費させる可能性があります。
:::

これらのトピックにパブリッシュされたメッセージは EMQX によってインターセプトされ消費され、ルーティングされずサブスクライバーに届きません。

## セッション設定

MQTT におけるセッションとは、クライアントとブローカー間の接続状態を指します。EMQX ではクライアントが接続するとセッションが確立され、トピックのサブスクライブやメッセージの受信、パブリッシュが可能になります。

このセクションではセッションの設定方法を紹介します。

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

| **設定項目**                   | ダッシュボード UI           | **説明**                                                                                              | **デフォルト値**                   | **設定可能値**                     |
| ------------------------------ | --------------------------- | --------------------------------------------------------------------------------------------------- | --------------------------------- | --------------------------------- |
| `max_subscriptions`            | Max Subscriptions           | クライアントが持てる最大サブスクリプション数を設定します。                                         | `infinity`                        | `1` - `infinity`                  |
| `upgrade_qos`                  | Upgrade QoS                 | メッセージのパブリッシュ後にクライアントが QoS（サービス品質）レベルをアップグレードできるかを設定します。 | `false`（無効）                   | `true`, `false`                   |
| `max_inflight`                 | Max Inflight                | EMQX が同時に配信できる未アック済みの QoS 1 および QoS 2 メッセージの最大数を制限します。MQTT 5.0 クライアントの場合、この値とクライアントの `Receive Maximum` のうち小さい方が適用されます。詳細は [Message Queue Delivery Behavior](../../develop/design/inflight-window-and-message-queue.md#message-queue-delivery-behavior) を参照。 | `32`                             | `1` - `65535`                    |
| `retry_interval`               | Retry Interval              | クライアント接続中に未アックの QoS 1 および QoS 2 メッセージを再送信する間隔を設定します。詳細は [Message Retransmission](../../develop/design/retransmission.md#basic-configuration) を参照。 | `infinity`                       | duration または `infinity`        |
| `max_awaiting_rel`             | Max Awaiting PUBREL         | 各セッションで `PUBREL` を受信するまで保留できる QoS 2 メッセージの最大数を設定します。この制限を超えると新規 QoS 2 `PUBLISH` はエラーコード `147(0x93)` で拒否されます。<br />MQTT の `PUBREL` は QoS 2 メッセージフローの制御パケットです。 | `100`                            | `1` - `infinity`                  |
| `await_rel_timeout`            | Max Awaiting PUBREL TIMEOUT | QoS 2 メッセージの `PUBREL` 受信待ち時間を設定します。制限を超えると EMQX はパケット ID を解放し、警告ログを出します。<br />注意：`PUBREL` の有無にかかわらず QoS 2 メッセージの転送は継続されます。 | `300s`（単位：秒）               | --                              |
| `session_expiry_interval`      | Session Expiry Interval     | クライアント切断後に EMQX がセッションを保持する期間を設定します。MQTT 3.1 および 3.1.1 クライアントで `Clean Session = false` の場合に適用されます。MQTT 5.0 クライアントは CONNECT プロパティの `Session-Expiry-Interval` で独自に設定します。デフォルトのインメモリセッションストアでは、切断されたセッションはこの期間メモリに保持されます。後述の警告も参照してください。 | `2h`                             | --                              |
| `max_session_expiry_interval`  | Max Session Expiry Interval | MQTT 5.0 クライアントが CONNECT および DISCONNECT パケットの `Session-Expiry-Interval` プロパティで要求できる最大値を制限します。接続時により長い値を要求した場合はこの値にクランプされ、CONNACK の同プロパティに返されます。DISCONNECT パケットの値も同様にクランプされます。MQTT 3.1 および 3.1.1 クライアントには影響しません。EMQX 6.3.0 以降で利用可能。 | `infinity`（制限なし）           | duration または `infinity`        |
| `max_mqueue_len`              | Max Message Queue Length    | インメモリセッションのメッセージキューに保持できる最大メッセージ数を設定します。詳細は [Message Queue Delivery Behavior](../../develop/design/inflight-window-and-message-queue.md#message-queue-delivery-behavior) を参照。 | `1000`                          | `0` - `infinity`                 |
| `mqueue_priorities`           | Topic Priorities            | トピック優先度を設定します。ここでの設定は `mqueue_default_priority` の設定を上書きします。          | `disabled`（`mqueue_default_priority` の優先度を使用） | `disabled` または `1` - `255`    |
| `mqueue_default_priority`     | Default Topic Priorities    | デフォルトのトピック優先度を設定します。                                                           | `lowest`                        | `highest`、`lowest`              |
| `mqueue_store_qos0`           | Store QoS 0 Message         | クライアント切断中や接続が輻輳している間に、QoS 0 メッセージをインメモリセッションのメッセージキューに保存するかを制御します。詳細は [Message Queue Delivery Behavior](../../develop/design/inflight-window-and-message-queue.md#message-queue-delivery-behavior) を参照。 | `true`                         | `true`, `false`                 |
| `force_shutdown`              | Enable Force Shutdown       | フォースシャットダウン機能を有効にするかを設定します。メールボックスキュー長（`max_mailbox_size`）やヒープサイズ（`max_heap_size`）が指定値に達するとクライアント接続処理が強制終了されます。 | `true`                         | `true`, `false`                 |
| `force_shutdown.max_mailbox_size` | Max Mailbox Size            | フォースシャットダウンをトリガーする最大メールボックスキュー長を設定します。                        | `1000`                         | `1` - `infinity`                |
| `force_shutdown.max_heap_size`    | Max Heap Size               | フォースシャットダウンをトリガーする最大ヒープサイズを設定します。                                  | `32MB`                         | --                              |
| `force_gc`                    | --                          | 指定したメッセージ数（`count`）または受信バイト数（`bytes`）に達した場合に強制ガベージコレクションを有効にするかを設定します。 | `true`                         | `true`, `false`                 |
| `force_gc.count`              | --                          | 強制ガベージコレクションをトリガーする受信メッセージ数を設定します。                               | `16000`                        | `0` - `infinity`                |
| `force_gc.bytes`              | --                          | 強制ガベージコレクションをトリガーする受信バイト数を設定します。                                   | `16MB`（単位：MB）             | --                              |

::: warning 切断されたセッションのメモリコスト

デフォルトのインメモリセッションストアでは、セッションの有効期限がゼロより大きい場合、クライアント切断時にセッションは削除されません。EMQX はクライアントの再接続または有効期限経過まで、セッション、サブスクリプション、メッセージキューをメモリに保持します。クライアントが有効期限内に再接続しない場合、ノード上の切断セッション数はクライアント切断率と有効期限の積に概ね比例します。

切断後のセッション保持は永続セッションの意図した動作です。ワークロードに応じて十分なメモリを割り当てるか、セッション状態をディスクに保存する [durable sessions](../durability/durability_introduction.md) の利用を検討してください。

:::

:::tip

MQTT 設定をダッシュボードで行う場合は、ダッシュボード左メニューの **Management** -> **MQTT Settings** をクリックしてください。ダッシュボードで設定した場合、設定ファイルの同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定すると、ダッシュボードからの変更が一時的なものとなり、EMQX 再起動時に失われるためです。

:::

::: tip

EMQX はより詳細なカスタマイズが可能な設定項目も提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) を参照してください。

:::
