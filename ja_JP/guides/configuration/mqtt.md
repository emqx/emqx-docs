# MQTT 設定

[MQTT](https://mqtt.org/) は、IoT（Internet of Things）デバイスを接続するための軽量なパブリッシュ／サブスクライブ型メッセージングプロトコルです。EMQX は MQTT 3.1、3.1.1、および 5.0 をサポートしています。

本ページでは、EMQX における MQTT プロトコルの動作設定について、パケット検証や制限、サブスクリプション、遅延パブリッシュ、キープアライブ処理、セッション設定などを説明します。

## 基本的な MQTT 設定

このセクションでは、パケットサイズ、クライアントIDの長さ、トピックレベル数、QoS（サービス品質）、トピックエイリアス、保持設定など、MQTT プロトコルの動作を決定する設定項目を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも確認できます。ダッシュボードで設定した場合、設定ファイル内の同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定した場合、ダッシュボードでの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

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

| **設定項目**               | ダッシュボード UI           | **説明**                                                                 | **デフォルト値** | **選択可能な値**          |
|----------------------------|----------------------------|--------------------------------------------------------------------------|------------------|---------------------------|
| `max_packet_size`           | Max Packet Size            | MQTT クライアントと EMQX 間でメッセージ送信に使用される MQTT パケットの最大サイズを設定します。 | `1MB`            |                           |
| `max_connect_packet_size`   | Max CONNECT Packet Size    | CONNECT パケットの最大サイズを設定します。CONNECT パケットは `max_connect_packet_size` と `max_packet_size` の両方の制限内である必要があります。どちらかを超えると EMQX は接続を切断します。 | `1MB`            |                           |
| `max_clientid_len`          | Max Client ID Length       | MQTT クライアント ID の最大長を設定します。過度に長いクライアント ID の使用を防止できます。 | `65535`          | `23` - `65535`            |
| `max_topic_levels`          | Max Topic Levels           | MQTT トピックの階層レベル数の最大値を設定します。                         | `128`            | `1` - `35`                |
| `max_qos_allowed`           | Max QoS                   | MQTT メッセージで許可される最大の QoS（サービス品質）レベルを設定します。  |                  |                           |
| `max_topic_alias`           | Max Topic Alias            | トピックエイリアスは、トピック名の代わりに短いエイリアスを使うことで MQTT パケットサイズを削減する仕組みです。MQTT セッションで使用可能な最大トピックエイリアス数を設定します。 | `65535`          | `1` - `65535`             |
| `retain_available`          | Retain Available           | 保持メッセージ機能を有効にするかどうかを設定します。保持メッセージは、トピックに最後にパブリッシュされたメッセージを保存し、新規サブスクライバーが最新メッセージを受け取れるようにします。 | `true`           | `true`, `false`           |
| `strict_mode`               | Strict Mode                | 追加のプロトコル準拠チェックを有効にするかどうかを設定します。チェックに失敗したパケットはクライアント接続を切断します。 | `true`           | `true`, `false`           |

### 厳格な MQTT パケット検証

EMQX 6.3.0 以降、`strict_mode = true` がデフォルトで有効になっており、以下のような不正な MQTT パケットを拒否します。

- 不正な MQTT 固定ヘッダーフラグの組み合わせ
- MQTT 3.1.1 の CONNECT パケットで、Username Flag なしに Password Flag が立っている場合
- クライアント ID、トピック名、ユーザー名、パスワード、Will トピック、MQTT 5.0 の文字列プロパティなどに不正な UTF-8 文字列（ヌル文字や禁止制御文字を含む）が含まれる場合
- MQTT プロトコルで非ゼロが要求されるパケット識別子がゼロの場合

不正パケット検出時は、EMQX はクライアント接続を切断し、`info` レベルで `frame_parse_error` ログに詳細理由を記録します。MQTT 5.0 クライアントには可能な場合、理由コード `0x81`（Malformed Packet）付きの CONNACK または DISCONNECT パケットを送信します。MQTT 3.1 および 3.1.1 クライアントは理由コードなしで切断されます。

既存クライアントがこれらのプロトコル要件に準拠しない場合、一時的に厳格モードの検証を無効化できます。

```bash
mqtt.strict_mode = false
```

特定のレガシークライアントのみで無効化したい場合は、ゾーンを設定し専用リスナーに割り当てます。

```bash
zones.legacy_clients {
  mqtt.strict_mode = false
}

listeners.tcp.legacy {
  bind = "0.0.0.0:1884"
  zone = legacy_clients
}
```

他のリスナー経由のクライアントは引き続き厳格検証が適用されます。ゾーンの詳細は[ゾーンオーバーライド](./configuration.md#zone-override)を参照してください。

## サブスクリプション設定

EMQX におけるサブスクリプションとは、クライアントが EMQX のトピックにサブスクライブするプロセスを指します。クライアントがトピックをサブスクライブすると、そのトピックにパブリッシュされたメッセージを受信できます。

ここでは、共有サブスクリプション、ワイルドカードサブスクリプション、排他サブスクリプションの設定方法を紹介します。

:::tip

対応する設定項目は EMQX ダッシュボードの **Management** -> **MQTT Settings** -> **General** でも確認できます。ダッシュボードで設定した場合、設定ファイル内の同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定した場合、ダッシュボードでの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

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

| **設定項目**                  | ダッシュボード UI               | **説明**                                                                                     | **デフォルト値** | **選択可能な値**                                                                                   |
|-------------------------------|-------------------------------|----------------------------------------------------------------------------------------------|------------------|--------------------------------------------------------------------------------------------------|
| `wildcard_subscription`       | Wildcard Subscription Available | ワイルドカードサブスクリプションは、`+` や `#` といったワイルドカードを使い、複数トピックを単一のサブスクリプションで受信可能にします。 | `true`           | `true`, `false`                                                                                   |
| `exclusive_subscription`      | Exclusive Subscription          | 排他サブスクリプションは、同時に1つのクライアントのみがトピックをサブスクライブ可能にします。 | `true`           | `true`, `false`                                                                                   |
| `shared_subscription`         | Shared Subscription Available   | 共有サブスクリプションは、複数のクライアントでトピックのサブスクリプションを共有可能にします。 | `true`           | `true`, `false`                                                                                   |
| `shared_subscription_strategy` |                               | 共有サブスクリプションでメッセージを複数クライアントに配信する際の分配戦略を設定します。<br />`shared_subscription` が `true` の場合のみ必要です。 | `round_robin`    | - `random`（ランダムにサブスクライバーへ配信）<br /><br />- `round_robin`（順番にサブスクライバーを選択）<br /><br />- `sticky`（最後に選択されたサブスクライバーへ常に配信、切断まで継続）<br /><br />- `hash`（`clientIds` のハッシュでサブスクライバーを選択） |

## 遅延パブリッシュ設定

遅延パブリッシュ機能は、クライアントがメッセージのパブリッシュを指定した時間だけ遅延させることを可能にします。この機能は、特定の時間にメッセージを送信したい場合や、特定条件が満たされた時にメッセージを送信したい場合に有用です。

ここでは、遅延パブリッシュの有効化方法と遅延メッセージの最大数設定を紹介します。

**設定例:**

```bash
delay {
  delayed_publish_enabled = true
  max_delayed_messages = 0
}
```

- `delayed_publish_enabled` は遅延パブリッシュ機能の有効化設定です。デフォルトは `true`、選択肢は `true` または `false` です。
- `max_delayed_messages` は許容される遅延メッセージの最大数を設定します。デフォルトは `0` です。

## キープアライブ設定

キープアライブは 2 バイトの整数で、秒単位の時間間隔を示します。MQTT クライアントと EMQX 間の接続がデータ送信がなくても維持されていることを保証する仕組みです。クライアントが EMQX に接続する際、CONNECT パケットの可変ヘッダーのキープアライブ変数に 0 以外の値を設定すると、双方でキープアライブ機能が有効になります。詳細は [MQTT キープアライブパラメータとは？](https://www.emqx.com/en/blog/mqtt-keep-alive) を参照してください。

MQTT 5.0 プロトコルによると、キープアライブを有効にしたクライアントに対し、サーバーはキープアライブ時間の 1.5 倍以内にクライアントから MQTT 制御パケットを受信しなければ、ネットワーク接続を切断しなければなりません。  
そのため、EMQX ではクライアントのキープアライブタイムアウト状態を定期的にチェックするために `keepalive_multiplier` 設定を導入しています。デフォルト値は `1.5` です。

```bash
keepalive_multiplier = 1.5
```

タイムアウト計算式は以下の通りです。

$$
\text{Keep Alive} \times \text{keepalive\_multiplier}
$$

### 動的キープアライブ調整

車載ネットワーク（T-Box）やモバイル IoT などのシナリオでは、MQTT クライアントが**アクティブ状態**（頻繁な通信）と**スリープ状態**（低消費電力アイドル）を切り替えます。固定のキープアライブ値では両方の状態を満たせません。

- 短いキープアライブはアクティブ時の切断検知を高速化しますが、駐車中やアイドル時にハートビート通信が過剰になりバッテリー消費が増えます。
- 長いキープアライブはスリープ時の通信量を削減しますが、アクティブ時の切断検知が遅延します。

EMQX は `$SETOPTS/` システムトピックを使ったクライアントごとの動的キープアライブ調整をサポートしています。クライアント自身がこれらのトピックにパブリッシュしてブローカー側のキープアライブ許容値を更新でき、あるいは権限のあるバックエンドサービスが複数クライアントを一括更新できます。MQTT 接続を切断したり再交渉したりすることなく、アクティブセッションに対してメモリ上でのみ適用され、永続化はされません。

::: warning リスナーのマウントポイントとの非互換性

動的キープアライブ調整は、[マウントポイント](./listener.md#mountpoint)が設定されたリスナー経由のクライアントでは動作しません。EMQX はマウントポイントを `$SETOPTS/` プレフィックスのマッチ前に適用するため、更新はマウントされたリテラルトピックへの通常メッセージとしてルーティングされ、クライアントにはエラーは通知されません。

:::

#### 単一クライアント更新：`$SETOPTS/mqtt/keepalive`

クライアントはこのトピックにパブリッシュすることで、自身のブローカー側キープアライブタイムアウトを更新します。EMQX は自動的にパブリッシュ元のセッションからクライアント ID を取得します。

**ペイロード:** 非負整数（秒）の文字列

```text
300
```

**有効範囲:** `0`～`65535` 秒。`0` はそのセッションのキープアライブチェックを無効化します。`65535` を超える値は `65535` にクランプされます。クライアントのゾーンに `mqtt.server_keepalive` が設定されている場合、実効値は両者の最小値となります。

**利用例:** 車両が駐車状態に入る際、T-Box クライアントが `$SETOPTS/mqtt/keepalive` に `300` をパブリッシュします。EMQX はブローカー側のキープアライブ許容時間を 300 秒（デフォルトの `1.5×` 倍数を考慮して実効アイドルタイムアウトは 450 秒）に延長し、リモートコマンド受信のために MQTT 接続を維持します。なおこれはブローカー側のタイムアウト調整のみであり、クライアントの実際の PINGREQ 間隔は変わりません。ハートビート通信量削減にはクライアント側もキープアライブ間隔を延長する必要があります。

#### 一括更新：`$SETOPTS/mqtt/keepalive-bulk`

バックエンドサービスはこのトピックにパブリッシュして複数クライアントのキープアライブを一括更新できます。

**ペイロード:** JSON 配列。各要素は以下を含みます。

| フィールド   | 型      | 必須 | 説明                         |
|--------------|---------|------|------------------------------|
| `clientid`   | 文字列  | はい | 対象の MQTT クライアント識別子 |
| `keepalive`  | 整数    | はい | 新しいキープアライブ間隔（秒、0～65535） |

```json
[
  { "clientid": "tbox-001", "keepalive": 300 },
  { "clientid": "tbox-002", "keepalive": 60 }
]
```

一括更新は非同期処理され、クラスター対応です。EMQX は対象クライアントをホストするノードを特定し、ノード間 RPC で更新を適用します。内部キューに 10 件以上のリクエストが溜まると追加リクエストは破棄され、警告ログが記録されます。

#### アクセス制御

2 つのトピックは細かい ACL 設定を可能にするために分けられています。

- 認証済みクライアントが `$SETOPTS/mqtt/keepalive` にパブリッシュできるようにし、各デバイスが自身のキープアライブを調整可能にします。
- `$SETOPTS/mqtt/keepalive-bulk` は信頼されたバックエンドサービスのみに制限します。

:::tip
信頼できないクライアントに `$SETOPTS/mqtt/keepalive` へのパブリッシュ権限を与えないでください。キープアライブを `0` に設定するとそのセッションのキープアライブチェックが完全に無効化され、過度に大きな値は切断検知遅延を招き、ブローカーリソースを消費し続ける恐れがあります。
:::

これらのトピックにパブリッシュされたメッセージは EMQX によってインターセプトされ、ルーティング前に消費されます。サブスクライバーには配信されません。

## セッション設定

MQTT におけるセッションとは、クライアントとブローカー間の接続状態を指します。EMQX では、クライアントが接続すると、トピックのサブスクライブやメッセージの受信、パブリッシュが可能なセッションが確立されます。

ここでは、セッションの設定方法を紹介します。

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

| **設定項目**                     | ダッシュボード UI           | **説明**                                                                                                  | **デフォルト値**                         | **選択可能な値**                     |
|---------------------------------|----------------------------|-----------------------------------------------------------------------------------------------------------|------------------------------------------|------------------------------------|
| `max_subscriptions`             | Max Subscriptions          | クライアントが持てる最大サブスクリプション数を設定します。                                               | `infinity`                               | `1` - `infinity`                   |
| `upgrade_qos`                   | Upgrade QoS                | メッセージパブリッシュ後に QoS（サービス品質）レベルをアップグレード可能にするか設定します。               | `false`（無効）                          | `true`, `false`                    |
| `max_inflight`                  | Max Inflight               | EMQX が同時に配信可能な未アック済み QoS 1 および QoS 2 メッセージの最大数を制限します。MQTT 5.0 クライアントでは、クライアントの `Receive Maximum` とこの値の小さい方が適用されます。詳細は[メッセージキュー配信動作](../../develop/design/inflight-window-and-message-queue.md#message-queue-delivery-behavior)を参照してください。 | `32`                                     | `1` - `65535`                     |
| `retry_interval`                | Retry Interval             | クライアント接続中に未アックの QoS 1 および QoS 2 メッセージを再送信する間隔を設定します。詳細は[メッセージ再送信](../../develop/design/retransmission.md#basic-configuration)を参照してください。 | `infinity`                               | duration または `infinity`         |
| `max_awaiting_rel`              | Max Awaiting PUBREL        | セッション内で `PUBREL` を受信またはタイムアウトするまで保留できる QoS 2 メッセージの最大数を設定します。この上限に達すると新規 QoS 2 `PUBLISH` はエラーコード `147(0x93)` で拒否されます。<br />MQTT の `PUBREL` は QoS 2 メッセージフローの制御パケットです。 | `100`                                    | `1` - `infinity`                  |
| `await_rel_timeout`             | Max Awaiting PUBREL TIMEOUT | QoS 2 メッセージの `PUBREL` 受信待機時間を設定します。タイムアウト後、EMQX はパケットIDを解放し、警告ログを出力します。<br />注意: EMQX は `PUBREL` の有無にかかわらず QoS 2 メッセージの転送を継続します。 | `300s`（単位: 秒）                       | --                                 |
| `session_expiry_interval`       | Session Expiry Interval    | クライアント切断後に EMQX がセッションを保持する期間を設定します。`Clean Session = false` で接続する MQTT 3.1 および 3.1.1 クライアントに適用されます。MQTT 5.0 クライアントは CONNECT プロパティの `Session-Expiry-Interval` で独自に設定し、`max_session_expiry_interval` の制限を受けます。<br />デフォルトのインメモリセッションストアでは、切断されたセッションはこの期間メモリ上に保持されます。以下の警告も参照してください。 | `2h`                                     | --                                 |
| `max_session_expiry_interval`   | Max Session Expiry Interval | MQTT 5.0 クライアントが CONNECT および DISCONNECT パケットの `Session-Expiry-Interval` プロパティで要求できる最大値を制限します。接続時に長い値を要求された場合、EMQX はこの値にクランプし、CONNACK の同プロパティにクランプ後の値を返します。DISCONNECT での長い値も同様にクランプされます。MQTT 3.1 および 3.1.1 クライアントには影響しません。<br />EMQX 6.3.0 以降で利用可能です。 | `infinity`（無制限）                     | duration または `infinity`         |
| `max_mqueue_len`                | Max Message Queue Length   | インメモリセッションのメッセージキューに保持できる最大メッセージ数を設定します。詳細は[メッセージキュー配信動作](../../develop/design/inflight-window-and-message-queue.md#message-queue-delivery-behavior)を参照してください。 | `1000`                                   | `0` - `infinity`                  |
| `mqueue_priorities`             | Topic Priorities           | トピック優先度を設定します。ここでの設定は `mqueue_default_priority` の設定を上書きします。                      | `disabled` <br />セッションは `mqueue_default_priority` の優先度を使用 | `disabled` または `1` - `255`      |
| `mqueue_default_priority`       | Default Topic Priorities   | デフォルトのトピック優先度を設定します。                                                        | `lowest`                                 | `highest`, `lowest`               |
| `mqueue_store_qos0`             | Store QoS 0 Message        | クライアント切断時や接続が輻輳している場合に、QoS 0 メッセージをインメモリセッションのメッセージキューに保存するか制御します。詳細は[メッセージキュー配信動作](../../develop/design/inflight-window-and-message-queue.md#message-queue-delivery-behavior)を参照してください。 | `true`                                   | `true`, `false`                  |
| `force_shutdown`                | Enable Force Shutdown      | フォースシャットダウン機能の有効化を設定します。メールボックスキュー長（`max_mailbox_size`）またはヒープサイズ（`max_heap_size`）が指定値に達すると、クライアント接続処理を強制終了します。 | `true`                                   | `true`, `false`                  |
| `force_shutdown.max_mailbox_size` | Max Mailbox Size           | フォースシャットダウンをトリガーするメールボックスキューの最大長を設定します。                           | `1000`                                   | `1` - `infinity`                 |
| `force_shutdown.max_heap_size`  | Max Heap Size              | フォースシャットダウンをトリガーするヒープサイズの最大値を設定します。                                 | `32MB`                                   | --                               |
| `force_gc`                     | --                         | 指定したメッセージ数（`count`）または受信バイト数（`bytes`）に達した場合に強制ガベージコレクションを有効化します。 | `true`                                   | `true`, `false`                  |
| `force_gc.count`               | --                         | 強制ガベージコレクションをトリガーする受信メッセージ数を設定します。                                  | `16000`                                  | `0` - `infinity`                 |
| `force_gc.bytes`               | --                         | 強制ガベージコレクションをトリガーする受信バイト数を設定します。単位は MB です。                        | `16MB`                                   | --                               |

::: warning 切断されたセッションのメモリコスト

デフォルトのインメモリセッションストアでは、セッションの有効期限が 0 より大きい場合、クライアント切断時にセッションは削除されません。EMQX はセッション、サブスクリプション、メッセージキューをメモリ上に保持し、クライアントの再接続または有効期限切れまで維持します。クライアントが再接続しない場合、ノード上の切断セッション数はクライアントの切断率と有効期限の積に概ね比例します。

切断後のセッション保持は永続セッションの意図された動作です。ワークロードに応じて十分なメモリを割り当てるか、セッション状態をディスクに保存する[Durable Sessions](../durability/durability_introduction.md)の利用を検討してください。

:::

:::tip

MQTT 設定をダッシュボードで行う場合は、ダッシュボード左メニューの **Management** -> **MQTT Settings** をクリックしてください。ダッシュボードで設定した場合、設定ファイル内の同じ項目より優先されます。  
設定ファイルから MQTT を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定した場合、ダッシュボードでの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

:::

::: tip

EMQX はさらに多くの設定項目を提供しており、カスタマイズニーズに対応しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) を参照してください。

:::
