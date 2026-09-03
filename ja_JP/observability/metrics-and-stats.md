# 統計とメトリクス

EMQX はメトリクス監視機能を提供しており、これにより運用・保守担当者は現在のサービス状況を監視し、システムの異常をトラブルシューティングできます。

EMQX は監視状態を「統計」と「メトリクス」に分類しています。

- 統計は整数型のゲージで、メトリクスが要求された時点の単一の値を返します。
- メトリクスは整数型のカウンターで、送受信されたバイト数やメッセージ数などの単純な増減を計測します。

EMQX はユーザーに複数の統計およびメトリクスの閲覧方法を提供しています。最も直接的には、EMQX ダッシュボード上でこれらのデータを確認できます。ダッシュボードへのアクセスが不便な場合は、[REST API](#request-monitoring-status-via-rest-api) や [システムトピック](#get-monitoring-status-via-system-topics) メッセージを通じてデータを取得することも可能です。さらに、監視機能をお使いの監視システムと簡単に統合することもできます。詳細は [Prometheus との統合](./prometheus.md) をご覧ください。

## ダッシュボードで統計を確認する

EMQX ダッシュボードの左ナビゲーションメニューから **Monitoring** -> **Cluster Overview** をクリックします。**Cluster Overview** ページで **Nodes** タブを選択し、ノード名をクリックすると右側に統計の詳細が表示されます。

<img src="./assets/node-statistics-ee.png" alt="ノード統計情報" style="zoom:45%;" />

統計には現在値と過去の最大値の2つの値が含まれます。例えば、現在のサブスクリプション数と過去の最大サブスクリプション数です。以下は EMQX の統計一覧です。


| 統計名                     | 説明                                                         |
| -------------------------- | ------------------------------------------------------------ |
| connections.count          | 現在の接続数                                                 |
| connections.max            | 過去の最大接続数                                             |
| live_connections.count     | 現在のライブ接続数                                           |
| live_connections.max       | 過去の最大ライブ接続数                                       |
| channels.count             | `sessions.count` と同じ                                     |
| channels.max               | `sessions.max` と同じ                                       |
| sessions.count             | 現在のセッション数                                           |
| sessions.max               | 過去の最大セッション数                                       |
| topics.count               | 現在のトピック数                                           |
| topics.max                 | 過去の最大トピック数                                       |
| suboptions.count           | `subscriptions.count` と同じ                                |
| suboptions.max             | `subscriptions.max` と同じ                                  |
| subscribers.count          | 現在のサブスクライバー数                                     |
| subscribers.max            | 過去の最大サブスクライバー数                                 |
| subscriptions.count        | 現在のサブスクリプション数（共有サブスクリプションを含む） |
| subscriptions.max          | 過去の最大サブスクリプション数                               |
| subscriptions.shared.count | 現在の共有サブスクリプション数                               |
| subscriptions.shared.max   | 過去の最大共有サブスクリプション数                           |
| retained.count             | 現在の保持メッセージ数                                       |
| retained.max               | 過去の最大保持メッセージ数                                   |
| delayed.count              | 現在の遅延メッセージ数                                       |
| delayed.max                | 過去の最大遅延メッセージ数                                   |

## ダッシュボードでメトリクスを確認する

EMQX ダッシュボードの左ナビゲーションメニューから **Monitoring** -> **Cluster Overview** をクリックします。**Cluster Overview** ページで **Metrics** タブを選択すると、クラスターまたは特定ノードのランタイムメトリクスを確認できます。

EMQX のメトリクスはカウンターとして実装されており、ノード起動以降の特定イベントの累積発生回数を記録します。これらのメトリクスは、システムの挙動観察、負荷パターンの評価、問題のトラブルシューティングに役立ちます。

ダッシュボード上のメトリクスは以下のカテゴリに分類されています：

- **接続およびセッションメトリクス**：クライアント接続、セッション、アクセス制御イベント
- **ルールおよびアクション（シンク）メトリクス**：ルールマッチングとデータ統合のためのアクション実行
- **メッセージングメトリクス**：バイト数、パケット数、メッセージ数、配信統計

### 接続およびセッションメトリクス

このセクションでは、クラスターまたはノードのイベント関連メトリクスを表示します。内容は [クライアント接続](#connections)、[接続セッション](#sessions)、[クライアントアクセス](#access) を含みます。

<img src="./assets/dashboard-event-metrics-ee.png" alt="ダッシュボードイベントメトリクス" style="zoom:50%;" />

#### Connections

| メトリクス             | 説明                                                         |
| ---------------------- | ------------------------------------------------------------ |
| client.connack         | クライアントが受信した接続確認（`CONNACK`）メッセージの数   |
| client.connect         | クライアントからの接続要求数（成功・失敗を含む）             |
| client.connected       | 成功したクライアント接続数                                   |
| client.disconnected    | クライアントの切断数（正常・異常切断を含む）                 |
| client.subscribe       | 成功したサブスクライブ数                                     |
| client.unsubscribe     | 成功したサブスクリプション解除数                             |

#### Sessions

| メトリクス            | 説明                                                         |
| --------------------- | ------------------------------------------------------------ |
| session.created       | 作成されたセッション数                                       |
| session.discarded     | 廃棄されたセッション数                                       |
| session.resumed       | 再開されたセッション数                                       |
| session.takenover     | 引き継がれたセッション数                                     |
| session.terminated    | 終了されたセッション数                                       |

#### Access

| メトリクス                     | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| authorization.allow            | クライアント認可成功の総数。キャッシュヒット（取得した認可結果）とポリシールールにマッチした認可要求の合計。 |
| authorization.deny             | クライアント認可失敗の総数。キャッシュヒット（取得した認可結果）とポリシールールにマッチしなかった認可要求の合計。 |
| authorization.matched.allow    | ルールにより認可成功したクライアント数                       |
| authorization.matched.deny     | ルールにより認可拒否されたクライアント数                     |
| authorization.nomatch          | どのルールにもマッチしなかった認可要求数                     |
| authorization.cache_hit        | キャッシュにより認可結果（許可または拒否）を得たクライアント数 |
| authorization.superuser        | スーパーユーザーとして認可されたクライアント数               |
| client.auth.anonymous          | 匿名ログインしたクライアント数                               |
| client.authenticate            | 認証がトリガーされた回数                                     |
| client.authorize               | 認可がトリガーされた回数                                     |

### ルールおよびアクション（シンク）

このセクションはデータ統合に関連するメトリクスを提供し、ルールのマッチ回数やアクション（シンク）の実行回数を把握できます。

これらのメトリクスは、ルールの有効性評価、下流データフローの監視、全体的なデータ統合利用状況の評価に役立ちます。

![dashboard-integration-metrics](./assets/rule-action-metrics.png)

#### ルール

| メトリクス       | 説明                                                         |
| ---------------- | ------------------------------------------------------------ |
| rules.matched    | メッセージやイベントがルールエンジンを通過する際にルールが正常にマッチした回数 |

#### アクション（シンク）

| メトリクス          | 説明                                                         |
| ------------------- | ------------------------------------------------------------ |
| actions.executed    | ルールマッチにより実行されたアクション（シンク）の回数       |
| actions.messages   | アクション実行で処理されたメッセージ数。一度のアクション実行で複数メッセージを処理するため、この値は `actions.executed` 以上となる。 |

### メッセージング

**Metrics** ページをスクロールすると、[バイト数](#bytes)、[パケット数](#packets)、[メッセージ](#message-publish-packet)、[配信](#delivery) に関するメトリクスが表示されます。

<img src="./assets/dashboard-messaging-metrics-ee.png" alt="ダッシュボードメッセージングメトリクス" style="zoom:50%;" />

#### バイト数

| メトリクス         | 説明                         |
| ------------------ | ---------------------------- |
| bytes.received     | 受信したバイト数             |
| bytes.sent         | 送信したバイト数             |

#### パケット数

| メトリクス                      | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| packets.received               | 受信したパケット数                                           |
| packets.sent                   | 送信したパケット数                                           |
| packets.connect.received       | 受信した CONNECT パケット数                                  |
| packets.connack.auth_error     | 理由コード 0x86 および 0x87 を含む送信した CONNACK メッセージ数 |
| packets.connack.error          | 理由コードが 0x00 以外の送信した CONNACK パケット数。この値は `packets.connack.auth_error` 以上。 |
| packets.connack.sent           | 送信した CONNACK パケット数                                  |
| packets.publish.received       | 受信した PUBLISH パケット数                                  |
| packets.publish.sent           | 送信した PUBLISH パケット数                                  |
| packets.publish.inuse          | パケット識別子が使用中の受信 PUBLISH パケット数             |
| packets.publish.auth_error     | ACL チェックに失敗した受信 PUBLISH パケット数                |
| packets.publish.error          | パブリッシュできなかった受信 PUBLISH パケット数              |
| packets.puback.received        | 受信した PUBACK パケット数                                   |
| packets.puback.sent            | 送信した PUBACK パケット数                                   |
| packets.puback.inuse           | 識別子が使用中の受信 PUBACK メッセージ数                     |
| packets.puback.missed          | 不明な識別子の受信 PUBACK パケット数                         |
| packets.pubrec.received        | 受信した PUBREC パケット数                                   |
| packets.pubrec.sent            | 送信した PUBREC パケット数                                   |
| packets.pubrec.inuse           | 識別子が使用中の受信 PUBREC メッセージ数                     |
| packets.pubrec.missed          | 不明な識別子の受信 PUBREC パケット数                         |
| packets.pubrel.received        | 受信した PUBREL パケット数                                   |
| packets.pubrel.sent            | 送信した PUBREL パケット数                                   |
| packets.pubrel.missed          | 不明な識別子の受信 PUBREL パケット数                         |
| packets.pubcomp.received       | 受信した PUBCOMP パケット数                                  |
| packets.pubcomp.sent           | 送信した PUBCOMP パケット数                                  |
| packets.pubcomp.inuse          | 識別子が使用中の受信 PUBCOMP メッセージ数                    |
| packets.pubcomp.missed         | 失われた PUBCOMP パケット数                                  |
| packets.subscribe.received     | 受信した SUBSCRIBE パケット数                                |
| packets.subscribe.error        | 失敗したサブスクライブを含む受信 SUBSCRIBE パケット数        |
| packets.subscribe.auth_error   | ACL チェックに失敗した受信 SUBACK パケット数                  |
| packets.suback.sent            | 送信した SUBACK パケット数                                   |
| packets.unsubscribe.received   | 受信した UNSUBSCRIBE パケット数                              |
| packets.unsubscribe.error      | 失敗したサブスクリプション解除を含む受信 UNSUBSCRIBE パケット数 |
| packets.unsuback.sent          | 送信した UNSUBACK パケット数                                 |
| packets.pingreq.received       | 受信した PINGREQ パケット数                                  |
| packets.pingresp.sent          | 送信した PINGRESP パケット数                                 |
| packets.disconnect.received    | 受信した DISCONNECT パケット数                               |
| packets.disconnect.sent        | 送信した DISCONNECT パケット数                               |
| packets.auth.received          | 受信した AUTH パケット数                                     |
| packets.auth.sent              | 送信した AUTH パケット数                                     |

#### メッセージ（PUBLISH パケット）

| メトリクス                       | 説明                                                         |
| -------------------------------- | ------------------------------------------------------------ |
| messages.acked                  | アック（ACK）されたメッセージ数                              |
| messages.delayed                | EMQX によって遅延パブリッシュのために保存されているメッセージ数 |
| messages.delivered              | EMQX 内部でサブスクリプション処理に転送されたメッセージ数    |
| messages.dropped                | サブスクリプション処理に転送される前に EMQX によって破棄されたメッセージ総数 |
| messages.dropped.no_subscribers | サブスクライバーがいないため破棄されたメッセージ数            |
| messages.dropped.await_pubrel_timeout | PUBREL 応答待ちタイムアウトにより破棄されたメッセージ数     |
| messages.dropped.quota_exceeded | クォータ超過（通常は接続数）により破棄されたメッセージ数      |
| messages.dropped.receive_maximum | Receive Maximum に達したため破棄されたメッセージ数           |
| messages.forward                | 他ノードに転送されたメッセージ数                             |
| messages.publish                | システムメッセージを除くパブリッシュされたメッセージ数       |
| messages.qos0.received          | クライアントから受信した QoS 0 メッセージ数                   |
| messages.qos1.received          | クライアントから受信した QoS 1 メッセージ数                   |
| messages.qos2.received          | クライアントから受信した QoS 2 メッセージ数                   |
| messages.qos0.sent              | クライアントに送信した QoS 0 メッセージ数                     |
| messages.qos1.sent              | クライアントに送信した QoS 1 メッセージ数                     |
| messages.qos2.sent              | クライアントに送信した QoS 2 メッセージ数                     |
| messages.received               | クライアントから受信したメッセージ数。`messages.qos0.received`、`messages.qos1.received`、`messages.qos2.received` の合計 |
| messages.sent                   | クライアントに送信したメッセージ数。`messages.qos0.sent`、`messages.qos1.sent`、`messages.qos2.sent` の合計 |

#### 配信

| メトリクス                     | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| delivery.dropped              | 配信中に破棄されたメッセージの総数                           |
| delivery.dropped.expired      | メッセージの有効期限切れにより配信中に破棄されたメッセージ数 |
| delivery.dropped.no_local     | `No Local` サブスクリプションオプションにより破棄されたメッセージ数 |
| delivery.dropped.qos0_msg     | メッセージキューが満杯のため配信中に破棄された QoS 0 メッセージ数 |
| delivery.dropped.queue_full   | メッセージキューが満杯のため配信中に破棄された非ゼロ QoS レベルのメッセージ数 |
| delivery.dropped.too_large    | 長さ制限を超えたため配信中に破棄されたメッセージ数           |

## REST API で監視状態を取得する

API を通じてメトリクスや統計を取得することも可能です。UI の左ナビゲーションメニューから **Metrics** をクリックすると、この API リクエストを実行できます。EMQX API の利用方法については [REST API](../admin/api.md) を参照してください。

<img src="./assets/metrics-api-doc.png" alt="メトリクスAPIドキュメント" style="zoom:35%;" />

## システムトピックで監視状態を取得する

EMQX は稼働状況、メッセージ統計、クライアントのオンライン・オフラインイベントに関するメッセージをシステムトピックを通じて定期的にパブリッシュします。クライアントはトピック名の前に `$SYS/` プレフィックスを付けてシステムトピックをサブスクライブできます。システムトピックの種類については [システムトピック](./mqtt-system-topics.md) をご覧ください。

システムトピックの設定はダッシュボードで行えます。左ナビゲーションメニューから **Management** -> **MQTT Settings** をクリックし、**System Topic** タブを選択してください。

<img src="./assets/system-topic-setting.png" alt="システムトピック設定" style="zoom:40%;" />

- **Messages publish interval**：`$SYS` トピック送信の間隔を設定します。
- **Heartbeat interval**：ハートビートメッセージ送信の間隔を設定します。
- **Client connected notification**：デフォルトで有効。クライアント接続時のイベントメッセージがパブリッシュされます。
- **Client disconnected notification**：デフォルトで有効。クライアント切断時のイベントメッセージがパブリッシュされます。
- **Client subscribed notification**：デフォルトで無効。有効にすると、クライアントがトピックをサブスクライブした際のイベントメッセージがパブリッシュされます。
- **Client unsubscribed notification**：デフォルトで無効。有効にすると、クライアントがトピックのサブスクリプションを解除した際のイベントメッセージがパブリッシュされます。
