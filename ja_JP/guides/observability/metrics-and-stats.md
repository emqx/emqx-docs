# 統計とメトリクス

EMQXはメトリクス監視機能を提供しており、運用・保守担当者はこれに基づいて現在のサービス状況を監視し、システムの不具合をトラブルシューティングできます。

EMQXは監視状態を統計（Statistics）とメトリクス（Metrics）に分類しています。

- 統計は整数型のゲージで、メトリクスが要求された時点の単一の値を返します。
- メトリクスは整数型のカウンターで、送受信されたバイト数やメッセージ数のような単純な増減を測定します。

EMQXは統計とメトリクスを閲覧するための複数の方法を提供しています。最も直接的には、EMQXダッシュボードでこれらのデータを確認できます。ダッシュボードへのアクセスが不便な場合は、[REST API](#request-monitoring-status-via-rest-api)や[システムトピック](#get-monitoring-status-via-system-topics)のメッセージを通じてもデータを取得可能です。さらに、監視機能を独自の監視システムと簡単に統合することもできます。詳細は[Prometheusとの統合](./prometheus.md)をご覧ください。

## ダッシュボードで統計を閲覧する

EMQXダッシュボードの左側ナビゲーションメニューから **Monitoring** -> **Cluster Overview** をクリックします。**Cluster Overview** ページで **Nodes** タブをクリックし、ノード名をクリックすると右側に統計の詳細が表示されます。

<img src="./assets/node-statistics-ee.png" alt="ノード統計情報" style="zoom:45%;" />

統計には現在値と過去の最大値の2つの値が含まれます。例えば、現在のサブスクリプション数と過去の最大サブスクリプション数です。以下はEMQXの統計一覧です：

| 統計名                     | 説明                                                         |
| -------------------------- | ------------------------------------------------------------ |
| connections.count          | 現在のコネクション数                                         |
| connections.max            | 過去の最大コネクション数                                     |
| live_connections.count     | 現在生存しているコネクション数                               |
| live_connections.max       | 過去の最大生存コネクション数                                 |
| channels.count             | `sessions.count` と同じ                                      |
| channels.max               | `sessions.max` と同じ                                        |
| sessions.count             | 現在のセッション数                                           |
| sessions.max               | 過去の最大セッション数                                       |
| topics.count               | 現在のトピック数                                             |
| topics.max                 | 過去の最大トピック数                                         |
| suboptions.count           | `subscriptions.count` と同じ                                 |
| suboptions.max             | `subscriptions.max` と同じ                                   |
| subscribers.count          | 現在のサブスクライバー数                                     |
| subscribers.max            | 過去の最大サブスクライバー数                                 |
| subscriptions.count        | 現在のサブスクリプション数（共有サブスクリプションを含む） |
| subscriptions.max          | 過去の最大サブスクリプション数                               |
| subscriptions.shared.count | 現在の共有サブスクリプション数                               |
| subscriptions.shared.max   | 過去の最大共有サブスクリプション数                           |
| retained.count             | 現在保持されているメッセージ数                               |
| retained.max               | 過去の最大保持メッセージ数                                   |
| delayed.count              | 現在遅延しているメッセージ数                                 |
| delayed.max                | 過去の最大遅延メッセージ数                                   |

## ダッシュボードでメトリクスを閲覧する

EMQXダッシュボードの左側ナビゲーションメニューから **Monitoring** -> **Cluster Overview** をクリックします。**Cluster Overview** ページで **Metrics** タブをクリックすると、クラスターまたは特定ノードのランタイムメトリクスを閲覧できます。

EMQXのメトリクスはカウンターとして実装されており、ノード起動以降の特定イベントの累積発生回数を記録します。これらのメトリクスはシステムの挙動観察、負荷パターンの評価、トラブルシューティングに役立ちます。

ダッシュボードのメトリクスは以下のカテゴリに分類されています：

- **コネクションとセッションメトリクス**：クライアントコネクション、セッション、アクセス制御イベント
- **ルールとアクション（シンク）メトリクス**：データ統合におけるルールマッチングとアクション実行
- **メッセージングメトリクス**：バイト数、パケット数、メッセージ数、配信統計

### コネクションとセッションメトリクス

このセクションではクラスターまたはノードのイベント関連メトリクスを表示します。対象は[クライアントコネクション](#connections)、[コネクションセッション](#sessions)、[クライアントアクセス](#access)です。

<img src="./assets/dashboard-event-metrics-ee.png" alt="ダッシュボードイベントメトリクス" style="zoom:50%;" />

#### コネクション

| メトリクス             | 説明                                                         |
| --------------------- | ------------------------------------------------------------ |
| client.connack        | クライアントが受信した接続確認（`CONNACK`）メッセージ数     |
| client.connect        | クライアントからの接続要求数（成功・失敗を含む）             |
| client.connected      | 成功したクライアントコネクション数                           |
| client.disconnected   | クライアントの切断数（正常・異常切断を含む）                 |
| client.subscribe      | 成功したサブスクリプション数                                 |
| client.unsubscribe    | 成功したサブスクリプション解除数                             |

#### セッション

| メトリクス            | 説明                                                         |
| -------------------- | ------------------------------------------------------------ |
| session.created      | 作成されたセッション数                                       |
| session.discarded    | 廃棄されたセッション数                                       |
| session.resumed      | 再開されたセッション数                                       |
| session.takenover    | 引き継がれたセッション数                                     |
| session.terminated   | 終了したセッション数                                         |

#### アクセス

| メトリクス                     | 説明                                                         |
| ----------------------------- | ------------------------------------------------------------ |
| authorization.allow           | クライアント認可成功の合計数。キャッシュヒット（認可結果取得）とポリシールールに一致した認可要求の合計。 |
| authorization.deny            | クライアント認可失敗の合計数。キャッシュヒット（認可結果取得）とポリシールールに一致しなかった認可要求の合計。 |
| authorization.matched.allow   | ルールにより認可成功したクライアント数                       |
| authorization.matched.deny    | ルールにより認可失敗（拒否）したクライアント数               |
| authorization.nomatch         | いずれのルールにも一致しなかった認可要求数                   |
| authorization.cache_hit       | キャッシュで認可結果（許可または拒否）を取得したクライアント数 |
| authorization.superuser       | スーパーユーザーとして認可されたクライアント数               |
| client.auth.anonymous         | 匿名ログインしたクライアント数                               |
| client.authenticate           | トリガーされた認証回数                                       |
| client.authorize              | トリガーされた認可回数                                       |

### ルールとアクション（シンク）

このセクションはデータ統合に関連するメトリクスを提供し、ルールのマッチ回数やアクション（シンク）の実行回数を把握できます。

これらのメトリクスはルールの有効性評価、下流データフローの監視、全体的なデータ統合利用状況の評価に役立ちます。

![dashboard-integration-metrics](./assets/rule-action-metrics.png)

#### ルール

| メトリクス       | 説明                                                         |
| --------------- | ------------------------------------------------------------ |
| rules.matched   | メッセージやイベントがルールエンジンを通過した際にルールが成功裏にマッチした回数 |

#### アクション（シンク）

| メトリクス          | 説明                                                         |
| ------------------ | ------------------------------------------------------------ |
| actions.executed   | ルールマッチにより実行されたアクション（シンク）の回数       |
| actions.messages   | アクション実行で処理されたメッセージ数。単一のアクション実行が複数メッセージのバッチ処理を行うため、この値は `actions.executed` 以上になる。 |

### メッセージング

**Metrics** ページをスクロールすると、[バイト数](#bytes)、[パケット数](#packets)、[メッセージ](#message-publish-packet)、[配信](#delivery)に関するメトリクスが表示されます。

<img src="./assets/dashboard-messaging-metrics-ee.png" alt="ダッシュボードメッセージングメトリクス" style="zoom:50%;" />

#### バイト数

| メトリクス        | 説明                       |
| ----------------- | -------------------------- |
| bytes.received    | 受信したバイト数           |
| bytes.sent        | 送信したバイト数           |

#### パケット数

| メトリクス                      | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| packets.received               | 受信したパケット数                                           |
| packets.sent                   | 送信したパケット数                                           |
| packets.connect.received       | 受信したCONNECTパケット数                                    |
| packets.connack.auth_error     | 理由コード0x86および0x87を含む送信したCONNACKメッセージ数  |
| packets.connack.error          | 0x00以外の理由コードを持つ送信したCONNACKパケット数。`packets.connack.auth_error`以上の値。 |
| packets.connack.sent           | 送信したCONNACKパケット数                                   |
| packets.publish.received       | 受信したPUBLISHパケット数                                   |
| packets.publish.sent           | 送信したPUBLISHパケット数                                   |
| packets.publish.inuse          | パケット識別子が使用中の受信PUBLISHパケット数               |
| packets.publish.auth_error     | ACLチェックに失敗した受信PUBLISHパケット数                   |
| packets.publish.error          | パブリッシュできなかった受信PUBLISHパケット数               |
| packets.puback.received        | 受信したPUBACKパケット数                                    |
| packets.puback.sent            | 送信したPUBACKパケット数                                    |
| packets.puback.inuse           | 識別子が使用中の受信PUBACKメッセージ数                       |
| packets.puback.missed          | 不明な識別子を持つ受信PUBACKパケット数                       |
| packets.pubrec.received        | 受信したPUBRECパケット数                                    |
| packets.pubrec.sent            | 送信したPUBRECパケット数                                    |
| packets.pubrec.inuse           | 識別子が使用中の受信PUBRECメッセージ数                       |
| packets.pubrec.missed          | 不明な識別子を持つ受信PUBRECパケット数                       |
| packets.pubrel.received        | 受信したPUBRELパケット数                                    |
| packets.pubrel.sent            | 送信したPUBRELパケット数                                    |
| packets.pubrel.missed          | 不明な識別子を持つ受信PUBRELパケット数                       |
| packets.pubcomp.received       | 受信したPUBCOMPパケット数                                   |
| packets.pubcomp.sent           | 送信したPUBCOMPパケット数                                   |
| packets.pubcomp.inuse          | 識別子が使用中の受信PUBCOMPメッセージ数                      |
| packets.pubcomp.missed         | 失われたPUBCOMPパケット数                                   |
| packets.subscribe.received     | 受信したSUBSCRIBEパケット数                                 |
| packets.subscribe.error        | 失敗したサブスクリプションを含む受信SUBSCRIBEパケット数     |
| packets.subscribe.auth_error   | ACLチェックに失敗した受信SUBACKパケット数                    |
| packets.suback.sent            | 送信したSUBACKパケット数                                    |
| packets.unsubscribe.received   | 受信したUNSUBSCRIBEパケット数                               |
| packets.unsubscribe.error      | 失敗したサブスクリプション解除を含む受信UNSUBSCRIBEパケット数 |
| packets.unsuback.sent          | 送信したUNSUBACKパケット数                                  |
| packets.pingreq.received       | 受信したPINGREQパケット数                                   |
| packets.pingresp.sent          | 送信したPINGRESPパケット数                                  |
| packets.disconnect.received    | 受信したDISCONNECTパケット数                                |
| packets.disconnect.sent        | 送信したDISCONNECTパケット数                                |
| packets.auth.received          | 受信したAUTHパケット数                                      |
| packets.auth.sent              | 送信したAUTHパケット数                                      |

#### メッセージ（PUBLISHパケット）

| メトリクス                       | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| messages.acked                 | アック（ACK）されたメッセージ数                              |
| messages.delayed               | EMQXにより遅延パブリッシュのため保存されているメッセージ数   |
| messages.delivered             | EMQX内部でサブスクリプションプロセスに転送されたメッセージ数 |
| messages.dropped               | サブスクリプションプロセスに転送される前にEMQXで破棄されたメッセージ数合計 |
| messages.dropped.no_subscribers | サブスクライバーがいないため破棄されたメッセージ数           |
| messages.dropped.await_pubrel_timeout | PUBREL待機タイムアウトにより破棄されたメッセージ数         |
| messages.dropped.quota_exceeded | クォータ超過（通常はコネクション数）により破棄されたメッセージ数 |
| messages.dropped.receive_maximum | Receive Maximumに達したため破棄されたメッセージ数           |
| messages.forward               | 他ノードに転送されたメッセージ数                             |
| messages.publish               | システムメッセージを除くパブリッシュされたメッセージ数       |
| messages.qos0.received         | クライアントから受信したQoS 0メッセージ数                    |
| messages.qos1.received         | クライアントから受信したQoS 1メッセージ数                    |
| messages.qos2.received         | クライアントから受信したQoS 2メッセージ数                    |
| messages.qos0.sent             | クライアントに送信したQoS 0メッセージ数                      |
| messages.qos1.sent             | クライアントに送信したQoS 1メッセージ数                      |
| messages.qos2.sent             | クライアントに送信したQoS 2メッセージ数                      |
| messages.received              | クライアントから受信したメッセージ数。`messages.qos0.received`、`messages.qos1.received`、`messages.qos2.received`の合計 |
| messages.sent                  | クライアントに送信したメッセージ数。`messages.qos0.sent`、`messages.qos1.sent`、`messages.qos2.sent`の合計 |

#### 配信

| メトリクス                     | 説明                                                         |
| ----------------------------- | ------------------------------------------------------------ |
| delivery.dropped              | 配信中に破棄されたメッセージの合計数                         |
| delivery.dropped.expired      | メッセージの有効期限切れにより配信中に破棄されたメッセージ数 |
| delivery.dropped.no_local     | `No Local`サブスクリプションオプションにより配信中に破棄されたメッセージ数 |
| delivery.dropped.qos0_msg     | メッセージキューが満杯のため配信中に破棄されたQoS 0メッセージ数 |
| delivery.dropped.queue_full   | メッセージキューが満杯のため配信中に破棄されたQoS 0以外のメッセージ数 |
| delivery.dropped.too_large    | 長さ制限超過により配信中に破棄されたメッセージ数             |

## REST APIによる監視状態の取得

APIを通じてメトリクスと統計を取得することも可能です。UIの左側ナビゲーションメニューで **Metrics** をクリックすると、このAPIリクエストが実行されます。EMQX APIの利用方法については[REST API](../api.md)をご参照ください。

<img src="./assets/metrics-api-doc.png" alt="メトリクスAPIドキュメント" style="zoom:35%;" />

## システムトピックによる監視状態の取得

EMQXは稼働状況、メッセージ統計、クライアントのオンライン・オフラインイベントに関するメッセージをシステムトピックを通じて定期的にパブリッシュします。クライアントはトピック名の前に `$SYS/` プレフィックスを付けてシステムトピックをサブスクライブできます。各種システムトピックの詳細は[システムトピック](./mqtt-system-topics.md)をご覧ください。

システムトピックの設定はダッシュボードで行えます。左側ナビゲーションメニューから **Management** -> **MQTT Settings** をクリックし、**System Topic** タブを選択してください。

<img src="./assets/system-topic-setting.png" alt="システムトピック設定" style="zoom:40%;" />

- **Messages publish interval**：`$SYS` トピック送信の時間間隔を設定します。
- **Heartbeat interval**：ハートビートメッセージ送信の時間間隔を設定します。
- **Client connected notification**：デフォルトで有効。クライアント接続時のイベントメッセージがパブリッシュされます。
- **Client disconnected notification**：デフォルトで有効。クライアント切断時のイベントメッセージがパブリッシュされます。
- **Client subscribed notification**：デフォルトで無効。有効にすると、クライアントがトピックをサブスクライブした際のイベントメッセージがパブリッシュされます。
- **Client unsubscribed notification**：デフォルトで無効。有効にすると、クライアントがトピックのサブスクリプションを解除した際のイベントメッセージがパブリッシュされます。
