# 統計とメトリクス

EMQXはメトリクス監視機能を提供しており、これにより運用・保守担当者は現在のサービス状況を監視し、システムの不具合の可能性をトラブルシューティングできます。

EMQXは監視状態を「統計」と「メトリクス」に分類しています。

- 統計は整数型のゲージで、メトリクスが要求された時点の単一の値を返します。
- メトリクスは整数型のカウンターで、送受信されたバイト数やメッセージ数のような単純な増減を計測します。

EMQXはユーザーに複数の方法で統計とメトリクスを閲覧する手段を提供しています。最も直接的には、EMQXダッシュボードでこれらのデータを確認できます。ダッシュボードへのアクセスが不便な場合は、[REST API](#request-monitoring-status-via-rest-api)や[システムトピック](#get-monitoring-status-via-system-topics)のメッセージを通じてデータを取得できます。さらに、監視機能を自身の監視システムと簡単に統合することも可能です。詳細は[Prometheusとの統合](./prometheus.md)をご参照ください。

## ダッシュボードで統計を確認する

EMQXダッシュボードの左ナビゲーションメニューから **Monitoring** -> **Cluster Overview** をクリックします。**Cluster Overview** ページで **Nodes** タブをクリックし、ノード名をクリックすると右側に統計の詳細が表示されます。

<img src="./assets/node-statistics-ee.png" alt="ノード統計詳細" style="zoom:45%;" />

統計には現在値と過去の最大値の2つの値が含まれます。例えば、現在のサブスクリプション数と過去の最大サブスクリプション数です。EMQXの統計一覧は以下の通りです。


| 統計項目                   | 説明                                                         |
| -------------------------- | ------------------------------------------------------------ |
| connections.count          | 現在の接続数                                                 |
| connections.max            | 過去の最大接続数                                             |
| live_connections.count     | 現在のライブ接続数                                           |
| live_connections.max       | 過去の最大ライブ接続数                                       |
| channels.count             | `sessions.count` と同じ                                     |
| channels.max               | `sessions.max` と同じ                                       |
| sessions.count             | 現在のセッション数                                           |
| sessions.max               | 過去の最大セッション数                                       |
| topics.count               | 現在のトピック数                                            |
| topics.max                 | 過去の最大トピック数                                        |
| suboptions.count           | `subscriptions.count` と同じ                                |
| suboptions.max             | `subscriptions.max` と同じ                                  |
| subscribers.count          | 現在のサブスクライバー数                                    |
| subscribers.max            | 過去の最大サブスクライバー数                                |
| subscriptions.count        | 現在のサブスクリプション数（共有サブスクリプションを含む） |
| subscriptions.max          | 過去の最大サブスクリプション数                              |
| subscriptions.shared.count | 現在の共有サブスクリプション数                              |
| subscriptions.shared.max   | 過去の最大共有サブスクリプション数                          |
| retained.count             | 現在の保持メッセージ数                                      |
| retained.max               | 過去の最大保持メッセージ数                                  |
| delayed.count              | 現在の遅延メッセージ数                                      |
| delayed.max                | 過去の最大遅延メッセージ数                                  |

## ダッシュボードでメトリクスを確認する

EMQXダッシュボードの左ナビゲーションメニューから **Monitoring** -> **Cluster Overview** をクリックします。**Cluster Overview** ページで **Metrics** タブをクリックすると、クラスターまたは特定のノードのランタイムメトリクスを閲覧できます。

EMQXのメトリクスはカウンターとして実装されており、ノード起動以降の特定イベントの累積発生回数を記録します。これらのメトリクスは、オペレーターがシステムの挙動を観察し、負荷パターンを評価し、問題をトラブルシューティングするのに役立ちます。

ダッシュボードのメトリクスは以下のカテゴリに分類されています。

- **接続およびセッションメトリクス**：クライアント接続、セッション、アクセス制御イベント
- **ルールおよびアクション（シンク）メトリクス**：ルールマッチングおよびデータ統合のためのアクション実行
- **メッセージングメトリクス**：バイト数、パケット数、メッセージ数、配信統計

### 接続およびセッションメトリクス

このセクションでは、クラスターまたはノードに関するイベント関連メトリクスを表示します。内容は[クライアント接続](#connections)、[接続セッション](#sessions)、[クライアントアクセス](#access)を含みます。

<img src="./assets/dashboard-event-metrics-ee.png" alt="ダッシュボードイベントメトリクス" style="zoom:50%;" />

#### Connections（接続）

| メトリクス             | 説明                                                         |
| ---------------------- | ------------------------------------------------------------ |
| client.connack         | クライアントが受信した接続確認（`CONNACK`）メッセージの数   |
| client.connect         | クライアントからの接続要求数（成功・失敗を含む）             |
| client.connected       | 成功したクライアント接続数                                   |
| client.disconnected    | クライアント切断数（正常切断・異常切断を含む）               |
| client.subscribe       | 成功したサブスクリプション数                                 |
| client.unsubscribe     | 成功したサブスクリプション解除数                             |

#### Sessions（セッション）

| メトリクス            | 説明                                                         |
| --------------------- | ------------------------------------------------------------ |
| session.created       | 作成されたセッション数                                       |
| session.discarded     | 廃棄されたセッション数                                       |
| session.resumed       | 再開されたセッション数                                       |
| session.takenover     | 引き継がれたセッション数                                     |
| session.terminated    | 終了したセッション数                                         |

#### Access（アクセス）

| メトリクス                     | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| authorization.allow            | クライアント認可成功の合計数。キャッシュヒット（認可結果取得）とポリシールールにマッチした認可要求の合計。 |
| authorization.deny             | クライアント認可失敗の合計数。キャッシュヒット（認可結果取得）とポリシールールにマッチしなかった認可要求の合計。 |
| authorization.matched.allow    | ルールに基づくクライアント認可成功数                         |
| authorization.matched.deny     | ルールにより拒否されたクライアント認可失敗数                 |
| authorization.nomatch          | いずれのルールにもマッチしなかったクライアント認可要求数     |
| authorization.cache_hit        | キャッシュで認可結果（許可または拒否）を取得したクライアント数 |
| authorization.superuser        | スーパーユーザーとして認可されたクライアント数               |
| client.auth.anonymous          | 匿名ログインしたクライアント数                               |
| client.authenticate            | 認証がトリガーされた回数                                     |
| client.authorize               | 認可がトリガーされた回数                                     |

### ルールおよびアクション（シンク）

このセクションではデータ統合に関するメトリクスを提供し、ルールのマッチ回数やアクション（シンク）の実行回数を把握できます。

これらのメトリクスはルールの有効性評価、下流データフローの監視、全体的なデータ統合利用状況の評価に役立ちます。

![dashboard-integration-metrics](./assets/rule-action-metrics.png)

#### ルール

| メトリクス       | 説明                                                         |
| ---------------- | ------------------------------------------------------------ |
| rules.matched    | メッセージやイベントがルールエンジンを通過した際にルールが正常にマッチした回数 |

#### アクション（シンク）

| メトリクス          | 説明                                                         |
| ------------------- | ------------------------------------------------------------ |
| actions.executed    | ルールマッチングの結果として実行されたアクション（シンク）の回数 |

### メッセージング

**Metrics** ページをスクロールすると、[バイト数](#bytes)、[パケット数](#packets)、[メッセージ](#message-publish-packet)、[配信](#delivery)に関するメトリクスを確認できます。

<img src="./assets/dashboard-messaging-metrics-ee.png" alt="ダッシュボードメッセージングメトリクス" style="zoom:50%;" />

#### バイト数

| メトリクス          | 説明                         |
| ------------------- | ---------------------------- |
| bytes.received      | 受信したバイト数             |
| bytes.sent          | 送信したバイト数             |

#### パケット数

| メトリクス                      | 説明                                                         |
| ------------------------------- | ------------------------------------------------------------ |
| packets.received               | 受信したパケット数                                           |
| packets.sent                   | 送信したパケット数                                           |
| packets.connect.received       | 受信したCONNECTパケット数                                    |
| packets.connack.auth_error     | 理由コード0x86および0x87を含むCONNACKメッセージの送信数    |
| packets.connack.error          | 0x00以外の理由コードを持つCONNACKパケットの送信数。この値は `packets.connack.auth_error` 以上。 |
| packets.connack.sent           | 送信したCONNACKパケット数                                   |
| packets.publish.received       | 受信したPUBLISHパケット数                                   |
| packets.publish.sent           | 送信したPUBLISHパケット数                                   |
| packets.publish.inuse          | パケット識別子が占有された受信PUBLISHパケット数             |
| packets.publish.auth_error     | ACLチェックに失敗した受信PUBLISHパケット数                   |
| packets.publish.error          | パブリッシュできなかった受信PUBLISHパケット数               |
| packets.puback.received        | 受信したPUBACKパケット数                                    |
| packets.puback.sent            | 送信したPUBACKパケット数                                    |
| packets.puback.inuse           | 識別子が占有された受信PUBACKメッセージ数                     |
| packets.puback.missed          | 不明な識別子を持つ受信PUBACKパケット数                       |
| packets.pubrec.received        | 受信したPUBRECパケット数                                    |
| packets.pubrec.sent            | 送信したPUBRECパケット数                                    |
| packets.pubrec.inuse           | 識別子が占有された受信PUBRECメッセージ数                     |
| packets.pubrec.missed          | 不明な識別子を持つ受信PUBRECパケット数                       |
| packets.pubrel.received        | 受信したPUBRELパケット数                                    |
| packets.pubrel.sent            | 送信したPUBRELパケット数                                    |
| packets.pubrel.missed          | 不明な識別子を持つ受信PUBRELパケット数                       |
| packets.pubcomp.received       | 受信したPUBCOMPパケット数                                   |
| packets.pubcomp.sent           | 送信したPUBCOMPパケット数                                   |
| packets.pubcomp.inuse          | 識別子が占有された受信PUBCOMPメッセージ数                    |
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

| メトリクス                     | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| messages.acked                 | アック（ACK）されたメッセージ数                             |
| messages.delayed               | EMQXにより遅延パブリッシュとして保存されているメッセージ数   |
| messages.delivered             | EMQX内部でサブスクリプション処理に転送されたメッセージ数     |
| messages.dropped               | サブスクリプション処理に転送される前にEMQXで破棄されたメッセージ数 |
| messages.dropped.no_subscribers | サブスクライバーがいないため破棄されたメッセージ数           |
| messages.dropped.await_pubrel_timeout | PUBREL待機タイムアウトにより破棄されたメッセージ数        |
| messages.dropped.quota_exceeded | クォータ超過（通常は接続数）により破棄されたメッセージ数     |
| messages.dropped.receive_maximum | Receive Maximumに達したため破棄されたメッセージ数           |
| messages.forward               | 他のノードに転送されたメッセージ数                           |
| messages.publish               | システムメッセージを除くパブリッシュされたメッセージ数       |
| messages.qos0.received         | クライアントから受信したQoS 0メッセージ数                    |
| messages.qos1.received         | クライアントから受信したQoS 1メッセージ数                    |
| messages.qos2.received         | クライアントから受信したQoS 2メッセージ数                    |
| messages.qos0.sent             | クライアントに送信したQoS 0メッセージ数                      |
| messages.qos1.sent             | クライアントに送信したQoS 1メッセージ数                      |
| messages.qos2.sent             | クライアントに送信したQoS 2メッセージ数                      |
| messages.received              | クライアントから受信したメッセージ数（`messages.qos0.received`、`messages.qos1.received`、`messages.qos2.received`の合計） |
| messages.sent                  | クライアントに送信したメッセージ数（`messages.qos0.sent`、`messages.qos1.sent`、`messages.qos2.sent`の合計） |

#### 配信

| メトリクス                     | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| delivery.dropped               | 配信中に破棄されたメッセージの合計数                         |
| delivery.dropped.expired       | メッセージの有効期限切れにより配信中に破棄されたメッセージ数 |
| delivery.dropped.no_local      | `No Local`サブスクリプションオプションにより配信中に破棄されたメッセージ数 |
| delivery.dropped.qos0_msg      | メッセージキューが満杯のため配信中に破棄されたQoS 0メッセージ数 |
| delivery.dropped.queue_full    | メッセージキューが満杯のため配信中に破棄された非ゼロQoSレベルメッセージ数 |
| delivery.dropped.too_large     | 長さ制限超過により配信中に破棄されたメッセージ数             |

## REST APIによる監視状態の取得

APIを通じてメトリクスや統計を取得することも可能です。UIの左ナビゲーションメニューで **Metrics** をクリックすると、このAPIリクエストを実行できます。EMQX APIの利用方法については[REST API](../admin/api.md)をご参照ください。

<img src="./assets/metrics-api-doc.png" alt="メトリクスAPIドキュメント" style="zoom:35%;" />

## システムトピックによる監視状態の取得

EMQXは稼働状況、メッセージ統計、クライアントのオンライン・オフラインイベントに関するメッセージをシステムトピックを通じて定期的にパブリッシュします。クライアントはトピック名の前に `$SYS/` プレフィックスを付けてシステムトピックをサブスクライブできます。システムトピックの種類については[システムトピック](./mqtt-system-topics.md)をご覧ください。

システムトピックの設定はダッシュボードで行えます。左ナビゲーションメニューから **Management** -> **MQTT Settings** をクリックし、**System Topic** タブを選択してください。

<img src="./assets/system-topic-setting.png" alt="システムトピック設定" style="zoom:40%;" />

- **Messages publish interval**：`$SYS` トピックを送信する間隔を設定します。
- **Heartbeat interval**：ハートビートメッセージを送信する間隔を設定します。
- **Client connected notification**：デフォルトで有効。クライアント接続時のイベントメッセージがパブリッシュされます。
- **Client disconnected notification**：デフォルトで有効。クライアント切断時のイベントメッセージがパブリッシュされます。
- **Client subscribed notification**：デフォルトで無効。有効にすると、クライアントがトピックをサブスクライブした際のイベントメッセージがパブリッシュされます。
- **Client unsubscribed notification**：デフォルトで無効。有効にすると、クライアントがトピックのサブスクリプションを解除した際のイベントメッセージがパブリッシュされます。
