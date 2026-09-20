# MQTT アドバンスド

## Retained Messages（保持メッセージ）

MQTTのRetained Messagesは、ブローカーが各トピックの最新メッセージを保存できる機能で、新たに接続したサブスクライバーが次のパブリッシュイベントを待たずに最新のデータを即座に受信できます。この仕組みは、スマートホームや産業用IoTのように、データ更新が頻繁でなくてもリアルタイムの状態把握が重要なシナリオで特に有用です。

EMQXはMQTT 5.0に準拠したRetained Messagesを完全にサポートしており、直感的なダッシュボードや管理APIを通じて保持メッセージの閲覧、管理、削除が可能です。メモリまたはディスクのストレージモード、メッセージの有効期限、最大保持エントリ数などを設定し、システムの信頼性や永続化要件に合わせて調整できます。

試すには、`docker run -d --name emqx -p 18083:18083 -p 1883:1883 emqx/emqx-enterprise` を実行し、組み込みのダッシュボードから保持メッセージを簡単に管理できます。さらに高度なユースケースには、セッション永続化、ワイルドカードサブスクライブ、メッセージ有効期限などのMQTT機能もサポートしており、堅牢なMQTTベースのアプリケーション構築に最適なプラットフォームです。

**詳細はこちら:** [MQTT Retained Messages: Beginner's Guide with Example](https://www.emqx.com/en/blog/mqtt5-features-retain-message#mqtt-retained-messages-in-emqx)

## Will Messages（遺言メッセージ）

MQTTのWill Messageは、クライアントが予期せず切断された場合に他のクライアントへの通知やバックアップ機器への切り替えなどの適切な処理を可能にする重要な機能です。クライアント接続時にWill Messageを指定すると、クライアントが正常なDISCONNECTパケットを送信せずに切断された際にサーバーがそのメッセージを送信します。この仕組みはクライアントの状態監視とIoTアプリケーションの信頼性確保に役立ちます。

MQTT 5.0ではWill Delay Intervalが導入され、一時的なネットワーク障害時の不要な通知を減らすためにWill Messageの公開を遅延させることが可能になりました。メッセージはサーバーのセッションに保存され、セッションの有効期限切れまたは遅延時間のいずれか早い方で配信されます。EMQXはWill Messageの保持やセッション有効期限通知との連携などをサポートし、クライアント状態の高度な監視を実現します。この機能はレジリエントでリアルタイムなIoTアプリケーション構築に不可欠であり、システム運用者に迅速なデバイスやクライアントの状態変化通知を提供します。

**詳細はこちら:** [MQTT Will Message (Last Will & Testament) Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/use-of-mqtt-will-message)

## Request / Response（リクエスト／レスポンス）

MQTT 5.0はRequest/Responseパターンを3つの主要機能で改善しました。**Response Topic**はリクエスターがレスポンス用の一意なトピックを指定でき、競合を減らします。**Correlation Data**は非同期や複数レスポンダーのシナリオでもレスポンスとリクエストの対応付けを助けます。**Response Information**はレスポンス用トピック構築のためにサーバー固有情報をリクエスターに提供し、トピック権限管理を容易にします。これらにより、特に複雑なIoT環境で信頼性と整理された通信が実現します。

**詳細はこちら:** [MQTT Request / Response Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-request-response#suggestions-for-using-mqtt-request-response)

## User Properties（ユーザープロパティ）

MQTT 5.0のUser Propertiesは、HTTPヘッダーのようにクライアントがメッセージにキーと値のペアでカスタムメタデータを添付できる機能です。これにより、ファイル転送、リソース形式識別、インテリジェントなメッセージルーティングなどのユースケースでプロトコルの柔軟性が大幅に向上します。User Propertiesは接続時、パブリッシュ時、サブスクライブ時、切断時に追加可能で、クライアントとブローカー間のシームレスなメタデータ交換を実現します。例えば、ファイル情報、データ形式、地域タグなどを運び、サーバーが効率的にメッセージを処理し、トレーサブルなアプリケーションレベルのルーティングを可能にします。EMQXはUser Propertiesを完全にサポートし、MQTT.jsや今後のMQTTXなどのクライアントで豊富な互換性を提供します。

**詳細はこちら:** [User Properties - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-user-properties)

## Topic Alias（トピックエイリアス）

Topic Aliasは、長く繰り返し使用されるトピック名を2バイトの整数に短縮し、パブリッシュ時の帯域幅消費を削減する機能です。

EMQXはTopic Aliasをサポートしており、特に帯域幅が限られた環境でメッセージサイズを最適化し効率を向上させます。

**詳細はこちら:** [Topic Alias - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-topic-alias)

## Payload Format Indicator & Content Type（ペイロードフォーマット指標とコンテントタイプ）

Payload Format IndicatorとContent Typeは、メッセージ解析をより明確にするMQTT 5.0の重要なプロパティです。前者はペイロードがバイナリかUTF-8テキストかを識別し、後者はMIMEタイプ（例：`application/json`）でペイロードの形式を表します。

これらにより、サブスクライバーは効率的にメッセージを解釈でき、トピック名の命名規則に依存しない柔軟なコンテンツ処理が可能になります。EMQXは両方のプロパティをサポートし、多様なIoTやメッセージングアプリケーションでスマートなペイロード処理を実現します。

**詳細はこちら:** [Introduction to MQTT Payload Format Indicator and Content Type | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-features-payload-format-indicator-and-content-type)

## Shared Subscriptions（共有サブスクリプション）

MQTT 5.0のShared Subscriptionsは、複数のクライアントが単一トピックのメッセージ消費を共有し、負荷分散とシステムのスケーラビリティ向上を実現します。EMQXはこの機能を完全にサポートし、さらにMQTT 3.1.1クライアントにも互換性を拡張しているため、既存デバイスはコード変更なしで`$share/{group}/{topic}`形式を使うだけで共有消費の恩恵を受けられます。

この仕組みはスループットを向上させ、単一クライアントのボトルネックを防ぎ、高可用性を確保します。ラウンドロビン、ハッシュ、ローカルファーストなど複数のロードバランシング戦略を内蔵し、クラスター環境でのトラフィック分散を柔軟に管理できます。

**詳細はこちら:** [MQTT Shared Subscriptions: Practical Guidelines and Use Cases | MQTT 5 Features](https://www.emqx.com/en/blog/introduction-to-mqtt5-protocol-shared-subscription)

## Subscription Options（サブスクリプションオプション）

MQTTのSubscription Optionsは、クライアントがメッセージ受信方法をカスタマイズできる機能です。MQTT 5.0ではQoS、No Local、Retain As Published、Retain Handlingの4つの主要オプションが導入され、柔軟性と制御性を高めています。これにより、メッセージ品質の管理、ブリッジでのメッセージループ回避、Retainフラグの保持、保持メッセージの受信タイミングの選択が可能です。

EMQXはMQTT 5.0の全サブスクリプションオプションを完全サポートし、例えばNo Localでブリッジ環境のメッセージストームを防ぎ、Retain As Publishedでブローカー間の保持メッセージ処理の一貫性を保証します。

**詳細はこちら:** [MQTT Subscription Options Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/an-introduction-to-subscription-options-in-mqtt)

## Subscription Identifier（サブスクリプション識別子）

MQTT 5.0のSubscription Identifierは、各サブスクリプションに一意の識別子を割り当て、クライアントが複数のサブスクリプションを効率的に管理・追跡できる機能です。多くのサブスクリプションを持つ複雑なアプリケーションで特に有用です。

EMQXはこの機能を完全にサポートし、ワイルドカードや重複するサブスクリプションでもメッセージの発信元サブスクリプションを正確に特定できます。PUBLISHパケットにSubscription Identifierを含めることで、クライアント側でのトピックフィルタマッチングを不要にし、メッセージ処理効率を大幅に向上させ、正確なコールバック実行を可能にします。

**詳細はこちら:** [MQTT Subscription Identifier Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/subscription-identifier-and-subscription-options)

## Keep Alive（キープアライブ）

MQTTのKeep Alive機構は、クライアントが一定間隔で定期的にパケットを送信することで半開きTCP接続を防ぎます。データ送信がない場合は`PINGREQ`で接続確認を行います。EMQXはクライアント指定およびサーバー強制のKeep Alive値を完全にサポートし、MQTT 5.0のServer Keep Alive機能も含みます。`server_keepalive`や`keepalive_backoff`などの設定項目により接続タイムアウトを細かく制御でき、IoT環境での信頼性を高め、クライアントの異常切断時にWill Messageを確実に配信します。

**詳細はこちら:** [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive)

## Message Expiry Interval（メッセージ有効期限間隔）

MQTT 5.0のMessage Expiry Intervalは、クライアントがメッセージを即時配信できない場合にブローカーが保持する時間の上限を設定できる機能です。期限切れ後はメッセージが破棄され、古い情報の送信を防ぎます。

EMQXはこの機能を完全にサポートし、メッセージ転送やブリッジ時に有効期限を減算して分散環境全体でメッセージの鮮度を保証します。

**詳細はこちら:** [Introduction to MQTT Message Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt-message-expiry-interval)

## Maximum Packet Size（最大パケットサイズ）

MQTT 5.0のMaximum Packet Sizeプロパティは、クライアントとサーバーが安全なパケットサイズの上限を交渉し、リソース制約のあるデバイスの過負荷を防ぐための機能です。クライアントは`CONNECT`パケットで受信可能な最大サイズを宣言し、サーバーは`CONNACK`パケットで応答します。EMQXはこの双方向制約を厳守し、サイズ超過のメッセージを破棄したり、応答パケットからUser Propertiesなどの低優先度メタデータを削除して接続の安定性を保ちます。共有サブスクリプションの場合は、サイズ超過メッセージを適格なグループメンバーにリダイレクトすることも可能です。

**詳細はこちら:** [MQTT Maximum Packet Size Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/best-practices-of-maximum-packet-size-in-mqtt)

## Reason Codes（理由コード）

MQTT 5.0はReason Codeシステムを導入し、MQTT 3.1.1の限定的なステータス応答に比べてプロトコルのフィードバックを大幅に強化しました。これらの理由コードは接続、パブリッシュ、サブスクライブなどの操作結果を詳細に示し、開発者が問題を迅速に診断し、デバイス管理やメッセージ処理を最適化するのに役立ちます。

EMQXはMQTT 5.0のReason Codesを完全にサポートし、正確なエラーハンドリングとインテリジェントな運用管理を可能にしてIoTシステムの安定性と応答性を向上させます。

**詳細はこちら:** [MQTT Reason Code Introduction and Quick Reference](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)

## Enhanced Authentication（拡張認証）

MQTT 5.0はEnhanced Authenticationを導入し、単純なパスワード認証の脆弱性（平文パスワード送信や相互認証の欠如）を解消するより安全な認証フレームワークを提供します。AUTHパケットを利用して複数回のメッセージ交換やDIGEST-MD5、SCRAM、KerberosなどのSASLメカニズムをサポートします。

EMQXはSCRAMをサポートしており、ユーザーはより強力で柔軟な認証方式を選択してIoTインフラを保護できます。

**詳細はこちら:** [Leveraging Enhanced Authentication for MQTT Security](https://www.emqx.com/en/blog/leveraging-enhanced-authentication-for-mqtt-security)

## Control Packets（制御パケット）

MQTTの制御パケットはクライアントとブローカー間の通信方法を定義し、接続、パブリッシュ、サブスクライブなど15種類の操作をカバーします。各パケットは固定ヘッダー、任意の可変ヘッダー、任意のペイロードで構成され、軽量かつ効率的なデータ交換を実現します。MQTT 5.0ではプロパティの導入や認証の改善により柔軟性がさらに向上しています。

**詳細はこちら:** [MQTT Control Packets: A Beginner's Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-control-packets)

MQTTの詳細については、こちらもご参照ください: [MQTT Guide 2025: Beginner to Advanced](https://www.emqx.com/en/mqtt-guide)
