# MQTT アドバンスド

## Retained Messages（保持メッセージ）

MQTTのRetained Messagesは、ブローカーが各トピックの最新メッセージを保存し、新たに接続したサブスクライバーが次のパブリッシュイベントを待つことなく即座に最新データを受信できる仕組みです。この機能は、スマートホームや産業用IoTのように、データ更新が稀でもリアルタイムの状態把握が重要なシナリオで特に有用です。

EMQXはMQTT 5.0に準拠した保持メッセージを完全にサポートしており、直感的なダッシュボードや管理APIを通じて保持メッセージの閲覧、管理、削除が可能です。メモリまたはディスクのストレージモード、メッセージの有効期限、最大保持エントリ数などを設定でき、システムの信頼性や永続化要件に合わせて調整できます。

試すには、`docker run -d --name emqx -p 18083:18083 -p 1883:1883 emqx/emqx-enterprise` を実行し、組み込みのダッシュボードから保持メッセージを簡単に管理できます。より高度なユースケースには、セッション永続化、ワイルドカードサブスクライブ、メッセージ有効期限などのMQTT機能もEMQXがサポートしており、堅牢なMQTTベースのアプリケーション構築に最適なプラットフォームです。

**詳細はこちら:** [MQTT Retained Messages: Beginner's Guide with Example](https://www.emqx.com/en/blog/mqtt5-features-retain-message#mqtt-retained-messages-in-emqx)

## Will Messages（遺言メッセージ）

MQTTのWill Messageは、クライアントの予期しない切断を検知し、他のクライアントへの通知やバックアップ機器への切り替えなどの適切な対応を可能にする重要な機能です。クライアント接続時にWill Messageを指定すると、クライアントが正常なDISCONNECTパケットを送信せずに切断した場合にサーバーがそのメッセージを送信します。この仕組みにより、クライアントの状態監視とIoTアプリケーションの信頼性確保が実現します。

MQTT 5.0ではWill Delay Intervalが導入され、Will Messageのパブリッシュを遅延させることで、一時的なネットワーク障害時の不要な通知を減らせます。メッセージはサーバーのセッションに保存され、セッションの有効期限切れまたは遅延時間のいずれか早い方で配信されます。EMQXはWill Messageの保持やセッション有効期限通知との統合などの機能をサポートし、クライアント状態監視を強化しています。この機能は、レジリエントでリアルタイムなIoTアプリケーション構築に不可欠であり、デバイスやクライアントの状態変化を迅速に運用者に通知します。

**詳細はこちら:** [MQTT Will Message (Last Will & Testament) Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/use-of-mqtt-will-message)

## Request / Response（リクエスト／レスポンス）

MQTT 5.0はリクエスト／レスポンスパターンを3つの主要機能で改善しました。**Response Topic**はリクエスターがレスポンス用の一意のトピックを指定でき、競合を減らします。**Correlation Data**は非同期や複数レスポンダーのシナリオでもレスポンスをリクエストに紐付けるのに役立ちます。**Response Information**はレスポンス用トピック構築のためにサーバー固有の情報をリクエスターに提供し、トピック権限管理を容易にします。これらにより、特に複雑なIoT環境で信頼性と整理された通信が実現します。

**詳細はこちら:** [MQTT Request / Response Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-request-response#suggestions-for-using-mqtt-request-response)

## User Properties（ユーザープロパティ）

MQTT 5.0のUser Propertiesは、HTTPヘッダーのようにクライアントがメッセージにキー・バリュー形式のカスタムメタデータを添付できる機能です。これにより、ファイル転送、リソースフォーマット識別、インテリジェントなメッセージルーティングなど、プロトコルの柔軟性が大幅に向上します。User Propertiesは接続時、パブリッシュ時、サブスクライブ時、切断時に追加可能で、クライアントとブローカー間のシームレスなメタデータ交換を実現します。例えばファイル情報、データ形式、地域タグなどを運び、サーバー側での効率的なメッセージ処理やトレーサブルなアプリケーションレベルのルーティングに役立ちます。EMQXはUser Propertiesを完全サポートし、MQTT.jsや今後のMQTTXなどのクライアントで豊富な互換性を提供しています。

**詳細はこちら:** [User Properties - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-user-properties)

## Topic Alias（トピックエイリアス）

Topic Aliasは、長く繰り返し使われるトピック名を2バイトの整数に置き換え、パブリッシュ時の帯域幅消費を削減する機能です。

EMQXはTopic Aliasをサポートしており、特に帯域幅が限られた環境でメッセージサイズを最適化し効率化を図れます。

**詳細はこちら:** [Topic Alias - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-topic-alias)

## Payload Format Indicator & Content Type（ペイロードフォーマット指標とコンテンツタイプ）

Payload Format IndicatorとContent Typeは、MQTT 5.0の重要なプロパティで、メッセージ解析の透明性を高めます。前者はペイロードがバイナリかUTF-8テキストかを識別し、後者はMIMEタイプ（例：`application/json`）でペイロードのフォーマットを表します。

これらにより、サブスクライバーは効率的にメッセージを解釈でき、トピック名に依存しない柔軟なコンテンツ処理が可能です。EMQXは両方のプロパティをサポートし、多様なIoTやメッセージングアプリケーションでスマートなペイロード処理を実現します。

**詳細はこちら:** [Introduction to MQTT Payload Format Indicator and Content Type | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-features-payload-format-indicator-and-content-type)

## Shared Subscriptions（共有サブスクリプション）

MQTT 5.0のShared Subscriptionsは、複数のクライアントが単一トピックのメッセージ消費を共有し、負荷分散とシステムのスケーラビリティ向上を実現します。EMQXはこの機能を完全サポートし、MQTT 3.1.1クライアントにも互換性を拡張しているため、既存デバイスはコード変更なしに`$share/{group}/{topic}`形式を使うだけで共有消費の恩恵を受けられます。

この仕組みはスループットを向上させ、単一クライアントのボトルネックを防ぎ、高可用性を確保します。ラウンドロビン、ハッシュ、ローカルファーストなど複数のロードバランシング戦略を内蔵し、クラスター環境でのトラフィック分散を柔軟に管理可能です。

**詳細はこちら:** [MQTT Shared Subscriptions: Practical Guidelines and Use Cases | MQTT 5 Features](https://www.emqx.com/en/blog/introduction-to-mqtt5-protocol-shared-subscription)

## Subscription Options（サブスクリプションオプション）

MQTTのSubscription Optionsは、クライアントがメッセージ受信方法をカスタマイズできる機能です。MQTT 5.0ではQoS、No Local、Retain As Published、Retain Handlingの4つの主要オプションが導入され、柔軟性と制御性を高めています。これにより、メッセージ品質の管理、ブリッジング時のメッセージループ回避、Retainフラグの保持、保持メッセージ受信のタイミング制御が可能です。

EMQXはMQTT 5.0のすべてのサブスクリプションオプションを完全サポートし、例えばNo Localでブリッジ設定時のメッセージ嵐を防止し、Retain As Publishedでブローカー間の保持メッセージ処理の一貫性を保証します。

**詳細はこちら:** [MQTT Subscription Options Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/an-introduction-to-subscription-options-in-mqtt)

## Subscription Identifier（サブスクリプション識別子）

MQTT 5.0のSubscription Identifierは、各サブスクリプションに一意の識別子を割り当て、クライアントが複数のサブスクリプションを効率的に管理・追跡できる機能です。多数のサブスクリプションを持つ複雑なアプリケーションで特に有用です。

EMQXはこの機能を完全サポートし、ワイルドカードや重複するサブスクリプションでもメッセージの発信元を正確に特定可能です。PUBLISHパケットにSubscription Identifierを含めることで、クライアント側でのトピックフィルタマッチングが不要となり、メッセージ処理効率が大幅に向上し、正確なコールバック実行が可能になります。

**詳細はこちら:** [MQTT Subscription Identifier Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/subscription-identifier-and-subscription-options)

## Keep Alive（キープアライブ）

MQTTのKeep Alive機構は、クライアントが一定間隔内に定期的なパケットを送信することで半開きTCP接続を防止します。データ送信がない場合は`PINGREQ`で接続確認を行います。EMQXはクライアント指定およびサーバー強制のKeep Alive値を完全サポートし、MQTT 5.0のServer Keep Alive機能も含みます。`server_keepalive`や`keepalive_backoff`などの設定項目で接続タイムアウトを細かく制御でき、IoT展開の信頼性を高め、クライアントの予期しない切断時にWill Messageの適時配信を保証します。

**詳細はこちら:** [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive)

## Message Expiry Interval（メッセージ有効期限間隔）

MQTT 5.0のMessage Expiry Intervalは、クライアントがメッセージのブローカー保存期間を設定できる機能です。即時配信できない場合、指定時間経過後にメッセージは破棄され、古い情報の送信を防ぎます。

EMQXはこの機能を完全サポートし、メッセージ転送やブリッジング時に有効期限を減算して分散展開全体でメッセージの鮮度を維持します。

**詳細はこちら:** [Introduction to MQTT Message Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt-message-expiry-interval)

## Maximum Packet Size（最大パケットサイズ）

MQTT 5.0のMaximum Packet Sizeプロパティは、クライアントとサーバーが安全なパケットサイズ上限を交渉し、リソース制約のあるデバイスの過負荷を防ぐための機能です。クライアントは`CONNECT`パケットで受信上限を宣言し、サーバーは`CONNACK`パケットで応答します。EMQXはこの双方向制約を厳格に適用し、サイズ超過のメッセージは破棄、またはレスポンスパケットから低優先度のメタデータ（User Propertiesなど）を削除して接続安定性を維持します。共有サブスクリプション環境では、サイズ超過メッセージを適格なグループメンバーにリダイレクトすることも可能です。

**詳細はこちら:** [MQTT Maximum Packet Size Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/best-practices-of-maximum-packet-size-in-mqtt)

## Reason Codes（理由コード）

MQTT 5.0は、MQTT 3.1.1の限定的なステータス応答に比べて大幅に強化された包括的なReason Codeシステムを導入しました。これらの理由コードは接続、パブリッシュ、サブスクライブなどの操作結果を詳細に提供し、開発者が問題を迅速に診断し、デバイス管理やメッセージ処理を最適化するのに役立ちます。

EMQXはMQTT 5.0のReason Codesを完全サポートし、正確なエラー処理とインテリジェントな運用管理を可能にして、IoTシステムの安定性と応答性を向上させます。

**詳細はこちら:** [MQTT Reason Code Introduction and Quick Reference](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)

## Enhanced Authentication（拡張認証）

MQTT 5.0は、平文パスワード送信や相互認証の欠如といった単純なパスワード認証の脆弱性を解消するため、より安全な拡張認証フレームワークを導入しました。AUTHパケットを利用し、複数のメッセージ交換やDIGEST-MD5、SCRAM、KerberosなどのSASLメカニズムをサポートします。

EMQXはSCRAMをサポートし、ユーザーがより強力で柔軟な認証方式を選択してIoTインフラを保護できるようにします。

**詳細はこちら:** [Leveraging Enhanced Authentication for MQTT Security](https://www.emqx.com/en/blog/leveraging-enhanced-authentication-for-mqtt-security)

## Control Packets（コントロールパケット）

MQTTのコントロールパケットは、クライアントとブローカー間の通信方法を定義し、接続、パブリッシュ、サブスクライブなど15種類の操作を含みます。各パケットは固定ヘッダー、オプションの可変ヘッダー、オプションのペイロードで構成され、軽量かつ効率的なデータ交換を実現します。MQTT 5.0ではプロパティや認証の改善によりさらに柔軟性が向上しています。

**詳細はこちら:** [MQTT Control Packets: A Beginner's Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-control-packets)

MQTTの詳細については、こちらをご覧ください: [MQTT Guide 2025: Beginner to Advanced](https://www.emqx.com/en/mqtt-guide)
