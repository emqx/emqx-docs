<<<<<<< HEAD
# MQTT アドバンスド

## Retained Messages（保持メッセージ）

MQTTのRetained Messagesは、ブローカーが各トピックの最新メッセージを保存し、新たに接続したサブスクライバーが次のパブリッシュイベントを待つことなく即座に最新データを受信できる仕組みです。この機能は、スマートホームや産業用IoTのように、データ更新が稀でもリアルタイムの状態把握が重要なシナリオで特に有用です。

EMQXはMQTT 5.0に準拠した保持メッセージを完全にサポートしており、直感的なダッシュボードや管理APIを通じて保持メッセージの閲覧、管理、削除が可能です。メモリまたはディスクのストレージモード、メッセージの有効期限、最大保持件数などを設定し、システムの信頼性や永続化要件に合わせて調整できます。

試すには、`docker run -d --name emqx -p 18083:18083 -p 1883:1883 emqx/emqx-enterprise` を実行し、組み込みのダッシュボードから保持メッセージを簡単に管理してください。より高度なユースケースには、セッション永続化、ワイルドカードサブスクライブ、メッセージ有効期限などのMQTT機能もサポートしており、堅牢なMQTTベースのアプリケーション構築に最適です。

**詳細はこちら:** [MQTT Retained Messages: Beginner's Guide with Example](https://www.emqx.com/en/blog/mqtt5-features-retain-message#mqtt-retained-messages-in-emqx)

## Will Messages（遺言メッセージ）

MQTTのWill Messageは、クライアントが予期せず切断された場合に他のクライアントへの通知やバックアップ機器への切り替えなどの適切な処理を可能にする重要な機能です。クライアントが接続時にWill Messageを指定すると、クライアントが正常なDISCONNECTパケットを送信せずに切断された場合にサーバーがそのメッセージを送信します。この仕組みはクライアントの状態監視とIoTアプリケーションの信頼性確保に役立ちます。

MQTT 5.0ではWill Delay Intervalが導入され、一時的なネットワーク障害時の不要な通知を減らすためにWill Messageの送信を遅延可能です。メッセージはサーバーのセッションに保存され、セッションの有効期限切れまたは遅延時間のいずれか早い方で配信されます。EMQXはWill Messageの保持やセッション有効期限通知との連携をサポートし、クライアント状態の高度な監視を実現します。この機能は、レジリエントでリアルタイムなIoTアプリケーション構築に不可欠であり、システム運用者がデバイスやクライアントの状態変化を迅速に把握できるようにします。

**詳細はこちら:** [MQTT Will Message (Last Will & Testament) Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/use-of-mqtt-will-message)

## Request / Response（リクエスト／レスポンス）

MQTT 5.0はRequest/Responseパターンを3つの主要機能で改善しました。**Response Topic**はリクエスターがレスポンス用のユニークなトピックを指定でき、競合を減らします。**Correlation Data**は非同期や複数レスポンダーのシナリオでもレスポンスとリクエストを紐付けるのに役立ちます。**Response Information**はレスポンストピック構築のためにリクエスターがサーバー固有の情報を受け取れるようにし、トピック権限管理を容易にします。これらにより、特に複雑なIoT環境で信頼性と整理された通信が実現します。

**詳細はこちら:** [MQTT Request / Response Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-request-response#suggestions-for-using-mqtt-request-response)

## User Properties（ユーザープロパティ）

MQTT 5.0のUser Propertiesは、HTTPヘッダーのようにクライアントがメッセージにキー・バリュー形式のカスタムメタデータを付加できる機能です。これにより、ファイル転送、リソース形式識別、インテリジェントなメッセージルーティングなどのユースケースでプロトコルの柔軟性が大幅に向上します。User Propertiesは接続時、パブリッシュ時、サブスクライブ時、切断時に追加可能で、クライアントとブローカー間のシームレスなメタデータ交換を実現します。例えばファイル情報、データ形式、地域タグなどを運び、サーバー側で効率的なメッセージ処理やトレーサブルなアプリケーションレベルルーティングを可能にします。EMQXはUser Propertiesを完全サポートし、MQTT.jsや今後リリース予定のMQTTXなどクライアントでも豊富な互換性を提供します。

**詳細はこちら:** [User Properties - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-user-properties)

## Topic Alias（トピックエイリアス）

Topic Aliasは、長く繰り返し使用されるトピック名を2バイトの整数に置き換え、パブリッシュ時の帯域幅消費を削減する機能です。

EMQXはTopic Aliasをサポートし、メッセージサイズの最適化と効率向上を実現、特に帯域幅が限られた環境で効果を発揮します。

**詳細はこちら:** [Topic Alias - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-topic-alias)

## Payload Format Indicator & Content Type（ペイロードフォーマットインジケーターとコンテントタイプ）

Payload Format IndicatorとContent Typeは、MQTT 5.0の重要なプロパティで、メッセージ解析の透明性を高めます。前者はペイロードがバイナリかUTF-8テキストかを識別し、後者はMIMEタイプ（例：`application/json`）でペイロード形式を表します。

これらによりサブスクライバーは効率的にメッセージを解釈でき、トピック名に依存しない柔軟なコンテンツ処理が可能になります。EMQXは両プロパティをサポートし、多様なIoTおよびメッセージングアプリケーションでスマートなペイロード処理を実現します。

**詳細はこちら:** [Introduction to MQTT Payload Format Indicator and Content Type | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-features-payload-format-indicator-and-content-type)

## Shared Subscriptions（共有サブスクリプション）

MQTT 5.0のShared Subscriptionsは、複数のクライアントが単一トピックのメッセージ消費を共有し、負荷分散とシステムのスケーラビリティ向上を可能にします。EMQXはこの機能を完全サポートし、MQTT 3.1.1クライアントにも互換性を拡張しているため、既存デバイスはコード変更なしで`$share/{group}/{topic}`形式を使うだけで共有消費の恩恵を受けられます。
=======
# MQTT 高度な機能

## リテインドメッセージ

MQTTのリテインドメッセージは、ブローカーが各トピックの最新メッセージを保存する機能で、新たに接続したサブスクライバーが次のパブリッシュイベントを待つことなく、即座に最新のデータを受信できます。この仕組みは、スマートホームや産業用IoTのように、データ更新が頻繁でなくてもリアルタイムの状態把握が重要なシナリオで特に有用です。

EMQXはMQTT 5.0に準拠したリテインドメッセージを完全にサポートしており、直感的なダッシュボードや管理APIを通じてリテインドメッセージの閲覧、管理、削除が可能です。ストレージモード（メモリまたはディスク）、メッセージの有効期限、最大リテインドエントリ数などを設定し、システムの信頼性や永続化要件に合わせて調整できます。

試すには、`docker run -d --name emqx -p 18083:18083 -p 1883:1883 emqx/emqx-enterprise` を実行し、組み込みのダッシュボードにアクセスしてリテインドメッセージを簡単に管理してください。より高度なユースケースには、セッション永続化、ワイルドカードサブスクライブ、メッセージ有効期限などのMQTT機能もサポートしており、堅牢なMQTTベースのアプリケーション構築に最適なプラットフォームです。

**詳細はこちら:** [MQTT Retained Messages: Beginner's Guide with Example](https://www.emqx.com/en/blog/mqtt5-features-retain-message#mqtt-retained-messages-in-emqx)

## ウィルメッセージ

MQTTのウィルメッセージは、クライアントが予期せず切断された場合に他のクライアントへの通知やバックアップ機器への切り替えなどの適切な処理を行うための重要な機能です。クライアント接続時にウィルメッセージを指定でき、クライアントが正常なDISCONNECTパケットを送信せずに切断された際にサーバーがこのメッセージを送信します。この仕組みにより、クライアントの状態監視とIoTアプリケーションの信頼性確保が可能になります。

MQTT 5.0ではウィル遅延間隔（Will Delay Interval）が導入され、一時的なネットワーク障害時の不要な通知を減らすためにウィルメッセージの公開を遅延させることができます。メッセージはサーバーのセッションに保存され、セッションの有効期限切れまたは遅延間隔のいずれか早い方で配信されます。EMQXはウィルメッセージ機能をサポートし、将来のサブスクライバー向けの保持やセッション有効期限通知との連携によるクライアント状態監視の強化を実現しています。この機能は、レジリエントでリアルタイムなIoTアプリケーション構築に不可欠であり、システム運用者がデバイスやクライアントの状態変化を迅速に把握できるようにします。

**詳細はこちら:** [MQTT Will Message (Last Will & Testament) Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/use-of-mqtt-will-message)

## リクエスト／レスポンス

MQTT 5.0はリクエスト／レスポンスパターンを改善し、3つの重要な機能を導入しました。**レスポンストピック**はリクエスターがレスポンス用の固有トピックを指定でき、競合を減らします。**相関データ**は非同期や複数レスポンダーのシナリオでレスポンスとリクエストの対応付けを支援します。**レスポンス情報**はリクエスターがサーバー固有の情報を受け取り、レスポンストピックの構築やトピック権限管理を容易にします。これらにより、特に複雑なIoT環境で信頼性が高く整理された通信が可能になります。

**詳細はこちら:** [MQTT Request / Response Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-request-response#suggestions-for-using-mqtt-request-response)

## ユーザープロパティ

MQTT 5.0のユーザープロパティは、HTTPヘッダーのようにキーと値のペアでクライアントがメッセージにカスタムメタデータを付加できる機能です。これにより、ファイル転送、リソース形式の識別、インテリジェントなメッセージルーティングなど、プロトコルの柔軟性が大幅に向上します。ユーザープロパティは接続時、パブリッシュ時、サブスクライブ時、切断時に追加可能で、クライアントとブローカー間でシームレスなメタデータ交換を実現します。例えば、ファイル情報、データ形式、地域タグなどを運び、サーバー側で効率的なメッセージ処理やトレーサブルなアプリケーションレベルのルーティングを可能にします。EMQXはユーザープロパティを完全サポートし、MQTT.jsや今後のMQTTXなどクライアントの豊富な互換性を提供しています。

**詳細はこちら:** [User Properties - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-user-properties)

## トピックエイリアス

トピックエイリアスは、長く繰り返し使用されるトピック名を2バイトの整数に短縮し、パブリッシュ時の帯域幅消費を削減する機能です。

EMQXはトピックエイリアスをサポートし、特に帯域幅が限られた環境でメッセージサイズを最適化し効率を向上させます。

**詳細はこちら:** [Topic Alias - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-topic-alias)

## ペイロードフォーマットインジケーターとコンテントタイプ

ペイロードフォーマットインジケーターとコンテントタイプは、MQTT 5.0の重要なプロパティで、メッセージ解析の透明性を高めます。前者はペイロードがバイナリかUTF-8テキストかを識別し、後者はMIMEタイプ（例：`application/json`）でペイロード形式を表します。

これらにより、サブスクライバーは効率的にメッセージを解釈でき、トピック名に依存しない柔軟なコンテンツ処理が可能になります。EMQXは両プロパティをサポートし、多様なIoTやメッセージングアプリケーションでスマートなペイロード処理を実現します。

**詳細はこちら:** [Introduction to MQTT Payload Format Indicator and Content Type | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-features-payload-format-indicator-and-content-type)

## 共有サブスクリプション

MQTT 5.0の共有サブスクリプションは、複数のクライアントが単一トピックのメッセージ消費を共有し、負荷分散とシステムのスケーラビリティ向上を実現します。EMQXはこの機能を完全サポートし、MQTT 3.1.1クライアントにも互換性を拡張しているため、既存デバイスはコード変更なしで `$share/{group}/{topic}` 形式を使うだけで共有消費の恩恵を受けられます。
>>>>>>> origin/release-5.10

この仕組みはスループットを向上させ、単一クライアントのボトルネックを防ぎ、高可用性を確保します。ラウンドロビン、ハッシュ、ローカルファーストなど複数のロードバランシング戦略を内蔵し、クラスター環境でのトラフィック分散を柔軟に管理できます。

**詳細はこちら:** [MQTT Shared Subscriptions: Practical Guidelines and Use Cases | MQTT 5 Features](https://www.emqx.com/en/blog/introduction-to-mqtt5-protocol-shared-subscription)

<<<<<<< HEAD
## Subscription Options（サブスクリプションオプション）

MQTTのSubscription Optionsは、クライアントがメッセージ受信方法をカスタマイズできる機能です。MQTT 5.0では、QoS、No Local、Retain As Published、Retain Handlingの4つの主要オプションが導入され、柔軟性と制御性を強化しています。これにより、メッセージ品質管理、ブリッジング時のメッセージループ回避、保持フラグの維持、保持メッセージの受信タイミングの制御が可能です。

EMQXはMQTT 5.0の全サブスクリプションオプションを完全サポートし、例えばNo Localでブリッジ設定時のメッセージ嵐を防止し、Retain As Publishedでブローカー間の保持メッセージ処理を一貫させるなど、細かなメッセージ配信制御を実現します。

**詳細はこちら:** [MQTT Subscription Options Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/an-introduction-to-subscription-options-in-mqtt)

## Subscription Identifier（サブスクリプション識別子）

MQTT 5.0のSubscription Identifierは、各サブスクリプションに一意の識別子を割り当て、クライアントが複数のサブスクリプションを効率的に管理・追跡できる機能です。多数のサブスクリプションを持つ複雑なアプリケーションで特に有用です。

EMQXはこの機能を完全サポートし、ワイルドカードや重複するサブスクリプションでもメッセージの発信元を正確に特定可能です。PUBLISHパケットにSubscription Identifierを含めることで、クライアント側でのトピックフィルター照合を不要にし、メッセージ処理効率を大幅に向上させ、正確なコールバック実行を可能にします。

**詳細はこちら:** [MQTT Subscription Identifier Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/subscription-identifier-and-subscription-options)

## Keep Alive（キープアライブ）

MQTTのKeep Alive機構は、クライアントが一定間隔内に定期的なパケットを送信することで、半開きTCP接続を防止します。データ送信がない場合は`PINGREQ`で接続確認を行います。EMQXはクライアント指定およびサーバー強制のKeep Alive値を完全サポートし、MQTT 5.0のServer Keep Alive機能も対応しています。`server_keepalive`や`keepalive_backoff`などの設定項目により、接続タイムアウトを細かく制御でき、IoT環境での信頼性向上やクライアントの予期せぬ切断時のWill Message送信を確実にします。

**詳細はこちら:** [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive)

## Message Expiry Interval（メッセージ有効期限間隔）

MQTT 5.0のMessage Expiry Intervalは、メッセージが即時配信できない場合にブローカーが保持する最大時間を設定できる機能です。期限切れ後はメッセージが破棄され、古い情報の送信を防止します。

EMQXはこの機能を完全サポートし、メッセージ転送やブリッジング時に有効期限を減算し、分散環境全体でメッセージの鮮度を保ちます。

**詳細はこちら:** [Introduction to MQTT Message Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt-message-expiry-interval)

## Maximum Packet Size（最大パケットサイズ）

MQTT 5.0のMaximum Packet Sizeプロパティは、クライアントとサーバーが安全なパケットサイズ上限を交渉し、リソース制約のあるデバイスの過負荷を防ぐ機能です。クライアントは`CONNECT`パケットで受信可能サイズを宣言し、サーバーは`CONNACK`パケットで応答します。EMQXは双方向の制約を適用し、規定サイズを超えるメッセージは破棄したり、レスポンスパケットから低優先度のメタデータ（User Propertiesなど）を削除して接続安定性を維持します。共有サブスクリプションでは、EMQXがサイズ超過メッセージを適格なグループメンバーにリダイレクトすることも可能です。

**詳細はこちら:** [MQTT Maximum Packet Size Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/best-practices-of-maximum-packet-size-in-mqtt)

## Reason Codes（理由コード）

MQTT 5.0は、MQTT 3.1.1の限定的なステータス応答に比べて大幅に強化された包括的なReason Codeシステムを導入しました。これにより接続、パブリッシュ、サブスクライブなどの操作結果を詳細に示し、開発者が問題を迅速に診断し、デバイス管理やメッセージ処理を最適化できます。

EMQXはMQTT 5.0のReason Codesを完全サポートし、正確なエラーハンドリングとインテリジェントな運用管理を可能にして、IoTシステムの安定性と応答性を向上させます。

**詳細はこちら:** [MQTT Reason Code Introduction and Quick Reference](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)

## Enhanced Authentication（拡張認証）

MQTT 5.0は、単純なパスワード認証の脆弱性（平文パスワード送信や相互認証の欠如）を克服するため、より安全な拡張認証フレームワークを導入しました。AUTHパケットを利用し、複数回のメッセージ交換やDIGEST-MD5、SCRAM、KerberosなどのSASLメカニズムをサポートします。

EMQXはSCRAMをサポートし、ユーザーが強力かつ柔軟な認証方式を選択してIoTインフラを保護できるようにします。

**詳細はこちら:** [Leveraging Enhanced Authentication for MQTT Security](https://www.emqx.com/en/blog/leveraging-enhanced-authentication-for-mqtt-security)

## Control Packets（コントロールパケット）

MQTTのコントロールパケットは、クライアントとブローカー間の通信方法を定義し、接続、パブリッシュ、サブスクライブなど15種類の操作を含みます。各パケットは固定ヘッダー、オプションの可変ヘッダー、オプションのペイロードで構成され、軽量かつ効率的なデータ交換を実現します。MQTT 5.0ではプロパティの追加や認証の強化によりさらに柔軟性が向上しています。

**詳細はこちら:** [MQTT Control Packets: A Beginner's Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-control-packets)

MQTTに関する詳細は以下をご覧ください: [MQTT Guide 2025: Beginner to Advanced](https://www.emqx.com/en/mqtt-guide)
=======
## サブスクリプションオプション

MQTTのサブスクリプションオプションは、クライアントがメッセージ受信方法をカスタマイズできる機能です。MQTT 5.0はQoS、No Local、Retain As Published、Retain Handlingの4つの主要オプションを導入し、柔軟性と制御性を強化しました。これにより、メッセージ品質の管理、ブリッジでのメッセージループ回避、リテインフラグの保持、リテインドメッセージ受信のタイミング制御が可能です。

EMQXはMQTT 5.0のすべてのサブスクリプションオプションを完全サポートし、例えばNo Localでブリッジ環境のメッセージ嵐を防ぎ、Retain As Publishedでブローカー間のリテインドメッセージ処理を一貫させることができます。

**詳細はこちら:** [MQTT Subscription Options Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/an-introduction-to-subscription-options-in-mqtt)

## サブスクリプション識別子

MQTT 5.0のサブスクリプション識別子は、各サブスクリプションに固有のIDを割り当て、クライアントが複数のサブスクリプションを効率的に管理・追跡できる機能です。多数のサブスクリプションを扱う複雑なアプリケーションで特に有用です。

EMQXはこの機能を完全サポートし、ワイルドカードや重複するサブスクリプションでもメッセージの発信元を正確に識別可能です。PUBLISHパケットにサブスクリプション識別子を含めることで、クライアント側でのトピックフィルタマッチングが不要になり、メッセージ処理効率が大幅に向上し、正確なコールバック実行を実現します。

**詳細はこちら:** [MQTT Subscription Identifier Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/subscription-identifier-and-subscription-options)

## キープアライブ

MQTTのキープアライブ機構は、半開きのTCP接続を防ぐためにクライアントが一定間隔内に定期的なパケット送信を要求します。データ送信がない場合は`PINGREQ`で接続確認を行います。EMQXはクライアント指定およびサーバー強制のキープアライブ値を完全サポートし、MQTT 5.0のサーバーキープアライブ機能も含みます。`server_keepalive`や`keepalive_backoff`などの設定項目により、接続タイムアウトを細かく制御でき、IoT環境での信頼性向上やクライアントの異常切断時のウィルメッセージの適時配信を保証します。

**詳細はこちら:** [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive)

## メッセージ有効期限間隔

MQTT 5.0のメッセージ有効期限間隔は、クライアントがメッセージをブローカーが即時配信できない場合に保存する最大時間を設定できる機能です。期限切れ後はメッセージが破棄され、古い情報の送信を防ぎます。

EMQXはこの機能を完全サポートし、メッセージ転送やブリッジ時に有効期限間隔を減算して分散環境全体でメッセージの鮮度を維持します。

**詳細はこちら:** [Introduction to MQTT Message Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt-message-expiry-interval)

## 最大パケットサイズ

MQTT 5.0の最大パケットサイズプロパティは、クライアントとサーバーが安全なパケットサイズの上限を交渉し、リソース制約のあるデバイスの過負荷を防ぐ機能です。クライアントは`CONNECT`パケットで受信可能な最大サイズを宣言し、サーバーは`CONNACK`パケットで応答します。EMQXはこの双方向制約を厳格に適用し、サイズ超過のメッセージは破棄するか、レスポンスパケットからユーザープロパティなど低優先度のメタデータを削除して接続安定性を維持します。共有サブスクリプション環境では、サイズ超過メッセージを適格なグループメンバーにリダイレクトすることも可能です。

**詳細はこちら:** [MQTT Maximum Packet Size Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/best-practices-of-maximum-packet-size-in-mqtt)

## リーズンコード

MQTT 5.0はMQTT 3.1.1の限定的なステータス応答に比べて大幅に強化された包括的なリーズンコードシステムを導入しました。これにより、接続、パブリッシュ、サブスクライブなどの操作結果を詳細に返し、開発者が問題を迅速に診断し、デバイス管理やメッセージ処理を最適化できます。

EMQXはMQTT 5.0のリーズンコードを完全サポートし、正確なエラーハンドリングとインテリジェントな運用管理を可能にして、IoTシステムの安定性と応答性を向上させます。

**詳細はこちら:** [MQTT Reason Code Introduction and Quick Reference](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)

## 強化認証

MQTT 5.0は単純なパスワード認証の脆弱性（平文パスワード送信や相互認証の欠如）を解決するため、より安全な強化認証フレームワークを導入しました。AUTHパケットを利用し、複数回のメッセージ交換やDIGEST-MD5、SCRAM、KerberosなどのSASLメカニズムをサポートします。

EMQXはSCRAMをサポートし、ユーザーがより強力で柔軟な認証方式を選択してIoTインフラを保護できるようにします。

**詳細はこちら:** [Leveraging Enhanced Authentication for MQTT Security](https://www.emqx.com/en/blog/leveraging-enhanced-authentication-for-mqtt-security)

## コントロールパケット

MQTTのコントロールパケットは、クライアントとブローカー間の通信方法を定義し、接続、パブリッシュ、サブスクライブなど15種類の操作を含みます。各パケットは固定ヘッダー、任意の可変ヘッダー、任意のペイロードで構成され、軽量かつ効率的なデータ交換を実現します。MQTT 5.0ではプロパティや認証の改善により柔軟性がさらに向上しています。

**詳細はこちら:** [MQTT Control Packets: A Beginner's Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-control-packets)

MQTTに関する詳細情報は以下をご覧ください: [MQTT Guide 2025: Beginner to Advanced](https://www.emqx.com/en/mqtt-guide)
>>>>>>> origin/release-5.10
