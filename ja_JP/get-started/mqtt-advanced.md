# MQTT Advanced

## Retained Messages

MQTTのリテインドメッセージは、ブローカーが各トピックの最新のメッセージを保存できる機能であり、新たに接続したサブスクライバーが次のパブリッシュイベントを待つことなく、即座に最新のデータを受信できます。この仕組みは、スマートホームや産業用IoTのように、データ更新が稀でもリアルタイムの状態把握が重要なシナリオで特に有用です。

EMQXはMQTT 5.0に準拠したリテインドメッセージを完全にサポートしており、直感的なダッシュボードや管理APIを通じてリテインドメッセージの閲覧、管理、削除が可能です。メモリまたはディスクのストレージモード、メッセージの有効期限、最大リテインドエントリ数などを設定でき、システムの信頼性や永続化要件に合わせて調整できます。

試すには、`docker run -d --name emqx -p 18083:18083 -p 1883:1883 emqx/emqx-enterprise` を実行し、組み込みのダッシュボードからリテインドメッセージを簡単に管理できます。より高度なユースケースでは、セッション永続化、ワイルドカードサブスクライブ、メッセージの有効期限などのMQTT機能もサポートしており、堅牢なMQTTベースのアプリケーション構築に最適なプラットフォームです。

**詳細はこちら:** [MQTT Retained Messages: Beginner's Guide with Example](https://www.emqx.com/en/blog/mqtt5-features-retain-message#mqtt-retained-messages-in-emqx)

## Will Messages

MQTTのWill Messageは、クライアントが予期せず切断された際に他のクライアントへ通知したりバックアップデバイスに切り替えたりするなど、優雅な処理を可能にする重要な機能です。クライアントが接続時にWill Messageを指定すると、適切なDISCONNECTパケットを送信せずに切断された場合にサーバーがそのメッセージを送信します。この仕組みはクライアントの状態監視とIoTアプリケーションの信頼性確保に役立ちます。

MQTT 5.0ではWill Delay Intervalが導入され、一時的なネットワーク障害時の不要な通知を減らすためにWill Messageのパブリッシュを遅延させることが可能になりました。メッセージはサーバーのセッションに保存され、セッションの有効期限切れか遅延時間のいずれか早い方で配信されます。EMQXはWill Messageの機能をサポートし、将来のサブスクライバー向けの保持やセッション有効期限通知との連携によるクライアント状態監視の強化も可能です。この機能は、堅牢でリアルタイムなIoTアプリケーション構築に不可欠であり、システム運用者がデバイスやクライアントの状態変化を迅速に把握できるようにします。

**詳細はこちら:** [MQTT Will Message (Last Will & Testament) Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/use-of-mqtt-will-message)

## Request / Response

MQTT 5.0はRequest/Responseパターンを3つの主要な機能で改善しました。**Response Topic**はリクエスターが応答用のユニークなトピックを指定でき、競合を減らします。**Correlation Data**は非同期や複数レスポンダーのシナリオでも応答とリクエストを紐付けるのに役立ちます。**Response Information**はリクエスターがサーバー固有の情報を受け取り応答トピックの構築に利用できるため、トピックの権限管理を容易にします。これらの機能により、特に複雑なIoT環境でより信頼性が高く整理された通信が可能になります。

**詳細はこちら:** [MQTT Request / Response Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-request-response#suggestions-for-using-mqtt-request-response)

## User Properties

MQTT 5.0のUser Propertiesは、クライアントがメッセージにHTTPヘッダーのようなキーとバリューのペアでカスタムメタデータを付加できる機能です。これにより、ファイル転送、リソースフォーマット識別、インテリジェントなメッセージルーティングなどのユースケースでプロトコルの柔軟性が大幅に向上します。User Propertiesは接続時、パブリッシュ時、サブスクライブ時、切断時に追加可能で、クライアントとブローカー間でシームレスなメタデータ交換を実現します。例えば、ファイル情報、データフォーマット、地域タグなどを運び、サーバー側で効率的なメッセージ処理やトレーサブルなアプリケーションレベルのルーティングを可能にします。EMQXはUser Propertiesを完全にサポートし、MQTT.jsや今後のMQTTXなどのクライアントでも豊富な互換性を提供しています。

**詳細はこちら:** [User Properties - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-user-properties)

## Topic Alias

Topic Aliasは、長く繰り返し使用されるトピック名を2バイトの整数に短縮し、パブリッシュ時の帯域幅消費を削減する機能です。

EMQXはTopic Aliasをサポートしており、特に帯域幅が限られた環境でメッセージサイズを最適化し効率を向上させます。

**詳細はこちら:** [Topic Alias - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-topic-alias)

## Payload Format Indicator & Content Type

Payload Format IndicatorとContent Typeは、MQTT 5.0のメッセージ解析をより明確にする2つの重要なプロパティです。前者はペイロードがバイナリかUTF-8テキストかを識別し、後者はMIMEタイプ（例：`application/json`）でペイロードのフォーマットを示します。

これらにより、サブスクライバーは効率的にメッセージを解釈でき、トピック名の命名規則に依存しない柔軟なコンテンツ処理が可能になります。EMQXは両方のプロパティをサポートし、多様なIoTやメッセージングアプリケーションでスマートなペイロード処理を実現します。

**詳細はこちら:** [Introduction to MQTT Payload Format Indicator and Content Type | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-features-payload-format-indicator-and-content-type)

## Shared Subscriptions

MQTT 5.0のShared Subscriptionsは、複数のクライアントが単一トピックのメッセージ消費を共有し、負荷分散とシステムのスケーラビリティ向上を実現します。EMQXはこの機能を完全にサポートし、MQTT 3.1.1クライアントにも互換性を拡張しているため、既存デバイスはコード変更なしで`$share/{group}/{topic}`形式を使うだけで共有消費の恩恵を受けられます。

この仕組みはスループットを向上させ、単一クライアントのボトルネックを防ぎ、高可用性を確保します。ラウンドロビン、ハッシュ、ローカルファーストなど複数のロードバランシング戦略を標準搭載し、クラスター環境でのトラフィック分散を柔軟に管理できます。

**詳細はこちら:** [MQTT Shared Subscriptions: Practical Guidelines and Use Cases | MQTT 5 Features](https://www.emqx.com/en/blog/introduction-to-mqtt5-protocol-shared-subscription)

## Subscription Options

MQTTのSubscription Optionsは、クライアントがメッセージ受信方法をカスタマイズできる機能です。MQTT 5.0では、QoS、No Local、Retain As Published、Retain Handlingの4つの主要オプションが導入され、柔軟性と制御性を高めています。これらにより、メッセージ品質の管理、ブリッジでのメッセージループ回避、リテインフラグの保持、リテインドメッセージの受信タイミングの制御が可能です。

EMQXはMQTT 5.0のすべてのサブスクリプションオプションを完全サポートし、例えばNo Localでブリッジ設定時のメッセージストームを防ぎ、Retain As Publishedでブローカー間のリテインドメッセージ処理の一貫性を確保します。

**詳細はこちら:** [MQTT Subscription Options Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/an-introduction-to-subscription-options-in-mqtt)

## Subscription Identifier

MQTT 5.0のSubscription Identifierは、各サブスクリプションに一意の識別子を割り当て、クライアントが複数のサブスクリプションを効率的に管理・追跡できる機能です。多数のサブスクリプションを持つ複雑なアプリケーションで特に有用です。

EMQXはこの機能を完全にサポートし、ワイルドカードや重複するサブスクリプションの場合でも、メッセージがどのサブスクリプション由来かを正確に識別可能です。PUBLISHパケットにSubscription Identifierを含めることで、クライアント側でのトピックフィルタマッチングを不要にし、メッセージ処理効率を大幅に向上させ、正確なコールバック実行を実現します。

**詳細はこちら:** [MQTT Subscription Identifier Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/subscription-identifier-and-subscription-options)

## Keep Alive

MQTTのKeep Alive機構は、クライアントが一定間隔内に定期的にパケットを送信することで半開きTCP接続を防ぎます。データ送信がない場合は`PINGREQ`で接続確認を行います。EMQXはクライアント指定およびサーバー強制のKeep Alive値を完全サポートし、MQTT 5.0のServer Keep Alive機能も含みます。`server_keepalive`や`keepalive_backoff`などの設定項目で接続タイムアウトを細かく制御でき、IoT環境での信頼性向上やクライアントの予期せぬ切断時のWill Messageの適時配信を実現します。

**詳細はこちら:** [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive)

## Message Expiry Interval

MQTT 5.0のMessage Expiry Intervalは、メッセージが即時配信できない場合にブローカーが保持する期間の上限をクライアントが設定できる機能です。期限切れ後はメッセージが破棄され、古い情報の送信を防ぎます。

EMQXはこの機能を完全サポートし、メッセージの転送やブリッジ時に有効期限を減算して分散環境全体でメッセージの鮮度を保ちます。

**詳細はこちら:** [Introduction to MQTT Message Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt-message-expiry-interval)

## Maximum Packet Size

MQTT 5.0のMaximum Packet Sizeプロパティは、クライアントとサーバーがパケットサイズの安全な上限を交渉し、リソース制約のあるデバイスでの過負荷を防ぐ機能です。クライアントは`CONNECT`パケットで受信可能な最大サイズを宣言し、サーバーは`CONNACK`パケットで自身の制限を応答します。EMQXはこの双方向制約を厳格に適用し、サイズ超過のメッセージを破棄したり、応答パケットからUser Propertiesなどの低優先度メタデータを削除して接続安定性を維持します。共有サブスクリプション環境では、サイズ超過メッセージを適格なグループメンバーにリダイレクトすることも可能です。

**詳細はこちら:** [MQTT Maximum Packet Size Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/best-practices-of-maximum-packet-size-in-mqtt)

## Reason Codes

MQTT 5.0は、MQTT 3.1.1の限定的なステータス応答に比べて大幅に強化されたReason Codeシステムを導入しました。これらのReason Codeは接続、パブリッシュ、サブスクライブなどの操作結果を詳細に示し、開発者が問題を迅速に診断し、デバイス管理やメッセージ処理の最適化を行いやすくします。

EMQXはMQTT 5.0のReason Codeを完全サポートし、正確なエラー処理とインテリジェントな運用管理を可能にしてIoTシステムの安定性と応答性を向上させます。

**詳細はこちら:** [MQTT Reason Code Introduction and Quick Reference](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)

## Enhanced Authentication

MQTT 5.0はEnhanced Authenticationを導入し、単純なパスワード認証の脆弱性（平文パスワード送信や相互認証の欠如など）を解消するより安全な認証フレームワークを提供します。Enhanced AuthenticationはAUTHパケットを利用し、複数回のメッセージ交換やDIGEST-MD5、SCRAM、KerberosなどのSASLメカニズムをサポートします。

EMQXはSCRAMをサポートしており、ユーザーはより強力で柔軟な認証方式を選択してIoTインフラを保護できます。

**詳細はこちら:** [Leveraging Enhanced Authentication for MQTT Security](https://www.emqx.com/en/blog/leveraging-enhanced-authentication-for-mqtt-security)

## Control Packets

MQTTのコントロールパケットは、クライアントとブローカー間の通信方法を定義し、接続、パブリッシュ、サブスクライブなど15種類の操作を含みます。各パケットは固定ヘッダー、オプションの可変ヘッダー、オプションのペイロードで構成され、軽量かつ効率的なデータ交換を実現します。MQTT 5.0ではプロパティの導入や認証の改善により柔軟性がさらに向上しています。

**詳細はこちら:** [MQTT Control Packets: A Beginner's Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-control-packets)

MQTTに関する詳細は、こちらをご覧ください: [MQTT Guide 2025: Beginner to Advanced](https://www.emqx.com/en/mqtt-guide)
