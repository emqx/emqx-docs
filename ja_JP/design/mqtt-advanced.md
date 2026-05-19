# MQTT 高度機能

## リテインドメッセージ

MQTTのリテインドメッセージは、ブローカーが各トピックの最新メッセージを保存し、新たに接続したサブスクライバーが次のパブリッシュイベントを待つことなく最新のデータを即座に受信できるようにする機能です。この仕組みは、スマートホームや産業用IoTのように、データ更新が頻繁でなくてもリアルタイムの状態把握が重要なシナリオで特に有用です。

EMQXはMQTT 5.0に準拠したリテインドメッセージを完全にサポートしており、直感的なダッシュボードや管理APIを通じてリテインドメッセージの閲覧、管理、削除が可能です。ストレージモード（メモリまたはディスク）、メッセージの有効期限、最大リテインドエントリ数などを設定し、システムの信頼性や永続化要件に合わせて調整できます。

試すには、単に `docker run -d --name emqx -p 18083:18083 -p 1883:1883 emqx/emqx-enterprise` を実行し、組み込みのダッシュボードにアクセスしてリテインドメッセージを簡単に管理してください。より高度なユースケースには、セッション永続化、ワイルドカードサブスクライブ、メッセージ有効期限などのMQTT機能もサポートしており、堅牢なMQTTベースのアプリケーション構築に最適なプラットフォームです。

**詳細はこちら:** [MQTT Retained Messages: Beginner's Guide with Example](https://www.emqx.com/en/blog/mqtt5-features-retain-message#mqtt-retained-messages-in-emqx)

## ウィルメッセージ

MQTTのウィルメッセージは、クライアントが予期せず切断された場合に他のクライアントへ通知したりバックアップ機器に切り替えたりするための重要な機能です。クライアントが接続時にウィルメッセージを指定すると、クライアントが正常なDISCONNECTパケットを送信せずに切断された場合にサーバーがそのメッセージを送信します。この仕組みによりクライアントの状態監視とIoTアプリケーションの信頼性確保が可能になります。

MQTT 5.0ではウィル遅延間隔（Will Delay Interval）が導入され、一時的なネットワーク障害時の不要な通知を減らすためにウィルメッセージのパブリッシュを遅延させることができます。メッセージはサーバーのセッションに保存され、セッションの有効期限切れまたは遅延間隔のどちらか早い方で配信されます。EMQXはウィルメッセージの保持やセッション有効期限通知との連携をサポートし、クライアント状態の高度な監視を実現します。この機能はレジリエントでリアルタイムなIoTアプリケーション構築に不可欠であり、システム運用者にデバイスやクライアントの状態変化を迅速に通知します。

**詳細はこちら:** [MQTT Will Message (Last Will & Testament) Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/use-of-mqtt-will-message)

## リクエスト／レスポンス

MQTT 5.0はリクエスト／レスポンスパターンを3つの主要機能で改善しました。**レスポンストピック**はリクエスターが応答用の固有トピックを指定でき、競合を減らします。**相関データ（Correlation Data）**は非同期や複数レスポンダーのシナリオで応答とリクエストを対応付けます。**レスポンス情報（Response Information）**はレスポンストピック構築のためのサーバー固有情報をリクエスターに提供し、トピック権限管理を支援します。これらの機能により、特に複雑なIoT環境で信頼性と整理された通信が実現します。

**詳細はこちら:** [MQTT Request / Response Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-request-response#suggestions-for-using-mqtt-request-response)

## ユーザープロパティ

MQTT 5.0のユーザープロパティは、HTTPヘッダーのようにキーと値のペアでクライアントがメッセージにカスタムメタデータを添付できる機能です。これにより、ファイル転送、リソースフォーマット識別、インテリジェントなメッセージルーティングなど、プロトコルの柔軟性が大幅に向上します。ユーザープロパティは接続時、パブリッシュ時、サブスクライブ時、切断時に追加可能で、クライアントとブローカー間でシームレスにメタデータを交換できます。例えば、ファイル情報、データ形式、地域タグなどを運び、サーバー側で効率的なメッセージ処理やトレーサブルなアプリケーションレベルルーティングを実現します。EMQXはユーザープロパティを完全サポートし、MQTT.jsや今後のMQTTXなどクライアントとの高い互換性を提供します。

**詳細はこちら:** [User Properties - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-user-properties)

## トピックエイリアス

トピックエイリアスは、長く繰り返し使用されるトピック名を2バイトの整数に短縮し、パブリッシュ時の帯域幅消費を削減する機能です。

EMQXはトピックエイリアスをサポートし、特に帯域幅が限られた環境でメッセージサイズを最適化し効率を向上させます。

**詳細はこちら:** [Topic Alias - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-topic-alias)

## ペイロードフォーマットインジケーター＆コンテンツタイプ

ペイロードフォーマットインジケーターとコンテンツタイプは、MQTT 5.0の重要なプロパティで、メッセージ解析をより明確にします。前者はペイロードがバイナリかUTF-8テキストかを識別し、後者はMIMEタイプ（例：`application/json`）でペイロードのフォーマットを示します。

これらによりサブスクライバーは効率的にメッセージを解釈でき、トピック名に依存しない柔軟なコンテンツ処理が可能になります。EMQXは両プロパティをサポートし、多様なIoTやメッセージングアプリケーションでスマートなペイロード処理を実現します。

**詳細はこちら:** [Introduction to MQTT Payload Format Indicator and Content Type | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-features-payload-format-indicator-and-content-type)

## 共有サブスクリプション

MQTT 5.0の共有サブスクリプションは、複数のクライアントが単一トピックのメッセージ消費を共有し、負荷分散とシステムのスケーラビリティ向上を実現します。EMQXはこの機能を完全サポートし、MQTT 3.1.1クライアントにも互換性を拡張しているため、既存デバイスはコード変更なしで`$share/{group}/{topic}`形式を使うだけで共有消費の恩恵を受けられます。

この仕組みはスループットを向上させ、単一クライアントのボトルネックを防ぎ、高可用性を確保します。ラウンドロビン、ハッシュ、ローカルファーストなど複数のロードバランシング戦略を内蔵し、クラスター環境で柔軟なトラフィック管理を支援します。

**詳細はこちら:** [MQTT Shared Subscriptions: Practical Guidelines and Use Cases | MQTT 5 Features](https://www.emqx.com/en/blog/introduction-to-mqtt5-protocol-shared-subscription)

## サブスクリプションオプション

MQTTのサブスクリプションオプションは、クライアントがメッセージ受信方法をカスタマイズできる機能です。MQTT 5.0では、QoS、No Local、Retain As Published、Retain Handlingの4つの主要オプションが導入され、柔軟性と制御性が向上しました。これらにより、メッセージ品質の管理、ブリッジでのメッセージループ回避、リテインフラグの保持、リテインドメッセージ受信タイミングの制御が可能です。

EMQXはMQTT 5.0のすべてのサブスクリプションオプションを完全サポートし、例えばNo Localでブリッジ環境のメッセージ嵐を防止し、Retain As Publishedでブローカー間の一貫したリテインドメッセージ処理を実現します。

**詳細はこちら:** [MQTT Subscription Options Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/an-introduction-to-subscription-options-in-mqtt)

## サブスクリプション識別子

MQTT 5.0のサブスクリプション識別子は、各サブスクリプションに固有の識別子を割り当て、クライアントが複数のサブスクリプションを効率的に管理・追跡できる機能です。多数のサブスクリプションを扱う複雑なアプリケーションで特に有用です。

EMQXはこの機能を完全サポートし、ワイルドカードや重複するサブスクリプションでもメッセージの発信元を正確に特定可能です。PUBLISHパケットにサブスクリプション識別子を含めることで、クライアント側でのトピックフィルタマッチングを不要にし、メッセージ処理効率を大幅に向上させ、正確なコールバック実行を可能にします。

**詳細はこちら:** [MQTT Subscription Identifier Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/subscription-identifier-and-subscription-options)

## キープアライブ

MQTTのキープアライブ機構は、クライアントが一定間隔内に定期的なパケットを送信することで半開きTCP接続を防止します。データ送信がない場合は`PINGREQ`で接続確認を行います。EMQXはクライアント指定およびサーバー強制のキープアライブ値を完全サポートし、MQTT 5.0のサーバーキープアライブ機能も含みます。`server_keepalive`や`keepalive_backoff`などの設定項目により接続タイムアウトを細かく制御でき、IoT環境での信頼性向上やクライアント切断時のウィルメッセージ適時配信を実現します。

**詳細はこちら:** [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive)

## メッセージ有効期限間隔

MQTT 5.0のメッセージ有効期限間隔は、クライアントがメッセージをブローカーに保存させる最大時間を設定できる機能です。即時配信できない場合、この間隔を過ぎるとメッセージは破棄され、古い情報の送信を防ぎます。

EMQXはこの機能を完全サポートし、メッセージ転送やブリッジ時に有効期限間隔を減算して分散環境でのメッセージ鮮度を維持します。

**詳細はこちら:** [Introduction to MQTT Message Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt-message-expiry-interval)

## 最大パケットサイズ

MQTT 5.0の最大パケットサイズプロパティは、クライアントとサーバーが安全なパケットサイズ上限を交渉し、リソース制約のあるデバイスでの過負荷を防ぐ機能です。クライアントは`CONNECT`パケットで受信可能な最大サイズを宣言し、サーバーは`CONNACK`パケットで自身の上限を返します。EMQXはこの双方向制約を強制し、規定サイズを超えるメッセージは破棄するか、レスポンスパケットの低優先度メタデータ（ユーザープロパティなど）を削除して接続安定性を保ちます。共有サブスクリプション環境では、EMQXがサイズ超過メッセージを適格なグループメンバーにリダイレクトすることも可能です。

**詳細はこちら:** [MQTT Maximum Packet Size Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/best-practices-of-maximum-packet-size-in-mqtt)

## リーズンコード

MQTT 5.0はMQTT 3.1.1の限定的なステータス応答を大幅に拡充した包括的なリーズンコードシステムを導入しました。これにより接続、パブリッシュ、サブスクライブなどの操作結果を詳細に示し、開発者が迅速に問題を診断し、デバイス管理やメッセージ処理を最適化できます。

EMQXはMQTT 5.0のリーズンコードを完全サポートし、正確なエラー処理とインテリジェントな運用管理を可能にしてIoTシステムの安定性と応答性を向上させます。

**詳細はこちら:** [MQTT Reason Code Introduction and Quick Reference](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)

## 強化認証

MQTT 5.0は単純なパスワード認証の脆弱性（平文パスワード送信や相互認証の欠如）を解決するため、より安全な強化認証フレームワークを導入しました。AUTHパケットを活用し、複数回のメッセージ交換やDIGEST-MD5、SCRAM、KerberosなどのSASLメカニズムをサポートします。

EMQXはSCRAMをサポートし、ユーザーがより強力で柔軟な認証方式を選択してIoTインフラを保護できるようにします。

**詳細はこちら:** [Leveraging Enhanced Authentication for MQTT Security](https://www.emqx.com/en/blog/leveraging-enhanced-authentication-for-mqtt-security)

## コントロールパケット

MQTTのコントロールパケットは、クライアントとブローカー間の通信方法を定義し、接続、パブリッシュ、サブスクライブなど15種類の操作を含みます。各パケットは固定ヘッダー、可変ヘッダー（オプション）、ペイロード（オプション）で構成され、軽量かつ効率的なデータ交換を実現します。MQTT 5.0ではプロパティや認証の改善によりさらに柔軟性が向上しています。

**詳細はこちら:** [MQTT Control Packets: A Beginner's Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-control-packets)

MQTTに関する詳細は以下をご覧ください: [MQTT Guide 2025: Beginner to Advanced](https://www.emqx.com/en/mqtt-guide)
