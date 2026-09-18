# MQTT 高度機能

## リテインドメッセージ

MQTTのリテインドメッセージは、ブローカーが各トピックの最新メッセージを保存し、新たに接続したサブスクライバーが次のパブリッシュイベントを待たずに最新データを即座に受信できるようにする機能です。この仕組みは、スマートホームや産業用IoTのように、データ更新が稀でもリアルタイムの状態把握が重要なシナリオで特に有用です。

EMQXはMQTT 5.0に準拠したリテインドメッセージを完全サポートしており、直感的なダッシュボードや管理APIを通じてリテインドメッセージの閲覧、管理、削除が可能です。メモリまたはディスクのストレージモード、メッセージの有効期限、最大リテインドエントリ数などを設定し、システムの信頼性と永続化要件に合わせて調整できます。

試すには、`docker run -d --name emqx -p 18083:18083 -p 1883:1883 emqx/emqx-enterprise` を実行し、組み込みのダッシュボードにアクセスしてリテインドメッセージを簡単に管理してください。より高度なユースケースには、セッション永続化、ワイルドカードサブスクライブ、メッセージ有効期限などMQTTの機能もサポートしており、堅牢なMQTTベースのアプリケーション構築に最適なプラットフォームです。

**詳細はこちら:** [MQTT Retained Messages: Beginner's Guide with Example](https://www.emqx.com/en/blog/mqtt5-features-retain-message#mqtt-retained-messages-in-emqx)

## ウィルメッセージ

MQTTのウィルメッセージは、クライアントが予期せず切断された場合に他のクライアントへの通知やバックアップ機器への切り替えなどの適切な処理を行うための重要な機能です。クライアント接続時にウィルメッセージを指定でき、クライアントが正常なDISCONNECTパケットを送信せずに切断した場合にサーバーがそのメッセージを送信します。この仕組みはクライアントの状態監視とIoTアプリケーションの信頼性確保に役立ちます。

MQTT 5.0ではウィル遅延間隔が導入され、一時的なネットワーク障害時の不要な通知を減らすためにウィルメッセージの公開を遅延できます。メッセージはサーバーのセッションに保存され、セッションの有効期限切れまたは遅延間隔のいずれか早い方で配信されます。EMQXはウィルメッセージ機能をサポートし、将来のサブスクライバー向けの保持やセッション有効期限通知との連携によるクライアント状態監視を強化しています。この機能はレジリエントでリアルタイムなIoTアプリケーション構築に不可欠であり、システム運用者がデバイスやクライアントの状態変化を迅速に把握できるようにします。

**詳細はこちら:** [MQTT Will Message (Last Will & Testament) Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/use-of-mqtt-will-message)

## リクエスト／レスポンス

MQTT 5.0はリクエスト／レスポンスパターンを3つの主要機能で改善しました。**レスポンストピック**はリクエスターがレスポンス用のユニークなトピックを指定でき、競合を減らします。**相関データ**は非同期や複数レスポンダーのシナリオでレスポンスとリクエストの対応付けを助けます。**レスポンス情報**はリクエスターがサーバー固有の情報を受け取り、レスポンストピックの構築やトピック権限管理を容易にします。これらの機能により、特に複雑なIoT環境で信頼性の高い整理された通信が実現します。

**詳細はこちら:** [MQTT Request / Response Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-request-response#suggestions-for-using-mqtt-request-response)

## ユーザープロパティ

MQTT 5.0のユーザープロパティは、HTTPヘッダーのようにクライアントがメッセージにキーと値のペアでカスタムメタデータを付加できる機能です。これにより、ファイル転送、リソース形式の識別、インテリジェントなメッセージルーティングなど、プロトコルの柔軟性が大幅に向上します。ユーザープロパティは接続時、パブリッシュ時、サブスクライブ時、切断時に追加可能で、クライアントとブローカー間のシームレスなメタデータ交換を実現します。例えば、ファイル情報、データ形式、地域タグなどを運び、サーバーが効率的にメッセージを処理し、トレーサブルでアプリケーションレベルのルーティングを可能にします。EMQXはユーザープロパティを完全サポートし、MQTT.jsや今後のMQTTXなどクライアントでの豊富な互換性を提供します。

**詳細はこちら:** [User Properties - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-user-properties)

## トピックエイリアス

トピックエイリアスは、長く繰り返し使用されるトピック名を2バイトの整数に短縮し、パブリッシュ時の帯域幅消費を削減する機能です。

EMQXはトピックエイリアスをサポートし、特に帯域幅が限られた環境でメッセージサイズを最適化し効率を向上させます。

**詳細はこちら:** [Topic Alias - MQTT 5.0 new features](https://www.emqx.com/en/blog/mqtt5-topic-alias)

## ペイロードフォーマットインジケーターとコンテンツタイプ

ペイロードフォーマットインジケーターとコンテンツタイプは、MQTT 5.0の重要なプロパティで、メッセージ解析をより明確にします。前者はペイロードがバイナリかUTF-8テキストかを識別し、後者はMIMEタイプ（例：`application/json`）でペイロードの形式を示します。

これらにより、サブスクライバーは効率的にメッセージを解釈でき、トピック名の命名規則に依存しない柔軟なコンテンツ処理が可能になります。EMQXは両方のプロパティをサポートし、多様なIoTやメッセージングアプリケーションでスマートなペイロード処理を実現します。

**詳細はこちら:** [Introduction to MQTT Payload Format Indicator and Content Type | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-features-payload-format-indicator-and-content-type)

## 共有サブスクリプション

MQTT 5.0の共有サブスクリプションは、複数のクライアントが単一トピックのメッセージ消費を共有し、負荷分散とシステムのスケーラビリティ向上を実現します。EMQXはこの機能を完全サポートし、MQTT 3.1.1クライアントにも対応しているため、既存デバイスはコード変更なしで`$share/{group}/{topic}`形式を使うだけで共有消費の恩恵を受けられます。

この仕組みはスループットを向上させ、単一クライアントのボトルネックを防ぎ、高可用性を確保します。ラウンドロビン、ハッシュ、ローカルファーストなど複数のロードバランシング戦略を内蔵し、クラスター環境でのトラフィック分散管理を柔軟にサポートします。

**詳細はこちら:** [MQTT Shared Subscriptions: Practical Guidelines and Use Cases | MQTT 5 Features](https://www.emqx.com/en/blog/introduction-to-mqtt5-protocol-shared-subscription)

## サブスクリプションオプション

MQTTのサブスクリプションオプションは、クライアントがメッセージ受信方法をカスタマイズできる機能です。MQTT 5.0ではQoS、No Local、Retain As Published、Retain Handlingの4つの主要オプションが導入され、柔軟性と制御性を高めています。これらにより、メッセージ品質の管理、ブリッジング時のメッセージループ回避、リテインフラグの保持、リテインドメッセージ受信タイミングの選択が可能です。

EMQXはMQTT 5.0の全サブスクリプションオプションを完全サポートし、例えばNo Localでブリッジ環境のメッセージ嵐を防ぎ、Retain As Publishedでブローカー間のリテインドメッセージ処理を一貫させるなど、細かなメッセージ配信制御を実現します。

**詳細はこちら:** [MQTT Subscription Options Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/an-introduction-to-subscription-options-in-mqtt)

## サブスクリプション識別子

MQTT 5.0のサブスクリプション識別子は、各サブスクリプションに固有の識別子を割り当て、クライアントが複数のサブスクリプションを効率的に管理・追跡できる機能です。多数のサブスクリプションを扱う複雑なアプリケーションで特に有用です。

EMQXはこの機能を完全サポートし、ワイルドカードや重複サブスクリプションの場合でもメッセージの発信元サブスクリプションを正確に識別可能です。PUBLISHパケットにサブスクリプション識別子を含めることで、クライアント側でのトピックフィルターマッチングが不要となり、メッセージ処理効率が大幅に向上し、正確なコールバック実行が可能になります。

**詳細はこちら:** [MQTT Subscription Identifier Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/subscription-identifier-and-subscription-options)

## キープアライブ

MQTTのキープアライブ機構は、クライアントが一定間隔内に定期的にパケットを送信することを要求し、半開きTCP接続を防止します。データ送信がない場合は`PINGREQ`で接続確認を行います。EMQXはクライアント指定およびサーバー強制のキープアライブ値を完全サポートし、MQTT 5.0のサーバーキープアライブ機能も含みます。`server_keepalive`や`keepalive_backoff`などの設定項目により、接続タイムアウトを細かく制御でき、IoT展開の信頼性を高め、クライアントが予期せず切断された際のウィルメッセージの適時配信を保証します。

**詳細はこちら:** [What is the MQTT Keep Alive parameter for?](https://www.emqx.com/en/blog/mqtt-keep-alive)

## メッセージ有効期限間隔

MQTT 5.0のメッセージ有効期限間隔は、クライアントがメッセージを即時配信できない場合にブローカーが保存する期間の上限を設定できる機能です。期限切れ後はメッセージが破棄され、古い情報の送信を防ぎます。

EMQXはこの機能を完全サポートし、メッセージ転送やブリッジング時に有効期限を減算し、分散展開におけるメッセージの鮮度を維持します。

**詳細はこちら:** [Introduction to MQTT Message Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt-message-expiry-interval)

## 最大パケットサイズ

MQTT 5.0の最大パケットサイズプロパティは、クライアントとサーバーが安全なパケットサイズの上限を交渉し、リソース制約のあるデバイスでの過負荷を防ぐ機能です。クライアントは`CONNECT`パケットで受信可能な最大サイズを宣言し、サーバーは`CONNACK`パケットで応答します。EMQXはこの双方向制約を適用し、規定サイズを超えるメッセージは破棄したり、レスポンスパケットからユーザープロパティなど優先度の低いメタデータを削除して接続の安定性を保ちます。共有サブスクリプションの場面では、EMQXはサイズ超過メッセージを適格なグループメンバーにリダイレクトすることも可能です。

**詳細はこちら:** [MQTT Maximum Packet Size Explained and Example | MQTT 5 Features](https://www.emqx.com/en/blog/best-practices-of-maximum-packet-size-in-mqtt)

## リーズンコード

MQTT 5.0は包括的なリーズンコードシステムを導入し、MQTT 3.1.1の限定的なステータス応答に比べてプロトコルのフィードバックを大幅に強化しました。これらのリーズンコードは接続、パブリッシュ、サブスクライブなどの操作結果を詳細に示し、開発者が問題を迅速に診断し、デバイス管理やメッセージ処理を最適化できるようにします。

EMQXはMQTT 5.0のリーズンコードを完全サポートし、正確なエラー処理とインテリジェントな運用管理を可能にしてIoTシステムの安定性と応答性を向上させます。

**詳細はこちら:** [MQTT Reason Code Introduction and Quick Reference](https://www.emqx.com/en/blog/mqtt5-new-features-reason-code-and-ack)

## 強化認証

MQTT 5.0は単純なパスワード認証の脆弱性（平文パスワード送信や相互認証の欠如）を解決するため、より安全な強化認証フレームワークを導入しました。強化認証はAUTHパケットを活用し、複数回のメッセージ交換やDIGEST-MD5、SCRAM、KerberosなどのSASLメカニズムをサポートします。

EMQXはSCRAMをサポートし、ユーザーがより強力で柔軟な認証方式を選択してIoTインフラを保護できるようにしています。

**詳細はこちら:** [Leveraging Enhanced Authentication for MQTT Security](https://www.emqx.com/en/blog/leveraging-enhanced-authentication-for-mqtt-security)

## コントロールパケット

MQTTのコントロールパケットは、クライアントとブローカー間の通信方法を定義し、接続、パブリッシュ、サブスクライブなど15種類の操作を含みます。各パケットは固定ヘッダー、任意の可変ヘッダー、任意のペイロードで構成され、軽量かつ効率的なデータ交換を実現します。MQTT 5.0ではプロパティの導入や認証の改善により柔軟性がさらに向上しています。

**詳細はこちら:** [MQTT Control Packets: A Beginner's Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-control-packets)

MQTTに関する詳細は以下をご覧ください: [MQTT Guide 2025: Beginner to Advanced](https://www.emqx.com/en/mqtt-guide)
