# MQTTの基本

## MQTTとは何か、そしてなぜ重要か

MQTT（Message Queuing Telemetry Transport）は、レイテンシが高いまたは信頼性の低いネットワークに最適化された軽量のパブリッシュ・サブスクライブ型メッセージングプロトコルであり、IoTシナリオに最適です。デバイスがブローカー方式を用いてリアルタイムに通信できるようにし、スマートホーム、産業オートメーション、コネクテッドカーの標準として広く採用されています。

**詳細はこちら:** [Mastering MQTT: The Ultimate Beginner's Guide for 2025](https://www.emqx.com/en/blog/the-easiest-guide-to-getting-started-with-mqtt)

## MQTTのパブリッシュ・サブスクライブパターン

MQTTにおけるパブリッシュ・サブスクライブパターンは、メッセージの生成者（パブリッシャー）と消費者（サブスクライバー）を分離します。パブリッシャーはサブスクライバーを意識せずにトピックにメッセージをパブリッシュし、サブスクライバーはパブリッシャーを意識せずにトピックからメッセージを受信します。このモデルにより、メッセージ配信のスケーラビリティと柔軟性が向上します。

**詳細はこちら:** [Introduction to MQTT Publish-Subscribe Pattern](https://www.emqx.com/en/blog/mqtt-5-introduction-to-publish-subscribe-model)

## MQTT接続の確立方法

MQTT接続の確立は、クライアントがブローカーに接続を開始し、必要に応じて認証情報を提供し、キープアライブ間隔やクリーンセッションフラグなどの接続パラメータを指定することを含みます。適切な設定により、信頼性が高く安全な通信が保証されます。

**詳細はこちら:** [How to Set Parameters When Establishing an MQTT Connection?](https://www.emqx.com/en/blog/how-to-set-parameters-when-establishing-an-mqtt-connection)

## MQTTのトピックとワイルドカード

MQTTのトピックはパブリッシュ／サブスクライブメッセージングのルーティング構造を定義します。階層的なトピックレベルとワイルドカード（単一レベルの`+`、複数レベルの`#`）を使うことで、開発者はデバイスグループやセンサー種別全体を柔軟にサブスクライブできます。EMQXはMQTT標準のトピックマッチングを完全にサポートし、ワイルドカードサブスクリプション管理、共有サブスクリプション、細かなアクセス制御など強力な機能を提供します。トピックとワイルドカードの仕組みを理解することは、EMQXでスケーラブルかつ安全で効率的なIoTアプリケーションを構築する鍵となります。

**詳細はこちら:** [MQTT Topics and Wildcards: A Beginner's Guide](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics)

## MQTTセッション

MQTTのセッションは、ネットワーク切断時のメッセージの取りこぼしや再サブスクライブのオーバーヘッドといった問題に対処し、クライアントの状態を維持します。セッションはクライアントのサブスクリプションやQoSメッセージなど重要なデータを保存し、切断後もシームレスな通信を可能にします。MQTT 5.0では、**Clean Start**や**Session Expiry Interval**といったセッションライフサイクルを制御する主要パラメータが導入され、セッションの保持に柔軟性が生まれました。Clean Startは既存セッションの再利用可否を決定し、Session Expiry Intervalは切断後のセッション保持期間を定義します。EMQXはセッション管理を強化し、セッション有効期限の細かな制御を提供することで、特に断続的な接続が多いIoT環境におけるリソース効率とシステムの信頼性を向上させています。

**詳細はこちら:** [Introduction to MQTT Clean Start and Session Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-feature-clean-start-and-session-expiry-interval)

## MQTTのQoSとプロトコルフロー

MQTTはメッセージ配信の信頼性とネットワーク効率のバランスを取るために、3つのQoS（サービス品質）レベルを定義しています：

- QoS 0：最大1回の配信（At most once）
- QoS 1：少なくとも1回の配信（At least once）
- QoS 2：ちょうど1回の配信（Exactly once）

各レベルは、クライアントとブローカー間のプロトコルフローを規定し、望ましい配信保証を実現します。

**詳細はこちら:** [MQTT QoS 0, 1, 2 Explained: A Quickstart Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-qos)
