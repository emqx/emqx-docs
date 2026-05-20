# MQTTの基本

## MQTTとは何か、そしてなぜ重要なのか

MQTT（Message Queuing Telemetry Transport）は、遅延が大きいまたは信頼性の低いネットワークに最適化された軽量のパブリッシュ・サブスクライブ型メッセージングプロトコルであり、IoTシナリオに最適です。デバイスがブローカー方式を用いてリアルタイムに通信できるようにし、スマートホーム、産業オートメーション、コネクテッドカーの標準として広く利用されています。

**詳しくはこちら:** [Mastering MQTT: The Ultimate Beginner's Guide for 2025](https://www.emqx.com/en/blog/the-easiest-guide-to-getting-started-with-mqtt)

## MQTTのパブリッシュ・サブスクライブパターン

MQTTのパブリッシュ・サブスクライブパターンは、メッセージの生成者（パブリッシャー）と消費者（サブスクライバー）を疎結合にします。パブリッシャーはサブスクライバーの存在を知らずにトピックにメッセージをパブリッシュし、サブスクライバーはパブリッシャーを意識せずにトピックからメッセージを受信します。このモデルにより、メッセージ配信のスケーラビリティと柔軟性が向上します。

**詳しくはこちら:** [Introduction to MQTT Publish-Subscribe Pattern](https://www.emqx.com/en/blog/mqtt-5-introduction-to-publish-subscribe-model)

## MQTT接続の確立方法

MQTT接続の確立は、クライアントがブローカーに接続を開始し、必要に応じて認証情報を提供し、キープアライブ間隔やクリーンセッションフラグなどの接続パラメータを指定することを含みます。適切な設定により、信頼性が高く安全な通信が実現します。

**詳しくはこちら:** [How to Set Parameters When Establishing an MQTT Connection?](https://www.emqx.com/en/blog/how-to-set-parameters-when-establishing-an-mqtt-connection)

## MQTTのトピックとワイルドカード

MQTTのトピックはパブリッシュ／サブスクライブメッセージングのルーティング構造を定義します。階層的なトピックレベルとワイルドカード（単一レベルは`+`、複数レベルは`#`）を使うことで、開発者はデバイス群やセンサー種別全体を柔軟にサブスクライブできます。EMQXはMQTT標準のトピックマッチングを完全にサポートし、ワイルドカードサブスクリプション管理、共有サブスクリプション、細粒度アクセス制御など強力な機能を提供します。トピックとワイルドカードの仕組みを理解することは、EMQXでスケーラブルかつ安全で効率的なIoTアプリケーションを構築する鍵となります。

**詳しくはこちら:** [MQTT Topics and Wildcards: A Beginner's Guide](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics)

## MQTTセッション

MQTTセッションは、ネットワーク切断時のメッセージロスや再サブスクライブのオーバーヘッドといった問題に対応し、クライアントのコンテキストを維持します。セッションはクライアントのサブスクリプションやQoSメッセージなど重要なデータを保存し、切断後もシームレスな通信を可能にします。MQTT 5.0では、**Clean Start**や**Session Expiry Interval**といったセッションライフサイクルを制御する重要なパラメータが導入され、セッション保持の柔軟性が向上しました。Clean Startは既存セッションの再利用可否を決定し、Session Expiry Intervalは切断後のセッション保持期間を定義します。EMQXはセッション管理を強化し、セッション有効期限の細かい制御を提供することで、特に断続的な接続環境のIoTシナリオにおいてリソース効率とシステム信頼性を向上させます。

**詳しくはこちら:** [Introduction to MQTT Clean Start and Session Expiry Interval | MQTT 5 Features](https://www.emqx.com/en/blog/mqtt5-new-feature-clean-start-and-session-expiry-interval)

## MQTTのQoSとプロトコルフロー

MQTTはメッセージ配信の信頼性とネットワーク効率のバランスを取るために、3つのQoS（サービス品質）レベルを定義しています：

- QoS 0：最大1回の配信（at most once）
- QoS 1：少なくとも1回の配信（at least once）
- QoS 2：ちょうど1回の配信（exactly once）

各レベルは、クライアントとブローカー間のプロトコルフローを規定し、目的とする配信保証を実現します。

**詳しくはこちら:** [MQTT QoS 0, 1, 2 Explained: A Quickstart Guide](https://www.emqx.com/en/blog/introduction-to-mqtt-qos)
