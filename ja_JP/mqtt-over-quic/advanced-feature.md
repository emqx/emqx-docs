# 高度な機能

MQTTブローカーが進化する中で、EMQXは常に先を行き、IoTアプリケーションの複雑な要求に応えるための新しい機能と能力を導入しています。本章では、以下の強力な機能について詳しく説明します。

- [メッセージキュー](../message-queue/message-queue-concept.md) は、信頼性の高いメッセージバッファリング、プロデューサーとコンシューマーの分離、バーストトラフィックや一時的にオフラインのクライアントを含むシナリオでのレジリエンス向上を可能にする組み込みのメッセージキューイング機構を紹介します。
- [MQTTストリーム](../mqtt-stream/mqtt-stream-concept.md) は、MQTTデータのストリーム処理機能を提供し、MQTTメッセージを順序付けられたストリームとして永続化、リプレイ、処理することで、リアルタイム分析やイベント駆動型アプリケーションを実現します。
- [MQTT over QUIC](./introduction.md) は、EMQXにおける画期的な機能を紹介し、その有効化方法を説明します。
- [Cluster Linking](../cluster-linking/introduction.md) は、複数の独立したクラスターを接続し、地理的に分散したクラスター間でのクライアント通信を可能にする機能を紹介します。
- [MQTT-based File Transfer](../file-transfer/introduction.md) は、MQTTプロトコルを用いて大容量ファイルをEMQXに転送する方法について解説します。
- [Multi-Protocol Gateway](../gateway/gateway.md) は、Stomp、MQTT-SN、CoAP、LwM2Mなど、一般的に使用される複数のゲートウェイの設計と利用方法を説明します。
- [MQTT Client Attributes](../client-attributes/client-attributes.md) は、MQTTクライアントに追加属性を定義・設定できる機能を提供し、アクセス制御、データ統合、MQTT拡張機能を強化するとともに、柔軟なテンプレート機能で個別のクライアント設定や認証プロセスの簡素化をサポートします。

これらの機能により、EMQXの能力が拡張され、追加のプロトコル活用やMQTTベースのアプリケーションの接続性および相互運用性の向上が可能になります。
