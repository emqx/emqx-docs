# Developer Guide

Developer Guideは、開発者がEMQXを使い始め、EMQX上でアプリケーションを構築するための情報を提供することを目的としています。

本章では、MQTTのコアコンセプト、MQTT固有の機能、およびEMQXの拡張機能について解説します。また、これらの機能をEMQXダッシュボードで設定し、クライアントツールを使ってテストする方法も説明します。ガイドで扱う機能は以下の通りです。

- [MQTTのコアコンセプト](../../get-started/messaging/mqtt-concepts.md)
- [MQTTクライアントでのテスト](../../get-started/messaging/publish-and-subscribe.md)
- [共有サブスクライブ](../../get-started/messaging/mqtt-shared-subscription.md)
- [保持メッセージ](../../get-started/messaging/mqtt-retained-message.md)
- [遺言メッセージ](../../get-started/messaging/mqtt-will-message.md)
- [ワイルドカードサブスクライブ](../../get-started/messaging/mqtt-wildcard-subscription.md)
- [排他サブスクライブ](../../get-started/messaging/mqtt-exclusive-subscription.md)
- [遅延パブリッシュ](../../get-started/messaging/mqtt-delayed-publish.md)
- [自動サブスクライブ](../../get-started/messaging/mqtt-auto-subscription.md)
- [トピック書き換え](../../get-started/messaging/mqtt-topic-rewrite.md)

MQTTメッセージング機能に加え、Developer GuideではEMQXとやり取りするさまざまな方法も紹介しています。

- [curlを使ったEMQXの操作](./curl.md)

また、EMQXの[MQTT Durable Session](../../guides/durability/durability_introduction.md)機能についても紹介し、すぐに試せる手順を提供しています。

EMQXはMQTTプロトコルをサポートしているため、ほとんどのMQTTクライアントライブラリやSDKと互換性があります。本ガイドには、開発者がMQTTプロジェクトを迅速に開始できるよう、[ステップバイステップの手順とコードサンプル](./introduction.md)を含んでいます。MQTTクライアントSDKの完全な一覧と比較については、[MQTTクライアントSDK](https://www.emqx.com/en/mqtt-client-sdk)をご覧ください。

::: tip

すべてのSDKがドキュメントに表示されているわけではありません。

:::

さらに、EMQXは開発を支援するAPIドキュメントも提供しています。[REST API](../../guides/api.md)では、EMQXが公開するHTTP管理APIの迅速な利用開始方法を案内しています。
