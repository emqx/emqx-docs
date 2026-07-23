# Developer Guide

Developer Guideは、開発者がEMQXを使い始め、EMQX上でアプリケーションを構築するための情報を提供することを目的としています。

本章では、MQTTのコアコンセプト、MQTT固有の機能、およびEMQXの拡張機能について解説します。また、EMQXダッシュボードでこれらの機能を設定し、クライアントツールを使ってテストする方法も説明します。以下の機能について取り扱います。

- [MQTTコアコンセプト](../../get-started/messaging/mqtt-concepts.md)
- [MQTTクライアントでのテスト](../../get-started/messaging/publish-and-subscribe.md)
- [共有サブスクライブ](../../get-started/messaging/mqtt-shared-subscription.md)
- [保持メッセージ](../../get-started/messaging/mqtt-retained-message.md)
- [遺言メッセージ](../../get-started/messaging/mqtt-will-message.md)
- [ワイルドカードサブスクライブ](../../get-started/messaging/mqtt-wildcard-subscription.md)
- [排他サブスクライブ](../../get-started/messaging/mqtt-exclusive-subscription.md)
- [遅延パブリッシュ](../../get-started/messaging/mqtt-delayed-publish.md)
- [自動サブスクライブ](../../get-started/messaging/mqtt-auto-subscription.md)
- [トピック書き換え](../../get-started/messaging/mqtt-topic-rewrite.md)

MQTTメッセージング機能に加えて、Developer GuideではEMQXとのさまざまなインタラクション方法も紹介しています。

- [curlを使ったEMQX操作](./curl.md)

また、EMQXの[MQTT耐久セッション](../../guides/durability/durability_introduction.md)機能についても紹介し、すぐに試せる手順を提供しています。

MQTTプロトコルをサポートしているため、EMQXはほとんどのMQTTクライアントライブラリやSDKと互換性があります。本ガイドには、開発者がMQTTプロジェクトを迅速に開始できるように、[ステップバイステップの手順とコードサンプル](./introduction.md)を含んでいます。MQTTクライアントSDKの完全な一覧と比較については、[MQTTクライアントSDK](https://www.emqx.com/en/mqtt-client-sdk)をご覧ください。

::: tip

すべてのSDKがドキュメントに表示されているわけではありません。

:::

さらに、EMQXは開発を支援するためのAPIドキュメントも提供しています。[REST API](../../guides/api.md)では、EMQXが公開するHTTP管理APIの迅速な利用開始方法を案内しています。
