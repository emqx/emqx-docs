# 開発者ガイド

開発者ガイドは、開発者がEMQXを使い始め、EMQX上でアプリケーションを構築するための情報を提供することを目的としています。

本章では、MQTTの基本概念、MQTT特有の機能、およびEMQXの拡張機能について解説します。また、これらの機能をEMQXダッシュボードで設定し、クライアントツールを使ってテストする方法も説明します。ガイドで扱う機能は以下の通りです。

- [MQTTの基本概念](../messaging/mqtt-concepts.md)
- [MQTTクライアントでのテスト](../messaging/publish-and-subscribe.md)
- [共有サブスクリプション](../messaging/mqtt-shared-subscription.md)
- [保持メッセージ](../messaging/mqtt-retained-message.md)
- [遺言メッセージ](../messaging/mqtt-will-message.md)
- [ワイルドカードサブスクリプション](../messaging/mqtt-wildcard-subscription.md)
- [排他サブスクリプション](../messaging/mqtt-exclusive-subscription.md)
- [遅延パブリッシュ](../messaging/mqtt-delayed-publish.md)
- [自動サブスクライブ](../messaging/mqtt-auto-subscription.md)
- [トピック書き換え](../messaging/mqtt-topic-rewrite.md)

さらに、本ガイドではEMQXの[MQTT Durable Session](../durability/durability_introduction.md)機能を紹介し、この機能を素早く体験するための手順も提供します。

EMQXはMQTTプロトコルをサポートしているため、ほとんどのMQTTクライアントライブラリやSDKと互換性があります。開発者ガイドでは、開発者が迅速にMQTTプロジェクトを開始できるように、[ステップバイステップの手順とコードサンプル](./introduction.md)を提供しています。MQTTクライアントSDKの完全なリストと比較については、[MQTTクライアントSDK](https://www.emqx.com/en/mqtt-client-sdk)をご覧ください。

::: tip

すべてのSDKがドキュメントに表示されているわけではありません。

:::

また、EMQXは開発を支援するためのAPIドキュメントも提供しています。[REST API](../admin/api.md)では、EMQXが公開するHTTP管理APIの迅速な利用方法を案内しています。
