# Developer Guide

Developer Guideは、開発者がEMQXを使い始め、EMQX上でアプリケーションを構築するための情報を提供することを目的としています。

<<<<<<< HEAD
本章では、MQTTのコアコンセプト、MQTT固有の機能、およびEMQXの拡張機能について説明します。また、これらの機能をEMQXダッシュボードで設定し、クライアントツールを使ってテストする方法も解説します。ガイドで扱う機能は以下の通りです。
=======
本章では、MQTTのコアコンセプト、MQTT固有の機能、およびEMQXの拡張機能について解説します。また、EMQXダッシュボードでこれらの機能を設定し、クライアントツールを使ってテストする方法も説明します。以下の機能について取り扱います。
>>>>>>> origin/release-5.9

- [MQTTコアコンセプト](../messaging/mqtt-concepts.md)
- [MQTTクライアントでのテスト](../messaging/publish-and-subscribe.md)
- [共有サブスクライブ](../messaging/mqtt-shared-subscription.md)
- [保持メッセージ](../messaging/mqtt-retained-message.md)
- [遺言メッセージ](../messaging/mqtt-will-message.md)
- [ワイルドカードサブスクライブ](../messaging/mqtt-wildcard-subscription.md)
- [排他サブスクライブ](../messaging/mqtt-exclusive-subscription.md)
- [遅延パブリッシュ](../messaging/mqtt-delayed-publish.md)
- [自動サブスクライブ](../messaging/mqtt-auto-subscription.md)
- [トピック書き換え](../messaging/mqtt-topic-rewrite.md)

<<<<<<< HEAD
MQTTメッセージング機能に加えて、Developer GuideではEMQXとやり取りするさまざまな方法についても説明しています。

- [curlを使ったEMQXの操作](./curl.md)

また、EMQXの[MQTT Durable Session](../durability/durability_introduction.md)機能を紹介し、すぐに試せる手順も提供しています。
=======
MQTTメッセージング機能に加えて、Developer GuideではEMQXとのさまざまなインタラクション方法も紹介しています。

- [curlを使ったEMQX操作](./curl.md)

また、EMQXの[MQTT耐久セッション](../durability/durability_introduction.md)機能についても紹介し、すぐに試せる手順を提供しています。
>>>>>>> origin/release-5.9

MQTTプロトコルをサポートしているため、EMQXはほとんどのMQTTクライアントライブラリやSDKと互換性があります。本ガイドには、開発者がMQTTプロジェクトを迅速に開始できるように、[ステップバイステップの手順とコードサンプル](./introduction.md)を含んでいます。MQTTクライアントSDKの完全な一覧と比較については、[MQTTクライアントSDK](https://www.emqx.com/en/mqtt-client-sdk)をご覧ください。

::: tip

すべてのSDKがドキュメントに表示されているわけではありません。

:::

<<<<<<< HEAD
EMQXは開発を支援するためにAPIドキュメントも提供しています。[REST API](../admin/api.md)では、EMQXが公開するHTTP管理APIの迅速な利用開始方法を案内しています。
=======
さらに、EMQXは開発を支援するためのAPIドキュメントも提供しています。[REST API](../admin/api.md)では、EMQXが公開するHTTP管理APIの迅速な利用開始方法を案内しています。
>>>>>>> origin/release-5.9
