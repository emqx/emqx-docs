<<<<<<< HEAD
# EMQX 概要
EMQX は「無制限の接続、シームレスな統合、どこでもデプロイ」を実現する大規模分散型 MQTT メッセージングプラットフォームです。高性能でスケーラブルな MQTT メッセージサーバーとして、EMQX Enterprise は IoT アプリケーション向けに信頼性の高いリアルタイムメッセージ伝送およびデバイス接続ソリューションを提供します。EMQX は50か国以上の2万社を超える法人ユーザーを有し、世界中で1億台以上の IoT デバイスを接続し、企業のデジタル化、リアルタイム化、インテリジェント化の変革を支えています。

商用のセルフホスト型 MQTT メッセージングプラットフォームである [EMQX Enterprise](https://www.emqx.com/en/products/emqx) は、クラスターあたり最大1億の同時 MQTT 接続をサポートします。単一サーバーで毎秒数百万の MQTT メッセージを処理可能でありながら、ミリ秒単位のレイテンシを維持します。堅牢な組み込みルールエンジンとデータ統合機能により、EMQX Enterprise は大量の IoT データに対してリアルタイムのデータ処理、変換、ルーティングを実行できます。さまざまなバックエンドデータベースや分析ツールとシームレスに統合し、企業が競争力のある IoT プラットフォームやアプリケーションを迅速に構築できるよう支援します。
=======
# EMQX Overview
EMQX is a large-scale distributed MQTT messaging platform that offers "unlimited connections, seamless integration, and anywhere deployment." As a high-performance, scalable MQTT message server, EMQX Enterprise provides reliable real-time message transmission and device connectivity solutions for IoT applications. EMQX has accumulated more than 20,000 corporate users from more than 50 countries, connecting more than 100 million IoT devices worldwide, serving enterprises' digital, real-time, and intelligent transformation.

As a commercial self-hosted MQTT messaging platform, [EMQX Enterprise](https://www.emqx.com/en/products/emqx) supports up to 100 million concurrent MQTT connections per cluster. A single server can handle and process millions of MQTT messages per second, all while maintaining millisecond-level latency. With its robust built-in rule engine and data integration capabilities, EMQX Enterprise can perform real-time data processing, transformation, and routing for massive IoT data. It seamlessly integrates IoT data with various backend databases and analytics tools, enabling enterprises to rapidly build IoT platforms and applications with leading competitiveness.
>>>>>>> origin/release-6.1

<img src="./assets/emqx_platform.png" alt="emqx_platform" style="zoom:70%;" />

## Key Benefits

<<<<<<< HEAD
- [**大規模スケール**](https://www.emqx.com/en/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)：単一ノードで150万の MQTT デバイス接続を安定的にサポートし、クラスターは水平スケールで最大1億の同時 MQTT 接続に対応。
- [**業務クリティカルな信頼性**](./deploy/cluster/mria-introduction.md)：組み込みの RocksDB パーシステンスによりデータ損失を防止。
- [**データセキュリティ**](./access-control/security-guide.md)：エンドツーエンドの暗号化と細粒度のアクセス制御でデータを保護。
- [**複数プロトコル対応**](https://www.emqx.com/en/blog/iot-protocols-mqtt-coap-lwm2m)：MQTT、QUIC、CoAP、Stomp、LwM2M などをサポート。
- [**完全な MQTT 5.0 対応**](https://www.emqx.com/en/blog/introduction-to-mqtt-5)：EMQX は MQTT 5.0 と 3.x 両方の標準に完全準拠し、スケーラビリティ、セキュリティ、信頼性を向上。
- [**高パフォーマンス**](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-supports-2m-message-throughput)：ノードあたり毎秒数百万の MQTT メッセージを効率的に取り込み・処理。
- [**低レイテンシ**](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-message-latency-response-time)：ソフトリアルタイムランタイムにより、ミリ秒未満のメッセージ配信レイテンシを保証。
- [**完全な可観測性**](./dashboard/introduction.md)：リアルタイム MQTT トレーシングによる監視、アラート、高度なエンドツーエンド分析。
- [**クラウドネイティブ＆K8s 対応**](./deploy/kubernetes/kubernetes.md)：**Kubernetes Operator** を使ってオンプレミスやパブリッククラウドに容易にデプロイ可能。
=======
- [**Massive Scale**](https://www.emqx.com/en/blog/how-emqx-5-0-achieves-100-million-mqtt-connections): A single node stably supports 1.5M MQTT device connections, and the cluster can scale horizontally to handle up to 100M concurrent MQTT connections.
- [**Business-Critical Reliability**](./deploy/cluster/mria-introduction.md): Ensure no data loss with built-in RocksDB data persistence.
- [**Data Security**](./access-control/security-guide.md): End-to-end data encryption and fine-grained access control to protect your data.
- [**Multiple protocols support**](https://www.emqx.com/en/blog/iot-protocols-mqtt-coap-lwm2m): MQTT, QUIC, CoAP, Stomp, LwM2M, and more
- [**Fully MQTT 5.0**](https://www.emqx.com/en/blog/introduction-to-mqtt-5): EMQX is **fully** compliant with both **MQTT 5.0 and 3.x** standards, providing better scalability, security, and reliability.
- [**High Performance**](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-supports-2m-message-throughput): Ingest and process millions of MQTT messages efficiently per second per node.
- [**Low Latency**](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-message-latency-response-time): Guarantee sub-millisecond latency in message delivery with the soft real-time runtime.
- [**Complete Observability**](./dashboard/introduction.md): Monitoring, alerting, and advanced end-to-end analysis with real-time MQTT tracing.
- [**Cloud-Native & K8s**](./deploy/kubernetes/kubernetes.md): Can be easily deployed on-premises or in public clouds using **Kubernetes Operator**.
>>>>>>> origin/release-6.1

## Main Components

<<<<<<< HEAD
EMQX Enterprise は複数のコンポーネントから構成され、強力でスケーラブルな MQTT メッセージングプラットフォームを構築します。以下は EMQX Enterprise の主要コンポーネントです。
=======
EMQX Enterprise consists of multiple components that together build a powerful and scalable MQTT messaging platform. Here are the core components of EMQX Enterprise:
>>>>>>> origin/release-6.1

### Device Connectivity

<<<<<<< HEAD
EMQX Enterprise は MQTT 5.0 および 3.x 仕様に100％準拠し、卓越したスケーラビリティにより膨大な数の MQTT デバイスクライアントの[接続](https://www.emqx.com/en/blog/reaching-100m-mqtt-connections)を容易に処理します。同時に、HTTP、QUIC、LwM2M/CoAP などの他のオープン標準プロトコルもサポートし、多様な IoT デバイスやシナリオの接続を可能にします。さらに、ファイル転送や遅延パブリッシュなどの機能も拡張し、利用シーンを豊かにしています。

#### MQTT over QUIC

EMQX Enterprise は先駆的に [MQTT over QUIC](./mqtt-over-quic/introduction.md) プロトコルを導入し、IoT クライアントが QUIC 経由で EMQX に接続して通信できるようにします。QUIC を利用するデバイスは接続およびメッセージスループット性能を向上させ、メッセージレイテンシを低減します。これは、ネットワーク環境が不安定でリンクの切り替えが頻繁に発生する自動車向けインターネット（IoV）などのシナリオで特に有効です。MQTT over QUIC はこうしたリアルタイムかつ効率的なメッセージ伝送の要件を満たします。
=======
EMQX Enterprise is 100% compatible with MQTT 5.0 and 3.x specifications, and its exceptional scalability allows it to easily handle a massive number of MQTT device client [connections](https://www.emqx.com/en/blog/reaching-100m-mqtt-connections-with-emqx-5-0). At the same time, it provides support for other open-standard protocols, including HTTP, QUIC, and LwM2M/CoAP, enabling connectivity for a wide range of IoT devices and scenarios. EMQX Enterprise also extends its capabilities to include features such as file transfer and delayed publishing, enriching its use cases.

#### MQTT over QUIC

EMQX Enterprise pioneeringly introduces the [MQTT over QUIC](./mqtt-over-quic/introduction.md) protocol, allowing IoT clients to establish connections with EMQX via QUIC for communication. Devices using QUIC can improve connection and message throughput performance while reducing message latency. This is particularly beneficial in scenarios such as the Internet of Vehicles (IoV), which commonly face weak network conditions, frequent link changes, and unstable network environments. MQTT over QUIC meets the requirements for real-time and efficient message transmission in such scenarios.
>>>>>>> origin/release-6.1

#### Multi-Protocol Gateways

<<<<<<< HEAD
[マルチプロトコルゲートウェイ](./gateway/gateway.md) により、EMQX Enterprise は MQTT 以外の異なる通信プロトコルを用いたデバイス接続をサポートします。これらのゲートウェイはデバイスの接続要求を受け付け、使用されている通信プロトコルを識別し、各プロトコル仕様に基づいてデバイスから送信されるメッセージやコマンド、データを解析します。ゲートウェイはこれらを MQTT メッセージ形式に変換し、後続のメッセージ処理に渡します。
=======
[Multi-protocol gateways](./gateway/gateway.md) enable EMQX Enterprise to support device connections using different communication protocols other than MQTT. These gateways listen to device connection requests, identify the communication protocols used by devices, and then parse the messages, commands, and data sent by devices according to the respective protocol specifications. The gateways convert this data into MQTT message formats for further message processing.
>>>>>>> origin/release-6.1

### Message Routing

<<<<<<< HEAD
EMQX Enterprise は [パブリッシュ／サブスクライブ](./messaging/introduction.md) パターンをサポートし、高信頼のメッセージ伝送メカニズムを提供します。これにより、メッセージが意図したデバイスやアプリケーションに確実に届けられます。QoS 機構とセッション保持機能により、不安定なネットワーク環境下でも迅速かつ確実にデータを配信し、業務の継続性と安定性を確保します。

### 分散クラスター

EMQX Enterprise はネイティブな[クラスタリング](./deploy/cluster/introduction.md)機能を提供し、シームレスかつ弾力的なスケーリングを可能にし、単一障害点を回避します。極限まで最適化された単一ノードは毎秒数百万の MQTT メッセージを[低レイテンシ](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-message-latency-response-time)で処理・配信可能です。クラスターの水平スケールにより最大1億の同時 MQTT 接続をサポートし、IoV、産業オートメーション、スマートホームなどの大規模 IoT 展開に不可欠な基盤となります。
=======
EMQX Enterprise provides a highly reliable message transmission mechanism through its support for the [publish/subscribe](./messaging/introduction.md) pattern. This ensures that messages are reliably delivered to the intended devices or applications. With QoS mechanisms and session retention capability, data can be quickly and reliably delivered even in unstable network environments, ensuring business continuity and stability.

### Distributed Clustering

EMQX Enterprise offers native [clustering](./deploy/cluster/introduction.md) capabilities, enabling seamless and elastic scaling, while avoiding single points of failure. With extreme optimization, a single node can process and distribute millions of MQTT messages per second with [low latency](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-message-latency-response-time). Through cluster horizontal scaling, it supports up to 100 million concurrent MQTT connections, making it crucial for large-scale IoT deployments in areas such as the IoV, industrial automation, and smart homes.
>>>>>>> origin/release-6.1

### Access Control and Data Security

<<<<<<< HEAD
[ TLS/SSL 暗号化](./network/overview.md)および[認証](./access-control/authn/authn.md)/[認可](./access-control/authz/authz.md)メカニズムにより、EMQX Enterprise はデバイスデータ伝送の機密性と完全性を確保します。

EMQX Enterprise はユーザー名／パスワード、JWT、拡張認証、PSK、X.509 証明書など複数のクライアント認証方式を備えています。ACL に基づくパブリッシュ／サブスクライブの認可機構を提供します。認証・認可データは LDAP、HTTP サービス、SQL、NoSQL データベースなどの外部企業セキュリティシステムと統合・管理でき、多様で柔軟なクライアントセキュリティ保護ソリューションを実現します。

さらに、EMQX Enterprise は[監査ログ](./dashboard/audit-log.md)、ロール・権限管理、[シングルサインオン](./dashboard/sso.md)を提供し、SOC 2 準拠や GDPR データプライバシー保護に対応します。包括的なセキュリティ機能により、企業は業界のセキュリティ基準に準拠した信頼性の高い IoT アプリケーションを構築できます。
=======
Through [TLS/SSL encryption](./network/overview.md) and [authentication](./access-control/authn/authn.md)/[authorization](./access-control/authz/authz.md) mechanism EMQX Enterprise ensures the confidentiality and integrity of device data transmission.

EMQX Enterprise includes multiple client authentication mechanisms, including username/password, JWT, enhanced authentication, PSK, and X.509 certificates. It provides publish/subscribe authorization mechanisms based on ACLs. Authentication and authorization data can be integrated and managed through external enterprise security systems, such as LDAP, HTTP services, SQL, and NoSQL databases, allowing for flexible and diverse client security protection solutions.

Additionally, the EMQX Enterprise offers [audit logs](./dashboard/audit-log.md), role and permission management, and [single sign-on](./dashboard/sso.md) to meet SOC 2 compliance requirements and GDPR data privacy protection. Its comprehensive security features help enterprises build trusted IoT applications that comply with industry security standards.
>>>>>>> origin/release-6.1

### Rule Engine and Data Integration

<<<<<<< HEAD
EMQX Enterprise は強力な[ルールエンジン](./data-integration/rules.md)を備え、EMQX 内でルールを設定して受信データを要件に応じて処理・ルーティングできます。さらに Sink 機能を利用して、EMQX Enterprise とクラウドサービスやデータベースを連携し、IoT データをクラウドに転送して保存・分析が可能です。
=======
EMQX Enterprise includes a powerful [rule engine](./data-integration/rules.md) that allows you to configure rules within EMQX to process and route incoming data based on your requirements. You can also use EMQX's Sink feature to integrate EMQX Enterprise with cloud services or databases for transferring IoT data to the cloud for storage and analysis.
>>>>>>> origin/release-6.1

#### Real-Time Data Processing

<<<<<<< HEAD
組み込みの SQL ベースルールエンジン、スキーマレジストリ、メッセージコーデック、[Flowデザイナー](./flow-designer/introduction.md)を活用し、デバイスイベントやメッセージ処理フローを簡単に作成・編集できます。これにより、IoT データのリアルタイム抽出、検証、フィルタリング、変換が可能です。
=======
With a built-in SQL-based rule engine, Schema Registry, message codecs, and [Flow Designer](./flow-designer/introduction.md), you can easily create and edit device events and message processing flows. This enables real-time extraction, validation, filtering, and transformation of IoT data.
>>>>>>> origin/release-6.1

#### Enterprise Data Integration

<<<<<<< HEAD
標準搭載の Webhook や Sink/Source により、Kafka、AWS RDS、MongoDB、Oracle、SAP、時系列データベースなど40以上のクラウドサービスや企業システムとシームレスに[統合](./data-integration/data-bridges.md)できます。これにより企業は IoT デバイスからのデータを効果的に管理・分析・活用し、多様なアプリケーションやビジネスニーズを支えます。
=======
Through out-of-the-box Webhooks and Sink/Source, you can seamlessly [integrate](./data-integration/data-bridges.md) IoT data with over 40 cloud services and enterprise systems, including Kafka, AWS RDS, MongoDB, Oracle, SAP, and time-series databases. This empowers enterprises to effectively manage, analyze, and utilize data from IoT devices, supporting various applications and business needs.
>>>>>>> origin/release-6.1

### Management and Monitoring Dashboard

<<<<<<< HEAD
EMQX Enterprise は[ダッシュボード](./dashboard/introduction.md)というグラフィカルな管理システムを提供し、主要メトリクスや運用状況をリアルタイムで監視できます。クライアント接続や機能設定の管理を簡素化し、クライアントやクラスターの異常診断・デバッグを可能にします。これにより MQTT デバイスのオンライン状態をエンドツーエンドでトラブルシューティングでき、問題解決時間を大幅に短縮します。さらに、Prometheus、Datadog、OpenTelemetry 対応サービスなど外部サービスへの可観測性メトリクス連携もサポートし、運用監視機能を強化します。

## デプロイモードとエディション比較

EMQ は EMQX のデプロイに3つの選択肢を提供しています。2つのマネージドサービス（EMQX Serverless と EMQX Dedicated）と1つのセルフホスト型（EMQX Enterprise）です。要件に最適なデプロイオプションを選択しやすいよう、以下の表に各デプロイタイプの機能対応比較を示します。詳細な対応機能比較は[機能比較](./getting-started/feature-comparison.md)を参照してください。
=======
EMQX Enterprise provides a graphical management system called the [Dashboard](./dashboard/introduction.md), allowing you to monitor key metrics and operational statuses in real time. It simplifies the management of client connections and feature configurations. The Dashboard also enables diagnostics and debugging of client and cluster anomalies, facilitating end-to-end troubleshooting of MQTT devices online, significantly reducing troubleshooting time. In addition, it supports the integration of observability metrics into external services such as Prometheus, Datadog, and services supporting OpenTelemetry, enhancing operational monitoring capabilities.

## Deployment Modes and Edition Comparison

EMQ provides three deployment options for EMQX: two managed services (EMQX Serverless and EMQX Dedicated) and one self-hosted option (EMQX Enterprise). To help you choose the best deployment option for your requirements, the following table lists a comparison of feature support across different deployment types. For a comparison of supported features in detail, refer to [Feature Comparison](./getting-started/feature-comparison.md). 
>>>>>>> origin/release-6.1

<table>
<thead>
  <tr>
    <th colspan="1">Self-Hosted</th>
    <th colspan="2">MQTT as a Service</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
  <tr>
<<<<<<< HEAD
    <td><a href="https://www.emqx.com/en/apply-licenses/emqx">無料トライアルライセンス取得</a></td>
    <td><a href="https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew">無料で始める</a></td>
    <td><a href="https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew">14日間無料トライアル開始</a></td>
  </tr>
  <tr>
    <td>✔️ Business Source License (BSL) 1.1<br>✔️ MQTT over QUIC<br>✔️ RocksDB によるセッションパーシステンス<br>✔️ Kafka/Confluent、Timescale、InfluxDB、PostgreSQL、Redis など40以上の企業システムとのデータ統合<br>✔️ 監査ログとシングルサインオン（SSO）<br>✔️ ロールベースアクセス制御（RBAC）<br>✔️ ファイル転送<br>✔️ メッセージコーデック<br>✔️ OCPP、JT/808、GBT32960 に対応したマルチプロトコルゲートウェイ<br>✔️ 24時間365日のグローバル技術サポート<br> </td>
    <td>✔️ 従量課金制<br>✔️ 毎月無料クォータあり<br>✔️ 最大1000接続<br>✔️ 数秒でデプロイ開始<br>✔️ オートスケーリング<br>✔️ 8時〜17時のグローバル技術サポート</td>
    <td>✔️ 14日間無料トライアル<br>✔️ 時間単位課金<br>✔️ 世界各地のマルチクラウドリージョン<br>✔️ 柔軟なスペック選択<br>✔️ VPC ピアリング、NAT ゲートウェイ、ロードバランサーなど<br>✔️ 40以上のクラウドサービスと即時統合<br>✔️ 24時間365日のグローバル技術サポート<br> </td>
=======
    <td><a href="https://www.emqx.com/en/apply-licenses/emqx">Get a Free Trial License</a></td>
    <td><a href="https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew">Get Started Free</a></td>
    <td><a href="https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew">Start a Free 14-Day Trial</a></td>
  </tr>
  <tr>
    <td>✔️ Business Source License (BSL) 1.1<br>✔️ MQTT over QUIC<br>✔️ Session persistence in RocksDB<br>✔️ Data integration with 40+ enterprise systems, including Kafka/Confluent, Timescale, InfluxDB, PostgreSQL, Redis etc.<br>✔️ Audit log and single sign-on (SSO)<br>✔️ Role-Based Access Control (RBAC)<br>✔️ File transfer<br>✔️ Message codec<br>✔️ Multi-protocol gateways, with extra support on OCPP, JT/808 and GBT32960<br>✔️ 24/7 global technical support<br> </td>
    <td>✔️ Pay as you go<br>✔️ Free quota every month<br>✔️ 1000 maximum connections<br>✔️ Start deployment in seconds<br>✔️ Auto scaling<br>✔️ 8/5 global technical support</td>
    <td>✔️ 14-days free trial<br>✔️ Hourly billing<br>✔️ Multi-cloud regions worldwide<br>✔️ Flexible specifications<br>✔️ VPC peering, NAT gateway, load balance and more<br>✔️ Out-of-box integration with over 40+ cloud services<br>✔️ 24/7 global technical support<br> </td>
>>>>>>> origin/release-6.1
  </tr>
</tbody>
</table>


<<<<<<< HEAD
EMQX Enterprise は包括的な IoT メッセージングプラットフォームとして、IoT デバイス接続およびデータ伝送のさまざまな段階で重要な役割を果たし、多様なビジネスニーズに強力な機能と柔軟性を提供します。

パブリッシュ・サブスクライブのメッセージ配信モデルに基づき、数百万のトピックと多様なモードで柔軟なメッセージ通信を実現し、さまざまなシナリオにおけるリアルタイムメッセージ配信ニーズに応えます。組み込みのルールエンジンと Sink/Source により、メッセージを各種クラウドサービスに送信し、デバイスデータを企業システムとシームレスに統合できます。データ処理、保存、分析、業務コマンド発行などのユースケースを容易にサポートします。以下は代表的なユースケースです。
=======

## Use Cases
>>>>>>> origin/release-6.1

EMQX Enterprise is a comprehensive IoT messaging platform that plays a crucial role in different stages of IoT device connectivity and data transmission, providing powerful functionality and flexibility for various business needs.

<<<<<<< HEAD
EMQX Enterprise は多様なデバイスとアプリケーションエンドポイント間の接続をサポートし、双方向通信を実現します。例えばスマートホームシナリオでは、モバイルアプリが複数のデバイスからセンサーデータを取得し、必要に応じて制御コマンドを送信できます。このモードはデバイス間およびデバイスとアプリケーション間の柔軟な1対1または1対多通信を可能にします。

<img src="./assets/use_case_1.png" alt="use_case_1" style="zoom:50%;" />

ミッションクリティカルなアプリケーションにおける双方向通信の主なメリットは以下の通りです。

- **トピックベースのパブリッシュ／サブスクライブメッセージング**：EMQX のトピックベースモデルにより効率的かつ柔軟なメッセージルーティングを実現。
- **超低レイテンシ配信**：1ミリ秒以下の高速データ転送でリアルタイム応答性を確保。
- **包括的な QoS 保証**：EMQX はエンドツーエンドの多段階 QoS 保証を提供し、信頼性の高い柔軟なメッセージ配信を実現。

以下はより具体的な利用シナリオです。
=======
Based on the publish-subscribe message delivery model, it can achieve flexible message communication with millions of topics and in different modes, meeting the real-time message delivery needs under various scenarios. Through its built-in rule engine and Sink/Source, EMQX Enterprise allows you to send messages to various cloud services, enabling seamless device data integration with enterprise systems. It can easily support use cases such as data processing, storage, analysis, and business command issuance. Here are some typical use cases:

### Bidirectional Communication

EMQX Enterprise supports connections between various devices and application endpoints, providing bidirectional communication between them. For example, in a smart home scenario, a mobile app can retrieve sensor data from various devices and send control commands to the devices when needed. This mode enables flexible one-to-one or one-to-many communication between devices and between devices and applications.

<img src="./assets/use_case_1.png" alt="use_case_1" style="zoom:50%;" />

Bidirectional communication in mission-critical applications brings you key benefits as follows:
>>>>>>> origin/release-6.1

- **Topic-Based Pub/Sub Messaging**: EMQX's topic-based publish/subscribe model streamlines the data flow to ensure efficient and flexible message routing.
- **Ultra-Low Latency Delivery**: Achieve rapid data transfer with latencies as low as 1 millisecond, ensuring real-time responsiveness.
- **Comprehensive Quality of Service (QoS) Guarantees**: EMQX offers end-to-end multi-level QoS guarantees, providing reliable and flexible message delivery.

<<<<<<< HEAD
EMQX を用いてピアツーピア通信を構築できます。非同期のパブリッシュ／サブスクライブモデルでは、パブリッシャーとサブスクライバーは動的に追加・削除可能で疎結合となり、アプリケーションやメッセージ通信に柔軟性をもたらします。

![use_case_1_ce](./assets/use_case_1_ce.png)
=======
Below are more specific using scenarios:

#### Peer-to-Peer Communication
>>>>>>> origin/release-6.1

You can build up peer-to-peer communications with EMQX. In the asynchronous Pub/Sub model, the message publisher and subscriber are decoupled from each other, as they can be dynamically added or removed as needed. This decoupling provides flexibility to your applications and message communication.

<<<<<<< HEAD
EMQX は金融市場の更新情報など、1対多メッセージングが重要なシナリオで優れた性能を発揮します。多数のクライアントに対してタイムリーにメッセージを配信します。

![use_case_2_ce](./assets/use_case_2_ce.png)
=======
![use_case_1_ce](./assets/use_case_1_ce.png)

#### Message Broadcasting to a Large Audience
>>>>>>> origin/release-6.1

EMQX excels in scenarios where one-to-many messaging is vital, such as financial market updates. It effectively broadcasts messages to a large number of clients, ensuring timely information dissemination.

<<<<<<< HEAD
EMQX の多対1メッセージパターンは、工場、近代的なビル、小売チェーン、電力網など大規模ネットワークでのデータ集約に最適です。ネットワーク内のエンドポイントからクラウドやオンプレミスの集中バックエンドサーバーへデータ転送・伝送を支援します。

![use_case_3_ce](./assets/use_case_3_ce.png)

#### リクエスト・レスポンス対応のトレーサブル通信

EMQX は MQTT 5.0 のリクエスト・レスポンス機能をサポートし、非同期通信アーキテクチャにおける通信認識性とトレーサビリティを向上させます。

![use_case_4_ce](./assets/use_case_4_ce.png)
=======
![use_case_2_ce](./assets/use_case_2_ce.png)

#### Data Consolidation from Massive Endpoints

The many-to-one message pattern in EMQX is ideal for consolidating data in large-scale networks, such as factory plats, modern buildings, retail chains, or electricity grids. EMQX can help you transfer and transmit the data from the endpoints in the network to your centralized backend servers on the cloud or on-premise.

![use_case_3_ce](./assets/use_case_3_ce.png)

#### Traceable Communication with Request-Response Awareness
>>>>>>> origin/release-6.1

EMQX supports the MQTT 5.0 feature Request-Response. With this feature, you can now increase communication awareness and traceability in your asynchronous communication architect.

<<<<<<< HEAD
組み込みの強力な SQL ベース[ルールエンジン](./data-integration/rules.md)により、EMQX は流れるデータをリアルタイムで抽出、フィルタリング、強化、変換できます。処理済みデータは外部 HTTP サーバーや MQTT サービスに容易に取り込めます。EMQX Enterprise では主流のデータベース、データストレージ、メッセージキューへの取り込みも可能です。

![use_case_6_ce](./assets/use_case_6_ce.png)
=======
![use_case_4_ce](./assets/use_case_4_ce.png)

### Flowing Data Transformation
>>>>>>> origin/release-6.1

With a built-in powerful SQL-based [rules engine](./data-integration/rules.md), EMQX can extract, filter, enrich, and transform the flowing data in real-time. Processed data can be easily ingested into external HTTP servers and MQTT services. If you are using EMQX Enterprise, you can also ingest data into mainstream databases, data storage, and message queues.

<<<<<<< HEAD
パーティション分割や制限されたネットワーク環境でも、EMQX はデータ統合を実現し、シームレスなメッセージング環境を提供します。

![use_case_5_ce](./assets/use_case_5_ce.png)

### テレメトリデータアップロード

EMQX Enterprise はデバイスデータをクラウドにアップロードし、クラウド上で特定トピックのデータ処理・保存をサポートします。例えば産業生産シナリオでは、工場内の各種産業機器データをリアルタイム処理し、製品品質のトレーサビリティや生産分析のためにデータベースに保存します。このモードはビジュアルに設定可能で、豊富なデータ処理機能を活用した迅速な開発を実現します。

<img src="./assets/use_case_2.png" alt="use_case_2" style="zoom:50%;" />
=======
![use_case_6_ce](./assets/use_case_6_ce.png)

### Data Integration Across Different Networks

In a partitioned, or limited network environment, EMQX can create the data integrations, and provide you with a seamless messaging environment.

![use_case_5_ce](./assets/use_case_5_ce.png)

### Telemetry Data Upload
>>>>>>> origin/release-6.1

EMQX Enterprise supports uploading device data to the cloud and processing and storing data from specified topics in the cloud. For example, in an industrial production scenario, EMQX can process various industrial equipment data from the factory floor in real-time and store it in a database for product quality traceability and production analysis. This mode can be configured visually and leverages rich data processing capabilities for rapid development.

<<<<<<< HEAD
EMQX Enterprise は MQTT プロトコルの[ファイル転送](./file-transfer/introduction.md)機能を提供し、デバイスが大容量ファイルをアップロードしてローカルまたは S3 ストレージに保存できます。例えば IoV シナリオでは、機械学習ログファイルやパッケージ化された CAN バスデータをクラウドストレージに送信し、インテリジェント運転アルゴリズムモデルの更新に活用します。このモードは構造化データとファイル型データを統一チャネルで扱い、アプリケーションの複雑さと保守コストを削減します。

<img src="./assets/use_case_3.png" alt="use_case_3" style="zoom:50%;" />
=======
<img src="./assets/use_case_2.png" alt="use_case_2" style="zoom:50%;" />

### Large File Upload
>>>>>>> origin/release-6.1

EMQX Enterprise provides MQTT protocol [file transfer](./file-transfer/introduction.md) capability, allowing devices to upload large file data and store it locally or in S3 storage. For example, in an IoV scenario, machine learning log files and packaged CAN Bus data can be transmitted to cloud storage to drive updates to intelligent driving algorithm models. This mode combines structured data and file-type data through a unified data channel, reducing application complexity and maintenance costs.

<<<<<<< HEAD
EMQX Enterprise は MQTT メッセージ、REST API、Kafka などの Source を通じてメッセージ発行を可能にし、データプッシュやリモートデバイス制御を実現します。例えば金融取引シナリオでは、クラウドサービスがユーザーのウォッチリストに基づくリアルタイムデータをグループにプッシュします。このモードはトピックマッピング、発行用データ処理、データ到達統計を提供し、柔軟で信頼性の高いデータ発行を可能にします。

<img src="./assets/use_case_4.png" alt="use_case_4" style="zoom:50%;" />

## 業界別ソリューション

EMQX Enterprise は多様な業界向けに信頼性の高いリアルタイム接続を提供し、ミッションクリティカルなアプリケーションを支えます。コネクテッドカーからスマート製造まで、EMQX は大規模なイノベーションを推進します。

### 自動車・コネクテッドビークル

EMQX はソフトウェア定義車両（SDV）の未来を支え、世界トップ10自動車メーカーのうち5社の100以上の車種、3000万台以上の車両を接続しています。プラットフォームはミッションクリティカルな V2X やテレマティクスアプリケーションのリアルタイムデータ基盤を提供し、不安定なネットワーク環境に最適化された[MQTT over QUIC](./mqtt-over-quic/introduction.md)を備えています。

![architecture_vehicle_to_cloud](./assets/architecture-v2c.svg)

- **コネクテッドカー＆SDV**：グローバル車両群のリモート診断、双方向コマンド制御、OTA アップデートを実現。[**詳細はこちら →**](https://www.emqx.com/en/solutions/internet-of-vehicles)
- **車両隊列テレマティクス**：リアルタイム位置追跡、利用ベース保険（UBI）、予知保全を超低レイテンシデータストリームで提供。[**詳細はこちら →**](https://www.emqx.com/en/solutions/fleet-telematics)
- **EV 充電ネットワーク**：充電ステーション管理、スマート充電、V2G アプリケーション向けにスケーラブルな MQTT 接続を提供。
- **自動車製造**：工場のロボット、PLC、センサーを接続し、継続的な監視と品質保証を実現。[**詳細はこちら →**](https://www.emqx.com/en/solutions/industrial-iot)

SAIC フォルクスワーゲンは EMQX を活用し、160万台以上のコネクテッドビークル向け次世代 IoV プラットフォームを構築、リモート制御とリアルタイムデータ監視を支えています。[**事例はこちら →**](https://www.emqx.com/en/customers/saic-volkswagen)

### 輸送・物流

一秒を争う業界において、EMQX はリアルタイムの車両可視化、不安定ネットワークでの信頼性あるデータ伝送、ジオ分散型デプロイによるレイテンシ最小化を提供します。数十万台の車両やデバイスを単一の統合基盤に接続します。

![architecture-transportation-logistics](./assets/architecture-transportation-logistics.svg)

- **車両管理**：車両位置追跡、ドライバー行動監視、リアルタイムルート最適化で燃料コスト削減と配送時間短縮を実現。[**詳細はこちら →**](https://www.emqx.com/en/solutions/fleet-management)
- **スマート都市交通**：大量の交通データを処理し、リアルタイム分析とインテリジェント交通システムを支援。
- **V2X 通信**：安全性向上、交通効率化、自動運転アプリケーションのための車車間通信を実現。[**詳細はこちら →**](https://www.emqx.com/en/solutions/software-defined-vehicles)
- **コールドチェーン監視**：温湿度をリアルタイム監視し、コンプライアンス遵守と腐敗防止を支援。

深圳都市交通計画センター（SUTPC）は EMQX を利用し、170万台以上の車両データを処理、リアルタイム交通分析とインテリジェント交通システムを実現しています。[**事例はこちら →**](https://www.emqx.com/en/customers/sutpc)

### 製造業・IIoT

EMQX は工場の機械、システム、アプリケーションをクラウドにつなぎ、OT と IT を AI ネイティブなデータ基盤で橋渡しします。Modbus、OPC-UA、Siemens S7 など100以上の産業プロトコルをサポートし、Sparkplug B による[統一ネームスペース（UNS）](https://www.emqx.com/en/solutions/unified-namespace)アーキテクチャを実現し、真のプラグアンドプレイ相互運用性を提供します。

![architecture-manufacturing](./assets/architecture-manufacturing.svg)

- **予知保全**：リアルタイムセンサーデータと AI により機械故障を予測し、計画外ダウンタイムを防止し、設備寿命を延長。
- **OEE 最適化**：リアルタイムで総合設備効率を追跡し、工場の生産性向上を支援。最大25％の OEE 向上、40％のダウンタイム削減を報告。
- **品質・トレーサビリティ**：品質異常を即時検知し、生産パラメータをリアルタイム監視、製品の完全トレーサビリティを実現。
- **ライブパフォーマンス監視**：EMQX の[メトリクスと可観測性](./observability/overview.md)機能を活用し、Prometheus や Datadog と連携したライブダッシュボードで生産ライン全体を可視化。

大手半導体ファブは EMQX を活用し、1工場あたり350万以上のデータタグを100ms収集周期で処理し、精密製造のため100％のデータ完全性を実現しています。[**詳細はこちら →**](https://www.emqx.com/en/solutions/industrial-iot)
=======
<img src="./assets/use_case_3.png" alt="use_case_3" style="zoom:50%;" />

### Cloud-Based Control Command Issuance

EMQX Enterprise allows message issuance through MQTT messages, REST APIs, and Source with, for example, Kafka, enabling data push or remote device control. For example, cloud services can push real-time data based on user watchlists in groups in a financial trading scenario. This mode provides topic mapping, data processing for issuance, and data reach statistics, enabling flexible and reliable data issuance.

<img src="./assets/use_case_4.png" alt="use_case_4" style="zoom:50%;" />

## Industry Solutions

EMQX Enterprise provides versatile IoT solutions across industries, delivering reliable real-time connectivity for mission-critical applications. From connected vehicles to smart manufacturing, EMQX powers innovation at scale.

### Automotive & Connected Vehicles

EMQX powers the future of software-defined vehicles (SDVs), connecting 30+ million vehicles across 100+ car models for 5 of the 10 largest automobile companies worldwide. The platform provides the real-time data backbone for mission-critical V2X and telematics applications, with [MQTT over QUIC](./mqtt-over-quic/introduction.md) optimized for unstable network conditions.

![architecture_vehicle_to_cloud](./assets/architecture-v2c.svg)

- **Connected Cars & SDVs**: Enable remote diagnostics, bidirectional command and control, and over-the-air (OTA) updates across global fleets. [**Learn More →**](https://www.emqx.com/en/solutions/internet-of-vehicles)
- **Fleet Telematics**: Real-time geo-location tracking, usage-based insurance (UBI), and predictive maintenance with ultra-low latency data streams. [**Learn More →**](https://www.emqx.com/en/solutions/fleet-telematics)
- **EV Charging Networks**: Scalable MQTT connectivity for charging station management, smart charging, and vehicle-to-grid (V2G) applications.
- **Automotive Manufacturing**: Unify data from factory floors by connecting robots, PLCs, and sensors for continuous monitoring and quality assurance. [**Learn More →**](https://www.emqx.com/en/solutions/industrial-iot)

SAIC Volkswagen relies on EMQX to power their next-generation IoV platform for over 1.6 million connected vehicles, supporting remote control and real-time data monitoring. [**Read Case Study →**](https://www.emqx.com/en/customers/saic-volkswagen)

### Transportation & Logistics

In an industry where every second counts, EMQX provides real-time fleet visibility, reliable data transmission over unstable networks, and geo-distributed deployment to minimize latency. The platform connects hundreds of thousands of vehicles and devices to a single, unified backbone.

![architecture-transportation-logistics](./assets/architecture-transportation-logistics.svg)

- **Fleet Management**: Track vehicle location, monitor driver behavior, and optimize routes in real time to reduce fuel costs and improve delivery times. [**Learn More →**](https://www.emqx.com/en/solutions/fleet-management)
- **Smart Urban Transport**: Process massive amounts of traffic data for real-time analysis and intelligent transportation systems.
- **V2X Communication**: Enable Vehicle-to-Everything communication for enhanced safety, traffic efficiency, and autonomous driving applications. [**Learn More →**](https://www.emqx.com/en/solutions/software-defined-vehicles)
- **Cold Chain Monitoring**: Monitor temperature and humidity of sensitive cargo in real time to ensure compliance and prevent spoilage.

The Shenzhen Urban Transport Planning Center (SUTPC) uses EMQX to process data from over 1.7 million vehicles, enabling real-time traffic analysis and intelligent transportation systems. [**Read Case Study →**](https://www.emqx.com/en/customers/sutpc)

### Manufacturing & IIoT

EMQX connects all machines, systems, and applications from the factory floor to the cloud, bridging OT and IT with an AI-native data backbone. With support for 100+ industrial protocols, including Modbus, OPC-UA, and Siemens S7, EMQX enables a [Unified Namespace (UNS)](https://www.emqx.com/en/solutions/unified-namespace) architecture with Sparkplug B for true plug-and-play interoperability.

![architecture-manufacturing](./assets/architecture-manufacturing.svg)

- **Predictive Maintenance**: Use real-time sensor data and AI to predict machine failures, prevent unplanned downtime, and extend equipment life.
- **OEE Optimization**: Boost factory output by tracking Overall Equipment Effectiveness in real time. Manufacturers report up to 25% increase in OEE and 40% reduction in downtime.
- **Quality & Traceability**: Detect quality deviations the moment they occur, monitor production parameters in real-time, and enable full product traceability.
- **Live Performance Monitoring**: Visualize your entire production line with live dashboards using EMQX [metrics and observability](./observability/overview.md) features, with integration to Prometheus and Datadog.

Leading semiconductor fabs use EMQX to unify equipment data, handling 3.5M+ data tags per plant with 100ms collection rates and 100% data integrity for precision manufacturing. [**Learn More →**](https://www.emqx.com/en/solutions/industrial-iot)

### Energy & Utilities

EMQX powers the modern energy grid, connecting 10M+ endpoints with sub-100ms latency for critical grid control and protection applications. The platform bridges legacy OT protocols with modern IT systems using [multi-protocol gateways](./gateway/gateway.md).

![architecture-energy-utilities](./assets/architecture-energy-utilities.svg)

**Smart Grid & Renewables**
- **Grid Balancing**: Integrate distributed energy resources (DERs) and respond to supply/demand changes in real time for grid stability.
- **EV Charging Management**: Build scalable EV charging networks with smart charging and vehicle-to-grid (V2G) capabilities.
- **Predictive Asset Maintenance**: Monitor substations, transformers, and renewable assets in real time to predict failures and optimize maintenance.

**Oil & Gas**
- **Remote Asset Monitoring**: Monitor and control remote assets such as wellheads, pumps, and pipelines in real time.
- **Pipeline Leak Detection**: Instantly detect and locate leaks by analyzing real-time pressure and flow data from sensors.

Huabei Oilfield Company uses EMQX to connect over 40,000 data collection points, enabling real-time monitoring and intelligent analysis of their oilfield operations. [**Read Case Study →**](https://www.emqx.com/en/customers/huabei-oilfield-company)

### Healthcare

EMQX enables real-time patient monitoring, medical device integration, and next-generation telehealth solutions with a scalable, [secure data backbone](./access-control/security-guide.md). The platform provides HIPAA-ready security features, including [TLS/SSL encryption](./network/overview.md), robust authentication, and fine-grained access control to protect sensitive patient data.

![architecture-healthcare](./assets/architecture-healthcare.svg)

- **Remote Patient Monitoring (RPM)**: Continuously monitor patients' vital signs and health status from their homes, enabling early intervention and reducing hospital readmissions.
- **Medical Device Integration**: Connect and integrate data from infusion pumps, ventilators, and lab equipment for a unified view of patient care.
- **Smart Hospital Automation**: Automate hospital operations from tracking medical assets to optimizing patient flow and environmental conditions.
- **Telehealth & Telemedicine**: Enable real-time communication and data exchange between patients and healthcare providers for remote consultations.

### Financial Services

EMQX powers real-time financial applications with millisecond-level latency, bank-grade security, and 7×24 continuous service. The platform has provided more than five years of stable operation for enterprise-level financial users.
>>>>>>> origin/release-6.1

![architecture-financial](./assets/architecture-financial.svg)

<<<<<<< HEAD
EMQX は現代のエネルギーグリッドを支え、1000万以上のエンドポイントを100ms未満のレイテンシで接続し、重要なグリッド制御と保護アプリケーションを支援します。マルチプロトコルゲートウェイによりレガシー OT プロトコルと最新 IT システムを橋渡しします。

![architecture-energy-utilities](./assets/architecture-energy-utilities.svg)

**スマートグリッド＆再生可能エネルギー**
- **グリッドバランシング**：分散型エネルギーリソース（DER）を統合し、需給変動にリアルタイム対応してグリッド安定化。
- **EV 充電管理**：スマート充電と V2G 機能を備えたスケーラブルな EV 充電ネットワークを構築。
- **予知資産保全**：変電所、変圧器、再生可能資産をリアルタイム監視し、故障予測と保全最適化を実現。

**石油・ガス**
- **遠隔資産監視**：井戸口、ポンプ、パイプラインなど遠隔資産をリアルタイムで監視・制御。
- **パイプライン漏洩検知**：センサーのリアルタイム圧力・流量データを解析し、漏洩を即時検知・特定。

華北油田会社は EMQX を活用し、4万以上のデータ収集ポイントを接続、油田運用のリアルタイム監視とインテリジェント分析を実現しています。[**事例はこちら →**](https://www.emqx.com/en/customers/huabei-oilfield-company)

### ヘルスケア

EMQX はスケーラブルで[安全なデータ基盤](./access-control/security-guide.md)を提供し、リアルタイム患者モニタリング、医療機器統合、次世代テレヘルスソリューションを支援します。HIPAA 準拠のセキュリティ機能として[ TLS/SSL 暗号化](./network/overview.md)、堅牢な認証、細粒度アクセス制御を備え、機微な患者データを保護します。

![architecture-healthcare](./assets/architecture-healthcare.svg)

- **遠隔患者モニタリング（RPM）**：患者のバイタルサインや健康状態を自宅から継続監視し、早期介入と再入院率低減を実現。
- **医療機器統合**：輸液ポンプ、人工呼吸器、検査機器のデータを統合し、患者ケアの統合ビューを提供。
- **スマート病院オートメーション**：医療資産追跡から患者フローや環境条件の最適化まで病院運営を自動化。
- **テレヘルス・遠隔医療**：患者と医療提供者間のリアルタイム通信とデータ交換を可能にし、遠隔診療を支援。

### 金融サービス

EMQX はミリ秒レベルのレイテンシ、銀行グレードのセキュリティ、24時間365日の連続サービスでリアルタイム金融アプリケーションを支えています。企業レベルの金融ユーザーに5年以上の安定稼働実績があります。

![architecture-financial](./assets/architecture-financial.svg)

- **リアルタイム POS 監視**：数百万の POS 端末を接続し、取引データや端末状態をリアルタイム監視、予防保守を実現。
- **不正検知**：取引データを即時分析し、不正行為を顧客影響前に検出・防止。
- **モダン決済システム**：モバイル決済、デジタルウォレット、リアルタイム決済基盤を信頼性高く低レイテンシで構築。
- **市場データ配信**：株価や取引などのリアルタイム市場データを数千クライアントに最小レイテンシで配信。

[**事例はこちら →**](https://www.emqx.com/en/customers/emqx-in-finance-and-payment-iot)

### 通信

EMQX はキャリアグレードのスケーラビリティを提供し、単一プラットフォームで1億以上の同時デバイス接続をサポートします。MQTT、CoAP、LwM2M などのマルチプロトコル対応により IT/OT/CT のシームレス統合を実現します。

![architecture-telecom](./assets/architecture-telecom.svg)

- **5G IoT プラットフォーム**：5G ネットワーク上で数億の IoT デバイスを安定接続し、付加価値サービスの基盤を提供。
- **ネットワーク監視**：ネットワークインフラの健全性と性能をリアルタイムで継続監視し、問題を事前検知・解決。
- **スマートシティ基盤**：交通システム、公共交通、公益事業、緊急サービスを接続するスマートシティのデータ基盤を構築。

中国電信は EMQX を活用し、全国 IoT プラットフォーム CTWing を支え、1億以上の同時デバイス接続を実現しています。[**事例はこちら →**](https://www.emqx.com/en/customers/china-telecom)

### 小売・コンシューマ IoT

EMQX は数百万の小売デバイスやコンシューマ IoT エンドポイントを接続し、オムニチャネル体験、スマートホームオートメーション、インタラクティブアプリケーションのリアルタイムデータ移動を可能にします。

![architecture-retail](./assets/architecture-retail.svg)

- **スマートリテール**：リアルタイム在庫管理、POS 監視、パーソナライズ顧客エンゲージメント、動的価格設定を全店舗で実現。数千のセルフサービスキオスクを接続し、ピーク時もシームレスな顧客体験を提供。
- **スマートホーム**：数百万のスマートホームデバイスをスケーラブルな[パブリッシュ／サブスクライブメッセージング](./messaging/introduction.md)基盤で接続し、ホームオートメーション、エネルギー監視、Alexa や Google Assistant との統合を実現。
- **ゲーム＆ソーシャル**：数百万の同時ユーザー向けに超低レイテンシ通信を提供し、ゲーム内チャット、リアルタイム通知、ライブイベントをサポート。

Signify（旧 Philips Lighting）は EMQX を活用し、数百万の接続照明の信頼性高いリアルタイム制御を実現。位置情報ベースのソーシャルアプリ JAGAT は EMQX で数百万ユーザーのリアルタイムメッセージングを支えています。[**事例はこちら →**](https://www.emqx.com/en/customers/how-jagat-achieved-seamless-social-interaction-with-emqx)
=======
- **Real-Time POS Monitoring**: Connect millions of POS terminals to monitor transaction data and device status in real time, enabling proactive maintenance.
- **Fraud Detection**: Instantly analyze transaction data as it occurs to detect and prevent fraudulent activity before it impacts customers.
- **Modern Payment Systems**: Build reliable, low-latency infrastructure for mobile payments, digital wallets, and real-time clearing and settlement.
- **Market Data Distribution**: Reliably distribute real-time market data such as stock quotes and trades to thousands of clients with minimal latency.

[**Read Case Study →**](https://www.emqx.com/en/customers/emqx-in-finance-and-payment-iot)

### Telecommunications

EMQX provides carrier-grade scalability for 5G IoT services, supporting 100+ million concurrent device connections on a single platform. The platform enables seamless IT/OT/CT integration with multi-protocol support including MQTT, CoAP, and LwM2M.

![architecture-telecom](./assets/architecture-telecom.svg)

- **5G IoT Platform**: Reliably connect hundreds of millions of IoT devices over 5G networks, providing a stable foundation for value-added services.
- **Network Monitoring**: Continuously monitor the health and performance of network infrastructure in real time to proactively identify and resolve issues.
- **Smart City Backbone**: Build the data backbone for smart cities, connecting traffic systems, public transportation, utilities, and emergency services.

China Telecom, one of the world's largest telecom providers, uses EMQX to power its national IoT platform CTWing, supporting over 100 million concurrent device connections. [**Read Case Study →**](https://www.emqx.com/en/customers/china-telecom)

### Retail & Consumer IoT

EMQX connects millions of retail devices and consumer IoT endpoints, enabling real-time data movement for omnichannel experiences, smart home automation, and interactive applications.

![architecture-retail](./assets/architecture-retail.svg)

- **Smart Retail**: Power real-time inventory management, POS monitoring, personalized customer engagement, and dynamic pricing across all store locations. Connect thousands of self-service kiosks, ensuring seamless customer experiences even during peak hours.
- **Smart Home**: Connect millions of smart home devices with a scalable [pub/sub messaging](./messaging/introduction.md) backbone, enabling home automation, energy monitoring, and integration with platforms like Alexa and Google Assistant.
- **Gaming & Social**: Build responsive online games and social apps with ultra-low latency communication for millions of concurrent users, supporting in-game chat, real-time notifications, and live events.

Signify (formerly Philips Lighting) uses EMQX to power global smart lighting solutions, ensuring reliable real-time control for millions of connected lights. JAGAT, a location-based social app, handles millions of users with EMQX for reliable real-time messaging. [**Read Case Study →**](https://www.emqx.com/en/customers/how-jagat-achieved-seamless-social-interaction-with-emqx)
>>>>>>> origin/release-6.1
