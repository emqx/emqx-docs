# EMQX 概要
EMQX は「無制限の接続、シームレスな統合、どこでもデプロイ」を実現する大規模分散型 MQTT メッセージングプラットフォームです。高性能かつスケーラブルな MQTT メッセージサーバーとして、EMQX Enterprise は IoT アプリケーション向けに信頼性の高いリアルタイムメッセージ伝送とデバイス接続ソリューションを提供します。EMQX は50か国以上の2万社以上の企業ユーザーに採用され、世界中で1億台以上の IoT デバイスを接続し、企業のデジタル化、リアルタイム化、インテリジェント化の変革を支えています。

商用のセルフホスト型 MQTT メッセージングプラットフォームである[EMQX Enterprise](https://www.emqx.com/en/products/emqx)は、クラスターあたり最大1億の同時 MQTT 接続をサポートします。単一サーバーで毎秒数百万の MQTT メッセージを処理しつつ、ミリ秒単位のレイテンシを維持します。強力な組み込みルールエンジンとデータ統合機能により、大規模な IoT データのリアルタイム処理、変換、ルーティングが可能です。IoT データを様々なバックエンドデータベースや分析ツールとシームレスに統合し、企業が競争力の高い IoT プラットフォームやアプリケーションを迅速に構築できるよう支援します。

<img src="./assets/emqx_platform.png" alt="emqx_platform" style="zoom:70%;" />

## 主なメリット

- [**大規模スケール**](https://www.emqx.com/en/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)：単一ノードで150万の MQTT デバイス接続を安定的にサポートし、クラスターは水平スケールで最大1億の同時 MQTT 接続を処理可能。
- [**業務クリティカルな信頼性**](./develop/cluster/mria-introduction.md)：組み込みの RocksDB パーシステンスによりデータ損失を防止。
- [**データセキュリティ**](./guides/security-guide.md)：エンドツーエンドの暗号化と細粒度アクセス制御でデータを保護。
- [**複数プロトコル対応**](https://www.emqx.com/en/blog/iot-protocols-mqtt-coap-lwm2m)：MQTT、QUIC、CoAP、Stomp、LwM2M など多様なプロトコルをサポート。
- [**完全な MQTT 5.0 対応**](https://www.emqx.com/en/blog/introduction-to-mqtt-5)：EMQX は MQTT 5.0 と 3.x の両標準に完全準拠し、優れたスケーラビリティ、セキュリティ、信頼性を提供。
- [**高性能**](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-supports-2m-message-throughput)：ノードあたり毎秒数百万の MQTT メッセージを効率的に処理。
- [**低レイテンシ**](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-message-latency-response-time)：ソフトリアルタイムランタイムによりメッセージ配信のサブミリ秒レイテンシを保証。
- [**完全な可観測性**](./guides/dashboard/introduction.md)：リアルタイム MQTT トレーシングによる監視、アラート、高度なエンドツーエンド分析。
- [**クラウドネイティブ & K8s**](./get-started/deploy/kubernetes/kubernetes.md)：**Kubernetes Operator** によりオンプレミスやパブリッククラウドへの容易なデプロイが可能。

## 主なコンポーネント

EMQX Enterprise は複数のコンポーネントで構成され、強力かつスケーラブルな MQTT メッセージングプラットフォームを構築します。以下は EMQX Enterprise の主要コンポーネントです。

### デバイス接続

EMQX Enterprise は MQTT 5.0 および 3.x 仕様に100％準拠し、卓越したスケーラビリティにより膨大な数の MQTT デバイスクライアント接続を容易に処理できます。[接続](https://www.emqx.com/en/blog/reaching-100m-mqtt-connections)を同時にサポートします。同時に HTTP、QUIC、LwM2M/CoAP などのオープン標準プロトコルもサポートし、多様な IoT デバイスやシナリオの接続を可能にします。さらにファイル転送や遅延パブリッシュなどの機能も拡張し、ユースケースを豊かにしています。

#### MQTT over QUIC

EMQX Enterprise は先駆的に[MQTT over QUIC](./develop/mqtt-over-quic/introduction.md)プロトコルを導入し、IoT クライアントが QUIC 経由で EMQX に接続して通信可能にします。QUIC を利用することで接続性能やメッセージスループットが向上し、メッセージレイテンシを低減します。特に、弱いネットワーク環境や頻繁なリンク切替、不安定なネットワークが課題となるインターネットオブビークル（IoV）などのシナリオに適しています。MQTT over QUIC はリアルタイムかつ効率的なメッセージ伝送要件を満たします。

#### マルチプロトコルゲートウェイ

[マルチプロトコルゲートウェイ](./develop/gateway/gateway.md)は、MQTT 以外の異なる通信プロトコルを使用するデバイス接続を EMQX Enterprise でサポートします。ゲートウェイはデバイスの接続要求を受け付け、使用されている通信プロトコルを識別し、各プロトコル仕様に従ってデバイスから送信されるメッセージ、コマンド、データを解析します。解析したデータを MQTT メッセージ形式に変換し、以降のメッセージ処理に渡します。

### メッセージルーティング

EMQX Enterprise は[パブリッシュ／サブスクライブ](./get-started/messaging/introduction.md)パターンをサポートし、高い信頼性のメッセージ伝送機構を提供します。これにより、メッセージが意図したデバイスやアプリケーションに確実に配信されます。QoS 機構やセッション保持機能により、不安定なネットワーク環境でも迅速かつ確実にデータを届け、業務の継続性と安定性を確保します。

### 分散クラスタリング

EMQX Enterprise はネイティブな[クラスタリング](./develop/cluster/introduction.md)機能を備え、シームレスかつ弾力的なスケーリングを実現し、単一障害点を回避します。高度に最適化された単一ノードは毎秒数百万の MQTT メッセージを[低レイテンシ](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-message-latency-response-time)で処理・配信可能です。クラスターの水平スケールにより最大1億の同時 MQTT 接続をサポートし、IoV、産業オートメーション、スマートホームなど大規模 IoT 展開に不可欠な基盤となります。

### アクセス制御とデータセキュリティ

[TLS/SSL 暗号化](./guides/network/overview.md)および[認証](./guides/access-control/authn/authn.md)/[認可](./guides/access-control/authz/authz.md)機構により、EMQX Enterprise はデバイスデータ伝送の機密性と完全性を確保します。

EMQX Enterprise はユーザー名／パスワード、JWT、拡張認証、PSK、X.509 証明書など複数のクライアント認証方式を提供します。ACL に基づくパブリッシュ／サブスクライブ認可機構も備えています。認証・認可データは LDAP、HTTP サービス、SQL／NoSQL データベースなど外部企業セキュリティシステムと連携・管理可能で、多様かつ柔軟なクライアントセキュリティ保護を実現します。

さらに、EMQX Enterprise は[監査ログ](./guides/dashboard/audit-log.md)、ロール・権限管理、[シングルサインオン](./guides/dashboard/sso.md)を提供し、SOC 2 準拠や GDPR データプライバシー保護に対応。包括的なセキュリティ機能により、業界のセキュリティ基準に準拠した信頼性の高い IoT アプリケーション構築を支援します。

### ルールエンジンとデータ統合

EMQX Enterprise は強力な[ルールエンジン](./develop/data-integration/rules.md)を搭載し、EMQX 内でルールを設定して受信データを要件に応じて処理・ルーティングできます。さらに Sink 機能を利用して、EMQX Enterprise とクラウドサービスやデータベースを連携し、IoT データをクラウドに転送して保存・分析できます。

#### リアルタイムデータ処理

組み込みの SQL ベースルールエンジン、スキーマレジストリ、メッセージコーデック、[Flowデザイナー](./develop/flow-designer/introduction.md)により、デバイスイベントやメッセージ処理フローを簡単に作成・編集可能です。これにより IoT データのリアルタイム抽出、検証、フィルタリング、変換が実現します。

#### 企業向けデータ統合

標準搭載の Webhook や Sink/Source により、Kafka、AWS RDS、MongoDB、Oracle、SAP、時系列データベースなど40以上のクラウドサービスや企業システムとシームレスに[統合](./develop/data-integration/data-bridges.md)可能です。これにより企業は IoT デバイスからのデータを効果的に管理・分析・活用し、多様なアプリケーションやビジネスニーズを支えます。

### 管理・監視ダッシュボード

EMQX Enterprise は[ダッシュボード](./guides/dashboard/introduction.md)というグラフィカルな管理システムを提供し、主要メトリクスや稼働状況をリアルタイムで監視できます。クライアント接続や機能設定の管理を簡素化し、クライアントやクラスターの異常診断・デバッグを可能にします。これにより MQTT デバイスのオンライン状態をエンドツーエンドでトラブルシューティングでき、問題解決時間を大幅に短縮します。さらに Prometheus、Datadog、OpenTelemetry 対応サービスなど外部サービスへの可観測性メトリクス連携もサポートし、運用監視能力を強化します。

## デプロイモードとエディション比較

EMQ は EMQX のデプロイに3つの選択肢を提供しています。2つのマネージドサービス（EMQX Serverless と EMQX Dedicated）と1つのセルフホスト型（EMQX Enterprise）です。要件に最適なデプロイを選択できるよう、以下の表に各デプロイタイプの機能対応比較を示します。詳細な対応機能比較は[機能比較](./get-started/feature-comparison.md)を参照してください。

<table>
<thead>
  <tr>
    <th colspan="1">セルフホスト</th>
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
    <td><a href="https://www.emqx.com/en/apply-licenses/emqx">無料トライアルライセンス取得</a></td>
    <td><a href="https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew">無料で始める</a></td>
    <td><a href="https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew">14日間無料トライアル開始</a></td>
  </tr>
  <tr>
    <td>✔️ Business Source License (BSL) 1.1<br>✔️ MQTT over QUIC<br>✔️ RocksDB によるセッションパーシステンス<br>✔️ Kafka/Confluent、Timescale、InfluxDB、PostgreSQL、Redis など40以上の企業システムとのデータ統合<br>✔️ 監査ログとシングルサインオン（SSO）<br>✔️ ロールベースアクセス制御（RBAC）<br>✔️ ファイル転送<br>✔️ メッセージコーデック<br>✔️ OCPP、JT/808、GBT32960 に対応したマルチプロトコルゲートウェイ<br>✔️ 24時間365日グローバルテクニカルサポート<br> </td>
    <td>✔️ 従量課金制<br>✔️ 毎月無料クォータあり<br>✔️ 最大1000接続<br>✔️ 数秒でデプロイ開始<br>✔️ オートスケーリング<br>✔️ 8時〜17時グローバルテクニカルサポート</td>
    <td>✔️ 14日間無料トライアル<br>✔️ 時間単位課金<br>✔️ 世界中のマルチクラウドリージョン<br>✔️ 柔軟なスペック選択<br>✔️ VPC ピアリング、NAT ゲートウェイ、ロードバランサーなど<br>✔️ 40以上のクラウドサービスとの即時統合<br>✔️ 24時間365日グローバルテクニカルサポート<br> </td>
  </tr>
</tbody>
</table>

## ユースケース

EMQX Enterprise は包括的な IoT メッセージングプラットフォームとして、IoT デバイス接続やデータ伝送の各段階で重要な役割を果たし、多様なビジネスニーズに強力な機能と柔軟性を提供します。

パブリッシュ・サブスクライブメッセージ配信モデルに基づき、数百万のトピックや異なるモードで柔軟なメッセージ通信を実現し、多様なシナリオのリアルタイムメッセージ配信要件を満たします。組み込みのルールエンジンや Sink/Source を通じて、メッセージを様々なクラウドサービスに送信し、デバイスデータを企業システムとシームレスに統合可能です。データ処理、保存、分析、業務コマンド発行などのユースケースを容易にサポートします。以下は代表的なユースケースです。

### 双方向通信

EMQX Enterprise は様々なデバイスとアプリケーションエンドポイント間の接続をサポートし、双方向通信を実現します。例えばスマートホームでは、モバイルアプリが複数のデバイスからセンサーデータを取得し、必要に応じて制御コマンドを送信できます。このモードはデバイス間、またはデバイスとアプリケーション間の1対1または1対多の柔軟な通信を可能にします。

<img src="./assets/use_case_1.png" alt="双方向通信" style="zoom:50%;" />

ミッションクリティカルなアプリケーションにおける双方向通信の主な利点は以下の通りです。

- **トピックベースのパブリッシュ／サブスクライブメッセージング**：EMQX のトピックベースモデルにより効率的かつ柔軟なメッセージルーティングを実現。
- **超低レイテンシ配信**：1ミリ秒以下のレイテンシで迅速なデータ転送を実現し、リアルタイム応答性を確保。
- **包括的な QoS 保証**：EMQX はエンドツーエンドの多層 QoS 保証を提供し、信頼性と柔軟性の高いメッセージ配信を実現。

以下はより具体的な利用シナリオです。

#### ピアツーピア通信

EMQX を使ってピアツーピア通信を構築可能です。非同期のパブリッシュ／サブスクライブモデルでは、メッセージパブリッシャーとサブスクライバーが動的に追加・削除でき、相互に疎結合となります。この疎結合によりアプリケーションやメッセージ通信の柔軟性が向上します。

![use_case_1_ce](./assets/use_case_1_ce.png)

#### 大規模向けメッセージブロードキャスト

EMQX は金融市場の情報更新など、1対多メッセージングが重要なシナリオで優れています。多数のクライアントに対してメッセージを効果的にブロードキャストし、タイムリーな情報伝達を実現します。

![use_case_2_ce](./assets/use_case_2_ce.png)

#### 大量エンドポイントからのデータ集約

EMQX の多対一メッセージパターンは、工場、近代的なビル、小売チェーン、電力網など大規模ネットワークのデータ集約に最適です。ネットワーク内のエンドポイントからデータを中央のバックエンドサーバー（クラウドまたはオンプレミス）に転送・伝送できます。

![use_case_3_ce](./assets/use_case_3_ce.png)

#### リクエスト・レスポンス認識によるトレーサブル通信

EMQX は MQTT 5.0 のリクエスト・レスポンス機能をサポートします。この機能により、非同期通信アーキテクチャにおける通信認識性とトレーサビリティを向上できます。

![use_case_4_ce](./assets/use_case_4_ce.png)

### 流れるデータの変換

強力な SQL ベースの[ルールエンジン](./develop/data-integration/rules.md)を内蔵し、EMQX は流れるデータをリアルタイムで抽出、フィルタリング、強化、変換可能です。処理済みデータは外部 HTTP サーバーや MQTT サービスに容易に取り込めます。EMQX Enterprise では主流のデータベース、データストレージ、メッセージキューへの取り込みも可能です。

![use_case_6_ce](./assets/use_case_6_ce.png)

### 異なるネットワーク間のデータ統合

パーティション化された限定的なネットワーク環境でも、EMQX はデータ統合を構築し、シームレスなメッセージング環境を提供します。

![use_case_5_ce](./assets/use_case_5_ce.png)

### テレメトリデータアップロード

EMQX Enterprise はデバイスデータのクラウドアップロードと、クラウド上の特定トピックからのデータ処理・保存をサポートします。例えば産業生産現場では、工場内の各種産業機器データをリアルタイム処理し、製品品質のトレーサビリティや生産分析のためにデータベースに保存できます。このモードはビジュアルに設定可能で、豊富なデータ処理機能を活用した迅速な開発を実現します。

<img src="./assets/use_case_2.png" alt="テレメトリデータアップロード" style="zoom:50%;" />

### 大容量ファイルアップロード

EMQX Enterprise は MQTT プロトコルの[ファイル転送](./develop/file-transfer/introduction.md)機能を提供し、デバイスが大容量ファイルをアップロードしてローカルまたは S3 ストレージに保存可能です。例えば IoV シナリオでは、機械学習ログファイルやパッケージ化された CAN Bus データをクラウドストレージに送信し、インテリジェント運転アルゴリズムモデルの更新に活用します。このモードは構造化データとファイルタイプデータを統一データチャネルで扱い、アプリケーションの複雑さと保守コストを削減します。

<img src="./assets/use_case_3.png" alt="大容量ファイルアップロード" style="zoom:50%;" />

### クラウドベースの制御コマンド発行

EMQX Enterprise は MQTT メッセージ、REST API、Kafka などの Source を通じてメッセージ発行を可能にし、データプッシュやリモートデバイス制御を実現します。例えば金融取引シナリオでクラウドサービスがユーザーのウォッチリストに基づくリアルタイムデータをグループにプッシュすることが可能です。このモードはトピックマッピング、発行用データ処理、データ到達統計を提供し、柔軟かつ信頼性の高いデータ発行を支援します。

<img src="./assets/use_case_4.png" alt="クラウドベースの制御コマンド発行" style="zoom:50%;" />

## 業界別ソリューション

EMQX Enterprise は産業を問わず多様な IoT ソリューションを提供し、ミッションクリティカルなアプリケーションに信頼性の高いリアルタイム接続を実現します。コネクテッドビークルからスマート製造まで、EMQX は大規模イノベーションを支えます。

### 自動車・コネクテッドビークル

EMQX はソフトウェア定義車両（SDV）の未来を支え、世界の上位10自動車メーカーのうち5社で100以上の車種、3000万台以上の車両を接続しています。プラットフォームはミッションクリティカルな V2X とテレマティクスアプリケーションのリアルタイムデータ基盤を提供し、不安定なネットワーク環境に最適化された[MQTT over QUIC](./develop/mqtt-over-quic/introduction.md)を活用しています。

![architecture_vehicle_to_cloud](./assets/architecture-v2c.svg)

- **コネクテッドカー & SDV**：グローバル車両群のリモート診断、双方向コマンド制御、OTA アップデートを実現。[**詳細 →**](https://www.emqx.com/en/solutions/internet-of-vehicles)
- **フリートテレマティクス**：リアルタイムの位置追跡、使用ベース保険（UBI）、予知保全を超低レイテンシデータストリームで提供。[**詳細 →**](https://www.emqx.com/en/solutions/fleet-telematics)
- **EV 充電ネットワーク**：充電ステーション管理、スマート充電、車両からグリッド（V2G）アプリケーション向けのスケーラブルな MQTT 接続。
- **自動車製造**：ロボット、PLC、センサーを接続し、工場フロアのデータを統合して継続的な監視と品質保証を実現。[**詳細 →**](https://www.emqx.com/en/solutions/industrial-iot)

SAIC フォルクスワーゲンは EMQX を活用し、160万台以上の接続車両を持つ次世代 IoV プラットフォームを構築、リモート制御とリアルタイムデータ監視を支えています。[**事例 →**](https://www.emqx.com/en/customers/saic-volkswagen)

### 輸送・物流

一秒を争う業界で、EMQX はリアルタイムの車両可視化、不安定ネットワークでの信頼性あるデータ伝送、地理分散型デプロイによるレイテンシ最小化を提供します。数十万の車両とデバイスを単一の統合基盤に接続します。

![architecture-transportation-logistics](./assets/architecture-transportation-logistics.svg)

- **フリート管理**：車両位置追跡、ドライバー行動監視、リアルタイムルート最適化で燃料コスト削減と配送時間短縮を実現。[**詳細 →**](https://www.emqx.com/en/solutions/fleet-management)
- **スマート都市交通**：膨大な交通データをリアルタイム処理し、インテリジェント交通システムを支援。
- **V2X 通信**：安全性向上、交通効率化、自動運転アプリケーションのための車両間通信を実現。[**詳細 →**](https://www.emqx.com/en/solutions/software-defined-vehicles)
- **コールドチェーン監視**：温度・湿度をリアルタイム監視し、コンプライアンスと品質保持を確保。

深圳都市交通計画センター（SUTPC）は EMQX を活用し、170万台以上の車両データを処理、リアルタイム交通分析とインテリジェント交通システムを実現しています。[**事例 →**](https://www.emqx.com/en/customers/sutpc)

### 製造・IIoT

EMQX は工場フロアからクラウドまで全ての機械、システム、アプリケーションを接続し、OT と IT を AI ネイティブなデータ基盤で橋渡しします。Modbus、OPC-UA、Siemens S7 など100以上の産業プロトコルをサポートし、Sparkplug B による[統一ネームスペース（UNS）](https://www.emqx.com/en/solutions/unified-namespace)アーキテクチャで真のプラグアンドプレイ相互運用性を実現します。

![architecture-manufacturing](./assets/architecture-manufacturing.svg)

- **予知保全**：リアルタイムセンサーデータと AI により機械故障を予測し、計画外ダウンタイムを防止、設備寿命を延長。
- **OEE 最適化**：リアルタイムで総合設備効率を追跡し、工場生産性を向上。最大25％の OEE 向上と40％のダウンタイム削減を報告。
- **品質・トレーサビリティ**：品質逸脱を即時検知し、生産パラメータをリアルタイム監視、製品の完全なトレーサビリティを実現。
- **ライブパフォーマンス監視**：EMQX の[メトリクスと可観測性](./guides/observability/overview.md)機能を活用し、Prometheus や Datadog と連携したライブダッシュボードで生産ライン全体を可視化。

大手半導体ファブは EMQX を使い、1工場あたり350万以上のデータタグを100ms の収集周期で処理し、100％のデータ完全性を保ちながら精密製造を実現しています。[**詳細 →**](https://www.emqx.com/en/solutions/industrial-iot)

### エネルギー・公益事業

EMQX は現代のエネルギーグリッドを支え、1,000万以上のエンドポイントを100ms 以下のレイテンシで接続し、重要なグリッド制御・保護アプリケーションを実現します。レガシー OT プロトコルと最新 IT システムを[マルチプロトコルゲートウェイ](./develop/gateway/gateway.md)で橋渡しします。

![architecture-energy-utilities](./assets/architecture-energy-utilities.svg)

**スマートグリッド & 再生可能エネルギー**
- **グリッドバランシング**：分散型エネルギーリソース（DER）を統合し、需給変動にリアルタイム対応してグリッド安定化。
- **EV 充電管理**：スマート充電と V2G 機能を備えたスケーラブルな EV 充電ネットワークを構築。
- **予知資産保全**：変電所、変圧器、再生可能エネルギー資産をリアルタイム監視し、故障予測と保全最適化。

**石油・ガス**
- **遠隔資産監視**：井戸口、ポンプ、パイプラインなど遠隔資産をリアルタイムで監視・制御。
- **パイプライン漏洩検知**：センサーのリアルタイム圧力・流量データを解析し、漏洩を即時検知・位置特定。

華北油田は EMQX を活用し、4万以上のデータ収集ポイントを接続、油田運用のリアルタイム監視とインテリジェント分析を実現しています。[**事例 →**](https://www.emqx.com/en/customers/huabei-oilfield-company)

### ヘルスケア

EMQX はスケーラブルで[セキュアなデータ基盤](./guides/security-guide.md)を提供し、リアルタイム患者モニタリング、医療機器統合、次世代テレヘルスソリューションを実現します。HIPAA 準拠のセキュリティ機能として、[TLS/SSL 暗号化](./guides/network/overview.md)、堅牢な認証、細粒度アクセス制御を備え、機密性の高い患者データを保護します。

![architecture-healthcare](./assets/architecture-healthcare.svg)

- **遠隔患者モニタリング（RPM）**：患者のバイタルサインや健康状態を自宅から継続監視し、早期介入と再入院率低減を支援。
- **医療機器統合**：輸液ポンプ、人工呼吸器、検査機器などのデータを統合し、患者ケアの統一ビューを提供。
- **スマート病院オートメーション**：医療資産追跡から患者フローや環境条件の最適化まで病院運営を自動化。
- **テレヘルス・遠隔医療**：患者と医療提供者間のリアルタイム通信とデータ交換を可能にし、遠隔診療を支援。

### 金融サービス

EMQX はミリ秒レベルのレイテンシ、銀行グレードのセキュリティ、24時間365日の連続稼働を実現し、リアルタイム金融アプリケーションを支えています。企業レベルの金融ユーザーに5年以上の安定運用実績があります。

![architecture-financial](./assets/architecture-financial.svg)

- **リアルタイム POS 監視**：数百万の POS 端末を接続し、取引データや端末状態をリアルタイム監視、予防保守を実現。
- **不正検知**：取引データを即時分析し、不正行為を顧客影響前に検出・防止。
- **モダン決済システム**：モバイル決済、デジタルウォレット、リアルタイム清算・決済の信頼性高い低レイテンシ基盤を構築。
- **市場データ配信**：株価や取引などのリアルタイム市場データを数千のクライアントに最小レイテンシで配信。

[**事例 →**](https://www.emqx.com/en/customers/emqx-in-finance-and-payment-iot)

### 通信

EMQX はキャリアグレードのスケーラビリティを提供し、単一プラットフォームで1億以上の同時デバイス接続をサポートします。MQTT、CoAP、LwM2M などのマルチプロトコル対応で IT/OT/CT のシームレスな統合を実現します。

![architecture-telecom](./assets/architecture-telecom.svg)

- **5G IoT プラットフォーム**：5G ネットワーク上で数億の IoT デバイスを安定的に接続し、付加価値サービスの基盤を提供。
- **ネットワーク監視**：ネットワークインフラの健全性と性能をリアルタイムで継続監視し、問題を事前検知・解決。
- **スマートシティ基盤**：交通システム、公共交通、公益事業、緊急サービスを接続するスマートシティのデータ基盤を構築。

中国電信は EMQX を活用し、全国 IoT プラットフォーム CTWing を支え、1億以上の同時デバイス接続を実現しています。[**事例 →**](https://www.emqx.com/en/customers/china-telecom)

### 小売・コンシューマ IoT

EMQX は数百万の小売デバイスやコンシューマ IoT エンドポイントを接続し、オムニチャネル体験、スマートホームオートメーション、インタラクティブアプリケーションのリアルタイムデータ移動を可能にします。

![architecture-retail](./assets/architecture-retail.svg)

- **スマートリテール**：リアルタイム在庫管理、POS 監視、パーソナライズ顧客エンゲージメント、動的価格設定を全店舗で実現。数千のセルフサービスキオスクを接続し、ピーク時でもシームレスな顧客体験を提供。
- **スマートホーム**：数百万のスマートホームデバイスをスケーラブルな[パブリッシュ／サブスクライブメッセージング](./get-started/messaging/introduction.md)基盤で接続し、ホームオートメーション、エネルギー監視、Alexa や Google Assistant との連携を実現。
- **ゲーム・ソーシャル**：数百万の同時ユーザー向けに超低レイテンシ通信を提供し、ゲーム内チャット、リアルタイム通知、ライブイベントを支援。

Signify（旧 Philips Lighting）は EMQX を活用し、数百万の接続照明の信頼性あるリアルタイム制御を実現。JAGAT は EMQX で数百万ユーザーのシームレスなソーシャルインタラクションを支えています。[**事例 →**](https://www.emqx.com/en/customers/how-jagat-achieved-seamless-social-interaction-with-emqx)
