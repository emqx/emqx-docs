# EMQX 概要
EMQX は「無制限の接続、シームレスな統合、どこでも展開」を実現する大規模分散型 MQTT メッセージングプラットフォームです。高性能でスケーラブルな MQTT メッセージサーバーとして、EMQX Enterprise は IoT アプリケーション向けに信頼性の高いリアルタイムメッセージ伝送およびデバイス接続ソリューションを提供します。EMQX は50か国以上の2万社以上の企業ユーザーを有し、世界中で1億台以上の IoT デバイスを接続し、企業のデジタル化、リアルタイム化、インテリジェント化の変革を支えています。

商用のセルフホスト型 MQTT メッセージングプラットフォームである [EMQX Enterprise](https://www.emqx.com/en/products/emqx) は、クラスターあたり最大1億の同時 MQTT 接続をサポートします。単一サーバーで毎秒数百万の MQTT メッセージを処理しつつ、ミリ秒単位のレイテンシを維持します。強力な組み込みルールエンジンとデータ統合機能により、大量の IoT データのリアルタイム処理、変換、ルーティングが可能です。IoT データをさまざまなバックエンドデータベースや分析ツールとシームレスに統合し、企業が競争力のある IoT プラットフォームやアプリケーションを迅速に構築できるよう支援します。

<img src="./assets/emqx_platform.png" alt="emqx_platform" style="zoom:70%;" />

## 主なメリット

- [**大規模スケール**](https://www.emqx.com/en/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)：単一ノードで150万の MQTT デバイス接続を安定的にサポートし、クラスターは水平スケールで最大1億の同時 MQTT 接続に対応可能。
- [**業務クリティカルな信頼性**](./develop/cluster/mria-introduction.md)：組み込みの RocksDB データパーシステンスによりデータ損失を防止。
- [**データセキュリティ**](https://www.emqx.com/en/use-cases/mqtt-security)：エンドツーエンドの暗号化と細粒度のアクセス制御でデータを保護。
- [**複数プロトコル対応**](https://www.emqx.com/en/blog/iot-protocols-mqtt-coap-lwm2m)：MQTT、QUIC、CoAP、STOMP、LwM2M などをサポート。
- [**完全な MQTT 5.0 対応**](https://www.emqx.com/en/blog/introduction-to-mqtt-5)：EMQX は MQTT 5.0 と 3.x の両標準に完全準拠し、高いスケーラビリティ、セキュリティ、信頼性を提供。
- [**高性能**](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-supports-2m-message-throughput)：ノードあたり毎秒数百万の MQTT メッセージを効率的に処理。
- [**低レイテンシ**](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-message-latency-response-time)：ソフトリアルタイムランタイムによりミリ秒未満のメッセージ伝送レイテンシを保証。
- [**完全な可観測性**](./guides/dashboard/introduction.md)：リアルタイム MQTT トレースによる監視、アラート、高度なエンドツーエンド分析。
- [**クラウドネイティブ＆K8s 対応**](https://www.emqx.com/en/emqx-kubernetes-operator)：Kubernetes Operator と Terraform を使いオンプレミスやパブリッククラウドに簡単にデプロイ可能。

## 主なコンポーネント

EMQX Enterprise は複数のコンポーネントで構成され、強力でスケーラブルな MQTT メッセージングプラットフォームを構築します。以下は EMQX Enterprise の主要コンポーネントです。

### デバイス接続

EMQX Enterprise は MQTT 5.0 および 3.x 仕様に100%準拠し、卓越したスケーラビリティにより膨大な数の MQTT デバイスクライアント接続を容易に処理できます。[接続](https://www.emqx.com/en/blog/reaching-100m-mqtt-connections)を同時に管理可能です。同時に HTTP、QUIC、LwM2M/CoAP などのオープン標準プロトコルもサポートし、多様な IoT デバイスやシナリオの接続を実現します。さらにファイル転送や遅延パブリッシュなどの機能も拡張し、ユースケースを豊富にしています。

#### MQTT over QUIC

EMQX Enterprise は先駆的に [MQTT over QUIC](./develop/mqtt-over-quic/introduction.md) プロトコルを導入し、IoT クライアントが QUIC 経由で EMQX に接続して通信できるようにします。QUIC を利用するデバイスは接続性能とメッセージスループットを向上させ、メッセージレイテンシを低減します。これは特に、ネットワーク環境が不安定でリンクの頻繁な切り替えが発生する自動車向けインターネット（IoV）などのシナリオに有効で、リアルタイムかつ効率的なメッセージ伝送要件を満たします。

#### マルチプロトコルゲートウェイ

[マルチプロトコルゲートウェイ](./develop/gateway/gateway.md) は、MQTT 以外の異なる通信プロトコルを使うデバイス接続を EMQX Enterprise でサポートします。これらのゲートウェイはデバイスの接続要求を受け付け、使用されている通信プロトコルを識別し、各プロトコル仕様に従ってデバイスから送信されたメッセージ、コマンド、データを解析します。ゲートウェイはこれらのデータを MQTT メッセージ形式に変換し、メッセージ処理へと渡します。

### メッセージルーティング

EMQX Enterprise は [パブリッシュ／サブスクライブ](./get-started/messaging/introduction.md) パターンをサポートし、高信頼なメッセージ伝送メカニズムを提供します。これにより、メッセージが対象デバイスやアプリケーションに確実に届けられます。QoS 機構とセッション保持機能により、不安定なネットワーク環境でもデータを迅速かつ確実に配信し、業務の継続性と安定性を確保します。

### 分散クラスタリング

EMQX Enterprise はネイティブな [クラスタリング](./guides/cluster/create-cluster.md) 機能を備え、シームレスかつ弾力的なスケーリングを実現し、単一障害点を回避します。極限まで最適化された単一ノードは毎秒数百万の MQTT メッセージを低レイテンシで処理・配信可能です。[レイテンシ](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-single-node-message-latency-response-time)を抑えつつ、クラスターの水平スケールにより最大1億の同時 MQTT 接続をサポートし、IoV、産業オートメーション、スマートホームなどの大規模 IoT 展開に不可欠です。

### アクセス制御とデータセキュリティ

[TLS/SSL 暗号化](./guides/network/overview.md)および[認証](./guides/access-control/authn/authn.md)/[認可](./guides/access-control/authz/authz.md)メカニズムにより、EMQX Enterprise はデバイスデータ伝送の機密性と完全性を確保します。

EMQX Enterprise はユーザー名／パスワード、JWT、拡張認証、PSK、X.509 証明書など複数のクライアント認証方式を備えています。ACL に基づくパブリッシュ／サブスクライブ認可メカニズムを提供し、認証・認可データは LDAP、HTTP サービス、SQL／NoSQL データベースなどの外部企業セキュリティシステムと連携・管理可能で、多様かつ柔軟なクライアントセキュリティ保護を実現します。

さらに EMQX Enterprise は[監査ログ](./guides/audit-log.md)、ロール・権限管理、[シングルサインオン](./guides/sso.md)を備え、SOC 2 準拠や GDPR データプライバシー保護に対応。包括的なセキュリティ機能で業界標準に準拠した信頼性の高い IoT アプリケーション構築を支援します。

### ルールエンジンとデータ統合

EMQX Enterprise は強力な[ルールエンジン](./develop/data-integration/rules.md)を搭載し、EMQX 内でルールを設定して受信データの処理やルーティングが可能です。Sink 機能を使えば、EMQX Enterprise とクラウドサービスやデータベースを統合し、IoT データをクラウドに転送して保存・分析できます。

#### リアルタイムデータ処理

組み込みの SQL ベースルールエンジン、スキーマレジストリ、メッセージコーデック、[Flowデザイナー](./develop/flow-designer/introduction.md)により、デバイスイベントやメッセージ処理フローを簡単に作成・編集可能。IoT データのリアルタイム抽出、検証、フィルタリング、変換を実現します。

#### 企業向けデータ統合

標準搭載の Webhook や Sink/Source を通じて、Kafka、AWS RDS、MongoDB、Oracle、SAP、時系列データベースなど40以上のクラウドサービスや企業システムとシームレスに[統合](./develop/data-integration/data-bridges.md)可能。企業は IoT デバイスからのデータを効果的に管理・分析・活用し、多様なアプリケーションやビジネスニーズに対応できます。

### 管理・監視ダッシュボード

EMQX Enterprise は [ダッシュボード](./guides/dashboard/introduction.md) と呼ばれるグラフィカル管理システムを提供し、主要メトリクスや運用状況をリアルタイムで監視可能です。クライアント接続や機能設定の管理を簡素化し、クライアントやクラスターの異常診断・デバッグを支援。MQTT デバイスのオンライン状態をエンドツーエンドでトラブルシューティングでき、問題解決時間を大幅に短縮します。また、Prometheus、Datadog、OpenTelemetry 対応サービスなど外部サービスへの可観測性メトリクス統合もサポートし、運用監視能力を強化します。

## デプロイメントモードとエディション比較

EMQ は EMQX のデプロイメントとして、2つのマネージドサービス（EMQX Serverless と EMQX Dedicated）と1つのセルフホスト型（EMQX Enterprise）を提供しています。最適なデプロイメントを選択するために、以下の表に各タイプの機能サポート比較を示します。詳細な対応機能比較は[機能比較](./get-started/feature-comparison.md)をご参照ください。

<table>
<thead>
  <tr>
    <th colspan="1">セルフホスト型</th>
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
    <td>✔️ Business Source License (BSL) 1.1<br>✔️ MQTT over QUIC<br>✔️ RocksDB によるセッション永続化<br>✔️ Kafka/Confluent、Timescale、InfluxDB、PostgreSQL、Redis など40以上の企業システムとのデータ統合<br>✔️ 監査ログとシングルサインオン（SSO）<br>✔️ ロールベースアクセス制御（RBAC）<br>✔️ ファイル転送<br>✔️ メッセージコーデック<br>✔️ OCPP、JT/808、GBT32960 対応のマルチプロトコルゲートウェイ<br>✔️ 24時間365日のグローバル技術サポート<br> </td>
    <td>✔️ 従量課金制<br>✔️ 毎月無料クォータあり<br>✔️ 最大1000接続<br>✔️ 数秒でデプロイ開始<br>✔️ 自動スケーリング<br>✔️ 8時～17時のグローバル技術サポート</td>
    <td>✔️ 14日間無料トライアル<br>✔️ 時間単位課金<br>✔️ 世界中のマルチクラウドリージョン<br>✔️ 柔軟なスペック選択<br>✔️ VPC ピアリング、NAT ゲートウェイ、ロードバランサーなど<br>✔️ 40以上のクラウドサービスとの標準統合<br>✔️ 24時間365日のグローバル技術サポート<br> </td>
  </tr>
</tbody>
</table>

## ユースケース

EMQX Enterprise は IoT デバイス接続とデータ伝送のさまざまな段階で重要な役割を果たす包括的な IoT メッセージングプラットフォームであり、多様なビジネスニーズに対応する強力な機能と柔軟性を提供します。

パブリッシュ・サブスクライブメッセージ配信モデルに基づき、数百万のトピックや多様なモードで柔軟なメッセージ通信を実現し、さまざまなシナリオのリアルタイムメッセージ配信ニーズを満たします。組み込みのルールエンジンと Sink/Source により、メッセージを各種クラウドサービスに送信し、デバイスデータを企業システムとシームレスに統合可能です。データ処理、保存、分析、業務指令発行などのユースケースを容易にサポートします。以下は代表的なユースケースです。

### 双方向通信

EMQX Enterprise は多様なデバイスとアプリケーションエンドポイント間の接続をサポートし、双方向通信を実現します。例えばスマートホームでは、モバイルアプリが各種デバイスのセンサーデータを取得し、必要に応じて制御コマンドを送信します。このモードはデバイス間およびデバイスとアプリケーション間の柔軟な1対1または1対多通信を可能にします。

<img src="./assets/use_case_1.png" alt="use_case_1" style="zoom:50%;" />

ミッションクリティカルなアプリケーションにおける双方向通信の主なメリットは以下の通りです。

- **トピックベースのパブリッシュ／サブスクライブメッセージング**：EMQX のトピックベースモデルにより効率的かつ柔軟なメッセージルーティングを実現。
- **超低レイテンシ配信**：1ミリ秒以下のレイテンシで高速データ転送を実現し、リアルタイム応答性を確保。
- **包括的な QoS（サービス品質）保証**：エンドツーエンドの多層 QoS により信頼性と柔軟性の高いメッセージ配信を提供。

以下はより具体的な利用シナリオです。

#### ピアツーピア通信

EMQX を使いピアツーピア通信を構築可能です。非同期のパブリッシュ／サブスクライブモデルでは、メッセージパブリッシャーとサブスクライバーが動的に追加・削除でき、相互に疎結合となります。この疎結合性がアプリケーションとメッセージ通信に柔軟性をもたらします。

![use_case_1_ce](./assets/use_case_1_ce.png)

#### 大規模向けメッセージブロードキャスト

EMQX は金融市場の情報更新など、1対多メッセージングが重要なシナリオに優れています。多数のクライアントに対して効果的にメッセージをブロードキャストし、タイムリーな情報伝達を実現します。

![use_case_2_ce](./assets/use_case_2_ce.png)

#### 大量エンドポイントからのデータ集約

EMQX の多対一メッセージパターンは、工場プラント、近代的なビル、流通チェーン、電力網など大規模ネットワークのデータ集約に最適です。ネットワーク内のエンドポイントからデータを集め、クラウドやオンプレミスの集中バックエンドサーバーに転送・伝送します。

![use_case_3_ce](./assets/use_case_3_ce.png)

#### リクエスト・レスポンス認識によるトレーサブル通信

EMQX は MQTT 5.0 のリクエスト・レスポンス機能をサポートし、この機能により非同期通信アーキテクチャにおける通信の認識性とトレーサビリティを向上させます。

![use_case_4_ce](./assets/use_case_4_ce.png)

### 流れるデータの変換

強力な SQL ベースの[ルールエンジン](./develop/data-integration/rules.md)を内蔵し、EMQX は流れるデータをリアルタイムに抽出、フィルタリング、付加価値付与、変換可能です。処理済みデータは外部 HTTP サーバーや MQTT サービスに簡単に取り込めます。EMQX Enterprise では主流のデータベース、データストレージ、メッセージキューにも取り込み可能です。

![use_case_6_ce](./assets/use_case_6_ce.png)

### 異なるネットワーク間のデータ統合

パーティション化された、または制限されたネットワーク環境でも、EMQX はデータ統合を実現し、シームレスなメッセージング環境を提供します。

![use_case_5_ce](./assets/use_case_5_ce.png)

### テレメトリデータのアップロード

EMQX Enterprise はデバイスデータをクラウドにアップロードし、クラウド上で指定トピックのデータ処理・保存をサポートします。例えば産業生産シナリオでは、工場の各種産業機器データをリアルタイム処理し、製品品質のトレーサビリティや生産分析のためにデータベースに保存します。このモードはビジュアル設定が可能で、豊富なデータ処理機能を活用した迅速な開発を実現します。

<img src="./assets/use_case_2.png" alt="use_case_2" style="zoom:50%;" />

### 大容量ファイルアップロード

EMQX Enterprise は MQTT プロトコルの[ファイル転送](./develop/file-transfer/introduction.md)機能を提供し、デバイスが大容量ファイルデータをアップロードしてローカルまたは S3 ストレージに保存可能です。例えば IoV シナリオでは、機械学習ログファイルやパッケージ化された CAN バスデータをクラウドストレージに送信し、インテリジェント運転アルゴリズムモデルの更新に活用します。このモードは構造化データとファイル型データを統一データチャネルで扱い、アプリケーションの複雑性と保守コストを削減します。

<img src="./assets/use_case_3.png" alt="use_case_3" style="zoom:50%;" />

### クラウドベースの制御コマンド発行

EMQX Enterprise は MQTT メッセージ、REST API、Kafka などの Source を通じてメッセージ発行を可能にし、データプッシュやリモートデバイス制御を実現します。例えば金融取引シナリオでは、クラウドサービスがユーザーのウォッチリストに基づくリアルタイムデータをグループにプッシュします。このモードはトピックマッピング、発行用データ処理、データ到達統計を提供し、柔軟かつ信頼性の高いデータ発行を支援します。

<img src="./assets/use_case_4.png" alt="use_case_4" style="zoom:50%;" />

## 業界ソリューション

EMQX Enterprise は多様な業界向けに柔軟な IoT ソリューションを提供し、信頼性の高いデータ接続、効率的な伝送、柔軟な処理を通じてイノベーションと運用の卓越性を推進します。

### 自動車

#### 車載インターネットおよびテレマティクスサービスプロバイダー

将来の TSP プラットフォームは「データ駆動型」かつ「サービス指向」である必要があります。成功には車両との信頼性の高い接続、効率的なデータ伝送、柔軟なデータ処理が不可欠です。EMQX は堅牢で高性能かつメンテナンス容易なデータインフラ構築に欠かせません。[**詳細はこちら →**](https://www.emqx.com/en/blog/revolutionizing-tsp-platforms)

![use_case_5](./assets/use_case_5.png)

#### スマートコックピットおよび車載インフォテインメント

EMQ のクラウド側エンドツーエンド協調ソフトウェアアーキテクチャに基づき、自動車メーカーのスマートコックピットのコア機能を車両とクラウドの連携で構築支援します。[**詳細はこちら →**](https://www.emqx.com/en/use-cases/smart-cockpit)

![use_case_6](./assets/use_case_6.png)

#### 電気自動車充電ネットワーク

EV Power は EMQX を活用し、分散した設備エリアの管理困難や過酷な展開環境の課題を解決した充電スタンド運用プラットフォームを構築しています。[**詳細はこちら →**](https://www.emqx.com/en/customers/ev-power)

![use_case_7](./assets/use_case_7.png)

### 交通・輸送

#### 物流資産管理

EMQ は物流資産管理向けにデータ駆動型の包括的ソリューションを提供し、データ収集・伝送・処理機能を備えています。これにより企業は資産をリアルタイム監視し、有益な情報を得て管理の意思決定や競争力向上につなげられます。[**詳細はこちら →**](https://www.emqx.com/en/blog/a-data-driven-solution-for-logistics-asset-tracking-and-maintenance)

![use_case_8](./assets/use_case_8.png)

#### 車両管理

物流業界の複雑かつ動的な性質に対応し、輸送・配送プロセス全体で車両フリートの効果的な監視、スケジューリング、最適化が不可欠です。貨物の適時かつ確実な配送、コスト最適化、顧客満足は効率的なフリート管理に大きく依存します。[**詳細はこちら →**](https://www.emqx.com/en/blog/how-emqx-revolutionizes-logistics-fleet-management)

<img src="./assets/use_case_9.png" alt="design_9" style="zoom:70%;" />

#### V2X（Vehicle to Everything）通信

[V2X（車両間通信）](https://www.emqx.com/en/blog/what-is-v2x-and-the-future-of-vehicle-to-everything-connectivity)は、車両が他の車両（V2V）、歩行者（V2P）、インフラ（V2I）、ネットワーク（V2N）など周囲の要素とデータ交換する通信技術です。CVIS（協調車両インフラシステム）は、V2X 技術を各種センサー技術、クラウドコンピューティング、エッジコンピューティング、交通制御と統合する知能交通システムの有望な方向性です。EMQX がこの全体像で重要な役割を果たしています。[**詳細はこちら →**](https://www.emqx.com/en/blog/enhancing-v2x-connectivity-with-emq)

![use_case_10](./assets/use_case_10.png)

### 製造業および IIoT

EMQ スマートファクトリーソリューションは、包括的なデータ収集、伝送、分配メカニズムを構築し、設備の健康管理、エネルギー消費最適化、生産監視・分析、製品品質トレーサビリティ、サプライチェーンのパラメータ最適化、予知保全、欠陥検出など多様なインテリジェントアプリケーションを迅速に展開可能にします。[**詳細はこちら →**](https://www.emqx.com/en/blog/data-infrastructure-for-smart-factory)

![use_case_11](./assets/use_case_11.png)

### 石油・ガス

EMQ は石油業界向けにリアルタイムデータ収集と油田 IoT 端末機器のクラウド側協調管理をサポートする IoT データ収集ソリューションを提供しています。[**詳細はこちら →**](https://www.emqx.com/en/use-cases/oil-extraction-transportation)

![use_case_12](./assets/use_case_12.png)

### 金融・決済

EMQ の金融決済業界ソリューションは、顧客に7×24時間の継続サービスを実現し、企業ユーザー向けに5年以上の安定稼働とサービスを提供し続けています。[**詳細はこちら →**](https://www.emqx.com/en/customers/emqx-in-finance-and-payment-iot)

![use_case_13](./assets/use_case_13.png)

### エネルギー・公益事業

EMQ の IoT メッセージングミドルウェアにおける技術的リーダーシップと、SGITG の国家電網技術・市場における強みを活かし、両社は次世代電力 IoT 製品の共同開発を進めています。[**詳細はこちら →**](https://www.emqx.com/en/customers/sgitg-sgcc)

### キャリア

EMQ と深い協力関係にある E-Surfing IoT は、CTWing を世界最大規模のグループレベル NB-IoT デバイス接続プラットフォームに成長させました。この IoT プラットフォームの累計接続デバイス数は数百万に達しています。[**詳細はこちら →**](https://www.emqx.com/en/customers/china-telecom)

### コンシューマーエレクトロニクスおよび AIoT

EMQX ベースの IoT データアクセスプラットフォームは、インテリジェントサービスロボット企業に安定かつ効率的なデータアクセスサービスを提供し、5000以上のエンドカスタマーへのリーチを支援しています。[**詳細はこちら →**](https://www.emqx.com/en/customers/intelligent-service-robot-aiot)
