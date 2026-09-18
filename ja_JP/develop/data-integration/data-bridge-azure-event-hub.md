# Azure Event Hubs への MQTT データストリーム

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データ取り込みに利用されます。EMQX の Azure Event Hubs との統合により、ユーザーは高スループット環境において信頼性の高いデータ転送および処理機能を利用できます。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、さらに Azure 仮想マシン上に展開された各種アプリケーションやサービスと統合します。現在、EMQX は SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを通じて Azure Event Hubs との統合をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ統合について包括的に解説し、ルールおよび Sink の作成と検証手順を実践的に説明します。

## 動作概要

Azure Event Hubs とのデータ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームをシームレスに Azure Event Hubs と連携させ、IoT アプリケーション開発における豊富なサービスや機能を活用できるよう設計されています。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Azure Event Hubs に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**: デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**: 内蔵のルールエンジンは、特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、文脈情報の付加などの処理を行います。
3. **Azure Event Hubs へのブリッジング**: ルールはメッセージを Azure Event Hubs に転送するアクションをトリガーします。データプロパティやオーダーキーの設定、MQTT トピックと Azure Event Hubs ヘッダーのマッピングが容易に行えます。これにより、データ統合における豊富なコンテキスト情報と順序保証を実現し、柔軟な IoT データ処理を可能にします。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析: 強力な Azure Event Hubs のデータ処理・分析ツールおよびストリーミング機能を活用し、メッセージデータのリアルタイム処理と分析を行い、有益なインサイトや意思決定支援を得られます。
- イベント駆動型機能: Azure のイベントハンドリングをトリガーし、動的かつ柔軟な機能の起動と処理を実現します。
- データ保存と共有: メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データの安全な保存と管理を行います。これにより、他の Azure サービスと連携してデータの共有や分析を行い、多様なビジネスニーズに対応可能です。

## 特長とメリット

EMQX と Azure Event Hubs 間のデータ統合は、以下の機能と利点をビジネスにもたらします。

- **高性能な大量メッセージスループット**: EMQX は膨大な数の MQTT クライアント接続をサポートし、毎秒数百万件のメッセージを継続的に Azure Event Hubs に取り込めます。これにより、非常に低いメッセージ転送および保存レイテンシを実現し、Azure Event Hubs の保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**: 設定された Azure Event Hubs を通じて、MQTT トピックと Azure Event Hubs のイベントセンター間で柔軟なマッピングが可能です。MQTT ユーザープロパティを Azure Event Hubs ヘッダーにマッピングすることもサポートし、データ統合における豊富なコンテキスト情報と順序保証を提供します。
- **弾力的なスケーリング対応**: EMQX と Azure Event Hubs の双方が弾力的なスケーリングをサポートし、アプリケーションの仕様に応じて数MBから数TBまでの IoT データサイズを容易に拡張できます。
- **豊富なエコシステム**: 標準 MQTT プロトコルの採用により、各種主流の IoT 伝送プロトコルをサポートし、多様な IoT デバイスとの接続を実現します。さらに、Azure Event Hubs が Azure Functions、各種プログラミング言語SDK、Kafka エコシステムをサポートすることで、デバイスからクラウドまでの IoT データアクセスと処理をシームレスに行えます。

これらの機能により、統合能力と柔軟性が向上し、ユーザーは大量の IoT デバイスデータを迅速に Azure と接続できます。クラウドコンピューティングによるデータ分析やインテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションの構築を支援します。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX のデータ統合[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hub のセットアップ

Azure Event Hub のデータ統合を利用するには、Azure アカウント上で Namespace と Event Hub をセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ方法の詳細があります。

- [クイックスタート: Azure ポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート: Azure Event Hubs と Apache Kafka を使ったデータストリーム](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX が接続に使用するため、「Connection String」の指示に従ってください。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka 用 Azure Event Hubs とは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubs データ統合を作成するには、Azure Event Hubs Sink を Azure Event Hubs に接続するためのコネクターを作成する必要があります。

1. EMQX ダッシュボードにアクセスし、**Integrations** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせにしてください。例: `my-azure-event-hubs`
5. 接続情報を設定します。
   - **Bootstrap Host**: Namespace のホスト名を入力します。デフォルトポートは `9093` です。EMQX 5.10.5 以降では、IPv6 アドレスを角括弧で囲んで指定可能です（例: `[::1]:9093`）。
   - **IP Family**: EMQX 5.10.5 以降で、Azure Event Hubs への接続に使用する IP アドレスファミリーを選択します。
     - **Auto** (`auto`): デフォルト。IP アドレスの場合はそのアドレスファミリーを使用。ホスト名の場合は IPv4 を優先し、失敗時に IPv6 を試行。
     - **IPv4** (`ipv4`): IPv4 のみで接続。
     - **IPv6** (`ipv6`): IPv6 のみで接続。
   - **Connection String**: Namespace の Shared access policies の「Connection string - primary key」にある接続文字列を入力します。詳細は[イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**: Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続の詳細設定は[外部リソースアクセスの TLS 有効化](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これで、Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールと Sink を作成し、Azure Event Hubs にストリーミングするデータを指定します。

## Azure Event Hubs Sink を含むルールの作成

このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に例として `my_rule` を入力します。

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` の MQTT メッセージを Azure Event Hubs に保存する例です。

   注意: 独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンから `Azure Event Hubs` を選択し、EMQX がルールで処理したデータを Azure Event Hubs に送信するようにします。

   **Action** ドロップダウンはデフォルトの `Create Action` のままにするか、既存の Azure Event Hubs アクションを選択できます。本デモでは新しい Sink を作成しルールに追加します。

6. **Name** と **Description** テキストボックスに Sink の名前と説明を入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。必要に応じてドロップダウン横のボタンから新規コネクターを作成することも可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink 情報を設定します。
   - **Event Hub Name**: 使用する Event Hub の名前を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートします。詳細は[Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**: Azure Event Hub にパブリッシュされるメッセージに追加されるヘッダーとして使用されるプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**: ヘッダーの値のエンコードモードを選択します。選択肢は `none` または `json` です。
   - **Extra Azure Event Hub headers**: **Add** をクリックして、Azure Event Hubs ヘッダーの追加のキー・バリューペアを指定できます。
   - **Message Key**: Event Hub メッセージキー。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Message Value**: Event Hub メッセージ値。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Partition Strategy**: プロデューサーがメッセージを Azure Event Hubs のパーティションに振り分ける方法を指定します。
     - `random`: 各メッセージに対してランダムにパーティションを選択。
     - `key_dispatch`: Azure Event Hubs メッセージキーをハッシュしてパーティション番号を決定。
   - **Partitions Limit**: プロデューサーがメッセージを送信できるパーティションの最大数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**: 必要に応じて **sync** または **async** クエリモードを選択します。詳細は[Sink の機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページの **Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示されます。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubs に送信・保存されていることがわかります。

## ルールのテスト

Azure Event Hubs データ統合が期待通りに動作するかテストするために、[MQTTX](https://mqttx.app/) を使ってクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュできます。

1. MQTTX を使ってトピック `t/1` にメッセージを送信します。

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新しい送信メッセージが 1 件あることを確認してください。
3. Kafka 互換のコンシューマーを使い、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法については、[Azure Event Hubs for Apache Kafka エコシステムでの Kafka CLI を使ったメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 高度な設定

このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じたカスタマイズ操作が可能な高度な設定オプションについて説明します。対応するオブジェクト作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| フィールド                         | 説明                                                         | 推奨値             |
| --------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation          | （プロデューサーのみ）クライアントがメタデータフェッチ要求を送信した際、存在しない Kafka トピックを自動作成することを許可します。 | `Disabled`         |
| Connect Timeout                    | TCP 接続確立の最大待機時間（認証有効時は認証時間も含む）       | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大時間（秒）。Confluent クラスターなどのリソースが完全に稼働し、データ処理準備が整うまで Sink の操作を保留するための設定です。 | `5` 秒             |
| Health Check Interval              | コネクターの稼働状態をチェックする間隔時間                   | `15` 秒            |
| Health Check Timeout               | Azure Event Hubs との接続に対する自動ヘルスチェックのタイムアウト時間 | `60` 秒            |
| Min Metadata Refresh Interval      | クライアントが Azure Event Hubs Kafka ブローカーおよびトピックのメタデータを更新する際の最小間隔。短すぎると Kafka サーバーに不要な負荷をかける可能性があります。 | `3` 秒             |
| Metadata Request Timeout           | Kafka からメタデータを要求する際の最大待機時間               | `5` 秒             |
| Socket Send / Receive Buffer Size  | ネットワーク伝送性能最適化のためのソケットバッファサイズ管理 | `1` MB             |
| No Delay                         | システムカーネルが TCP ソケットを即時送信するか遅延送信するかを選択。トグルをオンにすると「No Delay」が有効となり即時送信されます。オフの場合、送信内容が少ないときに約 40 ミリ秒の遅延が発生することがあります。 | `Enabled`          |
| TCP Keepalive                    | Kafka ブリッジ接続の TCP キープアライブ機構を有効化し、長時間の非アクティブ状態による接続切断を防止します。値はカンマ区切りの3つの数値（`Idle, Interval, Probes`）で指定します。<br>Idle: 接続がアイドル状態である秒数（Linux デフォルト 7200 秒）<br>Interval: キープアライブプローブ間隔（Linux デフォルト 75 秒）<br>Probes: 応答なしと判断するまでの最大プローブ数（Linux デフォルト 9 回）<br>例: `240,30,5` は、240秒アイドル後にプローブ開始、30秒間隔で最大5回プローブを送信し応答なしなら接続切断と判定します。 | `none`             |
