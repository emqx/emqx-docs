# Azure Event Hubs への MQTT データストリーム

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データの取り込みに利用されます。EMQX の Azure Event Hubs との統合により、高スループット環境での信頼性の高いデータ転送と処理が可能になります。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、Azure 仮想マシン上に展開された各種アプリケーションやサービスに統合できます。現在、EMQX は SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを通じて Azure Event Hubs との統合をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ統合について、ルールと Sink の作成および検証に関する実践的な手順を含めて包括的に解説します。

## 動作概要

Azure Event Hubs データ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Azure Event Hubs とシームレスに連携し、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を通じて MQTT データを Azure Event Hubs に転送します。処理の流れは以下の通りです：

1. **IoT デバイスがメッセージをパブリッシュする**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理する**：組み込みのルールエンジンは、特定のトピックにマッチする MQTT メッセージを処理します。ルールにマッチしたメッセージは、データ形式の変換、特定情報のフィルタリング、文脈情報の付加などの処理を受けます。
3. **Azure Event Hubs へのブリッジング**：ルールはメッセージを Azure Event Hubs に転送するアクションをトリガーし、データプロパティやオーダーキーの設定、MQTT トピックと Azure Event Hubs ヘッダーのマッピングを簡単に行えます。これにより、データ統合におけるより豊かな文脈情報と順序保証が実現し、柔軟な IoT データ処理が可能になります。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です：

- リアルタイムデータ処理と分析：強力な Azure Event Hubs のデータ処理・分析ツールおよびストリーミング機能を活用し、メッセージデータのリアルタイム処理と分析を行い、有益なインサイトや意思決定支援を得られます。
- イベント駆動型機能：Azure のイベントハンドリングをトリガーし、動的かつ柔軟な機能の起動と処理を実現します。
- データの保存と共有：メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データの安全な保存と管理を行います。これにより、他の Azure サービスとデータを共有・分析し、多様なビジネスニーズに対応できます。

## 特長と利点

EMQX と Azure Event Hubs 間のデータ統合は、以下の機能とメリットをビジネスにもたらします：

- **高性能な大量メッセージスループット**：EMQX は膨大な数の MQTT クライアント接続をサポートし、毎秒数百万件のメッセージを Azure Event Hubs に継続的に取り込みます。これにより、極めて低いメッセージ伝送および保存のレイテンシを実現し、Azure Event Hubs の保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：設定された Azure Event Hubs を通じて、MQTT トピックと Azure Event Hubs のイベントセンター間で柔軟なマッピングが可能です。MQTT のユーザープロパティを Azure Event Hubs ヘッダーにマッピングすることもサポートし、データ統合におけるより豊かな文脈情報と順序保証を提供します。
- **弾力的なスケーラビリティ対応**：EMQX と Azure Event Hubs の両方が弾力的なスケーラビリティをサポートし、アプリケーションの要件に応じて数 MB から数 TB までの IoT データ規模を容易に拡張できます。
- **豊富なエコシステム**：標準 MQTT プロトコルを採用し、主要な IoT 伝送プロトコルをサポートすることで、多様な IoT デバイスとの接続を実現します。さらに、Azure Event Hubs が Azure Functions、各種プログラミング言語 SDK、Kafka エコシステムをサポートすることで、デバイスからクラウドまでのシームレスな IoT データアクセスと処理を促進します。

これらの機能は統合能力と柔軟性を高め、ユーザーが大量の IoT デバイスデータと Azure の接続を迅速に実現できるよう支援します。クラウドコンピューティングによるデータ分析・知能化の利点をより便利に活用し、強力なデータ駆動型アプリケーションの構築を可能にします。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hub のセットアップ

Azure Event Hub データ統合を利用するには、Azure アカウント上でネームスペースと Event Hub をセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ方法が記載されています。

- [クイックスタート: Azure ポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート: Azure Event Hubs と Apache Kafka を使ったデータストリーム](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX は接続に「接続文字列」を使用するため、「Connection String」の手順に従ってください。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka 用 Azure Event Hubs とは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubs データ統合を作成するには、Azure Event Hubs Sink を Azure Event Hubs に接続するコネクターを作成する必要があります。

1. EMQX ダッシュボードにアクセスし、**Integrations** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は英大文字・小文字と数字の組み合わせで、例として `my-azure-event-hubs` とします。
5. 接続情報を設定します。
   - **Bootstrap Host**：ネームスペースのホスト名を入力します。デフォルトポートは `9093` です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：ネームスペースの共有アクセスポリシーの「Connection string - primary key」から取得した接続文字列を入力します。詳細は[イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続の詳細オプションは[外部リソースアクセスの TLS 有効化](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
   - **Request Timeout**：EMQX が Azure Event Hubs からの応答を待つ最大時間（秒）を指定します。デフォルトは `30` 秒です。タイムアウトを超えると接続が古くなったと判断し再接続します。値が小さすぎると、Azure Event Hubs はリクエストを受け入れても応答を遅延させる場合があり、EMQX は再接続後に同じバッチを再送し、重複メッセージや過剰な下流データ量を引き起こす可能性があります。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。

これで Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。続いて、ルールと Sink を作成し、Azure Event Hubs にストリームするデータを指定します。

## Azure Event Hubs Sink を持つルールの作成

このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. 例として、ルール ID に `my_rule` を入力します。

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` の MQTT メッセージを Azure Event Hubs に保存する例です。

   注意：独自の SQL 文を指定する場合は、Sink が必要とするすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択し、EMQX がルールで処理したデータを Azure Event Hubs に送信するようにします。

   **Action** ドロップダウンは `Create Action` のままにしてください。既存の Azure Event Hubs アクションを選択することも可能です。本デモでは新しい Sink を作成してルールに追加します。

6. Sink の名前と説明を **Name** および **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink の情報を設定します。
   - **Event Hub Name**：使用する Event Hub の名前を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートしています。詳細は[Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hub にパブリッシュする際にメッセージに追加されるヘッダーのプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。`none` または `json` が選択可能です。
   - **Extra Azure Event Hub headers**：**Add** をクリックして、Azure Event Hubs ヘッダーの追加のキー・バリューを設定できます。
   - **Message Key**：Event Hub メッセージのキーを入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が利用可能です。
   - **Message Value**：Event Hub メッセージの値を入力します。こちらもプレーン文字列またはプレースホルダーを含む文字列が利用可能です。
   - **Partition Strategy**：プロデューサーがメッセージを Azure Event Hubs のパーティションに割り当てる方法を指定します。
     - `random`：メッセージごとにランダムにパーティションを選択します。
     - `key_dispatch`：Event Hubs メッセージキーのハッシュ値に基づいてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーが送信可能な最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて **sync** または **async** クエリモードを選択します。詳細は[Sink の機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示されます。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubs に送信・保存されていることがわかります。

## ルールのテスト

Azure Event Hubs データ統合が期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/) を使ってクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュします。

1. MQTTX を使ってトピック `t/1` にメッセージを送信します：

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新しいアウトゴーイングメッセージが 1 件あるはずです。
3. Kafka 互換のコンシューマーを使って、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法は[Apache Kafka エコシステム向け Azure Event Hubs でのメッセージ送受信に Kafka CLI を使う](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 高度な設定

このセクションでは、コネクターおよび Sink のパフォーマンス最適化のための高度なオプションについて説明します。該当オブジェクト作成時に **Advanced Settings** を展開して設定してください。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producer のみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際に Kafka トピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP 接続確立の最大待機時間（認証時間含む）。                  | `5` 秒             |
| Start Timeout                     | 自動起動したリソースが正常状態になるまでの最大待機時間（秒）。Sink が接続先リソース（例：Confluent クラスター）の準備完了を確認してから処理を進めるための設定。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状態をチェックする間隔。                      | `15` 秒            |
| Health Check Timeout              | Azure Event Hubs との接続に対する自動ヘルスチェックのタイムアウト時間。 | `60` 秒            |
| Min Metadata Refresh Interval     | クライアントが Azure Event Hubs Kafka ブローカーおよびトピックのメタデータを更新する際の最短間隔。短すぎると Kafka サーバーの負荷が増加する可能性あり。 | `3` 秒             |
| Metadata Request Timeout          | Kafka にメタデータ要求を送る際の最大待機時間。                | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能最適化のためのソケットバッファサイズ管理。 | `1` MB             |
| No Delay                          | システムカーネルが TCP ソケットを即時送信するか遅延送信するかの設定。オンで即時送信、オフの場合は最小送信内容が少ないときに約 40 ミリ秒の遅延が発生。 | `Enabled`          |
| TCP Keepalive                     | Kafka ブリッジ接続の TCP キープアライブ設定。接続の長時間アイドルによる切断防止。値は `Idle, Interval, Probes` のカンマ区切り3数値で指定。<br>Idle：アイドル状態が続く秒数（Linux デフォルト 7200 秒）<br>Interval：キープアライブプローブ間隔秒数（Linux デフォルト 75 秒）<br>Probes：応答なしと判断するまでの最大プローブ回数（Linux デフォルト 9 回）<br>例：`240,30,5` は 240 秒アイドル後にプローブ開始、30 秒間隔で最大 5 回プローブ送信し応答なければ切断判定。 | `none`             |

### Azure Event Hubs プロデューサー Sink 設定

| 項目               | 説明                                                         | 推奨値             |
| ------------------ | ------------------------------------------------------------ | ------------------ |
| Max Batch Age      | プロデューサーバッファ内のメッセージが送信されずに保持可能な最大時間。すべてのメッセージがこの時間を超えるとバッチは破棄される。切断中のバッファリングや応答待ちのメッセージも含む。破棄されたメッセージは `dropped.expired` メトリクスにカウント。デフォルトの `infinity` は期限切れなし。バッファオーバーフロー時は破棄される可能性あり。 | `infinity`         |
| Max Retries        | Azure Event Hubs がリトライ可能なエラー（例：パーティションリーダー変更）を返した場合の最大リトライ回数。初回試行とリトライがすべて失敗するとバッチは破棄され、各メッセージは `failed` メトリクスにカウント。明示的なエラー応答のみリトライ回数にカウントし、接続喪失による再送はカウントしない。デフォルトは無制限の `infinity`。 | `infinity`         |
| Reconnect Delay    | 接続喪失後にプロデューサーが Azure Event Hubs へ再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積される（バッファ制限および `max_batch_age` の範囲内）。デフォルトは `2` 秒。 | `2` 秒             |
| Max Linger Time    | パーティションごとのプロデューサーがより大きなバッチを形成するためにメッセージを蓄積する最大待機時間。すべてのバッファモードに適用。デフォルトの `0` は待機なしでメッセージレイテンシを最適化。多少の遅延を許容するとリクエスト数を削減可能。バッチが満杯になると早期に送信される。ディスクバッファリング時は書き込み前の待機時間となるため、IOPS 削減には最低 `5ms` の設定推奨。 | `0` ミリ秒         |
| Max Linger Bytes   | パーティションごとのプロデューサーが蓄積する最大バイト数。これを超えると待機をやめてバッチを送信。 | `10` MB            |
