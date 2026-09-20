# Azure Event Hubs への MQTT データストリーム

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データの取り込みに利用されます。EMQX と Azure Event Hubs の統合により、高スループット環境下で信頼性の高いデータ転送および処理が可能になります。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、さらには Azure 仮想マシン上に展開されたさまざまなアプリケーションやサービスへ統合できます。現在、EMQX は SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを通じて Azure Event Hubs との統合をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ統合について包括的に紹介し、ルールと Sink の作成および検証方法を実践的に解説します。

## 動作概要

Azure Event Hubs とのデータ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Azure Event Hubs とシームレスに統合し、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Azure Event Hubs に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックに基づき MQTT メッセージを処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **Azure Event Hubs へのブリッジング**：ルールはメッセージを Azure Event Hubs に転送するアクションをトリガーし、データプロパティやオーダーキーの設定、MQTT トピックと Azure Event Hubs ヘッダーのマッピングを簡単に構成できます。これにより、より豊かなコンテキスト情報と順序保証が提供され、柔軟な IoT データ処理が可能になります。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：Azure Event Hubs の強力なデータ処理・分析ツールやストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、価値ある洞察や意思決定支援を得られます。
- イベント駆動型機能：Azure のイベント処理をトリガーし、動的かつ柔軟な関数の起動や処理を実現します。
- データの保存と共有：メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データを安全に保存・管理します。これにより他の Azure サービスと共有・分析し、さまざまなビジネスニーズに対応できます。

## 特長と利点

EMQX と Azure Event Hubs のデータ統合は、以下の機能とメリットをビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQX は膨大な数の MQTT クライアント接続をサポートし、毎秒数百万件のメッセージを継続的に Azure Event Hubs に取り込めます。これにより極めて低いメッセージ伝送・保存レイテンシを実現し、Azure Event Hubs の保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：Azure Event Hubs 側で設定した内容により、MQTT トピックと Azure Event Hubs のイベントセンター間で柔軟なマッピングが可能です。また、MQTT ユーザープロパティを Azure Event Hubs ヘッダーにマッピングでき、データ統合における豊かなコンテキスト情報と順序保証を提供します。
- **弾力的なスケーリング対応**：EMQX と Azure Event Hubs の双方が弾力的なスケーリングをサポートし、アプリケーションの仕様に応じて数MBから数TBまでの IoT データ規模を容易に拡張できます。
- **豊富なエコシステム**：標準 MQTT プロトコルを採用し、主要な IoT 通信プロトコルをサポートする EMQX は多様な IoT デバイスとの接続を実現します。さらに Azure Event Hubs は Azure Functions、各種プログラミング言語 SDK、Kafka エコシステムをサポートし、デバイスからクラウドまでのシームレスな IoT データアクセスと処理を促進します。

これらの機能により統合能力と柔軟性が向上し、ユーザーは大量の IoT デバイスデータと Azure の接続を迅速に実装できます。クラウドコンピューティングがもたらすデータ分析・インテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションを構築できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX のデータ統合に関する[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

### Azure Event Hub のセットアップ

Azure Event Hub データ統合を利用するには、Azure アカウントで Namespace と Event Hub をセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ方法の詳細があります。

- [クイックスタート：Azure ポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート：Azure Event Hubs と Apache Kafka を使ったデータストリーム](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX は「Connection String」の手順に従って接続します。
- [Event Hubs の接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka 用 Azure Event Hubs とは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubs データ統合を作成するには、Azure Event Hubs Sink と Azure Event Hubs を接続するコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integrations** -> **Connectors** をクリックします。
2. 画面右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプに **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとし、例として `my-azure-event-hubs` などを指定します。
5. 接続情報を設定します。
   - **Bootstrap Host**：Namespace のホスト名を入力します。デフォルトのポートは `9093` です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespace の Shared access policies の「Connection string - primary key」から取得した接続文字列を入力します。詳細は[Event Hubs の接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は[外部リソースアクセスの TLS 有効化](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
   - **Request Timeout**：EMQX が Azure Event Hubs からの応答を待つ最大時間を秒単位で指定します。デフォルトは `30` 秒です。タイムアウトを超えると EMQX は接続を古いものと判断して再接続します。この値が小さすぎると、Azure Event Hubs はパブリッシュ要求を受け入れても応答を遅延させる場合があり、EMQX は再接続後に同じバッチを再送するため、重複メッセージや下流の過剰なデータ量が発生する恐れがあります。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。

これで Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールと Sink を作成して Azure Event Hubs にストリームするデータを指定します。

## Azure Event Hubs Sink を持つルールの作成

このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. 例として、ルール ID に `my_rule` を入力します。

4. MQTT メッセージをトピック `t/#` で Azure Event Hubs に保存したい場合、**SQL Editor** に以下のステートメントを入力します。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択し、EMQX がルールで処理したデータを Azure Event Hubs に送信するようにします。

   **Action** ドロップダウンは `Create Action` のままにしてください。既存の Azure Event Hubs アクションを選択することも可能です。この例では新しい Sink を作成してルールに追加します。

6. Sink の名前と説明を **Name** と **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。隣のボタンをクリックして新しいコネクターを作成することも可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink の情報を設定します。
   - **Event Hub Name**：使用する Event Hub の名前を入力します。EMQX v5.7.2 以降では、このフィールドは動的トピック設定もサポートしています。詳細は[Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hub にパブリッシュされるメッセージに追加されるヘッダーとして使用するプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。選択肢は `none` または `json` です。
   - **Extra Azure Event Hub headers**：**Add** をクリックして、Azure Event Hubs ヘッダーの追加のキー・バリューを指定できます。
   - **Message Key**：Event Hub のメッセージキーを入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が使用可能です。
   - **Message Value**：Event Hub のメッセージ値を入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が使用可能です。
   - **Partition Strategy**：プロデューサーがメッセージを Azure Event Hubs のパーティションに振り分ける方法を指定します。
     - `random`：メッセージごとにランダムにパーティションを選択します。
     - `key_dispatch`：Event Hubs のメッセージキーをハッシュしてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトでは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリ Sink がメッセージ処理に失敗した場合にこれらのアクションがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて **sync** または **async** クエリモードを選択します。詳細は[Sink の機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを作成します。作成したルールはルール一覧に表示されます。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubs に送信・保存されていることがわかります。

## ルールのテスト

Azure Event Hubs データ統合が期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/) を使ってクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュします。

1. MQTTX を使ってトピック `t/1` にメッセージを送信します。

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新しい送信済みメッセージが 1 件あることを確認してください。
3. Kafka 互換のコンシューマーを使い、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法については、[Azure Event Hubs for Apache Kafka エコシステムで Kafka CLI を使ってメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 詳細設定

このセクションでは、コネクターおよび Sink のパフォーマンス最適化のための詳細オプションについて説明します。該当オブジェクト作成時に **Advanced Settings** を展開して設定してください。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producer のみ）有効にすると、クライアントがメタデータ取得要求を送信した際に存在しない Kafka トピックを自動作成します。 | `Disabled`         |
| Connect Timeout                   | TCP 接続確立の最大待機時間（認証有効時は認証時間も含む）       | `5` 秒             |
| Start Timeout                     | 自動起動したリソースが正常状態になるまで待機する最大秒数。これにより、Confluent クラスターなどのリソースが完全に稼働しデータ処理可能になるまで Sink の操作を保留できます。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状況をチェックする間隔                         | `15` 秒            |
| Health Check Timeout              | Azure Event Hubs との接続に対する自動ヘルスチェックのタイムアウト時間 | `60` 秒            |
| Min Metadata Refresh Interval     | クライアントが Azure Event Hubs Kafka ブローカーおよびトピックのメタデータを更新する最短間隔。短すぎると Kafka サーバーに不要な負荷がかかります。 | `3` 秒             |
| Metadata Request Timeout          | Kafka にメタデータ要求を送信した際の最大待機時間               | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能を最適化するためのソケットバッファサイズ   | `1` MB             |
| No Delay                          | システムカーネルが TCP ソケットを即時送信するか遅延送信するかの設定。オンにすると即時送信されます。オフの場合、送信内容が少ないときに約 40 ミリ秒の遅延が発生します。 | `Enabled`          |
| TCP Keepalive                     | Kafka ブリッジ接続の TCP キープアライブ機能を有効化し、長時間の非アクティブ状態による接続切断を防止します。値はカンマ区切りの3つの数値で指定します（Idle, Interval, Probes）：<br />Idle：接続がアイドル状態になる秒数（Linux デフォルト 7200 秒）<br />Interval：キープアライブプローブ間隔秒数（Linux デフォルト 75 秒）<br />Probes：応答なしで接続切断と判断するまでの最大プローブ数（Linux デフォルト 9）<br />例：`240,30,5,` は 240 秒アイドル後にプローブ開始、30 秒間隔で最大 5 回プローブし応答なしなら接続切断と判断します。 | `none`             |

### Azure Event Hubs プロデューサー Sink 設定

| 項目               | 説明                                                         | 推奨値             |
| ------------------ | ------------------------------------------------------------ | ------------------ |
| Max Batch Age      | プロデューサーバッファ内のメッセージが送信されずに保持される最大期間。すべてのメッセージがこの期間を超えるとバッチは破棄されます。切断中にバッファされたメッセージや、接続喪失時にアック待ちのメッセージも含みます。破棄されたメッセージは `dropped.expired` メトリクスにカウントされます。デフォルトの `infinity` はメッセージの期限切れを防ぎますが、バッファオーバーフローによる破棄は発生する可能性があります。 | `infinity`         |
| Max Retries        | Azure Event Hubs がリトライ可能なエラー（例：パーティションリーダー変更）を返した場合の最大リトライ回数。初回試行とすべてのリトライが失敗するとバッチは破棄され、各メッセージは `failed` メトリクスにカウントされます。明示的なエラー応答のみがリトライ回数を増やし、接続喪失による再送は増加させません。リトライは `max_batch_age` によって制限されます。デフォルトの `infinity` は無制限リトライを許可します。 | `infinity`         |
| Reconnect Delay    | 接続喪失後にプロデューサーが Azure Event Hubs に再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積され、バッファ制限と `max_batch_age` の影響を受けます。デフォルトは `2` 秒です。 | `2` 秒             |
| Max Linger Time    | パーティションごとのプロデューサーがより大きなバッチを作成するためにメッセージを蓄積する最大待機時間。すべてのバッファモードに適用されます。デフォルトの `0` は待機なしでメッセージングレイテンシを最適化します。小さな遅延を許容するとリクエスト数を削減できます。バッチが満杯になると早期に終了します。ディスクにバッファリングする場合は、バッチ書き込み前に待機が発生するため、IOPS 削減のため最低でも `5ms` の設定が推奨されます。 | `0` ミリ秒         |
| Max Linger Bytes   | パーティションごとのプロデューサーが蓄積して送信する最大バイト数 | `10` MB            |
