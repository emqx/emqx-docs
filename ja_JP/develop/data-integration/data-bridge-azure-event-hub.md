# Azure Event Hubs への MQTT データストリーム

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データ取り込みに利用されます。EMQX の Azure Event Hubs との統合により、高スループット環境において信頼性の高いデータ転送および処理機能をユーザーに提供します。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、さらに Azure 仮想マシン上に展開された各種アプリケーションやサービスへ統合できます。現在、EMQX は SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを通じて Azure Event Hubs との統合をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ統合について包括的に紹介し、ルールと Sink の作成および検証手順を実践的に解説します。

## 動作概要

Azure Event Hubs データ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Azure Event Hubs とシームレスに連携させ、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう支援します。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Azure Event Hubs に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**: デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**: 内蔵のルールエンジンは、特定のトピックに基づいて MQTT メッセージを処理します。ルールエンジンは該当するルールにマッチし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **Azure Event Hubs へのブリッジング**: ルールはメッセージを Azure Event Hubs に転送するアクションをトリガーします。データプロパティ、オーダーキーの設定や MQTT トピックと Azure Event Hubs ヘッダーのマッピングを簡単に構成でき、より豊富なコンテキスト情報と順序保証を提供し、柔軟な IoT データ処理を実現します。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理および分析：強力な Azure Event Hubs のデータ処理・分析ツールとストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、貴重な洞察と意思決定支援を得られます。
- イベント駆動型機能：Azure のイベント処理をトリガーし、動的かつ柔軟な機能の起動と処理を実現します。
- データの保存と共有：メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データの安全な保存と管理を行います。これにより、他の Azure サービスと連携してデータを共有・分析し、多様なビジネスニーズに対応できます。

## 特長とメリット

EMQX と Azure Event Hubs 間のデータ統合は、以下の機能と利点をビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQX は数百万の MQTT クライアント接続をサポートし、毎秒数百万メッセージを Azure Event Hubs に継続的に取り込みます。これにより非常に低いメッセージ伝送および保存レイテンシを実現し、Azure Event Hubs の保持期間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：Azure Event Hubs の設定を通じて、MQTT トピックと Azure Event Hubs のイベントセンター間の柔軟なマッピングが可能です。また、MQTT ユーザープロパティを Azure Event Hubs ヘッダーにマッピングでき、より豊かなコンテキスト情報と順序保証を提供します。
- **弾力的なスケーラビリティ対応**：EMQX と Azure Event Hubs は共に弾力的なスケーラビリティをサポートし、アプリケーションの仕様に応じて数 MB から数 TB までの IoT データサイズを容易に拡張できます。
- **豊富なエコシステム**：標準 MQTT プロトコルを採用し、各種主流 IoT 伝送プロトコルをサポートすることで、EMQX は多様な IoT デバイスとの接続を実現します。さらに Azure Event Hubs は Azure Functions、各種プログラミング言語 SDK、Kafka エコシステムをサポートし、デバイスからクラウドまでの IoT データアクセスと処理をシームレスに促進します。

これらの機能は統合能力と柔軟性を高め、大量の IoT デバイスデータを迅速に Azure と接続することを支援します。ユーザーはクラウドコンピューティングがもたらすデータ分析とインテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションを構築できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hub のセットアップ

Azure Event Hub データ統合を利用するには、Azure アカウントで Namespace と Event Hub をセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ手順が記載されています。

- [クイックスタート：Azure ポータルでイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート：Azure Event Hubs と Apache Kafka を使ったデータストリーム](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX は「Connection String」の手順に従って接続します。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka 用 Azure Event Hubs とは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubs データ統合を作成するには、Azure Event Hubs Sink を Azure Event Hubs に接続するためのコネクターを作成します。

1. EMQX ダッシュボードで **Integrations** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとし、例として `my-azure-event-hubs` などを推奨します。
5. 接続詳細を設定します。
   - **Bootstrap Host**: Namespace のホスト名を入力します。デフォルトポートは `9093` です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**: Namespace の共有アクセス ポリシーの「Connection string - primary key」にある接続文字列を入力します。詳細は[イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**: Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は[外部リソースアクセスのための TLS 暗号化の有効化](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
   - **Request Timeout**: EMQX が Azure Event Hubs からの応答を待つ最大時間（秒）を指定します。デフォルトは `30` 秒です。タイムアウトを超えると接続が古くなったと判断し再接続します。この値が小さすぎると、Azure Event Hubs はリクエストを受け入れても応答を遅延させる場合があり、EMQX は再接続後にバッチを再送するため、重複メッセージや下流の過剰なデータ量が発生する可能性があります。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これで Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールと Sink を作成して Azure Event Hubs にストリームするデータを指定します。

## Azure Event Hubs Sink を追加したルールの作成

このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. 例として、ルール ID に `my_rule` を入力します。

4. MQTT メッセージをトピック `t/#` で Azure Event Hubs に保存したい場合、**SQL Editor** に以下のステートメントを入力します。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択し、EMQX がルールで処理したデータを Azure Event Hubs へ送信するようにします。

   **Action** ドロップダウンは `Create Action` のままにしてください。既存の Azure Event Hubs アクションを選択することも可能です。本デモでは新しい Sink を作成しルールに追加します。

6. Sink の名前と説明を **Name** および **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。ドロップダウン横のボタンから新規コネクターを作成することも可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink 情報を設定します。
   - **Event Hub Name**: 利用する Event Hub の名前を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートしています。詳細は[Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**: Azure Event Hub にパブリッシュされるメッセージに追加されるヘッダーとして使用するプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**: ヘッダーの値のエンコードモードを選択します。`none` または `json` が選択可能です。
   - **Extra Azure Event Hub headers**: **Add** をクリックして、Azure Event Hubs ヘッダーの追加のキー・バリューペアを指定できます。
   - **Message Key**: Event Hub メッセージキー。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Message Value**: Event Hub メッセージ値。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Partition Strategy**: プロデューサーがメッセージを Azure Event Hubs のパーティションに振り分ける方法を指定します。
     - `random`: 各メッセージに対してランダムにパーティションを選択します。
     - `key_dispatch`: Azure Event Hubs メッセージキーをハッシュしてパーティション番号を決定します。
   - **Partitions Limit**: プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリ Sink がメッセージ処理に失敗した場合にこれらのアクションがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**: 必要に応じて **sync** または **async** クエリモードを選択します。詳細は[Sink の機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックして Sink 設定を完了します。**Create Rule** ページの **Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示されます。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され Azure Event Hubs に送信・保存されていることがわかります。

## ルールのテスト

Azure Event Hubs データ統合が期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/) を使ってクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュします。

1. MQTTX でトピック `t/1` にメッセージを送信します。

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新たに 1 件の送信メッセージがあることを確認してください。
3. Kafka 互換のコンシューマーを使い、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法は[Azure Event Hubs for Apache Kafka Ecosystem での Kafka CLI を使ったメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 詳細設定

このセクションでは、コネクターおよび Sink のパフォーマンス最適化のための詳細オプションについて説明します。該当オブジェクト作成時に **Advanced Settings** を展開して設定してください。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （プロデューサー専用）有効にすると、クライアントがメタデータ取得リクエストを送信した際に Kafka トピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP 接続確立の最大待機時間（秒）。認証が有効な場合は認証時間も含みます。 | `5` 秒             |
| Start Timeout                     | 自動起動したリソースが正常状態になるまでの最大待機時間（秒）。これにより、Confluent クラスターなどの接続リソースが完全に稼働しデータ処理準備が整うまで Sink の操作を待機させます。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状況をチェックする間隔（秒）。                 | `15` 秒            |
| Health Check Timeout              | Azure Event Hubs との接続に対する自動ヘルスチェックのタイムアウト時間（秒）。 | `60` 秒            |
| Min Metadata Refresh Interval     | Azure Event Hubs Kafka ブローカーおよびトピックのメタデータ更新の最小間隔（秒）。小さすぎると Kafka サーバーの負荷が増加します。 | `3` 秒             |
| Metadata Request Timeout          | Kafka からメタデータを要求する際の最大待機時間（秒）。           | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能を最適化するためのソケットバッファサイズ。    | `1` MB             |
| No Delay                          | システムカーネルが TCP ソケットを即時送信するか遅延送信するかを選択。トグルをオンにすると「No Delay」が有効になり即時送信されます。オフの場合、送信内容が少ないと最大 40 ミリ秒の遅延が発生します。 | `Enabled`          |
| TCP Keepalive                     | Kafka ブリッジ接続に対して TCP キープアライブ機能を有効化し、長時間の非アクティブ状態による接続切断を防止します。値は `Idle, Interval, Probes` の3つの数値のカンマ区切りで指定します。<br />Idle: 接続がアイドル状態となってからキープアライブプローブを開始するまでの秒数。Linux のデフォルトは 7200 秒。<br />Interval: キープアライブプローブ間の秒数。Linux のデフォルトは 75 秒。<br />Probes: 応答がない場合に送信する最大プローブ数。Linux のデフォルトは 9。<br />例：`240,30,5,` は、240 秒のアイドル後にプローブ開始、30 秒ごとにプローブ送信、5 回応答がなければ接続切断と判断。 | `none`             |

### Azure Event Hubs プロデューサー Sink 設定

| 項目               | 説明                                                         | 推奨値             |
| ------------------ | ------------------------------------------------------------ | ------------------ |
| Max Batch Age      | メッセージがプロデューサーバッファ内に滞留できる最大時間。これを超えたバッチは送信されず破棄されます。切断中にバッファリングされたメッセージや、接続喪失時にアック待ちのメッセージも含みます。破棄されたメッセージは `dropped.expired` メトリクスにカウントされます。デフォルトの `infinity` はメッセージの期限切れを防ぎますが、バッファオーバーフロー時は破棄される場合があります。 | `infinity`         |
| Max Retries        | Azure Event Hubs がリトライ可能なエラー（例：パーティションリーダー変更）を返した場合の最大リトライ回数。初回試行とリトライがすべて失敗するとバッチは破棄され、各メッセージは `failed` メトリクスにカウントされます。明示的なエラー応答のみリトライ回数に加算され、接続喪失による再送は加算されません。デフォルトは無制限の `infinity`。 | `infinity`         |
| Reconnect Delay    | 接続喪失後にプロデューサーが Azure Event Hubs へ再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積され、バッファ制限と `max_batch_age` の影響を受けます。デフォルトは `2` 秒。 | `2` 秒             |
| Max Linger Time    | パーティションごとのプロデューサーがメッセージをバッチにまとめる最大待機時間。すべてのバッファモードに適用されます。デフォルトの `0` は待機なしでメッセージ遅延を最適化します。多少の遅延を許容するとリクエスト数削減に寄与します。バッチが満たされると待機は早期終了します。ディスクバッファリング時はバッチ書き込み前に待機が発生し、IOPS 削減のため最低 `5ms` の設定を推奨します。 | `0` ミリ秒         |
| Max Linger Bytes   | パーティションごとのプロデューサーがバッチ送信を開始するまでに蓄積する最大バイト数。 | `10` MB            |
