# Azure Event Grid MQTTとのブリッジ

[Azure Event Grid](https://azure.microsoft.com/en-us/products/event-grid) は、Azure上のフルマネージドイベントルーティングサービスです。そのMQTTブローカー機能により、IoTデバイスとクラウドアプリケーション間で標準ベースの双方向MQTT通信を大規模に実現できます。EMQXはAzure Event Grid用の組み込みコネクターを提供しており、EMQXとAzure Event Grid間でMQTTデータをブリッジし、Azureのクラウドサービスエコシステムとシームレスに統合できます。

本ページでは、EMQXとAzure Event Grid MQTTの連携について、SinkおよびSourceの作成と検証に関する実践的な手順を含めて詳細に解説します。

## 動作概要

Azure Event Gridのデータ統合は、EMQXのデバイス接続およびメッセージ送信機能と、Azure Event GridのクラウドネイティブMQTTブローカーを組み合わせた、EMQXの標準機能です。EMQXはMQTTクライアントとしてAzure Event Grid MQTTブローカーに接続し、双方向のメッセージ送受信を可能にします。

- **送信メッセージ（Sink）**：EMQXはローカルのMQTTトピックからメッセージを取得し、Azure Event Gridの指定トピックにパブリッシュします。
- **受信メッセージ（Source）**：EMQXはAzure Event Gridのトピックをサブスクライブし、受信したメッセージをローカルのEMQXトピックに転送します。

以下の図は連携の典型的なアーキテクチャを示しています。

![EMQX Integration Azure Event Grid](./assets/emqx-integration-azure-event-grid.png)

## 特長とメリット

Azure Event Gridとのデータ統合は以下の特長とメリットを提供します。

- **標準ベースのMQTTブリッジ**：Azure Event GridはMQTT 3.1.1およびMQTT 5.0をサポートし、EMQXは標準MQTTプロトコルでブリッジ可能なため、MQTT対応の任意のクライアントやサービスと相互運用できます。
- **双方向データフロー**：EMQXからAzure Event Gridへのパブリッシュ（Sink）と、Azure Event GridのトピックのサブスクライブおよびEMQXへの転送（Source）の両方をサポートし、柔軟なIoTデータルーティングを実現します。
- **安全な接続**：Azure Event GridはTLSを必須としています。コネクターはデフォルトでTLSを有効にし、クライアント証明書認証もサポートしており、本番環境での推奨認証方式です。
- **柔軟なトピックマッピング**：EMQXのルールエンジンを通じて、メッセージのフィルタリング、変換、動的トピックマッピングによる特定のAzure Event Gridトピックスペースへのルーティングが可能です。
- **豊富なAzureエコシステム連携**：データがAzure Event Gridに到達すると、Azure Functions、Azure Event Hubs、Azure Storageなど他のAzureサービスへルーティングされ、さらなる処理や分析が行えます。

## はじめる前に

### 前提条件

- EMQXのデータ統合に関する[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

### Azure Event Gridのセットアップ

EMQXでデータ統合を作成する前に、MQTTブローカーサポートが有効なAzure Event Gridネームスペースをセットアップしてください。以下のMicrosoft公式ドキュメントに手順が記載されています。

- [クイックスタート：Azure Event Gridネームスペースを使ったMQTTメッセージのパブリッシュとサブスクライブ](https://learn.microsoft.com/en-us/azure/event-grid/mqtt-publish-and-subscribe-portal)
- [Azure Event Grid MQTTブローカー概要](https://learn.microsoft.com/en-us/azure/event-grid/mqtt-overview)
- [証明書チェーンを使ったMQTTクライアント認証方法](https://learn.microsoft.com/en-us/azure/event-grid/mqtt-certificate-chain-client-authentication)

セットアップ完了後、EMQXでコネクター作成時に必要な以下の接続情報を控えてください。

- **ホスト名**：Event GridネームスペースのMQTTブローカーホスト名。形式は `<namespace>.ts.<region>.eventgrid.azure.net`。ポートは `8883` です。
- **クライアント証明書と秘密鍵**：Azure Event Gridはクライアント証明書認証を要求します。ネームスペースから証明書と秘密鍵をエクスポートし、コネクターのTLS設定時に使用します。
- **トピックスペース**：Azure Event Gridで設定したトピックスペースと権限バインディング。

::: tip

サポートされている認証方式やTLS要件については、[Azure Event Gridドキュメント](https://learn.microsoft.com/en-us/azure/event-grid/mqtt-client-authentication)を参照してください。

:::

## コネクターの作成

EMQXとAzure Event Gridを接続するコネクターの作成手順を示します。

1. EMQXダッシュボードで **Integration** -> **Connectors** をクリックします。

2. 画面右上の **Create** をクリックします。

3. **Create Connector** ページで **Azure Event Grid** を選択し、**Next** をクリックします。

4. コネクター名を入力します。英数字の組み合わせで、例として `my_azure_event_grid` などを指定します。

5. 接続情報を設定します：

   - **Server Host**：Event GridネームスペースのMQTTブローカーエンドポイントを入力します。例：`myns.northeurope-1.ts.eventgrid.azure.net:8883`。デフォルトポートは `8883` です。
   - **ClientID Prefix**：（任意）EMQXが生成するクライアントIDのプレフィックスを指定します。EMQXは `[prefix]:{connector name}{random string}:{pool index}` の形式で一意のクライアントIDを自動生成します。詳細は[接続プールとクライアントID生成ルール](./data-bridge-mqtt.md#connection-pool-and-client-id-generation-rules)を参照してください。
   - **Username** と **Password**：空欄のままにします。Azure Event Grid MQTTはユーザー名/パスワード認証を使用しません。
   - **Keepalive**：キープアライブ間隔（秒）を指定します。デフォルトは `160` 秒です。
   - **MQTT Version**：MQTTプロトコルバージョンを選択します。Azure Event GridはMQTT 3.1.1（`v4`）およびMQTT 5.0（`v5`）の両方をサポートします。
   - **Static ClientId Entries**：（任意）特定のEMQXノード用に静的クライアントIDを設定します。Azure Event Gridで事前登録されたクライアントIDが必要な場合に有効です。詳細は[静的クライアントIDの設定](./data-bridge-mqtt.md#configure-static-client-ids)を参照してください。

     ::: tip

     静的クライアントIDが定義されている場合、明示的に割り当てられたEMQXノードのみがMQTT接続を開始します。

     :::

   - **Clean Start**：デフォルトで有効です。有効時は、EMQXがAzure Event Gridに接続するたびに新しいセッションを開始します。
   - **Enable TLS**：必ず有効にしてください。Azure Event GridはTLSを要求します。クライアント証明書認証を使用する場合は、ここで証明書と秘密鍵を設定します。TLS設定の詳細は[外部リソースアクセスのTLS設定](../network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。

6. **詳細設定（任意）**：詳細は[コネクターの詳細設定](#connector-advanced-settings)を参照してください。

7. **Create**をクリックする前に、**Test Connectivity**をクリックしてEMQXからAzure Event Gridへの接続を検証できます。

8. **Create**をクリックしてコネクターの作成を完了します。作成成功ダイアログが表示され、ルールを今すぐ作成するか尋ねられます。**Create Rule**をクリックすると、コネクターが事前選択された状態でルール作成画面に進みます。**Back To Connector List**をクリックすると後でルールを作成できます。

## Azure Event Grid Sinkを使ったルールの作成

ローカルEMQXトピック `t/#` からAzure Event GridへMQTTメッセージを転送するルールの作成手順を示します。

1. 前のステップで **Create Rule** をクリックした場合、**Add Action** パネルが自動的に開き、**Type of Action** が `Azure Event Grid` に設定され、コネクターが事前選択されています。ステップ5へ進んでください。

   そうでない場合は、EMQXダッシュボードで **Integration** -> **Rules** をクリックし、右上の **Create** をクリックしてから **+ Add Action** をクリックします。

2. 左側の **SQL Editor** にルールIDと以下のSQLを入力し、トピック `t/#` のメッセージにマッチさせます。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** をクリックし、**Enable Test** を有効にしてSQLルールを学習・テストしてください。

   :::

3. 右側の **Add Action** パネルで、**Type of Action** ドロップダウンから `Azure Event Grid` を選択します。**Action** はデフォルトの `Create Action` のままにします。

4. **Connectors** ドロップダウンから先ほど作成した `my_azure_event_grid` コネクターを選択します。ドロップダウン横のボタンから新規コネクターを作成することも可能です。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

5. Sinkの名前と任意の説明を入力します。

6. Azure Event GridにメッセージをパブリッシュするためのSinkパラメータを設定します：

   - **Topic**：Azure Event Gridでパブリッシュするトピック。`${var}` プレースホルダーをサポートします。例：`devices/${clientid}/messages` と入力すると、クライアントIDに基づいて動的にトピックが設定されます。
   - **QoS**：パブリッシュメッセージのQoSレベル。`0`、`1`、`2`のいずれか、または `${qos}` のようなプレースホルダーを選択し、元のメッセージのQoSに従うことも可能です。
   - **Retain**：`true`、`false`、または `${flags.retain}` のようなプレースホルダーを選択してリテインフラグを設定します。
   - **Payload**：メッセージペイロードのテンプレート。空欄の場合はルール出力全体を転送し、`${payload}` のように指定するとペイロードのみを転送します。

7. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のために1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

8. **詳細設定（任意）**：詳細は[Sinkの詳細設定](#sink-advanced-settings)を参照してください。

9. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがAzure Event Gridに接続できるかをテストできます。

10. **Create**をクリックしてSinkの設定を完了します。新しいSinkが**Action Outputs**に追加されます。

11. **Create Rule**ページに戻り、設定内容を確認して**Save**をクリックしルールを生成します。

これでルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると新しいAzure Event Grid Sinkが表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって処理されAzure Event Gridに転送されていることを検証できます。

## Azure Event Grid Sourceを使ったルールの作成

Azure Event Gridからのメッセージをサブスクライブし、ローカルEMQXトピックに転送するルールの作成手順を示します。

### Azure Event Grid Sourceの作成とルールへの追加

1. EMQXダッシュボードで **Integration** -> **Rules** をクリックし、右上の **Create** をクリックします。

2. ルールIDに `my_rule_source` を入力します。

3. ルールのトリガーソースを設定します。画面右側の **Data Inputs** タブでデフォルトの **Message** 入力を削除し、**Add Input** をクリックしてAzure Event Grid Sourceを作成します。

4. **Add Input** ダイアログで、**Input Type** ドロップダウンから `Azure Event Grid` を選択します。**Source** ドロップダウンはデフォルトの `Create Source` のままにします。

5. Sourceの名前と説明を入力します。

6. ドロップダウンから `my_azure_event_grid` コネクターを選択します。

7. Azure Event Gridのサブスクライブ設定を行います：

   - **Topic**：Azure Event Gridでサブスクライブするトピック。`+` および `#` ワイルドカードをサポートします。

     ::: tip

     EMQXがクラスター運用中、またはコネクターが接続プール設定の場合は、重複メッセージを避けるため共有サブスクリプションを使用してください。例：`$share/group/devices/#`

     :::

   - **QoS**：サブスクライブのQoS。`0` または `1` を選択します。

8. **Create** をクリックしてSourceの作成を完了します。ルールのSQLは自動的に以下のように更新されます。

   ```sql
   SELECT
     *
   FROM
     "$bridges/azure_event_grid:<source_name>"
   ```

### Republishアクションの作成

Azure Event Gridからサブスクライブしたメッセージは自動的にローカルEMQXトピックに転送されません。Republishアクションを作成してメッセージをルーティングします。

1. **Create Rule** ページ右側の **Action Outputs** タブに切り替え、**Add Action** をクリックします。

2. **Type of Action** ドロップダウンから `Republish` を選択します。

3. Republishパラメータを設定します：

   - **Topic**：転送先のローカルトピック。例：`azure/${topic}` と入力すると、元のトピックに `azure/` プレフィックスが付加されます。
   - **QoS**：`${qos}` を選択して元のメッセージQoSに従うか、固定値を設定します。
   - **Retain**：`false` を選択するかプレースホルダーを使用します。
   - **Payload**：`${payload}` と入力してペイロードのみを転送するか、空欄のままにしてルール出力全体を転送します。

4. **Add** をクリックしてアクションを追加し、**Save** をクリックしてルールを生成します。

## ルールのテスト

### Sinkのテスト

[MQTTX](https://mqttx.app/) を使ってEMQXのトピック `t/1` にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Grid" }'
```

Azure Event Grid Sinkの稼働統計を確認し、新しいマッチ数と送信数がそれぞれ1件増えていることを確認してください。AzureポータルやAzure Event Grid MQTTクライアントでメッセージ受信を検証できます。

### Sourceのテスト

1. ローカルEMQXトピック `azure/#` をサブスクライブします。

   ```bash
   mqttx sub -t azure/# -q 1 -v
   ```

2. Azure Event Gridの認証情報で設定したMQTTクライアントを使い、Azure Event Gridにメッセージをパブリッシュします。

   ```bash
   mqttx pub -t devices/device1/messages -m "hello from azure" \
     -h myns.northeurope-1.ts.eventgrid.azure.net -p 8883 \
     --tls --cert /path/to/client.crt --key /path/to/client.key
   ```

3. EMQXのトピック `azure/devices/device1/messages` にメッセージが転送されていることを確認します。

   ```bash
   topic: azure/devices/device1/messages
   payload: hello from azure
   ```

## 詳細設定

Azure Event GridコネクターおよびSinkの詳細設定オプションについて説明します。ダッシュボードで設定する際は、**Advanced Settings** を展開して用途に応じて以下のパラメータを調整できます。

### コネクターの詳細設定

| 項目名 | 説明 | デフォルト値 |
| --- | --- | --- |
| Message Retry Interval | メッセージ配信失敗時の再試行間隔 | `15` 秒 |
| Bridge Mode | 有効にすると、接続先ブローカーにブリッジ接続であることを通知するMQTTブリッジモードを使用 | 無効 |
| Max Inflight | 1接続あたり未アックメッセージの最大数 | `32` |
| Connection Pool Size | Azure Event Gridへの同時MQTT接続数。増加させるとスループット向上 | `8` |
| Connect Timeout | Azure Event GridへのTCP接続確立の最大待機時間 | `10` 秒 |
| Start Timeout | 自動起動リソースが正常になるまでの最大待機時間 | `5` 秒 |
| Health Check Interval | 接続の自動ヘルスチェック実行間隔 | `15` 秒 |
| Health Check Timeout | 各ヘルスチェックの最大実行時間 | `60` 秒 |

### Sinkの詳細設定

| 項目名 | 説明 | デフォルト値 |
| --- | --- | --- |
| Buffer Pool Size | EMQXとAzure Event Grid間のデータフローを処理するバッファワーカープロセス数。負荷が高い場合は増加推奨 | `16` |
| Request TTL | バッファ内でリクエストが有効な最大時間。超過したリクエストは破棄される | `45` 秒 |
| Health Check Interval | Sinkの自動ヘルスチェック実行間隔 | `15` 秒 |
| Health Check Interval Jitter | 複数ノードが同時にヘルスチェックを行わないようにランダム遅延を追加。複数のActionやSourceが同じコネクターを共有する場合に有効 | `0` ミリ秒 |
| Health Check Timeout | Sinkヘルスチェックの最大実行時間 | `60` 秒 |
| Max Buffer Queue Size | 各バッファワーカーが保持可能な最大バイト数。バーストが多い場合は増加推奨 | `256` MB |
| Query Mode | `async` はAzure Event Gridの書き込み確認を待たずにパブリッシュを継続。スループットは高いが順序が乱れる可能性あり。`sync` は確認後に処理継続 | `Async` |
| Inflight Window | 同時に許容される未アックリクエスト数。**Query Mode** が `async` の場合はクライアント単位のメッセージ順序保証のため `1` に設定推奨 | `100` |

### Sourceの詳細設定

| 項目名 | 説明 | デフォルト値 |
| --- | --- | --- |
| Health Check Interval | Sourceの自動ヘルスチェック実行間隔 | `15` 秒 |
| Health Check Interval Jitter | 複数ノードが同時にヘルスチェックを行わないようにランダム遅延を追加。複数のActionやSourceが同じコネクターを共有する場合に有効 | `0` ミリ秒 |
| Health Check Timeout | Sourceヘルスチェックの最大実行時間 | `60` 秒 |
