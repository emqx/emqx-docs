# RabbitMQへのMQTTデータ取り込み

[RabbitMQ](https://www.rabbitmq.com/)は、Advanced Message Queuing Protocol（AMQP）を実装した広く使われているオープンソースのメッセージブローカーです。分散システム間のメッセージングにおいて堅牢でスケーラブルなプラットフォームを提供します。EMQXはRabbitMQとの統合をサポートしており、MQTTメッセージやイベントをRabbitMQへ転送できます。また、RabbitMQサーバーからデータを取得し、EMQXの特定のトピックにパブリッシュすることも可能で、RabbitMQからMQTTへのメッセージ配信を実現します。

本ページでは、EMQXとRabbitMQ間のデータ統合について詳細に解説し、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作の仕組み

RabbitMQデータ統合は、MQTTベースのIoTデータとRabbitMQの強力なメッセージキュー処理機能を橋渡しするためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからRabbitMQへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

RabbitMQ Sinkを例にとると、以下の図はEMQXとRabbitMQ間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration RabbitMQ](./assets/emqx-integration-rabbitmq.png)

MQTTデータをRabbitMQに取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスがMQTTプロトコルを通じてEMQXに正常に接続し、リアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づき、RabbitMQへルーティングすべきメッセージを決定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの拡充などが適用されます。
3. **RabbitMQへのメッセージ取り込み**：ルールによる処理が完了すると、メッセージをRabbitMQへ転送するアクションがトリガーされます。処理済みのメッセージはシームレスにRabbitMQに書き込まれます。
4. **データの永続化と活用**：RabbitMQはメッセージをキューに格納し、適切なコンシューマーに配信します。メッセージは他のアプリケーションやサービスによって消費され、データ分析、可視化、保存などのさらなる処理に利用されます。

## 特長と利点

RabbitMQとのデータ統合は、以下の特長とメリットをビジネスにもたらします：

- **信頼性の高いIoTデータメッセージ配信**：EMQXはデバイスからクラウドへの信頼性の高い接続とメッセージ配信を保証し、RabbitMQはメッセージの永続化と異なるサービス間の信頼性の高い配信を担い、各プロセスでのデータ信頼性を確保します。
- **MQTTメッセージの変換**：ルールエンジンを用いてEMQXはMQTTメッセージのフィルタリングや変換が可能です。データ抽出、フィルタリング、拡充、変換を経てRabbitMQへ送信します。
- **柔軟なメッセージマッピング**：RabbitMQデータ統合はMQTTトピックとRabbitMQのルーティングキーおよびエクスチェンジの柔軟なマッピングをサポートし、MQTTとRabbitMQ間のシームレスな連携を実現します。
- **高可用性およびクラスター対応**：EMQXとRabbitMQはどちらも高可用なメッセージブローカークラスターの構築をサポートし、ノード障害時でもサービス継続を保証します。クラスター機能により優れたスケーラビリティも提供されます。
- **高スループット環境での処理能力**：RabbitMQデータ統合は同期および非同期の書き込みモードをサポートし、レイテンシとスループットのバランスをシナリオに応じて柔軟に調整可能です。

## はじめる前に

このセクションでは、RabbitMQデータ統合の作成に先立ち必要な準備について説明します。RabbitMQサーバーの起動方法およびテスト用のRabbitMQエクスチェンジとキューの作成方法を含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)および[リパブリッシュアクション](./rule-get-started.md#add-republish-action)に関する知識
- UNIXターミナルとコマンドの基本知識

### RabbitMQサーバーの起動

ここでは[Docker](https://www.docker.com/)を使用してRabbitMQサーバーを起動する方法を紹介します。

以下のコマンドを実行すると、管理プラグインが有効なRabbitMQサーバーが起動します。管理プラグインによりWebインターフェースでRabbitMQを監視できます。

```bash
docker run -it --rm --name rabbitmq -p 127.0.0.1:5672:5672 -p 127.0.0.1:15672:15672 rabbitmq:3.11-management
```

詳細は[Docker HubのRabbitMQのページ](https://hub.docker.com/_/rabbitmq)をご参照ください。

### メッセージ受信用のエクスチェンジとキューの作成

RabbitMQサーバー起動後、RabbitMQ管理Webインターフェースを使って、EMQXから転送されるメッセージ受信用のテスト用エクスチェンジとキューを作成できます。既にテスト用のエクスチェンジとキューがある場合はこのセクションをスキップしてください。

1. Webブラウザで http://localhost:15672/ にアクセスし、RabbitMQ管理Webインターフェースを開きます。ログイン画面でデフォルトの認証情報を入力し、**Login**をクリックします。
   - **Username**: `guest`
   - **Password**: `guest`
2. 上部メニューの**Exchanges**タブをクリックし、**Add a new exchange**を展開して以下を入力します：
   * **Name**: `test_exchange`
   * **Type**: ドロップダウンから`direct`を選択
   * **Durability**: `Durable`を選択（RabbitMQ再起動後もエクスチェンジが存在）
   * **Auto delete**: `No`
   * **Internal**: `No`
   * **Arguments**: 空欄のまま
3. **Add exchange**ボタンをクリックしてテスト用エクスチェンジを作成します。
4. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します：
   * **Type**: `Default for virtual host`
   * **Name**: `test_queue`
   * **Durability**: `Durable`を選択（キューを永続化）
   * **Arguments**: 空欄のまま
5. **Add queue**ボタンをクリックしてテスト用キューを作成します。新しい`test_queue`が**All queues**に表示されます。
6. キュー名の**test_queue**をクリックし詳細ページを開きます。**Bindings**を展開し、**Add binding to this queue**セクションに以下を入力します：
   * **From exchange**: `test_exchange`
   * **Routing key**: `test_routing_key`
   * **Arguments**: 空欄のまま
7. **Bind**ボタンをクリックして`test_queue`を`test_exchange`に指定のルーティングキーでバインドします。

### メッセージパブリッシュ用のキュー作成

RabbitMQ管理Webインターフェースを使って、RabbitMQメッセージのパブリッシュ用キューを作成できます。

1. RabbitMQ管理Webインターフェースにログインします。
2. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します：
   * **Type**: `Default for virtual host`
   * **Name**: `message-send`
   * **Durability**: `Durable`を選択（キューを永続化）
   * **Arguments**: 空欄のまま
3. **Add queue**ボタンをクリックしてキューを作成します。新しい`message-send`が**All queues**に表示されます。

## コネクターの作成

このセクションでは、Rabbit Sink/SourceをRabbitMQサーバーに接続するためのコネクター作成方法を示します。

以下の手順はEMQXとRabbitMQの両方をローカルマシンで実行している前提です。RabbitMQが別の場所にデプロイされている場合は設定を適宜調整してください。

1. ダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. 画面右上の**Create**をクリックします。
3. **Create Connector**ページで**RabbitMQ**を選択し、**Next**をクリックします。
4. コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例えば`my_rabbitmq`など。
5. 接続情報を入力します。
   - **Servers**: `host[:port]`形式でカンマ区切りのRabbitMQノードリストを入力します。例：`rmq1:5672,rmq2:5672`。1つのノードへの接続が失敗した場合、EMQXはリストの次のノードに接続を試みます。異なる接続プールワーカーはリストの異なる位置から開始し、接続を分散します。

     ::: tip
     EMQX 6.0.4以降では複数のRabbitMQノードを設定可能です。フェイルオーバーは接続確立時に発生し、確立済みのAMQP接続のノード間移行は行いません。`server`と`port`で単一ノードを指定している既存設定は互換性があります。
     :::

   - **Port**: **Servers**でポート指定がないノードのデフォルトポート。デフォルトは`5672`。
   - **Username**: `guest`
   - **Password**: `guest`
   - **Virtual Host**: RabbitMQの仮想ホスト。デフォルトは`/`。
   - 暗号化接続を行う場合は**Enable TLS**をオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがRabbitMQサーバーに接続できるかテストできます。
7. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択可能です。**Create Rule**をクリックすると以下の選択肢があります：
   - **Action Outputs**：RabbitMQ Sinkを使ったルール作成でRabbitMQへ転送するデータを指定します。[RabbitMQ Sinkでルールを作成](#create-a-rule-with-rabbitmq-sink)の手順も参照可能です。
   - **Data Inputs**：RabbitMQ Sourceを使ったルール作成です。[RabbitMQ Sourceでルールを作成](#create-a-rule-with-rabbitmq-source)の手順も参照可能です。

## RabbitMQ Sinkを使ったルール作成

このセクションでは、ソースMQTTトピック`t/#`からのメッセージを処理し、処理結果をRabbitMQのキュー`test_queue`に転送するSinkを設定したルールをダッシュボードで作成する方法を示します。

### SQLを定義したルール作成

1. EMQXダッシュボードで、**Integration -> Rules**をクリックします。

2. 画面右上の**Create**をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. SQLエディターに以下の文を入力します。トピックパターン`t/#`にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT
     payload,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールの学習とテストが可能です。

   :::

5. ルールにアクションを追加し、Sinkを設定します。詳細は[ルールにRabbitMQ Sinkを追加](#add-rabbitmq-sink)を参照してください。
6. アクション追加後、**Action Outputs**セクションに新規Sinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックし、ルール作成を完了します。

これでルールが正常に作成されました。**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブに新しいRabbitMQ Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示されます。トポロジーはトピック`t/#`のメッセージがルール`my_rule`で解析されRabbitMQに書き込まれる流れを視覚的に示します。

### RabbitMQ Sinkの追加

このセクションでは、処理結果をRabbitMQに書き込むSinkをルールに追加する方法を示します。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをRabbitMQに送信します。

2. **Type of Action**ドロップダウンから`RabbitMQ`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既存のSinkを選択することも可能ですが、ここでは新規Sinkを作成します。

3. Sinkの名前を入力します。大文字・小文字の英数字の組み合わせで入力してください。

4. **Connector**ドロップダウンから`my_rabbitmq`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックします。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

5. Sinkの設定を以下のように行います：

   * **Exchange**: 事前に作成した`test_exchange`を入力します。メッセージはこのエクスチェンジにパブリッシュされます。

       ::: tip 注意

       RabbitMQにエクスチェンジが作成済みであることを確認してください。存在しない場合、アクションは一時的に機能しなくなり、定期的に再接続を試みます。
       :::

   * **Routing Key**: 事前に作成した`test_routing_key`を入力します。RabbitMQのメッセージパブリッシュ用ルーティングキーです。

       ::: tip

       エクスチェンジとルーティングキーはテンプレート値として設定可能で、プレースホルダーを使い受信したMQTTメッセージのペイロードから値を動的に抽出しルーティングに利用できます。

       例：ペイロード内の`akey`フィールドをルーティングキーに動的設定する場合、`${payload.akey}`と設定します。これによりペイロードの`akey`の値がルーティングキーとして使われます。

       **注意**：バッチモードでは、エクスチェンジとルーティングキーのテンプレート値はバッチ内のすべてのメッセージで一定でなければなりません。これにより一貫したルーティングが保証され、バッチ処理中の競合を防止します。
       :::

   * **Virtual Host**: RabbitMQの仮想ホスト。デフォルトは`/`。

   * **Message Delivery Mode**ドロップダウンで`non_persistent`または`persistent`を選択します：

     * `non_persistent`（デフォルト）：メッセージはディスクに永続化されず、RabbitMQの再起動やクラッシュ時に失われる可能性があります。

     * `persistent`：メッセージはディスクに永続化され、RabbitMQの再起動やクラッシュ時にも耐久性があります。

       ::: tip

       メッセージの損失を防ぐために、キューとエクスチェンジも永続化設定にする必要があります。詳細はRabbitMQの[ドキュメント](https://www.rabbitmq.com/documentation.html)を参照してください。

       :::

   * **Wait for Publish Confirmations**：デフォルトで有効。メッセージがRabbitMQに正常にパブリッシュされたことを確認します。

     ::: tip

     このオプションを有効にすると、RabbitMQブローカーはメッセージ受領を確認してから成功とみなすため、メッセージ配信の信頼性が向上します。

     :::

   * **Headers Template**および**Properties Template**：テンプレートを使ってRabbitMQのカスタムヘッダーやプロパティを定義します。詳細は[ヘッダーとプロパティテンプレートの設定](#set-headers-and-properties-templates)を参照してください。

   * **Payload Template**：デフォルトは空文字列で、メッセージペイロードをJSON形式のテキストとしてRabbitMQにそのまま転送します。

     プレースホルダーを用いてカスタムのメッセージペイロードフォーマットを定義することも可能です。例えば、MQTTメッセージのペイロードとタイムスタンプを含めたい場合、以下のテンプレートを使用します：

     ```json
      {"payload": "${payload}", "timestamp": ${timestamp}}
     ```

     このテンプレートは、受信したMQTTメッセージのペイロードとタイムスタンプを含むJSON形式のメッセージを生成します。`${payload}`と`${timestamp}`はプレースホルダーで、実際の値に置き換えられます。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

7. **詳細設定（任意）**：

   - **Publish Confirmation Timeout**：デフォルトは30秒。パブリッシュ確認のタイムアウト時間で、ブローカーのアックを待つ最大時間です。
   - 必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがRabbitMQサーバーに接続できるかテスト可能です。

9. **Create**ボタンをクリックしてSink設定を完了します。作成成功後、ルール作成ページに戻り、新しいSinkが**Action Outputs**に追加されます。

#### ヘッダーおよびプロパティテンプレートの設定

EMQX 6.0以降、RabbitMQ Sinkアクション作成時にカスタムのRabbitMQヘッダーおよびプロパティを定義可能です。これにより、メッセージに直接メタデータを付与し、RabbitMQ内での互換性やルーティングの柔軟性を高めます。

これらのフィールドはルールSQLの結果の変数（例：`${payload.device_id}`）をテンプレートとして利用可能です。ヘッダーおよびプロパティテンプレートは任意で、空欄の場合はメッセージに追加メタデータは付与されません。

##### ヘッダーテンプレートの設定方法

RabbitMQヘッダーとして1つ以上のキー・バリューのペアを追加できます。これらはユーザー定義のカスタムメタデータで、RabbitMQのコンシューマーが解釈可能です。

- **Key**：ヘッダー名。文字列で指定します。
- **Value**：キーに対応する値。静的文字列またはテンプレート変数を使用可能です。

例：MQTTペイロードのデバイスIDを含める場合

| Key         | Value                  |
| ----------- | ---------------------- |
| `device_id` | `${payload.device_id}` |

##### プロパティテンプレートの設定方法

RabbitMQは標準的なメッセージプロパティセットをサポートしています。EMQXではこれらのプロパティを定義し、コンテンツタイプや相関IDなどメッセージレベルのメタデータを付与可能です。

- **Key**：以下の有効なプロパティキーから選択します（無効なキーは無視されます）。
- **Value**：静的値またはテンプレート変数を設定します。

有効なプロパティキー：

- `content_type`
- `content_encoding`
- `priority`
- `correlation_id`
- `reply_to`
- `expiration`
- `message_id`
- `timestamp`
- `type`
- `user_id`
- `app_id`
- `cluster_id`

例：コンテンツタイプとアプリケーションIDを指定する場合

| Key            | Value              |
| -------------- | ------------------ |
| `content_type` | `application/json` |
| `app_id`       | `my_iot_app`       |

##### 使用例

MQTTメッセージペイロードが以下の場合：

```json
{
  "device_id": "sensor-123",
  "status": "ok"
}
```

以下の設定を行いたいとします：

- ヘッダーにMQTTペイロードの`device_id`を設定
- プロパティに静的値の`app_id`を設定

設定例：

**Headers Template**：

| Key         | Value                  |
| ----------- | ---------------------- |
| `device_id` | `${payload.device_id}` |

**Properties Template**：

| Key      | Value    |
| -------- | -------- |
| `app_id` | `my_app` |

この設定により、RabbitMQに転送されるすべてのメッセージには以下が含まれます：

- コンシューマーのロジック用カスタムメタデータ（ヘッダー）
- メッセージ処理やデバッグ用の標準メタデータ（プロパティ）

## RabbitMQ Sinkを使ったルールのテスト

EMQXダッシュボード内蔵のWebSocketクライアントを使ってルールとSinkのテストが可能です。

1. ダッシュボード左メニューの**Diagnose** -> **WebSocket Client**をクリックします。

2. 現在のEMQXインスタンスへの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用可能です。
   - 認証設定を変更している場合はユーザー名やパスワードを入力してください。

3. **Connect**をクリックしてクライアントをEMQXに接続します。

4. 下にスクロールしパブリッシュエリアに以下を入力します：
   * **Topic**: `t/test`
   * **Payload**: `Hello World RabbitMQ from EMQX`
   * **QoS**: `2`

5. **Publish**をクリックしてメッセージを送信します。

   Sinkとルールが正常に作成されていれば、指定したエクスチェンジとルーティングキーでRabbitMQサーバーにメッセージがパブリッシュされます。

6. http://localhost:15672 のRabbitMQ管理コンソールにアクセスし、**Queues**セクションに移動します。

   ::: tip

   デフォルト設定の場合、ユーザー名とパスワードは共に`guest`です。

   :::

7. メッセージが適切なキューにルーティングされていることを確認します。キューをクリックし詳細画面で**Get Message(s)**ボタンを押すとメッセージ内容を確認できます。

<img src="./assets/rabbitmq/rabbit_mq_management_ui_got_message.png" alt="ブリッジ受信" style="zoom:67%;" />

## RabbitMQ Sourceを使ったルール作成

このセクションでは、RabbitMQキューからEMQXへデータを転送するルール作成方法を示します。RabbitMQ Sourceとメッセージリパブリッシュアクションの両方を作成し、RabbitMQサービスからメッセージを取得してEMQXに転送します。

1. ダッシュボードの**Integration** -> **Rules**ページに移動します。

2. 画面右上の**Create**をクリックします。

3. ルールIDに`my_rule_source`を入力します。

4. ルールをトリガーするソース（Data Inputs）を設定します。画面右側の**Data Inputs**タブをクリックし、デフォルトの`Messages`入力を削除してから**Add Input**をクリックし、RabbitMQ Sourceを作成します。

5. **Add Input**ポップアップで、**Input Type**ドロップダウンから`RabbitMQ`を選択します。**Source**ドロップダウンはデフォルトの`Create Source`のままにします。この例では新規Sourceを作成しルールに追加します。

6. Sourceの**Name**と（任意の）**Description**を入力します。名前は大文字・小文字の英数字の組み合わせで、例：`my-rabbitmq-source`。

7. **Connector**ドロップダウンから先に作成した`my-rabbitmq`コネクターを選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックし、[コネクターの作成](#create-a-connector)の設定パラメータを参照してください。

8. RabbitMQからEMQXへメッセージを取得するためのSource情報を設定します：

   - **Queue**：RabbitMQで作成済みの`message-send`キュー名を入力します。
   - **No Ack**：RabbitMQから`no_ack`モードでメッセージを消費するか選択します。`no_ack`を有効にすると、RabbitMQはメッセージをコンシューマーが正常処理したかに関わらず即座にキューから削除します。
   - **Wait for Publish Confirmations**：メッセージパブリッシャーのアックを待つかどうか指定します。

9. 詳細設定（任意）：デフォルト値を使用します。

10. **Create**ボタンをクリックしてSource作成を完了し、ルールのデータ入力に追加します。同時にルールSQLが以下のように変更されます：

    ```sql
    SELECT
    *
    FROM
    "$bridges/rabbitmq:my-rabbitmq-source"
    ```

    ルールSQLはRabbitMQ Sourceから以下のフィールドにアクセス可能で、データ処理のためにSQLを調整できます。ここではデフォルトSQLを使用します。

    | フィールド名 | 説明                                                      |
    | :----------- | :-------------------------------------------------------- |
    | payload      | RabbitMQメッセージの内容                                  |
    | event        | イベントトピック。形式は`$bridges/rabbitmq:<source名>`   |
    | metadata     | ルールID情報                                              |
    | timestamp    | メッセージがEMQXに到着したタイムスタンプ                 |
    | node         | メッセージが到着したEMQXノード名                          |
    | queue        | メッセージが消費されたキュー名                            |
    | exchange     | メッセージがルーティングされたエクスチェンジ名           |
    | routing_key  | エクスチェンジからキューへメッセージをルーティングするためのルーティングキー |

これでRabbitMQ Sourceの作成は完了しましたが、サブスクライブしたデータはまだEMQXに直接パブリッシュされません。次にメッセージリパブリッシュアクションを作成し、SourceのメッセージをEMQXに転送します。

![rabbitmq_source](./assets/rabbitmq/rabbitmq_source.png)

### ルールへのリパブリッシュアクション追加

このセクションでは、RabbitMQ Sourceから取得したメッセージをEMQXトピック`t/1`にパブリッシュするためのリパブリッシュアクションをルールに追加する方法を示します。

1. 画面右側の**Action Output**タブを選択し、**Add Action**をクリックします。**Type of Action**ドロップダウンから`Republish`アクションを選択します。

2. メッセージリパブリッシュの設定を入力します：

   - **Topic**：MQTTにパブリッシュするトピック。ここでは`t/1`を入力します。

   - **QoS**：`0`、`1`、`2`、`${qos}`のいずれかを選択、または他のフィールドからQoSを設定するためのプレースホルダーを入力可能です。`${qos}`を選択すると元のメッセージのQoSに従います。

   - **Retain**：`true`または`false`を選択。メッセージをリテインメッセージとしてパブリッシュするかどうかを決定します。プレースホルダーも使用可能です。この例では`false`を選択します。

   - **Payload**：転送するメッセージペイロードのテンプレート。デフォルトは空欄でルールの出力結果を転送します。ここでは`${payload}`を入力し、ペイロードのみを転送します。

   - **MQTT 5.0 Message Properties**：デフォルトで無効。詳細設定は[リパブリッシュアクションの追加](./rule-get-started.md#add-republish-action)を参照してください。

3. **Create**をクリックしてアクション作成を完了します。成功するとルール作成ページに戻り、リパブリッシュアクションが**Action Outputs**タブに追加されます。

4. ルール作成ページで**Create**ボタンをクリックし、ルール作成を完了します。

これでルールが正常に作成されました。**Rules**ページで新規ルールを確認でき、**Sources**タブに新しいRabbitMQ Sourceが表示されます。

また、**Integrate** -> **Flow Designer**をクリックするとトポロジーが表示され、RabbitMQ Sourceからのメッセージがリパブリッシュを経て`t/1`にパブリッシュされる流れを直感的に確認できます。

## RabbitMQ Sourceを使ったルールのテスト

1. [MQTTX CLI](https://mqttx.app/cli)を使ってトピック`t/1`をサブスクライブします：

   ```bash
   mqttx sub -t t/1
   ```

2. 以下のコマンドでRabbitMQにメッセージを生成できます：

   ```bash
   rabbitmqadmin --username=guest --password=guest \
        publish routing_key=message-send \
        payload="{ \"msg\": \"Hello EMQX\"}"
   ```

   - `publish`はメッセージをパブリッシュするコマンドです。
   - `routing_key=message-send`オプションはメッセージのルーティングキーを設定します。この例ではキュー名をルーティングキーとして使用しています。
   - `payload="{ \"msg\": \"Hello EMQX\"}"`オプションはメッセージ内容を設定します。

   または、RabbitMQ管理インターフェースからもメッセージをパブリッシュ可能です：

   1. 上部メニューの**Queues**タブをクリックします。
   2. **Name**列の`message-send`をクリックし詳細ページを開きます。
   3. **Publish message**を展開し、**Payload**ボックスに`"Hello EMQX"`を入力し、**Publish message**ボタンをクリックします。

3. MQTTXで以下のような出力が表示されます：

   ```bash
   [2024-2-23] [16:59:28] › payload: {"payload":{"msg":"Hello EMQX"},"event":"$bridges/rabbitmq:my-rabbitmq-source","metadata":{"rule_id":"rule_0ly1"},"timestamp":1708678768449,"node":"emqx@127.0.0.1"}
   ```
