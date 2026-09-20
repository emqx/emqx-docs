# RabbitMQへのMQTTデータ取り込み

[RabbitMQ](https://www.rabbitmq.com/)は、Advanced Message Queuing Protocol（AMQP）を実装した広く利用されているオープンソースのメッセージブローカーです。分散システム間のメッセージングにおいて堅牢かつスケーラブルなプラットフォームを提供します。EMQXはRabbitMQとの統合をサポートしており、MQTTメッセージやイベントをRabbitMQに転送できます。また、RabbitMQサーバーからデータを取得し、EMQXの特定のトピックにパブリッシュすることも可能で、RabbitMQからMQTTへのメッセージ配信を実現します。

本ページでは、EMQXとRabbitMQ間のデータ統合について詳細に解説し、データ統合の作成および検証手順を実践的に説明します。

## 動作概要

RabbitMQデータ統合は、MQTTベースのIoTデータとRabbitMQの強力なメッセージキュー処理機能をつなぐためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、EMQXからRabbitMQへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

RabbitMQ Sinkを例にとると、以下の図はEMQXとRabbitMQ間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration RabbitMQ](./assets/emqx-integration-rabbitmq.png)

MQTTデータをRabbitMQに取り込む流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、リアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づき、RabbitMQにルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、ペイロードの付加情報による拡充などが適用されます。
3. **RabbitMQへのメッセージ取り込み**：ルールによる処理が完了すると、RabbitMQへメッセージを転送するアクションがトリガーされます。処理済みメッセージはシームレスにRabbitMQに書き込まれます。
4. **データの永続化と活用**：RabbitMQはメッセージをキューに格納し、適切なコンシューマーに配信します。メッセージは他のアプリケーションやサービスによって消費され、データ分析、可視化、保存などのさらなる処理に利用されます。

## 特長と利点

RabbitMQとのデータ統合は、以下の特長とメリットをもたらします。

- **信頼性の高いIoTデータメッセージ配信**：EMQXはデバイスからクラウドへの安定した接続とメッセージ配信を保証し、RabbitMQはメッセージの永続化と異なるサービス間の信頼性の高い配信を担い、全体のデータ信頼性を確保します。
- **MQTTメッセージの変換**：ルールエンジンを利用し、EMQXはMQTTメッセージの抽出、フィルタリング、拡充、変換を実施してからRabbitMQに送信できます。
- **柔軟なメッセージマッピング**：RabbitMQデータ統合はMQTTトピックとRabbitMQのルーティングキーおよびエクスチェンジの柔軟なマッピングをサポートし、MQTTとRabbitMQ間のシームレスな統合を実現します。
- **高可用性とクラスター対応**：EMQXとRabbitMQはどちらも高可用性のメッセージブローカークラスター構築をサポートし、ノード障害時もサービス継続を可能にします。クラスター機能を活用することで優れたスケーラビリティも実現します。
- **高スループット環境での処理能力**：RabbitMQデータ統合は同期・非同期の書き込みモードをサポートし、用途に応じてレイテンシとスループットのバランスを柔軟に調整できます。

## はじめる前に

本節では、RabbitMQデータ統合の作成を始める前に必要な準備事項を説明します。RabbitMQサーバーの起動方法やテスト用のエクスチェンジおよびキューの作成方法を含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)および[リパブリッシュアクション](./rule-get-started.md#add-republish-action)に関する知識
- UNIXターミナルおよび基本コマンドの知識

### RabbitMQサーバーの起動

ここでは[Docker](https://www.docker.com/)を使ったRabbitMQサーバーの起動方法を紹介します。

以下のコマンドを実行して、管理プラグインを有効にしたRabbitMQサーバーを起動します。管理プラグインにより、WebインターフェースでRabbitMQを監視できます。

```bash
docker run -it --rm --name rabbitmq -p 127.0.0.1:5672:5672 -p 127.0.0.1:15672:15672 rabbitmq:3.11-management
```

Docker Hubの[RabbitMQのDocker実行に関する情報](https://hub.docker.com/_/rabbitmq)もご参照ください。

### メッセージ受信用のエクスチェンジとキューの作成

RabbitMQサーバー起動後、RabbitMQ管理Webインターフェースを使って、EMQXから転送されるメッセージ受信用のテスト用エクスチェンジとキューを作成できます。既にテスト用のエクスチェンジとキューがある場合はこの節をスキップしてください。

1. Webブラウザで http://localhost:15672/ にアクセスし、RabbitMQ管理Webインターフェースを開きます。ログイン画面で以下のデフォルト認証情報を入力し、**Login**をクリックします。
   - **Username**: `guest`
   - **Password**: `guest`
2. 上部メニューの**Exchanges**タブをクリックします。**Add a new exchange**を展開し、以下を入力します。
   * **Name**: `test_exchange`
   * **Type**: ドロップダウンから`direct`を選択
   * **Durability**: `Durable`を選択し、RabbitMQサーバー再起動後もエクスチェンジが残るように設定
   * **Auto delete**: `No`
   * **Internal**: `No`
   * **Arguments**: 空欄のまま
3. **Add exchange**ボタンをクリックしてエクスチェンジを作成します。
4. 上部メニューの**Queues**タブをクリックします。**Add a new queue**を展開し、以下を入力します。
   * **Type**: `Default for virtual host`
   * **Name**: `test_queue`
   * **Durability**: `Durable`を選択し、キューの永続化を設定
   * **Arguments**: 空欄のまま
5. **Add queue**ボタンをクリックしてキューを作成します。新しい`test_queue`が**All queues**に表示されます。
6. キュー名の**test_queue**をクリックして詳細ページを開きます。**Bindings**を展開し、**Add binding to this queue**セクションに以下を入力します。
   * **From exchange**: `test_exchange`
   * **Routing key**: `test_routing_key`
   * **Arguments**: 空欄のまま
7. **Bind**ボタンをクリックして、`test_queue`を指定したルーティングキーで`test_exchange`にバインドします。

### メッセージ送信用のキュー作成

RabbitMQ管理Webインターフェースを使って、RabbitMQメッセージ送信用のキューを作成できます。

1. RabbitMQ管理Webインターフェースにログインします。
2. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します。
   * **Type**: `Default for virtual host`
   * **Name**: `message-send`
   * **Durability**: `Durable`を選択し、キューの永続化を設定
   * **Arguments**: 空欄のまま
3. **Add queue**ボタンをクリックしてキューを作成します。新しい`message-send`が**All queues**に表示されます。

## コネクターの作成

本節では、Rabbit Sink/SourceをRabbitMQサーバーに接続するためのコネクター作成方法を説明します。

以下の手順はEMQXとRabbitMQをローカルマシンで実行していることを前提としています。RabbitMQが別環境にある場合は設定を適宜調整してください。

1. ダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. 画面右上の**Create**をクリックします。
3. **Create Connector**ページで**RabbitMQ**を選択し、**Next**をクリックします。
4. コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_rabbitmq`。
5. 接続情報を入力します。
   - **Servers**: `host[:port]`形式のRabbitMQノードをカンマ区切りで入力します。例：`rmq1:5672,rmq2:5672`。1つのノードへの接続が失敗した場合、EMQXはリスト内の次のノードに接続を試みます。異なる接続プールワーカーはリスト内の異なる位置から開始され、接続の分散化に寄与します。

     ::: tip
     EMQX 6.0.4以降、複数のRabbitMQノードを設定可能です。接続確立時にフェイルオーバーが発生しますが、確立済みのAMQP接続はノード間で移行しません。`server`と`port`で単一ノードを指定する既存設定も引き続き互換性があります。
     :::

   - **Port**: **Servers**でポート指定がないノードのデフォルトポート。デフォルトは`5672`。
   - **Username**: `guest`
   - **Password**: `guest`
   - **Virtual Host**: RabbitMQの仮想ホスト。デフォルトは`/`。
   - 暗号化接続を確立したい場合は、**Enable TLS**トグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがRabbitMQサーバーに接続可能かテストできます。
7. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択可能です。**Create Rule**を選ぶと以下の選択肢があります。
   - **Action Outputs**: RabbitMQ Sinkを使ったルール作成で、RabbitMQに転送するデータを指定します。[RabbitMQ Sinkでルールを作成](#create-a-rule-with-rabbitmq-sink)の手順も参照ください。
   - **Data Inputs**: RabbitMQ Sourceを使ったルール作成。[RabbitMQ Sourceでルールを作成](#create-a-rule-with-rabbitmq-source)の手順も参照ください。

## RabbitMQ Sinkでルールを作成する

本節では、ダッシュボード上でソースMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みのRabbitMQキュー`test_queue`に転送するルールの作成方法を説明します。

### SQLを定義してルールを作成する

1. EMQXダッシュボードで、**Integration -> Rules**をクリックします。
2. 画面右上の**Create**をクリックします。
3. ルールIDを入力します。例：`my_rule`。
4. SQLエディターに以下の文を入力します。トピックパターン`t/#`にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT
     payload,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストが可能です。

   :::

5. アクションを追加し、Sinkを設定します。詳細は[ルールにRabbitMQ Sinkを追加する](#add-rabbitmq-sink)を参照してください。
6. アクション追加後、**Action Outputs**セクションに新規Sinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックし、ルール作成を完了します。

これでルールが正常に作成されました。**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブに新しいRabbitMQ Sinkが表示されます。

また、**Integration** -> **Flow Designer**でトポロジーを確認できます。トポロジーはトピック`t/#`のメッセージがルール`my_rule`で解析され、RabbitMQに書き込まれる流れを視覚的に示します。

### RabbitMQ Sinkの追加

本節では、処理結果をRabbitMQに書き込むためのSinkをルールに追加する方法を説明します。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをRabbitMQに送信します。
2. **Type of Action**ドロップダウンから`RabbitMQ`を選択します。**Action**はデフォルトの`Create Action`のままにします。既存のSinkを選択することも可能ですが、ここでは新規Sinkを作成します。
3. Sinkの名前を入力します。大文字・小文字の英数字の組み合わせで入力してください。
4. **Connector**ドロップダウンから`my_rabbitmq`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータの詳細は[コネクターの作成](#create-a-connector)を参照ください。
5. Sinkの設定を以下のように行います。

   * **Exchange**: 事前に作成した`test_exchange`を入力します。メッセージはこのエクスチェンジにパブリッシュされます。

       ::: tip 注意

       RabbitMQにエクスチェンジが作成済みであることを確認してください。存在しない場合、アクションは一時的に動作しなくなり、定期的に再接続を試みます。
       :::

   * **Routing Key**: 事前に作成した`test_routing_key`を入力します。RabbitMQのメッセージパブリッシュ用ルーティングキーです。

       ::: tip

       エクスチェンジとルーティングキーはテンプレート値として設定可能で、プレースホルダーを使いMQTTメッセージペイロードから動的に値を抽出してルーティングできます。

       例として、ペイロード内のフィールドに基づいてルーティングキーを動的に設定する場合、`${payload.akey}`のように設定します。これによりペイロードの`akey`フィールドの値がルーティングキーとして使われます。

       **注意**：バッチモードでは、エクスチェンジとルーティングキーのテンプレート値はバッチ内の全メッセージで一定である必要があります。これによりバッチ処理中のルーティングの一貫性が保たれます。
       :::

   * **Virtual Host**: RabbitMQの仮想ホスト。デフォルトは`/`。
   * **Message Delivery Mode**ドロップダウンから`non_persistent`または`persistent`を選択します。

     * `non_persistent`（デフォルト）：メッセージはディスクに永続化されず、RabbitMQの再起動やクラッシュ時に失われる可能性があります。
     * `persistent`：メッセージはディスクに永続化され、RabbitMQの再起動やクラッシュ時にも耐久性があります。

       ::: tip

       メッセージの損失を防ぐために、キューとエクスチェンジもDurable（永続化）に設定する必要があります。詳細はRabbitMQの[ドキュメント](https://www.rabbitmq.com/documentation.html)を参照してください。

       :::

   * **Wait for Publish Confirmations**: デフォルトで有効。RabbitMQへのメッセージパブリッシュ成功を確認します。

     ::: tip

     このオプションが有効な場合、RabbitMQブローカーはメッセージ受領をアック（ACK）してからパブリッシュ成功とみなすため、メッセージ配信の信頼性が向上します。

     :::

   * **Headers Template**および**Properties Template**: テンプレートを用いてRabbitMQのカスタムヘッダーおよびプロパティを定義できます。詳細は[ヘッダーとプロパティテンプレートの設定](#set-headers-and-properties-templates)を参照してください。
   * **Payload Template**: デフォルトは空文字列で、メッセージペイロードをJSON形式のテキストとしてRabbitMQにそのまま転送します。

     プレースホルダーを用いてカスタムメッセージペイロード形式を定義することも可能です。例えば、MQTTメッセージのペイロードとタイムスタンプを含めたい場合、以下のテンプレートを使用します。

     ```json
      {"payload": "${payload}", "timestamp": ${timestamp}}
     ```

     このテンプレートは、MQTTメッセージのペイロードとタイムスタンプを含むJSON形式のメッセージを生成します。`${payload}`および`${timestamp}`はプレースホルダーで、転送時に実際の値に置き換えられます。

6. **フォールバックアクション（任意）**: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
7. **詳細設定（任意）**:

   - **Publish Confirmation Timeout**: デフォルト30秒。パブリッシュ確認のタイムアウト時間で、ブローカーのアック待ち時間を指定します。
   - 必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがRabbitMQサーバーに接続可能かテストできます。
9. **Create**ボタンをクリックしてSinkの設定を完了します。作成成功後、ルール作成ページに戻り、新規Sinkが**Action Outputs**に追加されます。

#### ヘッダーとプロパティテンプレートの設定

EMQX 6.0以降、RabbitMQ Sinkアクション作成時にカスタムのRabbitMQヘッダーおよびプロパティを定義可能です。これにより、メッセージにメタデータを直接付与し、RabbitMQ内でのメッセージ互換性やルーティングの柔軟性が向上します。

これらのフィールドはルールSQLの結果変数（例：`${payload.device_id}`）をテンプレートとして使用できます。ヘッダーおよびプロパティテンプレートは任意で、空欄の場合は追加メタデータは付与されません。

##### ヘッダーテンプレートの設定方法

RabbitMQヘッダーとして1つ以上のキー・バリューのペアを追加できます。これらはユーザー定義のメタデータで、RabbitMQのコンシューマーが解釈可能です。

- **Key**: ヘッダー名。文字列で指定。
- **Value**: キーに対応する値。静的文字列またはテンプレート変数を使用可能。

例：MQTTペイロードからデバイスIDを含める場合

| Key         | Value                  |
| ----------- | ---------------------- |
| `device_id` | `${payload.device_id}` |

##### プロパティテンプレートの設定方法

RabbitMQは標準のメッセージプロパティセットをサポートします。EMQXはこれらを定義可能で、メッセージレベルのメタデータ（コンテンツタイプや相関IDなど）を付与できます。

- **Key**: 下記の有効なプロパティキーから選択（無効なキーは無視されます）。
- **Value**: 静的値またはテンプレート変数を設定。

有効なプロパティキー一覧：

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

例：コンテンツタイプとアプリIDを指定する場合

| Key            | Value              |
| -------------- | ------------------ |
| `content_type` | `application/json` |
| `app_id`       | `my_iot_app`       |

##### 利用例

MQTTメッセージペイロードが以下の場合：

```json
{
  "device_id": "sensor-123",
  "status": "ok"
}
```

以下を設定したいとします。

- ヘッダーにMQTTペイロード由来の`device_id`
- プロパティに静的値の`app_id`

設定例：

**Headers Template**:

| Key         | Value                  |
| ----------- | ---------------------- |
| `device_id` | `${payload.device_id}` |

**Properties Template**:

| Key      | Value    |
| -------- | -------- |
| `app_id` | `my_app` |

この設定により、RabbitMQに転送されるすべてのメッセージに対して、

- コンシューマーロジック用のカスタムメタデータ（ヘッダー）
- メッセージ処理やデバッグ用の標準メタデータ（プロパティ）

が付与されます。

## RabbitMQ Sinkを使ったルールのテスト

EMQXダッシュボード内蔵のWebSocketクライアントを使い、ルールとSinkの動作をテストできます。

1. ダッシュボード左メニューの**Diagnose** -> **WebSocket Client**をクリックします。
2. 現在のEMQXインスタンスの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を利用可能です。
   - 認証設定を変更している場合は、ユーザー名・パスワードを入力してください。
3. **Connect**をクリックしてクライアントをEMQXに接続します。
4. 下にスクロールしてパブリッシュエリアに以下を入力します。
   * **Topic**: `t/test`
   * **Payload**: `Hello World RabbitMQ from EMQX`
   * **QoS**: `2`
5. **Publish**をクリックしてメッセージを送信します。

   Sinkとルールが正常に作成されていれば、指定したエクスチェンジに指定ルーティングキーでメッセージがパブリッシュされます。

6. http://localhost:15672 のRabbitMQ管理コンソールにアクセスし、**Queues**セクションに移動します。

   ::: tip

   デフォルト設定の場合、ユーザー名・パスワードともに`guest`を使用してください。

   :::

7. メッセージが適切なキューにルーティングされていることを確認します。キューをクリックして詳細を開き、**Get Message(s)**ボタンをクリックするとメッセージ内容を確認できます。

<img src="./assets/rabbitmq/rabbit_mq_management_ui_got_message.png" alt="ブリッジ受信" style="zoom:67%;" />

## RabbitMQ Sourceでルールを作成する

本節では、RabbitMQキューからEMQXへデータを転送するルールの作成方法を説明します。RabbitMQ Sourceとメッセージリパブリッシュアクションの両方を作成し、RabbitMQサービスからメッセージを消費してEMQXに転送します。

1. ダッシュボードの**Integration** -> **Rules**ページに移動します。
2. 画面右上の**Create**をクリックします。
3. ルールIDに`my_rule_source`を入力します。
4. ルールをトリガーするソース（Data Inputs）を設定します。画面右の**Data Inputs**タブをクリックし、デフォルトの`Messages`入力を削除してから、**Add Input**をクリックしRabbitMQ Sourceを作成します。
5. **Add Input**ポップアップで、**Input Type**ドロップダウンから`RabbitMQ`を選択します。**Source**ドロップダウンはデフォルトの`Create Source`のままにします。この例では新規Sourceを作成し、ルールに追加します。
6. Sourceの**Name**と（任意で）**Description**を入力します。名前は大文字・小文字の英数字の組み合わせで、例：`my-rabbitmq-source`。
7. **Connector**ドロップダウンから先ほど作成した`my-rabbitmq`コネクターを選択します。新規コネクター作成はドロップダウン横のボタンから可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。
8. Source情報を設定し、RabbitMQからEMQXへのメッセージ消費設定を完了します。

   - **Queue**: 先にRabbitMQで作成したキュー名`message-send`を入力します。
   - **No Ack**: RabbitMQの`no_ack`モードでメッセージを消費するか選択します。`no_ack`モードを有効にすると、RabbitMQはメッセージをコンシューマーが正常処理する前に即座にキューから削除します。
   - **Wait for Publish Confirmations**: メッセージパブリッシャーのアック待ちを行うか指定します。

9. 詳細設定（任意）：デフォルト値を使用します。
10. **Create**ボタンをクリックしてSource作成を完了し、ルールのデータ入力に追加します。同時にルールSQLは以下のように変更されます。

    ```sql
    SELECT
    *
    FROM
    "$bridges/rabbitmq:my-rabbitmq-source"
    ```

    RabbitMQ Sourceから以下のフィールドにアクセス可能で、SQLを調整してデータ処理が行えます。ここではデフォルトSQLを使用します。

    | フィールド名   | 説明                                                         |
    | :------------- | :------------------------------------------------------------ |
    | payload        | RabbitMQメッセージの内容                                     |
    | event          | イベントトピック。形式は`$bridges/rabbitmq:<source name>`   |
    | metadata       | ルールID情報                                                 |
    | timestamp      | メッセージがEMQXに到着したタイムスタンプ                     |
    | node           | メッセージが到着したEMQXノード名                             |
    | queue          | メッセージを消費したキュー名                                 |
    | exchange       | メッセージがルーティングされたエクスチェンジ                 |
    | routing_key    | エクスチェンジからキューへメッセージをルーティングする際のルーティングキー |

これでRabbitMQ Sourceの作成は完了しましたが、購読したデータは直接EMQXにパブリッシュされません。次に、SourceのメッセージをEMQXに転送するためのメッセージリパブリッシュアクションを作成します。

![rabbitmq_source](./assets/rabbitmq/rabbitmq_source.png)

### ルールにリパブリッシュアクションを追加する

本節では、RabbitMQ Sourceから消費したメッセージをEMQXトピック`t/1`にパブリッシュするためのリパブリッシュアクション追加方法を説明します。

1. 画面右の**Action Output**タブを選択し、**Add Action**ボタンをクリックします。**Type of Action**ドロップダウンから`Republish`アクションを選択します。
2. メッセージリパブリッシュの設定を入力します。

   - **Topic**: MQTTパブリッシュ先トピック。ここでは`t/1`を入力。
   - **QoS**: `0`、`1`、`2`、`${qos}`のいずれかを選択、または他フィールドからQoSを設定するプレースホルダーを入力可能。`${qos}`を選択すると元メッセージのQoSに従います。
   - **Retain**: `true`または`false`を選択。メッセージをリテインメッセージとしてパブリッシュするか決定します。プレースホルダーも利用可能。ここでは`false`を選択。
   - **Payload**: 転送メッセージペイロードのテンプレートを設定。空欄はルール出力結果をそのまま転送。ここでは`${payload}`を入力し、ペイロードのみ転送。
   - **MQTT 5.0 Message Properties**: デフォルトは無効。詳細は[リパブリッシュアクションの追加](./rule-get-started.md#add-republish-action)を参照。

3. **Create**をクリックしてアクション作成を完了します。成功するとルール作成ページに戻り、リパブリッシュアクションが**Action Outputs**タブに追加されます。
4. ルール作成ページで**Create**ボタンをクリックし、ルール全体の作成を完了します。

これでルールが正常に作成されました。**Rules**ページで新規ルールを確認でき、**Sources**タブに新規RabbitMQ Sourceが表示されます。

また、**Integrate** -> **Flow Designer**でトポロジーを確認できます。トポロジーにより、RabbitMQ Sourceからのメッセージがリパブリッシュを経て`t/1`にパブリッシュされる流れを直感的に把握できます。

## RabbitMQ Sourceを使ったルールのテスト

1. [MQTTX CLI](https://mqttx.app/cli)を使い、トピック`t/1`をサブスクライブします。

   ```bash
   mqttx sub -t t/1
   ```

2. 以下のコマンドでRabbitMQにメッセージを送信できます。

   ```bash
   rabbitmqadmin --username=guest --password=guest \
        publish routing_key=message-send \
        payload="{ \"msg\": \"Hello EMQX\"}"
   ```

   - `publish`はメッセージをパブリッシュするコマンドです。
   - `routing_key=message-send`オプションはメッセージのルーティングキーを設定します。この例ではキュー名をルーティングキーとして使用しています。
   - `payload="{ \"msg\": \"Hello EMQX\"}"`オプションはメッセージ内容を設定します。

   または、RabbitMQ管理インターフェースからもメッセージをパブリッシュ可能です。

   1. 上部メニューの**Queues**タブをクリック。
   2. **Name**列の`message-send`をクリックし詳細ページを開く。
   3. **Publish message**を展開し、**Payload**ボックスに`"Hello EMQX"`を入力し、**Publish message**ボタンをクリック。

3. MQTTXで以下のような出力を確認できます。

   ```bash
   [2024-2-23] [16:59:28] › payload: {"payload":{"msg":"Hello EMQX"},"event":"$bridges/rabbitmq:my-rabbitmq-source","metadata":{"rule_id":"rule_0ly1"},"timestamp":1708678768449,"node":"emqx@127.0.0.1"}
   ```
