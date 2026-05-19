# RabbitMQへのMQTTデータ取り込み

[RabbitMQ](https://www.rabbitmq.com/)は、Advanced Message Queuing Protocol（AMQP）を実装した広く利用されているオープンソースのメッセージブローカーです。分散システム間のメッセージングにおいて堅牢でスケーラブルなプラットフォームを提供します。EMQXはRabbitMQとの統合をサポートしており、MQTTメッセージやイベントをRabbitMQへ転送できます。また、RabbitMQサーバーからデータを取得してEMQXの特定トピックにパブリッシュすることも可能で、RabbitMQからMQTTへのメッセージ配信を実現します。

本ページでは、EMQXとRabbitMQ間のデータ統合について、実践的な作成および検証手順を含めて詳細に解説します。

## 動作概要

RabbitMQデータ統合は、MQTTベースのIoTデータとRabbitMQの強力なメッセージキュー処理機能をつなぐためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからRabbitMQへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

RabbitMQ Sinkを例に、以下の図はEMQXとRabbitMQ間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration RabbitMQ](./assets/emqx-integration-rabbitmq.png)

MQTTデータをRabbitMQに取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを通じてEMQXに接続し、リアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージ到着時にルールエンジンを通過し、EMQXで定義されたルールに基づいて処理されます。ルールは事前定義された条件によりRabbitMQへルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データフォーマットの変換や特定情報のフィルタリング、ペイロードの付加情報による強化などが適用されます。
3. **RabbitMQへのメッセージ取り込み**：ルール処理が完了すると、RabbitMQへメッセージを転送するアクションがトリガーされます。処理済みメッセージはシームレスにRabbitMQへ書き込まれます。
4. **データの永続化と活用**：RabbitMQはメッセージをキューに保存し、適切なコンシューマーへ配信します。これらのメッセージは他のアプリケーションやサービスで消費され、データ分析、可視化、保存などのさらなる処理に利用されます。

## 特長と利点

RabbitMQとのデータ統合は以下の特長とメリットをもたらします：

- **信頼性の高いIoTデータメッセージ配信**：EMQXはデバイスからクラウドへの信頼性の高い接続とメッセージ配信を保証し、RabbitMQはメッセージの永続化とサービス間の確実な配信を担い、全体のデータ信頼性を確保します。
- **MQTTメッセージの変換**：ルールエンジンを用いてMQTTメッセージの抽出、フィルタリング、強化、変換が可能で、RabbitMQへ送信する前に柔軟なメッセージ加工が行えます。
- **柔軟なメッセージマッピング**：RabbitMQデータ統合はMQTTトピックとRabbitMQのルーティングキーおよびエクスチェンジの柔軟なマッピングをサポートし、MQTTとRabbitMQ間のシームレスな連携を実現します。
- **高可用性とクラスター対応**：EMQXとRabbitMQは共に高可用なメッセージブローカークラスターの構築をサポートし、ノード障害時もサービス継続が可能です。クラスター機能を活用することで優れたスケーラビリティも提供します。
- **高スループット環境での処理能力**：RabbitMQデータ統合は同期・非同期の両方の書き込みモードをサポートし、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。

## はじめる前に

このセクションでは、RabbitMQデータ統合の作成前に必要な準備について説明します。RabbitMQサーバーの起動やテスト用のエクスチェンジ・キューの作成方法を含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)および[リパブリッシュアクション](./rule-get-started.md#add-republish-action)に関する知識
- UNIXターミナルとコマンドの基本知識

### RabbitMQサーバーの起動

[Docker](https://www.docker.com/)を使ったRabbitMQサーバーの起動方法を紹介します。

以下のコマンドを実行すると、管理プラグインを有効にしたRabbitMQサーバーが起動します。管理プラグインによりWebインターフェースでRabbitMQを監視できます。

```bash
docker run -it --rm --name rabbitmq -p 127.0.0.1:5672:5672 -p 127.0.0.1:15672:15672 rabbitmq:3.11-management
```

詳細は[Docker HubのRabbitMQページ](https://hub.docker.com/_/rabbitmq)をご参照ください。

### メッセージ受信用のエクスチェンジとキューの作成

RabbitMQサーバー起動後、RabbitMQ管理Webインターフェースを使って、EMQXから転送されるメッセージ受信用のテスト用エクスチェンジとキューを作成します。既にテスト用のエクスチェンジとキューがある場合はこのセクションをスキップしてください。

1. ブラウザで http://localhost:15672/ にアクセスし、RabbitMQ管理Webインターフェースを開きます。ログイン画面で以下のデフォルト認証情報を入力し、**Login**をクリックします。
   - **Username**: `guest`
   - **Password**: `guest`
2. 上部メニューの**Exchanges**タブをクリックし、**Add a new exchange**を展開して以下を入力します：
   * **Name**: `test_exchange`
   * **Type**: ドロップダウンから`direct`を選択
   * **Durability**: `Durable`を選択（RabbitMQ再起動後もエクスチェンジが持続）
   * **Auto delete**: `No`
   * **Internal**: `No`
   * **Arguments**: 空欄のまま
3. **Add exchange**ボタンをクリックしてエクスチェンジを作成します。
4. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します：
   * **Type**: `Default for virtual host`
   * **Name**: `test_queue`
   * **Durability**: `Durable`を選択（キューを永続化）
   * **Arguments**: 空欄のまま
5. **Add queue**ボタンをクリックしてキューを作成します。新しい`test_queue`が**All queues**に表示されます。
6. キュー名`test_queue`をクリックして詳細ページを開き、**Bindings**を展開します。**Add binding to this queue**セクションで以下を入力します：
   * **From exchange**: `test_exchange`
   * **Routing key**: `test_routing_key`
   * **Arguments**: 空欄のまま
7. **Bind**ボタンをクリックして、`test_queue`を`test_exchange`に指定したルーティングキーでバインドします。

### メッセージパブリッシュ用のキュー作成

RabbitMQ管理Webインターフェースを使って、RabbitMQメッセージのパブリッシュ用キューを作成します。

1. RabbitMQ管理Webインターフェースにログインします。
2. **Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します：
   * **Type**: `Default for virtual host`
   * **Name**: `message-send`
   * **Durability**: `Durable`を選択（キューを永続化）
   * **Arguments**: 空欄のまま
3. **Add queue**ボタンをクリックしてキューを作成します。新しい`message-send`が**All queues**に表示されます。

## コネクターの作成

このセクションでは、Rabbit Sink/SourceをRabbitMQサーバーに接続するためのコネクター作成方法を説明します。

以下の手順はEMQXとRabbitMQをローカルマシンで動作させていることを前提としています。RabbitMQが別の場所にある場合は設定を適宜調整してください。

1. ダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. 画面右上の**Create**をクリックします。
3. **Create Connector**ページで**RabbitMQ**を選択し、**Next**をクリックします。
4. コネクター名を入力します。英数字の組み合わせで、例：`my_rabbitmq`。
5. 接続情報を入力します。
   - **Server**: RabbitMQサーバーがローカルの場合は`localhost`、リモートの場合は実際のホスト名/IPを入力。
   - **Port**: 通常は`5672`、異なる場合は適宜入力。
   - **Username**: `guest`
   - **Password**: `guest`
   - **Virtual Host**: RabbitMQの仮想ホスト、デフォルトは`/`
   - 暗号化接続を行う場合は**Enable TLS**をオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**でRabbitMQサーバーへの接続確認ができます。
7. **Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択できます。**Create Rule**を選ぶと以下のオプションがあります：
   - **Action Outputs**：RabbitMQ Sinkを用いたルール作成。詳細は[Create a Rule with RabbitMQ Sink](#create-a-rule-with-rabbitmq-sink)を参照。
   - **Data Inputs**：RabbitMQ Sourceを用いたルール作成。詳細は[Create a Rule with RabbitMQ Source](#create-a-rule-with-rabbitmq-source)を参照。

## RabbitMQ Sinkを使ったルールの作成

このセクションでは、ソースMQTTトピック`t/#`からのメッセージを処理し、処理済みデータをRabbitMQのキュー`test_queue`へ転送するルールをダッシュボードで作成する方法を説明します。

### SQLを定義したルール作成

1. EMQXダッシュボードで、**Integration -> Rules**をクリックします。
2. 画面右上の**Create**をクリックします。
3. ルールIDを入力します。例：`my_rule`
4. SQLエディタに以下の文を入力します。`t/#`トピックパターンにマッチするMQTTメッセージを転送します。

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

5. ルールにアクションを追加し、Sinkを設定します。詳細は[Add RabbitMQ Sink to the Rule](#add-rabbitmq-sink-to-the-rule)を参照してください。
6. アクション追加後、**Action Outputs**セクションに新しいSinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックしてルール作成を完了します。

これでルールが作成されました。**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブで新しいRabbitMQ Sinkも確認できます。

また、**Integration** -> **Flow Designer**でトポロジーを確認できます。トポロジーはトピック`t/#`のメッセージがルール`my_rule`で解析されRabbitMQに書き込まれる流れを視覚的に示します。

### RabbitMQ Sinkの追加

このセクションでは、処理結果をRabbitMQに書き込むためにルールにSinkを追加する方法を説明します。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールでトリガーされるアクションを定義します。このアクションによりEMQXはルールで処理したデータをRabbitMQへ送信します。
2. **Type of Action**ドロップダウンリストから`RabbitMQ`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既存のSinkを選択することも可能ですが、ここでは新規作成します。
3. Sinkの名前を入力します。英数字の組み合わせで入力してください。
4. **Connector**ドロップダウンから先ほど作成した`my_rabbitmq`を選択します。新規作成する場合はドロップダウン横のボタンをクリックして作成します。設定パラメータは[Create a Connector](#create-a-connector)を参照してください。
5. Sinkの設定を以下のように行います：

   * **Exchange**：前に作成した`test_exchange`を入力します。ここにメッセージがパブリッシュされます。

       ::: tip 注意

       RabbitMQでエクスチェンジが作成済みであることを確認してください。未作成の場合、アクションは一時的に失敗し、定期的に再接続を試みます。

       :::

   * **Routing Key**：前に作成した`test_routing_key`を入力します。RabbitMQでのメッセージパブリッシュに使うルーティングキーです。

       ::: tip

       エクスチェンジとルーティングキーはテンプレート値として設定可能で、プレースホルダーを使い受信したMQTTメッセージのペイロードから動的に値を抽出してルーティングできます。

       例：ペイロード内のフィールド`akey`に基づきルーティングキーを動的設定したい場合、`${payload.akey}`と設定します。これによりペイロードの`akey`フィールドの値がルーティングキーとして使われます。

       **注意**：バッチモードではエクスチェンジとルーティングキーのテンプレート値はバッチ内の全メッセージで一定である必要があります。これにより一貫したルーティングが保証され、バッチ処理時の競合を防ぎます。

       :::

   * **Virtual Host**：RabbitMQの仮想ホスト。デフォルトは`/`です。

   * **Message Delivery Mode**ドロップダウンで`non_persistent`または`persistent`を選択します：

     * `non_persistent`（デフォルト）：メッセージはディスクに永続化されず、RabbitMQの再起動やクラッシュ時に失われる可能性があります。

     * `persistent`：メッセージはディスクに永続化され、RabbitMQの再起動やクラッシュ時にも耐久性があります。

       ::: tip

       メッセージの損失を防ぐために、キューやエクスチェンジもDurableに設定する必要があります。詳細はRabbitMQの[ドキュメント](https://www.rabbitmq.com/documentation.html)を参照してください。

       :::

   * **Wait for Publish Confirmations**：デフォルトで有効。RabbitMQへのメッセージパブリッシュ成功を確認します。

     ::: tip

     このオプションを有効にすると、RabbitMQブローカーがメッセージ受信をアック（ACK）してから成功とみなすため、メッセージ配信の信頼性が向上します。

     :::

   * **Headers Template**および**Properties Template**：テンプレートを使ってRabbitMQのカスタムHeadersおよびPropertiesを定義できます。詳細は[Set Headers and Properties Templates](#set-headers-and-properties-templates)を参照してください。

   * **Payload Template**：デフォルトは空文字列で、メッセージペイロードはJSON形式のテキストとしてRabbitMQにそのまま転送されます。

     プレースホルダーを使って動的にMQTTメッセージのデータを含むカスタムペイロードフォーマットを定義することも可能です。例えば、MQTTメッセージのペイロードとタイムスタンプを含めたい場合は以下のテンプレートを使います：

     ```json
      {"payload": "${payload}", "timestamp": ${timestamp}}
     ```

     このテンプレートは、受信したMQTTメッセージのペイロードとタイムスタンプを含むJSON形式のメッセージを生成します。`${payload}`と`${timestamp}`はプレースホルダーで、実際の値に置き換えられます。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。
7. **詳細設定（任意）**：

   - **Publish Confirmation Timeout**：デフォルト30秒。パブリッシュ確認のタイムアウト時間で、ブローカーのアックを待つ最大時間です。
   - 必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Features of Sink](./data-bridges.md#features-of-sink)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**でSinkがRabbitMQサーバーに接続できるかテストできます。
9. **Create**ボタンをクリックしてSink設定を完了します。作成成功後、ルール作成ページに戻り、新しいSinkが**Action Outputs**に追加されます。

#### HeadersおよびPropertiesテンプレートの設定

EMQX 6.0以降、RabbitMQ Sinkアクション作成時にカスタムRabbitMQ HeadersおよびPropertiesを定義可能です。これにより、メッセージにメタデータを直接付与し、RabbitMQ内でのメッセージ互換性やルーティングの柔軟性を向上させます。

これらのフィールドはルールSQLの変数（例：`${payload.device_id}`）を使ったテンプレートで設定できます。HeadersおよびPropertiesのテンプレートは任意で、空欄の場合はメッセージに追加メタデータは付与されません。

##### Headersテンプレートの設定方法

RabbitMQ Headersとして1つ以上のキー・バリューを追加できます。これらはユーザー定義のメタデータで、RabbitMQコンシューマーで解釈されます。

- **Key**：ヘッダー名。文字列で指定。
- **Value**：キーに対応する値。静的文字列またはテンプレート変数を使用可能。

例：MQTTペイロードのデバイスIDを含める場合

| Key         | Value                  |
| ----------- | ---------------------- |
| `device_id` | `${payload.device_id}` |

##### Propertiesテンプレートの設定方法

RabbitMQは標準のメッセージプロパティセットをサポートしています。EMQXではこれらを定義し、コンテンツタイプや相関IDなどのメッセージレベルのメタデータを付与できます。

- **Key**：以下の有効なプロパティキーから選択（無効なキーは無視されます）
- **Value**：静的値またはテンプレート変数

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

##### 利用例

MQTTメッセージペイロードが以下の場合：

```json
{
  "device_id": "sensor-123",
  "status": "ok"
}
```

以下を設定したいとします：

- ヘッダーにMQTTペイロードの`device_id`
- プロパティに静的値の`app_id`

設定例：

**Headersテンプレート**：

| Key         | Value                  |
| ----------- | ---------------------- |
| `device_id` | `${payload.device_id}` |

**Propertiesテンプレート**：

| Key      | Value    |
| -------- | -------- |
| `app_id` | `my_app` |

この設定により、RabbitMQに転送される全メッセージに対して以下が付与されます：

- コンシューマーロジック用のカスタムメタデータ（Headers）
- メッセージ処理やデバッグ用の標準メタデータ（Properties）

## RabbitMQ Sinkを使ったルールのテスト

EMQXダッシュボード内蔵のWebSocketクライアントを使い、ルールとSinkの動作をテストできます。

1. ダッシュボード左メニューの**Diagnose** -> **WebSocket Client**をクリックします。
2. 現在のEMQXインスタンスへの接続情報を入力します。
   - ローカルでEMQXを動かしている場合はデフォルト値を使えます。
   - 認証設定を変更している場合はユーザー名やパスワードを入力してください。
3. **Connect**をクリックしてEMQXに接続します。
4. ページ下部のパブリッシュエリアに以下を入力します：
   * **Topic**: `t/test`
   * **Payload**: `Hello World RabbitMQ from EMQX`
   * **QoS**: `2`
5. **Publish**をクリックしてメッセージを送信します。

   Sinkとルールが正しく作成されていれば、指定したエクスチェンジに指定ルーティングキーでメッセージがパブリッシュされているはずです。

6. http://localhost:15672 のRabbitMQ管理コンソールにアクセスし、**Queues**セクションに移動します。

   ::: tip

   デフォルト設定を変更していなければ、ユーザー名・パスワードともに`guest`を使用してください。

   :::

7. メッセージが適切なキューにルーティングされていることを確認します。キューをクリックし、**Get Message(s)**ボタンを押すと詳細なメッセージ内容が表示されます。

<img src="./assets/rabbitmq/rabbit_mq_management_ui_got_message.png" alt="bridge_igress" style="zoom:67%;" />

## RabbitMQ Sourceを使ったルールの作成

このセクションでは、RabbitMQキューからEMQXへデータを転送するルール作成方法を説明します。RabbitMQ Sourceとメッセージリパブリッシュアクションの両方を作成し、RabbitMQサービスからメッセージを消費してEMQXへ転送します。

1. ダッシュボードの**Integration** -> **Rules**ページに移動します。
2. 画面右上の**Create**をクリックします。
3. ルールIDに`my_rule_source`を入力します。
4. ルールをトリガーするソース（Data Inputs）を設定します。右側の**Data Inputs**タブをクリックし、デフォルトの`Messages`入力を削除してから**Add Input**をクリックし、RabbitMQ Sourceを作成します。
5. **Add Input**ポップアップで、**Input Type**ドロップダウンから`RabbitMQ`を選択します。**Source**ドロップダウンはデフォルトの`Create Source`のままにします。この例では新規Sourceを作成しルールに追加します。
6. Sourceの**Name**と（任意で）**Description**を入力します。名前は英数字の組み合わせで、例：`my-rabbitmq-source`。
7. **Connector**ドロップダウンから先ほど作成した`my-rabbitmq`を選択します。新規作成する場合はドロップダウン横のボタンでポップアップから作成可能です。設定パラメータは[Create a Connector](#create-a-connector)を参照してください。
8. RabbitMQからEMQXへメッセージを消費するためのSource情報を設定します：

   - **Queue**：RabbitMQで作成した`message-send`キュー名を入力。
   - **No Ack**：RabbitMQの`no_ack`モードでメッセージを消費するか選択。`no_ack`有効時はRabbitMQがメッセージのアックを待たず即座にキューから削除します。
   - **Wait for Publish Confirmations**：メッセージパブリッシャーのアックを待つか指定します。

9. 詳細設定（任意）：デフォルト値を使用します。
10. **Create**ボタンをクリックしてSource作成を完了し、ルールのデータ入力に追加します。同時にルールSQLは以下のように変更されます：

    ```sql
    SELECT
    *
    FROM
    "$bridges/rabbitmq:my-rabbitmq-source"
    ```

    ルールSQLはRabbitMQ Sourceから以下のフィールドにアクセスでき、データ処理のためにSQLを調整可能です。ここではデフォルトSQLを使用します。

    | フィールド名 | 説明                                                         |
    | :----------- | :----------------------------------------------------------- |
    | payload      | RabbitMQメッセージの内容                                     |
    | event        | イベントトピック。形式は`$bridges/rabbitmq:<source name>`   |
    | metadata     | ルールID情報                                                 |
    | timestamp    | メッセージがEMQXに到着したタイムスタンプ                     |
    | node         | メッセージが到着したEMQXノード名                             |
    | queue        | メッセージを消費したキュー名                                 |
    | exchange     | メッセージがルーティングされたエクスチェンジ                 |
    | routing_key  | エクスチェンジからキューへメッセージをルーティングするためのルーティングキー |

ここまででRabbitMQ Sourceの作成は完了しましたが、購読したデータはまだEMQXに直接パブリッシュされません。続いてメッセージリパブリッシュアクションを作成し、SourceのメッセージをEMQXへ転送します。

![rabbitmq_source](./assets/rabbitmq/rabbitmq_source.png)

### ルールへのリパブリッシュアクション追加

このセクションでは、RabbitMQ Sourceから消費したメッセージをEMQXトピック`t/1`にパブリッシュするためのリパブリッシュアクションをルールに追加する方法を説明します。

1. 画面右側の**Action Output**タブを選択し、**Add Action**ボタンをクリックします。**Type of Action**ドロップダウンから`Republish`アクションを選択します。
2. メッセージリパブリッシュ設定を入力します：

   - **Topic**：MQTTにパブリッシュするトピック。ここでは`t/1`を入力。
   - **QoS**：`0`、`1`、`2`、`${qos}`のいずれかを選択、または他のフィールドからQoSを設定するためのプレースホルダーを入力可能。`${qos}`を選ぶと元メッセージのQoSに従います。
   - **Retain**：`true`または`false`を選択。メッセージをリテインメッセージとしてパブリッシュするかを決定。プレースホルダーも使用可能。ここでは`false`を選択。
   - **Payload**：転送するメッセージペイロードのテンプレート。空欄はルール出力結果をそのまま転送。ここでは`${payload}`を入力しペイロードのみ転送。
   - **MQTT 5.0 Message Properties**：デフォルトで無効。詳細は[Add Republish Action](./rule-get-started.md#add-republish-action)を参照。
3. **Create**をクリックしてアクション作成を完了します。作成成功後、ルール作成ページに戻り、リパブリッシュアクションが**Action Outputs**タブに追加されます。
4. ルール作成ページで**Create**ボタンを押し、ルール全体の作成を完了します。

これでルールが作成されました。**Rules**ページで新規ルールを確認でき、**Sources**タブで新しいRabbitMQ Sourceも確認できます。

また、**Integrate** -> **Flow Designer**でトポロジーを確認できます。トポロジーにより、RabbitMQ Sourceからのメッセージがリパブリッシュを経てトピック`t/1`にパブリッシュされる流れを直感的に把握できます。

## RabbitMQ Sourceを使ったルールのテスト

1. [MQTTX CLI](https://mqttx.app/cli)を使い、トピック`t/1`をサブスクライブします：

   ```bash
   mqttx sub -t t/1
   ```

2. 以下のコマンドでRabbitMQにメッセージをパブリッシュできます：

   ```bash
   rabbitmqadmin --username=guest --password=guest \
        publish routing_key=message-send \
        payload="{ \"msg\": \"Hello EMQX\"}"
   ```

   - `publish`はメッセージをパブリッシュするコマンドです。
   - `routing_key=message-send`はメッセージのルーティングキーを設定。ここではキュー名をルーティングキーとして使用。
   - `payload="{ \"msg\": \"Hello EMQX\"}"`はメッセージ内容を設定。

   またはRabbitMQ管理インターフェースからもメッセージをパブリッシュ可能です：

   1. 上部メニューの**Queues**タブをクリック。
   2. **Name**欄の`message-send`をクリックして詳細ページを開く。
   3. **Publish message**を展開し、**Payload**欄に`"Hello EMQX"`を入力し、**Publish message**ボタンをクリック。

3. MQTTXで以下のような出力が表示されます：

   ```bash
   [2024-2-23] [16:59:28] › payload: {"payload":{"msg":"Hello EMQX"},"event":"$bridges/rabbitmq:my-rabbitmq-source","metadata":{"rule_id":"rule_0ly1"},"timestamp":1708678768449,"node":"emqx@127.0.0.1"}
   ```
