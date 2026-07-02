# RabbitMQへのMQTTデータ取り込み

[RabbitMQ](https://www.rabbitmq.com/)は、Advanced Message Queuing Protocol（AMQP）を実装した広く利用されているオープンソースのメッセージブローカーです。分散システム間のメッセージングにおいて堅牢かつスケーラブルなプラットフォームを提供します。EMQXはRabbitMQとの統合をサポートしており、MQTTメッセージやイベントをRabbitMQに転送できます。また、RabbitMQサーバーからデータを取得してEMQXの特定トピックにパブリッシュすることも可能で、RabbitMQからMQTTへのメッセージ配信を実現します。

本ページでは、EMQXとRabbitMQ間のデータ統合について詳細に解説し、実際の作成および検証手順を紹介します。

## 動作概要

RabbitMQデータ統合は、MQTTベースのIoTデータとRabbitMQの強力なメッセージキュー処理機能を橋渡しするためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからRabbitMQへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

RabbitMQ Sinkを例にとると、以下の図はEMQXとRabbitMQ間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration RabbitMQ](./assets/emqx-integration-rabbitmq.png)

MQTTデータをRabbitMQに取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、リアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXに定義されたルールで処理されます。ルールは事前定義された条件に基づき、RabbitMQへルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、ペイロードの付加的なコンテキストによる強化などが適用されます。
3. **RabbitMQへのメッセージ取り込み**：ルールによる処理が完了すると、メッセージをRabbitMQに転送するアクションがトリガーされます。処理済みのメッセージはシームレスにRabbitMQに書き込まれます。
4. **データの永続化と活用**：RabbitMQはメッセージをキューに保存し、適切なコンシューマーに配信します。メッセージは他のアプリケーションやサービスによって消費され、データ分析、可視化、保存などのさらなる処理に利用されます。

## 特長と利点

RabbitMQとのデータ統合は、以下の特長とメリットをビジネスにもたらします：

- **信頼性の高いIoTデータメッセージ配信**：EMQXはデバイスからクラウドへの信頼性の高い接続とメッセージ配信を保証し、RabbitMQはメッセージの永続化と異なるサービス間での信頼性の高い配信を担い、全体のデータ信頼性を確保します。
- **MQTTメッセージの変換機能**：ルールエンジンを用いて、EMQXはMQTTメッセージの抽出、フィルタリング、強化、変換を行い、RabbitMQへ送信します。
- **柔軟なメッセージマッピング**：RabbitMQデータ統合はMQTTトピックとRabbitMQのルーティングキーおよびエクスチェンジの柔軟なマッピングをサポートし、MQTTとRabbitMQ間のシームレスな連携を可能にします。
- **高可用性およびクラスター対応**：EMQXとRabbitMQは共に高可用なメッセージブローカークラスターの構築をサポートし、ノード障害時でもサービス継続を保証します。クラスター機能により優れたスケーラビリティも実現します。
- **高スループットシナリオでの処理能力**：RabbitMQデータ統合は同期・非同期の書き込みモードをサポートし、レイテンシとスループットのバランスを柔軟に調整可能です。

## はじめる前に

このセクションでは、RabbitMQデータ統合を作成する前に必要な準備として、RabbitMQサーバーの起動方法およびテスト用のExchangeとQueueの作成方法を説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)および[リパブリッシュアクション](./rule-get-started.md#add-republish-action)に関する知識
- UNIXターミナルおよびコマンドの基本知識

### RabbitMQサーバーの起動

ここでは[Docker](https://www.docker.com/)を使ったRabbitMQサーバーの起動方法を紹介します。

以下のコマンドを実行して、管理プラグインを有効にしたRabbitMQサーバーを起動します。管理プラグインによりWebインターフェースでRabbitMQを確認できます。

```bash
docker run -it --rm --name rabbitmq -p 127.0.0.1:5672:5672 -p 127.0.0.1:15672:15672 rabbitmq:3.11-management
```

Docker Hubの[RabbitMQのDocker実行に関する情報](https://hub.docker.com/_/rabbitmq)も参照してください。

### メッセージ受信用のExchangeとQueueの作成

RabbitMQサーバー起動後、RabbitMQ管理Webインターフェースを使って、EMQXから転送されるメッセージ受信用のテスト用ExchangeとQueueを作成します。すでにExchangeとQueueがある場合はこのセクションをスキップ可能です。

1. ブラウザで http://localhost:15672/ にアクセスし、RabbitMQ管理Webインターフェースを開きます。ログイン画面でデフォルトの認証情報を入力し、**Login**をクリックします。
   - **Username**: `guest`
   - **Password**: `guest`
2. 上部メニューの**Exchanges**タブをクリックし、**Add a new exchange**を展開して以下を入力します：
   * **Name**: `test_exchange`
   * **Type**: ドロップダウンから`direct`を選択
   * **Durability**: `Durable`を選択し、RabbitMQ再起動後もExchangeが持続するように設定
   * **Auto delete**: `No`
   * **Internal**: `No`
   * **Arguments**: 空欄のまま
3. **Add exchange**ボタンをクリックしてExchangeを作成します。
4. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します：
   * **Type**: `Default for virtual host`
   * **Name**: `test_queue`
   * **Durability**: `Durable`を選択し、Queueを永続化
   * **Arguments**: 空欄のまま
5. **Add queue**ボタンをクリックしてQueueを作成します。新しい`test_queue`が**All queues**セクションに表示されます。
6. `test_queue`をクリックして詳細ページを開き、**Bindings**を展開します。**Add binding to this queue**セクションで以下を入力します：
   * **From exchange**: `test_exchange`
   * **Routing key**: `test_routing_key`
   * **Arguments**: 空欄のまま
7. **Bind**ボタンをクリックして、`test_queue`を`test_exchange`に指定ルーティングキーでバインドします。

### メッセージ送信用のQueueの作成

RabbitMQ管理Webインターフェースを使って、RabbitMQメッセージ送信用のQueueを作成します。

1. RabbitMQ管理Webインターフェースにログインします。
2. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します：
   * **Type**: `Default for virtual host`
   * **Name**: `message-send`
   * **Durability**: `Durable`を選択し、Queueを永続化
   * **Arguments**: 空欄のまま
3. **Add queue**ボタンをクリックしてQueueを作成します。新しい`message-send`が**All queues**セクションに表示されます。

## コネクターの作成

このセクションでは、Rabbit Sink/SourceをRabbitMQサーバーに接続するためのコネクターの作成方法を説明します。

以下の手順はEMQXとRabbitMQをローカルマシンで実行していることを前提としています。RabbitMQが別の場所にデプロイされている場合は設定を適宜調整してください。

1. ダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. 画面右上の**Create**をクリックします。
3. **Create Connector**ページで**RabbitMQ**を選択し、**Next**をクリックします。
4. コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_rabbitmq`。
5. 接続情報を入力します。
   - **Server**: RabbitMQサーバーがローカルなら`localhost`、リモートなら実際のホスト名/IPを入力。
   - **Port**: 通常は`5672`、異なる場合は実際のポート番号を入力。
   - **Username**: `guest`
   - **Password**: `guest`
   - **Virtual Host**: RabbitMQの仮想ホスト。デフォルトは`/`。
   - 暗号化接続を行う場合は**Enable TLS**をオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがRabbitMQサーバーに接続できるかテストできます。
7. **Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択可能です。**Create Rule**を選ぶと以下のオプションがあります：
   - **Action Outputs**：RabbitMQ Sinkを使ったルール作成でRabbitMQへの転送データを指定します。[RabbitMQ Sinkでルールを作成](#create-a-rule-with-rabbitmq-sink)の手順も参照ください。
   - **Data Inputs**：RabbitMQ Sourceを使ったルール作成です。[RabbitMQ Sourceでルールを作成](#create-a-rule-with-rabbitmq-source)の手順も参照ください。

## RabbitMQ Sinkを使ったルールの作成

このセクションでは、ダッシュボード上でソースMQTTトピック`t/#`からのメッセージを処理し、処理結果をRabbitMQの`test_queue`に転送するルールを作成する方法を説明します。

### SQLを定義したルールの作成

1. EMQXダッシュボードで、**Integration -> Rules**をクリックします。
2. 画面右上の**Create**をクリックします。
3. ルールIDを入力します。例：`my_rule`。
4. SQLエディターに以下のステートメントを入力します。これはトピックパターン`t/#`にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT
     payload,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストできます。

   :::

5. ルールにアクションを追加し、Sinkを設定します。詳細は[ルールにRabbitMQ Sinkを追加](#add-rabbitmq-sink)を参照してください。
6. アクション追加後、**Action Outputs**セクションに新しいSinkが表示されます。**Create Rule**ページの**Save**ボタンをクリックしてルール作成を完了します。

これでルールの作成が完了しました。**Rules**ページで新しいルールを確認でき、**Actions (Sink)**タブで新しいRabbitMQ Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを視覚的に確認できます。トポロジーは、トピック`t/#`のメッセージがルール`my_rule`で解析された後、RabbitMQに書き込まれる流れを示します。

### RabbitMQ Sinkの追加

このセクションでは、ルールにSinkを追加して処理結果をRabbitMQに書き込む方法を説明します。

1. **Create Rule**ページの**Action Outputs**セクションで**Add Action**をクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをRabbitMQに送信します。
2. **Type of Action**ドロップダウンから`RabbitMQ`を選択します。**Action**はデフォルトの`Create Action`のままにします。すでに作成済みのSinkがあれば選択可能ですが、ここでは新規作成します。
3. Sinkの名前を入力します。大文字・小文字の英数字の組み合わせで指定してください。
4. **Connector**ドロップダウンから先に作成した`my_rabbitmq`を選択します。新規作成する場合はドロップダウン横のボタンをクリックして作成可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。
5. Sinkの設定を以下のように行います：

   * **Exchange**：事前に作成した`test_exchange`を入力します。メッセージはこのエクスチェンジにパブリッシュされます。

       ::: tip 注意

       ExchangeがRabbitMQに作成済みであることを確認してください。存在しない場合、アクションは一時的に失敗し、接続再試行を繰り返します。

       :::

   * **Routing Key**：事前に作成した`test_routing_key`を入力します。RabbitMQのメッセージパブリッシュ用ルーティングキーです。

       ::: tip

       Exchangeとルーティングキーはテンプレート値として設定可能で、プレースホルダーを使い受信したMQTTメッセージのペイロードから動的に値を抽出してルーティングできます。

       例：ペイロードのフィールド`akey`に基づいてルーティングキーを動的に設定する場合、`${payload.akey}`と指定します。これによりペイロードの`akey`の値がルーティングキーとして使われます。

       **注意**：バッチモードでは、Exchangeとルーティングキーのテンプレート値はバッチ内の全メッセージで一定でなければなりません。これにより一貫したルーティングが保証され、バッチ処理時の競合を防止します。

       :::

   * **Virtual Host**：RabbitMQの仮想ホスト。デフォルトは`/`。
   * **Message Delivery Mode**ドロップダウンで`non_persistent`または`persistent`を選択します：

     * `non_persistent`（デフォルト）：メッセージはディスクに永続化されず、RabbitMQの再起動やクラッシュ時に失われる可能性があります。

     * `persistent`：メッセージはディスクに永続化され、RabbitMQの再起動やクラッシュ時にも耐久性があります。

       ::: tip

       メッセージ損失を防ぐため、QueueとExchangeもDurableに設定する必要があります。詳細はRabbitMQの[ドキュメント](https://www.rabbitmq.com/documentation.html)を参照してください。

       :::

   * **Wait for Publish Confirmations**：デフォルトで有効。RabbitMQへのメッセージパブリッシュ成功を確認します。

     ::: tip

     このオプションを有効にすると、RabbitMQブローカーはメッセージ受領をアック（ACK）してから成功とみなすため、メッセージ配信の信頼性が向上します。

     :::

   * **Headers Template**および**Properties Template**：テンプレートを使ってRabbitMQのカスタムヘッダーおよびプロパティを定義可能です。詳細は[ヘッダーとプロパティテンプレートの設定](#set-headers-and-properties-templates)を参照してください。
   * **Payload Template**：デフォルトは空文字列で、メッセージペイロードをJSON形式のテキストとしてRabbitMQにそのまま転送します。

     プレースホルダーを使い、受信したMQTTメッセージのデータを動的に含めるカスタムペイロード形式も定義可能です。例えば、MQTTメッセージのペイロードとタイムスタンプを含めたい場合、以下のテンプレートを使用します：

     ```json
      {"payload": "${payload}", "timestamp": ${timestamp}}
     ```

     このテンプレートは、受信したMQTTメッセージのペイロードとタイムスタンプを含むJSON形式のメッセージを生成します。`${payload}`と`${timestamp}`はプレースホルダーで、実際のメッセージ値に置き換えられます。

6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
7. **詳細設定（任意）**：

   - **Publish Confirmation Timeout**：デフォルトは30秒。パブリッシュ確認のタイムアウト時間です。
   - 必要に応じて**sync**または**async**クエリモードを選択可能です。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがRabbitMQサーバーに接続できるかテストできます。
9. **Create**ボタンをクリックしてSinkの設定を完了します。作成成功後、ルール作成ページに戻り、新しいSinkが**Action Outputs**に追加されます。

#### ヘッダーとプロパティテンプレートの設定

EMQX 6.0以降、RabbitMQ Sinkアクション作成時にカスタムRabbitMQヘッダーおよびプロパティを定義可能になりました。これにより、メッセージにメタデータを直接付加でき、RabbitMQ内でのメッセージ互換性やルーティングの柔軟性が向上します。

これらのフィールドはルールSQLの変数（例：`${payload.device_id}`）を用いたテンプレート指定が可能です。ヘッダーとプロパティのテンプレートは任意で、空欄の場合は追加メタデータは付加されません。

##### ヘッダーテンプレートの設定方法

RabbitMQヘッダーとして1つ以上のキー・バリューのペアを追加できます。これらはユーザー定義のカスタムメタデータで、RabbitMQコンシューマーが解釈可能です。

- **Key**：ヘッダー名。文字列で指定。
- **Value**：キーに対応する値。静的文字列またはテンプレート変数を使用可能。

例：MQTTペイロードからデバイスIDを含める場合

| Key         | Value                  |
| ----------- | ---------------------- |
| `device_id` | `${payload.device_id}` |

##### プロパティテンプレートの設定方法

RabbitMQは標準的なメッセージプロパティセットをサポートしています。EMQXではこれらを定義して、コンテンツタイプや相関IDなどのメッセージレベルのメタデータを付加できます。

- **Key**：以下の有効なプロパティキーから選択（無効なキーは無視されます）。
- **Value**：静的値またはテンプレート変数を設定。

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

**Headers Template**：

| Key         | Value                  |
| ----------- | ---------------------- |
| `device_id` | `${payload.device_id}` |

**Properties Template**：

| Key      | Value    |
| -------- | -------- |
| `app_id` | `my_app` |

この設定により、RabbitMQに転送される全メッセージに対して以下が付加されます：

- コンシューマー処理用のカスタムメタデータ（ヘッダー）
- メッセージ処理やデバッグ用の標準メタデータ（プロパティ）

## RabbitMQ Sinkを使ったルールのテスト

EMQXダッシュボードに組み込まれたWebSocketクライアントを使って、ルールとSinkの動作をテストできます。

1. ダッシュボード左メニューの**Diagnose** -> **WebSocket Client**をクリックします。
2. 現在のEMQXインスタンスへの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用可能です。
   - 認証設定を変更している場合はユーザー名・パスワードを入力してください。
3. **Connect**をクリックしてEMQXに接続します。
4. ページ下部のパブリッシュエリアで以下を入力します：
   * **Topic**：`t/test`
   * **Payload**：`Hello World RabbitMQ from EMQX`
   * **QoS**：`2`
5. **Publish**をクリックしてメッセージを送信します。

   Sinkとルールが正常に作成されていれば、指定したExchangeに指定ルーティングキーでメッセージがパブリッシュされます。

6. http://localhost:15672 のRabbitMQ管理コンソールにアクセスし、**Queues**セクションに移動します。

   ::: tip

   デフォルト設定のままなら、ユーザー名・パスワードともに`guest`を使用してください。

   :::

7. メッセージが適切なQueueにルーティングされていることを確認します。Queueをクリックし、**Get Message(s)**ボタンを押すと詳細なメッセージ内容を確認できます。

<img src="./assets/rabbitmq/rabbit_mq_management_ui_got_message.png" alt="bridge_igress" style="zoom:67%;" />

## RabbitMQ Sourceを使ったルールの作成

このセクションでは、RabbitMQキューからEMQXへデータを転送するルールの作成方法を説明します。RabbitMQ Sourceとメッセージのリパブリッシュアクションを作成し、RabbitMQサービスからメッセージを消費してEMQXに転送します。

1. ダッシュボードの**Integration** -> **Rules**ページに移動します。
2. 画面右上の**Create**をクリックします。
3. ルールIDに`my_rule_source`を入力します。
4. ルールをトリガーするソース（Data Inputs）を設定します。右側の**Data Inputs**タブをクリックし、デフォルトの`Messages`入力を削除して、**Add Input**をクリックしRabbitMQ Sourceを作成します。
5. **Add Input**ポップアップで、**Input Type**ドロップダウンから`RabbitMQ`を選択します。**Source**はデフォルトの`Create Source`のままにし、新規Sourceを作成してルールに追加します。
6. Sourceの**Name**と任意の**Description**を入力します。名前は大文字・小文字の英数字の組み合わせで、例：`my-rabbitmq-source`。
7. **Connector**ドロップダウンから先に作成した`my-rabbitmq`を選択します。新規作成する場合はドロップダウン横のボタンをクリックしてポップアップで作成可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。
8. RabbitMQからメッセージを消費するためのSource情報を設定します：

   - **Queue**：RabbitMQで事前に作成した`message-send`を入力します。
   - **No Ack**：`no_ack`モードでRabbitMQからメッセージを消費するか選択します。`no_ack`を有効にすると、RabbitMQはコンシューマーの処理完了を待たずにメッセージをキューから即時削除します。
   - **Wait for Publish Confirmations**：メッセージパブリッシャーのアックを待つかどうかを指定します。

9. 詳細設定（任意）：デフォルト値を使用します。
10. **Create**ボタンをクリックしてSource作成を完了し、ルールのデータ入力に追加します。同時にルールSQLは以下のように変更されます：

    ```sql
    SELECT
    *
    FROM
    "$bridges/rabbitmq:my-rabbitmq-source"
    ```

    ルールSQLはRabbitMQ Sourceから以下のフィールドにアクセスでき、データ処理のためにSQLを調整可能です。ここではデフォルトSQLを使用します。

    | フィールド名 | 説明                                                          |
    | :----------- | :------------------------------------------------------------ |
    | payload      | RabbitMQメッセージの内容                                      |
    | event        | イベントトピック。形式は`$bridges/rabbitmq:<source name>`     |
    | metadata     | ルールID情報                                                  |
    | timestamp    | メッセージがEMQXに到着したタイムスタンプ                      |
    | node         | メッセージが到着したEMQXノード名                              |
    | queue        | メッセージを消費したキュー名                                  |
    | exchange     | メッセージがルーティングされたエクスチェンジ                  |
    | routing_key  | エクスチェンジからキューへメッセージをルーティングするためのルーティングキー |

これでRabbitMQ Sourceの作成は完了しましたが、購読したデータはまだEMQXに直接パブリッシュされません。次に、SourceのメッセージをEMQXに転送するためのメッセージリパブリッシュアクションを作成します。

![rabbitmq_source](./assets/rabbitmq/rabbitmq_source.png)

### ルールへのリパブリッシュアクション追加

このセクションでは、RabbitMQ Sourceから消費したメッセージをEMQXトピック`t/1`にパブリッシュするためのリパブリッシュアクションをルールに追加する方法を説明します。

1. 画面右側の**Action Output**タブを選択し、**Add Action**ボタンをクリックします。**Type of Action**ドロップダウンから`Republish`アクションを選択します。
2. メッセージリパブリッシュの設定を入力します：

   - **Topic**：MQTTにパブリッシュするトピック。ここでは`t/1`を入力。
   - **QoS**：`0`、`1`、`2`、`${qos}`のいずれかを選択、または他のフィールドからQoSを設定するためのプレースホルダーを入力可能。`${qos}`を選択すると元メッセージのQoSに従います。
   - **Retain**：`true`または`false`を選択。メッセージをリテインメッセージとしてパブリッシュするかどうか。プレースホルダーも使用可能。ここでは`false`を選択。
   - **Payload**：転送するメッセージペイロードのテンプレート。空欄の場合はルール出力結果をそのまま転送。ここでは`${payload}`を入力し、ペイロードのみ転送。
   - **MQTT 5.0 Message Properties**：デフォルトで無効。詳細は[リパブリッシュアクションの追加](./rule-get-started.md#add-republish-action)を参照。
3. **Create**をクリックしてアクション作成を完了します。成功するとルール作成ページに戻り、リパブリッシュアクションが**Action Outputs**タブに追加されます。
4. ルール作成ページで**Create**ボタンをクリックし、ルール全体の作成を完了します。

これでルールの作成が完了しました。**Rules**ページで新しいルールを確認でき、**Sources**タブでRabbitMQ Sourceも確認できます。

また、**Integrate** -> **Flow Designer**をクリックするとトポロジーを視覚的に確認でき、RabbitMQ Sourceからのメッセージがリパブリッシュにより`t/1`にパブリッシュされる様子が直感的に把握できます。

## RabbitMQ Sourceを使ったルールのテスト

1. [MQTTX CLI](https://mqttx.app/cli)を使ってトピック`t/1`をサブスクライブします：

   ```bash
   mqttx sub -t t/1
   ```

2. 以下のコマンドでRabbitMQにメッセージを送信できます：

   ```bash
   rabbitmqadmin --username=guest --password=guest \
        publish routing_key=message-send \
        payload="{ \"msg\": \"Hello EMQX\"}"
   ```

   - `publish`はメッセージをパブリッシュするコマンドです。
   - `routing_key=message-send`はメッセージのルーティングキーを設定します。この例ではキュー名をルーティングキーとして使用しています。
   - `payload="{ \"msg\": \"Hello EMQX\"}"`はメッセージの内容を設定します。

   または、RabbitMQ管理インターフェースからメッセージをパブリッシュすることも可能です：

   1. 上部メニューの**Queues**タブをクリック。
   2. **Name**列の`message-send`をクリックして詳細ページを開く。
   3. **Publish message**を展開し、**Payload**欄に`"Hello EMQX"`を入力して**Publish message**ボタンをクリック。

3. MQTTXの出力に以下のように表示されます：

   ```bash
   [2024-2-23] [16:59:28] › payload: {"payload":{"msg":"Hello EMQX"},"event":"$bridges/rabbitmq:my-rabbitmq-source","metadata":{"rule_id":"rule_0ly1"},"timestamp":1708678768449,"node":"emqx@127.0.0.1"}
   ```
