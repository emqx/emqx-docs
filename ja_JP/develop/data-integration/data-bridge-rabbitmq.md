# RabbitMQへのMQTTデータ取り込み

[RabbitMQ](https://www.rabbitmq.com/)は、Advanced Message Queuing Protocol（AMQP）を実装した広く使われているオープンソースのメッセージブローカーです。分散システム間のメッセージングにおいて堅牢かつスケーラブルなプラットフォームを提供します。EMQXはRabbitMQとの統合をサポートしており、MQTTメッセージやイベントをRabbitMQへ転送できます。また、RabbitMQサーバーからのデータを消費し、EMQXの特定トピックにパブリッシュすることも可能で、RabbitMQからMQTTへのメッセージ配信を実現します。

本ページでは、EMQXとRabbitMQ間のデータ統合の詳細と、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作概要

RabbitMQデータ統合は、MQTTベースのIoTデータとRabbitMQの強力なメッセージキュー処理機能の橋渡しを目的としたEMQXの標準機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからRabbitMQへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

RabbitMQ Sinkを例に、以下の図はEMQXとRabbitMQ間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration RabbitMQ](./assets/emqx-integration-rabbitmq.png)

MQTTデータをRabbitMQに取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、リアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージ到着時にルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づき、RabbitMQへルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **RabbitMQへのメッセージ取り込み**：ルールによる処理が完了すると、メッセージをRabbitMQに転送するアクションがトリガーされます。処理済みメッセージはシームレスにRabbitMQへ書き込まれます。
4. **データの永続化と活用**：RabbitMQはメッセージをキューに保存し、適切なコンシューマに配信します。メッセージは他のアプリケーションやサービスで消費され、データ分析、可視化、保存などのさらなる処理に利用されます。

## 特長と利点

RabbitMQとのデータ統合は、以下のような特長と利点をビジネスにもたらします：

- **信頼性の高いIoTデータメッセージ配信**：EMQXはデバイスからクラウドへの信頼性の高い接続とメッセージ配信を保証し、RabbitMQはメッセージの永続化と異なるサービス間での信頼性の高い配信を担い、データの信頼性を全工程で確保します。
- **MQTTメッセージの変換**：ルールエンジンを用いてEMQXはMQTTメッセージのフィルタリングや変換が可能です。データ抽出、フィルタリング、強化、変換を経てRabbitMQへ送信します。
- **柔軟なメッセージマッピング**：RabbitMQデータ統合はMQTTトピックからRabbitMQのRouting KeyおよびExchangeへの柔軟なマッピングをサポートし、MQTTとRabbitMQ間のシームレスな統合を実現します。
- **高可用性とクラスター対応**：EMQXとRabbitMQは共に高可用なメッセージブローカークラスターの構築をサポートし、ノード障害時もサービス継続を保証します。クラスター機能により優れたスケーラビリティも提供します。
- **高スループット環境での処理能力**：RabbitMQデータ統合は同期・非同期の書き込みモードをサポートし、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整可能です。

## はじめる前に

このセクションでは、RabbitMQデータ統合を作成する前に必要な準備、RabbitMQサーバーの起動方法およびテスト用のExchangeとQueueの作成方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)および[リパブリッシュアクション](./rule-get-started.md#add-republish-action)に関する知識
- UNIXターミナルとコマンドの基本知識

### RabbitMQサーバーの起動

ここでは[Docker](https://www.docker.com/)を使ったRabbitMQサーバーの起動方法を紹介します。

以下のコマンドを実行し、管理プラグインを有効にしたRabbitMQサーバーを起動します。管理プラグインによりWebインターフェースでRabbitMQを確認できます。

```bash
docker run -it --rm --name rabbitmq -p 127.0.0.1:5672:5672 -p 127.0.0.1:15672:15672 rabbitmq:3.11-management
```

詳細は[Docker HubのRabbitMQ実行方法](https://hub.docker.com/_/rabbitmq)をご覧ください。

### メッセージ受信用のExchangeとQueueの作成

RabbitMQサーバー起動後、RabbitMQ管理Webインターフェースを使って、EMQXから転送されるメッセージ受信用のテスト用ExchangeとQueueを作成します。既にExchangeとQueueがある場合はこのセクションをスキップできます。

1. Webブラウザで http://localhost:15672/ にアクセスし、RabbitMQ管理Webインターフェースを開きます。ログイン画面で以下のデフォルト認証情報を入力し、**Login**をクリックします。
   - **Username**: `guest`
   - **Password**: `guest`
2. 上部メニューの**Exchanges**タブをクリックし、**Add a new exchange**を展開して以下を入力します：
   * **Name**: `test_exchange`
   * **Type**: ドロップダウンから`direct`を選択
   * **Durability**: `Durable`を選択し、Exchangeを永続化（RabbitMQ再起動後も存在）
   * **Auto delete**: `No`
   * **Internal**: `No`
   * **Arguments**: 空欄のまま
3. **Add exchange**ボタンをクリックし、テスト用Exchangeを作成します。
4. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します：
   * **Type**: `Default for virtual host`
   * **Name**: `test_queue`
   * **Durability**: `Durable`を選択し、Queueを永続化
   * **Arguments**: 空欄のまま
5. **Add queue**ボタンをクリックし、テスト用Queueを作成します。新しい`test_queue`が**All queues**セクションに表示されます。
6. `test_queue`の名前をクリックして詳細ページを開き、**Bindings**を展開します。**Add binding to this queue**セクションに以下を入力します：
   * **From exchange**: `test_exchange`
   * **Routing key**: `test_routing_key`
   * **Arguments**: 空欄のまま
7. **Bind**ボタンをクリックし、`test_queue`を指定したExchangeとRouting Keyにバインドします。

### メッセージパブリッシュ用のQueue作成

RabbitMQ管理Webインターフェースを使って、RabbitMQメッセージのパブリッシュ用Queueを作成できます。

1. RabbitMQ管理Webインターフェースにログインします。
2. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します：
   * **Type**: `Default for virtual host`
   * **Name**: `message-send`
   * **Durability**: `Durable`を選択し、Queueを永続化
   * **Arguments**: 空欄のまま
3. **Add queue**ボタンをクリックし、`message-send`キューを作成します。新しいQueueが**All queues**に表示されます。

## コネクターの作成

このセクションでは、Rabbit Sink/SourceをRabbitMQサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとRabbitMQをローカルマシンで実行している前提です。RabbitMQが別環境にある場合は設定を適宜調整してください。

1. ダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. 画面右上の**Create**をクリックします。
3. **Create Connector**ページで**RabbitMQ**を選択し、**Next**をクリックします。
4. コネクター名を入力します。英数字の組み合わせで、例：`my_rabbitmq`。
5. 接続情報を入力します。
   - **Server**: RabbitMQサーバーがローカルなら`localhost`、リモートなら実際のホスト名/IP
   - **Port**: 通常は`5672`、異なる場合は適宜入力
   - **Username**: `guest`
   - **Password**: `guest`
   - **Virtual Host**: RabbitMQの仮想ホスト。デフォルトは`/`
   - 暗号化接続を行う場合は**Enable TLS**をオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**でRabbitMQサーバーへの接続確認が可能です。
7. **Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択できます。**Create Rule**を選ぶと以下の選択肢があります：
   - **Action Outputs**：RabbitMQ Sinkを使ったルール作成。RabbitMQへの転送データを指定します。[RabbitMQ Sinkでルール作成](#create-a-rule-with-rabbitmq-sink)の手順も参照ください。
   - **Data Inputs**：RabbitMQ Sourceを使ったルール作成。[RabbitMQ Sourceでルール作成](#create-a-rule-with-rabbitmq-source)の手順も参照ください。

## RabbitMQ Sinkでルール作成

このセクションでは、ダッシュボードでソースMQTTトピック`t/#`のメッセージを処理し、処理済みデータをRabbitMQのキュー`test_queue`へ転送するルール作成方法を説明します。

1. EMQXダッシュボードで、**Integration -> Rules**をクリックします。
2. 画面右上の**Create**をクリックします。
3. ルールIDを入力します。例：`my_rule`
4. SQLエディタに以下のステートメントを入力します。トピックパターン`t/#`にマッチするMQTTメッセージを転送します。

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

5. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションでEMQXはルール処理済みデータをRabbitMQへ送信します。
6. **Type of Action**ドロップダウンから`RabbitMQ`を選択します。**Action**はデフォルトの`Create Action`のままにします。既にSinkを作成済みなら選択も可能ですが、ここでは新規Sinkを作成します。
7. Sink名を入力します。英数字の組み合わせで入力してください。
8. **Connector**ドロップダウンから`my_rabbitmq`を選択します。新規コネクター作成はドロップダウン横のボタンから可能です。設定パラメータは[コネクター作成](#create-a-connector)を参照してください。
9. Sinkの設定を以下の通り行います：

   * **Exchange**：事前に作成した`test_exchange`を入力。メッセージはこのExchangeにパブリッシュされます。

       ::: tip 注意

       ExchangeがRabbitMQに存在することを確認してください。存在しない場合、アクションは一時的に失敗し、定期的に再接続を試みます。

       :::

   * **Routing Key**：事前に作成した`test_routing_key`を入力。RabbitMQのメッセージパブリッシュ用ルーティングキーです。

       ::: tip

       ExchangeとRouting Keyはテンプレート値として設定可能で、プレースホルダーを使い受信MQTTメッセージペイロードから値を動的に抽出しルーティングできます。

       例：Routing Keyをペイロード内のフィールドに基づき動的に設定する場合、`${payload.akey}`と設定します。これによりペイロードの`akey`フィールドの値がRouting Keyとして使われます。

       **注意**：バッチモードではExchangeとRouting Keyのテンプレート値はバッチ内全メッセージで一定である必要があります。これにより一貫したルーティングが保証され、バッチ処理時の競合を防ぎます。

       :::

   * **Virtual Host**：RabbitMQの仮想ホスト。デフォルトは`/`
   * **Message Delivery Mode**ドロップダウンで`non_persistent`または`persistent`を選択：
     * `non_persistent`（デフォルト）：メッセージはディスクに永続化されず、RabbitMQ再起動やクラッシュ時に失われる可能性があります。
     * `persistent`：メッセージはディスクに永続化され、RabbitMQ再起動やクラッシュ時にも耐久性があります。

       ::: tip

       メッセージ損失防止のため、QueueとExchangeもDurableに設定する必要があります。詳細はRabbitMQの[ドキュメント](https://www.rabbitmq.com/documentation.html)を参照してください。

       :::

   * **Payload Template**：デフォルトは空文字列で、メッセージペイロードはJSON形式のテキストとしてRabbitMQにそのまま転送されます。

     プレースホルダーを使い、受信MQTTメッセージのデータを動的に含むカスタムペイロードフォーマットを定義可能です。例えば、MQTTメッセージのペイロードとタイムスタンプを含めたい場合は以下のテンプレートを使います：

     ```json
      {"payload": "${payload}", "timestamp": ${timestamp}}
     ```

     このテンプレートは、受信MQTTメッセージのペイロードとタイムスタンプを含むJSON形式のメッセージを生成します。`${payload}`と`${timestamp}`はプレースホルダーで、実際の値に置き換えられます。

   - **Wait for Publish Confirmations**：デフォルトで有効。メッセージがRabbitMQに正常にパブリッシュされたことを保証します。

     ::: tip

     このオプション有効時、RabbitMQブローカーはメッセージ受領をアック（ACK）し、成功を確認してからパブリッシュ完了とみなすため、メッセージ配信の信頼性が向上します。

     :::

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
11. **詳細設定（任意）**：
    - **Publish Confirmation Timeout**：デフォルト30秒。パブリッシャーがブローカーのアックを待つ最大時間です。
    - 必要に応じて**sync**または**async**クエリモードを選択可能。詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。
12. **Create**をクリックする前に、**Test Connectivity**でSinkがRabbitMQサーバーに接続できるか確認可能です。
13. **Create**ボタンをクリックし、Sink設定を完了します。新しいSinkが**Action Outputs**に追加されます。
14. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでRabbitMQ Sinkを介したデータ転送用ルールが作成されました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブをクリックすると、新しいRabbitMQ Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されRabbitMQに送信・保存されていることが視覚的に確認できます。

## RabbitMQ Sinkルールのテスト

EMQXダッシュボードの組み込みWebSocketクライアントを使ってルールとSinkの動作をテストできます。

1. ダッシュボード左メニューで**Diagnose** -> **WebSocket Client**をクリックします。
2. 現在のEMQXインスタンスへの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用可能です。
   - 認証設定を変更している場合はユーザー名やパスワードを入力してください。
3. **Connect**をクリックし、クライアントをEMQXに接続します。
4. ページ下部のパブリッシュエリアに以下を入力します：
   * **Topic**: `t/test`
   * **Payload**: `Hello World RabbitMQ from EMQX`
   * **QoS**: `2`
5. **Publish**をクリックしてメッセージを送信します。

   Sinkとルールが正常に作成されていれば、指定したExchangeに指定したRouting Keyでメッセージがパブリッシュされているはずです。

6. http://localhost:15672 のRabbitMQ管理コンソールにアクセスし、**Queues**セクションに移動します。

   ::: tip

   デフォルト設定の場合、ユーザー名とパスワードは共に`guest`を使用してください。

   :::

7. メッセージが適切なキューにルーティングされていることを確認します。キュー名をクリックして詳細を開き、**Get Message(s)**ボタンをクリックするとメッセージ内容を確認できます。

<img src="./assets/rabbitmq/rabbit_mq_management_ui_got_message.png" alt="ブリッジイングレス" style="zoom:67%;" />

## RabbitMQ Sourceルールの作成

このセクションでは、RabbitMQキューからEMQXへデータを転送するルール作成方法を説明します。RabbitMQ Sourceとメッセージリパブリッシュアクションの両方を作成し、RabbitMQサービスからメッセージを消費しEMQXへ転送します。

1. ダッシュボードの**Integration** -> **Rules**ページに移動します。
2. 画面右上の**Create**をクリックします。
3. ルールIDに`my_rule_source`を入力します。
4. ルールをトリガーするソース（Data Inputs）を設定します。右側の**Data Inputs**タブをクリックし、デフォルトの`Messages`入力を削除後、**Add Input**をクリックしてRabbitMQ Sourceを作成します。
5. **Add Input**ポップアップで、**Input Type**ドロップダウンから`RabbitMQ`を選択します。**Source**はデフォルトの`Create Source`のままにし、新規Sourceを作成してルールに追加します。
6. Sourceの**Name**と（任意で）**Description**を入力します。英数字の組み合わせで例：`my-rabbitmq-source`。
7. **Connector**ドロップダウンから先に作成した`my-rabbitmq`コネクターを選択します。新規コネクター作成はドロップダウン横のボタンから可能です。設定は[コネクター作成](#create-a-connector)を参照してください。
8. RabbitMQからEMQXへメッセージを消費するためのSource情報を設定します：

   - **Queue**：RabbitMQで先に作成した`message-send`キュー名を入力
   - **No Ack**：状況に応じてRabbitMQの`no_ack`モードでメッセージを消費するか選択。`no_ack`有効時はRabbitMQがメッセージを配信後すぐにキューから削除し、コンシューマの処理成功を待ちません。
   - **Wait for Publish Confirmations**：メッセージパブリッシャーのアックを使用する場合、RabbitMQの確認を待つか指定します。

9. 詳細設定（任意）：デフォルト値のままで問題ありません。
10. **Create**ボタンをクリックしSource作成を完了、ルールのデータ入力に追加します。同時にルールSQLは以下のように変更されます：

    ```sql
    SELECT
    *
    FROM
    "$bridges/rabbitmq:my-rabbitmq-source"
    ```

    ルールSQLはRabbitMQ Sourceから以下のフィールドにアクセス可能で、SQLを調整してデータ処理が行えます。ここではデフォルトSQLを使用します。

    | フィールド名     | 説明                                                        |
    | :--------------- | :---------------------------------------------------------- |
    | payload          | RabbitMQメッセージの内容                                    |
    | event            | イベントトピック。形式は`$bridges/rabbitmq:<source name>` |
    | metadata         | ルールID情報                                                |
    | timestamp        | メッセージがEMQXに到着したタイムスタンプ                   |
    | node             | メッセージが到着したEMQXノード名                           |
    | queue            | メッセージを消費したキュー名                               |
    | exchange         | メッセージがルーティングされたExchange名                   |
    | routing_key      | ExchangeからQueueへメッセージをルーティングするためのRouting Key |

ここまででRabbitMQ Sourceの作成は完了しましたが、購読したデータはEMQXに直接パブリッシュされません。次にメッセージリパブリッシュアクションを作成し、SourceのメッセージをEMQXへ転送します。

![rabbitmq_source](./assets/rabbitmq/rabbitmq_source.png)

### ルールにリパブリッシュアクションを追加

このセクションでは、RabbitMQ Sourceから消費したメッセージをEMQXトピック`t/1`にパブリッシュするためのリパブリッシュアクション追加方法を説明します。

1. 画面右側の**Action Output**タブを選択し、**Add Action**ボタンをクリックします。**Type of Action**ドロップダウンから`Republish`アクションを選択します。
2. メッセージリパブリッシュ設定を入力します：

   - **Topic**：MQTTにパブリッシュするトピック。ここでは`t/1`を入力。
   - **QoS**：`0`、`1`、`2`、`${qos}`のいずれかを選択、または他フィールドからQoSを設定するプレースホルダーを入力可能。`${qos}`を選ぶと元メッセージのQoSに従います。
   - **Retain**：`true`または`false`を選択。メッセージをリテインメッセージとしてパブリッシュするか指定。プレースホルダーも使用可能。ここでは`false`を選択。
   - **Payload**：転送メッセージのペイロード生成用テンプレート。空欄はルール出力結果をそのまま転送。ここでは`${payload}`を入力し、ペイロードのみ転送。
   - **MQTT 5.0 Message Properties**：デフォルトで無効。詳細設定は[リパブリッシュアクション追加](./rule-get-started.md#add-republish-action)を参照。
3. **Create**をクリックしアクション作成を完了。成功するとルール作成ページに戻り、リパブリッシュアクションが**Action Outputs**タブに追加されます。
4. ルール作成ページで**Create**をクリックし、ルール全体の作成を完了します。

これでルールが正常に作成されました。**Rules**ページで新規ルールを確認できます。**Sources**タブには新規RabbitMQ Sourceが表示されます。

また、**Integrate** -> **Flow Designer**をクリックするとトポロジーが表示され、RabbitMQ Sourceからのメッセージがリパブリッシュを通じて`t/1`にパブリッシュされる様子を直感的に確認できます。

## RabbitMQ Sourceルールのテスト

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

   - `publish`はメッセージパブリッシュコマンドです。
   - `routing_key=message-send`はメッセージのルーティングキーを設定します。この例ではキュー名をルーティングキーとして使用。
   - `payload="{ \"msg\": \"Hello EMQX\"}"`はメッセージ内容を設定します。

   あるいは、RabbitMQ管理インターフェースからもメッセージをパブリッシュ可能です：

   1. 上部メニューの**Queues**タブをクリック。
   2. **Name**列の`message-send`をクリックし詳細ページを開く。
   3. **Publish message**を展開し、**Payload**欄に`"Hello EMQX"`を入力し、**Publish message**ボタンをクリック。

3. MQTTXで以下のような出力が表示されます：

   ```bash
   [2024-2-23] [16:59:28] › payload: {"payload":{"msg":"Hello EMQX"},"event":"$bridges/rabbitmq:my-rabbitmq-source","metadata":{"rule_id":"rule_0ly1"},"timestamp":1708678768449,"node":"emqx@127.0.0.1"}
   ```
