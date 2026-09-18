# RabbitMQへのMQTTデータ取り込み

[RabbitMQ](https://www.rabbitmq.com/) は、Advanced Message Queuing Protocol（AMQP）を実装した広く使われているオープンソースのメッセージブローカーです。分散システム間のメッセージングにおいて堅牢かつスケーラブルなプラットフォームを提供します。EMQXはRabbitMQとの統合をサポートしており、MQTTメッセージやイベントをRabbitMQに転送できます。また、RabbitMQサーバーからデータを取得し、EMQXの特定のトピックにパブリッシュすることも可能で、RabbitMQからMQTTへのメッセージ配信を実現します。

本ページでは、EMQXとRabbitMQ間のデータ統合について詳細に解説し、データ統合の作成および検証の実践的な手順を紹介します。

## 動作概要

RabbitMQデータ統合は、MQTTベースのIoTデータとRabbitMQの強力なメッセージキュー処理機能を橋渡しするためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、EMQXからRabbitMQへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

RabbitMQ Sinkを例にとると、以下の図はEMQXとRabbitMQ間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration RabbitMQ](./assets/emqx-integration-rabbitmq.png)

MQTTデータをRabbitMQに取り込む流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、リアルタイムのMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づき、RabbitMQにルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **RabbitMQへのメッセージ取り込み**：ルールの処理が完了すると、RabbitMQへのメッセージ転送アクションがトリガーされます。処理済みメッセージはシームレスにRabbitMQへ書き込まれます。
4. **データの永続化と活用**：RabbitMQはメッセージをキューに保存し、適切なコンシューマに配信します。メッセージは他のアプリケーションやサービスによって消費され、データ分析、可視化、保存などのさらなる処理に利用されます。

## 特長とメリット

RabbitMQとのデータ統合は以下の特長と利点をもたらします。

- **信頼性の高いIoTデータメッセージ配信**：EMQXはデバイスからクラウドへの信頼性の高い接続とメッセージ配信を保証し、RabbitMQはメッセージの永続化と異なるサービス間の信頼性の高い配信を担い、各プロセスにおけるデータの信頼性を確保します。
- **MQTTメッセージの変換**：ルールエンジンを用いてEMQXはMQTTメッセージのフィルタリングや変換が可能です。メッセージはデータ抽出、フィルタリング、強化、変換を経てRabbitMQに送信されます。
- **柔軟なメッセージマッピング**：RabbitMQデータ統合はMQTTトピックとRabbitMQのルーティングキーおよびエクスチェンジの柔軟なマッピングをサポートし、MQTTとRabbitMQ間のシームレスな統合を実現します。
- **高可用性とクラスターサポート**：EMQXとRabbitMQは共に高可用なメッセージブローカークラスターの構築をサポートし、ノード障害時でもサービスの継続提供を可能にします。クラスター機能により優れたスケーラビリティも提供されます。
- **高スループット環境での処理能力**：RabbitMQデータ統合は同期および非同期の書き込みモードをサポートし、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。

## はじめる前に

このセクションでは、RabbitMQデータ統合の作成に先立ち必要な準備について説明します。RabbitMQサーバーの起動およびテスト用のRabbitMQエクスチェンジとキューの作成方法を含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)および[リパブリッシュアクション](./rule-get-started.md#add-republish-action)に関する知識
- UNIXターミナルおよびコマンドの基本知識

### RabbitMQサーバーの起動

ここでは[Docker](https://www.docker.com/)を使ったRabbitMQサーバーの起動方法を紹介します。

以下のコマンドを実行すると、管理プラグインが有効なRabbitMQサーバーが起動します。管理プラグインによりWebインターフェースでRabbitMQを監視できます。

```bash
docker run -it --rm --name rabbitmq -p 127.0.0.1:5672:5672 -p 127.0.0.1:15672:15672 rabbitmq:3.11-management
```

詳細は[Docker HubのRabbitMQのページ](https://hub.docker.com/_/rabbitmq)をご覧ください。

### メッセージ受信用のエクスチェンジとキューの作成

RabbitMQサーバー起動後、RabbitMQ管理Webインターフェースを使ってテスト用のエクスチェンジとキューを作成し、EMQXから転送されるメッセージを受信できます。すでにテスト用のエクスチェンジとキューがある場合はこのセクションをスキップしてください。

1. ブラウザで http://localhost:15672/ にアクセスし、RabbitMQ管理Webインターフェースを開きます。ログイン画面で以下のデフォルト認証情報を入力し、**Login**をクリックします。
   - **Username**: `guest`
   - **Password**: `guest`
2. 上部メニューの**Exchanges**タブをクリックし、**Add a new exchange**を展開して以下を入力します。
   * **Name**: `test_exchange`
   * **Type**: ドロップダウンから`direct`を選択
   * **Durability**: `Durable`を選択（RabbitMQサーバー再起動後もエクスチェンジが残る）
   * **Auto delete**: `No`
   * **Internal**: `No`
   * **Arguments**: 空欄のまま
3. **Add exchange**ボタンをクリックしてエクスチェンジを作成します。
4. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します。
   * **Type**: `Default for virtual host`
   * **Name**: `test_queue`
   * **Durability**: `Durable`を選択（キューの永続化）
   * **Arguments**: 空欄のまま
5. **Add queue**ボタンをクリックしてキューを作成します。新しい`test_queue`が**All queues**に表示されます。
6. キュー名`test_queue`をクリックして詳細ページを開き、**Bindings**を展開します。**Add binding to this queue**セクションで以下を入力します。
   * **From exchange**: `test_exchange`
   * **Routing key**: `test_routing_key`
   * **Arguments**: 空欄のまま
7. **Bind**ボタンをクリックして、`test_queue`を`test_exchange`に指定のルーティングキーでバインドします。

### メッセージ送信用のキュー作成

RabbitMQ管理WebインターフェースでRabbitMQメッセージのパブリッシュ用キューを作成できます。

1. RabbitMQ管理Webインターフェースにログインします。
2. 上部メニューの**Queues**タブをクリックし、**Add a new queue**を展開して以下を入力します。
   * **Type**: `Default for virtual host`
   * **Name**: `message-send`
   * **Durability**: `Durable`を選択（キューの永続化）
   * **Arguments**: 空欄のまま
3. **Add queue**ボタンをクリックしてキューを作成します。新しい`message-send`が**All queues**に表示されます。

## コネクターの作成

このセクションでは、Rabbit Sink/SourceとRabbitMQサーバーを接続するコネクターの作成方法を示します。

以下の手順はEMQXとRabbitMQをローカルマシンで実行していることを前提としています。RabbitMQが別環境にある場合は設定を適宜調整してください。

1. ダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. 画面右上の**Create**をクリックします。
3. **Create Connector**ページで**RabbitMQ**を選択し、**Next**をクリックします。
4. コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_rabbitmq`。
5. 接続情報を入力します。
   - **Server**: RabbitMQサーバーがローカルの場合は`localhost`、リモートの場合はホスト名/IPを入力。
   - **Port**: 通常は`5672`、異なる場合は実際のポート番号を入力。
   - **Username**: `guest`
   - **Password**: `guest`
   - **Virtual Host**: RabbitMQの仮想ホスト。デフォルトは`/`。
   - 暗号化接続を行う場合は**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがRabbitMQサーバーに接続できるかテストできます。
7. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択できます。**Create Rule**を選択すると以下の選択肢があります。
   - **Action Outputs**：RabbitMQ Sinkを使ったルール作成で、RabbitMQに転送するデータを指定します。[RabbitMQ Sinkを使ったルール作成](#create-a-rule-with-rabbitmq-sink)の手順も参照してください。
   - **Data Inputs**：RabbitMQ Sourceを使ったルール作成。[RabbitMQ Sourceを使ったルール作成](#create-a-rule-with-rabbitmq-source)の手順も参照してください。

## RabbitMQ Sinkを使ったルール作成

このセクションでは、ダッシュボードでMQTTトピック`t/#`からのメッセージを処理し、処理済みデータをRabbitMQのキュー`test_queue`に転送するルールの作成方法を示します。

1. EMQXダッシュボードで**Integration -> Rules**をクリックします。
2. 画面右上の**Create**をクリックします。
3. ルールIDを入力します。例：`my_rule`。
4. SQLエディタに以下のステートメントを入力します。これはトピックパターン`t/#`にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT
     payload,
     now_timestamp() as timestamp
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールの学習とテストを行えます。

   :::

5. + **Add Action**ボタンをクリックしてルールにトリガーされるアクションを定義します。このアクションによりEMQXはルールで処理したデータをRabbitMQに送信します。
6. **Type of Action**のドロップダウンから`RabbitMQ`を選択します。**Action**はデフォルトの`Create Action`のままにします。すでにSinkを作成している場合は選択も可能です。この例では新規Sinkを作成します。
7. Sinkの名前を入力します。大文字・小文字の英数字の組み合わせで入力してください。
8. **Connector**のドロップダウンから`my_rabbitmq`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクター作成](#create-a-connector)を参照してください。
9. Sinkの以下の情報を設定します。

   * **Exchange**: 事前に作成した`test_exchange`を入力します。メッセージはこのエクスチェンジにパブリッシュされます。

       ::: tip 注意

       RabbitMQにエクスチェンジが作成済みであることを確認してください。未作成の場合、アクションは一時的に失敗し、定期的に再接続を試みます。

       :::

   * **Routing Key**: 事前に作成した`test_routing_key`を入力します。RabbitMQのメッセージパブリッシュ用ルーティングキーです。

       ::: tip

       エクスチェンジとルーティングキーはテンプレート値として設定可能で、プレースホルダーを使い受信MQTTメッセージペイロードから値を動的に抽出してルーティングを行えます。

       例：ルーティングキーをペイロード内のフィールド`akey`に基づいて動的に設定する場合、`${payload.akey}`と設定します。これによりペイロードの`akey`フィールドの値がルーティングキーとして使われます。

       **注意**：バッチモードでは、エクスチェンジとルーティングキーのテンプレート値はバッチ内の全メッセージで一定である必要があります。これにより一貫したルーティングが保証され、バッチ処理時の競合を防ぎます。

       :::

   * **Virtual Host**: RabbitMQの仮想ホスト。デフォルトは`/`。
   * **Message Delivery Mode**のドロップダウンから`non_persistent`または`persistent`を選択します。

     * `non_persistent`（デフォルト）：メッセージはディスクに永続化されず、RabbitMQの再起動やクラッシュ時に失われる可能性があります。

     * `persistent`：メッセージはディスクに永続化され、RabbitMQの再起動やクラッシュ時でも耐久性があります。

       ::: tip

       メッセージの損失を防ぐために、キューとエクスチェンジも耐久性（Durable）に設定する必要があります。詳細はRabbitMQの[ドキュメント](https://www.rabbitmq.com/documentation.html)を参照してください。

       :::

   * **Payload Template**: デフォルトは空文字列で、メッセージペイロードはJSON形式のテキストとしてRabbitMQにそのまま転送されます。

     プレースホルダーを使ってカスタムメッセージペイロード形式を定義し、受信MQTTメッセージのデータを動的に含めることも可能です。例えば、MQTTメッセージのペイロードとタイムスタンプをRabbitMQメッセージに含める場合、以下のテンプレートを使います。

     ```json
      {"payload": "${payload}", "timestamp": ${timestamp}}
     ```

     このテンプレートは、受信したMQTTメッセージのペイロードとタイムスタンプを含むJSON形式のメッセージを生成します。`${payload}`と`${timestamp}`はプレースホルダーで、実際の値に置換されてRabbitMQサーバーに転送されます。

   - **Wait for Publish Confirmations**: デフォルトで有効です。RabbitMQへのメッセージパブリッシュ成功を確認します。

     ::: tip

     このオプションを有効にすると、RabbitMQブローカーはメッセージ受領をアック（ACK）してからパブリッシュ成功と見なすため、メッセージ配信の信頼性が向上します。

     :::

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、プライマリSinkが失敗した場合にトリガーされるフォールバックアクションを1つ以上定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
11. **詳細設定（任意）**：

    - **Publish Confirmation Timeout**：デフォルト30秒。パブリッシャーがブローカーのアックを待つ最大時間です。
    - 必要に応じて**sync**または**async**クエリモードを選択可能です。詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。

12. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがRabbitMQサーバーに接続できるかテストできます。
13. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。
14. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでRabbitMQ Sinkを通じたデータ転送ルールが作成されました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいRabbitMQ Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されRabbitMQに送信・保存されている様子を確認できます。

## RabbitMQ Sinkルールのテスト

EMQXダッシュボードに組み込まれたWebSocketクライアントを使ってルールとSinkのテストが可能です。

1. ダッシュボード左メニューの**Diagnose** -> **WebSocket Client**をクリックします。
2. 現在のEMQXインスタンスへの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用可能です。
   - 認証設定を変更している場合はユーザー名とパスワードを入力してください。
3. **Connect**をクリックしてクライアントをEMQXに接続します。
4. ページ下部のパブリッシュエリアで以下を入力します。
   * **Topic**: `t/test`
   * **Payload**: `Hello World RabbitMQ from EMQX`
   * **QoS**: `2`
5. **Publish**をクリックしてメッセージを送信します。

   Sinkとルールが正常に作成されていれば、指定したルーティングキーでRabbitMQサーバーの指定エクスチェンジにメッセージがパブリッシュされているはずです。

6. http://localhost:15672 のRabbitMQ管理コンソールにアクセスし、**Queues**セクションに移動します。

   ::: tip

   デフォルト設定を変更していなければ、ユーザー名・パスワードともに`guest`を使用してください。

   :::

7. メッセージが適切なキューにルーティングされていることを確認します。キューをクリックして詳細を開き、**Get Message(s)**ボタンをクリックするとメッセージ内容を確認できます。

<img src="./assets/rabbitmq/rabbit_mq_management_ui_got_message.png" alt="bridge_igress" style="zoom:67%;" />

## RabbitMQ Sourceルールの作成

このセクションでは、RabbitMQキューからEMQXへデータを転送するルールの作成方法を示します。RabbitMQ Sourceとメッセージリパブリッシュアクションの両方を作成し、RabbitMQサービスからメッセージを取得してEMQXに転送します。

1. ダッシュボードの**Integration** -> **Rules**ページに移動します。
2. 画面右上の**Create**をクリックします。
3. ルールIDに`my_rule_source`を入力します。
4. ルールをトリガーするソース（Data Inputs）を設定します。画面右の**Data Inputs**タブをクリックし、デフォルトの`Messages`入力を削除後、**Add Input**をクリックしてRabbitMQ Sourceを作成します。
5. **Add Input**ポップアップで、**Input Type**のドロップダウンから`RabbitMQ`を選択します。**Source**はデフォルトの`Create Source`のままにします。この例では新規Sourceを作成しルールに追加します。
6. Sourceの**Name**と任意の**Description**を入力します。名前は大文字・小文字の英数字の組み合わせで、例：`my-rabbitmq-source`。
7. **Connector**のドロップダウンから先に作成した`my-rabbitmq`を選択します。新規コネクター作成はドロップダウン横のボタンから可能です。設定パラメータは[コネクター作成](#create-a-connector)を参照してください。
8. Source情報を設定し、RabbitMQからEMQXへのメッセージ消費設定を完了します。

   - **Queue**: 先にRabbitMQで作成したキュー名`message-send`を入力。
   - **No Ack**: RabbitMQの`no_ack`モードでメッセージを消費するか選択。`no_ack`を有効にすると、RabbitMQはメッセージをコンシューマの処理成功を待たずに即座にキューから削除します。
   - **Wait for Publish Confirmations**: メッセージパブリッシャーのアックを使う場合にRabbitMQの確認を待つかどうかを指定。
9. 詳細設定（任意）：デフォルト値を使用。
10. **Create**ボタンをクリックしてSource作成を完了し、ルールのデータ入力に追加します。同時にルールSQLが以下のように変更されます。

    ```sql
    SELECT
    *
    FROM
    "$bridges/rabbitmq:my-rabbitmq-source"
    ```

    ルールSQLはRabbitMQ Sourceから以下のフィールドにアクセスでき、SQLでデータ処理を調整可能です。ここではデフォルトSQLを使用します。

    | フィールド名     | 説明                                                      |
    | :-------------- | :-------------------------------------------------------- |
    | payload         | RabbitMQメッセージの内容                                  |
    | event           | イベントトピック。形式は`$bridges/rabbitmq:<source name>` |
    | metadata        | ルールID情報                                              |
    | timestamp       | メッセージがEMQXに到着したタイムスタンプ                  |
    | node            | メッセージが到着したEMQXノード名                          |
    | queue           | メッセージが消費されたキュー名                            |
    | exchange        | メッセージがルーティングされたエクスチェンジ名            |
    | routing_key     | エクスチェンジからキューへのメッセージルーティングに使われたルーティングキー |

ここまででRabbitMQ Sourceの作成は完了しましたが、サブスクライブしたデータは直接EMQXにパブリッシュされません。次にメッセージリパブリッシュアクションを作成し、SourceのメッセージをEMQXに転送します。

![rabbitmq_source](./assets/rabbitmq/rabbitmq_source.png)

### ルールへのリパブリッシュアクション追加

このセクションでは、RabbitMQ Sourceから消費したメッセージをEMQXのトピック`t/1`にパブリッシュするためのリパブリッシュアクションの追加方法を示します。

1. 画面右の**Action Output**タブを選択し、**Add Action**ボタンをクリックします。**Type of Action**ドロップダウンから`Republish`アクションを選択します。
2. メッセージリパブリッシュ設定を入力します。

   - **Topic**: MQTTにパブリッシュするトピック。ここでは`t/1`を入力。
   - **QoS**: `0`、`1`、`2`、または`${qos}`を選択可能。`${qos}`を選ぶと元メッセージのQoSに従います。
   - **Retain**: `true`または`false`を選択。メッセージをリテインメッセージとしてパブリッシュするかどうかを決定します。プレースホルダーも利用可能。この例では`false`を選択。
   - **Payload**: 転送するメッセージペイロードのテンプレート。空欄の場合はルールの出力結果をそのまま転送します。ここでは`${payload}`を入力し、ペイロードのみを転送します。
   - **MQTT 5.0 Message Properties**: デフォルトで無効。詳細設定は[リパブリッシュアクションの追加](./rule-get-started.md#add-republish-action)を参照してください。
3. **Create**をクリックしてアクション作成を完了します。作成成功後、ルール作成ページに戻り、リパブリッシュアクションが**Action Outputs**タブに追加されます。
4. ルール作成ページで**Create**をクリックし、ルール全体の作成を完了します。

これでルールが正常に作成されました。**Rules**ページで新規ルールを確認できます。**Sources**タブには新しいRabbitMQ Sourceが表示されます。

また、**Integrate** -> **Flow Designer**をクリックするとトポロジーが表示され、RabbitMQ Sourceからのメッセージがリパブリッシュを経てトピック`t/1`にパブリッシュされる様子を直感的に確認できます。

## RabbitMQ Sourceルールのテスト

1. [MQTTX CLI](https://mqttx.app/cli)を使ってトピック`t/1`をサブスクライブします。

   ```bash
   mqttx sub -t t/1
   ```

2. 以下のコマンドでRabbitMQにメッセージを生成できます。

   ```bash
   rabbitmqadmin --username=guest --password=guest \
        publish routing_key=message-send \
        payload="{ \"msg\": \"Hello EMQX\"}"
   ```

   - `publish`はメッセージをパブリッシュするコマンドです。
   - `routing_key=message-send`はメッセージのルーティングキーを設定します。この例ではキュー名をルーティングキーとして使用しています。
   - `payload="{ \"msg\": \"Hello EMQX\"}"`はメッセージの内容を設定します。

   または、RabbitMQ管理インターフェースからもメッセージをパブリッシュできます。

   1. 上部メニューの**Queues**タブをクリック。
   2. **Name**列の`message-send`をクリックして詳細ページを開く。
   3. **Publish message**を展開し、**Payload**欄に`"Hello EMQX"`と入力して**Publish message**ボタンをクリック。

3. MQTTXで以下のような出力が表示されます。

   ```bash
   [2024-2-23] [16:59:28] › payload: {"payload":{"msg":"Hello EMQX"},"event":"$bridges/rabbitmq:my-rabbitmq-source","metadata":{"rule_id":"rule_0ly1"},"timestamp":1708678768449,"node":"emqx@127.0.0.1"}
   ```
