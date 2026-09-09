# Nari SysKeeperを介したMQTTデータのブリッジ

Nari SysKeeper 2000はネットワーク物理分離装置です。セキュリティ管理システムとして、特に重要インフラや企業ITシステムなど高レベルのセキュリティ対策が求められる分野で広く利用されています。EMQXは、異なるプロダクションゾーンに展開されたEMQXクラスター間のデータブリッジをサポートしています。プロダクションゾーンは3つのセキュリティレベル（I〜III）に分かれており、セキュリティゾーンI-IIはより安全で管理された領域を表し、セキュリティゾーンIIIは制限が緩い領域で、公開向けサービスとより安全な内部領域の橋渡し役を担います。通常、セキュリティゾーンI-IIとIIIは相互に分離されています。データブリッジを通じて、MQTTメッセージはセキュリティゾーンI-IIとIII間の一方向SysKeeperネットワークゲートを通過し、異なるセキュリティゾーンの別のEMQXクラスターとブリッジされます。

本ページでは、EMQXとNari SysKeeper間のデータ統合について包括的に紹介し、データ統合の作成および検証手順を実践的に解説します。

## 動作概要

Nari SysKeeperデータブリッジはEMQXの標準機能であり、MQTTのリアルタイムデータキャプチャとブリッジ機能をSysKeeperの強力なセキュリティ分離機能と組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントを通じて、SysKeeper経由のEMQXブリッジが簡素化され、複雑なコーディングが不要になります。

以下の図は、EMQXとSysKeeper間のデータブリッジの典型的なアーキテクチャを示しています。

<img src="./assets/syskeeper_bridge_architecture.png" alt="syskeeper_bridge_architecture" style="zoom:67%;" />

このパススルー動作は、セキュリティゾーンI-IIとセキュリティゾーンIIIに展開された2つのEMQXクラスター間の一方向データブリッジとして捉えられ、以下のワークフローで動作します。

1. **SysKeeper Proxyの作成**：セキュリティゾーンIIIのEMQX上にSysKeeper Proxyを作成します。SysKeeper ProxyはSysKeeper Forwarderからのメッセージを受信するための特別なTCPリスナーを起動します。
2. **メッセージのパブリッシュと受信**：電力システム内の各種デバイスは、直接EMQXに接続するか、[NeuronEX](https://www.emqx.com/en/products/neuronex)などのゲートウェイを介してMQTTプロトコルに変換し、EMQXに正常に接続します。これらのデバイスは、稼働状況や計測値、イベント発生に応じてMQTTメッセージを送信します。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
3. **メッセージデータの処理**：メッセージはルールエンジンを通過し、EMQXに定義されたルールで処理されます。ルールは、どのメッセージをSysKeeper経由で別のEMQXクラスターにブリッジするかを事前定義された条件に基づいて判定します。ルールでデータ処理操作が指定されている場合は、データ形式の変換や特定情報のフィルタリング、メッセージへの追加コンテキスト付加などが適用されます。
4. **SysKeeper Forwarder経由の転送**：ルールの結果はSysKeeper Forwarderを通じてSysKeeper分離装置を経由し、セキュリティゾーンIIIのEMQXに作成されたSysKeeper Proxyに送信され、セキュリティゾーンIIIにメッセージが取り込まれます。SysKeeper Proxyが利用できない場合、EMQXはメモリ内メッセージバッファを提供し、データ損失を防ぎます。データは一時的にバッファに保持され、メモリ過負荷を防ぐためにディスクにオフロードされることもあります。ただし、データ統合やEMQXノードが再起動されるとデータは保持されません。
5. **データの活用**：セキュリティゾーンIIIでは、MQTTメッセージが元の形式で再パブリッシュされ、ビジネスはルールエンジンやデータ統合を用いてさらに処理を行うことができます。

## はじめる前に

このセクションでは、DashboardでNari SysKeeperデータブリッジを作成する前に完了すべき準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- Nari SysKeeper装置の基本概念および動作原理の理解

### セキュリティゾーンIIIでNari SysKeeper Proxyを起動する

Nari SysKeeperを介してMQTTメッセージを送信するには、ゾーンIIIでSysKeeper Forwarderからの接続を受け付けるデータプロキシを有効にする必要があります。

ここでは、セキュリティゾーンIIIでNari SysKeeper Proxyを起動する方法を紹介します。

1. Dashboardにアクセスし、**Integration** -> **Connector**をクリックします。

2. ページ右上の**Create**をクリックし、**SysKeeper Proxy**を選択して**Next**をクリックします。

3. コネクターの名前を入力します。名前は大文字・小文字の英字または数字の組み合わせとしてください。例：`my_sysk_proxy`

4. **Listen Address**を`0.0.0.0:9002`に設定します。SysKeeper ProxyはTCPリスナーを起動します。ポートが他のプロセスで使用されていないこと、ファイアウォールがこのポートへのアクセスを許可していることを確認してください。

5. その他の設定はデフォルトのままにします。

6. **Create**ボタンをクリックします。

これでセキュリティゾーンIIIにNari SysKeeper Proxyが作成されました。次にNari SysKeeper Forwarderを作成します。

## コネクターの作成

このセクションでは、セキュリティゾーンI-IIでSysKeeper Proxyへの接続を転送するためのNari SysKeeper Forwarderコネクターの設定方法を説明します。

1. EMQX Dashboardにアクセスし、**Integration** -> **Connector**をクリックします。

2. ページ右上の**Create**をクリックし、**SysKeeper Forwarder**を選択して**Next**をクリックします。

3. コネクターの名前を入力します。名前は大文字・小文字の英字または数字の組み合わせとしてください。例：`my_sysk`

4. **Server**にSysKeeper Proxyサーバーのアドレスを設定します。例：`172.17.0.1:9002`

   - アドレス`172.17.0.1`はSysKeeperがセキュリティゾーンIII用に設定した仮想IPアドレスです。
   - ポート`9002`はセキュリティゾーンIIIのEMQX SysKeeper Proxyがリスニングしているポートです。

5. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがSysKeeper Proxyに接続できるかテストできます。

6. **Create**をクリックしてコネクターの作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてルールとSinkの作成に進みます。詳細は[ルールとSinkの作成](#create-a-rule-and-sink)を参照してください。

## SysKeeper Forwarder Sinkを用いたルールの作成

このセクションでは、EMQXでソースMQTTトピック`t/#`からのメッセージを処理し、処理結果を設定済みのSysKeeper Forwarder Sink経由で別のEMQXクラスターのSysKeeper Proxyに送信するルールの作成方法を示します。

1. EMQX Dashboardにアクセスし、**Integration -> Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. SQLエディターに以下のステートメントを入力します。これはトピックパターン`t/#`にマッチするMQTTメッセージを転送します。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   
   ```

   ::: tip

   初心者の方は、**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストが可能です。

   :::

5. + **Add Action**ボタンをクリックし、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをSysKeeperに送信します。

6. **Type of Action**ドロップダウンリストから`SysKeeper Forwarder`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能ですが、この例では新規Sinkを作成します。

7. Sinkの名前を入力します。名前は大文字・小文字の英字および数字の組み合わせとしてください。

8. **Connector**ドロップダウンから先ほど作成した`my_sysk`を選択します。

9. 以下の設定情報を入力します。

   - **Topic**：再パブリッシュするメッセージのトピック。プレースホルダーが使用可能です。例：`${topic}`
   - **QoS**：再パブリッシュするメッセージのQoS
   - **Message Template**：再パブリッシュするメッセージのペイロードテンプレート。プレースホルダーが使用可能です。例：`${payload}`

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **Create**をクリックしてSinkの作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

12. **Create Rule**ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいSysKeeper Forwarderが確認できます。

**Integration** -> **Flow Designer**をクリックするとトポロジーを確認できます。トピック`t/#`のメッセージがルール`my_rule`で解析された後、SysKeeper Forwarderを通じてパブリッシュされていることがわかります。

## ルールのテスト

Dashboardに組み込まれたWebSocketクライアントを使って、SysKeeper Forwarder Sinkとルールの動作をテストできます。

1. セキュリティゾーンIIIで、Dashboardの左ナビゲーションメニューから**Diagnose** -> **WebSocket Client**をクリックします。

2. 現在のEMQXインスタンスへの接続情報を入力します。

   - ローカルでEMQXを実行している場合はデフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要です。

3. **Connect**をクリックしてクライアントをEMQXインスタンスに接続します。

4. このクライアントでトピック`t/test`をサブスクライブします。

5. セキュリティゾーンI-IIで、同様の手順でパブリッシュ用クライアントを作成します。

6. パブリッシュエリアに以下を入力します。

   - **Topic**：`t/test`

   - **Payload**：

     ```json
     {
       "hello": "I am from the Security Zone I-II"
     }
     ```

   - **QoS**：`1`

7. **Publish**をクリックしてメッセージを送信します。

8. セキュリティゾーンIIIで、すべて正しく設定されていればクライアントがこのメッセージを受信します。
