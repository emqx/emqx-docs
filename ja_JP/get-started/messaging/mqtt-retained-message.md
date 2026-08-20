# MQTT Retained Message

EMQX は MQTT のリテインドメッセージ機能を実装しています。特定のトピックでパブリッシュされたメッセージを `Retained` としてフラグを立て、EMQX 上に永続的なメッセージとして保存できます。新しいサブスクライバーがリテインドメッセージのトピックにサブスクライブすると、そのメッセージがサブスクライブ前にパブリッシュされていても、即座に受信できます。

クライアントツールを使って EMQX に接続し、このメッセージングサービスを試すことができます。本節では、[MQTTX Desktop](https://mqttx.app/) と [MQTTX CLI](https://mqttx.app/cli) を使ってクライアントをシミュレートし、リテインドメッセージのパブリッシュと受信の動作を確認する方法を紹介します。

:::tip 前提条件

- MQTT の [Retained Message](./mqtt-concepts.md) に関する知識
- [MQTTX](./publish-and-subscribe.md) を使った基本的なパブリッシュおよびサブスクライブ操作

:::

## MQTTX Desktop でリテインドメッセージをパブリッシュする

1. EMQX と MQTTX Desktop を起動します。**New Connection** をクリックして、パブリッシャーとしてクライアント接続を作成します。

   - **Name** フィールドに `Demo` と入力します。
   - **Host** に localhost の `127.0.0.1` を入力します（このデモの例として使用）。
   - 他の設定はデフォルトのままにして、**Connect** をクリックします。

   ::: tip

   MQTT 接続の作成方法の詳細は、[MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop) をご覧ください。

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="新しい接続の設定" style="zoom:35%;" />

3. 接続に成功したら、テキストボックスにトピック `sensor/t1` を入力し、スクリーンショットのようにメッセージペイロードを作成します。送信ボタンをクリックします。トピック `sensor/t1` へのメッセージがメッセージダイアログに表示されます。

   <img src="./assets/Publish-message-1.png" alt="メッセージのパブリッシュ 1" style="zoom:35%;" />

4. トピック `sensor/t2` でリテインドメッセージを2つパブリッシュします。

   - 1つ目のメッセージに `1` を入力し、**Retain** を選択して送信ボタンをクリックします。
   - 2つ目のメッセージに `2` を入力して送信ボタンをクリックします。

   <img src="./assets/Publish-message-2.png" alt="メッセージのパブリッシュ 2" style="zoom:35%;" />

5. **Connections** ペインで **+** -> **New Connection** をクリックし、メッセージを受信するクライアントとしてサブスクライバー `Subscriber` を作成します。

5. **+ New Subscription** をクリックし、トピック `sensor/+` をサブスクライブします。**Confirm** ボタンをクリックします。

   :::tip

   トピックを `sensor/+` に設定すると、`sensor/t1` と `sensor/t2` の両方がサブスクライブされます。トピックとワイルドカードの詳細は、[Understanding MQTT Topics & Wildcards by Case](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics) をご参照ください。

   :::

   クライアント `Subscriber` は、トピック `sensor/t1` の最初のメッセージや、トピック `sensor/t2` の最初のリテインドメッセージは受信せず、最新のリテインドメッセージのみ受信することがわかります。これは EMQX が各トピックの最新リテインドメッセージのみを保存するためです。

   <img src="./assets/Receive-retained-message.png" alt="リテインドメッセージの受信" style="zoom:35%;" />

これで MQTTX クライアントを使ったリテインドメッセージの送信を試しました。EMQX ダッシュボードから保存されている最新のリテインドメッセージを確認することもできます。詳細は [View Retained Message in Dashboard](#view-retained-message-in-dashboard) をご覧ください。

## MQTTX CLI でリテインドメッセージをパブリッシュする

1. クライアントで接続要求を開始します。

1. 以下のコマンドを使ってリテインドメッセージをパブリッシュします。トピックを `t/1`、ペイロードを `A retained message from MQTTX CLI`、`retain = true` に設定します：

   ```bash
   mqttx pub -t 't/1' -m 'A retained message from MQTTX CLI' --retain true -h 'localhost' -p 1883
   ```

3. 同じブローカーに別の新しいクライアント接続要求を開始し、新しいクライアントでトピック `t/1` をサブスクライブします。リテインドメッセージを受信します。

   新しいクライアントを継続的に作成してトピック `t/1` をサブスクライブさせると、作成したすべての新しいクライアントがリテインドメッセージを受信します。

   ```bash
   $ mqttx sub -t 't/1' -h 'localhost' -p 1883 -v
   topic:  t/1
   payload:  A retained message from MQTTX CLI
   retain: true
   ```

3. 空のメッセージをパブリッシュしてリテインドメッセージをクリアします：

   ```bash
   mqttx pub -t 't/1' -m '' --retain true -h 'localhost' -p 1883
   ```

4. 新しいクライアント接続を開始し、トピック `t/1` をサブスクライブします。リテインドメッセージが受信されず、リテインドメッセージがクリアされたことを示します。

## ダッシュボードでリテインドメッセージを確認する

リテインドメッセージをパブリッシュすると、EMQX はこのメッセージをシステム内に保存します。リテインドメッセージの一覧ページでこのメッセージを確認できます。このリテインドメッセージのトピックにサブスクライブすると、EMQX はこのメッセージをトピックにパブリッシュし、即座に受信できます。

リテインドメッセージのデフォルトの有効期限は無期限で、手動で削除しない限り失効しません。

### リテインドメッセージ一覧

**Monitoring** -> **Retained Messages** ページでは、システム内のすべてのリテインドメッセージをトピック、QoS、パブリッシュ時間、クライアントIDとともに確認できます。検索ボックスで検索によるフィルタリングが可能で、トピックワイルドカードにも対応しています。

また、**Show Payload** ボタンでリテインドメッセージのペイロードを表示し、**Delete** ボタンで削除できます。**Refresh** ボタンで一覧を更新し、**Settings** ボタンからリテインドメッセージの設定ページにアクセスできます。

デフォルトで以下の3種類のリテインドメッセージが [システムトピック](./mqtt-concepts.md) から確認できます：

- $SYS/brokers/+/sysdescr: 現在の EMQX ノードのシステム説明
- $SYS/brokers/+/version: 現在の EMQX ノードのバージョン番号
- $SYS/brokers: 現在の EMQX のすべてのノードの数と名前

<img src="./assets/retained-messages.png" alt="リテインドメッセージ一覧" style="zoom:67%;" />

### リテインドメッセージの削除

EMQX でリテインドメッセージを削除するには、クライアントでリテインドメッセージのトピックに空のメッセージをパブリッシュするか、EMQX ダッシュボードを使用します。ダッシュボードでは、特定のリテインドメッセージの **Delete** ボタンをクリックして削除できます。クラスター内のすべてのリテインドメッセージを削除するには、**Clear All** ボタンを使用します。

さらに、リテインドメッセージの有効期限をリテインドメッセージ設定ページで設定でき、有効期限切れにより EMQX が自動的に削除することも可能です。
