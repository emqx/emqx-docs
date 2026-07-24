# Auto Subscribe

Auto Subscribeは、EMQXがサポートする拡張されたMQTT機能です。**Auto Subscription**を有効にすると、ユーザーは複数のEMQXルールを設定できます。クライアントがEMQXに正常に接続されると、EMQXはクライアントのサブスクライブ処理を自動的に完了し、クライアントはもはや`SUBSCRIBE`リクエストを送信する必要がなくなります。

EMQX 5.0以前では、この機能は**Proxy Subscription**と呼ばれていました。

## DashboardでのAuto Subscribe設定

1. EMQXダッシュボードを開きます。左側のナビゲーションメニューで、**Management** -> **Auto Subscribe**をクリックします。

2. **Auto Subscribe**ページで、右上の**+ Add**ボタンをクリックします。

3. ポップアップダイアログで、**Topic**テキストボックスにテスト用トピック`a/1`を入力します。他の設定はデフォルトのままにします。

   - **Topic**: クライアントが自動的にサブスクライブするトピックを入力します。プレースホルダーを使って動的にトピックを構築できます。詳細は[Placeholders](#placeholders)を参照してください。

   - **QoS**: トピックのサービス品質を指定します。選択肢は`0`、`1`、`2`です。

   - **No local**: 選択肢は`False`または`True`です。

   - **Retain as Published**: 指定したトピックで送信されたメッセージを保持するかどうかを指定します。選択肢は`False`または`True`です。

   - **Retained Handling**: 選択肢は`0`、`1`、`2`です。

     <img src="./assets/config-auto-subscribe-dashboard.png" alt="Auto SubscribeのDashboard設定画面" style="zoom:35%;" />

   ダイアログボックスの**Add**ボタンをクリックします。これで自動サブスクライブトピック`a/1`が正常に作成されます。

   <img src="./assets/auto-sub-success.png" alt="Auto Subscribe設定成功画面" style="zoom:50%;" />

これでAuto Subscription機能が有効になりました。新しいサブスクライバーは、ブローカーに接続されると自動的にトピック`a/1`をサブスクライブします。

## MQTTX DesktopでAuto Subscriptionを試す

[DashboardでのAuto Subscribe設定](#dashboardでのauto-subscribe設定)でトピック`a/1`が自動サブスクライブトピックとして設定されています。以下の手順は、クライアントがブローカーに接続されると自動的にトピック`a/1`をサブスクライブする様子を示します。

:::tip 前提条件

[MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop)を使った基本的なパブリッシュとサブスクライブ操作の理解。

:::

1. EMQXとMQTTX Desktopを起動します。**New Connection**をクリックして、パブリッシャーとしてクライアント接続を作成します。

   - **Name**フィールドに`Demo`と入力します。
   - **Host**にローカルホスト`127.0.0.1`を入力します（このデモの例として）。
   - 他の設定はデフォルトのままにして、**Connect**をクリックします。

   ::: tip

   MQTT接続の作成に関する詳細な手順は[MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop)を参照してください。

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="新規接続設定画面" style="zoom:35%;" />

3. もう一つのMQTTクライアント接続を作成し、名前を`Subscriber`にします。

3. **Connections**ペインでクライアント`Demo`を選択し、トピックに`a/1`を入力してメッセージを送信します。

   - クライアント`Subscriber`は、新たにサブスクライブを作成しなくても自動的にメッセージを受信します。

   - クライアント`Demo`も新しい接続であるため、メッセージを受信します。

     ::: tip

     パブリッシュ／サブスクライブパターンでは、クライアントは送信者とサブスクライバーの両方になり得ます。

     :::

4. EMQXダッシュボードに戻り、左側のナビゲーションメニューから**Monitoring** -> **Subscriptions**をクリックします。トピック`a/1`に自動的にサブスクライブされた2つのサブスクリプションが表示されます。

   <img src="./assets/view-auto-sub-dashboard.png" alt="Auto Subscribeのサブスクリプション表示画面" style="zoom:50%;" />

## MQTTX CLIでAuto Subscriptionを試す

:::tip 前提条件

[MQTTX CLI](./publish-and-subscribe.md#mqttx-cli)を使った基本的なパブリッシュとサブスクライブ操作の理解。

:::

1. クライアントIDを`emqx_c`として新しい接続を作成します。

   ```bash
   mqttx conn -i emqx_c
   ```

2. EMQXダッシュボードに移動し、左側のナビゲーションメニューから**Monitoring** -> **Subscriptions**をクリックします。クライアント`emqx_c`がトピック`a/1`をサブスクライブしていることが表示されます。

   <img src="./assets/auto-sub-emqx_c.png" alt="emqx_cのAuto Subscribeサブスクリプション画面" style="zoom:60%;" />

## Placeholders

Auto Subscribeはプレースホルダーをサポートしており、トピックを動的に構築できます。プレースホルダーの形式は`${}`です。サポートされる変数は以下の通りです。

- `${clientid}`: クライアントID
- `${username}`: クライアントのユーザー名
- `${host}`: クライアントがEMQXに接続した際のIPアドレス

例えば、クライアントIDが`emqx_c`で、設定されたトピックが`a/${clientid}`の場合、クライアントはEMQXに接続後、自動的にトピック`a/emqx_c`をサブスクライブします。
