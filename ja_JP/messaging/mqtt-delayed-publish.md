# 遅延パブリッシュ

遅延パブリッシュは、EMQXがサポートする拡張されたMQTT機能です。クライアントがトピックプレフィックス `$delayed/{DelayInterval}` を付けてEMQXにメッセージをパブリッシュすると、遅延パブリッシュ機能がトリガーされます。メッセージはユーザーが事前に定義した時間経過後にパブリッシュされます。

遅延パブリッシュのトピックの具体的な形式は以下の通りです。

```bash
$delayed/{DelayInterval}/{TopicName}
```

- `$delayed`：`$delayed`で始まるメッセージは遅延が必要なメッセージとして扱われます。遅延時間は次のトピックレベルの内容で決まります。
- `{DelayTime}`：このMQTTメッセージのパブリッシュを遅延させる時間間隔またはタイムスタンプを秒単位で指定します。時間間隔の場合、最大許容間隔は42949669秒（約497日）です。タイムスタンプの場合は、現在のシステム時刻から42949669秒以内でなければなりません。`{DelayTime}`が整数にパースできないか、有効範囲外の場合はメッセージは破棄されます。
- `{TopicName}`：MQTTメッセージのトピック名です。

例：

- `$delayed/15/x/y`：15秒後にトピック `x/y` にMQTTメッセージをパブリッシュする
- `$delayed/60/a/b`：1分後にトピック `a/b` にMQTTメッセージをパブリッシュする
- `$delayed/1743490800/chat/id`：2025年4月1日9:00（ストックホルム時間）にトピック `chat/id` にメッセージをパブリッシュする
- `$delayed/3600/$SYS/topic`：1時間後にトピック `$SYS/topic` にMQTTメッセージをパブリッシュする

## ダッシュボードで遅延パブリッシュを設定する

1. EMQXダッシュボードを開きます。左のナビゲーションメニューで **Management** -> **Delayed Publish** をクリックします。

2. **Delayed Publish** ページで以下の設定が可能です。

   - **Enable**：遅延パブリッシュの有効化または無効化。デフォルトでは有効です。
   - **Max Delayed Messages**：遅延メッセージの最大数を指定できます。
   

<img src="./assets/configure-delayed-publish-dashboard.png" alt="遅延パブリッシュのダッシュボード設定" style="zoom:45%;" />

## MQTTX Desktopで遅延パブリッシュを試す

:::tip 前提条件

[MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop) を使った基本的なパブリッシュおよびサブスクライブ操作

:::

1. EMQXとMQTTX Desktopを起動します。**New Connection** をクリックしてパブリッシャーとしてクライアント接続を作成します。

   - **Name** フィールドに `Demo` と入力します。
   - **Host** にローカルホスト `127.0.0.1` を入力します（このデモの例として）。
   - 他の設定はデフォルトのままにして **Connect** をクリックします。

   ::: tip

   MQTT接続の作成に関する詳細な手順は [MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop) を参照してください。

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="新規接続の一般設定" style="zoom:35%;" />

2. もう一つMQTT接続を作成し、サブスクライバーとして設定します。

3. **Connections** ペインで `Demo` の接続を選択し、トピックテキストボックスに `$delayed/10/x/y` と入力し、メッセージに `Delayed Message` と入力します。

   - `$delayed`：遅延メッセージであることを示します。
   - `10`：遅延間隔が10秒であることを示します。
   - `x/y`：メッセージのトピック名を示します。

4. **Connections** ペインで `Subscriber` の接続を選択します。**New Subscription** ボタンをクリックしてサブスクリプションを作成します。**Topic** テキストボックスに `x/y` と入力してこのトピックをサブスクライブします。**Confirm** をクリックします。

   <img src="./assets/subscribe-delayed-message.png" alt="遅延メッセージのサブスクライブ" style="zoom:35%;" />

5. **Connections** ペインで `Demo` の接続を選択し、送信ボタンをクリックしてトピック `$delayed/10/x/y` で `Delayed Message` を送信します。

6. 10秒待ちます。`Subscriber` の接続が10秒後に遅延メッセージを受信するのが確認できます。

   <img src="./assets/receive-delayed-message.png" alt="遅延メッセージの受信" style="zoom:35%;" />

## MQTTX CLIで遅延パブリッシュを試す

::: tip 前提条件

[MQTTX CLI](./publish-and-subscribe.md#mqttx-cli) を使った基本的なパブリッシュおよびサブスクライブ操作

:::

1. サブスクライバーとして新しい接続を作成し、トピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. ターミナルで新しいウィンドウを開き、パブリッシャーとして以下のコマンドを使って遅延メッセージを送信します。サブスクライバーは5秒後にメッセージを受信します。

   ```bash
   mqttx pub -t "\$delayed/5/t/1" -m "Hello Delayed msg"
   ```
