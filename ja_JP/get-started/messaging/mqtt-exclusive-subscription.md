# Exclusive Subscription

Exclusive Subscription は、EMQX がサポートする拡張された MQTT 機能です。これはトピックに対して相互排他的なサブスクライブを可能にします。つまり、あるトピックに対して同時にサブスクライブできるのは一人のサブスクライバーのみであり、現在のサブスクライバーがそのサブスクリプションを解除するまで、他のサブスクライバーは同じトピックにサブスクライブできません。

サブスクリプションを排他的にするには、トピックの先頭にプレフィックスを追加する必要があります。以下の表は例を示しています。

| 例 | プレフィックス | 実際のトピック名 |
| --------------- | ----------- | ------------ |
| $exclusive/t/1 | $exclusive/ | t/1 |

クライアント **A** が `$exclusive/t/1` にサブスクライブすると、**A** が `$exclusive/t/1` のサブスクリプションを解除するまで、他のクライアントは `$exclusive/t/1` にサブスクライブできません。

::: tip

Exclusive Subscription は必ず `$exclusive/` プレフィックスを付ける必要があります。上記の例では、他のクライアントは `t/1` に対しては通常通りサブスクライブ可能です。

:::

## 設定ファイルで Exclusive Subscription を有効化する

Exclusive Subscription はデフォルトで無効になっています。設定ファイルでこの機能を有効にできます。

```bash
mqtt.exclusive_subscription.enable = true
```

## MQTTX Desktop で Exclusive Subscription を試す

::: tip 前提条件

- [MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop) を使った基本的なパブリッシュおよびサブスクライブ操作ができること
- Exclusive Subscription が有効になっていること

:::

1. EMQX と MQTTX Desktop を起動します。**New Connection** をクリックして、パブリッシャーとしてクライアント接続を作成します。

   - **Name** フィールドに `Demo` と入力します。
   - **Host** にローカルホストの `127.0.0.1` を入力します（このデモの例として）。
   - 他の設定はデフォルトのままにして **Connect** をクリックします。

   ::: tip

   MQTT 接続の作成方法については、[MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop) に詳しい説明があります。

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="新規接続の一般設定" style="zoom:35%;" />

2. さらに2つの MQTT 接続を作成し、それぞれ `Subscriber1` と `Subscriber2` として設定します。

3. **Connections** ペインで `Subscriber1` を選択し、**New Subscription** ボタンをクリックしてサブスクリプションを作成します。**Topic** テキストボックスに `$exclusive/t/1` と入力してこのトピックをサブスクライブします。**Confirm** をクリックします。

   <img src="./assets/subscribe-exclusive-topic.png" alt="排他的トピックのサブスクライブ" style="zoom:35%;" />

4. 同様に **Connections** ペインで `Subscriber2` を選択し、**New Subscription** ボタンをクリックしてサブスクリプションを作成します。**Topic** テキストボックスに `$exclusive/t/1` と入力してこのトピックをサブスクライブします。**Confirm** をクリックします。

   - エラーメッセージが表示されます。

   <img src="./assets/fail-to-exclusive-subscription.png" alt="排他的サブスクリプション失敗" style="zoom:35%;" />

## MQTTX CLI で Exclusive Subscription を試す

::: tip 前提条件

- [MQTTX CLI](./publish-and-subscribe.md#mqttx-cli) を使った基本的なパブリッシュおよびサブスクライブ操作ができること
- Exclusive Subscription が有効になっていること

:::

1. 以下のコマンドで排他的サブスクリプションを行います。

   ```bash
   mqttx sub -t "$exclusive/t/1"
   ```

2. ステップ1と同じコマンドを再度実行し、トピック `$exclusive/t/1` に別のサブスクリプションを試みます。以下のように返されます。

   ```bash
   subscription negated to t/2 with code 135
   ```

   Exclusive Subscription のエラーコード一覧：

   | コード | 理由                                                    |
   | ---- | --------------------------------------------------------- |
   | 0x8F | Exclusive Subscription が有効でない状態で `$exclusive/` を使用した。 |
   | 0x97 | すでに別のクライアントがこのトピックにサブスクライブしている。     |
