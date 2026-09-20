# 排他サブスクライブ

排他サブスクライブは、EMQXがサポートする拡張されたMQTT機能です。これはトピックに対して相互排他的なサブスクライブを可能にします。つまり、あるトピックに対して同時にサブスクライブできるサブスクライバーは1つだけです。現在のサブスクライバーがサブスクライブを解除するまで、他のサブスクライバーは該当トピックにサブスクライブできません。

サブスクライブを排他にするには、トピックの先頭にプレフィックスを追加する必要があります。以下の表は例を示しています。

| 例 | プレフィックス | 実際のトピック名 |
| --------------- | ----------- | ------------ |
| $exclusive/t/1 | $exclusive/ | t/1 |

クライアント**A**が`$exclusive/t/1`にサブスクライブすると、他のクライアントは**A**が`$exclusive/t/1`のサブスクライブを解除するまで、`$exclusive/t/1`にサブスクライブできません。

::: tip

排他サブスクライブは必ず`$exclusive/`で始まる必要があります。上記の例では、他のクライアントは` t/1`を使って通常通りサブスクライブ可能です。

:::

## 設定ファイルで排他サブスクライブを有効化する

排他サブスクライブはデフォルトで無効になっています。設定ファイルで以下のように有効化できます。

```bash
mqtt.exclusive_subscription.enable = true
```

## MQTTX Desktopで排他サブスクライブを試す

::: tip 前提条件

- [MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop)を使った基本的なパブリッシュおよびサブスクライブ操作の理解
- 排他サブスクライブが有効になっていること

:::

1. EMQXとMQTTX Desktopを起動し、**New Connection**をクリックしてパブリッシャーとしてクライアント接続を作成します。

   - **Name**欄に`Demo`と入力します。
   - **Host**欄にローカルホストの`127.0.0.1`を入力します（このデモの例として）。
   - 他の設定はデフォルトのままにして**Connect**をクリックします。

   ::: tip

   MQTT接続の作成方法の詳細は[MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop)にて紹介しています。

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="新しい接続の一般設定" style="zoom:35%;" />

2. さらに2つのMQTT接続を作成し、それぞれ`Subscriber1`、`Subscriber2`と設定します。

3. **Connections**ペインで`Subscriber1`を選択し、**New Subscription**ボタンをクリックしてサブスクライブを作成します。**Topic**テキストボックスに`$exclusive/t/1`と入力してこのトピックにサブスクライブします。**Confirm**をクリックします。

   <img src="./assets/subscribe-exclusive-topic.png" alt="排他トピックへのサブスクライブ" style="zoom:35%;" />

4. **Connections**ペインで`Subscriber2`を選択し、同様に**New Subscription**ボタンをクリックしてサブスクライブを作成します。**Topic**に`$exclusive/t/1`と入力してサブスクライブします。**Confirm**をクリックします。

   - エラーメッセージが表示されます。

   <img src="./assets/fail-to-exclusive-subscription.png" alt="排他サブスクライブ失敗" style="zoom:35%;" />

## MQTTX CLIで排他サブスクライブを試す

::: tip 前提条件

- [MQTTX CLI](./publish-and-subscribe.md#mqttx-cli)を使った基本的なパブリッシュおよびサブスクライブ操作の理解
- 排他サブスクライブが有効になっていること

:::

1. 以下のコマンドで排他サブスクライブを行います。

   ```bash
   mqttx sub -t "$exclusive/t/1"
   ```

2. もう一度ステップ1のコマンドを使って同じトピック`$exclusive/t/1`にサブスクライブしようとすると、以下が返されます。

   ```bash
   subscription negated to t/2 with code 135
   ```

   排他サブスクライブのエラーコード一覧：

   | コード | 理由                                                    |
   | ---- | --------------------------------------------------------- |
   | 0x8F | 排他サブスクライブが有効でない状態で`$exclusive/`を使用した。 |
   | 0x97 | 既に他のクライアントがこのトピックにサブスクライブしている。    |
