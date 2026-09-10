# ワイルドカードサブスクライブ

MQTTのトピック名は、メッセージルーティングに使用されるUTF-8エンコードされた文字列です。より柔軟性を持たせるために、MQTTは階層的なトピック名前空間をサポートしています。トピックは通常、レベルごとに区切られ、スラッシュ `/` で区切られます（例：`chat/room/1`）。[ワイルドカードサブスクライブ](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Topic_Names_and)とは、ワイルドカード文字を1つ以上含むトピックフィルターでのサブスクライブのことです。これにより、複数のトピック名にマッチするサブスクライブが可能になります。本ページでは、MQTTでサポートされている2種類のワイルドカードの使い方と、EMQXでワイルドカード文字を含むトピックにサブスクライブする方法を紹介します。

::: tip 注意

ワイルドカードはサブスクライブにのみ使用可能で、パブリッシュには使用できません。また、**多数のクライアントに対してワイルドカードサブスクライブを使用することは、パフォーマンスに影響を与える可能性があるため避けてください**。

:::

## シングルレベルワイルドカード

`+`（U+002B）は、トピックの1レベルにのみマッチするワイルドカード文字です。シングルレベルワイルドカードは、トピックフィルターの任意のレベル（最初や最後のレベルも含む）で使用できます。ただし、使用する場合はそのレベル全体を占めなければなりません。トピックフィルター内で複数のレベルにわたって使用可能で、マルチレベルワイルドカードと組み合わせて使うこともできます。以下はシングルレベルワイルドカードの使用例です。

```
"+" は有効
"sensor/+" は有効
"sensor/+/temperature" は有効
"sensor+" は無効（レベル全体を占めていない）
```

クライアントが `sensor/+/temperature` にサブスクライブすると、以下のトピックからのメッセージを受信します。

```awk
sensor/1/temperature
sensor/2/temperature
...
sensor/n/temperature
```

ただし、以下のトピックにはマッチしません。

```bash
sensor/temperature
sensor/bedroom/1/temperature
```

## マルチレベルワイルドカード

`#`（U+0023）は、トピックの任意のレベル数にマッチするワイルドカード文字です。マルチレベルワイルドカードを使用する場合は、そのレベル全体を占め、かつトピックの最後の文字でなければなりません。例を示します。

```pgsql
"#" は有効で、すべてのトピックにマッチ
"sensor/#" は有効
"sensor/bedroom#" は無効（+ または # はワイルドカードレベルとしてのみ使用可能）
"sensor/#/temperature" は無効（# は最後のレベルでなければならない）
```

クライアントが `sensor/#` にサブスクライブすると、以下のトピックからのメッセージを受信します。

```pgsql
sensor
sensor/temperature
sensor/1/temperature
```

## MQTTXクライアントでワイルドカードサブスクライブを試す

このセクションでは、MQTTXクライアントを使ってワイルドカードトピックにサブスクライブする方法を説明します。このデモでは、1つのクライアント接続 `Demo` をパブリッシャーとして作成し、メッセージをパブリッシュします。次に、別のクライアント接続をサブスクライバーとして作成し、以下のワイルドカードトピックにサブスクライブします。

- `testtopic/+/temperature`
- `testtopic/#`

:::tip 前提条件

- MQTTの[ワイルドカード](./mqtt-concepts.md#topic-and-wildcards)に関する知識
- [MQTTX](./publish-and-subscribe.md)を使った基本的なパブリッシュおよびサブスクライブ操作

:::

1. EMQXとMQTTX Desktopを起動します。**New Connection**をクリックして、パブリッシャー用のクライアント接続を作成します。

   - **Name** フィールドに `Demo` と入力します。
   - **Host** にローカルホストの `127.0.0.1` を入力します（このデモの例として）。
   - 他の設定はデフォルトのままにして、**Connect** をクリックします。

   ::: tip

   MQTT接続の作成方法の詳細は、[MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop)をご参照ください。

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="新規接続の一般設定" style="zoom:35%;" />

2. **Connections** ペインの **+** をクリックして、サブスクライバー用の別の接続を作成します。**Name** に `Subscriber` と入力します。

3. **Connections** で `Subscriber` を選択し、**+ New Subscription** をクリックします。ポップアップダイアログで、**Topic** フィールドに `testtopic/+/temperature` と入力します。他のオプションはデフォルトのままにします。

   <img src="./assets/wildcard-sub-1.png" alt="ワイルドカードサブスクライブ1" style="zoom: 38%;" />

4. **Connections** で `Demo` を選択します。トピックフィールドに `testtopic/room1/temperature` と入力し、メッセージフィールドにペイロード `28 degree` を入力します。送信ボタンをクリックします。同じペイロードでトピック `testtopic/room2/temperature` にもメッセージを送信します。

      <img src="./assets/wildcard-sub-2.png" alt="ワイルドカードサブスクライブ2" style="zoom:40%;" />

5. **Connections** で `Subscriber` を選択します。サブスクライバーがパブリッシャーから送信された異なるトピックの2つのメッセージを受信していることが確認できます。

      <img src="./assets/wildcard-sub-3.png" alt="ワイルドカードサブスクライブ3" style="zoom:40%;" />

6. **+ New Subscription** をクリックします。ポップアップダイアログで、デフォルトのトピック `testtopic/#` を **Topic** フィールドに入力します。他のオプションはデフォルトのままにします。

7. **Connections** で `Demo` を選択します。トピックフィールドに `testtopic/bedroom/room1/temperature` と入力し、メッセージフィールドにペイロード `28 degree` を入力します。送信ボタンをクリックします。

8. **Connections** で `Subscriber` を選択します。メッセージが `testtopic/#` のサブスクライブにのみ送信されていることが確認できます。

      <img src="./assets/wildcard-sub-4.png" alt="ワイルドカードサブスクライブ4" style="zoom:40%;" />
