# ワイルドカードサブスクライブ

MQTTのトピック名はメッセージルーティングに使用されるUTF-8エンコードされた文字列です。より柔軟にするために、MQTTは階層的なトピックネームスペースをサポートしています。トピックは通常、レベルごとに区切られ、スラッシュ `/` で区切られます。例として `chat/room/1` があります。[ワイルドカードサブスクライブ](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Topic_Names_and)とは、ワイルドカード文字を1つ以上含むトピックフィルターによるサブスクライブのことです。これにより、複数のトピック名にマッチするサブスクライブが可能になります。本ページでは、MQTTでサポートされている2種類のワイルドカードの使い方と、EMQXでワイルドカード文字を含むトピックをサブスクライブする方法を紹介します。

::: tip 注意

ワイルドカードはサブスクライブにのみ使用可能で、パブリッシュには使用できません。また、**多数のクライアントでワイルドカードサブスクライブを使用することは、パフォーマンスへの影響を避けるために控えてください。**

:::

## シングルレベルワイルドカード

`+`（U+002B）は、トピックの1レベルにのみマッチするワイルドカード文字です。シングルレベルワイルドカードは、トピックフィルターの任意のレベル（最初や最後のレベルも含む）で使用可能です。使用する場合、そのレベル全体を占める必要があります。トピックフィルター内で複数のレベルに使用でき、マルチレベルワイルドカードと組み合わせて使うことも可能です。以下はシングルレベルワイルドカードの使用例です。

```
"+" は有効
"sensor/+" は有効
"sensor/+/temperature" は有効
"sensor+" は無効（レベル全体を占めていないため）
```

クライアントがトピック `sensor/+/temperature` をサブスクライブすると、以下のトピックからのメッセージを受信します。

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

`#`（U+0023）は、トピックの任意の数のレベルにマッチするワイルドカード文字です。マルチレベルワイルドカードを使用する場合、そのレベル全体を占め、かつトピックの最後の文字でなければなりません。例を以下に示します。

```pgsql
"#" は有効で、すべてのトピックにマッチ
"sensor/#" は有効
"sensor/bedroom#" は無効（+ または # はワイルドカードレベルとしてのみ使用可能）
"sensor/#/temperature" は無効（# は最後のレベルでなければならない）
```

クライアントがトピック `sensor/#` をサブスクライブすると、以下のトピックからのメッセージを受信します。

```pgsql
sensor
sensor/temperature
sensor/1/temperature
```

## MQTTXクライアントでワイルドカードサブスクライブを試す

このセクションでは、MQTTXクライアントを使ってワイルドカードトピックのサブスクライブを作成する方法を示します。デモでは、1つのクライアント接続 `Demo` をパブリッシャーとしてメッセージをパブリッシュします。もう1つのクライアント接続をサブスクライバーとして作成し、以下のワイルドカードトピックをサブスクライブします。

- `testtopic/+/temperature`
- `testtopic/#`

:::tip 前提条件

- MQTTの[ワイルドカード](./mqtt-concepts.md#topic-and-wildcards)に関する知識
- [MQTTX](./publish-and-subscribe.md)を使った基本的なパブリッシュとサブスクライブ操作

:::

1. EMQXとMQTTX Desktopを起動します。**New Connection** をクリックして、パブリッシャー用のクライアント接続を作成します。

   - **Name** フィールドに `Demo` と入力します。
   - **Host** にローカルホストの `127.0.0.1` を入力します（本デモの例として）。
   - その他の設定はデフォルトのままにして、**Connect** をクリックします。

   ::: tip

   MQTT接続の作成に関する詳細な手順は、[MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop)で紹介しています。

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="新しい接続の一般設定" style="zoom:35%;" />

2. **Connections** ペインの **+** をクリックして、サブスクライバー用の別の接続を作成します。**Name** を `Subscriber` に設定します。

3. **Connections** で `Subscriber` を選択し、**+ New Subscription** をクリックします。ポップアップダイアログで、**Topic** フィールドに `testtopic/+/temperature` と入力します。他のオプションはデフォルトのままにします。

   <img src="./assets/wildcard-sub-1.png" alt="ワイルドカードサブスクライブ1" style="zoom: 38%;" />

4. **Connections** で `Demo` を選択します。トピックフィールドに `testtopic/room1/temperature` と入力し、メッセージフィールドにペイロード `28 degree` を入力します。送信ボタンをクリックします。同じペイロードでトピック `testtopic/room2/temperature` にもメッセージを送信します。

      <img src="./assets/wildcard-sub-2.png" alt="ワイルドカードサブスクライブ2" style="zoom:40%;" />

5. **Connections** で `Subscriber` を選択します。サブスクライバーがパブリッシャーから送信された異なるトピックの2つのメッセージを受信していることが確認できます。

      <img src="./assets/wildcard-sub-3.png" alt="ワイルドカードサブスクライブ3" style="zoom:40%;" />

6. **+ New Subscription** をクリックします。ポップアップダイアログで、デフォルトのトピック `testtopic/#` を **Topic** フィールドに使用します。他のオプションはデフォルトのままにします。

7. **Connections** で `Demo` を選択します。トピックフィールドに `testtopic/bedroom/room1/temperature` と入力し、メッセージフィールドにペイロード `28 degree` を入力します。送信ボタンをクリックします。

8. **Connections** で `Subscriber` を選択します。メッセージがサブスクライブされた `testtopic/#` にのみ送信されていることが確認できます。

      <img src="./assets/wildcard-sub-4.png" alt="ワイルドカードサブスクライブ4" style="zoom:40%;" />
