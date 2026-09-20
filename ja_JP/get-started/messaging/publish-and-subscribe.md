# MQTTクライアントによるテスト

リアルタイムデバイスをEMQXに接続しIoTアプリケーションを開発する前に、クライアントツールを使ってEMQXのメッセージングサービスをテストすることは、より安全かつ効率的です。

EMQXをローカルにデプロイする前でも、EMQが提供する無料のオンライン公開[MQTTブローカー](https://www.emqx.com/en/mqtt/public-mqtt5-broker)およびMQTTクライアントツールを検証ツールとして活用し、MQTTメッセージングサービスやアプリケーション開発の簡単なテストを行うことができます。

<img src="./assets/public-borker.png" alt="公開ブローカー" style="zoom:45%;" />

本セクションでは、よく使われるMQTT 5.0クライアントツールを紹介し、以下のメッセージングサービスをテストするための簡単なデモを提供します。

- クライアント接続の確立
- トピックのサブスクライブ
- メッセージのパブリッシュ
- メッセージの受信と表示

## MQTTX

[MQTTX](https://mqttx.app)は、EMQがオープンソースで提供する洗練されたクロスプラットフォームMQTT 5.0検証ツールです。以下の3種類のツールが含まれます。

- MQTTX クライアント
- MQTTX CLI
- MQTT Web

### MQTTX デスクトップ

[MQTTX デスクトップ](https://mqttx.app)はクロスプラットフォーム対応のMQTTデスクトップクライアントツールです。使いやすいグラフィカルインターフェースを提供し、ユーザーが迅速にMQTT接続を作成し、MQTTメッセージのパブリッシュ／サブスクライブをテストできます。

テストを始める前に、MQTTXクライアントをダウンロードしてインストールしてください。

1. ご利用のOSに応じて、アプリケーションストアまたは[MQTTX公式サイト](https://mqttx.app/)からインストールパッケージをダウンロードします。
2. MQTTXクライアントをインストールします。詳細な手順は[MQTTX - インストール](https://mqttx.app/docs/downloading-and-installation)をご参照ください。

以下の手順に従い、MQTTXデスクトップクライアントを使った簡単なテストを行います。

1. MQTTXクライアントを起動し、**New Connection**をクリックしてMQTT接続を作成します。

2. メッセージをパブリッシュするクライアントとして新規接続を設定します。

   **General**セクションでクライアントの一般情報を入力します。

   - **Name**: 接続の`Name`を入力します。
   - **Client ID**: デフォルトのままにします。クライアント接続の一意識別子で、更新ボタンを押すと自動生成されます。
   - **Host**: 使用するプロトコルを選択します。`mqtt://`または`ws://`を選択してください。`SSL/TLS`認証接続の場合は`mqtts://`または`wss://`を選択します。ホストIPアドレスはデフォルトで`broker.emqx.io`となっており、公開ブローカーに接続します。ご自身のEMQXを使用する場合は実際のIPに置き換えてください。
   - **Port**: 選択したプロトコルに対応するポート番号を入力します。
   - **Username**および**Password**: ブローカーでユーザー認証が有効な場合は入力し、無ければ空欄のままにします。
   - **SSL/TLS**: `SSL/TLS`認証接続の場合はトグルボタンをクリックして有効にします。

   その他の設定はデフォルトのままにし、右上の**Connect**ボタンをクリックします。

   <img src="./assets/New-connection-fill-parameters.png" alt="新規接続パラメータ入力" style="zoom:35%;" />

3. 接続成功後、テキストボックスにトピック名`test`を入力し、スクリーンショットのようにメッセージを作成します。送信ボタンをクリックすると、トピック`test`にメッセージが表示されます。

   <img src="./assets/Publish-test-message.png" alt="テストメッセージのパブリッシュ" style="zoom:35%;" />

4. **Connections**ペインの**+** -> **New Connection**をクリックし、メッセージを受信するクライアントとして新規接続を作成します。名前を`Subscriber`に設定し、その他の一般接続設定はクライアント`Demo`と同じにします。

5. **Connections**ペインでクライアント`Subscriber`を選択し、**+ New Subscription**をクリックします。

   - **Topic**: テキストボックスに`test`を入力します。
   - **QoS**: デフォルト値のままにします。
   - **Color**: サブスクリプションを識別するための色を選択できます。

   その他のオプションは空欄のままにし、**Confirm**ボタンをクリックします。

   <img src="./assets/Subscribe-test-topic.png" alt="テストトピックのサブスクライブ" style="zoom:35%;" />

6. **Connections**ペインでクライアント`Demo`を選択し、トピック`test`に新しいメッセージをパブリッシュします。クライアント`Subscriber`が新しいメッセージを受信するのが確認できます。

   <img src="./assets/Receive-test-again-message.png" alt="メッセージ受信の確認" style="zoom:35%;" />

これでMQTTXクライアントを使った基本的なパブリッシュとサブスクライブの操作を試しました。詳細かつ高度な操作については[MQTTX - パブリッシュとサブスクリプション](https://mqttx.app/docs/get-started#publish-and-subscription)をご覧ください。

### MQTTX CLI

[MQTTX CLI](https://mqttx.app/cli)は、EMQが提供するオープンソースのMQTT 5.0コマンドラインツールです。グラフィカルインターフェースを必要とせず、コマンドライン上でMQTTサービスやアプリケーションのテスト・デバッグが可能です。

以下の手順に従い、MQTTX CLIで接続、パブリッシュ／サブスクライブ、メッセージの表示を行います。

1. MQTT CLIをダウンロードしてインストールします。ここではmacOSを例に示します。その他のOSは[MQTTX CLI - インストール](https://mqttx.app/docs/cli/downloading-and-installation)をご参照ください。

   ```bash
   # Homebrew
   brew install emqx/mqttx/mqttx-cli
   # Intelチップ
   curl -LO https://www.emqx.com/zh/downloads/MQTTX/v1.9.0/mqttx-cli-macos-x64
   sudo install ./mqttx-cli-macos-x64 /usr/local/bin/mqttx
   # Apple Silicon
   curl -LO https://www.emqx.com/zh/downloads/MQTTX/v1.9.0/mqttx-cli-macos-arm64
   sudo install ./mqttx-cli-macos-arm64 /usr/local/bin/mqttx
   ```

2. コマンドラインツールで以下のコマンドを実行し、EMQXに接続して`testtopic/#`トピックをサブスクライブします。

   ```shell
   mqttx sub -t 'testtopic/#' -q 1 -h 'localhost' -p 1883 'public' -v
   ```

   パラメータの説明:

   - `-t`: サブスクライブするトピック
   - `-q`: メッセージのQoS（デフォルト: 0）
   - `-h`: リスナーのIPアドレス（デフォルト: `localhost`）
   - `-p`: ブローカーのポート（デフォルト: `1883`）
   - `-v`: メッセージの前にトピックを表示

   実行成功後、コマンドラインは受信待ち状態となり、メッセージ受信時に内容を表示します。

   さらに詳しいパラメータについては[MQTTX CLI - サブスクライブ](https://mqttx.app/docs/cli/get-started#subscribe)をご覧ください。

3. 新しいコマンドラインウィンドウを開き、以下のコマンドを実行してEMQXに接続し、トピック`testtopic/#`にメッセージをパブリッシュします。

   ```bash
   mqttx pub -t 'testtopic/1' -q 1 -h 'localhost' -p 1883 -m 'from MQTTX CLI'
   ```

   パラメータ:

   - `-t`: パブリッシュするトピック
   - `-q`: メッセージのQoS（デフォルト: 0）
   - `-h`: リスナーのIPアドレス（デフォルト: `localhost`）
   - `-p`: ブローカーのポート（デフォルト: `1883`）
   - `-m`: メッセージ本文

   実行成功後、コマンドラインは接続を確立しメッセージをパブリッシュしてからブローカーとの接続を切断します。ステップ2のコマンドラインウィンドウには以下のメッセージが表示されます。

   ```bash
   topic:  testtopic/1
   payload:  from MQTTX CLI
   ```

   さらに詳しいパラメータについては[MQTTX CLI - パブリッシュ](https://mqttx.app/docs/cli/get-started#publish)をご覧ください。

### MQTTX Web

[MQTTX Web](https://mqttx.app/web)はブラウザベースのMQTT 5.0 WebSocketクライアントツールです。ダウンロードやインストール不要で、WebSocket経由のMQTT開発やデバッグを完結できます。MQTTX Webでのテスト操作は[MQTTX クライアント](#mqttx-デスクトップ)と基本的に同じです。

<img src="./assets/mqtt-x-web.png" alt="MQTTX Web" style="zoom:35%;" />

## ダッシュボード WebSocket

[EMQX ダッシュボード](../../guides/dashboard/introduction.md)は、迅速かつ効果的なMQTTテストツールとしてWebSocketクライアントを提供しています。MQTT over WebSocketを利用し、EMQXへの接続、トピックのサブスクライブ、メッセージのパブリッシュをテストできます。

1. EMQXダッシュボードの左ナビゲーションメニューで**Diagnose** -> **WebSocket Client**をクリックします。

2. **Connection**セクションに接続情報を入力します。

   - **Host**: 対応するIPアドレスを入力します（デフォルト: `localhost`）。
   - **Port**: デフォルトのポート`8083`のままにします。
   - **Username**および**Password**: ある場合は入力し、アクセス制御がない場合は空欄のままにします。

   その他の設定はデフォルトのままにします。

3. **Connect**ボタンをクリックして接続を確立します。

4. **Subscription**セクションでサブスクライブするトピックを`testtopic/#`に設定し、**Subscribe**ボタンをクリックしてサブスクリプションを完了します。トピック`testtopic/#`が下のテーブルに追加されます。

   <img src="./assets/Dashboard-Websocket-Client.png" alt="ダッシュボード WebSocket クライアント" style="zoom:60%;" />

   サブスクライブ後、該当トピックにマッチするすべてのメッセージがこの接続に転送されます。

5. **Publish**セクションでパブリッシュするメッセージのトピックを設定します。

   - **Topic**: `testtopic/1`に設定します（ワイルドカード`+`や`#`は非対応）。
   - **Payload**: `{"msg": 'Hello"}`に設定します。
   - **QoS**: デフォルト値`0`のままにします。
   - **Retain**: メッセージをリテインしたい場合はチェックボックスを選択します。リテインメッセージの詳細は[リテインメッセージ](./mqtt-concepts.md)をご覧ください。

   **Publish**ボタンをクリックすると、**Published**セクションに1件のレコードが追加されます。メッセージはすべてのサブスクライバーにルーティングされます。このテストではパブリッシャー自身も受信者のため、**Received**セクションにも新しいレコードが追加されます。

   <img src="./assets/Dashboard-Websocket-Client-receive.png" alt="ダッシュボード WebSocket クライアント メッセージ受信" style="zoom:50%;" />
