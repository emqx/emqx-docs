---
description: この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。
---

# EMQXを始める

EMQXは、世界で最もスケーラブルで信頼性の高いMQTTメッセージングプラットフォームであり、ビジネスデータをリアルタイムで確実に接続、移動、処理するのに役立ちます。このオールインワンのMQTTプラットフォームを使えば、重要なビジネスインパクトをもたらすIoTアプリケーションを簡単に構築できます。

この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。

::: tip
このクイックスタートガイドで紹介するデプロイ方法のほかに、IoT向けのフルマネージドMQTTサービスであるEMQX Cloudもぜひお試しください。インフラのメンテナンスが不要で、[アカウント登録](https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2Fnew)を行うだけで、すぐにMQTTサービスを開始し、IoTデバイスを任意のクラウドに接続できます。
:::

## EMQXのインストール

EMQXは、[Docker](../deploy/install-docker.md)で実行するか、[EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator)でインストールするか、またはダウンロードパッケージを使ってコンピューターや仮想マシン（VM）にインストールできます。ダウンロードパッケージでのインストールを選択した場合、現在以下のオペレーティングシステムがサポートされています。

- RedHat
- CentOS
- RockyLinux
- AmazonLinux
- Ubuntu
- Debian
- macOS
- Linux

上記にないプラットフォームについては、[EMQ](https://www.emqx.com/en/contact)までお問い合わせください。

### Dockerを使ったEMQXのインストール

コンテナデプロイは、EMQXを素早く試す最も簡単な方法です。このクイックスタートガイドでは、Dockerを使ったEMQXのインストールと起動方法を紹介します。

1. 最新バージョンのEMQXをダウンロードして起動するには、以下のコマンドを入力してください。

   実行前に[Docker](https://www.docker.com/)がインストールされ、起動していることを確認してください。

   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:latest
   ```

2. Webブラウザを起動し、アドレスバーに `http://localhost:18083/`（`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントへの接続や稼働状況の確認ができます。

   デフォルトのユーザー名とパスワードは以下の通りです。

   `admin`

   `public`

### インストールパッケージを使ったEMQXのインストール

コンピューターやVMにインストールパッケージを使ってEMQXをインストールし、設定の調整やパフォーマンスチューニングを簡単に行うことも可能です。以下の手順はmacOS 15（Sequoia）およびarm64アーキテクチャ（Apple Silicon）を例に説明しています。

::: tip

すべてのランタイム依存関係を考慮すると、テストやホットアップグレードにはインストールパッケージの利用を推奨しますが、本番環境での使用は**推奨しません**。

:::

1. [公式ダウンロードサイトのmacOSタブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=macOS)にアクセスします。

2. 最新バージョン `@EE_VERSION@` を選択し、**Package Type** から `macOS 15 arm64 / zip` を選びます。

3. パッケージをダウンロードしてインストールします。ページ内のコマンド説明も参考にしてください。

4. EMQXを起動するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx foreground
   ```
   これで対話型シェルでEMQXが起動します。シェルを閉じるとEMQXも停止します。
   または（推奨しませんが）、以下のコマンドでバックグラウンド起動も可能です。

   ```bash
   ./emqx/bin/emqx start
   ```

5. Webブラウザを起動し、アドレスバーに `http://localhost:18083/`（`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントへの接続や稼働状況の確認ができます。

   デフォルトのユーザー名とパスワードは `admin` と `public` です。ログイン後にパスワード変更を求められます。

6. EMQXを停止するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx stop
   ```

テスト終了後にEMQXをアンインストールするには、EMQXフォルダーを削除してください。

## MQTTXで接続を検証する

EMQXの起動に成功したら、MQTTXを使って接続とメッセージサービスのテストを続けられます。

[MQTTX](https://mqttx.app)は、macOS、Linux、Windowsで動作する洗練されたクロスプラットフォームのMQTT 5.0デスクトップクライアントです。ユーザーはチャットスタイルのUIを通じて簡単に接続を作成し、複数のクライアントを保存できます。また、MQTT/MQTTS接続のテストやメッセージのサブスクライブ・パブリッシュが可能です。

この節では、ブラウザベースのMQTT 5.0 WebSocketクライアントツールである[MQTTX Web](https://mqttx.app/web)を使った接続検証方法を紹介します。アプリのダウンロードやインストールは不要です。

::: tip 前提条件
接続テスト前に以下の情報を準備してください。

- **EMQXアドレス**：通常はサーバーのIPアドレス
- **ポート**：ダッシュボードの左ナビゲーションメニューから **Management** -> **Listeners** をクリックして確認
:::

### 接続の作成

1. [MQTTX Web](https://mqttx.app/web-client#/recent_connections)にアクセスします。

2. MQTT接続の設定と確立を行います。**+ New Connection** ボタンをクリックして設定ページを開きます。

   - **Name**：接続名を入力します。例：`MQTTX_Test`

   - **Host**

     - プロトコルタイプをドロップダウンリストから選択します。WebSocketプロトコルを使用する場合は `ws://` を選択してください。MQTTX WebはWebSocketプロトコルのみ対応しています。SSL/TLS接続をテストする場合は、[MQTTXデスクトップクライアント](https://mqttx.app/)をダウンロードしてください。
     - EMQXアドレスを入力します。例：`emqx@127.0.0.1`

   - **Port**：例としてWebSocketプロトコル用の `8083`

   他の項目はデフォルトのままか、ビジネス要件に応じて設定してください。各項目の詳細は[MQTTユーザーマニュアル - Connect](https://mqttx.app/docs/get-started)をご参照ください。

3. 画面右上の **Connect** ボタンをクリックします。

4. メッセージのパブリッシュ／受信をテストします。チャットエリア右下の送信アイコンをクリックすると、送信に成功したメッセージがチャットウィンドウに表示されます。

### トピックのパブリッシュとサブスクライブ

接続が成功したら、続けて異なるトピックのサブスクライブやメッセージのパブリッシュが可能です。

1. **+ New Subscription** をクリックします。MQTTX Webは設定に基づき、QoSレベル0でトピック `testtopic/#` のサブスクライブを自動入力します。この手順を繰り返して複数のトピックをサブスクライブ可能で、MQTTX Webはトピックごとに色分けして区別します。

2. チャットエリア右下の送信アイコンをクリックしてメッセージのパブリッシュ／受信をテストします。送信成功したメッセージがチャットウィンドウに表示されます。

<img src="./assets/MQTTXWeb-test.png" alt="MQTT X Webテスト" style="zoom: 25%;" />

さらに、一方向／双方向SSL認証のテストやカスタムスクリプトによるテストデータのシミュレーションなどを行いたい場合は、[MQTTX](https://mqttx.app)を引き続きご活用ください。

### ダッシュボードでメトリクスを確認

EMQXダッシュボードのクラスター概要ページでは、**接続数**、**トピック数**、**サブスクリプション数**、**受信メッセージ数**、**送信メッセージ数**、**ドロップメッセージ数**などのメトリクスを確認できます。

![emqx-dashboard_ee](./assets/emqx-dashboard_ee.png)

## 次のステップ

ここまででEMQXのインストール、起動、アクセス確認が完了しました。次は[認証と認可](../access-control/authn/authn.md)や[ルールエンジン](../data-integration/rules.md)との連携など、EMQXのより高度な機能をお試しください。

## よくある質問

[EMQ Q&Aコミュニティ](https://askemq.com/)では、EMQXやその他EMQ関連製品の使い方に関する議論や質問・回答、IoT関連技術のユーザー同士の情報交換が行えます。また、専門的な技術サポートが必要な場合は、いつでも[お問い合わせ](https://www.emqx.com/en/contact)ください。
