---
description: この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。
---

# EMQXを始める

EMQXは、世界で最もスケーラブルかつ信頼性の高いMQTTメッセージングプラットフォームであり、ビジネスデータをリアルタイムで確実に接続、移動、処理するのに役立ちます。このオールインワンのMQTTプラットフォームを使えば、IoTアプリケーションを簡単に構築し、ビジネスに大きな影響を与えることができます。

本章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。

::: tip
本クイックスタートガイドで紹介しているデプロイ方法のほかに、IoT向けの完全マネージドMQTTサービスであるEMQX Cloudもぜひお試しください。インフラ管理不要で、[アカウント登録](https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2Fnew)を行うだけで、すぐにMQTTサービスを開始し、IoTデバイスを任意のクラウドに接続できます。
:::

## EMQXのインストール

EMQXは、[Docker](../deploy/install-docker.md)での実行、[EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator)によるインストール、またはコンピュータや仮想マシン（VM）にダウンロードパッケージでインストールすることが可能です。ダウンロードパッケージによるインストールを選択した場合、現在以下のOSがサポートされています。

- RedHat
- CentOS
- RockyLinux
- AmazonLinux
- Ubuntu
- Debian
- macOS
- Linux

上記以外のプラットフォームについては、[EMQ](https://www.emqx.com/en/contact)までお問い合わせください。

### Dockerを使ったEMQXのインストール

コンテナによるデプロイは、EMQXを素早く試す最も簡単な方法です。本クイックスタートガイドでは、Dockerを使ったEMQXのインストールと起動方法を説明します。

1. 最新版のEMQXをダウンロードして起動するには、以下のコマンドを入力してください。

   事前に[Docker](https://www.docker.com/)がインストールされ、起動していることを確認してください。

   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:latest
   ```

2. Webブラウザを起動し、アドレスバーに `http://localhost:18083/` （`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントの接続や稼働状況の確認が可能です。

   デフォルトのユーザー名とパスワード：

   `admin`

   `public`

### インストールパッケージを使ったEMQXのインストール

コンピュータやVMにインストールパッケージを使ってEMQXをインストールし、設定の調整やパフォーマンスチューニングを行うことも可能です。以下の手順はmacOS 15（Sequoia）およびarm64アーキテクチャ（Apple Silicon）を例に説明しています。

::: tip

すべてのランタイム依存関係を考慮すると、テストやホットアップグレードにはインストールパッケージの使用を推奨しますが、本番環境での使用は**推奨しません**。

:::

1. [公式ダウンロードサイトのmacOSタブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=macOS)にアクセスします。

2. 最新バージョン `@EE_VERSION@` を選択し、**Package Type**から `macOS 15 arm64 / zip` を選びます。

3. 下のリンクをクリックしてパッケージをダウンロードし、インストールします。ページ内のコマンド説明も参照できます。

5. EMQXを起動するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx foreground
   ```
   これによりインタラクティブシェルでEMQXが起動します。シェルを閉じるとEMQXも停止します。  
   なお（推奨しませんが）、以下のコマンドでバックグラウンド起動も可能です。

   ```bash
   ./emqx/bin/emqx start
   ```

6. Webブラウザを起動し、アドレスバーに `http://localhost:18083/` （`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントの接続や稼働状況の確認が可能です。

   デフォルトのユーザー名とパスワードは `admin` と `public` です。ログイン後にパスワード変更を求められます。

7. EMQXを停止するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx stop
   ```

テスト終了後にEMQXをアンインストールするには、EMQXフォルダを削除するだけです。

## MQTTXで接続を検証する

EMQXの起動に成功したら、MQTTXを使って接続とメッセージサービスのテストを続けて行えます。

[MQTTX](https://mqttx.app)は、macOS、Linux、Windowsで動作する洗練されたクロスプラットフォームのMQTT 5.0デスクトップクライアントです。チャットスタイルのUIで複数のクライアント接続を素早く作成・保存でき、MQTT/MQTTS接続やメッセージのサブスクライブ・パブリッシュのテストが可能です。

ここでは、アプリのダウンロードやインストール不要で使えるブラウザベースのMQTT 5.0 WebSocketクライアントツールである[MQTTX Web](https://mqttx.app/web)を使った接続検証方法を紹介します。

::: tip 前提条件
接続テスト前に以下の情報を準備してください：

- **EMQXアドレス**：一般的にはサーバーのIPアドレス
- **ポート**：ダッシュボードの左メニューから **Management** -> **Listeners** をクリックし、ポート番号を確認
:::

### 接続の作成

1. [MQTTX Web](https://mqttx.app/web-client#/recent_connections)にアクセスします。

2. MQTT接続の設定と確立を行います。**+ New Connection** ボタンをクリックして設定画面を開きます：

   - **Name**：接続名を入力します。例：`MQTTX_Test`

   - **Host**

     - プロトコルタイプをドロップダウンリストから選択します。WebSocketプロトコルを使う場合は `ws://` を選択してください。MQTTX WebはWebSocketプロトコルのみサポートしています。SSL/TLS接続をテストする場合は、[MQTTXデスクトップクライアント](https://mqttx.app/)をダウンロードしてください。
     - EMQXアドレスを入力します。例：`emqx@127.0.0.1`

   - **Port**：例としてWebSocketプロトコル用の `8083`

   他の項目はデフォルトのままか、ビジネス要件に合わせて設定してください。各項目の詳細は[MQTT User Manual - Connect](https://mqttx.app/docs/get-started)を参照してください。

3. 画面右上の **Connect** ボタンをクリックします。

4. メッセージのパブリッシュ／受信をテストします。チャットエリア右下の送信アイコンをクリックすると、送信に成功したメッセージがチャットウィンドウに表示されます。

### トピックのパブリッシュとサブスクライブ

接続が成功したら、続けて異なるトピックのサブスクライブとメッセージのパブリッシュを行えます。

1. **+ New Subscription** をクリックします。MQTTX Webは設定に基づき、QoSレベル0でトピック `testtopic/#` をサブスクライブします。この手順を繰り返して異なるトピックをサブスクライブでき、MQTTX Webはトピックごとに色分けして区別します。

2. チャットエリア右下の送信アイコンをクリックしてメッセージのパブリッシュ／受信をテストします。送信に成功したメッセージがチャットウィンドウに表示されます。

<img src="./assets/MQTTXWeb-test.png" alt="MQTT X Web テスト" style="zoom: 25%;" />

さらに、一方向／双方向SSL認証のテストやカスタムスクリプトによるテストデータのシミュレーションなどを行いたい場合は、[MQTTX](https://mqttx.app)を使って引き続き検証できます。

### ダッシュボードでメトリクスを確認

EMQXダッシュボードのクラスター概要ページでは、**Connections**、**Topics**、**Subscriptions**、**Incoming Messages**、**Outgoing messages**、**Dropped Messages**などのメトリクスを確認できます。

![emqx-dashboard_ee](./assets/emqx-dashboard_ee.png)

## 次のステップ

ここまででEMQXのインストール、起動、アクセスのテストが完了しました。次は、[認証と認可](../access-control/authn/authn.md)や[ルールエンジン](../data-integration/rules.md)との連携など、EMQXのより高度な機能を試してみてください。

## よくある質問

[EMQ Q&Aコミュニティ](https://askemq.com/)では、EMQXやその他EMQ関連製品の使い方に関する議論、質問・回答、IoT関連技術のユーザー同士の情報交換が行えます。また、専門的な技術サポートが必要な場合は、いつでも[お問い合わせ](https://www.emqx.com/en/contact)ください。
