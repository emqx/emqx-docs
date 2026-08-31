---
description: この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。
---

# EMQXを始める

EMQXは、世界で最もスケーラブルかつ信頼性の高いMQTTメッセージングプラットフォームであり、ビジネスデータをリアルタイムで確実に接続、移動、処理するのに役立ちます。このオールインワンのMQTTプラットフォームを使えば、IoTアプリケーションを簡単に構築し、ビジネスに大きな影響を与えることができます。

この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。

::: tip
このクイックスタートガイドで紹介するデプロイ方法のほかに、完全マネージド型のMQTTサービスであるEMQX Cloudもご利用いただけます。インフラのメンテナンス不要で、[アカウント登録](https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2Fnew)を行うだけで、すぐにMQTTサービスを開始し、IoTデバイスを任意のクラウドに接続できます。
:::

## EMQXのインストール

EMQXは、[Docker](../deploy/install-docker.md)で実行するか、[EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator)を使ってインストールするか、またはダウンロードパッケージを使ってコンピューターや仮想マシン（VM）にインストールできます。ダウンロードパッケージでのインストールを選択した場合、現在以下のオペレーティングシステムがサポートされています。

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

コンテナでのデプロイは、EMQXを素早く試す最も簡単な方法です。このクイックスタートガイドでは、Dockerを使ったEMQXのインストールと起動方法を紹介します。

1. 最新版のEMQXをダウンロードして起動するには、以下のコマンドを入力してください。

   実行前に[Docker](https://www.docker.com/)がインストールされ、起動していることを確認してください。

   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:latest
   ```

2. Webブラウザを起動し、アドレスバーに `http://localhost:18083/` （`localhost`はIPアドレスに置き換え可能）を入力して、[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントへの接続や稼働状況の確認ができます。

   デフォルトのユーザー名とパスワード：

   `admin`

   `public`

### インストールパッケージを使ったEMQXのインストール

コンピューターやVMにインストールパッケージを使ってEMQXをインストールし、設定調整やパフォーマンスチューニングを行うことも可能です。以下の手順はmacOS 26（Tahoe）およびarm64アーキテクチャ（Apple Silicon）を例に説明しています。

::: tip

すべてのランタイム依存関係を考慮すると、テストやホットアップグレードにはインストールパッケージの利用を推奨しますが、本番環境での利用は**推奨しません**。

:::

1. [公式ダウンロードサイトのmacOSタブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=macOS)にアクセスします。

2. 最新バージョン `@EE_VERSION@` を選択し、**Package Type**から `macOS 26 arm64 / zip` を選びます。

3. リンクをクリックしてパッケージをダウンロード・インストールします。ページ内のコマンド説明も参照可能です。

4. EMQXを起動するには、以下を入力します。

   ```bash
   ./emqx/bin/emqx foreground
   ```
   これは対話型シェルでEMQXを起動します。シェルを閉じるとEMQXも停止します。
   代わりに（推奨しませんが）、以下のコマンドでバックグラウンド起動も可能です。

   ```bash
   ./emqx/bin/emqx start
   ```

5. Webブラウザを起動し、アドレスバーに `http://localhost:18083/` （`localhost`はIPアドレスに置き換え可能）を入力して、[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントへの接続や稼働状況の確認ができます。

   デフォルトのユーザー名とパスワードは `admin` と `public` です。ログイン後にパスワード変更を求められます。

6. EMQXを停止するには、以下を入力します。

   ```bash
   ./emqx/bin/emqx stop
   ```

テスト終了後にEMQXをアンインストールするには、EMQXフォルダーを削除するだけです。

## MQTTXで接続を検証する

EMQXを正常に起動できたら、MQTTXを使って接続とメッセージサービスのテストを続けられます。

[MQTTX](https://mqttx.app)は、macOS、Linux、Windowsで動作する洗練されたクロスプラットフォームのMQTT 5.0デスクトップクライアントです。チャットスタイルのユーザーインターフェースで複数のクライアントを素早く作成・保存でき、MQTT/MQTTS接続やサブスクライブ、パブリッシュのテストも可能です。

ここでは、アプリケーションのダウンロードやインストール不要で使えるブラウザベースのMQTT 5.0 WebSocketクライアントツールである[MQTTX Web](https://mqttx.app/web)を使った接続検証方法を紹介します。

::: tip 前提条件
接続テストの前に、ブローカーのアドレスとポート情報を準備してください。

- **EMQXアドレス**：一般的にはサーバーのIPアドレス
- **ポート**：ダッシュボードの左ナビゲーションメニューから **Management** -> **Listeners** をクリックしてポート番号を確認
:::

### 接続の作成

1. [MQTTX Web](https://mqttx.app/web-client#/recent_connections)にアクセスします。

2. MQTT接続を設定し、確立します。画面右上の **+ New Connection** ボタンをクリックして接続設定ページを開きます。

   - **Name**：接続名を入力します。例：`MQTTX_Test`

   - **Host**：

     - プロトコルをドロップダウンリストから選択します。例：WebSocketプロトコルを使う場合は `ws://` を選択します。
   
       > MQTTX WebはWebSocket接続のみ対応しています。SSL/TLS接続をテストする場合は、[MQTTXデスクトップクライアント](https://mqttx.app/)をダウンロードしてください。
   
     - EMQXのアドレスを入力します。例：`127.0.0.1`
   
   - **Port**：ポート番号を入力します。例：WebSocket接続には一般的に `8083` を使用します。
   
   他の項目はデフォルトのままか、必要に応じて調整してください。各オプションの詳細は[MQTTユーザーマニュアル – 接続](https://mqttx.app/docs/get-started)をご参照ください。

3. 画面右上の **Connect** ボタンをクリックします。

4. メッセージのパブリッシュと受信を確認します。メッセージエリア右下の **Send** アイコンをクリックしてください。正常に送信されたメッセージはチャットウィンドウに表示されます。

### トピックのパブリッシュとサブスクライブ

接続が確立したら、さまざまなトピックをサブスクライブし、メッセージをパブリッシュできます。

1. **+ New Subscription** をクリックします。MQTTX Webは設定に基づき、トピック `testtopic/#` をQoSレベル0でサブスクライブするようにフィールドを自動入力します。この手順を繰り返して異なるトピックをサブスクライブでき、MQTTX Webはトピックごとに色分けして区別します。

2. チャットエリア右下の送信アイコンをクリックして、メッセージのパブリッシュ／受信をテストします。正常に送信されたメッセージはチャットウィンドウに表示されます。

<img src="./assets/MQTTXWeb-test.png" alt="MQTTX Webのテスト画面" style="zoom: 25%;" />

さらに、片方向／双方向SSL認証やカスタムスクリプトによるテストデータのシミュレーションなどを行いたい場合は、[MQTTX](https://mqttx.app)での検証を続けてください。

### ダッシュボードでメトリクスを確認する

EMQXダッシュボードのクラスター概要ページでは、**接続数（Connections）**、**トピック数（Topics）**、**サブスクリプション数（Subscriptions）**、**受信メッセージ数（Incoming Messages）**、**送信メッセージ数（Outgoing messages）**、**破棄されたメッセージ数（Dropped Messages）**などのメトリクスを確認できます。

<img src="./assets/view_metrics_dashboard.png" alt="ダッシュボードでのメトリクス表示" style="zoom:150%;" />

## 次のステップ

ここまででEMQXのインストール、起動、アクセスのテストが完了しました。次は、[認証と認可](../access-control/authn/authn.md)や[ルールエンジン](../data-integration/rules.md)との連携など、EMQXのより高度な機能をお試しください。

## よくある質問

[EMQ Q&Aコミュニティ](https://askemq.com/)では、EMQXやその他EMQ関連製品の使い方に関する質問や回答、IoT関連技術に関するユーザー同士の情報交換ができます。専門的な技術サポートが必要な場合は、いつでも[お問い合わせ](https://www.emqx.com/en/contact)ください。
