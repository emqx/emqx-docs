---
description: この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。
---

# EMQXのはじめ方

EMQXは、世界で最もスケーラブルかつ信頼性の高いMQTTメッセージングプラットフォームであり、ビジネスデータをリアルタイムで確実に接続、移動、処理するのに役立ちます。このオールインワンMQTTプラットフォームを使えば、重要なビジネスインパクトをもたらすIoTアプリケーションを簡単に構築できます。

この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。

::: tip
このクイックスタートガイドで紹介しているデプロイ方法のほかに、IoT向けのフルマネージドMQTTサービスであるEMQX Cloudもぜひお試しください。アカウントを[登録](https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2Fnew)するだけで、インフラのメンテナンス不要で任意のクラウドにIoTデバイスを接続し、MQTTサービスを開始できます。
:::

## EMQXのインストール

EMQXは、[Docker](../deploy/install-docker.md)で実行するか、[EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator)でインストールするか、またはダウンロードパッケージを使ってコンピューターや仮想マシン（VM）にインストールできます。ダウンロードパッケージを使う場合、現在サポートされているOSは以下の通りです。

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

コンテナでのデプロイは、EMQXを最速で試す方法です。このクイックスタートガイドでは、Dockerを使ったEMQXのインストールと起動方法を紹介します。

1. 最新版のEMQXをダウンロードして起動するには、以下のコマンドを実行してください。

   実行前に[Docker](https://www.docker.com/)がインストールされ、起動していることを確認してください。

   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:latest
   ```

2. Webブラウザを起動し、アドレスバーに `http://localhost:18083/` （`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントの接続や稼働状況を確認できます。

   デフォルトのユーザー名とパスワード：

   `admin`

   `public`

### インストールパッケージを使ったEMQXのインストール

コンピューターやVMにインストールパッケージを使ってEMQXをインストールし、設定の調整やパフォーマンスチューニングを行うことも可能です。以下の手順はmacOS 15（Sequoia）およびarm64アーキテクチャ（Apple Silicon）を例に説明しています。

::: tip

すべてのランタイム依存関係を考慮すると、テストやホットアップグレードにはインストールパッケージの利用を推奨しますが、本番環境での利用は**推奨しません**。

:::

1. [公式ダウンロードサイトのmacOSタブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=macOS)にアクセスします。

2. 最新バージョン `@EE_VERSION@` を選択し、**Package Type**から `macOS 15 arm64 / zip` を選びます。

3. 下のリンクをクリックしてパッケージをダウンロードし、インストールします。ページ内のコマンド説明も参考にしてください。

4. EMQXを起動するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx foreground
   ```

   これでEMQXが対話型シェルで起動します。シェルを閉じるとEMQXも停止します。なお、推奨しませんがバックグラウンドで起動する場合は以下のコマンドを使えます。

   ```bash
   ./emqx/bin/emqx start
   ```

5. Webブラウザを起動し、アドレスバーに `http://localhost:18083/` （`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントの接続や稼働状況を確認できます。

   デフォルトのユーザー名とパスワードは `admin` と `public` です。ログイン後にパスワード変更を求められます。

6. EMQXを停止するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx stop
   ```

テスト終了後にEMQXをアンインストールするには、EMQXフォルダーを削除してください。

## MQTTXで接続を検証する

EMQXの起動に成功したら、MQTTXを使って接続とメッセージサービスのテストを続けられます。

[MQTTX](https://mqttx.app)は、macOS、Linux、Windowsで動作する洗練されたクロスプラットフォームのMQTT 5.0デスクトップクライアントです。チャットスタイルのUIで複数のクライアントを素早く作成・保存でき、MQTT/MQTTSの接続、サブスクライブ、パブリッシュのテストも可能です。

ここでは、アプリのダウンロードやインストール不要で使えるブラウザベースのMQTT 5.0 WebSocketクライアントツールである[MQTTX Web](https://mqttx.app/web)を使った接続検証方法を紹介します。

::: tip 前提条件
接続テストの前に、ブローカーのアドレスとポート情報を準備してください。

- **EMQXアドレス**：一般的にはサーバーのIPアドレス
- **ポート**：ダッシュボードの左ナビゲーションメニューから **Management** -> **Listeners** をクリックしてポート番号を確認
:::

### 接続の作成

1. [MQTTX Web](https://mqttx.app/web-client#/recent_connections)にアクセスします。

2. MQTT接続を設定して確立します。画面の **+ New Connection** ボタンをクリックして接続設定ページを開きます。

   - **Name**：接続名を入力します。例：`MQTTX_Test`

   - **Host**：

     - プロトコルをドロップダウンリストから選択します。例：WebSocketプロトコルを使う場合は `ws://` を選択します。
   
       > MQTTX WebはWebSocket接続のみ対応しています。SSL/TLS接続をテストする場合は、[MQTTXデスクトップクライアント](https://mqttx.app/)をダウンロードしてください。
   
     - EMQXのアドレスを入力します。例：`127.0.0.1`
   
   - **Port**：ポート番号を入力します。例：WebSocket接続では一般的に `8083` を使用します。
   
   他の項目はデフォルトのままか、必要に応じて調整してください。各オプションの詳細は[MQTTユーザーマニュアル – 接続](https://mqttx.app/docs/get-started)を参照してください。

3. 画面右上の **Connect** ボタンをクリックします。

4. メッセージのパブリッシュと受信を確認します。メッセージエリア右下の送信アイコンをクリックしてください。正常に送信されたメッセージはチャットウィンドウに表示されます。

### トピックのパブリッシュとサブスクライブ

接続が成功したら、続けて異なるトピックのサブスクライブやメッセージのパブリッシュを行えます。

1. **+ New Subscription** をクリックします。MQTTX Webは設定に基づき、トピック `testtopic/#` をQoSレベル0でサブスクライブするように一部フィールドを自動入力します。異なるトピックをサブスクライブする場合はこの手順を繰り返してください。MQTTX Webはトピックごとに色分けして区別します。

2. チャットエリア右下の送信アイコンをクリックしてメッセージのパブリッシュ／受信をテストします。正常に送信されたメッセージはチャットウィンドウに表示されます。

<img src="./assets/MQTTXWeb-test.png" alt="MQTTX Webのテスト画面" style="zoom: 25%;" />

さらに、一方向／双方向SSL認証やカスタムスクリプトによるテストデータのシミュレーションなどのテストを続けたい場合は、[MQTTX](https://mqttx.app)を活用してください。

### ダッシュボードでメトリクスを確認する

EMQXダッシュボードのクラスター概要ページでは、**接続数**、**トピック数**、**サブスクリプション数**、**受信メッセージ数**、**送信メッセージ数**、**ドロップメッセージ数**などのメトリクスを確認できます。

<img src="./assets/view_metrics_dashboard.png" alt="ダッシュボードでのメトリクス表示" style="zoom:150%;" />

## 次のステップ

ここまででEMQXのインストール、起動、アクセスのテストが完了しました。続けて、[認証と認可](../access-control/authn/authn.md)や[ルールエンジン](../data-integration/rules.md)との連携など、EMQXのより高度な機能を試してみてください。

## よくある質問

[EMQ Q&Aコミュニティ](https://askemq.com/)では、EMQXやその他EMQ関連製品の使い方に関する質問や回答、IoT関連技術に関するユーザー同士の情報交換ができます。専門的な技術サポートが必要な場合は、いつでも[お問い合わせ](https://www.emqx.com/en/contact)ください。
