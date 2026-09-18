---
description: この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。
---

# EMQXのはじめ方

EMQXは、世界で最もスケーラブルで信頼性の高いMQTTメッセージングプラットフォームであり、ビジネスデータをリアルタイムで確実に接続・移動・処理するのに役立ちます。このオールインワンのMQTTプラットフォームを使えば、重要なビジネスインパクトをもたらすIoTアプリケーションを簡単に構築できます。

この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。

::: tip
このクイックスタートガイドで紹介するデプロイ方法のほかに、IoT向けのフルマネージドMQTTサービスであるEMQX Cloudもぜひお試しください。インフラのメンテナンス不要で、[アカウント登録](https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2Fnew)を行うだけで、すぐにMQTTサービスを開始し、IoTデバイスを任意のクラウドに接続できます。
:::

## EMQXのインストール

EMQXは、[Docker](./deploy/install-docker.md)で実行するか、[EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator)でインストールするか、またはダウンロードパッケージを使ってコンピュータや仮想マシン（VM）にインストールできます。ダウンロードパッケージでのインストールを選択した場合、現在以下のオペレーティングシステムがサポートされています。

- RedHat
- CentOS
- RockyLinux
- AmazonLinux
- Ubuntu
- Debian
- macOS
- Linux

上記にないプラットフォームについては、[EMQ](https://www.emqx.com/en/contact)までお問い合わせください。

<!-- TODO @wivwiv Update K8s link when EMQX Terraform 5.0 document ready -->

また、[EMQX Terraform](https://www.emqx.com/en/emqx-terraform)を使って、クラウド上でワンクリックでEMQXをデプロイすることも可能です。例えば、[Alibaba Cloud](https://github.com/emqx/tf-alicloud)や[AWS](https://github.com/emqx/tf-aws)があります。

<!-- TODO @wivwiv Update Terraform link when EMQX Terraform 5.0 document ready -->

### Dockerを使ったEMQXのインストール

コンテナデプロイはEMQXをすばやく試す最も簡単な方法です。このクイックスタートガイドでは、Dockerを使ったEMQXのインストールと起動方法を紹介します。

1. 最新バージョンのEMQXをダウンロードして起動するには、以下のコマンドを実行してください。

   実行前に[Docker](https://www.docker.com/)がインストールされ、起動していることを確認してください。

   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:latest
   ```

2. Webブラウザを起動し、アドレスバーに `http://localhost:18083/`（`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../guides/dashboard/introduction.md)にアクセスします。ここからクライアントの接続や稼働状況の確認ができます。

   デフォルトのユーザー名とパスワード：

   `admin`

   `public`

### インストールパッケージを使ったEMQXのインストール

コンピュータやVMにインストールパッケージを使ってEMQXをインストールし、設定の調整やパフォーマンスチューニングを簡単に行うことも可能です。以下の手順はmacOS 26（Tahoe）かつarm64アーキテクチャ（Apple Silicon）を例に説明しています。

::: tip

すべてのランタイム依存関係を考慮すると、テストやホットアップグレードにはインストールパッケージの使用を推奨しますが、本番環境での利用は**推奨しません**。

:::

1. [公式ダウンロードサイトのmacOSタブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=macOS)にアクセスします。

2. 最新バージョン `@EE_VERSION@` を選択し、**パッケージタイプ**から `macOS 26 arm64 / zip` を選びます。

3. リンクをクリックしてパッケージをダウンロードし、インストールしてください。ページ内のコマンド説明も参考にできます。

5. EMQXを起動するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx foreground
   ```
   これは対話型シェルでEMQXを起動します。シェルを閉じるとEMQXも停止します。
   なお、以下のコマンドでバックグラウンド起動も可能ですが推奨しません。

   ```bash
   ./emqx/bin/emqx start
   ```

6. Webブラウザを起動し、アドレスバーに `http://localhost:18083/`（`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../guides/dashboard/introduction.md)にアクセスします。ここからクライアントの接続や稼働状況の確認ができます。

   デフォルトのユーザー名とパスワードは `admin` と `public` です。ログイン後にパスワード変更を求められます。

7. EMQXを停止するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx stop
   ```

テスト終了後にEMQXをアンインストールする場合は、EMQXフォルダを削除してください。

## MQTTXで接続を検証する

EMQXの起動に成功したら、MQTTXを使って接続とメッセージサービスのテストを続けられます。

[MQTTX](https://mqttx.app)は、macOS、Linux、Windowsで動作する洗練されたクロスプラットフォームのMQTT 5.0デスクトップクライアントです。チャットスタイルのユーザーインターフェースを通じて、ユーザーはすばやく接続を作成し、複数のクライアントを保存できます。また、MQTT/MQTTS接続のテストや、MQTTメッセージのサブスクライブ・パブリッシュも可能です。

ここでは、アプリケーションのダウンロードやインストール不要で使えるブラウザベースのMQTT 5.0 WebSocketクライアントツールである[MQTTX Web](https://mqttx.app/web)を使った接続検証方法を紹介します。

::: tip 前提条件
接続テストの前に、ブローカーのアドレスとポート情報を準備してください。

- **EMQXアドレス**：一般的にはサーバーのIPアドレス
- **ポート**：ダッシュボードの左ナビゲーションメニューから **Management** -> **Listeners** をクリックしてポート番号を確認
:::

### 接続の作成

1. [MQTTX Web](https://mqttx.app/web-client#/recent_connections)をクリックしてブラウザベースのMQTTXにアクセスします。

2. MQTT接続を設定して確立します。画面の **+ New Connection** ボタンをクリックして設定ページに入ります。

   - **Name**：接続名を入力します。例：`MQTTX_Test`

   - **Host**

     - ドロップダウンリストからプロトコルタイプを選択します。WebSocketプロトコルを使う場合は `ws://` を選択してください。MQTTX WebはWebSocketプロトコルのみ対応しており、SSL/TLS接続のテストには[MQTTXデスクトップクライアント](https://mqttx.app/)をダウンロードしてください。
     - EMQXのアドレスを入力します。例：`emqx@127.0.0.1`

   - **Port**：WebSocketプロトコルの場合は例として `8083`

   他の項目はデフォルトのままか、ビジネス要件に応じて設定してください。各項目の詳細は[MQTTユーザーマニュアル - 接続](https://mqttx.app/docs/get-started)を参照してください。

3. 画面右上の **Connect** ボタンをクリックします。

4. メッセージのパブリッシュ／受信をテストします。チャットエリア右下の送信アイコンをクリックすると、送信に成功したメッセージが上部のチャットウィンドウに表示されます。

### トピックのパブリッシュとサブスクライブ

接続が成功したら、続けてさまざまなトピックのサブスクライブやメッセージのパブリッシュを試せます。

1. **+ New Subscription** をクリックします。MQTTX Webは設定に基づいて、QoSレベル0でトピック `testtopic/#` をサブスクライブするようにいくつかの項目を自動入力します。この操作を繰り返して複数のトピックをサブスクライブできます。MQTTX Webはトピックごとに色分けして区別します。

2. チャットエリア右下の送信アイコンをクリックしてメッセージのパブリッシュ／受信をテストします。送信に成功したメッセージはチャットウィンドウに表示されます。

<img src="./assets/MQTTXWeb-test.png" alt="MQTTX Webのテスト画面" style="zoom: 25%;" />

さらに、片方向／双方向SSL認証のテストやカスタムスクリプトによるテストデータのシミュレーションなどを行いたい場合は、[MQTTX](https://mqttx.app)を使って引き続き探索してください。

### ダッシュボードでメトリクスを確認

EMQXダッシュボードのクラスター概要ページでは、**接続数**、**トピック数**、**サブスクリプション数**、**受信メッセージ数**、**送信メッセージ数**、**ドロップされたメッセージ数**などのメトリクスを確認できます。

![emqx-dashboard_ee](./assets/emqx-dashboard_ee.png)

## 次のステップ

ここまででEMQXのインストール、起動、アクセス確認が完了しました。次は、[認証と認可](../guides/access-control/authn/authn.md)や[ルールエンジン](../develop/data-integration/rules.md)との連携など、EMQXのより高度な機能を試してみてください。

## よくある質問

[EMQ Q&Aコミュニティ](https://askemq.com/)では、EMQXやその他EMQ関連製品の使い方に関する議論や質問・回答が行われており、IoT関連技術の経験を他のEMQXユーザーと共有できます。専門的な技術サポートが必要な場合は、いつでも[お問い合わせ](https://www.emqx.com/en/contact)ください。
