---
description: この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法を紹介します。
---

# EMQXのはじめ方

<<<<<<< HEAD
EMQXは、世界で最もスケーラブルで信頼性の高いMQTTメッセージングプラットフォームであり、ビジネスデータをリアルタイムで確実に接続、移動、処理するのに役立ちます。このオールインワンのMQTTプラットフォームを使えば、重要なビジネスインパクトをもたらすIoTアプリケーションを簡単に構築できます。

この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法を紹介します。

::: tip
このクイックスタートガイドで紹介するデプロイ方法のほかに、IoT向けのフルマネージドMQTTサービスであるEMQX Cloudもぜひお試しください。インフラのメンテナンス不要でMQTTサービスを開始し、IoTデバイスを任意のクラウドに接続するには、[アカウント登録](https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2Fnew)だけで済みます。
=======
EMQXは、世界で最もスケーラブルで信頼性の高いMQTTメッセージングプラットフォームであり、ビジネスデータをリアルタイムで確実に接続、移動、処理するのに役立ちます。このオールインワンのMQTTプラットフォームを使えば、ビジネスに大きな影響を与えるIoTアプリケーションを簡単に構築できます。

この章では、EMQXのダウンロードとインストール方法、および組み込みのWebSocketツールを使った接続とメッセージングサービスのテスト方法をご案内します。

::: tip
このクイックスタートガイドで紹介しているデプロイ方法のほかに、IoT向けのフルマネージドMQTTサービスであるEMQX Cloudもぜひお試しください。インフラ管理不要で、[アカウント登録](https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2Fnew)を行うだけで、すぐにMQTTサービスを開始し、IoTデバイスを任意のクラウドに接続できます。
>>>>>>> origin/release-5.10
:::

## EMQXのインストール

<<<<<<< HEAD
EMQXは、[Docker](../deploy/install-docker.md)で実行するか、[EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator)でインストールするか、ダウンロードパッケージを使ってコンピューターや仮想マシン（VM）にインストールできます。ダウンロードパッケージでのインストールを選択した場合、現在以下のOSがサポートされています。
=======
EMQXは、[Docker](../deploy/install-docker.md)での実行、[EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator)によるインストール、またはダウンロードパッケージを使ってコンピューターや仮想マシン（VM）にインストールする方法があります。ダウンロードパッケージでのインストールを選択する場合、現在以下のOSがサポートされています。
>>>>>>> origin/release-5.10

- RedHat
- CentOS
- RockyLinux
- AmazonLinux
- Ubuntu
- Debian
- macOS
- Linux

上記にないプラットフォームについては、[EMQ](https://www.emqx.com/en/contact)までお問い合わせください。
<<<<<<< HEAD

### Dockerを使ったEMQXのインストール

コンテナでのデプロイは、EMQXをすばやく試す最も簡単な方法です。このクイックスタートガイドでは、Dockerを使ったEMQXのインストールと起動方法を説明します。
=======

<!-- TODO @wivwiv EMQX Terraform 5.0ドキュメント準備時にK8sリンクを更新 -->

また、クラウド上でのワンクリックデプロイとして、[EMQX Terraform](https://www.emqx.com/en/emqx-terraform)を利用可能です。例えば、[Alibaba Cloud](https://github.com/emqx/tf-alicloud)や[AWS](https://github.com/emqx/tf-aws)があります。

<!-- TODO @wivwiv EMQX Terraform 5.0ドキュメント準備時にTerraformリンクを更新 -->

### Dockerを使ったEMQXのインストール

コンテナによるデプロイは、EMQXをすぐに試す最速の方法です。このクイックスタートガイドでは、Dockerを使ったEMQXのインストールと起動方法を紹介します。
>>>>>>> origin/release-5.10

1. 最新版のEMQXをダウンロードして起動するには、以下のコマンドを実行してください。

   実行前に[Docker](https://www.docker.com/)がインストールされ、起動していることを確認してください。

   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:latest
   ```

2. Webブラウザを起動し、アドレスバーに `http://localhost:18083/`（`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントへの接続や稼働状況の確認ができます。

   デフォルトのユーザー名とパスワード：

   `admin`

   `public`

### インストールパッケージを使ったEMQXのインストール

<<<<<<< HEAD
コンピューターやVMにインストールパッケージを使ってEMQXをインストールし、設定の調整やパフォーマンスチューニングを簡単に行うこともできます。以下の手順はmacOS 15（Sequoia）およびarm64アーキテクチャ（Apple Silicon）を例に説明しています。
=======
コンピューターやVMにインストールパッケージを使ってEMQXをインストールし、設定の調整やパフォーマンスチューニングを行うことも可能です。以下の手順は、macOS 15（Sequoia）およびarm64アーキテクチャ（Apple Silicon）を例に説明しています。
>>>>>>> origin/release-5.10

::: tip

すべてのランタイム依存関係を考慮すると、テストやホットアップグレードにはインストールパッケージの使用を推奨しますが、本番環境での利用は**推奨しません**。

:::

1. [公式ダウンロードサイトのmacOSタブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=macOS)にアクセスします。

2. 最新バージョン `@EE_VERSION@` を選択し、**Package Type** から `macOS 15 arm64 / zip` を選びます。

3. 下のリンクをクリックしてパッケージをダウンロードし、インストールします。ページ内のコマンド説明も参考にしてください。

4. EMQXを起動するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx foreground
   ```
<<<<<<< HEAD
   これにより対話型シェルでEMQXが起動します。シェルを閉じるとEMQXも停止します。
=======
   これによりインタラクティブシェルでEMQXが起動します。シェルを閉じるとEMQXも停止します。
>>>>>>> origin/release-5.10
   なお（推奨しませんが）、以下のコマンドでバックグラウンド起動も可能です。

   ```bash
   ./emqx/bin/emqx start
   ```

<<<<<<< HEAD
5. Webブラウザを起動し、アドレスバーに `http://localhost:18083/`（`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントへの接続や稼働状況の確認ができます。
=======
6. Webブラウザを起動し、アドレスバーに `http://localhost:18083/`（`localhost`はIPアドレスに置き換え可能）を入力して[EMQXダッシュボード](../dashboard/introduction.md)にアクセスします。ここからクライアントへの接続や稼働状況の確認ができます。
>>>>>>> origin/release-5.10

   デフォルトのユーザー名とパスワードは `admin` と `public` です。ログイン後にパスワード変更が求められます。

6. EMQXを停止するには、以下を実行します。

   ```bash
   ./emqx/bin/emqx stop
   ```

<<<<<<< HEAD
テスト終了後にEMQXをアンインストールするには、EMQXフォルダーを削除するだけです。
=======
テスト終了後にEMQXをアンインストールする場合は、EMQXフォルダを削除してください。
>>>>>>> origin/release-5.10

## MQTTXを使った接続確認

<<<<<<< HEAD
EMQXを正常に起動できたら、MQTTXを使って接続とメッセージサービスのテストを続けられます。

[MQTTX](https://mqttx.app)は、macOS、Linux、Windowsで動作する洗練されたクロスプラットフォームのMQTT 5.0デスクトップクライアントです。ユーザーはチャットスタイルのUIで素早く接続を作成し、複数のクライアントを保存できます。MQTT/MQTTS接続のテストや、メッセージのサブスクライブおよびパブリッシュも可能です。

ここでは、アプリのダウンロードやインストール不要のブラウザベースMQTT 5.0 WebSocketクライアントツールである[MQTTX Web](https://mqttx.app/web)を使った接続検証方法を紹介します。

::: tip 前提条件
接続テストの前に、ブローカーのアドレスとポート情報を準備してください。

- **EMQXアドレス**：一般的にはサーバーのIPアドレス
- **ポート**：ダッシュボードの左ナビゲーションメニューから **Management** -> **Listeners** をクリックしてポート番号を確認
=======
EMQXを起動できたら、次にMQTTXを使って接続とメッセージサービスのテストを行えます。

[MQTTX](https://mqttx.app)は、macOS、Linux、Windowsで動作する洗練されたクロスプラットフォームのMQTT 5.0デスクトップクライアントです。チャットスタイルのUIで複数のクライアントを素早く作成・保存でき、MQTT/MQTTS接続のテストやメッセージのサブスクライブ・パブリッシュが可能です。

ここでは、アプリのダウンロードやインストール不要で使えるブラウザベースのMQTT 5.0 WebSocketクライアントツールである[MQTTX Web](https://mqttx.app/web)を使った接続確認方法を紹介します。

::: tip 前提条件
接続テスト前に以下の情報を準備してください。

- **EMQXアドレス**：一般的にはサーバーのIPアドレス
- **ポート番号**：ダッシュボードの左ナビゲーションメニューから **Management** -> **Listeners** をクリックして確認
>>>>>>> origin/release-5.10
:::

### 接続の作成

1. [MQTTX Web](https://mqttx.app/web-client#/recent_connections)にアクセスしてブラウザベースのMQTTXを開きます。

<<<<<<< HEAD
2. MQTT接続を設定して確立します。画面の **+ New Connection** ボタンをクリックして設定ページに入ります。
=======
2. MQTT接続の設定と確立を行います。画面の **+ New Connection** ボタンをクリックして設定画面を開きます。
>>>>>>> origin/release-5.10

   - **Name**：接続名を入力します（例：`MQTTX_Test`）。

   - **Host**

<<<<<<< HEAD
     - プロトコルタイプをドロップダウンリストから選択します。例：WebSocketプロトコルを使う場合は `ws://` を選択。MQTTX WebはWebSocketプロトコルのみ対応しています。SSL/TLS接続をテストする場合は[MQTTXデスクトップクライアント](https://mqttx.app/)をダウンロードしてください。
     - EMQXのアドレスを入力します。例：`emqx@127.0.0.1`
=======
     - プロトコルタイプをドロップダウンリストから選択します。WebSocketプロトコルを使用する場合は `ws://` を選択してください。MQTTX WebはWebSocketプロトコルのみ対応しています。SSL/TLS接続をテストする場合は、[MQTTXデスクトップクライアント](https://mqttx.app/)をダウンロードしてください。
     - EMQXアドレスを入力します（例：`emqx@127.0.0.1`）。
>>>>>>> origin/release-5.10

   - **Port**：WebSocketプロトコルの場合は例として `8083` を入力します。

<<<<<<< HEAD
   他の項目はデフォルトのままか、ビジネスニーズに応じて設定してください。各項目の詳細は[MQTTユーザーマニュアル - 接続](https://mqttx.app/docs/get-started)を参照してください。
=======
   他の項目はデフォルトのままか、ビジネス要件に合わせて設定してください。各項目の詳細は[MQTTユーザーマニュアル - 接続](https://mqttx.app/docs/get-started)をご参照ください。
>>>>>>> origin/release-5.10

3. 画面右上の **Connect** ボタンをクリックします。

4. メッセージのパブリッシュ／受信をテストします。チャットエリア右下の送信アイコンをクリックすると、送信に成功したメッセージがチャットウィンドウに表示されます。

### トピックのパブリッシュとサブスクライブ

<<<<<<< HEAD
接続が成功したら、さまざまなトピックのサブスクライブとメッセージのパブリッシュを続けられます。

1. **+ New Subscription** をクリックします。MQTTX Webは設定に基づき、トピック `testtopic/#` をQoSレベル0でサブスクライブするためのフィールドを自動入力します。この操作を繰り返して異なるトピックをサブスクライブできます。MQTTX Webはトピックごとに色分けして区別します。

2. チャットエリア右下の送信アイコンをクリックしてメッセージのパブリッシュ／受信をテストします。送信に成功したメッセージはチャットウィンドウに表示されます。

<img src="./assets/MQTTXWeb-test.png" alt="MQTTX Webのテスト画面" style="zoom: 25%;" />

さらに、片方向／双方向SSL認証のテストやカスタムスクリプトによるテストデータのシミュレーションなどを行いたい場合は、[MQTTX](https://mqttx.app)を引き続きご利用ください。
=======
接続が確立したら、引き続き異なるトピックのサブスクライブやメッセージのパブリッシュを試せます。

1. **+ New Subscription** をクリックします。MQTTX Webは設定に基づき、トピック `testtopic/#` をQoSレベル0でサブスクライブするフィールドを自動入力します。この手順を繰り返して異なるトピックをサブスクライブでき、MQTTX Webはトピックごとに色分けして区別します。

2. チャットエリア右下の送信アイコンをクリックしてメッセージのパブリッシュ／受信をテストします。送信成功したメッセージがチャットウィンドウに表示されます。

<img src="./assets/MQTTXWeb-test.png" alt="MQTTX Webのテスト画面" style="zoom: 25%;" />

さらに、片方向／双方向SSL認証のテストやカスタムスクリプトによるテストデータのシミュレーションなどを行いたい場合は、[MQTTX](https://mqttx.app)を使ってさらに探求できます。
>>>>>>> origin/release-5.10

### ダッシュボードでのメトリクス確認

<<<<<<< HEAD
EMQXダッシュボードのクラスター概要ページでは、**接続数**、**トピック数**、**サブスクリプション数**、**受信メッセージ数**、**送信メッセージ数**、**破棄されたメッセージ数**などのメトリクスを確認できます。
=======
EMQXダッシュボードのクラスター概要ページでは、**接続数**、**トピック数**、**サブスクリプション数**、**受信メッセージ数**、**送信メッセージ数**、**ドロップメッセージ数**などのメトリクスを確認できます。
>>>>>>> origin/release-5.10

![emqx-dashboard_ee](./assets/emqx-dashboard_ee.png)

## 次のステップ

ここまでで、EMQXのインストール、起動、アクセス確認が完了しました。次は、[認証と認可](../access-control/authn/authn.md)や[ルールエンジン](../data-integration/rules.md)との連携など、より高度な機能を試してみてください。

## よくある質問

<<<<<<< HEAD
[EMQ Q&Aコミュニティ](https://askemq.com/)では、EMQXやその他EMQ関連製品の使い方に関する議論や質問・回答が行われており、IoT関連技術のユーザー同士で経験を共有できます。また、専門的な技術サポートが必要な場合は、いつでも[お問い合わせ](https://www.emqx.com/en/contact)ください。
=======
[EMQ Q&Aコミュニティ](https://askemq.com/)では、EMQXやその他のEMQ関連製品の利用に関する議論、質問・回答、IoT関連技術に関するユーザー同士の情報交換が行えます。また、専門的な技術サポートが必要な場合は、いつでも[お問い合わせ](https://www.emqx.com/en/contact)ください。
>>>>>>> origin/release-5.10
