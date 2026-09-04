# UbuntuへのEMQXインストール

このページでは、UbuntuシステムへのEMQXのインストールと起動方法について説明します。

対応バージョン：

- Ubuntu 24.04
- Ubuntu 22.04

::: tip
EMQX 6.3.0以降、AptまたはDEBパッケージからのEMQXインストールは、Dockerイメージと同様の`/opt/emqx/...`パスを提供します。パスマッピングについては、[ファイルとディレクトリ](./install.md#files-and-directories)をご参照ください。
:::

## Aptパッケージマネージャーによるインストール

EMQXは、Aptパッケージマネージャーによるインストールをサポートしており、ユーザーに便利で信頼性の高いEMQXのインストールおよびアップデート管理手段を提供します。AptでのEMQXインストール手順は以下の通りです。

1. EMQXのaptリポジトリをインストールします：

   ```bash
   curl -s https://packagecloud.io/install/repositories/emqx/emqx-enterprise5/script.deb.sh | sudo bash
   ```

2. EMQXをインストールします：

   ```bash
   sudo apt-get install emqx
   ```

3. EMQXを起動します：

   ```bash
   sudo systemctl start emqx
   ```

## 手動パッケージインストール

EMQXはdebパッケージまたはtar.gzパッケージによるインストールもサポートしています。その他の対応システムでのインストールや別バージョンを試す場合は、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご覧ください。

### debパッケージでのインストール

1. 公式ダウンロードページの[**Ubuntu**タブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン`@EE_VERSION@`を選択し、必要なバージョンとCPUアーキテクチャに応じて**パッケージタイプ**のドロップダウンから`deb`パッケージを選択します。
3. 下のリンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールすることも可能です。

#### EMQXの起動

systemdサービスとしてEMQXを起動します。

```bash
sudo systemctl start emqx
```

::: tip
EMQX 6.3.0以降、`EMQX_SECURITY_PROFILE`などの起動時環境変数は`/etc/emqx/emqx.env`に設定してください。`emqx`コマンドはサービス起動、フォアグラウンド起動、`emqx ctl`実行時にこのファイルを読み込みます。パッケージアップグレード時もこのファイルの変更は保持されます。起動時環境変数の変更を反映するにはEMQXノードを再起動してください。詳細は[起動時環境変数](../configuration/configuration.md#boot-time-environment-variables)をご覧ください。
:::

#### EMQXのアンインストール

EMQXをアンインストールするには、以下を実行します。

```
sudo apt remove --purge emqx
```

### tar.gzパッケージでのインストール

1. 公式ダウンロードページの[**Ubuntu**タブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン`@EE_VERSION@`を選択し、必要なバージョンとCPUアーキテクチャに応じて**パッケージタイプ**のドロップダウンから`tar.gz`パッケージを選択します。
3. 下のリンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールすることも可能です。

#### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
