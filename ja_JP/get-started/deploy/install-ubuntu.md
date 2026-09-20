# UbuntuへのEMQXインストール

このページでは、UbuntuシステムにEMQXをインストールして起動する方法を案内します。

対応バージョン：

- Ubuntu 24.04
- Ubuntu 22.04

::: tip
EMQX 6.3.0以降、AptまたはDEBパッケージからのEMQXインストールは、Dockerイメージと同じ`/opt/emqx/...`パスを提供します。パスマッピングの詳細は[ファイルとディレクトリ](./install.md#files-and-directories)を参照してください。
:::

## Aptパッケージマネージャーによるインストール

EMQXはAptパッケージマネージャーによるインストールをサポートしており、ユーザーに便利で信頼性の高いEMQXのインストールおよびアップデート管理方法を提供します。aptでEMQXをインストールする手順は以下の通りです。

1. EMQXのaptリポジトリをインストールします。

   ```bash
   curl -s https://packagecloud.io/install/repositories/emqx/emqx-enterprise5/script.deb.sh | sudo bash
   ```

2. EMQXをインストールします。

   ```bash
   sudo apt-get install emqx
   ```

3. EMQXを起動します。

   ```bash
   sudo systemctl start emqx
   ```

## 手動パッケージインストール

EMQXはdebパッケージまたはtar.gzパッケージによるインストールもサポートしています。他の対応システムでのインストールや別バージョンの試用については、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご覧ください。

### debパッケージによるインストール

1. 公式ダウンロードページの[**Ubuntu**タブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン`@EE_VERSION@`を選択し、**パッケージタイプ**のドロップダウンから必要なバージョンとCPUアーキテクチャに応じて`deb`パッケージを選択します。
3. 下のリンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールも可能です。

#### EMQXの起動

systemdサービスとしてEMQXを起動します。

```bash
sudo systemctl start emqx
```

::: tip
EMQX 6.3.0以降、`EMQX_SECURITY_PROFILE`などの起動時環境変数は`/etc/emqx/emqx.env`に設定してください。`emqx`コマンドはサービス起動、フォアグラウンド起動、`emqx ctl`実行時にこのファイルを読み込みます。パッケージのアップグレード時もこのファイルの変更は保持されます。起動時環境変数の変更を反映するにはEMQXノードを再起動してください。[起動時環境変数](../../guides/configuration/configuration.md#boot-time-environment-variables)を参照してください。
:::

#### EMQXのアンインストール

EMQXをアンインストールするには、以下のコマンドを実行します。

```
sudo apt remove --purge emqx
```

### tar.gzパッケージによるインストール

1. 公式ダウンロードページの[**Ubuntu**タブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン`@EE_VERSION@`を選択し、**パッケージタイプ**のドロップダウンから必要なバージョンとCPUアーキテクチャに応じて`tar.gz`パッケージを選択します。
3. 下のリンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールも可能です。

#### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
