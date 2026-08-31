# UbuntuへのEMQXインストール

このページでは、UbuntuシステムにEMQXをインストールし起動する方法を案内します。

対応バージョン：

- Ubuntu 24.04
- Ubuntu 22.04

## Aptパッケージマネージャーでのインストール

EMQXはAptパッケージマネージャーによるインストールをサポートしており、ユーザーに便利で信頼性の高いEMQXのインストールおよび更新管理方法を提供します。aptを使ったEMQXのインストール手順は以下の通りです。

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

EMQXはdebパッケージまたはtar.gzパッケージによるインストールをサポートしています。他の対応システムへのインストールや他バージョンの試用については、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご参照ください。

### debでのインストール

1. 公式ダウンロードページにアクセスし、[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)を選択します。
2. 最新バージョン `@EE_VERSION@` を選択し、**Package Type**のドロップダウンから必要なバージョンとCPUアーキテクチャに応じて`deb`パッケージを選択します。
3. 下記リンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールも可能です。

#### EMQXの起動

EMQXをsystemdサービスとして起動します。

```bash
sudo systemctl start emqx
```

::: tip
EMQX 6.3.0以降では、`EMQX_SECURITY_PROFILE`などの起動時環境変数を`/etc/emqx/emqx.env`に設定してください。`emqx`コマンドはサービス起動、フォアグラウンド起動、`emqx ctl`実行時にこのファイルを読み込みます。パッケージアップグレード時もこのファイルの変更は保持されます。起動時環境変数の変更を反映するにはEMQXノードを再起動してください。詳細は[起動時環境変数](../configuration/configuration.md#boot-time-environment-variables)を参照してください。
:::

#### EMQXのアンインストール

EMQXをアンインストールするには、以下のコマンドを実行します。

```
sudo apt remove --purge emqx
```

### tar.gzでのインストール

1. 公式ダウンロードページにアクセスし、[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)を選択します。
2. 最新バージョン `@EE_VERSION@` を選択し、**Package Type**のドロップダウンから必要なバージョンとCPUアーキテクチャに応じて`tar.gz`パッケージを選択します。
3. 下記リンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールも可能です。

#### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
