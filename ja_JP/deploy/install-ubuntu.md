# UbuntuへのEMQXインストール

このページでは、UbuntuシステムにEMQXをインストールし起動する方法をご案内します。

対応バージョン：

- Ubuntu 24.04
- Ubuntu 22.04

## Aptパッケージマネージャーによるインストール

EMQXは、Aptパッケージマネージャーによるインストールをサポートしており、ユーザーの皆様に便利で信頼性の高いEMQXのインストールおよびアップデート管理方法を提供します。以下はaptを使ったEMQXのインストール手順です。

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

EMQXはdebパッケージまたはtar.gzパッケージによるインストールもサポートしています。他の対応システムへのインストールや他バージョンの試用については、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご覧ください。

### debパッケージによるインストール

1. 公式ダウンロードページの[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン`@EE_VERSION@`を選択し、必要なバージョンとCPUアーキテクチャに応じて**パッケージタイプ**のドロップダウンから`deb`パッケージを選択します。
3. 下記のリンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールも可能です。

#### EMQXの起動

systemdサービスとしてEMQXを起動します。

```bash
sudo systemctl start emqx
```

#### EMQXのアンインストール

EMQXをアンインストールするには、以下を実行してください。

```
sudo apt remove --purge emqx
```

### tar.gzパッケージによるインストール

1. 公式ダウンロードページの[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン`@EE_VERSION@`を選択し、必要なバージョンとCPUアーキテクチャに応じて**パッケージタイプ**のドロップダウンから`tar.gz`パッケージを選択します。
3. 下記のリンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールも可能です。

#### EMQXの起動

インストール後、以下のコマンドを実行してEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
