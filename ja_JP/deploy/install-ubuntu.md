# UbuntuへのEMQXインストール

このページでは、UbuntuシステムへのEMQXのインストールおよび起動方法について説明します。

対応バージョン：

- Ubuntu 24.04
- Ubuntu 22.04

## Aptパッケージマネージャーによるインストール

EMQXは、Aptパッケージマネージャーによるインストールをサポートしており、ユーザーに便利で信頼性の高いEMQXのインストールおよび更新管理方法を提供します。以下はaptを使ったEMQXのインストール手順です。

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

### debパッケージによるインストール

1. 公式ダウンロードページの[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン `@EE_VERSION@` を選択し、**Package Type**ドロップダウンから必要なバージョンとCPUアーキテクチャに応じた`deb`パッケージを選択します。
3. 下記リンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールも可能です。

#### EMQXの起動

systemdサービスとしてEMQXを起動します。

```bash
sudo systemctl start emqx
```

#### EMQXのアンインストール

EMQXをアンインストールするには、以下のコマンドを実行します。

```
sudo apt remove --purge emqx
```

### tar.gzパッケージによるインストール

1. 公式ダウンロードページの[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン `@EE_VERSION@` を選択し、**Package Type**ドロップダウンから必要なバージョンとCPUアーキテクチャに応じた`tar.gz`パッケージを選択します。
3. 下記リンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールも可能です。

#### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
