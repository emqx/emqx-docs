# UbuntuへのEMQXインストール

このページでは、UbuntuシステムにEMQXをインストールし起動する手順を案内します。

対応バージョン：

- Ubuntu 24.04
- Ubuntu 22.04

## Aptパッケージマネージャーでのインストール

EMQXはAptパッケージマネージャーによるインストールをサポートしており、ユーザーの皆様に便利で信頼性の高いEMQXのインストールおよび更新方法を提供します。以下はaptを使ったEMQXのインストール方法です：

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

EMQXはdebパッケージまたはtar.gzパッケージによるインストールもサポートしています。他の対応システムでのインストールや別バージョンを試す場合は、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご参照ください。

### debパッケージでのインストール

1. 公式ダウンロードページにアクセスし、[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)を選択します。
2. 最新バージョン `@EE_VERSION@` を選び、**Package Type**ドロップダウンから必要なバージョンとCPUアーキテクチャに合った`deb`パッケージを選択します。
3. 下記リンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順もご利用いただけます。

#### EMQXの起動

systemdサービスとしてEMQXを起動します。

```bash
sudo systemctl start emqx
```

#### EMQXのアンインストール

EMQXをアンインストールするには、以下のコマンドを実行してください。

```
sudo apt remove --purge emqx
```

### tar.gzパッケージでのインストール

1. 公式ダウンロードページにアクセスし、[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)を選択します。
2. 最新バージョン `@EE_VERSION@` を選び、**Package Type**ドロップダウンから必要なバージョンとCPUアーキテクチャに合った`tar.gz`パッケージを選択します。
3. 下記リンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順もご利用いただけます。

#### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
