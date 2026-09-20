# UbuntuへのEMQXインストール

このページでは、UbuntuシステムにEMQXをインストールし起動する方法を案内します。

対応バージョン：

- Ubuntu 24.04
- Ubuntu 22.04

## Aptパッケージマネージャーでのインストール

EMQXは、Aptパッケージマネージャーによるインストールをサポートしており、ユーザーの皆様に便利で信頼性の高いEMQXのインストールおよびアップデート管理手段を提供します。以下はaptを使ったEMQXのインストール手順です。

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

EMQXはdebパッケージまたはtar.gzパッケージによるインストールもサポートしています。その他の対応システムへのインストールや別バージョンの試用については、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご参照ください。

### debパッケージでのインストール

1. 公式ダウンロードページの[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン`@EE_VERSION@`を選択し、必要なバージョンとCPUアーキテクチャに応じて**パッケージタイプ**のドロップダウンから`deb`パッケージを選びます。
3. 下記リンクをクリックしてダウンロードしてください。コマンドラインでのダウンロードおよびインストール手順もご利用いただけます。

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

1. 公式ダウンロードページの[**Ubuntuタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu)にアクセスします。
2. 最新バージョン`@EE_VERSION@`を選択し、必要なバージョンとCPUアーキテクチャに応じて**パッケージタイプ**のドロップダウンから`tar.gz`パッケージを選びます。
3. 下記リンクをクリックしてダウンロードしてください。コマンドラインでのダウンロードおよびインストール手順もご利用いただけます。

#### EMQXの起動

インストール後、以下のコマンドを実行してEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
