# CentOS/RHELへのEMQXインストール

このページでは、CentOS/RHELシステムにEMQXをインストールし起動する手順を案内します。

対応バージョン:

- Amazon Linux 2023
- Amazon Linux 2
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)
- CentOS 7 (RHEL 7)

以下のセクションでは、CentOS 8システムに最新バージョンのEMQXをインストールする例を示します。その他のシステムへのインストールや別バージョンのインストールについては、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご参照ください。

## rpmによるインストール

1. 公式ダウンロードページの[**CentOS/RHELタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)にアクセスします。
2. 最新バージョン `@EE_VERSION@` を選択します。**Package Type**のドロップダウンから、必要なCPUアーキテクチャに応じて `RHEL 8 (CentOS 8) amd64` または `RHEL 8 (CentOS 8) arm64` -> `rpm` パッケージを選択します。
3. 下記リンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順も併せてご利用いただけます。

### EMQXの起動

systemdサービスとしてEMQXを起動します。

```bash
sudo systemctl start emqx
```

### EMQXのアンインストール

EMQXをアンインストールするには、以下を実行してください。

```
sudo yum remove emqx
```

## tar.gzによるインストール

1. 公式ダウンロードページの[**CentOS/RHELタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)にアクセスします。
2. 最新バージョン `@EE_VERSION@` を選択します。**Package Type**のドロップダウンから、必要なCPUアーキテクチャに応じて `RHEL 8 (CentOS 8) amd64` または `RHEL 8 (CentOS 8) arm64` -> `tar.gz` パッケージを選択します。
3. 下記リンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順も併せてご利用いただけます。

### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
