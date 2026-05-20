# CentOS/RHELへのEMQXインストール

このページでは、CentOS/RHELシステムにEMQXをインストールして起動する方法を案内します。

対応バージョン:

- Amazon Linux 2023
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)

以下のセクションでは、CentOS 8システムにEMQXの最新バージョンをインストールする例を示します。その他のシステムへのインストールや別バージョンのインストールについては、詳細情報が記載された[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご参照ください。

## rpmによるインストール

1. 公式ダウンロードページにアクセスし、[**CentOS/RHEL**タブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)を選択します。
2. 最新バージョン`@EE_VERSION@`を選択します。**Package Type**のドロップダウンから、必要なCPUアーキテクチャに応じて`RHEL 8 (CentOS 8) amd64`または`RHEL 8 (CentOS 8) arm64` -> `rpm`パッケージを選択します。
3. 下記のリンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順も併せてご利用いただけます。

### EMQXの起動

EMQXをsystemdサービスとして起動します。

```bash
sudo systemctl start emqx
```

### EMQXのアンインストール

EMQXをアンインストールするには、以下を実行します。

```
sudo yum remove emqx
```

## tar.gzによるインストール

1. 公式ダウンロードページにアクセスし、[**CentOS/RHEL**タブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)を選択します。
2. 最新バージョン`@EE_VERSION@`を選択します。**Package Type**のドロップダウンから、必要なCPUアーキテクチャに応じて`RHEL 8 (CentOS 8) amd64`または`RHEL 8 (CentOS 8) arm64` -> `tar.gz`パッケージを選択します。
3. 下記のリンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順も併せてご利用いただけます。

### EMQXの起動

インストール後、以下のコマンドを実行してEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
