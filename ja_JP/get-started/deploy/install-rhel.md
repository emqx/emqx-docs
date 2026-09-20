# CentOS/RHELへのEMQXインストール

このページでは、CentOS/RHELシステムにEMQXをインストールし、起動する方法を案内します。

対応バージョン：

- Amazon Linux 2023
- Rocky Linux 10 (RHEL 10)
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)

以下のセクションでは、公式ダウンロードサイトから最新バージョンのEMQXをダウンロードする方法を示します。その他のシステムへのインストールや別バージョンのインストールについては、詳細情報が記載された[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご覧ください。

## rpmによるインストール

1. 公式ダウンロードページにアクセスし、[**CentOS/RHELタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)を選択します。
2. 最新バージョン `@EE_VERSION@` を選択します。**パッケージタイプ**のドロップダウンから、ご利用のRHEL互換ディストリビューションのバージョンおよびCPUアーキテクチャに合った `rpm` パッケージを選択してください。
3. 下のリンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順も併せてご利用いただけます。

### EMQXの起動

EMQXをsystemdサービスとして起動します。

```bash
sudo systemctl start emqx
```

### EMQXのアンインストール

EMQXをアンインストールするには、以下のコマンドを実行してください。

```
sudo yum remove emqx
```

## tar.gzによるインストール

1. 公式ダウンロードページにアクセスし、[**CentOS/RHELタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)を選択します。
2. 最新バージョン `@EE_VERSION@` を選択します。**パッケージタイプ**のドロップダウンから、ご利用のRHEL互換ディストリビューションのバージョンおよびCPUアーキテクチャに合った `tar.gz` パッケージを選択してください。
3. 下のリンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順も併せてご利用いただけます。

### EMQXの起動

インストール後、以下のコマンドを実行してEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
