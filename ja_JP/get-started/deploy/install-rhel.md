# CentOS/RHELへのEMQXインストール

このページでは、CentOS/RHELシステムにEMQXをインストールし起動する方法を案内します。

対応バージョン：

- Amazon Linux 2023
- Rocky Linux 10 (RHEL 10)
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)

以下のセクションでは、公式ダウンロードサイトから最新バージョンのEMQXをダウンロードする方法を示します。その他のシステムへのインストールや他バージョンのインストールについては、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご参照ください。

## rpmによるインストール

1. 公式ダウンロードページにアクセスし、[**CentOS/RHELタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)を選択します。
2. 最新バージョン `@EE_VERSION@` を選択します。**パッケージタイプ**のドロップダウンから、RHEL互換のディストリビューションバージョンとCPUアーキテクチャに合った `rpm` パッケージを選択します。
3. 下のリンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールすることも可能です。

### EMQXの起動

systemdサービスとしてEMQXを起動します。

```bash
sudo systemctl start emqx
```

### EMQXのアンインストール

EMQXをアンインストールするには、以下を実行します。

```
sudo yum remove emqx
```

## tar.gzによるインストール

1. 公式ダウンロードページにアクセスし、[**CentOS/RHELタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)を選択します。
2. 最新バージョン `@EE_VERSION@` を選択します。**パッケージタイプ**のドロップダウンから、RHEL互換のディストリビューションバージョンとCPUアーキテクチャに合った `tar.gz` パッケージを選択します。
3. 下のリンクをクリックしてダウンロードします。コマンドラインガイドの手順に従ってダウンロードおよびインストールすることも可能です。

### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
