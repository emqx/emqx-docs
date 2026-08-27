# CentOS/RHELへのEMQXインストール

このページでは、CentOS/RHELシステムにEMQXをインストールし起動する手順を説明します。

対応バージョン：

- Amazon Linux 2023
- Rocky Linux 10 (RHEL 10)
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)

以下のセクションでは、公式ダウンロードサイトから最新バージョンのEMQXをダウンロードする方法を示します。その他のシステムへのインストールや別バージョンのインストールについては、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご参照ください。

## rpmによるインストール

1. 公式ダウンロードページの[**CentOS/RHELタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)にアクセスします。  
2. 最新バージョン `@EE_VERSION@` を選択します。**パッケージタイプ**のドロップダウンから、RHEL互換のディストリビューションバージョンとCPUアーキテクチャに合った `rpm` パッケージを選択します。  
3. 下のリンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順も併せてご利用いただけます。

### EMQXの起動

EMQXをsystemdサービスとして起動します。

```bash
sudo systemctl start emqx
```

::: tip
EMQX 6.3.0以降、`EMQX_SECURITY_PROFILE`などの起動時環境変数は`/etc/emqx/emqx.env`に設定してください。`emqx`コマンドはサービス起動、フォアグラウンド起動、`emqx ctl`実行時にこのファイルを読み込みます。パッケージのアップグレード時もこのファイルの変更は保持されます。起動時環境変数の変更を反映するにはEMQXノードを再起動してください。詳細は[起動時環境変数](../configuration/configuration.md#boot-time-environment-variables)を参照してください。
:::

### EMQXのアンインストール

EMQXをアンインストールするには、以下のコマンドを実行します。

```
sudo yum remove emqx
```

## tar.gzによるインストール

1. 公式ダウンロードページの[**CentOS/RHELタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL)にアクセスします。  
2. 最新バージョン `@EE_VERSION@` を選択します。**パッケージタイプ**のドロップダウンから、RHEL互換のディストリビューションバージョンとCPUアーキテクチャに合った `tar.gz` パッケージを選択します。  
3. 下のリンクをクリックしてダウンロードします。コマンドラインでのダウンロードおよびインストール手順も併せてご利用いただけます。

### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
