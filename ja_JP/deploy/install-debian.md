# DebianへのEMQXインストール

このページでは、Debianシステムに最新のEMQXをインストールし、起動する手順を案内します。

対応バージョン：

- Debian 13
- Debian 12
- Debian 11

他のシステムへのインストールや他バージョンのインストールについては、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご覧ください。

## debパッケージでのインストール

1. ダウンロードサイトにアクセスし、[**Debian**タブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=Debian)を選択します。
2. 最新バージョン `@EE_VERSION@` を選択します。**Package Type**のドロップダウンから、DebianのバージョンとCPUアーキテクチャに応じた`deb`パッケージを選択してください。
3. 下のリンクをクリックしてダウンロードします。ページ内のコマンド指示に従うことも可能です。

### EMQXの起動

systemdサービスとしてEMQXを起動するには、以下を実行します。

```bash
sudo systemctl start emqx
```

### EMQXのアンインストール

EMQXをアンインストールするには、以下を実行します。

```shell
sudo apt remove --purge emqx
```

## tar.gzパッケージでのインストール

1. ダウンロードサイトにアクセスし、[**Debian**タブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=Debian)を選択します。
2. 最新バージョン `@EE_VERSION@` を選択します。**Package Type**のドロップダウンから、DebianのバージョンとCPUアーキテクチャに応じた`tar.gz`パッケージを選択してください。
3. 下のリンクをクリックしてダウンロードします。ページ内のコマンド指示に従うことも可能です。

### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
