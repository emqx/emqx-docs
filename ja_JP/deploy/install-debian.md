# DebianへのEMQXインストール

このページでは、Debianシステムに最新のEMQXをインストールし起動する手順を案内します。

対応バージョン：

- Debian 13
- Debian 12
- Debian 11

他のシステムへのインストールや別バージョンのインストールについては、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご覧ください。

::: tip
EMQX 6.3.0以降、DEBインストールはDockerイメージと同じ`/opt/emqx/...`パスをサポートしています。パスの対応については[ファイルとディレクトリ](./install.md#files-and-directories)を参照してください。
:::

## debパッケージでのインストール

1. ダウンロードサイトにアクセスし、[**Debianタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Debian)を選択します。
2. 最新バージョン`@EE_VERSION@`を選択し、**パッケージタイプ**のドロップダウンから、DebianのバージョンとCPUアーキテクチャに応じた`deb`パッケージを選択します。
3. 下のリンクをクリックしてダウンロードします。ページのコマンド指示に従うことも可能です。

### EMQXの起動

systemdサービスとしてEMQXを起動するには、以下を実行します。

```bash
sudo systemctl start emqx
```

::: tip
EMQX 6.3.0以降、`EMQX_SECURITY_PROFILE`などの起動時環境変数は`/etc/emqx/emqx.env`に設定します。`emqx`コマンドはサービス起動、フォアグラウンド起動、`emqx ctl`実行時にこのファイルを読み込みます。パッケージのアップグレード時もこのファイルの変更は保持されます。起動時環境変数の変更を反映するにはEMQXノードを再起動してください。[起動時環境変数](../configuration/configuration.md#boot-time-environment-variables)を参照してください。
:::

### EMQXのアンインストール

EMQXをアンインストールするには、以下を実行します。

```shell
sudo apt remove --purge emqx
```

## tar.gzパッケージでのインストール

1. ダウンロードサイトにアクセスし、[**Debianタブ**](https://www.emqx.com/en/downloads-and-install/enterprise?os=Debian)を選択します。
2. 最新バージョン`@EE_VERSION@`を選択し、**パッケージタイプ**のドロップダウンから、DebianのバージョンとCPUアーキテクチャに応じた`tar.gz`パッケージを選択します。
3. 下のリンクをクリックしてダウンロードします。ページのコマンド指示に従うことも可能です。

### EMQXの起動

インストール後、以下のコマンドでEMQXを起動します。

```bash
./emqx/bin/emqx foreground
```
