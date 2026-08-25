# macOSへのEMQXインストール
このページでは、macOSにzipファイルを使ってEMQXをインストールし、起動する方法を案内します。

対応バージョン：

- macOS 26
- macOS 15
- macOS 14

他のシステムへのインストールや別バージョンのインストールについては、[EMQX Enterpriseダウンロードサイト](https://www.emqx.com/en/downloads-and-install/enterprise)をご覧ください。

## EMQX Enterpriseのインストール

1. ダウンロードサイトにアクセスし、[**macOS**タブ](https://www.emqx.com/en/downloads-and-install/enterprise?os=macOS)を選択します。
2. 最新バージョン`@EE_VERSION@`を選択します。**パッケージタイプ**のドロップダウンから、macOSのバージョンとCPUアーキテクチャに応じた`zip`パッケージを選択してください。
3. 以下のリンクをクリックしてダウンロードします。ページ上のコマンド指示に従っても構いません。

## EMQXの起動と停止

EMQXはデーモンモード、フォアグラウンドモード、またはインタラクティブモードで起動できます。デフォルト設定では、同時に起動できるEMQXのインスタンスは1つだけです。

```bash
# デーモンとして起動
./bin/emqx start

# フォアグラウンドで起動
./bin/emqx foreground

# インタラクティブモードで起動（Erlangシェル付き）
./bin/emqx console
```

起動に成功すると、フォアグラウンドまたはインタラクティブモードで起動した場合に以下のメッセージが表示されます。

```bash
EMQX Enterprise @EE_VERSION@ is running now!
```

また、本番環境の運用者向けの警告メッセージが表示されることがありますが、テストや実験、クライアント開発のためにローカル環境でEMQXを使用する場合は無視して問題ありません。

```bash
ERROR: DB Backend is RLOG, but an incompatible OTP version has been detected. Falling back to using Mnesia DB backend.
WARNING: ulimit -n is 256; 1024 is the recommended minimum.
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /path/to/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

EMQXの状態は以下のコマンドで確認できます。

```bash
./bin/emqx ctl status
```

ブラウザを起動し、アドレスバーに`http://localhost:18083/`（`localhost`はIPアドレスに置き換え可能）を入力すると、[EMQXダッシュボード](../dashboard/introduction.md)にアクセスできます。ここからクライアントへの接続や稼働状況の確認が可能です。

初期のユーザー名とパスワードは`admin`と`public`です。ログイン後に初期パスワードの変更を求められます。

EMQXの停止方法：

* デーモンモードで起動している場合は、`emqx stop`または`bin/emqx stop`を使用します。
* フォアグラウンドモードの場合は、Ctrl+Cを押します。
* インタラクティブモードの場合は、Ctrl+Cを2回押します。
