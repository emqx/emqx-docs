# インストールとマイグレーション

本章では、EMQXの基本的なインストール手順、最低限のハードウェア仕様、および将来の設定や保守作業を容易にするためのファイルおよびディレクトリの場所について説明します。また、EMQX Enterpriseのライセンス設定方法とEMQX 4.4からEMQX 5.1へのマイグレーション方法についても解説します。

## 対応オペレーティングシステム

以下の表は、EMQXがサポートするオペレーティングシステムとそのバージョンを示しています。

| オペレーティングシステム                  | 対応バージョン                      | x86_64/amd64 | arm64 |
| :----------------------------------------- | :-------------------------------- | :----------- | :----- |
| [Ubuntu](./install-ubuntu.md)               | Ubuntu 18.04<br />Ubuntu 20.04<br />Ubuntu 22.04<br />Ubuntu 24.04 | 対応 | 対応 |
| [Debian](./install-debian.md)               | Debian 11<br />Debian 12<br />Debian 13 | 対応 | 対応 |
| [CentOS/RHEL](./install-rhel.md)            | CentOS 7<br />Rocky Linux 8<br />Rocky Linux 9 | 対応 | 対応 |
| [Amazon Linux](./install-rhel.md)            | Amazon Linux 2<br />Amazon Linux 2023 | 対応 | 対応 |
| [macOS](./install-macOS.md)                  | macOS 14<br />macOS 15 | 対応 | 対応 |

<!-- ## ハードウェア仕様

クライアント接続数、メッセージレート、メッセージサイズ、および有効化された機能に応じて、EMQXの最低ハードウェア仕様は異なります。

以下は、シンプルなワークロードでEMQXを実行し、100,000クライアント接続および100,000メッセージ/秒のスループットをサポートするためのハードウェア仕様です。

| 項目           | 最小構成             | 推奨構成               |
| -------------- | -------------------- | ---------------------- |
| **ノード**     | 1                    | 2                      |
| **CPU**        | 1コア                | 16コア                 |
| **メモリ**     | 512 MB               | 32 GB                  |
| **ディスク容量** | 1 GB                 | 50 GB                  |

::: tip

本番環境では、[Server Estimate](https://www.emqx.com/en/server-estimate)計算ツールを使用して、最大接続数やメッセージスループットに応じた推奨ハードウェア仕様を算出できます。

::: -->

## インストール環境

EMQXを動作させるErlang VMは、[ファイル名](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#unicode-filenames)や対話型Erlangシェルの[端末IO](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#the-interactive-shell)などのUnicode対応機能を有効にするために、システムのロケール設定に依存しています。

Linux OSを使用する場合は、EMQXを起動する前にシステム環境でUTF-8ロケールが有効になっていることを確認することを推奨します。以下のタブから各プラットフォームでのUTF-8ロケールの有効化方法をご覧ください。

:::: tabs

::: tab Amazon Linux

[`cloud-init`](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/amazon-linux-ami-basics.html#amazon-linux-cloud-init)の設定でUTF-8ロケールを有効化します。

```bash
cat <<EOF | sudo tee /etc/cloud/cloud.cfg.d/99_locale.cfg
#cloud-config
locale: C.utf8
EOF
```

:::

::: tab CentOS

通常、systemd環境下で`localectl`により有効化されます。

```bash
sudo localectl set-locale LANG=C.UTF-8
```

:::

::: tab Debian

UTF-8ロケールは以下の2通りの方法で有効化できます。

- systemd環境下では通常[`localectl`](https://www.freedesktop.org/software/systemd/man/localectl.html)で設定します。

  ```bash
  sudo localectl set-locale LANG=C.UTF-8
  ```

- それ以外の場合は[`update-locale`](https://manpages.debian.org/buster/locales/update-locale.8.en.html)で設定可能です。

  ```bash
  sudo update-locale LANG=C.UTF-8
  ```

:::

::: tab Ubuntu

[`update-locale`](https://manpages.ubuntu.com/manpages/jammy/man8/update-locale.8.html)でUTF-8ロケールを有効化します。

```bash
sudo update-locale LANG=C.UTF-8
```

:::

::::

## ポート使用状況

EMQXはデフォルトで以下のポートを使用します。これらのポートが他のアプリケーションで使用されていないことを確認し、必要に応じてファイアウォールを開放してEMQXが正常に動作するようにしてください。

| ポート  | プロトコル | 説明                                                  |
| ------- | ---------- | ----------------------------------------------------- |
| 1883    | TCP        | 暗号化されていないMQTT接続向けのTCPリスナーポート。 |
| 8883    | TCP        | 暗号化されたMQTT接続向けのSSL/TLSリスナーポート。    |
| 8083    | TCP        | WebSocket経由のMQTT通信向けのWebSocketリスナーポート。 |
| 8084    | TCP        | 暗号化されたWebSocket接続向けのWSSリスナーポート。    |
| 18083   | HTTP       | EMQXダッシュボードおよびREST APIの管理コンソール用ポート。 |
| 4370    | TCP        | Erlang分散ポート。実際のポートはノード名により`BasePort (4370) + Offset`となる場合があります。 |
| 5370    | TCP        | クラスターRPCポート（Docker環境では5369）。実際のポートはノード名により`BasePort (5370) + Offset`となる場合があります。 |

::: tip 注意

クラスターを形成していなくても、EMQXはポート4370と5370でリスンします。この2つのポートは固定で変更できません。Offsetはノード名の`Name@Host`のName部分の数値サフィックスで決定されます。数値サフィックスがない場合はデフォルトで0となります。詳細は[ポートマッピング](../../guides/cluster/security.md#port-mapping)を参照してください。

:::

## ファイルとディレクトリ

インストール後、EMQXは実行ファイルや設定ファイル、データ、ログを格納するためのいくつかのディレクトリを作成します。以下の表は、インストール方法ごとに作成されるディレクトリとそのパスを示しています。

| ディレクトリ  | 説明               | tar.gzインストール時のパス | RPM/DEBインストール時のパス |
| ------------ | ------------------ | -------------------------- | ---------------------------- |
| `etc`        | 静的設定ファイル     | `./etc`                   | `/etc/emqx`                  |
| `data`       | データベースおよび設定 | `./data`                  | `/var/lib/emqx`              |
| `log`        | ログファイル         | `./log`                   | `/var/log/emqx`              |
| `releases`   | 起動指示ファイル     | `./releases`              | `/usr/lib/emqx/releases`     |
| `bin`        | 実行ファイル         | `./bin`                   | `/usr/lib/emqx/bin`          |
| `lib`        | Erlangコード         | `./lib`                   | `/usr/lib/emqx/lib`          |
| `erts-*`     | Erlangランタイム     | `./erts-*`                | `/usr/lib/emqx/erts-*`       |
| `plugins`    | プラグイン           | `./plugins`               | `/usr/lib/emqx/plugins`      |

::: tip

1. 圧縮パッケージでインストールした場合、ディレクトリはソフトウェアをインストールしたディレクトリの相対パスです。
2. Dockerコンテナでインストールした場合、EMQXは`/opt/emqx`ディレクトリにインストールされます。
3. `data`、`log`、`plugins`ディレクトリは設定ファイルで変更可能です。パフォーマンス向上のため、`data`ディレクトリは高性能ディスクにマウントすることを推奨します。同一クラスターに属するノードでは`data`ディレクトリの設定を統一してください。クラスターの詳細は[クラスター](../../develop/cluster/introduction.md)を参照してください。

:::

以下の表は、主なディレクトリのファイルやサブフォルダの説明です。

| ディレクトリ | 説明               | 権限   | ファイル                                                        |
| ------------ | ------------------ | ------ | --------------------------------------------------------------- |
| bin          | 実行ファイル         | 読み取り | `emqx`および`emqx.cmd`: EMQXの実行ファイル。詳細は[コマンドラインインターフェース](../../guides/cli.md)を参照してください。 |
| etc          | 設定ファイル         | 読み取り | `base.hocon`: ランタイム設定変更で上書き可能なベース設定。<br /><br />`emqx.conf`: 上書き不可の静的設定。<br /><br />`emqx-example-en.conf`: EMQXのデモ設定ファイルで、すべての設定項目を含みます。<br /><br />`acl.conf`: デフォルトのACLルール。<br /><br />`vm.args`: Erlang VMの起動パラメータ。<br /><br />`certs/`: EMQXのSSLリスナー用X.509鍵および証明書ファイル。外部システム連携時のSSL/TLS接続にも使用される場合があります。 |
| data         | 運用データ           | 書き込み | `authz`: REST APIやダッシュボードからアップロードされたファイル認可ルールを格納。詳細は[認可 - ファイル](../../guides/access-control/authz/file.md)を参照。<br /><br />`certs`: REST APIやダッシュボードからアップロードされた証明書ファイルを格納。<br /><br />`configs`: 起動時に生成される設定ファイルやAPI/CLIからの設定上書きを格納。<br /><br />`mnesia`: EMQXの運用データを格納する組み込みデータベース。アラーム記録、クライアントの認証・認可情報、ダッシュボードユーザー情報などを含みます。**このディレクトリを削除すると、これらの運用データはすべて失われます。**<br /><br />  —  ノードごとに`emqx@127.0.0.1`のようなサブディレクトリが存在する場合があります。ノード名変更時は対応するサブディレクトリも削除または移動してください。<br /><br />  —  組み込みデータベースのクエリは`emqx ctl mnesia`コマンドを使用します。詳細は[管理コマンドCLI](https://docs.emqx.com/en/enterprise/v5.0/admin/cli.html)を参照。<br /><br />`patches`: EMQXがホットパッチとして読み込む`.beam`ファイルを格納。迅速な修正に利用可能。<br /><br />`trace`: オンライントレースログファイル。<br /><br />本番環境では、データ安全のため`trace`フォルダを除く`data`ディレクトリの定期バックアップを推奨します。 |
| log          | 運用ログ             | 読み取り | `emqx.log.*`: EMQXの運用ログ。詳細は[ログ](../../guides/observability/log.md)を参照。 |

:::tip

EMQXは設定情報を`data/configs`および`etc`ディレクトリに保存します。`etc`ディレクトリは読み取り専用の設定ファイルを格納し、ダッシュボードやREST APIからの設定更新は`data/configs`に保存され、ランタイムでのホットリロードをサポートします。

- `etc/base.hocon`: ランタイム設定変更で上書き可能なベース設定。
- `etc/emqx.conf`: 上書き不可の静的設定。
- `data/configs/cluster.hocon`: ランタイム設定の上書き。

EMQXはこれらのファイルから設定項目を読み込み、Erlangネイティブの設定ファイル形式に変換してランタイムに適用します。

:::
