# インストールとマイグレーション

本章では、EMQXの基本的なインストール手順、最低限のハードウェア仕様、および将来の設定やメンテナンス作業を容易にするためのファイルおよびディレクトリの場所について説明します。また、EMQX Enterpriseのライセンス設定方法とEMQX 4.4からEMQX 5.1へのマイグレーション方法についても解説します。

## サポートされているオペレーティングシステム

以下の表は、EMQXがサポートするオペレーティングシステムとそのバージョンを示しています。

| オペレーティングシステム          | サポートされているバージョン                   | x86_64/amd64 | arm64 |
| :--------------------------------- | :-------------------------------------------- | :----------- | :---- |
| [Ubuntu](./install-ubuntu.md)      | Ubuntu 22.04<br />Ubuntu 24.04                 | Yes          | Yes   |
| [Debian](./install-debian.md)      | Debian 11<br />Debian 12<br />Debian 13        | Yes          | Yes   |
| [CentOS/RHEL](./install-rhel.md)   | Rocky Linux 8<br />Rocky Linux 9                | Yes          | Yes   |
| [Amazon Linux](./install-rhel.md)  | Amazon Linux 2023                               | Yes          | Yes   |
| [macOS 14+](./install-macOS.md)    | macOS 14<br />macOS 15                           | No           | Yes   |

<!-- ## ハードウェア仕様

クライアント接続数、メッセージレート、メッセージサイズ、および有効化されている機能に応じて、EMQXの最低ハードウェア仕様は異なります。

以下は、単純なワークロードでEMQXを実行し、100,000のクライアント接続数および毎秒100,000メッセージのスループットをサポートするためのハードウェア仕様です。

| 項目           | 最小構成           | 推奨構成           |
| -------------- | ------------------ | ------------------ |
| **ノード**     | 1                  | 2                  |
| **CPU**        | 1コア              | 16コア             |
| **メモリ**     | 512 MB             | 32 GB              |
| **ディスク容量** | 1 GB               | 50 GB              |

::: tip

本番環境では、[Server Estimate](https://www.emqx.com/en/server-estimate) 計算ツールを使用して、最大接続数やメッセージスループットに応じた推奨ハードウェア仕様を算出できます。

::: -->

## インストール環境

EMQXを動作させるErlang VMは、[ファイル名](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#unicode-filenames)や[対話型Erlangシェルの端末IO](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#the-interactive-shell)などの機能におけるUnicodeサポートを有効にするために、システムのロケール設定に依存しています。

Linux OSを使用する場合は、EMQXを起動する前にシステム環境でUTF-8ロケールが有効になっていることを確認することを推奨します。以下のタブから各プラットフォームでのUTF-8ロケールの有効化方法をご覧ください。

:::: tabs

::: tab Amazon Linux

[`cloud-init`](https://docs.aws.amazon.com/linux/al2023/ug/cloud-init.html) の設定でUTF-8ロケールを有効化します。

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

- systemd環境下では通常[`localectl`](https://www.freedesktop.org/software/systemd/man/latest/localectl.html)で有効化されます。

  ```bash
  sudo localectl set-locale LANG=C.UTF-8
  ```

- それ以外の場合は[`update-locale`](https://manpages.debian.org/buster/locales/update-locale.8.en.html)で有効化できます。

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

EMQXはデフォルトで以下のポートを使用します。これらのポートが他のアプリケーションによって使用されていないことを確認し、必要に応じてファイアウォールを開放してEMQXが正常に動作するようにしてください。

| ポート  | プロトコル | 説明                                                        |
| ------- | ---------- | ----------------------------------------------------------- |
| 1883    | TCP        | TCP上のMQTTリスナーポート。主に暗号化されていないMQTT接続に使用されます。 |
| 8883    | TCP        | SSL/TLS上のMQTTリスナーポート。暗号化されたMQTT接続に使用されます。   |
| 8083    | TCP        | WebSocket上のMQTTリスナーポート。WebSocket経由のMQTT通信に使用されます。 |
| 8084    | TCP        | WSS（SSL上のWebSocket）リスナーポート。暗号化されたWebSocket接続に使用されます。 |
| 18083   | HTTP       | EMQXダッシュボードおよびREST APIの管理コンソールとAPIインターフェース用ポート。 |
| 4370    | TCP        | Erlang分散ポート。実際のポートはノード名に応じて`BasePort (4370) + Offset`となります。 |
| 5370    | TCP        | クラスターRPCポート（Docker環境では5369）。実際のポートはノード名に応じて`BasePort (5370) + Offset`となります。 |

::: tip 注意

クラスターを形成していなくても、EMQXはポート4370および5370をリッスンします。これらの2つのポートは固定で変更できません。Offsetはノード名のName部分（`Name@Host`）の数値サフィックスによって決まります。数値サフィックスがない場合はデフォルトで0となります。詳細は[ポートマッピング](./cluster/security.md#port-mapping)を参照してください。

:::

## ファイルとディレクトリ

インストール後、EMQXは実行ファイルや設定ファイル、データ、ログを保存するためのいくつかのディレクトリを作成します。以下の表は、インストール方法ごとに作成されるディレクトリとそのパスを示しています。

| ディレクトリ | 説明               | tar.gzでインストールした場合 | RPM/DEBでインストールした場合 |
| ------------ | ------------------ | ---------------------------- | ----------------------------- |
| `etc`        | 静的設定ファイル     | `./etc`                     | `/etc/emqx`                   |
| `data`       | データベースおよび設定 | `./data`                    | `/var/lib/emqx`               |
| `log`        | ログファイル         | `./log`                     | `/var/log/emqx`               |
| `releases`   | 起動指示ファイル     | `./releases`                | `/usr/lib/emqx/releases`      |
| `bin`        | 実行ファイル         | `./bin`                     | `/usr/lib/emqx/bin`           |
| `lib`        | Erlangコード         | `./lib`                     | `/usr/lib/emqx/lib`           |
| `erts-*`     | Erlangランタイム     | `./erts-*`                  | `/usr/lib/emqx/erts-*`        |
| `plugins`    | プラグイン           | `./plugins`                 | `/usr/lib/emqx/plugins`       |

::: tip

1. 圧縮パッケージでインストールした場合、ディレクトリはソフトウェアをインストールしたディレクトリを基準とした相対パスです。  
2. Dockerコンテナでインストールした場合、EMQXは`/opt/emqx`ディレクトリにインストールされます。  
3. `data`、`log`、`plugins`ディレクトリは設定ファイルで変更可能です。パフォーマンス向上のため、`data`ディレクトリは高速ディスクにマウントすることを推奨します。同一クラスターに属するノードでは、`data`ディレクトリの設定を統一してください。クラスターの詳細は[クラスター](./cluster/introduction.md)を参照してください。

:::

以下の表は、一部ディレクトリのファイルやサブフォルダの説明です。

| ディレクトリ | 説明               | 権限       | ファイル                                                       |
| ------------ | ------------------ | ---------- | -------------------------------------------------------------- |
| bin          | 実行ファイル         | 読み取り   | `emqx` および `emqx.cmd`: EMQXの実行ファイル。詳細は[コマンドラインインターフェース](../admin/cli.md)を参照してください。 |
| etc          | 設定ファイル         | 読み取り   | `base.hocon`: ランタイム設定変更で上書き可能なベース設定。<br /><br />`emqx.conf`: 上書き不可の静的設定。<br /><br />`emqx-example-en.conf`: EMQXの全設定項目を含むデモ設定ファイル。<br /><br />`acl.conf`: デフォルトのACLルール。<br /><br />`vm.args`: Erlang仮想マシンの動作パラメータ。<br /><br />`certs/`: EMQXのSSLリスナー用X.509鍵および証明書ファイル。外部システムとのSSL/TLS接続時にも使用される場合があります。 |
| data         | 運用データ           | 書き込み   | `authz`: REST APIまたはダッシュボードからアップロードされたファイル認可ルールを格納します。詳細は[認可 - ファイル](../access-control/authz/file.md)を参照してください。<br /><br />`certs`: REST APIまたはダッシュボードからアップロードされた証明書ファイルを格納します。<br /><br />`configs`: 起動時に生成された設定ファイルやAPI・CLIからの設定上書きを格納します。<br /><br />`mnesia`: EMQXの運用データを格納する組み込みデータベースで、アラーム記録、クライアントの認証・認可情報、ダッシュボードユーザー情報などを含みます。**このディレクトリを削除すると、これらの運用データはすべて失われます。**<br /><br />  —  ノードごとに名前を付けたサブディレクトリ（例：`emqx@127.0.0.1`）を含む場合があります。ノード名変更時は対応するサブディレクトリも削除または移動してください。<br /><br />  —  組み込みデータベースの照会には`emqx ctl mnesia`コマンドを使用します。詳細は[管理コマンドCLI](https://docs.emqx.com/en/enterprise/v5.0/admin/cli.html)を参照してください。<br /><br />`patches`: EMQXがホットパッチとしてロードする`.beam`ファイルを格納します。迅速な修正に使用可能です。<br /><br />`trace`: オンライントレースログファイル。<br /><br />本番環境では、データの安全性確保のため、`trace`フォルダを除く`data`ディレクトリの定期的なバックアップを推奨します。 |
| log          | 運用ログ             | 読み取り   | `emqx.log.*`: EMQXの運用ログ。詳細は[ログ](../observability/log.md)を参照してください。 |

:::tip

EMQXは設定情報を`data/configs`および`etc`ディレクトリに保存します。`etc`ディレクトリは読み取り専用の設定ファイルを格納し、ダッシュボードやREST APIからの設定更新は`data/configs`に保存され、ランタイムでのホットリロードをサポートします。

- `etc/base.hocon`: ランタイム設定変更で上書き可能なベース設定。  
- `etc/emqx.conf`: 上書き不可の静的設定。  
- `data/configs/cluster.hocon`: ランタイム設定の上書き。  

EMQXはこれらのファイルから設定項目を読み込み、Erlangネイティブの設定ファイル形式に変換してランタイムに適用します。
