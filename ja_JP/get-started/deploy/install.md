# インストールとマイグレーション

本章では、EMQXの基本的なインストール手順、最低限のハードウェア仕様、および将来の設定やメンテナンス作業を容易にするためのファイルおよびディレクトリの場所について説明します。また、EMQX Enterpriseのライセンス設定方法とEMQX 4.4からEMQX 5.1へのマイグレーション方法についても解説します。

## 対応オペレーティングシステム

以下の表は、EMQXがサポートするオペレーティングシステムとそのバージョンを示しています。

| オペレーティングシステム           | 対応バージョン                            | x86_64/amd64 | arm64 |
| :--------------------------------- | :------------------------------------- | :----------: | :----: |
| [Ubuntu](./install-ubuntu.md)      | Ubuntu 18.04<br />Ubuntu 20.04<br />Ubuntu 22.04<br />Ubuntu 24.04 | Yes          | Yes   |
| [Debian](./install-debian.md)      | Debian 11<br />Debian 12<br />Debian 13 | Yes          | Yes   |
| [CentOS/RHEL](./install-rhel.md)   | CentOS 7<br />Rocky Linux 8<br />Rocky Linux 9 | Yes          | Yes   |
| [Amazon Linux](./install-rhel.md)  | Amazon Linux 2<br />Amazon Linux 2023   | Yes          | Yes   |
| [macOS](./install-macOS.md)        | macOS 14<br />macOS 15<br />macOS 26     | No           | Yes   |

<!-- ## ハードウェア仕様

クライアント接続数、メッセージレート、メッセージサイズ、および有効化されている機能により、EMQXの最低ハードウェア仕様は異なります。

以下は、100,000クライアント接続および毎秒100,000メッセージのスループットをサポートするシンプルなワークロードでEMQXを実行するためのハードウェア仕様です。

| 項目           | 最小構成             | 推奨構成               |
| -------------- | -------------------- | ---------------------- |
| **ノード**     | 1                    | 2                      |
| **CPU**        | 1コア                | 16コア                 |
| **メモリ**     | 512 MB               | 32 GB                  |
| **ディスク容量** | 1 GB                 | 50 GB                  |

::: tip

本番環境では、[Server Estimate](https://www.emqx.com/en/server-estimate) 計算ツールを使用して、最大接続数やメッセージスループットに応じた推奨ハードウェア仕様を算出できます。

::: -->

## インストール環境

EMQXを動作させるErlang VMは、[ファイル名](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#unicode-filenames)や[対話型Erlangシェルの端末IO](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#the-interactive-shell)などの機能でUnicodeサポートを有効にするために、システムのロケール設定に依存しています。

Linux OSを使用する場合は、EMQXを起動する前にシステム環境でUTF-8ロケールが有効になっていることを確認することを推奨します。以下のタブから各プラットフォームでのUTF-8ロケール有効化方法をご覧ください。

:::: tabs

::: tab Amazon Linux

[`cloud-init`](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/amazon-linux-ami-basics.html#amazon-linux-cloud-init)の設定でUTF-8ロケールを有効にします。

```bash
cat <<EOF | sudo tee /etc/cloud/cloud.cfg.d/99_locale.cfg
#cloud-config
locale: C.utf8
EOF
```

:::

::: tab CentOS

通常、systemdの`localectl`コマンドで有効化されます。

```bash
sudo localectl set-locale LANG=C.UTF-8
```

:::

::: tab Debian

UTF-8ロケールは以下の2通りの方法で有効化できます。

- 通常はsystemdの[`localectl`](https://www.freedesktop.org/software/systemd/man/localectl.html)で有効化されます。

  ```bash
  sudo localectl set-locale LANG=C.UTF-8
  ```

- それ以外の場合は[`update-locale`](https://manpages.debian.org/buster/locales/update-locale.8.en.html)で有効化できます。

  ```bash
  sudo update-locale LANG=C.UTF-8
  ```

:::

::: tab Ubuntu

[`update-locale`](https://manpages.ubuntu.com/manpages/jammy/man8/update-locale.8.html)でUTF-8ロケールを有効にします。

```bash
sudo update-locale LANG=C.UTF-8
```

:::

::::

## ポート使用状況

EMQXはデフォルトで以下のポートを使用します。これらのポートが他のアプリケーションで使用されていないことを確認し、必要に応じてファイアウォールを開放してEMQXが正常に動作するようにしてください。

| ポート  | プロトコル | 説明                                                  |
| ------- | ---------- | ----------------------------------------------------- |
| 1883    | TCP        | 暗号化されていないMQTT接続用のTCPリスナーポート。     |
| 8883    | TCP        | 暗号化されたMQTT接続用のSSL/TLSリスナーポート。       |
| 8083    | TCP        | MQTT over WebSocket通信のためのWebSocketリスナーポート。 |
| 8084    | TCP        | 暗号化されたWebSocket接続用のWSS（SSL上のWebSocket）リスナーポート。 |
| 18083   | HTTP       | EMQXダッシュボードおよびREST API用の管理コンソールとAPIインターフェースのポート。 |
| 4370    | TCP        | Erlang分散用ポート。実際のポートはノード名により`BasePort (4370) + Offset`となる場合があります。 |
| 5370    | TCP        | クラスターRPCポート（Docker環境では5369）。実際のポートはノード名により`BasePort (5370) + Offset`となる場合があります。 |

::: tip 注意

クラスターを形成していない場合でも、EMQXはポート4370および5370でリッスンします。この2つのポートは固定で変更できません。Offsetはノード名のName部分（`Name@Host`）の数値サフィックスによって決まります。数値サフィックスがない場合はデフォルトで0です。詳細は[ポートマッピング](../../guides/cluster/security.md#port-mapping)を参照してください。

:::

## ファイルとディレクトリ

インストール後、EMQXは実行ファイルや設定ファイル、データ、ログを格納するためのいくつかのディレクトリを作成します。以下の表は、インストール方法ごとに作成されるディレクトリとそのパスを示しています。

| ディレクトリ  | 説明               | tar.gzでインストール | RPM/DEBでインストール   |
| ------------ | ------------------ | -------------------- | ----------------------- |
| `etc`        | 静的設定ファイル    | `./etc`              | `/etc/emqx`             |
| `data`       | データベースおよび設定 | `./data`             | `/var/lib/emqx`         |
| `log`        | ログファイル        | `./log`              | `/var/log/emqx`         |
| `releases`   | 起動指示ファイル    | `./releases`         | `/usr/lib/emqx/releases`|
| `bin`        | 実行ファイル        | `./bin`              | `/usr/lib/emqx/bin`     |
| `lib`        | Erlangコード       | `./lib`              | `/usr/lib/emqx/lib`     |
| `erts-*`     | Erlangランタイム   | `./erts-*`           | `/usr/lib/emqx/erts-*`  |
| `plugins`    | プラグイン         | `./plugins`          | `/usr/lib/emqx/plugins` |

::: tip

1. 圧縮パッケージでインストールした場合、ディレクトリはソフトウェアをインストールしたディレクトリを基準とした相対パスです。
2. Dockerコンテナでインストールした場合、EMQXは`/opt/emqx`ディレクトリにインストールされます。
3. `data`、`log`、`plugins`ディレクトリは設定ファイルで変更可能です。パフォーマンス向上のため、`data`ディレクトリは高速なディスクにマウントすることを推奨します。同じクラスターに属するノードでは、`data`ディレクトリの設定を統一してください。クラスターの詳細は[クラスター](../../guides/cluster/create-cluster.md)を参照してください。

:::

以下の表は、いくつかのディレクトリのファイルやサブフォルダの説明です。

| ディレクトリ | 説明               | 権限   | ファイル                                                        |
| ------------ | ------------------ | ------ | -------------------------------------------------------------- |
| bin          | 実行ファイル       | 読み取り | `emqx`および`emqx.cmd`：EMQXの実行ファイル。詳細は[コマンドラインインターフェース](../../guides/cli.md)を参照してください。 |
| etc          | 設定ファイル       | 読み取り | `base.hocon`：ランタイム設定変更で上書き可能なベース設定。<br /><br />`emqx.conf`：上書き不可の静的設定。<br /><br />`emqx-example-en.conf`：EMQXのデモ設定ファイルで、全設定項目を含む。<br /><br />`acl.conf`：デフォルトのACLルール。<br /><br />`vm.args`：Erlang VMの起動パラメータ。<br /><br />`certs/`：EMQX SSLリスナー用のX.509鍵および証明書ファイル。外部システム統合時のSSL/TLS接続にも使用される場合あり。 |
| data         | 運用データ         | 書き込み | `authz`：REST APIやダッシュボードからアップロードされたファイル認可ルールを保存。詳細は[認可 - ファイル](../../guides/access-control/authz/file.md)を参照。<br /><br />`certs`：REST APIやダッシュボードからアップロードされた証明書ファイルを保存。<br /><br />`configs`：起動時に生成された設定ファイルやAPI/CLIからの設定上書きを保存。<br /><br />`mnesia`：アラーム記録、クライアントの認証・認可情報、ダッシュボードユーザー情報などEMQXの運用データを格納する組み込みデータベース。**このディレクトリを削除すると、これらの運用データはすべて失われます。**<br /><br />  —  ノードごとに名前を付けたサブディレクトリ（例：`emqx@127.0.0.1`）を含む場合があります。ノード名変更時は対応するサブディレクトリも削除または移動してください。<br /><br />  —  組み込みデータベースのクエリには`emqx ctl mnesia`コマンドを使用します。詳細は[管理コマンドCLI](https://docs.emqx.com/en/enterprise/v5.0/admin/cli.html)を参照。<br /><br />`patches`：EMQXがホットパッチとして読み込む`.beam`ファイルを保存。迅速な修正に利用可能。<br /><br />`trace`：オンライントレースログファイル。<br /><br />本番環境では、データ安全のため`trace`フォルダを除いた`data`ディレクトリの定期的なバックアップを推奨。 |
| log          | 運用ログ           | 読み取り | `emqx.log.*`：EMQXの運用ログ。詳細は[ログ](../../guides/observability/log.md)を参照。 |

:::tip

EMQXは設定情報を`data/configs`および`etc`ディレクトリに保存します。`etc`ディレクトリは読み取り専用の設定ファイルを格納し、ダッシュボードやREST APIからの設定更新は`data/configs`に保存され、ランタイムでのホット設定リロードをサポートします。

- `etc/base.hocon`：ランタイム設定変更で上書き可能なベース設定。
- `etc/emqx.conf`：上書き不可の静的設定。
- `data/configs/cluster.hocon`：ランタイム設定の上書き。

EMQXはこれらのファイルから設定項目を読み込み、Erlangネイティブの設定ファイル形式に変換してランタイムに適用します。

:::
