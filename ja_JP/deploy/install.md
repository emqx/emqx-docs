# インストールとマイグレーション

本章では、EMQXの基本的なインストール手順、最低限のハードウェア仕様、および将来の設定やメンテナンス作業を容易にするためのファイルとディレクトリの場所について説明します。また、EMQX Enterpriseのライセンス設定方法やEMQX 4.4からEMQX 5.1へのマイグレーション方法についても解説します。

## サポートされているオペレーティングシステム

以下の表は、EMQXがサポートするオペレーティングシステムとそのバージョンを示しています。

| オペレーティングシステム           | サポートされているバージョン                | x86_64/amd64 | arm64 |
| :--------------------------------- | :------------------------------------------ | :----------- | :---- |
| [Ubuntu](./install-ubuntu.md)      | Ubuntu 18.04<br />Ubuntu 20.04<br />Ubuntu 22.04<br />Ubuntu 24.04 | 対応         | 対応   |
| [Debian](./install-debian.md)      | Debian 10<br />Debian 11<br />Debian 12     | 対応         | 対応   |
| [CentOS/RHEL](./install-rhel.md)   | CentOS 7<br />Rocky Linux 8<br />Rocky Linux 9 | 対応         | 対応   |
| [Amazon Linux](./install-rhel.md)  | Amazon Linux 2<br />Amazon Linux 2023       | 対応         | 対応   |
| [macOS 13](./install-macOS.md)     | macOS 13                                    | 対応         | 非対応 |
| [macOS 14+](./install-macOS.md)    | macOS 14<br />macOS 15                       | 非対応       | 対応   |

<!-- ## ハードウェア仕様

クライアント接続数、メッセージレート、メッセージサイズ、および有効化された機能によって、EMQXの最低ハードウェア仕様は異なります。

以下は、単純なワークロードでEMQXを実行し、100,000クライアント接続および毎秒100,000メッセージのスループットをサポートするためのハードウェア仕様です。

| 項目           | 最低構成             | 推奨構成               |
| -------------- | -------------------- | ---------------------- |
| **ノード数**   | 1                    | 2                      |
| **CPU**        | 1コア                | 16コア                 |
| **メモリ**     | 512 MB               | 32 GB                  |
| **ディスク容量** | 1 GB                 | 50 GB                  |

::: tip

本番環境では、[Server Estimate](https://www.emqx.com/en/server-estimate) 計算ツールを利用して、最大接続数やメッセージスループットに応じた推奨ハードウェア仕様を算出できます。

::: -->

## インストール環境

EMQXを動作させるErlang VMは、[ファイル名](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#unicode-filenames)や対話型Erlangシェルの[端末IO](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#the-interactive-shell)などのUnicode対応機能を有効にするために、システムのロケール設定に依存しています。

Linux OSを使用する場合は、EMQX起動前にシステム環境でUTF-8ロケールが有効になっていることを確認することを推奨します。以下のタブをクリックすると、各プラットフォームでUTF-8ロケールを有効にする方法が表示されます。

:::: tabs

::: tab Amazon Linux

[`cloud-init`](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/amazon-linux-ami-basics.html#amazon-linux-cloud-init)設定でUTF-8ロケールを有効にします。

```bash
cat <<EOF | sudo tee /etc/cloud/cloud.cfg.d/99_locale.cfg
#cloud-config
locale: C.utf8
EOF
```

:::

::: tab CentOS

通常、systemd環境下で`localectl`コマンドにより有効化されます。

```bash
sudo localectl set-locale LANG=C.UTF-8
```

:::

::: tab Debian

UTF-8ロケールは以下の2通りの方法で有効化できます。

- systemd環境下では通常、[`localectl`](https://www.freedesktop.org/software/systemd/man/localectl.html)で有効化されます。

  ```bash
  sudo localectl set-locale LANG=C.UTF-8
  ```

- それ以外の場合は、[`update-locale`](https://manpages.debian.org/buster/locales/update-locale.8.en.html)で有効化可能です。

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

| ポート  | プロトコル | 説明                                                                                  |
| ------- | ---------- | ------------------------------------------------------------------------------------- |
| 1883    | TCP        | 主に暗号化されていないMQTT接続に使用される、TCP上のMQTTリスナーポート。               |
| 8883    | TCP        | 暗号化されたMQTT接続のためのSSL/TLS上のMQTTリスナーポート。                           |
| 8083    | TCP        | WebSocket経由のMQTT通信のためのMQTT over WebSocketリスナーポート。                   |
| 8084    | TCP        | 暗号化されたWebSocket接続のためのMQTT over WSS（SSL上のWebSocket）リスナーポート。    |
| 18083   | HTTP       | EMQXダッシュボードおよびREST APIの管理コンソールとAPIインターフェース用ポート。       |
| 4370    | TCP        | Erlang分散通信ポート。実際のポートはノード名により`BasePort (4370) + Offset`となる場合があります。 |
| 5370    | TCP        | クラスターRPCポート（Docker環境では5369）。実際のポートはノード名により`BasePort (5370) + Offset`となる場合があります。 |

::: tip 注意

クラスターを形成していない場合でも、EMQXはポート4370と5370でリッスンします。これら2つのポートは固定で変更できません。Offsetはノード名の`Name@Host`のName部分の数値サフィックスによって決まります。数値サフィックスがない場合はデフォルトで0です。詳細は[ポートマッピング](./cluster/security.md#port-mapping)を参照してください。

:::

## ファイルとディレクトリ

インストール後、EMQXは実行ファイルや設定ファイル、データ、ログを保存するためのいくつかのディレクトリを作成します。以下の表は、インストール方法ごとに作成されるディレクトリとそのパスを示しています。

| ディレクトリ  | 説明               | tar.gzでインストール時のパス | RPM/DEBでインストール時のパス |
| ------------ | ------------------ | ---------------------------- | ----------------------------- |
| `etc`        | 静的設定ファイル    | `./etc`                     | `/etc/emqx`                   |
| `data`       | データベースと設定  | `./data`                    | `/var/lib/emqx`               |
| `log`        | ログファイル        | `./log`                     | `/var/log/emqx`               |
| `releases`   | 起動指示ファイル    | `./releases`                | `/usr/lib/emqx/releases`      |
| `bin`        | 実行ファイル        | `./bin`                     | `/usr/lib/emqx/bin`           |
| `lib`        | Erlangコード       | `./lib`                     | `/usr/lib/emqx/lib`           |
| `erts-*`     | Erlangランタイム   | `./erts-*`                  | `/usr/lib/emqx/erts-*`        |
| `plugins`    | プラグイン          | `./plugins`                 | `/usr/lib/emqx/plugins`       |

::: tip

1. 圧縮パッケージでインストールした場合、ディレクトリはソフトウェアをインストールしたディレクトリからの相対パスです。  
2. Dockerコンテナでインストールした場合、EMQXは`/opt/emqx`ディレクトリにインストールされます。  
3. `data`、`log`、`plugins`ディレクトリは設定ファイルで変更可能です。パフォーマンス向上のため、`data`ディレクトリは高速ディスクにマウントすることを推奨します。同一クラスターに属するノードでは`data`ディレクトリの設定を統一してください。クラスターの詳細は[クラスター](./cluster/introduction.md)を参照してください。

:::

以下の表は、各ディレクトリのファイルやサブフォルダの概要を示しています。

| ディレクトリ | 説明               | パーミッション | ファイル                                                         |
| ------------ | ------------------ | -------------- | ---------------------------------------------------------------- |
| bin          | 実行ファイル        | 読み取り       | `emqx` と `emqx.cmd`: EMQXの実行ファイル。詳細は[コマンドラインインターフェース](../admin/cli.md)を参照。 |
| etc          | 設定ファイル        | 読み取り       | `base.hocon`: ランタイム設定変更で上書き可能な基本設定。<br /><br />`emqx.conf`: 上書き不可の静的設定。<br /><br />`emqx-example-en.conf`: EMQXの全設定項目を含むサンプル設定ファイル。<br /><br />`acl.conf`: デフォルトのACLルール。<br /><br />`vm.args`: Erlang仮想マシンの起動パラメータ。<br /><br />`certs/`: EMQXのSSLリスナー用X.509鍵と証明書ファイル。外部システム連携時のSSL/TLS接続にも使用可能。 |
| data         | 実行データ          | 書き込み       | `authz`: REST APIやダッシュボードからアップロードされたファイル認可ルールを保存。詳細は[認可 - ファイル](../access-control/authz/file.md)参照。<br /><br />`certs`: REST APIやダッシュボードからアップロードされた証明書ファイルを保存。<br /><br />`configs`: 起動時に生成される設定ファイルやAPI/CLIからの設定上書きを保存。<br /><br />`mnesia`: EMQXの実行データを格納する組み込みデータベース。アラーム記録、クライアントの認証・認可情報、ダッシュボードユーザー情報などを含む。**このディレクトリを削除すると、これらの実行データはすべて失われます。**<br /><br />  —  ノード名を冠したサブディレクトリ（例: `emqx@127.0.0.1`）を含む場合があります。ノード名変更時は対応するサブディレクトリも削除または移動してください。<br /><br />  —  組み込みデータベースのクエリには`emqx ctl mnesia`コマンドを使用します。詳細は[管理コマンドCLI](https://docs.emqx.com/en/enterprise/v5.0/admin/cli.html)を参照。<br /><br />`patches`: ホットパッチとしてEMQXが読み込む`.beam`ファイルを保存。迅速な修正に利用可能。<br /><br />`trace`: オンライントレースログファイル。<br /><br />本番環境では、データの安全性確保のため、定期的に`data`ディレクトリ（`trace`フォルダを除く）をバックアップすることを推奨します。 |
| log          | 実行ログ            | 読み取り       | `emqx.log.*`: EMQXの操作ログ。詳細は[ログ](../observability/log.md)を参照。 |

:::tip

EMQXは設定情報を`data/configs`と`etc`ディレクトリに保存します。`etc`ディレクトリは読み取り専用の設定ファイルを格納し、ダッシュボードやREST APIからの設定更新は`data/configs`に保存され、ランタイムでのホットリロードをサポートします。

- `etc/base.hocon`: ランタイム設定変更で上書き可能な基本設定。  
- `etc/emqx.conf`: 上書き不可の静的設定。  
- `data/configs/cluster.hocon`: ランタイム設定の上書き。

EMQXはこれらのファイルから設定項目を読み込み、Erlangネイティブの設定ファイル形式に変換してランタイムに適用します。

:::
