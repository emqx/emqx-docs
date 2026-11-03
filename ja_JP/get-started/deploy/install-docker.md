---
description: このページでは、公式Dockerイメージを使用してEMQXをインストールおよび実行する方法と、Docker Composeを使用してEMQXクラスターを構築する方法を紹介します。
---

# Dockerを使用したEMQXのインストール
このページでは、公式Dockerイメージを使用してEMQX Enterpriseをインストールおよび実行する方法と、Docker Composeを使用してEMQXクラスターを構築する方法を紹介します。

## はじめに

DockerでEMQXを起動する前に、以下のデプロイメントに関する注意点を確認してください。

### 安定したノード名を選択する

EMQXはノードデータを `data/mnesia/<node_name>` ディレクトリに保存します。コンテナ起動後にノード名が変更されるとデータが失われる可能性があるため、コンテナを起動する前に安定したノード名を選択してください。

単一ノードのデプロイメントの場合、`EMQX_NODE_NAME` 環境変数を `emqx@<host>` の形式で設定します。コンテナのホスト名も同じ `<host>` の値に設定してください。

**注意:** `<host>` 部分はIPアドレスまたは完全修飾ドメイン名（FQDN）、例えば `node1.emqx.com` である必要があります。EMQXはErlangノードをロングネームモードで実行するため、ドットなしの短いホスト名（例: `node1`）は使用できません。

### 永続ストレージの準備

コンテナ削除後もEMQXのデータを保持するには、以下のコンテナ内ディレクトリをホストにマウントしてください。

- `/opt/emqx/data`: EMQXのデータを保存します。
- `/opt/emqx/log`: ファイルログおよびクラッシュダンプを保存します。

EMQXコンテナはデフォルトでコンソールログを使用しますが、Erlang VMはノードが異常終了した場合にクラッシュダンプを `/opt/emqx/log` に書き込みます。マウントしていない場合、コンテナ削除時にダンプが失われます。ホストのログディレクトリはコンテナ内の `emqx` ユーザー（UID 1000）が書き込み可能である必要があります。詳細は[Dockerでのクラッシュダンプ](../../guides/configuration/logs.md#crash-dumps-in-docker)を参照してください。

EMQXのディレクトリ構成の詳細は[EMQXのファイルとディレクトリ](./install.md#files-and-directories)をご覧ください。

### ホスト上のサービスへのアクセス

EMQXがホスト上で動作するサービスにアクセスする場合、サービスアドレスに `localhost` や `127.0.0.1` を使用しないでください。これらはコンテナ自身のネットワークインターフェースを指します。ホストのIPアドレスまたは[ホストネットワーキング](https://docs.docker.com/network/host/)を使用してください。MacやWindowsのDocker Desktopでは `host.docker.internal` も使用可能です。

### ランタイムイメージについて理解する

EMQX Enterprise 6.3.1以降の公式DockerリリースイメージはDocker Hardened Debian 13（Trixie）を使用しています。ランタイムイメージには `apt` や `dpkg` などのパッケージマネージャーが含まれていないため、実行中のEMQXコンテナ内でこれらのツールを使ってパッケージをインストールすることはできません。

追加のツールやランタイム依存関係が必要な場合は、カスタムイメージに含めて検証した上でデプロイしてください。公式コンテナの実行中にパッケージをインストールすることに依存しないでください。

リリースイメージにはビルドの由来情報やソフトウェア部品表（SBOM）も含まれています。Docker Scoutはこれらの情報を利用して、ハードニングされたベースイメージの脆弱性評価（VEXステートメント）を適用できます。

#### 信頼されたCAバンドル <Badge text="6.3.3" />

各公式Dockerリリースイメージのビルドプロセスでは、curl.seからMozilla CAバンドルをダウンロードし、公開されているSHA-256チェックサムを検証した上で `/etc/ssl/certs/ca-certificates.crt` にインストールします。

Dockerデプロイメントでは、デフォルトの `node.trusted_certs` 値である `system_default` はこのCAバンドルを使用します。EMQXは明示的にCA証明書を設定していないサポート対象のアウトバウンドTLS接続のサーバー証明書検証にこのバンドルを利用します。

追加または置換のCA証明書を提供する方法はスコープに応じて選択してください。

- 特定リソースのプライベートCAを信頼する場合は、プライベートCAバンドルをコンテナ内の別パスにマウントし、そのリソースのTLS設定で `cacertfile` を明示的にそのパスに設定します。これは共有システムトラストストアを変更しないため推奨される方法です。
- もしイメージに含まれるCAバンドルが古い、または信頼しないルートCAを含む場合は、置換用トラストストアを `/etc/ssl/certs/ca-certificates.crt` にマウントしてください。
- デフォルトでプライベートルートCAを信頼する場合は、プライベートルート証明書をデフォルトバンドルのコピーに追記して結合バンドルを作成し、それを `/etc/ssl/certs/ca-certificates.crt` にマウントしてください。

::: warning 共有システムトラストストアについて
`/etc/ssl/certs/ca-certificates.crt` のCAバンドルは、`system_default` を使用するすべてのサポート対象アウトバウンドTLS接続で共有されます。プライベートCA証明書のみを含むバンドルに置き換えると、Google Cloud、AWS、Azureなどのサービスへの接続に必要な公開ルートが削除され、接続障害を引き起こす可能性があります。
:::

詳細は[アウトバウンドTLSの信頼されたCA証明書](../../guides/network/tls-certificate.md#trusted-ca-certificates-for-outbound-tls)をご覧ください。

## Dockerを使って単一のEMQXノードを実行する

単一ノードのEMQXを実行する手順は以下の通りです。公式EMQX Dockerイメージの詳細は[Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)を参照してください。

1. Dockerイメージをプルします。

   ```bash
   docker pull emqx/emqx-enterprise:@EE_VERSION@
   ```

2. ホスト上にディレクトリを作成し、ログディレクトリをコンテナ内の `emqx` ユーザーが書き込み可能にします。

   ```bash
   mkdir -p $PWD/data $PWD/log
   sudo chown $UID:$GID $PWD/log
   ```

3. 安定したノード名とマウント済みディレクトリを指定してコンテナを起動します。

   ```bash
   docker run -d --name emqx-enterprise \
     --hostname node1.emqx.com \
     -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
     -p 1883:1883 -p 8083:8083 \
     -p 8084:8084 -p 8883:8883 \
     -p 18083:18083 \
     -v $PWD/data:/opt/emqx/data \
     -v $PWD/log:/opt/emqx/log \
     emqx/emqx-enterprise:@EE_VERSION@
   ```

### Dockerでのデフォルトリスナーアドレスの設定

**Dockerイメージのデフォルト**

EMQX 6.3.0以降、公式イメージのエントリポイントは `EMQX_NODE__DEFAULT_LISTENER_ADDRESS` が未設定または空の場合に `all` を設定します。

これにより、ポートのみ指定されたMQTTリスナー、ゲートウェイリスナー、およびダッシュボードHTTPリスナーがすべてのネットワークインターフェースにバインドされ、公開されたコンテナポート経由で両方の[セキュリティプロファイル](../../guides/access-control/security-profile.md)からアクセス可能になります。

リスナーのバインドに明示的なIPアドレスが指定されている場合は変更されません。この設定はバインドアドレスのみを制御し、認証や認可の要件を緩和するものではありません。

**デフォルトの上書き**

このデフォルトを上書きするには、`docker run -e EMQX_NODE__DEFAULT_LISTENER_ADDRESS=<value>` でサポートされる別の値を渡すか、Docker Composeのサービスの `environment` セクションで変数を設定してください。

環境変数は設定ファイルより優先されるため、マウントした `emqx.conf` 内で `node.default_listener_address` を設定してもエントリポイントのデフォルトは上書きされません。

サポートされる値は[デフォルトリスナーアドレス](../../guides/access-control/security-profile.md#default-listener-address)を参照してください。

::: warning 重要なお知らせ
Dockerのブリッジネットワーク環境では、この変数を `loopback` に設定すると、対象のリスナーはコンテナのネットワーク名前空間内のループバックにバインドされます。そのため、`-p` オプションで公開してもポート経由でアクセスできなくなります。

公開ポートで使用するホストアドレスの制御については、[Dockerのポート公開とマッピング](https://docs.docker.com/engine/network/port-publishing/)を参照してください。
:::

### Feature Gatesを使ってEMQXを起動する

EMQX 6.3.0以降、`EMQX_FEATURES` 環境変数で起動時に利用可能なオプション機能を制御できます。例えば、コアアプリケーションのみで起動するには以下のように実行します。

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=ESSENTIAL" \
  -p 1883:1883 -p 8083:8083 \
  -p 8084:8084 -p 8883:8883 \
  emqx/emqx-enterprise:@EE_VERSION@
```

カスタム機能セットで起動するには以下のように実行します。

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=dashboard,metrics,plugins" \
  -p 1883:1883 -p 18083:18083 \
  emqx/emqx-enterprise:@EE_VERSION@
```

機能一覧と依存関係の詳細は[Feature Gates](./feature-gates.md)を参照してください。

## Docker Composeを使ってEMQXクラスターを構築する

Docker Composeは複数コンテナのDockerアプリケーションを定義・実行するツールです。このセクションではDocker Composeを使って静的なEMQXクラスターを作成する方法を紹介します。

このセクションのDocker Compose例はローカルテスト用であり、ボリュームマウントはコメントアウトされています。データやクラッシュダンプを保持するには、[はじめに](#はじめに)で説明したホストディレクトリを準備し、`volumes` エントリのコメントを解除してください。商用クラスターのデプロイについては[クラスタリング](../../develop/cluster/introduction.md)を参照してください。

:::tip

Docker ComposeはDocker Desktopに含まれています。まだインストールが必要な場合は、[Docker Composeのインストール](https://docs.docker.com/compose/install/)をご覧ください。

:::

1. 任意のディレクトリに `docker-compose.yml` ファイルを作成し、以下の内容を記述します。

   ```yml
   version: '3'

   services:
     emqx1:
       image: emqx/emqx-enterprise:@EE_VERSION@
       container_name: emqx1
       environment:
       - "EMQX_NODE_NAME=emqx@node1.emqx.com"
       # - "EMQX_FEATURES=dashboard,metrics,plugins"
       - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
       healthcheck:
         test: ["CMD", "/opt/emqx/bin/emqx", "ctl", "status"]
         interval: 5s
         timeout: 25s
         retries: 5
       networks:
         emqx-bridge:
           aliases:
           - node1.emqx.com
       ports:
         - 1883:1883
         - 8083:8083
         - 8084:8084
         - 8883:8883
         - 18083:18083
       # volumes:
       #   - $PWD/emqx1_data:/opt/emqx/data
       #   - $PWD/emqx1_log:/opt/emqx/log

     emqx2:
       image: emqx/emqx-enterprise:@EE_VERSION@
       container_name: emqx2
       environment:
       - "EMQX_NODE_NAME=emqx@node2.emqx.com"
       # - "EMQX_FEATURES=dashboard,metrics,plugins"
       - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
       healthcheck:
         test: ["CMD", "/opt/emqx/bin/emqx", "ctl", "status"]
         interval: 5s
         timeout: 25s
         retries: 5
       networks:
         emqx-bridge:
           aliases:
           - node2.emqx.com
       # volumes:
       #   - $PWD/emqx2_data:/opt/emqx/data
       #   - $PWD/emqx2_log:/opt/emqx/log

   networks:
     emqx-bridge:
       driver: bridge
   ```

   Docker Composeクラスターで `EMQX_FEATURES` を設定する場合は、すべてのEMQXサービスで同じ値を使用してください。

2. コマンドラインツールで `docker-compose.yml` があるディレクトリに移動し、以下のコマンドを実行してEMQXクラスターを起動します。

   ```bash
   docker-compose up -d
   ```

3. クラスターの状態を確認するには、以下を実行します。

   ```bash
   $ docker exec -it emqx1 sh -c "emqx ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                     stopped_nodes => []}
   ```

## 次のステップ

MQTTクライアントを使ってEMQXに接続し、メッセージのパブリッシュ／サブスクライブを行ってください。詳細は[パブリッシュとサブスクライブ](../messaging/publish-and-subscribe.md)を参照してください。

- EMQXのパラメータ設定やその他機能については[設定](../../guides/configuration/configuration.md)をご覧ください。

- 複数ノードによるEMQXクラスターの構築方法は[クラスタリング](../../develop/cluster/introduction.md)を参照してください。
