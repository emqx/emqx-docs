---
description: このページでは、公式Dockerイメージを使ったEMQXのインストールと起動方法、およびDocker Composeを使ったEMQXクラスターの構築方法を紹介します。
---

# Dockerを使ったEMQXのインストール
このページでは、公式Dockerイメージを使ってEMQX Enterpriseをインストールおよび起動する方法と、Docker Composeを使ってEMQXクラスターを構築する方法を紹介します。

## はじめに

Docker上でEMQXを起動する前に、以下のデプロイメントに関する注意点を確認してください。

### 安定したノード名を選択する

EMQXはノードデータを `data/mnesia/<node_name>` ディレクトリに保存します。コンテナ起動後にノード名が変更されるとデータが失われる可能性があるため、コンテナ起動前に安定したノード名を選択してください。

単一ノードのデプロイメントでは、`EMQX_NODE_NAME` 環境変数を `emqx@<host>` の形式で設定します。コンテナのホスト名も同じ `<host>` に設定してください。

**注意:** `<host>` 部分はIPアドレスまたは完全修飾ドメイン名（FQDN）である必要があります。例：`node1.emqx.com`。EMQXはErlangノードをロングネームモードで起動するため、ドットを含まない短いホスト名（例：`node1`）は使用できません。

### 永続ストレージの準備

コンテナ削除後もEMQXのデータを保持するには、以下のコンテナ内ディレクトリをホストにマウントしてください。

- `/opt/emqx/data`: EMQXのデータを保存します。
- `/opt/emqx/log`: ファイルログおよびクラッシュダンプを保存します。

EMQXコンテナはデフォルトでコンソールログを使用しますが、Erlang VMはノードが異常終了した際にクラッシュダンプを `/opt/emqx/log` に書き込みます。マウントしていない場合、コンテナ削除時にダンプは失われます。ホストのログディレクトリはコンテナ内の `emqx` ユーザー（UID 1000）が書き込み可能である必要があります。詳細は[Dockerでのクラッシュダンプ](../configuration/logs.md#crash-dumps-in-docker)を参照してください。

EMQXのディレクトリ構成の詳細は[EMQXのファイルとディレクトリ](./install.md#files-and-directories)を参照してください。

### ホスト上のサービスへのアクセス

EMQXがホスト上で稼働するサービスにアクセスする場合、サービスアドレスに `localhost` や `127.0.0.1` を使用しないでください。これらはコンテナ自身のネットワークインターフェースを指します。ホストのIPアドレスか[ホストネットワーク](https://docs.docker.com/network/host/)を使用してください。Docker Desktop for MacやWindowsでは `host.docker.internal` も使用可能です。

## Dockerを使った単一EMQXノードの起動

以下の手順で単一のEMQXノードを起動します。公式EMQX Dockerイメージの詳細は[Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)を参照してください。

1. Dockerイメージをプルします。

   ```bash
   docker pull emqx/emqx-enterprise:@EE_VERSION@
   ```

2. ホスト上にディレクトリを作成し、ログディレクトリをコンテナ内の `emqx` ユーザーが書き込み可能にします。

   ```bash
   mkdir -p $PWD/data $PWD/log
   sudo chown $UID:$GID $PWD/log
   ```

3. 安定したノード名とマウントしたディレクトリを指定してコンテナを起動します。

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

EMQX 6.3.0以降、公式イメージのエントリポイントは `EMQX_NODE__DEFAULT_LISTENER_ADDRESS` が未設定または空の場合、自動的に `all` に設定します。

これにより、ポートのみを指定したMQTTリスナー、ゲートウェイリスナー、ダッシュボードHTTPリスナーはすべてのネットワークインターフェースにバインドされ、どちらの[セキュリティプロファイル](../access-control/security-profile.md)でも公開されたコンテナポート経由でアクセス可能になります。

リスナーのバインドに明示的なIPアドレスが指定されている場合は変更されません。この設定はバインドアドレスのみを制御し、認証や認可の要件を緩和するものではありません。

**デフォルトの上書き**

このデフォルトを上書きするには、`docker run -e EMQX_NODE__DEFAULT_LISTENER_ADDRESS=<value>` で別のサポートされる値を渡すか、Docker Composeのサービスの `environment` セクションで設定してください。

環境変数は設定ファイルより優先されるため、マウントした `emqx.conf` のみで `node.default_listener_address` を設定してもエントリポイントのデフォルトは上書きされません。

サポートされる値の詳細は[デフォルトリスナーアドレス](../access-control/security-profile.md#default-listener-address)を参照してください。

::: warning 重要なお知らせ
Dockerのブリッジネットワーク環境でこの変数を `loopback` に設定すると、対象のリスナーはコンテナのネットワーク名前空間内のループバックにバインドされます。そのため、`-p` オプションでポートを公開していても外部からアクセスできなくなります。

公開ポートに使用するホストアドレスを制御するには、[Dockerのポート公開とマッピング](https://docs.docker.com/engine/network/port-publishing/)を参照してください。
:::

### 機能ゲートを使ったEMQXの起動

EMQX 6.3.0以降、`EMQX_FEATURES` 環境変数で起動時に有効にするオプション機能を制御できます。例えば、コアアプリケーションのみで起動するには以下のように実行します。

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=ESSENTIAL" \
  -p 1883:1883 -p 8083:8083 \
  -p 8084:8084 -p 8883:8883 \
  emqx/emqx-enterprise:@EE_VERSION@
```

カスタム機能セットで起動するには、以下のように実行します。

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=dashboard,metrics,plugins" \
  -p 1883:1883 -p 18083:18083 \
  emqx/emqx-enterprise:@EE_VERSION@
```

機能一覧と依存関係の詳細は[機能ゲート](./feature-gates.md)を参照してください。

## Docker Composeを使ったEMQXクラスターの構築

Docker Composeは複数コンテナのDockerアプリケーションを定義・実行するツールです。このセクションではDocker Composeを使って静的なEMQXクラスターを作成する方法を紹介します。

このセクションのDocker Composeの例はローカルテスト用であり、ボリュームマウントはコメントアウトされています。データやクラッシュダンプを保持するには、[はじめに](#はじめに)の手順でホストディレクトリを準備し、`volumes` のコメントを外してください。本番環境でのクラスター構築は[クラスター](./cluster/introduction.md)を参照してください。

:::tip

Docker ComposeはDocker Desktopに含まれています。まだインストールが必要な場合は、[Docker Composeのインストール](https://docs.docker.com/compose/install/)を参照してください。

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

- EMQXのパラメーター設定やその他の機能については[設定](../configuration/configuration.md)を参照してください。

- 複数ノードによるEMQXクラスターの構築方法は[クラスター](./cluster/introduction.md)を参照してください。
