---
description: このページでは、公式Dockerイメージを使ってEMQXをインストールおよび起動する方法と、Docker Composeを使ってEMQXクラスターを構築する方法を紹介します。
---

# Dockerを使ったEMQXのインストール
このページでは、公式Dockerイメージを使ってEMQX Enterpriseをインストールおよび起動する方法と、Docker Composeを使ってEMQXクラスターを構築する方法を紹介します。

## はじめに

DockerでEMQXを起動する前に、以下のデプロイメントに関する注意点を確認してください。

### 安定したノード名を選択する

EMQXはノードのデータを `data/mnesia/<node_name>` ディレクトリに保存します。後からノード名が変更されることでデータが失われるのを防ぐため、コンテナ起動前に安定したノード名を選択してください。

単一ノードのデプロイメントの場合、`EMQX_NODE_NAME` 環境変数を `emqx@<host>` の形式で設定します。コンテナのホスト名も同じ `<host>` に設定してください。

**注意:** `<host>` 部分はIPアドレスか完全修飾ドメイン名（FQDN）、例えば `node1.emqx.com` である必要があります。EMQXはErlangノードをロングネームモードで実行するため、ドットのない短いホスト名（例: `node1`）は使用できません。

### 永続ストレージの準備

コンテナ削除後もEMQXのデータを保持するために、以下のコンテナ内ディレクトリをホストにマウントしてください。

- `/opt/emqx/data`: EMQXのデータを保存します。
- `/opt/emqx/log`: ファイルログとクラッシュダンプを保存します。

EMQXコンテナはデフォルトでコンソールログを使用しますが、Erlang VMはノードが異常終了した際にクラッシュダンプを `/opt/emqx/log` に書き込みます。マウントしない場合、コンテナ削除時にダンプが失われます。ホストのログディレクトリはコンテナ内の `emqx` ユーザー（UID 1000）が書き込み可能である必要があります。詳細は[Dockerにおけるクラッシュダンプ](../configuration/logs.md#crash-dumps-in-docker)を参照してください。

EMQXのディレクトリ構成の詳細は[EMQXのファイルとディレクトリ](./install.md#files-and-directories)をご覧ください。

### ホスト上のサービスへのアクセス

EMQXがホスト上で動作するサービスにアクセスする必要がある場合、サービスアドレスに `localhost` や `127.0.0.1` を使用しないでください。これらはコンテナ自身のネットワークインターフェースを指します。ホストのIPアドレスか、[ホストネットワーキング](https://docs.docker.com/network/host/)を使用してください。Docker Desktop for MacやWindowsでは `host.docker.internal` も利用可能です。

## Dockerを使って単一のEMQXノードを起動する

以下の手順で単一のEMQXノードを起動します。公式EMQX Dockerイメージの詳細は[Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)を参照してください。

1. Dockerイメージをプルします。

   ```bash
   docker pull emqx/emqx-enterprise:@EE_VERSION@
   ```

2. ホスト上にディレクトリを作成し、ログディレクトリをコンテナ内の `emqx` ユーザーが書き込み可能にします。

   ```bash
   mkdir -p $PWD/data $PWD/log
   sudo chown 1000:1000 $PWD/log
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

## Docker Composeを使ってEMQXクラスターを構築する

Docker ComposeはマルチコンテナDockerアプリケーションを定義・実行するツールです。このセクションでは、Docker Composeを使って静的なEMQXクラスターを作成する方法を紹介します。

このセクションのDocker Composeの例はローカルテスト用であり、ボリュームマウントはコメントアウトされています。データやクラッシュダンプを保持するには、[はじめに](#はじめに)で説明したホストディレクトリを準備し、`volumes` の行をコメント解除してください。本番環境でのクラスター構築については[クラスタリング](./cluster/introduction.md)を参照してください。

:::tip

Docker ComposeはDocker Desktopに含まれています。もしDocker Composeが未インストールの場合は、[Docker Composeのインストール](https://docs.docker.com/compose/install/)を参照してください。

:::

1. 任意のディレクトリに以下の内容で `docker-compose.yml` ファイルを作成します。

   ```yml
   version: '3'
   
   services:
     emqx1:
       image: emqx/emqx-enterprise:@EE_VERSION@
       container_name: emqx1
       environment:
       - "EMQX_NODE_NAME=emqx@node1.emqx.com"
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

- EMQXのパラメータ設定やその他の機能については[設定](../configuration/configuration.md)をご覧ください。

- 複数ノードによるEMQXクラスターの構築方法については[クラスタリング](./cluster/introduction.md)を参照してください。
