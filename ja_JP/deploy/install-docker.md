---
description: このページでは、公式Dockerイメージを使用してEMQXをインストールおよび実行する方法と、Docker Composeを使用してEMQXクラスターを構築する方法を紹介します。
---

# Dockerを使用したEMQXのインストール
このページでは、公式Dockerイメージを使用してEMQX Enterpriseをインストールおよび実行する方法と、Docker Composeを使用してEMQXクラスターを構築する方法を紹介します。

## Dockerを使用して単一のEMQXノードを実行する

このセクションでは、Dockerイメージを使用して最新バージョンのEMQXをインストールする方法を紹介します。EMQX公式Dockerイメージの詳細については、[Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)をご参照ください。

1. Dockerイメージを取得するには、以下を実行します：

   ```bash
   docker pull emqx/emqx-enterprise:@EE_VERSION@
   ```

2. Dockerコンテナを起動するには、以下を実行します：

   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083  emqx/emqx-enterprise:@EE_VERSION@
   ```

### Dockerデプロイ時の注意点

1. EMQX Dockerコンテナ内で生成されたデータを永続化したい場合は、以下のディレクトリを保持する必要があります。これにより、コンテナが存在しなくなってもデータは保持されます。

   ```bash
   /opt/emqx/data
   /opt/emqx/log
   ```
   
   EMQXのディレクトリ構造の詳細については、[EMQX - ファイルとディレクトリ](./install.md#files-and-directories)を参照してください。
   
   コンテナを起動し、ディレクトリをマウントする例：
   
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
   
2. Docker環境では、`localhost`や`127.0.0.1`はコンテナ自身の内部ネットワークインターフェースを指し、ホストマシンのものではありません。ホストマシン上で動作するサービスにアクセスするには、ホストのIPアドレスを使用するか、[ホストネットワーク設定](https://docs.docker.com/network/host/)を利用してください。Docker for MacやDocker for Windowsを使用している場合は、`host.docker.internal`をホストアドレスとして使用できます。

3. EMQXはデータ保存に`data/mnesia/<node_name>`ディレクトリを使用します。ノード名にはFQDN（完全修飾ドメイン名）などの安定した識別子を選択することが重要です。これにより、ノード名の変更によるデータ損失を防げます。

   単一ノードのデプロイでノード名を設定するには、`EMQX_NODE_NAME`環境変数に`emqx@<host>`の形式で指定します。また、コンテナのホスト名もこれに合わせて設定してください。上記の例を参照してください。

   **注意:** `<host>`部分はIPアドレスまたは`node1.emqx.com`のような完全修飾ドメイン名（FQDN）でなければなりません。EMQXはErlangノードをロングネームモードで実行するため、`node1`のようなドットを含まない短いホスト名は使用できません。

## Docker Composeを使用してEMQXクラスターを構築する

Docker Composeは複数コンテナのDockerアプリケーションを定義・実行するツールです。このセクションでは、Docker Composeを使用して静的なEMQXクラスターを作成する方法を紹介します。

なお、このセクションのDocker Composeの例ファイルはローカルテスト用であり、本番環境でクラスターをデプロイする場合は[クラスター](./cluster/introduction.md)を参照してください。

:::tip

Docker ComposeはDocker Desktopに既に含まれています。もしDocker Composeが未インストールの場合は、[Docker Composeのインストール](https://docs.docker.com/compose/install/)を参照して詳細な手順をご確認ください。

:::

1. 任意のディレクトリに以下の内容で`docker-compose.yml`ファイルを作成します：

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
   
   networks:
     emqx-bridge:
       driver: bridge
   ```

2. コマンドラインツールで`docker-compose.yml`があるディレクトリに移動し、以下のコマンドを実行してEMQXクラスターを起動します：

   ```bash
   docker-compose up -d
   ```

3. クラスターの状態を確認するには、以下を実行します：

   ```bash
   $ docker exec -it emqx1 sh -c "emqx ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                     stopped_nodes => []}
   ```

## 次のステップ

MQTTクライアントを使用してEMQXに接続し、メッセージのパブリッシュ／サブスクライブを行います。詳細は[パブリッシュとサブスクライブ](../messaging/publish-and-subscribe.md)をご覧ください。

- EMQXのパラメーター設定やその他機能については、[設定](../configuration/configuration.md)を参照してください。

- 複数ノードによるEMQXクラスターの構築方法については、[クラスター](./cluster/introduction.md)をご覧ください。
