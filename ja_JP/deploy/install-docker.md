---
description: このページでは、公式Dockerイメージを使用してEMQXをインストールおよび起動する方法と、Docker Composeを使用してEMQXクラスターを構築する方法を紹介します。
---

# Dockerを使ったEMQXのインストール
<<<<<<< HEAD
このページでは、公式Dockerイメージを使ってEMQX Enterpriseをインストールおよび起動する方法と、Docker Composeを使ってEMQXクラスターを構築する方法を紹介します。

## Dockerを使って単一のEMQXノードを起動する

このセクションでは、Dockerイメージを使って最新バージョンのEMQXをインストールする方法を紹介します。EMQX公式Dockerイメージの詳細については、[Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)をご参照ください。

1. Dockerイメージを取得するには、以下のコマンドを実行します。
=======
このページでは、公式Dockerイメージを使用してEMQX Enterpriseをインストールおよび起動する方法と、Docker Composeを使用してEMQXクラスターを構築する方法を紹介します。

## Dockerを使って単一のEMQXノードを起動する

このセクションでは、Dockerイメージを使用して最新バージョンのEMQXをインストールする方法を紹介します。EMQX公式Dockerイメージの詳細については、[Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)をご覧ください。

1. Dockerイメージを取得するには、以下を実行します：
>>>>>>> origin/release-5.10

   ```bash
   docker pull emqx/emqx-enterprise:@EE_VERSION@
   ```

<<<<<<< HEAD
2. Dockerコンテナを起動するには、以下のコマンドを実行します。
=======
2. Dockerコンテナを起動するには、以下を実行します：
>>>>>>> origin/release-5.10

   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083  emqx/emqx-enterprise:@EE_VERSION@
   ```

### Dockerデプロイ時の注意点

1. EMQX Dockerコンテナ内で生成されたデータを永続化したい場合は、以下のディレクトリをマウントして保持する必要があります。これにより、コンテナが存在しなくなってもデータが保持されます。

   ```bash
   /opt/emqx/data
   /opt/emqx/log
   ```
   
   EMQXのディレクトリ構成の詳細については、[EMQX - ファイルとディレクトリ](./install.md#files-and-directories)を参照してください。
   
<<<<<<< HEAD
   コンテナを起動し、ディレクトリをマウントする例：
=======
   コンテナ起動時にディレクトリをマウントする例：
>>>>>>> origin/release-5.10
   
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
   
<<<<<<< HEAD
2. Docker環境では、`localhost`や`127.0.0.1`はコンテナ自身の内部ネットワークインターフェースを指し、ホストマシンのものではありません。ホストマシンで動作しているサービスにアクセスするには、ホストのIPアドレスを使用するか、[ホストネットワーク設定](https://docs.docker.com/network/host/)を利用してください。Docker for MacやDocker for Windowsを使用している場合は、`host.docker.internal`をホストアドレスとして使用できます。

3. EMQXはデータ保存に`data/mnesia/<node_name>`ディレクトリを使用します。ノード名は安定した識別子（例えばFQDN）を選ぶことが重要です。これにより、ノード名の変更によるデータ損失を防げます。

   単一ノードのデプロイでは、`EMQX_NODE_NAME`環境変数に`emqx@hostname`形式でノード名を設定してください。また、コンテナのホスト名もこれに合わせて設定する必要があります。上記の例を参照してください。

   **注意:** ノード名は`emqx@<hostname>`の形式で、`<hostname>`はコンテナのホスト名または安定したFQDNと一致させる必要があります。
=======
2. Docker環境では、`localhost`や`127.0.0.1`はコンテナ自身の内部ネットワークインターフェースを指し、ホストマシンのものではありません。ホストマシン上で稼働しているサービスにアクセスする場合は、ホストのIPアドレスを使用するか、[ホストネットワーク設定](https://docs.docker.com/network/host/)を利用してください。Docker for MacやDocker for Windowsを使用している場合は、`host.docker.internal`をホストアドレスとして使用できます。

3. EMQXはデータ保存のために`data/mnesia/<node_name>`ディレクトリを使用します。ノード名は安定した識別子（例えばFQDN）を選択することが重要です。これにより、ノード名の変更によるデータ損失を防げます。
>>>>>>> origin/release-5.10

   単一ノードのデプロイでノード名を設定するには、`EMQX_NODE_NAME`環境変数に`emqx@hostname`の形式で設定します。また、コンテナのホスト名もこれに合わせて設定してください。上記の例を参照してください。

<<<<<<< HEAD
Docker Composeは複数コンテナのDockerアプリケーションを定義・実行するツールです。このセクションでは、Docker Composeを使って静的なEMQXクラスターを作成する方法を紹介します。
=======
   **注意:** ノード名は`emqx@<hostname>`の形式で、`<hostname>`はコンテナのホスト名または安定したFQDNと一致させる必要があります。
>>>>>>> origin/release-5.10

   単一ノード構成でノード名を設定するには、`EMQX_NODE_NAME`環境変数を`emqx@hostname`形式で指定します。また、コンテナのホスト名もこれに合わせて設定してください。上記の例を参照してください。

Docker Composeは複数のコンテナからなるDockerアプリケーションを定義・実行するツールです。このセクションでは、Docker Composeを使って静的なEMQXクラスターを作成する方法を紹介します。

なお、このセクションのDocker Composeの例ファイルはローカルテスト用です。実運用環境でクラスターをデプロイする場合は、[クラスター](./cluster/introduction.md)を参照してください。

:::tip

<<<<<<< HEAD
Docker ComposeはDocker Desktopに標準で含まれています。もしDocker Composeが未インストールの場合は、[Docker Composeのインストール](https://docs.docker.com/compose/install/)を参照してインストールしてください。

:::

1. 任意のディレクトリに`docker-compose.yml`ファイルを作成し、以下の内容を記述します。
=======
Docker ComposeはDocker Desktopに既に含まれています。もしDocker Composeがまだインストールされていない場合は、[Docker Composeのインストール](https://docs.docker.com/compose/install/)を参照して詳細な手順を確認してください。

:::

1. 任意のディレクトリに`docker-compose.yml`ファイルを作成し、以下の内容を記述します：
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
3. クラスターの状態を確認するには、以下のコマンドを実行します。
=======
3. クラスターの状態を確認するには、以下を実行します：
>>>>>>> origin/release-5.10

   ```bash
   $ docker exec -it emqx1 sh -c "emqx ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                     stopped_nodes => []}
   ```

## 次のステップ

MQTTクライアントを使ってEMQXに接続し、メッセージのパブリッシュ／サブスクライブを行ってください。詳細は[パブリッシュとサブスクライブ](../messaging/publish-and-subscribe.md)を参照してください。

<<<<<<< HEAD
- EMQXのパラメータ設定やその他の機能については、[設定](../configuration/configuration.md)を参照してください。
=======
- EMQXのパラメーター設定やその他機能については、[設定](../configuration/configuration.md)をご覧ください。
>>>>>>> origin/release-5.10

- 複数ノードによるEMQXクラスターの構築方法については、[クラスター](./cluster/introduction.md)を参照してください。
