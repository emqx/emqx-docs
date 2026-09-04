---
description: このページでは、公式Dockerイメージを使ってEMQXをインストールおよび起動する方法と、Docker Composeを使ってEMQXクラスターを構築する方法を紹介します。
---

# Dockerを使ったEMQXのインストール
このページでは、公式Dockerイメージを使ってEMQX Enterpriseをインストールおよび起動する方法と、Docker Composeを使ってEMQXクラスターを構築する方法を紹介します。

## Dockerを使って単一のEMQXノードを起動する

このセクションでは、Dockerイメージを使って最新バージョンのEMQXをインストールする方法を紹介します。EMQX公式Dockerイメージの詳細については、[Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)をご覧ください。

1. Dockerイメージを取得するには、以下を実行します。

   ```bash
   docker pull emqx/emqx-enterprise:@EE_VERSION@
   ```

2. Dockerコンテナを起動するには、以下を実行します。

   ```bash
   docker run -d --name emqx-enterprise -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083  emqx/emqx-enterprise:@EE_VERSION@
   ```

### Dockerにおけるリスナーアドレス

EMQX 6.3.0以降、公式イメージのエントリポイントは、環境変数が未設定または空の場合に `EMQX_NODE__DEFAULT_LISTENER_ADDRESS=all` を設定します。これにより、ポートのみを指定したバインドのMQTTリスナー、ゲートウェイリスナー、およびダッシュボードHTTPリスナーはすべてのネットワークインターフェースを選択し、公開されたコンテナポートを通じてどちらのセキュリティプロファイルでもアクセス可能になります。リスナーのバインドに明示的なIPアドレスがある場合は変更されません。この設定はバインドアドレスのみを制御し、認証や認可の要件を緩和するものではありません。

このデフォルトを上書きするには、`docker run -e EMQX_NODE__DEFAULT_LISTENER_ADDRESS=<値>` で別のサポートされている値を渡すか、Docker Composeのサービスの `environment` セクションで変数を設定してください。環境変数は設定ファイルより優先されるため、マウントした `emqx.conf` のみで `node.default_listener_address` を設定してもエントリポイントのデフォルトは上書きされません。サポートされている値の詳細は、[Default Listener Address](../access-control/security-profile.md#default-listener-address)をご参照ください。

Dockerのブリッジネットワークを使用する場合、この変数を `loopback` に設定すると、対象のリスナーはコンテナのネットワーク名前空間内のループバックにバインドされます。そのため、`-p` を使用しても公開ポート経由でアクセスできなくなります。公開ポートに使用するホストアドレスを制御するには、[Dockerのポート公開とマッピング](https://docs.docker.com/engine/network/port-publishing/)をご覧ください。

### Feature Gatesを使ったEMQXの起動

EMQX 6.3.0以降、`EMQX_FEATURES` 環境変数を使って起動時に利用可能なオプション機能を制御できます。例えば、コアアプリケーションのみでEMQXを起動するには、以下を実行します。

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=ESSENTIAL" \
  -p 1883:1883 -p 8083:8083 \
  -p 8084:8084 -p 8883:8883 \
  emqx/emqx-enterprise:@EE_VERSION@
```

カスタム機能セットでEMQXを起動するには、以下を実行します。

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=dashboard,auth,metrics" \
  -p 1883:1883 -p 18083:18083 \
  emqx/emqx-enterprise:@EE_VERSION@
```

機能一覧と依存関係の詳細は、[Feature Gates](./feature-gates.md)をご覧ください。

### Dockerデプロイ時の注意点

1. EMQX Dockerコンテナ内で生成されたデータを永続化したい場合は、以下のディレクトリを保持する必要があります。これにより、コンテナが存在しなくなってもデータが保持されます。

   ```bash
   /opt/emqx/data
   /opt/emqx/log
   ```
   
   EMQXのディレクトリ構成の詳細は、[EMQX - ファイルとディレクトリ](./install.md#files-and-directories)をご参照ください。
   
   コンテナ起動時にディレクトリをマウントする例：

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
   
2. Docker環境では、`localhost` や `127.0.0.1` はコンテナ自身の内部ネットワークインターフェースを指し、ホストマシンのものではありません。ホストマシン上で動作するサービスにアクセスするには、ホストのIPアドレスを使うか、[ホストネットワーク設定](https://docs.docker.com/network/host/)を利用してください。Docker for MacやDocker for Windowsを使用している場合は、ホストアドレスとして `host.docker.internal` を利用できます。

3. EMQXはデータ保存に `data/mnesia/<node_name>` ディレクトリを使用します。ノード名にはFQDN（完全修飾ドメイン名）など安定した識別子を選ぶことが重要です。これにより、ノード名変更によるデータ損失を防げます。

   単一ノードのデプロイでノード名を設定するには、`EMQX_NODE_NAME` 環境変数に `emqx@<host>` 形式で指定してください。コンテナのホスト名も合わせて設定することを推奨します（上記例参照）。

   **注意:** `<host>` 部分はIPアドレスかFQDN（例：`node1.emqx.com`）でなければなりません。EMQXはErlangノードをロングネームモードで実行するため、ドットなしの短いホスト名（例：`node1`）は使用できません。

## Docker Composeを使ってEMQXクラスターを構築する

Docker Composeは複数コンテナのDockerアプリケーションを定義・実行するツールです。このセクションでは、Docker Composeを使って静的なEMQXクラスターを作成する方法を紹介します。

なお、このセクションのDocker Compose例ファイルはローカルテスト用です。実運用環境でクラスターをデプロイする場合は、[クラスター](./cluster/introduction.md)をご参照ください。

:::tip

Docker ComposeはDocker Desktopに既に含まれています。もしDocker Composeが未インストールの場合は、[Docker Composeのインストール](https://docs.docker.com/compose/install/)を参照してインストールしてください。

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
       # - "EMQX_FEATURES=dashboard,auth,metrics"
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
       # - "EMQX_FEATURES=dashboard,auth,metrics"
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

MQTTクライアントを使ってEMQXに接続し、メッセージのパブリッシュ／サブスクライブを行ってください。詳細は[パブリッシュとサブスクライブ](../messaging/publish-and-subscribe.md)をご覧ください。

- EMQXのパラメータ設定やその他機能については、[設定](../configuration/configuration.md)をご参照ください。

- 複数ノードによるEMQXクラスターの構築方法については、[クラスター](./cluster/introduction.md)をご覧ください。
