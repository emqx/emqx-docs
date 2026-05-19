# HStreamDBへのMQTTデータストリーミング

[HStreamDB](https://hstream.io/)は、リアルタイムのメッセージ、イベント、その他のデータストリームを効率的に取り込み、保存、処理、配信できるオープンソースのストリーミングデータプラットフォームです。EMQXとHStreamDBの統合により、MQTTメッセージやクライアントイベントをHStreamDBに保存でき、大規模なIoTデータの収集、伝送、保存を実現し、標準SQLやマテリアライズドビューを用いたデータストリームのリアルタイム処理、監視、分析が可能になります。

本ページでは、EMQXとHStreamDB間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

::: tip

HStreamDBデータ統合はEMQX 5.2.0以降でのみサポートされています。

:::

::: tip

HStreamDBデータ統合はEMQX 6.0で削除されます。

:::

## 動作概要

HStreamDBデータ統合はEMQXの標準機能であり、EMQXのデバイス接続およびメッセージ伝送機能とHStreamDBの強力なデータ保存・処理機能を組み合わせています。組み込みのルールエンジンコンポーネントにより、両プラットフォーム間のデータストリーミングと処理が簡素化されています。

以下の図は、EMQXとHStreamDB間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration HStreamDB](./assets/emqx-integration-hstreamdb.png)

EMQXはルールエンジンと設定済みのSinkを通じてMQTTデータをHStreamDBに転送し、全体の流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：IoTデバイスはMQTTプロトコルを介して正常に接続し、特定のトピックにテレメトリやステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンを使用して、特定のトピックに基づくMQTTメッセージを処理します。ルールエンジンは対応するルールとマッチングし、データフォーマット変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **HStreamDBへのデータストリーミング**：ルールがトリガーされると、メッセージがHStreamDBに転送されます。データはHStreamDBのストリーム名、パーティションキー、レコードに簡単に設定でき、後続のデータ処理や分析を容易にします。

MQTTメッセージデータがApache HStreamDBに書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です：

- 特定のMQTTメッセージ受信時にHStreamDBのルールエンジンコンポーネントを使って対応するアクションやイベントをトリガーし、システム間やアプリケーション間のイベント駆動機能を実現。
- HStreamDB内でMQTTデータストリームをリアルタイムに分析し、異常や特定のイベントパターンを検知してアラート通知や対応アクションを実行。
- 複数のMQTTトピックからのデータを統合し、HStreamDBの計算機能を活用してリアルタイム集計、計算、分析を行い、より包括的なデータインサイトを獲得。

## 特長と利点

HStreamDBとのデータ統合は、以下の特長と利点をビジネスにもたらします：

- **信頼性の高いIoTデータメッセージ配信**：EMQXはMQTTメッセージをバッチで確実にHStreamDBに送信でき、IoTデバイスとHStreamDBおよびアプリケーションシステムの統合を実現します。
- **MQTTメッセージ変換**：ルールエンジンを用いてMQTTメッセージのフィルタリングや変換が可能です。データ抽出、フィルタリング、付加情報の追加、変換を行った上でHStreamDBに送信できます。
- **大規模データストリーム保存**：HStreamDBは数百万のデータストリームを分散型でフォールトトレラントなログストレージクラスターに信頼性高く保存可能です。必要に応じてリアルタイムのデータストリーム更新をアプリケーションにリプレイまたはプッシュできます。EMQXのメッセージモデルと完全に統合し、大規模なIoTデータ収集・伝送・保存を実現します。
- **クラスターとスケーラビリティ**：クラウドネイティブアーキテクチャにより、EMQXとHStreamDBはオンラインスケールやクラスターの動的な拡張・縮小をサポートし、増大するビジネスニーズに柔軟に対応可能です。
- **柔軟な処理能力**：HStreamDBでは馴染みのあるSQLを使い、複数のデータストリームのフィルタリング、変換、集計、結合が可能です。標準SQLやマテリアライズドビューを用いたリアルタイム処理、監視、分析に対応し、リアルタイムのデータインサイトを提供します。
- **高スループットシナリオでの処理能力**：HStreamDBデータ統合は同期・非同期の書き込みモードをサポートし、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。

## はじめる前に

このセクションでは、HStreamDBデータ統合の作成を始める前に必要な準備、HStreamDBサービスの起動およびストリームの作成方法について説明します。

以下のサブセクションでは、Linux/MacOSでDockerイメージを使用してHStreamDBをインストールし接続する方法を説明します。Dockerがインストールされており、可能であればDocker Compose v2を使用してください。その他のHStreamDBおよびHStreamDBプラットフォームのインストール方法は、[Quickstart with Docker-Compose](https://docs.hstream.io/start/quickstart-with-docker.html)および[Getting Started with HStream Platform](https://docs.hstream.io/start/try-out-hstream-platform.html)を参照してください。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### HStreamDBサービスの起動とストリームの作成

::::: tabs

:::: tab HStreamDB TCPサービスの起動とストリーム作成

このセクションでは、ローカルDocker環境で単一ノードのHStreamDB TCPサービスを起動し、HStreamDBにストリームを作成する方法を説明します。

::: tip 注意

HStreamDBリソースが接続状態の場合、ストリームの削除や再作成などの操作を行うと、HStreamDBへの再接続（つまりHStreamDBリソースの再起動）が必要になります。

:::

1. 以下の内容で`docker-compose-tcp.yaml`ファイルを作成します。

   ::: details `docker-compose-tcp.yaml`

   ```yaml
   version: "3.9"

   services:
     hserver:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tcp-hserver
       depends_on:
         - zookeeper
         - hstore
       ports:
         - "127.0.0.1:6570:6570"
       expose:
         - 6570
       networks:
         - quickstart-tcp
       volumes:
         - /var/run/docker.sock:/var/run/docker.sock
         - /tmp:/tmp
         - data_store:/data/store
       command:
         - bash
         - "-c"
         - |
           set -e
           /usr/local/script/wait-for-storage.sh hstore 6440 zookeeper 2181 600 \
           /usr/local/bin/hstream-server \
           --bind-address 0.0.0.0 --port 6570 \
           --internal-port 6571 \
           --server-id 100 \
           --seed-nodes "$$(hostname -I | awk '{print $$1}'):6571" \
           --advertised-address $$(hostname -I | awk '{print $$1}') \
           --metastore-uri zk://zookeeper:2181 \
           --store-config /data/store/logdevice.conf \
           --store-admin-host hstore --store-admin-port 6440 \
           --store-log-level warning \
           --io-tasks-path /tmp/io/tasks \
           --io-tasks-network quickstart-tcp

     hstore:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tcp-hstore
       networks:
         - quickstart-tcp
       volumes:
         - data_store:/data/store
       command:
         - bash
         - "-c"
         - |
           set -ex
           # N.B. "enable-dscp-reflection=false" is required for linux kernel which
           # doesn't support dscp reflection, e.g. centos7.
           /usr/local/bin/ld-dev-cluster --root /data/store \
           --use-tcp --tcp-host $$(hostname -I | awk '{print $$1}') \
           --user-admin-port 6440 \
           --param enable-dscp-reflection=false \
           --no-interactive

     zookeeper:
       image: zookeeper:3.8.1
       container_name: quickstart-tcp-zk
       expose:
         - 2181
       networks:
         - quickstart-tcp
       volumes:
         - data_zk_data:/data
         - data_zk_datalog:/datalog

   networks:
     quickstart-tcp:
       name: quickstart-tcp

   volumes:
     data_store:
       name: quickstart_tcp_data_store
     data_zk_data:
       name: quickstart_tcp_data_zk_data
     data_zk_datalog:
       name: quickstart_tcp_data_zk_datalog
   ```

   :::

2. 以下のシェルコマンドを実行してHStreamDB TCPサービスを起動します。

   ```bash
   docker compose -f docker-compose-tcp.yaml up --build
   ```

3. HStreamDBコンテナに入り、`mqtt_connect`と`mqtt_message`という2つのストリームを作成します。

   ::: tip

   HStreamDBのインタラクティブSQL CLIを使ってストリームを作成することも可能です。`hstream --help`でコマンドの詳細を確認してください。

   :::

   ```bash
   $ docker container exec -it quickstart-tcp-hserver bash
   # Stream `mqtt_connect`を作成
   root@9c7ce2f51860:/# hstream stream create mqtt_connect
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_connect | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   # Stream `mqtt_message`を作成
   root@9c7ce2f51860:/# hstream stream create mqtt_message
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_message | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   # すべてのストリームを一覧表示
   root@9c7ce2f51860:/# hstream stream list
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_message | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   | mqtt_connect | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   ```

::::
:::: tab HStreamDB TLSサービスの起動とストリーム作成

このセクションでは、ローカルDocker環境で二ノードのHStreamDB TLSサービスを起動し、HStreamDBにストリームを作成する方法を説明します。

::: tip 注意

HStreamDBリソースが接続状態の場合、ストリームの削除や再作成などの操作を行うと、HStreamDBへの再接続（つまりHStreamDBリソースの再起動）が必要になります。

:::

::: tip Dockerネットワーク環境と証明書ファイルについて

- このDocker Composeファイルは`172.100.0.0/24`のネットワークサブネットをDockerネットワークブリッジとして使用しています。別のネットワーク設定が必要な場合は、Docker Composeファイルを適宜修正してください。
- 現バージョンのHStreamでは、コンテナ間通信に影響を与える可能性があるため、`http_proxy`、`https_proxy`、`all_proxy`などの環境変数をコンテナに設定しないよう注意してください。詳細は[_Docker Network Proxy_](https://docs.docker.com/network/proxy/)を参照してください。
- ルート証明書および自己署名証明書は[_smallstep/step-ca_](https://hub.docker.com/r/smallstep/step-ca)コンテナを使って自動生成され、`172.100.0.10`と`172.100.0.11`の2つのSubject Alternative Nameが設定されています。
- 他の証明書要件がある場合は、証明書ファイルをHStreamDBコンテナにマウントするか、[_Configuring step-ca_](https://smallstep.com/docs/step-ca/configuration/index.html)を参照してください。
  - step-caのデフォルト設定で生成された証明書は有効期限が1日です。証明書の有効期限を変更したい場合は、`ca`ディレクトリ内の証明書を削除し、[_step-ca-configuration-options_](https://smallstep.com/docs/step-ca/configuration/#configuration-options)に従って設定を変更してください。

:::

1. 証明書を保存するために`tls-deploy/ca`ディレクトリを作成します。

   ```bash
   mkdir tls-deploy/ca
   ```

2. `tls-deploy`配下に以下の内容で`docker-compose-tls.yaml`ファイルを作成します。

   ::: details `docker-compose-tls.yaml`

   ```yaml
   version: "3.9"

   services:
     step-ca:
       image: smallstep/step-ca:0.23.0
       container_name: quickstart-tls-step-ca
       networks:
         - quickstart-tls
       volumes:
         - ${step_ca}:/home/step
       environment:
         - DOCKER_STEPCA_INIT_NAME=HStream
         - DOCKER_STEPCA_INIT_DNS_NAMES=step-ca

     generate-hstream-cert:
       image: smallstep/step-ca:0.23.0
       container_name: quickstart-tls-generate-hstream-cert
       depends_on:
         step-ca:
           condition: service_healthy
       networks:
         - quickstart-tls
       volumes:
         - ${step_ca}:/home/step
       command:
         - bash
         - "-c"
         - |
           sleep 1
           if [ -f hstream.crt ]; then exit 0; fi
           step ca certificate "hstream" hstream.crt hstream.key \
           --provisioner-password-file secrets/password --ca-url https://step-ca:9000 \
           --root certs/root_ca.crt \
           --san localhost \
           --san 127.0.0.1 \
           --san 172.100.0.10 \
           --san 172.100.0.11 \
           --san quickstart-tls-hserver-0 \
           --san quickstart-tls-hserver-1

     hserver0:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tls-hserver-0
       depends_on:
         - generate-hstream-cert
         - zookeeper
         - hstore
       ports:
         - "127.0.0.1:6570:6570"
       networks:
         quickstart-tls:
           ipv4_address: 172.100.0.10
       volumes:
         - /var/run/docker.sock:/var/run/docker.sock
         - /tmp:/tmp
         - data_store:/data/store
         - ${step_ca}:/data/server
       command:
         - bash
         - "-c"
         - |
           set -e
           /usr/local/script/wait-for-storage.sh hstore 6440 zookeeper 2181 600; \
           timeout=60; \
           until ( \
              [ -f /data/server/hstream.crt ] && [ -f /data/server/hstream.key ] \
           ) >/dev/null 2>&1; do
               >&2 echo 'Waiting for tls files ...'
               sleep 1
               timeout=$$((timeout - 1))
               [ $$timeout -le 0 ] && echo 'Timeout!' && exit 1;
           done; \
           /usr/local/bin/hstream-server \
           --bind-address 0.0.0.0 --port 26570 \
           --internal-port 6571 \
           --server-id 100 \
           --seed-nodes "hserver0:6571,hserver1:6573" \
           --advertised-address $$(hostname -I | awk '{print $$1}') \
           --metastore-uri zk://zookeeper:2181 \
           --store-config /data/store/logdevice.conf \
           --store-admin-host hstore --store-admin-port 6440 \
           --io-tasks-path /tmp/io/tasks \
           --io-tasks-network quickstart-tls \
           --tls-cert-path /data/server/hstream.crt \
           --tls-key-path /data/server/hstream.key \
           --advertised-listeners l1:hstream://172.100.0.10:6570 \
           --listeners-security-protocol-map l1:tls

           # NOTE:
           # advertised-listeners ip addr should same as container addr for tls listener

     hserver1:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tls-hserver-1
       depends_on:
         - zookeeper
         - hstore
       ports:
         - "127.0.0.1:6572:6572"
       expose:
         - 6572
         - 26572
       networks:
         quickstart-tls:
           ipv4_address: 172.100.0.11
       volumes:
         - /var/run/docker.sock:/var/run/docker.sock
         - /tmp:/tmp
         - data_store:/data/store
         - ${step_ca}:/data/server
       command:
         - bash
         - "-c"
         - |
           set -e
           /usr/local/script/wait-for-storage.sh hstore 6440 zookeeper 2181 600; \
           timeout=60; \
           until ( \
              [ -f /data/server/hstream.crt ] && [ -f /data/server/hstream.key ] \
           ) >/dev/null 2>&1; do
               >&2 echo 'Waiting for tls files ...'
               sleep 1
               timeout=$$((timeout - 1))
               [ $$timeout -le 0 ] && echo 'Timeout!' && exit 1;
           done; \
           /usr/local/bin/hstream-server \
           --bind-address 0.0.0.0 --port 26572 \
           --internal-port 6573 \
           --server-id 101 \
           --seed-nodes "hserver0:6571,hserver1:6573" \
           --advertised-address $$(hostname -I | awk '{print $$1}') \
           --metastore-uri zk://zookeeper:2181 \
           --store-config /data/store/logdevice.conf \
           --store-admin-host hstore --store-admin-port 6440 \
           --io-tasks-path /tmp/io/tasks \
           --io-tasks-network quickstart-tls \
           --tls-cert-path /data/server/hstream.crt \
           --tls-key-path /data/server/hstream.key \
           --advertised-listeners l1:hstream://172.100.0.11:6572 \
           --listeners-security-protocol-map l1:tls

           # NOTE:
           # advertised-listeners ip addr should same as container addr for tls listener

     hserver-init:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tls-hserver-init
       depends_on:
         - hserver0
         - hserver1
       networks:
         - quickstart-tls
       command:
         - bash
         - "-c"
         - |
           timeout=60
           until ( \
               /usr/local/bin/hadmin server --host 172.100.0.10 --port 26570 status && \
               /usr/local/bin/hadmin server --host 172.100.0.11 --port 26572 status \
           ) >/dev/null 2>&1; do
               >&2 echo 'Waiting for servers ...'
               sleep 1
               timeout=$$((timeout - 1))
               [ $$timeout -le 0 ] && echo 'Timeout!' && exit 1;
           done; \
           /usr/local/bin/hadmin server --host hserver0 --port 26570 init

     hstore:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tls-hstore
       networks:
         - quickstart-tls
       volumes:
         - data_store:/data/store
       command:
         - bash
         - "-c"
         - |
           set -ex
           /usr/local/bin/ld-dev-cluster --root /data/store \
           --use-tcp --tcp-host $$(hostname -I | awk '{print $$1}') \
           --user-admin-port 6440 \
           --no-interactive

     zookeeper:
       image: zookeeper:3.8.1
       container_name: quickstart-tls-zk
       expose:
         - 2181
       networks:
         - quickstart-tls
       volumes:
         - data_zk_data:/data
         - data_zk_datalog:/datalog

   networks:
     quickstart-tls:
       ipam:
         driver: default
         config:
           - subnet: "172.100.0.0/24"
       name: quickstart-tls

   volumes:
     data_store:
       name: quickstart_tls_data_store
     data_zk_data:
       name: quickstart_tls_data_zk_data
     data_zk_datalog:
       name: quickstart_tls_data_zk_datalog
   ```

   :::

   これでディレクトリ構成は以下のようになります。

   ```bash
   $ tree tls-deploy
   tls-deploy
   ├── ca
   └── docker-compose-tls.yaml

   2 directories, 1 file
   ```

3. `tls-deploy`ディレクトリに移動し、以下のシェルコマンドを実行してHStreamDB TLSサービスを起動します。

   ```bash
   env step_ca=$PWD/ca docker compose -f docker-compose-tls.yaml up --build
   ```

4. HStreamDBコンテナに入り、`mqtt_connect`と`mqtt_message`という2つのストリームを作成します。

   :::tip TLS接続コマンドオプション

   TCPサービスと同様に、コマンドに`--tls-ca [CA_PATH]`オプションを追加するだけで接続できます。ノード`quickstart-tls-hserver-1`でコマンドを実行する場合は、docker-composeファイルで指定されたポートと一致させるために`--port 6572`オプションも指定してください。

   :::

   ```bash
   $ docker container exec -it quickstart-tls-hserver-0 bash
   # Stream `mqtt_connect`を作成
   root@75c9351cbb38:/# hstream --tls-ca /data/server/certs/root_ca.crt stream create mqtt_connect
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_connect | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   # Stream `mqtt_message`を作成
   root@75c9351cbb38:/# hstream --tls-ca /data/server/certs/root_ca.crt stream create mqtt_message
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_message | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   # すべてのストリームを一覧表示
   root@75c9351cbb38:/# hstream --tls-ca /data/server/certs/root_ca.crt stream list
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_message | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   | mqtt_connect | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   ```

::::
:::::

## コネクターの作成

このセクションでは、SinkをHStreamDBサーバーに接続するためのコネクターを作成する方法を示します。

以下の手順は、EMQXとHStreamDBの両方をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードにログインし、**Integration** -> **Connectors**をクリックします。
2. 画面右上の**Create**をクリックします。
3. **Create Connector**ページで**HStreamDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します（アスタリスク付きは必須項目です）：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_hstreamdb`。
   - **HStreamDB Server URL**：`hstream://127.0.0.1:6570`、または実際のHStreamDBのアドレスとポートを指定します。
     - スキームは`http`、`https`、`hstream`、`hstreams`をサポートします。
     - TLS接続の場合はスキームを`hstreams`または`https`にします。例：`hstreams://127.0.0.1:6570`。
   - **HStreamDB Stream Name**：前述で作成したストリーム名を入力します。
     - クライアントメッセージ保存用は`mqtt_message`。
     - イベント記録用は`mqtt_connect`。
   - **HStreamDB Partition Key**：HStreamDBのパーティションやノード内のデータ保存先を決定するためのパーティションキーを指定します。例として`${topic}`を指定すると、同じトピックのメッセージが順序通りに書き込まれます。未指定の場合はデフォルトキーが使われ、データはデフォルトのシャードにマッピングされます。
   - **HStreamDB gRPC Timeout**：gRPCリクエスト時にHStreamDBサーバーからの応答を待つ最大時間（秒）を指定します。デフォルトは`30`秒です。
   - **Enable TLS**：必要に応じてTLS接続を有効にするためのトグルスイッチです。TLSを有効にした場合は**TLS Verify**を無効にしてください。`tls-deploy/ca`ディレクトリに生成された証明書とキーをアップロードします：
     - `ca/hstream.crt`を**TLS Cert**にアップロード。
     - `ca/hstream.key`を**TLS Key**にアップロード。
     - `ca/certs/root_ca.crt`を**CA Cert**にアップロード。
5. 高度な設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがHStreamDBサーバーに接続できるかテストできます。
7. 画面下部の**Create**ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを指定したルールの作成に進めます。詳細は[メッセージ保存用HStreamDB Sinkのルール作成](#create-a-rule-with-hstreamdb-sink-for-message-storage)および[イベント記録用HStreamDB Sinkのルール作成](#create-a-rule-with-hstreamdb-sink-for-events-recording)を参照してください。

## メッセージ保存用HStreamDB Sinkのルール作成

このセクションでは、ダッシュボードでMQTTトピック`t/#`のメッセージを処理し、処理済みデータを設定済みのSink経由でHStreamDBストリーム`mqtt_message`に書き込むルールを作成する方法を示します。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Rules**をクリックします。

2. 画面右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力します。**SQL Editor**で以下のステートメントを設定します。これはトピック`t/#`配下のMQTTメッセージをHStreamDBに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`句に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストできます。

   :::

4. + **Add Action**ボタンをクリックして、ルールがトリガーするアクションを定義します。このアクションにより、EMQXはルールで処理したデータをHStreamDBに送信します。

5. **Type of Action**ドロップダウンから`HStreamDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能ですが、ここでは新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせにしてください。

7. **Connector**ドロップダウンから先ほど作成した`my_hstreamdb`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

8. メッセージを特定トピックに転送するための**HStream Record Template**を以下のテンプレートで設定します。

   ```json
   {"id": ${id}, "topic": "${topic}", "qos": ${qos}, "payload": "${payload}"}
   ```

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **高度な設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**を押してSinkがHStreamDBサーバーに接続できるかテストします。

12. **Create**ボタンをクリックしてSinkの設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これで、HStreamDB Sinkを通じてデータ転送およびオンライン/オフライン状態の記録用ルールが正常に作成されました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいHStreamDB Sinkが表示されます。

また、**Integration** -> **Flow Designer**でトポロジーを確認すると、トピック`t/#`配下のメッセージがルール`my_rule`で解析され、HStreamDBに送信・保存されていることが分かります。

## イベント記録用HStreamDB Sinkのルール作成

このセクションでは、クライアントのオンライン/オフライン状態を記録し、イベントデータを設定済みSink経由でHStreamDBストリーム`mqtt_connect`に書き込むルールの作成方法を示します。

ルール作成手順は[メッセージ保存用HStreamDB Sinkのルール作成](#create-a-rule-with-hstreamdb-sink-for-message-storage)とほぼ同様ですが、SQLルール構文とストリームレコードテンプレートが異なります。

オンライン/オフライン状態記録用のSQLルール構文は以下の通りです：

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

Sinkの**Stream Record Template**は以下の通りです：

```sql
{"clientid": "${clientid}", "event_type": "${event}", "event_time": ${timestamp}}
```

## ルールのテスト

MQTTXを使ってトピック`t/1`にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello HStreamDB" }'
```

2つのSinkの動作状況を確認します。

- メッセージ保存用Sinkでは、新規の受信メッセージと送信メッセージが1件ずつあるはずです。ストリーム`mqtt_message`にデータが書き込まれているか確認します。

```bash
# ストリーム`mqtt_message`の読み取りを停止するにはCtrl-Cを押します
root@9c7ce2f51860:/# hstream stream read-stream mqtt_message
timestamp: "1693903488278", id: 1947758763121538-8589934593-0, key: "", record: {"id": 00060498A3B3C4F8F4400100127E0002, "topic": "t/1", "qos": 0, "payload": { "msg": "Hello HStreamDB" }}
^CRead Done.
```

- オンライン/オフライン状態記録用Sinkでは、クライアント接続および切断の2つの新規イベントが記録されているはずです。ストリーム`mqtt_connect`に状態記録が書き込まれているか確認します。

```bash
# ストリーム`mqtt_connect`の読み取りを停止するにはCtrl-Cを押します
root@9c7ce2f51860:/# hstream stream read-stream mqtt_connect
timestamp: "1693903488274", id: 1947758827604597-8589934593-0, key: "", record: {"clientid": "emqx_c", "event_type": "client.connected", "event_time": 1693903488266}
timestamp: "1693903488294", id: 1947758827604597-8589934594-0, key: "", record: {"clientid": "emqx_c", "event_type": "client.disconnected", "event_time": 1693903488271}
^CRead Done.
```
