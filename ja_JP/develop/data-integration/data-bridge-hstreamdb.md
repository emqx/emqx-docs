# HStreamDBへのMQTTデータストリーミング

[HStreamDB](https://hstream.io/)は、リアルタイムのメッセージ、イベント、その他のデータストリームを効率的に取り込み、保存、処理、配信できるオープンソースのストリーミングデータプラットフォームです。EMQXとHStreamDBの統合により、MQTTメッセージやクライアントイベントをHStreamDBに保存でき、大規模なIoTデータの収集、伝送、保存を実現し、標準SQLやマテリアライズドビューを用いたデータストリームのリアルタイム処理、監視、分析が可能になります。

本ページでは、EMQXとHStreamDB間のデータ統合について包括的に紹介し、データ統合の作成と検証に関する実践的な手順を提供します。

::: tip

HStreamDBデータ統合はEMQX 5.2.0以降でのみサポートされています。

:::

## 動作の仕組み

HStreamDBデータ統合は、EMQXのデバイス接続およびメッセージ伝送機能とHStreamDBの堅牢なデータ保存・処理機能を組み合わせたEMQXの標準機能です。組み込みのルールエンジンコンポーネントにより、両プラットフォーム間のデータストリーミングと処理が簡素化されています。

以下の図は、EMQXとHStreamDB間のデータ統合の典型的なアーキテクチャを示しています：

![EMQX Integration HStreamDB](./assets/emqx-integration-hstreamdb.png)

EMQXはルールエンジンと設定されたSinkを通じてMQTTデータをHStreamDBに転送し、処理の流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：IoTデバイスはMQTTプロトコルで正常に接続し、特定のトピックにテレメトリや状態データをパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンを使い、特定のソースからのMQTTメッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチさせ、データ形式変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **HStreamDBへのデータストリーミング**：ルールがトリガーされると、メッセージをHStreamDBに転送するアクションが実行されます。データはHStreamDBのストリーム名、パーティションキー、レコードに簡単に設定でき、その後のデータ処理や分析を容易にします。

MQTTメッセージデータがApache HStreamDBに書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です：

- 特定のMQTTメッセージ受信時にHStreamDBのルールエンジンコンポーネントを使って対応するアクションやイベントをトリガーし、システム間やアプリケーション間のイベント駆動機能を実現。
- HStreamDB内でMQTTデータストリームをリアルタイムに分析し、異常や特定のイベントパターンを検出してアラート通知や対応アクションを実行。
- 複数のMQTTトピックからのデータを統合し、HStreamDBの計算機能を活用してリアルタイム集計、計算、分析を行い、より包括的なデータインサイトを獲得。

## 特徴と利点

HStreamDBとのデータ統合は、ビジネスに以下の特徴と利点をもたらします：

- **信頼性の高いIoTデータメッセージ配信**：EMQXはMQTTメッセージをバッチ処理で確実にHStreamDBに送信でき、IoTデバイスとHStreamDBおよびアプリケーションシステムの統合を実現します。
- **MQTTメッセージの変換**：ルールエンジンを用いて、EMQXはMQTTメッセージの抽出、フィルタリング、付加、変換を行い、HStreamDBに送信します。
- **大規模データストリーム保存**：HStreamDBは分散型でフォールトトレラントなログストレージクラスターを備え、数百万のデータストリームを信頼性高く保存します。必要に応じてリアルタイムのデータストリーム更新をアプリケーションに再生またはプッシュ可能です。EMQXのメッセージモデルと完全に統合し、大規模なIoTデータ収集、伝送、保存を実現します。
- **クラスターとスケーラビリティ**：クラウドネイティブアーキテクチャを採用し、EMQXとHStreamDBはオンラインスケーリングやクラスターの動的な拡張・縮小をサポートし、増大するビジネス需要に柔軟に対応します。
- **柔軟な処理能力**：HStreamDBではおなじみのSQLを使って複数のデータストリームのフィルタリング、変換、集約、結合が可能です。標準SQLやマテリアライズドビューを使ったリアルタイム処理、監視、分析をサポートし、リアルタイムのデータインサイトを提供します。
- **高スループットシナリオでの処理能力**：HStreamDBデータ統合は同期・非同期の両書き込みモードをサポートし、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。

## はじめる前に

このセクションでは、HStreamDBデータ統合の作成を始める前に必要な準備、HStreamDBサービスの起動方法とストリームの作成方法について説明します。

以下のサブセクションでは、Linux/MacOS環境でDockerイメージを使ってHStreamDBをインストールし接続する方法を説明します。Dockerがインストールされており、可能であればDocker Compose v2を使用してください。その他のHStreamDBおよびHStreamDBプラットフォームのインストール方法は、[Quickstart with Docker-Compose](https://docs.hstream.io/start/quickstart-with-docker.html)および[Getting Started with HStream Platform](https://docs.hstream.io/start/try-out-hstream-platform.html)を参照してください。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### HStreamDBサービスの起動とストリームの作成

::::: tabs

:::: tab HStreamDB TCPサービスの起動とストリーム作成

このセクションでは、ローカルのDocker環境で単一ノードのHStreamDB TCPサービスを起動し、その後HStreamDBでストリームを作成する方法を説明します。

::: tip 注意

HStreamDBリソースが接続状態になった後、ストリームの削除や再作成などの操作を行う場合は、HStreamDBへの再接続（つまりHStreamDBリソースの再起動）が必要です。

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

3. HStreamコンテナに入り、`mqtt_connect`と`mqtt_message`という2つのストリームを作成します。

   ::: tip

   HStreamDBの対話型SQL CLIを使ってストリームを作成することも可能です。`hstream --help`で`hstream`コマンドの使用方法を確認してください。

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
   # 全ストリーム一覧表示
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

このセクションでは、ローカルのDocker環境で2ノードのHStreamDB TLSサービスを起動し、その後HStreamDBでストリームを作成する方法を説明します。

::: tip 注意

HStreamDBリソースが接続状態になった後、ストリームの削除や再作成などの操作を行う場合は、HStreamDBへの再接続（つまりHStreamDBリソースの再起動）が必要です。

:::

::: tip Dockerネットワーク環境と証明書ファイルについて

- このDocker Composeファイルは`172.100.0.0/24`のネットワークサブネットをDockerネットワークブリッジとして使用しています。別のネットワーク設定が必要な場合はDocker Composeファイルを適宜修正してください。
- 現バージョンのHStreamでは、コンテナ間通信に影響を与える可能性があるため、`http_proxy`、`https_proxy`、`all_proxy`などのデフォルト環境変数をコンテナに設定しないでください。詳細は[_Docker Network Proxy_](https://docs.docker.com/network/proxy/)を参照してください。
- ルート証明書と自己署名証明書は[_smallstep/step-ca_](https://hub.docker.com/r/smallstep/step-ca)コンテナを使って自動生成され、`172.100.0.10`と`172.100.0.11`の2つのサブジェクト代替名で設定されています。
- 別の証明書要件がある場合は、証明書ファイルをHStreamDBコンテナにマウントするか、[_Configuring step-ca_](https://smallstep.com/docs/step-ca/configuration/index.html)を参照してください。
  - step-caのデフォルト設定で生成される証明書は有効期限が1日です。有効期限を変更したい場合は`ca`ディレクトリ内の証明書を削除し、[_step-ca-configuration-options_](https://smallstep.com/docs/step-ca/configuration/#configuration-options)に従って設定を変更してください。

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

   これでディレクトリ構造は以下のようになります：

   ```bash
   $ tree tls-deploy
   tls-deploy
   ├── ca
   └── docker-compose-tls.yaml

   2 directories, 1 file
   ```

3. `tls-deploy`ディレクトリに入り、以下のシェルコマンドを実行してHStreamDB TLSサービスを起動します。

   ```bash
   env step_ca=$PWD/ca docker compose -f docker-compose-tls.yaml up --build
   ```

4. HStreamDBコンテナに入り、`mqtt_connect`と`mqtt_message`という2つのストリームを作成します。

   :::tip TLS接続コマンドオプション

   TCPサービスと同様に、コマンドラインに`--tls-ca [CA_PATH]`オプションを追加するだけで接続可能です。`quickstart-tls-hserver-1`ノードでコマンドを実行する場合は、docker-composeファイルで指定されたポートと一致させるために`--port 6572`オプションを追加してください。

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
   # 全ストリーム一覧表示
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

このセクションでは、SinkをHStreamDBサーバーに接続するためのコネクターの作成方法を示します。

以下の手順は、EMQXとHStreamDBをローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**HStreamDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します（*印のある項目は必須です）：
   - **Connector name**：コネクター名を英数字の組み合わせで入力します。例：`my_hstreamdb`
   - **HStreamDB Server URL**：`hstream://127.0.0.1:6570`または実際のHStreamDBのアドレスとポートを入力します。
     - スキームは`http`、`https`、`hstream`、`hstreams`をサポートします。
     - TLS接続の場合はスキームを`hstreams`または`https`にします。例：`hstreams://127.0.0.1:6570`
   - **HStreamDB Stream Name**：事前に作成したストリーム名を入力します。
     - クライアントメッセージ保存用は`mqtt_message`
     - イベント記録用は`mqtt_connect`
   - **HStreamDB Partition Key**：HStreamDB内のパーティションやノードのどこにデータを保存するかを決定するためのパーティションキーを指定します。例として`${topic}`を指定すると、同じトピックのメッセージがHStreamDB内で順序を保って書き込まれます。未指定の場合はデフォルトキーが使用され、データはデフォルトのシャードにマッピングされます。
   - **HStreamDB gRPC Timeout**：gRPCリクエスト時にHStreamDBサーバーからの応答を待つ最大時間（秒）を指定します。デフォルトは`30`秒です。
   - **Enable TLS**：必要に応じてTLS接続を有効にするためにトグルスイッチをクリックします。TLS有効時は**TLS Verify**を無効にします。`tls-deploy/ca`ディレクトリで生成した証明書とキーをアップロードします：
     - `ca/hstream.crt`を**TLS Cert**にアップロード
     - `ca/hstream.key`を**TLS Key**にアップロード
     - `ca/certs/root_ca.crt`を**CA Cert**にアップロード
5. 詳細設定（任意）：詳細は[Features of Sink](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがHStreamDBサーバーに接続できるかテストできます。
7. ページ下部の**Create**ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSink付きのルール作成に進み、HStreamDBに転送するデータやクライアントイベントの記録を指定します。詳細は[Create a Rule with HStreamDB Sink for Message Storage](#create-a-rule-with-hstreamdb-sink-for-message-storage)および[Create a Rule with HStreamDB Sink for Events Recording](#create-a-rule-with-hstreamdb-sink-for-events-recording)を参照してください。

## HStreamDB Sinkを使ったメッセージ保存ルールの作成

このセクションでは、ダッシュボード上でソースMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定したSink経由でHStreamDBストリーム`mqtt_message`に書き込むルールの作成方法を示します。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**で以下のステートメントを設定します。これはトピック`t/#`以下のMQTTメッセージをHStreamDBに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`句に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストできます。

   :::

4. + **Add Action**ボタンをクリックし、ルールがトリガーするアクションを定義します。このアクションにより、EMQXはルールで処理したデータをHStreamDBに送信します。

5. **Type of Action**ドロップダウンリストから`HStreamDB`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能ですが、このデモでは新しいSinkを作成します。

6. Sinkの名前を入力します。名前は英数字の組み合わせにしてください。

7. **Connector**ドロップダウンから先ほど作成した`my_hstreamdb`を選択します。新しいコネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[Create a Connector](#create-a-connector)を参照してください。

8. メッセージを特定のトピックに転送するための**HStream Record Template**を以下のテンプレートで設定します：

   ```json
   {"id": ${id}, "topic": "${topic}", "qos": ${qos}, "payload": "${payload}"}
   ```

9. **Fallback Actions（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Features of Sink](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがHStreamDBサーバーに接続できるかテストします。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでHStreamDB Sinkを通じたデータ転送とオンライン/オフライン状態の記録ルールが正常に作成されました。**Integration** -> **Rules**ページで新規作成ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいHStreamDB Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されHStreamDBに送信・保存されている様子が確認できます。

## HStreamDB Sinkを使ったイベント記録ルールの作成

このセクションでは、クライアントのオンライン/オフライン状態を記録し、イベントデータを設定したSink経由でHStreamDBストリーム`mqtt_connect`に書き込むルールの作成方法を示します。

ルール作成手順は[メッセージ保存用のStream Sinkでルールを作成](#create-a-rule-with-hstreamdb-sink-for-message-storage)とほぼ同様ですが、SQLルール構文とStream Recordテンプレートが異なります。

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

2つのSinkの稼働状況を確認します。

- メッセージ保存用Sinkでは、新規の受信メッセージと送信メッセージが1件ずつあるはずです。ストリーム`mqtt_message`にデータが書き込まれているか確認します：

```bash
# ストリーム`mqtt_message`の読み取り後、Control-Cで停止
root@9c7ce2f51860:/# hstream stream read-stream mqtt_message
timestamp: "1693903488278", id: 1947758763121538-8589934593-0, key: "", record: {"id": 00060498A3B3C4F8F4400100127E0002, "topic": "t/1", "qos": 0, "payload": { "msg": "Hello HStreamDB" }}
^CRead Done.
```

- オンライン/オフライン状態記録用Sinkでは、新たに2件のイベント（クライアント接続・切断）が記録されているはずです。ストリーム`mqtt_connect`に状態記録が書き込まれているか確認します：

```bash
# ストリーム`mqtt_connect`の読み取り後、Control-Cで停止
root@9c7ce2f51860:/# hstream stream read-stream mqtt_connect
timestamp: "1693903488274", id: 1947758827604597-8589934593-0, key: "", record: {"clientid": "emqx_c", "event_type": "client.connected", "event_time": 1693903488266}
timestamp: "1693903488294", id: 1947758827604597-8589934594-0, key: "", record: {"clientid": "emqx_c", "event_type": "client.disconnected", "event_time": 1693903488271}
^CRead Done.
```
