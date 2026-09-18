# クラスターの作成と管理

EMQXクラスターは手動または自動で作成できます。本ページでは手動クラスタリングと自動クラスタリングの両方の方法を紹介し、これら2つの異なるアプローチによるEMQXクラスターの作成と管理について案内します。

::: tip

クラスター機能は有効なライセンスキーがある場合のみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識や動作については、[Cluster](../../develop/cluster/introduction.md)および[Architecture](../../develop/cluster/mria-introduction.md)をご参照ください。クラスターを作成するには、以下の概念も理解しておく必要があります。

### ノード名

EMQXのノードはノード名で識別されます。すべてのノードは `name@host` 形式の一意のノード名が設定されており、hostはIPアドレスまたは完全修飾ドメイン名（FQDN）でなければなりません。例：

- サーバー `s1.emqx.io` にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` とします。
- このサーバーが静的IP（`192.168.0.10`）を持つ場合、ノード名は `emqx@192.168.0.10` となります。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更不可です。したがって、EMQXノード名には静的なFQDNの使用を推奨します。
:::

### ノードディスカバリー

ノードディスカバリーはクラスター作成に必須のプロセスであり、個々のEMQXノードが互いに発見し通信できるようにします。場所やIPアドレスに依存しません。

### 手動クラスタリングと自動クラスタリング

ノードディスカバリーの戦略に基づき、クラスター作成方法は手動クラスタリングと自動クラスタリングに分かれます。

手動クラスタリングは、どのノードをクラスターに含めるかを手動で指定してEMQXクラスターを作成する方法です。自動クラスタリングは、複数のEMQXノードが手動設定なしで自動的にクラスターを形成する方法で、クラスターのセットアップを簡素化し、動的にノードの追加・削除を容易にします。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesなどに基づく自動クラスタリングをサポートしています。

以下の表はEMQXがサポートするノードディスカバリー戦略とクラスター作成方法を示します。

| 戦略           | 説明                                                         |
| -------------- | ------------------------------------------------------------ |
| `manual`       | コマンドによる手動クラスター作成                             |
| `static`       | 静的ノードリストによる自動クラスタリング                     |
| `dns`          | DNSのAレコードおよびSRVレコードによる自動クラスタリング      |
| `etcd`         | etcdによる自動クラスタリング                                 |
| `k8s`          | Kubernetesによる自動クラスタリング                           |
| `singleton`    | クラスタリング無効。ノードは他ノードとの接続試行をすべて拒否。 |

EMQXは[Erlang/OTPアプリケーション向けに開発されたクラスター管理ライブラリEkka](https://github.com/emqx/ekka)に基づく自動クラスター作成をサポートしています。EkkaはErlangノードの自動発見（サービスディスカバリー）や自動クラスタリング（Autocluster）に加え、ネットワーク分断の自動修復（Network Partition Autoheal）やダウンしたノードの自動削除（Autoclean）などの機能も実装しています。

`emqx.conf`設定ファイルでノードディスカバリー戦略を設定することでクラスタリング方法を定義できます。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめる前に

このセクションでは、クラスター作成前にノードやネットワーク環境をどのように設定するかについて説明します。

### ノード名の設定

クラスターに参加するノードの名前付け方法を理解しておく必要があります。例えば、`s1.emqx.io` と `s2.emqx.io` にそれぞれデプロイされた2ノードのクラスターを作成する場合、以下の手順に従います。

1つ目のノードの `emqx.conf` にノード名を設定します。

```bash
node.name = emqx@s1.emqx.io
```

または環境変数でノード名を上書きできます。例えば、`docker run` の `-e` オプションや systemd の `emqx.service` ファイルで以下のように定義します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

同様に、クラスターに参加する他のノードも同様に設定します。

これで2つのノード `emqx@s1.emqx.io` と `emqx@s2.emqx.io` の名前付けが完了し、手動または自動でクラスターを作成できます。

### ノードクッキーの設定

セキュリティ上の理由から、クラスターに参加するすべてのノードの `emqx.conf` でデフォルトのクッキー設定をSecretクッキーに変更してください。クラスターに参加するすべてのノードは同じSecretクッキーを使用する必要があります。マジッククッキーの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)をご覧ください。

```
node {
  cookie = "<Secretクッキー>"
}
```

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合、以下のクラスター内部通信に必要なポートを開放する必要があります。

- **4370**: Erlang分散トランスポートポート
- **5370**: クラスターRPCポート（物理マシン環境向け）
- **5369**: クラスターRPCポート（Docker環境向け）

1台のサーバーに複数のEMQXノードをデプロイする場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[Intra-cluster communication port](./security.md)をご参照ください。

## クイックスタート

このセクションでは、Dockerネットワーク上で2つの異なるクラスタリング方法を用いてクラスターを素早く作成する方法を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを実行しクラスターを形成する場合は、追加の設定が必要です。コンテナ内で必要なクラスター通信ポートをマッピングし、ファイアウォールでこれらのポートを開放してください。詳細は[Configure Network Environment](#configure-network-environment)をご覧ください。

:::

:::: tabs type:card

::: tab 手動クラスタリングの例

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 1つ目のノードを起動し、環境変数でノード名を設定します。EMQXのデフォルトクラスタリング方法は手動なので追加設定は不要です。ノードをDockerネットワークに追加し、ノードホストに対応するネットワークエイリアスを設定します。

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       --network emqx-net \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx-enterprise:@EE_VERSION@
   ```

3. 1つ目のノード起動後、2つ目のノードを起動します。新しいノードは1つ目のノードと同じネットワークに参加する必要があります。1つ目のノードが既に1883などのポートを占有しているため、ここではポートマッピングは行いません。

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       --network emqx-net \
       --network-alias node2.emqx.com \
       emqx/emqx-enterprise:@EE_VERSION@
   ```

4. 任意のノード上で以下のコマンドを実行し、現在のノードを他のノードに接続してクラスターを作成します。コマンドの詳細は[Manual Clustering](#manual-clustering)をご参照ください。

   ```bash
   docker exec -it emqx2 \
       emqx ctl cluster join emqx@node1.emqx.com
   ```

:::

::: tab 自動クラスタリングの例（静的方式）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 1つ目のノードを起動し、環境変数でノード名とクラスタリング方法を設定します。

   - `EMQX_NODE_NAME` 環境変数でノード名を設定
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY` 環境変数でクラスターディスカバリー戦略を設定（ここでは[静的クラスタリング](#autocluster-by-static-node-list)）
   - `EMQX_CLUSTER__STATIC__SEEDS` 環境変数で静的ノードリストを設定（すべてのノード名を含める）

   また、ノードをDockerネットワークに追加し、ノードホストに対応するネットワークエイリアスを設定します。

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
       -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
       --network emqx-net \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx-enterprise:@EE_VERSION@
   ```

3. 1つ目のノード起動後、2つ目のノードを起動します。クラスタリング方法と新しいノードは1つ目のノードと同じネットワークに参加する必要があります。1つ目のノードが既に1883などのポートを占有しているため、ここではポートマッピングは行いません。

   ```bash
   docker run -d \
      --name emqx2 \
      -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
      -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
      -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
      --network emqx-net \
      --network-alias node2.emqx.com \
      emqx/emqx-enterprise:@EE_VERSION@
   ```

:::

::::

任意のノード上で `emqx ctl cluster status` コマンドを実行するとクラスターの状態を確認できます。正常な場合、以下のような情報が出力されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの指示に従い、必要なクラスター作成方法を選択して修正・デプロイできます。

## 手動クラスタリング

このセクションでは手動でクラスターを作成する手順を説明します。手動クラスタリングではクラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も構築する必要があります。自動クラスタリングに比べてカスタムネットワークトポロジーの細かい調整が可能であり、自動クラスタリングが利用できない、または適さない状況に適しています。

:::tip

手動クラスタリングはコアノードのみで使用可能です。コア-レプリカノードのデプロイアーキテクチャを使用している場合は、自動クラスタリングでクラスターを管理してください。

:::

例えば、`emqx@node1.emqx.com` と `emqx@node2.emqx.com` の2ノードがある場合、以下の手順で手動クラスターを作成できます。

1. クラスターのディスカバリー戦略を `manual` に設定します。

   ```bash
   cluster {
       ## Options: manual | static | dns | etcd | k8s | singleton
       discovery_strategy  =  manual
   }
   ```

2. 2つのノードを起動後、いずれかのノードでクラスター参加コマンドを実行します。

   ```bash
   $ ./bin/emqx ctl cluster join emqx@node1.emqx.com

   Join the cluster successfully.
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

   :::tip

   - このコマンドはクラスターに参加する側のノードで実行する必要があります。つまり**招待**ではなく**参加要求**です。
   - `emqx@s2.emqx.io` が `emqx@s1.emqx.io` に参加してクラスターを形成すると、ローカルデータはクリアされ、`emqx@s1.emqx.io` のデータと同期されます。
   - `emqx@s2.emqx.io` が別のクラスターに参加したい場合、まず現在のクラスターを離脱する必要があります。離脱方法は[Leave Cluster](#leave-cluster)をご覧ください。

   :::

3. 任意のノードでクラスター状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status

   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

これで2ノードのクラスター作成に成功しました。クラスターの監視方法や管理方法については、[Query Cluster Status](#query-cluster-status)、[Manage Cluster Nodes](#manage-cluster-nodes)、[Configure Network Protocols](#configure-network-protocols)の各セクションをご覧ください。

EMQX v5.9.0以降は、EMQXダッシュボードからもノードの招待や管理が可能です。詳細は[Cluster](../dashboard/cluster_settings.md#cluster)をご参照ください。

## 自動クラスタリング

このセクションでは各種自動クラスタリング方法によるクラスター作成方法を説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードにあらかじめ定義された静的ノードリストを設定し、起動後にノードリストに従って自動的にクラスターを形成する方法です。

静的クラスタリングは他のネットワークコンポーネントやサービスに依存せず、最も簡単にEMQXクラスターを自動作成できる方法です。各ノードがTCPプロトコルで相互通信可能であれば、EMQXクラスターを形成できます。

この機能を有効にするには、`emqx.conf`でクラスターモードとノードリストを設定します。

**例:**

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@s1.emqx.io", "emqx@s2.emqx.io"]
    }
}
```

<!--v5.0.23 e5.0.4 以前は ["emqx1", "emqx2"] のみ対応
v5.0.23e5.0.4 以降は両方対応-->

- `discovery_strategy` はノードディスカバリー戦略で、`static` に設定
- `seeds` は配列で、クラスターに参加するノード名を複数カンマ区切りで指定

すべてのノードを起動するとクラスターが自動的に形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)（ドメインネームシステム）は、DNSサーバーがドメイン名の問い合わせに対して該当ドメインのIPアドレス（Aレコード）を返す仕組みです。DNSは1つのドメイン名に複数のAレコード（複数IPアドレス）を設定でき、1つの名前に複数のIPアドレスを対応させることができます。EMQXのDNS自動クラスタリングはこの1対多の対応を利用し、クラスター内のすべてのノードを特定し、各独立ノードがクラスターに参加できるようにします。

#### DNSサービスの設定

ほとんどのパブリッククラウドサービスはDNSサービスを提供しています。ドメイン名を割り当てた後、各EMQXノードのIPアドレスをこのドメインのAレコードに追加するだけで設定完了です。プライベートクラウドや内部ネットワークにEMQXをデプロイする場合は、自身でDNSシステム（例：ソフトウェア[BIND](https://www.isc.org/bind/)）を構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf` の `cluster.dns` 設定項目にクラスター参加ノードを追加します。

**例:**

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        ## DNS AレコードおよびDNS SRVレコードをサポート
        record_type = a
    }
}
```

- `discovery_strategy` はノードディスカバリー戦略で `dns` に設定
- `cluster.dns.name` は文字列でDNS名を指定（例：localhost）
- `cluster.dns.record_type` は列挙型で、`a` または `srv` を指定可能

すべてのノードを起動するとクラスターが自動的に形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/) はCoreOSが開発したオープンソースプロジェクトで、分散システムにおけるサービスディスカバリーや接続確立に広く使われています。EMQXの自動クラスタリングに最適です。

ネットワーク内にetcdサーバー（クラスター）をデプロイ後、EMQXはetcd経由で自動的にクラスターを作成できます。etcdのインストール・設定方法は[etcd Install](https://etcd.io/docs/latest/install/)をご覧ください。

etcdによる自動クラスタリングを有効にするには、`emqx.conf` の `cluster.etcd` 設定項目を使用します。

**例:**

```bash
cluster {
    discovery_strategy = etcd
    etcd {
        server = "http://127.0.0.1:2379"
        prefix = emqxcl
        node_ttl = 1m
    }
}
```

- `discovery_strategy` はノードディスカバリー戦略で `etcd` に設定
- `cluster.etcd.server` はetcdサーバーのアドレス。複数ノードはカンマ区切りで指定可能
- `cluster.etcd.prefix` はEMQXサービスディスカバリーに使うetcdのキー接頭辞
- `cluster.etcd.node_ttl` はetcdキーの有効期限（デフォルト1分）

設定完了後、EMQXノードを順に起動し、etcdctlツールでetcdサーバーの変化を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

結果はすべてのノードが正常に起動し自動的にクラスターに参加したことを示しています。

### Kubernetesによる自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/) はKubernetes環境でのEMQXクラスター作成と管理を支援し、デプロイや管理作業を低コストでラベル付け可能な繰り返し可能なジョブに変換することで大幅に簡素化します。

自身でEMQXをデプロイ・管理する場合でも、Kubernetes APIを使ったノードディスカバリーと自動クラスタリングが可能です。この機能を使うには、EMQX PodがKubernetes APIServerからendpointsリソース経由でクラスター情報を取得できるようRBACを作成する必要があります。設定方法は[Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)をご覧ください。

KubernetesでのEMQX自動クラスタリングを有効にするには、`emqx.conf` の `cluster.k8s` 設定項目を使用します。

```bash
cluster {
    discovery_strategy = k8s
    K8s {
        apiserver = "http://10.110.111.204:8080"
        service_name = emqx
        address_type = ip
        namespace = default
    }
}
```

- `discovery_strategy` はノードディスカバリー戦略で `k8s` に設定
- `cluster.K8s.apiserver` はKubernetes APIエンドポイントURL（デフォルト `http://10.110.111.204:8080`）
- `cluster.K8s.service_name` はEMQXサービス名（デフォルト `emqx`）
- `cluster.K8s.address_type` は検出ノード接続用アドレス種別（デフォルト `ip`、選択肢：`ip`、`dns`、`hostname`）
- [任意] `cluster.K8s.suffix` はノード名サフィックス。`address_type` が `dns` の場合のみ必要（デフォルト `pod.local`）
- `cluster.K8s.namespace` はKubernetesのネームスペース（文字列、デフォルト `default`）

設定後、ノードを順に起動するとクラスターが自動的に形成されます。

::: tip

Kubernetes上でEMQX自動クラスタリングを使用する場合、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の使用を推奨します。

:::

## クラスターの管理

クラスター作成後は、クラスター状態の監視やノード管理が可能です。

### クラスター状態の確認

任意のクラスター内ノードで以下のコマンドを実行し、クラスター状態を確認します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスターからの離脱

クラスターから離脱する方法は2つあります。

1. `cluster leave` コマンドを実行：現在のノードがクラスターから離脱します。クラスター内の他ノードに通知し、クラスター操作への参加を停止します。離脱前に進行中のタスクを完了します。
2. `cluster force-leave <node@host>` コマンドを実行：指定ノードをクラスターから強制的に削除します。通常、ノードが故障または応答しない場合に使用します。

例として、既存クラスターで `emqx@s2.emqx.io` が離脱したい場合、`emqx@s2.emqx.io` 上で以下を実行します。

```bash
./bin/emqx ctl cluster leave
```

または、`emqx@s1.emqx.io` 上で以下を実行し、`emqx@s2.emqx.io` をクラスターから削除します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間のネットワークプロトコルを設定できます。EMQXはTCPまたはTLSでノード間接続をサポートしています。接続方法は `emqx.conf` で設定します。

TCP IPv4およびTCP IPv6を使用する場合、`emqx.conf` の `cluster.proto_dist` を設定します。

- TCP IPv4: `inet_tcp` （デフォルト）
- TCP IPv6: `inet6_tcp`

SSLを有効にするには、まず `cluster.proto_dist` を `inet_tls` に設定し、`etc` フォルダの `ssl_dist.conf` ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)をご覧ください。

<!--ここに例コードが必要です-->

## 疑似分散クラスター

EMQXはテストや開発目的で疑似分散クラスター機能を提供しています。これは1台のマシン上で複数のEMQXインスタンスを起動し、それぞれをクラスターのノードとして設定する構成を指します。

1つ目のノードを起動します。

```bash
  EMQX_NODE__NAME='emqx1@127.0.0.1' \
  EMQX_LOG__FILE_HANDLERS__DEFAULT__FILE='log1/emqx.log' \
  EMQX_LISTENERS__TCP__DEFAULT__BIND='127.0.0.1:1883' \
  EMQX_LISTENERS__SSL__DEFAULT__BIND='127.0.0.1:8883' \
  EMQX_LISTENERS__WS__DEFAULT__BIND='127.0.0.1:8083' \
  EMQX_LISTENERS__WSS__DEFAULT__BIND='127.0.0.1:8084' \
  EMQX_DASHBOARD__LISTENERS__HTTP__BIND=18083 \
  EMQX_NODE__DATA_DIR="./data1" \
./bin/emqx start
```

次に、2つ目のノードを起動し手動でクラスターに参加させます。ポート競合を避けるため、各ノードで異なるリスニングポートを使用し、ログファイルや内部データベース用のディレクトリも分ける必要があります。

```bash
  EMQX_NODE__NAME='emqx2@127.0.0.1' \
  EMQX_LOG__FILE_HANDLERS__DEFAULT__FILE='log2/emqx.log' \
  EMQX_LISTENERS__TCP__DEFAULT__BIND='127.0.0.1:1882' \
  EMQX_LISTENERS__SSL__DEFAULT__BIND='127.0.0.1:8882' \
  EMQX_LISTENERS__WS__DEFAULT__BIND='127.0.0.1:8082' \
  EMQX_LISTENERS__WSS__DEFAULT__BIND='127.0.0.1:8085' \
  EMQX_DASHBOARD__LISTENERS__HTTP__BIND=18082 \
  EMQX_NODE__DATA_DIR="./data2" \
./bin/emqx start
  EMQX_NODE__NAME='emqx2@127.0.0.1' ./bin/emqx ctl cluster join 'emqx1@127.0.0.1'
```

上記は手動でクラスターを作成する例です。自動クラスタリングによる作成方法は[Auto Clustering](#auto-clustering)セクションを参照してください。

なお、この構成は本番環境での使用は推奨されません。
