# クラスターの作成と管理

EMQXクラスターは手動または自動で作成できます。本ページでは、手動および自動クラスタリングの方法を紹介し、これら2つの異なるアプローチを用いたEMQXクラスターの作成と管理についてご案内します。

::: tip 注意

クラスター機能は有効なライセンスキーがある場合にのみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識と動作については、[Cluster](./introduction.md)および[Architecture](./mria-introduction.md)をご参照ください。クラスターを作成するには、以下の概念も理解しておく必要があります。

### ノード名

EMQXのノードはノード名で識別されます。すべてのノードは `name@host` 形式の一意のノード名が設定されており、`host` はIPアドレスまたは完全修飾ドメイン名（FQDN）である必要があります。例：

- サーバー `s1.emqx.io` にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` とします。
- このサーバーが固定IP（`192.168.0.10`）を持つ場合、ノード名は `emqx@192.168.0.10` とします。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更できません。そのため、EMQXノード名には静的なFQDNの使用を推奨します。
:::

### ノードディスカバリー

ノードディスカバリーはクラスター作成に必須のプロセスで、個々のEMQXノードがお互いを発見し、IPアドレスや場所に関係なく通信できるようにします。

### 手動クラスタリングと自動クラスタリング

ノードディスカバリーの戦略に基づき、クラスター作成方法は手動クラスタリングと自動クラスタリングに分けられます。

手動クラスタリングは、クラスターに参加するノードを手動で指定してEMQXクラスターを作成する方法です。自動クラスタリングは、複数のEMQXノードが手動設定なしに自動的にクラスターを形成できる方法で、クラスターの設定を簡素化し、動的にノードの追加・削除が容易になります。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesなどに基づく自動クラスタリングをサポートしています。

以下の表は、EMQXがサポートするノードディスカバリー戦略とクラスター作成方法の一覧です。

| 戦略          | 説明                                                         |
| ------------- | ------------------------------------------------------------ |
| `manual`      | コマンドで手動によりクラスターを作成                         |
| `static`      | 静的ノードリストによる自動クラスタリング                     |
| `dns`         | DNSのAレコードおよびSRVレコードによる自動クラスタリング     |
| `etcd`        | etcdによる自動クラスタリング                                 |
| `k8s`         | Kubernetesによる自動クラスタリング                           |
| `singleton`   | クラスタリング無効。ノードは他ノードとの接続をすべて拒否     |

EMQXは[Erlang/OTPアプリケーション向けに開発されたEkkaライブラリ](https://github.com/emqx/ekka)に基づく自動クラスター作成をサポートしています。EkkaはErlangノードの自動発見（サービスディスカバリー）や自動クラスタリング（Autocluster）だけでなく、ネットワークパーティションの自動修復（Network Partition Autoheal）や停止したノードの自動削除（Autoclean）などの機能も実装しています。

クラスタリング方式は `emqx.conf` 設定ファイルのノードディスカバリー戦略で定義できます。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめる前に

クラスター作成前にノードやネットワーク環境の設定方法について説明します。

### ノード名の設定

クラスターに参加するノードの名前付け方法を理解しておく必要があります。例えば、`s1.emqx.io` と `s2.emqx.io` にそれぞれ2つのノードをデプロイしクラスターを作成する場合、以下の手順に従います。

1つ目のノードの `emqx.conf` にノード名を設定します。

```bash
node.name = emqx@s1.emqx.io
```

ノード名は環境変数で上書き可能です。例えば、`docker run` コマンドの `-e` オプションや systemd の `emqx.service` ファイルで以下のように設定します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

クラスターに参加する他のノードも同様に設定してください。

これで `emqx@s1.emqx.io` と `emqx@s2.emqx.io` の2つのノード名が用意できました。手動または自動でクラスターを作成できます。

### ノードクッキーの設定

セキュリティ上の理由から、クラスターに参加する全ノードの `emqx.conf` でデフォルトのクッキー設定をSecretクッキーに変更してください。クラスターに参加する全ノードは同じSecretクッキーを使用する必要があります。マジッククッキーの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)を参照してください。

```
node {
  cookie = "<Secretクッキー>"
}
```

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合は、内部クラスター通信に必要な以下のポートを開放してください。

- **4370**: Erlang分散トランスポートポート
- **5370**: クラスターRPCポート（物理マシン環境向け）
- **5369**: クラスターRPCポート（Docker環境向け）

1台のサーバーに複数のEMQXノードをデプロイする場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[Intra-cluster communication port](./security.md)を参照してください。

## クイックスタート

このセクションでは、Dockerネットワーク上で2つの異なるクラスタリング方法を用いてクラスターを迅速に作成する方法を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを実行しクラスターを形成する場合は、追加の設定が必要です。コンテナ内で必要なクラスター通信ポートをマッピングし、ファイアウォールでポートを開放してください。詳細は[Configure Network Environment](#configure-network-environment)を参照してください。

:::

:::: tabs type:card

::: tab 手動クラスタリング（コマンド）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 1つ目のノードを起動し、環境変数でノード名を設定します。EMQXのデフォルトクラスタリング方式は手動なので追加設定は不要です。ノードをDockerネットワークに参加させ、ノードホストに対応するネットワークエイリアスを設定します。

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
   
3. 1つ目のノード起動後、2つ目のノードを起動します。新ノードは1つ目のノードと同じネットワークに参加する必要があります。1つ目のノードが1883などのポートを占有しているため、ここではポートマッピングは行いません。

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       --network emqx-net \
       --network-alias node2.emqx.com \
       emqx/emqx-enterprise:@EE_VERSION@
   ```
   
4. 任意のノード上で以下のコマンドを実行し、現在のノードを他のノードに接続してクラスターを作成します。コマンドの詳細は[Manual Clustering](#manual-clustering)を参照してください。

   ```bash
   docker exec -it emqx2 \
       emqx ctl cluster join emqx@node1.emqx.com
   ```

:::

::: tab 手動クラスタリング（ダッシュボード）

EMQX v5.9.0以降、ダッシュボードから直接クラスターを作成できます。

1. すべてのノードが起動し、適切な `name@host`、同一のクッキー設定、ネットワーク経由で相互接続可能であることを確認します。

2. 任意のノードの**ダッシュボード**にアクセスします。

3. **管理 > MQTT設定 > クラスター** に移動します。

4. （任意）**クラスター説明**欄にクラスターの用途や環境を識別する説明を入力し、**保存**をクリックします。

   > この機能はEMQX v6.0.0以降で利用可能です。

5. **招待**をクリックし、ノード名（例：`emqx@node2.emqx.com`）を入力して確定します。

6. 招待されたノードは状態を同期後、クラスターに参加します。

詳細はダッシュボードの[Cluster Settings](../../dashboard/cluster_settings.md#cluster)を参照してください。

:::

::: tab 自動クラスタリング（静的ノードリスト方式）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 1つ目のノードを起動し、環境変数でノード名とクラスタリング方式を設定します。

   - `EMQX_NODE_NAME` でノード名を設定
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY` でクラスターディスカバリー戦略を設定（ここでは静的クラスタリング）
   - `EMQX_CLUSTER__STATIC__SEEDS` で静的ノードリストを設定（すべてのノード名を含める）

   また、ノードをDockerネットワークに参加させ、ノードホストに対応するネットワークエイリアスを設定します。

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
   
3. 1つ目のノード起動後、2つ目のノードを起動します。クラスタリング方式とノードは1つ目のノードと同じネットワークに参加する必要があります。1つ目のノードが1883などのポートを占有しているため、ここではポートマッピングは行いません。

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

任意のノード上で `emqx ctl cluster status` コマンドを実行し、クラスター状態を確認できます。正常な場合、以下のような情報が出力されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの手順に従い、ご自身のニーズに合ったクラスター作成方法を選択して設定・展開してください。

## 手動クラスタリング

このセクションでは、手動でクラスターを作成する手順を説明します。手動クラスタリングでは、クラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も構築する必要があります。自動クラスタリングに比べて、カスタムネットワークトポロジーの細かい調整が可能であり、自動クラスタリングが利用できないまたは適さない場合に適しています。

:::tip 

手動クラスタリングはコアノードにのみ使用可能です。コア-レプリカノードのデプロイ構成を使用している場合は、自動クラスタリングでクラスターを管理してください。

:::

例として、`emqx@node1.emqx.com` と `emqx@node2.emqx.com` の2ノードでクラスターを手動作成する手順は以下の通りです。

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

   - このコマンドはクラスターに参加するノード上で実行する必要があります。つまり、**招待する側ではなく参加を要求する側**で実行します。
   - `emqx@s2.emqx.io` が `emqx@s1.emqx.io` に参加すると、ローカルデータはクリアされ、`emqx@s1.emqx.io` のデータと同期されます。
   - `emqx@s2.emqx.io` が別のクラスターに参加したい場合は、まず現在のクラスターを離脱する必要があります。離脱方法は[Leave Cluster](#leave-cluster)を参照してください。

   :::

3. 任意のノードでクラスター状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status
   
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

2ノードでクラスター作成に成功しました。クラスター状態の監視方法やノード管理については、[Query Cluster Status](#query-cluster-status)、[Manage Cluster Nodes](#manage-cluster-nodes)、[Configure Network Protocols](#configure-network-protocols)を参照してください。

EMQX v5.9.0以降はダッシュボードからノードの招待や管理も可能で、より直感的かつ使いやすくなっています。詳細は[Cluster Settings](../../dashboard/cluster_settings.md#cluster)を参照してください。

## 自動クラスタリング

このセクションでは、さまざまな自動クラスタリング方式によるクラスター作成方法を説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードに事前定義された静的ノードリストを設定し、起動後にノードリストに従って自動的にクラスターを形成する方式です。

静的クラスタリングは、他のネットワークコンポーネントやサービスに依存せず、TCPプロトコルで相互通信できれば簡単にEMQXクラスターを自動作成できます。

この機能を有効にするには、`emqx.conf` にクラスター方式とノードリストを設定します。

**例：**

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@s1.emqx.io", "emqx@s2.emqx.io"]
    }
}
```

<!--v5.0.23 e5.0.4 以前は ["emqx1", "emqx2"] のみサポート
v5.0.23以降は両方サポート-->

- `discovery_strategy` はノードディスカバリー戦略で、`static` に設定
- `seeds` は配列で、クラスターに参加するノード名を複数カンマ区切りで指定

すべてのノードを起動すると、自動的にクラスターが形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)（Domain Name System）は、ドメイン名の問い合わせに対して対応するIPアドレス（Aレコード）を返す仕組みです。DNSは1つのドメイン名に複数のAレコード（複数IPアドレス）を持つことができ、1つの名前に対して複数のIPアドレスを対応させることが可能です。EMQXのDNS自動クラスタリングはこの1対多のマッピングを利用し、クラスター内のすべてのノードを特定し、各ノードがクラスターに参加できるようにします。

#### DNSサービスの設定

ほとんどのパブリッククラウドサービスはDNSサービスを提供しています。ドメイン名を割り当てた後、各EMQXノードのIPアドレスをこのドメインのAレコードに追加するだけで設定が完了します。プライベートクラウドや内部ネットワークにデプロイする場合は、[BIND](https://www.isc.org/bind/)などのDNSソフトウェアで独自のDNSシステムを構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf` の `cluster.dns` 設定項目にクラスター参加ノードを追加します。

**例：**

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

- `discovery_strategy` はノードディスカバリー戦略で、`dns` に設定
- `cluster.dns.name` は問い合わせるDNS名／ドメイン名（例：`localhost`）
- `cluster.dns.record_type` は列挙型で、`a` または `srv` のいずれかを指定

すべてのノードを起動すると、自動的にクラスターが形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/) はCoreOSが開発したオープンソースプロジェクトで、分散システムにおけるサービスディスカバリーや接続確立に広く使われています。これはEMQXの自動クラスタリングに最適です。

ネットワーク内にetcdサーバー（クラスター）を展開すると、EMQXはetcdを介して自動的にクラスターを作成できます。etcdのインストール・設定方法は[etcd Install](https://etcd.io/docs/latest/install/)を参照してください。

etcdを使った自動クラスタリングを有効にするには、`emqx.conf` の `cluster.etcd` 設定項目を使用します。

**例：**

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

- `discovery_strategy` はノードディスカバリー戦略で、`etcd` に設定
- `cluster.etcd.server` はetcdサーバーのアドレス。複数ノードはカンマ区切りで指定可能
- `cluster.etcd.prefix` はEMQXサービスディスカバリー用のetcdキーのプレフィックス
- `cluster.etcd.node_ttl` はetcdキーの有効期限（デフォルト：`1m`）

設定完了後、EMQXノードを順次起動し、etcdctlツールでetcdサーバーの状態を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

結果はすべてのノードが正常に起動し、自動的にクラスターに参加していることを示しています。

### Kubernetes上での自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/) を使うと、Kubernetes環境でEMQXクラスターの作成・管理が簡単になり、デプロイや管理作業をラベル付きで繰り返し可能な低コストジョブに変換できます。

自分でEMQXをデプロイ・管理する場合でも、Kubernetes APIを用いたノードディスカバリーと自動クラスタリングが可能です。この機能を使うには、EMQX PodがKubernetes APIServerのendpointsリソースからクラスター情報を取得できるようにRBACを作成する必要があります。設定方法は[Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)を参照してください。

Kubernetes上でのEMQX自動クラスタリングを有効にするには、`emqx.conf` の `cluster.k8s` 設定項目を使用します。

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

- `discovery_strategy` はノードディスカバリー戦略で、`k8s` に設定
- `cluster.K8s.apiserver` はKubernetes APIエンドポイントURL（デフォルト：`http://10.110.111.204:8080`）
- `cluster.K8s.service_name` はEMQXサービス名（デフォルト：`emqx`）
- `cluster.K8s.address_type` は発見したノードへの接続アドレス種別（デフォルト：`ip`）。選択肢は `ip`、`dns`、`hostname`
- （オプション）`cluster.K8s.suffix` はノード名のサフィックス。`address_type` が `dns` の場合のみ必要（デフォルト：`pod.local`）
- `cluster.K8s.namespace` はKubernetesのネームスペース（文字列、デフォルト：`default`）

設定後、ノードを順次起動すると自動的にクラスターが形成されます。

::: tip

Kubernetes上でEMQX自動クラスタリングを利用する場合、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の使用を推奨します。

:::

## クラスター管理

クラスター作成後は、クラスター状態の監視やノード管理が可能です。

### クラスター状態の確認

任意のクラスター内ノードで以下のコマンドを実行し、クラスター状態を確認します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスターからの離脱

クラスターから離脱する方法は2通りあります。

1. `cluster leave` コマンドを実行：現在のノードがクラスターから離脱します。クラスター内の他ノードに通知し、クラスター操作への参加を停止します。離脱前に進行中のタスクを完了します。
2. `cluster force-leave <node@host>` コマンドを実行：指定ノードをクラスターから強制的に削除します。通常、ノードが故障または応答不能になった場合に使用します。

例として、既存クラスターで `emqx@s2.emqx.io` が離脱する場合、`emqx@s2.emqx.io` 上で以下を実行します。

```bash
./bin/emqx ctl cluster leave
```

または、`emqx@s1.emqx.io` 上で以下を実行し、`emqx@s2.emqx.io` をクラスターから削除します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間のネットワークプロトコルを設定できます。EMQXはTCPまたはTLSによるノード接続をサポートしており、`emqx.conf` で設定可能です。

TCP IPv4およびTCP IPv6を使用する場合は、`emqx.conf` の `cluster.proto_dist` を設定します。

- TCP IPv4: `inet_tcp`（デフォルト）
- TCP IPv6: `inet6_tcp`

SSLを有効にするには、まず `cluster.proto_dist` を `inet_tls` に設定し、`etc` フォルダ内の `ssl_dist.conf` ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)を参照してください。

<!-- ここに例コードを追加すると良い -->

## 疑似分散クラスター

EMQXはテストや開発用途向けに疑似分散クラスター機能を提供しています。これは1台のマシン上で複数のEMQXインスタンスを起動し、それぞれをクラスターのノードとして設定する構成です。

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

次に、2つ目のノードを起動し手動でクラスターに参加させます。ポート競合を避けるため、ノードごとに異なるリスニングポートを使用し、ログファイルや内部データベース用のディレクトリも分ける必要があります。

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

上記は手動でクラスターを作成する例です。自動クラスタリングの方法は[Auto Clustering](#auto-clustering)セクションを参照してください。

この構成は本番環境での使用は推奨されません。
