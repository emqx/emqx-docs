# クラスターの作成と管理

EMQXクラスターは手動または自動で作成できます。本ページでは、手動クラスタリングと自動クラスタリングの両方の方法について紹介し、これら2つの異なるアプローチを用いたEMQXクラスターの作成と管理方法を案内します。

::: tip

クラスター機能は有効なライセンスキーがある場合にのみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識や動作については、[Cluster](./introduction.md) と [Architecture](../../develop/cluster/mria-introduction.md) をご参照ください。クラスターを作成するには、以下の概念にも慣れておく必要があります。

### ノード名

EMQXノードはノード名で識別されます。すべてのノードは `name@host` 形式の一意なノード名を持ち、`host` にはIPアドレスまたは完全修飾ドメイン名（FQDN）を指定します。例：

- サーバー `s1.emqx.io` にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` となります。
- このサーバーに静的IP（`192.168.0.10`）がある場合、ノード名は `emqx@192.168.0.10` となります。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更できません。そのため、EMQXノード名には静的なFQDNの使用を推奨します。
:::

### ノード検出

ノード検出はクラスター作成に必要なプロセスであり、個々のEMQXノードが互いに発見し通信できるようにします。場所やIPアドレスに関係なく通信が可能です。

### 手動クラスタリングと自動クラスタリング

ノード検出の戦略に基づき、クラスター作成方法は手動クラスタリングと自動クラスタリングに分けられます。

手動クラスタリングは、どのノードをクラスターに参加させるかを手動で指定してクラスターを作成する方法です。自動クラスタリングは、複数のEMQXノードが手動設定なしに自動でクラスターを形成できる方法です。自動クラスタリングはクラスター構築の手間を軽減し、ノードの動的な追加や削除を容易にします。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesなどに基づく自動クラスタリングをサポートしています。

以下の表は、EMQXがサポートするノード検出戦略とクラスター作成方法の一覧です。

| 戦略           | 説明                                                         |
| -------------- | ------------------------------------------------------------ |
| `manual`       | コマンドで手動でクラスターを作成                             |
| `static`       | 静的ノードリストによる自動クラスタリング                     |
| `dns`          | DNSのAレコードおよびSRVレコードによる自動クラスタリング      |
| `etcd`         | etcdによる自動クラスタリング                                 |
| `k8s`          | Kubernetesによる自動クラスタリング                           |
| `singleton`    | クラスタリング無効。ノードは他ノードとの接続試行をすべて拒否 |

EMQXは[Erlang/OTPアプリケーション向けに開発されたEkkaライブラリ](https://github.com/emqx/ekka)に基づく自動クラスター作成をサポートしています。EkkaはErlangノードの自動検出（Service Discovery）や自動クラスタリング（Autocluster）に加え、ネットワークパーティションの自動修復（Network Partition Autoheal）やダウンしたノードの自動削除（Autoclean）などの機能も実装しています。

クラスタリング方法は `emqx.conf` 設定ファイルでノード検出戦略を設定することで指定できます。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめる前に

このセクションでは、クラスター作成前にノードやネットワーク環境をどのように設定するかを案内します。

### ノード名の設定

クラスターに参加するノードの名前付け方法を理解しておく必要があります。例えば、`s1.emqx.io` と `s2.emqx.io` にそれぞれデプロイされた2ノードのクラスターを作成する場合、以下の手順に従います。

1つ目のノードの `emqx.conf` にノード名を設定します。

```bash
node.name = emqx@s1.emqx.io
```

ノード名は環境変数で上書きすることも可能です。例えば、`docker run` コマンドの `-e` オプションや systemd の `emqx.service` ファイルで以下のように定義します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

クラスターに参加させる他のノードも同様に設定してください。

これで `emqx@s1.emqx.io` と `emqx@s2.emqx.io` の2ノードがクラスターに参加する準備ができました。手動または自動でクラスターを作成できます。

### ノードクッキーの設定

セキュリティ上の理由から、クラスター参加ノードすべての `emqx.conf` でデフォルトのクッキー設定を秘密のクッキーに変更してください。クラスターに参加するすべてのノードは同じ秘密のクッキーを使用する必要があります。マジッククッキーの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)をご参照ください。

```
node {
  cookie = "<秘密のクッキー>"
}
```

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合は、内部クラスター通信に必要な以下のポートを開放してください。

- **4370**: Erlang分散トランスポートポート
- **5370**: クラスターRPCポート（物理マシン環境向け）
- **5369**: クラスターRPCポート（Docker環境向け）

1台のサーバーに複数のEMQXノードをデプロイする場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[クラスター内通信ポート](./security.md)をご参照ください。

## クイックスタート

このセクションでは、Dockerネットワーク上で2つの異なるクラスタリング方法を使ってクラスターを素早く作成する手順を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを実行しクラスターを形成する場合、追加の設定が必要です。コンテナ内のクラスター通信ポートのマッピングとファイアウォールでのポート開放については[ネットワーク環境の設定](#configure-network-environment)を参照してください。

:::

:::: tabs type:card

::: tab 手動クラスタリングの例

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 1つ目のノードを起動し、環境変数でノード名を設定します。EMQXのデフォルトクラスタリング方法は手動なので追加設定は不要です。ノードをDockerネットワークに参加させ、ノードホスト名に一致するネットワークエイリアスを設定します。

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
   
4. いずれかのノード上で以下のコマンドを実行し、現在のノードを他のノードに接続してクラスターを作成します。コマンドの詳細は[手動クラスタリング](#manual-clustering)を参照してください。

   ```bash
   docker exec -it emqx2 \
       emqx ctl cluster join emqx@node1.emqx.com
   ```

:::

::: tab 自動クラスタリングの例（静的ノードリスト方式）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 1つ目のノードを起動し、環境変数でノード名とクラスタリング方法を設定します。

   - `EMQX_NODE_NAME` 環境変数でノード名を設定
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY` 環境変数でクラスタ検出戦略を設定（ここでは[静的クラスタリング](#autocluster-by-static-node-list)を使用）
   - `EMQX_CLUSTER__STATIC__SEEDS` 環境変数で静的ノードリストを設定（すべてのノード名を含む必要があります）

   また、ノードをDockerネットワークに参加させ、ノードホスト名に一致するネットワークエイリアスを設定します。

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
   
3. 1つ目のノード起動後、2つ目のノードを起動します。クラスタリング方法は同じにし、新しいノードも1つ目のノードと同じネットワークに参加させます。1つ目のノードが既に1883などのポートを占有しているため、ここではポートマッピングは行いません。

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

いずれかのノード上で `emqx ctl cluster status` コマンドを実行し、クラスター状態を確認します。正常にクラスターが構築されていれば、以下のような情報が出力されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの指示に従ってクラスター作成方法を選択し、設定やデプロイを行えます。

## 手動クラスタリング

このセクションでは手動でクラスターを作成する手順を説明します。手動クラスタリングでは、クラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も構築する必要があります。自動クラスタリングに比べて、カスタムネットワークトポロジーの細かい調整が可能であり、自動クラスタリングが利用できないまたは適さない場合に適しています。

:::tip 

手動クラスタリングはコアノードのみで使用可能です。コア-レプリカノード構成の場合は自動クラスタリングを利用してください。

:::

例えば、`emqx@node1.emqx.com` と `emqx@node2.emqx.com` の2ノードがある場合、以下の手順で手動クラスタリングを行います。

1. クラスター検出戦略を `manual` に設定します。

   ```bash
   cluster {
       ## Options: manual | static | dns | etcd | k8s | singleton
       discovery_strategy  =  manual
   }
   ```

2. 2ノードを起動後、いずれかのノードでクラスター参加コマンドを実行します。

   ```bash
   $ ./bin/emqx ctl cluster join emqx@node1.emqx.com
   
   Join the cluster successfully.
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

   :::tip

   - このコマンドはクラスターに参加するノード上で実行する必要があります。つまり、**招待する側ではなく参加を要求する側で実行します**。
   - `emqx@s2.emqx.io` が `emqx@s1.emqx.io` に参加すると、ローカルデータはクリアされ、`emqx@s1.emqx.io` のデータに同期されます。
   - `emqx@s2.emqx.io` が別のクラスターに参加したい場合は、まず現在のクラスターから離脱する必要があります。離脱方法は [クラスターからの離脱](#leave-cluster) を参照してください。

   :::

3. いずれかのノードでクラスター状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status
   
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

これで2ノードのクラスター作成に成功しました。クラスター状態の監視やノード管理については、[クラスター状態の照会](#query-cluster-status)、[クラスターのノード管理](#manage-cluster-nodes)、[ネットワークプロトコルの設定](#configure-network-protocols)をご参照ください。

EMQX v5.9.0以降は、EMQXダッシュボードからノードの招待や管理も可能です。詳細は[Cluster](../dashboard/cluster_settings.md#cluster)を参照してください。

## 自動クラスタリング

このセクションでは、各種自動クラスタリング方法によるクラスター作成方法を説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードにあらかじめ定義された静的ノードリストを設定し、起動後にノードリストに基づいて自動的にクラスターを形成する方式です。

静的クラスタリングは、他のネットワークコンポーネントやサービスに依存せず、最も簡単にEMQXクラスターを自動作成できる方法です。各ノードがTCPプロトコルで相互通信できればクラスターを形成可能です。

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

<!--v5.0.23 e5.0.4 以前は ["emqx1", "emqx2"] のみ対応
v5.0.23e5.0.4 以降は両方対応-->

- `discovery_strategy` はノード検出戦略で `static` に設定
- `seeds` は配列で、クラスターに参加するノード名を複数カンマ区切りで指定可能

すべてのノードを起動すると、自動的にクラスターが形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)（Domain Name System）は、ドメイン名の問い合わせに対して対応するIPアドレス（Aレコード）を返す仕組みです。1つのドメイン名に複数のAレコード（複数IPアドレス）を設定でき、1つの名前に複数のIPを対応させることができます。EMQXのDNS自動クラスタリングはこの1対多のマッピングを利用し、クラスター内のすべてのノードを特定し、各ノードがクラスターに参加できるようにします。

#### DNSサービスの設定

多くのパブリッククラウドサービスはDNSサービスを提供しています。ドメイン名を割り当てた後、各EMQXノードのIPアドレスをこのドメインのAレコードに追加するだけで設定完了です。プライベートクラウドや内部ネットワークにEMQXをデプロイする場合は、[BIND](https://www.isc.org/bind/)などのソフトウェアで独自のDNSシステムを構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf` の `cluster.dns` 設定項目でクラスターに参加するすべてのノードを指定します。

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

- `discovery_strategy` はノード検出戦略で `dns` に設定
- `cluster.dns.name` は文字列でドメイン名を指定
- `cluster.dns.record_type` は列挙型で `a` または `srv` を指定可能

すべてのノードを起動すると、自動的にクラスターが形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/) はCoreOSが開発したオープンソースプロジェクトで、分散システムにおけるサービス検出や接続確立に広く利用されています。EMQXの自動クラスタリングに適しています。

ネットワーク内にetcdサーバー（クラスター）を構築すれば、EMQXはetcd経由で自動的にクラスターを作成できます。etcdのインストールや設定方法は[etcd Install](https://etcd.io/docs/latest/install/)を参照してください。

etcdによる自動クラスタリングを有効にするには、`emqx.conf` の `cluster.etcd` 設定項目を使用します。

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

- `discovery_strategy` はノード検出戦略で `etcd` に設定
- `cluster.etcd.server` はetcdサーバーのアドレス。複数ノードはカンマ区切りで指定可能
- `cluster.etcd.prefix` はEMQXサービス検出に使うetcdのキーのプレフィックス
- `cluster.etcd.node_ttl` はetcdキーの有効期限（デフォルトは `1m`）

設定完了後、EMQXノードを順次起動し、etcdctlツールでetcdサーバーの変化を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

この結果は、すべてのノードが正常に起動し自動的にクラスターに参加したことを示しています。

### Kubernetes上での自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/) を使えば、Kubernetes環境でEMQXクラスターを素早く作成・管理でき、デプロイや管理作業を低コストでラベル付け可能な繰り返し作業に変換できます。

自分でEMQXをKubernetes上にデプロイ・管理する場合でも、Kubernetes APIを使ったノード検出と自動クラスタリングが可能です。この機能を使うには、まずEMQX Pod用にRBACを作成し、Kubernetes APIサーバーのendpointsリソースからクラスターのノード情報を取得できるようにします。設定方法は[Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)を参照してください。

Kubernetes上のEMQX自動クラスタリングは、`emqx.conf` の `cluster.k8s` 設定項目で設定します。

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

- `discovery_strategy` はノード検出戦略で `k8s` に設定
- `cluster.K8s.apiserver` はKubernetes APIエンドポイントURL（デフォルトは `http://10.110.111.204:8080`）
- `cluster.K8s.service_name` はEMQXサービス名（デフォルトは `emqx`）
- `cluster.K8s.address_type` は検出したノードに接続するためのアドレスタイプ（デフォルトは `ip`、オプションは `ip`、`dns`、`hostname`）
- [任意] `cluster.K8s.suffix` はノード名のサフィックス。`cluster.K8s.address_type` が `dns` の場合のみ必要（デフォルトは `pod.local`）
- `cluster.K8s.namespace` はKubernetesのネームスペース（文字列、デフォルトは `default`）

設定後、ノードを順次起動すると自動的にクラスターが形成されます。

::: tip

Kubernetes上のEMQX自動クラスタリングでは、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の使用を推奨します。

:::

## クラスターの管理

クラスター作成後は、クラスター状態の監視やノード管理が可能です。

### クラスター状態の照会

任意のクラスター内ノードで以下のコマンドを実行し、クラスター状態を照会します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスターからの離脱

クラスターから離脱する方法は2通りあります。

1. `cluster leave` コマンドを実行する方法：現在のノードがクラスターから離脱します。他のノードに通知し、クラスター操作への参加を停止します。離脱前に進行中のタスクを完了します。
2. `cluster force-leave <node@host>` コマンドを実行する方法：指定したノードをクラスターから強制的に削除します。通常、ノードが故障したり応答しなくなった場合に使用します。

例えば、先ほど作成したクラスターで `emqx@s2.emqx.io` が離脱したい場合、`emqx@s2.emqx.io` 上で以下を実行します。

```bash
./bin/emqx ctl cluster leave
```

または、`emqx@s1.emqx.io` 上で以下を実行して `emqx@s2.emqx.io` をクラスターから削除します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間の接続に使用するネットワークプロトコルを設定できます。EMQXはTCPまたはTLSによるノード接続をサポートしています。接続方法は `emqx.conf` で設定可能です。

TCP IPv4およびTCP IPv6を使用する場合は、`emqx.conf` の `cluster.proto_dist` を設定します。

- TCP IPv4: `inet_tcp` （デフォルト）
- TCP IPv6: `inet6_tcp`

SSLを有効にするには、まず `cluster.proto_dist` を `inet_tls` に設定し、`etc` フォルダ内の `ssl_dist.conf` ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)を参照してください。

<!-- ここに設定例が必要です -->
