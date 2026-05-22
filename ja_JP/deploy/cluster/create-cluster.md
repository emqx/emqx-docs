# クラスターの作成と管理

EMQXクラスターは手動または自動のいずれかの方法で作成できます。本ページでは、手動クラスタリングと自動クラスタリングの両方の方法を紹介し、これら2つの異なるアプローチを用いたEMQXクラスターの作成および管理方法を案内します。

::: tip 注意

クラスターモードは有効なライセンスキーがある場合にのみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識や動作については、[クラスター](./introduction.md)および[アーキテクチャ](./mria-introduction.md)をご覧ください。クラスターを作成するには、以下の概念にも精通しておく必要があります。

### ノード名

EMQXのノードは名前によって識別されます。すべてのノードは `name@host` という形式の一意のノード名が設定されており、hostにはIPアドレスまたは完全修飾ドメイン名（FQDN）を指定する必要があります。例：

- サーバー`s1.emqx.io`にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` となります。
- このサーバーが固定IP（`192.168.0.10`）を持つ場合、ノード名は `emqx@192.168.0.10` となります。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更できません。そのため、EMQXノード名には静的なFQDNの使用を推奨します。
:::

### ノードディスカバリー

ノードディスカバリーはクラスター作成に必須のプロセスであり、個々のEMQXノードがお互いを発見し、場所やIPアドレスに関係なく通信できるようにします。

### 手動クラスタリングと自動クラスタリング

ノードディスカバリーストラテジーに基づき、クラスター作成方法は手動クラスタリングと自動クラスタリングに分けられます。

手動クラスタリングは、どのノードをクラスターに含めるかを手動で指定してEMQXクラスターを作成する方法です。自動クラスタリングは、複数のEMQXノードが手動設定なしに自動的にクラスターを形成する方法で、クラスターの設定を簡素化し、動的にノードの追加・削除を容易にします。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesなどに基づく自動クラスタリングをサポートしています。

以下の表は、EMQXがサポートするノードディスカバリーストラテジーとクラスター作成方法を示しています。

| ストラテジー    | 説明                                                        |
| --------------- | ----------------------------------------------------------- |
| `manual`        | コマンドで手動によりクラスターを作成                        |
| `static`        | 静的ノードリストによる自動クラスタリング                   |
| `dns`           | DNSのAレコードおよびSRVレコードによる自動クラスタリング    |
| `etcd`          | etcdによる自動クラスタリング                                |
| `k8s`           | Kubernetesによる自動クラスタリング                          |
| `singleton`     | クラスタリング無効。ノードは他ノードとの接続をすべて拒否。  |

EMQXは[Erlang/OTPアプリケーション向けに開発されたEkkaライブラリ](https://github.com/emqx/ekka)に基づく自動クラスター作成をサポートしています。EkkaはErlangノードの自動発見（サービスディスカバリー）や自動クラスタリング（Autocluster）だけでなく、ネットワークパーティションの自動修復（Network Partition Autoheal）やダウンしたノードの自動削除（Autoclean）などの機能も実装しています。

クラスターの作成方法は、`emqx.conf`設定ファイルのノードディスカバリーストラテジーを設定することで定義できます。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめる前に

このセクションでは、クラスター作成前にノードおよびネットワーク環境をどのように設定するかについて説明します。

### ノード名の設定

クラスターを作成する前に、クラスターに参加するノードの名前付け方法を理解しておく必要があります。例えば、`s1.emqx.io`と`s2.emqx.io`にそれぞれデプロイされた2つのノードのクラスターを作成する場合、以下の手順に従います。

1つ目のノードの`emqx.conf`設定ファイルにノード名を設定します。例：

```bash
node.name = emqx@s1.emqx.io
```

ノード名は環境変数で上書きすることも可能です。例えば、`docker run`コマンドの`-e`オプションやsystemdの`emqx.service`ファイルで以下のように定義します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

クラスターに参加する他のノードでも同様の手順を繰り返します。

これでクラスターに参加する2つのノード、`emqx@s1.emqx.io`と`emqx@s2.emqx.io`の名前付けが完了しました。手動または自動のいずれかの方法でクラスターを作成できます。

### ノードクッキーの設定

セキュリティ上の理由から、クラスターに参加するすべてのノードの`emqx.conf`でデフォルトのクッキー設定をSecretクッキーに変更してください。クラスターに参加するすべてのノードは同じSecretクッキーを使用する必要があります。使用されるマジッククッキーの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)をご参照ください。

```
node {
  cookie = "<Secretクッキー>"
}
```

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合は、内部クラスター通信に必要な以下のポートを開放してください。

- **4370**：Erlang分散トランスポートポート
- **5370**：クラスターRPCポート（物理マシン環境向け）
- **5369**：クラスターRPCポート（Docker環境向け）

1台のサーバーに複数のEMQXノードをデプロイする場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[クラスター内通信ポート](./security.md)をご覧ください。

## クイックスタート

このセクションでは、Dockerネットワーク上で2つの異なるクラスタリング方法を使ってクラスターを素早く作成する方法を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを実行しクラスターを形成する場合は、追加の設定が必要です。コンテナ内で必要なクラスター通信ポートをマッピングし、ファイアウォールでこれらのポートが開放されていることを[ネットワーク環境の設定](#configure-network-environment)で確認してください。

:::

:::: tabs type:card

::: tab 手動クラスタリング（コマンド）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互にアクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 最初のノードを起動し、環境変数でノード名を設定します。EMQXのデフォルトクラスタリング方法は手動なので追加設定は不要です。ノードをDockerネットワークに参加させ、ノードホストに対応するネットワークエイリアスを設定します。

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
   
3. 最初のノード起動後、2つ目のノードを起動します。新しいノードは最初のノードと同じネットワークに参加する必要があります。最初のノードが1883などのポートを既に占有しているため、ここではポートマッピングは行いません。

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

::: tab 手動クラスタリング（ダッシュボード）

EMQX v5.9.0以降、ダッシュボードから直接クラスターを作成できます。

1. すべてのノードが起動しており、適切な`name@host`、同一のクッキー、相互に通信可能であることを確認します。

2. 任意のノードの**ダッシュボード**にアクセスします。

3. **管理 > MQTT設定 > クラスター**に移動します。

4. （任意）**クラスター説明**欄にクラスターの目的や環境を識別する説明を入力し、**保存**をクリックします。

   > この機能はEMQX v6.0.0以降で利用可能です。

5. **招待**をクリックし、ノード名（例：`emqx@node2.emqx.com`）を入力して確定します。

6. 招待されたノードは状態を同期後、クラスターに参加します。

詳細はダッシュボードの[クラスター設定](../../dashboard/cluster_settings.md#cluster)をご覧ください。

:::

::: tab 自動クラスタリング（静的ノードリスト）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互にアクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 最初のノードを起動し、環境変数でノード名とクラスタリング方法を設定します。

   - `EMQX_NODE_NAME`環境変数でノード名を設定します。
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY`環境変数でクラスターディスカバリーストラテジーを設定します。ここでは[静的クラスタリング](#autocluster-by-static-node-list)を使用します。
   - `EMQX_CLUSTER__STATIC__SEEDS`環境変数で静的ノードリストを設定します。すべてのノード名を含める必要があります。

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
   
3. 最初のノード起動後、2つ目のノードを起動します。クラスタリング方法は同じで、新しいノードも最初のノードと同じネットワークに参加する必要があります。最初のノードが1883などのポートを既に占有しているため、ここではポートマッピングは行いません。

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

任意のノード上で`emqx ctl cluster status`コマンドを実行し、クラスター状態を確認します。クラスター状態が正常な場合、以下のような情報が出力されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの手順に従い、必要なクラスター作成方法を選択して設定・デプロイを行えます。

## 手動クラスタリング

このセクションでは、手動でクラスターを作成する手順を説明します。手動クラスタリングでは、クラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も構築する必要があります。自動クラスタリングと比較して、カスタムネットワークトポロジーの細かな調整が可能であり、自動クラスタリングが利用できないまたは不適切な状況に適しています。

:::tip 

手動クラスタリングはコアノードにのみ使用可能です。コア-レプリカノードのデプロイアーキテクチャを使用している場合は、自動クラスタリングを利用してクラスターを管理してください。

:::

例として、`emqx@node1.emqx.com`と`emqx@node2.emqx.com`の2つのノードがある場合、以下の手順で手動クラスターを作成できます。

1. クラスターのディスカバリーストラテジーを`manual`に設定します。

   ```bash
   cluster {
       ## Options: manual | static | dns | etcd | k8s | singleton
       discovery_strategy  =  manual
   }
   ```

2. 2つのノードを起動後、いずれかのノード上でクラスター参加コマンドを実行します。

   ```bash
   $ ./bin/emqx ctl cluster join emqx@node1.emqx.com
   
   Join the cluster successfully.
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

   :::tip

   - このコマンドはクラスターに参加するノード上で実行する必要があります。つまり、**招待**ではなく**参加要求**です。
   - `emqx@s2.emqx.io`が`emqx@s1.emqx.io`に参加してクラスターを形成すると、ローカルデータはクリアされ、`emqx@s1.emqx.io`のデータに同期されます。
   - `emqx@s2.emqx.io`が別のクラスターに参加したい場合、まず現在のクラスターから離脱する必要があります。離脱方法は[クラスターからの離脱](#leave-cluster)を参照してください。

   :::

3. 任意のノード上でクラスター状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status
   
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

2ノードでクラスター作成に成功したので、[クラスター状態の確認](#query-cluster-status)、[クラスターノードの管理](#manage-cluster-nodes)、[ネットワークプロトコルの設定](#configure-network-protocols)のセクションを参照し、クラスター状態の監視や管理方法を学んでください。

EMQX v5.9.0以降はダッシュボードからノードの招待や管理も可能で、より直感的で使いやすい操作ができます。詳細は[クラスター設定](../../dashboard/cluster_settings.md#cluster)をご覧ください。

## 自動クラスタリング

このセクションでは、さまざまな自動クラスタリング方法によるクラスターの自動作成方法を説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードに事前定義された静的ノードリストを設定し、起動後にノードリストに従って自動的にクラスターを形成する方法です。

静的クラスタリングは、他のネットワークコンポーネントやサービスに依存せず、TCPプロトコルで相互通信できる限り最も簡単にEMQXクラスターを自動作成できる方法です。

この機能を有効にするには、`emqx.conf`でクラスターモードとノードリストを設定します。

**設定例：**

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@s1.emqx.io", "emqx@s2.emqx.io"]
    }
}
```

<!--v5.0.23 e5.0.4 以前は ["emqx1", "emqx2"] のみサポート
v5.0.23e5.0.4 以降は両方サポート-->

- `discovery_strategy`はノードディスカバリーストラテジーで、`static`に設定します。
- `seeds`は配列で、クラスターに参加するノード名を複数カンマ区切りで指定します。

すべてのノードが起動すると、クラスターが自動的に形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)はドメインネームシステムの略称です。DNSサーバーはドメイン名の問い合わせに対し、そのドメイン名に対応するIPアドレス（Aレコード）を返します。DNSは1つのドメイン名に複数のAレコード（複数IPアドレス）を持つことができ、1つの名前に対して複数のIPアドレスを対応させるマッピングを形成します。EMQXのDNS自動クラスタリングはこの1対多のマッピングを利用し、クラスター内のすべてのノードを特定し、各独立したノードがクラスターに参加できるようにします。

#### DNSサービスの設定

ほとんどのパブリッククラウドサービスはDNSサービスを提供しています。ドメイン名を割り当てた後、このドメインのAレコードに各EMQXノードのIPアドレスを追加するだけで設定は完了します。EMQXをプライベートクラウドや内部ネットワークにデプロイする場合は、[BIND](https://www.isc.org/bind/)などのソフトウェアで独自のDNSシステムを構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf`の`cluster.dns`設定項目にクラスターに参加するすべてのノードを追加します。

**設定例：**

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

- `discovery_strategy`はノードディスカバリーストラテジーで、`dns`に設定します。
- `cluster.dns.name`はDNS名／ドメイン名を文字列で指定します。例：`localhost`
- `cluster.dns.record_type`は列挙型で、`a`または`srv`のいずれかを指定します。

すべてのノードが起動すると、クラスターが自動的に形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/)はCoreOSが開始したオープンソースプロジェクトで、分散システムにおけるサービスディスカバリーや接続確立に広く使われています。これはまさにEMQXの自動クラスタリングが必要とする機能です。

ネットワーク内にetcdサーバー（クラスター）をデプロイした後、EMQXはetcdを介して自動的にクラスターを作成できます。etcdのインストールや設定方法は[etcd Install](https://etcd.io/docs/latest/install/)をご覧ください。

etcdを使った自動クラスタリングを有効にするには、`emqx.conf`の`cluster.etcd`設定項目を設定します。

**設定例：**

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

- `discovery_strategy`はノードディスカバリーストラテジーで、`etcd`に設定します。
- `cluster.etcd.server`はetcdサーバーのアドレスで、複数ノードはカンマ区切りで指定可能です。
- `cluster.etcd.prefix`はEMQXサービスディスカバリーに使うetcdのキーのプレフィックスです。
- `cluster.etcd.node_ttl`はetcdキーの有効期限を示す期間で、デフォルトは`1m`（1分）です。

設定完了後、EMQXノードを順に起動し、etcdctlツールでetcdサーバーの状態を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

この結果は、すべてのノードが正常に起動し、自動的にクラスターに参加したことを示しています。

### Kubernetes上での自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/)は、Kubernetes環境でのEMQXクラスターの作成と管理を迅速に行うためのツールで、デプロイや管理作業を低コストでラベル付けされた繰り返し可能なジョブに変換し、EMQXクラスターの展開と管理を大幅に簡素化します。

自分でEMQXをデプロイ・管理したい場合でも、Kubernetes APIを使ったノードディスカバリーと自動クラスタリングは利用可能です。この機能を使うには、EMQX PodがKubernetes APIサーバーからエンドポイントリソースを通じてクラスターのノード情報を取得できるようにRBACを作成する必要があります。設定方法は[RBAC認可の使用](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)をご覧ください。

Kubernetes上でEMQX自動クラスタリングを有効にするには、`emqx.conf`の`cluster.k8s`設定項目を設定します。

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

- `discovery_strategy`はノードディスカバリーストラテジーで、`k8s`に設定します。
- `cluster.K8s.apiserver`はKubernetes APIエンドポイントURLで、デフォルトは`http://10.110.111.204:8080`です。
- `cluster.K8s.service_name`はEMQXサービス名で、デフォルトは`emqx`です。
- `cluster.K8s.address_type`は検出したノードに接続するためのアドレス種別で、デフォルトは`ip`です。選択肢は`ip`、`dns`、`hostname`です。
- （オプション）`cluster.K8s.suffix`はノード名のサフィックスで、`cluster.K8s.address_type`が`dns`の場合のみ必要です。デフォルトは`pod.local`です。
- `cluster.K8s.namespace`はKubernetesのネームスペースで、文字列型です。デフォルトは`default`です。

設定後、ノードを順に起動するとクラスターが自動的に形成されます。

::: tip

Kubernetes上でEMQX自動クラスタリングを利用する場合、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の利用を推奨します。

:::

## クラスターの管理

クラスターを作成した後は、クラスター状態の監視やノード管理が可能です。

### クラスター状態の確認

任意のクラスター内ノード上で以下のコマンドを実行し、クラスター状態を確認します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスターからの離脱

クラスターから離脱する方法は2つあります。

1. `cluster leave`コマンドを実行：現在のノードがクラスターから離脱します。クラスター内の他ノードに通知し、クラスター運用から停止します。離脱前に進行中のタスクは完了します。
2. `cluster force-leave <node@host>`コマンドを実行：指定したノードをクラスターから強制的に削除します。通常、ノードが故障または応答しない場合に使用します。

例として、先に作成したクラスターで`emqx@s2.emqx.io`が離脱したい場合、`emqx@s2.emqx.io`上で以下を実行します。

```bash
./bin/emqx ctl cluster leave
```

または`emqx@s1.emqx.io`上で以下を実行し、`emqx@s2.emqx.io`をクラスターから削除します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間のネットワークプロトコルを設定できます。EMQXはTCPまたはTLSによるノード間接続をサポートしています。接続方法は`emqx.conf`で設定します。

TCP IPv4およびTCP IPv6を使用する場合は、`emqx.conf`の`cluster.proto_dist`を設定します。

- TCP IPv4：`inet_tcp`（デフォルト）
- TCP IPv6：`inet6_tcp`

SSLを有効にするには、まず`cluster.proto_dist`を`inet_tls`に設定し、`etc`フォルダ内の`ssl_dist.conf`ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)をご覧ください。

<!--ここに設定例が必要です-->

## 疑似分散クラスター

EMQXはテストや開発目的のために疑似分散クラスター機能を提供しています。これは、1台のマシン上で複数のEMQXインスタンスを実行し、それぞれをクラスター内のノードとして設定する構成を指します。

最初のノードを起動：

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

次に、以下のコマンドで2つ目のノードを起動し、手動でクラスターに参加させます。ポート競合を避けるため、ノードごとに異なるリスニングポートを使用し、ログファイルや内部データベース用のディレクトリも分ける必要があります。

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

上記は手動でクラスターを作成する例です。自動クラスタリングによる作成方法は[自動クラスタリング](#auto-clustering)セクションを参照してください。

なお、この構成は本番環境での利用は推奨されません。
