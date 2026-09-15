# クラスターの作成と管理

EMQXクラスターは手動または自動で作成できます。本ページでは、手動クラスタリングと自動クラスタリングの両方の方法を紹介し、これら2つの異なるアプローチを用いたEMQXクラスターの作成と管理について案内します。

::: tip 注意

クラスター機能は有効なライセンスキーがある場合のみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識や動作については、[Cluster](../../develop/cluster/introduction.md)および[Architecture](../../develop/cluster/mria-introduction.md)をご参照ください。クラスターを作成するには、以下の概念も理解しておく必要があります。

### ノード名

EMQXのノードは名前で識別されます。すべてのノードは `name@host` 形式の一意なノード名を持ち、`host` はIPアドレスまたは完全修飾ドメイン名（FQDN）である必要があります。例えば：

- サーバー`s1.emqx.io`にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` とします。
- このサーバーが静的IP（`192.168.0.10`）を持つ場合、ノード名は `emqx@192.168.0.10` とします。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更できません。したがって、EMQXノード名には静的なFQDNを使用することを推奨します。
:::

### ノードディスカバリー

ノードディスカバリーはクラスター作成に必須のプロセスであり、個々のEMQXノードが互いに発見し通信できるようにします。場所やIPアドレスに関係なくノード同士が接続可能になります。

### 手動クラスタリングと自動クラスタリング

ノードディスカバリーの戦略に基づき、クラスターの作成方法は手動クラスタリングと自動クラスタリングに分けられます。

手動クラスタリングは、どのノードをクラスターに含めるかを手動で指定してEMQXクラスターを作成する方法です。自動クラスタリングは、複数のEMQXノードが手動設定なしに自動的にクラスターを形成する方法で、クラスターのセットアップを簡略化し、ノードの追加や削除を動的に行いやすくします。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesなどをベースにした自動クラスタリングをサポートしています。

以下の表は、EMQXがサポートするノードディスカバリー戦略とクラスター作成方法の一覧です。

| 戦略          | 説明                                                         |
| ------------- | ------------------------------------------------------------ |
| `manual`      | コマンドで手動によりクラスターを作成                         |
| `static`      | 静的ノードリストによる自動クラスタリング                     |
| `dns`         | DNSのAレコードおよびSRVレコードによる自動クラスタリング      |
| `etcd`        | etcdによる自動クラスタリング                                 |
| `k8s`         | Kubernetesによる自動クラスタリング                           |
| `singleton`   | クラスタリング無効。ノードは他ノードとの接続試行をすべて拒否。 |

EMQXは[Erlang/OTPアプリケーション向けに開発されたEkkaライブラリ](https://github.com/emqx/ekka)をベースに自動クラスター作成をサポートしています。EkkaはErlangノードの自動検出（サービスディスカバリー）や自動クラスタリング（Autocluster）に加え、ネットワークパーティションの自動修復（Network Partition Autoheal）やダウンしたノードの自動削除（Autoclean）などの機能も実装しています。

クラスターの作成方法は、`emqx.conf`設定ファイルのノードディスカバリー戦略で定義します。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめる前に

このセクションでは、クラスター作成前にノードやネットワーク環境をどのように設定するかについて説明します。

### ノード名の設定

クラスターを作成する前に、クラスターに参加するノードの名前付け方法を理解しておく必要があります。例えば、`s1.emqx.io`と`s2.emqx.io`にそれぞれデプロイされた2つのノードでクラスターを作成する場合、以下の手順に従います。

1つ目のノードの`emqx.conf`設定ファイルにノード名を設定します。例：

```bash
node.name = emqx@s1.emqx.io
```

また、環境変数でノード名を上書きすることも可能です。例えば、`docker run`コマンドの`-e`オプションやsystemdの`emqx.service`ファイルで以下のように定義します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

クラスターに参加する他のノードについても同様の手順を繰り返します。

これでクラスターに参加する2つのノード、`emqx@s1.emqx.io`と`emqx@s2.emqx.io`の名前付けが完了しました。手動または自動のいずれかでクラスターを作成できます。

### ノードクッキーの設定

セキュリティのため、クラスターに参加するすべてのノードで`emqx.conf`のデフォルトクッキー設定をSecretクッキーに変更してください。クラスターに参加するすべてのノードは同一のSecretクッキーを使用する必要があります。マジッククッキーの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)をご参照ください。

```
node {
  cookie = "<Secretなクッキー>"
}
```

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合は、内部クラスター通信に必要な以下のポートを開放する必要があります。

- **4370**: Erlang分散トランスポートポート
- **5370**: クラスターRPCポート（物理マシン環境向け）
- **5369**: クラスターRPCポート（Docker環境向け）

1台のサーバーに複数のEMQXノードをデプロイする場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[クラスター内通信ポート](./security.md)をご参照ください。

## クイックスタート

このセクションでは、Dockerネットワーク上で2つの異なるクラスタリング方法を使い、クラスターを素早く作成する方法を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを稼働させクラスターを形成する場合は、追加の設定が必要です。コンテナ内の必要なクラスター通信ポートのマッピングとファイアウォールの開放については[ネットワーク環境の設定](#ネットワーク環境の設定)を参照してください。

:::

:::: tabs type:card

::: tab 手動クラスタリング（コマンド）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセスできます。

   ```bash
   docker network create emqx-net
   ```

2. 1つ目のノードを起動し、環境変数でノード名を設定します。EMQXのデフォルトクラスタリング方法は手動なので追加設定は不要です。ノードをDockerネットワークに参加させ、ノードホストに対応するネットワークエイリアスを設定します。

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

3. 1つ目のノード起動後、2つ目のノードを起動します。新しいノードは1つ目のノードと同じネットワークに参加する必要があります。1つ目のノードが1883などのポートを既に占有しているため、ここではポートマッピングは行いません。

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       --network emqx-net \
       --network-alias node2.emqx.com \
       emqx/emqx-enterprise:@EE_VERSION@
   ```

4. どちらかのノード上で以下のコマンドを実行し、現在のノードを他のノードに接続してクラスターを作成します。コマンドの詳細は[手動クラスタリング](#manual-clustering)を参照してください。

   ```bash
   docker exec -it emqx2 \
       emqx ctl cluster join emqx@node1.emqx.com
   ```

:::

::: tab 手動クラスタリング（ダッシュボード）

EMQX v5.9.0以降、ダッシュボードから直接クラスターを作成できます。

1. すべてのノードが起動し、適切な`name@host`、同一のクッキーを持ち、ネットワークで相互に到達可能であることを確認します。

2. 任意のノードの**ダッシュボード**にアクセスします。

3. **管理 > MQTT設定 > クラスター**に移動します。

4. （任意）**クラスター説明**欄にクラスターの目的や環境を識別する説明を入力し、**保存**をクリックします。

   > この機能はEMQX v6.0.0以降で利用可能です。

5. **招待**をクリックし、ノード名（例：`emqx@node2.emqx.com`）を入力して確定します。

6. 招待されたノードは状態を同期後、クラスターに参加します。

詳細はダッシュボードの[クラスター設定](../dashboard/cluster_settings.md#cluster)をご覧ください。

:::

::: tab 自動クラスタリング（静的ノードリスト方式）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセスできます。

   ```bash
   docker network create emqx-net
   ```

2. 1つ目のノードを起動し、環境変数でノード名とクラスタリング方法を設定します。

   - `EMQX_NODE_NAME` 環境変数でノード名を設定します。
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY` 環境変数でクラスターディスカバリー戦略を設定します。ここでは[静的クラスタリング](#autocluster-by-static-node-list)を使用します。
   - `EMQX_CLUSTER__STATIC__SEEDS` 環境変数で静的ノードリストを設定します。すべてのノード名を含める必要があります。

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

3. 1つ目のノード起動後、2つ目のノードを起動します。クラスタリング方法は同じで、新しいノードも1つ目のノードと同じネットワークに参加する必要があります。1つ目のノードが1883などのポートを既に占有しているため、ここではポートマッピングは行いません。

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

任意のノード上で`emqx ctl cluster status`コマンドを実行し、クラスター状態を確認できます。クラスター状態が正常であれば、以下のような情報が出力されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの指示に従い、必要なクラスター作成方法を選択して修正・デプロイを行えます。

## 手動クラスタリング

このセクションでは、手動でクラスターを作成する手順を説明します。手動クラスタリングでは、クラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も構築する必要があります。自動クラスタリングと比べて、カスタムネットワークトポロジーの細かい調整が可能であり、自動クラスタリングが利用できない場合や適さない場合に適しています。

:::tip

手動クラスタリングはコアノードのみで使用可能です。コア-レプリカノード構成の場合は、自動クラスタリングを利用してください。

:::

例えば、`emqx@node1.emqx.com`と`emqx@node2.emqx.com`の2ノードでクラスターを手動作成する場合、以下の手順を実行します。

1. クラスターのディスカバリー戦略を`manual`に設定します。

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

   - このコマンドはクラスターに参加するノード上で実行する必要があります。つまり**招待**ではなく**参加要求**です。
   - `emqx@s2.emqx.io`が`emqx@s1.emqx.io`に参加すると、ローカルデータはクリアされ、`emqx@s1.emqx.io`のデータと同期されます。
   - `emqx@s2.emqx.io`が別のクラスターに参加したい場合は、まず現在のクラスターを離脱する必要があります。離脱方法は[クラスター離脱](#leave-cluster)をご参照ください。

   :::

3. 任意のノード上でクラスター状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status

   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

2ノードでのクラスター作成に成功しました。次に、[クラスター状態の確認](#query-cluster-status)、[クラスターのノード管理](#manage-cluster-nodes)、[ネットワークプロトコルの設定](#configure-network-protocols)のセクションを参照し、クラスターの監視や管理方法を学んでください。

EMQX v5.9.0以降はダッシュボードからノードの招待や管理も可能で、より直感的かつ使いやすい操作ができます。詳細は[クラスター設定](../dashboard/cluster_settings.md#cluster)をご覧ください。

## 自動クラスタリング

このセクションでは、さまざまな自動クラスタリング方法によるクラスター作成方法を説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードに事前定義された静的ノードリストを持たせ、起動後にリストに基づいて自動的にクラスターを形成する方法です。

静的クラスタリングは、他のネットワークコンポーネントやサービスに依存せず、最も簡単にEMQXクラスターを自動作成できる方法です。各ノードがTCPプロトコルで相互通信できれば、EMQXクラスターを形成できます。

この機能を有効にするには、`emqx.conf`でクラスターのモードとノードリストを設定します。

**設定例：**

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

ここで、

- `discovery_strategy` はノードディスカバリー戦略で、`static`に設定します。
- `seeds` は配列で、クラスターに参加するノード名を複数カンマ区切りで追加します。

すべてのノードを起動すると、クラスターが自動的に形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)（ドメインネームシステム）は、DNSサーバーがドメイン名の問い合わせを受けると、そのドメイン名に対応するIPアドレス（Aレコード）を返す仕組みです。DNSは1つのドメイン名に複数のAレコード（複数IPアドレス）を持つことができ、1つの名前に対し複数のIPアドレスを対応付けることが可能です。EMQXのDNS自動クラスタリングはこの1対多のマッピングを利用し、クラスター内のすべてのノードを特定し、各ノードがクラスターに参加できるようにします。

#### DNSサービスの設定

多くのパブリッククラウドサービスはDNSサービスを提供しています。ドメイン名を割り当てた後、各EMQXノードのIPアドレスをそのドメインのAレコードに追加するだけで設定完了です。プライベートクラウドや内部ネットワークでEMQXをデプロイする場合は、[BIND](https://www.isc.org/bind/)などのソフトウェアで独自のDNSシステムを構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf`の`cluster.dns`設定項目にクラスター参加ノードを追加します。

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

ここで、

- `discovery_strategy` はノードディスカバリー戦略で、`dns`に設定します。
- `cluster.dns.name` は文字列で、対象のホスト名を入力します。
- `cluster.dns.record_type` は列挙型で、`a`または`srv`を指定可能です。

すべてのノードを起動すると、クラスターが自動的に形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/)はCoreOSが開始したオープンソースプロジェクトで、分散システムのサービスディスカバリーや接続確立に広く利用されています。これはEMQXの自動クラスタリングに必要な機能と合致します。

ネットワーク内にetcdサーバー（クラスター）を展開した後、EMQXはetcdを介して自動的にクラスターを作成できます。etcdのインストールや設定方法は[etcd Install](https://etcd.io/docs/latest/install/)をご参照ください。

etcdを使った自動クラスタリングを有効にするには、`emqx.conf`の`cluster.etcd`設定項目を使用します。

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

ここで、

- `discovery_strategy` はノードディスカバリー戦略で、`etcd`に設定します。
- `cluster.etcd.server` はetcdサーバーのアドレスで、複数ノードはカンマ区切り可能です。
- `cluster.etcd.prefix` はEMQXサービスディスカバリーに使うetcdのキー接頭辞です。
- `cluster.etcd.node_ttl` は期間で、ノードに関連付けられたetcdキーの有効期限を示します。デフォルトは`1m`です。

設定完了後、EMQXノードを順に起動し、etcdctlツールでetcdサーバーの変化を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

結果はすべてのノードが正常に起動し、自動的にクラスターに参加したことを示しています。

### Kubernetesによる自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/)は、Kubernetes環境でのEMQXクラスターの作成と管理を迅速に行えるよう支援し、デプロイと管理の工数を低減し、ラベル付けされた反復可能なジョブとして簡素化します。

自分でEMQXをデプロイ・管理したい場合でも、Kubernetes APIを利用したノードディスカバリーと自動クラスタリングが可能です。この機能を使うには、まずEMQX PodにRBACを作成し、Kubernetes APIServerのendpointsリソースからクラスターのノード情報を取得できるようにします。設定方法は[Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)をご参照ください。

Kubernetes上でのEMQX自動クラスタリングを有効にするには、`emqx.conf`の`cluster.k8s`設定項目を使用します。

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

ここで、

- `discovery_strategy` はノードディスカバリー戦略で、`k8s`に設定します。
- `cluster.K8s.apiserver` はKubernetes APIエンドポイントURLで、デフォルトは`http://10.110.111.204:8080`です。
- `cluster.K8s.service_name` はEMQXサービス名で、デフォルトは`emqx`です。
- `cluster.K8s.address_type` は検出したノードに接続するためのアドレス種別で、デフォルトは`ip`です。選択肢は`ip`、`dns`、`hostname`です。
- （オプション）`cluster.K8s.suffix` はノード名のサフィックスで、`cluster.K8s.address_type`が`dns`の場合のみ必要です。デフォルトは`pod.local`です。
- `cluster.K8s.namespace` はKubernetesのネームスペースで、文字列型です。デフォルトは`default`です。

設定後、ノードを順に起動するとクラスターが自動的に形成されます。

::: tip

Kubernetes上でEMQX自動クラスタリングを利用する場合、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の使用が推奨されます。

:::

## クラスターの管理

クラスター作成後は、クラスター状態の監視やノード管理を行えます。

### クラスター状態の確認

任意のクラスター内ノード上で以下のコマンドを実行し、クラスター状態を確認します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスターからの離脱

クラスターから離脱する方法は2通りあります。

1. `cluster leave`コマンドを実行：現在のノードがクラスターから離脱します。クラスター内の他ノードに通知し、クラスター操作への参加を停止します。離脱前に進行中のタスクを完了します。
2. `cluster force-leave <node@host>`コマンドを実行：指定したノードをクラスターから強制的に削除します。対象ノードが故障または応答しない場合に使用します。

例えば、先に作成したクラスターで`emqx@s2.emqx.io`が離脱する場合、`emqx@s2.emqx.io`上で以下を実行します。

```bash
./bin/emqx ctl cluster leave
```

または、`emqx@s1.emqx.io`上で以下を実行し、`emqx@s2.emqx.io`をクラスターから削除します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間のネットワークプロトコルを設定できます。EMQXはTCPまたはTLSでノード間接続をサポートしており、`emqx.conf`で接続方法を設定します。

TCP IPv4およびTCP IPv6を使用する場合は、`emqx.conf`の`cluster.proto_dist`を設定します。

- TCP IPv4: `inet_tcp`（デフォルト）
- TCP IPv6: `inet6_tcp`

SSLを有効にするには、まず`cluster.proto_dist`を`inet_tls`に設定し、`etc`フォルダ内の`ssl_dist.conf`ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)をご参照ください。

<!--ここに設定例が必要-->

## 疑似分散クラスター

EMQXはテストや開発目的で疑似分散クラスター機能を提供しています。これは1台のマシン上で複数のEMQXインスタンスを起動し、それぞれをクラスターのノードとして設定する構成です。

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

上記は手動でクラスターを作成する例です。自動クラスタリングによる作成方法は[自動クラスタリング](#auto-clustering)セクションも参照してください。

この構成は本番環境には推奨されませんのでご注意ください。
