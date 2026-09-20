# クラスターの作成と管理

EMQXクラスターは手動または自動で作成できます。本ページでは、手動クラスタリングと自動クラスタリングの両方の方法を紹介し、これら2つの異なるアプローチを用いてEMQXクラスターを作成および管理する手順を案内します。

::: tip 注意事項

クラスター機能は有効なライセンスキーがある場合のみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識と動作については、[Cluster](../../develop/cluster/introduction.md)および[Architecture](../../develop/cluster/mria-introduction.md)をご参照ください。クラスターを作成するには、以下の概念にも精通しておく必要があります。

### ノード名

EMQXのノードは名前で識別されます。すべてのノードは `name@host` 形式の一意なノード名が設定されており、host部分はIPアドレスまたは完全修飾ドメイン名（FQDN）でなければなりません。例：

- サーバー`s1.emqx.io`にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` とします。
- このサーバーに静的IP（`192.168.0.10`）がある場合、ノード名は `emqx@192.168.0.10` とします。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更不可能です。そのため、EMQXノード名には静的なFQDNの使用を推奨します。
:::

### ノードディスカバリー

ノードディスカバリーはクラスター作成時に必要なプロセスで、個々のEMQXノードが互いを発見し、場所やIPアドレスに関係なく通信できるようにします。

### 手動クラスタリングと自動クラスタリング

ノードディスカバリーの戦略に基づき、クラスター作成方法は手動クラスタリングと自動クラスタリングに分かれます。

手動クラスタリングは、どのノードをクラスターに含めるかを手動で指定してEMQXクラスターを作成する方法です。一方、自動クラスタリングは複数のEMQXノードが手動設定なしで自動的にクラスターを形成する方法です。自動クラスタリングはEMQXクラスターのセットアップを簡素化し、ノードの追加や削除を動的に行いやすくします。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesなどに基づく自動クラスタリングをサポートしています。

以下の表はEMQXがサポートするノードディスカバリー戦略とクラスター作成方法の一覧です：

| 戦略           | 説明                                                         |
| -------------- | ------------------------------------------------------------ |
| `manual`       | コマンドで手動によりクラスターを作成                         |
| `static`       | 静的ノードリストによる自動クラスタリング                     |
| `dns`          | DNSのAレコードおよびSRVレコードによる自動クラスタリング      |
| `etcd`         | etcdによる自動クラスタリング                                 |
| `k8s`          | Kubernetesによる自動クラスタリング                           |
| `singleton`    | クラスタリング無効。ノードは他ノードとの接続試行をすべて拒否。 |

EMQXは[Erlang/OTPアプリケーション向けに開発されたEkkaライブラリ](https://github.com/emqx/ekka)に基づく自動クラスター作成をサポートしています。EkkaはErlangノードの自動発見（Service Discovery）や自動クラスタリング（Autocluster）に加え、ネットワークパーティションの自動修復（Network Partition Autoheal）やダウンしたノードの自動削除（Autoclean）などの機能も実装しています。

クラスターの作成方法は`emqx.conf`設定ファイルのノードディスカバリー戦略で定義します。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめる前に

クラスター作成前にノードやネットワーク環境の設定方法について説明します。

### ノード名の設定

クラスターに参加するノードの名前の付け方を理解しておく必要があります。例えば、`s1.emqx.io`と`s2.emqx.io`にそれぞれ2つのノードをデプロイし、クラスターを作成する場合、以下の手順でノード名を設定します。

1つ目のノードの`emqx.conf`設定ファイルにノード名を設定します。

```bash
node.name = emqx@s1.emqx.io
```

環境変数でノード名を上書きすることも可能です。例えば、`docker run`コマンドの`-e`オプションやsystemdの`emqx.service`ファイルで以下のように定義します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

クラスターに参加させる他のノードについても同様に設定してください。

これで、`emqx@s1.emqx.io`と`emqx@s2.emqx.io`という2つのノード名が設定されました。これらのノードで手動または自動のいずれかの方法でクラスターを作成できます。

### ノードクッキーの設定

セキュリティ上の理由から、クラスターに参加するすべてのノードで`emqx.conf`のデフォルトクッキー設定をSecretクッキーに変更してください。すべてのノードは同じSecretクッキーを使用する必要があります。使用されるマジッククッキーの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)をご覧ください。

```
node {
  cookie = "<Secretクッキー>"
}
```

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合は、以下の内部クラスター通信に必要なポートを開放する必要があります。

- **4370**: Erlang分散トランスポートポート
- **5370**: クラスターRPCポート（物理マシン環境向け）
- **5369**: クラスターRPCポート（Docker環境向け）

1台のサーバーに複数のEMQXノードをデプロイする場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[Intra-cluster communication port](./security.md)をご参照ください。

## クイックスタート

このセクションでは、Dockerネットワーク上で2つの異なるクラスタリング方法を用いてクラスターを素早く作成する方法を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを実行しクラスターを形成する場合は、追加のセットアップが必要です。コンテナ内で必要なクラスター通信ポートをマッピングし、ファイアウォールでこれらのポートを開放する方法については[Configure Network Environment](#configure-network-environment)を参照してください。

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

3. 最初のノード起動後、2つ目のノードを起動します。新しいノードは最初のノードと同じネットワークに参加する必要があります。最初のノードが1883などのポートを占有しているため、ここではポートマッピングは行いません。

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

1. すべてのノードが起動し、適切な`name@host`、同一のクッキー、相互に到達可能なネットワーク環境であることを確認します。

2. 任意のノードの**ダッシュボード**にアクセスします。

3. **管理 > MQTT設定 > クラスター**に移動します。

4. （任意）**クラスター説明**欄にクラスターの目的や環境を識別する説明を入力し、**保存**をクリックします。

   > この機能はEMQX v6.0.0以降で利用可能です。

5. **招待**をクリックし、ノード名（例：`emqx@node2.emqx.com`）を入力して確定します。

6. 招待されたノードは状態を同期した後、クラスターに参加します。

詳細はダッシュボードの[クラスター設定](../dashboard/cluster_settings.md#cluster)をご覧ください。

:::

::: tab 自動クラスタリング（静的ノードリスト）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互にアクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 最初のノードを起動し、環境変数でノード名とクラスタリング方法を設定します。

   - `EMQX_NODE_NAME`環境変数でノード名を設定します。
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY`環境変数でクラスターディスカバリー戦略を設定します。ここでは[静的クラスタリング](#autocluster-by-static-node-list)を使用します。
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

3. 最初のノード起動後、2つ目のノードを起動します。クラスタリング方法と新しいノードは最初のノードと同じネットワークに参加する必要があります。最初のノードが1883などのポートを占有しているため、ここではポートマッピングは行いません。

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

任意のノード上で`emqx ctl cluster status`コマンドを実行してクラスターの状態を確認できます。クラスター状態が正常であれば、以下のような情報が出力されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの指示に従い、必要なクラスター作成方法を選択して修正・デプロイを行ってください。

## 手動クラスタリング

このセクションでは、手動でクラスターを作成する手順を説明します。手動クラスタリングでは、クラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も手動で構築する必要があります。自動クラスタリングと比較して、カスタムなネットワークトポロジーを細かく調整できるため、自動クラスタリングが利用できない、または適さない状況に非常に適しています。

:::tip

手動クラスタリングはコアノードにのみ使用可能です。コア-レプリカノードのデプロイアーキテクチャを使用している場合は、自動クラスタリングでクラスターを管理してください。

:::

例えば、`emqx@node1.emqx.com`と`emqx@node2.emqx.com`の2つのノードがある場合、以下の手順で手動クラスターを作成できます。

1. クラスターのディスカバリー戦略を`manual`に設定します。

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

   - このコマンドはクラスターに参加するノード上で実行する必要があります。つまり、**招待**ではなく**参加要求**です。
   - `emqx@s2.emqx.io`が`emqx@s1.emqx.io`に参加してクラスターを形成すると、ローカルデータはクリアされ、`emqx@s1.emqx.io`のデータと同期されます。
   - `emqx@s2.emqx.io`が別のクラスターに参加したい場合、まず現在のクラスターを離脱する必要があります。クラスター離脱方法は[Leave Cluster](#leave-cluster)を参照してください。

   :::

3. 任意のノードでクラスター状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status

   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

これで2ノードのクラスター作成に成功しました。クラスター状態の監視やクラスター管理方法については、[Query Cluster Status](#query-cluster-status)、[Manage Cluster Nodes](#manage-cluster-nodes)、[Configure Network Protocols](#configure-network-protocols)の各セクションをご覧ください。

EMQX v5.9.0以降はダッシュボードからノードの招待や管理も可能で、より直感的で使いやすい操作ができます。詳細は[Cluster Settings](../dashboard/cluster_settings.md#cluster)を参照してください。

## 自動クラスタリング

このセクションでは、さまざまな自動クラスタリング方法でクラスターを自動作成する方法を説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードにあらかじめ定義された静的ノードリストを設定し、起動後にノードリストに従って自動的にクラスターを形成する方法です。

静的クラスタリングは、他のネットワークコンポーネントやサービスに依存せず、TCPプロトコルで相互通信できる限り簡単にEMQXクラスターを自動作成できる最もシンプルな方法です。

この機能を有効にするには、`emqx.conf`でクラスターモードとノードリストを設定します。

**例：**

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@s1.emqx.io", "emqx@s2.emqx.io"]
    }
}
```

<!--v5.0.23 e5.0.4 以前は["emqx1", "emqx2"]のみサポート
v5.0.23e5.0.4以降は両方サポート-->

- `discovery_strategy`はノードディスカバリー戦略で、`static`に設定します。
- `seeds`は配列で、クラスターに参加するノード名を複数カンマ区切りで追加できます。

すべてのノードが起動すると、クラスターは自動的に形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)（Domain Name System）は、ドメイン名の問い合わせに対して対応するIPアドレス（Aレコード）を返す仕組みです。DNSは1つのドメイン名に複数のAレコード（複数IPアドレス）を持つことができ、1つの名前に対して複数のIPアドレスを対応付けることが可能です。EMQXのDNS自動クラスタリングはこの1対多のマッピングを利用し、クラスター内のすべてのノードを特定して、各ノードがクラスターに参加できるようにします。

#### DNSサービスの設定

ほとんどのパブリッククラウドサービスにはDNSサービスがあります。ドメイン名を割り当てた後は、このドメインのAレコードに各EMQXノードのIPアドレスを追加するだけで設定が完了します。プライベートクラウドや内部ネットワークにEMQXをデプロイする場合は、[BIND](https://www.isc.org/bind/)などのソフトウェアで独自のDNSシステムを構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf`の`cluster.dns`設定項目にクラスター参加ノードを追加します。

**例：**

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        ## DNSのAレコードおよびSRVレコードをサポート
        record_type = a
    }
}
```

- `discovery_strategy`はノードディスカバリー戦略で、`dns`に設定します。
- `cluster.dns.name`は問い合わせるDNS名/ドメイン名の文字列（例：`localhost`）です。
- `cluster.dns.record_type`は列挙型で、`a`または`srv`のいずれかを指定します。

すべてのノードが起動すると、クラスターは自動的に形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/)はCoreOSが開始したオープンソースプロジェクトで、分散システムにおけるサービスディスカバリーや接続確立に広く使われています。これはまさにEMQXの自動クラスタリングに必要な機能です。

ネットワーク内にetcdサーバー（クラスター）をデプロイすると、EMQXはetcdを介して自動的にクラスターを作成できます。etcdのインストールや設定方法は[etcd Install](https://etcd.io/docs/latest/install/)をご覧ください。

etcdを使った自動クラスタリングを有効にするには、`emqx.conf`の`cluster.etcd`設定項目を使用します。

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

- `discovery_strategy`はノードディスカバリー戦略で、`etcd`に設定します。
- `cluster.etcd.server`はetcdサーバーのアドレスで、複数ノードはカンマ区切りで指定可能です。
- `cluster.etcd.prefix`はEMQXサービスディスカバリー用のetcdキーのプレフィックスです。
- `cluster.etcd.node_ttl`はetcdキーの有効期限を示す期間で、デフォルトは`1m`です。

設定完了後、EMQXノードを順に起動し、etcdctlツールでetcdサーバーの変化を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

結果はすべてのノードが正常に起動し、自動的にクラスターに参加したことを示しています。

### Kubernetes上での自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/)はKubernetes環境でのEMQXクラスター作成・管理を迅速化し、デプロイや管理作業を低コストでラベル付けされた繰り返し可能なジョブに変換することで、EMQXクラスターの導入と管理を大幅に簡素化します。

自分でEMQXをデプロイ・管理したい場合は、Kubernetes APIを使ったノードディスカバリーと自動クラスタリングも利用可能です。この機能を使うには、まずEMQX PodにRBACを作成し、Kubernetes APIServerのendpointsリソースからクラスターのノード情報を取得できるようにします。設定方法は[Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)を参照してください。

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

- `discovery_strategy`はノードディスカバリー戦略で、`k8s`に設定します。
- `cluster.K8s.apiserver`はKubernetes APIエンドポイントURLで、デフォルトは`http://10.110.111.204:8080`です。
- `cluster.K8s.service_name`はEMQXサービス名で、デフォルトは`emqx`です。
- `cluster.K8s.address_type`は発見したノードに接続するためのアドレスタイプで、デフォルトは`ip`です。選択肢は`ip`、`dns`、`hostname`です。
- （オプション）`cluster.K8s.suffix`はノード名のサフィックスで、`cluster.K8s.address_type`が`dns`の場合のみ必要です。デフォルトは`pod.local`です。
- `cluster.K8s.namespace`はKubernetesのネームスペースで、文字列型。デフォルトは`default`です。

設定後、ノードを順に起動するとクラスターは自動的に形成されます。

::: tip

Kubernetes上でEMQX自動クラスタリングを使用する場合、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の使用を推奨します。

:::

## クラスター管理

クラスター作成後は、クラスター状態の監視やノード管理が可能です。

### クラスター状態の照会

任意のクラスター内ノードで以下のコマンドを実行し、クラスター状態を照会します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスターからの離脱

クラスターから離脱する方法は2つあります。

1. `cluster leave`コマンドを実行する：現在のノードがクラスターから離脱します。クラスター内の他ノードに通知し、クラスター運用から外れます。離脱前に進行中のタスクは完了します。
2. `cluster force-leave <node@host>`コマンドを実行する：指定したノードをクラスターから強制的に削除します。通常、ノードが故障したり応答しなくなった場合に使用します。

例えば、先に作成したクラスターで`emqx@s2.emqx.io`が離脱したい場合、`emqx@s2.emqx.io`上で以下のコマンドを実行します。

```bash
./bin/emqx ctl cluster leave
```

または、`emqx@s1.emqx.io`上で以下のコマンドを実行して`emqx@s2.emqx.io`をクラスターから削除します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間のネットワークプロトコルを設定できます。EMQXはTCPまたはTLSでノード間接続をサポートしており、接続方法は`emqx.conf`で設定します。

TCP IPv4およびTCP IPv6を使用する場合は、`emqx.conf`の`cluster.proto_dist`で設定します。

- TCP IPv4: `inet_tcp`（デフォルト）
- TCP IPv6: `inet6_tcp`

SSLを有効にするには、まず`cluster.proto_dist`を`inet_tls`に設定し、`etc`フォルダー内の`ssl_dist.conf`ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)をご覧ください。

<!--ここに例コードが必要-->

## 疑似分散クラスター

EMQXはテストや開発目的で疑似分散クラスター機能を提供しています。これは1台のマシン上で複数のEMQXインスタンスを起動し、それぞれをクラスター内のノードとして設定する構成を指します。

最初のノードを起動します。

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

上記の例は手動でクラスターを作成する方法です。自動クラスタリングによるクラスター作成方法は[Auto Clustering](#auto-clustering)セクションも参照してください。

この構成は本番環境での利用は推奨されません。
