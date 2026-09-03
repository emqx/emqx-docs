# クラスターの作成と管理

EMQXクラスターは手動または自動で作成できます。本ページでは手動および自動クラスタリングの方法を紹介し、これら2つの異なるアプローチを用いたEMQXクラスターの作成と管理方法を案内します。

::: tip 注意

クラスター機能は有効なライセンスキーがある場合にのみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識や動作については、[Cluster](./introduction.md)および[Architecture](./mria-introduction.md)をご参照ください。クラスターを作成するには、以下の概念にも慣れておく必要があります。

### ノード名

EMQXのノードは名前で識別されます。すべてのノードは `name@host` 形式の一意のノード名を持ち、hostはIPアドレスまたは完全修飾ドメイン名（FQDN）でなければなりません。例：

- サーバー`s1.emqx.io`に展開されたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` とします。
- このサーバーに静的IP（`192.168.0.10`）がある場合、ノード名は `emqx@192.168.0.10` とします。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更できません。そのため、EMQXノード名には静的なFQDNの使用を推奨します。
:::

### ノードディスカバリー

ノードディスカバリーはクラスター作成に必須のプロセスであり、個々のEMQXノードが互いに発見し通信できるようにします。場所やIPアドレスに関係なくノード間の接続を可能にします。

### 手動クラスタリングと自動クラスタリング

ノードディスカバリーストラテジーに基づき、クラスター作成方法は手動クラスタリングと自動クラスタリングに分かれます。

手動クラスタリングは、どのノードをクラスターに含めるかを手動で指定してEMQXクラスターを作成する方法です。自動クラスタリングは、複数のEMQXノードが手動設定なしで自動的にクラスターを形成する方法です。自動クラスタリングはEMQXクラスターのセットアップを簡素化し、ノードの動的な追加・削除を容易にします。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesなどに基づく自動クラスタリングをサポートしています。

以下の表はEMQXがサポートするノードディスカバリーストラテジーとクラスター作成方法を示します。

| ストラテジー | 説明                                         |
| ------------ | -------------------------------------------- |
| `manual`     | コマンドで手動によりクラスターを作成          |
| `static`     | 静的ノードリストによる自動クラスタリング      |
| `dns`        | DNSのAレコードおよびSRVレコードによる自動クラスタリング |
| `etcd`       | etcdによる自動クラスタリング                  |
| `k8s`        | Kubernetesによる自動クラスタリング            |
| `singleton`  | クラスタリング無効。ノードは他ノードとの接続を拒否 |

EMQXは[Erlang/OTPアプリケーション向けに開発されたEkka](https://github.com/emqx/ekka)ライブラリに基づく自動クラスター作成をサポートしています。EkkaはErlangノードの自動発見（サービスディスカバリー）や自動クラスタリング（Autocluster）に加え、ネットワークパーティションの自動修復（Network Partition Autoheal）やダウンノードの自動削除（Autoclean）などの機能も提供します。

`emqx.conf`設定ファイルでノードディスカバリーストラテジーを設定することでクラスタリング方式を定義できます。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめる前に

クラスター作成前にノードやネットワーク環境の設定方法を説明します。

### ノード名の設定

クラスターに参加するノード名の付け方を理解しておく必要があります。例えば、`s1.emqx.io` と `s2.emqx.io` にそれぞれ展開された2つのノードでクラスターを作成する場合、以下の手順でノード名を設定します。

1つ目のノードの`emqx.conf`にノード名を設定します。

```bash
node.name = emqx@s1.emqx.io
```

環境変数でノード名を上書きすることも可能です。例えば、`docker run`コマンドの`-e`オプションやsystemdの`emqx.service`ファイルで以下のように設定します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

クラスターに参加する他のノードも同様に設定してください。

これでクラスターに参加する2つのノード、`emqx@s1.emqx.io` と `emqx@s2.emqx.io` の名前が設定できました。手動または自動のいずれかでクラスターを作成できます。

### ノードクッキーの設定

セキュリティのため、クラスターに参加する各ノードのデフォルトクッキーは置き換え、すべてのノードで同じ秘密クッキーを使用してください。`emqx.conf`の`node.cookie`で設定します。設定ファイルにクッキー値を直接書きたくない場合は、`file://` URLを利用します。

```hocon
node.cookie = "file:///run/secrets/emqx-cookie"
```

`EMQX_NODE__COOKIE`環境変数も`file://` URLを受け付けます。通常ファイル、FIFO、起動時読み込みの詳細は[Load the Node Cookie from a File](../../configuration/secret-from-file.md#load-the-node-cookie-from-a-file)を参照してください。マジッククッキーの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)をご覧ください。

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合は、以下のクラスター内部通信に必要なポートを開放してください。

- **4370**: Erlang分散トランスポートポート
- **5370**: クラスターRPCポート（物理マシン環境向け）
- **5369**: クラスターRPCポート（Docker環境向け）

1台のサーバーに複数のEMQXノードを展開する場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[Intra-cluster communication port](./security.md)を参照してください。

## クイックスタート

このセクションでは、2つの異なるクラスタリング方法を用いてDockerネットワーク上でクラスターを素早く作成する方法を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを実行しクラスターを形成する場合は、追加の設定が必要です。コンテナ内で必要なクラスター通信ポートをマッピングし、ファイアウォールでこれらのポートを開放してください。詳細は[Configure Network Environment](#configure-network-environment)をご覧ください。

:::

:::: tabs type:card

::: tab 手動クラスタリング（コマンド）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 最初のノードを起動し、環境変数でノード名を設定します。EMQXのデフォルトクラスタリング方式は手動なので追加設定は不要です。ノードをDockerネットワークに参加させ、ノードホストに対応するネットワークエイリアスを設定します。

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

1. すべてのノードが起動し、適切な`name@host`、同一のクッキーを持ち、相互にネットワークで到達可能であることを確認します。

2. 任意のノードの**ダッシュボード**にアクセスします。

3. **Management > MQTT Settings > Cluster** に移動します。

4. （任意）**Cluster Description**欄にクラスターの目的や環境を識別する説明を入力し、**Save**をクリックします。

   > この機能はEMQX v6.0.0以降で利用可能です。

5. **Invite**をクリックし、ノード名（例：`emqx@node2.emqx.com`）を入力して確定します。

6. 招待されたノードは状態を同期後、クラスターに参加します。

詳細はダッシュボードの[Cluster Settings](../../dashboard/cluster_settings.md#cluster)をご覧ください。

:::

::: tab 自動クラスタリング（静的ノードリスト方式）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 最初のノードを起動し、環境変数でノード名とクラスタリング方式を設定します。

   - `EMQX_NODE_NAME` 環境変数でノード名を設定します。
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY` 環境変数でクラスターディスカバリーストラテジーを設定します。ここでは[静的クラスタリング](#autocluster-by-static-node-list)を使用します。
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
   
3. 最初のノード起動後、2つ目のノードを起動します。クラスタリング方式は同じで、新しいノードも最初のノードと同じネットワークに参加する必要があります。最初のノードが1883などのポートを占有しているため、ここではポートマッピングは行いません。

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

任意のノード上で`emqx ctl cluster status`コマンドを実行し、クラスター状態を確認できます。正常な場合、以下のような情報が出力されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの指示に従い、必要なクラスター作成方法を選択して修正・展開してください。

## 手動クラスタリング

このセクションでは手動でクラスターを作成する手順を説明します。手動クラスタリングではクラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も構築する必要があります。自動クラスタリングと比べてカスタムネットワークトポロジーを細かく調整でき、自動クラスタリングが利用できないまたは適さない場合に適しています。

:::tip 

手動クラスタリングはコアノードにのみ利用可能です。コア-レプリカノード構成を使用している場合は、自動クラスタリングでクラスターを管理してください。

:::

例えば、`emqx@node1.emqx.com` と `emqx@node2.emqx.com` の2つのノードがある場合、以下の手順で手動クラスターを作成できます。

1. クラスターのディスカバリーストラテジーを`manual`に設定します。

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

   - このコマンドはクラスターに参加するノード上で実行する必要があります。つまり**招待**ではなく**参加要求**です。
   - `emqx@s2.emqx.io`が`emqx@s1.emqx.io`に参加すると、ローカルデータはクリアされ、`emqx@s1.emqx.io`のデータと同期されます。
   - `emqx@s2.emqx.io`が別のクラスターに参加したい場合、まず現在のクラスターから離脱する必要があります。離脱方法は[Leave Cluster](#leave-cluster)を参照してください。

   :::

3. 任意のノードでクラスター状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status
   
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

これで2ノードのクラスター作成に成功しました。クラスター状態の監視や管理方法は、[Query Cluster Status](#query-cluster-status)、[Manage Cluster Nodes](#manage-cluster-nodes)、[Configure Network Protocols](#configure-network-protocols)をご覧ください。

EMQX v5.9.0以降はダッシュボードからノードの招待や管理も可能で、より直感的かつ使いやすくなっています。詳細は[Cluster Settings](../../dashboard/cluster_settings.md#cluster)をご参照ください。

## 自動クラスタリング

このセクションでは各種自動クラスタリング方法によるクラスター作成方法を説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードに事前定義された静的ノードリストを設定し、起動後にそのリストに基づいて自動的にクラスターを形成する方法です。

静的クラスタリングは他のネットワークコンポーネントやサービスに依存せず、最も簡単にEMQXクラスターを自動作成できる方法です。各ノードがTCPプロトコルで相互通信可能であればクラスターを形成できます。

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

- `discovery_strategy` はノードディスカバリーストラテジーで、`static`に設定します。
- `seeds` はノード名の配列で、クラスターに参加するノードを複数カンマ区切りで指定します。

すべてのノードを起動すると、自動的にクラスターが形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)（Domain Name System）は、ドメイン名の問い合わせに対して対応するIPアドレス（Aレコード）を返す仕組みです。DNSは1つのドメイン名に複数のAレコード（複数IPアドレス）を持てるため、1つの名前に対して複数IPアドレスを対応付けることができます。EMQXのDNS自動クラスタリングはこの1対多のマッピングを利用し、クラスター内のすべてのノードを特定し、各ノードがクラスターに参加できるようにします。

#### DNSサービスの設定

ほとんどのパブリッククラウドサービスはDNSサービスを提供しています。ドメイン名を割り当てた後、すべてのEMQXノードのIPアドレスをそのドメインのAレコードに追加するだけで設定が完了します。プライベートクラウドや内部ネットワークに展開する場合は、[BIND](https://www.isc.org/bind/)などのDNSソフトウェアで独自のDNSシステムを構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf`の`cluster.dns`設定項目でクラスターに参加するノードを指定します。

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

- `discovery_strategy` はノードディスカバリーストラテジーで`dns`に設定します。
- `cluster.dns.name` は問い合わせるDNS名/ドメイン名の文字列です。例：`localhost`
- `cluster.dns.record_type` は列挙型で、`a`または`srv`が指定可能です。

すべてのノードを起動すると、自動的にクラスターが形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/)はCoreOSが開発したオープンソースプロジェクトで、分散システムにおけるサービスディスカバリーや接続確立に広く利用されています。EMQXの自動クラスタリングに最適です。

ネットワーク内にetcdサーバー（クラスター）を展開した後、EMQXはetcdを介して自動的にクラスターを作成できます。etcdのインストールや設定方法は[etcd Install](https://etcd.io/docs/latest/install/)を参照してください。

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

- `discovery_strategy` はノードディスカバリーストラテジーで`etcd`に設定します。
- `cluster.etcd.server` はetcdサーバーのアドレスで、複数ノードはカンマ区切りで指定可能です。
- `cluster.etcd.prefix` はEMQXサービスディスカバリーに使うetcdのキー接頭辞です。
- `cluster.etcd.node_ttl` はetcdキーの有効期限（デフォルトは`1m`）を示す期間です。

設定完了後、EMQXノードを順に起動し、`etcdctl`ツールでetcdサーバーの変化を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

結果はすべてのノードが正常に起動し、自動的にクラスターに参加したことを示しています。

### Kubernetes上での自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/)はKubernetes環境でのEMQXクラスター作成と管理を迅速に行うためのツールで、デプロイや管理の労力を低コストでラベル付け可能な繰り返し可能なジョブに変換し、EMQXクラスターの展開と管理を大幅に簡素化します。

独自にEMQXをデプロイ・管理したい場合は、Kubernetes APIを使ったノードディスカバリーと自動クラスタリングも利用可能です。この機能を使うには、EMQX PodがKubernetes APIServerからendpointsリソース経由でクラスターのノード情報を取得できるようRBACを作成する必要があります。設定方法は[Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)をご覧ください。

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

- `discovery_strategy` はノードディスカバリーストラテジーで`k8s`に設定します。
- `cluster.K8s.apiserver` はKubernetes APIエンドポイントURL（デフォルトは`http://10.110.111.204:8080`）
- `cluster.K8s.service_name` はEMQXサービス名（デフォルトは`emqx`）
- `cluster.K8s.address_type` は検出したノードに接続するためのアドレス種別（デフォルトは`ip`）。`ip`、`dns`、`hostname`が指定可能です。
- （オプション）`cluster.K8s.suffix` はノード名のサフィックスで、`address_type`が`dns`の場合にのみ必要。デフォルトは`pod.local`
- `cluster.K8s.namespace` はKubernetesのネームスペース（文字列）。デフォルトは`default`

設定後、ノードを順に起動すると自動的にクラスターが形成されます。

::: tip

Kubernetes上のEMQX自動クラスタリングでは、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の使用が推奨されます。

:::

## クラスターの管理

クラスター作成後は、クラスター状態の監視やノード管理が可能です。

### クラスター状態の確認

任意のクラスター内ノード上で以下のコマンドを実行し、クラスター状態を確認します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスターからの離脱

クラスターから離脱する方法は2つあります。

1. `cluster leave` コマンドを実行：現在のノードがクラスターから離脱します。クラスター内の他ノードに通知し、クラスター操作への参加を停止します。離脱前に進行中のタスクは完了します。
2. `cluster force-leave <node@host>` コマンドを実行：指定したノードをクラスターから強制的に削除します。通常、ノードが故障または応答しない場合に使用します。

例えば、既存のクラスターで`emqx@s2.emqx.io`が離脱したい場合、`emqx@s2.emqx.io`上で以下を実行します。

```bash
./bin/emqx ctl cluster leave
```

または、`emqx@s1.emqx.io`上で以下を実行し、`emqx@s2.emqx.io`をクラスターから削除します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間のネットワークプロトコルを設定できます。EMQXはTCPまたはTLSでノード間接続をサポートしています。接続方式は`emqx.conf`で設定します。

TCP IPv4およびTCP IPv6を使用する場合、`emqx.conf`の`cluster.proto_dist`を設定します。

- TCP IPv4: `inet_tcp`（デフォルト）
- TCP IPv6: `inet6_tcp`

SSLを有効にするには、まず`cluster.proto_dist`を`inet_tls`に設定し、`etc`フォルダ内の`ssl_dist.conf`ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)をご覧ください。

<!-- ここに例コードが必要 -->

## 疑似分散クラスター

EMQXはテストや開発用途向けに疑似分散クラスター機能を提供しています。これは1台のマシン上で複数のEMQXインスタンスを起動し、それぞれをクラスターのノードとして設定する構成を指します。

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

次に以下のコマンドで2つ目のノードを起動し、手動でクラスターに参加させます。ポート競合を避けるため、ノードごとに異なるリスニングポートを使用し、ログファイルや内部データベース用に別々のディレクトリを指定してください。

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

なお、この構成は本番環境での利用は推奨されません。
