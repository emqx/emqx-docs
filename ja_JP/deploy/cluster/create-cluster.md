# クラスターの作成と管理

EMQXクラスターは手動または自動で作成できます。本ページでは、手動クラスタリングと自動クラスタリングの両方の方法を紹介し、これら2つの異なるアプローチを用いたEMQXクラスターの作成および管理方法を案内します。

::: tip 注意

クラスター機能は有効なライセンスキーがある場合にのみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識や動作については、[Cluster](./introduction.md)および[Architecture](./mria-introduction.md)をご参照ください。クラスターを作成するには、以下の概念も理解しておく必要があります。

### ノード名

EMQXのノードはノード名で識別されます。すべてのノードは `name@host` という形式の一意なノード名が設定されており、hostはIPアドレスまたは完全修飾ドメイン名（FQDN）でなければなりません。例：

- サーバー`s1.emqx.io`にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` とします。
- このサーバーに固定IPアドレス（`192.168.0.10`）がある場合、ノード名は `emqx@192.168.0.10` とします。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更できません。そのため、EMQXノード名には固定のFQDNを使用することを推奨します。
:::

### ノードディスカバリー

ノードディスカバリーはクラスター作成に必要なプロセスで、個々のEMQXノードが互いを検出し、場所やIPアドレスに関係なく通信できるようにします。

### 手動クラスタリングと自動クラスタリング

ノードディスカバリーの戦略に基づき、クラスター作成方法は手動クラスタリングと自動クラスタリングに分けられます。

手動クラスタリングは、クラスターに参加するノードを手動で指定してEMQXクラスターを作成する方法です。自動クラスタリングは、複数のEMQXノードが手動設定なしに自動的にクラスターを形成できる方法で、クラスターのセットアップを簡素化し、ノードの動的な追加・削除を容易にします。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesなどに基づく自動クラスタリングをサポートしています。

以下の表は、EMQXがサポートするノードディスカバリー戦略とクラスター作成方法を示しています。

| 戦略          | 説明                                                         |
| ------------- | ------------------------------------------------------------ |
| `manual`      | コマンドで手動でクラスターを作成                             |
| `static`      | 静的ノードリストによる自動クラスタリング                     |
| `dns`         | DNSのAレコードおよびSRVレコードによる自動クラスタリング     |
| `etcd`        | etcdによる自動クラスタリング                                 |
| `k8s`         | Kubernetesによる自動クラスタリング                           |
| `singleton`   | クラスタリング無効。ノードは他ノードとの接続試行をすべて拒否 |

EMQXは[Erlang/OTPアプリケーション向けに開発されたEkkaライブラリ](https://github.com/emqx/ekka)に基づく自動クラスター作成をサポートしています。EkkaはErlangノードの自動検出（Service Discovery）や自動クラスタリング（Autocluster）に加え、ネットワークパーティションの自動修復（Network Partition Autoheal）やダウンしたノードの自動削除（Autoclean）などの機能も実装しています。

クラスタリング方法は`emqx.conf`設定ファイルのノードディスカバリー戦略で定義できます。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめる前に

クラスター作成前にノードやネットワーク環境の設定方法について説明します。

### ノード名の設定

クラスターに参加するノードの名前付け方法を理解しておく必要があります。例えば、`s1.emqx.io`と`s2.emqx.io`にそれぞれデプロイされた2つのノードでクラスターを作成する場合、以下の手順に従います。

1つ目のノードの`emqx.conf`にノード名を設定します。

```bash
node.name = emqx@s1.emqx.io
```

環境変数でノード名を上書きすることも可能です。例えば、`docker run`コマンドの`-e`オプションやsystemdの`emqx.service`ファイルで以下のように定義します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

クラスターに参加する他のノードも同様に設定します。

これでクラスターに参加する2つのノード、`emqx@s1.emqx.io`と`emqx@s2.emqx.io`のノード名が決まりました。手動または自動でクラスターを作成できます。

### ノードクッキーの設定

セキュリティのため、クラスターに参加するすべてのノードで`emqx.conf`のデフォルトクッキー設定をSecretクッキーに変更してください。すべてのノードは同じSecretクッキーを使用する必要があります。magic cookieの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)を参照してください。

```
node {
  cookie = "<Secretクッキー>"
}
```

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合、クラスター内部通信に必要な以下のポートを開放してください。

- **4370**：Erlang分散トランスポートポート
- **5370**：クラスターRPCポート（物理マシン環境向け）
- **5369**：クラスターRPCポート（Docker環境向け）

単一サーバーに複数のEMQXノードをデプロイする場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[クラスター内部通信ポート](./security.md)をご参照ください。

## クイックスタート

このセクションでは、Dockerネットワーク上で2つの異なるクラスタリング方法を使い、クラスターを素早く作成する方法を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを実行しクラスターを形成する場合、追加設定が必要です。コンテナ内のクラスター通信ポートのマッピングとファイアウォールでのポート開放については[ネットワーク環境の設定](#configure-network-environment)を参照してください。

:::

:::: tabs type:card

::: tab 手動クラスタリング（コマンド）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

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
   
3. 最初のノード起動後、2つ目のノードを起動します。新しいノードは最初のノードと同じネットワークに参加する必要があります。最初のノードが既に1883などのポートを占有しているため、ここではポートマッピングを行いません。

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       --network emqx-net \
       --network-alias node2.emqx.com \
       emqx/emqx-enterprise:@EE_VERSION@
   ```
   
4. 任意のノード上で以下のコマンドを実行し、現在のノードを他のノードに接続してクラスターを作成します。コマンドの詳細は[手動クラスタリング](#manual-clustering)を参照してください。

   ```bash
   docker exec -it emqx2 \
       emqx ctl cluster join emqx@node1.emqx.com
   ```

:::

::: tab 手動クラスタリング（ダッシュボード）

EMQX v5.9.0以降、ダッシュボードから直接クラスターを作成できます。

1. すべてのノードが起動しており、正しい`name@host`、同一のクッキー設定、相互に通信可能なネットワーク環境であることを確認します。

2. 任意のノードの**ダッシュボード**にアクセスします。

3. **管理 > MQTT設定 > クラスター**に移動します。

4. （任意）**クラスター説明**欄にクラスターの目的や環境を識別する説明を入力し、**保存**をクリックします。

   > この機能はEMQX v6.0.0以降で利用可能です。

5. **招待**をクリックし、ノード名（例：`emqx@node2.emqx.com`）を入力して確定します。

6. 招待されたノードは状態を同期後、クラスターに参加します。

詳細はダッシュボードの[クラスター設定](../../dashboard/cluster_settings.md#cluster)を参照してください。

:::

::: tab 自動クラスタリング（静的ノードリスト方式）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 最初のノードを起動し、環境変数でノード名とクラスタリング方法を設定します。

   - `EMQX_NODE_NAME` 環境変数でノード名を設定。
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY` 環境変数でクラスターディスカバリー戦略を設定（ここでは静的クラスタリング）。
   - `EMQX_CLUSTER__STATIC__SEEDS` 環境変数で静的ノードリストを設定。すべてのノード名を含めます。

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
   
3. 最初のノード起動後、2つ目のノードを起動します。クラスタリング方法とノードは最初のノードと同じネットワークに参加する必要があります。最初のノードが既に1883などのポートを占有しているため、ここではポートマッピングを行いません。

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

任意のノードで`emqx ctl cluster status`コマンドを実行し、クラスター状態を確認できます。正常な場合、以下のように表示されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの手順に従い、必要なクラスター作成方法を選択して修正・展開してください。

## 手動クラスタリング

このセクションでは、手動でクラスターを作成する手順を説明します。手動クラスタリングでは、クラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も手動で構築します。自動クラスタリングに比べて、カスタムネットワークトポロジーの細かい調整が可能であり、自動クラスタリングが利用できないまたは適さない状況に適しています。

:::tip 

手動クラスタリングはコアノードにのみ使用可能です。コア-レプリカノードのデプロイ構成を使用している場合は、自動クラスタリングでクラスターを管理してください。

:::

例として、`emqx@node1.emqx.com`と`emqx@node2.emqx.com`の2ノードでクラスターを手動作成する手順は以下の通りです。

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

   - このコマンドはクラスターに参加する側のノードで実行する必要があります。すなわち**リクエスト**であり、**招待**ではありません。
   - `emqx@s2.emqx.io`が`emqx@s1.emqx.io`に参加してクラスターを形成すると、ローカルデータがクリアされ、`emqx@s1.emqx.io`のデータと同期されます。
   - `emqx@s2.emqx.io`が別のクラスターに参加したい場合は、まず現在のクラスターから離脱する必要があります。離脱方法は[クラスターから離脱](#leave-cluster)を参照してください。

   :::

3. 任意のノードでクラスター状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status
   
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

これで2ノードのクラスター作成に成功しました。クラスター状態の監視方法やノード管理方法については、[クラスター状態の照会](#query-cluster-status)、[クラスターのノード管理](#manage-cluster-nodes)、[ネットワークプロトコルの設定](#configure-network-protocols)を参照してください。

EMQX v5.9.0以降はダッシュボードからノードの招待や管理も可能で、より直感的で使いやすくなっています。詳細は[クラスター設定](../../dashboard/cluster_settings.md#cluster)をご覧ください。

## 自動クラスタリング

このセクションでは、さまざまな自動クラスタリング方法によるクラスター作成方法を説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードに事前定義された静的ノードリストを設定し、起動時にそのリストに基づいて自動的にクラスターを形成する方法です。

静的クラスタリングは他のネットワークコンポーネントやサービスに依存せず、最も簡単にEMQXクラスターを自動作成できる方法です。各ノードがTCPプロトコルで相互通信できればクラスターを形成可能です。

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

<!--v5.0.23 e5.0.4 以前は["emqx1", "emqx2"]のみ対応
v5.0.23e5.0.4以降は両方対応-->

- `discovery_strategy` はノードディスカバリー戦略で、`static`に設定。
- `seeds` は配列で、クラスターに参加するノード名を複数カンマ区切りで指定。

すべてのノードを起動すると自動的にクラスターが形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)（ドメインネームシステム）は、ドメイン名の問い合わせに対して対応するIPアドレス（Aレコード）を返します。DNSは1つのドメイン名に複数のAレコード（複数IPアドレス）を設定でき、1つの名前に対して複数のIPアドレスを対応させることが可能です。EMQXのDNS自動クラスタリングはこの1対多の対応を利用し、クラスター内のすべてのノードを特定し、各ノードがクラスターに参加できるようにします。

#### DNSサービスの設定

ほとんどのパブリッククラウドサービスにはDNSサービスがあります。ドメイン名を割り当てた後、そのドメインのAレコードに各EMQXノードのIPアドレスを追加するだけで設定完了です。プライベートクラウドや内部ネットワークにデプロイする場合は、[BIND](https://www.isc.org/bind/)などのDNSソフトウェアで独自のDNSシステムを構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf`の`cluster.dns`設定項目でクラスターに参加するすべてのノードを指定します。

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

- `discovery_strategy` はノードディスカバリー戦略で、`dns`に設定。
- `cluster.dns.name` は問い合わせるDNS名/ドメイン名（例：`localhost`）。
- `cluster.dns.record_type` は列挙型で、`a`または`srv`を指定。

すべてのノードを起動すると自動的にクラスターが形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/)はCoreOSが開始したオープンソースプロジェクトで、分散システムにおけるサービスディスカバリーや接続確立に広く利用されています。これはEMQXの自動クラスタリングに必要な機能と合致します。

ネットワーク内にetcdサーバー（クラスター）を展開すると、EMQXはetcdを介して自動的にクラスターを作成できます。etcdのインストールと設定方法は[etcd Install](https://etcd.io/docs/latest/install/)を参照してください。

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

- `discovery_strategy` はノードディスカバリー戦略で、`etcd`に設定。
- `cluster.etcd.server` はetcdサーバーのアドレス。複数ノードはカンマ区切りで指定可能。
- `cluster.etcd.prefix` はEMQXのサービスディスカバリーに使用するetcdキーのプレフィックス。
- `cluster.etcd.node_ttl` はetcdキーの有効期限を示す期間。デフォルトは`1m`。

設定完了後、EMQXノードを順に起動し、etcdctlツールでetcdサーバーの変化を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

この結果はすべてのノードが正常に起動し、自動的にクラスターに参加したことを示します。

### Kubernetes上での自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/)はKubernetes環境でのEMQXクラスター作成と管理を迅速に行うためのツールで、デプロイと管理作業を低コストでラベル付けされた繰り返し可能なジョブに変換し、クラスターの展開と管理を大幅に簡素化します。

自分でEMQXをデプロイ・管理する場合でも、Kubernetes APIを使ったノードディスカバリーと自動クラスタリングが利用可能です。この機能を使うには、EMQX PodがKubernetesのAPIServerからendpointsリソースを通じてクラスターのノード情報を取得できるようにRBACを作成する必要があります。設定方法は[Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)を参照してください。

Kubernetes上でのEMQX自動クラスタリングは、`emqx.conf`の`cluster.k8s`設定項目で行います。

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

- `discovery_strategy` はノードディスカバリー戦略で、`k8s`に設定。
- `cluster.K8s.apiserver` はKubernetes APIエンドポイントURL。デフォルトは`http://10.110.111.204:8080`。
- `cluster.K8s.service_name` はEMQXサービス名。デフォルトは`emqx`。
- `cluster.K8s.address_type` は検出したノードへの接続に使うアドレス種別。デフォルトは`ip`。選択肢は`ip`、`dns`、`hostname`。
- （オプション）`cluster.K8s.suffix` はノード名のサフィックス。`cluster.K8s.address_type`が`dns`の場合のみ必要。デフォルトは`pod.local`。
- `cluster.K8s.namespace` はKubernetesのネームスペース。文字列型でデフォルトは`default`。

設定後、ノードを順に起動すると自動的にクラスターが形成されます。

::: tip

Kubernetes上でEMQX自動クラスタリングを利用する場合、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の使用を推奨します。

:::

## クラスター管理

クラスター作成後は、クラスター状態の監視やノード管理が可能です。

### クラスター状態の照会

任意のクラスター内ノードで以下のコマンドを実行し、クラスター状態を確認します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスターからの離脱

クラスターから離脱する方法は2つあります。

1. `cluster leave`コマンドを実行：現在のノードがクラスターから離脱します。クラスター内の他ノードに通知し、クラスター運用への参加を停止します。離脱前に進行中のタスクを完了します。
2. `cluster force-leave <node@host>`コマンドを実行：指定したノードを強制的にクラスターから除外します。主にノードが故障または応答しない場合に使用します。

例として、既存クラスターで`emqx@s2.emqx.io`が離脱する場合、`emqx@s2.emqx.io`上で以下を実行します。

```bash
./bin/emqx ctl cluster leave
```

または、`emqx@s1.emqx.io`上で以下を実行し、`emqx@s2.emqx.io`をクラスターから除外します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間のネットワークプロトコルを設定できます。EMQXはTCPまたはTLSでノード間接続をサポートしています。接続方法は`emqx.conf`で設定します。

TCP IPv4およびTCP IPv6を利用する場合、`emqx.conf`の`cluster.proto_dist`で設定します。

- TCP IPv4: `inet_tcp`（デフォルト）
- TCP IPv6: `inet6_tcp`

SSLを有効にするには、まず`cluster.proto_dist`を`inet_tls`に設定し、`etc`フォルダの`ssl_dist.conf`ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)を参照してください。

<!--ここに設定例を追加予定-->

## 疑似分散クラスター

EMQXはテストや開発目的で疑似分散クラスター機能を提供しています。これは単一マシン上で複数のEMQXインスタンスを起動し、それぞれをクラスターのノードとして設定する構成です。

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

次に、以下のコマンドで2つ目のノードを起動し、手動でクラスターに参加させます。ポート競合を避けるため、ノードごとに異なるリスニングポートを使用し、ログファイルや内部データベース用ディレクトリも分ける必要があります。

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

上記は手動でクラスターを作成する例です。自動クラスタリングによる作成方法は[自動クラスタリング](#auto-clustering)を参照してください。

この構成は本番環境での利用は推奨されません。
