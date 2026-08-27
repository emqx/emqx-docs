# クラスターの作成と管理

EMQXクラスターは手動または自動で作成できます。本ページでは、手動クラスタリングと自動クラスタリングの両方の方法を紹介し、これら2つの異なるアプローチでのEMQXクラスターの作成と管理方法を案内します。

::: tip 注意

クラスター機能は有効なライセンスキーがある場合のみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識や動作については、[Cluster](./introduction.md)および[Architecture](./mria-introduction.md)をご参照ください。クラスターを作成するには、以下の概念も理解しておく必要があります。

### ノード名

EMQXのノードは名前で識別されます。すべてのノードは `name@host` 形式の一意なノード名を持ち、`host` はIPアドレスまたは完全修飾ドメイン名（FQDN）である必要があります。例：

- サーバー `s1.emqx.io` にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` とします。
- このサーバーが固定IP（`192.168.0.10`）を持つ場合、ノード名は `emqx@192.168.0.10` とします。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更できません。そのため、EMQXノード名には静的なFQDNの使用を推奨します。
:::

### ノードディスカバリー

ノードディスカバリーはクラスター作成に必須のプロセスで、個々のEMQXノードが互いを発見し、場所やIPアドレスに関わらず通信できるようにします。

### 手動クラスタリングと自動クラスタリング

ノードディスカバリーの戦略に基づき、クラスター作成方法は手動クラスタリングと自動クラスタリングに分けられます。

手動クラスタリングは、どのノードをクラスターに含めるかを手動で指定してEMQXクラスターを作成する方法です。一方、自動クラスタリングは複数のEMQXノードが手動設定なしで自動的にクラスターを形成する方法で、クラスターの設定を簡素化し、ノードの動的な追加・削除を容易にします。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesに基づく自動クラスタリングをサポートしています。

以下の表は、EMQXがサポートするノードディスカバリー戦略とクラスター作成方法を示します。

| 戦略           | 説明                                                         |
| -------------- | ------------------------------------------------------------ |
| `manual`       | コマンドで手動によりクラスターを作成                         |
| `static`       | 静的ノードリストによる自動クラスタリング                     |
| `dns`          | DNSのAレコードおよびSRVレコードによる自動クラスタリング      |
| `etcd`         | etcdによる自動クラスタリング                                 |
| `k8s`          | Kubernetesによる自動クラスタリング                           |
| `singleton`    | クラスタリング無効化。ノードは他ノードとの接続をすべて拒否。 |

EMQXは[Erlang/OTPアプリケーション向けに開発されたクラスター管理ライブラリEkka](https://github.com/emqx/ekka)に基づく自動クラスター作成をサポートしています。EkkaはErlangノードの自動発見（サービスディスカバリー）や自動クラスタリング（オートクラスタリング）に加え、ネットワーク分断の自動回復（Network Partition Autoheal）やダウンノードの自動削除（Autoclean）などの機能も実装しています。

クラスタリング方法は `emqx.conf` のノードディスカバリー戦略を設定することで定義できます。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめる前に

クラスター作成前にノードとネットワーク環境の設定方法について説明します。

### ノード名の設定

クラスターに参加するノードの名前付け方法を理解しておく必要があります。例えば、`s1.emqx.io` と `s2.emqx.io` にそれぞれ2つのノードをデプロイしてクラスターを作成する場合、以下の手順に従います。

1つ目のノードの `emqx.conf` にノード名を設定します。

```bash
node.name = emqx@s1.emqx.io
```

環境変数でノード名を上書きすることも可能です。例えば、`docker run` の `-e` オプションや systemd の `emqx.service` ファイルで以下のように設定します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

他のノードも同様に設定し、クラスターに参加させます。

これでクラスターに参加する2つのノード名 `emqx@s1.emqx.io` と `emqx@s2.emqx.io` が設定されました。手動または自動でクラスターを作成できます。

### ノードクッキーの設定

セキュリティ上の理由から、すべてのクラスター参加ノードで `emqx.conf` のデフォルトクッキーをSecretクッキーに変更してください。すべてのノードは同一のSecretクッキーを使用する必要があります。使用されるマジッククッキーの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)を参照してください。

```
node {
  cookie = "<Secretなクッキー>"
}
```

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合は、クラスター内部通信に必要な以下のポートを開放してください。

- **4370**: Erlang分散トランスポートポート
- **5370**: クラスターRPCポート（物理マシン環境向け）
- **5369**: クラスターRPCポート（Docker環境向け）

1台のサーバーに複数のEMQXノードをデプロイする場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[Intra-cluster communication port](./security.md)を参照してください。

## クイックスタート

このセクションでは、Dockerネットワーク上で2つの異なるクラスタリング方法を使ってクラスターを素早く作成する方法を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを実行しクラスターを形成する場合は、追加の設定が必要です。コンテナ内のクラスター通信ポートのマッピングとファイアウォールでのポート開放については[Configure Network Environment](#configure-network-environment)を参照してください。

:::

:::: tabs type:card

::: tab 手動クラスタリング（コマンド）

1. ノード間通信のためDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

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
   
3. 1つ目のノード起動後、2つ目のノードを起動します。新しいノードは1つ目のノードと同じネットワークに参加する必要があります。1つ目のノードが既に1883などのポートを占有しているため、ここではポートマッピングを行いません。

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

1. すべてのノードが起動しており、適切な `name@host`、同一のクッキー、相互に通信可能であることを確認します。

2. 任意のノードの**ダッシュボード**にアクセスします。

3. **管理 > MQTT設定 > クラスター** に移動します。

4. （任意）**クラスター説明**欄にクラスターの目的や環境を識別する説明を入力し、**保存**をクリックします。

   > この機能はEMQX v6.0.0以降で利用可能です。

5. **招待**をクリックし、ノード名（例：`emqx@node2.emqx.com`）を入力して確定します。

6. 招待されたノードは状態同期後にクラスターに参加します。

詳細はダッシュボードの[クラスター設定](../../dashboard/cluster_settings.md#cluster)を参照してください。

:::

::: tab 自動クラスタリング（静的ノードリスト方式）

1. ノード間通信のためDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセス可能です。

   ```bash
   docker network create emqx-net
   ```

2. 1つ目のノードを起動し、環境変数でノード名とクラスタリング方法を設定します。

   - `EMQX_NODE_NAME` でノード名を設定。
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY` でクラスターディスカバリー戦略を設定（ここでは静的クラスタリング）。
   - `EMQX_CLUSTER__STATIC__SEEDS` で静的ノードリストを設定。すべてのノード名を含める必要があります。

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
   
3. 1つ目のノード起動後、2つ目のノードを起動します。クラスタリング方法は同じで、新しいノードも1つ目のノードと同じネットワークに参加します。1つ目のノードが既に1883などのポートを占有しているため、ここではポートマッピングを行いません。

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

任意のノード上で `emqx ctl cluster status` コマンドを実行しクラスター状態を確認します。正常な場合、以下のような情報が出力されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの指示に従い、必要なクラスター作成方法を選択して修正・デプロイしてください。

## 手動クラスタリング

このセクションでは手動でクラスターを作成する手順を説明します。手動クラスタリングでは、クラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も構築する必要があります。自動クラスタリングと比べて、カスタムネットワークトポロジーの細かな調整が可能であり、自動クラスタリングが利用できない、または適さない状況に適しています。

:::tip 

手動クラスタリングはコアノードのみで使用可能です。コア-レプリカノード構成の場合は自動クラスタリングで管理してください。

:::

例として、2つのノード `emqx@node1.emqx.com` と `emqx@node2.emqx.com` があるとします。以下の手順で手動クラスターを作成できます。

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

   - このコマンドはクラスターに参加するノード上で実行する必要があります。つまり**招待**ではなく**参加要求**です。
   - `emqx@s2.emqx.io` が `emqx@s1.emqx.io` に参加すると、ローカルデータはクリアされ、`emqx@s1.emqx.io` のデータと同期されます。
   - `emqx@s2.emqx.io` が別のクラスターに参加したい場合は、まず現在のクラスターから離脱する必要があります。離脱方法は[Leave Cluster](#leave-cluster)を参照してください。

   :::

3. 任意のノードでクラスターの状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status
   
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

2ノードでクラスターを正常に作成できました。次に[Query Cluster Status](#query-cluster-status)、[Manage Cluster Nodes](#manage-cluster-nodes)、[Configure Network Protocols](#configure-network-protocols)のセクションを参照し、クラスターの監視や管理方法を学んでください。

EMQX v5.9.0以降はダッシュボードからノードの招待や管理も可能で、より直感的で使いやすい操作ができます。詳細は[クラスター設定](../../dashboard/cluster_settings.md#cluster)を参照してください。

## 自動クラスタリング

このセクションでは、さまざまな自動クラスタリング方法によるクラスターの自動作成について説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードに事前定義された静的ノードリストを設定し、起動後にノードリストに基づいて自動的にクラスターを形成する方法です。

静的クラスタリングは、他のネットワークコンポーネントやサービスに依存せず、TCPプロトコルで相互通信可能なノード群であれば簡単にEMQXクラスターを自動作成できます。

この機能を有効にするには、`emqx.conf` にクラスターのモードとノードリストを設定します。

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
v5.0.23以降は両方対応-->

- `discovery_strategy` はノードディスカバリー戦略で、`static` に設定。
- `seeds` は配列で、クラスターに参加するノード名を複数カンマ区切りで追加可能。

すべてのノードが起動すると、自動的にクラスターが形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)（Domain Name System）は、ドメイン名の問い合わせに対して対応するIPアドレス（Aレコード）を返す仕組みです。DNSでは1つのドメイン名に複数のAレコード（複数IPアドレス）を設定可能で、1つの名前に対して複数のIPアドレスのマッピングを形成します。EMQXのDNS自動クラスタリングはこの1対多のマッピングを利用し、クラスター内のすべてのノードを特定し、各ノードがクラスターに参加できるようにします。

#### DNSサービスの設定

ほとんどのパブリッククラウドサービスはDNSサービスを提供しています。ドメイン名を割り当てた後、各EMQXノードのIPアドレスをこのドメインのAレコードに追加すれば設定完了です。プライベートクラウドや内部ネットワークでEMQXをデプロイする場合は、[BIND](https://www.isc.org/bind/)などのソフトウェアで独自のDNSシステムを構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf` の `cluster.dns` 設定項目でクラスターに参加するすべてのノードを追加します。

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

- `discovery_strategy` はノードディスカバリー戦略で、`dns` に設定。
- `cluster.dns.name` は問い合わせるDNS名・ドメイン名の文字列（例：`localhost`）。
- `cluster.dns.record_type` は列挙型で、`a` または `srv` のいずれか。

すべてのノードが起動すると、自動的にクラスターが形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/) はCoreOSが開発したオープンソースプロジェクトで、分散システムのサービスディスカバリーや接続確立に広く利用されています。EMQXの自動クラスタリングに最適です。

ネットワーク内にetcdサーバー（クラスター）を構築した後、EMQXはetcdを介して自動的にクラスターを作成できます。etcdのインストールや設定方法は[etcd Install](https://etcd.io/docs/latest/install/)を参照してください。

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

- `discovery_strategy` はノードディスカバリー戦略で、`etcd` に設定。
- `cluster.etcd.server` はetcdサーバーのアドレス。複数ノードはカンマ区切りで指定可能。
- `cluster.etcd.prefix` はEMQXサービスディスカバリーに使用するetcdのキー接頭辞。
- `cluster.etcd.node_ttl` はetcdキーの有効期限（デフォルト1分）。

設定完了後、EMQXノードを順次起動し、etcdctlツールでetcdサーバーの状態を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

結果はすべてのノードが正常に起動し、自動的にクラスターに参加したことを示しています。

### Kubernetes上での自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/) はKubernetes環境でのEMQXクラスターの作成と管理を迅速化し、デプロイや管理作業を低コストでラベル付け可能な繰り返し作業に変換します。

自分でEMQXをデプロイ・管理したい場合は、Kubernetes APIを使ったノードディスカバリーと自動クラスタリングも利用可能です。この機能を使うには、EMQX PodがKubernetes APIサーバーからエンドポイントリソースを通じてクラスターのノード情報を取得できるようにRBACを作成する必要があります。設定方法は[Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)を参照してください。

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

- `discovery_strategy` はノードディスカバリー戦略で、`k8s` に設定。
- `cluster.K8s.apiserver` はKubernetes APIエンドポイントURL（デフォルト: `http://10.110.111.204:8080`）。
- `cluster.K8s.service_name` はEMQXサービス名（デフォルト: `emqx`）。
- `cluster.K8s.address_type` は検出したノードへの接続に使うアドレス種別（デフォルト: `ip`）。選択肢は `ip`、`dns`、`hostname`。
- （オプション）`cluster.K8s.suffix` はノード名のサフィックス。`cluster.K8s.address_type` が `dns` の場合のみ必要（デフォルト: `pod.local`）。
- `cluster.K8s.namespace` はKubernetesのネームスペース（文字列、デフォルト: `default`）。

設定後、ノードを順次起動すると自動的にクラスターが形成されます。

::: tip

Kubernetes上でEMQX自動クラスタリングを使用する場合は、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の使用を推奨します。

:::

## クラスターの管理

クラスター作成後は、クラスターの状態監視やノード管理が可能です。

### クラスター状態の確認

任意のクラスター内ノードで以下のコマンドを実行し、クラスター状態を確認します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスターからの離脱

クラスターから離脱する方法は2つあります。

1. `cluster leave` コマンドを実行：現在のノードがクラスターから離脱します。クラスター内の他ノードに通知し、クラスター運用から外れます。離脱前に進行中の処理は完了します。
2. `cluster force-leave <node@host>` コマンドを実行：指定ノードをクラスターから強制的に削除します。通常、ノードが故障または応答しない場合に使用します。

例として、既存クラスターで `emqx@s2.emqx.io` が離脱する場合、`emqx@s2.emqx.io` 上で以下を実行します。

```bash
./bin/emqx ctl cluster leave
```

または、`emqx@s1.emqx.io` 上で以下を実行し、`emqx@s2.emqx.io` をクラスターから削除します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間のネットワークプロトコルを設定できます。EMQXはTCPまたはTLSでノード間接続をサポートしています。接続方法は `emqx.conf` で設定します。

TCP IPv4およびTCP IPv6を使用する場合は、`emqx.conf` の `cluster.proto_dist` を設定します。

- TCP IPv4: `inet_tcp` （デフォルト）
- TCP IPv6: `inet6_tcp`

SSLを有効にする場合は、まず `cluster.proto_dist` を `inet_tls` に設定し、`etc` フォルダ内の `ssl_dist.conf` ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)を参照してください。

<!--ここに例コードが必要-->

## 疑似分散クラスター

EMQXはテストや開発目的で疑似分散クラスター機能を提供しています。これは1台のマシン上で複数のEMQXインスタンスを実行し、それぞれをクラスターのノードとして設定する構成を指します。

1つ目のノードを起動：

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

次に2つ目のノードを起動し、手動でクラスターに参加させます。ポート競合を避けるため、異なるノードで異なるリスニングポートを使用し、ログファイルや内部データベース用のディレクトリも分ける必要があります。

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

この構成は本番環境には推奨されません。
