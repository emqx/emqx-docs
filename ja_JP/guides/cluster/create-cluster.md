# クラスターの作成と管理

EMQXクラスターは手動または自動で作成できます。本ページでは、手動クラスタリングと自動クラスタリングの両方の方法を紹介し、これら2つの異なるアプローチを用いたEMQXクラスターの作成と管理について案内します。

::: tip 注意

クラスター機能は有効なライセンスキーがある場合にのみ利用可能です。

:::

## 基本概念

EMQXクラスターの基本知識や動作については、[Cluster](../../develop/cluster/introduction.md)および[Architecture](../../develop/cluster/mria-introduction.md)をご参照ください。クラスターを作成するには、以下の概念も理解しておく必要があります。

### ノード名

EMQXのノードは名前で識別されます。すべてのノードは `name@host` 形式の一意のノード名を持ち、hostにはIPアドレスまたは完全修飾ドメイン名（FQDN）を指定します。例：

- サーバー `s1.emqx.io` にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` となります。
- このサーバーに静的IP（`192.168.0.10`）がある場合、ノード名は `emqx@192.168.0.10` となります。

::: tip
EMQXノード名はデータベーススキーマやデータファイルに組み込まれているため変更不可です。したがって、EMQXノード名には静的なFQDNの使用を推奨します。
:::

### ノードディスカバリー

ノードディスカバリーはクラスター作成に必須のプロセスで、個々のEMQXノードが互いを発見し、場所やIPアドレスに関係なく通信できるようにします。

### 手動クラスタリングと自動クラスタリング

ノードディスカバリーストラテジーに基づき、クラスター作成方法は手動クラスタリングと自動クラスタリングに分けられます。

手動クラスタリングは、クラスターに参加するノードを手動で指定してEMQXクラスターを作成する方法です。自動クラスタリングは、複数のEMQXノードが手動設定なしに自動的にクラスターを形成する方法で、クラスターのセットアップを簡素化し、ノードの動的な追加・削除を容易にします。EMQXは静的ノードリスト、DNSレコード、etcd、Kubernetesに基づく自動クラスタリングをサポートしています。

以下の表は、EMQXがサポートするノードディスカバリーストラテジーとクラスター作成方法の一覧です。

| ストラテジー | 説明                                      |
| ------------ | ----------------------------------------- |
| `manual`     | コマンドで手動クラスターを作成            |
| `static`     | 静的ノードリストによる自動クラスタリング  |
| `dns`        | DNSのAレコードおよびSRVレコードによる自動クラスタリング |
| `etcd`       | etcdによる自動クラスタリング              |
| `k8s`        | Kubernetesによる自動クラスタリング        |
| `singleton`  | クラスタリング無効。ノード間の接続をすべて拒否 |

EMQXは[Erlang/OTPアプリケーション向けに開発されたEkkaライブラリ](https://github.com/emqx/ekka)に基づく自動クラスター作成をサポートしています。EkkaはErlangノードの自動ディスカバリー（サービスディスカバリー）や自動クラスタリング（Autocluster）のほか、ネットワークパーティションの自動修復（Network Partition Autoheal）や停止したノードの自動削除（Autoclean）などの機能も実装しています。

クラスタリング方法は `emqx.conf` のノードディスカバリーストラテジー設定で定義できます。デフォルトは手動クラスタリングです。

```bash
cluster {
    ## Options: manual | static | dns | etcd | k8s | singleton
    discovery_strategy  =  manual
}
```

## はじめに

クラスター作成前にノードやネットワーク環境の設定方法について説明します。

### ノード名の設定

クラスターに参加するノード名の付け方を理解しておく必要があります。例えば、`s1.emqx.io` と `s2.emqx.io` にそれぞれ2つのノードをデプロイしクラスターを作成する場合、以下の手順に従います。

1台目のノードの `emqx.conf` にノード名を設定します。

```bash
node.name = emqx@s1.emqx.io
```

または環境変数でノード名を上書きできます。例えば、`docker run` の `-e` オプションや systemd の `emqx.service` ファイルで以下のように設定します。

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

2台目のノードも同様に設定し、クラスターに参加させます。

これでクラスターに参加する2つのノード、`emqx@s1.emqx.io` と `emqx@s2.emqx.io` の名前が決まりました。手動または自動でクラスターを作成できます。

### ノードクッキーの設定

セキュリティ上の理由から、すべてのクラスター参加ノードで `emqx.conf` のデフォルトクッキーをSecretクッキーに変更してください。すべてのノードは同じSecretクッキーを使用する必要があります。magic cookieの詳細は[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)を参照してください。

```
node {
  cookie = "<Secretなクッキー>"
}
```

### ネットワーク環境の設定

ノード間のネットワーク接続が正常に機能していることを確認してください。ノード間にファイアウォールやセキュリティグループがある場合、以下のクラスター内部通信に必要なポートを開放する必要があります。

- **4370**: Erlang分散トランスポートポート
- **5370**: クラスターRPCポート（物理マシン環境向け）
- **5369**: クラスターRPCポート（Docker環境向け）

1台のサーバーに複数のEMQXノードをデプロイする場合、各ノードは異なるクラスター通信ポートを使用します。ファイアウォール設定の詳細は[クラスター内部通信ポート](./security.md)を参照してください。

## クイックスタート

ここでは、Dockerネットワーク上で2つの異なるクラスタリング方法を使ってクラスターを素早く作成する方法を示します。

::: tip

複数の物理マシンにまたがるDocker環境でEMQXを実行しクラスターを形成する場合は、追加の設定が必要です。コンテナ内のクラスター通信ポートをマッピングし、ファイアウォールでこれらのポートを開放してください。詳細は[ネットワーク環境の設定](#configure-network-environment)を参照してください。

:::

:::: tabs type:card

::: tab 手動クラスタリング（コマンド）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセスできます。

   ```bash
   docker network create emqx-net
   ```

2. 1台目のノードを起動し、環境変数でノード名を設定します。EMQXのデフォルトクラスタリング方法は手動なので追加設定は不要です。ノードをDockerネットワークに追加し、ノードホストに対応するネットワークエイリアスを設定します。

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

3. 1台目のノード起動後、2台目のノードを起動します。新しいノードは1台目と同じネットワークに参加する必要があります。1台目がすでに1883などのポートを占有しているため、ここではポートマッピングを行いません。

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

1. すべてのノードが起動し、適切な `name@host`、同一のクッキーを持ち、ネットワークで相互に到達可能であることを確認します。

2. 任意のノードの**ダッシュボード**にアクセスします。

3. **管理 > MQTT設定 > クラスター** に移動します。

4. （任意）**クラスター説明**欄にクラスターの用途や環境を識別する説明を入力し、**保存**をクリックします。

   > この機能はEMQX v6.0.0以降で利用可能です。

5. **招待**をクリックし、ノード名（例：`emqx@node2.emqx.com`）を入力して確定します。

6. 招待されたノードは状態を同期後、クラスターに参加します。

詳細はダッシュボードの[クラスター設定](../dashboard/cluster_settings.md#cluster)を参照してください。

:::

::: tab 自動クラスタリング（静的ノードリスト）

1. ノード間通信のためのDockerネットワークを作成します。同じネットワーク内のコンテナはコンテナ名やネットワークエイリアスで相互アクセスできます。

   ```bash
   docker network create emqx-net
   ```

2. 1台目のノードを起動し、環境変数でノード名とクラスタリング方法を設定します。

   - `EMQX_NODE_NAME` でノード名を設定。
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY` でクラスターディスカバリーストラテジーを設定（ここでは静的クラスタリング）。
   - `EMQX_CLUSTER__STATIC__SEEDS` で静的ノードリストを設定。クラスターに参加するすべてのノード名を含めます。

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

3. 1台目のノード起動後、2台目のノードを起動します。クラスタリング方法と新しいノードは1台目と同じネットワークに参加する必要があります。1台目がすでに1883などのポートを占有しているため、ここではポートマッピングを行いません。

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

任意のノードで `emqx ctl cluster status` コマンドを実行し、クラスター状態を確認します。正常な場合、以下のような情報が出力されます。

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

これで簡単なクラスター作成が完了しました。次に、以下のセクションの指示に従い、必要なクラスター作成方法を選択して修正・デプロイしてください。

## 手動クラスタリング

本セクションでは手動でクラスターを作成する手順を説明します。手動クラスタリングでは、クラスター内の各ノードを手動で設定し、ノード間のネットワーク接続も手動で構築する必要があります。自動クラスタリングと比べてカスタムネットワークトポロジーの細かい調整が可能であり、自動クラスタリングが利用できないまたは適切でない場合に適しています。

:::tip

手動クラスタリングはコアノードにのみ使用可能です。コア-レプリカノード構成の場合は自動クラスタリングを使用してください。

:::

例として、`emqx@node1.emqx.com` と `emqx@node2.emqx.com` の2ノードでクラスターを手動作成する手順は以下の通りです。

1. クラスターのディスカバリーストラテジーを `manual` に設定します。

   ```bash
   cluster {
       ## Options: manual | static | dns | etcd | k8s | singleton
       discovery_strategy  =  manual
   }
   ```

2. 2ノードを起動後、いずれかのノードでクラスタージョインコマンドを実行します。

   ```bash
   $ ./bin/emqx ctl cluster join emqx@node1.emqx.com

   Join the cluster successfully.
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

   :::tip

   - このコマンドはクラスターに参加するノード上で実行する必要があります。つまり、**招待（invite）**ではなく**参加要求（request）**です。
   - `emqx@s2.emqx.io` が `emqx@s1.emqx.io` に参加すると、ローカルデータはクリアされ、`emqx@s1.emqx.io` のデータと同期されます。
   - `emqx@s2.emqx.io` が別のクラスターに参加したい場合、まず現在のクラスターを離脱する必要があります。離脱方法は[クラスター離脱](#leave-cluster)を参照してください。

   :::

3. 任意のノードでクラスター状態を確認します。

   ```bash
   $ ./bin/emqx ctl cluster status

   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

これで2ノードのクラスター作成に成功しました。クラスター状態の監視やノード管理方法については、[クラスター状態の照会](#query-cluster-status)、[クラスターのノード管理](#manage-cluster-nodes)、[ネットワークプロトコルの設定](#configure-network-protocols)を参照してください。

EMQX v5.9.0以降はダッシュボードからノードの招待や管理も可能で、より直感的で使いやすい操作ができます。詳細は[クラスター設定](../dashboard/cluster_settings.md#cluster)をご覧ください。

## 自動クラスタリング

本セクションでは各種自動クラスタリング方法によるクラスター作成方法を説明します。

### 静的ノードリストによる自動クラスタリング

EMQXの静的ノードリストによる自動クラスタリングは、各ノードにあらかじめ定義した静的ノードリストを設定し、起動後にノードリストに従って自動的にクラスターを形成する方法です。

静的クラスタリングは他のネットワークコンポーネントやサービスに依存せず、TCPプロトコルで相互通信が可能な限り簡単にEMQXクラスターを自動作成できます。

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
v5.0.23 e5.0.4 以降は両方対応-->

- `discovery_strategy` はノードディスカバリーストラテジーで、`static` に設定。
- `seeds` は配列で、クラスターに参加するノード名を複数カンマ区切りで追加可能。

すべてのノード起動後、自動的にクラスターが形成されます。

### DNSレコードによる自動クラスタリング

[DNS](https://tools.ietf.org/html/rfc1034)（Domain Name System）は、ドメイン名の問い合わせに対して対応するIPアドレス（Aレコード）を返す仕組みです。DNSは1つのドメイン名に複数のAレコード（複数IPアドレス）を持つことができ、1つの名前が複数IPに対応するマッピングを形成します。EMQXのDNS自動クラスタリングはこの1対多のマッピングを利用し、クラスター内の全ノードを特定し、各ノードがクラスターに参加できるようにします。

#### DNSサービスの設定

ほとんどのパブリッククラウドサービスはDNSサービスを提供しています。ドメイン名を割り当てた後、各EMQXノードのIPアドレスをそのドメインのAレコードに追加すれば設定完了です。プライベートクラウドや内部ネットワークにEMQXをデプロイする場合は、[BIND](https://www.isc.org/bind/)などのDNSソフトウェアで独自DNSシステムを構築する必要があります。

### DNSレコードによる自動クラスタリングの設定

DNSサービスが準備できたら、`emqx.conf` の `cluster.dns` 設定項目でクラスターに参加するノードを追加します。

**例：**

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        ## DNSのAレコードおよびSRVレコードに対応
        record_type = a
    }
}
```

- `discovery_strategy` はノードディスカバリーストラテジーで `dns` に設定。
- `cluster.dns.name` は問い合わせるDNS名／ドメイン名（例：`localhost`）。
- `cluster.dns.record_type` は列挙型で、`a` または `srv` を指定可能。

すべてのノード起動後、自動的にクラスターが形成されます。

### etcdによる自動クラスタリング

[etcd](https://etcd.io/) はCoreOSが開発したオープンソースプロジェクトで、分散システムにおけるサービスディスカバリーや接続確立に広く使われています。EMQXの自動クラスタリングにも適しています。

ネットワーク内にetcdサーバー（クラスター）をデプロイした後、EMQXはetcdを介して自動的にクラスターを作成できます。etcdのインストール・設定方法は[etcd Install](https://etcd.io/docs/latest/install/)を参照してください。

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

- `discovery_strategy` はノードディスカバリーストラテジーで `etcd` に設定。
- `cluster.etcd.server` はetcdサーバーのアドレス。複数ノードはカンマ区切りで指定可能。
- `cluster.etcd.prefix` はEMQXサービスディスカバリーに使うetcdキーのプレフィックス。
- `cluster.etcd.node_ttl` はetcdキーの有効期限（デフォルト1分）。

設定完了後、EMQXノードを順次起動し、etcdctlツールでetcdサーバーの変化を確認できます。

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

結果はすべてのノードが正常に起動し、自動的にクラスターに参加していることを示します。

### Kubernetes上での自動クラスタリング

[EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/) はKubernetes環境でのEMQXクラスター作成・管理を支援し、デプロイ・管理作業をラベル付けされた低コストで繰り返し可能なジョブに変換することで大幅に簡素化します。

自分でEMQXをデプロイ・管理したい場合も、Kubernetes APIを使ったノードディスカバリーと自動クラスタリングが可能です。この機能を使うには、EMQX PodがKubernetes APIServerのendpointsリソースからクラスター情報を取得できるようRBACを作成する必要があります。設定方法は[Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)を参照してください。

Kubernetes上でのEMQX自動クラスタリングは、`emqx.conf` の `cluster.k8s` 設定項目を使用します。

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

- `discovery_strategy` はノードディスカバリーストラテジーで `k8s` に設定。
- `cluster.K8s.apiserver` はKubernetes APIエンドポイントURL（デフォルト `http://10.110.111.204:8080`）。
- `cluster.K8s.service_name` はEMQXサービス名（デフォルト `emqx`）。
- `cluster.K8s.address_type` は発見したノードへ接続するためのアドレスタイプ（デフォルト `ip`）。選択肢は `ip`、`dns`、`hostname`。
- （任意）`cluster.K8s.suffix` はノード名のサフィックス。`address_type` が `dns` の場合のみ必要（デフォルト `pod.local`）。
- `cluster.K8s.namespace` はKubernetesのネームスペース（文字列、デフォルト `default`）。

設定後、ノードを順次起動すると自動的にクラスターが形成されます。

::: tip

Kubernetes上でのEMQX自動クラスタリングには、Fannelプラグインよりも[Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/)の使用を推奨します。

:::

## クラスター管理

クラスター作成後は、クラスター状態の監視やノード管理が可能です。

### クラスター状態の照会

任意のクラスター内ノードで以下のコマンドを実行し、クラスター状態を照会します。

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### クラスター離脱

クラスターから離脱する方法は2つあります。

1. `cluster leave` コマンドを実行：現在のノードがクラスターから離脱します。クラスター内の他ノードに通知し、クラスター操作への参加を停止します。離脱前に進行中のタスクは完了します。
2. `cluster force-leave <node@host>` コマンドを実行：指定ノードを強制的にクラスターから除外します。通常、ノードが故障または応答しない場合に使用します。

例として、先に作成したクラスターで `emqx@s2.emqx.io` が離脱したい場合、`emqx@s2.emqx.io` 上で以下を実行します。

```bash
./bin/emqx ctl cluster leave
```

または、`emqx@s1.emqx.io` 上で以下を実行し、`emqx@s2.emqx.io` をクラスターから除外します。

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### ネットワークプロトコルの設定

クラスター作成後、ノード間のネットワークプロトコル設定を行えます。EMQXはTCPまたはTLSでノード間接続をサポートしており、`emqx.conf` で設定可能です。

TCP IPv4およびTCP IPv6を使用する場合は、`emqx.conf` の `cluster.proto_dist` を設定します。

- TCP IPv4: `inet_tcp`（デフォルト）
- TCP IPv6: `inet6_tcp`

SSLを有効にするには、まず `cluster.proto_dist` を `inet_tls` に設定し、`etc` フォルダー内の `ssl_dist.conf` ファイルでTLS証明書を指定します。詳細は[Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)を参照してください。

<!--ここに設定例があると良い-->

## 疑似分散クラスター

EMQXはテストや開発目的のために疑似分散クラスター機能を提供しています。これは1台のマシン上で複数のEMQXインスタンスを起動し、それぞれをクラスターのノードとして設定する構成です。

1台目のノードを起動します。

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

次に、2台目のノードを起動し手動でクラスターに参加させます。ポート競合を避けるため、ノードごとに異なるリスニングポートを使用し、ログファイルや内部データベース用のディレクトリも分ける必要があります。

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

上記は手動でクラスターを作成する例です。自動クラスタリングの方法は[自動クラスタリング](#auto-clustering)セクションも参照してください。

なお、この構成は本番環境での使用は推奨されません。
