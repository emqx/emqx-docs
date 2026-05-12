# クラスター構成

EMQXにおけるクラスターとは、高いスケーラビリティとフォールトトレランスを備えたMQTTメッセージングシステムを提供するために協調動作するEMQXノードのグループです。クラスタリングにより、複数のノードに負荷を分散でき、1つ以上のノードが故障してもシステムが継続して稼働できるようにします。

## ノード名の設定

クラスター作成の前に、EMQXのノード名の概念について理解しましょう。EMQXノードは名前で識別されます。ノード名は、ノード名部分とホスト部分の2つで構成され、`@`で区切られます。例えば、`emqx@s1.emqx.io` のようになります。ホスト部分はIPアドレスか完全修飾ドメイン名（FQDN）でなければなりません。例として：

- サーバー `s1.emqx.io` にデプロイされたEMQXノードの場合、ノード名は `emqx@s1.emqx.io` とします。
- このサーバーが固定IP（`192.168.0.10`）を持つ場合、ノード名は `emqx@192.168.0.10` とします。

`emqx.conf`でノードを設定する場合、以下のように記述します。

```bash
node {
  name = "emqx@s1.emqx.io"
  role = core
}
```

ここで、

- `name` は設定したいノード名を指します。例：`emqx@localhost`。
- `role` はEMQXクラスター内でノードが果たす役割を示します。役割はコアノード（core）とレプリカントノード（replicant）の2種類があります。コアノードとレプリカントノードの詳細は[EMQXクラスタリング - コアノードとレプリカントノード](../../develop/cluster/mria-introduction.md)を参照してください。  
  - デフォルト値：`core`  
  - 選択可能な値：`core` または `replicant`

## クラスターの設定

このセクションでは、EMQXクラスターの設定方法を説明します。クラスター設定はコアノードまたはレプリカントノードのいずれかで行えます。レプリカントノードで設定する場合、例えば `core_nodes` のように特定の条件下でのみ有効になる設定項目があります。

- ノードの `node.db_backend` が `rlog` に設定されていること（`rlog` をデータベースバックエンドとして使用）。
- ノードの `node.role` が `replicant` に設定されていること（レプリカントノードとして機能）。
- ノードの `node.discovery_strategy` が `manual` または `static` に設定されていること。自動クラスター検出機構を使用する場合は設定不要です。ノード検出戦略と関連設定の詳細は[クラスターの作成](../cluster/create-cluster.md)を参照してください。

```bash
cluster {
  name = emqxcl
  discovery_strategy = manual
  core_nodes = []
  driver = tcp
  ssl_options {
    certfile = ""
    keyfile = ""
    cacertfile = ""
  }
}
```

各設定項目の説明は以下の通りです。

| 設定項目                  | 説明                                                         | デフォルト値  | 選択可能な値                                      |
| ------------------------- | ------------------------------------------------------------ | ------------ | ------------------------------------------------- |
| `name`                    | クラスターの名前を設定します                                 | `emqxcl`     |                                                   |
| `discovery_strategy`      | クラスターのノード検出戦略を設定します                       | `manual`     | `manual`, `static`, `dns`, `etcd`, `k8s`, `singleton` |
| `core_nodes`              | レプリカントノードが接続するコアノードを設定します。複数ノードはカンマ区切りで指定可能 | --           | --                                                |
| `driver`                  | EMQXノード間通信のトランスポートプロトコルを設定します       | `tcp`        | `tcp`, `SSL`                                      |
| `ssl_options`             | リスナーのSSL/TLS設定オプションで、3つのプロパティを持ちます | --           | --                                                |
| `ssl_options.cacertfile`  | クライアント証明書の真正性を検証するための信頼されたCA証明書（PEM形式）ファイル | --           | --                                                |
| `ssl_options.certfile`    | リスナー用のSSL/TLS証明書チェーン（PEM形式）ファイル。ルートCA以外の証明書の場合は中間CA証明書を連結してチェーンを形成 | --           | --                                                |
| `ssl_options.keyfile`     | SSL/TLS証明書に対応する秘密鍵（PEM形式）ファイル             | --           | --                                                |
| `ssl_options.fail_if_no_peer_cert` | `true`の場合、クライアントが証明書を送信しない（空の証明書）とサーバーは失敗します。`false`の場合は無効な証明書送信時のみ失敗します（空の証明書は有効とみなす） | --           | --                                                |

::: tip

EMQXはより詳細なカスタマイズに対応するため、多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご覧ください。

:::
