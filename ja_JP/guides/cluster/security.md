# クラスターセキュリティ

EMQXは、データの機密性、完全性、および可用性を確保するために、ノードレベルでの[認証](../access-control/authn/authn.md)および[認可](../access-control/authz/authz.md)機構、クラスター内のノード間通信を安全にするための[シークレットクッキー](#set-node-cookie)、およびノード間トラフィックのエンドツーエンド暗号化を提供する[TLS/SSL暗号化](#configure-tls-ssl-to-secure-cluster-connections)など、複数のセキュリティ機構を提供しています。

ファイアウォールルールのプロビジョニングを支援するために、EMQXのブローカー間[通信ポートマッピングルール](#port-mapping)についても説明します。

## ノードクッキーの設定

セキュリティ上の理由から、クラスターに参加するすべてのノードの`emqx.conf`でデフォルトのクッキー設定をシークレットクッキーに変更する必要があります。

注意：クラスターに参加するすべてのノードは同じシークレットクッキーを使用する必要があります。使用されるマジッククッキーの詳細については、[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)をご覧ください。

```
node {
  cookie = "<シークレットクッキー>"
}
```

:::tip ヒント

クラスターに参加するすべてのノードは同じセキュリティクッキーを使用する必要があります。使用されるマジッククッキーの詳細については、[Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security)をご覧ください。

:::

## クラスター接続を保護するためのTLS/SSLの設定

EMQXは、ノード間の通信チャネルをTLSで保護し、交換されるデータの機密性、完全性、および真正性を守ることもサポートしています。TLSはCPU負荷とRAM使用量の増加を伴うため、ビジネスニーズに応じて設定してください。

このセクションでは、EMQXクラスターでTLSを設定する方法を紹介します。SSL/TLS証明書の取得方法については、[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)をご覧ください。

## クラスターRPC接続にTLS/SSLを使用する

クラスターRPCにTLS/SSLを設定するには、以下の設定項目を`emqx.conf`に設定してください。

```
rpc {
  driver = ssl
  # リスナーがクラスターのピアの真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEM形式のファイル。
  cacertfile = "/path/to/cert/ca.pem"
  # リスナーのSSL/TLS証明書チェーンを含むPEM形式のファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に追加してチェーンを形成する必要があります。
  certfile = "/path/to/cert/domain.pem"
  # SSL/TLS証明書に対応する秘密鍵を含むPEM形式のファイル。
  keyfile = "/path/to/cert/domain.key"
  # クライアント証明書の真正性を検証する場合は'verify_peer'、そうでなければ'verify_none'に設定。
  verify = verify_peer
  # trueに設定すると、ピアが証明書を送信しない（空の証明書を送信）場合にハンドシェイクが失敗します。falseの場合は、ピアが無効な証明書を送信した場合のみ失敗します（空の証明書は有効とみなされます）。
  fail_if_no_peer_cert = true
}
```

### Erlang DistributionにTLS/SSLを使用する

EMQXコアノードは、データベースの更新同期やクラスター内のノード管理（コンポーネントの起動/停止やランタイムメトリクスの収集など）にErlang Distributionを使用しています。

* `etc/ssl_dist.conf`ファイルに正しい鍵および証明書のパスが設定されていることを確認してください。
* 設定`cluster.proto_dist`が`inet_tls`に設定されていることを確認してください。

## ポートマッピング

クラスターのポートを内部に限定し、ファイアウォールルール（例： [AWSセキュリティグループ](https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html) や [iptables](https://en.wikipedia.org/wiki/Iptables)）を設定することは良いプラクティスです。クラスターノード間にファイアウォールがある場合、他のノードが到達できるように従来のリスニングポートを許可する必要があります。このセクションでは、ファイアウォールルールが正しく設定され、EMQXノードが相互に接続できる一方で外部からの不正アクセスを防止するためのポートマッピングルールを紹介します。

EMQXはクラスター通信の信頼性と効率性を確保するためにポートマッピングルールを使用しています。EMQXノードは、Erlang DistributionポートとクラスターRPCポートという2つの異なるチャネルを通じて通信します。

| チャネル                       | 説明                                                         | デフォルトポート                                   |
| ----------------------------- | ------------------------------------------------------------ | -------------------------------------------------- |
| **Erlang Distributionポート** | ノード間通信に使用                                           | `4370`                                             |
| **クラスターRPCポート**       | ノードの管理タスク（ノードの参加や離脱など）に使用          | `5370` または<br />Dockerデプロイ時は `5369`      |

EMQXはErlang DistributionポートとクラスターRPCポートに同じポートマッピングルールを適用しており、以下の式で計算されます。

```
ListeningPort = BasePort + Offset
```

オフセットはノード名の数値サフィックスに基づいて計算されます。ノード名に数値サフィックスがない場合、オフセットは0に設定されます。例：

- ノード`emqx@192.168.0.12`は数値サフィックスがないため、Erlang Distributionポートは`4370`（クラスターRPCポートは`5370`）になります。
- ノード`emqx1@192.168.0.12`は数値サフィックスが1のため、ポートは`4371`（クラスターRPCポートは`5371`）になります。
