# ロードバランサーの設定

ロードバランサー（LB）は複数のネットワークコンポーネント間で負荷を分散し、リソースの最適化を行うことで、過負荷によるシステム障害を回避します。LBはEMQXにおいて必須のコンポーネントではありませんが、以下のような明確なシステム上の利点をもたらします。

- EMQXの負荷を分散し、単一ノードの過負荷を防止する  
- クライアント設定を簡素化し、クライアントはLBにのみ接続すればよく、クラスター内のスケーリングを意識する必要がない  
- TLS/SSL終端によりEMQXクラスターの負荷を軽減する  
- クラスターの前段にLBを配置することで不要なトラフィックを遮断し、悪意ある攻撃からEMQXクラスターを保護しセキュリティを向上させる  

本節では、EMQXにおけるLBの設定方法を紹介します。

## デプロイメントアーキテクチャ

ここでは、3種類のロードバランサーのデプロイメントアーキテクチャを紹介します。

### TCPロードバランサー

LBを設定したEMQXクラスターでは、LBが受信したTCPトラフィックを処理し、MQTT接続要求やメッセージを異なるEMQXノードに振り分けます。典型的なデプロイメントアーキテクチャは以下の通りです。

<img src="./assets/lb_2.png" alt="TLS終端" style="zoom:45%;" />

### TLS終端とロードバランサー

SSL/TLSが有効な場合は、LBでSSL/TLS接続を終端することを推奨します。つまり、クライアントとLB間はSSL/TLSで保護し、LBとEMQXノード間はTCP接続を用いることで、EMQXクラスターのパフォーマンスを最大化します。アーキテクチャは以下の通りです。

<img src="./assets/lb_3.png" alt="image" style="zoom:50%;" />

### ハイブリッドデプロイメント

クラウドサービスプロバイダーのLBを接続および負荷分散層として利用したいが、TLS終端をサポートしていない、またはプロキシプロトコルなどのTLS機能が不足している場合は、ハイブリッドデプロイメントを選択できます。EMQXの前段にHAProxyやNGINXを配置し、SSL/TLS接続を終端します。

EMQXで直接TLS接続を処理するよりも高いパフォーマンスを得られる場合があります。デプロイメントアーキテクチャは以下の通りです。

<img src="./assets/lb_6.png" alt="EMQXロードバランシングハイブリッドデプロイメント" style="zoom:80%;" />

負荷分散デプロイメントクラスターのほかに、DNSラウンドロビンでEMQXクラスターに直接接続する方法もあります。これはすべてのノードをDNSラウンドロビンリストに追加し、デバイスがドメイン名またはIPアドレスリスト経由でクラスターにアクセスする方法です。ただし、本番環境でのDNSラウンドロビンの利用は一般的に推奨されません。

## 実際のIPおよびTLS証明書情報の取得

LBを導入した後、EMQXは通常クライアントの実際の送信元IPやTLS証明書情報を取得する必要があります。そのためには、LBで[Proxy Protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol)の設定を有効にするか、実IPを取得するための関連設定を有効にしてください。

LBでProxy Protocolを有効にした場合、EMQXの該当リスナーでも`proxy_protocol`設定を有効にする必要があります。例えば、TCP 1883リスナーの場合、設定ファイルに以下を追加します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000

  proxy_protocol = true
}
```

LBでProxy Protocolを有効にする方法については、各LBのドキュメントを参照してください。Proxy ProtocolをサポートしないLB製品でも、バックエンドサービスが実際のクライアントIPを取得できる場合があります。LBやクラウドサービスプロバイダーの仕様に応じて適切に設定してください。

### クライアントTLS証明書情報

クライアント証明書情報（Common Name（CN）やSubjectなど）を転送できるのはProxy Protocol v2のみです。ロードバランサーがクライアント証明書情報をTCPリスナーに送信する場合は、Proxy Protocol v2を使用していることを確認してください。

## LB製品の選択

現在、多くのLB製品が存在し、オープンソース版や商用版、またパブリッククラウドプロバイダーのロードバランシングサービスも利用可能です。

パブリッククラウド向けLB製品：

| クラウドプロバイダー                      | SSL終端対応 | Proxy Protocol対応 | LB製品                                                    |
| ----------------------------------------- | ----------- | ------------------ | --------------------------------------------------------- |
| [AWS](https://aws.amazon.com)             | 対応        | 対応               | <https://aws.amazon.com/elasticloadbalancing/?nc1=h_ls>   |
| [Azure](https://azure.microsoft.com)      | 不明        | 不明               | <https://azure.microsoft.com/en-us/products/load-balancer/> |
| [Google Cloud](https://cloud.google.com/) | 対応        | 対応               | <https://cloud.google.com/load-balancing>                 |

プライベートクラウド向けLB製品：

| オープンソースLB                     | SSL終端対応 | Proxy Protocol対応 | ドキュメント/URL                                         |
| ---------------------------------- | ----------- | ------------------ | ------------------------------------------------------- |
| [HAProxy](https://www.haproxy.org) | 対応        | 対応               | <https://www.haproxy.com/solutions/load-balancing.html> |
| [NGINX](https://www.nginx.com)     | 対応        | 対応               | <https://www.nginx.com/solutions/load-balancing/>       |

以下の2ページでは、プライベートにデプロイしたLBサーバーを例に、EMQXクラスターの設定およびロードバランシング方法を紹介します。

- [NGINXによるEMQXクラスターのロードバランス](./lb-nginx.md)  
- [HAProxyによるEMQXクラスターのロードバランス](./lb-haproxy.md)
