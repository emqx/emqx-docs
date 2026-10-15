# ロードバランサーの設定

ロードバランサー（LB）は複数のネットワークコンポーネント間で負荷を分散し、リソースの最適化を行うことで、過負荷によるシステム障害を回避します。LBはEMQXにおいて必須のコンポーネントではありませんが、以下のような明確なシステム上の利点をもたらします。

- EMQXの負荷を分散し、単一ノードの過負荷を防止する。
- クライアントの設定を簡素化し、クライアントはLBに接続するだけでクラスター内のスケーリングを意識する必要がない。
- TLS/SSL終端によりEMQXクラスターの負荷を軽減する。
- クラスターの前面にLBを配置することで不要なトラフィックをブロックし、悪意のある攻撃からEMQXクラスターを保護し、セキュリティを向上させる。

本セクションでは、EMQXにおけるLBの設定方法について説明します。

## デプロイメントアーキテクチャ

ここでは、3つの異なるロードバランサーのデプロイメントアーキテクチャを紹介します。

### TCPロードバランサー

LBを構成したEMQXクラスターでは、LBが受信したTCPトラフィックを処理し、受け取ったMQTT接続要求やメッセージを異なるEMQXノードに分配します。典型的なデプロイメントアーキテクチャは以下の通りです。

<img src="./assets/lb_2.png" alt="TLS終端" style="zoom:45%;" />

### TLS終端とロードバランサー

SSL/TLSが有効な場合、SSL/TLS接続をLBで終端することを推奨します。つまり、クライアントとLB間の接続はSSL/TLSで保護し、LBとEMQXノード間はTCP接続を用いることで、EMQXクラスターのパフォーマンスを最大化します。アーキテクチャは以下の通りです。

<img src="./assets/lb_3.png" alt="image" style="zoom:50%;" />

### ハイブリッドデプロイメント

クラウドサービスプロバイダーのLBを接続および負荷分散層として利用したいが、TLS終端に対応していない、またはプロキシプロトコルなどの特定のTLS機能が不足している場合は、ハイブリッドデプロイメントを選択できます。具体的には、EMQXの前にHAProxyやNGINXを配置し、SSL/TLS接続を終端します。

EMQXで直接TLS接続を処理する場合と比較して、より高いパフォーマンス効果が期待できます。デプロイメントアーキテクチャは以下の通りです。

<img src="./assets/lb_6.png" alt="EMQXロードバランシングハイブリッドデプロイメント" style="zoom:80%;" />

負荷分散デプロイメントクラスターの他に、DNSラウンドロビンを用いてEMQXクラスターに直接接続する方法もあります。この場合、全ノードをDNSラウンドロビンリストに追加し、デバイスはドメイン名またはIPアドレスリストを介してクラスターにアクセスします。ただし、DNSラウンドロビンは本番環境での使用は一般的に推奨されません。

## 実際のIPおよびTLS証明書情報の取得

LBをデプロイした後、EMQXは通常、クライアントの実際の送信元IPやTLS証明書情報を取得する必要があります。そのためには、LBで[プロキシプロトコル](https://www.haproxy.com/blog/haproxy/proxy-protocol)の設定を有効にするか、実際のIPを取得するための関連設定を有効にする必要があります。

LBでプロキシプロトコルを有効にした場合、EMQXの該当リスナーでも`proxy_protocol`設定を有効にする必要があります。例えば、TCP 1883リスナーの場合、設定ファイルに以下を追加します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000

  proxy_protocol = true
}
```

LBでのプロキシプロトコル有効化方法については、それぞれのLBのドキュメントを参照してください。プロキシプロトコルに対応していないLB製品でも、バックエンドサービスが実際のクライアントIPを取得できる場合があります。LBやクラウドサービスプロバイダーの仕様に応じて適切に設定してください。

### クライアントTLS証明書情報

プロキシプロトコルv2は、ロードバランサーからEMQXのTCPリスナーへクライアントTLS証明書の一部情報（Common Name（CN）やSubjectなど）を転送できます。ただし、証明書のSubject Alternative Names（SANs）は転送されません。

`cert_san.*`の値からクライアント属性を初期化するには、TLS接続がEMQXで終端され、クライアントがEMQXのTLSリスナーに証明書を提示している必要があります。LBでTLSを終端した場合、SAN値はEMQXで利用できません。LBがTLSを終端せずにEMQXにTLS接続を転送する場合は、クライアント証明書がEMQXに提示されるため、EMQXはSANを抽出できます。設定の詳細は[証明書のSubject Alternative Namesからクライアント属性を初期化する](../../develop/client-attributes/client-attributes.md#initialize-client-attributes-from-certificate-subject-alternative-names)を参照してください。

## LB製品の選択

現在、多くのLB製品がオープンソースおよび商用版で提供されており、パブリッククラウドプロバイダーもロードバランシングサービスを提供しています。

パブリッククラウド向けLB製品：

| クラウドプロバイダー                       | SSL終端対応 | プロキシプロトコル対応 | LB製品                                                    |
| ----------------------------------------- | ----------- | ---------------------- | --------------------------------------------------------- |
| [AWS](https://aws.amazon.com)             | 対応        | 対応                   | <https://aws.amazon.com/elasticloadbalancing/?nc1=h_ls>   |
| [Azure](https://azure.microsoft.com)      | 不明        | 不明                   | <https://azure.microsoft.com/en-us/products/load-balancer/> |
| [Google Cloud](https://cloud.google.com/) | 対応        | 対応                   | <https://cloud.google.com/load-balancing>                 |

プライベートクラウド向けLB製品：

| オープンソースLB                      | SSL終端対応 | プロキシプロトコル対応 | ドキュメント/URL                                         |
| ------------------------------------ | ----------- | ---------------------- | ------------------------------------------------------- |
| [HAProxy](https://www.haproxy.org)  | 対応        | 対応                   | <https://www.haproxy.com/solutions/load-balancing.html> |
| [NGINX](https://www.nginx.com)       | 対応        | 対応                   | <https://www.nginx.com/solutions/load-balancing/>       |

以降の2ページでは、プライベートにデプロイしたLBサーバーを例に、EMQXクラスターの設定およびロードバランシング方法を紹介します。

- [NGINXでEMQXクラスターをロードバランスする](./lb-nginx.md)
- [HAProxyでEMQXクラスターをロードバランスする](./lb-haproxy.md)
