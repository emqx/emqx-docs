# ネットワークとTLS

IoTシナリオにおけるエンドツーエンドの暗号化通信にはセキュリティが不可欠です。Secure Sockets Layer（SSL）およびTransport Layer Security（TLS）プロトコルは、ネットワーク通信においてデータ伝送の機密性を確保し、攻撃者による傍受や改ざんを防ぐために広く採用されています。SSL/TLS暗号化機能はトランスポート層でネットワーク接続を暗号化し、関係者の身元を認証するためにデジタル証明書を使用し、安全な通信チャネルを確立します。

EMQXは以下の場合に安全なネットワーク通信を実現するためにSSLおよびTLS暗号プロトコルを採用しています。

- MQTTクライアントとEMQX間の接続確立時
- データベースなど外部リソースへの接続時
- クラスター内の異なるEMQXノード間の通信時

EMQXは一方向／双方向認証やX.509証明書認証を含むSSL/TLS機能を包括的にサポートしています。

## クライアント接続のTLS

本章の[Enable SSL/TLS Connection](./emqx-mqtt-tls.md)では、MQTTクライアントとEMQX間のSSL/TLS接続の有効化方法を詳細に解説しています。[Obtain SSL/TLS Certificates](./tls-certificate.md)ページでは自己署名証明書の作成手順を案内しています。SSL/TLSのセキュリティをさらに強化するために、証明書検証用の[CRLチェック](./crl.md)やSSL/TLS証明書の失効状況確認のための[OCSPスタップリング](./ocsp.md)も有効化可能です。[Client TLS](./mqtt-client-tls.md)セクションにはサンプルMQTTクライアントコードとプロジェクトがあり、TLS使用ガイドも含まれています。

## 外部リソースアクセスのTLS

EMQXは外部リソースにアクセスする際にもTLSを有効化するオプションを提供しています。例えば、HTTPS経由でのウェブサーバーアクセス時のパスワード認証や、データ統合のためのデータベース接続時などです。EMQXダッシュボードでこれらの機能を設定する際に**Enable TLS**をオンにできます。

- **SNI**はServer Name Indicationの略で、サーバーのドメイン名と証明書が一致しているかどうかを示します。null値は検証なしを意味します。
- サーバーがクライアント証明書の検証を必要とする場合は、**TLS Cert**と**TLS Key**の入力が必須です。
- **TLS Verify**を有効にすると、サーバー証明書の正当性を検証するために**CA Cert**フィールドの入力が必要です。

<img src="./assets/enable-TLS-dashboard.png" alt="TLS有効化ダッシュボード" style="zoom:50%;" />

また、設定ファイルで機能を設定する際に`ssl`オプションを追加することも可能です。例えば、設定ファイルの`authentication`グループに以下のように記述できます。

```bash
authentication {
  url = "https://127.0.0.1:8080"
  backend = "http"

  ...

  ssl {
    enable = true
    # HTTPクライアントがHTTPサーバーの信頼性を検証するために使用する信頼されたCA（認証局）証明書を含むPEM形式ファイル。
    cacertfile = "etc/certs/cacert.pem"
    # HTTPクライアントが送信するSSL/TLS証明書チェーンを含むPEM形式ファイル。証明書がルートCAから直接発行されていない場合は、中間CA証明書をリスナー証明書の後に連結してチェーンを形成する必要があります。
    certfile = "etc/certs/cert.pem"
    # 証明書に対応する秘密鍵を含むPEM形式ファイル
    keyfile = "etc/certs/key.pem"
    ## サーバーの証明書チェーンの正当性を検証する場合は'verify_peer'、検証しない場合は'verify_none'を設定
    verify = verify_peer
  }
}
```

## ノード間通信のTLS

クラスター接続におけるSSL/TLSの有効化方法については本章では扱っておらず、詳細は[Cluster Security](../cluster/security.md)をご参照ください。

## IPv6サポート

EMQXはクライアント接続、ダッシュボード、ノード間クラスター通信、外部サービスへのアウトバウンド接続においてIPv6を完全にサポートしています。設定の詳細は[IPv6](./ipv6.md)をご覧ください。
