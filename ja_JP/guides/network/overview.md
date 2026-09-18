# ネットワークとTLS

IoTシナリオにおけるエンドツーエンドの暗号化通信にはセキュリティが不可欠です。Secure Sockets Layer（SSL）およびTransport Layer Security（TLS）プロトコルは、データ伝送が機密性を保ち、攻撃者による傍受や改ざんを防ぐためにネットワーク通信でよく採用されます。SSL/TLS暗号化機能はトランスポート層でネットワーク接続を暗号化し、デジタル証明書を用いて通信当事者の認証と安全な通信チャネルの確立を行います。

EMQXは以下の場合に、安全なネットワーク通信を確保するためにSSLおよびTLS暗号化プロトコルを採用しています。

- MQTTクライアントとEMQX間の接続確立時
- データベースなど外部リソースへの接続時
- クラスター内の異なるEMQXノード間の通信時

EMQXは片方向／双方向認証やX.509証明書認証を含む、SSL/TLS機能を包括的にサポートしています。

## クライアント接続のためのTLS

本章の[Enable SSL/TLS Connection](./emqx-mqtt-tls.md)では、MQTTクライアントとEMQX間でのSSL/TLS接続の有効化方法を詳細に解説しています。[Obtain SSL/TLS Certificates](./tls-certificate.md)ページでは自己署名証明書の作成手順を案内しています。SSL/TLSを有効にしたセキュリティ強化のために、証明書検証用の[CRLチェック](./crl.md)やSSL/TLS証明書の失効状態確認用の[OCSPスタップリング](./ocsp.md)も有効化可能です。[Client TLS](./mqtt-client-tls.md)セクションには、サンプルMQTTクライアントコードとプロジェクトが含まれており、TLS利用ガイドも提供しています。

## 外部リソースアクセスのためのTLS

EMQXは外部リソースアクセス時にTLSを有効化するオプションも提供しています。例えば、HTTPS経由でのWebサーバーアクセスによるパスワード認証や、データ統合のためのデータベース接続時などです。EMQXダッシュボードでこれらの機能を設定する際に、**Enable TLS**をオンにできます。

- **SNI**はServer Name Indicationの略で、サーバーのドメイン名と証明書が一致しているかを示します。null値は検証なしを意味します。
- サーバーがクライアント証明書を検証する必要がある場合は、**TLS Cert**と**TLS Key**の入力が必須です。
- **TLS Verify**を有効にした場合は、サーバー証明書の正当性を検証するために**CA Cert**の入力が必須です。

<img src="./assets/enable-TLS-dashboard.png" alt="TLS有効化ダッシュボード" style="zoom:50%;" />

また、設定ファイルで機能を構成する際に`ssl`オプションを追加することもできます。例えば、設定ファイルの`authentication`グループに以下の記述を追加して設定可能です。

```bash
authentication {
  url = "https://127.0.0.1:8080"
  backend = "http"

  ...

  ssl {
    enable = true
    # HTTPクライアントがHTTPサーバーの正当性を検証するために使用する信頼されたCA（認証局）証明書を含むPEM形式ファイル。
    cacertfile = "etc/certs/cacert.pem"
    # HTTPクライアントが送信するSSL/TLS証明書チェーンを含むPEM形式ファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成する必要があります。
    certfile = "etc/certs/cert.pem"
    # 証明書に対応する秘密鍵を含むPEM形式ファイル
    keyfile = "etc/certs/key.pem"
    ## サーバーの証明書チェーンの正当性を検証する場合は'verify_peer'、検証しない場合は'verify_none'を設定
    verify = verify_peer
  }
}
```

## ノード間通信のためのTLS

クラスター接続におけるSSL/TLS有効化手順は本章では扱っていません。詳細は[Cluster Security](../cluster/security.md)を参照してください。
