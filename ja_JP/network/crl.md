# CRL チェック

EMQX Enterprise v5.0.3 以降、MQTT SSL リスナーに対して証明書失効リスト（CRL）チェックがサポートされています。なお、Secure WebSocket や QUIC リスナーは対象外であり、`ssl` タイプのリスナーのみがこの機能をサポートしています。

この機能を有効にすると、EMQX は接続してくるクライアント証明書の CRL 配布ポイントに基づき、証明書が失効していないかを検証し、SSL/TLS ハンドシェイクの段階で失効したクライアント証明書による接続を拒否します。なお、CRL 自体に ["Issuing Distribution Point" 拡張](https://www.rfc-editor.org/rfc/rfc3280#section-5.2.5) が含まれている必要があり、これがなければ失効チェックは適用されません。

この機能を有効にするには、リスナーの対応オプションを有効にするだけでなく、リスナーの `verify` オプションを `verify_peer` に設定し、クライアントを CRL に対して検証する必要があります。

CRL チェックを有効にする設定例:


```hcl
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    # リスナーがクライアントの真正性を検証するために使用する信頼された CA（認証局）証明書を含む PEM 形式ファイル。
    cacertfile = "/etc/emqx/certs/ca.pem"
    # リスナーの SSL/TLS 証明書チェーンを含む PEM 形式ファイル。証明書がルート CA によって直接発行されていない場合、中間 CA 証明書をリスナー証明書の後に連結してチェーンを形成する必要があります。
    certfile = "/etc/emqx/certs/server.pem"
    # SSL/TLS 証明書に対応する秘密鍵を含む PEM 形式ファイル。
    keyfile = "/etc/emqx/certs/server.key"
    # ピア証明書を必ず検証する
    verify = verify_peer
    # クライアントが空でない証明書を送信しなければ TLS ハンドシェイクを失敗させる
    fail_if_no_peer_cert = true
    # クライアント証明書の失効状態も検証する
    enable_crl_check = true
  }
}
```
