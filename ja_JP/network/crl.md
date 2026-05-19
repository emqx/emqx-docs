# CRL チェック

EMQX v5.0.3 以降、EMQX は MQTT SSL リスナーに対して証明書失効リスト（CRL）チェックをサポートしています。この機能は `ssl` タイプのリスナーのみで利用可能であり、セキュア WebSocket（`wss`）および QUIC リスナーはサポートされていません。

CRL チェックが有効な場合、EMQX は接続してくるクライアントの証明書がクライアント証明書に指定された CRL 配布ポイントを参照して失効されていないかを検証します。証明書が失効リストにある場合、SSL/TLS ハンドシェイクの段階で接続は拒否されます。

> **注意**
>
> CRL の適用を有効にするには、CRL に [RFC 3280, Section 5.2.5](https://www.rfc-editor.org/rfc/rfc3280#section-5.2.5) で定義されている *Issuing Distribution Point* 拡張が含まれている必要があります。

## CRL チェックの有効化

CRL チェックを有効にするには、以下の手順を実施してください。

1. SSL リスナーで CRL チェックオプションを有効にする。
2. リスナーの `verify` オプションを `verify_peer` に設定し、クライアント証明書の検証を行う。

以下の例は、CRL チェックを有効にした SSL リスナーの設定例です。

```hcl
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    # リスナーがクライアントの真正性を検証するために使用する信頼された CA（認証局）証明書を含む PEM 形式ファイル
    cacertfile = "/etc/emqx/certs/ca.pem"
    # リスナーの SSL/TLS 証明書チェーンを含む PEM 形式ファイル。証明書がルート CA によって直接発行されていない場合、中間 CA 証明書をリスナー証明書の後に連結してチェーンを形成する必要があります。
    certfile = "/etc/emqx/certs/server.pem"
    # SSL/TLS 証明書に対応する秘密鍵を含む PEM 形式ファイル
    keyfile = "/etc/emqx/certs/server.key"
    # クライアント証明書の検証を必須にする
    verify = verify_peer
    # クライアントが空の証明書を送信した場合は TLS ハンドシェイクを失敗させる
    fail_if_no_peer_cert = true
    # クライアント証明書の失効状態も検証する
    enable_crl_check = true
  }
}
```

## CRL キャッシュ

CRL 配布ポイントへの過剰な HTTP リクエストを避けるため、EMQX は取得した CRL をローカルにキャッシュします。

クライアントが接続し、EMQX が初めて新しい CRL URL を検出した場合、クライアント証明書の配布ポイントから CRL を取得します。デフォルトでは、EMQX はキャッシュした CRL を 15 分ごとに更新し、失効情報を最新に保ちます。
