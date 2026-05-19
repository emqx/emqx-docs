# AWS IoT Core から EMQX への移行

このページでは、IoTデバイスを AWS IoT Core から EMQX に移行するための包括的な手順を説明します。デバイスおよび EMQX ブローカーの再設定方法を示し、デバイス群全体のスムーズな移行を実現します。

本ガイドは、両プラットフォームでサポートされている最も一般的かつ堅牢な認証方式である X.509 クライアント証明書（mTLS）認証に焦点を当てています。AWS IoT Core に登録された独自のカスタム認証局（CA）を使用してデバイス証明書に署名していることを前提としています。AWS発行の（「ワンクリック」）証明書を使用している場合は、それらの証明書を再利用できません。AWS はこれらの証明書に署名した中間 CA を公開していないため、EMQX のような MQTT ブローカーで信頼できません。この場合は、独自の CA を作成し、デバイス証明書を再発行する必要があります。

## 移行の概要：標準フロー

AWS IoT Core で独自 CA によって署名された X.509 クライアント証明書（mTLS）を使用しているデバイスの場合、EMQX への移行は簡単です。公式の AWS IoT Device SDK を含む標準準拠クライアントは、クライアント側コードの変更を最小限に抑え、エンドポイントとサーバー CA 証明書のみを更新すれば EMQX に接続できます。既存のデバイス証明書と秘密鍵はそのまま有効です。

移行プロセスは以下の3つの主要フェーズで構成されます。

1. **CA 証明書の準備**  
   AWS IoT Core に登録され、デバイス証明書の署名に使用されているカスタム CA 証明書を特定します。

2. **EMQX の mTLS 設定**  
   EMQX ブローカーで SSL/TLS リスナーを設定し、ピア認証を必須にして、CA を信頼するようにリスナーを構成します。

3. **デバイスクライアントの更新**  
   デバイスクライアントコードのエンドポイントと EMQX サーバーの CA 証明書を更新します。

以下の表は、主要なパラメータの変更点を高レベルでまとめたものです。

| **パラメータ** | **AWS IoT Core（例）** | **EMQX（例）** | **備考** |
| -------------- | ---------------------- | -------------- | -------- |
| **エンドポイントホスト名** | `agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com` | `mqtt.example.com` | デバイスクライアントコード／ファームウェアを更新 |
| **エンドポイントポート** | `8883`（MQTT/TLS）、`443`（MQTT/TLS または WebSocket/TLS） | `8883`（MQTT/TLS）、`8084`（WebSocket/TLS） | ポート `8883` を使うデバイスは変更不要。WebSocket（`443`）の場合は EMQX の `8084` に変更が必要。 |
| **デバイス証明書** | `device-001.cert.pem` | `device-001.cert.pem` | 変更なし。既存の CA 署名証明書を継続使用。 |
| **デバイス秘密鍵** | `device-001.key.pem` | `device-001.key.pem` | 変更なし。既存の秘密鍵を継続使用。 |
| **サーバー検証**（デバイスがサーバーを信頼） | クライアントは `AmazonRootCA1.pem` を使用 | クライアントは `emqx-server-ca.pem` を使用するよう更新 | EMQX サーバー証明書を発行した CA をクライアントが信頼する必要あり。 |
| **クライアント検証**（サーバーがデバイスを信頼） | AWS IoT Core は登録済み CA を信頼 | EMQX リスナーの `cacertfile` に `your-ca.pem` を設定し、`verify` を `verify_peer` に設定 | AWS IoT Core に登録したのと同じ CA を EMQX で信頼するよう設定。 |

## フェーズ 1：CA 証明書の準備

本ガイドは、AWS IoT Core の「Bring Your Own CA」（BYOCA）機能を使って登録した独自のカスタム認証局（CA）を使用していることを前提としています。デバイス証明書はこの CA によって署名されており、AWS の独自中間 CA ではありません。

**作業内容**：CA 証明書ファイル（例：`my-company-ca.pem`）を特定してください。これは AWS IoT Core に登録し、デバイス証明書の署名に使用したものと同じ CA 証明書です。

デバイス証明書を署名した CA を確認するには、以下のコマンドで証明書の発行者情報を確認します。

```bash
openssl x509 -in device-001.cert.pem -text -noout | grep "Issuer"
```

発行者が組織の CA であり、AWS の中間 CA ではないことを確認してください。

::: tip
**AWS 発行証明書を使用している場合**、AWS IoT Core の「ワンクリック」証明書生成は顧客に公開されない独自の中間 CA を使用しています。AWS 発行証明書を使っている場合は、独自の CA を作成し、デバイス証明書を再発行してから EMQX に移行する必要があります。本ガイドの範囲外ですが、OpenSSL や PKI ソリューションを使って CA を作成し、デバイス証明書を発行してください。
:::

## フェーズ 2：EMQX の mTLS 認証設定

CA 証明書が準備できたら、次は EMQX ブローカーを設定し、CA によって署名された証明書を使うデバイスを受け入れ認証できるようにします。

### mTLS リスナーの有効化と設定

移行の核心は、EMQX のリスナーで双方向 SSL/TLS 認証（mTLS）を有効にすることです。この設定により、EMQX は接続してくるクライアントから証明書の提示を要求し、CA に対してその真正性を検証します。

SSL/TLS 設定の詳細は [Enable SSL/TLS Connections](../network/emqx-mqtt-tls.md) を、証明書管理については [TLS Certificates](../network/tls-certificate.md) をご参照ください。

**作業内容**：EMQX の設定ファイル（例：`emqx.conf`）を開き、SSL/TLS リスナーを設定するか、ダッシュボードの **Management** -> **Listeners** から設定してください。

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    # EMQX サーバー証明書
    certfile = "etc/certs/server-cert.pem"

    # EMQX サーバー秘密鍵
    keyfile = "etc/certs/server-key.pem"

    # --- デバイス認証のための mTLS 設定 ---

    # フェーズ1で準備したあなたの CA 証明書
    # AWS IoT Core に登録したのと同じ CA
    cacertfile = "etc/certs/my-company-ca.pem"

    # クライアント証明書検証を有効化
    verify = verify_peer

    # クライアント証明書未提示を拒否
    fail_if_no_peer_cert = true
  }
}
```

::: tip
AWS IoT Core と EMQX は共に MQTT over TLS/SSL のデフォルトポートとして `8883` を使用しているため、デバイスクライアントのポート変更は不要です。
:::

**主な設定パラメータ**：

* `cacertfile`：AWS IoT Core に登録した CA 証明書ファイルのパス。EMQX が接続するデバイス証明書の真正性を検証するために使用します。
* `verify`：`verify_peer` に設定し、mTLS を有効にします。
* `fail_if_no_peer_cert`：`true` に設定し、クライアント証明書未提示の接続を拒否して mTLS を強制します。
* `certfile` と `keyfile`：EMQX サーバー自身の証明書と秘密鍵。クライアントはこれを検証して正しいブローカーに接続していることを確認します。

設定ファイルを更新したら、設定をリロードします。

```bash
emqx ctl conf reload
```

ダッシュボードから変更した場合は、**Update** をクリックして適用してください。リスナーは自動的に再起動して設定を反映します。

### （任意）証明書の CN を ClientID または Username にマッピング

多くの AWS IoT Core 実装では、認可ポリシーが証明書の情報を元に変数を埋め込んでおり、例えば証明書の Common Name（CN）を `iot:ClientId` として使用しています。EMQX でも同様の動作を再現でき、認可ルールの移行を容易にします。

**作業内容**：デバイス証明書から MQTT の ClientID または Username を自動的に設定するには、`emqx.conf` に以下を追加します。

```hocon
# 証明書の Common Name (CN) を ClientID として使用
mqtt.peer_cert_as_clientid = cn

# 証明書の Common Name (CN) を Username として使用
mqtt.peer_cert_as_username = cn
```

この設定により、TLS ハンドシェイク時にピア証明書から CN（または Distinguished Name の場合は `dn`）を抽出し、MQTT セッションの ClientID または Username として利用します。これにより、`${clientid}` や `${username}` を使った既存の ACL などの認可ロジックが移行後もそのまま機能します。

例えば、デバイス証明書の CN が `device-001` であれば、`mqtt.peer_cert_as_clientid = cn` を有効にすると接続時の MQTT クライアント ID が自動的に `device-001` になります。

## フェーズ 3：デバイスクライアントの更新と移行検証

最後のフェーズは、デバイスクライアントコードを新しい EMQX ブローカーのエンドポイントに更新することです。ここでは公式の [AWS IoT Device SDK for Python v2](https://github.com/aws/aws-iot-device-sdk-python-v2) を例に示します。

AWS IoT SDK は AWS プラットフォームにロックインされておらず、標準準拠の MQTT-over-TLS クライアントとして動作します。つまり、既存のアプリケーションコードはそのまま利用可能で、接続先エンドポイントとサーバー CA 証明書のパラメータのみ変更すれば移行できます。

### クライアント側コードの変更例（Python）

`aws-iot-device-sdk-python-v2` の `mqtt5_client_builder` モジュールを使った場合、AWS IoT Core から EMQX への移行に必要な接続パラメータの変更は以下の通りです。

1. **エンドポイントの更新**  
   * AWS: `endpoint="agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com"`  
   * EMQX: `endpoint="mqtt.example.com"`（EMQX ブローカーのホスト名/FQDN）

2. **サーバー CA 証明書の更新**（`ca_filepath`）  
   * EMQX サーバーの身元を検証するための CA 証明書  
   * AWS: 省略（システムの信頼ストアを使用）または `ca_filepath="AmazonRootCA1.pem"`  
   * EMQX: `ca_filepath="emqx-server-ca.pem"`（EMQX サーバー証明書を発行した CA）

3. **デバイス証明書は変更なし**  
   * `cert_filepath`（デバイス証明書）: 変更不要。既存の CA 署名証明書を継続使用。  
   * `pri_key_filepath`（デバイス秘密鍵）: 変更不要。既存の秘密鍵を継続使用。

### 完全な例：AWS SDK から EMQX への接続

以下は AWS IoT Device SDK for Python v2 を使って EMQX に接続する例です。`samples/mqtt/mqtt5_x509.py` サンプルスクリプトを最小限の変更で利用できます。

**AWS IoT Core 版（移行前）**:
```bash
python3 mqtt5_x509.py \
  --endpoint agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com \
  --cert device-001.cert.pem \
  --key device-001.key.pem \
  --client_id basicPubSub \
  --topic sdk/test/python \
  --count 10
```

**EMQX 版（移行後）**:
```bash
python3 mqtt5_x509.py \
  --endpoint mqtt.example.com \
  --cert device-001.cert.pem \
  --key device-001.key.pem \
  --client_id basicPubSub \
  --topic sdk/test/python \
  --count 10
```

証明書と鍵のパラメータは変更せず、エンドポイントのみ変更しています。

システムの信頼ストアを使わずにサーバー CA 証明書を明示的に指定する場合は、SDK のサンプルコードで `ca_filepath` パラメータを追加してください。

```python
# mqtt5_x509.py 内の mqtt5_client_builder.mtls_from_path() 呼び出し部分に
# ca_filepath パラメータを追加：

client = mqtt5_client_builder.mtls_from_path(
    endpoint=args.input_endpoint,
    cert_filepath=args.input_cert,
    pri_key_filepath=args.input_key,
    ca_filepath="emqx-server-ca.pem",  # ここを追加
    on_publish_received=on_publish_received,
    on_lifecycle_stopped=on_lifecycle_stopped,
    on_lifecycle_attempting_connect=on_lifecycle_attempting_connect,
    on_lifecycle_connection_success=on_lifecycle_connection_success,
    on_lifecycle_connection_failure=on_lifecycle_connection_failure,
    on_lifecycle_disconnection=on_lifecycle_disconnection,
    client_id=args.input_clientId
)
```

**主な変更点まとめ**：
- **エンドポイント**：AWS IoT Core のエンドポイントから EMQX ブローカーのホスト名に変更  
- **サーバー CA**：必要に応じて EMQX サーバー証明書を発行した CA を指定  
- **デバイス証明書**：変更なし。既存の証明書と秘密鍵を使用  
- **アプリケーションロジック**：変更不要。パブリッシュ、サブスクライブ、メッセージ処理はそのまま

この更新済みコマンドを実行すると、接続、サブスクライブ、パブリッシュが正常に動作し、移行が完了したことを確認できます。

## 高度な移行シナリオ

同様の移行手法は、より高度な接続シナリオにも適用できます。

### PKCS11（HSM）を使用するデバイスの移行

秘密鍵をハードウェアセキュリティモジュール（HSM）に格納しているデバイスの場合も移行は簡単です。秘密鍵は HSM 内に保持され、デバイス証明書は独自 CA によって署名されていれば有効です。

**クライアント側コードの変更例**：

EMQX サーバー側設定（フェーズ 2）は同じです。クライアント側では、`mtls_with_pkcs11` ビルダーを使い、エンドポイントを更新してください。

```python
client = mqtt5_client_builder.mtls_with_pkcs11(
    # 変更：EMQX ブローカーのホスト名
    endpoint="mqtt.example.com",

    # 変更：EMQX サーバーの CA 証明書
    ca_filepath="emqx-server-ca.pem",

    # デバイス証明書（変更なし）
    cert_filepath="device-001.cert.pem",

    # HSM 設定（変更なし）
    pkcs11_lib="/path/to/pkcs11.so",
    user_pin="YOUR-HSM-PIN",
    slot_id=pkcs11_slot_id,
    token_label="YOUR-TOKEN-LABEL",
    private_key_label="device-001-key",

    on_publish_received=on_publish_received,
    # ... その他のコールバック ...
    client_id="device-001"
)
```

### HTTP プロキシ経由で接続するデバイスの移行

制限されたネットワーク環境で HTTP プロキシ経由で接続するデバイスも、標準パスと同様に移行可能です。mTLS 接続は HTTP CONNECT リクエストでトンネリングされます。

EMQX サーバー側設定（フェーズ 2）は**同じ**です。プロキシは EMQX リスナーに対して透過的であり、mTLS 接続のみを受け取ります。

クライアント SDK の設定でプロキシ情報を追加し、エンドポイントを更新してください。

**クライアント側コードの変更例（Python）**：

```python
from awscrt import http

# 1. HTTP プロキシの設定
http_proxy_options = http.HttpProxyOptions(
    host_name="my-proxy.my-network.com",
    port=8888
)

# 2. プロキシオプションを指定してクライアント作成
client = mqtt5_client_builder.mtls_from_path(
    # 変更：EMQX ブローカーのホスト名
    endpoint="mqtt.example.com",

    # 変更：EMQX サーバーの CA 証明書
    ca_filepath="emqx-server-ca.pem",

    # デバイス証明書（変更なし）
    cert_filepath="device-001.cert.pem",
    pri_key_filepath="device-001.key.pem",

    # プロキシ設定を追加
    http_proxy_options=http_proxy_options,

    on_publish_received=on_publish_received,
    # ... その他のコールバック ...
    client_id="device-001"
)
```

## まとめ

独自のカスタム認証局を使用している mTLS ベースのデバイスを AWS IoT Core から EMQX に移行するのは簡単です。主に設定変更で済み、再プロビジョニングの手間はほとんどありません。

本ガイドで示した3つのフェーズを順に実施することで、

1. 独自 CA 証明書の特定  
2. EMQX ブローカーの mTLS 認証設定  
3. デバイスクライアントのエンドポイント更新

デバイス群を安全かつ確実に EMQX に移行できます。既存のデバイス証明書、秘密鍵、AWS IoT Device SDK のアプリケーションロジックはそのまま維持でき、接続パラメータの更新だけで済みます。これにより、最小限のダウンタイムでセキュリティベストプラクティスを保ちながら IoT インフラを EMQX に移行可能です。

::: tip
現在 AWS 発行証明書（ワンクリック方式）を使用している場合は、独自の CA インフラを構築し、デバイス証明書を再プロビジョニングしてから移行する必要があります。これは AWS 独自の証明書チェーンから脱却するための前提条件です。
:::
