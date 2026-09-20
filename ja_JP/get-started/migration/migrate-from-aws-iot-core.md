# AWS IoT Core から EMQX への移行

このページでは、AWS IoT Core から EMQX へ IoT デバイスを移行するための包括的な手順を説明します。デバイスおよび EMQX ブローカーの再設定方法を示し、デバイス群全体のスムーズな移行を実現します。

本ガイドは、両プラットフォームでサポートされている最も一般的かつ堅牢な認証方式である X.509 クライアント証明書（mTLS）認証に焦点を当てています。AWS IoT Core に登録された独自のカスタム認証局（CA）を使用してデバイス証明書に署名していることを前提としています。AWS 発行の（「ワンクリック」）証明書を使用している場合は、それらの証明書を再利用できません。AWS はこれらの証明書に署名した中間 CA を公開していないため、EMQX のような MQTT ブローカーから信頼できません。この場合は、自身で CA を作成し、デバイス証明書を再発行する必要があります。

## 移行の概要：標準フロー

AWS IoT Core で独自 CA によって署名された X.509 クライアント証明書（mTLS）を使用しているデバイスの場合、EMQX への移行は簡単です。公式の AWS IoT Device SDK を含む標準準拠クライアントは、クライアント側コードの変更を最小限に抑え、エンドポイントとサーバー CA 証明書の更新のみで EMQX に接続できます。既存のデバイス証明書と秘密鍵は引き続き有効です。

移行プロセスは以下の3つのフェーズで構成されます。

1. **CA 証明書の準備**  
   AWS IoT Core に登録済みで、デバイス証明書の署名に使用しているカスタム CA 証明書を特定します。

2. **EMQX の mTLS 設定**  
   EMQX ブローカーに SSL/TLS リスナーを設定し、ピア認証を必須にして、CA を信頼するようにリスナーを構成します。

3. **デバイスクライアントの更新**  
   デバイスクライアントコードを新しい EMQX エンドポイントアドレスと EMQX サーバーの CA 証明書に更新します。

以下の表は、主要なパラメーター変更の概要を示しています。

| **パラメーター**       | **AWS IoT Core（例）**                                  | **EMQX（例）**                  | **備考**                                                                                   |
|-----------------------|-------------------------------------------------------|--------------------------------|--------------------------------------------------------------------------------------------|
| **エンドポイントホスト名** | `agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com`       | `mqtt.example.com`              | デバイスクライアントコード／ファームウェアの更新が必要                                   |
| **エンドポイントポート**   | `8883`（MQTT/TLS）、`443`（MQTT/TLS または WebSocket/TLS） | `8883`（MQTT/TLS）、`8084`（WebSocket/TLS） | ポート `8883` を使うデバイスは変更不要。WebSocket（`443`）接続の場合は、EMQX の `8084` に更新が必要 |
| **デバイス証明書**       | `device-001.cert.pem`                                  | `device-001.cert.pem`           | 変更なし。既存の CA による署名済み証明書を引き続き使用                                     |
| **デバイス秘密鍵**       | `device-001.key.pem`                                   | `device-001.key.pem`            | 変更なし。既存の秘密鍵を引き続き使用                                                     |
| **サーバー検証**（デバイスがサーバーを信頼） | デバイスクライアントは `AmazonRootCA1.pem` を使用        | デバイスクライアントは `emqx-server-ca.pem` を使用する必要あり | クライアントは EMQX サーバー証明書を発行した CA を信頼する必要があります                   |
| **クライアント検証**（サーバーがデバイスを信頼） | AWS IoT Core は登録済み CA を信頼                         | EMQX リスナーの `cacertfile` に `your-ca.pem` を設定し、`verify` を `verify_peer` に設定 | AWS IoT Core に登録したのと同じ CA を EMQX に信頼させる設定が必要                         |

## フェーズ 1：CA 証明書の準備

本ガイドは、AWS IoT Core の「Bring Your Own CA（BYOCA）」機能を利用して登録した独自のカスタム認証局（CA）を使用していることを前提としています。デバイス証明書は AWS 独自の中間 CA ではなく、この CA によって署名されています。

**操作**：CA 証明書ファイル（例：`my-company-ca.pem`）を特定してください。これは AWS IoT Core に登録し、デバイス証明書の署名に使用した CA 証明書と同じものです。

デバイス証明書の署名者を確認するには、以下のコマンドを使用します。

```bash
openssl x509 -in device-001.cert.pem -text -noout | grep "Issuer"
```

Issuer（発行者）が AWS の中間 CA ではなく、組織の CA と一致することを確認してください。

::: tip
**AWS 発行の証明書を使用している場合**、AWS IoT Core の「ワンクリック」証明書生成は顧客に公開されていない独自の中間 CA を使用しています。AWS 発行証明書を使っている場合は、独自の CA を作成し、デバイス証明書を再発行してから EMQX へ移行する必要があります。本ガイドの範囲外ですが、OpenSSL や PKI ソリューションを用いて CA を作成し、デバイス証明書を発行してください。
:::

## フェーズ 2：EMQX の mTLS 認証設定

CA 証明書を特定したら、次に EMQX ブローカーを設定し、CA によって署名された証明書を使うデバイスを受け入れ、認証できるようにします。

### mTLS リスナーの有効化と設定

移行の核となるのは、EMQX リスナーで双方向 SSL/TLS 認証（mTLS）を有効にすることです。この設定により、EMQX は接続するクライアントに証明書の提示を要求し、その証明書の正当性を CA に対して検証します。

SSL/TLS 設定の詳細は [Enable SSL/TLS Connections](../../guides/network/emqx-mqtt-tls.md) を、証明書管理については [TLS Certificates](../../guides/network/tls-certificate.md) を参照してください。

**操作**：EMQX の設定ファイル（例：`emqx.conf`）を開き、SSL/TLS リスナーを設定するか、ダッシュボードの **Management** -> **Listeners** から設定してください。

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    # EMQX サーバー証明書
    certfile = "etc/certs/server-cert.pem"

    # EMQX サーバー秘密鍵
    keyfile = "etc/certs/server-key.pem"

    # --- デバイス認証のための mTLS 設定 ---

    # フェーズ 1 で用意した YOUR CA 証明書
    # AWS IoT Core に登録した CA と同じもの
    cacertfile = "etc/certs/my-company-ca.pem"

    # クライアント証明書検証を有効化
    verify = verify_peer

    # クライアント証明書がない場合は接続拒否
    fail_if_no_peer_cert = true
  }
}
```

::: tip
AWS IoT Core と EMQX はともに MQTT over TLS/SSL のデフォルトポートとして `8883` を使用しているため、デバイスクライアントのポート変更は不要です。
:::

**主な設定パラメーター**：

* `cacertfile`：AWS IoT Core に登録した CA 証明書のパス。EMQX はこれを使って接続デバイス証明書の正当性を検証します。
* `verify`：`verify_peer` に設定し、mTLS を有効化します。
* `fail_if_no_peer_cert`：`true` に設定し、クライアント証明書未提示の接続を拒否します。
* `certfile` と `keyfile`：EMQX サーバー自身の証明書と秘密鍵。クライアントはこれを検証して正しいブローカーに接続していることを確認します。

設定ファイルを更新したら、設定をリロードします。

```bash
emqx ctl conf reload
```

ダッシュボードで変更した場合は、**Update** ボタンをクリックすると適用されます。リスナーは自動的に再起動され、新設定が反映されます。

### （任意）証明書の CN を ClientID または Username にマッピング

多くの AWS IoT Core 実装では、認可ポリシーが証明書の情報（例：Common Name（CN））を `iot:ClientId` として利用しています。EMQX はこれをシームレスに再現可能で、認可ルールの移行を容易にします。

**操作**：デバイス証明書から MQTT の ClientID または Username を自動的に設定するには、`emqx.conf` に以下を追加します。

```hocon
# 証明書の Common Name (CN) を ClientID として使用
mqtt.peer_cert_as_clientid = cn

# 証明書の Common Name (CN) を Username として使用
mqtt.peer_cert_as_username = cn
```

この設定により、TLS ハンドシェイク時にピア証明書から CN（または Distinguished Name の場合は `dn`）を抽出し、MQTT セッションの ClientID または Username として利用します。これにより、`${clientid}` や `${username}` を用いた ACL などの既存認可ロジックが移行後も機能します。

例えば、デバイス証明書の CN が `device-001` であれば、`mqtt.peer_cert_as_clientid = cn` を有効にすると接続時の ClientID が自動的に `device-001` になります。

## フェーズ 3：デバイスクライアントの更新と移行確認

最後のフェーズは、デバイスクライアントコードを新しい EMQX ブローカーのエンドポイントに更新することです。ここでは公式の [AWS IoT Device SDK for Python v2](https://github.com/aws/aws-iot-device-sdk-python-v2) を例に示します。

AWS IoT SDK は AWS プラットフォームにロックインされておらず、標準準拠の MQTT-over-TLS クライアントとして機能します。つまり、既存のアプリケーションコードはそのまま使え、接続先エンドポイントとサーバー CA 証明書のパラメーターだけ変更すれば移行可能です。

### クライアント側コードの変更例（Python）

`aws-iot-device-sdk-python-v2` の `mqtt5_client_builder` モジュールを使った接続パラメーターの更新例：

1. **エンドポイントの更新**  
   * AWS: `endpoint="agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com"`  
   * EMQX: `endpoint="mqtt.example.com"` （EMQX ブローカーのホスト名/FQDN）

2. **サーバー CA 証明書の更新** (`ca_filepath`)  
   * デバイスが EMQX サーバーの正当性を検証するために使用  
   * AWS: 省略されることが多い（システムの信頼ストアを使用）か、`ca_filepath="AmazonRootCA1.pem"`（Amazon Root CA 1）  
   * EMQX: `ca_filepath="emqx-server-ca.pem"` （EMQX サーバー証明書を発行した CA）

3. **デバイス証明書は変更なし**  
   * `cert_filepath`（デバイス証明書）: 変更不要。既存の CA による署名済み証明書を使用し続ける。  
   * `pri_key_filepath`（デバイス秘密鍵）: 変更不要。既存の秘密鍵を使用し続ける。

### フル例：AWS SDK から EMQX への接続

以下は、AWS IoT Device SDK for Python v2 を使い、`samples/mqtt/mqtt5_x509.py` サンプルスクリプトを最小限の変更で EMQX に接続する例です。

**AWS IoT Core 版（移行前）**：

```bash
python3 mqtt5_x509.py \
  --endpoint agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com \
  --cert device-001.cert.pem \
  --key device-001.key.pem \
  --client_id basicPubSub \
  --topic sdk/test/python \
  --count 10
```

**EMQX 版（移行後）**：

```bash
python3 mqtt5_x509.py \
  --endpoint mqtt.example.com \
  --cert device-001.cert.pem \
  --key device-001.key.pem \
  --client_id basicPubSub \
  --topic sdk/test/python \
  --count 10
```

証明書と秘密鍵のパラメーターは変更不要で、エンドポイントのみ変更しています。

システムの信頼ストアを使わず、明示的にサーバー CA 証明書を指定する場合は、SDK のサンプルコードの `mqtt5_client_builder.mtls_from_path()` 呼び出しに `ca_filepath` パラメーターを追加します。

```python
# mqtt5_x509.py 内の mqtt5_client_builder.mtls_from_path() 呼び出し部分に以下を追加：

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
- **アプリケーションロジック**：変更不要。パブリッシュ・サブスクライブ・メッセージ処理はそのまま

この更新後のコマンドを実行すると、接続、サブスクライブ、パブリッシュが成功し、デバイス移行が完了したことを確認できます。

## 高度な移行シナリオ

同様の移行手順は、より高度な接続シナリオにも適用可能です。

### PKCS11（HSM）を使うデバイスの移行

秘密鍵をハードウェアセキュリティモジュール（HSM）に格納しているデバイスの場合も、移行は簡単です。秘密鍵は HSM 内に保持され、デバイス証明書は独自 CA による署名済みのまま有効です。

**クライアント側コード変更例**：

EMQX サーバー側設定（フェーズ 2）は同じです。クライアント側は、`mtls_with_pkcs11` ビルダーを使い、エンドポイントを更新します。

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

制限されたネットワーク内で HTTP プロキシ経由で接続する必要があるデバイスも、標準フローと同様に移行可能です。mTLS 接続は HTTP CONNECT リクエストを通じてトンネリングされます。

EMQX サーバー側設定（フェーズ 2）は **同じ** で、プロキシは EMQX リスナーに対して透過的です。

クライアント SDK の設定で、更新済みエンドポイントに加えプロキシ設定を追加します。

**クライアント側コード変更例（Python）**：

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

    # デバイス認証情報（変更なし）
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

独自のカスタム認証局を使用している mTLS ベースのデバイスを AWS IoT Core から EMQX に移行するのは簡単です。主に設定変更で対応でき、再プロビジョニングの手間はほとんどありません。

本ガイドで示した3つのフェーズに従うことで、

1. 独自 CA 証明書の特定  
2. EMQX ブローカーの mTLS 認証設定  
3. デバイスクライアントのエンドポイント更新

を実施し、デバイス群を安全かつ確実に EMQX に移行できます。既存のデバイス証明書、秘密鍵、AWS IoT Device SDK 上のアプリケーションロジックはそのまま維持でき、接続パラメーターの最小限の更新だけで移行が完了します。組織はセキュリティベストプラクティスを守りながら、IoT インフラを効率的に EMQX に移行できます。

::: tip
現在 AWS 発行証明書（ワンクリック方式）を使用している場合は、独自の CA インフラを構築し、デバイス証明書を再プロビジョニングしてから移行を行う必要があります。これは AWS 独自の証明書チェーンから脱却するための前提条件です。
:::
