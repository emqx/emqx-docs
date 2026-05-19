# Azure IoT Hub から EMQX への移行

本ガイドでは、Azure IoT Hub から EMQX への IoT デバイス移行の実践的な手順を説明します。以下の2つの移行パスを扱います。

1. **X.509 証明書認証**：クライアント証明書を使用するデバイス向け  
2. **SAS トークン認証**：HTTP ベース認証で Shared Access Signature (SAS) トークンを使用するデバイス向け

## 移行の概要

X.509 証明書を使用するデバイスの場合、移行は主に設定変更です。デバイス証明書と秘密鍵は変更せず、ブローカーのエンドポイントとサーバー CA 証明書のみを更新します。EMQX は Azure と同じ CA を信頼し、証明書の Common Name (CN) を deviceId とする Azure の ID マッピングモデルを再現する必要があります。

移行プロセスは以下の3つのフェーズに分かれます。

1. **CA 証明書の特定**  
   デバイス証明書を発行した CA 証明書を見つけます。

2. **EMQX の mTLS 設定**  
   EMQX ブローカーに SSL/TLS リスナーを設定し、ピア認証を必須にして、CA を信頼し証明書 CN を deviceId にマッピングするよう設定します。

3. **デバイスクライアントの更新**  
   デバイスコードを EMQX エンドポイントに接続するよう更新し、EMQX サーバー CA 証明書を信頼させます。デバイスは引き続き Azure IoT SDK を利用するか、標準 MQTT クライアントを使用できます。

以下の表はパラメーターの変更点をまとめたものです。

| **パラメーター**           | **Azure IoT Hub (例)**           | **EMQX (例)**             | **備考**                             |
|----------------------------|---------------------------------|---------------------------|------------------------------------|
| **エンドポイントホスト名** | `my-hub.azure-devices.net`       | `mqtt.example.com`         | デバイスクライアントコードを更新    |
| **デバイス証明書**         | `device-001.cert.pem`            | `device-001.cert.pem`      | 変更なし。既存証明書を継続使用       |
| **デバイス秘密鍵**         | `device-001.key.pem`             | `device-001.key.pem`       | 変更なし。既存秘密鍵を継続使用       |
| **サーバー検証** (デバイスがサーバーを信頼) | デバイスは Azure の公開 CA を信頼 | デバイスは `emqx-server-ca.pem` を信頼 | EMQX サーバー CA をデバイスに配布    |
| **クライアント検証** (サーバーがデバイスを信頼) | Azure はあなたの CA を信頼（CA アップロードまたはサムプリント登録） | EMQX の `cacertfile` にあなたの CA を設定 | Azure と同じ CA を使用               |
| **ID マッピング**           | Azure は `CN=deviceId` を抽出    | `mqtt.peer_cert_as_clientid = cn` を有効化 | deviceId ベースの認可を維持           |

## フェーズ 1: CA 証明書の特定

**必要なもの**：デバイス証明書に署名した CA 証明書（PEM 形式、例：`device-ca.pem`）。これは EMQX が mTLS 認証時にデバイスの身元を検証するために必須です。

Azure IoT Hub では X.509 登録方法が2種類あります。

- **CA 登録**：CA を Azure IoT Hub にアップロード済み。この場合、アップロードした CA ファイルを特定する必要があります。  
- **サムプリント登録**：各デバイスを証明書のサムプリントで個別登録。CA はアップロードされていませんが、証明書は内部 CA、自己署名 CA、または企業 PKI などの CA によって署名されています。これらの証明書を発行した CA を特定する必要があります。

どちらの場合も証明書階層は同じで、デバイスは常に自分の CA によって署名されています。EMQX に移行するには、この CA 証明書を入手し、EMQX がデバイスを検証できるようにします。

### デバイス証明書を発行した CA の特定

OpenSSL を使ってデバイス証明書の Issuer フィールドを確認します。

```bash
openssl x509 -in device-001.cert.pem -noout -issuer
```

期待される出力例：

```
issuer=CN = Azure-Device-CA
```

対応する CA ファイル（例：`Azure-Device-CA.pem`）が EMQX に提供すべき CA 証明書です。特にサムプリント登録の場合、この方法が正しい CA を特定する最も確実な手段です。

### 証明書要件の検証

Azure は証明書の Subject Common Name (CN) が deviceId（モジュールの場合は `deviceId/moduleId`）と一致することを要求します。以下のコマンドで確認できます。

```bash
openssl x509 -in device-001.cert.pem -noout -subject
```

期待される出力例：

```
subject=CN = device-001
```

EMQX は mTLS 認証時にこの CN を抽出し、デバイスの ID として使用します。

### デバイス認証情報のアクセス確認

各デバイスは以下の認証情報を安全に保持しています。

- デバイスのリーフ証明書 (`device-001.cert.pem`)  
- デバイスの秘密鍵 (`device-001.key.pem`)

Azure IoT Hub と EMQX は共に標準の X.509 認証を使用するため、この移行パスでは証明書の再プロビジョニングは不要です。

## フェーズ 2: Azure 互換の mTLS 用 EMQX 設定

EMQX を、Azure IoT Hub と同じ CA と ID マッピングルールを使ってデバイスを認証するよう設定します。

### mTLS リスナーの有効化と設定

EMQX で双方向 SSL/TLS 認証（mTLS）を SSL リスナーに設定します。SSL/TLS 設定の詳細は [Enable SSL/TLS Connections](../network/emqx-mqtt-tls.md) を参照してください。

EMQX の設定ファイル (`emqx.conf`) を開き、SSL/TLS リスナーを以下のように設定するか、ダッシュボードの **Management** -> **Listeners** から設定します。

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    # EMQX サーバー証明書
    certfile = "etc/certs/server-cert.pem"

    # EMQX サーバー秘密鍵
    keyfile = "etc/certs/server-key.pem"

    # --- デバイス認証用 mTLS 設定 ---

    # デバイス証明書を署名した CA 証明書
    cacertfile = "etc/certs/azure-device-ca.pem"

    # クライアント証明書検証を有効化
    verify = verify_peer

    # 証明書を提示しないクライアントを拒否
    fail_if_no_peer_cert = true
  }
}
```

::: tip
Azure IoT Hub と EMQX は共に MQTT over TLS/SSL のデフォルトポートとして `8883` を使用するため、デバイスクライアントのポート変更は不要です。
:::

**主な設定パラメーター**：
* `cacertfile`：CA 証明書（または自己署名デバイス証明書のバンドル）へのパス。EMQX はこれを使ってデバイス証明書を検証します。  
* `verify`：`verify_peer` に設定し、mTLS を有効化します。  
* `fail_if_no_peer_cert`：`true` に設定し、証明書提示を必須にします。

### Azure の CN=deviceId ID マッピングを再現

Azure IoT Hub は証明書の Common Name を抽出し、それを deviceId として認可に使用します。EMQX でも同様に設定します。

```hocon
mqtt.peer_cert_as_clientid = cn
mqtt.peer_cert_as_username = cn
```

この設定により：

- MQTT の ClientID が証明書 CN（deviceId）に自動設定されます。  
- ユーザー名も証明書 CN に設定されます。  
- `${clientid}` または `${username}` を使った EMQX ACL ルールで deviceId に基づく認可を実現できます。

モジュール（`deviceId/moduleId`）を使うデバイスは、CN に両方の識別子が含まれているため、そのまま EMQX ACL で利用可能です。

### 設定変更の適用

設定ファイルを更新後、設定をリロードします。

```bash
emqx ctl conf reload
```

ダッシュボードから変更した場合は **Update** をクリックしてください。リスナーは自動的に再起動し、新設定が反映されます。

mTLS が有効か確認するには：

```bash
openssl s_client -connect mqtt.example.com:8883 -showcerts
```

クライアント証明書なしでは接続が失敗するはずです。

## フェーズ 3: デバイスクライアントの更新と移行検証

最終フェーズは、デバイスクライアントコードを Azure IoT Hub から EMQX へ接続するよう更新することです。

### EMQX サーバー CA 証明書の準備

デバイスコードを更新する前に、EMQX サーバーの CA 証明書を入手してください。これは EMQX サーバー TLS 証明書を署名した CA です。

**自己署名の EMQX サーバー証明書の場合**、サーバー CA をデバイスの信頼済み証明書ストアに追加する必要があります。

**Linux**:

```bash
# CA をシステムの信頼ストアにコピー
sudo cp emqx-server-ca.pem /usr/local/share/ca-certificates/emqx-ca.crt
sudo update-ca-certificates
```

**macOS**:

```bash
# システムキーチェーンに追加
sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain emqx-server-ca.pem
```

**Windows**:

```powershell
# 信頼済みルート CA ストアにインポート
Import-Certificate -FilePath emqx-server-ca.pem -CertStoreLocation Cert:\LocalMachine\Root
```

::: tip
EMQX サーバーが Let's Encrypt などの公開 CA 証明書を使っている場合、この手順は不要です。システムが既に CA を信頼しています。
:::

### デバイスクライアントコードの更新

Azure IoT SDK（Python など）は `server_verification_cert` とカスタム `hostname` パラメーターを使ってカスタム MQTT ブローカーに接続可能です。これによりコード変更は最小限に抑えられます。

**Python の例**：

```python
from azure.iot.device import IoTHubDeviceClient, X509

# デバイス認証情報の読み込み
x509 = X509(
    cert_file="certs/device-001.cert.pem",
    key_file="certs/device-001.key.pem"
)

# EMQX サーバー CA 証明書の内容を読み込み
with open("certs/emqx-server-ca.pem", "r") as f:
    emqx_server_ca = f.read()

# EMQX を指すクライアントを作成
client = IoTHubDeviceClient.create_from_x509_certificate(
    x509=x509,
    hostname="mqtt.example.com",  # Azure ではなく EMQX のホスト名
    device_id="device-001",
    server_verification_cert=emqx_server_ca  # 証明書内容を文字列で渡す
)

# 接続して従来通り利用
client.connect()
client.send_message("Hello from migrated device")
```

::: tip
- `server_verification_cert` パラメーターは証明書の**内容を文字列で**渡す必要があり、ファイルパスではありません。  
- EMQX サーバー CA をシステムの信頼ストアに追加済みなら、このパラメーターは省略してシステムに検証を任せることも可能です。  
- Azure IoT SDK を使うことで既存のアプリケーションコード構造を維持しつつ設定変更だけで移行できるため、X.509 認証デバイスの最も簡単な移行方法です。
:::

### デバイス側パラメーターまとめ

必要なパラメーター変更は以下の通りです。

1. **エンドポイント／ホスト名**  
   - Azure: `my-hub.azure-devices.net`  
   - EMQX: `mqtt.example.com`

2. **サーバー CA 証明書**  
   - Azure: システムの信頼ストアまたは Azure CA を使用  
   - EMQX: 明示的に `emqx-server-ca.pem` を提供

3. **デバイス認証情報**（変更なし）  
   - 証明書：既存のデバイス証明書を継続使用  
   - 秘密鍵：既存の秘密鍵を継続使用

4. **ClientId**：証明書 CN と一致する deviceId に設定

### 移行検証チェックリスト

- デバイスが EMQX ダッシュボードに `clientid = deviceId` として表示される  
- TLS ハンドシェイクが成功し、デバイス証明書が検証される  
- デバイスが許可されたトピックにパブリッシュできる  
- デバイスが許可されたトピックをサブスクライブできる  
- EMQX ログに認証エラーが発生しない

## 標準移行パスのバリエーション

上記の基本移行フローに加え、いくつかの簡単なバリエーションを持つデバイス群もあります。これらはデバイス証明書やファームウェアを変更せずに EMQX で対応可能です。

### CA 署名済みデバイス群

- CA 証明書を EMQX にアップロードするだけで、同じ CA によって署名されたすべてのデバイスが自動的に信頼されます。  
- 証明書のライフサイクル管理が集中化されシンプルになります。  
- 新規デバイス追加時に EMQX の設定変更は不要です。

このシナリオは Azure IoT Hub の CA ベースプロビジョニングモデルと同様で、大規模デバイス群の最もスケーラブルな移行パスです。

### モジュール利用デバイス (`deviceId/moduleId`)

- 証明書の CN が `deviceId/moduleId` 形式のデバイスを完全にサポートします。  
- EMQX は CN 全体を ID マッピングと認証に使用可能です。  
- ACL ルールは CN 全体を参照でき、Azure のモジュールレベルアクセス制御を維持できます。

これにより Azure のモジュール階層を使うデバイスも証明書変更やカスタム ID ロジックなしでシームレスに移行できます。

## 代替案：HTTP 認証を使った SAS トークン認証

デバイスが Azure SAS トークンを使っている場合、EMQX でも HTTP 認証サービスを実装することで同様の認証を継続可能です。HTTP 認証の詳細は [Use HTTP Service](../access-control/authn/http.md) を参照してください。

### SAS トークン認証の仕組み

Azure IoT Hub は MQTT のユーザー名とパスワードフィールドに SAS 認証情報を送信します。

- **ユーザー名**：`{iothubhostname}/{deviceId}/?api-version=2021-04-12`  
- **パスワード**：`SharedAccessSignature sr={resource}&sig={signature}&se={expiry}`

EMQX はこれらの値を HTTP 認証サービスに転送し、サービス側で SAS トークンの検証を行います。

### SAS トークン用 HTTP 認証サービスの実装

1. HTTP 認証サービスを作成し、以下の処理を行います。  
   - EMQX から受け取ったユーザー名とパスワードを取得  
   - ユーザー名から `deviceId` を抽出  
   - パスワードから SAS トークンを解析  
   - デバイスの対称鍵を使ってトークン署名を検証  
   - トークンの有効期限 (`se` フィールド) をチェック  
   - 検証結果に応じて `{"result": "allow"}` または `{"result": "deny"}` を返す

2. EMQX HTTP 認証プラグインを設定し、上記サービスを利用するようにします。ダッシュボードまたは設定ファイルで以下のように設定します。

```hocon
authentication = [
  {
    mechanism = password_based
    backend = http
    method = post
    url = "http://your-auth-service:8080/auth"
    body {
      username = "${username}"
      password = "${password}"
      clientid = "${clientid}"
    }
    headers {
      "Content-Type" = "application/json"
    }
  }
]
```

3. デバイス認証情報のプロビジョニング。Azure IoT Hub の ID レジストリからデバイス ID と対称鍵をエクスポートし、HTTP 認証サービスのデータベースに登録して署名検証に利用します。

### HTTP 認証サービスのレスポンス例

サービスは以下のような JSON を返します。

```json
{
  "result": "allow",
  "is_superuser": false,
  "client_attrs": {
    "device_id": "device-001"
  }
}
```

::: tip
この方法により、SAS トークン認証デバイスはファームウェア変更なしで移行可能です。ただし、長期的な移植性とセキュリティ向上のためには X.509 証明書認証への移行を推奨します。
:::

## まとめ

Azure IoT Hub から EMQX へのデバイス移行は、デバイスの認証方式に応じて2つのパスがあります。

### X.509 証明書ベースのデバイス

最も簡単で直接的な移行パスです。既存のデバイス証明書と秘密鍵は変更せず、以下の更新のみ行います。

- EMQX に Azure と同じ CA を信頼させる  
- mTLS と証明書ベースの ID マッピングを有効化  
- デバイスの接続先エンドポイントとサーバー CA 証明書を更新

これにより、同じセキュリティモデルと証明書ワークフローを維持しつつ EMQX に接続できます。

### SAS トークンベースのデバイス

Azure SAS トークンを使うデバイスは、HTTP 認証サービスを実装することで EMQX でも継続利用可能です。これによりファームウェア変更なしで移行できます。

ただし、長期的な運用性とセキュリティ強化のためには X.509 証明書への移行を推奨します。

::: tip
X.509 と SAS トークン両方のデバイスが混在する場合、まず X.509 デバイス群を優先的に移行し、検証を加速させることを検討してください。その後、SAS トークンデバイスを HTTP 認証サービスで対応するか、X.509 へ移行するかを評価すると良いでしょう。
:::
