# Azure IoT Hub から EMQX への移行

本ガイドでは、Azure IoT Hub から EMQX へ IoT デバイスを移行する実践的な手順を説明します。以下の2つの移行パスをカバーしています。

1. **X.509 証明書認証**：クライアント証明書を使用するデバイス向け
2. **SAS トークン認証**：HTTP ベース認証で共有アクセス署名（SAS）トークンを使用するデバイス向け

## 移行の概要

X.509 証明書を使用するデバイスの場合、移行は主に設定の変更です。デバイス証明書と秘密鍵は変更せず、ブローカーのエンドポイントとサーバー CA 証明書のみを更新します。EMQX は Azure が信頼するのと同じ CA を信頼するよう設定し、証明書の Common Name（CN）が deviceId に等しいという Azure の ID マッピングモデルを再現する必要があります。

移行プロセスは以下の3つのフェーズで構成されます。

1. **CA 証明書の特定**：デバイス証明書に署名した CA 証明書を見つける。
2. **EMQX の mTLS 設定**：EMQX ブローカーで SSL/TLS リスナーを設定し、ピア認証を必須にして、CA を信頼し、証明書の CN を deviceId にマッピングする。
3. **デバイスクライアントの更新**：デバイスコードを EMQX エンドポイントに接続するよう更新し、EMQX サーバー CA 証明書を信頼させる。デバイスは引き続き Azure IoT SDK を利用するか、標準 MQTT クライアントを使用できます。

以下の表はパラメータの変更点をまとめたものです。

| **パラメータ** | **Azure IoT Hub（例）** | **EMQX（例）** | **備考** |
| -------------- | ----------------------- | -------------- | -------- |
| **エンドポイントホスト名** | `my-hub.azure-devices.net` | `mqtt.example.com` | デバイスクライアントコードを更新 |
| **デバイス証明書** | `device-001.cert.pem` | `device-001.cert.pem` | 変更なし。既存証明書を継続使用 |
| **デバイス秘密鍵** | `device-001.key.pem` | `device-001.key.pem` | 変更なし。既存秘密鍵を継続使用 |
| **サーバー検証**（デバイスがサーバーを信頼） | デバイスは Azure の公開 CA を信頼 | デバイスは `emqx-server-ca.pem` を信頼 | EMQX サーバー CA をデバイスに配布 |
| **クライアント検証**（サーバーがデバイスを信頼） | Azure はあなたの CA を信頼（CA アップロードまたはサムプリント登録） | EMQX の `cacertfile` にあなたの CA を設定 | Azure と同じ CA を使用 |
| **ID マッピング** | Azure は `CN=deviceId` を抽出 | `mqtt.peer_cert_as_clientid = cn` を有効化 | deviceId ベースの認可を維持 |

## フェーズ 1：CA 証明書の特定

**必要なもの**：デバイス証明書に署名した CA 証明書（PEM 形式、例：`device-ca.pem`）。EMQX が mTLS 認証時にデバイスの識別を検証するために必須です。

Azure IoT Hub は X.509 登録方法として以下の2つをサポートしています。

- **CA 登録**：CA 証明書を Azure IoT Hub にアップロード済み。元々アップロードした同じ CA ファイルを特定する必要があります。
- **サムプリント登録**：各デバイスを証明書のサムプリントで個別登録。Azure に CA はアップロードされていませんが、デバイス証明書は内部 CA、自署 CA、または企業 PKI などの CA によって署名されています。これらの証明書を発行した CA を特定する必要があります。

どちらの場合も証明書階層は同じで、デバイスは常に自分の CA によって署名されています。EMQX での検証のために、この CA 証明書を入手してください。

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

### 証明書要件の確認

Azure は証明書の Subject Common Name（CN）が deviceId（モジュールの場合は `deviceId/moduleId`）と一致することを要求しています。以下のコマンドで確認可能です。

```bash
openssl x509 -in device-001.cert.pem -noout -subject
```

期待される出力例：

```
subject=CN = device-001
```

EMQX は mTLS 認証時にこの CN を抽出し、デバイスの ID として使用します。

### デバイス認証情報のアクセス確認

各デバイスは自身の認証情報に安全にアクセス可能です。

- デバイスのリーフ証明書（`device-001.cert.pem`）
- デバイスの秘密鍵（`device-001.key.pem`）

Azure IoT Hub と EMQX は共に標準的な X.509 認証を使用しているため、この移行パスでは証明書の再プロビジョニングは不要です。

## フェーズ 2：Azure 互換の mTLS 用に EMQX を設定

EMQX を、Azure IoT Hub が X.509 認証で使用するのと同じ CA と ID マッピングルールでデバイス認証できるように設定します。

### mTLS リスナーの有効化と設定

EMQX で双方向 SSL/TLS 認証（mTLS）を有効にし、SSL リスナーを設定します。SSL/TLS 設定の詳細は[Enable SSL/TLS Connections](../../guides/network/emqx-mqtt-tls.md)を参照してください。

EMQX の設定ファイル（`emqx.conf`）を開き、SSL/TLS リスナーを設定するか、ダッシュボードの **Management** -> **Listeners** から設定します。

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    # EMQX サーバーの証明書
    certfile = "etc/certs/server-cert.pem"

    # EMQX サーバーの秘密鍵
    keyfile = "etc/certs/server-key.pem"

    # --- デバイス認証のための mTLS 設定 ---

    # デバイス証明書に署名した CA 証明書
    cacertfile = "etc/certs/azure-device-ca.pem"

    # クライアント証明書検証を有効化
    verify = verify_peer

    # クライアント証明書未提示を拒否
    fail_if_no_peer_cert = true
  }
}
```

::: tip
Azure IoT Hub と EMQX は共に MQTT over TLS/SSL のデフォルトポートとして `8883` を使用するため、デバイスクライアントのポート変更は不要です。
:::

**主要な設定パラメータ**：
* `cacertfile`：CA 証明書（または自己署名デバイス証明書のバンドル）へのパス。EMQX はこれを使ってデバイス証明書を検証します。
* `verify`：`verify_peer` に設定し、mTLS を有効にします。
* `fail_if_no_peer_cert`：`true` に設定し、証明書提示を必須にします。

### Azure の CN=deviceId ID マッピングを再現

Azure IoT Hub は証明書の Common Name を抽出し、deviceId として認可に使用します。EMQX でこれを再現するには以下を設定します。

```hocon
mqtt.peer_cert_as_clientid = cn
mqtt.peer_cert_as_username = cn
```

これにより：

- MQTT ClientID が証明書の CN（deviceId）に自動設定される
- ユーザー名も証明書の CN に設定される
- `${clientid}` や `${username}` を使った EMQX ACL ルールで deviceId ベースの認可モデルを再現可能

モジュール（`deviceId/moduleId`）を使用するデバイスの場合、CN に両方の識別子が含まれているため、そのまま EMQX ACL で利用できます。

### 設定変更の適用

設定ファイルを更新後、以下で設定をリロードします。

```bash
emqx ctl conf reload
```

ダッシュボードで変更した場合は **Update** をクリックしてください。リスナーは自動的に再起動して新設定を反映します。

mTLS が適用されているか確認するには：

```bash
openssl s_client -connect mqtt.example.com:8883 -showcerts
```

クライアント証明書なしでは接続が失敗するはずです。

## フェーズ 3：デバイスクライアントの更新と移行検証

最終フェーズでは、デバイスクライアントコードを Azure IoT Hub から EMQX へ接続するように更新します。

### EMQX サーバー CA 証明書の準備

デバイスコードを更新する前に、EMQX サーバーの CA 証明書を入手します。これは EMQX サーバーの TLS 証明書に署名した CA です。

**自己署名の EMQX サーバー証明書の場合**、サーバー CA をデバイスの信頼証明書ストアに追加する必要があります。

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
# 信頼されたルート証明機関ストアにインポート
Import-Certificate -FilePath emqx-server-ca.pem -CertStoreLocation Cert:\LocalMachine\Root
```

::: tip
EMQX サーバーが Let's Encrypt などの公開 CA 証明書を使用している場合、この手順は不要です。システムが既に CA を信頼しています。
:::

### デバイスクライアントコードの更新

Azure IoT SDK（Python など）は `server_verification_cert` とカスタム `hostname` パラメータを使ってカスタム MQTT ブローカーに接続可能です。これによりコード変更は最小限に抑えられます。

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
    server_verification_cert=emqx_server_ca  # 証明書内容を文字列で指定
)

# 接続して従来通り使用
client.connect()
client.send_message("Hello from migrated device")
```

::: tip
- `server_verification_cert` パラメータはファイルパスではなく、**証明書の内容を文字列で渡す**必要があります。
- EMQX サーバー CA をシステムの信頼ストアに追加済みの場合は、このパラメータを省略してシステムに検証を任せられます。
- Azure IoT SDK を使うことで既存のアプリケーションコード構造を維持しつつ設定変更のみで移行可能です。X.509 認証を使うデバイスにとって最も簡単な移行パスです。
:::

### デバイス側パラメータまとめ

変更が必要なパラメータは以下の通りです。

1. **エンドポイント／ホスト名**：
   - Azure：`my-hub.azure-devices.net`
   - EMQX：`mqtt.example.com`

2. **サーバー CA 証明書**：
   - Azure：システムの信頼ストアまたは Azure CA を使用
   - EMQX：`emqx-server-ca.pem` を明示的に提供

3. **デバイス認証情報**（変更なし）：
   - 証明書：既存のデバイス証明書を継続使用
   - 秘密鍵：既存の秘密鍵を継続使用

4. **ClientId**：証明書 CN と一致する deviceId に設定

### 検証チェックリスト

- EMQX ダッシュボードに `clientid = deviceId` でデバイスが表示されている
- TLS ハンドシェイクが成功し、デバイス証明書が検証されている
- デバイスが許可されたトピックにパブリッシュできる
- デバイスが許可されたトピックをサブスクライブできる
- EMQX ログに認証エラーがない

## 標準移行パスのバリエーション

上記のコア移行フロー以外に、X.509 ベースの移行プロセス内で対応可能な簡単なバリエーションを採用するデバイス群もあります。ここでは2つの代表例を紹介し、EMQX がどのように対応可能か説明します。

### CA 署名済みデバイス群

- CA 証明書を EMQX にアップロードするだけで済みます。
- この CA によって署名されたすべてのデバイスが自動的に信頼されます。
- 証明書のライフサイクル管理が集中管理されシンプルです。
- 新しいデバイス追加時に EMQX の設定変更は不要です。

このシナリオは Azure IoT Hub の CA ベースプロビジョニングモデルと同様で、大規模デバイス群のスケーラブルな移行パスを提供します。

### モジュールを使用するデバイス（`deviceId/moduleId`）

- 証明書の CN に `deviceId/moduleId` 形式を含むデバイスを完全にサポートします。
- EMQX は CN 全体を ID マッピングと認証に使用可能です。
- ACL ルールは CN 全体を参照でき、Azure のモジュールレベルアクセス制御を維持できます。

これにより、Azure のモジュール階層を使うデバイスも証明書変更やカスタム ID ロジックなしでシームレスに移行可能です。

## 代替：HTTP 認証サービスによる SAS トークン認証

デバイスが Azure SAS トークンを使用している場合、EMQX で HTTP 認証サービスを実装することで同じ認証方式を継続可能です。HTTP 認証の詳細は[Use HTTP Service](../../guides/access-control/authn/http.md)を参照してください。

### SAS トークン認証の仕組み

Azure IoT Hub は MQTT のユーザー名とパスワードフィールドに SAS 認証情報を送信します。

- **ユーザー名**：`{iothubhostname}/{deviceId}/?api-version=2021-04-12`
- **パスワード**：`SharedAccessSignature sr={resource}&sig={signature}&se={expiry}`

EMQX はこれらの値を HTTP 認証サービスに転送し、実際の SAS トークン検証を行います。

### SAS トークン用 HTTP 認証サービスの実装

1. HTTP 認証サービスを作成し、以下を実行します。
   - EMQX から送信されたユーザー名とパスワードを受け取る
   - ユーザー名から `deviceId` を抽出する
   - パスワードから SAS トークンを解析する
   - デバイスの対称鍵を使ってトークン署名を検証する
   - トークンの有効期限（`se` フィールド）をチェックする
   - 検証結果に基づき `{"result": "allow"}` または `{"result": "deny"}` を返す

2. EMQX の HTTP 認証機構を設定し、上記サービスを利用します。ダッシュボードまたは設定ファイルで HTTP 認証を追加します。

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

3. デバイス認証情報のプロビジョニング。Azure IoT Hub の ID レジストリからデバイス ID と対称鍵をエクスポートし、HTTP 認証サービスが SAS 署名を検証できるようにデータベースに保存します。

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
この方法により、SAS トークン認証デバイスはファームウェア変更なしで移行可能です。ただし、長期的な可搬性とセキュリティ強化のためには X.509 証明書認証への移行を推奨します。
:::

## まとめ

Azure IoT Hub から EMQX へのデバイス移行は、デバイスのプロビジョニング方法に応じて2つの認証パスのいずれかを選択できます。

### X.509 証明書ベースのデバイス

最も簡単かつ直接的な移行パスです。既存のデバイス証明書と秘密鍵は変更せず、以下の更新のみ必要です。

- Azure と同じ CA を信頼するよう EMQX を設定
- mTLS と証明書ベースの ID マッピングを有効化
- デバイスのエンドポイントとサーバー CA 証明書を更新

これにより、デバイスは同じセキュリティモデルと証明書ワークフローを維持しつつ EMQX に接続可能です。

### SAS トークンベースのデバイス

Azure SAS トークンを使うデバイスは、HTTP 認証サービスを実装することで EMQX でも継続利用可能です。これによりファームウェア変更なしで移行できます。

しかし、長期的な可搬性とセキュリティ強化のためには X.509 証明書への移行を推奨します。

::: tip
X.509 と SAS トークンの両方のデバイスが混在する場合、まず X.509 デバイス群を優先的に移行し、検証を加速することを検討してください。その後、SAS トークンデバイスを HTTP 認証サービスで対応するか、X.509 証明書に移行するかを評価するとよいでしょう。
:::
