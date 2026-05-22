# HiveMQ から EMQX への移行

<<<<<<< HEAD
本ガイドでは、既存の HiveMQ デプロイメントを EMQX に移行する方法について説明します。TLS（ポート 8883）経由でデバイスが接続し、HiveMQ Enterprise Security Extension（ESE）で管理される X.509 クライアント証明書またはユーザー名/パスワード認証情報を使用する一般的なエンタープライズパターンに焦点を当てています。目的は、HOCON 設定およびルールエンジンを用いて、EMQX で同等の接続性、認証、およびデータ統合の動作を再現することです。

## 移行の概要

移行は以下の3つのフェーズに分けられます：

1. **HiveMQ 資産のインベントリ**：TLS キーストア、`config.xml`、ESE ファイル、およびリスナー、認証、クラスタリング、データパイプラインを定義する拡張プロパティを収集します。
2. **EMQX の設定**：HiveMQ の設定を EMQX の HOCON に変換し、キーストアを PEM に変換し、リスナーとクラスタ設定を再作成し、認証チェーンとルールエンジンをセットアップします。
3. **デバイスおよび統合の更新**：デバイスを EMQX エンドポイントにリダイレクトし、EMQX サーバー CA 証明書を展開し、クライアントの識別を検証し、Kafka や Prometheus などの下流統合を移行します。

以下の表は、HiveMQ と EMQX 間の主要なアーティファクトとその対応関係をまとめたものです：

| **パラメーター / アーティファクト** | **HiveMQ（例）** | **EMQX（例）** | **備考** |
| --- | --- | --- | --- |
| エンドポイントホスト名 | `mqtt.internal.example.com`（ロードバランサー / Control Center に設定） | `mqtt.example.com`（EMQX ロードバランサー / VIP） | デバイスのファームウェアやデプロイマニフェストを更新してください。 |
| TLS 資産 | `conf/hivemq.jks` | `/etc/emqx/certs/server-cert.pem`、`/etc/emqx/certs/server-key.pem` | `keytool` と `openssl` を使って JKS/PKCS12 を PEM に変換します。 |
| クライアント認証 | ESE ファイルレルム（`credentials.xml`） | `authentication = [{mechanism = password_based, backend = built_in_database}]` | REST API またはダッシュボード経由でユーザーリストをインポートします。 |
| クライアント証明書 | デバイスフリートに PEM 形式で格納、mTLS 有効時に HiveMQ で検証 | 同じデバイス証明書、EMQX リスナー `ssl_options.cacertfile = "device-ca.pem"` | 同じ CA を使用する場合は再プロビジョニング不要です。 |
| クラスター検出 | DNS または `extensions/*-discovery*/*.properties` | `cluster.discovery_strategy = dns`（または `static`、`etcd`、`k8s`） | 拡張ベースの検出を EMQX ネイティブ戦略に置き換えます。 |
| Kafka 統合 | `extensions/hivemq-kafka-extension/kafka-configuration.xml` | EMQX コネクター + ルール + アクション（`SELECT ... FROM "device/+/data"`） | Java ベースの拡張の代わりに EMQX データ統合を使用します。 |
| レート制限 / 制約 | `<restrictions>` ブロック + 過負荷保護 | `listeners.*.max_connections`、`messages_rate`、`bytes_rate`、`limiter.*` | リスナーごとのクォータとグローバルリミッターを設定します。 |
=======
本ガイドでは、既存の HiveMQ デプロイメントを EMQX に移行する方法を説明します。TLS（ポート 8883）経由でデバイスが接続し、HiveMQ Enterprise Security Extension（ESE）で管理される X.509 クライアント証明書またはユーザー名／パスワード認証を使用する一般的なエンタープライズパターンに焦点を当てています。目的は、HOCON 設定とルールエンジンを用いて、EMQX で同等の接続性、認証、およびデータ統合の動作を再現することです。

## 移行の概要

移行は以下の3つのフェーズに分けられます。

1. **HiveMQ 資産のインベントリ**：TLS キーストア、`config.xml`、ESE ファイル、およびリスナー、認証、クラスタリング、データパイプラインを定義する拡張プロパティを収集します。
2. **EMQX の設定**：HiveMQ の設定を EMQX の HOCON に変換し、キーストアを PEM に変換し、リスナーとクラスタ設定を再作成し、認証チェーンとルールエンジンをセットアップします。
3. **デバイスと統合の更新**：デバイスを EMQX エンドポイントにリダイレクトし、EMQX サーバー CA 証明書を配布し、クライアントの識別を検証し、Kafka や Prometheus などの下流統合を移行します。

以下の表は、HiveMQ と EMQX 間の主要なアーティファクトの対応をまとめたものです。

| **パラメーター／アーティファクト** | **HiveMQ（例）** | **EMQX（例）** | **備考** |
| --- | --- | --- | --- |
| エンドポイントホスト名 | `mqtt.internal.example.com`（ロードバランサー／コントロールセンターで設定） | `mqtt.example.com`（EMQX ロードバランサー／VIP） | デバイスのファームウェアやデプロイメントマニフェストを更新してください。 |
| TLS 資産 | `conf/hivemq.jks` | `/etc/emqx/certs/server-cert.pem`、`/etc/emqx/certs/server-key.pem` | `keytool` と `openssl` で JKS/PKCS12 を PEM に変換します。 |
| クライアント認証 | ESE ファイルレルム（`credentials.xml`） | `authentication = [{mechanism = password_based, backend = built_in_database}]` | REST API またはダッシュボード経由でユーザーリストをインポートします。 |
| クライアント証明書 | デバイスフリートに PEM 形式で保存、mTLS 有効時に HiveMQ が検証 | 同じデバイス証明書、EMQX リスナーの `ssl_options.cacertfile = "device-ca.pem"` | 同じ CA を使用している場合、再プロビジョニングは不要です。 |
| クラスター検出 | DNS または `extensions/*-discovery*/*.properties` | `cluster.discovery_strategy = dns`（または `static`、`etcd`、`k8s`） | 拡張ベースの検出を EMQX ネイティブの戦略に置き換えます。 |
| Kafka 統合 | `extensions/hivemq-kafka-extension/kafka-configuration.xml` | EMQX コネクター＋ルール＋アクション（`SELECT ... FROM "device/+/data"`） | Java ベースの拡張の代わりに EMQX データ統合を使用します。 |
| レート制限／制約 | `<restrictions>` ブロック＋過負荷保護 | `listeners.*.max_connections`、`messages_rate`、`bytes_rate`、`limiter.*` | リスナーごとのクォータとグローバルリミッターを設定します。 |
>>>>>>> origin/release-6.1

## フェーズ 1：HiveMQ 設定アーティファクトのインベントリ

### TLS キーストアの収集と変換

<<<<<<< HEAD
1. `<tls-tcp-listener>` で参照されているキーストアを特定します（例：`/opt/hivemq/conf/hivemq.jks`）。
2. サーバー証明書と秘密鍵をエクスポートします：
=======
1. `<tls-tcp-listener>` で参照されているキーストア（例：`/opt/hivemq/conf/hivemq.jks`）を特定します。
2. サーバー証明書と秘密鍵をエクスポートします。
>>>>>>> origin/release-6.1

```
keytool -importkeystore \
  -srckeystore /opt/hivemq/conf/hivemq.jks \
  -destkeystore /tmp/hivemq.p12 \
  -deststoretype PKCS12

openssl pkcs12 -in /tmp/hivemq.p12 -nodes -nokeys -out /tmp/server-cert.pem
openssl pkcs12 -in /tmp/hivemq.p12 -nodes -nocerts -out /tmp/server-key.pem
```

<<<<<<< HEAD
3. 生成された PEM ファイルを `/etc/emqx/certs/`（またはコンテナのシークレットマウント）にコピーします。HiveMQ で信頼されているデバイス CA（`device-ca.pem`）は保持し、EMQX は mTLS 検証にこの証明書を再利用します。

### HiveMQ 設定ファイルのエクスポート

以下のアーティファクトをバージョン管理に保存し、トレーサビリティを確保します。環境変数プレースホルダー（例：`${ENV:HIVEMQ_PORT}`）は、EMQX のダブルアンダースコア環境変数オーバーライド構文（`EMQX_LISTENERS__TCP__DEFAULT__BIND=0.0.0.0:1883`）にマッピングできるように明示してください。

- `conf/config.xml`：リスナー、制限、クラスタリング、パーシステンス、Control Center ユーザー
- `conf/logback.xml`：ログ出力先（EMQX の `log` セクションに翻訳）
=======
3. 生成された PEM ファイルを `/etc/emqx/certs/`（またはコンテナのシークレットマウント）にコピーします。HiveMQ で信頼されているデバイス CA（`device-ca.pem`）はそのまま保持し、EMQX で mTLS 検証に再利用します。

### HiveMQ 設定ファイルのエクスポート

以下のアーティファクトをバージョン管理に保存し、トレーサビリティを確保します。環境変数プレースホルダー（例：`${ENV:HIVEMQ_PORT}`）は、EMQX のダブルアンダースコア環境変数オーバーライド構文（`EMQX_LISTENERS__TCP__DEFAULT__BIND=0.0.0.0:1883`）にマッピングできるようにハイライトしてください。

- `conf/config.xml`：リスナー、制限、クラスタリング、パーシステンス、コントロールセンターのユーザー
- `conf/logback.xml`：ログ出力先（EMQX の `log` セクションに変換）
>>>>>>> origin/release-6.1
- `extensions/<name>/conf/*.xml` または `.properties`：検出、Kafka、Prometheus、カスタム認証
- `extensions/hivemq-enterprise-security-extension/enterprise-security-extension.xml`：認証レルムとパイプライン
- ESE で参照される `credentials.xml` やカスタムユーザーストア

### 認証モードの分類

<<<<<<< HEAD
使用している認証方法を判別します：

- ファイルレルムまたは SQL レルムによる **ユーザー名/パスワード**
- CN = クライアント ID の **X.509 クライアント証明書**（mTLS）
- **ハイブリッド**（例：TLS + SASL プラグイン）

それぞれの経路は特定の EMQX 認証チェーンにマッピングされます。

## フェーズ 2：HiveMQ ベースラインを再現するための EMQX 設定

### MQTT リスナーの再作成

各 `<tcp-listener>`、`<tls-tcp-listener>`、`<websocket-listener>`、および `<tls-websocket-listener>` 要素を HOCON に変換します。
=======
使用している認証方法を特定します。

- ファイルレルムまたは SQL レルムによる **ユーザー名／パスワード**
- CN = クライアント ID の **X.509 クライアント証明書**（mTLS）
- ハイブリッド（例：TLS + SASL プラグイン）

それぞれのパスは特定の EMQX 認証チェーンにマッピングされます。

## フェーズ 2：HiveMQ のベースラインを反映する EMQX 設定

### MQTT リスナーの再作成

各 `<tcp-listener>`、`<tls-tcp-listener>`、`<websocket-listener>`、`<tls-websocket-listener>` 要素を HOCON に変換します。
>>>>>>> origin/release-6.1

HiveMQ 設定例：

```xml
<hivemq>
    <listeners>
        <tcp-listener>
            <port>1883</port>
            <bind-address>0.0.0.0</bind-address>
        </tcp-listener>
        <tls-tcp-listener>
            <port>8883</port>
            <bind-address>0.0.0.0</bind-address>
            <tls>
                <keystore>
                    <path>/opt/hivemq/conf/keystore.jks</path>
                    <password>password</password>
                    <private-key-password>pkpassword</private-key-password>
                </keystore>
                <truststore>
                    <path>/opt/hivemq/conf/truststore.jks</path>
                    <password>password</password>
                </truststore>
                <client-authentication-mode>NONE</client-authentication-mode>
            </tls>
        </tls-tcp-listener>
        <tls-websocket-listener>
            <port>8084</port>
            <bind-address>0.0.0.0</bind-address>
            <path>/mqtt</path>
            <subprotocols>
                <subprotocol>mqttv3.1</subprotocol>
                <subprotocol>mqtt</subprotocol>
            </subprotocols>
            <tls>
                <keystore>
                    <path>/opt/hivemq/conf/keystore.jks</path>
                    <password>hivemq</password>
                </keystore>
                <truststore>
                    <path>/opt/hivemq/conf/truststore.jks</path>
                    <password>hivemq</password>
                </truststore>
            </tls>
        </tls-websocket-listener>
    </listeners>
</hivemq>
```

対応する EMQX 設定スニペット：

```hocon
listeners.tcp.default {
  bind = "0.0.0.0:1883"
}

listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "/etc/certs/server-cert.pem"
    keyfile  = "/etc/certs/server-key.pem"
  }
}

listeners.wss.default {
  bind = "0.0.0.0:8083"
  mqtt_path = "/mqtt"
  ssl_options {
    certfile = "/etc/certs/server-cert.pem"
    keyfile  = "/etc/certs/server-key.pem"
  }
}
```

<<<<<<< HEAD
`truststore.jks` と `keystore.jks` を PEM に変換する手順は、[TLS キーストアの収集と変換](#tls-キーストアの収集と変換) を参照してください。

### MQTT 設定オプションのマッピング

HiveMQ のキューサイズ、QoS、保持メッセージの動作などの設定は、EMQX の `mqtt` セクションに直接マッピングされます。
=======
`truststore.jks` と `keystore.jks` を PEM に変換する手順は、[TLS キーストアの収集と変換](#tls-キーストアの収集と変換)を参照してください。

### MQTT 設定オプションのマッピング

HiveMQ のキューサイズ、QoS、保持メッセージの挙動などの設定は、EMQX の `mqtt` セクションに直接マッピングされます。
>>>>>>> origin/release-6.1

HiveMQ 設定例：

```xml
<queued-messages>
    <max-queue-size>1000</max-queue-size>
    <strategy>discard</strategy>
</queued-messages>

<topic-alias>
    <enabled>true</enabled>
    <max-per-client>5</max-per-client>
</topic-alias>

<message-expiry>
    <max-interval>4294967296</max-interval>
</message-expiry>

<session-expiry>
    <max-interval>4294967295</max-interval>
</session-expiry>

<packets>
    <max-packet-size>268435460</max-packet-size>
</packets>

<receive-maximum>
    <server-receive-maximum>10</server-receive-maximum>
</receive-maximum>

<quality-of-service>
    <max-qos>2</max-qos>
</quality-of-service>

<wildcard-subscriptions>
    <enabled>true</enabled>
</wildcard-subscriptions>

<shared-subscriptions>
    <enabled>true</enabled>
</shared-subscriptions>

<subscription-identifier>
    <enabled>true</enabled>
</subscription-identifier>

<retained-messages>
    <enabled>true</enabled>
</retained-messages>
```

対応する EMQX 設定：

```hocon
mqtt {
  max_mqueue_len          = 1000
  mqueue_priorities       = disabled
  max_topic_alias         = 5
  message_expiry_interval = infinity   # HiveMQ の 4294967296 に相当
  session_expiry_interval = infinity
  max_packet_size         = "256MB"
  max_inflight            = 10
  max_qos_allowed         = 2
  wildcard_subscription   = true
  shared_subscription     = true
  retain_available        = true
  # subscription_identifier はデフォルトで有効
}
```

### `<restrictions>` ブロックのマッピング

HiveMQ はグローバル制限を `<restrictions>` にまとめています。EMQX はこれらの値をグローバルな `mqtt` セクションと各リスナーに分割します。

HiveMQ 設定例：

```xml
<restrictions>
    <max-client-id-length>65535</max-client-id-length>
    <max-connections>-1</max-connections>
    <incoming-bandwidth-throttling>0</incoming-bandwidth-throttling>
    <no-connect-idle-timeout>10000</no-connect-idle-timeout>
</restrictions>
```

対応する EMQX 設定スニペット：

```hocon
listeners.ssl.default {
  bind             = "0.0.0.0:8883"
  max_connections  = infinity
  bytes_rate       = "0"        # 'incoming-bandwidth-throttling'
  bytes_burst      = "0"
}

mqtt {
  max_clientid_len = 65535
  idle_timeout     = "10s"      # no-connect-idle-timeout
}
```

### クラスタリングの設定

HiveMQ の検出拡張やその他の検出方法を EMQX のネイティブ戦略に置き換えます。

HiveMQ クラスター設定例：

```xml
<cluster>
    <enabled>true</enabled>
    <transport>
        <tcp>
            <bind-address>127.0.0.1</bind-address>
            <bind-port>7800</bind-port>
        </tcp>
    </transport>
    <discovery>
        <static>
            <node>
                <host>127.0.0.1</host>
                <port>7800</port>
            </node>
            <node>
                <host>127.0.0.1</host>
                <port>7801</port>
            </node>
        </static>
    </discovery>
</cluster>
```

対応する EMQX 設定：

```
cluster {
  discovery_strategy = static
  static {
    seeds = [
      "emqx1@127.0.0.1",
      "emqx2@127.0.0.1"
    ]
  }
}
```

<<<<<<< HEAD
EMQX は同一マシン上で複数ノードを実行する場合、自動的に Erlang 分散ポートを割り当てるため、`bind-port` を手動で選択する必要はありません。

その他の検出方法（etcd、Kubernetes、静的ファイルなど）については、[クラスターの作成と管理](../deploy/cluster/create-cluster.md) を参照してください。

### 認証と認可の変換

HiveMQ は Enterprise Security Extension（ESE）を通じて、**レルム**（データソース）と **パイプライン**（ロジック）を管理します。EMQX は **認証チェーン**（順序付けられたバックエンド）と **認可ソース**（ACL）を使用します。

| HiveMQ ESE コンポーネント | EMQX 対応 | 移行戦略 |
| :--- | :--- | :--- |
| **ファイルレルム**（`credentials.xml`） | [**組み込みデータベース**](../access-control/authn/mnesia.md) | HiveMQ ユーザーをエクスポートし、EMQX REST API でインポートします。 |
| **SQL レルム**（JDBC） | [**MySQL**](../access-control/authn/mysql.md) / [**PostgreSQL**](../access-control/authn/postgresql.md) | `mysql` または `postgresql` バックエンドでパスワード認証を設定し、既存のユーザーテーブルを再利用します。 |
| **LDAP レルム** / AD | [**LDAP**](../access-control/authn/ldap.md) | LDAP バックエンドでパスワード認証を設定し、HiveMQ の DN パターンを EMQX フィルターテンプレートにマッピングします。 |
| **OAuth / JWT** | [**JWT**](../access-control/authn/jwt.md) | JWT 認証メカニズムを設定し、公開鍵または JWKS エンドポイントを構成します。 |
| **HTTP / Webhooks** | [**HTTP サーバー**](../access-control/authn/http.md) | HTTP バックエンドでパスワード認証を設定し、外部認証サービスに資格情報を委譲します。 |
| **X.509 証明書** | [**X.509**](../access-control/authn/x509.md) / [**mTLS**](../network/emqx-mqtt-tls.md#enable-ssl-tls-with-two-way-authentication) | `TLS` リスナーと相互（双方向）認証を使用し、既存の CA とクライアント証明書を再利用します。 |

#### ファイルレルムユーザーの移行

**ソース：** HiveMQ `conf/credentials.xml`（暗号化/ハッシュ済み）

**宛先：** EMQX 組み込みデータベース

1. **エクスポート：** HiveMQ ファイルレルム（`credentials.xml`）からユーザーを抽出します。このファイルには通常、ハッシュ化されたパスワードとソルトが含まれています。XML を解析して EMQX 用の JSON または CSV インポートファイルを生成する必要があります。
2. **インポート：** EMQX REST API を使用してユーザーを作成します。EMQX は bcrypt や pbkdf2 などのパスワードハッシュを含むユーザーの一括インポートをサポートしています。ファイル形式の詳細は [ユーザーのインポート](../access-control/authn/user_management.md#importing-users) を参照してください。

```bash
# 例：プレーンパスワードでユーザーをインポート
=======
EMQX は同一マシン上で複数ノードが稼働する場合に Erlang 分散ポートを自動割り当てするため、`bind-port` を手動で選択する必要はありません。

その他の検出方法（etcd、Kubernetes、静的ファイルなど）については、[クラスターの作成と管理](../deploy/cluster/create-cluster.md)を参照してください。

### 認証と認可の翻訳

HiveMQ は Enterprise Security Extension（ESE）を通じて、**レルム**（データソース）と **パイプライン**（ロジック）を定義しています。EMQX は **認証チェーン**（順序付けられたバックエンド）と **認可ソース**（ACL）を使用します。

| HiveMQ ESE コンポーネント | EMQX の対応 | 移行戦略 |
| :--- | :--- | :--- |
| **ファイルレルム**（`credentials.xml`） | [**組み込みデータベース**](../access-control/authn/mnesia.md) | HiveMQ のユーザーをエクスポートし、EMQX REST API でインポートします。 |
| **SQL レルム**（JDBC） | [**MySQL**](../access-control/authn/mysql.md) / [**PostgreSQL**](../access-control/authn/postgresql.md) | `mysql` または `postgresql` バックエンドでパスワード認証を設定。既存のユーザーテーブルを再利用します。 |
| **LDAP レルム** / AD | [**LDAP**](../access-control/authn/ldap.md) | LDAP バックエンドでパスワード認証を設定。HiveMQ の DN パターンを EMQX のフィルターテンプレートにマッピングします。 |
| **OAuth / JWT** | [**JWT**](../access-control/authn/jwt.md) | JWT 認証メカニズムを設定。公開鍵または JWKS エンドポイントを設定します。 |
| **HTTP / Webhooks** | [**HTTP サーバー**](../access-control/authn/http.md) | 外部認証サービスに認証情報を委譲する HTTP バックエンドでパスワード認証を設定します。 |
| **X.509 証明書** | [**X.509**](../access-control/authn/x509.md) / [**mTLS**](../network/emqx-mqtt-tls.md#enable-ssl-tls-with-two-way-authentication) | `TLS` リスナーと相互（双方向）認証を使用し、既存の CA およびクライアント証明書を再利用します。 |

#### ファイルレルムユーザーの移行

**ソース：** HiveMQ `conf/credentials.xml`（暗号化／ハッシュ済み）

**宛先：** EMQX 組み込みデータベース

1. **エクスポート：** HiveMQ のファイルレルム（`credentials.xml`）からユーザーを抽出します。このファイルには通常、ハッシュ化されたパスワードとソルトが含まれています。XML を解析して EMQX 用の JSON または CSV インポートファイルを生成する必要があります。
2. **インポート：** EMQX REST API を使用してユーザーを作成します。EMQX は bcrypt や pbkdf2 などのパスワードハッシュを含むユーザーの一括インポートをサポートしています。ファイル形式の詳細は [ユーザーのインポート](../access-control/authn/user_management.md#importing-users) を参照してください。

```bash
# 例：平文パスワードでユーザーをインポート
>>>>>>> origin/release-6.1
curl -u admin:public -X POST \
  http://emqx-node:18083/api/v5/authentication/password_based:built_in_database/users \
  -d '{"user_id":"device-001","password":"StrongPass!"}'
```

#### 外部統合（SQL、LDAP、HTTP）の移行

`enterprise-security-extension.xml` のパイプラインを EMQX の HOCON `authentication` ブロックに変換します。

<<<<<<< HEAD
**例：SQL レルムから EMQX MySQL へ**

HiveMQ は SQL レルムに対して [固定のデータベーススキーマ](https://docs.hivemq.com/hivemq-enterprise-security-extension/latest/ese.html#table_users) を使用しますが、EMQX は [独自のスキーマとクエリを定義可能](../access-control/authn/mysql.md) です。**既存の MySQL または PostgreSQL データベースを変更する必要はありません。**

以下の EMQX 設定例は、標準的な HiveMQ `users` テーブル構造に合わせて調整されたクエリ（`SELECT password_hash, salt ...`）を使用しています。

HiveMQ MySQL データベースに対してスキーマを変更せずに認証するための EMQX 設定例：
=======
**例：SQL レルムから EMQX MySQL**

HiveMQ は SQL レルムに対して [固定のデータベーススキーマ](https://docs.hivemq.com/hivemq-enterprise-security-extension/latest/ese.html#table_users) を使用します。一方、EMQX は [独自のスキーマとクエリを定義可能](../access-control/authn/mysql.md) です。**既存の MySQL または PostgreSQL データベースを変更する必要はありません。**

以下の EMQX 設定例は、標準的な HiveMQ `users` テーブル構造に特化したクエリ（`SELECT password_hash, salt ...`）を使用しています。
>>>>>>> origin/release-6.1

```hocon
authentication = [
  {
    mechanism = "password_based"
    backend = "mysql"
    server = "127.0.0.1:3306"
    database = "mqtt"
    username = "root"
    password = ""
    query = "SELECT password_hash, salt FROM users WHERE username = ${username}"
    password_hash_algorithm {
        name = "sha256"
        salt_position = "suffix"
    }
  }
]
```

**例：LDAP レルム**

```hocon
authentication = [
  {
    mechanism = "password_based"
    backend = "ldap"
    server = "ldap.example.com:636"
    ssl {
      enable = true
    }
    method {
      type = bind
      bind_password = "${password}"
    }
    username = "root"
    password = "root password"
    base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
    filter = "(objectClass=mqttUser)"
  }
]
```

#### 認可（ACL）の移行

<<<<<<< HEAD
HiveMQ は `enterprise-security-extension.xml`（ファイルレルム）や外部データベースでアクセスポリシーを定義します。EMQX は複数のバックエンドを同時にサポートする柔軟な **認可チェーン** を使用します（ファイル、Redis、MySQL、PostgreSQL、MongoDB、HTTP など）。
=======
HiveMQ は `enterprise-security-extension.xml`（ファイルレルム）や外部データベースでアクセスポリシーを定義します。EMQX は複数のバックエンド（ファイル、Redis、MySQL、PostgreSQL、MongoDB、HTTP など）を同時にサポートする柔軟な **認可チェーン** を使用します。
>>>>>>> origin/release-6.1

**HiveMQ XML ポリシー例：**

```xml
<permission>
    <topic>device/${clientid}/#</topic>
    <activity>ALL</activity>
</permission>
```

<<<<<<< HEAD
**EMQX 対応例：**

- [**ファイル（`acl.conf`）**](../access-control/authz/file.md)：`{allow, all, subscribe, ["device/${clientid}/#"]}.`
- [**組み込みデータベース**](../access-control/authz/mnesia.md)：クライアント ID、ユーザー名、トピックに基づきダッシュボードまたは API でルールを設定。
=======
**EMQX の対応例：**

- [**ファイル（`acl.conf`）**](../access-control/authz/file.md)：`{allow, all, subscribe, ["device/${clientid}/#"]}.`
- [**組み込みデータベース**](../access-control/authz/mnesia.md)：クライアント ID、ユーザー名、トピックに基づくルールをダッシュボードまたは API で設定。
>>>>>>> origin/release-6.1
- [**MySQL**](../access-control/authz/mysql.md)：`SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl where clientid = ${clientid} and ipaddress = ${peerhost}`
- [**PostgreSQL**](../access-control/authz/postgresql.md)：`SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl where clientid = ${clientid} and ipaddress = ${peerhost}`

詳細は [認可](../access-control/authz/authz.md) ドキュメントを参照してください。

### データ統合の設定

<<<<<<< HEAD
HiveMQ は Kafka 拡張など個別の拡張に依存していますが、EMQX ではすべてのデータ統合が組み込みであり、すぐに利用可能です。

特定の統合を設定する前に、以下のコアコンセプトを理解してください：
- [**データ統合概要**](../data-integration/data-bridges.md)
=======
HiveMQ は Kafka 拡張など個別の拡張に依存していますが、EMQX はすべてのデータ統合機能を標準で備えています。

特定の統合を設定する前に、以下のコアコンセプトを理解してください。

- [**データ統合の概要**](../data-integration/data-bridges.md)
>>>>>>> origin/release-6.1
- [**ルールエンジン**](../data-integration/rules.md)
- [**Flowデザイナー**](../flow-designer/introduction.md)

#### 例：Kafka 拡張の移行

HiveMQ Kafka 拡張設定例：

```xml
<kafka-configuration xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:noNamespaceSchemaLocation="config.xsd">
    <kafka-clusters>
        <kafka-cluster>
            <id>cluster01</id>
            <bootstrap-servers>127.0.0.1:9092</bootstrap-servers>
        </kafka-cluster>
    </kafka-clusters>

    <mqtt-to-kafka-mappings>
        <mqtt-to-kafka-mapping>
            <id>mapping01</id>
            <cluster-id>cluster01</cluster-id>
            <mqtt-topic-filters>
                <mqtt-topic-filter>#</mqtt-topic-filter>
            </mqtt-topic-filters>
            <kafka-topic>emqx</kafka-topic>
        </mqtt-to-kafka-mapping>
    </mqtt-to-kafka-mappings>

    <kafka-to-mqtt-mappings>
        <kafka-to-mqtt-mapping>
            <id>mapping02</id>
            <cluster-id>cluster01</cluster-id>
            <kafka-topics>
                <kafka-topic>topic1</kafka-topic>
                <kafka-topic>topic2</kafka-topic>
            </kafka-topics>
        </kafka-to-mqtt-mapping>
    </kafka-to-mqtt-mappings>
</kafka-configuration>
```

対応する EMQX 設定：

```hocon
connectors {
  kafka_producer {
    cluster01 {
      bootstrap_hosts = "127.0.0.1:9092"
      enable = true
    }
  }
  kafka_consumer {
    cluster01 {
      bootstrap_hosts = "127.0.0.1:9092"
      enable = true
    }
  }
}
actions {
  kafka_producer {
    mapping01 {
      connector = "cluster01"
      enable = true
      parameters {
        message {
          value = "${.}"
        }
        topic = "emqx"
      }
    }
  }
}
rule_engine {
  rules {
    mqtt-to-kafka-mapping-mapping01 {
      sql = "SELECT * FROM '#'"
      actions = [
        "kafka_producer:mapping01"
      ]
      enable = true
    }
    kafka-to-mqtt-mapping-mapping02 {
      actions = [
        {
          args {
            topic = "kafka"
          }
          function = "republish"
        }
      ]
      enable = true
      sql = "SELECT * FROM '$bridges/kafka_consumer:cluster01-topic1','$bridges/kafka_consumer:cluster01-topic2'"
    }
  }
}
sources {
  kafka_consumer {
    cluster01-topic1 {
      connector = "cluster01"
      parameters {
        topic = "topic1"
      }
      enable = true
    }
    cluster01-topic2 {
      connector = "cluster01"
      parameters {
        topic = "topic2"
      }
      enable = true
    }
  }
}
```

<<<<<<< HEAD
### 可観測性の設定
=======
### オブザーバビリティの設定
>>>>>>> origin/release-6.1

#### Prometheus

HiveMQ は「Prometheus Monitoring HiveMQ Extension」を使用しますが、EMQX はネイティブで Prometheus をサポートしています。

Prometheus がメトリクスをスクレイプするエンドポイントはデフォルトで有効です：`http://emqx-node:18083/api/v5/prometheus/stats`

<<<<<<< HEAD
Pushgateway を使用する場合は、以下のように設定できます：
=======
Pushgateway を利用する場合は以下のように設定します。
>>>>>>> origin/release-6.1

```hocon
prometheus {
  push_gateway {
    enable = true
    url = "http://127.0.0.1:9091"
  }
}
```

詳細は [Prometheus との統合](../observability/prometheus.md#integrate-with-prometheus) を参照してください。

#### ロギング

<<<<<<< HEAD
HiveMQ は `logback.xml`（Java 標準）を使用しますが、EMQX は HOCON で設定可能な組み込みのロギング機能を持ちます。
=======
HiveMQ は `logback.xml`（Java 標準）を使用しますが、EMQX は HOCON で設定可能な組み込みロギング機能を備えています。
>>>>>>> origin/release-6.1

HiveMQ (`logback.xml`) 設定例：

```xml
<appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
    <encoder>
        <pattern>%-30(%d %level)- %msg%n%ex</pattern>
    </encoder>
</appender>

<appender name="FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
    <file>${hivemq.log.folder}/hivemq.log</file>
    <append>true</append>

    <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
        <!-- daily rollover -->
        <fileNamePattern>${hivemq.log.folder}/hivemq.%d{yyyy-MM-dd}.log</fileNamePattern>

        <!-- keep 30 days' worth of history -->
        <maxHistory>30</maxHistory>
    </rollingPolicy>
    <encoder>
        <pattern>%-30(%d %level)- %msg%n%ex</pattern>
    </encoder>
</appender>
```

対応する EMQX 設定：

```hocon
log {
  file {
    default {
      enable = true
      level = warning
      path = "/var/log/emqx/emqx.log"
      rotation_count = 30
      rotation_size = "50MB"
    }
  }
  console {
    enable = true
    level = warning
  }
}
```

<<<<<<< HEAD
ログレベル、ローテーション、フォーマッター（テキスト/JSON）の設定は [ログ](../observability/log.md) を参照してください。

#### ログトレース

HiveMQ の「Trace Recordings」は特定クライアントセッションのデバッグに利用されます。EMQX は組み込みの **ログトレース** 機能（ダッシュボードまたは CLI）を提供し、特定のクライアント ID、トピック、IP のログをリアルタイムにフィルタリングできます。

特定クライアントのトレースを開始するには、以下のコマンドを使用します：
=======
ログレベル、ローテーション、フォーマッター（テキスト／JSON）の設定は [ログ](../observability/log.md) を参照してください。

#### ログトレース

HiveMQ の「Trace Recordings」は特定クライアントセッションのデバッグに使われます。EMQX はダッシュボードまたは CLI で特定のクライアント ID、トピック、IP のログをリアルタイムにフィルタリングできる組み込みの **ログトレース** 機能を提供します。

特定クライアントのトレースを開始するには、以下のコマンドを使用します。
>>>>>>> origin/release-6.1

```bash
emqx ctl trace start client device-001 trace.log
```

詳細は [ログトレース](../observability/tracer.md) を参照してください。

<<<<<<< HEAD
## フェーズ 3：デバイスおよび統合の更新

### EMQX サーバー CA をデバイスに展開

- EMQX が内部 CA を使用している場合、各デバイスに `device-ca.pem` をインストールします（システムトラストストアまたはアプリケーションバンドル）。
- EMQX が Let’s Encrypt などの公開 CA を使用している場合、デバイス側の対応は不要です。
=======
## フェーズ 3：デバイスと統合の更新

### EMQX サーバー CA をデバイスに配布

- EMQX が内部 CA を使用する場合は、各デバイスに `device-ca.pem` をインストールしてください（システムの信頼ストアまたはアプリケーションバンドル）。
- EMQX が Let’s Encrypt などの公開 CA を使用する場合は、デバイス側の対応は不要です。
>>>>>>> origin/release-6.1

### デバイス接続パラメーターの更新

**例（mqtt-cli）**

```bash
# 移行前（HiveMQ）
mqtt pub -h mqtt.internal.example.com -p 8883 \
  -u device-001 -pw StrongPass! \
  --cafile device-ca.pem --topic device/001/data --message test

# 移行後（EMQX）
mqtt pub -h mqtt.example.com -p 8883 \
  -u device-001 -pw StrongPass! \
  --cafile device-ca.pem --topic device/001/data --message test
```

<<<<<<< HEAD
**例（Python paho-mqtt で mTLS 使用時）**

```python
=======
**例（Python paho-mqtt + mTLS）**

```hocon
>>>>>>> origin/release-6.1
client.tls_set(
    ca_certs="certs/device-ca.pem",
    certfile="certs/device-001.cert.pem",
    keyfile="certs/device-001.key.pem",
    tls_version=ssl.PROTOCOL_TLS_CLIENT
)
client.connect("mqtt.example.com", 8883)
```

<<<<<<< HEAD
エンドポイントホスト名とサーバー CA ファイルのみが変更されます。デバイス証明書と秘密鍵は、EMQX の `ssl_options.cacertfile` で参照される同じ CA によって署名されていれば引き続き使用可能です。

### 統合の検証

- EMQX ルールメトリクス（`emqx ctl rule show`）で Kafka トピックへのメッセージ受信を確認します。
- 監視ダッシュボードを更新して EMQX メトリクスをスクレイプします。
- Splunk や ELK などのアラートシステムを EMQX のログフォーマットに合わせて再構成します。
=======
変更点はエンドポイントホスト名とサーバー CA ファイルのみです。デバイス証明書と秘密鍵は、EMQX の `ssl_options.cacertfile` で参照される同じ CA によって署名されていれば引き続き有効です。

### 統合の検証

- EMQX ルールのメトリクス（`emqx ctl rule show`）で Kafka トピックがメッセージを受信していることを確認します。
- 監視ダッシュボードを更新して EMQX メトリクスをスクレイプします。
- Splunk、ELK などのアラートシステムを EMQX ログフォーマットに合わせて再構成します。
>>>>>>> origin/release-6.1

## 高度な移行シナリオ

### 保持メッセージとセッション

<<<<<<< HEAD
HiveMQ のパーシステンスファイルは直接インポートできません。移行スクリプトを使用してください：

1. HiveMQ を一時的に稼働させ続けます。
2. HiveMQ で `#` をサブスクライブし、保持メッセージを EMQX に再パブリッシュするブリッジクライアントを実行します。
3. QoS 1/2 のキューイングされたメッセージは、DNS 切り替え前にインフライトトランザクションを完了させます。

### 共有サブスクリプション

HiveMQ の `$share/group/topic` 構文は EMQX で完全にサポートされています。以前 `$queue/topic` を使用していた場合は、`$share/queue/topic` にマッピングしてください。`broker.shared_subscription_strategy`（例：`round_robin`、`hash_clientid`）を調整して、コンシューマーが期待するロードバランシング動作を模倣します。

### HTTP/API ベースの設定

HiveMQ は静的 XML と拡張固有のリロードセマンティクスに依存していますが、EMQX は動的設定 API を提供します：
=======
HiveMQ のパーシステンスファイルは直接インポートできません。移行スクリプトを使用してください。

1. HiveMQ を一時的に稼働させ続けます。
2. HiveMQ で `#` をサブスクライブし、保持メッセージを EMQX に再パブリッシュするブリッジクライアントを実行します。
3. QoS 1/2 のキューイングメッセージは、DNS 切り替え前にインフライトトランザクションを完了させます。

### 共有サブスクリプション

HiveMQ の `$share/group/topic` 構文は EMQX で完全にサポートされています。以前 `$queue/topic` を使用していた場合は、`$share/queue/topic` にマッピングしてください。`broker.shared_subscription_strategy`（例：`round_robin`、`hash_clientid`）を調整し、コンシューマーのロードバランシング動作を模倣します。

### HTTP/API ベースの設定

HiveMQ は静的 XML と拡張固有のリロードセマンティクスに依存していますが、EMQX は動的設定 API を提供します。
>>>>>>> origin/release-6.1

```bash
curl -s -H "Authorization: Bearer $TOKEN" \
  -H "Content-type: application/json" \
  -X PUT "http://emqx-node:18083/api/v5/listeners/ssl:default" \
  -d '{"type": "ssl", "bind": "0.0.0.0:8883", "id": "ssl:default", "max_connections": 200000}'
```

これは `data/configs/cluster.hocon` に書き込まれます。設定を不変（`emqx.conf` のみ）にするか、環境ごとのオーバーライドを許容する EMQX の二層モデルを採用するかを検討してください。

## 検証チェックリスト

<<<<<<< HEAD
本番トラフィック切り替え前に以下を確認してください：

- すべての EMQX リスナーが `running` と表示される（`emqx ctl listeners list`）。
- TLS ハンドシェイクが成功し、mTLS デバイスでクライアント証明書なしの場合は失敗する。
- EMQX セッションのデバイス ID が元の HiveMQ クライアント ID と一致する。
- ACL が HiveMQ で適用していたトピックアクセスを同様に強制している。
- クラスターのノードがネットワーク分断後に自動復旧する。
- Kafka 統合が変換なしでデータを受信している。
- Prometheus でメトリクスが可視化されている。

## 結論

HiveMQ から EMQX への移行は主に設定の翻訳作業です。Java 中心のアーティファクト（XML、JKS、拡張）を EMQX の HOCON 設定、柔軟な認証チェーン、およびデータ統合フレームワークに変換します。

インベントリ、設定、更新の3フェーズを踏むことで、デバイス資格情報、トピック構造、統合フローを保持しつつ、EMQX の高並行性な Erlang ランタイムと動的設定機能を活用できます。
=======
本番トラフィックを切り替える前に、以下を確認してください。

- すべての EMQX リスナーが `running` 状態である（`emqx ctl listeners list`）。
- TLS ハンドシェイクが成功し、クライアント証明書未提供時に失敗する（mTLS デバイスの場合）。
- EMQX セッションのデバイス ID が元の HiveMQ クライアント ID と一致する。
- ACL が HiveMQ で適用していたトピックアクセスを同様に強制している。
- クラスターのノードがネットワーク分断後に自動復旧する。
- Kafka 統合が変換の問題なくデータを受信している。
- Prometheus にメトリクスが表示されている。

## 結論

HiveMQ から EMQX への移行は主に設定の翻訳作業です。Java 中心のアーティファクト（XML、JKS、拡張）を EMQX の HOCON 設定、柔軟な認証チェーン、データ統合フレームワークに変換します。

インベントリ、設定、更新の3フェーズを順に進めることで、デバイス資格情報、トピック構造、統合フローを維持しつつ、EMQX の高並行性な Erlang ランタイムと動的設定機能を活用できます。
>>>>>>> origin/release-6.1

移行計画を慎重に立て、各リスナーと統合を検証し、自信を持って切り替えを実行してください。
