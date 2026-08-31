# HiveMQ から EMQX への移行

本ガイドでは、既存の HiveMQ デプロイメントを EMQX に移行する方法を説明します。TLS（ポート 8883）経由でデバイスが接続し、HiveMQ Enterprise Security Extension（ESE）で管理される X.509 クライアント証明書またはユーザー名/パスワード認証情報を使用する一般的なエンタープライズパターンに焦点を当てています。目的は、HOCON 設定とルールエンジンを用いて、EMQX で同等の接続性、認証、およびデータ統合の動作を再現することです。

## 移行の概要

移行は以下の3つのフェーズに分けられます：

1. **HiveMQ 資産のインベントリ**：TLS キーストア、`config.xml`、ESE ファイル、およびリスナー、認証、クラスタリング、データパイプラインを定義する拡張プロパティを収集します。
2. **EMQX の設定**：HiveMQ の設定を EMQX の HOCON に変換し、キーストアを PEM に変換し、リスナーとクラスタ設定を再作成し、認証チェーンとルールエンジンを構築します。
3. **デバイスおよび統合の更新**：デバイスを EMQX のエンドポイントにリダイレクトし、EMQX サーバ CA 証明書を配布し、クライアントの識別を検証し、Kafka や Prometheus などの下流統合を移行します。

以下の表は、HiveMQ と EMQX 間の主要なアーティファクトの対応をまとめたものです：

| **パラメーター / アーティファクト** | **HiveMQ（例）** | **EMQX（例）** | **備考** |
| --- | --- | --- | --- |
| エンドポイントホスト名 | `mqtt.internal.example.com`（ロードバランサー / Control Center で設定） | `mqtt.example.com`（EMQX ロードバランサー / VIP） | デバイスのファームウェアやデプロイメントマニフェストを更新してください。 |
| TLS 資産 | `conf/hivemq.jks` | `/etc/emqx/certs/server-cert.pem`、`/etc/emqx/certs/server-key.pem` | `keytool` と `openssl` を使って JKS/PKCS12 を PEM に変換します。 |
| クライアント認証 | ESE ファイルレルム（`credentials.xml`） | `authentication = [{mechanism = password_based, backend = built_in_database}]` | REST API またはダッシュボード経由でユーザーリストをインポートします。 |
| クライアント証明書 | デバイスフリートに PEM 形式で保存、mTLS 有効時に HiveMQ が検証 | 同じデバイス証明書、EMQX リスナーの `ssl_options.cacertfile = "device-ca.pem"` | 同じ CA を使う場合は再プロビジョニング不要です。 |
| クラスター検出 | DNS または `extensions/*-discovery*/*.properties` | `cluster.discovery_strategy = dns`（または `static`、`etcd`、`k8s`） | 拡張ベースの検出を EMQX ネイティブ戦略に置き換えます。 |
| Kafka 統合 | `extensions/hivemq-kafka-extension/kafka-configuration.xml` | EMQX コネクター＋ルール＋アクション（`SELECT ... FROM "device/+/data"`） | Java ベースの拡張の代わりに EMQX データ統合を使用します。 |
| レート制限 / 制約 | `<restrictions>` ブロック＋過負荷保護 | `listeners.*.max_connections`、`messages_rate`、`bytes_rate`、`limiter.*` | リスナーごとのクォータとグローバルリミッターを設定します。 |

## フェーズ 1：HiveMQ 設定アーティファクトのインベントリ

### TLS キーストアの収集と変換

1. `<tls-tcp-listener>` で参照されているキーストアを特定します（例：`/opt/hivemq/conf/hivemq.jks`）。
2. サーバ証明書とキーをエクスポートします：

```
keytool -importkeystore \
  -srckeystore /opt/hivemq/conf/hivemq.jks \
  -destkeystore /tmp/hivemq.p12 \
  -deststoretype PKCS12

openssl pkcs12 -in /tmp/hivemq.p12 -nodes -nokeys -out /tmp/server-cert.pem
openssl pkcs12 -in /tmp/hivemq.p12 -nodes -nocerts -out /tmp/server-key.pem
```

3. 生成された PEM ファイルを `/etc/emqx/certs/`（またはコンテナのシークレットマウント）にコピーします。HiveMQ で信頼されているデバイス CA（`device-ca.pem`）はそのまま保持し、EMQX でも mTLS 検証に再利用します。

### HiveMQ 設定ファイルのエクスポート

以下のアーティファクトをバージョン管理に保存し、トレーサビリティを確保してください。環境変数プレースホルダー（例：`${ENV:HIVEMQ_PORT}`）は、EMQX のダブルアンダースコア環境変数オーバーライド構文（`EMQX_LISTENERS__TCP__DEFAULT__BIND=0.0.0.0:1883`）にマッピングできるようにハイライトしてください。

- `conf/config.xml`：リスナー、制限、クラスタリング、パーシステンス、Control Center ユーザー
- `conf/logback.xml`：ログターゲット（EMQX の `log` セクションに翻訳）
- `extensions/<name>/conf/*.xml` または `.properties`：検出、Kafka、Prometheus、カスタム認証
- `extensions/hivemq-enterprise-security-extension/enterprise-security-extension.xml`：認証レルムとパイプライン
- ESE で参照される `credentials.xml` やカスタムユーザーストア

### 認証モードの分類

使用している認証方式を特定します：

- ファイルレルムまたは SQL レルムによる **ユーザー名/パスワード**
- CN = クライアント ID の **X.509 クライアント証明書**（mTLS）
- **ハイブリッド**（例：TLS + SASL プラグイン）

それぞれの方式は、特定の EMQX 認証チェーンにマッピングされます。

## フェーズ 2：HiveMQ のベースラインを反映する EMQX の設定

### MQTT リスナーの再作成

各 `<tcp-listener>`、`<tls-tcp-listener>`、`<websocket-listener>`、`<tls-websocket-listener>` 要素を HOCON に変換します。

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

`truststore.jks` と `keystore.jks` を PEM に変換する手順は、[TLS キーストアの収集と変換](#tls-キーストアの収集と変換) を参照してください。

### MQTT 設定オプションのマッピング

HiveMQ のキューサイズ、QoS、保持メッセージの動作などの設定は、EMQX の `mqtt` セクションに直接マッピングされます。

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

HiveMQ はグローバル制限を `<restrictions>` にまとめています。EMQX はこれらの値をグローバルな `mqtt` セクションと各リスナーに分割して設定します。

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

### クラスター設定

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

EMQX は同一マシン上に複数ノードがある場合、Erlang 分散ポートを自動割り当てするため、`bind-port` を手動で選択する必要はありません。

その他の検出方法（etcd、Kubernetes、静的ファイルなど）については、[クラスターの作成と管理](../deploy/cluster/create-cluster.md) を参照してください。

### 認証と認可の翻訳

HiveMQ は Enterprise Security Extension（ESE）で **レルム**（データソース）と **パイプライン**（ロジック）を管理し、またはレガシープラグインを利用します。EMQX は **認証チェーン**（順序付きバックエンド）と **認可ソース**（ACL）を使用します。

| HiveMQ ESE コンポーネント | EMQX 対応 | 移行戦略 |
| :--- | :--- | :--- |
| **ファイルレルム**（`credentials.xml`） | [**ビルトインデータベース**](../access-control/authn/mnesia.md) | HiveMQ ユーザーをエクスポートし、EMQX REST API でインポート。 |
| **SQL レルム**（JDBC） | [**MySQL**](../access-control/authn/mysql.md) / [**PostgreSQL**](../access-control/authn/postgresql.md) | `mysql` または `postgresql` バックエンドでパスワード認証を設定。既存ユーザーテーブルを再利用。 |
| **LDAP レルム** / AD | [**LDAP**](../access-control/authn/ldap.md) | LDAP バックエンドでパスワード認証を設定。HiveMQ の DN パターンを EMQX フィルターテンプレートにマッピング。 |
| **OAuth / JWT** | [**JWT**](../access-control/authn/jwt.md) | JWT 認証機構を設定。公開鍵または JWKS エンドポイントを設定。 |
| **HTTP / Webhooks** | [**HTTP サーバー**](../access-control/authn/http.md) | HTTP バックエンドでパスワード認証を設定し、外部認証サービスに委譲。 |
| **X.509 証明書** | [**X.509**](../access-control/authn/x509.md) / [**mTLS**](../network/emqx-mqtt-tls.md#enable-ssl-tls-with-two-way-authentication) | `TLS` リスナーと相互認証（双方向認証）を使用し、既存の CA とクライアント証明書を再利用。 |

#### ファイルレルムユーザーの移行

**ソース：** HiveMQ の `conf/credentials.xml`（暗号化・ハッシュ済み）

**宛先：** EMQX ビルトインデータベース

1. **エクスポート：** HiveMQ ファイルレルム（`credentials.xml`）からユーザーを抽出します。このファイルには通常、ハッシュ化されたパスワードとソルトが含まれます。XML を解析し、EMQX 用の JSON または CSV インポートファイルを生成してください。
2. **インポート：** EMQX REST API を使用してユーザーを作成します。EMQX は bcrypt や pbkdf2 などのパスワードハッシュを含むユーザーの一括インポートをサポートしています。ファイル形式の詳細は [ユーザーのインポート](../access-control/authn/user_management.md#importing-users) を参照してください。

```bash
# 例：プレーンパスワードでユーザーをインポート
curl -u admin:public -X POST \
  http://emqx-node:18083/api/v5/authentication/password_based:built_in_database/users \
  -d '{"user_id":"device-001","password":"StrongPass!"}'
```

#### 外部統合（SQL、LDAP、HTTP）の移行

`enterprise-security-extension.xml` のパイプラインを EMQX の HOCON `authentication` ブロックに翻訳します。

**例：SQL レルムから EMQX MySQL**

HiveMQ は SQL レルムに [固定のデータベーススキーマ](https://docs.hivemq.com/hivemq-enterprise-security-extension/latest/ese.html#table_users) を使用しますが、EMQX は [独自のスキーマとクエリを定義可能](../access-control/authn/mysql.md) です。**既存の MySQL または PostgreSQL データベースを変更する必要はありません。**

以下の EMQX 設定例は、標準的な HiveMQ `users` テーブル構造に合わせて調整されたクエリ（`SELECT password_hash, salt ...`）を使用しています。

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

HiveMQ は `enterprise-security-extension.xml`（ファイルレルム）や外部データベースでアクセスポリシーを定義します。EMQX は複数バックエンドを同時にサポートする柔軟な **認可チェーン** を使用します（ファイル、Redis、MySQL、PostgreSQL、MongoDB、HTTP など）。

**HiveMQ XML ポリシー：**

```xml
<permission>
    <topic>device/${clientid}/#</topic>
    <activity>ALL</activity>
</permission>
```

**EMQX 対応例：**

- [**ファイル（`acl.conf`）**](../access-control/authz/file.md)：`{allow, all, subscribe, ["device/${clientid}/#"]}.`
- [**ビルトインデータベース**](../access-control/authz/mnesia.md)：クライアント ID、ユーザー名、トピックに基づくルールをダッシュボードまたは API で設定。
- [**MySQL**](../access-control/authz/mysql.md)：`SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl where clientid = ${clientid} and ipaddress = ${peerhost}`。
- [**PostgreSQL**](../access-control/authz/postgresql.md)：`SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl where clientid = ${clientid} and ipaddress = ${peerhost}`。

詳細は [認可](../access-control/authz/authz.md) ドキュメントを参照してください。

### データ統合の設定

HiveMQ は個別の拡張（例：Kafka Extension）に依存してデータ統合を行いますが、EMQX はすべてのデータ統合機能を標準で備えています。

特定の統合を設定する前に、以下のコアコンセプトを理解してください：
- [**データ統合概要**](../data-integration/data-bridges.md)
- [**ルールエンジン**](../data-integration/rules.md)
- [**Flowデザイナー**](../flow-designer/introduction.md)

#### 例：Kafka Extension の移行

HiveMQ Kafka Extension 設定例：

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

### 可観測性の設定

#### Prometheus

HiveMQ は「Prometheus Monitoring HiveMQ Extension」を使用しますが、EMQX はネイティブで Prometheus をサポートしています。

Prometheus がメトリクスをスクレイプするエンドポイントはデフォルトで有効です：`http://emqx-node:18083/api/v5/prometheus/stats`。

EMQX 6.3.0 以降、Prometheus スクレイプエンドポイントはデフォルトで認証が必要です。API キーとシークレットキーでスクレイパーを設定するか、明示的に認証を無効化してください。詳細は [Prometheus との統合](../observability/prometheus.md#authentication) を参照してください。

Pushgateway を利用する場合は以下のように設定できます：

```hocon
prometheus {
  push_gateway {
    enable = true
    url = "http://127.0.0.1:9091"
  }
}
```

Pushgateway の設定詳細は [Prometheus との統合](../observability/prometheus.md#configure-push-mode-integration) を参照してください。

#### ロギング

HiveMQ は `logback.xml`（Java 標準）を使用しますが、EMQX は HOCON で設定可能な組み込みログ機能を使用します。

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

ログレベル、ローテーション、フォーマッター（テキスト/JSON）の設定は [ログ](../observability/log.md) を参照してください。

#### ログトレース

HiveMQ の「Trace Recordings」は特定クライアントセッションのデバッグに使われます。EMQX はダッシュボードまたは CLI で特定のクライアント ID、トピック、IP のログをリアルタイムにフィルタリングできる組み込みの **ログトレース** 機能を提供します。

特定クライアントのトレースを開始するには、以下のコマンドを使用します：

```bash
emqx ctl trace start client device-001 trace.log
```

詳細は [ログトレース](../observability/tracer.md) を参照してください。

## フェーズ 3：デバイスおよび統合の更新

### EMQX サーバ CA をデバイスに配布

- EMQX が内部 CA を使用している場合は、各デバイスに `device-ca.pem` をインストールしてください（システムトラストストアまたはアプリケーションバンドル）。
- EMQX が Let’s Encrypt などの公開 CA を使用している場合は、デバイス側の対応は不要です。

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

**例（Python paho-mqtt + mTLS）**

```hocon
client.tls_set(
    ca_certs="certs/device-ca.pem",
    certfile="certs/device-001.cert.pem",
    keyfile="certs/device-001.key.pem",
    tls_version=ssl.PROTOCOL_TLS_CLIENT
)
client.connect("mqtt.example.com", 8883)
```

変更点はエンドポイントホスト名とサーバ CA ファイルのみです。デバイス証明書と秘密鍵は、EMQX の `ssl_options.cacertfile` で参照される同じ CA によって署名されていれば引き続き使用可能です。

### 統合の検証

- Kafka トピックがメッセージを受信していることを EMQX ルールメトリクス（`emqx ctl rule show`）で確認してください。
- 監視ダッシュボードを更新し、EMQX メトリクスをスクレイプするように設定してください。
- アラートシステム（Splunk、ELK など）を EMQX のログフォーマットに合わせて再設定してください。

## 高度な移行シナリオ

### 保持メッセージとセッション

HiveMQ のパーシステンスファイルは直接インポートできません。移行スクリプトを使用してください：

1. HiveMQ を一時的に稼働させ続けます。
2. HiveMQ で `#` をサブスクライブし、保持メッセージを EMQX に再パブリッシュするブリッジクライアントを実行します。
3. QoS 1/2 のキューイングされたメッセージは、DNS 切り替え前にインフライトトランザクションを完了させます。

### 共有サブスクリプション

HiveMQ の `$share/group/topic` 構文は EMQX で完全にサポートされています。以前 `$queue/topic` を使用していた場合は、`$share/queue/topic` にマッピングしてください。`broker.shared_subscription_strategy`（例：`round_robin`、`hash_clientid`）を調整し、コンシューマーが期待するロードバランシング動作を模倣します。

### HTTP/API ベースの設定

HiveMQ は静的 XML と拡張固有のリロードセマンティクスに依存していますが、EMQX は動的設定 API を提供します：

```bash
curl -s -H "Authorization: Bearer $TOKEN" \
  -H "Content-type: application/json" \
  -X PUT "http://emqx-node:18083/api/v5/listeners/ssl:default" \
  -d '{"type": "ssl", "bind": "0.0.0.0:8883", "id": "ssl:default", "max_connections": 200000}'
```

これは `data/configs/cluster.hocon` に書き込まれます。設定を不変（`emqx.conf` のみ）にするか、環境ごとのオーバーライドを許容する EMQX の二層モデルを採用するかは検討してください。

## 検証チェックリスト

本番トラフィック切り替え前に以下を確認してください：

- すべての EMQX リスナーが `running` 状態であること（`emqx ctl listeners list`）。
- TLS ハンドシェイクが成功し、mTLS デバイスでクライアント証明書なしの場合は失敗すること。
- EMQX セッションのデバイス ID が元の HiveMQ クライアント ID と一致していること。
- ACL が HiveMQ と同様のトピックアクセスを強制していること。
- クラスターのノードがネットワーク分断後に自動復旧すること。
- Kafka 統合が変換なしでデータを受信していること。
- Prometheus でメトリクスが可視化されていること。

## 結論

HiveMQ から EMQX への移行は主に設定の翻訳作業です。Java 中心のアーティファクト（XML、JKS、拡張）を EMQX の HOCON 設定、柔軟な認証チェーン、データ統合フレームワークに変換します。

インベントリ、設定、更新の3フェーズを順に実施することで、デバイス認証情報、トピック構造、統合フローを維持しつつ、EMQX の高並行性な Erlang ランタイムと動的設定機能を活用できます。

移行計画を慎重に立て、各リスナーと統合を検証し、自信を持って切り替えを実行してください。
