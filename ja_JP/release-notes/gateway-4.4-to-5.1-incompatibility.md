# EMQX 4.4 と EMQX 5.1 間のゲートウェイ互換性

本ページでは、EMQX 4.4 と EMQX 5.1 間のゲートウェイ設定の互換性情報を紹介します。

## 共通の互換性変更点

### 設定

EMQX 4.x では、ゲートウェイは `etc/plugins/emqx_stomp.conf` のような設定ファイルや、ダッシュボード上のモジュール（`POST http://127.0.0.1:18084/api/v4/modules` インターフェース）を通じて設定できます。

EMQX 5.1 では、すべてのゲートウェイ設定は `etc/emqx.conf` または `PUT http://127.0.0.1:18083/api/v5/gateways/coap` で行います。

例えば、EMQX 4.x では：

```
stomp.listener = 61613
stomp.listener.acceptors = 4
stomp.listener.max_connections = 512
#stomp.listener.ssl = off
stomp.default_user.login = guest
stomp.default_user.passcode = guest
stomp.allow_anonymous = true

stomp.frame.max_headers = 10
stomp.frame.max_header_length = 1024
stomp.frame.max_body_length = 8192
```

EMQX 5.x では：

```
gateway.stomp {
    mountpoint = "stomp/"
    frame {
        max_headers = 10
        max_headers_length = 1024
        max_body_length = 65536
    }
    listeners.tcp.default {
        bind = "0.0.0.0:61613"
    }
}
```

### HTTP API やダッシュボードによる管理

EMQX 4.x では、管理用の専用 HTTP API やウェブページはありません。例えば、MQTT-SN のデバイス一覧を取得するには `GET http://127.0.0.1:8081/api/v4/clients?protocol=mqtt-sn` を使用し、MQTT デバイス（および他のプロトコル、例：CoAP、LwM2M など）とのクエリインターフェースが統合されています。

EMQX 5.x では、これらの機能を実現するために専用のインターフェースを多数提供しています。例えば、`GET /api/v5/gateways/mqttsn/clients` や以下の新規 HTTP API があります：

- [Gateways](https://docs.emqx.com/en/enterprise/v5.0/admin/api-docs.html#tag/Gateways)  
- [Gateway-Authentication](https://docs.emqx.com/en/enterprise/v5.0/admin/api-docs.html#tag/Gateway-Authentication)  
- [Gateway-Clients](https://docs.emqx.com/en/enterprise/v5.0/admin/api-docs.html#tag/Gateway-Clients)  

また、クライアント管理、ゲートウェイ設定、リスナー管理などの専用ダッシュボードページも提供しています。

### リスナー

EMQX 4.x では、各ゲートウェイごとにリスナー設定のフォーマットが異なっていました。しかし EMQX 5.1 では、すべてのリスナー設定フォーマットが統一されています。

例えば、EMQX 4.x では：

```
## etc/plugins/emqx_stomp.conf
stomp.listener = 61613
stomp.listener.acceptors = 4
stomp.listener.max_connections = 512
## etc/plugins/emqx_sn.conf
mqtt.sn.port = 1884
## etc/plugins/emqx_coap.conf
coap.bind.udp.1 = 0.0.0.0:5683
coap.bind.dtls.1 = 0.0.0.0:5684
## etc/plugins/emqx_lwm2m.conf
lwm2m.bind.udp.1 = 0.0.0.0:5683
lwm2m.bind.dtls.1 = 0.0.0.0:5684
```

EMQX 5.x では、すべてのプロトコルゲートウェイで同じフォーマットを使用します。Exproto ゲートウェイの例：

```
## etc/emqx.conf または base.hocon (EMQX 5.9 以降)
gateway.exproto {
    listeners.tcp.default {
        bind = "0.0.0.0:7993"
    }
    listeners.ssl.default {
        bind = "0.0.0.0:7994"
        cacertfile = ..
        certfile = ..
        keyfile = ..
    }
    listeners.udp.default { ... }
    listeners.dtls.default { ... }
}
```

### 認証

EMQX 4.x では、各ゲートウェイは MQTT 用のハイブリッド認証で設定されていました。

EMQX 5.0 以降は、ゲートウェイごとに別々の認証機構を設定する必要があります。例：

```
gateway.coap {
    ...
    authentication {
      backend = "http"
      method = "post"
      url = "http://127.0.0.1:8080/auth"
      headers {"content-type" = "application/json"}
      body {password = "${password}", username = "${username}"}
    }
}
```

## プロトコル機能および設定項目の非互換性

### Stomp

`stomp.default_user.login`、`stomp.default_user.passcode`、および `stomp.allow_anonymous` は EMQX 5.x で削除されました。

### MQTT-SN

- DTLS タイプのリスナーは EMQX 5.1 でサポートされていますが、EMQX 4.x では未対応です。  
- `mqtt.sn.username`、`mqtt.sn.password`、`mqtt.sn.subs_resume` は削除されていません。  
- `mqtt.sn.advertise_duration` は `gateway.mqttsn.broadcast` に名称変更されました。

### ExProto

以前の ConnectionAdapter サービス設定フォーマット：

```
exproto.server.http.port = 9100
exproto.server.https.port = 9101
exproto.server.https.cacertfile = etc/certs/cacert.pem
exproto.server.https.certfile = etc/certs/cert.pem
exproto.server.https.keyfile = etc/certs/key.pem
```

現在は：

```
gateway.exproto {
  server {
    bind = "0.0.0.0:9100"
    ssl_options {verify = "verify_none"}
  }
}
```

以前の ConnectionHandler 設定はリスナー上にありました：

```
exproto.listener.protoname.connection_handler_url = http://127.0.0.1:9001
#exproto.listener.protoname.connection_handler_certfile =
#exproto.listener.protoname.connection_handler_cacertfile =
#exproto.listener.protoname.connection_handler_keyfile =
```

現在は：

```
gateway.exproto {
  handler {
    address = "http://127.0.0.1:9001"
    ssl_options {enable = false}
  }
}
```

つまり、5.0 以降はリスニングポートごとに異なる ConnectionHandler サービスアドレスを指定できません。

### CoAP

CoAP プロトコルの実装仕様は完全に再設計されました。

新しい設計については [CoAP](../develop/gateway/coap.md) を参照してください。

### LwM2M

以前のオプション構造：

```
lwm2m.topics.command = dn/#
lwm2m.topics.response = up/resp
lwm2m.topics.notify = up/notify
lwm2m.topics.register = up/resp
lwm2m.topics.update = up/resp
```

現在の構造：

```
gateway.lwm2m {
  translators {
    command {qos = 0, topic = "dn/#"}
    notify {qos = 0, topic = "up/notify"}
    register {qos = 0, topic = "up/resp"}
    response {qos = 0, topic = "up/resp"}
    update {qos = 0, topic = "up/update"}
  }
}
```
