# HAProxyによるEMQXクラスターのロードバランス

HAProxyは、クライアントのネットワーク接続要求を複数のバックエンドサーバーに分散する、無料で高速かつ信頼性の高いロードバランスソフトウェアです。EMQXは複数のMQTTサーバーで構成される分散クラスターアーキテクチャをネイティブにサポートしています。HAProxyを用いてEMQXクラスターをデプロイすることで、IoTデバイスからのMQTT接続をロードバランスし、多数のデバイス接続をクラスター内の異なるEMQXノードに分散できます。

本ページでは、主にHAProxyのインストールと設定方法を説明し、EMQXクラスター内でMQTTサーバーのロードバランスを構築する手順を解説します。

## 特長とメリット

HAProxyを用いたEMQX MQTTロードバランスの主な特長とメリットは以下の通りです。

- HAProxyを介してEMQXクラスターをデプロイすることで、バックエンドノード情報をリバースプロキシの背後に隠蔽し、外部には統一されたアクセスアドレスを提供。システムの保守性とスケーラビリティを向上させます。
- MQTT over TLS接続の終端をサポートし、EMQXのSSL暗号化計算負荷を軽減。証明書の展開と管理を簡素化します。
- MQTTプロトコルをネイティブにサポートし、MQTTメッセージの解析によりセッションのスティッキー性やインテリジェントなロードバランス機構を実現。不正接続の検知も可能でセキュリティを強化します。
- プライマリ・スタンバイ構成の高可用性機構を備え、バックエンドのヘルスチェックと組み合わせることでミリ秒単位のフェイルオーバーを実現し、サービスの継続性を確保します。

![EMQX LB HAProxy](./assets/emqx-lb-haproxy.png)

## クイックスタート

以下はDocker Composeを用いた実践的な例で、簡単にセットアップと検証が可能です。手順は以下の通りです。

1. サンプルリポジトリをクローンし、`mqtt-lb-haproxy`ディレクトリに移動します。

```bash
git clone https://github.com/emqx/emqx-usage-example
cd emqx-usage-example/mqtt-lb-haproxy
```

2. Docker Composeでサンプルを起動します。

```bash
docker compose up -d
```

3. [MQTTX](https://mqttx.app/) CLIを使って10個のTCP接続を確立し、MQTTクライアント接続をシミュレートします。

```bash
mqttx bench conn -c 10
```

4. HAProxyの接続モニタリングとEMQXクライアント接続の分布状況を確認できます。

   - HAProxyのステータス監視ページ http://localhost:8888/stats でクライアント接続状況を確認可能です。

   ![HAProxy stats MQTT](./assets/haproxy-stats-mqtt.png)

   ここでは現在のアクティブ接続数やリクエスト処理統計が表示されます。

   - 各EMQXノードのクライアント接続状況は以下のコマンドで確認できます。

   ```bash
   docker exec -it emqx1 emqx ctl broker stats | grep connections.count
   docker exec -it emqx2 emqx ctl broker stats | grep connections.count
   docker exec -it emqx3 emqx ctl broker stats | grep connections.count
   ```

   これにより各ノードの接続数とアクティブ接続数が表示され、10接続がクラスター内のノードに均等に分散されていることが確認できます。

   ```bash
   connections.count             : 4
   live_connections.count        : 4
   connections.count             : 3
   live_connections.count        : 3
   connections.count             : 3
   live_connections.count        : 3
   ```

以上の手順で、HAProxyのロードバランス機能を検証し、EMQXクラスター内でのクライアント接続の分布を観察できます。設定は `emqx-usage-example/mqtt-lb-haproxy/haproxy.conf` ファイルを編集してカスタマイズ可能です。

## HAProxyのインストールと使用方法

ここではHAProxyのインストールと基本的な使用方法を詳しく紹介します。

### 前提条件

開始前に、以下の3つのEMQXノードで構成されたクラスターを作成している必要があります。EMQXクラスターの作成方法は[クラスターの作成](./create-cluster.md)を参照してください。

| ノードアドレス           | MQTT TCPポート | MQTT WebSocketポート |
| ------------------------ | ------------- | ------------------- |
| emqx1-cluster.emqx.io    | 1883          | 8083                |
| emqx2-cluster.emqx.io    | 1883          | 8083                |
| emqx3-cluster.emqx.io    | 1883          | 8083                |

本ページの例では、単一のHAProxyサーバーをロードバランサーとして設定し、これら3つのEMQXノードからなるクラスターにリクエストを分散します。

### HAProxyのインストール

Ubuntu 22.04 LTS環境でのHAProxyインストール手順は以下の通りです。

```bash
# パッケージインデックスの更新
sudo apt update 

# HAProxyのインストール
sudo apt install haproxy

# バージョン確認
haproxy -v
```

### はじめに

HAProxyの設定ファイルはデフォルトで `/etc/haproxy/haproxy.cfg` にあります。本ページの例を参考に設定をファイル末尾に追加してください。稼働中はHAProxyが `/var/log/haproxy.log` に継続的にログを出力するため、デバッグ時に確認できます。

HAProxyの基本的なコマンドは以下の通りです。

設定ファイルの文法チェック：

```bash
sudo haproxy -c -f /etc/haproxy/haproxy.cfg
```

HAProxyの起動：

```bash
sudo systemctl start haproxy
```

設定変更適用のためのリロード（事前に設定チェック推奨）：

```bash
sudo systemctl reload haproxy
```

HAProxyの停止：

```bash
sudo systemctl stop haproxy
```

HAProxyの稼働状況確認：

```bash
sudo systemctl status haproxy
```

## HAProxyのリバースプロキシおよびロードバランス設定

ここでは、HAProxyを様々なロードバランス要件に対応させる設定方法を説明します。

### 基本設定

HAProxyサーバーを起動するための参考設定例です。`haproxy.cfg`に以下の2つの設定項目が含まれていることを確認してください。

```bash
global  
  log 127.0.0.1 local3 info 
  daemon  
  maxconn 1024000

defaults  
  log global 
  mode tcp 
  option tcplog 
  #option dontlognull  
  timeout connect 10000 
  # timeout > mqttのキープアライブ * 1.2  
  timeout client 240s  
  timeout server 240s 
  maxconn 20000
```

### MQTTリバースプロキシ設定

以下の設定をHAProxyの設定ファイルに追加することで、MQTT接続をリバースプロキシし、クライアント要求をバックエンドMQTTサーバーにルーティングできます。

```bash
backend mqtt_backend
  mode tcp
  stick-table type string len 32 size 100k expire 30m
  stick on req.payload(0,0), mqtt_field_value(connect, client_identifier)

  # send-proxyを追加すると実IPがEMQXに渡され、対応するバックエンドリスナーはproxy_protocolを有効にする必要があります
  # server emqx1 emqx1-cluster.emqx.io:1883 check send-proxy-v2-ssl-cn
  server emqx1 emqx1-cluster.emqx.io:1883
  server emqx2 emqx2-cluster.emqx.io:1883
  server emqx3 emqx3-cluster.emqx.io:1883

frontend mqtt_servers
  bind *:1883
  mode tcp
  # MQTTメッセージ解析のためバッファが満たされるまで待機
  tcp-request inspect-delay 10s
  # MQTT以外の接続を拒否
  tcp-request content reject unless { req.payload(0,0), mqtt_is_valid }
  default_backend mqtt_backend
```

### MQTT SSLリバースプロキシ設定

以下の設定により、HAProxyがMQTTのTLS接続を終端し、クライアントからの暗号化されたMQTT要求をバックエンドMQTTサーバーに転送して通信の安全性を確保します。

基本的なTCP設定にSSLおよび証明書関連パラメータを追加してください。

:::tip Tip
HAProxyの証明書ファイルは証明書と秘密鍵の両方を含む必要があり、`cat`コマンドで1つのファイルに結合できます。

```bash
cat server.crt server.key > server.pem
```

:::

```bash
backend mqtt_backend
  mode tcp
  balance roundrobin
 
  # send-proxyを追加すると実IPがEMQXに渡され、対応するバックエンドリスナーはproxy_protocolを有効にする必要があります
  server emqx1 emqx1-cluster.emqx.io:1883 check-send-proxy send-proxy-v2-ssl-cn
  server emqx2 emqx2-cluster.emqx.io:1883 check-send-proxy send-proxy-v2-ssl-cn
  server emqx3 emqx3-cluster.emqx.io:1883 check-send-proxy send-proxy-v2-ssl-cn

frontend mqtt_tls_frontend
  bind *:8883 ssl crt /etc/haproxy/certs/server.pem 
  # 相互認証
  # bind *:8883 ssl ca-file /etc/haproxy/certs/cacert.pem crt /etc/haproxy/certs/server.pem verify required
  mode tcp
  default_backend mqtt_backend
```

### MQTT WebSocketリバースプロキシ設定

以下の設定により、HAProxyがMQTT WebSocket接続をリバースプロキシし、クライアント要求をバックエンドMQTTサーバーに転送します。`server_name`でHTTPのドメイン名やIPアドレスを指定してください。

```bash
backend mqtt_ws_backend
  mode tcp
  balance roundrobin
  server emqx1 emqx1-cluster.emqx.io:8083 check
  server emqx2 emqx2-cluster.emqx.io:8083 check
  server emqx3 emqx3-cluster.emqx.io:8083 check

frontend mqtt_ws_frontend
  bind *:8083 
  mode tcp
  default_backend mqtt_ws_backend
```

### MQTT WebSocket SSLリバースプロキシ設定

以下の設定により、HAProxyがMQTT WebSocket接続のTLSを終端し、クライアントからの暗号化されたMQTT要求をバックエンドMQTTサーバーに転送して通信の安全性を確保します。`server_name`でHTTPのドメイン名やIPアドレスを指定してください。

基本的なWebSocket設定にSSLおよび証明書関連パラメータを追加してください。

:::tip
HAProxyの証明書ファイルは証明書と秘密鍵の両方を含む必要があり、`cat`コマンドで1つのファイルに結合できます。

```bash
cat server.crt server.key > server.pem
```

:::

```bash
backend mqtt_ws_backend
  mode tcp
  balance roundrobin
  server emqx1 emqx1-cluster.emqx.io:8083 check
  server emqx2 emqx2-cluster.emqx.io:8083 check
  server emqx3 emqx3-cluster.emqx.io:8083 check

frontend mqtt_ws_tls_frontend
  bind *:8084 ssl crt /etc/haproxy/certs/server.pem
  mode tcp 
  default_backend mqtt_ws_backend
```

### ロードバランス戦略の設定

HAProxyは接続の分散方法を制御する様々なロードバランス戦略を提供しています。実際の運用では、サーバー性能やトラフィック要件などに応じて適切な戦略を選択することが重要です。

以下にHAProxyがサポートするロードバランス戦略と設定例を示します。

#### ラウンドロビン

デフォルトのロードバランス戦略で、リクエストをバックエンドサーバーに順番に循環して割り当てます。負荷を均等に分散し、バックエンドサーバーの性能がほぼ同等の場合に適しています。

```bash
backend mqtt_backend
  mode tcp
  balance roundrobin
  server emqx1 emqx1-cluster.emqx.io:1883 check
  server emqx2 emqx2-cluster.emqx.io:1883 check
  server emqx3 emqx3-cluster.emqx.io:1883 check
```

#### 重み付きラウンドロビン

ラウンドロビンをベースに、各EMQXノードに異なる重みを割り当ててリクエストの分布を調整します。重みの高いサーバーほど多くのリクエストを受け取ります。

```bash
backend mqtt_backend
  mode tcp
  balance roundrobin
  server emqx1 emqx1-cluster.emqx.io:1883 check weight 5
  server emqx2 emqx2-cluster.emqx.io:1883 check weight 2
  server emqx3 emqx3-cluster.emqx.io:1883 check weight 3
```

#### IPハッシュ

クライアントのIPアドレスを基にハッシュを計算し、リクエストを固定のバックエンドサーバーに割り当てます。同一クライアントからのリクエストは常に同じサーバーに送られるため、セッションの一貫性を保てます。

```bash
backend mqtt_backend
  mode tcp
  balance source
  server emqx1 emqx1-cluster.emqx.io:1883
  server emqx2 emqx2-cluster.emqx.io:1883
  server emqx3 emqx3-cluster.emqx.io:1883
```

#### 最小接続数

現在の接続数が最も少ないサーバーにリクエストを割り当て、負荷をできるだけ均等に分散します。サーバー性能に大きな差がある場合に適しています。

```bash
backend mqtt_backend
  mode tcp
  balance leastconn
  server emqx1 emqx1-cluster.emqx.io:1883
  server emqx2 emqx2-cluster.emqx.io:1883
  server emqx3 emqx3-cluster.emqx.io:1883
```

### MQTTスティッキーセッションロードバランスの設定

MQTTのスティッキーセッションロードバランスはHAProxy 2.4で導入されました。

「スティッキー」とは、クライアントが再接続する際に同じサーバーにルーティングし、MQTTのセッション奪取を防ぐ機能を指します。複数クライアントが頻繁に再接続する場合や問題のあるクライアントが断続的に接続・切断を繰り返す場合に効果的です。

スティッキーセッションを実装するには、サーバーが接続要求内のクライアント識別子（通常はクライアントID）を特定する必要があります。これにはロードバランサーがMQTTパケットを解析する必要があります。クライアント識別子を取得後、静的クラスターではハッシュでサーバーIDに変換したり、ロードバランサーがクライアント識別子とターゲットノードIDのマッピングテーブルを保持して柔軟にルーティングできます。

```bash
backend mqtt_backend
  mode tcp
  # スティッキーセッション用テーブルを作成
  stick-table type string len 32 size 100k expire 30m

  # クライアントIDをキーとして使用
  stick on req.payload(0,0), mqtt_field_value(connect, client_identifier)
 
  server emqx1 emqx1-cluster.emqx.io:1883
  server emqx2 emqx2-cluster.emqx.io:1883
  server emqx3 emqx3-cluster.emqx.io:1883
```

## HAProxyのステータス監視

HAProxyは特別なフロントエンドを設定することでステータス監視を有効化できます。これにより、各バックエンド・フロントエンドの接続状況やグローバルな接続統計を閲覧可能です。詳細は[Exploring the HAProxy Stats Page](https://www.haproxy.com/blog/exploring-the-haproxy-stats-page)を参照してください。

```bash
frontend stats
  mode http
  bind *:8888
  stats enable
  stats uri /stats
  stats refresh 10s
```

http://localhost:8888/stats を開くとステータスデータが表示されます。

![HAProxy stats Page](./assets/haproxy-stats-all.png)

### HAProxy高可用性ソリューションの紹介

HAProxyとKeepalivedは、高可用性かつロードバランスを実現する一般的な組み合わせです。KeepalivedはLinux向けの軽量な高可用性ソリューションで、複数サーバー間で仮想IPアドレス（VIP）を管理し、サーバー障害時にVIPを別のサーバーに移動させて高可用性を提供します。さらにKeepalivedはHAProxyプロセスの監視と必要に応じた再起動も行い、ロードバランスサービスの可用性を確保します。

Keepalivedを利用することでHAProxyの高可用性を実現できます。プライマリHAProxyサーバーが障害を起こした場合、Keepalivedが自動的にVIPをバックアップサーバーに移動し、サービスの継続性を保証します。このソリューションの実装方法は[HAProxyドキュメント](https://www.haproxy.com/documentation/hapee/latest/high-availability/active-standby/)を参照してください。

## さらに詳しく

EMQXはHAProxyに関する豊富なリソースを提供しています。以下のリンクから詳細情報をご覧ください。

**ブログ：**

- [HAProxyベースのEMQXクラスター構築](https://www.emqx.com/en/blog/emqx-haproxy)
- [スティッキーセッションロードバランス - MQTTブローカークラスタリングパート2](https://www.emqx.com/en/blog/mqtt-broker-clustering-part-2-sticky-session-load-balancing)
