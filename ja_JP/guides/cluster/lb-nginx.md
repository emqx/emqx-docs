# NGINXによるEMQXクラスターのロードバランス

NGINXは高性能で多機能なサーバーソフトウェアであり、ウェブサーバーやリバースプロキシサーバーとして機能します。さらに、NGINXはロードバランサーとしても動作し、クライアントからのリクエストを複数のバックエンドサーバーに分散させることで、負荷分散とパフォーマンスの最適化を実現します。NGINXは特に、多数の同時リクエストを処理する必要があるIoTアプリケーションに適しています。IoTでは多数のデバイスが存在するため、高負荷のリクエストを処理可能なサーバーが求められます。EMQXは複数のMQTTサーバーからなる分散クラスターアーキテクチャをネイティブにサポートしています。したがって、NGINXをロードバランサーとして導入しEMQXクラスターと組み合わせることで、高可用性とスケーラビリティを確保できます。

本ページでは、NGINXのインストール方法と、リバースプロキシおよびロードバランス用途のNGINX設定方法を紹介し、EMQXクラスター用MQTTサーバーの構築方法を説明します。また、NGINX Plusを用いたEMQX展開の最適化方法も紹介します。

## 特長と利点

NGINXを用いてEMQXクラスターのロードバランスを行うことで、以下のような特長と利点があります。

- リバースプロキシサーバーとしてNGINXはMQTTサーバー側に位置し、MQTTクライアントを代表してEMQXクラスターへのMQTT接続要求を開始し、EMQXクラスターの応答をMQTTクライアントに返します。この構成により複数のクラスターを隠蔽し、MQTTクライアントには単一のアクセスポイントを公開します。MQTTクライアントはNGINXとだけ通信すればよく、背後のクラスターの数や配置を意識する必要がありません。この方式はシステムの保守性とスケーラビリティを向上させます。
- NGINXはMQTTクライアントとEMQXクラスター間のSSL暗号化接続を終了（ターミネート）でき、EMQXクラスターの暗号化・復号処理負荷を軽減します。これにより、パフォーマンス向上、証明書管理の簡素化、セキュリティ強化などの利点があります。
- NGINXは柔軟なロードバランス戦略を提供し、クラスター内のどのEMQXノードにリクエストを送るかを制御できます。これによりトラフィックやリクエストの分散が可能となり、パフォーマンスと信頼性が向上します。例えば、スティッキー（sticky）ロードバランスは同一クライアントのリクエストを同じバックエンドサーバーに振り分け、パフォーマンスとセッションの持続性を高めます。

![EMQX LB NGINX](./assets/emqx-lb-nginx.png)

## クイックスタート

このセクションでは、実際の例を用いたDocker Compose構成を紹介し、NGINXの機能を簡単に検証・テストできるようにします。以下の手順で進めてください。

1. サンプルリポジトリをクローンし、`mqtt-lb-nginx`ディレクトリに移動します。

```bash
git clone https://github.com/emqx/emqx-usage-example
cd emqx-usage-example/mqtt-lb-nginx
```

2. Docker Composeでサンプルを起動します。

```bash
docker compose up -d
```

3. [MQTTX](https://mqttx.app) CLIを使い、MQTTクライアント接続を模擬して10個のTCP接続を確立します。

```bash
mqttx bench conn -c 10
```

4. NGINXの接続状況とEMQXクライアント接続の分布を確認できます。

   - 以下のコマンドでNGINXの接続状況を確認します。

     ```bash
     $ curl http://localhost:8888/status                                
     Active connections: 11 
     server accepts handled requests
      60 60 65 
     Reading: 0 Writing: 1 Waiting: 0
     ```

     これは現在のアクティブ接続数やサーバーのリクエスト処理統計（読み込み、書き込み、待機状態）を表示します。

   - 以下のコマンドで各EMQXノードのクライアント接続状況を確認します。

     ```bash
     docker exec -it emqx1 emqx ctl broker stats | grep connections.count
     docker exec -it emqx2 emqx ctl broker stats | grep connections.count
     docker exec -it emqx3 emqx ctl broker stats | grep connections.count
     ```

     各ノードの接続数とアクティブ接続数が表示され、10接続がクラスター内のノードに均等に分散されていることがわかります。

     ```bash
     connections.count             : 3
     live_connections.count        : 3
     connections.count             : 4
     live_connections.count        : 4
     connections.count             : 3
     live_connections.count        : 3
     ```

これらの手順で、NGINXのロードバランス機能とEMQXクラスター内のクライアント接続分布を検証できます。`emqx-usage-example/mqtt-lb-nginx/nginx.conf`ファイルを編集してカスタム設定の検証も可能です。

## NGINXのインストールと利用

このセクションでは、NGINXのインストール方法と利用方法を詳しく説明します。

### 前提条件

開始前に、以下の3つのEMQXノードからなるクラスターを作成していることを確認してください。EMQXクラスターの作成方法は[クラスターの作成](./create-cluster.md)を参照してください。

| ノードアドレス           | MQTT TCPポート | MQTT WebSocketポート |
| ------------------------ | -------------- | -------------------- |
| emqx1-cluster.emqx.io    | 1883           | 8083                 |
| emqx2-cluster.emqx.io    | 1883           | 8083                 |
| emqx3-cluster.emqx.io    | 1883           | 8083                 |

本ページの例では、単一のNGINXサーバーをロードバランサーとして設定し、これら3つのEMQXノードからなるクラスターにリクエストを転送します。

### NGINXのインストール

デモではUbuntu 22.04 LTS上にソースコードからNGINXをインストールします。Dockerやバイナリパッケージを利用してインストールすることも可能です。

#### 必要な依存関係

NGINXのコンパイルとインストール前に、以下の依存関係がシステムにインストールされていることを確認してください。

- GNU CおよびC++コンパイラ
- PCRE（Perl Compatible Regular Expressions）ライブラリ
- zlib圧縮ライブラリ
- OpenSSLライブラリ

Ubuntuでは以下のコマンドでインストールできます。

```bash
sudo apt-get update
sudo apt-get install build-essential libpcre3-dev zlib1g-dev libssl-dev
```

#### ソースコードのダウンロード

NGINXの最新安定版は[NGINX公式サイト](https://nginx.org/en/download.html)からダウンロード可能です。例：

```bash
wget https://nginx.org/download/nginx-1.24.0.tar.gz
```

#### コンパイル設定

ダウンロード後、ソースコードを展開し、ディレクトリに移動します。

```bash
tar -zxvf nginx-1.24.0.tar.gz
cd nginx-1.24.0
```

以下のコマンドでコンパイルオプションを設定します。

```bash
./configure \
 --with-threads \
 --with-http_stub_status_module \
  --with-http_ssl_module \
  --with-http_realip_module \
  --with-stream \
  --with-stream_ssl_module
```

上記の`--with-http_ssl_module`はSSLサポート追加、`--with-stream`と`--with-stream_ssl_module`はTCPリバースプロキシサポート追加のためのパラメータです。

#### コンパイル開始

以下のコマンドでコンパイルを開始します。

```bash
make
```

#### インストール

コンパイル後、以下のコマンドでNGINXをインストールします。

```bash
sudo make install
```

システムのPATHにあるディレクトリにNGINX実行ファイルへのシンボリックリンクを作成します。

```bash
sudo ln -s /usr/local/nginx/sbin/nginx /usr/local/bin/nginx
```

### 利用開始

NGINXの設定ファイルはデフォルトで`/usr/local/nginx/conf/nginx.conf`にあります。本ページの設定例をファイル末尾に追記してください。基本的なNGINX操作コマンドは以下の通りです。

設定ファイルの検証：

```bash
sudo nginx -t
```

設定ファイルが正常ならNGINXを起動：

```bash
sudo nginx
```

稼働中のNGINXに設定変更を反映するには、事前に設定検証を行い、リロードします。

```bash
sudo nginx -s reload
```

NGINXを停止する場合：

```bash
sudo nginx stop
```

## MQTTのリバースプロキシおよびロードバランス用NGINX設定

このセクションでは、さまざまなロードバランス要件に対応するNGINX設定方法を説明します。

### MQTTのリバースプロキシ設定

以下の設定をNGINXの設定ファイルに記述することで、クライアントからのMQTT接続要求をリバースプロキシし、バックエンドのMQTTサーバーに転送できます。

```bash
stream {
  upstream mqtt_servers {
    # down: 現在サーバーが一時的にロードバランス対象外であることを示す
    # max_fails: 許容される失敗リクエスト数（デフォルトは1）
    # fail_timeout: max_failsに達した際の失敗リクエストのタイムアウト（デフォルトは10秒）
    # backup: 非バックアップサーバーが全てダウンまたはビジー時にリクエストを受けるバックアップサーバー

    server emqx1-cluster.emqx.io:1883 max_fails=2 fail_timeout=10s;
    server emqx2-cluster.emqx.io:1883 down;
    server emqx3-cluster.emqx.io:1883 backup;
  }

  server {
    listen 1883;
    proxy_pass mqtt_servers;

    # このオプションを有効にする場合、対応するバックエンドリスナーもproxy_protocolを有効にする必要があります
    proxy_protocol on;
    proxy_connect_timeout 10s;
    # デフォルトのキープアライブ時間は10分
    proxy_timeout 1800s;
    proxy_buffer_size 3M;
    tcp_nodelay on;
  }
}
```

### MQTT SSLのリバースプロキシ設定

NGINXでMQTTのTLS接続を終端し、クライアントからの暗号化されたMQTTリクエストをバックエンドMQTTサーバーに転送して通信の安全性を確保できます。TCPベースの設定にSSL関連パラメータを追加するだけです。

```bash
stream {
  upstream mqtt_servers {
    server emqx1-cluster.emqx.io:1883;
    server emqx2-cluster.emqx.io:1883;
  }

  server {
    listen 8883 ssl;

    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    ssl_certificate /usr/local/nginx/certs/emqx.pem;
    ssl_certificate_key /usr/local/nginx/certs/emqx.key;
    ssl_verify_depth 2;
    ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers HIGH:!aNULL:!MD5;

    # 相互認証を有効にする場合はCA証明書とクライアント証明書検証を追加
    # ssl_client_certificate /usr/local/nginx/certs/ca.pem;
    # ssl_verify_client on;
    # ssl_verify_depth 1;

    proxy_pass mqtt_servers;

    # このオプションを有効にする場合、対応するバックエンドリスナーもproxy_protocolを有効にする必要があります
    proxy_protocol on;
    proxy_connect_timeout 10s;
    # デフォルトのキープアライブ時間は10分
    proxy_timeout 1800s;
    proxy_buffer_size 3M;
    tcp_nodelay on;
  }
}
```

### MQTT WebSocketのリバースプロキシ設定

以下の設定で、NGINXがMQTT WebSocket接続をリバースプロキシし、クライアントからのリクエストをバックエンドMQTTサーバーに転送します。`server_name`でHTTPのドメイン名またはIPアドレスを指定してください。

EMQX 6.3.0以降、WebSocketリスナーはデフォルトで転送されたクライアントアドレスヘッダーを読みません。NGINXが設定する`X-Forwarded-For`ヘッダーを利用するには、各バックエンドEMQXノードの`base.hocon`に以下を追加してください。

```hocon
listeners.ws.default.websocket.proxy_address_header = "x-forwarded-for"
```

```bash
http {
  upstream mqtt_websocket_servers {
    server emqx1-cluster.emqx.io:8083;
    server emqx2-cluster.emqx.io:8083;
  }

  server {
    listen 80;
    server_name mqtt.example.com;

    location /mqtt {
      proxy_pass http://mqtt_websocket_servers;

      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection "Upgrade";

      # キャッシュ無効化
      proxy_buffering off;

      proxy_connect_timeout 10s;
      # WebSocket接続タイムアウト
      # この時間内にデータ交換がなければ自動的に切断（デフォルト60秒）
      proxy_send_timeout 3600s;
      proxy_read_timeout 3600s;

      # リバースプロキシの実IP設定
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header REMOTE-HOST $remote_addr;
      proxy_set_header X-Forwarded-For $remote_addr;
    }
  }
}
```

::: tip
WebSocketの例では、EMQXが`proxy_address_header`設定時に`X-Forwarded-For`ヘッダーの最左（最初）の値を読み取るため、`X-Forwarded-For`を`$remote_addr`で上書きしています。`$proxy_add_x_forwarded_for`を使うと、クライアント由来の偽装可能な値が最左に残ってしまうため避けてください。詳細は[転送クライアントアドレス](../configuration/listener.md#forwarded-client-address-websocket-listeners)を参照してください。
:::

### MQTT WebSocket SSLのリバースプロキシ設定

NGINXでMQTT WebSocketのTLS接続を終端し、クライアントからの暗号化されたMQTTリクエストをバックエンドMQTTサーバーに転送して通信の安全性を確保します。`server_name`でHTTPのドメイン名またはIPアドレスを指定してください。WebSocketベースの設定にSSLおよび証明書関連パラメータを追加するだけです。

```bash
http {
  upstream mqtt_websocket_servers {
    server emqx1-cluster.emqx.io:8083;
    server emqx2-cluster.emqx.io:8083;
  }

  server {
    listen 443 ssl;
    server_name mqtt.example.com;

    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    ssl_certificate /usr/local/nginx/certs/emqx.pem;
    ssl_certificate_key /usr/local/nginx/certs/emqx.key;
    ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers HIGH:!aNULL:!MD5;

    # 相互認証を有効にする場合はCA証明書とクライアント証明書検証を追加
    # ssl_client_certificate /usr/local/nginx/certs/ca.pem;
    # ssl_verify_client on;

    location /mqtt {
        proxy_pass http://mqtt_websocket_servers;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "Upgrade";

        # リバースプロキシの実IP設定
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header REMOTE-HOST $remote_addr;
        proxy_set_header X-Forwarded-For $remote_addr;

        # キャッシュ無効化
        proxy_buffering off;
    }
  }
}
```

### ロードバランス戦略の設定

NGINXは接続分散の制御に複数のロードバランス戦略を提供します。実際の運用では、サーバー性能やトラフィック要件に応じて適切な戦略を選択することが重要です。以下は`upstream`ブロックで設定可能な代表的なNGINXロードバランス戦略です。

#### ラウンドロビン

デフォルトのロードバランス戦略です。リクエストを各バックエンドサーバーに順番に均等に分配します。バックエンドサーバーの性能がほぼ同等の場合に適しています。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

#### 重み付きラウンドロビン

ラウンドロビンに加え、各EMQXノードに異なる重みを割り当て、リクエストの分配比率を調整します。重みの高いサーバーほど多くのリクエストを受けます。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883 weight=3;
  server emqx2-cluster.emqx.io:1883 weight=2;
  server emqx3-cluster.emqx.io:1883 weight=1;
}
```

#### IPハッシュ

クライアントのIPアドレスに基づいてハッシュを計算し、特定のバックエンドサーバーにリクエストを割り当てます。同一クライアントからのリクエストは常に同じサーバーにルーティングされます。

```bash
upstream backend_servers {
  ip_hash;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

#### 最小接続数

現在の接続数が最も少ないサーバーにリクエストを割り当て、各サーバーの負荷を均等化します。サーバー性能に大きな差がある場合に適しています。

```bash
upstream backend_servers {
  least_conn;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

## NGINX Plusを用いたEMQX展開の最適化

このセクションでは、NGINX Plus固有の機能を設定してEMQX展開を最適化する方法を紹介します。本節の機能はNGINX Plus版のみで利用可能であり、本ページでコンパイル・インストールしたNGINXでは参照できません。NGINX Plusを用いたMQTT接続最適化の詳細は[こちらのドキュメント](https://www.nginx.com/blog/optimizing-mqtt-deployments-in-enterprise-environments-nginx-plus/)を参照してください。

### MQTTスティッキーセッションロードバランスの設定

「スティッキー」とは、ロードバランサーがクライアントの再接続時に同じサーバーにルーティングし、セッションの乗っ取りを防ぐ機能です。頻繁に再接続するクライアントや問題のあるクライアントの効率化に役立ちます。

スティッキーを実現するには、サーバーが接続要求内のクライアント識別子（通常はクライアントID）を特定する必要があります。ロードバランサーがMQTTパケットを解析しクライアントIDを取得した後、静的クラスターではクライアントIDをハッシュしてサーバーIDに変換するか、ロードバランサーがクライアントIDと宛先ノードIDのマッピングテーブルを保持して柔軟にルーティングします。

以下はこの機能の設定例です。

```bash
mqtt_preread on;

upstream backend_servers {
    hash $mqtt_preread_clientid consistent;
    server emqx1-cluster.emqx.io:1883;
    server emqx2-cluster.emqx.io:1883;
    server emqx3-cluster.emqx.io:1883;
}
```

上記例は環境に応じて調整が必要な場合があります。設定で使用されるモジュール（`ip_hash`や`least_conn`など）はNGINX組み込みモジュールであり、追加のモジュール依存はありません。

### クライアントID置換機能の設定

MQTT通信におけるセキュリティは重要です。デバイスはシリアル番号などの機密情報をクライアントIDとして使用することが多く、MQTTサーバーのデータベースに保存することはセキュリティリスクとなります。NGINX PlusはクライアントID置換機能を提供し、NGINX Plusの設定で別の値にクライアントIDを置き換えられます。

以下はこの機能の設定例です。

```bash
stream {
    mqtt on;

    server {
        listen 1883 ssl;
        ssl_certificate /etc/NGINX/certs/emqx.pem;
        ssl_certificate_key /etc/NGINX/certs/emqx.key;
        ssl_client_certificate /etc/NGINX/certs/ca.crt;      
        ssl_session_cache shared:SSL:10m;
        ssl_verify_client on;
        proxy_pass 10.0.0.113:1883;
        proxy_connect_timeout 1s;  

        mqtt_set_connect clientid $ssl_client_serial;
    }
}
```

この例ではクライアントの相互認証を有効にし、クライアントSSL証明書のシリアル番号を一意の識別子として取得し、元のクライアントIDを置換しています。`$ssl_client_s_dn`など他の値を用いて証明書DNを抽出することも可能です。

## NGINXのパフォーマンス最適化とモニタリング有効化

このセクションでは、NGINXのパフォーマンスを設定で最適化し、ステータスモニタリング機能を有効にする方法を説明します。

### NGINX基本設定の調整

- `worker_processes`：ワーカープロセス数。サーバーのCPUコア数に近い値に設定しますが、多すぎるとリソース競合が発生します。
- `worker_connections`：単一ワーカープロセスが処理可能な同時接続数の最大値。OSのファイルディスクリプタ上限を超えないように設定してください。

```bash
worker_processes auto;

events {
 worker_connections 20480;
}
```

### リバースプロキシにおけるNGINXのマルチNIC対応による大量接続処理

リバースプロキシではNGINXがクライアントとしてバックエンドEMQXノードに接続を確立します。この場合、単一IPアドレスで約6万の長時間接続が上限となります。より多くの接続をサポートするには、複数のNGINXサーバーを展開するか、複数のIPアドレスを設定します。

以下はNGINX組み込みの`split_clients`モジュールを使い、クライアントのIPアドレスとポート番号に基づいて変数`$multi_ip`を定義し、複数IPを分散利用する例です。使用するIPアドレスはローカルで利用可能なものを指定してください。

```bash
stream {
 split_clients "$remote_addr$remote_port" $multi_ip {
    20% 10.211.55.5;
    20% 10.211.55.20;
    20% 10.211.55.21;
    20% 10.211.55.22;
    * 10.211.55.23;
  }

  upstream mqtt_servers {
    server emqx1-cluster.emqx.io:1883;
    server emqx2-cluster.emqx.io:1883;
  }

  server {
    listen 1883;

    proxy_pass mqtt_servers;
    proxy_bind $multi_ip;
  }
}
```

### NGINXステータスモニタリング

NGINXのステータスモニタリングを有効にするには、`http_stub_status_module`モジュールがインストールされている必要があります。インストール済みであれば、以下のように設定してNGINXのステータス監視を有効にできます。

```bash
http {
  server {
    listen 8888;

    location /status {
      stub_status on;
      access_log off;
    }
  }
}
```

http://localhost:8888/status にアクセスするとステータス情報が得られます。

```bash
$ curl http://localhost:8888/status
Active connections: 12
server accepts handled requests
 25 25 60
Reading: 0 Writing: 1 Waiting: 1
```

## 付録：主なパラメータの説明

以下は例示した設定で使用される主なパラメータの説明です。これらはバックエンドMQTTサーバーへの安定した接続を確保し、NGINX経由でMQTT通信を暗号化・保護しつつ、IoTアプリケーションにおける通信のプライバシーと整合性を守るためのベストプラクティスです。

| パラメータ名            | 説明                                                         |
| ----------------------- | ------------------------------------------------------------ |
| proxy_protocol          | PROXYプロトコルを有効にし、NGINXが接続先へ接続開始時に追加のプロキシ情報を付加。これによりEMQXは実際のクライアントIPを取得可能。 |
| proxy_pass              | バックエンドMQTTサーバーのアドレスを定義し、クライアントからの全リクエストをここに転送。 |
| proxy_connect_timeout   | バックエンドMQTTサーバーへの接続確立タイムアウト。時間内に接続できなければNGINXは接続試行を中止。 |
| proxy_timeout           | バックエンドMQTTサーバーの応答タイムアウト。時間内に応答がなければNGINXは接続を切断。 |
| proxy_buffer_size       | バックエンドMQTTサーバーから受信したデータを格納するバッファサイズ。大きなデータストリームに対応可能な十分なサイズを確保。 |
| tcp_nodelay             | TCP_NODELAYオプションを有効にし、Nagleアルゴリズムを無効化。パケット送信のレイテンシを低減し、リアルタイムMQTT通信に有利。 |
| ssl_session_cache       | 共有SSLセッションキャッシュを設定。SSLセッションの状態を保存し、クライアント再接続時のハンドシェイク高速化に寄与。`shared:SSL:10m`はキャッシュ名とサイズ（10MB）を指定。 |
| ssl_session_timeout     | SSLセッションの有効期限を10分に設定。期限内に再利用されなかったセッションは削除。 |
| ssl_certificate         | SSL証明書ファイルのパス。サーバーの身元証明に使用。 |
| ssl_certificate_key     | SSL証明書に対応する秘密鍵ファイルのパス。 |
| ssl_protocols           | 許可するSSL/TLSプロトコルのバージョンを指定。                      |
| ssl_ciphers             | 許可する暗号化アルゴリズム（暗号スイート）を設定。`HIGH:!aNULL:!MD5`は強力な暗号スイートを使用し、空の暗号スイートやMD5ハッシュアルゴリズムを除外。 |
| ssl_client_certificate  | クライアント証明書の検証に使用する認証局（CA）証明書ファイルのパス。 |
| ssl_verify_client       | クライアント証明書の検証を有効化。`on`に設定するとNGINXはクライアントに有効なSSL証明書の提示を要求。 |
| ssl_verify_depth        | クライアント証明書検証の最大深度を設定。ここでは`1`でクライアント証明書とCA証明書の1段階検証を意味。 |

## 参考情報

EMQXはNGINXに関する豊富なリソースを提供しています。以下のリンクもご参照ください。

**ブログ:**

- [NGINX PlusによるEMQXのスティッキーセッション活用：'Client ID'を魔法の鍵に](https://www.emqx.com/en/blog/harnessing-sticky-sessions-for-mqtt-load-balancing-with-nginx-plus)
- [NGINX PlusのクライアントID置換とEMQX EnterpriseによるMQTTアプリケーションのセキュリティ強化](https://www.emqx.com/en/blog/securing-your-mqtt-based-applications-with-nginx-plus-client-id-substitution-and-emqx-enterprise)
- [EMQXとNGINX Plusによるクライアント証明書認証でMQTTセキュリティを向上](https://www.emqx.com/en/blog/elevating-mqtt-security-with-client-certificate-authentication)
