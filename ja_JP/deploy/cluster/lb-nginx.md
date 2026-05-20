# NGINXによるEMQXクラスターのロードバランス

NGINXは高性能で多機能なサーバーソフトウェアであり、ウェブサーバーやリバースプロキシサーバーとして動作できます。さらに、NGINXはロードバランサーとしても機能し、クライアントからのリクエストを複数のバックエンドサーバーに分散させることで、負荷分散とパフォーマンスの最適化を実現します。NGINXは大量の同時リクエストを処理する必要があるIoTアプリケーションに特に適しています。IoTでは多数のデバイスが存在するため、高いリクエスト負荷に対応可能なサーバーが求められます。EMQXは複数のMQTTサーバーからなる分散クラスターアーキテクチャをネイティブにサポートしています。そのため、NGINXをロードバランサーとして導入し、EMQXクラスターを構築することで、高可用性とスケーラビリティを確保できます。

本ページでは、NGINXのインストール方法と、EMQXクラスター用MQTTサーバーのリバースプロキシおよびロードバランス設定方法を紹介します。また、NGINX Plusを用いたEMQXデプロイメントの最適化方法も解説します。

## 特長とメリット

NGINXを用いてEMQXクラスターのロードバランスを行うことで、以下のような特長と利点があります。

- リバースプロキシサーバーとして、NGINXはMQTTサーバー側に位置し、MQTTクライアントを代表してEMQXクラスターへのMQTT接続要求を開始し、EMQXクラスターの応答をMQTTクライアントに返します。この構成により複数のクラスターを隠蔽し、MQTTクライアントには単一のアクセスポイントを公開します。MQTTクライアントはNGINXとだけ通信すればよく、背後のクラスターの数や配置を意識する必要がありません。この方式はシステムの保守性とスケーラビリティを向上させます。
- NGINXはMQTTクライアントとEMQXクラスター間のSSL暗号化されたMQTT接続を終端できるため、EMQXクラスターの暗号化・復号化負荷を軽減します。これによりパフォーマンス向上、証明書管理の簡素化、セキュリティ強化などのメリットが得られます。
- NGINXは柔軟なロードバランス戦略を提供し、クラスター内のどのEMQXノードにリクエストを送るかを制御できます。これによりトラフィックやリクエストの分散が可能となり、パフォーマンスと信頼性が向上します。例えば、スティッキーなロードバランスは同一のバックエンドサーバーにリクエストをルーティングし、パフォーマンスとセッションの持続性を高めます。

![EMQX LB NGINX](./assets/emqx-lb-nginx.png)

## クイックスタート

このセクションでは、実際の例を用いたDocker Compose構成を紹介し、NGINXの機能を簡単に検証・テストできるようにします。以下の手順に従ってください。

1. 例のリポジトリをクローンし、`mqtt-lb-nginx`ディレクトリに移動します。

```bash
git clone https://github.com/emqx/emqx-usage-example
cd emqx-usage-example/mqtt-lb-nginx
```

2. Docker Composeで例を起動します。

```bash
docker compose up -d
```

3. [MQTTX](https://mqttx.app) CLIを使い、MQTTクライアント接続を模擬して10のTCP接続を確立します。

```bash
mqttx bench conn -c 10
```

4. NGINXの接続モニタリングおよびEMQXクライアント接続の分布状況を確認できます。

   - 以下のコマンドでNGINXの接続モニタリングを確認します。

     ```bash
     $ curl http://localhost:8888/status                                
     Active connections: 11 
     server accepts handled requests
      60 60 65 
     Reading: 0 Writing: 1 Waiting: 0
     ```

     現在のアクティブ接続数やサーバーのリクエスト処理状況（読み込み、書き込み、待機状態）を表示します。

   - 以下のコマンドで各EMQXノードのクライアント接続状況をそれぞれ確認します。

     ```bash
     docker exec -it emqx1 emqx ctl broker stats | grep connections.count
     docker exec -it emqx2 emqx ctl broker stats | grep connections.count
     docker exec -it emqx3 emqx ctl broker stats | grep connections.count
     ```

     各ノードの接続数とアクティブ接続数を表示し、10接続がクラスター内のノードに均等に分散されていることがわかります。

     ```bash
     connections.count             : 3
     live_connections.count        : 3
     connections.count             : 4
     live_connections.count        : 4
     connections.count             : 3
     live_connections.count        : 3
     ```

以上の手順で、NGINXのロードバランス機能とEMQXクラスター内のクライアント接続分布を検証できます。`emqx-usage-example/mqtt-lb-nginx/nginx.conf`ファイルを編集してカスタム設定の検証も可能です。

## NGINXのインストールと使用方法

このセクションでは、NGINXのインストールと使用方法を詳しく紹介します。

### 前提条件

開始前に、以下の3つのEMQXノードからなるクラスターを作成していることを確認してください。EMQXクラスターの作成方法は[Create a Cluster](./create-cluster.md)を参照してください。

| ノードアドレス           | MQTT TCPポート | MQTT WebSocketポート |
| ------------------------ | -------------- | -------------------- |
| emqx1-cluster.emqx.io    | 1883           | 8083                 |
| emqx2-cluster.emqx.io    | 1883           | 8083                 |
| emqx3-cluster.emqx.io    | 1883           | 8083                 |

本ページの例では、単一のNGINXサーバーをロードバランサーとして設定し、これら3つのEMQXノードからなるクラスターにリクエストを転送します。

### NGINXのインストール

デモではUbuntu 22.04 LTS上にソースコードからNGINXをインストールします。Dockerやバイナリパッケージを用いたインストールも可能です。

#### 必要な依存パッケージ

NGINXのコンパイル・インストール前に、以下の依存パッケージがシステムにインストールされていることを確認してください。

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

最新の安定版NGINXは[NGINX公式サイト](https://nginx.org/en/download.html)からダウンロード可能です。例：

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

上記のうち、`--with-http_ssl_module`はSSLサポート追加、`--with-stream`および`--with-stream_ssl_module`はTCPリバースプロキシサポート追加のためのオプションです。

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

### はじめに

NGINXの設定ファイルはデフォルトで`/usr/local/nginx/conf/nginx.conf`にあります。本ページの設定例をファイル末尾に追記してください。基本的なNGINX操作コマンドは以下の通りです。

設定ファイルの検証：

```bash
sudo nginx -t
```

設定ファイルが正常ならNGINXを起動：

```bash
sudo nginx
```

起動中のNGINXに設定を反映するには、事前に設定検証を推奨します。

```bash
sudo nginx -s reload
```

NGINXの停止：

```bash
sudo nginx stop
```

## MQTTのリバースプロキシおよびロードバランス用NGINX設定

このセクションでは、さまざまなロードバランス要件に対応するNGINX設定方法を説明します。

### MQTTのリバースプロキシ設定

以下の設定をNGINXの設定ファイルに追加すると、クライアントからのMQTT接続要求をリバースプロキシし、バックエンドMQTTサーバーに転送できます。

```bash
stream {
  upstream mqtt_servers {
    # down: 現在サーバーが一時的にロードバランスに参加しないことを示す
    # max_fails: 許容される失敗リクエスト数（デフォルトは1）
    # fail_timeout: max_failsに達した際の失敗リクエストのタイムアウト（デフォルト10秒）
    # backup: 全ての非バックアップサーバーがダウンまたはビジー時にリクエストをバックアップサーバーに送る

    server emqx1-cluster.emqx.io:1883 max_fails=2 fail_timeout=10s;
    server emqx2-cluster.emqx.io:1883 down;
    server emqx3-cluster.emqx.io:1883 backup;
  }

  server {
    listen 1883;
    proxy_pass mqtt_servers;

    # このオプションを有効にする場合、対応するバックエンドリスナーもproxy_protocolを有効にする必要がある
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

NGINXでMQTTのTLS接続を終端し、暗号化されたMQTTリクエストをバックエンドMQTTサーバーに転送して通信の安全性を確保できます。TCPベースの設定にSSL関連パラメータを追加するだけです。

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

    # このオプションを有効にする場合、対応するバックエンドリスナーもproxy_protocolを有効にする必要がある
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

以下の設定で、NGINXがMQTT WebSocket接続をリバースプロキシし、クライアントのリクエストをバックエンドMQTTサーバーに転送します。`server_name`でHTTPのドメイン名またはIPアドレスを指定してください。

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

      # キャッシュを無効化
      proxy_buffering off;

      proxy_connect_timeout 10s;
      # WebSocket接続のタイムアウト
      # この時間内にデータ交換がなければ自動的に切断される（デフォルト60秒）
      proxy_send_timeout 3600s;
      proxy_read_timeout 3600s;

      # リバースプロキシ元の実IPを設定
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header REMOTE-HOST $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
  }
}
```

### MQTT WebSocket SSLのリバースプロキシ設定

NGINXでMQTT WebSocketのTLS接続を終端し、暗号化されたMQTTリクエストをバックエンドMQTTサーバーに転送して通信の安全性を確保できます。`server_name`でHTTPのドメイン名またはIPアドレスを指定します。WebSocketベースの設定にSSLおよび証明書関連パラメータを追加するだけです。

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

        # リバースプロキシ元の実IPを設定
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header REMOTE-HOST $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;

        # キャッシュを無効化
        proxy_buffering off;
    }
  }
}
```

### ロードバランス戦略の設定

NGINXは接続の分散方法を制御する複数のロードバランス戦略を提供します。実際の運用では、サーバー性能やトラフィック要件などに応じて適切な戦略を選択することが重要です。以下は`upstream`ブロックで設定可能な代表的なNGINXロードバランス戦略です。

#### ラウンドロビン

デフォルトのロードバランス戦略で、リクエストをバックエンドサーバーに順番に均等分配します。バックエンドサーバーの性能がほぼ同等の場合に適しています。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

#### 重み付きラウンドロビン

ラウンドロビンに加え、各EMQXノードに異なる重みを割り当て、リクエストの分配比率を調整します。重みが高いサーバーほど多くのリクエストを受け取ります。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883 weight=3;
  server emqx2-cluster.emqx.io:1883 weight=2;
  server emqx3-cluster.emqx.io:1883 weight=1;
}
```

#### IPハッシュ

クライアントのIPアドレスを基にハッシュを計算し、特定のバックエンドサーバーにリクエストを割り当てます。同一クライアントからのリクエストは常に同じサーバーにルーティングされます。

```bash
upstream backend_servers {
  ip_hash;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

#### 最小接続数

現在の接続数が最も少ないサーバーにリクエストを分配し、各サーバーの負荷を均等化します。サーバー性能に差がある場合に適しています。

```bash
upstream backend_servers {
  least_conn;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

## NGINX Plusを用いたEMQXデプロイメントの最適化

このセクションでは、NGINX Plus固有の機能を設定してEMQXデプロイメントを最適化する方法を紹介します。本ページでコンパイル・インストールしたNGINX（オープンソース版）ではこれらの設定例は利用できません。NGINX Plusを用いたMQTT接続最適化の詳細は[こちらのドキュメント](https://www.nginx.com/blog/optimizing-mqtt-deployments-in-enterprise-environments-nginx-plus/)を参照してください。

### MQTTスティッキーセッションロードバランスの設定

「スティッキー」とは、ロードバランサーがクライアントを再接続時に同じサーバーにルーティングし、セッションの乗っ取りを防ぐ機能です。頻繁に再接続するクライアントや切断・再接続を繰り返す問題のあるクライアントに有効で、効率を改善します。

スティッキーを実現するには、サーバーが接続要求内のクライアント識別子（通常はクライアントID）を特定する必要があります。ロードバランサーがMQTTパケットを解析しクライアント識別子を取得後、静的クラスターでは識別子をハッシュ化してサーバーIDに変換するか、ロードバランサーがクライアント識別子と宛先ノードIDのマッピングテーブルを保持して柔軟にルーティングします。

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

上記例は環境に応じて調整が必要な場合があります。設定で使用されるモジュール（`ip_hash`や`least_conn`など）はNGINX標準モジュールであり、追加のモジュール依存はありません。

### クライアントID置換機能の設定

MQTT通信におけるセキュリティは重要です。デバイスはシリアル番号などの機密情報をクライアントIDとして使用することが多く、MQTTサーバーのデータベースに保存するとセキュリティリスクとなる場合があります。NGINX PlusはクライアントID置換機能を提供し、クライアントIDをNGINX Plus設定で指定した別の値に置換可能です。

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

この例ではクライアントの相互認証を有効にし、クライアントSSL証明書のシリアル番号をユニーク識別子として抽出し、元のクライアントIDを置換しています。`$ssl_client_s_dn`など他の値を使って証明書DNを抽出することも可能です。

## NGINXのパフォーマンス最適化とモニタリング有効化

このセクションでは、NGINXのパフォーマンスを設定で最適化し、ステータスモニタリング機能を有効にする方法を説明します。

### NGINX基本設定の調整

- `worker_processes`: ワーカープロセス数。サーバーのCPUコア数に近い値に設定しますが、多すぎるとリソース競合が発生するため注意が必要です。
- `worker_connections`: 1つのワーカープロセスが同時に処理可能な最大接続数。OSの最大ファイルディスクリプタ制限を超えないように設定してください。

```bash
worker_processes auto;

events {
 worker_connections 20480;
}
```

### 大量接続対応のためのNGINXマルチNICサポート（リバースプロキシ）

リバースプロキシ環境では、NGINXはクライアントとしてバックエンドEMQXノードに接続します。この場合、単一IPアドレスで約60,000の長時間接続を作成可能です。より多くの接続をサポートするには、複数のNGINXサーバーを展開するか、複数IPアドレスを設定します。

以下はNGINX組み込みの`split_clients`モジュールを使い、変数`$multi_ip`を定義してクライアントのIPアドレスとポート番号に基づきリクエストを分散する例です。利用するIPアドレスはローカルで利用可能であることを確認してください。

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

NGINXのステータスモニタリングを有効にするには、`http_stub_status_module`モジュールがインストールされている必要があります。インストール済みであれば、以下のように設定してNGINXのステータスモニタリングを有効化できます。

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

ブラウザやcurlで http://localhost:8888/status にアクセスするとステータス情報が取得できます。

```bash
$ curl http://localhost:8888/status
Active connections: 12
server accepts handled requests
 25 25 60
Reading: 0 Writing: 1 Waiting: 1
```

## 付録：主なパラメータの説明

以下は例示設定で使用される主なパラメータの説明です。これらはバックエンドMQTTサーバーへの安定した接続を確保したり、NGINX経由でMQTT通信を暗号化・保護し、IoTアプリケーションの通信プライバシーと整合性を守るためのベストプラクティスに基づいています。

| パラメータ名              | 説明                                                         |
| ------------------------- | ------------------------------------------------------------ |
| proxy_protocol            | PROXYプロトコルを有効化し、NGINXが接続開始時に追加のプロキシ情報を付加可能にします。これによりEMQXは実際のクライアントIPを取得できます。 |
| proxy_pass                | バックエンドMQTTサーバーのアドレスを定義し、クライアントからの全リクエストをここに転送します。 |
| proxy_connect_timeout     | バックエンドMQTTサーバーへの接続確立タイムアウト。指定時間内に接続できない場合、NGINXは接続試行を中止します。 |
| proxy_timeout             | バックエンドMQTTサーバーの応答タイムアウト。指定時間内に応答がない場合、NGINXは接続を切断します。 |
| proxy_buffer_size         | バックエンドMQTTサーバーから受信したデータを格納するバッファサイズ。大きなデータストリームに対応するため十分なサイズを確保します。 |
| tcp_nodelay               | TCP_NODELAYオプションを有効化し、Nagleアルゴリズムを無効化します。これによりパケット送信のレイテンシが低減され、リアルタイムMQTT通信に有利です。 |
| ssl_session_cache         | 共有SSLセッションキャッシュを設定します。これによりクライアント再接続時のハンドシェイク処理が高速化されます。`shared:SSL:10m`はキャッシュ名とサイズ（10MB）を指定しています。 |
| ssl_session_timeout       | SSLセッションのタイムアウトを10分に設定。再利用されなかったセッションはこの時間後にクリアされます。 |
| ssl_certificate           | SSL証明書ファイルのパス。サーバーの身元証明に使用されます。 |
| ssl_certificate_key       | SSL証明書に対応する秘密鍵ファイルのパス。 |
| ssl_protocols             | 許可するSSL/TLSプロトコルバージョンを指定します。             |
| ssl_ciphers               | 許可する暗号化アルゴリズム（暗号スイート）を設定します。`HIGH:!aNULL:!MD5`は強力な暗号スイートを使用し、空の暗号スイートやMD5などの弱いアルゴリズムを除外します。 |
| ssl_client_certificate    | クライアント証明書の検証に用いる認証局（CA）証明書ファイルのパス。 |
| ssl_verify_client         | クライアント証明書の検証を有効化します。`on`に設定するとNGINXは有効なSSL証明書の提示をクライアントに要求します。 |
| ssl_verify_depth          | クライアント証明書検証の最大深度を設定します。ここでは`1`に設定し、クライアント証明書とCA証明書の1階層のみ検証します。 |

## 参考情報

EMQXはNGINXに関する豊富なリソースを提供しています。以下のリンクもご参照ください。

**ブログ:**

- [NGINX PlusでのEMQXスティッキーセッション活用：「クライアントID」を魔法の鍵に](https://www.emqx.com/en/blog/harnessing-sticky-sessions-for-mqtt-load-balancing-with-nginx-plus)
- [NGINX PlusのクライアントID置換とEMQX EnterpriseでMQTTアプリケーションをセキュアに](https://www.emqx.com/en/blog/securing-your-mqtt-based-applications-with-nginx-plus-client-id-substitution-and-emqx-enterprise)
- [EMQXとNGINX Plusによるクライアント証明書認証でMQTTセキュリティを強化](https://www.emqx.com/en/blog/elevating-mqtt-security-with-client-certificate-authentication)
