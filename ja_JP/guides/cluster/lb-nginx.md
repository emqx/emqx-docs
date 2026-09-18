# NGINXによるEMQXクラスターのロードバランス

NGINXは、高性能で多機能なサーバーソフトウェアであり、ウェブサーバーやリバースプロキシサーバーとして動作します。さらに、NGINXはロードバランサーとしても機能し、クライアントからのリクエストを複数のバックエンドサーバーに分散させることで、負荷分散とパフォーマンスの最適化を実現します。NGINXは、多数の同時リクエストを処理する必要があるIoTアプリケーションに特に適しています。IoTでは通常、多数のデバイスが存在し、高負荷のリクエストを処理できるサーバーが求められます。EMQXは複数のMQTTサーバーからなる分散クラスターアーキテクチャをネイティブにサポートしています。そのため、NGINXをロードバランサーとして導入し、EMQXクラスターと組み合わせることで、高可用性とスケーラビリティを実現できます。

本ページでは、NGINXのインストール方法と、EMQXクラスターのMQTTサーバーを構築するためのリバースプロキシおよびロードバランス設定方法を紹介します。また、NGINX Plusを使用してEMQXのデプロイを最適化する方法も解説します。

## 特長と利点

NGINXを用いてEMQXクラスターのロードバランスを行うことには、以下のような特長と利点があります。

- リバースプロキシサーバーとして、NGINXはMQTTサーバー側に位置し、MQTTクライアントを代表してEMQXクラスターへのMQTT接続リクエストを開始し、EMQXクラスターの応答をMQTTクライアントに返します。この構成により、複数のクラスターを隠蔽し、MQTTクライアントには単一のアクセスポイントを公開します。MQTTクライアントはNGINXとだけ通信すればよく、背後のクラスターの数や構成を意識する必要がありません。この方法はシステムの保守性とスケーラビリティを向上させます。
- NGINXはMQTTクライアントとEMQXクラスター間のSSL暗号化接続を終端でき、EMQXクラスターの暗号化・復号負荷を軽減します。これにより、パフォーマンス向上、証明書管理の簡素化、セキュリティ強化などの利点があります。
- NGINXは柔軟なロードバランス戦略を提供し、クラスター内のどのEMQXノードにリクエストを送るかを制御できます。これによりトラフィックやリクエストの分散が可能となり、パフォーマンスと信頼性が向上します。例えば、スティッキー（sticky）ロードバランスは同じバックエンドサーバーにリクエストをルーティングし、パフォーマンスとセッションの持続性を向上させます。

![EMQX LB NGINX](./assets/emqx-lb-nginx.png)

## クイックスタート

このセクションでは、実際の例を用いたDocker Compose構成を提供し、NGINXの機能を簡単に検証・テストできるようにします。以下の手順に従ってください。

1. サンプルリポジトリをクローンし、`mqtt-lb-nginx`ディレクトリに移動します。

```bash
git clone https://github.com/emqx/emqx-usage-example
cd emqx-usage-example/mqtt-lb-nginx
```

2. Docker Composeでサンプルを起動します。

```bash
docker compose up -d
```

3. [MQTTX](https://mqttx.app) CLIを使って10個のTCP接続を確立し、MQTTクライアントの接続をシミュレートします。

```bash
mqttx bench conn -c 10
```

4. NGINXの接続状況やEMQXクライアント接続の分布を確認できます。

   - 以下のコマンドでNGINXの接続モニタリングを表示します。

     ```bash
     $ curl http://localhost:8888/status
     Active connections: 11
     server accepts handled requests
      60 60 65
     Reading: 0 Writing: 1 Waiting: 0
     ```

     これは現在のアクティブ接続数とサーバーのリクエスト処理統計（読み取り、書き込み、待機状態）を示します。

   - 各EMQXノードのクライアント接続状況を以下のコマンドで確認します。

     ```bash
     docker exec -it emqx1 emqx ctl broker stats | grep connections.count
     docker exec -it emqx2 emqx ctl broker stats | grep connections.count
     docker exec -it emqx3 emqx ctl broker stats | grep connections.count
     ```

     これにより各ノードの接続数とアクティブ接続数が表示され、10接続がクラスターのノードに均等に分散されていることがわかります。

     ```bash
     connections.count             : 3
     live_connections.count        : 3
     connections.count             : 4
     live_connections.count        : 4
     connections.count             : 3
     live_connections.count        : 3
     ```

これらの手順により、NGINXのロードバランス機能とEMQXクラスターのクライアント接続分布を検証できます。`emqx-usage-example/mqtt-lb-nginx/nginx.conf`ファイルを編集してカスタム設定の検証も可能です。

## NGINXのインストールと使用方法

このセクションでは、NGINXのインストールおよび使用方法を詳述します。

### 前提条件

開始前に、以下の3つのEMQXノードからなるクラスターを作成していることを確認してください。EMQXクラスターの作成方法は、[Create a Cluster](./create-cluster.md)を参照してください。

| ノードアドレス           | MQTT TCPポート | MQTT WebSocketポート |
| ------------------------ | -------------- | -------------------- |
| emqx1-cluster.emqx.io    | 1883           | 8083                 |
| emqx2-cluster.emqx.io    | 1883           | 8083                 |
| emqx3-cluster.emqx.io    | 1883           | 8083                 |

本ページの例では、単一のNGINXサーバーをロードバランサーとして設定し、これら3つのEMQXノードからなるクラスターにリクエストを転送します。

### NGINXのインストール

デモではUbuntu 22.04 LTSにソースコードからNGINXをインストールします。Dockerやバイナリパッケージを使ったインストールも可能です。

#### 必要な依存関係

NGINXのコンパイル・インストール前に、以下の依存関係がインストールされていることを確認してください。

- GNU CおよびC++コンパイラ
- PCRE（Perl Compatible Regular Expressions）ライブラリ
- zlib圧縮ライブラリ
- OpenSSLライブラリ

Ubuntuの場合、以下のコマンドでインストールできます。

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

ダウンロード後、ソースコードを展開しディレクトリに移動します。

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

上記の`--with-http_ssl_module`はSSLサポート追加、`--with-stream`と`--with-stream_ssl_module`はTCPリバースプロキシサポート追加のためのオプションです。

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

システムのPATHにあるディレクトリにNGINX実行ファイルのシンボリックリンクを作成します。

```bash
sudo ln -s /usr/local/nginx/sbin/nginx /usr/local/bin/nginx
```

### 使い始め

NGINXの設定ファイルはデフォルトで`/usr/local/nginx/conf/nginx.conf`にあります。本ページの設定例をファイルの末尾に追加してください。基本的なNGINX操作コマンドは以下の通りです。

設定ファイルの検証：

```bash
sudo nginx -t
```

設定ファイルが正常であれば、NGINXを起動します。

```bash
sudo nginx
```

稼働中のNGINXに新設定を反映するには、事前に設定検証を行い、以下のコマンドを実行します。

```bash
sudo nginx -s reload
```

NGINXを停止するには：

```bash
sudo nginx stop
```

## NGINXのリバースプロキシおよびロードバランス設定

このセクションでは、さまざまなロードバランス要件に対応するNGINXの設定方法を解説します。

### MQTTのリバースプロキシ設定

以下の設定をNGINXの設定ファイルに記述することで、クライアントからのMQTT接続リクエストをリバースプロキシし、バックエンドのMQTTサーバーに転送できます。

```bash
stream {
  upstream mqtt_servers {
    # down: 現在のサーバーが一時的にロードバランス対象外であることを示す
    # max_fails: 許容される失敗リクエスト数（デフォルトは1）
    # fail_timeout: max_failsに達した際の失敗リクエストのタイムアウト（デフォルト10秒）
    # backup: すべての非バックアップサーバーがダウンまたはビジー時にリクエストをバックアップサーバーに送る

    server emqx1-cluster.emqx.io:1883 max_fails=2 fail_timeout=10s;
    server emqx2-cluster.emqx.io:1883 down;
    server emqx3-cluster.emqx.io:1883 backup;
  }

  server {
    listen 1883;
    proxy_pass mqtt_servers;

    # このオプションを有効にする場合、対応するバックエンドのリスナーもproxy_protocolを有効にする必要がある
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

NGINXを使ってMQTTのTLS接続を終端し、クライアントからの暗号化されたMQTTリクエストをバックエンドMQTTサーバーに転送し、通信の安全性を確保できます。TCPベースの設定にSSL関連パラメータを追加するだけで実現可能です。

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

    # 相互認証を有効にする場合はCA証明書とクライアント証明書の検証を追加
    # ssl_client_certificate /usr/local/nginx/certs/ca.pem;
    # ssl_verify_client on;
    # ssl_verify_depth 1;

    proxy_pass mqtt_servers;

    # このオプションを有効にする場合、対応するバックエンドのリスナーもproxy_protocolを有効にする必要がある
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

以下の設定で、NGINXがMQTT WebSocket接続をリバースプロキシし、クライアントリクエストをバックエンドMQTTサーバーに転送します。`server_name`でHTTPのドメイン名またはIPアドレスを指定する必要があります。

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
      # この時間内にデータ交換がない場合、自動的に切断される（デフォルト60秒）
      proxy_send_timeout 3600s;
      proxy_read_timeout 3600s;

      # リバースプロキシの実IP設定
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header REMOTE-HOST $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
  }
}
```

### MQTT WebSocket SSLのリバースプロキシ設定

NGINXを使ってMQTT WebSocketのTLS接続を終端し、クライアントからの暗号化されたMQTTリクエストをバックエンドMQTTサーバーに転送し、通信の安全性を確保できます。`server_name`でHTTPのドメイン名またはIPアドレスを指定し、WebSocketベースの設定にSSLおよび証明書関連パラメータを追加するだけで実現可能です。

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

    # 相互認証を有効にする場合はCA証明書とクライアント証明書の検証を追加
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
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;

        # キャッシュを無効化
        proxy_buffering off;
    }
  }
}
```

### ロードバランス戦略の設定

NGINXは接続の分散方法を制御する複数のロードバランス戦略を提供しています。実際の運用では、サーバー性能やトラフィック要件などに応じて適切な戦略を選択することが重要です。以下は`upstream`ブロックで設定可能な代表的なNGINXロードバランス戦略です。

#### ラウンドロビン

デフォルトのロードバランス戦略です。リクエストをバックエンドサーバーに順番に均等に分配します。バックエンドサーバーの性能がほぼ同等の場合に適しています。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

#### 重み付きラウンドロビン

ラウンドロビンをベースに、各EMQXノードに異なる重みを割り当てます。重みが高いサーバーほど多くのリクエストを受け取ります。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883 weight=3;
  server emqx2-cluster.emqx.io:1883 weight=2;
  server emqx3-cluster.emqx.io:1883 weight=1;
}
```

#### IPハッシュ

クライアントのIPアドレスに基づいてハッシュ値を計算し、特定のバックエンドサーバーにリクエストを割り当てます。同じクライアントからのリクエストは常に同じサーバーにルーティングされます。

```bash
upstream backend_servers {
  ip_hash;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

#### 最小接続数

現在の接続数が最も少ないサーバーにリクエストを割り当て、各サーバーの負荷をできるだけ均等にします。サーバー性能に大きな差がある場合に適しています。

```bash
upstream backend_servers {
  least_conn;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

## NGINX PlusによるEMQXデプロイの最適化

このセクションでは、NGINX Plus固有の機能を使ったEMQXデプロイの最適化方法を紹介します。本ページでコンパイル・インストールしたNGINXでは利用できない機能も含まれます。NGINX Plusを使ったMQTT接続の最適化については、[こちらのドキュメント](https://www.nginx.com/blog/optimizing-mqtt-deployments-in-enterprise-environments-nginx-plus/)を参照してください。

### MQTTスティッキーセッションロードバランスの設定

「スティッキー」とは、ロードバランサーがクライアントを再接続時に同じサーバーにルーティングし、セッションの乗っ取りを防ぐ機能を指します。頻繁に再接続するクライアントや、切断と再接続を繰り返す問題のあるクライアントに対して有効で、効率化に寄与します。

スティッキーを実現するには、サーバーが接続リクエスト内のクライアント識別子（通常はクライアントID）を特定する必要があります。ロードバランサーがMQTTパケットを解析し、クライアント識別子を取得します。静的クラスターの場合は識別子をハッシュ化してサーバーIDに変換したり、ロードバランサーがクライアント識別子と宛先ノードIDのマッピングテーブルを保持して柔軟にルーティングすることも可能です。

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

上記例は環境に応じて調整が必要な場合があります。設定で使用されるモジュール（`ip_hash`や`least_conn`など）はNGINX標準モジュールであり、追加のモジュール依存は不要です。

### クライアントID置換機能の設定

MQTT通信におけるセキュリティは重要です。デバイスはシリアル番号などの機微な情報をクライアントIDとして使用することが多く、MQTTサーバーのデータベースに保存するとセキュリティリスクとなる可能性があります。NGINX PlusはクライアントID置換機能を提供し、NGINX Plusの設定で指定した別の値にクライアントIDを置換できます。

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

この例ではクライアントの相互認証を有効にし、クライアントSSL証明書のシリアル番号をユニーク識別子として取得し、元のクライアントIDを置換しています。`$ssl_client_s_dn`など他の値を使って証明書のDNを抽出することも可能です。

## NGINXパフォーマンス最適化とモニタリング有効化

このセクションでは、NGINXのパフォーマンスを設定で最適化し、ステータスモニタリング機能を有効にする方法を説明します。

### NGINX基本設定の調整

- `worker_processes`：ワーカープロセス数。サーバーのCPUコア数に近い値に設定します。ただし、多すぎるとリソース競合が発生するため注意が必要です。
- `worker_connections`：単一ワーカープロセスが処理可能な同時接続数の最大値。OSのファイルディスクリプタ上限を超えないように設定します。

```bash
worker_processes auto;

events {
 worker_connections 20480;
}
```

### リバースプロキシにおけるNGINXのマルチNIC対応による大量接続処理

リバースプロキシでは、NGINXはクライアントとしてバックエンドEMQXノードに接続します。この場合、単一IPアドレスで最大約6万の長時間接続を作成可能です。より多くの接続をサポートするには、複数のNGINXサーバーを展開するか、複数のIPアドレスを設定します。

以下はNGINX組み込みの`split_clients`モジュールを使い、変数`$multi_ip`を定義してクライアントのIPアドレスとポート番号に基づきリクエストを分散させる例です。使用するIPアドレスはローカルで利用可能なものを指定してください。

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

NGINXのステータスモニタリングを有効にするには、`http_stub_status_module`モジュールがインストールされている必要があります。インストール済みであれば、以下のように設定してNGINXのステータスモニタリングを有効にできます。

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

http://localhost:8888/status にアクセスするとステータス情報が確認できます。

```bash
$ curl http://localhost:8888/status
Active connections: 12
server accepts handled requests
 25 25 60
Reading: 0 Writing: 1 Waiting: 1
```

## 付録：主なパラメータの説明

以下は例示した設定で使用されている主なパラメータの説明です。これらはバックエンドMQTTサーバーへの安定した接続を確保し、NGINX経由でMQTT通信を暗号化・保護し、IoTアプリケーションの通信プライバシーと整合性を守るためのベストプラクティスに沿っています。

| パラメータ名             | 説明                                                         |
| ------------------------ | ------------------------------------------------------------ |
| proxy_protocol           | PROXYプロトコルを有効化し、NGINXが接続開始時に追加のプロキシ情報を付加して転送します。これによりEMQXは実際のクライアントIPを取得可能です。 |
| proxy_pass               | バックエンドMQTTサーバーのアドレスを定義し、クライアントからのリクエストをすべてここに転送します。 |
| proxy_connect_timeout    | バックエンドMQTTサーバーへの接続確立のタイムアウト時間。指定時間内に接続できなければNGINXは接続試行を中断します。 |
| proxy_timeout            | バックエンドMQTTサーバーの応答待ちタイムアウト。指定時間内に応答がなければNGINXは接続を切断します。 |
| proxy_buffer_size        | バックエンドMQTTサーバーから受信したデータを格納するバッファサイズ。大容量のデータストリームを処理するために十分なサイズを確保します。 |
| tcp_nodelay              | TCP_NODELAYオプションを有効にし、Nagleアルゴリズムを無効化します。これによりパケット送信のレイテンシが低減され、リアルタイムMQTT通信に有利です。 |
| ssl_session_cache        | 共有SSLセッションキャッシュを設定します。SSLセッションの状態を保存し、クライアント再接続時のハンドシェイク高速化に寄与します。`shared:SSL:10m`はキャッシュ名とサイズ（10MB）を指定しています。 |
| ssl_session_timeout      | SSLセッションのタイムアウト時間を10分に設定します。この時間内に再利用されなかったセッションはクリアされます。 |
| ssl_certificate          | SSL証明書ファイルのパスを指定します。サーバーの身元証明に使用されます。 |
| ssl_certificate_key      | SSL証明書に対応する秘密鍵ファイルのパスを指定します。 |
| ssl_protocols            | 許可するSSL/TLSプロトコルのバージョンを指定します。           |
| ssl_ciphers              | 許可する暗号化アルゴリズム（暗号スイート）を設定します。`HIGH:!aNULL:!MD5`は強力な暗号スイートを使用し、空の暗号スイートやMD5ハッシュアルゴリズムを除外しています。 |
| ssl_client_certificate   | クライアント証明書の正当性を検証するための認証局（CA）証明書ファイルのパスを指定します。 |
| ssl_verify_client        | クライアント証明書の検証を有効にします。`on`に設定するとNGINXはクライアントに有効なSSL証明書の提示を要求します。 |
| ssl_verify_depth         | クライアント証明書検証の最大深度を設定します。ここでは`1`に設定し、クライアント証明書とCA証明書の1段階の検証を行います。 |

## 参考情報

EMQXはNGINXに関する豊富なリソースを提供しています。以下のリンクもご参照ください。

**ブログ:**

- [NGINX PlusでのEMQXスティッキーセッション活用：「Client ID」を魔法の鍵に](https://www.emqx.com/en/blog/harnessing-sticky-sessions-for-mqtt-load-balancing-with-nginx-plus)
- [NGINX PlusのクライアントID置換とEMQX EnterpriseによるMQTTアプリケーションのセキュリティ強化](https://www.emqx.com/en/blog/securing-your-mqtt-based-applications-with-nginx-plus-client-id-substitution-and-emqx-enterprise)
- [EMQXとNGINX Plusでのクライアント証明書認証によるMQTTセキュリティの向上](https://www.emqx.com/en/blog/elevating-mqtt-security-with-client-certificate-authentication)
