# NGINXによるEMQXクラスターのロードバランス

NGINXは高性能で多機能なサーバーソフトウェアであり、ウェブサーバーやリバースプロキシサーバーとして動作できます。さらに、NGINXはロードバランサーとしても機能し、クライアントからのリクエストを複数のバックエンドサーバーに分散させることで、負荷分散とパフォーマンスの最適化を実現します。NGINXは多数の同時リクエストを処理する必要があるIoTアプリケーションに特に適しています。IoTでは通常、多数のデバイスが存在し、高負荷のリクエストを処理できるサーバーが求められます。EMQXは複数のMQTTサーバーからなる分散クラスターアーキテクチャをネイティブにサポートしています。したがって、NGINXをロードバランサーとして導入し、EMQXクラスターを構築することで、高可用性とスケーラビリティを確保できます。

本ページでは、NGINXのインストール方法と、EMQXクラスターのMQTTサーバーをセットアップするためのリバースプロキシおよびロードバランスの設定方法を紹介します。また、NGINX Plusを使用してEMQXのデプロイを最適化する方法も紹介します。

## 特長と利点

NGINXを使ってEMQXクラスターのロードバランスを行うことには、以下のような特長と利点があります。

- リバースプロキシサーバーとして、NGINXはMQTTサーバー側に位置し、MQTTクライアントを代表してEMQXクラスターへのMQTT接続要求を開始し、EMQXクラスターの応答をMQTTクライアントに返します。この構成により複数のクラスターを隠蔽し、MQTTクライアントに対して単一のアクセスポイントを公開します。MQTTクライアントはNGINXとだけ通信すればよく、背後のクラスターの数や配置を意識する必要がありません。この方式はシステムの保守性とスケーラビリティを向上させます。
- NGINXはMQTTクライアントとEMQXクラスター間のSSL暗号化されたMQTT接続を終端でき、EMQXクラスターの暗号化・復号負荷を軽減します。これにより、パフォーマンスの向上、証明書管理の簡素化、セキュリティ強化などの利点があります。
- NGINXは柔軟なロードバランス戦略を提供し、クラスター内のどのEMQXノードにリクエストを送るかを決定できます。これによりトラフィックやリクエストの分散が可能となり、パフォーマンスと信頼性が向上します。例えば、スティッキー（sticky）ロードバランスは同一のバックエンドサーバーへリクエストをルーティングし、パフォーマンスとセッションの持続性を高めます。

![EMQX LB NGINX](./assets/emqx-lb-nginx.png)

## クイックスタート

このセクションでは、実際の例を用いたDocker Compose構成を提供し、NGINXの機能を簡単に検証・テストできます。以下の手順に従ってください。

1. 例のリポジトリをクローンし、`mqtt-lb-nginx`ディレクトリに移動します。

```bash
git clone https://github.com/emqx/emqx-usage-example
cd emqx-usage-example/mqtt-lb-nginx
```

2. Docker Composeで例を起動します。

```bash
docker compose up -d
```

3. [MQTTX](https://mqttx.app) CLIを使って10個のTCP接続を確立し、MQTTクライアント接続をシミュレートします。

```bash
mqttx bench conn -c 10
```

4. NGINXの接続モニタリングとEMQXクライアント接続の分布状況を確認できます。

   - 以下のコマンドでNGINXの接続モニタリングを表示します。

     ```bash
     $ curl http://localhost:8888/status
     Active connections: 11
     server accepts handled requests
      60 60 65
     Reading: 0 Writing: 1 Waiting: 0
     ```

     これは現在のアクティブ接続数とサーバーのリクエスト処理統計（読み込み、書き込み、待機状態）を表示します。

   - 以下のコマンドで各EMQXノードのクライアント接続状況をそれぞれ確認します。

     ```bash
     docker exec -it emqx1 emqx ctl broker stats | grep connections.count
     docker exec -it emqx2 emqx ctl broker stats | grep connections.count
     docker exec -it emqx3 emqx ctl broker stats | grep connections.count
     ```

     これにより各ノードの接続数とアクティブ接続数が表示され、10接続がクラスター内のノードに均等に分散されていることがわかります。

     ```bash
     connections.count             : 3
     live_connections.count        : 3
     connections.count             : 4
     live_connections.count        : 4
     connections.count             : 3
     live_connections.count        : 3
     ```

これらの手順で、例におけるNGINXのロードバランス機能とEMQXクラスターのクライアント接続分布を検証できます。また、`emqx-usage-example/mqtt-lb-nginx/nginx.conf`ファイルを編集してカスタム設定の検証も可能です。

## NGINXのインストールと使用方法

このセクションでは、NGINXのインストールおよび使用方法を詳しく紹介します。

### 前提条件

開始する前に、以下の3つのEMQXノードからなるクラスターを作成していることを確認してください。EMQXクラスターの作成方法については、[クラスターの作成](./create-cluster.md)を参照してください。

| ノードアドレス           | MQTT TCPポート | MQTT WebSocketポート |
| ------------------------ | -------------- | -------------------- |
| emqx1-cluster.emqx.io    | 1883           | 8083                 |
| emqx2-cluster.emqx.io    | 1883           | 8083                 |
| emqx3-cluster.emqx.io    | 1883           | 8083                 |

本ページの例では、単一のNGINXサーバーをロードバランサーとして設定し、これら3つのEMQXノードからなるクラスターにリクエストを転送します。

### NGINXのインストール

デモではUbuntu 22.04 LTSにソースコードからNGINXをインストールします。Dockerやバイナリパッケージを使ったインストールも可能です。

#### 必要な依存パッケージ

NGINXのコンパイル・インストール前に、以下の依存パッケージがシステムにインストールされていることを確認してください。

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

最新の安定版NGINXは[NGINX公式サイト](https://nginx.org/en/download.html)からダウンロードできます。例：

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

上記のうち、`--with-http_ssl_module`はSSLサポート追加、`--with-stream`および`--with-stream_ssl_module`はTCPリバースプロキシサポート追加のためのパラメータです。

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

### 使い始め

NGINXの設定ファイルはデフォルトで`/usr/local/nginx/conf/nginx.conf`にあります。本ページの設定例をファイルの末尾に追加してください。基本的なNGINX操作コマンドは以下の通りです。

設定ファイルの検証：

```bash
sudo nginx -t
```

設定ファイルが正常ならNGINXを起動：

```bash
sudo nginx
```

起動中のNGINXに新設定を反映する場合は、エラーを確認してからリロードします。

```bash
sudo nginx -s reload
```

NGINXを停止する場合：

```bash
sudo nginx stop
```

## NGINXのリバースプロキシおよびロードバランス設定

このセクションでは、さまざまなロードバランス要件に対応するNGINXの設定方法を説明します。

### MQTTのリバースプロキシ設定

以下の設定をNGINXの設定ファイルに追加することで、クライアントからのMQTT接続要求をリバースプロキシし、バックエンドのMQTTサーバーに転送できます。

```bash
stream {
  upstream mqtt_servers {
    # down: 現在のサーバーが一時的にロードバランスに参加しないことを示す
    # max_fails: 許容される失敗リクエスト数（デフォルトは1）
    # fail_timeout: max_failsに達した場合の失敗リクエストのタイムアウト（デフォルト10秒）
    # backup: 全ての非バックアップサーバーがダウンまたはビジー時にリクエストを送るバックアップサーバー

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

以下の設定でNGINXがMQTT WebSocket接続をリバースプロキシし、クライアントリクエストをバックエンドMQTTサーバーに転送します。`server_name`でHTTPのドメイン名またはIPアドレスを指定する必要があります。

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
      # この時間内にデータ交換がなければWebSocket接続は自動切断される（デフォルト60秒）
      proxy_send_timeout 3600s;
      proxy_read_timeout 3600s;

      # リバースプロキシの実IP
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header REMOTE-HOST $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
  }
}
```

::: tip
`$proxy_add_x_forwarded_for`は、受信リクエストに既に存在する`X-Forwarded-For`ヘッダーに`$remote_addr`を追加しますが、EMQXはこのヘッダーの最初（左端）の値を読み取ります。追加形式ではクライアント由来の値が最初に来るため偽装される可能性があります。EMQXがこのヘッダーをクライアントの送信元アドレスとして使用する場合（[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners)参照）、NGINXが観測したアドレスでヘッダーを上書きしてください：`proxy_set_header X-Forwarded-For $remote_addr;`。
:::

### MQTT WebSocket SSLのリバースプロキシ設定

NGINXでMQTT WebSocketのTLS接続を終端し、クライアントからの暗号化されたMQTTリクエストをバックエンドMQTTサーバーに転送して通信の安全性を確保できます。`server_name`でHTTPのドメイン名またはIPアドレスを指定します。WebSocketベースの設定にSSLおよび証明書関連パラメータを追加するだけです。

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

        # リバースプロキシの実IP
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

NGINXは接続の分散方法を制御する複数のロードバランス戦略を提供します。実際の運用では、サーバー性能やトラフィック要件などに応じて適切な戦略を選択することが重要です。以下は`upstream`ブロックで設定可能な代表的なNGINXのロードバランス戦略です。

#### ラウンドロビン

デフォルトのロードバランス戦略です。リクエストを順番に各バックエンドサーバーに均等に分配します。バックエンドサーバーの性能がほぼ同等の場合に適しています。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

#### 重み付きラウンドロビン

ラウンドロビンに加え、各EMQXノードに異なる重みを割り当ててリクエストの分配比率を調整します。重みの高いサーバーほど多くのリクエストを受け取ります。

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

現在の接続数が最も少ないサーバーにリクエストを分配し、各サーバーの負荷をできるだけ均等にします。サーバー性能に差が大きい場合に適しています。

```bash
upstream backend_servers {
  least_conn;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

## NGINX Plusを使ったEMQXデプロイの最適化

このセクションでは、NGINX Plus固有の機能を設定してEMQXのデプロイを最適化する方法を紹介します。本ページでコンパイル・インストールしたNGINXでは利用できないため、NGINX Plus版の設定例のみ記載しています。NGINX Plusを使ったMQTT接続の最適化については、こちらの[ドキュメント](https://www.nginx.com/blog/optimizing-mqtt-deployments-in-enterprise-environments-nginx-plus/)を参照してください。

### MQTTスティッキーセッションロードバランスの設定

スティッキーとは、クライアントが再接続した際に同じサーバーにルーティングし、セッションの乗っ取りを防ぐ機能です。頻繁に再接続するクライアントや切断・再接続を繰り返す問題のあるクライアントに有効で、効率化に役立ちます。

スティッキーを実現するには、サーバーが接続要求内のクライアント識別子（通常はクライアントID）を特定する必要があり、ロードバランサーがMQTTパケットを解析します。クライアント識別子を取得後、静的クラスターではハッシュしてサーバーIDに変換したり、ロードバランサーがクライアント識別子と宛先ノードIDのマッピングテーブルを保持して柔軟にルーティングできます。

以下は設定例です。

```bash
mqtt_preread on;

upstream backend_servers {
    hash $mqtt_preread_clientid consistent;
    server emqx1-cluster.emqx.io:1883;
    server emqx2-cluster.emqx.io:1883;
    server emqx3-cluster.emqx.io:1883;
}
```

上記例は環境に応じて調整が必要な場合があります。設定で使われているモジュール（`ip_hash`や`least_conn`など）はNGINX組み込みモジュールであり、追加のモジュール依存はありません。

### クライアントID置換機能の設定

MQTT通信におけるセキュリティは重要です。デバイスはシリアル番号などの機微なデータをクライアントIDとして使うことが多く、MQTTサーバーのデータベースに保存することはセキュリティリスクとなります。NGINX PlusはクライアントID置換機能を提供し、クライアントIDをNGINX Plus設定で指定した別の値に置き換えられます。

設定例は以下の通りです。

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

この例ではクライアントの相互認証を有効にし、クライアントのSSL証明書から証明書のシリアル番号を抽出して一意の識別子とし、元のクライアントIDを置換しています。`$ssl_client_s_dn`など他の値を使って証明書のDNを抽出することも可能です。

## NGINXのパフォーマンス最適化とモニタリング有効化

このセクションでは、NGINXのパフォーマンスを設定で最適化し、ステータスモニタリング機能を有効にする方法を説明します。

### NGINX基本設定の調整

- `worker_processes`：ワーカープロセス数。サーバーのCPUコア数に近い値に設定しますが、多すぎるとリソース競合が発生するため注意が必要です。
- `worker_connections`：単一ワーカープロセスが処理可能な同時接続数の最大値。OSの最大ファイルディスクリプタ制限を超えないように設定してください。

```bash
worker_processes auto;

events {
 worker_connections 20480;
}
```

### リバースプロキシにおける大量接続対応のためのNGINXマルチNICサポート

リバースプロキシではNGINXがクライアントとしてバックエンドEMQXノードに接続します。この場合、単一IPアドレスで最大約6万の長時間接続を作成可能です。より多くの接続をサポートするには、複数のNGINXサーバーを展開するか、複数のIPアドレスを設定します。

以下はNGINX組み込みの`split_clients`モジュールを使い、クライアントのIPアドレスとポート番号に基づいて変数`$multi_ip`を定義し、リクエストを分散する例です。使用するIPアドレスはローカルで利用可能なものを指定してください。

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

NGINXのステータスモニタリングを有効にするには、`http_stub_status_module`モジュールがインストールされている必要があります。インストール済みの場合、以下のようにNGINXのステータスモニタリングを有効化できます。

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

http://localhost:8888/status にアクセスするとステータス情報が取得できます。

```bash
$ curl http://localhost:8888/status
Active connections: 12
server accepts handled requests
 25 25 60
Reading: 0 Writing: 1 Waiting: 1
```

## 付録：主なパラメータの説明

以下は例示した設定で使用される主なパラメータの説明です。これらのパラメータは、バックエンドMQTTサーバーとの安定した接続を確保したり、NGINX経由でMQTT通信を暗号化・保護し、IoTアプリケーションの通信のプライバシーと完全性を守るためのベストプラクティスに従っています。

| パラメータ名             | 説明                                                         |
| ------------------------ | ------------------------------------------------------------ |
| proxy_protocol           | PROXYプロトコルを有効化し、NGINXがリクエスト転送時に接続の先頭に追加のプロキシ情報を付加します。これによりEMQXが実際のクライアントIPを取得可能になります。 |
| proxy_pass               | バックエンドMQTTサーバーのアドレスを定義し、クライアントからの全リクエストをこのアドレスに転送します。 |
| proxy_connect_timeout    | バックエンドMQTTサーバーへの接続確立のタイムアウト。指定時間内に接続できなければNGINXは接続試行を中止します。 |
| proxy_timeout            | バックエンドMQTTサーバーのタイムアウト。指定時間内に応答がなければNGINXは接続を切断します。 |
| proxy_buffer_size        | バックエンドMQTTサーバーから受信したデータを格納するバッファのサイズ。大きなデータストリームを処理するために十分なサイズを確保します。 |
| tcp_nodelay              | TCP_NODELAYオプションを有効にし、Nagleアルゴリズムを無効化します。これによりパケット送信のレイテンシが低減され、リアルタイムMQTT通信に有利です。 |
| ssl_session_cache        | 共有SSLセッションキャッシュを設定します。SSLセッションの状態を保存し、クライアントの再接続時にハンドシェイクを高速化します。`shared:SSL:10m`はキャッシュ名とサイズ（10MB）を指定しています。 |
| ssl_session_timeout      | SSLセッションのタイムアウトを10分に設定します。この時間内に再利用されないセッションはクリアされます。 |
| ssl_certificate          | SSL証明書ファイルのパスを指定します。サーバーの身元証明に使用されます。 |
| ssl_certificate_key      | SSL証明書に対応する秘密鍵ファイルのパスを指定します。 |
| ssl_protocols            | 許可するSSL/TLSプロトコルのバージョンを指定します。           |
| ssl_ciphers              | 許可する暗号化アルゴリズム（暗号スイート）を設定します。`HIGH:!aNULL:!MD5`は強力な暗号スイートを使用し、空の暗号スイートやMD5ハッシュアルゴリズムを除外します。 |
| ssl_client_certificate   | クライアント証明書の真正性を検証するための認証局（CA）証明書ファイルのパスを指定します。 |
| ssl_verify_client        | クライアント証明書の検証を有効化します。`on`に設定すると、NGINXはクライアントに有効なSSL証明書の提示を要求します。 |
| ssl_verify_depth         | クライアント証明書検証の最大深度を設定します。ここでは`1`に設定し、クライアント証明書とCA証明書の1段階のみ検証します。 |

## 参考情報

EMQXはNGINXに関する豊富なリソースを提供しています。以下のリンクから詳細をご覧ください。

**ブログ：**

- [NGINX PlusによるEMQXのスティッキーセッション活用：「Client ID」を魔法の鍵に](https://www.emqx.com/en/blog/harnessing-sticky-sessions-for-mqtt-load-balancing-with-nginx-plus)
- [NGINX PlusのクライアントID置換とEMQX EnterpriseでMQTTアプリケーションをセキュアに](https://www.emqx.com/en/blog/securing-your-mqtt-based-applications-with-nginx-plus-client-id-substitution-and-emqx-enterprise)
- [EMQXとNGINX Plusでクライアント証明書認証によるMQTTセキュリティ強化](https://www.emqx.com/en/blog/elevating-mqtt-security-with-client-certificate-authentication)
