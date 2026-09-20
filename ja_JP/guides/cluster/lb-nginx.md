# NGINXによるEMQXクラスターのロードバランス

NGINXは高性能で多機能なサーバソフトウェアであり、Webサーバやリバースプロキシサーバとして機能します。さらに、NGINXはロードバランサーとしても動作し、クライアントからのリクエストを複数のバックエンドサーバに分散させ、負荷分散とパフォーマンスの最適化を実現します。NGINXは特に、大量の同時リクエスト処理が重要なIoTアプリケーションに適しています。IoTでは多数のデバイスが存在するため、高負荷のリクエストを処理できるサーバが求められます。EMQXは複数のMQTTサーバからなる分散クラスターアーキテクチャをネイティブにサポートしています。したがって、NGINXをロードバランサーとして導入しEMQXクラスターを構築することで、高可用性とスケーラビリティを確保できます。

本ページでは、NGINXのインストール方法と、EMQXクラスターのMQTTサーバを構築するためのリバースプロキシおよびロードバランス設定方法を紹介します。また、NGINX Plusを用いたEMQX展開の最適化方法も解説します。

## 特長とメリット

NGINXを用いてEMQXクラスターのロードバランスを行うことには、以下のような特長と利点があります。

- リバースプロキシサーバとして、NGINXはMQTTサーバ側に位置し、MQTTクライアントのMQTT接続要求をEMQXクラスターに代わって開始し、EMQXクラスターからの応答をMQTTクライアントに返します。この構成により複数のクラスターを隠蔽し、MQTTクライアントには単一のアクセスポイントを公開します。MQTTクライアントはNGINXとだけ通信すればよく、背後のクラスターの数や構成を意識する必要がありません。これによりシステムの保守性とスケーラビリティが向上します。
- NGINXはMQTTクライアントとEMQXクラスター間のSSL暗号化接続を終端でき、EMQXクラスターの暗号化・復号化負荷を軽減します。これによりパフォーマンス向上、証明書管理の簡素化、セキュリティ強化などのメリットがあります。
- NGINXは柔軟なロードバランス戦略を提供し、クラスター内のどのEMQXノードにリクエストを送るかを制御できます。これによりトラフィックやリクエストを分散し、パフォーマンスと信頼性を向上させます。例えば、スティッキー（sticky）ロードバランスにより同じバックエンドサーバにリクエストをルーティングし、パフォーマンスやセッションの持続性を高められます。

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

3. [MQTTX](https://mqttx.app) CLIを使って10個のTCP接続を確立し、MQTTクライアント接続をシミュレートします。

```bash
mqttx bench conn -c 10
```

4. NGINXの接続状況とEMQXクライアント接続の分散状況を確認できます。

   - 以下のコマンドでNGINXの接続モニタリングを確認します。

     ```bash
     $ curl http://localhost:8888/status
     Active connections: 11
     server accepts handled requests
      60 60 65
     Reading: 0 Writing: 1 Waiting: 0
     ```

     現在のアクティブ接続数やサーバのリクエスト処理統計（読み込み中、書き込み中、待機中の状態）を表示します。

   - 以下のコマンドで各EMQXノードのクライアント接続状況をそれぞれ確認します。

     ```bash
     docker exec -it emqx1 emqx ctl broker stats | grep connections.count
     docker exec -it emqx2 emqx ctl broker stats | grep connections.count
     docker exec -it emqx3 emqx ctl broker stats | grep connections.count
     ```

     各ノードの接続数とアクティブ接続数を表示し、10接続がクラスター内のノードに均等に分散されていることを確認できます。

     ```bash
     connections.count             : 3
     live_connections.count        : 3
     connections.count             : 4
     live_connections.count        : 4
     connections.count             : 3
     live_connections.count        : 3
     ```

これらの手順により、サンプルでNGINXのロードバランス機能とEMQXクラスター内のクライアント接続分散を検証できます。`emqx-usage-example/mqtt-lb-nginx/nginx.conf`ファイルを編集してカスタム設定の検証も可能です。

## NGINXのインストールと使用方法

このセクションでは、NGINXのインストールと使用方法を詳しく説明します。

### 前提条件

開始前に、以下の3つのEMQXノードからなるクラスターを作成しておいてください。EMQXクラスターの作成方法は[クラスターの作成](./create-cluster.md)を参照してください。

| ノードアドレス           | MQTT TCPポート | MQTT WebSocketポート |
| ------------------------ | -------------- | -------------------- |
| emqx1-cluster.emqx.io    | 1883           | 8083                 |
| emqx2-cluster.emqx.io    | 1883           | 8083                 |
| emqx3-cluster.emqx.io    | 1883           | 8083                 |

本ページの例では、単一のNGINXサーバをロードバランサーとして設定し、これら3つのEMQXノードからなるクラスターにリクエストを転送します。

### NGINXのインストール

本デモではUbuntu 22.04 LTSにソースコードからNGINXをインストールします。Dockerやバイナリパッケージでのインストールも可能です。

#### 必要な依存パッケージ

NGINXのコンパイル・インストール前に以下の依存パッケージがインストールされていることを確認してください。

- GNU C/C++コンパイラ
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

ダウンロード後、展開してソースコードディレクトリに移動します。

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

上記のうち、`--with-http_ssl_module`はSSL対応、`--with-stream`および`--with-stream_ssl_module`はTCPリバースプロキシ対応のためのオプションです。

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

システムのPATH内にNGINX実行ファイルへのシンボリックリンクを作成します。

```bash
sudo ln -s /usr/local/nginx/sbin/nginx /usr/local/bin/nginx
```

### 使い始め

NGINXの設定ファイルはデフォルトで`/usr/local/nginx/conf/nginx.conf`にあります。本ページの設定例をファイル末尾に追記してください。基本的なNGINX操作コマンドは以下の通りです。

設定ファイルの検証：

```bash
sudo nginx -t
```

設定ファイルが正常ならNGINXを起動：

```bash
sudo nginx
```

稼働中のNGINXに設定を反映するには、エラーがないか確認してからリロードします。

```bash
sudo nginx -s reload
```

NGINXを停止するには：

```bash
sudo nginx stop
```

## NGINXのリバースプロキシおよびロードバランス設定

このセクションでは、さまざまなロードバランス要件に対応するNGINXの設定方法を説明します。

### MQTTのリバースプロキシ設定

以下の設定をNGINXの設定ファイルに記述することで、クライアントからのMQTT接続要求をリバースプロキシし、バックエンドのMQTTサーバに転送できます。

```bash
stream {
  upstream mqtt_servers {
    # down: 現在サーバが一時的にロードバランス対象外であることを示す
    # max_fails: 許容される失敗リクエスト数（デフォルトは1）
    # fail_timeout: max_failsに達した際の失敗リクエストのタイムアウト（デフォルト10秒）
    # backup: 全ての非バックアップサーバがダウンまたはビジー時にリクエストを送るバックアップサーバ

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

NGINXでMQTTのTLS接続を終端し、クライアントからの暗号化されたMQTT要求をバックエンドMQTTサーバに転送して通信の安全性を確保できます。TCPベースの設定にSSL関連パラメータを追加するだけです。

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

以下の設定で、NGINXがMQTT WebSocket接続をリバースプロキシし、クライアント要求をバックエンドMQTTサーバに転送します。`server_name`でHTTPのドメイン名またはIPアドレスを指定する必要があります。

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
      # WebSocket接続のタイムアウト
      # この時間内にデータ交換がない場合、WebSocket接続は自動的に切断される。デフォルトは60秒
      proxy_send_timeout 3600s;
      proxy_read_timeout 3600s;

      # リバースプロキシ元の実IP
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header REMOTE-HOST $remote_addr;
      proxy_set_header X-Forwarded-For $remote_addr;
    }
  }
}
```

::: tip
WebSocketの例では、EMQXがクライアントの送信元アドレスとしてこのヘッダーの最初（左端）の値を読み取るため、`X-Forwarded-For`を`$remote_addr`で上書きしています。`$proxy_add_x_forwarded_for`は使用しないでください。これは既存の`X-Forwarded-For`ヘッダーに`$remote_addr`を追加するため、クライアントが偽装可能な値が左端に残ってしまいます。詳細は[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners)を参照してください。
:::

### MQTT WebSocket SSLのリバースプロキシ設定

NGINXでMQTT WebSocketのTLS接続を終端し、クライアントからの暗号化されたMQTT要求をバックエンドMQTTサーバに転送して通信の安全性を確保できます。`server_name`でHTTPのドメイン名またはIPアドレスを指定します。WebSocketベースの設定にSSLおよび証明書関連パラメータを追加するだけです。

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

        # リバースプロキシ元の実IP
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

NGINXは接続分散の制御に複数のロードバランス戦略を提供しています。実際の運用ではサーバ性能やトラフィック要件に応じて適切な戦略を選択することが重要です。以下は`upstream`ブロック内で設定可能な代表的なNGINXロードバランス戦略です。

#### ラウンドロビン

デフォルトのロードバランス戦略です。リクエストを各バックエンドサーバに順番に均等に分配します。バックエンドサーバの性能がほぼ同等の場合に適しています。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

#### 重み付きラウンドロビン

ラウンドロビンに加え、各EMQXノードに異なる重みを割り当ててリクエストの分配比率を調整します。重みが高いサーバほど多くのリクエストを受け取ります。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883 weight=3;
  server emqx2-cluster.emqx.io:1883 weight=2;
  server emqx3-cluster.emqx.io:1883 weight=1;
}
```

#### IPハッシュ

クライアントのIPアドレスを基にハッシュ計算し、特定のバックエンドサーバにリクエストを割り当てます。同じクライアントからのリクエストは常に同じサーバにルーティングされます。

```bash
upstream backend_servers {
  ip_hash;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

#### 最小接続数

現在の接続数が最も少ないサーバにリクエストを割り当て、各サーバの負荷を均等化します。サーバ性能に差がある場合に適しています。

```bash
upstream backend_servers {
  least_conn;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

## NGINX Plusを使ったEMQX展開の最適化

このセクションでは、NGINX Plus固有の機能を用いたEMQX展開の最適化方法を紹介します。本ページでコンパイル・インストールしたNGINX（オープンソース版）では利用できない設定例も含まれます。NGINX Plusを用いたMQTT接続の最適化については、[こちらのドキュメント](https://www.nginx.com/blog/optimizing-mqtt-deployments-in-enterprise-environments-nginx-plus/)を参照してください。

### MQTTスティッキーセッションロードバランスの設定

「スティッキー」とは、ロードバランサーがクライアントの再接続時に同じサーバへルーティングし、セッションの乗っ取りを防ぐ機能です。頻繁に再接続するクライアントや切断・再接続を繰り返す問題のあるクライアントに効果的で、効率化に寄与します。

スティッキーを実現するには、サーバが接続要求内のクライアント識別子（通常はクライアントID）を特定する必要があります。ロードバランサーがMQTTパケットを解析しクライアント識別子を取得後、静的クラスターではこれをハッシュ化してサーバIDに変換したり、ロードバランサー側でクライアント識別子と宛先ノードIDのマッピングテーブルを保持して柔軟にルーティングします。

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

上記例は環境に応じて調整が必要な場合があります。設定で使われるモジュール（`ip_hash`や`least_conn`など）はNGINX標準モジュールであり、追加のモジュール依存はありません。

### クライアントID置換機能の設定

MQTT通信においてセキュリティは重要です。デバイスはシリアル番号などの機密情報をクライアントIDに使うことが多く、MQTTサーバのデータベースに保存するとリスクがあります。NGINX PlusはクライアントID置換機能を提供し、NGINX Plus設定で指定した別の値にクライアントIDを置き換えられます。

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

この例ではクライアントの相互認証を有効化し、クライアントのSSL証明書から証明書シリアル番号を抽出して元のクライアントIDの代わりに使用しています。`$ssl_client_s_dn`など他の値を使って証明書のDNを抽出することも可能です。

## NGINXのパフォーマンス最適化とモニタリング有効化

このセクションでは、NGINXのパフォーマンスを設定で最適化し、ステータスモニタリング機能を有効にする方法を説明します。

### NGINX基本設定の調整

- `worker_processes`：ワーカープロセス数。サーバのCPUコア数に近い値に設定します。ただし多すぎるとリソース競合が発生するため注意してください。
- `worker_connections`：1つのワーカープロセスが処理可能な同時接続数の最大値。OSの最大ファイルディスクリプタ制限を超えないように設定します。

```bash
worker_processes auto;

events {
 worker_connections 20480;
}
```

### リバースプロキシにおける大量接続対応のためのNGINXマルチNICサポート

リバースプロキシでは、NGINXがクライアントとしてバックエンドEMQXノードに接続します。この場合、単一IPアドレスで最大約6万の長時間接続を作成可能です。より多くの接続をサポートするには、複数のNGINXサーバを展開するか、複数のIPアドレスを設定します。

以下はNGINX標準の`split_clients`モジュールを使い、クライアントのIPアドレスとポート番号に基づいて変数`$multi_ip`を定義し、複数IPに分散する例です。使用するIPアドレスはローカルに存在するものを指定してください。

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

http://localhost:8888/status にアクセスするとステータス情報を取得できます。

```bash
$ curl http://localhost:8888/status
Active connections: 12
server accepts handled requests
 25 25 60
Reading: 0 Writing: 1 Waiting: 1
```

## 付録：主なパラメータの説明

以下は例示した設定で使用されている主なパラメータの説明です。これらのパラメータはバックエンドMQTTサーバへの安定した接続を確保したり、NGINX経由でMQTT通信を暗号化・保護し、IoTアプリケーションの通信プライバシーと整合性を守るためのベストプラクティスに従っています。

| パラメータ名            | 説明                                                         |
| ----------------------- | ------------------------------------------------------------ |
| proxy_protocol          | PROXYプロトコルを有効化し、NGINXが接続開始時に追加のプロキシ情報を付加して転送。EMQXが実クライアントIPを取得可能にする。 |
| proxy_pass              | バックエンドMQTTサーバのアドレスを定義。クライアントからの全リクエストをここに転送する。 |
| proxy_connect_timeout   | バックエンドMQTTサーバへの接続確立タイムアウト。時間内に接続できなければNGINXは接続試行を中止。 |
| proxy_timeout           | バックエンドMQTTサーバの応答タイムアウト。時間内に応答がなければNGINXは接続を切断。 |
| proxy_buffer_size       | バックエンドMQTTサーバから受信したデータを格納するバッファサイズ。大きなデータストリームに対応できるよう十分なサイズを確保。 |
| tcp_nodelay             | TCP_NODELAYオプションを有効化し、Nagleアルゴリズムを無効化。パケット送信のレイテンシを低減し、リアルタイムMQTT通信に寄与。 |
| ssl_session_cache       | 共有SSLセッションキャッシュを設定。SSLセッションの状態を保存し、クライアント再接続時のハンドシェイク高速化に寄与。`shared:SSL:10m`はキャッシュ名とサイズ（10MB）を指定。 |
| ssl_session_timeout     | SSLセッションのタイムアウトを10分に設定。再利用されなかったセッションは期限切れでクリアされる。 |
| ssl_certificate         | SSL証明書ファイルのパス。サーバの身元証明に使用。 |
| ssl_certificate_key     | SSL証明書に対応する秘密鍵ファイルのパス。 |
| ssl_protocols           | 許可するSSL/TLSプロトコルのバージョンを指定。 |
| ssl_ciphers             | 許可する暗号化アルゴリズム（暗号スイート）を設定。`HIGH:!aNULL:!MD5`は強力な暗号スイートを使用し、空の暗号スイートやMD5ハッシュを除外。 |
| ssl_client_certificate  | クライアント証明書の検証に用いる認証局（CA）証明書ファイルのパス。 |
| ssl_verify_client       | クライアント証明書の検証を有効化。`on`に設定するとNGINXは有効なSSL証明書をクライアントに要求。 |
| ssl_verify_depth        | クライアント証明書検証の最大深度を設定。ここでは`1`でクライアント証明書とCA証明書の1段階検証を行う。 |

## 参考情報

EMQXはNGINXに関する豊富なリソースを提供しています。以下のリンクから詳細をご覧ください。

**ブログ：**

- [NGINX PlusでのEMQXスティッキーセッション活用：「クライアントID」を魔法の鍵に](https://www.emqx.com/en/blog/harnessing-sticky-sessions-for-mqtt-load-balancing-with-nginx-plus)
- [NGINX PlusのクライアントID置換とEMQX EnterpriseによるMQTTアプリケーションのセキュリティ強化](https://www.emqx.com/en/blog/securing-your-mqtt-based-applications-with-nginx-plus-client-id-substitution-and-emqx-enterprise)
- [EMQXとNGINX Plusでのクライアント証明書認証によるMQTTセキュリティ向上](https://www.emqx.com/en/blog/elevating-mqtt-security-with-client-certificate-authentication)
