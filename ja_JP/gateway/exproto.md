# ExProto ゲートウェイ

Extension Protocol（ExProto）は、gRPC通信を用いて実装されたカスタムプロトコル解析ゲートウェイです。Java、Python、Goなどの好みのプログラミング言語でgRPCサービスを開発でき、これらのサービスはデバイスのネットワークプロトコルを解析し、デバイス接続、認証、メッセージ送信などの機能を実現します。

本ページでは、ExProtoゲートウェイの動作原理と、EMQXにおけるExProtoゲートウェイの設定および使用方法を紹介します。

<!--アーキテクチャの簡単な紹介-->

## ExProtoゲートウェイとgRPCサービスの動作

EMQXでExProtoゲートウェイを有効にすると、特定のポート（例：7993）でデバイス接続を待ち受けます。クライアントデバイスの接続を受け取ると、クライアントデバイスから生成されたバイトデータやイベントをユーザーのgRPCサービスに渡します。これには、ExProtoゲートウェイ内のgRPCクライアントが、ユーザーのgRPCサーバーで実装された`ConnectionUnaryHandler`サービスのメソッドを呼び出す必要があります。

ユーザーのgRPCサーバー内のgRPCサービスは、ExProtoゲートウェイから受け取ったバイトデータやイベントを解析し、クライアントのネットワークプロトコルを解釈してバイトデータやイベントをPub/Subリクエストに変換し、ExProtoゲートウェイに返送します。ExProtoゲートウェイで実装された`ConnectionAdapter`サービスは、ユーザーのgRPCサーバーとやり取りするためのインターフェースを提供します。これにより、クライアントデバイスはExProtoゲートウェイを介してEMQXにメッセージをパブリッシュし、トピックをサブスクライブし、クライアント接続を管理できます。

以下の図は、ExProtoゲートウェイとgRPCサービスの動作アーキテクチャを示しています。

<img src="./assets/exproto-gateway-architecture.png" alt="exproto-gateway-architecture" style="zoom:50%;" />

### `exproto.proto` ファイル

`exproto.proto` ファイルは、ExProtoゲートウェイとユーザーのgRPCサービス間のインターフェースを定義しています。ファイルには以下の2つのサービスが指定されています。

- `ConnectionAdapter` サービス：ExProtoゲートウェイが実装し、gRPCサーバーへのインターフェースを提供。
- `ConnectionUnaryHandler` サービス：ユーザーのgRPCサーバーが実装し、クライアントソケットの接続およびバイト解析の処理メソッドを定義。

### `ConnectionUnaryHandler` サービス

`ConnectionUnaryHandler` サービスは、ユーザーのgRPCサーバーで実装され、クライアントソケットの接続管理とバイト解析を担当します。

このサービスには以下のメソッドが含まれます。

| メソッド名           | 説明                                                         |
| -------------------- | ------------------------------------------------------------ |
| OnSocketCreated      | 新しいソケットがExProtoゲートウェイに接続されるたびに呼び出されるコールバック。 |
| OnSocketClosed       | ソケットが閉じられるたびに呼び出されるコールバック。         |
| OnReceivedBytes      | クライアントのソケットからデータを受信するたびに呼び出されるコールバック。 |
| OnTimerTimeout       | タイマーがタイムアウトするたびに呼び出されるコールバック。     |
| OnReceivedMessages   | サブスクライブしたトピックにメッセージが届くたびに呼び出されるコールバック。 |

ExProtoゲートウェイがこれらのメソッドを呼び出す際、どのソケットがこのイベントを送出したかを識別するために、パラメータに一意の識別子`conn`が渡されます。例えば、`OnSocketCreated`関数のパラメータは以下のようになります。

```
message SocketCreatedRequest {
  string conn = 1;
  ConnInfo conninfo = 2;
}
```

::: tip

ExProtoゲートウェイはプライベートプロトコルのメッセージフレームの開始と終了を認識できないため、TCPパケットのスティッキングやスプリッティングが発生した場合は、`OnReceivedBytes`コールバック内で処理する必要があります。

:::

### `ConnectionAdapter` サービス

`ConnectionAdapter` サービスはExProtoゲートウェイによって実装され、gRPCサービスがサブスクリプションの開始、メッセージのパブリッシュ、タイマーの開始、接続のクローズなどの接続管理機能を呼び出せるインターフェースを提供します。以下のメソッドを含みます。

| メソッド名       | 説明                                                         |
| ---------------- | ------------------------------------------------------------ |
| Send             | 指定された接続にバイトを送信する。                           |
| Close            | 指定された接続をクローズする。                               |
| Authenticate     | クライアントをExProtoゲートウェイに登録し、認証を完了する。   |
| StartTimer       | 指定された接続のためにタイマーを開始する。通常は生存確認に使用。 |
| Publish          | 指定された接続からEMQXにメッセージをパブリッシュする。       |
| Subscribe        | 指定された接続のためにサブスクリプションを作成する。         |
| Unsubscribe      | 指定された接続のサブスクリプションを削除する。               |
| RawPublish       | EMQXにメッセージをパブリッシュする。                         |

## ExProtoゲートウェイの有効化

EMQXのExProtoゲートウェイは、ダッシュボード、REST API、または設定ファイル`base.hocon`を通じて設定および有効化できます。本節では、ダッシュボードを使ったExProtoゲートウェイの有効化方法を説明します。

EMQXダッシュボードの左側ナビゲーションメニューから **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**ExProto** を見つけ、**Actions** 列の **Setup** をクリックします。すると、**Initialize ExProto** ページに遷移します。

::: tip

EMQXをクラスターで運用している場合、ダッシュボードやREST APIで行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md)でゲートウェイを設定してください。

:::

設定を簡単にするため、EMQXは**Gateways**ページのすべての必須フィールドにデフォルト値を用意しています。大幅なカスタマイズが不要な場合、以下の3クリックでExProtoゲートウェイを有効化できます。

1. **Basic Configuration** ステップページで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. 続いて表示される **Listeners** ステップページでは、EMQXがポート7993でTCPリスナーを事前設定しています。再度 **Next** をクリックして設定を確認します。
3. **Enable** ボタンをクリックしてExProtoゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、ExProtoゲートウェイのステータスが **Enabled** と表示されます。

<img src="./assets/exproto-enabled.png" alt="Enabled ExProto gateway" style="zoom:50%;" />

上記の設定はREST APIでも行えます。

**例：**

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/exproto' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "exproto",
  "enable": true,
  "mountpoint": "exproto/",
  "server": {
    "bind": "0.0.0.0:9100"
  }
  "handler": {
    "address": "http://127.0.0.1:9001"
    "ssl_options": {"enable": false}
  }
  "listeners": [
    {
      "type": "tcp",
      "bind": "7993",
      "name": "default",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```

詳細なREST APIの説明は[REST API](../admin/api.md)を参照してください。

カスタマイズが必要な場合、リスナーの追加や認証ルールの追加などは、[Customize Your ExProto Gateway](#customize-your-exproto-gateway)をご覧ください。

## ExProtoゲートウェイのカスタマイズ

デフォルト設定に加え、EMQXはさまざまな設定オプションを提供し、特定のビジネス要件に柔軟に対応できます。本節では、**Gateways** ページで利用可能な設定オプションを詳しく解説します。

### 基本設定

**Gateways** ページで **ExProto** を見つけ、**Actions** 列の **Settings** をクリックします。**Settings** タブでは、`ConnectionUnaryHandler`サービスのアドレス、`ConnectionAdapter`のリスニングポート、ゲートウェイのMountPoint文字列をカスタマイズできます。

<img src="./assets/exproto-basic-config.png" alt="Basic Configuration" style="zoom:50%;" />

- **Enable Statistics**：ゲートウェイによる統計収集と報告を許可するか設定。デフォルトは`true`。選択肢は`true`、`false`。
- **Idle Timeout**：接続されたクライアントが非アクティブとみなされ切断されるまでの秒数。デフォルトは30秒。
- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列。異なるプロトコル間でメッセージルーティングの分離を実現するために使用されます（例：`mqttsn/`）。このトピックプレフィックスはゲートウェイが管理し、クライアントは明示的に付加する必要はありません。
- **gRPC ConnectionAdapter**：`ConnectionAdapter`サービス起動のための設定。
  - **Bind**：gRPCサーバーのリッスンアドレスとポート。デフォルトは`0.0.0.0:9100`。
  - **TLS Verify Client**：ピア認証の有効・無効。デフォルトは無効。有効にすると、関連する**TLS Cert**、**TLS Key**、**CA Cert**情報をファイル内容の入力または**Select File**ボタンでアップロードして設定可能。詳細は[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)参照。
- **gRPC ConnectionHandler**：`ConnectionUnaryHandler`を実装したコールバックサーバーの設定。
  - **Server**：コールバックgRPCサーバーのアドレス。
  - **Enable TLS**：gRPCサーバーのTLS接続を有効化。デフォルトは無効。有効にすると以下の設定が可能。
    - **TLS Verify**：ピア認証の有効・無効。デフォルトは無効。設定方法は上記と同様。
    - **SNI**：TLSのServer Name Indication拡張で使用するホスト名を指定。

### リスナーの追加

デフォルトで、名前が**default**のTCPリスナーがポート`7993`に設定されており、1秒あたり最大1,000接続、最大1,024,000の同時接続をサポートします。より詳細な設定を行いたい場合は、**Listeners** タブをクリックし、編集、削除、新規リスナーの追加が可能です。

<img src="./assets/exproto-listener.png" alt="exproto-listener" style="zoom:50%;" />

**+ Add Listener** をクリックすると、以下の設定項目を入力できる**Add Listener**ページが開きます。

**基本設定**

- **Name**：リスナーの一意識別子を設定。
- **Type**：プロトコルタイプを選択。ExProtoでは`udp`または`dtls`が選択可能。
- **Bind**：リスナーが接続を受け付けるポート番号を設定。
- **MountPoint**（任意）：パブリッシュやサブスクライブ時にトピックの前に付加される文字列。異なるプロトコル間のメッセージルーティング分離に利用。

**リスナー設定**

- **Acceptor**：アクセプタープールのサイズ。デフォルトは16。
- **Max Connections**：リスナーが処理可能な最大同時接続数。デフォルトは1,024,000。
- **Max Connection Rate**：リスナーが1秒あたり受け入れ可能な新規接続の最大レート。デフォルトは1,000。
- **Proxy Protocol**：EMQXクラスターがHAProxyやNGINXの背後にある場合、Proxy Protocol V1/V2を有効化。デフォルトは`false`。
- **Proxy Protocol Timeout**：プロキシプロトコルのタイムアウト。指定時間内にプロキシプロトコルパケットが受信されない場合、EMQXはTCP接続を閉じる。デフォルトは3秒。

**TCP設定**

- **ActiveN**：ソケットの`{active, N}`オプション。ソケットが積極的に処理可能な受信パケット数。詳細は[Erlang Documentation - setopts/2](https://erlang.org/doc/man/inet.html#setopts-2)参照。
- **Buffer**：受信および送信パケットを格納するバッファサイズ（KB単位）。
- **TCP_NODELAY**：接続にTCP_NODELAYフラグを設定。デフォルトは`false`。
- **SO_REUSEADDR**：ローカルのポート番号再利用を許可するか。デフォルトは`true`。
- **Send Timeout**：接続のTCP送信タイムアウト。デフォルトは15秒。
- **Send Timeout Close**：送信タイムアウト時に接続を閉じるか。デフォルトは`true`。

**TLS設定**（SSLリスナーのみ）

TLS Verifyの有効化はトグルスイッチで設定可能ですが、その前に関連する**TLS Cert**、**TLS Key**、**CA Cert**情報をファイル内容の入力または**Select File**ボタンでアップロードして設定してください。詳細は[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)参照。

続いて以下の設定が可能です。

- **SSL Versions**：サポートするTLSバージョン。デフォルトは`tlsv1`、`tlsv1.1`、`tlsv1.2`、`tlsv1.3`。
- **SSL Fail If No Peer Cert**：クライアントが空の証明書を送信した場合にEMQXが接続を拒否するか。デフォルトは`false`。選択肢は`true`、`false`。
- **CACert Depth**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数。デフォルトは10。
- **Key File Passphrase**：プライベートキーがパスワード保護されている場合のパスワード。

### 認証の設定

ExProtoゲートウェイは以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../access-control/authn/mnesia.md)
- [MySQL認証](../access-control/authn/mysql.md)
- [MongoDB認証](../access-control/authn/mongodb.md)
- [PostgreSQL認証](../access-control/authn/postgresql.md)
- [Redis認証](../access-control/authn/redis.md)
- [HTTPサーバー認証](../access-control/authn/http.md)
- [JWT認証](../access-control/authn/jwt.md)
- [LDAP認証](../access-control/authn/ldap.md)

クライアント情報のClient ID、Username、Passwordはすべて`ConnectionAdapter`の`Authenticate`メソッドに渡されるパラメータから取得されます。

本節ではダッシュボードを例に認証設定方法を説明します。

ExProtoページで**Authentication**タブをクリックします。

**+ Create Authentication**をクリックし、**Mechanism**に`Password-Based`を選択、**Backend**に`HTTP Server`を選択して**Next**をクリックします。**Configuration**では認証ルールを設定できます。各項目の詳細は[HTTPサーバー認証](../access-control/authn/http.md)を参照してください。

<img src="./assets/exproto-authn-config.png" alt="mqttsn authentication" style="zoom:43%;" />

上記設定はREST APIでも可能です。

**例：**

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway/exproto/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "method": "post",
  "url": "http://127.0.0.1:8080",
  "headers": {
    "content-type": "application/json"
  },
  "body": {
    "username": "${username}",
    "password": "${password}"
  },
  "pool_size": 8,
  "connect_timeout": "5s",
  "request_timeout": "5s",
  "enable_pipelining": 100,
  "ssl": {
    "enable": false,
    "verify": "verify_none"
  },
  "backend": "http",
  "mechanism": "password_based",
  "enable": true
}'
```

## テスト用のサンプルgRPCサービスの起動

本節では、ExProtoゲートウェイとgRPCサービスが連携して動作する様子を、サンプルgRPCサービスを起動して示します。

このデモでは、`telnet`コマンドを使ってTCPプロトコルでメッセージの送受信を行うクライアントをシミュレートします。実際の環境では、カスタムプライベートプロトコルを実装したデバイスがポート7993のTCPリスナーに接続します。ExProtoゲートウェイはポート7993でクライアント接続を受け付け、ポート9100で`exproto.proto`ファイルで定義された`ConnectionAdapter`サービスを提供します。

[emqx-extension-examples](https://github.com/emqx/emqx-extension-examples)にはさまざまな言語のサンプルgRPCサービスが用意されています。本デモでは、Pythonで`ConnectionUnaryHandler`サービスを実装したエコープログラム`exproto-svr-python`を例に使用します。このプログラムはTCPクライアントから受信したデータをそのまま返します。実際の環境では、これらのアップストリームメッセージをEMQXにパブリッシュしたり、トピックをサブスクライブしてEMQXからのメッセージをクライアント接続に届けたりします。

以下の手順は`exproto-svr-python`を例に説明します。

::: tip 前提条件

開始前に以下を完了していることを確認してください。

- EMQX 5.1.0以上を起動し、デフォルト設定でExProtoゲートウェイを有効化している。
- Python 3.7以上をインストールし、以下の依存パッケージをインストールしている。

  ```
  python -m pip install grpcio
  python -m pip install grpcio-tools
  ```

:::

1. EMQXが動作している同一マシンでサンプルコードをクローンし、`exproto-svr-python`ディレクトリに移動します。

   ```bash
   git clone https://github.com/emqx/emqx-extension-examples
   cd exproto-svr-python
   ```

2. 以下のコマンドでgRPCサーバーを起動します。

   ```
   python exproto_server.py
   ```

   正常に起動すると、以下のような出力が表示されます。

   ```
   ConnectionUnaryHandler started successfully, listening on 9001
   
   Tips: If the Listener of EMQX ExProto gateway listen on 7993:
         You can use the telnet to test the server, for example:
   
         telnet 127.0.0.1 7993
   
   Waiting for client connections...
   ```

3. `telnet`を使ってExProtoゲートウェイがリッスンしているポート`7993`にアクセスし、`Hi, this is tcp client!`と入力してgRPCサーバーが正常に動作しているかテストします。例：

   ```
   $ telnet 127.0.0.1 7993
   Trying 127.0.0.1...
   Connected to 127.0.0.1.
   Escape character is '^]'.
   Hi, this is tcp client!
   Hi, this is tcp client!
   ```

4. EMQXダッシュボードで左側ナビゲーションメニューから **Management** -> **Gateways** をクリックし、ExProtoの**Clients**をクリックします。ExProtoページで、telnetで接続したクライアントが表示されていることを確認できます。

   <img src="./assets/connected-exproto-client.png" alt="Connected ExProto Client" style="zoom:50%;" />

### サンプルのシーケンス図

以下の図は、本サンプルにおける接続とメッセージ配送のシーケンスを示しています。

<img src="./assets/exproto-sequence-diagram.png" alt="exproto-sequence-diagram" style="zoom:80%;" />

<!--```mermaid sequenceDiagram
    Telnet ->> ExProto Gateway: Establish a TCP connection
rect rgb(191, 223, 255)
    ExProto Gateway ->> exproto-svr-python: Call OnSocketCreated
  exproto-svr-python ->> ExProto Gateway: Call `Authenticate` to register client
  ExProto Gateway -->> exproto-svr-python: Succeed
  exproto-svr-python ->> ExProto Gateway: Call 'Subscribe' to subscribe 'test/echo'
    ExProto Gateway -->> exproto-svr-python: Succeed
  exproto-svr-python ->> ExProto Gateway: Call 'StartTimer' to start keepalive timer
    ExProto Gateway -->> exproto-svr-python: Succeed
    exproto-svr-python -->> ExProto Gateway: `OnSocketCreated` return
end
  Telnet ->> ExProto Gateway: Send 'Hi, this is...'
rect rgb(100,150, 240)
  ExProto Gateway ->> exproto-svr-python: Call `OnReceivedBytes`
  exproto-svr-python --> exproto-svr-python: Use 'Hi, this is...' to create a message
  exproto-svr-python ->> ExProto Gateway: Call `Publish` to publish message to 'test/echo'
  ExProto Gateway -->> ExProto Gateway: Route the message
  ExProto Gateway -->> exproto-svr-python: Succeed
  exproto-svr-python -->> ExProto Gateway: `OnReceivedBytes` return
end
rect rgb(100, 150, 200)
  ExProto Gateway ->> exproto-svr-python: Call `OnReceivedMessages`
  exproto-svr-python -->> exproto-svr-python: Use message payload
  exproto-svr-python ->> ExProto Gateway: Call `Send` to deliver bytes 'Hi, this is ...'
  ExProto Gateway -->> exproto-svr-python: Succeed
  ExProto Gateway ->> Telnet: Deliver 'Hi, this is...'
  exproto-svr-python -->> ExProto Gateway: `OnReceivedMessages` return
end ```-->
