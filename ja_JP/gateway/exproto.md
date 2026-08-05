# ExProto ゲートウェイ

Extension Protocol（ExProto）は、gRPC通信を用いて実装されたカスタムプロトコル解析ゲートウェイです。Java、Python、Goなど、ユーザーが好むプログラミング言語でgRPCサービスを開発できるようにし、デバイスのネットワークプロトコルを解析し、デバイス接続、認証、メッセージ送信などの機能を実現します。

本ページでは、ExProtoゲートウェイの動作原理と、EMQXにおけるExProtoゲートウェイの設定および利用方法について紹介します。

::: warning 重要なお知らせ
ExProtoゲートウェイはEMQX 6.2.0以降非推奨となり、EMQX 6.3.0で削除されました。
:::

<!--a brief introduction of the architecture-->

## ExProtoゲートウェイとgRPCサービスの動作

EMQXでExProtoゲートウェイを有効にすると、特定のポート（例：7993）でデバイス接続を待ち受けます。クライアントデバイスから接続があると、クライアントデバイスから生成されたバイトデータやイベントをユーザーのgRPCサービスに渡します。これには、ExProtoゲートウェイ内のgRPCクライアントが、ユーザーのgRPCサーバーで実装された`ConnectionUnaryHandler`サービスのメソッドを呼び出す必要があります。

ユーザーのgRPCサーバー内のgRPCサービスは、ExProtoゲートウェイから受け取ったバイトデータやイベントを解析し、クライアントのネットワークプロトコルを解釈してバイトデータやイベントをPub/Subリクエストに変換し、再びExProtoゲートウェイに送信します。ExProtoゲートウェイで実装された`ConnectionAdapter`サービスは、ユーザーのgRPCサーバーとやり取りするためのインターフェースを提供します。これにより、クライアントデバイスはExProtoゲートウェイを通じてEMQXにメッセージをパブリッシュしたり、トピックをサブスクライブしたり、クライアント接続を管理したりできます。

以下の図は、ExProtoゲートウェイとgRPCサービスの動作アーキテクチャを示しています。

<img src="./assets/exproto-gateway-architecture.png" alt="ExProtoゲートウェイのアーキテクチャ" style="zoom:50%;" />

### `exproto.proto` ファイル

`exproto.proto`ファイルは、ExProtoゲートウェイとユーザーのgRPCサービス間のインターフェースを定義します。ファイルには以下の2つのサービスが指定されています。

- `ConnectionAdapter`サービス：ExProtoゲートウェイで実装され、gRPCサーバーへのインターフェースを提供します。
- `ConnectionUnaryHandler`サービス：ユーザーのgRPCサーバーで実装され、クライアントソケットの接続処理やバイト解析のメソッドを定義します。

### `ConnectionUnaryHandler` サービス

`ConnectionUnaryHandler`サービスは、ユーザーのgRPCサーバーで実装され、クライアントソケットの接続処理やバイト解析を担当します。

このサービスには以下のメソッドが含まれます。

| メソッド名           | 説明                                                         |
| -------------------- | ------------------------------------------------------------ |
| OnSocketCreated      | 新しいソケットがExProtoゲートウェイに接続されるたびに呼び出されるコールバックです。 |
| OnSocketClosed       | ソケットが閉じられるたびに呼び出されるコールバックです。     |
| OnReceivedBytes      | クライアントのソケットからデータを受信するたびに呼び出されるコールバックです。 |
| OnTimerTimeout       | タイマーがタイムアウトするたびに呼び出されるコールバックです。 |
| OnReceivedMessages   | サブスクライブしたトピックにメッセージが届くたびに呼び出されるコールバックです。 |

ExProtoゲートウェイがこれらのメソッドを呼び出す際、どのソケットからのイベントかを識別するために、パラメータに一意の識別子`conn`が渡されます。例えば、`OnSocketCreated`の関数パラメータは以下のようになります。

```
message SocketCreatedRequest {
  string conn = 1;
  ConnInfo conninfo = 2;
}
```

::: tip

ExProtoゲートウェイはプライベートプロトコルのメッセージフレームの開始と終了を認識できないため、TCPパケットのスティッキングや分割が発生する場合は、`OnReceivedBytes`コールバック内で処理する必要があります。

:::

### `ConnectionAdapter` サービス

`ConnectionAdapter`サービスはExProtoゲートウェイで実装され、gRPCサービスがサブスクリプションの開始、メッセージのパブリッシュ、タイマーの開始、接続のクローズなどの接続管理機能を呼び出せるようにします。以下のメソッドを含みます。

| メソッド名       | 説明                                                         |
| ---------------- | ------------------------------------------------------------ |
| Send             | 指定した接続にバイトデータを送信します。                     |
| Close            | 指定した接続を閉じます。                                     |
| Authenticate     | クライアントをExProtoゲートウェイに登録し、認証を完了します。 |
| StartTimer       | 指定した接続のタイマーを開始します。通常は生存確認に使用します。 |
| Publish          | 指定した接続からEMQXへメッセージをパブリッシュします。       |
| Subscribe        | 指定した接続のサブスクリプションを作成します。               |
| Unsubscribe      | 指定した接続のサブスクリプションを削除します。               |
| RawPublish       | EMQXにメッセージをパブリッシュします。                       |

## ExProtoゲートウェイの有効化

EMQXのExProtoゲートウェイは、ダッシュボード、REST API、または設定ファイル`base.hocon`から設定および有効化できます。本節ではダッシュボードを使った有効化方法を説明します。

EMQXダッシュボードの左側ナビゲーションメニューから **管理** -> **ゲートウェイ** をクリックします。**ゲートウェイ**ページにはサポートされているすべてのゲートウェイが一覧表示されます。**ExProto**を見つけ、**操作**列の**セットアップ**をクリックすると、**ExProtoの初期化**ページに遷移します。

::: tip

EMQXをクラスターで運用している場合、ダッシュボードやREST APIで行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md)でゲートウェイを設定してください。

:::

設定を簡略化するため、EMQXは**ゲートウェイ**ページのすべての必須フィールドにデフォルト値を用意しています。大幅なカスタマイズが不要な場合、以下の3クリックでExProtoゲートウェイを有効化できます。

1. **基本設定**ステップページで**次へ**をクリックし、すべてのデフォルト設定を受け入れます。
2. 次に表示される**リスナー**ステップページでは、EMQXがポート7993でTCPリスナーを事前設定しています。再度**次へ**をクリックして設定を確定します。
3. **有効化**ボタンをクリックしてExProtoゲートウェイを起動します。

ゲートウェイの有効化が完了すると、**ゲートウェイ**ページに戻り、ExProtoゲートウェイの状態が**有効**と表示されます。

<img src="./assets/exproto-enabled.png" alt="有効化されたExProtoゲートウェイ" style="zoom:50%;" />

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

より詳細なカスタマイズやリスナーの追加、認証ルールの追加が必要な場合は、[ExProtoゲートウェイのカスタマイズ](#customize-your-exproto-gateway)を参照してください。

## ExProtoゲートウェイのカスタマイズ

デフォルト設定に加え、EMQXはさまざまな設定オプションを提供し、特定のビジネス要件に柔軟に対応できます。本節では**ゲートウェイ**ページで利用可能な設定オプションを詳しく解説します。

### 基本設定

**ゲートウェイ**ページで**ExProto**を見つけ、**操作**列の**設定**をクリックします。**設定**タブでは、`ConnectionUnaryHandler`サービスのアドレス、`ConnectionAdapter`のリスニングポート、ゲートウェイのMountPoint文字列をカスタマイズできます。

<img src="./assets/exproto-basic-config.png" alt="基本設定" style="zoom:50%;" />

- **統計情報の有効化**：ゲートウェイの統計収集とレポートを許可するか設定します。デフォルトは`true`。選択肢は`true`または`false`です。
- **アイドルタイムアウト**：クライアントが非アクティブと見なされるまでの秒数を設定します。デフォルトは`30秒`です。
- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックに接頭辞として付加される文字列を設定します。これにより異なるプロトコル間でメッセージルーティングの分離が可能です（例：`mqttsn/`）。このトピック接頭辞はゲートウェイが管理し、クライアントは明示的に付加する必要はありません。
- **gRPC ConnectionAdapter**：`ConnectionAdapter`サービス起動の設定を行います。
  - **Bind**：gRPCサーバーの待ち受けアドレスとポート。デフォルトは`0.0.0.0:9100`です。
    - **TLS クライアント認証**：ピア認証の有効/無効。デフォルトは無効。有効にすると、関連する**TLS証明書**、**TLS秘密鍵**、**CA証明書**をファイル内容入力またはファイル選択ボタンでアップロードして設定できます。詳細は[SSL/TLS接続の有効化](../network/emqx-mqtt-tls.md)を参照してください。
- **gRPC ConnectionHandler**：`ConnectionUnaryHandler`を実装したコールバックサーバーの設定を行います。
  - **サーバー**：コールバックgRPCサーバーのアドレス。
    - **TLSを有効化**：gRPCサーバーのTLS接続を有効化。デフォルトは無効。有効化すると以下の設定が可能です。
      - **TLS検証**：ピア認証の有効/無効。デフォルトは無効。有効化すると、関連する**TLS証明書**、**TLS秘密鍵**、**CA証明書**をファイル内容入力またはファイル選択ボタンでアップロードして設定できます。
      - **SNI**：TLS Server Name Indication拡張で使用するホスト名を指定します。

### リスナーの追加

デフォルトで、ポート`7993`に名前が**default**のTCPリスナーが1つ設定されており、1秒あたり最大1,000接続、最大1,024,000同時接続をサポートします。よりカスタマイズした設定を行うには、**リスナー**タブをクリックして編集、削除、新規リスナー追加が可能です。

<img src="./assets/exproto-listener.png" alt="ExProtoリスナー設定" style="zoom:50%;" />

**+ リスナー追加**をクリックすると**リスナー追加**ページが開き、以下の設定項目を入力できます。

**基本設定**

- **名前**：リスナーの一意識別子を設定します。
- **タイプ**：プロトコルタイプを選択します。ExProtoでは`udp`または`dtls`を選択可能です。
- **バインド**：リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**（任意）：パブリッシュやサブスクライブ時にすべてのトピックに接頭辞として付加される文字列を設定します。

**リスナー設定**

- **アセプター**：アセプタープールのサイズを設定します。デフォルトは`16`です。
- **最大接続数**：リスナーが処理可能な最大同時接続数を設定します。デフォルトは`1024000`です。
- **最大接続レート**：リスナーが1秒あたり受け入れ可能な新規接続の最大数を設定します。デフォルトは`1000`です。
- **プロキシプロトコル**：EMQXクラスターがHAProxyやNGINXの背後にある場合、Proxy Protocol V1/V2を有効化します。デフォルトは`false`です。
- **プロキシプロトコルタイムアウト**：プロキシプロトコルのタイムアウト時間。タイムアウト内にプロキシプロトコルパケットを受信しない場合、EMQXはTCP接続を切断します。デフォルトは`3秒`です。

**TCP設定**

- **ActiveN**：ソケットの`{active, N}`オプションを設定します。これはソケットが積極的に処理できる受信パケット数を意味します。詳細は[Erlangドキュメント - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2)を参照してください。
- **バッファ**：受信および送信パケットを格納するバッファサイズ（KB単位）を設定します。
- **TCP_NODELAY**：接続のTCP_NODELAYフラグを設定します。デフォルトは`false`です。
- **SO_REUSEADDR**：ローカルのポート番号再利用を許可するか設定します。デフォルトは`true`です。
- **送信タイムアウト**：接続のTCP送信タイムアウト時間を設定します。デフォルトは`15秒`です。
- **送信タイムアウト時の切断**：送信タイムアウト発生時に接続を切断するか設定します。デフォルトは`true`です。

**TLS設定**（SSLリスナーのみ）

TLS検証の有効化はトグルスイッチで設定できます。ただし、その前に関連する**TLS証明書**、**TLS秘密鍵**、**CA証明書**をファイル内容入力またはファイル選択ボタンでアップロードして設定する必要があります。詳細は[SSL/TLS接続の有効化](../network/emqx-mqtt-tls.md)を参照してください。

続いて以下の設定が可能です。

- **SSLバージョン**：サポートするTLSバージョンを設定します。デフォルトは`tlsv1`、`tlsv1.1`、`tlsv1.2`、`tlsv1.3`です。
- **クライアント証明書なしで失敗**：クライアントが空の証明書を送信した場合に接続を拒否するか設定します。デフォルトは`false`。選択肢は`true`または`false`です。
- **CA証明書の深さ**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは`10`です。
- **秘密鍵パスフレーズ**：秘密鍵がパスワード保護されている場合に使用するパスワードを設定します。

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

クライアントのClient ID、ユーザー名、パスワードはすべて`ConnectionAdapter`の`Authenticate`メソッドで渡されるパラメータから取得されます。

本節ではダッシュボードを例に認証設定方法を説明します。

ExProtoページで**認証**タブをクリックします。

**+ 認証作成**をクリックし、**メカニズム**に`Password-Based`を選択、**バックエンド**に`HTTP Server`を選択して**次へ**をクリックします。**設定**画面で認証ルールを設定できます。各項目の詳細は[HTTPサーバー認証](../access-control/authn/http.md)を参照してください。

<img src="./assets/exproto-authn-config.png" alt="mqttsn認証設定" style="zoom:43%;" />

上記の設定はREST APIでも実行可能です。

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

本節では、ExProtoゲートウェイとgRPCサービスがどのように連携するかを示すため、サンプルgRPCサービスの起動方法を説明します。

このデモでは、`telnet`コマンドを使ってTCPプロトコルでメッセージ送受信を行うクライアントをシミュレートします。実際の環境では、カスタムプライベートプロトコルを実装したデバイスがポート7993のTCPリスナーに接続します。ExProtoゲートウェイはポート7993でクライアント接続を待ち受け、ポート9100で`exproto.proto`ファイルで定義された`ConnectionAdapter`サービスを提供します。

[emqx-extension-examples](https://github.com/emqx/emqx-extension-examples)にはさまざまな言語のサンプルgRPCサービスが用意されています。本デモではPythonで実装されたエコープログラム`exproto-svr-python`を例に、`ConnectionUnaryHandler`サービスを実装します。これはTCPクライアントから受信したデータをそのまま返すシンプルなサービスです。実際の環境では、これらのアップストリームメッセージをEMQXにパブリッシュしたり、トピックをサブスクライブしてEMQXからのメッセージをクライアント接続に配信したりします。

以下は`exproto-svr-python`を例にした手順です。

::: tip 前提条件

開始前に以下を完了していることを確認してください。

- EMQX 5.1.0以降を起動し、デフォルト設定でExProtoゲートウェイを有効化していること。
- Python 3.7以上をインストールし、以下の依存パッケージをインストールしていること。

  ```
  python -m pip install grpcio
  python -m pip install grpcio-tools
  ```

:::

1. EMQXが稼働している同じマシンで、サンプルコードをクローンし、`exproto-svr-python`ディレクトリに移動します。

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

3. `telnet`コマンドでExProtoゲートウェイが待ち受けるポート`7993`にアクセスし、`Hi, this is tcp client!`と入力してgRPCサーバーが正常に動作しているか確認します。例：

   ```
   $ telnet 127.0.0.1 7993
   Trying 127.0.0.1...
   Connected to 127.0.0.1.
   Escape character is '^]'.
   Hi, this is tcp client!
   Hi, this is tcp client!
   ```

4. EMQXダッシュボードの左側ナビゲーションメニューから **管理** -> **ゲートウェイ** をクリックし、ExProtoの**クライアント**をクリックします。ExProtoページで、telnetで接続したクライアントが表示されます。

   <img src="./assets/connected-exproto-client.png" alt="接続されたExProtoクライアント" style="zoom:50%;" />

### サンプルのシーケンス図

以下の図は、本サンプルにおける接続とメッセージ配信のシーケンスを示しています。

<img src="./assets/exproto-sequence-diagram.png" alt="ExProtoシーケンス図" style="zoom:80%;" />

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
