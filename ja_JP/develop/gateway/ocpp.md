# OCPPゲートウェイ

[OCPP](https://www.openchargealliance.org/)（Open Charge Point Protocol）は、充電ステーションと中央管理システムを接続するオープンな通信プロトコルであり、電気自動車充電インフラ向けの統一された通信標準を提供することを目的としています。OCPPゲートウェイはプロトコル変換器として機能し、OCPPとMQTTプロトコル間の橋渡しを行うことで、これらのプロトコルを使用するクライアント同士の通信を可能にします。

EMQXは[OCPP 1.6-J](https://openchargealliance.org/protocols/open-charge-point-protocol/#OCPP1.6)に対応したプロトコルゲートウェイを追加しており、OCPP仕様に準拠した様々なブランドの充電ステーション機器と接続可能です。ルールエンジン、データ統合、REST APIなどを通じて管理システム（Central System）と連携し、ユーザーが迅速に電気自動車充電インフラを構築できるよう支援します。

本ページでは、EMQXにおけるOCPPゲートウェイの設定および使用方法を紹介します。

## OCPPゲートウェイの有効化

EMQXのOCPPゲートウェイは、ダッシュボード、REST API、設定ファイル`base.hocon`を通じて設定および有効化が可能です。本節ではダッシュボードによる設定例を用いて操作手順を説明します。

EMQXダッシュボードの左側ナビゲーションメニューから **Management** -> **Gateways** をクリックします。**Gateways**ページにはサポートされているすべてのゲートウェイが一覧表示されます。**OCPP**を探し、**Actions**列の**Setup**をクリックすると、**Initialize OCPP**ページに遷移します。

::: tip

EMQXをクラスターで運用している場合、ダッシュボードやREST APIで行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../../guides/configuration/configuration.md)で設定してください。

:::

設定を簡略化するため、EMQXは**Gateways**ページのすべての必須フィールドにデフォルト値を用意しています。大幅なカスタマイズが不要な場合、以下の3クリックでOCPPゲートウェイを有効化できます。

1. **Basic Configuration**タブで**Next**をクリックし、すべてのデフォルト設定を受け入れます。
2. 次に**Listeners**タブに遷移し、EMQXがポート`33033`でWebsocketリスナーを事前設定しています。再度**Next**をクリックして設定を確認します。
3. 最後に**Enable**ボタンをクリックしてOCPPゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways**ページに戻り、OCPPゲートウェイのステータスが**Enabled**と表示されます。

<img src="./assets/ocpp-enabled.png" alt="OCPPゲートウェイ有効化" style="zoom:50%;" />

上記の設定はREST APIでも行えます。

**例:**

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateways/ocpp' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "ocpp",
  "enable": true,
  "mountpoint": "ocpp/",
  "listeners": [
    {
      "type": "ws",
      "name": "default",
      "bind": "33033",
      "websocket": {
        "path": "/ocpp"
      }
    }
  ]
}'
```

## OCPPクライアントとの連携

OCPPゲートウェイが稼働したら、OCPPクライアントツールを使って接続テストや設定の動作確認が可能です。

ここでは[ocpp-go](https://github.com/lorenzodonini/ocpp-go)を例に、EMQXのOCPPゲートウェイへの接続方法を紹介します。

1. まず、OCPPゲートウェイと連携するMQTTクライアントを準備します。例えば[MQTTX](https://mqttx.app/downloads)を用い、EMQXに接続してトピック`ocpp/#`をサブスクライブするよう設定します。

   <img src="./assets/ocpp-mqttx-create-conn.png" alt="MQTT接続作成" style="zoom:67%;" />

2. ocpp-goクライアントを実行し、OCPPゲートウェイに接続します。

   **注意**：以下のコマンド内の`<host>`はEMQXサーバーのアドレスに置き換えてください。

   ```shell
   docker run -e CLIENT_ID=chargePointSim -e CENTRAL_SYSTEM_URL=ws://<host>:33033/ocpp -it --rm --name charge-point ldonini/ocpp1.6-charge-point:latest
   ```

   接続成功時は以下のようなログが出力されます。

   ```css
   INFO[2023-12-01T03:08:39Z] connecting to server logger=websocket
   INFO[2023-12-01T03:08:39Z] connected to server as chargePointSim logger=websocket
   INFO[2023-12-01T03:08:39Z] connected to central system at ws://172.31.1.103:33033/ocpp
   INFO[2023-12-01T03:08:39Z] dispatched request 1200012677 to server logger=ocppj
   ```

3. MQTTXで以下のようなメッセージが受信されることを確認します。

   ```json
   Topic: ocpp/cp/chargePointSim
   {
     "UniqueId": "1200012677",
     "Payload": {
       "chargePointVendor": "vendor1",
       "chargePointModel": "model1"
     },
     "Action": "BootNotification"
   }
   ```

   このメッセージはocpp-goクライアントがOCPPゲートウェイに接続し、`BootNotification`リクエストを送信したことを示します。

4. MQTTXでトピック`ocpp/cs/chargePointSim`に対して以下の内容のメッセージを作成し送信します。

   **注意**：`UniqueId`は前のメッセージで受信した値に置き換えてください。

   ```json
   {
     "MessageTypeId": 3,
     "UniqueId": "***",
     "Payload": {
       "currentTime": "2023-12-01T14:20:39+00:00",
       "interval": 300,
       "status": "Accepted"
     },
     "Action": "BootNotification"
   }
   ```

5. その後、MQTTXで`StatusNotification`のステータスレポートを受信します。これはOCPPクライアントがOCPPゲートウェイとの接続を正常に確立したことを示しています。

   ```json
   Topic: ocpp/cp/chargePointSim
   Payload:
   {
     "UniqueId": "3062609974",
     "Payload": {
       "status": "Available",
       "errorCode": "NoError",
       "connectorId": 0
     },
     "MessageTypeId": 2,
     "Action": "StatusNotification"
   }
   ```

## OCPPゲートウェイのカスタマイズ

デフォルト設定に加え、EMQXは多様な設定オプションを提供しており、特定のビジネス要件に合わせた調整が可能です。本節では**Gateways**ページで利用可能な各フィールドを詳しく解説します。

### 基本設定

GatewaysページでOCPPゲートウェイの**Actions**列にある**Settings**ボタンをクリックすると、**Basic Configuration**タブで以下の項目を設定できます。

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定します。異なるプロトコル間でメッセージルーティングの分離を実現する手段として利用可能です。例：`ocpp/`。
- **Default Heartbeat Interval**：デフォルトのハートビート間隔（秒）、初期値は`60s`。
- **Heartbeat Checking Times Backoff**：ハートビートチェックのバックオフ回数、初期値は`1`。
- **Message Format Checking**：メッセージフォーマットの妥当性チェックを有効にするかどうか。EMQXはアップロードおよびダウンロードのストリームメッセージをjson-schemaで定義されたフォーマットに対して検証し、チェックに失敗した場合は対応する応答メッセージを返します。設定可能な値は以下の通りです。

    - `disable`：メッセージのチェックを行わない（デフォルト）。
    - `upstream_only`：アップロードストリームメッセージのみチェック。
    - `dnstream_only`：ダウンロードストリームメッセージのみチェック。
    - `all`：すべてのメッセージをチェック。

- **JSON Schema Directory**：OCPPメッセージ定義のJSON Schemaディレクトリ、デフォルトは`${application}/priv/schemas`。
- **JSON Schema ID Prefix**：OCPPメッセージスキーマのIDプレフィックス、デフォルトは`urn:OCPP:1.6:2019:12:`。
- **Idle Timeout**：非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）。
- **Upstream**：アップロードストリームの設定グループ。
    - **Topic**：アップロードストリームのCall Requestメッセージ用トピック、デフォルトは`cp/${cid}`。
    - **Reply Topic**：アップロードストリームのReplyメッセージ用トピック、デフォルトは`cp/${cid}/Reply`。
    - **Error Topic**：アップロードストリームのErrorメッセージ用トピック、デフォルトは`cp/${cid}/Reply`。
    - **Topic Override Mapping**：メッセージ名によるアップロードストリームのトピック上書きマッピング。
- **Downstream**：ダウンロードストリームの設定グループ。
    - **Topic**：EMQXからのリクエスト／制御メッセージを受信するダウンロードストリームのトピック。これはすべての接続されたチャージポイントがサブスクライブするワイルドカードトピック名です。デフォルトは`cs/${cid}`。
    - **Max Message Queue Length**：ダウンロードストリームのメッセージ配信における最大キュー長、デフォルトは`100`。

### リスナーの追加

ポート`33033`で名前が**default**のWebsocketリスナーが既に設定されており、プール内のアセプター数は最大16、同時接続数は最大1,024,000まで対応可能です。リスナーの**Settings**で詳細設定を行ったり、**Delete**でリスナーを削除、または**+ Add Listener**で新規リスナーを追加できます。

::: tip

OCPPゲートウェイはWebsocketおよびTLS上のWebsocketタイプのリスナーのみをサポートしています。

:::

**Add Listener**をクリックすると**Add Listener**ページが開き、以下の設定を行えます。

**基本設定**

- **Name**：リスナーの一意識別子を設定します。
- **Type**：プロトコルタイプを選択します。OCPPの場合は`ws`または`wss`を選択可能です。
- **Bind**：リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティング分離を実現します。

**リスナー設定**

- **Path**：接続アドレスのパスプレフィックスを設定します。クライアントは接続時にこのパス全体を指定する必要があります。デフォルトは`/ocpp`です。
- **Acceptor**：アセプタープールのサイズを設定します。デフォルトは`16`。
- **Max Connections**：リスナーが処理可能な最大同時接続数を設定します。デフォルトは`1024000`。
- **Max Connection Rate**：リスナーが1秒あたりに受け入れ可能な新規接続の最大レートを設定します。デフォルトは`1000`。
- **Proxy Protocol**：EMQXが[ロードバランサー](../../guides/cluster/lb.md)の背後にある場合に、プロトコルV1/V2を有効化します。
- **Proxy Protocol Timeout**：非アクティブ状態でプロキシプロトコルパッケージを待機する最大時間（秒）、デフォルトは`3s`。

**TCP設定**

- **ActiveN**：ソケットの`{active, N}`オプションを設定します。これはソケットが積極的に処理可能な受信パケット数です。詳細は[Erlangドキュメント - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2)を参照してください。
- **Buffer**：受信および送信パケットを格納するバッファのサイズ（KB単位）を設定します。
- **TCP_NODELAY**：クライアントが前のデータのアックを待たずに追加データを送信できるかどうかを設定します。デフォルトは`false`。設定値は`true`または`false`。
- **SO_REUSEADDR**：ポート番号のローカル再利用を許可するかどうかを設定します。
- **Send Timeout**：非アクティブ状態で送信タイムアウトを待機する最大時間（秒）、デフォルトは`15s`。
- **Send Timeout Close**：送信タイムアウト時に接続を切断するかどうかを設定します。

**SSL設定**（wssリスナーのみ）

TLS検証の有効化はトグルスイッチで設定可能です。ただし事前に**TLS Cert**、**TLS Key**、**CA Cert**の情報をファイル内容入力または**Select File**ボタンでアップロードして設定する必要があります。詳細は[SSL/TLS接続の有効化](../../guides/network/emqx-mqtt-tls.md)を参照してください。

続いて以下の設定が可能です。

- **SSL Versions**：サポートするSSLバージョンを設定します。デフォルトは`tlsv1.3`、`tlsv1.2`、`tlsv1.1`、`tlsv1`。
- **Fail If No Peer Cert**：クライアントが空の証明書を送信した場合に接続を拒否するかどうか。デフォルトは`false`。設定値は`true`または`false`。
- **Intermediate Certificate Depth**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数。デフォルトは`10`。
- **Key Password**：プライベートキーがパスワード保護されている場合に使用するユーザーパスワード。

## 認証の設定

OCPPプロトコルの接続メッセージにはすでにユーザー名とパスワードの概念が定義されているため、OCPPは以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../../guides/access-control/authn/mnesia.md)
- [MySQL認証](../../guides/access-control/authn/mysql.md)
- [MongoDB認証](../../guides/access-control/authn/mongodb.md)
- [PostgreSQL認証](../../guides/access-control/authn/postgresql.md)
- [Redis認証](../../guides/access-control/authn/redis.md)
- [HTTPサーバ認証](../../guides/access-control/authn/http.md)
- [JWT認証](../../guides/access-control/authn/jwt.md)
- [LDAP認証](../../guides/access-control/authn/ldap.md)

OCPPゲートウェイはWebsocketハンドシェイクメッセージのBasic Authentication情報を用いてクライアントの認証フィールドを生成します。

- クライアントID：固定パスプレフィックス以降の接続アドレス部分の値。
- ユーザー名：Basic AuthenticationのUsernameの値。
- パスワード：Basic AuthenticationのPasswordの値。

REST APIを使ってOCPPゲートウェイ用の組み込みデータベース認証を作成することも可能です。

**例:**

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateways/ocpp/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "backend": "built_in_database",
  "mechanism": "password_based",
  "password_hash_algorithm": {
    "name": "sha256",
    "salt_position": "suffix"
  },
  "user_id_type": "username"
}'
```

::: tip

MQTTプロトコルとは異なり、**ゲートウェイは認証器の作成のみをサポートし、認証器一覧（または認証チェーン）の作成はサポートしていません**。

認証器が有効化されていない場合、すべてのOCPPクライアントのログインが許可されます。

:::
