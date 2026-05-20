# OCPPゲートウェイ

[OCPP](https://www.openchargealliance.org/)（Open Charge Point Protocol）は、充電ステーションと中央管理システムを接続するためのオープンな通信プロトコルであり、電気自動車充電インフラ向けの統一された通信標準を提供することを目的としています。OCPPゲートウェイはプロトコルの変換装置として機能し、OCPPとMQTTプロトコル間の橋渡しを行うことで、これらのプロトコルを使用するクライアント同士の通信を可能にします。

EMQXは[OCPP 1.6-J](https://openchargealliance.org/protocols/open-charge-point-protocol/#OCPP1.6)に対応したプロトコルゲートウェイを追加しており、OCPP仕様に準拠したさまざまなブランドの充電ステーション機器と接続可能です。ルールエンジン、データ統合、REST APIなどを通じて管理システム（Central System）と連携し、ユーザーが迅速に電気自動車充電インフラを構築できるよう支援します。

本ページでは、EMQXにおけるOCPPゲートウェイの設定方法と利用方法を紹介します。

## OCPPゲートウェイの有効化

EMQXのOCPPゲートウェイは、ダッシュボード、REST API、設定ファイル`base.hocon`から設定および有効化が可能です。本節ではダッシュボードを用いた設定手順を例に説明します。

EMQXダッシュボードの左側ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways**ページにはサポートされているすべてのゲートウェイが一覧表示されます。**OCPP**を探し、**Actions**列の**Setup**をクリックすると、**Initialize OCPP**ページに遷移します。

::: tip

EMQXをクラスターで運用している場合、ダッシュボードやREST APIで行った設定はクラスター全体に影響します。特定のノードだけ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md)で設定してください。

:::

設定を簡略化するため、EMQXは**Gateways**ページのすべての必須項目にデフォルト値を用意しています。大幅なカスタマイズが不要な場合は、以下の3クリックでOCPPゲートウェイを有効化できます。

1. **Basic Configuration**タブで**Next**をクリックし、すべてのデフォルト設定を受け入れます。
2. 次に**Listeners**タブに遷移し、EMQXがポート`33033`でWebsocketリスナーを事前設定しています。再度**Next**をクリックして設定を確認します。
3. 最後に**Enable**ボタンをクリックしてOCPPゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways**ページに戻り、OCPPゲートウェイのステータスが**Enabled**と表示されます。

<img src="./assets/ocpp-enabled.png" alt="OCPPゲートウェイが有効化された状態" style="zoom:50%;" />

上記の設定はREST APIでも可能です。

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

OCPPゲートウェイが稼働したら、OCPPクライアントツールを使って接続テストや動作確認が可能です。

ここでは実例として[ocpp-go](https://github.com/lorenzodonini/ocpp-go)を用い、EMQXのOCPPゲートウェイへの接続方法を紹介します。

1. まず、OCPPゲートウェイと連携するためのMQTTクライアントを準備します。例えば[MQTTX](https://mqttx.app/downloads)を使い、EMQXに接続してトピック`ocpp/#`をサブスクライブする設定を行います。

   <img src="./assets/ocpp-mqttx-create-conn.png" alt="MQTT接続の作成" style="zoom:67%;" />

2. ocpp-goクライアントを起動し、OCPPゲートウェイに接続します。

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

3. MQTTXで以下のようなメッセージを受信することを確認します。

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

   これはocpp-goクライアントがOCPPゲートウェイに接続し、`BootNotification`リクエストを送信したことを示しています。

4. MQTTXでトピック`ocpp/cs/chargePointSim`に対して以下の内容のメッセージを作成し、送信します。

   **注意**：`UniqueId`は前のメッセージで受信したものに置き換えてください。

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

5. その後、MQTTXは`StatusNotification`のステータスレポートを受信します。これはOCPPクライアントがOCPPゲートウェイとの接続を正常に確立したことを示しています。

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

デフォルト設定に加え、EMQXはさまざまな設定項目を提供しており、ビジネス要件に応じた柔軟な調整が可能です。本節では**Gateways**ページで設定可能な各項目を詳しく解説します。

### 基本設定

**Gateways**ページのOCPPゲートウェイの**Actions**列にある**Settings**ボタンをクリックすると、**Basic Configuration**タブで以下の項目を設定できます。

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離に利用可能です。例：`ocpp/`
- **Default Heartbeat Interval**：デフォルトのハートビート間隔（秒）、デフォルトは`60s`
- **Heartbeat Checking Times Backoff**：ハートビートチェックのバックオフ回数、デフォルトは`1`
- **Message Format Checking**：メッセージフォーマットの妥当性チェックを有効にするかどうか。EMQXはアップロードおよびダウンロードストリームのメッセージをjson-schemaで定義されたフォーマットに対して検証し、チェックに失敗した場合は対応する応答メッセージを返します。設定可能な値は以下の通りです。

    - `disable`：メッセージのチェックを行わない（デフォルト）
    - `upstream_only`：アップロードストリームのメッセージのみチェック
    - `dnstream_only`：ダウンロードストリームのメッセージのみチェック
    - `all`：すべてのメッセージをチェック
- **JSON Schema Directory**：OCPPメッセージ定義のJSONスキーマディレクトリ、デフォルトは`${application}/priv/schemas`
- **JSON Schema ID Prefix**：OCPPメッセージスキーマのIDプレフィックス、デフォルトは`urn:OCPP:1.6:2019:12:`
- **Idle Timeout**：非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）
- **Upstream**：アップロードストリームの設定グループ
    - **Topic**：アップロードストリームのCall Requestメッセージ用トピック、デフォルトは`cp/${cid}`
    - **Reply Topic**：アップロードストリームのReplyメッセージ用トピック、デフォルトは`cp/${cid}/Reply`
    - **Error Topic**：アップロードストリームのErrorメッセージ用トピック、デフォルトは`cp/${cid}/Reply`
    - **Topic Override Mapping**：メッセージ名ごとのアップロードストリームトピックのオーバーライドマッピング
- **Downstream**：ダウンロードストリームの設定グループ
    - **Topic**：EMQXからリクエストや制御メッセージを受信するためのダウンロードストリームトピック。すべての接続されたチャージポイントがサブスクライブするワイルドカードトピック名。デフォルトは`cs/${cid}`
    - **Max Message Queue Length**：ダウンロードストリームのメッセージ配信における最大メッセージキュー長、デフォルトは`100`

### リスナーの追加

ポート`33033`で名前が**default**のWebsocketリスナーが既に設定されており、最大16のアセプターをプールし、最大1,024,000の同時接続をサポートしています。**Settings**をクリックすると詳細設定が可能で、**Delete**でリスナーを削除、**+ Add Listener**で新規リスナーを追加できます。

::: tip

OCPPゲートウェイはWebsocketおよびTLS上のWebsocketタイプのリスナーのみをサポートしています。

:::

**Add Listener**をクリックすると**Add Listener**ページが開き、以下の設定が行えます。

**基本設定**

- **Name**：リスナーの一意識別子を設定
- **Type**：プロトコルタイプを選択。OCPPでは`ws`または`wss`が選択可能
- **Bind**：リスナーが接続を受け入れるポート番号を設定
- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティング分離を実現

**リスナー設定**

- **Path**：接続アドレスのパスプレフィックスを設定。クライアントは接続時にこのパス全体を指定する必要があり、デフォルトは`/ocpp`
- **Acceptor**：アセプタープールのサイズを設定、デフォルトは`16`
- **Max Connections**：リスナーが処理可能な最大同時接続数、デフォルトは`1024000`
- **Max Connection Rate**：リスナーが1秒あたりに受け入れ可能な新規接続の最大レート、デフォルトは`1000`
- **Proxy Protocol**：EMQXが[ロードバランサー](../deploy/cluster/lb.md)の背後にある場合に、プロトコルV1/V2を有効化
- **Proxy Protocol Timeout**：プロキシプロトコルパッケージを待機する最大時間（秒）、非アクティブの場合は接続を切断、デフォルトは`3s`

**TCP設定**

- **ActiveN**：ソケットの`{active, N}`オプションを設定。ソケットが積極的に処理可能な受信パケット数。詳細は[Erlangドキュメント - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2)参照
- **Buffer**：受信および送信パケットを格納するバッファサイズ（KB単位）を設定
- **TCP_NODELAY**：`TCP_NODELAY`フラグを有効にするかどうか。クライアントが前のデータのアックを待たずに追加データを送信するかの設定。デフォルトは`false`、選択肢は`true`または`false`
- **SO_REUSEADDR**：ローカルでのポート番号再利用を許可するかどうか
- **Send Timeout**：送信タイムアウトまでの最大待機時間（秒）。非アクティブの場合は接続を切断、デフォルトは`15s`
- **Send Timeout Close**：送信タイムアウト時に接続を切断するかどうか

**SSL設定**（wssリスナーのみ）

TLS検証の有効化はトグルスイッチで設定可能ですが、その前に関連する**TLS Cert**、**TLS Key**、**CA Cert**情報をファイルの内容入力または**Select File**ボタンでアップロードして設定する必要があります。詳細は[SSL/TLS接続の有効化](../network/emqx-mqtt-tls.md)を参照してください。

続いて以下の設定が可能です。

- **SSL Versions**：サポートするSSLバージョンを設定。デフォルトは`tlsv1.3`、`tlsv1.2`、`tlsv1.1`、`tlsv1`
- **Fail If No Peer Cert**：クライアントが空の証明書を送信した場合に接続を拒否するかどうか。デフォルトは`false`、選択肢は`true`または`false`
- **Intermediate Certificate Depth**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数、デフォルトは`10`
- **Key Password**：プライベートキーがパスワード保護されている場合のパスワード設定

## 認証の設定

OCPPプロトコルの接続メッセージにはユーザー名とパスワードの概念が既に定義されているため、OCPPは以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../access-control/authn/mnesia.md)
- [MySQL認証](../access-control/authn/mysql.md)
- [MongoDB認証](../access-control/authn/mongodb.md)
- [PostgreSQL認証](../access-control/authn/postgresql.md)
- [Redis認証](../access-control/authn/redis.md)
- [HTTPサーバー認証](../access-control/authn/http.md)
- [JWT認証](../access-control/authn/jwt.md)
- [LDAP認証](../access-control/authn/ldap.md)

OCPPゲートウェイはWebsocketハンドシェイクメッセージのBasic認証情報を用いて、クライアントの認証フィールドを生成します。

- クライアントID：固定パスプレフィックスの後の接続アドレス部分の値
- ユーザー名：Basic認証のUsernameの値
- パスワード：Basic認証のPasswordの値

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

MQTTプロトコルとは異なり、**ゲートウェイでは認証器の作成のみをサポートし、認証器のリスト（または認証チェーン）の作成はサポートしていません**。

認証器が有効化されていない場合、すべてのOCPPクライアントのログインが許可されます。

:::
