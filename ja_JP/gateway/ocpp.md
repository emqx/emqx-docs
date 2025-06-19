# OCPPゲートウェイ

[OCPP](https://www.openchargealliance.org/)（Open Charge Point Protocol）は、充電ステーションと中央管理システムを接続するためのオープンな通信プロトコルであり、電気自動車充電インフラの統一された通信標準を提供することを目的としています。OCPPゲートウェイはプロトコル変換器として機能し、OCPPとMQTTプロトコル間の橋渡しを行い、これらのプロトコルを使用するクライアント同士の通信を可能にします。

EMQXは[OCPP 1.6-J](https://www.openchargealliance.org/protocols/ocpp-16/)向けのプロトコルゲートウェイを追加しており、OCPP仕様に準拠したさまざまなブランドの充電設備と接続可能です。ルールエンジン、データ統合、REST APIなどを通じて管理システム（Central System）と連携し、ユーザーが迅速に電気自動車充電インフラを構築できるよう支援します。

本ページでは、EMQXにおけるOCPPゲートウェイの設定および利用方法を紹介します。

## OCPPゲートウェイの有効化

EMQXのOCPPゲートウェイは、ダッシュボード、REST API、設定ファイル `base.hocon` を通じて設定および有効化できます。本節ではダッシュボードを用いた設定手順を例に説明します。

EMQXダッシュボードの左側ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**OCPP** を探し、**Actions** 列の **Setup** をクリックすると、**Initialize OCPP** ページに遷移します。

::: tip

EMQXをクラスターで運用している場合、ダッシュボードやREST APIで行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md)で設定してください。

:::

設定を簡略化するため、EMQXは**Gateways**ページの必須項目に対してすべてデフォルト値を用意しています。大幅なカスタマイズが不要な場合、以下の3クリックでOCPPゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. 続いて表示される **Listeners** タブでは、EMQXがポート `33033` でWebsocketリスナーを事前設定しています。再度 **Next** をクリックして設定を確定します。
3. 最後に **Enable** ボタンをクリックしてOCPPゲートウェイを起動します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、OCPPゲートウェイの状態が **Enabled** と表示されます。

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

ここでは実例として [ocpp-go](https://github.com/lorenzodonini/ocpp-go) を用い、EMQXのOCPPゲートウェイへの接続方法を紹介します。

1. まず、OCPPゲートウェイと連携するMQTTクライアントを準備します。例として [MQTTX](https://mqttx.app/downloads) を使い、EMQXに接続してトピック `ocpp/#` をサブスクライブする設定を行います。

   <img src="./assets/ocpp-mqttx-create-conn.png" alt="MQTT接続の作成" style="zoom:67%;" />

2. ocpp-goクライアントを起動し、OCPPゲートウェイに接続します。

   **注意**：以下のコマンド中の `<host>` はEMQXサーバーのアドレスに置き換えてください。

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

   このメッセージはocpp-goクライアントがOCPPゲートウェイに接続し、`BootNotification` リクエストを送信したことを示します。

4. MQTTXでトピック `ocpp/cs/chargePointSim` に対し、以下の内容のメッセージを作成して送信します。

   **注意**：`UniqueId` は前のメッセージで受信した値に置き換えてください。

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

5. その後、MQTTXで `StatusNotification` のステータスレポートを受信します。これはOCPPクライアントがOCPPゲートウェイとの接続を正常に確立したことを示します。

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

デフォルト設定に加え、EMQXはさまざまな設定オプションを提供しており、ビジネス要件に応じた柔軟なカスタマイズが可能です。本節では**Gateways**ページで設定可能な各項目の詳細を解説します。

### 基本設定

GatewaysページのOCPPゲートウェイの**Actions**列にある**Settings**ボタンをクリックすると、**Basic Configuration**タブで以下の項目を設定できます。

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離に利用可能です。例：`ocpp/`
- **Default Heartbeat Interval**：デフォルトのハートビート間隔（秒）、初期値：`60s`
- **Heartbeat Checking Times Backoff**：ハートビートチェックのバックオフ回数、初期値：`1`
- **Message Format Checking**：メッセージフォーマットの妥当性チェックを有効にするかどうか。EMQXはアップロード／ダウンロードストリームのメッセージをjson-schemaで定義されたフォーマットと照合し、チェックに失敗した場合は対応する応答メッセージを返します。設定可能な値は以下の通りです。

    - `all`：すべてのメッセージをチェック
    - `upstream_only`：アップロードストリームのメッセージのみチェック
    - `dnstream_only`：ダウンロードストリームのメッセージのみチェック
    - `disable`：チェックを行わない
- **JSON Schema Directory**：OCPPメッセージ定義のJSONスキーマディレクトリ、初期値：`${application}/priv/schemas`
- **JSON Schema ID Prefix**：OCPPメッセージスキーマのIDプレフィックス、初期値：`urn:OCPP:1.6:2019:12:`
- **Idle Timeout**：非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）
- **Upstream**：アップロードストリームの設定グループ
    - **Topic**：アップロードストリームのCall Requestメッセージ用トピック、初期値：`cp/${cid}`
    - **Reply Topic**：アップロードストリームのReplyメッセージ用トピック、初期値：`cp/${cid}/Reply`
    - **Error Topic**：アップロードストリームのErrorメッセージ用トピック、初期値：`cp/${cid}/Reply`
    - **Topic Override Mapping**：メッセージ名によるアップロードストリームのトピック上書きマッピング
- **Downstream**：ダウンロードストリームの設定グループ
    - **Topic**：EMQXからのリクエスト／制御メッセージを受信するダウンロードストリームのトピック。すべての接続中のチャージポイントがサブスクライブするワイルドカードトピック名。初期値：`cs/${cid}`
    - **Max Message Queue Length**：ダウンロードストリームのメッセージ配信における最大メッセージキュー長、初期値：`100`

### リスナーの追加

ポート `33033` に **default** という名前のWebsocketリスナーがすでに設定されており、最大16のアクセプターをプールし、最大1,024,000の同時接続をサポートしています。リスナーの**Settings**をクリックすると詳細設定が可能で、**Delete**で削除、**+ Add Listener**で新規リスナーの追加ができます。

::: tip

OCPPゲートウェイはWebsocketおよびTLS上のWebsocketタイプのリスナーのみサポートしています。

:::

**Add Listener** をクリックすると設定画面が開き、以下の項目を設定できます。

**基本設定**

- **Name**：リスナーの一意識別子を設定
- **Type**：プロトコルタイプを選択。OCPPでは `ws` または `wss` が選択可能
- **Bind**：リスナーが接続を受け付けるポート番号を設定
- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティング分離に利用可能

**リスナー設定**

- **Path**：接続アドレスのパスプレフィックスを設定。クライアントは接続時にこの完全なアドレスを使用する必要があり、デフォルトは `/ocpp`
- **Acceptor**：アクセプタープールのサイズを設定、初期値：`16`
- **Max Connections**：リスナーが処理可能な最大同時接続数、初期値：`1024000`
- **Max Connection Rate**：リスナーが1秒あたり受け入れ可能な新規接続の最大レート、初期値：`1000`
- **Proxy Protocol**：EMQXが[ロードバランサー](../deploy/cluster/lb.md)の背後にある場合にプロトコルV1/2を有効化
- **Proxy Protocol Timeout**：プロキシプロトコルパッケージ待機の最大時間（秒）、非アクティブ時に接続を切断、初期値：`3s`

**TCP設定**

- **ActiveN**：ソケットの `{active, N}` オプションを設定。ソケットが積極的に処理可能な受信パケット数。詳細は[Erlangドキュメント - setopts/2](https://erlang.org/doc/man/inet.html#setopts-2)参照。
- **Buffer**：受信および送信パケットを格納するバッファサイズ（KB単位）
- **TCP_NODELAY**：`TCP_NODELAY` フラグの有効化設定。クライアントが前回のデータのアックを待たずに追加データを送信するかどうか。初期値：`false`、選択肢：`true`、`false`
- **SO_REUSEADDR**：ローカルポート番号の再利用を許可するかどうか
- **Send Timeout**：送信タイムアウトの最大時間（秒）、非アクティブ時に接続を切断、初期値：`15s`
- **Send Timeout Close**：送信タイムアウト時に接続を切断するかどうか

**SSL設定**（wssリスナーのみ）

TLS検証の有効化はトグルスイッチで設定可能ですが、その前に関連する **TLS Cert**、**TLS Key**、**CA Cert** の情報をファイル内容の入力または **Select File** ボタンでアップロードして設定する必要があります。詳細は[SSL/TLS接続の有効化](../network/emqx-mqtt-tls.md)を参照してください。

続いて以下を設定できます。

- **SSL Versions**：サポートするSSLバージョンを設定。初期値は `tlsv1.3`、`tlsv1.2`、`tlsv1.1`、`tlsv1`
- **Fail If No Peer Cert**：クライアントが空の証明書を送信した場合に接続を拒否するかどうか。初期値：`false`、選択肢：`true`、`false`
- **Intermediate Certificate Depth**：ピア証明書に続く有効な認証パスに含まれる非自己発行中間証明書の最大数、初期値：`10`
- **Key Password**：秘密鍵がパスワード保護されている場合に使用するパスワード

## 認証の設定

OCPPプロトコルの接続メッセージにはすでにユーザー名とパスワードの概念があるため、OCPPは以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../access-control/authn/mnesia.md)
- [MySQL認証](../access-control/authn/mysql.md)
- [MongoDB認証](../access-control/authn/mongodb.md)
- [PostgreSQL認証](../access-control/authn/postgresql.md)
- [Redis認証](../access-control/authn/redis.md)
- [HTTPサーバー認証](../access-control/authn/http.md)
- [JWT認証](../access-control/authn/jwt.md)
- [LDAP認証](../access-control/authn/ldap.md)

OCPPゲートウェイはWebsocketハンドシェイクメッセージのBasic認証情報を用いてクライアントの認証フィールドを生成します。

- クライアントID：固定パスプレフィックス以降の接続アドレス部分の値
- ユーザー名：Basic認証のUsernameの値
- パスワード：Basic認証のPasswordの値

REST APIを使ってOCPPゲートウェイの組み込みデータベース認証を作成することも可能です。

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

MQTTプロトコルとは異なり、**ゲートウェイは認証方式の一覧（または認証チェーン）の作成には対応しておらず、認証方式の作成のみをサポートしています**。

認証方式が有効化されていない場合、すべてのOCPPクライアントのログインが許可されます。

:::
