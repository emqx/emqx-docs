# OCPP ゲートウェイ

<<<<<<< HEAD
[OCPP](https://www.openchargealliance.org/)（Open Charge Point Protocol）は、充電ステーションと中央管理システムを接続するためのオープンな通信プロトコルであり、電気自動車充電インフラ向けの統一された通信標準を提供することを目的としています。OCPPゲートウェイはプロトコルの変換装置として機能し、OCPPとMQTTプロトコル間の橋渡しを行うことで、これらのプロトコルを使用するクライアント同士の通信を可能にします。

EMQXは[OCPP 1.6-J](https://openchargealliance.org/protocols/open-charge-point-protocol/#OCPP1.6)に対応したプロトコルゲートウェイを追加しており、OCPP仕様に準拠したさまざまなブランドの充電ステーション機器と接続可能です。ルールエンジン、データ統合、REST APIなどを通じて管理システム（Central System）と連携し、ユーザーが迅速に電気自動車充電インフラを構築できるよう支援します。

本ページでは、EMQXにおけるOCPPゲートウェイの設定方法と利用方法を紹介します。
=======
[OCPP](https://www.openchargealliance.org/)（Open Charge Point Protocol）は、充電ステーションと中央管理システムを接続するためのオープンな通信プロトコルであり、電気自動車充電インフラ向けの統一通信標準の提供を目的としています。OCPP ゲートウェイはプロトコル変換器として機能し、OCPP と MQTT プロトコル間の橋渡しを行うことで、これらのプロトコルを使用するクライアント同士の通信を可能にします。

EMQX は [OCPP 1.6-J](https://openchargealliance.org/protocols/open-charge-point-protocol/#OCPP1.6) に対応したプロトコルゲートウェイを追加しており、OCPP 仕様に準拠した様々なブランドの充電ステーション機器と接続可能です。ルールエンジン、データ統合、REST API などを通じて管理システム（Central System）と連携し、ユーザーが迅速に電気自動車充電インフラを構築できるよう支援します。

本ページでは、EMQX における OCPP ゲートウェイの設定および利用方法を紹介します。
>>>>>>> origin/release-6.1

## OCPP ゲートウェイの有効化

<<<<<<< HEAD
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
=======
EMQX の OCPP ゲートウェイは、ダッシュボード、REST API、設定ファイル `base.hocon` を通じて設定および有効化が可能です。本節ではダッシュボードによる設定例を用いて操作手順を説明します。

EMQX ダッシュボードの左ナビゲーションメニューで **管理** -> **ゲートウェイ** をクリックします。**ゲートウェイ** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**OCPP** を探し、**操作** 列の **設定** をクリックすると、**OCPP 初期化** ページに遷移します。

::: tip

EMQX をクラスターで運用している場合、ダッシュボードや REST API で行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md) で設定してください。

:::

設定を簡略化するために、EMQX は **ゲートウェイ** ページのすべての必須項目にデフォルト値を用意しています。大幅なカスタマイズが不要であれば、以下の3ステップで OCPP ゲートウェイを有効化できます。

1. **基本設定** タブで **次へ** をクリックし、すべてのデフォルト設定を受け入れます。
2. 続いて表示される **リスナー** タブでは、EMQX がポート `33033` で Websocket リスナーを事前設定しています。ここでも **次へ** をクリックして設定を確定します。
3. 最後に **有効化** ボタンをクリックして OCPP ゲートウェイを起動します。

ゲートウェイの有効化が完了すると、**ゲートウェイ** ページに戻り、OCPP ゲートウェイのステータスが **有効** と表示されます。
>>>>>>> origin/release-6.1

<img src="./assets/ocpp-enabled.png" alt="OCPP ゲートウェイが有効化された状態" style="zoom:50%;" />

上記の設定は REST API でも可能です。

**実行例:**

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

## OCPP クライアントとの連携

OCPP ゲートウェイが稼働したら、OCPP クライアントツールを使って接続テストや動作確認を行えます。

<<<<<<< HEAD
ここでは実例として[ocpp-go](https://github.com/lorenzodonini/ocpp-go)を用い、EMQXのOCPPゲートウェイへの接続方法を紹介します。

1. まず、OCPPゲートウェイと連携するためのMQTTクライアントを準備します。例えば[MQTTX](https://mqttx.app/downloads)を使い、EMQXに接続してトピック`ocpp/#`をサブスクライブする設定を行います。
=======
ここでは実例として [ocpp-go](https://github.com/lorenzodonini/ocpp-go) を用い、EMQX の OCPP ゲートウェイに接続する方法を紹介します。

1. まず、OCPP ゲートウェイと連携する MQTT クライアントを準備します。例えば [MQTTX](https://mqttx.app/downloads) を使い、EMQX に接続してトピック `ocpp/#` をサブスクライブする設定を行います。
>>>>>>> origin/release-6.1

   <img src="./assets/ocpp-mqttx-create-conn.png" alt="MQTT 接続の作成" style="zoom:67%;" />

2. ocpp-go クライアントを起動し、OCPP ゲートウェイに接続します。

<<<<<<< HEAD
   **注意**：以下のコマンド内の`<host>`はEMQXサーバーのアドレスに置き換えてください。
=======
   **注意**：以下のコマンド内の `<host>` は EMQX サーバーのアドレスに置き換えてください。
>>>>>>> origin/release-6.1

   ```shell
   docker run -e CLIENT_ID=chargePointSim -e CENTRAL_SYSTEM_URL=ws://<host>:33033/ocpp -it --rm --name charge-point ldonini/ocpp1.6-charge-point:latest
   ```

   接続成功時には以下のようなログが出力されます。

   ```css
   INFO[2023-12-01T03:08:39Z] connecting to server logger=websocket
   INFO[2023-12-01T03:08:39Z] connected to server as chargePointSim logger=websocket
   INFO[2023-12-01T03:08:39Z] connected to central system at ws://172.31.1.103:33033/ocpp
   INFO[2023-12-01T03:08:39Z] dispatched request 1200012677 to server logger=ocppj
   ```

3. MQTTX で以下のようなメッセージを受信することを確認します。

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

<<<<<<< HEAD
   これはocpp-goクライアントがOCPPゲートウェイに接続し、`BootNotification`リクエストを送信したことを示しています。

4. MQTTXでトピック`ocpp/cs/chargePointSim`に対して以下の内容のメッセージを作成し、送信します。

   **注意**：`UniqueId`は前のメッセージで受信したものに置き換えてください。
=======
   このメッセージは ocpp-go クライアントが OCPP ゲートウェイに接続し、`BootNotification` リクエストを送信したことを示します。

4. MQTTX でトピック `ocpp/cs/chargePointSim` に対し、以下の内容のメッセージを作成して送信します。

   **注意**：`UniqueId` は前のメッセージで受信したものに置き換えてください。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
5. その後、MQTTXは`StatusNotification`のステータスレポートを受信します。これはOCPPクライアントがOCPPゲートウェイとの接続を正常に確立したことを示しています。
=======
5. その後、MQTTX は `StatusNotification` ステータスレポートを受信します。これは OCPP クライアントが正常に OCPP ゲートウェイと接続を確立したことを示しています。
>>>>>>> origin/release-6.1

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

## OCPP ゲートウェイのカスタマイズ

<<<<<<< HEAD
デフォルト設定に加え、EMQXはさまざまな設定項目を提供しており、ビジネス要件に応じた柔軟な調整が可能です。本節では**Gateways**ページで設定可能な各項目を詳しく解説します。

### 基本設定

**Gateways**ページのOCPPゲートウェイの**Actions**列にある**Settings**ボタンをクリックすると、**Basic Configuration**タブで以下の項目を設定できます。

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離に利用可能です。例：`ocpp/`
- **Default Heartbeat Interval**：デフォルトのハートビート間隔（秒）、デフォルトは`60s`
- **Heartbeat Checking Times Backoff**：ハートビートチェックのバックオフ回数、デフォルトは`1`
- **Message Format Checking**：メッセージフォーマットの妥当性チェックを有効にするかどうか。EMQXはアップロードおよびダウンロードストリームのメッセージをjson-schemaで定義されたフォーマットに対して検証し、チェックに失敗した場合は対応する応答メッセージを返します。設定可能な値は以下の通りです。
=======
デフォルト設定に加え、EMQX はさまざまな設定オプションを提供しており、特定のビジネス要件に合わせた調整が可能です。本節では **ゲートウェイ** ページで設定可能な各項目について詳しく解説します。

### 基本設定

ゲートウェイページの OCPP ゲートウェイの **操作** 列にある **設定** ボタンをクリックすると、**基本設定** タブで以下の項目を設定できます。

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付与される文字列です。異なるプロトコル間でメッセージルーティングの分離を実現するために利用します。例：`ocpp/`
- **デフォルトハートビート間隔**：ハートビートのデフォルト間隔、デフォルト値は `60s`
- **ハートビートチェック回数のバックオフ**：ハートビートチェック回数のバックオフ値、デフォルトは `1`
- **メッセージフォーマットチェック**：メッセージのフォーマットの妥当性チェックを有効にするかどうか。EMQX はアップロードストリームおよびダウンロードストリームのメッセージフォーマットを json-schema で定義された形式に対して検証します。チェックに失敗した場合、EMQX は対応する応答メッセージを返します。設定可能な値は以下の通りです。
>>>>>>> origin/release-6.1

    - `disable`：メッセージのチェックを行わない（デフォルト）
    - `upstream_only`：アップロードストリームのメッセージのみチェック
    - `dnstream_only`：ダウンロードストリームのメッセージのみチェック
    - `all`：すべてのメッセージをチェック
<<<<<<< HEAD
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
=======
- **JSON スキーマディレクトリ**：OCPP メッセージ定義の JSON スキーマディレクトリ、デフォルトは `${application}/priv/schemas`
- **JSON スキーマ ID プレフィックス**：OCPP メッセージスキーマの ID プレフィックス、デフォルトは `urn:OCPP:1.6:2019:12:`
- **アイドルタイムアウト**：非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）
- **アップストリーム**：アップロードストリームの設定グループ
    - **トピック**：アップロードストリームの Call Request メッセージ用トピック、デフォルトは `cp/${cid}`
    - **返信トピック**：アップロードストリームの返信メッセージ用トピック、デフォルトは `cp/${cid}/Reply`
    - **エラートピック**：アップロードストリームのエラーメッセージ用トピック、デフォルトは `cp/${cid}/Reply`
    - **トピックオーバーライドマッピング**：メッセージ名によるアップロードストリームのトピックオーバーライドマッピング
- **ダウンストリーム**：ダウンロードストリームの設定グループ
    - **トピック**：EMQX からリクエストや制御メッセージを受信するためのダウンロードストリームトピック。これは接続されたすべてのチャージポイントがサブスクライブするワイルドカードトピック名です。デフォルトは `cs/${cid}`
    - **最大メッセージキュー長**：ダウンロードストリームのメッセージ配信における最大メッセージキュー長、デフォルトは `100`

### リスナーの追加

ポート `33033` で名前が **default** の Websocket リスナーがすでに設定されており、プール内の最大アセプター数は16、最大同時接続数は1,024,000です。より詳細な設定を行う場合は **設定** をクリックし、リスナーを削除したい場合は **削除** をクリック、新規リスナーを追加する場合は **+ リスナー追加** をクリックしてください。

::: tip

OCPP ゲートウェイは Websocket および TLS 上の Websocket タイプのリスナーのみサポートしています。

:::

**リスナー追加** ページでは以下の設定項目を指定できます。

**基本設定**

- **名前**：リスナーの一意識別子を設定します。
- **タイプ**：プロトコルタイプを選択します。OCPP では `ws` または `wss` が選択可能です。
- **バインド**：リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付与される文字列で、異なるプロトコル間でのメッセージルーティング分離に利用します。

**リスナー設定**

- **パス**：接続アドレスのパスプレフィックスを設定します。クライアントは接続時にこの完全なアドレスを指定する必要があります。デフォルトは `/ocpp`
- **アセプター**：アセプタープールのサイズを設定します。デフォルトは `16`
- **最大接続数**：リスナーが処理可能な同時接続の最大数を設定します。デフォルトは `1024000`
- **最大接続レート**：リスナーが1秒あたりに受け入れる新規接続の最大レートを設定します。デフォルトは `1000`
- **プロキシプロトコル**：EMQX が [ロードバランサー](../deploy/cluster/lb.md) の背後にある場合に、プロトコル V1/V2 を有効化します。
- **プロキシプロトコルタイムアウト**：非アクティブ状態でプロキシプロトコルパッケージを待機する最大時間（秒）を設定し、デフォルトは `3s`
>>>>>>> origin/release-6.1

**TCP 設定**

<<<<<<< HEAD
- **ActiveN**：ソケットの`{active, N}`オプションを設定。ソケットが積極的に処理可能な受信パケット数。詳細は[Erlangドキュメント - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2)参照
- **Buffer**：受信および送信パケットを格納するバッファサイズ（KB単位）を設定
- **TCP_NODELAY**：`TCP_NODELAY`フラグを有効にするかどうか。クライアントが前のデータのアックを待たずに追加データを送信するかの設定。デフォルトは`false`、選択肢は`true`または`false`
- **SO_REUSEADDR**：ローカルでのポート番号再利用を許可するかどうか
- **Send Timeout**：送信タイムアウトまでの最大待機時間（秒）。非アクティブの場合は接続を切断、デフォルトは`15s`
- **Send Timeout Close**：送信タイムアウト時に接続を切断するかどうか
=======
- **ActiveN**：ソケットの `{active, N}` オプションを設定します。これはソケットが積極的に処理可能な受信パケット数を示します。詳細は [Erlang ドキュメント - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2) を参照してください。
- **バッファ**：受信および送信パケットを格納するバッファサイズを KB 単位で設定します。
- **TCP_NODELAY**：`TCP_NODELAY` フラグを有効化するかどうかを設定します。これはクライアントが前のデータのアックを待たずに追加データを送信できるかどうかを制御します。デフォルトは `false`、選択肢は `true` または `false`
- **SO_REUSEADDR**：ローカルのポート番号再利用を許可するかどうかを設定します。
- **送信タイムアウト**：非アクティブ状態で送信タイムアウトが発生するまでの最大待機時間（秒）、デフォルトは `15s`
- **送信タイムアウト時の切断**：送信タイムアウト時に接続を切断するかどうかを設定します。
>>>>>>> origin/release-6.1

**SSL 設定**（wss リスナーのみ）

<<<<<<< HEAD
TLS検証の有効化はトグルスイッチで設定可能ですが、その前に関連する**TLS Cert**、**TLS Key**、**CA Cert**情報をファイルの内容入力または**Select File**ボタンでアップロードして設定する必要があります。詳細は[SSL/TLS接続の有効化](../network/emqx-mqtt-tls.md)を参照してください。

続いて以下の設定が可能です。

- **SSL Versions**：サポートするSSLバージョンを設定。デフォルトは`tlsv1.3`、`tlsv1.2`、`tlsv1.1`、`tlsv1`
- **Fail If No Peer Cert**：クライアントが空の証明書を送信した場合に接続を拒否するかどうか。デフォルトは`false`、選択肢は`true`または`false`
- **Intermediate Certificate Depth**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数、デフォルトは`10`
- **Key Password**：プライベートキーがパスワード保護されている場合のパスワード設定

## 認証の設定

OCPPプロトコルの接続メッセージにはユーザー名とパスワードの概念が既に定義されているため、OCPPは以下のような多様な認証方式をサポートしています。
=======
TLS 検証の有効化はトグルスイッチで設定可能ですが、その前に関連する **TLS 証明書**、**TLS キー**、**CA 証明書** の情報をファイル内容の入力または **ファイル選択** ボタンからアップロードして設定する必要があります。詳細は [SSL/TLS 接続の有効化](../network/emqx-mqtt-tls.md) を参照してください。

続いて以下の設定が可能です。

- **SSL バージョン**：サポートする SSL バージョンを設定します。デフォルトは `tlsv1.3`、`tlsv1.2`、`tlsv1.1`、`tlsv1`
- **ピア証明書なしの場合の拒否**：クライアントが空の証明書を送信した場合に接続を拒否するかどうかを設定します。デフォルトは `false`、選択肢は `true` または `false`
- **中間証明書の深さ**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは `10`
- **キーのパスワード**：秘密鍵がパスワード保護されている場合に使用するパスワードを設定します。

## 認証の設定

OCPP プロトコルの接続メッセージにはすでにユーザー名とパスワードの概念が定義されているため、OCPP は以下のような多様な認証方式をサポートしています。
>>>>>>> origin/release-6.1

- [組み込みデータベース認証](../access-control/authn/mnesia.md)
- [MySQL 認証](../access-control/authn/mysql.md)
- [MongoDB 認証](../access-control/authn/mongodb.md)
- [PostgreSQL 認証](../access-control/authn/postgresql.md)
- [Redis 認証](../access-control/authn/redis.md)
- [HTTP サーバー認証](../access-control/authn/http.md)
- [JWT 認証](../access-control/authn/jwt.md)
- [LDAP 認証](../access-control/authn/ldap.md)

<<<<<<< HEAD
OCPPゲートウェイはWebsocketハンドシェイクメッセージのBasic認証情報を用いて、クライアントの認証フィールドを生成します。

- クライアントID：固定パスプレフィックスの後の接続アドレス部分の値
- ユーザー名：Basic認証のUsernameの値
- パスワード：Basic認証のPasswordの値

REST APIを使ってOCPPゲートウェイ用の組み込みデータベース認証を作成することも可能です。
=======
OCPP ゲートウェイは Websocket ハンドシェイクメッセージの Basic Authentication 情報を利用してクライアントの認証フィールドを生成します。

- クライアント ID：固定パスプレフィックス以降の接続アドレス部分の値
- ユーザー名：Basic Authentication のユーザー名
- パスワード：Basic Authentication のパスワード

REST API を使って OCPP ゲートウェイ用の組み込みデータベース認証を作成することも可能です。
>>>>>>> origin/release-6.1

**実行例:**

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

<<<<<<< HEAD
MQTTプロトコルとは異なり、**ゲートウェイでは認証器の作成のみをサポートし、認証器のリスト（または認証チェーン）の作成はサポートしていません**。

認証器が有効化されていない場合、すべてのOCPPクライアントのログインが許可されます。
=======
MQTT プロトコルとは異なり、**ゲートウェイは認証方式の作成のみをサポートし、認証方式の一覧（または認証チェーン）の作成はサポートしていません**。

認証方式が有効化されていない場合、すべての OCPP クライアントのログインが許可されます。
>>>>>>> origin/release-6.1

:::
