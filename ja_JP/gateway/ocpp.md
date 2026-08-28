# OCPP ゲートウェイ

[OCPP](https://www.openchargealliance.org/)（Open Charge Point Protocol）は、充電ステーションと中央管理システムを接続するためのオープンな通信プロトコルであり、電気自動車充電インフラ向けの統一通信標準の提供を目的としています。OCPP ゲートウェイはプロトコル変換器として機能し、OCPP と MQTT プロトコル間の橋渡しを行うことで、これらのプロトコルを使用するクライアント同士の通信を可能にします。

EMQX は [OCPP 1.6-J](https://openchargealliance.org/protocols/open-charge-point-protocol/#OCPP1.6) に対応したプロトコルゲートウェイを追加しており、OCPP 仕様に準拠した様々なブランドの充電設備と接続可能です。ルールエンジン、データ統合、REST API などを通じて管理システム（Central System）と連携し、ユーザーが迅速に電気自動車充電インフラを構築できるよう支援します。

本ページでは、EMQX における OCPP ゲートウェイの設定および利用方法を紹介します。

## OCPP ゲートウェイの有効化

EMQX の OCPP ゲートウェイは、ダッシュボード、REST API、設定ファイル `base.hocon` を通じて設定および有効化が可能です。本節ではダッシュボードを用いた設定例を示し、操作手順を解説します。

EMQX ダッシュボードの左側ナビゲーションメニューから **Management** → **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**OCPP** を探し、**Actions** 列の **Setup** をクリックすると、**Initialize OCPP** ページに遷移します。

::: tip

EMQX をクラスターで稼働させている場合、ダッシュボードや REST API で行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md) にて設定してください。

:::

設定を簡略化するために、EMQX は **Gateways** ページのすべての必須項目にデフォルト値を用意しています。大幅なカスタマイズが不要な場合、以下の3クリックで OCPP ゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. 次に表示される **Listeners** タブでは、EMQX がポート `33033` で Websocket リスナーを事前設定しています。再度 **Next** をクリックして設定を確定します。
3. 最後に **Enable** ボタンをクリックして OCPP ゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、OCPP ゲートウェイのステータスが **Enabled** と表示されます。

<img src="./assets/ocpp-enabled.png" alt="OCPP ゲートウェイ有効化" style="zoom:50%;" />

上記の設定は REST API でも行えます。

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

## OCPP クライアントとの連携

OCPP ゲートウェイが稼働したら、OCPP クライアントツールを使って接続テストや設定の動作確認ができます。

ここでは [ocpp-go](https://github.com/lorenzodonini/ocpp-go) を例に、EMQX の OCPP ゲートウェイへの接続方法を紹介します。

1. まず、OCPP ゲートウェイとインターフェースする MQTT クライアントを用意します。例えば [MQTTX](https://mqttx.app/downloads) を使い、EMQX に接続してトピック `ocpp/#` をサブスクライブするよう設定します。

   <img src="./assets/ocpp-mqttx-create-conn.png" alt="MQTT 接続作成" style="zoom:67%;" />

2. ocpp-go クライアントを起動し、OCPP ゲートウェイに接続します。

   **注意**: 以下のコマンド内の `<host>` は EMQX サーバーのアドレスに置き換えてください。

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

   これは ocpp-go クライアントが OCPP ゲートウェイに接続し、`BootNotification` リクエストを送信したことを示しています。

4. MQTTX でトピック `ocpp/cs/chargePointSim` に対して以下の内容のメッセージを作成し送信します。

   **注意**: `UniqueId` は前のメッセージで受信したものに置き換えてください。

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

5. その後、MQTTX は `StatusNotification` ステータスレポートを受信します。これは OCPP クライアントが OCPP ゲートウェイとの接続に成功したことを示しています。

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

デフォルト設定に加え、EMQX は多様な設定オプションを提供しており、特定のビジネス要件に合わせて調整可能です。本節では **Gateways** ページで利用できる各種項目を詳しく解説します。

### 基本設定

Gateways ページの OCPP ゲートウェイの **Actions** 列にある **Settings** ボタンをクリックすると、**Basic Configuration** タブで以下の項目を設定できます。

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

- **MountPoint**: パブリッシュやサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離を実現できます。例：`ocpp/`
- **Default Heartbeat Interval**: デフォルトのハートビート間隔（秒）、初期値は `60s`
- **Heartbeat Checking Times Backoff**: ハートビートチェックのバックオフ回数、初期値は `1`
- **Message Format Checking**: メッセージフォーマットの妥当性チェックを有効にするかどうか。EMQX はアップロードおよびダウンロードストリームのメッセージを json-schema で定義された形式と照合し、チェック失敗時には対応する応答メッセージを返します。設定値は以下のいずれかです。

    - `disable`: メッセージチェックを行わない（デフォルト）
    - `upstream_only`: アップロードストリームのみチェック
    - `dnstream_only`: ダウンロードストリームのみチェック
    - `all`: すべてのメッセージをチェック
- **JSON Schema Directory**: OCPP メッセージ定義の JSON スキーマディレクトリ、デフォルトは `${application}/priv/schemas`
- **JSON Schema ID Prefix**: OCPP メッセージスキーマの ID プレフィックス、デフォルトは `urn:OCPP:1.6:2019:12:`
- **Idle Timeout**: 非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）
- **Upstream**: アップロードストリームの設定グループ
    - **Topic**: アップロードストリームの Call Request メッセージ用トピック、デフォルトは `cp/${cid}`
    - **Reply Topic**: アップロードストリームの Reply メッセージ用トピック、デフォルトは `cp/${cid}/Reply`
    - **Error Topic**: アップロードストリームの Error メッセージ用トピック、デフォルトは `cp/${cid}/Reply`
    - **Topic Override Mapping**: メッセージ名によるアップロードストリームのトピック上書きマッピング
- **Downstream**: ダウンロードストリームの設定グループ
    - **Topic**: EMQX からのリクエストや制御メッセージを受信するダウンロードストリームのトピック。これはすべての接続されたチャージポイントがサブスクライブするワイルドカードトピック名です。デフォルトは `cs/${cid}`
    - **Max Message Queue Length**: ダウンロードストリームのメッセージ配信における最大キュー長、デフォルトは `100`

### リスナーの追加

ポート `33033` に名前が **default** の Websocket リスナーがすでに設定されており、最大16個のアセプターをプールし、最大1,024,000の同時接続をサポートします。より詳細な設定は **Settings** をクリックし、リスナーの削除は **Delete**、新規リスナーの追加は **+ Add Listener** をクリックしてください。

::: tip

OCPP ゲートウェイは Websocket および TLS 上の Websocket タイプのリスナーのみをサポートしています。

:::

**Add Listener** ページでは以下の設定が可能です。

**基本設定**

- **Name**: リスナーの一意識別子を設定
- **Type**: プロトコルタイプを選択。OCPP では `ws` または `wss` を指定可能
- **Bind**: リスナーが接続を受け付けるポート番号を設定
- **MountPoint**: パブリッシュやサブスクライブ時にトピックの先頭に付加される文字列。異なるプロトコル間のメッセージルーティング分離に利用可能

**リスナー設定**

- **Path**: 接続アドレスのパスプレフィックスを設定。クライアントはこの完全なアドレスを用いて接続する必要があります。デフォルトは `/ocpp`
- **Acceptor**: アセプタープールのサイズを設定、デフォルトは `16`
- **Max Connections**: リスナーが処理可能な最大同時接続数、デフォルトは `1024000`
- **Max Connection Rate**: リスナーが1秒あたりに受け入れ可能な新規接続の最大レート、デフォルトは `1000`
- **Proxy Protocol**: EMQX が [ロードバランサー](../deploy/cluster/lb.md) 配下にある場合にプロトコル V1/V2 を有効化
- **Proxy Protocol Timeout**: プロキシプロトコルパッケージ受信待機の最大秒数。非アクティブ時に接続を切断するまでの時間、デフォルトは `3s`

**TCP 設定**

- **ActiveN**: ソケットの `{active, N}` オプションを設定。ソケットが積極的に処理可能な受信パケット数。詳細は [Erlang ドキュメント - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2) を参照
- **Buffer**: 受信および送信パケットを格納するバッファサイズ（KB単位）
- **TCP_NODELAY**: `TCP_NODELAY` フラグを有効にするかどうか。前のデータのアックを待たずに追加データを送信するか。デフォルトは `false`、選択肢は `true` または `false`
- **SO_REUSEADDR**: ポート番号のローカル再利用を許可するかどうか
- **Send Timeout**: プロキシプロトコルパッケージ受信待機の最大秒数。非アクティブ時に接続を切断するまでの時間、デフォルトは `15s`
- **Send Timeout Close**: 送信タイムアウト時に接続を切断するかどうか

**SSL 設定**（wss リスナーのみ）

TLS 検証の有効化はトグルスイッチで設定可能です。ただし事前に関連する **TLS Cert**、**TLS Key**、**CA Cert** の情報を、ファイル内容の直接入力または **Select File** ボタンによるアップロードで設定しておく必要があります。詳細は [SSL/TLS 接続の有効化](../network/emqx-mqtt-tls.md) を参照してください。

続いて以下の設定が可能です。

- **SSL Versions**: サポートする SSL バージョンを設定。デフォルトは `tlsv1.3`、`tlsv1.2`、`tlsv1.1`、`tlsv1`
- **Fail If No Peer Cert**: クライアントが空の証明書を送信した場合に接続を拒否するかどうか。デフォルトは `false`、選択肢は `true` または `false`
- **Intermediate Certificate Depth**: ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数。デフォルトは `10`
- **Key Password**: プライベートキーがパスワード保護されている場合のユーザーパスワード

## 認証の設定

OCPP プロトコルの接続メッセージにはユーザー名とパスワードの概念が既に定義されているため、OCPP は以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../access-control/authn/mnesia.md)
- [MySQL 認証](../access-control/authn/mysql.md)
- [MongoDB 認証](../access-control/authn/mongodb.md)
- [PostgreSQL 認証](../access-control/authn/postgresql.md)
- [Redis 認証](../access-control/authn/redis.md)
- [HTTP サーバー認証](../access-control/authn/http.md)
- [JWT 認証](../access-control/authn/jwt.md)
- [LDAP 認証](../access-control/authn/ldap.md)

OCPP ゲートウェイは Websocket ハンドシェイクメッセージの Basic Authentication 情報を用いてクライアントの認証フィールドを生成します。

- クライアント ID: 固定パスプレフィックス以降の接続アドレス部分の値
- ユーザー名: Basic Authentication の Username の値
- パスワード: Basic Authentication の Password の値

REST API を使って OCPP ゲートウェイ用の組み込みデータベース認証を作成することも可能です。

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

MQTT プロトコルとは異なり、**ゲートウェイは認証器の作成のみをサポートし、認証器の一覧（または認証チェーン）の作成はサポートしていません**。

認証器が有効化されていない場合、すべての OCPP クライアントはログインを許可されます。

:::
