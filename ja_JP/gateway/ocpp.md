# OCPP ゲートウェイ

[OCPP](https://www.openchargealliance.org/)（Open Charge Point Protocol）は、充電ステーションと中央管理システムを接続するオープンな通信プロトコルであり、電気自動車充電インフラ向けの統一された通信標準を提供することを目的としています。OCPP ゲートウェイはプロトコルの翻訳機として機能し、OCPP と MQTT プロトコル間の橋渡しを行うことで、これらのプロトコルを使用するクライアント同士の通信を可能にします。

EMQX は [OCPP 1.6-J](https://openchargealliance.org/protocols/open-charge-point-protocol/#OCPP1.6) に対応したプロトコルゲートウェイを追加しており、OCPP 仕様に準拠したさまざまなブランドの充電ステーション機器と接続可能です。ルールエンジン、データ統合、REST API などを通じて管理システム（Central System）と連携し、ユーザーが迅速に電気自動車充電インフラを構築できるよう支援します。

本ページでは、EMQX における OCPP ゲートウェイの設定および利用方法について紹介します。

## OCPP ゲートウェイの有効化

EMQX の OCPP ゲートウェイは、ダッシュボード、REST API、および設定ファイル `base.hocon` を通じて設定および有効化が可能です。本節ではダッシュボードによる設定手順を例に操作方法を説明します。

EMQX ダッシュボードの左側ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**OCPP** を見つけ、**Actions** 列の **Setup** をクリックすると、**Initialize OCPP** ページに遷移します。

::: tip

EMQX をクラスターで運用している場合、ダッシュボードや REST API で行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md) で設定してください。

:::

設定を簡略化するため、EMQX は **Gateways** ページのすべての必須フィールドにデフォルト値を用意しています。大幅なカスタマイズが不要な場合、以下の3ステップで OCPP ゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. **Listeners** タブに遷移し、EMQX がポート `33033` で Websocket リスナーを事前設定しているので、再度 **Next** をクリックして設定を確定します。
3. **Enable** ボタンをクリックして OCPP ゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、OCPP ゲートウェイのステータスが **Enabled** と表示されます。

<img src="./assets/ocpp-enabled.png" alt="OCPP ゲートウェイ有効化済み" style="zoom:50%;" />

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

OCPP ゲートウェイが稼働すると、OCPP クライアントツールを使って接続テストや設定の動作確認が可能です。

ここでは [ocpp-go](https://github.com/lorenzodonini/ocpp-go) を例に、EMQX の OCPP ゲートウェイへの接続方法を紹介します。

1. まず、OCPP ゲートウェイと連携する MQTT クライアントを準備します。例えば [MQTTX](https://mqttx.app/downloads) を使用し、EMQX に接続してトピック `ocpp/#` をサブスクライブするよう設定します。

   <img src="./assets/ocpp-mqttx-create-conn.png" alt="MQTT 接続作成" style="zoom:67%;" />

2. ocpp-go クライアントを実行し、OCPP ゲートウェイに接続します。

   **注意**：以下のコマンド内の `<host>` は EMQX サーバーのアドレスに置き換えてください。

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

   このメッセージは、ocpp-go クライアントが OCPP ゲートウェイに接続し、`BootNotification` リクエストを送信したことを示しています。

4. MQTTX でトピック `ocpp/cs/chargePointSim` に以下の内容のメッセージを作成し、送信します。

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

5. その後、MQTTX は `StatusNotification` ステータスレポートを受信します。これは OCPP クライアントが OCPP ゲートウェイとの接続を正常に確立したことを示しています。

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

デフォルト設定に加え、EMQX はさまざまな設定オプションを提供しており、特定の業務要件に合わせて柔軟に対応可能です。本節では **Gateways** ページで利用できる各種フィールドについて詳しく説明します。

### 基本設定

Gateways ページで OCPP ゲートウェイの **Actions** 列にある **Settings** ボタンをクリックし、**Basic Configuration** タブで以下のフィールドを設定できます。

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

- **MountPoint**: パブリッシュやサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離を実現するために利用します。例：`ocpp/`
- **Default Heartbeat Interval**: デフォルトのハートビート間隔（秒）、初期値は `60s`
- **Heartbeat Checking Times Backoff**: ハートビートチェックのバックオフ回数、初期値は `1`
- **Message Format Checking**: メッセージフォーマットの妥当性チェックを有効にするかどうか。EMQX はアップロードおよびダウンロードストリームのメッセージを json-schema で定義されたフォーマットと照合し、チェックに失敗した場合は対応する応答メッセージを返します。設定可能な値は以下の通りです。

    - `disable`: メッセージのチェックを行わない（デフォルト）
    - `upstream_only`: アップロードストリームのメッセージのみチェック
    - `dnstream_only`: ダウンロードストリームのメッセージのみチェック
    - `all`: すべてのメッセージをチェック
- **JSON Schema Directory**: OCPP メッセージ定義用の JSON スキーマディレクトリ、デフォルトは `${application}/priv/schemas`
- **JSON Schema ID Prefix**: OCPP メッセージスキーマの ID プレフィックス、デフォルトは `urn:OCPP:1.6:2019:12:`
- **Idle Timeout**: 非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）
- **Upstream**: アップロードストリームの設定グループ
    - **Topic**: アップロードストリームの Call Request メッセージ用トピック、デフォルトは `cp/${cid}`
    - **Reply Topic**: アップロードストリームの返信メッセージ用トピック、デフォルトは `cp/${cid}/Reply`
    - **Error Topic**: アップロードストリームのエラーメッセージ用トピック、デフォルトは `cp/${cid}/Reply`
    - **Topic Override Mapping**: メッセージ名ごとのアップロードストリームトピック上書きマッピング
- **Downstream**: ダウンロードストリームの設定グループ
    - **Topic**: EMQX からリクエスト・制御メッセージを受信するダウンロードストリームトピック。すべての接続されたチャージポイントがサブスクライブするワイルドカードトピック名です。デフォルトは `cs/${cid}`
    - **Max Message Queue Length**: ダウンロードストリームのメッセージ配信における最大メッセージキュー長、デフォルトは `100`

### リスナーの追加

ポート `33033` に名前が **default** の Websocket リスナーが既に設定されており、プール内の最大受け入れ数は 16、最大同時接続数は 1,024,000 です。より詳細な設定を行う場合は **Settings** をクリックし、リスナーを削除したい場合は **Delete** をクリック、新しいリスナーを追加する場合は **+ Add Listener** をクリックしてください。

::: tip

OCPP ゲートウェイは Websocket および Websocket over TLS のリスナータイプのみサポートしています。

:::

**Add Listener** ページでは以下の設定が可能です。

**基本設定**

- **Name**: リスナーの一意識別子を設定します。
- **Type**: プロトコルタイプを選択します。OCPP では `ws` または `wss` を指定可能です。
- **Bind**: リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**: パブリッシュやサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティング分離を実現します。

**リスナー設定**

- **Path**: 接続アドレスのパスプレフィックスを設定します。クライアントは接続時にこの完全なアドレスを指定する必要があります。デフォルトは `/ocpp`
- **Acceptor**: 受け入れプールのサイズを設定します。デフォルトは `16`
- **Max Connections**: リスナーが処理可能な最大同時接続数を設定します。デフォルトは `1024000`
- **Max Connection Rate**: リスナーが1秒あたりに受け入れ可能な新規接続の最大レートを設定します。デフォルトは `1000`
- **Proxy Protocol**: EMQX が [ロードバランサー](../deploy/cluster/lb.md) の背後にある場合に V1/V2 プロトコルを有効化します。
- **Proxy Protocol Timeout**: プロキシプロトコルパッケージの受信を待つ最大時間（秒）。非アクティブ時に接続を切断するまでの時間。デフォルトは `3s`

**TCP 設定**

- **ActiveN**: ソケットの `{active, N}` オプションを設定します。これはソケットが能動的に処理可能な受信パケット数を示します。詳細は [Erlang Documentation - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2) を参照してください。
- **Buffer**: 受信および送信パケットを格納するバッファサイズ（KB単位）を設定します。
- **TCP_NODELAY**: 接続に対して `TCP_NODELAY` フラグを有効にするかどうかを設定します。これはクライアントが前回のデータのアック（ACK）を待たずに追加データを送信できるかを制御します。デフォルトは `false`。設定値は `true` または `false`。
- **SO_REUSEADDR**: ポート番号のローカル再利用を許可するかどうかを設定します。
- **Send Timeout**: 送信タイムアウトまでの最大待機時間（秒）。非アクティブ時に接続を切断するまでの時間。デフォルトは `15s`
- **Send Timeout Close**: 送信タイムアウト時に接続を切断するかどうかを設定します。

**SSL 設定**（wss リスナーのみ）

TLS 検証の有効化はトグルスイッチで設定可能ですが、その前に関連する **TLS Cert**、**TLS Key**、および **CA Cert** の情報をファイルの内容を入力するか、**Select File** ボタンでアップロードして設定する必要があります。詳細は [SSL/TLS 接続の有効化](../network/emqx-mqtt-tls.md) を参照してください。

設定可能な項目は以下の通りです。

- **SSL Versions**: サポートする SSL バージョンを設定します。デフォルトは `tlsv1.3`、`tlsv1.2`、`tlsv1.1`、`tlsv1`
- **Fail If No Peer Cert**: クライアントが空の証明書を送信した場合に接続を拒否するかどうかを設定します。デフォルトは `false`。設定値は `true` または `false`
- **Intermediate Certificate Depth**: ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは `10`
- **Key Password**: プライベートキーがパスワード保護されている場合に使用するパスワードを設定します。

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

OCPP ゲートウェイは Websocket ハンドシェイクメッセージの Basic Authentication 情報を利用してクライアントの認証フィールドを生成します。

- クライアント ID: 固定パスプレフィックス以降の接続アドレス部分の値
- ユーザー名: Basic Authentication の Username の値
- パスワード: Basic Authentication の Password の値

REST API を用いて OCPP ゲートウェイ用の組み込みデータベース認証を作成することも可能です。

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

MQTT プロトコルとは異なり、**ゲートウェイは認証方式の作成のみをサポートし、認証方式の一覧（または認証チェーン）の作成はサポートしていません**。

認証方式が有効化されていない場合、すべての OCPP クライアントのログインが許可されます。

:::
