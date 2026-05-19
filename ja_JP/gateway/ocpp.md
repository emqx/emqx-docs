# OCPP ゲートウェイ

[OCPP](https://www.openchargealliance.org/)（Open Charge Point Protocol）は、充電ステーションと中央管理システムを接続するためのオープンな通信プロトコルであり、電気自動車充電インフラの統一された通信標準の提供を目的としています。OCPP ゲートウェイはプロトコルの変換器として機能し、OCPP と MQTT プロトコル間の橋渡しを行うことで、これらのプロトコルを使用するクライアント同士の通信を可能にします。

EMQX は [OCPP 1.6-J](https://www.openchargealliance.org/protocols/ocpp-16/) に対応したプロトコルゲートウェイを追加しており、OCPP 仕様に準拠した各種ブランドの充電ステーション機器と接続可能です。ルールエンジン、データ統合、REST API などを通じて管理システム（Central System）と連携し、ユーザーが迅速に電気自動車充電インフラを構築できるよう支援します。

本ページでは、EMQX における OCPP ゲートウェイの設定および利用方法を紹介します。

## OCPP ゲートウェイの有効化

EMQX の OCPP ゲートウェイは、ダッシュボード、REST API、設定ファイル `base.hocon` を通じて設定および有効化が可能です。本節ではダッシュボードによる設定例を用いて操作手順を説明します。

EMQX ダッシュボードの左ナビゲーションメニューで **管理** -> **ゲートウェイ** をクリックします。**ゲートウェイ** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**OCPP** を見つけ、**操作** 列の **設定** をクリックすると、**OCPP 初期化** ページに遷移します。

::: tip

EMQX をクラスターで運用している場合、ダッシュボードや REST API で行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md) で設定してください。

:::

設定を簡略化するため、EMQX は **ゲートウェイ** ページ上のすべての必須フィールドにデフォルト値を用意しています。大幅なカスタマイズが不要な場合は、3クリックで OCPP ゲートウェイを有効化できます。

1. **基本設定** タブで **次へ** をクリックし、すべてのデフォルト設定を受け入れます。
2. 続いて **リスナー** タブに遷移し、EMQX はポート `33033` に Websocket リスナーを事前設定しています。再度 **次へ** をクリックして設定を確定します。
3. 最後に **有効化** ボタンをクリックして OCPP ゲートウェイを起動します。

ゲートウェイの有効化が完了すると、**ゲートウェイ** ページに戻り、OCPP ゲートウェイのステータスが **有効** と表示されます。

<img src="./assets/ocpp-enabled.png" alt="OCPP ゲートウェイが有効化された状態" style="zoom:50%;" />

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

OCPP ゲートウェイが稼働したら、OCPP クライアントツールを使って接続テストや設定の動作確認が可能です。

ここでは [ocpp-go](https://github.com/lorenzodonini/ocpp-go) を例に、EMQX の OCPP ゲートウェイへの接続方法を紹介します。

1. まず、OCPP ゲートウェイとインターフェースする MQTT クライアントを準備します。例えば [MQTTX](https://mqttx.app/downloads) を使い、EMQX に接続しトピック `ocpp/#` をサブスクライブするよう設定します。

   <img src="./assets/ocpp-mqttx-create-conn.png" alt="MQTT 接続の作成" style="zoom:67%;" />

2. ocpp-go クライアントを実行し、OCPP ゲートウェイへ接続します。

   **注意**：以下のコマンド中の `<host>` は EMQX サーバーのアドレスに置き換えてください。

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

   このメッセージは ocpp-go クライアントが OCPP ゲートウェイに接続し、`BootNotification` リクエストを送信したことを示します。

4. MQTTX でトピック `ocpp/cs/chargePointSim` に以下の内容のメッセージを作成し送信します。

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

5. その後、MQTTX は `StatusNotification` ステータスレポートを受信します。これは OCPP クライアントが正常に OCPP ゲートウェイと接続を確立したことを示しています。

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

デフォルト設定に加え、EMQX はさまざまな設定オプションを提供しており、特定のビジネス要件に合わせた調整が可能です。本節では **ゲートウェイ** ページ上の各種フィールドの詳細を解説します。

### 基本設定

ゲートウェイページで OCPP ゲートウェイの **操作** 列にある **設定** ボタンをクリックすると、**基本設定** タブが表示され、以下の項目を設定できます。

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離に利用できます。例：`ocpp/`
- **Default Heartbeat Interval**：デフォルトのハートビート間隔（秒）、デフォルト値は `60s`
- **Heartbeat Checking Times Backoff**：ハートビートチェックのバックオフ回数、デフォルト値は `1`
- **Message Format Checking**：メッセージフォーマットの正当性チェックを有効にするかどうか。EMQX はアップロードおよびダウンロードストリームのメッセージフォーマットを json-schema で定義された形式と照合し、チェックに失敗した場合は対応する応答メッセージを返します。設定可能な値は以下の通りです。

    - `all`：すべてのメッセージをチェック
    - `upstream_only`：アップロードストリームのメッセージのみチェック
    - `dnstream_only`：ダウンロードストリームのメッセージのみチェック
    - `disable`：チェックを行わない
- **JSON Schema Directory**：OCPP メッセージ定義の JSON スキーマディレクトリ、デフォルトは `${application}/priv/schemas`
- **JSON Schema ID Prefix**：OCPP メッセージスキーマの ID プレフィックス、デフォルトは `urn:OCPP:1.6:2019:12:`
- **Idle Timeout**：非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）
- **Upstream**：アップロードストリームの設定グループ
    - **Topic**：アップロードストリームの Call Request メッセージ用トピック、デフォルトは `cp/${cid}`
    - **Reply Topic**：アップロードストリームの返信メッセージ用トピック、デフォルトは `cp/${cid}/Reply`
    - **Error Topic**：アップロードストリームのエラーメッセージ用トピック、デフォルトは `cp/${cid}/Reply`
    - **Topic Override Mapping**：メッセージ名によるアップロードストリームのトピック上書きマッピング
- **Downstream**：ダウンロードストリームの設定グループ
    - **Topic**：EMQX からのリクエスト／制御メッセージを受信するダウンロードストリームのトピック。すべての接続されたチャージポイントがサブスクライブするワイルドカードトピック名です。デフォルトは `cs/${cid}`
    - **Max Message Queue Length**：ダウンロードストリームのメッセージ配信における最大メッセージキュー長、デフォルトは `100`

### リスナーの追加

ポート `33033` に名前が **default** の Websocket リスナーがすでに設定されており、最大 16 のアクセプターをプールし、最大 1,024,000 の同時接続をサポートします。より詳細な設定を行う場合は **設定** をクリックし、リスナーを削除する場合は **削除** をクリック、新たにリスナーを追加する場合は **+ リスナー追加** をクリックしてください。

::: tip

OCPP ゲートウェイは Websocket および TLS 上の Websocket のリスナータイプのみをサポートしています。

:::

**リスナー追加** ページでは以下の設定が可能です。

**基本設定**

- **名前**：リスナーの一意識別子を設定します。
- **タイプ**：プロトコルタイプを選択します。OCPP では `ws` または `wss` が選択可能です。
- **バインド**：リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティングの分離に利用します。

**リスナー設定**

- **パス**：接続アドレスのパスプレフィックスを設定します。クライアントはこの完全なアドレスを用いて接続します。デフォルトは `/ocpp`
- **アクセプター**：アクセプタープールのサイズを設定します。デフォルトは `16`
- **最大接続数**：リスナーが処理可能な同時接続数の上限を設定します。デフォルトは `1024000`
- **最大接続レート**：リスナーが1秒あたりに受け入れる新規接続の最大レートを設定します。デフォルトは `1000`
- **プロキシプロトコル**：EMQX が [ロードバランサー](../deploy/cluster/lb.md) の背後にある場合にプロトコル V1/V2 を有効化します。
- **プロキシプロトコルタイムアウト**：非アクティブ状態でプロキシプロトコルパッケージの受信を待機する最大時間（秒）、デフォルトは `3s`

**TCP 設定**

- **ActiveN**：ソケットの `{active, N}` オプションを設定します。これはソケットが能動的に処理可能な受信パケット数です。詳細は [Erlang ドキュメント - setopts/2](https://erlang.org/doc/man/inet.html#setopts-2) を参照してください。
- **バッファ**：受信および送信パケットを格納するバッファサイズを KB 単位で設定します。
- **TCP_NODELAY**：接続に対して `TCP_NODELAY` フラグを有効にするかどうかを設定します。これはクライアントが前回のデータのアックを待たずに追加データを送信するかどうかを制御します。デフォルトは `false`、設定可能値は `true`、`false`
- **SO_REUSEADDR**：ローカルポート番号の再利用を許可するかどうかを設定します。
- **送信タイムアウト**：非アクティブ状態で送信タイムアウトを待機する最大時間（秒）、デフォルトは `15s`
- **送信タイムアウト時の切断**：送信タイムアウト発生時に接続を切断するかどうかを設定します。

**SSL 設定**（wss リスナーのみ）

TLS 検証の有効化はトグルスイッチで設定可能ですが、その前に関連する **TLS 証明書**、**TLS キー**、および **CA 証明書** の情報をファイル内容の入力または **ファイル選択** ボタンでアップロードして設定する必要があります。詳細は [SSL/TLS 接続の有効化](../network/emqx-mqtt-tls.md) を参照してください。

続いて以下の設定が可能です。

- **SSL バージョン**：サポートする SSL バージョンを設定します。デフォルトは `tlsv1.3`、`tlsv1.2`、`tlsv1.1`、`tlsv1`
- **証明書がない場合の拒否**：クライアントが空の証明書を送信した場合に EMQX が接続を拒否するかどうかを設定します。デフォルトは `false`、設定可能値は `true`、`false`
- **中間証明書の深さ**：ピア証明書に続く有効な認証経路に含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは `10`
- **キーのパスワード**：秘密鍵がパスワード保護されている場合のパスワードを設定します。

## 認証の設定

OCPP プロトコルの接続メッセージにはすでにユーザー名とパスワードの概念が定義されているため、OCPP は以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../access-control/authn/mnesia.md)
- [MySQL 認証](../access-control/authn/mysql.md)
- [MongoDB 認証](../access-control/authn/mongodb.md)
- [PostgreSQL 認証](../access-control/authn/postgresql.md)
- [Redis 認証](../access-control/authn/redis.md)
- [HTTP サーバー認証](../access-control/authn/http.md)
- [JWT 認証](../access-control/authn/jwt.md)
- [LDAP 認証](../access-control/authn/ldap.md)

OCPP ゲートウェイは Websocket ハンドシェイクメッセージの Basic Authentication 情報を用いてクライアントの認証フィールドを生成します。

- クライアント ID：固定パスプレフィックス以降の接続アドレス部分の値
- ユーザー名：Basic Authentication のユーザー名
- パスワード：Basic Authentication のパスワード

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

MQTT プロトコルとは異なり、**ゲートウェイは認証器の作成のみをサポートし、認証器のリスト（または認証チェーン）の作成はサポートしていません**。

認証器が有効化されていない場合、すべての OCPP クライアントのログインが許可されます。

:::
