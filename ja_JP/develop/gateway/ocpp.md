# OCPP ゲートウェイ

[OCPP](https://www.openchargealliance.org/)（Open Charge Point Protocol）は、充電ステーションと中央管理システムを接続するためのオープンな通信プロトコルであり、電気自動車充電インフラの統一された通信標準を提供することを目的としています。OCPP ゲートウェイはプロトコル変換器として機能し、OCPP と MQTT プロトコル間の橋渡しを行い、これらのプロトコルを使用するクライアント同士の通信を可能にします。

EMQX は [OCPP 1.6-J](https://www.openchargealliance.org/protocols/ocpp-16/) に対応したプロトコルゲートウェイを追加しており、OCPP 仕様に準拠したさまざまなブランドの充電設備と接続可能です。ルールエンジン、データ統合、REST API などを通じて管理システム（Central System）と連携し、ユーザーが電気自動車充電インフラを迅速に構築できるよう支援します。

本ページでは、EMQX における OCPP ゲートウェイの設定および利用方法を紹介します。

## OCPP ゲートウェイの有効化

EMQX の OCPP ゲートウェイは、ダッシュボード、REST API、設定ファイル `base.hocon` を通じて設定および有効化できます。本節ではダッシュボードによる設定を例に操作手順を説明します。

EMQX ダッシュボードの左側ナビゲーションメニューで **管理** -> **ゲートウェイ** をクリックします。**ゲートウェイ** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**OCPP** を探し、**操作** 列の **設定** をクリックすると、**OCPP 初期化** ページに遷移します。

::: tip

EMQX をクラスターで運用している場合、ダッシュボードや REST API で行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../../guides/configuration/configuration.md) で設定してください。

:::

設定を簡略化するため、EMQX は **ゲートウェイ** ページのすべての必須項目にデフォルト値を用意しています。大幅なカスタマイズが不要な場合は、以下の3クリックで OCPP ゲートウェイを有効化できます。

1. **基本設定** タブで **次へ** をクリックし、すべてのデフォルト設定を受け入れます。
2. 続いて **リスナー** タブに遷移し、EMQX がポート `33033` で Websocket リスナーを事前設定しています。再度 **次へ** をクリックして設定を確定します。
3. 最後に **有効化** ボタンをクリックして OCPP ゲートウェイを起動します。

ゲートウェイの有効化が完了すると、**ゲートウェイ** ページに戻り、OCPP ゲートウェイの状態が **有効** と表示されます。

<img src="./assets/ocpp-enabled.png" alt="OCPP ゲートウェイ有効化" style="zoom:50%;" />

上記の設定は REST API でも可能です。

**例：**

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

OCPP ゲートウェイが稼働したら、OCPP クライアントツールを使って接続テストおよび設定の動作確認ができます。

ここでは [ocpp-go](https://github.com/lorenzodonini/ocpp-go) を例に、EMQX の OCPP ゲートウェイへの接続方法を紹介します。

1. まず、OCPP ゲートウェイと連携する MQTT クライアントを準備します。例えば [MQTTX](https://mqttx.app/downloads) を使い、EMQX に接続してトピック `ocpp/#` をサブスクライブするよう設定します。

   <img src="./assets/ocpp-mqttx-create-conn.png" alt="MQTT 接続作成" style="zoom:67%;" />

2. ocpp-go クライアントを起動し、OCPP ゲートウェイに接続します。

   **注意**：以下のコマンド内の `<host>` は EMQX サーバーのアドレスに置き換えてください。

   ```shell
   docker run -e CLIENT_ID=chargePointSim -e CENTRAL_SYSTEM_URL=ws://<host>:33033/ocpp -it --rm --name charge-point ldonini/ocpp1.6-charge-point:latest
   ```

   接続成功時は、以下のようなログが出力されます。

   ```css
   INFO[2023-12-01T03:08:39Z] connecting to server logger=websocket
   INFO[2023-12-01T03:08:39Z] connected to server as chargePointSim logger=websocket
   INFO[2023-12-01T03:08:39Z] connected to central system at ws://172.31.1.103:33033/ocpp
   INFO[2023-12-01T03:08:39Z] dispatched request 1200012677 to server logger=ocppj
   ```

3. MQTTX で以下のようなメッセージが受信されることを確認します。

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

   このメッセージは ocpp-go クライアントが OCPP ゲートウェイに接続し、`BootNotification` リクエストを送信したことを示しています。

4. MQTTX でトピック `ocpp/cs/chargePointSim` に対し、以下の内容のメッセージを作成して送信します。

   **注意**：`UniqueId` は前のメッセージで受信したものに置き換えてください。

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

5. その後、MQTTX は `StatusNotification` ステータスレポートを受信します。これは OCPP クライアントが OCPP ゲートウェイとの接続に成功したことを示します。

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

デフォルト設定に加え、EMQX はさまざまな設定項目を提供し、特定のビジネス要件に合わせた調整が可能です。本節では **ゲートウェイ** ページで利用可能な各フィールドの詳細を解説します。

### 基本設定

ゲートウェイページの OCPP ゲートウェイの **操作** 列にある **設定** ボタンをクリックすると、**基本設定** タブで以下の項目を設定できます。

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定します。異なるプロトコル間でメッセージルーティングの分離を実現するために使用します。例：`ocpp/`
- **デフォルトハートビート間隔**：ハートビートのデフォルト間隔（秒）、デフォルト値：`60s`
- **ハートビートチェック回数のバックオフ**：ハートビートチェックのバックオフ回数、デフォルト値：`1`
- **メッセージフォーマットチェック**：メッセージフォーマットの妥当性チェックを有効にするかどうか。EMQX はアップロードストリームおよびダウンロードストリームのメッセージを json-schema で定義されたフォーマットに対して検証します。検証失敗時は対応する応答メッセージを返します。チェック戦略は以下のいずれかを指定可能です。

    - `all`：すべてのメッセージをチェック
    - `upstream_only`：アップロードストリームのメッセージのみチェック
    - `dnstream_only`：ダウンロードストリームのメッセージのみチェック
    - `disable`：チェックしない
- **JSON スキーマディレクトリ**：OCPP メッセージ定義の JSON スキーマディレクトリ、デフォルト：`${application}/priv/schemas`
- **JSON スキーマ ID プレフィックス**：OCPP メッセージスキーマの ID プレフィックス、デフォルト：`urn:OCPP:1.6:2019:12:`
- **アイドルタイムアウト**：非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）
- **アップストリーム**：アップロードストリームの設定グループ
    - **トピック**：アップロードストリームの Call Request メッセージ用トピック、デフォルト：`cp/${cid}`
    - **返信トピック**：アップロードストリームの返信メッセージ用トピック、デフォルト：`cp/${cid}/Reply`
    - **エラートピック**：アップロードストリームのエラーメッセージ用トピック、デフォルト：`cp/${cid}/Reply`
    - **トピックオーバーライドマッピング**：メッセージ名ごとのアップロードストリームトピックの上書きマッピング
- **ダウンストリーム**：ダウンロードストリームの設定グループ
    - **トピック**：EMQX からのリクエスト／制御メッセージを受信するダウンロードストリームのトピック。接続されたすべてのチャージポイントがサブスクライブするワイルドカードトピック名。デフォルト：`cs/${cid}`
    - **最大メッセージキュー長**：ダウンロードストリームのメッセージ配信における最大キュー長、デフォルト：`100`

### リスナーの追加

ポート `33033` で名前が **default** の Websocket リスナーがあらかじめ設定されており、プール内の最大アクセプター数は16、最大同時接続数は1,024,000まで対応しています。より詳細な設定は **設定** ボタンで行え、リスナーの削除は **削除** ボタン、新規リスナーの追加は **+ リスナー追加** ボタンをクリックしてください。

::: tip

OCPP ゲートウェイは Websocket および TLS 上の Websocket タイプのリスナーのみをサポートしています。

:::

**リスナー追加** ページでは以下の設定項目を入力します。

**基本設定**

- **名前**：リスナーの一意識別子を設定
- **タイプ**：プロトコルタイプを選択。OCPP では `ws` または `wss` を指定可能
- **バインド**：リスナーが接続を受け付けるポート番号を設定
- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定し、異なるプロトコル間でメッセージルーティングの分離を実現

**リスナー設定**

- **パス**：接続アドレスのパスプレフィックスを設定。クライアントは接続時にこの完全なアドレスを指定する必要があります。デフォルト：`/ocpp`
- **アクセプター数**：アクセプタープールのサイズを設定、デフォルト：`16`
- **最大接続数**：リスナーが処理可能な最大同時接続数、デフォルト：`1024000`
- **最大接続レート**：リスナーが1秒間に受け入れ可能な新規接続の最大数、デフォルト：`1000`
- **プロキシプロトコル**：EMQX が [ロードバランサー](../../guides/cluster/lb.md) 配下にある場合にプロトコル V1/V2 を有効化
- **プロキシプロトコルタイムアウト**：非アクティブ状態でプロキシプロトコルパッケージを待機する最大時間（秒）、デフォルト：`3s`

**TCP 設定**

- **ActiveN**：ソケットの `{active, N}` オプションを設定。ソケットが積極的に処理可能な受信パケット数。詳細は [Erlang ドキュメント - setopts/2](https://erlang.org/doc/man/inet.html#setopts-2) を参照
- **バッファ**：受信および送信パケットを格納するバッファサイズ（KB単位）
- **TCP_NODELAY**：接続に対して `TCP_NODELAY` フラグを有効化するかどうか。前のデータのアックを待たずに追加データを送信するかの設定。デフォルト：`false`、選択肢：`true`、`false`
- **SO_REUSEADDR**：ローカルのポート番号再利用を許可するかどうか
- **送信タイムアウト**：非アクティブ状態で送信タイムアウトを待機する最大時間（秒）、デフォルト：`15s`
- **送信タイムアウト時の切断**：送信タイムアウト時に接続を切断するかどうか

**SSL 設定**（wss リスナーのみ）

TLS 検証の有効化はトグルスイッチで設定可能ですが、その前に関連する **TLS 証明書**、**TLS キー**、**CA 証明書** の情報を、ファイルの内容を入力するか **ファイル選択** ボタンでアップロードして設定する必要があります。詳細は [SSL/TLS 接続の有効化](../../guides/network/emqx-mqtt-tls.md) を参照してください。

続いて以下の設定が可能です。

- **SSL バージョン**：サポートする SSL バージョンを設定。デフォルトは `tlsv1.3`、`tlsv1.2`、`tlsv1.1`、`tlsv1`
- **ピア証明書なし時の拒否**：クライアントが空の証明書を送信した場合に接続を拒否するかどうか。デフォルト：`false`、選択肢：`true`、`false`
- **中間証明書の最大深度**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数、デフォルト：`10`
- **キーのパスワード**：秘密鍵がパスワード保護されている場合のユーザーパスワード

## 認証の設定

OCPP プロトコルの接続メッセージにはすでにユーザー名とパスワードの概念が定義されているため、OCPP は以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../../guides/access-control/authn/mnesia.md)
- [MySQL 認証](../../guides/access-control/authn/mysql.md)
- [MongoDB 認証](../../guides/access-control/authn/mongodb.md)
- [PostgreSQL 認証](../../guides/access-control/authn/postgresql.md)
- [Redis 認証](../../guides/access-control/authn/redis.md)
- [HTTP サーバー認証](../../guides/access-control/authn/http.md)
- [JWT 認証](../../guides/access-control/authn/jwt.md)
- [LDAP 認証](../../guides/access-control/authn/ldap.md)

OCPP ゲートウェイは Websocket ハンドシェイクメッセージの Basic Authentication 情報を用いてクライアントの認証フィールドを生成します。

- クライアント ID：固定パスプレフィックスの後の接続アドレス部分の値
- ユーザー名：Basic Authentication の Username の値
- パスワード：Basic Authentication の Password の値

REST API を使って OCPP ゲートウェイに組み込みデータベース認証を作成することも可能です。

**例：**

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

MQTT プロトコルとは異なり、**ゲートウェイは認証器のリスト（または認証チェーン）の作成ではなく、認証器の単一作成のみをサポートしています**。

認証器が有効化されていない場合は、すべての OCPP クライアントのログインが許可されます。

:::
