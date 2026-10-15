# CoAP ゲートウェイ

EMQX の CoAP ゲートウェイは、[Publish-Subscribe Broker for the CoAP](https://datatracker.ietf.org/doc/html/draft-ietf-core-coap-pubsub-09) プロトコルに準拠し、標準的なパブリッシュ、サブスクライブ、メッセージ受信を実装可能にします。

以下は、コネクションモードとコネクションレスモードでサポートされる機能一覧です。

| 機能               | コネクションレスモード | コネクションモード |
| ------------------ | ---------------------- | ------------------ |
| メッセージパブリッシュ | √                      | √                  |
| トピックサブスクライブ | √                      | √                  |
| トピックのサブスクライブ解除 | ×                      | √                  |
| コネクションの作成     | ×                      | √                  |
| コネクションのクローズ | ×                      | √                  |
| ハートビート         | ×                      | √                  |
| 認証                 | ×                      | √                  |

<!--アーキテクチャの簡単な紹介-->

## CoAP ゲートウェイの有効化

EMQX 5 では、CoAP ゲートウェイはダッシュボード、REST API、設定ファイル `base.hocon` を通じて設定および有効化できます。本節ではダッシュボードを用いた設定例を示します。

EMQX ダッシュボードの左ナビゲーションメニューから **Extensions** -> **Gateways** をクリックします。**Gateway** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**CoAP** を探し、**Actions** 列の **Setup** をクリックすると、**Initialize CoAP** ページに遷移します。

::: tip

EMQX をクラスターで運用している場合、ダッシュボードや REST API で行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../../guides/configuration/configuration.md) で設定してください。

:::

EMQX CoAP ゲートウェイはコネクションレスモードとコネクションモードの両方をサポートしています。コネクションレスモードではメッセージは一回限りの送信として扱われ、センサーの読み取りや単純なコマンド送信など短時間のやり取りに適しています。コネクションモードでは、クライアントがデータ転送開始前にブローカーとコネクションを確立します。

**Connection Requested** の設定でコネクションモード（`true`）またはコネクションレスモード（`false`）を選択できます。デフォルトは `false` です。

コネクションモードを確認後、設定を続けられます。特にカスタマイズが不要な場合は、以下の3クリックで CoAP ゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. **Listeners** タブに遷移し、EMQX がポート `5683` で UDP リスナーを事前設定しています。設定を確認し、再度 **Next** をクリックします。
3. **Enable** ボタンをクリックして CoAP ゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、CoAP ゲートウェイの状態が **Enabled** と表示されます。

<img src="./assets/coap-enabled.png" alt="CoAP ゲートウェイ有効化" style="zoom:50%;" />

上記の設定は REST API でも可能です。

**例:**

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateways/coap' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "coap",
  "enable": true,
  "mountpoint": "coap/",
  "connection_required": false,
  "listeners": [
    {
      "type": "udp",
      "name": "default",
      "bind": "5683",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```

詳細な REST API の説明は [REST API - Gateway](../../guides/api.md) をご覧ください。

カスタマイズが必要な場合やリスナー追加、認証ルールの追加を行いたい場合は、[CoAP ゲートウェイのカスタマイズ](#customize-your-coap-gateway) をご参照ください。

CoAP ゲートウェイは UDP および DTLS タイプリスナーのみをサポートしています。設定可能なパラメータの完全なリストは [Gateway Configuration - Listeners](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) を参照してください。

## CoAP クライアントとの連携

### クライアントライブラリ

CoAP ゲートウェイを構築後、CoAP クライアントツールを使って接続テストを行い、正常に動作することを確認できます。以下は推奨される CoAP クライアントツールの例です。

- [libcoap](https://github.com/obgm/libcoap)
- [californium](https://github.com/eclipse/californium)

## パブリッシュ／サブスクライブ

CoAP ゲートウェイは [Publish-Subscribe Broker for the CoAP](https://datatracker.ietf.org/doc/html/draft-ietf-core-coap-pubsub-09) 標準で定義された URI パスとメソッドを使用します。

詳細なパラメータは [メッセージパブリッシュ](#message-publish)、[トピックサブスクライブ](#topic-subscribe)、[トピックサブスクライブ解除](#topic-unsubscribe) を参照してください。

## CoAP ゲートウェイのカスタマイズ

デフォルト設定に加え、EMQX はビジネス要件に応じた多様な設定オプションを提供します。本節では **Gateways** ページで利用可能な各フィールドの詳細を説明します。スクリーンショット下の説明文もご参照ください。

<img src="./assets/coap-basic-conf.png" alt="image-20230420152920254" style="zoom:50%;" />

- **Connection Required**: コネクションレスモードまたはコネクションモードを有効にするか設定します。デフォルトは `false`（コネクションレス）。選択肢は `false`（コネクションレス）、`true`（コネクション）。

- **Notification Message Type**: 配信される CoAP メッセージのタイプを設定します。デフォルトは `qos`。選択肢は以下の通りです。

  - **qos**: CoAP 通知のアック要否は受信メッセージの QoS レベルに依存します。
    - QoS 0 の場合、クライアントからのアックは不要。
    - QoS 1/2 の場合、クライアントからのアックが必要。
  - **con**: CoAP 通知はクライアントからのアックが必要。
  - **non**: CoAP 通知はクライアントからのアックは不要。

- **Heartbeat**: **Connection Required** が `true` の場合のみ必要。接続維持のための最小ハートビート間隔を設定します。デフォルトは 30 秒。

- **Enable Statistics**: ゲートウェイによる統計収集と報告を許可するか設定します。デフォルトは `true`。選択肢は `true`、`false`。

- **Subscriber QoS**: サブスクライブ要求のデフォルト QoS レベルを設定します。デフォルトは `coap`。選択肢は以下の通りです。

  - **coap**: **Notification Message Type** の設定に従い QoS レベルを決定します。
    - アック不要の場合は QoS 0。
    - アック必要の場合は QoS 1。
  - **qos0**, **qos1**, **qos2**

- **Publish QoS**: パブリッシュ要求のデフォルト QoS レベルを設定します。デフォルトは `coap`。選択肢は `coap`、`qos0`、`qos1`、`qos2`。

- **MountPoint**: パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離を実現します。例: *CoAP*

  **注意**: このトピックプレフィックスはゲートウェイが管理します。CoAP クライアントはパブリッシュやサブスクライブ時に明示的にこのプレフィックスを付加する必要はありません。

### リスナーの追加

デフォルトで、名前が **default** の UDP リスナーがポート `5683` に設定されており、最大 1,024,000 の同時接続をサポートしています。**Settings** をクリックすると詳細設定が可能で、**Delete** でリスナーを削除、**Add Listener** で新規リスナーを追加できます。

![coap-advanced-conf](./assets/coap-advanced-conf.png)

**Add Listener** をクリックすると、以下の設定項目を入力できるページが開きます。

**基本設定**

- **Name**: リスナーの一意識別子を設定します。
- **Type**: プロトコルタイプを選択します。CoAP では `udp` または `dtls` が選べます。
- **Bind**: リスナーが受け付ける接続のポート番号を設定します。
- **MountPoint**（任意）: パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティング分離を実現します。

**リスナー設定**

- **Max Connections**: リスナーが処理可能な最大同時接続数を設定します。デフォルトは 1024000。
- **Max Connection Rate**: リスナーが1秒あたりに受け入れ可能な新規接続の最大レートを設定します。デフォルトは 1000。

**UDP 設定**

- **ActiveN**: ソケットの `{active, N}` オプションを設定します。これはソケットが積極的に処理できる受信パケット数を意味します。詳細は [Erlang Documentation - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2) を参照してください。
- **Buffer**: 受信および送信パケットを格納するバッファサイズを KB 単位で設定します。
- **Receive Buffer**: 受信バッファのサイズを KB 単位で設定します。
- **Send Buffer**: 送信バッファのサイズを KB 単位で設定します。
- **SO_REUSEADDR**: ローカルでのポート番号再利用を許可するか設定します。

**DTLS 設定**（DTLS リスナーのみ）

TLS Verify の有効化はトグルスイッチで設定可能です。ただし、その前に関連する **TLS Cert**、**TLS Key**、**CA Cert** の情報をファイル内容の入力か **Select File** ボタンでのアップロードにより設定する必要があります。詳細は [Enable SSL/TLS Connections](https://docs.emqx.com/en/enterprise/v5.0/network/emqx-mqtt-tls.html) をご覧ください。

### 認証の設定

クライアント ID、ユーザー名、パスワードはクライアントの [Create Connection](#create-connection) リクエストで提供されます。CoAP ゲートウェイは以下の認証方式をサポートしています。

- [組み込みデータベース認証](../../guides/access-control/authn/mnesia.md)
- [MySQL 認証](../../guides/access-control/authn/mysql.md)
- [MongoDB 認証](../../guides/access-control/authn/mongodb.md)
- [PostgreSQL 認証](../../guides/access-control/authn/postgresql.md)
- [Redis 認証](../../guides/access-control/authn/redis.md)
- [HTTP サーバ認証](../../guides/access-control/authn/http.md)
- [JWT 認証](../../guides/access-control/authn/jwt.md)
- [LDAP 認証](../../guides/access-control/authn/ldap.md)

ここではダッシュボードを例に認証設定方法を説明します。

**Gateways** ページで **CoAP** を探し、**Actions** 列の **Setup** をクリックし、**Authentication** タブに入ります。

**Create Authentication** をクリックし、**Mechanism** に **Password-Based** または **JWT** を選択し、必要に応じて **Backend** を選びます。

認証方式の詳細設定方法は本節冒頭の各リンク先を参照してください。

ダッシュボード以外に REST API でも認証設定が可能です。例えば、CoAP ゲートウェイ用に組み込みデータベース認証を作成する場合は以下のコードを使用します。

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/coap/authentication' \
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

MQTT プロトコルとは異なり、**ゲートウェイは認証器の作成のみをサポートし、認証器リスト（または認証チェーン）はサポートしません**。認証器が有効化されていない場合、すべての CoAP クライアントのログインが許可されます。

:::

## リファレンス：CoAP クライアントガイド

### コネクションの作成

`Connection Mode` でのみ利用可能です。

このインターフェースは CoAP ゲートウェイへのクライアントコネクションを作成するために使用します。CoAP ゲートウェイの認証が有効な場合、このリクエストで提供された `clientid`、`username`、`password` を検証し、不正ユーザーを防止します。

**リクエストパラメータ:**

- メソッド: `POST`
- URI: `mqtt/connection{?QueryString*}`、`QueryString` は以下の通りです。
  - `clientid`: 必須パラメータ、UTF-8 文字列。ゲートウェイはこの文字列をコネクションの一意識別子として使用します。
  - `username`: 任意パラメータ、UTF-8 文字列。接続認証に使用。
  - `password`: 任意パラメータ、UTF-8 文字列。接続認証に使用。
- ペイロード: 空

**レスポンス:**

- 戻りコード:
  - `2.01`: コネクション作成成功。このコネクションのトークン文字列がメッセージ本文で返されます。
  - `4.00`: 不正なリクエスト。詳細なエラー情報がメッセージ本文で返されます。
  - `4.01`: 認可失敗。リクエスト形式は正しいが認可に失敗。
- ペイロード: 戻りコードが `2.01` の場合は `Token`、それ以外は `ErrorMessage`。
  - `Token`: 以降のリクエストで使用するトークン文字列。
  - `ErrorMessage`: エラー説明メッセージ。

`libcoap` を例に示します。

```bash
# clientid 123、username と password に admin/public を指定して接続リクエストを送信。
# 戻りトークンは 3404490787
coap-client -m post -e "" "coap://127.0.0.1/mqtt/connection?clientid=123&username=admin&password=public"

3404490787
```

:::tip
コネクション作成成功後、ダッシュボード、REST API、CLI で CoAP ゲートウェイのクライアント一覧を確認できます。
:::

### コネクションのクローズ

`Connection Mode` でのみ利用可能です。

このインターフェースは CoAP コネクションをクローズするために使用します。

**リクエストパラメータ:**

- メソッド: `DELETE`
- URI: `mqtt/connection{?QueryString*}`、`QueryString` は以下の通りです。
  - `clientid`: 必須パラメータ、UTF-8 文字列。ゲートウェイはこの文字列をコネクションの一意識別子として使用します。
  - `token`: 必須パラメータ。`Create Connection` リクエストで返されたトークン文字列を使用。
- ペイロード: 空

**レスポンス:**

- 戻りコード:
  - `2.01`: コネクションのクローズ成功。
  - `4.00`: 不正なリクエスト。詳細なエラー情報がメッセージ本文で返されます。
  - `4.01`: 認可失敗。リクエスト形式は正しいが認可に失敗。
- ペイロード: 戻りコードが `2.01` の場合は `Token`、それ以外は `ErrorMessage`。

例:

```bash
coap-client -m delete -e "" "coap://127.0.0.1/mqtt/connection?clientid=123&token=3404490787"
```

### ハートビート

`Connection Mode` でのみ利用可能です。

このインターフェースは CoAP クライアントとゲートウェイ間のコネクション維持に使用します。ハートビートが期限切れになると、ゲートウェイはセッションとサブスクリプションを削除し、クライアントのすべてのリソースを解放します。

**リクエストパラメータ:**

- メソッド: `PUT`
- URI: `mqtt/connection{?QueryString*}`、`QueryString` は以下の通りです。
  - `clientid`: 必須パラメータ、UTF-8 文字列。ゲートウェイはこの文字列をコネクションの一意識別子として使用します。
  - `token`: 必須パラメータ。`Create Connection` リクエストで返されたトークン文字列を使用。
- ペイロード: 空

**レスポンス:**

- 戻りコード:
  - `2.01`: コネクションのクローズ成功。
  - `4.00`: 不正なリクエスト。詳細なエラー情報がメッセージ本文で返されます。
  - `4.01`: 認可失敗。リクエスト形式は正しいが認可に失敗。
- ペイロード: 戻りコードが `2.01` の場合は `Token`、それ以外は `ErrorMessage`。

例:

```bash
coap-client -m put -e "" "coap://127.0.0.1/mqtt/connection?clientid=123&token=3404490787"
```

:::tip
ハートビート間隔は CoAP ゲートウェイの `heartbeat` オプションで決定され、デフォルトは 30 秒です。
:::

### メッセージパブリッシュ

このインターフェースは CoAP クライアントが指定したトピックにメッセージを送信するために使用します。`Connection Mode` が有効な場合は追加の識別情報を含める必要があります。

**リクエストパラメータ:**

- メソッド: `POST`
- URI: `ps/{+topic}{?QueryString*}`
  - `{+topic}` はパブリッシュするメッセージのトピックです。例: `coap/test` にパブリッシュする場合、URI は `ps/coap/test` となります。
  - `{?QueryString}` はリクエストパラメータです。
    - `clientid`: `Connection Mode` では必須、`Connectionless Mode` では任意。
    - `token`: `Connection Mode` のみ必須。
    - `retain`（任意）: リテインメッセージとしてパブリッシュするかどうか。ブール値で、デフォルトは `false`。
    - `qos`: メッセージの QoS。MQTT クライアントがメッセージを受信する際の QoS レベルを指定します。`0`、`1`、`2` の列挙値。
    - `expiry`: メッセージの有効期限（秒単位）。デフォルトは 0（期限なし）。

- ペイロード: メッセージペイロード

**レスポンス:**

- 戻りコード:
  - `2.04`: パブリッシュ成功
  - `4.00`: 不正なリクエスト。詳細なエラー情報がメッセージ本文で返されます。
  - `4.01`: 認可失敗。リクエスト形式は正しいが認可に失敗。
- ペイロード: 戻りコードが `2.04` の場合は空、そうでなければ `ErrorMessage`。

例：`Connectionless Mode` でメッセージをパブリッシュする場合：

```bash
coap-client -m post -e "Hi, this is libcoap" "coap://127.0.0.1/ps/coap/test"
```

または `Connection Mode` で `clientid` と `token` を含める場合：

```bash
coap-client -m post -e "Hi, this is libcoap" "coap://127.0.0.1/ps/coap/test?clientid=123&token=3404490787"
```

### トピックサブスクライブ

このインターフェースは CoAP クライアントがトピックをサブスクライブするために使用します。`Connection Mode` が有効な場合は追加の識別情報を含める必要があります。

**リクエストパラメータ:**

- メソッド: `GET`
- オプション: `Observe` を `0` に設定。
- CoAP トークン: Observe 関係を識別するために空でないトークンを使用。
- URI: `ps/{+topic}{?QueryString*}`
  - `{+topic}` はサブスクライブするトピックです。例: `coap/test` にサブスクライブする場合、URI は `ps/coap/test` となります。
  - `{?QueryString}` はリクエストパラメータです。
    - `clientid`: `Connection Mode` では必須、`Connectionless Mode` では任意。
    - `token`: `Connection Mode` のみ必須。
    - `qos`: サブスクライブ QoS。ゲートウェイが CoAP クライアントにメッセージを配信する際に使用する MessageType (`CON` または `NON`) を示します。以下の列挙値があります。
      - `0`: `NON` メッセージを使用して配信
      - `1` または `2`: `CON` メッセージを使用して配信

- ペイロード: 空

**レスポンス:**

- 戻りコード:
  - `2.05`: サブスクライブ成功
  - `4.00`: 不正なリクエスト。詳細なエラー情報がメッセージ本文で返されます。
  - `4.01`: 認可失敗。リクエスト形式は正しいが認可に失敗。
- ペイロード: 戻りコードが `2.05` の場合は空、そうでなければ `ErrorMessage`。

例：`Connectionless Mode` で `coap/test` をサブスクライブする場合：

```bash
coap-client -m get -s 60 -O 6,0x00 -o - -T "obstoken" "coap://127.0.0.1/ps/coap/test"
```

または `Connection Mode` で `clientid` と `token` を含めてサブスクライブする場合：

```bash
coap-client -m get -s 60 -O 6,0x00 -o - -T "obstoken" "coap://127.0.0.1/ps/coap/test?clientid=123&token=3404490787"
```

`-T "obstoken"` で設定された CoAP トークンは Observe 関係を識別します。これは URI クエリパラメータの `token`（`Connection Mode` での認証用）とは別物です。

### トピックサブスクライブ解除

このインターフェースは CoAP クライアントがトピックのサブスクライブを解除するために使用します。

Observe 関係を明示的に解除するには、元の Observe トークンを用いて同じリソースに対し `Observe: 1` の GET リクエストを送信します。

現在の実装では、サブスクライブ解除操作は `Connection Mode` のみ利用可能です。

**リクエストパラメータ:**

- メソッド: `GET`
- オプション: `Observe` を `1` に設定。
- CoAP トークン: 元の Observe リクエストで使用したトークンを使用。
- URI: `ps/{+topic}{?QueryString*}`
  - `{+topic}` はサブスクライブ解除するトピックです。例: `coap/test` のサブスクライブを解除する場合、URI は `ps/coap/test` となります。
  - `{?QueryString}` はリクエストパラメータです。
    - `clientid`: `Connection Mode` では必須、`Connectionless Mode` では任意。
    - `token`: `Connection Mode` のみ必須。

- ペイロード: 空

**レスポンス:**

- 戻りコード:
  - `2.07`: サブスクライブ解除成功
  - `4.00`: 不正なリクエスト。詳細なエラー情報がメッセージ本文で返されます。
  - `4.01`: 認可失敗。リクエスト形式は正しいが認可に失敗。
- ペイロード: 戻りコードが `2.07` の場合は空、そうでなければ `ErrorMessage`。

例：`Connection Mode` で `coap/test` のサブスクライブを解除する場合：

```bash
coap-client -m get -O 6,0x01 -T "obstoken" "coap://127.0.0.1/ps/coap/test?clientid=123&token=3404490787"
```

### プロトコル仕様との違い

確認応答（`CON`）の Observe 通知がクライアントからリセットメッセージで拒否された場合、またはすべての再送試行がアックを受信せずに失敗した場合、EMQX は以下の処理を行います。

- 失敗した通知を破棄します。
- その通知に関連するトランスポートおよび Block2 状態をクリーンアップします。
- Observe 関係およびブローカーのサブスクリプションは保持します。

そのため、後続のリソース変更は保持された関係を通じて配信され続ける可能性があります。関係を保持することは失敗した通知の再配信を保証するものではありません。これらの失敗は関係のキャンセルにはなりません。通常のクライアントセッションのクリーンアップが適用されます。

この動作は互換性のために維持されており、[RFC 7641 セクション4.5](https://www.rfc-editor.org/rfc/rfc7641.html#section-4.5) の仕様とは異なります。RFC ではリセットメッセージ受信後または最終再送タイムアウト後にサーバーがオブザーバーを削除することを求めています。

### 短縮パラメータ名

メッセージサイズ削減のため、CoAP ゲートウェイは短縮パラメータ名をサポートしています。

例えば、`clientid=barx` は `c=bar` と書けます。サポートされる短縮パラメータ名は以下の通りです。

| パラメータ名   | 短縮名  |
| -------------- | ------- |
| `clientid`     | `c`     |
| `username`     | `u`     |
| `password`     | `p`     |
| `token`        | `t`     |
| `qos`          | `q`     |
| `retain`       | `r`     |
