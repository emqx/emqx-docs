# STOMPゲートウェイ

EMQX STOMPゲートウェイは、[STOMP](https://stomp.github.io/stomp-specification-1.2.html)とMQTTプロトコル間の橋渡しを行うメッセージングプロトコルトランスレーターであり、これらのプロトコルを使用するクライアント同士の通信を可能にします。

このSTOMPゲートウェイは、クライアントとサーバーに対して軽量かつシンプルなメッセージングソリューションを提供し、さまざまなメッセージング環境でのメッセージ交換を実現します。TCPおよびSSLタイプのリスナーをサポートしており、柔軟で多用途なメッセージングシステム構築ツールです。

::: tip

STOMPゲートウェイは[Stomp v1.2](https://stomp.github.io/stomp-specification-1.2.html)に基づいており、STOMP v1.0およびv1.1仕様とも互換性があります。

:::

## STOMPゲートウェイの有効化

EMQX 5では、STOMPゲートウェイはダッシュボード、REST API、および設定ファイル `base.hocon` を通じて設定および有効化できます。本節ではダッシュボードを用いた設定例を示し、操作手順を説明します。

EMQXダッシュボードの左側ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**STOMP** を探し、**Actions** 列の **Setup** をクリックすると、**Initialize STOMP** ページに遷移します。

::: tip

EMQXをクラスターで運用している場合、ダッシュボードやREST APIで行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../../guides/configuration/configuration.md)で設定してください。

:::

設定を簡略化するために、EMQXは**Gateways**ページ上のすべての必須項目にデフォルト値を用意しています。大幅なカスタマイズが不要であれば、3クリックでSTOMPゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. 続いて表示される **Listeners** タブでは、EMQXがポート `61613` でUDPリスナーを事前設定しています。ここでも **Next** をクリックして設定を確定します。
3. 最後に **Enable** ボタンをクリックしてSTOMPゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、STOMPゲートウェイのステータスが **Enabled** と表示されていることを確認できます。

<img src="./assets/STOMP-enabled.png" alt="STOMPゲートウェイ有効化" style="zoom:50%;" />

EMQX 5.0では、STOMPゲートウェイはダッシュボードを通じて設定および有効化が可能です。

上記の設定はREST APIでも行えます。

**例:**

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateways/stomp' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "stomp",
  "enable": true,
  "mountpoint": "stomp/",
  "listeners": [
    {
      "type": "tcp",
      "name": "default",
      "bind": "61613",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```

## STOMPクライアントとの連携

### クライアントライブラリ

STOMPゲートウェイを構築した後は、STOMPクライアントツールを使って接続テストを行い、正常に動作することを確認できます。例えば、[stomp.py](https://github.com/jasonrbriggs/stomp.py)などがあります。

### パブリッシュ／サブスクライブ

STOMPプロトコルはPUB/SUBメッセージングモデルに完全対応しており、STOMPゲートウェイは以下のように動作します。

- STOMPプロトコルの `SEND` メッセージをメッセージパブリッシュに使用します。`SEND` メッセージの `destination` フィールドがトピックを指定し、メッセージ内容は `SEND` メッセージのボディに含まれます。QoSは固定で0です。
- STOMPプロトコルの `SUBSCRIBE` メッセージをサブスクライブ要求に使用します。`SUBSCRIBE` メッセージの `destination` フィールドがトピックを指定します。QoSは固定で0であり、MQTTプロトコルで定義されたワイルドカードもサポートします。
- STOMPプロトコルの `UNSUBSCRIBE` メッセージをサブスクライブ解除要求に使用します。`UNSUBSCRIBE` メッセージの `destination` フィールドがトピックを指定します。

## STOMPゲートウェイのカスタマイズ

デフォルト設定に加え、EMQXはさまざまな設定オプションを提供しており、特定のビジネス要件により適した構成が可能です。本節では**Gateways**ページで利用可能な各種フィールドについて詳しく解説します。

### 基本設定

**Basic Configuration** タブでは、許容する最大ヘッダー数、ヘッダー長の最大値、統計収集の有効化、ゲートウェイのMountPoint文字列の設定が可能です。以下に各項目の詳細を示します。

<!--スクリーンショットは後日追加予定-->

1. **Max Header**：許容するSTOMPヘッダーの最大数を設定します。デフォルトは `10` です。

2. **Max Each Header Length**：STOMPヘッダー名と値の合計長の最大値をバイト単位で設定します。デフォルトは `1024` バイトです。

3. **Max Body Length**：STOMPフレームのボディの最大サイズをバイト単位で設定します。デフォルトは `65536` バイトです。

4. **Idle Timeout**：非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）を設定します。

5. **Enable Statistics**：ゲートウェイの統計収集および報告を許可するかどうかを設定します。デフォルトは `true`、選択肢は `true` または `false` です。

6. **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。これにより異なるプロトコル間でのメッセージルーティングの分離を実現できます。例：`stomp/`

   **注意**：このトピックプレフィックスはゲートウェイが管理しており、クライアントはパブリッシュやサブスクライブ時に明示的にこのプレフィックスを付加する必要はありません。

### STOMPトランザクションの制限 <Badge type="tip" text="6.0.5+" />

`gateway.stomp.transaction` を使って、各STOMP接続が保持するトランザクション状態を制限できます。トランザクション設定が省略された場合、EMQXは以下のデフォルト値を適用します。

| オプション | デフォルト | 説明 |
| --- | --- | --- |
| `max_transactions` | `100` | 1接続あたり同時に開けるトランザクションの最大数 |
| `max_actions_per_transaction` | `1000` | 1トランザクション内にバッファできる `SEND`、`ACK`、`NACK` フレームの最大数 |
| `max_retained_bytes` | `16777216` バイト | 1接続あたりすべての開いているトランザクションが保持する最大合計バイト数 |
| `timeout` | `60s` | 開いたトランザクションが期限切れとなりEMQXが破棄するまでの時間（0より大きい必要あり） |

設定されたトランザクション数、バッファフレーム数、または保持バイト数の制限を超えた場合、EMQXはSTOMPの `ERROR` フレームを送信し、接続を切断します。

### リスナーの追加

ポート `61613` に名前が **default** のtcpリスナーが既に設定されており、最大16個のアクセプターをプールで管理し、最大1,024,000の同時接続をサポートしています。より詳細な設定を行う場合は **Settings** をクリックし、リスナーを削除する場合は **Delete** をクリックします。新規リスナーを追加する場合は **+ Add Listener** をクリックしてください。

::: tip

STOMPゲートウェイはTCPおよびSSLタイプのリスナーのみをサポートしています。

:::

**Add Listener** をクリックすると **Add Listener** ページが開き、以下の設定項目を続けて設定できます。

**基本設定**

- **Name**：リスナーの一意識別子を設定します。
- **Type**：プロトコルタイプを選択します。STOMPの場合、**tcp** または **ssl** のいずれかです。
- **Bind**：リスナーが受け付ける接続のポート番号を設定します。
- **MountPoint**（任意）：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティング分離を実現します。

**リスナー設定**

- **Acceptor**：アクセプタープールのサイズを設定します。デフォルトは **16** です。
- **Max Connections**：リスナーが処理可能な同時接続数の最大値を設定します。デフォルトは **1024000** です。
- **Max Connection Rate**：リスナーが1秒あたりに受け入れ可能な新規接続の最大レートを設定します。デフォルトは **1000** です。
- **Proxy Protocol**：EMQXが[ロードバランサー](../../guides/cluster/lb.md)の背後にある場合、プロトコルV1/V2を有効にします。
- **Proxy Protocol Timeout**：非アクティブ状態でプロキシプロトコルパッケージを待機する最大時間（秒）を設定します。デフォルトは **3秒** です。

**TCP設定**

- **ActiveN**：ソケットの `{active, N}` オプションを設定します。これはソケットが能動的に処理可能な受信パケット数を意味します。詳細は[Erlangドキュメント - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2)を参照してください。
- **Buffer**：受信および送信パケットを格納するバッファサイズをKB単位で設定します。
- **TCP_NODELAY**：接続に対して `TCP_NODELAY` フラグを有効にするかどうかを設定します。これはクライアントが前のデータのアックを待たずに追加データを送信できるかを意味します。デフォルトは **false**、選択肢は **true** または **false** です。
- **SO_REUSEADDR**：ローカルのポート番号再利用を許可するかどうかを設定します。
- **Send Timeout**：非アクティブ状態で送信タイムアウトになるまでの最大待機時間（秒）を設定します。デフォルトは **15秒** です。
- **Send Timeout**：送信タイムアウト時に接続を切断するかどうかを設定します。

**SSL設定**（SSLリスナーのみ）

TLS検証の有効化はトグルスイッチで設定可能ですが、その前に関連する **TLS Cert**、**TLS Key**、および **CA Cert** の情報をファイル内容の入力または **Select File** ボタンでアップロードして設定する必要があります。詳細は[SSL/TLS接続の有効化](../../guides/network/emqx-mqtt-tls.md)を参照してください。

続いて以下の設定が可能です。

- **SSL Versions**：サポートするSSLバージョンを設定します。デフォルトは **tlsv1.3**、**tlsv1.2**、**tlsv1.1**、**tlsv1** です。
- **Fail If No Peer Cert**：クライアントが空の証明書を送信した場合にEMQXが接続を拒否するかどうかを設定します。デフォルトは **false**、選択肢は **true** または **false** です。
- **Intermediate Certificate Depth**：ピア証明書に続く有効な認証経路に含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは **10** です。
- **Key Password**：プライベートキーがパスワード保護されている場合に使用するパスワードを設定します。

## 認証の設定

STOMPプロトコルの接続メッセージにはユーザー名とパスワードの概念が既に定義されているため、STOMPは以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../../guides/access-control/authn/mnesia.md)
- [MySQL認証](../../guides/access-control/authn/mysql.md)
- [MongoDB認証](../../guides/access-control/authn/mongodb.md)
- [PostgreSQL認証](../../guides/access-control/authn/postgresql.md)
- [Redis認証](../../guides/access-control/authn/redis.md)
- [HTTPサーバー認証](../../guides/access-control/authn/http.md)
- [JWT認証](../../guides/access-control/authn/jwt.md)
- [LDAP認証](../../guides/access-control/authn/ldap.md)

STOMPゲートウェイはSTOMPプロトコルの `CONNECT` または `STOMP` メッセージ内の情報を用いてクライアントの認証情報を生成します。

- クライアントID：ランダム生成された文字列
- ユーザー名：`CONNECT` または `STOMP` メッセージヘッダーの `login` フィールドの値
- パスワード：`CONNECT` または `STOMP` メッセージヘッダーの `passcode` フィールドの値

REST APIを使って、STOMPゲートウェイ用の組み込みデータベース認証を作成することも可能です。

**例:**

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/stomp/authentication' \
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

MQTTプロトコルとは異なり、**ゲートウェイは認証器の作成のみをサポートし、認証器のリスト（または認証チェーン）はサポートしません**。

認証器が有効化されていない場合、すべてのSTOMPクライアントはログインが許可されます。

:::
