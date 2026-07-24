# STOMPゲートウェイ

EMQX STOMPゲートウェイは、[STOMP](https://stomp.github.io/stomp-specification-1.2.html)とMQTTプロトコル間のギャップを橋渡しするメッセージングプロトコルトランスレーターであり、これらのプロトコルを使用するクライアント同士の通信を可能にします。

このSTOMPゲートウェイは、クライアントとサーバーに対して軽量かつシンプルなメッセージングソリューションを提供し、さまざまなメッセージング環境でのメッセージ交換を実現します。TCPおよびSSLタイプのリスナーをサポートしているため、STOMPゲートウェイは柔軟で多用途なメッセージングシステム構築ツールです。

::: tip

STOMPゲートウェイは[Stomp v1.2](https://stomp.github.io/stomp-specification-1.2.html)をベースにしており、STOMP v1.0およびv1.1仕様と互換性があります。

:::

## STOMPゲートウェイの有効化

EMQX 5では、STOMPゲートウェイはダッシュボード、REST API、および設定ファイル `base.hocon` を通じて設定および有効化できます。本節では、ダッシュボードを使った設定例をもとに操作手順を説明します。

EMQXダッシュボードの左ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**STOMP** を探し、**Actions** 列の **Setup** をクリックすると、**Initialize STOMP** ページに遷移します。

::: tip

EMQXをクラスターで稼働している場合、ダッシュボードまたはREST APIで行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../../guides/configuration/configuration.md)で設定してください。

:::

設定を簡略化するために、EMQXは**Gateways**ページ上のすべての必須フィールドにデフォルト値を用意しています。大幅なカスタマイズが不要な場合、STOMPゲートウェイは以下の3クリックで有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. 次に **Listeners** タブに遷移し、EMQXがポート `61613` でUDPリスナーを事前設定しています。ここでも **Next** をクリックして設定を確認します。
3. 最後に **Enable** ボタンをクリックしてSTOMPゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、STOMPゲートウェイのステータスが **Enabled** と表示されていることを確認できます。

<img src="./assets/STOMP-enabled.png" alt="STOMPゲートウェイ有効化済み" style="zoom:50%;" />

EMQX 5.0では、ダッシュボードを通じてStompゲートウェイの設定と有効化が可能です。

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

STOMPゲートウェイを構築した後、STOMPクライアントツールを使用して接続テストを行い、正常に動作していることを確認できます。以下は推奨されるSTOMPクライアントツールの例です。

- [erlang-stomp-client](https://github.com/KodiEhf/erlang-stomp-client)
- [stomp.py](https://github.com/jasonrbriggs/stomp.py)

### パブリッシュ／サブスクライブ

STOMPプロトコルはPUB/SUBメッセージングモデルと完全に互換性があり、STOMPゲートウェイは以下のように動作します。

- STOMPプロトコルの `SEND` メッセージをメッセージのパブリッシュに使用します。`SEND` メッセージの `destination` フィールドがトピックを指定し、メッセージ内容は `SEND` メッセージのボディに含まれます。QoS（サービス品質）は固定で0です。
- STOMPプロトコルの `SUBSCRIBE` メッセージをサブスクライブ要求に使用します。`SUBSCRIBE` メッセージの `destination` フィールドがトピックを指定します。QoSは固定で0であり、MQTTプロトコルで定義されるワイルドカードをサポートします。
- STOMPプロトコルの `UNSUBSCRIBE` メッセージをサブスクライブ解除要求に使用します。`UNSUBSCRIBE` メッセージの `destination` フィールドがトピックを指定します。

## STOMPゲートウェイのカスタマイズ

デフォルト設定に加え、EMQXはさまざまな設定オプションを提供し、特定のビジネス要件により適合させることができます。本節では、**Gateways** ページで利用可能な各種フィールドについて詳しく解説します。

### 基本設定

**Basic Configuration** タブでは、許容される最大ヘッダー数、ヘッダーの最大長、統計情報の有効化設定、ゲートウェイのMountPoint文字列の設定が可能です。以下に各フィールドの詳細を示します。

<!--後日スクリーンショット追加予定-->

1. **Max Header**：許容されるSTOMPヘッダーの最大数を設定します。デフォルトは `10`。
2. **Max Each Header Length**：ヘッダー値の最大文字列長を設定します。デフォルトは `1024`。
3. **Max Body Length**：STOMPパケットの最大バイト数を設定します。デフォルトは `65536`。
4. **Idle Timeout**：非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）を設定します。
5. **Enable Statistics**：ゲートウェイが統計情報を収集・報告するかどうかを設定します。デフォルトは `true`、選択肢は `true` または `false`。
6. **MountPoint**：パブリッシュおよびサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定します。これにより、異なるプロトコル間でのメッセージルーティングの分離を実現できます。例：`stomp/`。

   **注意**：このトピックプレフィックスはゲートウェイが管理しており、クライアントはパブリッシュやサブスクライブ時に明示的にこのプレフィックスを付加する必要はありません。

### リスナーの追加

ポート `61613` で名前が **default** のtcpリスナーがすでに設定されており、プール内の最大16個のアセプターを許容し、最大1,024,000の同時接続をサポートします。より詳細な設定を行う場合は **Settings** をクリックし、リスナーを削除する場合は **Delete** をクリック、新しいリスナーを追加する場合は **+ Add Listener** をクリックしてください。

::: tip

STOMPゲートウェイはTCPおよびSSLタイプのリスナーのみをサポートしています。

:::

**Add Listener** をクリックすると **Add Listener** ページが開き、以下の設定を行えます。

**基本設定**

- **Name**：リスナーの一意の識別子を設定します。
- **Type**：プロトコルタイプを選択します。STOMPの場合は **tcp** または **ssl** のいずれかです。
- **Bind**：リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**（オプション）：パブリッシュおよびサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティングの分離を実現します。

**リスナー設定**

- **Acceptor**：アセプタープールのサイズを設定します。デフォルトは **16**。
- **Max Connections**：リスナーが処理可能な最大同時接続数を設定します。デフォルトは **1024000**。
- **Max Connection Rate**：リスナーが1秒あたりに受け入れ可能な新規接続の最大レートを設定します。デフォルトは **1000**。
- **Proxy Protocol**：EMQXが[ロードバランサー](../../guides/cluster/lb.md)の背後にある場合に、プロトコルV1/V2を有効化します。
- **Proxy Protocol Timeout**：非アクティブ状態でプロキシプロトコルパッケージを待機する最大時間（秒）を設定し、タイムアウト時に接続を切断します。デフォルトは **3秒**。

**TCP設定**

- **ActiveN**：ソケットの `{active, N}` オプションを設定します。これはソケットが能動的に処理できる受信パケット数を意味します。詳細は[Erlangドキュメント - setopts/2](https://erlang.org/doc/man/inet.html#setopts-2)を参照してください。
- **Buffer**：受信および送信パケットを格納するバッファサイズをKB単位で設定します。
- **TCP_NODELAY**：接続に対して `TCP_NODELAY` フラグを有効にするかどうかを設定します。これはクライアントが前回のデータのアックを待たずに追加データを送信できるかどうかを意味します。デフォルトは **false**、選択肢は **true** または **false**。
- **SO_REUSEADDR**：ローカルでのポート番号の再利用を許可するかどうかを設定します。
- **Send Timeout**：非アクティブ状態で送信タイムアウトまでの最大時間（秒）を設定し、タイムアウト時に接続を切断します。デフォルトは **15秒**。
- **Send Timeout**：送信タイムアウト時に接続を切断するかどうかを設定します。

**SSL設定**（SSLリスナーのみ）

TLS検証の有効化はトグルスイッチで設定できます。ただし、事前に関連する **TLS Cert**、**TLS Key**、および **CA Cert** の情報を、ファイル内容の入力または **Select File** ボタンによるアップロードで設定しておく必要があります。詳細は[SSL/TLS接続の有効化](../../guides/network/emqx-mqtt-tls.md)を参照してください。

続いて以下の設定が可能です。

- **SSL Versions**：サポートするSSLバージョンを設定します。デフォルトは **tlsv1.3**、**tlsv1.2**、**tlsv1.1**、および **tlsv1**。
- **Fail If No Peer Cert**：クライアントが空の証明書を送信した場合にEMQXが接続を拒否するかどうかを設定します。デフォルトは **false**、選択肢は **true** または **false**。
- **Intermediate Certificate Depth**：ピア証明書に続く有効な認証経路に含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは **10**。
- **Key Password**：プライベートキーがパスワード保護されている場合に使用するユーザーパスワードを設定します。

## 認証の設定

STOMPプロトコルの接続メッセージにはユーザー名とパスワードの概念がすでに定義されているため、STOMPは以下のような多様な認証タイプをサポートしています。

- [組み込みデータベース認証](../../guides/access-control/authn/mnesia.md)
- [MySQL認証](../../guides/access-control/authn/mysql.md)
- [MongoDB認証](../../guides/access-control/authn/mongodb.md)
- [PostgreSQL認証](../../guides/access-control/authn/postgresql.md)
- [Redis認証](../../guides/access-control/authn/redis.md)
- [HTTPサーバー認証](../../guides/access-control/authn/http.md)
- [JWT認証](../../guides/access-control/authn/jwt.md)
- [LDAP認証](../../guides/access-control/authn/ldap.md)

STOMPゲートウェイはSTOMPプロトコルの `CONNECT` または `STOMP` メッセージ内の情報を用いてクライアントの認証フィールドを生成します。

- クライアントID：ランダムに生成される文字列
- ユーザー名：`CONNECT` または `STOMP` メッセージヘッダー内の `login` フィールドの値
- パスワード：`CONNECT` または `STOMP` メッセージヘッダー内の `passcode` フィールドの値

REST APIを使用してStompゲートウェイ用の組み込みデータベース認証を作成することも可能です。

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

MQTTプロトコルとは異なり、**ゲートウェイは認証器のリスト（または認証チェーン）ではなく、認証器の作成のみをサポートします**。

認証器が有効化されていない場合、すべてのSTOMPクライアントはログインを許可されます。

:::
