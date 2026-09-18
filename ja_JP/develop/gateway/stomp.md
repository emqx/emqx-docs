# STOMP ゲートウェイ

EMQX STOMP ゲートウェイは、[STOMP](https://stomp.github.io/stomp-specification-1.2.html) と MQTT プロトコル間の橋渡しを行うメッセージングプロトコルトランスレーターであり、これらのプロトコルを使用するクライアント同士の通信を可能にします。

この STOMP ゲートウェイは、クライアントとサーバーに対して軽量かつシンプルなメッセージングソリューションを提供し、さまざまなメッセージング環境でのメッセージ交換を実現します。TCP および SSL タイプのリスナーをサポートしているため、STOMP ゲートウェイは柔軟で多用途なメッセージングシステム構築ツールです。

::: tip

STOMP ゲートウェイは [Stomp v1.2](https://stomp.github.io/stomp-specification-1.2.html) をベースとしており、STOMP v1.0 および v1.1 の仕様とも互換性があります。

:::

## STOMP ゲートウェイの有効化

EMQX 5 では、STOMP ゲートウェイはダッシュボード、REST API、および設定ファイル `base.hocon` を通じて設定および有効化が可能です。本節では、ダッシュボードを使った設定例を示し、操作手順を説明します。

EMQX ダッシュボードの左ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**STOMP** を見つけ、**Actions** 列の **Setup** をクリックすると、**Initialize STOMP** ページに遷移します。

::: tip

EMQX をクラスターで稼働させている場合、ダッシュボードや REST API で行った設定はクラスター全体に影響します。特定のノードだけ設定を変更したい場合は、[`base.hocon`](../../guides/configuration/configuration.md) で設定してください。

:::

設定を簡略化するために、EMQX は **Gateways** ページのすべての必須項目にデフォルト値を用意しています。大幅なカスタマイズが不要な場合は、以下の3ステップで STOMP ゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. 続いて表示される **Listeners** タブでは、EMQX がポート `61613` で UDP リスナーを事前設定しています。ここでも **Next** をクリックして設定を確定します。
3. 最後に **Enable** ボタンをクリックして STOMP ゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、STOMP ゲートウェイのステータスが **Enabled** と表示されていることを確認できます。

<img src="./assets/STOMP-enabled.png" alt="STOMP ゲートウェイ有効化" style="zoom:50%;" />

EMQX 5.0 では、ダッシュボードを通じて Stomp ゲートウェイの設定および有効化が可能です。

上記の設定は REST API でも行えます。

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

## STOMP クライアントとの連携

### クライアントライブラリ

STOMP ゲートウェイを構築した後は、STOMP クライアントツールを用いて接続テストを行い、正常に動作することを確認できます。以下は推奨される STOMP クライアントツールの例です。

- [erlang-stomp-client](https://github.com/KodiEhf/erlang-stomp-client)
- [stomp.py](https://github.com/jasonrbriggs/stomp.py)

### パブリッシュ／サブスクライブ

STOMP プロトコルは PUB/SUB メッセージングモデルと完全に互換性があり、STOMP ゲートウェイは以下のように動作します。

- STOMP プロトコルの `SEND` メッセージをメッセージのパブリッシュに使用します。`SEND` メッセージの `destination` フィールドがトピックを指定し、メッセージ内容は `SEND` メッセージのボディに含まれます。QoS（サービス品質）は固定で 0 です。
- STOMP プロトコルの `SUBSCRIBE` メッセージをサブスクライブ要求に使用します。`SUBSCRIBE` メッセージの `destination` フィールドがトピックを指定します。QoS は固定で 0 であり、MQTT プロトコルで定義されたワイルドカードもサポートします。
- STOMP プロトコルの `UNSUBSCRIBE` メッセージをサブスクライブ解除要求に使用します。`UNSUBSCRIBE` メッセージの `destination` フィールドがトピックを指定します。

## STOMP ゲートウェイのカスタマイズ

デフォルト設定に加え、EMQX はさまざまな設定オプションを提供しており、特定のビジネス要件に合わせて柔軟に対応できます。本節では **Gateways** ページで利用可能な各種フィールドについて詳しく解説します。

### 基本設定

**Basic Configuration** タブでは、許容される最大ヘッダー数やヘッダー長、統計情報の有効化、ゲートウェイの MountPoint 文字列設定などが行えます。以下に各フィールドの詳細を示します。

<!--スクリーンショットは後日追加予定-->

1. **Max Header**: 許容される最大の STOMP ヘッダー数を設定します。デフォルトは `10` です。

2. **Max Each Header Length**: ヘッダー値の最大文字列長を設定します。デフォルトは `1024` です。

3. **Max Body Length**: STOMP パケットの最大バイト数を設定します。デフォルトは `65536` です。

4. **Idle Timeout**: 非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）を設定します。

5. **Enable Statistics**: ゲートウェイが統計情報を収集・報告するかどうかを設定します。デフォルトは `true`、選択肢は `true` または `false` です。

6. **MountPoint**: パブリッシュやサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定します。これにより異なるプロトコル間でのメッセージルーティングの分離が可能です。例: *stomp/*。

   **注意**: このトピックプレフィックスはゲートウェイ側で管理されるため、クライアントはパブリッシュやサブスクライブ時に明示的にこのプレフィックスを付加する必要はありません。

### リスナーの追加

ポート `61613` で名前が **default** の tcp リスナーがすでに設定されており、最大 16 個のアセプターをプールで管理し、最大 1,024,000 の同時接続をサポートしています。より詳細な設定を行うには **Settings** をクリックし、リスナーを削除する場合は **Delete** をクリック、新規リスナー追加は **+ Add Listener** をクリックします。

::: tip

STOMP ゲートウェイは TCP および SSL タイプのリスナーのみをサポートしています。

:::

**Add Listener** をクリックすると **Add Listener** ページが開き、以下の設定項目を入力できます。

**基本設定**

- **Name**: リスナーの一意識別子を設定します。
- **Type**: プロトコルタイプを選択します。STOMP では **tcp** または **ssl** のいずれかを指定します。
- **Bind**: リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**（任意）: パブリッシュやサブスクライブ時にすべてのトピックの先頭に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティングの分離を実現します。

**リスナー設定**

- **Acceptor**: アセプタープールのサイズを設定します。デフォルトは **16**。
- **Max Connections**: リスナーが処理可能な最大同時接続数を設定します。デフォルトは **1024000**。
- **Max Connection Rate**: リスナーが1秒あたりに受け入れる新規接続の最大レートを設定します。デフォルトは **1000**。
- **Proxy Protocol**: EMQX が [ロードバランサー](../../guides/cluster/lb.md) の背後に配置されている場合に、プロトコル V1/V2 を有効化します。
- **Proxy Protocol Timeout**: プロキシプロトコルパッケージの受信を待機する最大時間（秒）を設定し、タイムアウト時に接続を切断します。デフォルトは **3秒**。

**TCP 設定**

- **ActiveN**: ソケットの `{active, N}` オプションを設定します。これはソケットが能動的に処理可能な受信パケット数を示します。詳細は [Erlang ドキュメント - setopts/2](https://erlang.org/doc/man/inet.html#setopts-2) を参照してください。
- **Buffer**: 受信および送信パケットを格納するバッファサイズを KB 単位で設定します。
- **TCP_NODELAY**: 接続に対して `TCP_NODELAY` フラグを有効にするかどうかを設定します。これはクライアントが前のデータのアックを待たずに追加データを送信できるかを制御します。デフォルトは **false**、選択肢は **true** または **false** です。
- **SO_REUSEADDR**: ローカルポート番号の再利用を許可するかどうかを設定します。
- **Send Timeout**: 送信タイムアウト時間（秒）を設定し、タイムアウト時に接続を切断します。デフォルトは **15秒**。
- **Send Timeout**: 送信タイムアウト時に接続を切断するかどうかを設定します。

**SSL 設定**（SSL リスナーのみ）

TLS 検証の有効化はトグルスイッチで設定可能です。ただし、その前に関連する **TLS Cert**、**TLS Key**、および **CA Cert** の情報をファイル内容の入力または **Select File** ボタンによるアップロードで設定する必要があります。詳細は [SSL/TLS 接続の有効化](../../guides/network/emqx-mqtt-tls.md) を参照してください。

続いて以下の設定が可能です。

- **SSL Versions**: サポートする SSL バージョンを設定します。デフォルトは **tlsv1.3**、**tlsv1.2**、**tlsv1.1**、**tlsv1** です。
- **Fail If No Peer Cert**: クライアントが空の証明書を送信した場合に EMQX が接続を拒否するかどうかを設定します。デフォルトは **false**、選択肢は **true** または **false** です。
- **Intermediate Certificate Depth**: ピア証明書に続く有効な認証パスに含まれる非自己発行中間証明書の最大数を設定します。デフォルトは **10**。
- **Key Password**: プライベートキーがパスワード保護されている場合に使用するユーザーパスワードを設定します。

## 認証の設定

STOMP プロトコルの接続メッセージにはユーザー名とパスワードの概念が既に定義されているため、STOMP は以下のようなさまざまな認証方式をサポートしています。

- [組み込みデータベース認証](../../guides/access-control/authn/mnesia.md)
- [MySQL 認証](../../guides/access-control/authn/mysql.md)
- [MongoDB 認証](../../guides/access-control/authn/mongodb.md)
- [PostgreSQL 認証](../../guides/access-control/authn/postgresql.md)
- [Redis 認証](../../guides/access-control/authn/redis.md)
- [HTTP サーバー認証](../../guides/access-control/authn/http.md)
- [JWT 認証](../../guides/access-control/authn/jwt.md)
- [LDAP 認証](../../guides/access-control/authn/ldap.md)

STOMP ゲートウェイは、STOMP プロトコルの `CONNECT` または `STOMP` メッセージ内の情報を用いてクライアントの認証フィールドを生成します。

- クライアント ID: ランダムに生成される文字列
- ユーザー名: `CONNECT` または `STOMP` メッセージヘッダーの `login` フィールドの値
- パスワード: `CONNECT` または `STOMP` メッセージヘッダーの `passcode` フィールドの値

また、REST API を使って Stomp ゲートウェイ用の組み込みデータベース認証を作成することも可能です。

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

MQTT プロトコルとは異なり、**ゲートウェイでは認証器のリスト（または認証チェーン）ではなく、認証器の作成のみをサポートしています**。

認証器が有効化されていない場合、すべての STOMP クライアントはログインが許可されます。

:::
