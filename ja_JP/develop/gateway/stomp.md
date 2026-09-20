# STOMPゲートウェイ

EMQX STOMPゲートウェイは、[STOMP](https://stomp.github.io/stomp-specification-1.2.html)とMQTTプロトコル間の橋渡しを行うメッセージングプロトコルトランスレーターであり、これらのプロトコルを使用するクライアント同士の通信を可能にします。

このSTOMPゲートウェイは、クライアントとサーバーに対して軽量でシンプルなメッセージングソリューションを提供し、さまざまなメッセージング環境でのメッセージ交換を実現します。TCPおよびSSLタイプのリスナーをサポートしているため、STOMPゲートウェイは柔軟かつ多用途なメッセージングシステム構築ツールです。

::: tip

STOMPゲートウェイは[Stomp v1.2](https://stomp.github.io/stomp-specification-1.2.html)をベースにしており、STOMP v1.0およびv1.1仕様と互換性があります。

:::

## STOMPゲートウェイの有効化

EMQX 5では、STOMPゲートウェイはダッシュボード、REST API、および設定ファイル`base.hocon`を通じて設定および有効化できます。本節では、ダッシュボードを使った設定手順を例に操作方法を説明します。

EMQXダッシュボードの左ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways**ページにはサポートされているすべてのゲートウェイが一覧表示されます。**STOMP**を見つけ、**Actions**列の**Setup**をクリックしてください。すると、**Initialize STOMP**ページに遷移します。

::: tip

EMQXをクラスターで運用している場合、ダッシュボードやREST APIで行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../../guides/configuration/configuration.md)で設定してください。

:::

設定を簡略化するため、EMQXは**Gateways**ページのすべての必須フィールドにデフォルト値を提供しています。大幅なカスタマイズが不要な場合は、以下の3ステップでSTOMPゲートウェイを有効化できます。

1. **Basic Configuration**タブで**Next**をクリックし、すべてのデフォルト設定を受け入れます。
2. 次に**Listeners**タブに遷移し、EMQXがポート`61613`でUDPリスナーを事前設定しています。再度**Next**をクリックして設定を確認します。
3. 最後に**Enable**ボタンをクリックしてSTOMPゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways**ページに戻り、STOMPゲートウェイのステータスが**Enabled**になっていることを確認できます。

<img src="./assets/STOMP-enabled.png" alt="STOMPゲートウェイ有効化済み" style="zoom:50%;" />

EMQX 5.0では、ダッシュボードを通じてStompゲートウェイの設定および有効化が可能です。

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

STOMPゲートウェイを構築した後は、STOMPクライアントツールを使って接続テストを行い、正常に動作することを確認できます。例えば[stomp.py](https://github.com/jasonrbriggs/stomp.py)などがあります。

### パブリッシュ／サブスクライブ

STOMPプロトコルはPUB/SUBメッセージングモデルと完全に互換性があり、STOMPゲートウェイは以下のように動作します。

- STOMPプロトコルの`SEND`メッセージをメッセージパブリッシュに使用します。`SEND`メッセージの`destination`フィールドがトピックを指定し、メッセージ内容は`SEND`メッセージのボディに含まれます。QoSは固定で0です。
- STOMPプロトコルの`SUBSCRIBE`メッセージをサブスクライブ要求に使用します。`SUBSCRIBE`メッセージの`destination`フィールドがトピックを指定します。QoSは固定で0であり、MQTTプロトコルで定義されているワイルドカードもサポートします。
- STOMPプロトコルの`UNSUBSCRIBE`メッセージをサブスクライブ解除要求に使用します。`SUBSCRIBE`メッセージの`destination`フィールドがトピックを指定します。

## STOMPゲートウェイのカスタマイズ

デフォルト設定に加え、EMQXはさまざまな設定オプションを提供しており、特定のビジネス要件に合わせて調整可能です。本節では、**Gateways**ページで利用可能な各種フィールドについて詳しく解説します。

### 基本設定

**Basic Configuration**タブでは、許容する最大ヘッダー数、ヘッダー長、統計情報の有効化設定、ゲートウェイのMountPoint文字列などを設定できます。以下の説明をご参照ください。

<!--後日スクリーンショット追加予定-->

1. **Max Header**: 許容するSTOMPヘッダーの最大数を設定します。デフォルトは`10`です。

2. **Max Each Header Length**: ヘッダー値の最大文字列長を設定します。デフォルトは`1024`です。

3. **Max Body Length**: STOMPパケットの最大バイト数を設定します。デフォルトは`65536`です。

4. **Idle Timeout**: 非アクティブ状態が続いた場合にSTOMPフレーム受信を待つ最大秒数を設定し、その時間を超えると接続を切断します。

5. **Enable Statistics**: ゲートウェイが統計情報を収集・報告するかどうかを設定します。デフォルトは`true`で、選択肢は`true`または`false`です。

6. **MountPoint**: パブリッシュやサブスクライブ時にすべてのトピックに接頭辞として付与される文字列を設定します。これにより異なるプロトコル間でのメッセージルーティングの分離を実現できます。例：`stomp/`

   **注意**: このトピック接頭辞はゲートウェイ側で管理されるため、クライアントはパブリッシュやサブスクライブ時に明示的にこの接頭辞を付ける必要はありません。

### リスナーの追加

ポート`61613`で名前が**default**のtcpリスナーがすでに設定されており、最大16個のアクセプターをプールし、最大1,024,000の同時接続をサポートします。より詳細な設定を行う場合は**Settings**をクリックし、リスナーを削除する場合は**Delete**をクリック、新しいリスナーを追加する場合は**+ Add Listener**をクリックしてください。

::: tip

STOMPゲートウェイはTCPおよびSSLタイプのリスナーのみをサポートしています。

:::

**Add Listener**をクリックすると**Add Listener**ページが開き、以下の設定が可能です。

**基本設定**

- **Name**: リスナーの一意識別子を設定します。
- **Type**: プロトコルタイプを選択します。STOMPの場合は**tcp**または**ssl**が選べます。
- **Bind**: リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**（任意）: パブリッシュやサブスクライブ時にすべてのトピックに付与される接頭辞文字列を設定し、異なるプロトコル間でのメッセージルーティング分離を実現します。

**リスナー設定**

- **Acceptor**: アクセプタープールのサイズを設定します。デフォルトは**16**です。
- **Max Connections**: リスナーが処理可能な最大同時接続数を設定します。デフォルトは**1024000**です。
- **Max Connection Rate**: リスナーが1秒あたりに受け入れ可能な新規接続の最大レートを設定します。デフォルトは**1000**です。
- **Proxy Protocol**: EMQXが[ロードバランサー](../../guides/cluster/lb.md)の背後にある場合に、プロトコルV1/V2を有効化します。
- **Proxy Protocol Timeout**: プロキシプロトコルパッケージを待機する最大秒数を設定し、超過すると接続を切断します。デフォルトは**3秒**です。

**TCP設定**

- **ActiveN**: ソケットの`{active, N}`オプションを設定します。これはソケットが能動的に処理可能な受信パケット数を示します。詳細は[Erlangドキュメント - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2)を参照してください。
- **Buffer**: 受信および送信パケットを格納するバッファサイズをKB単位で設定します。
- **TCP_NODELAY**: 接続に対して`TCP_NODELAY`フラグを有効にするかどうかを設定します。これはクライアントが前のデータのアックを待たずに追加データを送信できるかを制御します。デフォルトは**false**で、選択肢は**true**または**false**です。
- **SO_REUSEADDR**: ローカルポート番号の再利用を許可するかどうかを設定します。
- **Send Timeout**: 送信タイムアウトの最大秒数を設定し、超過すると接続を切断します。デフォルトは**15秒**です。
- **Send Timeout**: 送信タイムアウト時に接続を閉じるかどうかを設定します。

**SSL設定**（SSLリスナーのみ）

TLS検証の有効化はトグルスイッチで設定可能です。ただし、その前に関連する**TLS Cert**、**TLS Key**、および**CA Cert**情報をファイルの内容を入力するか、**Select File**ボタンでアップロードして設定する必要があります。詳細は[SSL/TLS接続の有効化](../../guides/network/emqx-mqtt-tls.md)を参照してください。

続いて以下を設定できます。

- **SSL Versions**: サポートするSSLバージョンを設定します。デフォルトは**tlsv1.3**、**tlsv1.2**、**tlsv1.1**、および**tlsv1**です。
- **Fail If No Peer Cert**: クライアントが空の証明書を送信した場合にEMQXが接続を拒否するかどうかを設定します。デフォルトは**false**で、選択肢は**true**または**false**です。
- **Intermediate Certificate Depth**: ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは**10**です。
- **Key Password**: プライベートキーがパスワード保護されている場合に使用するユーザーパスワードを設定します。

## 認証の設定

STOMPプロトコルの接続メッセージにはすでにユーザー名とパスワードの概念が定義されているため、STOMPゲートウェイは以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../../guides/access-control/authn/mnesia.md)
- [MySQL認証](../../guides/access-control/authn/mysql.md)
- [MongoDB認証](../../guides/access-control/authn/mongodb.md)
- [PostgreSQL認証](../../guides/access-control/authn/postgresql.md)
- [Redis認証](../../guides/access-control/authn/redis.md)
- [HTTPサーバー認証](../../guides/access-control/authn/http.md)
- [JWT認証](../../guides/access-control/authn/jwt.md)
- [LDAP認証](../../guides/access-control/authn/ldap.md)

STOMPゲートウェイはSTOMPプロトコルの`CONNECT`または`STOMP`メッセージ内の情報を使ってクライアントの認証フィールドを生成します。

- クライアントID：ランダムに生成される文字列
- ユーザー名：`CONNECT`または`STOMP`メッセージヘッダーの`login`フィールドの値
- パスワード：`CONNECT`または`STOMP`メッセージヘッダーの`passcode`フィールドの値

REST APIを使ってStompゲートウェイ用の組み込みデータベース認証を作成することも可能です。

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

MQTTプロトコルとは異なり、**ゲートウェイは認証器のリスト（または認証チェーン）ではなく、認証器の作成のみをサポートしています**。

認証器が有効化されていない場合、すべてのSTOMPクライアントのログインが許可されます。

:::
