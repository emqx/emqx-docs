# STOMPゲートウェイ

EMQX STOMPゲートウェイは、[STOMP](https://stomp.github.io/stomp-specification-1.2.html)とMQTTプロトコル間の橋渡しを行うメッセージングプロトコルトランスレーターであり、これらのプロトコルを使用するクライアント同士の通信を可能にします。

このSTOMPゲートウェイは、クライアントとサーバーのための軽量かつシンプルなメッセージングソリューションを提供し、さまざまなメッセージング環境でのメッセージ交換を実現します。TCPおよびSSLタイプのリスナーをサポートしているため、STOMPゲートウェイは柔軟で多用途なメッセージングシステム構築ツールです。

::: tip

STOMPゲートウェイは[Stomp v1.2](https://stomp.github.io/stomp-specification-1.2.html)をベースとしており、STOMP v1.0およびv1.1仕様と互換性があります。

:::

## STOMPゲートウェイの有効化

EMQX 5では、STOMPゲートウェイはダッシュボード、REST API、および設定ファイル`base.hocon`を通じて設定および有効化できます。本節では、ダッシュボードを使った設定手順を例に操作方法を説明します。

EMQXダッシュボードの左側ナビゲーションメニューで**Management** -> **Gateways**をクリックします。**Gateways**ページにはサポートされているすべてのゲートウェイが一覧表示されます。**STOMP**を見つけ、**Actions**列の**Setup**をクリックすると、**Initialize STOMP**ページに遷移します。

::: tip

EMQXをクラスターで運用している場合、ダッシュボードやREST APIを通じて行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../../guides/configuration/configuration.md)で設定してください。

:::

設定を簡略化するため、EMQXは**Gateways**ページのすべての必須フィールドにデフォルト値を提供しています。大幅なカスタマイズが不要な場合は、3クリックでSTOMPゲートウェイを有効化できます。

1. **Basic Configuration**タブで**Next**をクリックし、すべてのデフォルト設定を受け入れます。
2. 次に**Listeners**タブに遷移し、EMQXがポート`61613`でUDPリスナーを事前設定しています。設定を確認して**Next**をクリックします。
3. 最後に**Enable**ボタンをクリックしてSTOMPゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways**ページに戻り、STOMPゲートウェイのステータスが**Enabled**と表示されていることを確認できます。

<img src="./assets/STOMP-enabled.png" alt="STOMPゲートウェイ有効化" style="zoom:50%;" />

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

STOMPゲートウェイを構築した後は、STOMPクライアントツールを使用して接続テストを行い、正常に動作することを確認できます。例えば、[stomp.py](https://github.com/jasonrbriggs/stomp.py)などがあります。

### パブリッシュ／サブスクライブ

STOMPプロトコルはPUB/SUBメッセージングモデルと完全に互換性があり、STOMPゲートウェイは以下を使用します。

- STOMPプロトコルの`SEND`メッセージをメッセージのパブリッシュに利用します。`SEND`メッセージの`destination`フィールドがトピックを指定し、メッセージ内容は`SEND`メッセージのボディに含まれます。QoS（サービス品質）は固定で0です。
- STOMPプロトコルの`SUBSCRIBE`メッセージをサブスクライブ要求に利用します。`SUBSCRIBE`メッセージの`destination`フィールドがトピックを指定します。QoSは固定で0であり、MQTTプロトコルで定義されたワイルドカードをサポートします。
- STOMPプロトコルの`UNSUBSCRIBE`メッセージをサブスクライブ解除要求に利用します。`UNSUBSCRIBE`メッセージの`destination`フィールドがトピックを指定します。

## STOMPゲートウェイのカスタマイズ

デフォルト設定に加え、EMQXはさまざまな設定オプションを提供し、特定のビジネス要件に合わせて調整可能です。本節では**Gateways**ページで利用可能な各種フィールドについて詳しく解説します。

### 基本設定

**Basic Configuration**タブでは、許容する最大ヘッダー数、ヘッダー長の最大値、統計情報の有効化、ゲートウェイのMountPoint文字列設定などが行えます。以下の説明をご参照ください。

<!--スクリーンショットは後日追加予定-->

1. **Max Header**：許容するSTOMPヘッダーの最大数を設定します。デフォルトは`10`です。

2. **Max Each Header Length**：STOMPヘッダー名と値の合計長の最大値をバイト単位で設定します。デフォルトは`1024`バイトです。

3. **Max Body Length**：STOMPフレームのボディの最大サイズをバイト単位で設定します。デフォルトは`65536`バイトです。

4. **Idle Timeout**：非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）を設定します。

5. **Enable Statistics**：ゲートウェイが統計情報を収集・報告するかどうかを設定します。デフォルトは`true`で、選択肢は`true`または`false`です。

6. **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。これにより異なるプロトコル間でのメッセージルーティングの分離が可能になります。例：`stomp/`

   **注意**：このトピックプレフィックスはゲートウェイ側で管理されるため、クライアントはパブリッシュやサブスクライブ時に明示的にこのプレフィックスを付ける必要はありません。

### STOMPトランザクションの制限 <Badge type="tip" text="6.0.5+" />

`gateway.stomp.transaction`を使用して、各STOMP接続が保持するトランザクション状態を制限できます。トランザクション設定が省略された場合、EMQXは以下のデフォルトを適用します。

| オプション | デフォルト | 説明 |
| --- | --- | --- |
| `max_transactions` | `100` | 1接続あたり同時に開けるトランザクションの最大数 |
| `max_actions_per_transaction` | `1000` | 1トランザクション内でバッファリング可能な`SEND`、`ACK`、`NACK`フレームの最大数 |
| `max_retained_bytes` | `16777216`バイト | 1接続あたり開いているすべてのトランザクションが保持可能な最大バイト数合計 |
| `timeout` | `60s` | 開いたトランザクションが期限切れとなりEMQXが破棄するまでの時間。0より大きい値である必要があります。 |

設定値を超過した場合、EMQXはSTOMPの`ERROR`フレームを送信し、接続を切断します。

### リスナーの追加

ポート`61613`で名前が**default**のtcpリスナーがすでに設定されており、プール内のアセプター数は最大16、同時接続数は最大1,024,000まで対応可能です。より詳細な設定は**Settings**をクリックして行えます。リスナーの削除は**Delete**、新規リスナー追加は**+ Add Listener**をクリックしてください。

::: tip

STOMPゲートウェイはTCPおよびSSLタイプのリスナーのみサポートしています。

:::

**Add Listener**をクリックすると**Add Listener**ページが開き、以下の設定を行えます。

**基本設定**

- **Name**：リスナーの一意識別子を設定します。
- **Type**：プロトコルタイプを選択します。STOMPでは**tcp**または**ssl**を選択可能です。
- **Bind**：リスナーが接続受付を行うポート番号を設定します。
- **MountPoint**（任意）：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティング分離を実現します。

**リスナー設定**

- **Acceptor**：アセプタープールのサイズを設定します。デフォルトは**16**です。
- **Max Connections**：リスナーが処理可能な同時接続数の最大値を設定します。デフォルトは**1024000**です。
- **Max Connection Rate**：リスナーが1秒あたり受け入れ可能な新規接続数の最大値を設定します。デフォルトは**1000**です。
- **Proxy Protocol**：EMQXが[ロードバランサー](../../guides/cluster/lb.md)の背後に配置されている場合に、プロトコルV1/V2を有効化します。
- **Proxy Protocol Timeout**：非アクティブ状態で接続を切断するまでの、プロキシプロトコルパッケージ待機時間（秒）を設定します。デフォルトは**3秒**です。

**TCP設定**

- **ActiveN**：ソケットの`{active, N}`オプションを設定します。これはソケットが積極的に処理可能な受信パケット数を示します。詳細は[Erlangドキュメント - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2)をご参照ください。
- **Buffer**：受信および送信パケットを格納するバッファサイズをKB単位で設定します。
- **TCP_NODELAY**：`TCP_NODELAY`フラグを有効にするかどうかを設定します。これはクライアントが前回のデータのアック（ACK）を待たずに追加データを送信できるかを制御します。デフォルトは**false**で、選択肢は**true**または**false**です。
- **SO_REUSEADDR**：ローカルでのポート番号再利用を許可するかどうかを設定します。
- **Send Timeout**：非アクティブ状態で接続を切断するまでの送信タイムアウト時間（秒）を設定します。デフォルトは**15秒**です。
- **Send Timeout**：送信タイムアウト時に接続を切断するかどうかを設定します。

**SSL設定**（SSLリスナーのみ）

TLS検証を有効にするかどうかをトグルスイッチで設定できます。ただし、その前に関連する**TLS Cert**、**TLS Key**、および**CA Cert**情報をファイルの内容を直接入力するか、**Select File**ボタンでアップロードして設定する必要があります。詳細は[SSL/TLS接続の有効化](../../guides/network/emqx-mqtt-tls.md)をご参照ください。

続いて以下の設定を行えます。

- **SSL Versions**：サポートするSSLバージョンを設定します。デフォルトは**tlsv1.3**、**tlsv1.2**、**tlsv1.1**、**tlsv1**です。
- **Fail If No Peer Cert**：クライアントが空の証明書を送信した場合に接続を拒否するかどうかを設定します。デフォルトは**false**で、選択肢は**true**または**false**です。
- **Intermediate Certificate Depth**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは**10**です。
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

STOMPゲートウェイは、STOMPプロトコルの`CONNECT`または`STOMP`メッセージ内の情報を用いてクライアントの認証情報を生成します。

- クライアントID：ランダム生成された文字列
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

認証器が有効化されていない場合、すべてのSTOMPクライアントはログインが許可されます。

:::
