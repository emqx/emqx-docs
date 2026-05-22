# STOMP ゲートウェイ

EMQX STOMP ゲートウェイは、[STOMP](https://stomp.github.io/stomp-specification-1.2.html) と MQTT プロトコル間のギャップを埋めるメッセージングプロトコルトランスレーターであり、これらのプロトコルを使用するクライアント同士の通信を可能にします。

<<<<<<< HEAD
この STOMP ゲートウェイは、クライアントとサーバーに対して軽量でシンプルなメッセージングソリューションを提供し、さまざまなメッセージング環境でのメッセージ交換を実現します。TCP および SSL タイプのリスナーをサポートしており、STOMP ゲートウェイは柔軟で多用途なメッセージングシステム構築ツールです。

::: tip

STOMP ゲートウェイは [Stomp v1.2](https://stomp.github.io/stomp-specification-1.2.html) をベースとしており、STOMP v1.0 および v1.1 の仕様と互換性があります。
=======
この STOMP ゲートウェイは、クライアントとサーバーに対して軽量かつシンプルなメッセージングソリューションを提供し、さまざまなメッセージング環境でのメッセージ交換を実現します。TCP および SSL タイプのリスナーをサポートしており、柔軟で多用途なメッセージングシステム構築ツールです。

::: tip

STOMP ゲートウェイは [Stomp v1.2](https://stomp.github.io/stomp-specification-1.2.html) をベースにしており、STOMP v1.0 および v1.1 の仕様と互換性があります。
>>>>>>> origin/release-6.1

:::

## STOMP ゲートウェイの有効化

<<<<<<< HEAD
EMQX 5 では、STOMP ゲートウェイはダッシュボード、REST API、および設定ファイル `base.hocon` を通じて設定および有効化できます。本節では、ダッシュボードを用いた設定手順を例に操作方法を説明します。

EMQX ダッシュボードの左側ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**STOMP** を見つけ、**Actions** 列の **Setup** をクリックしてください。すると、**Initialize STOMP** ページに遷移します。

::: tip

EMQX をクラスターで運用している場合、ダッシュボードや REST API で行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md) で設定してください。

:::

設定を簡略化するため、EMQX は **Gateways** ページのすべての必須フィールドにデフォルト値を用意しています。大幅なカスタマイズが不要な場合、STOMP ゲートウェイはわずか3クリックで有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。  
2. 次に **Listeners** タブに遷移し、EMQX はポート `61613` で UDP リスナーを事前設定しています。設定を確認して再度 **Next** をクリックします。  
=======
EMQX 5 では、STOMP ゲートウェイはダッシュボード、REST API、設定ファイル `base.hocon` を通じて設定および有効化が可能です。本節では、ダッシュボードを用いた設定手順を例に操作方法を説明します。

EMQX ダッシュボードの左ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**STOMP** を見つけ、**Actions** 列の **Setup** をクリックすると、**Initialize STOMP** ページに遷移します。

::: tip

EMQX をクラスターで運用している場合、ダッシュボードや REST API を通じて行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md) で設定してください。

:::

設定を簡略化するため、EMQX は **Gateways** ページのすべての必須フィールドにデフォルト値を提供しています。大幅なカスタマイズが不要な場合は、3クリックで STOMP ゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。  
2. 次に **Listeners** タブに遷移し、EMQX はポート `61613` に UDP リスナーを事前設定しています。ここで再度 **Next** をクリックして設定を確定します。  
>>>>>>> origin/release-6.1
3. 最後に **Enable** ボタンをクリックして STOMP ゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、STOMP ゲートウェイの状態が **Enabled** と表示されていることを確認できます。

<img src="./assets/STOMP-enabled.png" alt="STOMP ゲートウェイ有効化済み" style="zoom:50%;" />

EMQX 5.0 では、STOMP ゲートウェイはダッシュボードを通じて設定および有効化が可能です。

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

<<<<<<< HEAD
STOMP ゲートウェイを構築した後、STOMP クライアントツールを使って接続テストを行い、正常に動作するか確認できます。例えば [stomp.py](https://github.com/jasonrbriggs/stomp.py) などがあります。
=======
STOMP ゲートウェイを構築した後は、STOMP クライアントツールを使って接続テストを行い、正常に動作することを確認できます。例えば [stomp.py](https://github.com/jasonrbriggs/stomp.py) などがあります。
>>>>>>> origin/release-6.1

### パブリッシュ／サブスクライブ

STOMP プロトコルは PUB/SUB メッセージングモデルと完全に互換性があり、STOMP ゲートウェイは以下のように動作します。

- STOMP プロトコルの `SEND` メッセージをメッセージのパブリッシュに使用します。`SEND` メッセージの `destination` フィールドがトピックを指定し、メッセージ内容は `SEND` メッセージのボディに含まれます。QoS（サービス品質）は固定で 0 です。  
<<<<<<< HEAD
- STOMP プロトコルの `SUBSCRIBE` メッセージをサブスクライブ要求に使用します。`SUBSCRIBE` メッセージの `destination` フィールドがトピックを指定します。QoS は固定で 0 であり、MQTT プロトコルで定義されたワイルドカードもサポートします。  
=======
- STOMP プロトコルの `SUBSCRIBE` メッセージをサブスクライブ要求に使用します。`SUBSCRIBE` メッセージの `destination` フィールドがトピックを指定します。QoS は固定で 0 であり、MQTT プロトコルで定義されているワイルドカードをサポートします。  
>>>>>>> origin/release-6.1
- STOMP プロトコルの `UNSUBSCRIBE` メッセージをサブスクライブ解除要求に使用します。`UNSUBSCRIBE` メッセージの `destination` フィールドがトピックを指定します。

## STOMP ゲートウェイのカスタマイズ

<<<<<<< HEAD
デフォルト設定に加え、EMQX はさまざまな設定オプションを提供しており、特定のビジネス要件に合わせて調整可能です。本節では **Gateways** ページで利用可能な各種フィールドについて詳しく説明します。

### 基本設定

**Basic Configuration** タブでは、許容する最大ヘッダー数、ヘッダー長の最大値、統計情報の有効化設定、ゲートウェイの MountPoint 文字列設定が可能です。以下に各フィールドの詳細を示します。
=======
デフォルト設定に加え、EMQX はさまざまな設定オプションを提供しており、特定のビジネス要件により適した構成が可能です。本節では **Gateways** ページで利用可能な各種フィールドについて詳細に解説します。

### 基本設定

**Basic Configuration** タブでは、許容する最大ヘッダー数、ヘッダー長、統計情報の有効化、ゲートウェイの MountPoint 文字列の設定が可能です。以下に各フィールドの詳細を示します。
>>>>>>> origin/release-6.1

<!--スクリーンショットは後日追加予定-->

<<<<<<< HEAD
1. **Max Header**: 許容する最大の STOMP ヘッダー数を設定します。デフォルトは `10` です。  
2. **Max Each Header Length**: ヘッダー値の最大文字列長を設定します。デフォルトは `1024` です。  
3. **Max Body Length**: STOMP パケットの最大バイト数を設定します。デフォルトは `65536` です。  
4. **Idle Timeout**: 非アクティブ状態で STOMP フレームを待機する最大秒数を設定し、その時間を超えると接続を切断します。  
5. **Enable Statistics**: ゲートウェイが統計情報を収集・報告するかどうかを設定します。デフォルトは `true` で、選択肢は `true` または `false` です。  
6. **MountPoint**: パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。これにより異なるプロトコル間でメッセージルーティングの分離を実現できます。例：`stomp/`。
=======
1. **Max Header**: 許容する最大の STOMP ヘッダー数を設定します。デフォルトは `10`。  
2. **Max Each Header Length**: ヘッダー値の最大文字列長を設定します。デフォルトは `1024`。  
3. **Max Body Length**: STOMP パケットの最大バイト数を設定します。デフォルトは `65536`。  
4. **Idle Timeout**: 非アクティブ状態が続いた場合に接続を切断するまでの最大待機時間（秒）を設定します。  
5. **Enable Statistics**: ゲートウェイが統計情報を収集・報告するかどうかを設定します。デフォルトは `true`、選択肢は `true` または `false`。  
6. **MountPoint**: パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離を実現するために用います。例：`stomp/`。
>>>>>>> origin/release-6.1

   **注意**: このトピックプレフィックスはゲートウェイ側で管理されるため、クライアントはパブリッシュやサブスクライブ時に明示的にこのプレフィックスを付加する必要はありません。

### リスナーの追加

<<<<<<< HEAD
ポート `61613` で名前が **default** の TCP リスナーが既に設定されており、最大16のアセプターをプールし、最大1,024,000の同時接続をサポートします。より詳細な設定を行うには **Settings** をクリックし、リスナーを削除するには **Delete** をクリック、新しいリスナーを追加するには **+ Add Listener** をクリックしてください。
=======
ポート `61613` に名前が **default** の tcp リスナーがすでに設定されており、最大 16 個のアセプターをプールで管理し、最大 1,024,000 の同時接続をサポートしています。より詳細な設定を行うには **Settings** をクリックし、リスナーを削除するには **Delete** をクリック、新規リスナーを追加するには **+ Add Listener** をクリックしてください。
>>>>>>> origin/release-6.1

::: tip

STOMP ゲートウェイは TCP および SSL タイプのリスナーのみをサポートしています。

:::

**Add Listener** をクリックすると **Add Listener** ページが開き、以下の設定項目を入力できます。

**基本設定**

- **Name**: リスナーの一意識別子を設定します。  
<<<<<<< HEAD
- **Type**: プロトコルタイプを選択します。STOMP では **tcp** または **ssl** のいずれかです。  
- **Bind**: リスナーが受け入れる接続のポート番号を設定します。  
- **MountPoint**（任意）: パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間のメッセージルーティング分離を実現します。

**リスナー設定**

- **Acceptor**: アセプタープールのサイズを設定します。デフォルトは **16** です。  
- **Max Connections**: リスナーが処理可能な同時接続数の最大値を設定します。デフォルトは **1024000** です。  
- **Max Connection Rate**: リスナーが1秒あたりに受け入れ可能な新規接続の最大レートを設定します。デフォルトは **1000** です。  
- **Proxy Protocol**: EMQX が [ロードバランサー](../deploy/cluster/lb.md) の背後にある場合に、プロトコル V1/V2 を有効化します。  
- **Proxy Protocol Timeout**: プロキシプロトコルパッケージを待機する最大秒数を設定し、その時間を超えると接続を切断します。デフォルトは **3秒** です。

**TCP 設定**

- **ActiveN**: ソケットの `{active, N}` オプションを設定します。これはソケットが能動的に処理可能な受信パケット数を意味します。詳細は [Erlang Documentation - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2) を参照してください。  
- **Buffer**: 受信および送信パケットを格納するバッファサイズを KB 単位で設定します。  
- **TCP_NODELAY**: 接続に対して `TCP_NODELAY` フラグを有効にするかどうかを設定します。これはクライアントが前のデータのアック（ACK）を待たずに追加データを送信できるかどうかを意味します。デフォルトは **false** で、選択肢は **true** または **false** です。  
- **SO_REUSEADDR**: ポート番号のローカル再利用を許可するかどうかを設定します。  
- **Send Timeout**: 送信タイムアウトの最大秒数を設定し、その時間を超えると接続を切断します。デフォルトは **15秒** です。  
- **Send Timeout**: 送信タイムアウト時に接続を切断するかどうかを設定します。

**SSL 設定**（SSL リスナーのみ）

TLS 検証を有効にするかどうかをトグルスイッチで設定できます。ただし、その前に関連する **TLS Cert**、**TLS Key**、および **CA Cert** の情報をファイルの内容を入力するか、**Select File** ボタンでアップロードして設定する必要があります。詳細は [SSL/TLS 接続の有効化](../network/emqx-mqtt-tls.md) を参照してください。
=======
- **Type**: プロトコルタイプを選択します。STOMP では **tcp** または **ssl** が選択可能です。  
- **Bind**: リスナーが接続を受け付けるポート番号を設定します。  
- **MountPoint**（任意）: パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティングの分離を実現します。

**リスナー設定**

- **Acceptor**: アセプタープールのサイズを設定します。デフォルトは **16**。  
- **Max Connections**: リスナーが処理可能な最大同時接続数を設定します。デフォルトは **1024000**。  
- **Max Connection Rate**: リスナーが1秒あたりに受け入れ可能な新規接続の最大レートを設定します。デフォルトは **1000**。  
- **Proxy Protocol**: EMQX が [ロードバランサー](../deploy/cluster/lb.md) の背後に配置されている場合に、プロトコル V1/V2 を有効化します。  
- **Proxy Protocol Timeout**: プロキシプロトコルパッケージを待機する最大時間（秒）を設定し、非アクティブ時に接続を切断します。デフォルトは **3秒**。

**TCP 設定**

- **ActiveN**: ソケットの `{active, N}` オプションを設定します。これはソケットが積極的に処理できる受信パケット数を示します。詳細は [Erlang Documentation - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2) を参照してください。  
- **Buffer**: 受信および送信パケットを格納するバッファサイズを KB 単位で設定します。  
- **TCP_NODELAY**: `TCP_NODELAY` フラグを有効にするかを設定します。これはクライアントが前のデータのアック（ACK）を待たずに追加データを送信できるかどうかを制御します。デフォルトは **false**、選択肢は **true** または **false**。  
- **SO_REUSEADDR**: ローカルでのポート番号の再利用を許可するかどうかを設定します。  
- **Send Timeout**: 送信タイムアウト時に接続を切断するかどうかを設定します。  
- **Send Timeout**: プロキシプロトコルパッケージを待機する最大時間（秒）を設定し、非アクティブ時に接続を切断します。デフォルトは **15秒**。

**SSL 設定**（SSL リスナーのみ）

TLS 検証を有効にするかどうかをトグルスイッチで設定できます。ただし、その前に関連する **TLS Cert**、**TLS Key**、**CA Cert** の情報をファイル内容の入力または **Select File** ボタンによるアップロードで設定する必要があります。詳細は [SSL/TLS 接続の有効化](../network/emqx-mqtt-tls.md) を参照してください。
>>>>>>> origin/release-6.1

その後、以下の設定を行えます。

<<<<<<< HEAD
- **SSL Versions**: サポートする SSL バージョンを設定します。デフォルトは **tlsv1.3**、**tlsv1.2**、**tlsv1.1**、および **tlsv1** です。  
- **Fail If No Peer Cert**: クライアントが空の証明書を送信した場合に EMQX が接続を拒否するかどうかを設定します。デフォルトは **false** で、選択肢は **true** または **false** です。  
- **Intermediate Certificate Depth**: ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは **10** です。  
=======
- **SSL Versions**: サポートする SSL バージョンを設定します。デフォルトは **tlsv1.3**、**tlsv1.2**、**tlsv1.1**、**tlsv1**。  
- **Fail If No Peer Cert**: クライアントが空の証明書を送信した場合に接続を拒否するかどうかを設定します。デフォルトは **false**、選択肢は **true** または **false**。  
- **Intermediate Certificate Depth**: ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは **10**。  
>>>>>>> origin/release-6.1
- **Key Password**: プライベートキーがパスワード保護されている場合に使用するパスワードを設定します。

## 認証の設定

STOMP プロトコルの接続メッセージにはユーザー名とパスワードの概念が既に定義されているため、STOMP は以下のような多様な認証方式をサポートしています。

- [組み込みデータベース認証](../access-control/authn/mnesia.md)  
- [MySQL 認証](../access-control/authn/mysql.md)  
- [MongoDB 認証](../access-control/authn/mongodb.md)  
- [PostgreSQL 認証](../access-control/authn/postgresql.md)  
- [Redis 認証](../access-control/authn/redis.md)  
- [HTTP サーバー認証](../access-control/authn/http.md)  
- [JWT 認証](../access-control/authn/jwt.md)  
- [LDAP 認証](../access-control/authn/ldap.md)

STOMP ゲートウェイは STOMP プロトコルの `CONNECT` または `STOMP` メッセージに含まれる情報を用いてクライアントの認証フィールドを生成します。

- クライアント ID: ランダムに生成される文字列  
- ユーザー名: `CONNECT` または `STOMP` メッセージヘッダーの `login` フィールドの値  
- パスワード: `CONNECT` または `STOMP` メッセージヘッダーの `passcode` フィールドの値

REST API を使って STOMP ゲートウェイ用の組み込みデータベース認証を作成することも可能です。

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

<<<<<<< HEAD
MQTT プロトコルとは異なり、**ゲートウェイは認証器の作成のみをサポートし、認証器のリスト（または認証チェーン）はサポートしていません**。
=======
MQTT プロトコルとは異なり、**ゲートウェイは認証器の作成のみをサポートし、認証器の一覧（または認証チェーン）の作成はサポートしていません**。
>>>>>>> origin/release-6.1

認証器が有効化されていない場合、すべての STOMP クライアントはログインを許可されます。

:::
