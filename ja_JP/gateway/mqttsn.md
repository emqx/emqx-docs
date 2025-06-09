# MQTT-SN ゲートウェイ

MQTT-SN（MQTT for Sensor Networks）は、ワイヤレスセンサーネットワーク向けの軽量なパブリッシュ／サブスクライブプロトコルです。EMQX MQTT-SN ゲートウェイは、これらのデバイスが EMQX に接続して通信できるようにし、MQTT-SN と標準 MQTT プロトコルの橋渡しを行います。

このページでは、EMQX における MQTT-SN ゲートウェイの設定および使用方法を紹介します。

::: tip

MQTT-SN ゲートウェイは [MQTT-SN v1.2](https://www.oasis-open.org/committees/download.php/66091/MQTT-SN_spec_v1.2.pdf) に基づいています。

:::

<!--a brief introduction of the architecture-->

## MQTT-SN ゲートウェイの有効化

EMQX 5.0 では、MQTT-SN ゲートウェイはダッシュボード、HTTP API、設定ファイル `base.hocon` を通じて設定および有効化が可能です。本節ではダッシュボードによる設定例を用いて操作手順を説明します。

EMQX ダッシュボードの左側ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**MQTT-SN** を探し、**Actions** 列の **Setup** をクリックすると、**Initialize MQTT-SN** ページに遷移します。

::: tip

EMQX をクラスターで運用している場合、ダッシュボードや HTTP API で行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md) で設定してください。

:::

設定を簡略化するために、EMQX は **Gateways** ページのすべての必須フィールドにデフォルト値を用意しています。大幅なカスタマイズが不要な場合は、以下の3クリックで MQTT-SN ゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. 次に **Listeners** タブに遷移し、EMQX はポート 1884 で UDP リスナーを事前設定しています。再度 **Next** をクリックして設定を確認します。
3. 最後に **Enable** ボタンをクリックして MQTT-SN ゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、MQTT-SN ゲートウェイのステータスが **Enabled** と表示されていることを確認できます。

<img src="./assets/mqttsn-enabled.png" alt="有効化された MQTT-SN ゲートウェイ" style="zoom:50%;" />

上記の設定は HTTP API でも可能です。

**例:**

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateways/mqttsn' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "mqttsn",
  "enable": true,
  "gateway_id": 1,
  "mountpoint": "mqttsn/",
  "listeners": [
    {
      "type": "udp",
      "bind": "1884",
      "name": "default",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```

HTTP API の詳細は [HTTP API - Gateway](../admin/api.md) を参照してください。

カスタマイズが必要でリスナーを追加したり認証ルールを設定したい場合は、[MQTT-SN ゲートウェイのカスタマイズ](#customize-your-mqtt-sn-gateway) セクションをお読みください。

## MQTT-SN クライアントとの連携

### クライアントライブラリ

MQTT-SN ゲートウェイを構築した後は、MQTT-SN クライアントツールを使って接続をテストし、正常に動作するか確認できます。以下は推奨される MQTT-SN クライアントツールの例です。

- [paho.mqtt-sn.embedded-c](https://github.com/eclipse/paho.mqtt-sn.embedded-c)
- [mqtt-sn-tools](https://github.com/njh/mqtt-sn-tools)

### パブリッシュ／サブスクライブ

MQTT-SN プロトコルはすでにパブリッシュ／サブスクライブの動作を定義しています。例えば：

- MQTT-SN プロトコルの `PUBLISH` メッセージはパブリッシュ操作に使用され、このメッセージでトピックと QoS が指定されます。
- `SUBSCRIBE` メッセージはサブスクライブ操作に使用され、トピックと QoS の両方が指定されます。
- `UNSUBSCRIBE` メッセージはサブスクライブ解除操作に使用され、トピックが指定されます。

## MQTT-SN ゲートウェイのカスタマイズ

デフォルト設定に加えて、EMQX はさまざまな設定オプションを提供し、特定のビジネス要件に柔軟に対応できます。本節では **Gateways** ページで利用可能な各種フィールドについて詳しく説明します。

### 基本設定

**Basic Configuration** タブでは、ゲートウェイ ID のカスタマイズ、事前定義トピックリストの設定、ゲートウェイの MountPoint 文字列の設定が可能です。以下のスクリーンショット下の説明をご参照ください。

![Basic Configuration](./assets/mqttsn-basic-config.png)

- **Gateway ID**：ゲートウェイの一意識別子を設定します。例：1。

- **Enable Broadcast**：ゲートウェイがクライアントにゲートウェイ広告をブロードキャストするかどうかを設定します。指定した Gateway ID を含むメッセージをブロードキャストします。デフォルトは `true`、選択肢は `true` または `false`。

- **Enable QoS3**：ゲートウェイが QoS 3（Exactly-once）メッセージ配信をサポートするかどうかを設定します。この設定は主に `PUBLISH` メッセージのみを送信する基本クライアント向けです。デフォルトは `true`、選択肢は `true` または `false`。

- **Idle Timeout**：接続中の MQTT-SN クライアントが非アクティブとみなされ切断されるまでの秒数を設定します。デフォルトは `30s`。

- **Enable Statistics**：ゲートウェイが統計情報を収集・報告するかどうかを設定します。デフォルトは `true`、選択肢は `true` または `false`。

- **Predefined Topic List**：事前定義されたトピック ID と対応するトピック名を設定します。**Add** をクリックして新しいエントリを追加可能です。

  - **Topic ID**：トピック ID を設定します。1～65535 の整数で指定してください。
  - **Topic**：トピック名を設定します。<!--, multiple topics can be added here, separated with a `,`-->

- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離を実現するために使用します。例：`mqttsn/`。

  **注意**：このトピックプレフィックスはゲートウェイが管理しており、MQTT-SN クライアントはパブリッシュやサブスクライブ時に明示的にこのプレフィックスを付ける必要はありません。

### リスナーの追加

デフォルトでは、名前が **default** の UDP リスナーがポート `1884` で設定されており、1秒あたり最大1,000接続、最大1,024,000同時接続をサポートします。**Settings** をクリックすると詳細設定が可能で、**Delete** でリスナーの削除、**+ Add Listener** で新規リスナーの追加ができます。

<img src="./assets/mqttsn-listerner.png" alt="MQTT-SN リスナー" style="zoom:50%;" />

**Add Listener** をクリックすると **Add Listener** ページが開き、以下の設定が行えます。

**基本設定**

- **Name**：リスナーの一意識別子を設定します。
- **Type**：プロトコルタイプを選択します。MQTT-SN では **udp** または **dtls** が選択可能です。
- **Bind**：リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**（任意）：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティングの分離を実現します。

**リスナー設定**

- **Acceptor**（DTLS リスナーのみ）：アクセプタープールのサイズを設定します。デフォルトは **16**。
- **Max Connections**：リスナーが処理可能な同時接続の最大数を設定します。デフォルトは **1024000**。
- **Max Connection Rate**：リスナーが1秒あたり受け入れ可能な新規接続の最大レートを設定します。デフォルトは **1000**。

**UDP 設定**

- **ActiveN**：ソケットの `{active, N}` オプションを設定します。これはソケットが積極的に処理可能な受信パケット数を示します。詳細は [Erlang Documentation - setopts/2](https://erlang.org/doc/man/inet.html#setopts-2) を参照してください。
- **Buffer**：受信および送信パケットを格納するバッファのサイズを KB 単位で設定します。
- **Receive Buffer**：受信バッファのサイズを KB 単位で設定します。
- **Send Buffer**：送信バッファのサイズを KB 単位で設定します。
- **SO_REUSEADDR**：ローカルでのポート番号の再利用を許可するかどうかを設定します。

**DTLS 設定**（DTLS リスナーのみ）

TLS Verify を有効にするかどうかの切り替えスイッチがあります。ただし、その前に関連する **TLS Cert**、**TLS Key**、**CA Cert** の情報を設定する必要があります。ファイルの内容を直接入力するか、**Select File** ボタンでアップロードしてください。詳細は [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。

続いて以下の設定が可能です。

- **DTLS Versions**：サポートする DTLS バージョンを設定します。デフォルトは **dtlsv1.2** と **dtlsv1**。
- **Fail If No Peer Cert**：クライアントが空の証明書を送信した場合に EMQX が接続を拒否するかどうかを設定します。デフォルトは **false**。選択肢は **true**、**false**。
- **Intermediate Certificate Depth**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは **10**。
- **Key Password**：プライベートキーがパスワード保護されている場合に使用するユーザーパスワードを設定します。

### 認証の設定

MQTT-SN プロトコルの接続メッセージはクライアントの Client ID のみを提供するため、MQTT-SN ゲートウェイは [HTTP Server Authentication](../access-control/authn/http.md) のみをサポートしています。

クライアント情報の生成ルールは以下の通りです。

- Client ID：`CONNECT` メッセージの Client ID フィールドを使用します。
- Username：未定義
- Password：未定義

ここではダッシュボードを例に認証設定方法を説明します。

**Gateways** ページで **MQTT-SN** を探し、**Actions** 列の **Setup** をクリックし、**Authentication** タブに入ります。

**Create Authentication** をクリックし、**Mechanism** に **Password-Based** を選択、**Backend** に **HTTP Server** を選択します。続いて **Configuration** タブで認証ルールを設定します。

![mqttsn authentication](./assets/mqttsn-authn-config.png)

各フィールドの詳細は [HTTP Server Authentication](../access-control/authn/http.md) を参照してください。

上記の設定は HTTP API でも実行可能です。

**例:**

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway/mqttsn/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "method": "post",
  "url": "http://127.0.0.1:8080",
  "headers": {
    "content-type": "application/json"
  },
  "body": {
    "clientid": "${clientid}"
  },
  "pool_size": 8,
  "connect_timeout": "5s",
  "request_timeout": "5s",
  "enable_pipelining": 100,
  "ssl": {
    "enable": false,
    "verify": "verify_none"
  },
  "backend": "http",
  "mechanism": "password_based",
  "enable": true
}'
```
