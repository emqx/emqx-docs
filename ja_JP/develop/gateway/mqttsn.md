# MQTT-SN ゲートウェイ

MQTT-SN（MQTT for Sensor Networks）はワイヤレスセンサーネットワーク向けの軽量なパブサブプロトコルであり、EMQX MQTT-SN ゲートウェイはこれらのデバイスがEMQXに接続し通信できるようにし、MQTT-SNと標準MQTTプロトコルの橋渡しを行います。

本ページでは、EMQXにおけるMQTT-SNゲートウェイの設定方法と利用方法を紹介します。

::: tip

MQTT-SNゲートウェイは[MQTT-SN v1.2](https://www.oasis-open.org/committees/download.php/66091/MQTT-SN_spec_v1.2.pdf)に基づいています。

:::

<!--アーキテクチャの簡単な紹介-->

## MQTT-SN ゲートウェイの有効化

EMQX 5.0では、MQTT-SNゲートウェイはダッシュボード、REST API、設定ファイル `base.hocon` を通じて設定および有効化できます。本節では、ダッシュボードを使った設定例を示し、操作手順を説明します。

EMQXダッシュボードの左側ナビゲーションメニューで **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**MQTT-SN** を探し、**Actions** 列の **Setup** をクリックすると、**Initialize MQTT-SN** ページに遷移します。

::: tip

EMQXをクラスターで運用している場合、ダッシュボードやREST APIで行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../../guides/configuration/configuration.md)で設定してください。

:::

設定を簡略化するため、EMQXは**Gateways**ページのすべての必須フィールドにデフォルト値を用意しています。大幅なカスタマイズが不要な場合は、以下の3クリックでMQTT-SNゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。
2. 次に **Listeners** タブに遷移し、EMQXはポート1884でUDPリスナーを事前設定しています。再度 **Next** をクリックして設定を確認します。
3. 最後に **Enable** ボタンをクリックしてMQTT-SNゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、MQTT-SNゲートウェイのステータスが **Enabled** と表示されていることを確認できます。

<img src="./assets/mqttsn-enabled.png" alt="有効化されたMQTT-SNゲートウェイ" style="zoom:50%;" />

上記の設定はREST APIでも可能です。

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

詳細なREST APIの説明は[REST API - Gateway](../../guides/api.md)をご参照ください。

カスタマイズが必要な場合やリスナーの追加、認証ルールの設定を行いたい場合は、[MQTT-SNゲートウェイのカスタマイズ](#customize-your-mqtt-sn-gateway)をご覧ください。

## MQTT-SN クライアントとの連携

### クライアントライブラリ

MQTT-SNゲートウェイを構築した後は、MQTT-SNクライアントツールを使って接続をテストし、正常に動作しているか確認できます。以下は推奨されるMQTT-SNクライアントツールの例です。

- [paho.mqtt-sn.embedded-c](https://github.com/eclipse/paho.mqtt-sn.embedded-c)
- [mqtt-sn-tools](https://github.com/njh/mqtt-sn-tools)

### パブリッシュ／サブスクライブ

MQTT-SNプロトコルはすでにパブリッシュ／サブスクライブの動作を定義しています。例：

- MQTT-SNプロトコルの `PUBLISH` メッセージはパブリッシュ操作に使用され、このメッセージでトピックとQoSが指定されます。
- `SUBSCRIBE` メッセージはサブスクライブ操作に使用され、トピックとQoSが指定されます。
- `UNSUBSCRIBE` メッセージはサブスクライブ解除操作に使用され、トピックが指定されます。

## MQTT-SN ゲートウェイのカスタマイズ

デフォルト設定に加え、EMQXはさまざまな設定オプションを提供し、特定のビジネス要件に柔軟に対応できます。本節では、**Gateways** ページで利用可能な各種フィールドについて詳しく解説します。

### 基本設定

**Basic Configuration** タブでは、ゲートウェイIDのカスタマイズ、事前定義されたトピックリストの設定、ゲートウェイのMountPoint文字列の設定が可能です。以下のスクリーンショット下の説明をご参照ください。

![基本設定](./assets/mqttsn-basic-config.png)

- **Gateway ID**：ゲートウェイの一意の識別子を設定します。例：1。

- **Enable Broadcast**：ゲートウェイがゲートウェイ広告をクライアントにブロードキャストするかどうかを設定します。指定したGateway IDを含むメッセージをブロードキャストします。デフォルト：`true`。選択肢：`true`、`false`。

- **Enable QoS 3**：QoS -1とも呼ばれ、アックやサブスクライブを必要とせず、`PUBLISH`メッセージのみをゲートウェイに送信する基本的なクライアント向けの設定です。デフォルト：`true`。選択肢：`true`、`false`。

- **Idle Timeout**：接続中のMQTT-SNクライアントが非アクティブとみなされるまでの秒数を設定します。デフォルト：`30s`。

- **Enable Statistics**：ゲートウェイが統計情報を収集・報告するかどうかを設定します。デフォルト：`true`。選択肢：`true`、`false`。

- **Predefined Topic List**：事前定義されたトピックIDと対応するトピック名を設定します。**Add** をクリックして新しいエントリを追加します。

  - **Topic ID**：トピックIDを設定します。1から65535の整数で指定してください。
  - **Topic**：トピック名を設定します。

- **MountPoint**：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。異なるプロトコル間でのメッセージルーティングの分離を実現するために使用します。例：`mqttsn/`。

  **注意**：このトピックプレフィックスはゲートウェイ側で管理されており、MQTT-SNクライアントはパブリッシュやサブスクライブ時に明示的にこのプレフィックスを付加する必要はありません。

### リスナーの追加

デフォルトでは、ポート `1884` で名前が **default** のUDPリスナーが1つ設定されており、1秒あたり最大1,000接続、最大1,024,000同時接続をサポートします。より詳細な設定を行うには **Settings** をクリックし、リスナーの削除は **Delete** をクリック、新規リスナー追加は **+ Add Listener** をクリックします。

<img src="./assets/mqttsn-listener.png" alt="MQTT-SNリスナー" style="zoom:50%;" />

**Add Listener** をクリックすると **Add Listener** ページが開き、以下の設定が可能です。

**基本設定**

- **Name**：リスナーの一意の識別子を設定します。
- **Type**：プロトコルタイプを選択します。MQTT-SNでは **udp** または **dtls** が選べます。
- **Bind**：リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**（任意）：パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でのメッセージルーティング分離を実現します。

**リスナー設定**

- **Acceptor**（DTLSリスナーのみ）：アクセプタープールのサイズを設定します。デフォルト：**16**。
- **Max Connections**：リスナーが処理可能な最大同時接続数を設定します。デフォルト：**1024000**。
- **Max Connection Rate**：リスナーが1秒あたり受け入れ可能な新規接続の最大レートを設定します。デフォルト：**1000**。

**UDP設定**

- **ActiveN**：ソケットの `{active, N}` オプションを設定します。これはソケットが積極的に処理できる受信パケット数を示します。詳細は[Erlang Documentation - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2)をご参照ください。
- **Buffer**：受信および送信パケットを格納するバッファのサイズをKB単位で設定します。
- **Receive Buffer**：受信バッファのサイズをKB単位で設定します。
- **Send Buffer**：送信バッファのサイズをKB単位で設定します。
- **SO_REUSEADDR**：ローカルでポート番号の再利用を許可するかどうかを設定します。

**DTLS設定**（DTLSリスナーのみ）

TLS検証の有効化はトグルスイッチで設定可能ですが、その前に関連する **TLS Cert**、**TLS Key**、**CA Cert** の情報をファイル内容の入力または **Select File** ボタンでアップロードして設定する必要があります。詳細は[SSL/TLS接続の有効化](../../guides/network/emqx-mqtt-tls.md)をご参照ください。

その後、以下の設定が可能です。

- **DTLS Versions**：サポートするDTLSバージョンを設定します。デフォルトは **dtlsv1.2** と **dtlsv1**。
- **Fail If No Peer Cert**：クライアントが空の証明書を送信した場合に接続を拒否するかどうかを設定します。デフォルト：**false**。選択肢：**true**、**false**。
- **Intermediate Certificate Depth**：ピア証明書に続く有効な認証パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルト：**10**。
- **Key Password**：プライベートキーがパスワード保護されている場合に使用するパスワードを設定します。

### 認証の設定

MQTT-SNプロトコルの接続メッセージはクライアントのClient IDのみを提供するため、MQTT-SNゲートウェイは[HTTPサーバー認証](../../guides/access-control/authn/http.md)のみをサポートしています。

クライアント情報の生成ルールは以下の通りです。

- Client ID：`CONNECT` メッセージのClient IDフィールドを使用。
- Username：未定義
- Password：未定義

ここではダッシュボードを例に認証設定方法を説明します。

**Gateways** ページで **MQTT-SN** を探し、**Actions** 列の **Setup** をクリック、次に **Authentication** タブをクリックします。

**Create Authentication** をクリックし、**Mechanism** に **Password-Based** を選択、**Backend** に **HTTP Server** を選択します。続いて **Configuration** タブで認証ルールを設定します。

![mqttsn 認証設定](./assets/mqttsn-authn-config.png)

ページ上の各フィールドの詳細は[HTTPサーバー認証](../../guides/access-control/authn/http.md)をご参照ください。

上記の設定はREST APIでも実行可能です。

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
