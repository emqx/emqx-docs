# MQTT-SN ゲートウェイ

MQTT-SN（MQTT for Sensor Networks）は、ワイヤレスセンサーネットワーク向けの軽量なパブリッシュ／サブスクライブプロトコルです。EMQX MQTT-SN ゲートウェイは、これらのデバイスが EMQX に接続し通信できるようにし、MQTT-SN と標準 MQTT プロトコル間の橋渡しを行います。

本ページでは、EMQX における MQTT-SN ゲートウェイの設定および使用方法について紹介します。

::: tip

MQTT-SN ゲートウェイは [MQTT-SN v1.2](https://www.oasis-open.org/committees/download.php/66091/MQTT-SN_spec_v1.2.pdf) に基づいています。

:::

<!--アーキテクチャの簡単な紹介-->

## MQTT-SN ゲートウェイの有効化

EMQX 5.0 では、MQTT-SN ゲートウェイはダッシュボード、REST API、設定ファイル `base.hocon` を通じて設定および有効化できます。本節では、ダッシュボードを用いた設定例をもとに操作手順を説明します。

EMQX ダッシュボードの左側ナビゲーションメニューから **Management** -> **Gateways** をクリックします。**Gateways** ページにはサポートされているすべてのゲートウェイが一覧表示されます。**MQTT-SN** を探し、**Actions** 列の **Setup** をクリックすると、**Initialize MQTT-SN** ページに遷移します。

::: tip

EMQX をクラスターで稼働させている場合、ダッシュボードや REST API で行った設定はクラスター全体に影響します。特定のノードのみ設定を変更したい場合は、[`base.hocon`](../configuration/configuration.md) にて設定してください。

:::

設定を簡略化するため、EMQX は **Gateways** ページのすべての必須フィールドにデフォルト値を提供しています。大幅なカスタマイズが不要であれば、以下の3クリックで MQTT-SN ゲートウェイを有効化できます。

1. **Basic Configuration** タブで **Next** をクリックし、すべてのデフォルト設定を受け入れます。  
2. 次に **Listeners** タブに遷移し、EMQX はポート1884で UDP リスナーを事前設定しています。ここでも **Next** をクリックして設定を確定します。  
3. 最後に **Enable** ボタンをクリックして MQTT-SN ゲートウェイを有効化します。

ゲートウェイの有効化が完了すると、**Gateways** ページに戻り、MQTT-SN ゲートウェイのステータスが **Enabled** と表示されていることを確認できます。

<img src="./assets/mqttsn-enabled.png" alt="有効化された MQTT-SN ゲートウェイ" style="zoom:50%;" />

上記の設定は REST API でも可能です。

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

詳細な REST API の説明は [REST API - Gateway](../admin/api.md) をご参照ください。

カスタマイズやリスナーの追加、認証ルールの設定が必要な場合は、[MQTT-SN ゲートウェイのカスタマイズ](#customize-your-mqtt-sn-gateway) セクションをお読みください。

## MQTT-SN クライアントとの連携

### クライアントライブラリ

MQTT-SN ゲートウェイを構築した後は、MQTT-SN クライアントツールを使って接続テストを行い、正常に動作することを確認できます。以下は推奨される MQTT-SN クライアントツールの例です。

- [paho.mqtt-sn.embedded-c](https://github.com/eclipse/paho.mqtt-sn.embedded-c)
- [mqtt-sn-tools](https://github.com/njh/mqtt-sn-tools)

### パブリッシュ／サブスクライブ

MQTT-SN プロトコルはパブリッシュ／サブスクライブの動作を定義しています。例：

- MQTT-SN プロトコルの `PUBLISH` メッセージはパブリッシュ操作に使用され、トピックおよび QoS はこのメッセージで指定されます。
- `SUBSCRIBE` メッセージはサブスクライブ操作に使用され、トピックおよび QoS はこのメッセージで指定されます。
- `UNSUBSCRIBE` メッセージはサブスクライブ解除操作に使用され、トピックはこのメッセージで指定されます。

## MQTT-SN ゲートウェイのカスタマイズ

デフォルト設定に加え、EMQX はさまざまな設定オプションを提供し、特定のビジネス要件に柔軟に対応できます。本節では **Gateways** ページで利用可能な各種フィールドについて詳しく解説します。

### 基本設定

**Basic Configuration** タブでは、ゲートウェイ ID のカスタマイズ、事前定義トピックリストの設定、ゲートウェイの MountPoint 文字列の指定が可能です。以下のスクリーンショット下の説明をご参照ください。

![基本設定](./assets/mqttsn-basic-config.png)

- **Gateway ID**: ゲートウェイの一意識別子を設定します。例：1。

- **Enable Broadcast**: ゲートウェイがクライアントにゲートウェイ広告をブロードキャストするかどうかを設定します。ブロードキャストされるメッセージには先ほど指定した Gateway ID が含まれます。デフォルト：`true`。選択肢：`true`、`false`。

- **Enable QoS 3**: QoS -1 とも呼ばれ、アックやサブスクライブを必要とせず、単に `PUBLISH` メッセージをゲートウェイに送信するだけの基本的なクライアント向けの設定です。デフォルト：`true`。選択肢：`true`、`false`。

- **Idle Timeout**: 非アクティブ状態が続いた後、接続中の MQTT-SN クライアントを切断とみなすまでの秒数を設定します。デフォルト：`30s`。

- **Enable Statistics**: ゲートウェイが統計情報を収集・報告するかどうかを設定します。デフォルト：`true`。選択肢：`true`、`false`。

- **Predefined Topic List**: 事前定義されたトピック ID と対応するトピック名を設定します。**Add** をクリックして新規エントリを追加可能です。

  - **Topic ID**: トピック ID を設定します。1～65535 の整数で指定してください。
  - **Topic**: トピック名を設定します。

- **MountPoint**: パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定します。これにより異なるプロトコル間でメッセージルーティングの分離を実現できます。例：`mqttsn/`。

  **注意**：このトピックプレフィックスはゲートウェイ側で管理されるため、MQTT-SN クライアントはパブリッシュやサブスクライブ時に明示的にこのプレフィックスを付加する必要はありません。

### リスナーの追加

デフォルトでは、ポート `1884` で名前が **default** の UDP リスナーが1つ設定されており、1秒あたり最大1,000接続、同時最大1,024,000接続をサポートします。より詳細な設定を行う場合は **Settings** をクリック、リスナーを削除する場合は **Delete** をクリック、新規リスナーを追加する場合は **+ Add Listener** をクリックしてください。

<img src="./assets/mqttsn-listener.png" alt="MQTT-SN リスナー" style="zoom:50%;" />

**Add Listener** をクリックすると **Add Listener** ページが開き、以下の設定項目を入力できます。

**基本設定**

- **Name**: リスナーの一意識別子を設定します。
- **Type**: プロトコルタイプを選択します。MQTT-SN では **udp** または **dtls** が選択可能です。
- **Bind**: リスナーが接続を受け付けるポート番号を設定します。
- **MountPoint**（任意）: パブリッシュやサブスクライブ時にすべてのトピックの前に付加される文字列を設定し、異なるプロトコル間でメッセージルーティングの分離を実現します。

**リスナー設定**

- **Acceptor**（DTLS リスナーのみ）: アクセプタプールのサイズを設定します。デフォルトは **16**。
- **Max Connections**: リスナーが同時に処理可能な最大接続数を設定します。デフォルトは **1024000**。
- **Max Connection Rate**: リスナーが1秒あたりに受け入れ可能な新規接続の最大レートを設定します。デフォルトは **1000**。

**UDP 設定**

- **ActiveN**: ソケットの `{active, N}` オプションを設定します。これはソケットが積極的に処理可能な受信パケット数を示します。詳細は [Erlang Documentation - setopts/2](https://www.erlang.org/doc/apps/kernel/inet.html#setopts/2) をご参照ください。
- **Buffer**: 受信および送信パケットを格納するバッファサイズを KB 単位で設定します。
- **Receive Buffer**: 受信バッファのサイズを KB 単位で設定します。
- **Send Buffer**: 送信バッファのサイズを KB 単位で設定します。
- **SO_REUSEADDR**: ローカルのポート番号再利用を許可するかどうかを設定します。

**DTLS 設定**（DTLS リスナーのみ）

**TLS Verify** の有効化はトグルスイッチで設定可能ですが、その前に関連する **TLS Cert**、**TLS Key**、**CA Cert** の情報をファイル内容の入力または **Select File** ボタンによるアップロードで設定する必要があります。詳細は [Enable SSL/TLS Connections](../network/emqx-mqtt-tls.md) をご参照ください。

続けて以下の設定が可能です。

- **DTLS Versions**: サポートする DTLS バージョンを設定します。デフォルトは **dtlsv1.2** と **dtlsv1**。
- **Fail If No Peer Cert**: クライアントが空の証明書を送信した場合に EMQX が接続を拒否するかどうかを設定します。デフォルトは **false**。選択肢は **true**、**false**。
- **Intermediate Certificate Depth**: ピア証明書に続く有効な証明書パスに含まれる自己発行でない中間証明書の最大数を設定します。デフォルトは **10**。
- **Key Password**: プライベートキーがパスワード保護されている場合に使用するユーザーパスワードを設定します。

### 認証の設定

MQTT-SN プロトコルの接続メッセージはクライアントの Client ID のみを提供するため、MQTT-SN ゲートウェイは [HTTP サーバー認証](../access-control/authn/http.md) のみをサポートしています。

クライアント情報の生成ルールは以下の通りです。

- Client ID: `CONNECT` メッセージの Client ID フィールドを使用
- Username: 未定義
- Password: 未定義

ここではダッシュボードを例に認証設定方法を説明します。

**Gateways** ページで **MQTT-SN** を探し、**Actions** 列の **Setup** をクリックし、**Authentication** タブに入ります。

**Create Authentication** をクリックし、**Mechanism** に **Password-Based** を選択、**Backend** に **HTTP Server** を選択します。続いて **Configuration** タブで認証ルールを設定できます。

![mqttsn 認証設定](./assets/mqttsn-authn-config.png)

各フィールドの詳細は [HTTP サーバー認証](../access-control/authn/http.md) をご参照ください。

上記設定は REST API でも行えます。

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
