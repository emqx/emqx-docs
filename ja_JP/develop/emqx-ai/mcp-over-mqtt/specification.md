# 仕様

本仕様は、MQTT固有の要件（MQTTトピックおよびクライアントID形式など）を定義しています。また、サービスディスカバリー、初期化、機能リストの変更、リソース更新、シャットダウン手順を含むMQTTトランスポートのライフサイクルについても概説しています。

本仕様は、[MCP仕様](https://modelcontextprotocol.io/specification/2025-06-18)と併せて読む必要があります。

## 用語

- **server-name**: server-nameはMCPサーバーの識別子であり、トピックに含まれます。

  同じ`server-name`を持つ複数の接続は、同一のMCPサーバーの複数インスタンスとみなされ、全く同じサービスを提供します。MCPクライアントが初期化メッセージを送信する際には、クライアント側で決定された戦略に従ってそのうちの1つを選択する必要があります。

  異なる`server-name`を持つ複数のMCPサーバーは、類似の機能を提供する場合があります。この場合、クライアントは初期化メッセージを送信する際に必要に応じてそのうちの1つを選択して接続を確立します。選択基準はクライアントの権限、LLMからの推奨、ユーザーの選択などに基づくことができます。

  MQTTブローカーに接続後、ブローカーはMQTT CONNECTメッセージのユーザープロパティに`MCP-SERVER-NAME`を含めてMCPサーバーに`server-name`を提案する場合があります。その場合、MCPサーバーは**必ず**この`server-name`を自身のserver-nameとして使用しなければなりません。ブローカーが`server-name`を提案しない場合、MCPサーバーは提供する機能に基づいたデフォルトの`server-name`を**推奨**します。

  `server-name`は階層型トピックスタイルで`/`で区切られ、クライアントはMQTTトピックのワイルドカードを使って特定タイプのMCPサーバーにサブスクライブできます。例：`server-type/sub-type/name`

  `server-name`には`+`および`#`文字を含めてはいけません。

  `server-name`はすべてのMCPサーバー間で一意である必要があります。

- **server-name-filter**: `server-name`にマッチするMQTTトピックフィルターであり、`/`、`+`、`#`文字を含むことがあります。詳細は**server-name**の説明を参照してください。

  MQTTブローカーに接続後、ブローカーはMQTT CONNACKメッセージのユーザープロパティに`MCP-SERVER-NAME-FILTERS`を含めてMCPクライアントに`server-name-filter`を提案する場合があります。その場合、MCPクライアントは**必ず**この`server-name-filter`を使用してサーバーのプレゼンストピックにサブスクライブしなければなりません。`MCP-SERVER-NAME-FILTERS`の値は文字列のJSON配列であり、それぞれがMQTTトピックフィルターです。ブローカーが`server-name-filter`を提案しない場合、MCPクライアントは提供する機能に基づいたデフォルトの`server-name-filter`を**推奨**します。

- **server-id**: MCPサーバーインスタンスのMQTTクライアントID。`/`、`+`、`#`以外の任意の文字列で、グローバルに一意である必要があります。トピックにも含まれます。

- **mcp-client-id**: クライアントのMQTTクライアントID。`/`、`+`、`#`以外の任意の文字列で、グローバルに一意である必要があります。トピックにも含まれます。初期化要求ごとに異なるクライアントIDを使用しなければなりません。

## メッセージトピック

MCP over MQTTはMQTTトピックを通じてメッセージを送受信します。本プロトコルには以下のメッセージトピックがあります：

| トピック名                          | トピック名（例）                                                    | 説明                                                                                   |
|------------------------------------|--------------------------------------------------------------------|----------------------------------------------------------------------------------------|
| サーバーの制御トピック             | `$mcp-server/{server-id}/{server-name}`                            | 初期化メッセージやその他制御メッセージの送受信用。                                     |
| サーバーの機能変更トピック         | `$mcp-server/capability/{server-id}/{server-name}`                 | サーバーの機能リスト変更やリソース更新通知の送受信用。                                 |
| サーバーのプレゼンストピック       | `$mcp-server/presence/{server-id}/{server-name}`                   | サーバーのオンライン／オフライン状態メッセージの送受信用。                             |
| クライアントのプレゼンストピック   | `$mcp-client/presence/{mcp-client-id}`                             | クライアントのオンライン／オフライン状態メッセージの送受信用。                         |
| クライアントの機能変更トピック     | `$mcp-client/capability/{mcp-client-id}`                           | クライアントの機能リスト変更通知の送受信用。                                           |
| RPCトピック                       | `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`               | RPCリクエスト／レスポンスおよび通知メッセージの送受信用。                             |

## MQTTプロトコルバージョン

MCPサーバーおよびクライアントは**必ず**MQTTプロトコルバージョン5.0を使用しなければなりません。

## ユーザープロパティ

`CONNECT`メッセージでは以下のユーザープロパティを**必ず**設定します：
- `MCP-COMPONENT-TYPE`: `mcp-client`または`mcp-server`
- `MCP-META`: MCPコンポーネントのバージョン、実装情報、場所などのメタデータを含むJSONオブジェクト。ブローカーはこれを使ってMCPサーバーにserver-name、MCPクライアントにserver-name-filterを提案できます。

ブローカーが送信する`CONNACK`メッセージでは以下のユーザープロパティを**任意で**設定できます：
- `MCP-SERVER-NAME`: MCPサーバーに提案するserver-name。MCPサーバーの場合のみ存在。
- `MCP-RBAC`: MCPクライアントがMCPサーバーに対して持つロールを決定するためのサーバー名とロール名のJSON配列。各要素は`server_name`と`role_name`の2フィールドを持つJSONオブジェクト。MCPクライアントの場合のみ存在。
- `MCP-SERVER-NAME-FILTERS`: MCPクライアントに提案するserver-name-filterのJSON配列。各文字列はMQTTトピックフィルター。MCPクライアントの場合のみ存在。

`PUBLISH`メッセージでは以下のユーザープロパティを**必ず**設定します：
- `MCP-COMPONENT-TYPE`: `mcp-client`または`mcp-server`
- `MCP-MQTT-CLIENT-ID`: 送信者のMQTTクライアントID

## セッション有効期限

セッション有効期限は**必ず**0に設定し、クライアント切断時にセッションをクリーンアップします。

## MQTTクライアントID

### MCPサーバー

MCPサーバーのクライアントIDは`/`、`+`、`#`以外の任意の文字列で、`server-id`と呼ばれます。

### MCPクライアント

MCPクライアントのクライアントIDは`/`、`+`、`#`以外の任意の文字列で、`mcp-client-id`と呼ばれます。初期化要求ごとに異なるクライアントIDを使用しなければなりません。

## MQTTトピックおよびトピックフィルター

### MCPサーバーのサブスクライブ

| トピックフィルター                                         | 説明                                                                                      |
|----------------------------------------------------------|-------------------------------------------------------------------------------------------|
| `$mcp-server/{server-id}/{server-name}`                   | MCPサーバーの制御トピック。制御メッセージ受信用。                                       |
| `$mcp-client/capability/{mcp-client-id}`                  | MCPクライアントの機能変更トピック。クライアントの機能リスト変更通知受信用。               |
| `$mcp-client/presence/{mcp-client-id}`                    | MCPクライアントのプレゼンストピック。クライアントの切断通知受信用。                       |
| `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`      | RPCトピック。MCPクライアントからのRPCリクエスト、レスポンス、通知の受信用。               |

::: info
- サーバーはRPCトピック（`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`）のサブスクライブ時に**No Local**オプションを設定し、自身のメッセージを受信しないようにしなければなりません。
:::

### MCPサーバーのパブリッシュ

| トピック名                                               | メッセージ内容                                                                                  |
|---------------------------------------------------------|-----------------------------------------------------------------------------------------------|
| `$mcp-server/capability/{server-id}/{server-name}`       | 機能リスト変更またはリソース更新通知。                                                         |
| `$mcp-server/presence/{server-id}/{server-name}`         | MCPサーバーのプレゼンス（オンライン状態）メッセージ。<br>詳細は[サービスディスカバリー](#service-discovery)参照。 |
| `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`     | RPCリクエスト、レスポンス、通知。                                                             |

::: info
- サーバーはサーバープレゼンスメッセージをパブリッシュする際、トピック`$mcp-server/presence/{server-id}/{server-name}`に対し**RETAIN**フラグを`True`に設定しなければなりません。
- MQTTブローカーに接続する際、サーバーは予期せぬ切断時に保持メッセージをクリアするため、`$mcp-server/presence/{server-id}/{server-name}`をウィルトピックとして空ペイロードで設定しなければなりません。
:::

### MCPクライアントのサブスクライブ

| トピックフィルター                                         | 説明                                                                                      |
|----------------------------------------------------------|-------------------------------------------------------------------------------------------|
| `$mcp-server/capability/{server-id}/{server-name-filter}` | MCPサーバーの機能変更トピック。機能リスト変更やリソース更新通知の受信用。                   |
| `$mcp-server/presence/+/{server-name-filter}`             | MCPサーバーのプレゼンス（オンライン状態）メッセージの受信用。                             |
| `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name-filter}` | MCPサーバーから送信されるRPCリクエスト、レスポンス、通知の受信用。                         |

::: tip 注意

クライアントはRPCトピック（`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name-filter}`）のサブスクライブ時に**必ず**No Localオプションを設定し、自身のメッセージを受信しないようにしなければなりません。
:::

### MCPクライアントのパブリッシュ

| トピック名                                               | メッセージ内容                                                   |
|---------------------------------------------------------|----------------------------------------------------------------|
| `$mcp-server/{server-id}/{server-name}`                 | 初期化リクエストなどの制御メッセージ送信用。                     |
| `$mcp-client/capability/{mcp-client-id}`                | クライアントの機能リスト変更通知送信用。                         |
| `$mcp-client/presence/{mcp-client-id}`                   | MCPクライアントの切断通知送信用。                               |
| `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`    | 特定サーバーへのRPCリクエスト／レスポンス送信用。               |

::: tip 注意

MQTTブローカーに接続する際、クライアントは予期せぬ切断時にサーバーに通知するため、`$mcp-client/presence/{mcp-client-id}`をウィルトピックとして「disconnected」通知をペイロードに設定しなければなりません。
:::

## サービスディスカバリー

### サービス登録

MCPサーバー起動後、MQTTブローカーにサービスを登録します。サービスディスカバリーおよび登録用のプレゼンストピックは`$mcp-server/presence/{server-id}/{server-name}`です。

MCPサーバーは起動時に「server/online」通知をサービスプレゼンスのトピックに**必ず**パブリッシュし、**RETAIN**フラグを`True`に設定しなければなりません。

「server/online」通知はメッセージサイズが大きくなりすぎないよう、サーバーの限定的な情報のみを提供することが**推奨**されます。クライアントは初期化後に詳細情報を要求できます。

- MCPサーバーの機能の簡単な説明（クライアントが必要に応じてどのMCPサーバーを初期化すべきか判断するため）
- ロールや権限などのメタデータ（クライアントがMCPサーバーのアクセス制御ポリシーを理解するため）。メタデータの`rbac`フィールドにはロールが含まれ、それぞれ名前、説明、許可されたメソッド、許可されたツール、許可されたリソースを持ちます。これはMQTTブローカーがMCPサーバーのロールベースアクセス制御（RBAC）を実装する際に利用される可能性があります。

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/server/online",
  "params": {
      "server_name": "example/server",
      "description": "このMCPサーバーが提供する機能についての簡単な説明で、クライアントが必要に応じて選択できるようにします。ツールが提供されている場合は利用可能なツールを説明しますが、メッセージサイズ削減のためツールパラメータは含みません。",
      "meta": {
        "rbac": {
          "roles": [
            {
              "name": "admin",
              "description": "フルアクセスを持つ管理者ロール",
              "allowed_methods": [
                "notifications/initialized",
                "ping", "tools/list", "tools/call", "resources/list", "resources/read",
                "resources/subscribe", "resources/unsubscribe"
              ],
              "allowed_tools": "all",
              "allowed_resources": "all"
            },
            {
              "name": "user",
              "description": "限定的アクセスを持つユーザーロール",
              "allowed_methods": [
                "notifications/initialized",
                "ping", "tools/list", "tools/call", "resources/list", "resources/read"
              ],
              "allowed_tools": [
                "get_vehicle_status", "get_vehicle_location"
              ],
              "allowed_resources": [
                "file:///vehicle/telemetry.data"
              ]
            }
          ]
        }
      }
  }
}
```

ツールのパラメータ詳細など、より詳細な情報はクライアントが必要に応じて`**/list`リクエストをサーバーに送信して取得することが**推奨**されます。

クライアントは任意のタイミングで`$mcp-server/presence/+/{server-name-filter}`トピックにサブスクライブできます。`{server-name-filter}`はserver-nameのフィルターです。

例えば、server-nameが`{server-type}/{sub-type}/{name}`であり、クライアントの権限により`{server-type}/{sub-type}`タイプのMCPサーバーのみアクセス可能と判断された場合、`$mcp-server/presence/+/{server-type}/{sub-type}/#`にサブスクライブすることで、`{sub-type}`タイプのすべてのMCPサーバーのサービスプレゼンスを一括で受信できます。

クライアントは`$mcp-server/presence/+/#`にサブスクライブしてすべてのタイプのMCPサーバーを取得可能ですが、管理者がMQTTブローカーのACLで`$mcp-rpc/{mcp-client-id}/{server-id}/{server-type}/{sub-type}/#`のようなRPCトピックのみ送受信を許可している場合があります。そのため、広範囲すぎるトピックのサブスクライブは有効ではありません。`{server-name-filter}`を適切に設計することで、不要な情報の干渉を減らせます。

### サービス登録解除

MQTTブローカーに接続する際、サーバーは予期せぬ切断時に登録情報をクリアするため、`$mcp-server/presence/{server-id}/{server-name}`をウィルトピックとして空ペイロードで設定しなければなりません。

MQTTブローカーから積極的に切断する前に、サーバーは**必ず**`$mcp-server/presence/{server-id}/{server-name}`トピックに空ペイロードメッセージを送信し、登録情報をクリアしなければなりません。

`$mcp-server/presence/{server-id}/{server-name}`トピックにおいて：

- クライアントが`server/online`通知を受信した場合、その`{server-id}`を当該`{server-name}`のインスタンスの1つとして記録します。
- クライアントが空ペイロードメッセージを受信した場合、キャッシュされた`{server-id}`をクリアします。いずれかのインスタンスがオンラインであれば、クライアントはMCPサーバーをオンラインとみなします。

サービス登録および登録解除のメッセージフローは以下の通りです：

```mermaid
sequenceDiagram
    participant MCP_Server as MCPサーバー
    participant MQTT_Broker as MQTTブローカー
    participant MCP_Client as MCPクライアント

    MCP_Server ->> MQTT_Broker: サービス登録<br/>トピック: $mcp-server/presence/{server-id}/{server-name}<br/>Retain: True
    Note right of MQTT_Broker: 保持メッセージを保存

    MCP_Client ->> MQTT_Broker: サービス購読<br/>トピックフィルター: $mcp-server/presence/+/ {server-name-filter}

    MQTT_Broker ->> MCP_Client: サービス説明<br/>トピック: $mcp-server/presence/{server-id}/{server-name}<br/>ペイロード: "notifications/server/online"
    Note left of MCP_Client: server-idをserver-nameのインスタンスとして記録

    MCP_Server ->> MQTT_Broker: サービス登録解除<br/>トピック: $mcp-server/presence/{server-id}/{server-name}<br/>Retain: True<br/>ペイロード: 空
    Note right of MQTT_Broker: 保持メッセージを削除

    MQTT_Broker ->> MCP_Client: サービス説明<br/>トピック: $mcp-server/presence/{server-id}/{server-name}<br/>ペイロード: 空
    Note left of MCP_Client: server-idを削除
```

## 初期化

本節は初期化フェーズのMQTTトランスポート固有部分のみを説明しています。詳細は[ライフサイクル](https://modelcontextprotocol.io/specification/2025-06-18/basic/lifecycle#initialization)を参照してください。

初期化フェーズはクライアントとサーバー間の最初のやり取りである必要があります。

クライアントは初期化リクエストを送信する前に、RPCトピック（`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`）に**必ず**No Localオプションを付けてサブスクライブしなければなりません。

サーバーは初期化レスポンスを送信する前に、RPCトピック（`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`）に**必ず**No Localオプションを付けてサブスクライブしなければなりません。

```mermaid
sequenceDiagram
  participant MCP_Client as MCPクライアント
  participant MCP_Server as MCPサーバー

  Note right of MCP_Client: サーバーのRPCトピックをサブスクライブ
  MCP_Client ->> MCP_Server: 初期化リクエスト<br/>トピック: $mcp-server/{server-id}/{server-name}
  Note left of MCP_Server: クライアントのRPCトピックをサブスクライブ
  MCP_Server ->> MCP_Client: 初期化レスポンス<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
  MCP_Client ->> MCP_Server: 初期化完了通知<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
  MCP_Client ->> MCP_Server: RPCリクエスト／レスポンス／通知<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
  MCP_Server ->> MCP_Client: RPCリクエスト／レスポンス／通知<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
```

クライアントは以下を含む`initialize`リクエストをトピック`$mcp-server/{server-id}/{server-name}`に送信してこのフェーズを開始しなければなりません：

- サポートするプロトコルバージョン
- クライアントの機能
- クライアントの実装情報

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "roots": {
        "listChanged": true
      },
      "sampling": {}
    },
    "clientInfo": {
      "name": "ExampleClient",
      "version": "1.0.0"
    }
  }
}
```

サーバーは自身の機能と情報をトピック`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`にレスポンスとして送信しなければなりません：

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "logging": {},
      "prompts": {
        "listChanged": true
      },
      "resources": {
        "subscribe": true,
        "listChanged": true
      },
      "tools": {
        "listChanged": true
      }
    },
    "serverInfo": {
      "name": "ExampleServer",
      "version": "1.0.0"
    }
  }
}
```

初期化成功後、クライアントは通常の操作開始準備完了を示すため、トピック`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`に`initialized`通知を送信しなければなりません：

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

## 機能リスト変更

初期化リクエストを送信する前に、MCPクライアントはMCPサーバーの機能リスト変更トピック`$mcp-server/capability/{server-id}/{server-name-filter}`にサブスクライブしなければなりません。`{server-name-filter}`はserver-nameのフィルターです。

MCPサーバーは初期化リクエストに応答する前に、MCPクライアントの機能リスト変更トピック`$mcp-client/capability/{mcp-client-id}`にサブスクライブしなければなりません。

機能リストの更新があった場合：

- サーバーは通知を`$mcp-server/capability/{server-id}/{server-name}`に送信します。
- クライアントは通知を`$mcp-client/capability/{mcp-client-id}`に送信します。

機能リスト変更通知のペイロードは変更された特定の機能に依存します。例えばツールの場合は`notifications/tools/list_changed`です。機能リスト変更通知を受信後、クライアントまたはサーバーは更新された機能リストを取得する必要があります。詳細は各機能のドキュメントを参照してください。

```mermaid
sequenceDiagram
    participant MCP_Client as MCPクライアント
    participant MCP_Server as MCPサーバー

    Note right of MCP_Client: クライアントはサーバーの<br/>機能変更トピックをサブスクライブ
    MCP_Client ->> MCP_Server: 初期化

    Note left of MCP_Server: サーバーはクライアントの<br/>機能変更トピックをサブスクライブ
    MCP_Server ->> MCP_Client: 初期化レスポンス
    MCP_Client ->> MCP_Server: 初期化完了通知

    MCP_Server -->> MCP_Client: 機能リスト変更通知<br/>トピック: $mcp-server/capability/{server-id}/{server-name}

    MCP_Client ->> MCP_Server: 機能リスト取得リクエスト<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}

    MCP_Server -->> MCP_Client: 機能リスト取得レスポンス<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}
```

## リソース更新

MCPプロトコルではクライアントが特定リソースの変更をサブスクライブ可能です。

サーバーがリソースサブスクライブ機能を提供する場合、クライアントは初期化完了通知を送信する前にリソース変更にサブスクライブできます。

リソース変更のサブスクライブトピックは`$mcp-server/capability/{server-id}/{server-name}`です。

リソースが変更された場合、サーバーは`$mcp-server/capability/{server-id}/{server-name}`に通知を送信することが**推奨**されます。

```mermaid
sequenceDiagram
    participant MCP_Client as MCPクライアント
    participant MCP_Server as MCPサーバー

    MCP_Client ->> MCP_Server: 初期化
    MCP_Server ->> MCP_Client: 初期化レスポンス
    Note right of MCP_Client: クライアントはサーバーの<br/>リソース更新トピックをサブスクライブ
    MCP_Client ->> MCP_Server: 初期化完了通知

    MCP_Client ->> MCP_Server: リソース一覧取得リクエスト<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}

    MCP_Server -->> MCP_Client: リソース一覧取得レスポンス<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}<br/>URI一覧: [{resource-uri}, {resource-uri}, ...]

    MCP_Server -->> MCP_Client: リソース更新通知<br/>トピック: $mcp-server/capability/{server-id}/{server-name}<br/>URI: {resource-uri}

    MCP_Client ->> MCP_Server: リソース読み取りリクエスト<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}<br/>URI: {resource-uri}

    MCP_Server -->> MCP_Client: リソース読み取りレスポンス<br/>トピック: $mcp-rpc/{mcp-client-id}/{server-id}/{server-name}<br/>URI: {resource-uri}
```

## シャットダウン

### サーバー切断

サーバーは予期せぬ切断時にクライアントに通知するため、ウィルメッセージを設定しなければなりません。ウィルトピックは`$mcp-server/presence/{server-id}/{server-name}`で、ペイロードは空です。

MCPサーバーがMQTTブローカーから切断する前に、**必ず**プレゼンストピック`$mcp-server/presence/{server-id}/{server-name}`に空ペイロードメッセージを送信して登録情報をクリアしなければなりません。

MCPサーバーはMCPクライアントとの「非初期化（de-initialize）」を行いながらMQTTブローカーとの接続を維持したい場合、RPCトピック`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`に「disconnected」通知を送信し、以下のトピックからサブスクライブ解除しなければなりません：
- `$mcp-client/capability/{mcp-client-id}`
- `$mcp-client/presence/{mcp-client-id}`
- `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`

MCPサーバーの「disconnected」通知のメッセージ形式：

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/disconnected"
}
```

MCPクライアントがサーバーのプレゼンストピックで空ペイロードメッセージ、またはRPCトピックで「disconnected」通知を受信した場合、サーバーをオフラインとみなし、当該`{server-name}`のキャッシュされた`{server-id}`をクリアし、以下のトピックからサブスクライブ解除しなければなりません：
- `$mcp-server/capability/{server-id}/{server-name-filter}`
- `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name-filter}`

### クライアント切断

サーバーは初期化レスポンス送信前にクライアントのプレゼンストピック（`$mcp-client/presence/{mcp-client-id}`）をサブスクライブしなければなりません。

クライアントは予期せぬ切断時にサーバーに通知するため、ウィルメッセージを設定しなければなりません。ウィルトピックは`$mcp-client/presence/{mcp-client-id}`で、ペイロードは「disconnected」通知です。

クライアントがMQTTブローカーから切断する前に、**必ず**トピック`$mcp-client/presence/{mcp-client-id}`に「disconnected」通知を送信しなければなりません。

クライアントがMCPサーバーとの「非初期化（de-initialize）」を行いながらMQTTブローカーとの接続を維持したい場合、RPCトピック`$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`に「disconnected」通知を送信し、以下のトピックからサブスクライブ解除しなければなりません：
- `$mcp-server/capability/{server-id}/{server-name-filter}`
- `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name-filter}`

MCPサーバーはクライアントから「disconnected」通知を受信後、以下のトピックからサブスクライブ解除しなければなりません：
- `$mcp-client/capability/{mcp-client-id}`
- `$mcp-client/presence/{mcp-client-id}`
- `$mcp-rpc/{mcp-client-id}/{server-id}/{server-name}`

MCPクライアントの「disconnected」通知のメッセージ形式：

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/disconnected"
}
```

## ヘルスチェック

クライアントまたはサーバーは任意のタイミングでサーバーに`ping`リクエストを送信して相手のヘルスチェックを行うことが**可能**です。

- クライアントがサーバーから合理的な時間内に`ping`レスポンスを受信しない場合、クライアントはトピック`$mcp-client/presence/{mcp-client-id}`に「disconnected」通知を送信し、自身を切断しなければなりません。
- サーバーがクライアントから合理的な時間内に`ping`レスポンスを受信しない場合、サーバーは他のRPCリクエストをクライアントに送信しなければなりません。

詳細は[Ping](https://modelcontextprotocol.io/specification/2025-06-18/basic/utilities/ping)を参照してください。

## タイムアウト

すべてのRPCリクエストはMQTTメッセージで非同期送信されるため、タイムアウトの考慮が必要です。タイムアウト時間はRPCリクエストごとに異なる場合がありますが、設定可能であるべきです。

本プロトコルで推奨される各RPCリクエストのデフォルトタイムアウト値は以下の通りです：

- "initialize": 30秒
- "ping": 10秒
- "roots/list": 30秒
- "resources/list": 30秒
- "tools/list": 30秒
- "prompts/list": 30秒
- "prompts/get": 30秒
- "sampling/createMessage": 60秒
- "resources/read": 30秒
- "resources/templates/list": 30秒
- "resources/subscribe": 30秒
- "tools/call": 60秒
- "completion/complete": 60秒
- "logging/setLevel": 30秒

<!-- {< callout type="info" >}
進捗リクエストは通知として送信され、レスポンス不要のためタイムアウトは不要です。
{< /callout >} -->

## エラーハンドリング

実装は以下のエラーケースに対応できることが**推奨**されます：

- プロトコルバージョン不一致
- 必要な機能のネゴシエーション失敗
- 初期化リクエストのタイムアウト
- シャットダウンのタイムアウト

実装はすべてのリクエストに適切なタイムアウトを実装し、接続のハングやリソース枯渇を防止することが**推奨**されます。

初期化エラーの例：

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32602,
    "message": "Unsupported protocol version",
    "data": {
      "supported": ["2025-03-26"],
      "requested": "1.0.0"
    }
  }
}
```
