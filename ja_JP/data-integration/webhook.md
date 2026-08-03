# Webhook

Webhookは、EMQXのクライアントメッセージやイベントを外部のHTTPサーバーと連携させる手段を提供します。ルールエンジンやデータブリッジを使用する場合と比べて、Webhookはよりシンプルな方法であり、導入のハードルを大幅に下げ、EMQXと外部システム間の連携を迅速に実現できます。

本ページでは、Webhookに関する情報と実践的な利用方法を包括的に紹介します。


<video
  src="https://assets.emqx.com/data/video/emqx-docs/data-integration/webhook_integration.mp4"
  preload="metadata"
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## How It Works

クライアントが特定のトピックにメッセージをパブリッシュしたり、特定の操作を行うとWebhookがトリガーされます。Webhookはルールエンジンがサポートするすべてのメッセージおよびイベントに対応しています。

Webhookは以下のシナリオでトリガーされるように設定できます。各イベントのリクエスト内容については、[SQLデータソースとフィールド](./rule-sql-events-and-fields.md)を参照してください。

![EMQX Webhook 集成](./assets/emqx-integration-http.jpg)

### メッセージ

パブリッシャーがメッセージをパブリッシュしたり、メッセージの状態が変化した場合に発生します。具体的には以下のイベントです：

- メッセージがパブリッシュされた
- メッセージが配信された
- メッセージがアックされた
- メッセージがフォワードされドロップされた
- メッセージの配信がドロップされた

複数のトピックフィルターをメッセージに設定可能で、マッチしたメッセージのみがWebhookをトリガーします。

### イベント

クライアントが特定の操作を行ったり、状態が変化した場合に発生します。具体的には以下のイベントです：

- 接続が確立された
- 接続が切断された
- 接続が確認された
- 認可結果
- セッションのサブスクライブ完了
- セッションのサブスクライブ解除

## Features

EMQXのWebhook連携を利用することで、以下のようなメリットをビジネスにもたらします：

- **より多くの下流システムへデータを渡せる**：Webhookを使うことで、MQTTデータを分析プラットフォームやクラウドサービスなど、より多くの外部システムに簡単に連携でき、多様なシステム間でのデータ配信が可能になります。
- **リアルタイム応答と業務プロセスのトリガー**：Webhookを通じて外部システムがMQTTデータをリアルタイムに受け取り、業務プロセスをトリガーできます。例えば、アラームデータを受けて業務フローを起動するなど、迅速な対応が可能です。
- **データ処理のカスタマイズ**：外部システム側で受け取ったデータをさらに加工し、より複雑な業務ロジックを実装できます。EMQXの機能に縛られず柔軟な処理が可能です。
- **疎結合な連携方式**：WebhookはシンプルなHTTPインターフェースを利用するため、システム間の疎結合な連携方法を提供します。

まとめると、Webhook連携はリアルタイムかつ柔軟でカスタマイズ可能なデータ連携機能を提供し、多様で豊かなアプリケーション開発ニーズに応えます。

## Get Started

本節ではmacOSを例に、Webhookの設定と利用方法を紹介します。

### HTTPサービスの作成

ここではPythonを使ってローカルのポート5000で待ち受けるHTTPサーバーを簡単に作成し、Webhookリクエストを受け取った際にURLを表示します。実際の用途では、ご自身の業務サーバーに置き換えてください。

まず、Pythonで`POST /`リクエストを受け取る簡単なHTTPサービスを作成します。リクエスト内容を表示し、200 OKを返します：

```python
from flask import Flask, json, request

api = Flask(__name__)

@api.route('/', methods=['POST'])
def print_messages():
  reply= {"result": "ok", "message": "success"}
  print("got post request: ", request.get_data())
  return json.dumps(reply), 200

if __name__ == '__main__':
  api.run()
```

上記コードを`http_server.py`として保存し、ファイルのあるディレクトリで以下のコマンドを実行します：

```shell
# flask依存関係のインストール
pip install flask

# サービス起動
python3 http_server.py
```

### Webhookの作成

1. ダッシュボードの左メニューから **Integration** -> **Webhooks** をクリックします。

2. ページ上の **Create Webhook** ボタンをクリックします。

3. Webhookの **Name** と任意で **Note** を入力します。

   名前は英大文字・小文字と数字のみを含むべきです。例：`my_webhook`

4. 要件に応じて **Trigger** を選択します。

   本例では **All Messages and Events** を選択します。その他のオプションの詳細は[How it Works](#how-it-works)を参照してください。

5. リクエスト設定を行います：

   - **Method**：`POST`
   - **URL**：`http://localhost:5000`

   URL欄の横にある **Test** ボタンで接続確認が可能です。他の設定はデフォルトのままで構いません。

6. ページ下部の **Save** をクリックし、Webhookを作成します。

   ![EMQX Webhook](./assets/webhook.png)

これでWebhookが正常に作成されました。

#### OAuth2クライアントクレデンシャルの設定

EMQX 6.0.4以降、WebhookはOAuth 2.0のクライアントクレデンシャルグラントをサポートしています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュし、自動で更新します。Webhookリクエスト送信時には、`Authorization: Bearer <access_token>` ヘッダーを付与し、ターゲットサーバー側でEMQXの認証を行います。

トークンエンドポイント、クライアントクレデンシャル、許可されたスコープはOAuth2認可サーバー、IDプロバイダー（IdP）、またはターゲットAPI管理者から取得してください。**OAuth2 Client Credentials**をオンにし、以下の設定を行います：

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークン取得に使用するOAuth2認可サーバーのエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含んではいけません。 |
| **Client ID** | 必須。アクセストークン取得に使用するOAuth2クライアントID。 |
| **Client Secret** | 必須。アクセストークン取得に使用するOAuth2クライアントシークレット。 |
| **Scope** | 任意。アクセストークンに要求するOAuth2スコープ。複数スコープはスペース区切り。認可サーバーがスコープを要求しない場合は空欄にします。 |
| **Token Request Timeout** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは`5`秒。 |
| **Enable TLS** | トークンエンドポイントにTLSを有効にする場合はトグルをオンにします。 |

EMQXは`application/x-www-form-urlencoded`のコンテンツタイプで`POST`リクエストをトークンエンドポイントに送信します。リクエストボディには`grant_type`、`client_id`、`client_secret`、および任意の`scope`が含まれます。トークンエンドポイントは`200`レスポンスでJSONボディに`access_token`を返す必要があります。`token_type`と`expires_in`も返すことができ、存在する場合は`token_type`は`Bearer`、`expires_in`は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2を有効にした場合、Webhookに`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証の`Authorization`ヘッダーによる認証はサポートされていません。

:::

### Webhookのテスト

MQTTX CLIを使って`t/1`トピックにメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Webhook" }'
```

この操作により、以下のイベントが順にトリガーされます：

- 接続確立
- 接続確認
- 認可チェック完了
- メッセージパブリッシュ
- 接続切断

もし`t/1`トピックにサブスクライバーがいなければ、メッセージパブリッシュ後に**メッセージフォワードおよびドロップ**イベントもトリガーされます。

対応するイベントとメッセージデータがHTTPサービスに転送されているか確認してください。以下のようなデータが表示されるはずです：

```shell
got post request:  b'{"username":"undefined","timestamp":1694681417717,"sockname":"127.0.0.1:1883","receive_maximum":32,"proto_ver":5,"proto_name":"MQTT","peername":"127.0.0.1:61003","node":"emqx@127.0.0.1","mountpoint":"undefined","metadata":{"rule_id":"my-webhook_WH_D"},"keepalive":30,"is_bridge":false,"expiry_interval":0,"event":"client.connected","connected_at":1694681417714,"conn_props":{"User-Property":{},"Request-Problem-Information":1},"clientid":"emqx_c","clean_start":true}'
127.0.0.1 - - [14/Sep/2023 16:50:17] "POST / HTTP/1.1" 200 -
got post request:  b'{"username":"undefined","timestamp":1694681417719,"sockname":"127.0.0.1:1883","reason_code":"success","proto_ver":5,"proto_name":"MQTT","peername":"127.0.0.1:61003","node":"emqx@127.0.0.1","metadata":{"rule_id":"my-webhook_WH_D"},"keepalive":30,"expiry_interval":0,"event":"client.connack","conn_props":{"User-Property":{},"Request-Problem-Information":1},"clientid":"emqx_c","clean_start":true}'
127.0.0.1 - - [14/Sep/2023 16:50:17] "POST / HTTP/1.1" 200 -
got post request:  b'{"username":"undefined","topic":"t/1","timestamp":1694681417728,"result":"allow","peerhost":"127.0.0.1","node":"emqx@127.0.0.1","metadata":{"rule_id":"my-webhook_WH_D"},"event":"client.check_authz_complete","clientid":"emqx_c","authz_source":"file","action":"publish"}'
127.0.0.1 - - [14/Sep/2023 16:50:17] "POST / HTTP/1.1" 200 -
got post request:  b'{"username":"undefined","topic":"t/1","timestamp":1694681417728,"qos":0,"publish_received_at":1694681417728,"pub_props":{"User-Property":{}},"peerhost":"127.0.0.1","payload":"{ \\"msg\\": \\"Hello Webhook\\" }","node":"emqx@127.0.0.1","metadata":{"rule_id":"my-webhook_WH_D"},"id":"0006054DC3E940F8F445000038A60002","flags":{"retain":false,"dup":false},"event":"message.publish","clientid":"emqx_c"}'
127.0.0.1 - - [14/Sep/2023 16:50:17] "POST / HTTP/1.1" 200 -
got post request:  b'{"username":"undefined","topic":"t/1","timestamp":1694681417729,"reason":"no_subscribers","qos":0,"publish_received_at":1694681417728,"pub_props":{"User-Property":{}},"peerhost":"127.0.0.1","payload":"{ \\"msg\\": \\"Hello Webhook\\" }","node":"emqx@127.0.0.1","metadata":{"rule_id":"my-webhook_WH_D"},"id":"0006054DC3E940F8F445000038A60002","flags":{"retain":false,"dup":false},"event":"message.dropped","clientid":"emqx_c"}'
127.0.0.1 - - [14/Sep/2023 16:50:17] "POST / HTTP/1.1" 200 -
got post request:  b'{"username":"undefined","timestamp":1694681417729,"sockname":"127.0.0.1:1883","reason":"normal","proto_ver":5,"proto_name":"MQTT","peername":"127.0.0.1:61003","node":"emqx@127.0.0.1","metadata":{"rule_id":"my-webhook_WH_D"},"event":"client.disconnected","disconnected_at":1694681417729,"disconn_props":{"User-Property":{}},"clientid":"emqx_c"}'
127.0.0.1 - - [14/Sep/2023 16:50:17] "POST / HTTP/1.1" 200 -
```
