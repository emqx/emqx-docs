# Webhook

Webhookは、EMQXのクライアントメッセージやイベントを外部のHTTPサーバーと連携させる方法を提供します。ルールエンジンやデータブリッジを使用する場合と比べて、Webhookはよりシンプルな手法であり、導入のハードルを大幅に下げ、EMQXと外部システムの迅速な連携を実現します。

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

Webhookは以下のシナリオでトリガーされるよう設定できます。各イベントのリクエスト内容については、[SQL Data Source and Fields](./rule-sql-events-and-fields.md)を参照してください。

![EMQX Webhook 集成](./assets/emqx-integration-http.jpg)

### メッセージ

パブリッシャーがメッセージをパブリッシュしたり、メッセージの状態が変化したときにトリガーされます。具体的には以下のイベントです。

- メッセージがパブリッシュされた
- メッセージが配信された
- メッセージがアックされた
- メッセージがフォワードされドロップされた
- メッセージ配信がドロップされた

複数のトピックフィルターを設定でき、該当するメッセージのみがWebhookをトリガーします。

### イベント

クライアントが特定の操作を行ったり、状態が変化したときにトリガーされます。具体的には以下のイベントです。

- 接続確立
- 接続切断
- 接続確認
- 認可結果
- セッションのサブスクライブ完了
- セッションのサブスクライブ解除

## Features

EMQXのWebhook連携を利用することで、以下のようなメリットをビジネスにもたらします。

- **より多くの下流システムへデータを渡す**  
  Webhookにより、MQTTデータを分析プラットフォームやクラウドサービスなど、より多くの外部システムに簡単に連携でき、マルチシステムでのデータ配信が可能になります。

- **リアルタイム応答と業務プロセスのトリガー**  
  Webhookを通じて外部システムがMQTTデータをリアルタイムに受信し、業務プロセスをトリガーできるため迅速な対応が可能です。例えば、アラームデータを受け取り業務ワークフローを起動するなどです。

- **データ処理のカスタマイズ**  
  外部システム側で受信したデータをさらに加工し、より複雑な業務ロジックを実装でき、EMQXの機能に制限されません。

- **疎結合な連携手法**  
  WebhookはシンプルなHTTPインターフェースを利用するため、システム間の疎結合な連携方法を提供します。

まとめると、Webhook連携はリアルタイムで柔軟かつカスタマイズ可能なデータ統合を実現し、柔軟で豊かなアプリケーション開発のニーズに応えます。

## Get Started

本節ではmacOSを例に、Webhookの設定と利用方法を紹介します。

### HTTPサービスの作成

ここではPythonを使ってローカルのポート5000で待ち受けるHTTPサーバーを簡単に作成し、Webhookリクエストを受信した際に内容を表示します。実際の運用では業務サーバーに置き換えてください。

まず、Pythonで`POST /`リクエストを受け取るシンプルなHTTPサービスを作成します。リクエスト内容を表示し、200 OKを返します。

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

上記コードを`http_server.py`として保存し、ファイルのあるディレクトリで以下のコマンドを実行します。

```shell
# flask依存関係のインストール
pip install flask

# サービス起動
python3 http_server.py
```

### Webhookの作成

1. ダッシュボードの左メニューから **Integration** -> **Webhooks** をクリックします。

2. ページの **Create Webhook** ボタンをクリックします。

3. Webhookの **Name** と任意の **Note** を入力します。

   名前は英大文字・小文字と数字のみを含む必要があります。例：`my_webhook`

5. Webhookリクエスト設定を行います。

   - リクエストメソッドに `POST` を選択し、**URL** に `http://localhost:5000` を設定します。
   - 必要に応じて **Query String** にクエリパラメータを追加したり、**Headers** にカスタムHTTPリクエストヘッダーを設定できます。
   - OAuth2でWebhookリクエストを保護する場合は、**OAuth2 Client Credentials** をオンにして必要な設定を行います。詳細は[Configure OAuth2 Client Credentials](#configure-oauth2-client-credentials)を参照してください。URL入力欄横の **Test** ボタンで接続テストが可能です。

   本例では **All Messages and Events** を選択します。その他のオプションについては[How it Works](#how-it-works)を参照してください。

5. リクエスト設定を行います。

   - **Method**: `POST`
   - **URL**: `http://localhost:5000`

   URL欄横の **Test** ボタンで接続確認ができます。その他の設定はデフォルトのままで構いません。

6. ページ下部の **Save** をクリックしてWebhookを作成します。

   ![EMQX Webhook](./assets/webhook.png)

Webhookが正常に作成されました。

#### OAuth2 Client Credentialsの設定

EMQX 6.0.4以降、WebhookはOAuth 2.0 Client Credentials Grantをサポートしています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュし、自動で更新します。Webhookリクエスト送信時には`Authorization: Bearer <access_token>`ヘッダーにトークンを含め、ターゲットサーバーがEMQXを認証できるようにします。

OAuth2認可サーバーやIdP、ターゲットAPI管理者からトークンエンドポイント、クライアント認証情報、許可されたスコープを取得してください。**OAuth2 Client Credentials**をオンにして以下の設定を行います。

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークン取得に使用するOAuth2認可サーバーのエンドポイント。HTTPまたはHTTPSで、ユーザー情報を含まないURLである必要があります。 |
| **Client ID** | 必須。アクセストークン取得に使用するOAuth2クライアントID。 |
| **Client Secret** | 必須。アクセストークン取得に使用するOAuth2クライアントシークレット。 |
| **Scope** | 任意。アクセストークン取得時に要求するOAuth2スコープ。複数はスペースで区切る。認可サーバーがスコープ不要の場合は空欄にします。 |
| **Token Request Timeout** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは`5`秒。 |
| **Enable TLS** | トークンエンドポイントにTLSを有効にする場合はオンにします。 |

EMQXは`application/x-www-form-urlencoded`のPOSTリクエストで、`grant_type`、`client_id`、`client_secret`、任意の`scope`を含むリクエストボディを送信します。トークンエンドポイントは`200`レスポンスでJSONボディに`access_token`を返す必要があります。`token_type`と`expires_in`も返すことができ、`token_type`がある場合は`Bearer`でなければならず、`expires_in`は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2を有効にした場合、Webhookの`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証の`Authorization`ヘッダーによる認証はサポートされていません。

:::

### Webhookのテスト

MQTTX CLIを使って` t/1`トピックにメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Webhook" }'
```

この操作により、以下のイベントが順にトリガーされます。

- 接続確立
- 接続確認
- 認可チェック完了
- メッセージパブリッシュ
- 接続切断

` t/1`トピックにサブスクライバーがいない場合は、メッセージパブリッシュ後に**メッセージフォワードおよびドロップ**イベントもトリガーされます。

HTTPサービスに対応するイベントとメッセージデータが転送されているか確認してください。以下のようなデータが表示されるはずです。

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
