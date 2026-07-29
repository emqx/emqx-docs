# Webhook

Webhookは、EMQXのクライアントメッセージやイベントを外部のHTTPサーバーと連携させる方法を提供します。ルールエンジンやデータブリッジを使用する場合と比べて、Webhookはよりシンプルな手法であり、導入のハードルを大幅に下げ、EMQXと外部システムの迅速な連携を可能にします。

本ページでは、Webhookに関する情報と実践的な利用方法を包括的に紹介します。

## 仕組み

クライアントが特定のトピックにメッセージをパブリッシュしたり、特定の操作を行うと、Webhookがトリガーされます。Webhookはルールエンジンがサポートするすべてのメッセージおよびイベントに対応しています。

Webhookは以下のシナリオでトリガーされるよう設定できます。各イベントのリクエスト内容については、[SQLデータソースとフィールド](./rule-sql-events-and-fields.md)を参照してください。

![EMQX Webhook 集成](./assets/emqx-integration-http.jpg)

### メッセージ

パブリッシャーがメッセージをパブリッシュしたり、メッセージの状態が変化した場合にトリガーされます。具体的には以下のイベントです：

- メッセージパブリッシュ
- メッセージ配信完了
- メッセージアック（確認応答）
- メッセージ転送およびドロップ
- メッセージ配信ドロップ

メッセージに対して複数のトピックフィルターを設定可能で、マッチしたメッセージのみがWebhookをトリガーします。

### イベント

クライアントが特定の操作を行ったり、状態が変化した場合にトリガーされます。具体的には以下のイベントです：

- 接続確立
- 接続終了
- 接続確認完了
- 認可結果
- セッションのサブスクライブ完了
- セッションのサブスクライブ解除

## 特長

EMQXのWebhook連携を利用することで、以下のようなメリットが得られます。

- **より多くの下流システムへのデータ連携**  
  Webhookにより、MQTTデータを分析プラットフォームやクラウドサービスなどの外部システムに簡単に連携でき、複数システムへのデータ配信を実現します。

- **リアルタイム応答および業務プロセスのトリガー**  
  Webhookを通じて外部システムがMQTTデータをリアルタイムに受信し、業務プロセスをトリガー可能です。例えば、アラームデータを受け取り業務ワークフローを起動するなど迅速な対応が可能です。

- **データ処理のカスタマイズ**  
  外部システム側で受信データをさらに加工し、より複雑な業務ロジックを実装できます。EMQXの機能に制約されることなく柔軟な処理が可能です。

- **疎結合な連携手法**  
  WebhookはシンプルなHTTPインターフェースを利用するため、システム間の疎結合な連携方式を提供します。

まとめると、Webhook連携はリアルタイムで柔軟かつカスタマイズ可能なデータ連携機能を提供し、多様で豊富なアプリケーション開発ニーズに応えます。

## はじめに

本節ではmacOSを例に、Webhookの設定および利用方法を紹介します。

### HTTPサービスの作成

ここではPythonを使ってローカルのポート5000で待ち受けるHTTPサーバーを簡単に作成し、Webhookリクエストを受信した際にURLを表示します。実際の運用では業務用サーバーに置き換えてください。

まず、Pythonで`POST /`リクエストを受け付けるシンプルなHTTPサービスを構築します。リクエスト内容を標準出力に表示し、200 OKを返します。

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

2. ページ上の **Create** ボタンをクリックします。

3. Webhook名と説明を入力します。英数字の大文字・小文字の組み合わせで指定してください。ここでは `my_webhook` と入力します。

4. トリガーを用途に応じて選択します。ここでは **All messages and events** を選択します。その他の選択肢は[仕組み](#仕組み)を参照してください。

5. リクエストメソッドに `POST` を選択し、**URL** に `http://localhost:5000` を設定します。OAuth2でWebhookリクエストを保護する場合は、**OAuth2 Client Credentials** をオンにして必要な設定を行います。詳細は[OAuth2 Client Credentialsの設定](#configure-oauth2-client-credentials)を参照してください。URL入力欄横の **Test** ボタンで接続テストが可能です。

6. **Save** をクリックしてWebhook作成を完了します。

   ![EMQX Webhook](./assets/webhook.png)

これでWebhookの作成が完了しました。

#### OAuth2 Client Credentialsの設定

EMQX 6.0.4以降、WebhookはOAuth 2.0 Client Credentials Grantに対応しています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュし、自動で更新します。Webhookリクエスト送信時には、`Authorization: Bearer <access_token>` ヘッダーを付与し、送信先サーバーでEMQXの認証を可能にします。

OAuth2認可サーバー、IDプロバイダー（IdP）、または対象API管理者からトークンエンドポイント、クライアント認証情報、許可されたスコープを取得してください。**OAuth2 Client Credentials** をオンにし、以下の設定を行います。

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークン取得に使用するOAuth2認可サーバーのエンドポイント。HTTPまたはHTTPSのURLで、ユーザー情報を含んではいけません。 |
| **Client ID** | 必須。アクセストークン取得に使用するOAuth2クライアントID。 |
| **Client Secret** | 必須。アクセストークン取得に使用するOAuth2クライアントシークレット。 |
| **Scope** | 任意。アクセストークンに要求するOAuth2スコープ。複数ある場合はスペース区切りで指定。認可サーバーがスコープを要求しない場合は空欄にします。 |
| **Token Request Timeout** | トークンエンドポイントへのHTTPリクエストのタイムアウト秒数。デフォルトは `5` 秒です。 |
| **Enable TLS** | トークンエンドポイントにTLSを有効にする場合はスイッチをオンにします。 |

EMQXは`application/x-www-form-urlencoded`のコンテンツタイプで`POST`リクエストをトークンエンドポイントに送信します。リクエストボディには`grant_type`、`client_id`、`client_secret`、および任意の`scope`が含まれます。トークンエンドポイントは`200`レスポンスで`access_token`を含むJSONボディを返す必要があります。`token_type`と`expires_in`も返すことができ、存在する場合は`token_type`は`Bearer`、`expires_in`は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2を有効にしている場合、Webhookに対して`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証の`Authorization`ヘッダーによる認証はサポートしていません。

:::

### Webhookのテスト

MQTTX CLIを使って、`t/1`トピックにメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Webhook" }'
```

この操作により、以下のイベントが順にトリガーされます。

- 接続確立
- 接続確認完了
- 認可チェック完了
- メッセージパブリッシュ
- 接続終了

もし`t/1`トピックにサブスクライバーがいなければ、メッセージパブリッシュ後に**メッセージ転送およびドロップ**イベントもトリガーされます。

HTTPサービスに対応するイベントおよびメッセージデータが転送されているか確認してください。以下のようなデータが表示されるはずです。

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
