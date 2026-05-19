# Webhook

Webhookは、EMQXのクライアントメッセージやイベントを外部のHTTPサーバーと連携させる方法を提供します。ルールエンジンやデータブリッジを使用する場合と比べて、Webhookはよりシンプルな手法であり、導入のハードルを大幅に下げ、EMQXと外部システム間の迅速な連携を可能にします。

本ページでは、Webhookに関する情報と実際の利用方法を包括的に紹介します。


<video
  src="https://assets.emqx.com/data/video/emqx-docs/data-integration/webhook_integration.mp4"
  preload="metadata"
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## 動作原理

クライアントが特定のトピックにメッセージをパブリッシュしたり、特定の操作を行うとWebhookがトリガーされます。Webhookはルールエンジンがサポートするすべてのメッセージおよびイベントに対応しています。

Webhookは以下のシナリオでトリガーされるよう設定可能です。各イベントのリクエスト内容については[SQLデータソースとフィールド](./rule-sql-events-and-fields.md)を参照してください。

![EMQX Webhook 集成](./assets/emqx-integration-http.png)

### メッセージ

パブリッシャーがメッセージをパブリッシュした場合や、メッセージの状態が変化した場合にトリガーされます。具体的には以下を含みます：

- メッセージがパブリッシュされたとき
- メッセージが配信されたとき
- メッセージがアックされたとき
- メッセージが転送されドロップされたとき
- メッセージ配信がドロップされたとき

複数のトピックフィルターを設定でき、マッチしたメッセージのみがWebhookをトリガーします。

### イベント

クライアントが特定の操作を行った場合や状態が変化した場合にトリガーされます。具体的には以下を含みます：

- 接続確立時
- 接続終了時
- 接続確認時
- 認可結果時
- セッションのサブスクライブ完了時
- セッションのサブスクライブ解除時

## 特長

EMQXのWebhook連携を利用することで、以下のようなメリットがあります：

- **より多くの下流システムへデータを渡せる**：Webhookにより、MQTTデータを分析プラットフォームやクラウドサービスなど、より多くの外部システムに簡単に連携でき、多システムへのデータ配信が可能になります。
- **リアルタイム応答と業務プロセスのトリガー**：Webhookを通じて外部システムがMQTTデータをリアルタイムに受信し、業務プロセスをトリガーできるため、迅速な対応が可能です。例えば、アラームデータを受け取って業務フローを起動するなど。
- **データ処理のカスタマイズ**：外部システム側で受信したデータをさらに必要に応じて処理し、より複雑な業務ロジックを実装でき、EMQXの機能に制限されません。
- **疎結合な連携方式**：WebhookはシンプルなHTTPインターフェースを利用するため、システム連携を疎結合に実現できます。

まとめると、Webhook連携はリアルタイムで柔軟かつカスタマイズ可能なデータ統合を提供し、柔軟で豊富なアプリケーション開発のニーズに応えます。

## はじめに

本節ではmacOSを例に、Webhookの設定と利用方法を紹介します。

### HTTPサービスの作成

ここではPythonを使ってローカルのポート8082で待ち受けるHTTPサーバーを簡単に作成し、Webhookリクエストを受信した際にURLを表示します。実際の運用では、ご自身の業務サーバーに置き換えてください。

まず、Pythonで`POST /`リクエストを受け付ける簡単なHTTPサービスを作成します。リクエスト内容を表示し、200 OKを返します：

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

2. ページの **Create Webhook** ボタンをクリックします。

3. Webhookの **Name** と任意の **Note** を入力します。

   名前は英大文字・小文字および数字のみで構成してください。例：`my_webhook`

4. 要件に応じて **Trigger** を選択します。

   本例では **All Messages and Events** を選択します。その他のオプションの詳細は[動作原理](#動作原理)を参照してください。

5. リクエスト設定を行います：

   - **Method**：`POST`
   - **URL**：`http://localhost:5000`

   URL欄の横にある **Test** ボタンで接続確認が可能です。他の設定はデフォルトのままで構いません。

6. ページ下部の **Save** をクリックしてWebhookを作成します。

   ![EMQX Webhook](./assets/webhook.png)

これでWebhookが正常に作成されました。

### Webhookのテスト

MQTTX CLIを使って` t/1`トピックにメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Webhook" }'
```

この操作により、以下のイベントが順にトリガーされます：

- 接続確立
- 接続確認
- 認可チェック完了
- メッセージパブリッシュ
- 接続終了

もし` t/1`トピックにサブスクライバーがいなければ、メッセージパブリッシュ後に**メッセージ転送およびドロップ**イベントもトリガーされます。

HTTPサービスに対応するイベントおよびメッセージデータが転送されているか確認してください。以下のようなデータが表示されるはずです：

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
