# Webhook

Webhook は、EMQX クライアントのメッセージやイベントを外部の HTTP サーバーと連携させる方法を提供します。ルールエンジンやデータブリッジを使用する場合と比較して、Webhook はよりシンプルな手法であり、導入のハードルを大幅に下げ、EMQX と外部システム間の連携を迅速に実現できます。

このページでは、Webhook に関する情報と実践的な使用方法を包括的に紹介します。


<video
  src="https://assets.emqx.com/data/video/emqx-docs/data-integration/webhook_integration.mp4"
  preload="metadata"
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## 仕組み

クライアントが特定のトピックにメッセージをパブリッシュしたり、特定の操作を行うと、Webhook がトリガーされます。Webhook はルールエンジンがサポートするすべてのメッセージおよびイベントに対応しています。

Webhook は以下のシナリオでトリガーされるように設定できます。各イベントのリクエスト内容については、[SQL データソースとフィールド](./rule-sql-events-and-fields.md)を参照してください。

![EMQX Webhook 集成](./assets/emqx-integration-http.png)

### メッセージ

パブリッシャーがメッセージをパブリッシュしたり、メッセージの状態が変化した場合、以下を含みます：

- メッセージがパブリッシュされた
- メッセージが配信された
- メッセージがアックされた
- メッセージが転送されドロップされた
- メッセージ配信がドロップされた

メッセージに対して複数のトピックフィルターを設定でき、マッチしたメッセージのみが Webhook をトリガーします。

### イベント

クライアントが特定の操作を行ったり、状態が変化した場合、以下を含みます：

- 接続が確立された
- 接続が終了した
- 接続が確認された
- 認可結果
- セッションのサブスクライブ完了
- セッションのサブスクライブ解除

## 特長

EMQX の Webhook 連携を利用することで、以下のようなメリットをビジネスにもたらします：

- **より多くの下流システムへデータを連携**：Webhook により、MQTT データを分析プラットフォームやクラウドサービスなど、より多くの外部システムに簡単に連携でき、多システムへのデータ配信が可能になります。
- **リアルタイム応答と業務プロセスのトリガー**：Webhook を通じて外部システムが MQTT データをリアルタイムで受け取り、業務プロセスをトリガーできるため、迅速な対応が可能です。例えば、アラームデータを受け取って業務ワークフローを起動するなど。
- **データ処理のカスタマイズ**：外部システム側で受け取ったデータをさらに処理し、より複雑な業務ロジックを実装できます。EMQX の機能に制約されることなく柔軟に対応可能です。
- **疎結合な連携方式**：Webhook はシンプルな HTTP インターフェースを用いるため、システム間の疎結合な連携手法を提供します。

まとめると、Webhook 連携はリアルタイムかつ柔軟でカスタマイズ可能なデータ連携機能を提供し、柔軟かつ豊富なアプリケーション開発ニーズに応えます。

## はじめに

ここでは macOS を例に、Webhook の設定と利用方法を紹介します。

### HTTP サービスの作成

ここでは Python を使ってローカルのポート 8082 で待ち受ける HTTP サーバーを簡単に作成し、Webhook リクエストを受信した際に URL を出力します。実際の運用では、ビジネス用のサーバーに置き換えてください。

まず、Python で `POST /` リクエストを受け取る簡単な HTTP サービスを作成します。リクエスト内容を表示し、200 OK を返します：

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

上記コードを `http_server.py` というファイル名で保存し、ファイルのあるディレクトリで以下のコマンドを実行します：

```shell
# flask の依存関係をインストール
pip install flask

# サービスを起動
python3 http_server.py
```

### Webhook の作成

1. ダッシュボードの左メニューから **Integration** -> **Webhooks** をクリックします。

2. ページ上の **Create Webhook** ボタンをクリックします。

3. Webhook の **Name** と任意で **Note** を入力します。

   名前は英大文字・小文字および数字のみを含むべきです。例：`my_webhook`

4. 要件に応じて **Trigger** を選択します。

   本例では **All Messages and Events** を選択します。その他のオプションの詳細は [仕組み](#仕組み) を参照してください。

5. リクエスト設定を行います：

   - **Method**：`POST`
   - **URL**：`http://localhost:5000`

   URL フィールド横の **Test** ボタンで接続確認が可能です。他の設定はデフォルトのままで問題ありません。

6. ページ下部の **Save** をクリックして Webhook を作成します。

   ![EMQX Webhook](./assets/webhook.png)

これで Webhook の作成が完了しました。

### Webhook のテスト

MQTTX CLI を使って `t/1` トピックにメッセージをパブリッシュします：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Webhook" }'
```

この操作により、以下のイベントが順にトリガーされます：

- 接続確立
- 接続確認
- 認可チェック完了
- メッセージパブリッシュ
- 接続終了

もし `t/1` トピックにサブスクライバーがいなければ、メッセージパブリッシュ後に **メッセージ転送およびドロップ** イベントもトリガーされます。

HTTP サービスに対応するイベントとメッセージデータが転送されているか確認してください。以下のようなデータが表示されるはずです：

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
