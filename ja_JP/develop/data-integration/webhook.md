# Webhook

Webhook は、EMQX クライアントのメッセージやイベントを外部の HTTP サーバーと連携させる方法を提供します。ルールエンジンやデータブリッジを利用する場合と比べて、Webhook はよりシンプルな手段であり、導入のハードルを大幅に下げ、EMQX と外部システムの迅速な連携を可能にします。

本ページでは、Webhook に関する情報と実際の利用方法を包括的に紹介します。

## 仕組み

クライアントが特定のトピックにメッセージをパブリッシュしたり、特定の操作を行うと、Webhook がトリガーされます。Webhook はルールエンジンがサポートするすべてのメッセージとイベントに対応しています。

Webhook は以下のシナリオでトリガーされるよう設定可能です。各イベントのリクエスト内容については、[SQL データソースとフィールド](./rule-sql-events-and-fields.md)を参照してください。

![EMQX Webhook 集成](./assets/emqx-integration-http.jpg)

### メッセージ

パブリッシャーがメッセージをパブリッシュしたり、メッセージの状態が変化したときにトリガーされます。具体的には以下のイベントです：

- メッセージがパブリッシュされた
- メッセージが配信された
- メッセージがアックされた
- メッセージが転送されドロップされた
- メッセージの配信がドロップされた

メッセージに対して複数のトピックフィルターを設定でき、マッチしたメッセージのみが Webhook をトリガーします。

### イベント

クライアントが特定の操作を行ったり、状態が変化したときにトリガーされます。具体的には以下のイベントです：

- 接続が確立された
- 接続が終了した
- 接続が確認された
- 認可結果
- セッションのサブスクライブ完了
- セッションのサブスクライブ解除

## 特長

EMQX の Webhook 連携を利用することで、以下のようなメリットがあります：

- **より多くの下流システムへデータを渡せる**  
  Webhook により、MQTT データを分析プラットフォームやクラウドサービスなど、より多くの外部システムに簡単に連携でき、マルチシステムでのデータ配信が可能になります。
- **リアルタイム応答と業務プロセスのトリガー**  
  Webhook を通じて外部システムが MQTT データをリアルタイムに受け取り、業務プロセスをトリガーして迅速な対応が可能です。例えば、アラームデータを受けて業務ワークフローを起動するなどです。
- **データ処理のカスタマイズ**  
  外部システム側で受け取ったデータをさらに自由に処理でき、EMQX の機能に制限されない複雑な業務ロジックを実装可能です。
- **疎結合な連携方式**  
  Webhook はシンプルな HTTP インターフェースを利用するため、システム間の疎結合な連携方法を提供します。

まとめると、Webhook 連携はリアルタイムで柔軟かつカスタマイズ可能なデータ連携機能を提供し、柔軟で豊富なアプリケーション開発ニーズに応えます。

## はじめに

本節では macOS を例に、Webhook の設定と利用方法を紹介します。

### HTTP サービスの作成

ここでは Python を使ってローカルのポート 5000 で待ち受ける簡単な HTTP サーバーを作成し、Webhook リクエストを受け取った際に内容を表示します。実際の用途では、ビジネスサーバーに置き換えてください。

まず、Python で `POST /` リクエストを受け取るシンプルな HTTP サービスを作成します。リクエスト内容を表示し、200 OK を返します。

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

上記コードを `http_server.py` ファイルとして保存し、ファイルのあるディレクトリで以下のコマンドを実行します。

```shell
# flask の依存関係をインストール
pip install flask

# サービス起動
python3 http_server.py
```

### Webhook の作成

1. ダッシュボードの左メニューから **Integration** -> **Webhooks** をクリックします。

2. ページ上の **Create** ボタンをクリックします。

3. Webhook 名と説明を入力します。英大文字・小文字と数字の組み合わせで入力してください。ここでは `my_webhook` と入力します。

4. トリガーを用途に応じて選択します。今回は **All messages and events** を選択します。その他の選択肢は [仕組み](#仕組み)を参照してください。

5. Webhook リクエストの設定を行います。

   - リクエストメソッドに `POST` を選び、**URL** に `http://localhost:5000` を設定します。
   - 必要に応じて **Query String** にクエリパラメータを追加したり、**Headers** にカスタム HTTP ヘッダーを設定できます。
   - OAuth2 で Webhook リクエストを保護する場合は、**OAuth2 Client Credentials** をオンにして必要な設定を行います。詳細は [OAuth2 クライアント認証の設定](#configure-oauth2-client-credentials)を参照してください。URL 入力欄横の **Test** ボタンで接続テストも可能です。

6. **Save** をクリックして Webhook 作成を完了します。

   ![EMQX Webhook](./assets/webhook.png)

これで Webhook の作成が完了しました。

#### OAuth2 クライアント認証の設定

EMQX 6.0.4 以降、Webhook は OAuth 2.0 のクライアントクレデンシャルズグラントをサポートしています。OAuth2 を有効にすると、EMQX は設定されたトークンエンドポイントからアクセストークンを取得・キャッシュし、自動的に更新します。Webhook リクエスト送信時には `Authorization: Bearer <access_token>` ヘッダーを付与し、ターゲットサーバーが EMQX を認証できるようにします。

トークンエンドポイント、クライアントクレデンシャル、許可されたスコープは、OAuth2 認可サーバーやアイデンティティプロバイダー（IdP）、ターゲット API 管理者から取得してください。**OAuth2 Client Credentials** をオンにし、以下の設定を行います。

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークン取得に使用する OAuth2 認可サーバーのエンドポイント。HTTP または HTTPS の URL で、ユーザー情報を含まないこと。 |
| **Client ID** | 必須。アクセストークン取得に使用する OAuth2 クライアント ID。 |
| **Client Secret** | 必須。アクセストークン取得に使用する OAuth2 クライアントシークレット。 |
| **Scope** | 任意。アクセストークン取得時に要求する OAuth2 スコープ。複数ある場合はスペース区切り。認可サーバーがスコープを要求しない場合は空欄にします。 |
| **Token Request Timeout** | トークンエンドポイントへの HTTP リクエストのタイムアウト（秒）。デフォルトは `5` 秒。 |
| **Enable TLS** | トークンエンドポイントへの TLS を有効にする場合はオンにします。 |

EMQX は `application/x-www-form-urlencoded` コンテンツタイプの `POST` リクエストをトークンエンドポイントに送信します。リクエストボディには `grant_type`、`client_id`、`client_secret`、任意の `scope` が含まれます。トークンエンドポイントは `200` レスポンスで JSON ボディに `access_token` を返す必要があります。`token_type` と `expires_in` を返すこともできます。存在する場合、`token_type` は `Bearer`、`expires_in` は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2 を有効にした場合、Webhook の `Authorization` ヘッダーを設定しないでください。EMQX は自動生成される Bearer 認証ヘッダーと競合するため、設定を拒否します。
- トークンエンドポイントはクライアント ID とクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic 認証ヘッダーによる認証はサポートされていません。

:::

### Webhook のテスト

MQTTX CLI を使って `t/1` トピックにメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Webhook" }'
```

この操作により、以下のイベントが順にトリガーされます。

- 接続確立
- 接続確認
- 認可チェック完了
- メッセージパブリッシュ
- 接続終了

もし `t/1` トピックにサブスクライバーがいなければ、メッセージパブリッシュ後に **メッセージ転送およびドロップ** イベントもトリガーされます。

対応するイベントやメッセージデータが HTTP サービスに転送されているか確認してください。以下のようなデータが表示されるはずです。

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
