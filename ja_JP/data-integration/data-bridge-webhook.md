# MQTTデータをHTTPサーバーに取り込む

HTTPサーバーデータ統合は、EMQXを外部HTTPサービスと迅速に連携させるための機能です。リクエストメソッドやリクエストデータ形式の柔軟な設定をサポートし、HTTPSによる安全な通信と認証機構を提供します。クライアントのメッセージやイベントデータをリアルタイムに効率的かつ柔軟に送信でき、IoTデバイスの状態通知やアラート通知、データ統合などのシナリオに対応可能です。

本ページでは、HTTPサーバーデータ統合の機能概要と特徴を詳しく解説し、HTTPサーバーデータ統合の設定方法について実践的な手順を紹介します。

:::tip

HTTPサービスとの連携は必要だが、ルールを使ったデータ処理が不要なユーザーには、より簡単で使いやすい[Webhook](./webhook.md)の利用を推奨します。

:::

<video
  src="https://assets.emqx.com/data/video/emqx-docs/data-integration/http_server_integration.mp4"
  preload="metadata"
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## 動作概要

HTTPサーバーデータ統合はEMQXの標準機能であり、簡単な設定で外部HTTPサービスと連携できます。HTTPサービス側では、任意のプログラミング言語やフレームワークでコードを記述し、柔軟かつ複雑なデータ処理ロジックを実装可能です。

<img src="./assets/emqx-integration-http.jpg" alt="emqx-integration-http" style="zoom:67%;" />

EMQXはルールエンジンとSinkを介してデバイスのイベントやメッセージをHTTPサーバーに転送します。ワークフローは以下の通りです。

1. **デバイスがEMQXに接続**：IoTデバイスが正常に接続されると、デバイスIDや送信元IPアドレスなどの属性を含むオンラインイベントが発生します。
2. **デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンがトリガーされます。
3. **ルールエンジンがメッセージを処理**：ルールエンジンはトピックフィルターに基づいてメッセージをマッチングし、フィールドのフィルタリング、データ形式の変換、追加コンテキストによるメッセージの拡充などの設定されたルールで処理します。
4. **HTTPサーバーへのブリッジング**：ルールは処理済みのメッセージやイベントをHTTPサーバーに転送するアクションをトリガーします。リクエストヘッダー、リクエストボディ、URLはルールの出力から動的に構築可能です。

イベントやメッセージデータがHTTPサーバーに送信された後、以下のような柔軟な処理が行えます。

- デバイス管理システムでのデバイス状態の更新やイベント記録
- メッセージデータのデータベースへの書き込みによる保存
- SQLルールで検出した異常データに基づくアラートや通知システムの起動

## 特徴とメリット

EMQXのHTTPサーバーデータ統合を利用することで、以下のようなメリットがあります。

- **より多くの下流システムへのデータ連携拡張**：HTTPサービスにより、MQTTデータを分析プラットフォームやクラウドサービスなど多様な外部システムとシームレスに連携でき、複数システム間でのデータ配信を実現します。
- **リアルタイム応答と業務プロセスのトリガー**：HTTPサービスを通じて外部システムはMQTTデータをリアルタイムに受信し、業務プロセスをトリガーして迅速な対応を可能にします。例えば、アラートデータの受信による業務ワークフローの起動などです。
- **カスタムデータ処理**：外部システムは受信したデータに対し二次処理を行うことができ、EMQXの機能に制限されないより複雑な業務ロジックを実装可能です。
- **疎結合な連携**：HTTPサービスはシンプルなHTTPインターフェースを利用するため、システム間の疎結合な連携を実現します。

まとめると、HTTPサービスはリアルタイムかつ柔軟でカスタマイズ可能なデータ統合機能を提供し、多様なアプリケーション開発ニーズに応えます。

## はじめる前に

このセクションでは、HTTPサーバーデータ統合の作成を開始する前に必要な準備について説明します。簡単なHTTPサーバーのセットアップも含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### 簡単なHTTPサーバーのセットアップ

1. Pythonを使って簡単なHTTPサービスを構築します。このHTTPサービスは`POST /`リクエストを受け取り、リクエスト内容を表示した後に`200 OK`を返します。

```bash
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

2. 上記コードを`http_server.py`というファイル名で保存し、以下のコマンドでサーバーを起動します。

```shell
pip install flask

python3 http_server.py
```

## コネクターの作成

このセクションでは、SinkをHTTPサーバーに接続するためのHTTPサーバーコネクターの設定方法を説明します。

1. ダッシュボードの左メニューから **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックします。
3. コネクタータイプとして **HTTP Server** を選択し、**Next** をクリックします。
4. コネクター名を入力します。名前は英数字の組み合わせで、例：`httpserver`。
5. **URL** にHTTPサーバーのアドレスを設定します。例：`http://localhost:5000`。
6. その他の設定はデフォルトのままにします。
7. 詳細設定（任意）：詳細は[Sinkの特徴](./data-bridges.md#features-of-sink)を参照してください。
8. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがHTTPサーバーに接続できるか確認できます。
9. **Create**をクリックしてコネクターの設定を完了します。

コネクター作成後、ルールをこのコネクターで作成するかどうかのダイアログが表示されます。

- **Create Rule** をクリックするとルール作成ページに直接移動し、連携設定を続行できます。
- または、**Back To Connector List** をクリックしてコネクター一覧に戻り、後で **Integration** -> **Rules** からルールを作成できます。

ここでは、**Create Rule** をクリックして続行します。

## HTTPサーバーSinkを使ったルールの作成

このセクションでは、ルールを作成し、HTTPサーバーSinkを設定してMQTTメッセージをHTTPサーバーに送信する方法を説明します。

**Create Rule** をクリックすると自動的に **Create Rule** ページに移動し、HTTPサーバーSinkの設定用の **Action pane** が表示され、先ほど作成したコネクターが利用可能な状態になっています。

1. **Type of Action** と **Action** は自動で `HTTP Server` と `Create Action` に設定され、新しいSinkが作成されます。

2. Sinkの名前と説明を入力します。**Connector** は先ほど作成した `httpserver` が自動入力されます。

3. HTTPリクエストを設定します。

   - **URL Path**：`/`
   - **Method**：`POST`

   最終的なリクエストURLはコネクターのURLとこのパスを組み合わせて構築されます。

4. MQTTメッセージデータをHTTPサーバーに送信するための**Request Body**を設定します。

   ```json
   {
     "topic": "${topic}",
     "payload": ${payload},
     "clientid": "${clientid}",
     "qos": ${qos},
     "timestamp": ${timestamp}
   }
   ```

   テンプレート内の変数はルールのSQLで選択されたフィールドから値が埋め込まれます。

5. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のために、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがHTTPサーバーに接続可能か確認できます。

7. **Create**をクリックしてSinkの設定を完了します。新しいSinkはルールの**Action Outputs**セクションに表示されます。

8. **Rule ID** を入力します。システムがランダム生成するか、自分で定義可能（任意）です。例：`my_rule`。

9. **SQLエディター**に以下のSQL文を入力します。

   ```bash
   SELECT
     *
   FROM
     "t/#"
   ```

   このルールは`"t/#"`以下のトピックにパブリッシュされたすべてのMQTTメッセージにマッチします。

   :::tip

   独自のSQL文を指定する場合は、Sinkで必要とされるすべてのフィールドが`SELECT`句に含まれていることを確認してください。

   :::

10. ルール設定を確認後、**Save**をクリックしてルールを生成します。

ルール作成後、`t/#`以下のトピックにパブリッシュされたメッセージはルールによって処理され、設定したHTTPサーバーに転送されます。

また、**Integration** -> **Flow Designer** からルールとHTTPサーバーSinkのデータフロートポロジーを確認できます。

## ルールのテスト

1. MQTTXを使ってトピック`t/1`にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
   ```

2. ダッシュボードの**Rule**ページでルール名をクリックし、統計情報を確認します。メトリクスに新しい受信メッセージと送信メッセージが1件ずつ表示されていれば、HTTPサーバーSinkによるメッセージの処理と転送が成功しています。

3. HTTPサーバーがリクエストを受信していることを確認します。

   PythonのHTTPサーバーが起動している場合、ターミナルには以下のような出力が表示されます。

   ```text
   python3 http_server.py
    * Serving Flask app 'http_server'
    * Environment: production
      WARNING: This is a development server. Do not use it in a production deployment.
      Use a production WSGI server instead.
    * Debug mode: off
    * Running on http://127.0.0.1:5000 (Press CTRL+C to quit)
   
   got post request:  b'{"topic":"t/1","payload":{"msg":"hello HTTP Server"},"clientid":"emqx_c","qos":0,"timestamp":1700000000000}'
   ```

   表示された内容から、EMQXがMQTTメッセージをJSON形式でHTTPサーバーに転送していることがわかります。リクエストボディ内のフィールドはSinkのリクエストボディテンプレートで設定した変数に対応しています。
