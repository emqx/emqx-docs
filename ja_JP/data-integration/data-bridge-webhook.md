# MQTTデータをHTTPサーバーに取り込む

HTTPサーバーデータ統合は、EMQXを外部HTTPサービスと迅速に連携させるための機能です。リクエストメソッドやリクエストデータ形式の柔軟な設定をサポートし、HTTPSによる安全な通信や認証機構も提供します。クライアントのメッセージやイベントデータをリアルタイムかつ効率的に柔軟に送信でき、IoTデバイスの状態通知、アラート通知、データ統合などのシナリオに対応可能です。

本ページでは、HTTPサーバーとのデータ統合の機能と特徴を詳細に解説し、HTTPサーバーデータ統合の設定方法について実践的なガイドを提供します。

:::tip 

HTTPサービスとの連携が必要だがルールによるデータ処理が不要なユーザーには、より簡単で使いやすい[Webhook](./webhook.md)の利用を推奨します。

:::

<video
  src="https://assets.emqx.com/data/video/emqx-docs/data-integration/http_server_integration.mp4"
  preload="metadata"
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## 動作概要

HTTPサーバーデータ統合はEMQXの標準機能で、簡単な設定により外部HTTPサービスと連携できます。HTTPサービス側では、任意のプログラミング言語やフレームワークでコードを書き、カスタムで柔軟かつ複雑なデータ処理ロジックを実装できます。

<img src="./assets/emqx-integration-http.png" alt="emqx-integration-http" style="zoom:67%;" />

EMQXはルールエンジンとSinkを介してデバイスのイベントやメッセージをHTTPサーバーに転送します。ワークフローは以下の通りです。

1. **デバイスがEMQXに接続**：IoTデバイスが正常に接続すると、デバイスIDや送信元IPアドレスなどの属性を含むオンラインイベントが発生します。
2. **デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
3. **ルールエンジンがメッセージを処理**：ルールエンジンはトピックフィルターに基づいてメッセージをマッチングし、フィールドのフィルタリング、データ形式の変換、追加コンテキストによるメッセージの強化など設定されたルールで処理します。
4. **HTTPサーバーへのブリッジング**：ルールは処理済みメッセージやイベントをHTTPサーバーに転送するアクションをトリガーします。リクエストヘッダー、リクエストボディ、URLはルールの出力から動的に構築可能です。

イベントやメッセージデータがHTTPサーバーに送信された後、以下のような柔軟な処理が可能です。

- デバイス管理システムでデバイス状態の更新やイベント記録を行う。
- メッセージデータをデータベースに書き込み保存する。
- SQLルールで検出した異常データに基づきアラートや通知システムを起動する。

## 特徴とメリット

EMQXのHTTPサーバー統合を利用することで、以下のようなメリットがあります。

- **より多くの下流システムへのデータ配信拡張**：HTTPサービスにより、MQTTデータを分析プラットフォームやクラウドサービスなど多様な外部システムとシームレスに連携し、複数システム間でのデータ配信を実現します。
- **リアルタイム応答と業務プロセスのトリガー**：HTTPサービスを通じて外部システムはMQTTデータをリアルタイムで受信し、業務プロセスを迅速に起動できます。例えば、アラートデータを受け取って業務フローをトリガーするケースなどです。
- **カスタムデータ処理**：外部システム側で受信データの二次処理が可能なため、EMQXの機能に制限されない複雑な業務ロジックを実装できます。
- **疎結合な連携**：HTTPサービスはシンプルなHTTPインターフェースを利用するため、システム連携の疎結合なアプローチを提供します。

まとめると、HTTPサービスはリアルタイムかつ柔軟でカスタマイズ可能なデータ統合機能を提供し、多様で柔軟なアプリケーション開発ニーズに応えます。

## はじめる前に

このセクションでは、HTTPサーバーデータ統合を作成する前に必要な準備、簡単なHTTPサーバーの構築方法を説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### 簡単なHTTPサーバーの構築

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

2. 上記コードを`http_server.py`ファイルとして保存し、以下のコマンドでサーバーを起動します。

```shell
pip install flask

python3 http_server.py
```

## コネクターの作成

このセクションでは、SinkをHTTPサーバーに接続するためのHTTPサーバーコネクターの設定方法を説明します。

1. ダッシュボードの左メニューから **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックします。
3. コネクタータイプとして **HTTP Server** を選択し、**Next** をクリックします。
4. コネクター名を入力します。名前は英数字の組み合わせとし、例：`httpserver`。
5. **URL** にHTTPサーバーのアドレスを設定します。例：`http://localhost:5000`。
6. その他の設定はデフォルトのままにします。
7. 高度な設定（任意）：詳細は[Sinkの特徴](./data-bridges.md#features-of-sink)を参照してください。
8. **Create**をクリックする前に、**Test Connectivity**を押してコネクターがHTTPサーバーに接続できるか確認できます。
9. **Create**をクリックしてコネクターの設定を完了します。

コネクター作成後、ルールをこのコネクターを使って作成するかどうかのダイアログが表示されます。

- **Create Rule**をクリックするとルール作成ページに直接移動し、統合設定を続行できます。
- あるいは**Back To Connector List**をクリックしてコネクター一覧に戻り、後から**Integration** -> **Rules**でルールを作成できます。

本例では**Create Rule**をクリックして続行します。

## HTTPサーバーSinkを使ったルールの作成

このセクションでは、ルールを作成しHTTPサーバーSinkを設定してMQTTメッセージをHTTPサーバーに送信する方法を説明します。

**Create Rule**をクリックすると自動的に**Create Rule**ページに遷移し、HTTPサーバーSinkの設定用の**Action pane**が表示され、先ほど作成したコネクターが選択された状態になります。

1. **Type of Action**と**Action**は自動で`HTTP Server`と`Create Action`が入力され、新しいSink作成が設定されます。

2. Sinkの名前と説明を入力します。**Connector**は先ほど作成した`httpserver`が自動入力されます。

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

   テンプレート内の変数はルールSQLで選択したフィールドから値が埋め込まれます。

5. **フォールバックアクション（任意）**：メッセージ送信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**を押してSinkがHTTPサーバーに接続できるか確認できます。

7. **Create**をクリックしてSinkの設定を完了します。新しいSinkは**Create Rule**ページの**Action Outputs**セクションに表示されます。

8. **Rule ID**を入力します。システムがランダム生成するか、自分で任意に定義できます（任意）。例：`my_rule`。

9. **SQLエディター**に以下のSQL文を入力します。

   ```bash
   SELECT
     *
   FROM
     "t/#"
   ```

   このルールは`"t/#"`以下のトピックにパブリッシュされたすべてのMQTTメッセージにマッチします。

   ::: tip

   独自のSQL文を指定する場合は、Sinkで必要なすべてのフィールドが`SELECT`句に含まれていることを確認してください。

   :::

10. ルール設定を確認後、**Save**をクリックしてルールを生成します。

ルール作成後、`t/#`以下のトピックにパブリッシュされたメッセージはルールで処理され、設定したHTTPサーバーに転送されます。

また、**Integration** -> **Flow Designer**でルールとHTTPサーバーSinkのデータフロートポロジーを確認できます。

## ルールのテスト

1. MQTTXを使い、トピック`t/1`にメッセージを送信してオンライン／オフラインイベントをトリガーします。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
   ```

2. ダッシュボードの**Rule**ページでルール名をクリックし統計情報を確認します。新しい受信メッセージ数と送信メッセージ数が1件ずつ増えていれば、HTTPサーバーSinkによるメッセージ処理と転送が成功しています。

3. HTTPサーバーがリクエストを受信していることを確認します。

   PythonのHTTPサーバーが起動中であれば、ターミナルに以下のような出力が表示されます。

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

   表示された内容は、EMQXがMQTTメッセージをJSON形式でHTTPサーバーに転送したことを示しています。リクエストボディのフィールドはSinkのリクエストボディテンプレートで設定した変数に対応しています。
