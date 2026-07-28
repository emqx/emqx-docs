# MQTTデータをHTTPサーバーに取り込む

HTTPサーバーデータ統合は、EMQXと外部HTTPサービスを迅速に連携させる方法を提供します。リクエストメソッドやリクエストデータ形式の柔軟な設定をサポートし、HTTPSによる安全な通信や認証機構も備えています。クライアントのメッセージやイベントデータをリアルタイムかつ効率的に柔軟に送信でき、IoTデバイスの状態通知やアラート通知、データ統合などのシナリオに対応可能です。

本ページでは、HTTPサーバーとのデータ統合の機能と特徴を詳しく解説し、HTTPサーバーデータ統合の設定方法について実践的なガイダンスを提供します。

:::tip

HTTPサービスとの連携が必要だが、ルールによるデータ処理を必要としないユーザーには、より簡単で使いやすい[Webhook](./webhook.md)の利用を推奨します。

:::

<video
  src="https://assets.emqx.com/data/video/emqx-docs/data-integration/http_server_integration.mp4"
  preload="metadata"
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## 動作の仕組み

HTTPサーバーデータ統合はEMQXの標準機能であり、簡単な設定で外部HTTPサービスと連携できます。HTTPサービスを利用することで、ユーザーは任意のプログラミング言語やフレームワークでコードを書き、柔軟かつ複雑なデータ処理ロジックを実装可能です。

<img src="./assets/emqx-integration-http.jpg" alt="emqx-integration-http" style="zoom:67%;" />

EMQXはルールエンジンとSinkを介してデバイスのイベントやメッセージをHTTPサーバーに転送します。ワークフローは以下の通りです：

1. **デバイスがEMQXに接続**：IoTデバイスが正常に接続すると、デバイスIDや送信元IPアドレスなどの属性を含むオンラインイベントが発生します。
2. **デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
3. **ルールエンジンがメッセージを処理**：ルールエンジンはトピックフィルターに基づいてメッセージをマッチングし、フィールドのフィルタリングやデータ形式の変換、追加コンテキストによるメッセージの拡充など、設定されたルールに従って処理します。
4. **HTTPサーバーへのブリッジング**：ルールは処理済みのメッセージやイベントをHTTPサーバーに転送するアクションをトリガーします。リクエストヘッダーやリクエストボディ、URLはルールの出力から動的に構築可能です。

イベントやメッセージデータがHTTPサーバーに送信された後は、以下のような柔軟な処理が可能です：

- デバイス管理システムでデバイスの状態更新やイベント記録を行う。
- メッセージデータをデータベースに書き込み保存する。
- SQLルールで検知した異常データに基づきアラートや通知システムを起動する。

## 特徴とメリット

EMQXのHTTPサーバー統合を利用することで、以下のようなメリットが得られます：

- **より多くの下流システムへのデータ配信拡張**：HTTPサービスにより、MQTTデータを分析プラットフォームやクラウドサービスなど多様な外部システムとシームレスに連携でき、複数システムへのデータ配信を実現します。
- **リアルタイム応答と業務プロセスのトリガー**：HTTPサービスを通じて外部システムがMQTTデータをリアルタイムに受信し、業務プロセスを即時に起動できるため、迅速な対応が可能です。例えば、アラートデータを受けて業務ワークフローを開始するなど。
- **カスタムデータ処理**：外部システム側で受信データに対し二次処理を行い、EMQXの機能に制約されない複雑な業務ロジックを実装できます。
- **疎結合な統合方式**：HTTPサービスはシンプルなHTTPインターフェースを使用し、システム間の疎結合な連携を提供します。

まとめると、HTTPサービスはリアルタイムかつ柔軟でカスタマイズ可能なデータ統合機能を提供し、多様で柔軟なアプリケーション開発ニーズに応えます。

## はじめる前に

このセクションでは、HTTPサーバーデータ統合の作成を始める前に必要な準備事項を説明します。簡単なHTTPサーバーのセットアップも含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### 簡単なHTTPサーバーのセットアップ

1. Pythonを使って簡単なHTTPサービスを構築します。このHTTPサービスは`POST /`リクエストを受け取り、リクエスト内容を表示した後に`200 OK`を返します：

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

2. 上記コードを`http_server.py`というファイル名で保存し、以下のコマンドでサーバーを起動します：

```shell
pip install flask

python3 http_server.py
```

## コネクターの作成

このセクションでは、SinkをHTTPサーバーに接続するためのHTTPサーバーコネクターの設定方法を説明します。

1. ダッシュボードの左メニューから **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックします。
3. コネクタータイプとして **HTTP Server** を選択し、**Next** をクリックします。
4. コネクターの名前を入力します。名前は英数字の組み合わせで、例：`httpserver`。
5. **URL** にHTTPサーバーのアドレスを設定します。例：`http://localhost:5000`。
6. その他の設定はデフォルトのままにします。
7. 詳細設定（任意）：詳細は[Sinkの特徴](./data-bridges.md#features-of-sink)を参照してください。
8. **Create**をクリックする前に、**Test Connectivity**を押してコネクターがHTTPサーバーに接続できるか確認できます。
9. **Create**をクリックしてコネクターの設定を完了します。

コネクター作成後、ルールをこのコネクターで作成するかどうかのダイアログが表示されます。

- **Create Rule**をクリックすると、ルール作成ページに直接移動して統合設定を続行できます。
- あるいは、**Back To Connector List**をクリックしてコネクター一覧に戻り、後で **Integration** -> **Rules** からルールを作成することも可能です。

本例では、**Create Rule**をクリックして続行します。

## HTTPサーバーSinkを使ったルールの作成

このセクションでは、ルールを作成し、HTTPサーバーSinkを設定してMQTTメッセージをHTTPサーバーに送信する方法を説明します。

**Create Rule**をクリックすると、自動的にルール作成ページに遷移し、HTTPサーバーSinkの設定用アクションペインが表示され、先ほど作成したコネクターが選択済みの状態になります。

1. **Type of Action** と **Action** は自動で `HTTP Server` と `Create Action` に設定され、新しいSinkを作成します。

2. Sinkの名前と説明を入力します。**Connector** は先ほど作成した `httpserver` が自動入力されます。

3. HTTPリクエストを設定します：

   - **URL Path**：`/`
   - **Method**：`POST`

   最終的なリクエストURLはコネクターのURLとこのパスを組み合わせて構築されます。

4. MQTTメッセージデータをHTTPサーバーに送信するための**Request Body**を設定します：

   ```json
   {
     "topic": "${topic}",
     "payload": ${payload},
     "clientid": "${clientid}",
     "qos": ${qos},
     "timestamp": ${timestamp}
   }
   ```

   テンプレート内の変数はルールSQLで選択されたフィールドで埋められます。

5. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**を押してSinkがHTTPサーバーに接続できるか確認できます。

7. **Create**をクリックしてSinkの設定を完了します。新しいSinkはルール作成ページの**Action Outputs**セクションに表示されます。

8. **Rule ID**を入力します。システムがランダム生成するか、自分で任意に定義できます（任意）。例：`my_rule`。

9. **SQLエディター**に以下のSQL文を入力します：

   ```bash
   SELECT
     *
   FROM
     "t/#"
   ```

   このルールは`"t/#"`以下のトピックにパブリッシュされたすべてのMQTTメッセージにマッチします。

   ::: tip

   独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`句に含めていることを確認してください。

   :::

10. ルール設定を確認後、**Save**をクリックしてルールを生成します。

ルール作成後、`t/#`以下のトピックにパブリッシュされたメッセージはルールで処理され、設定したHTTPサーバーに転送されます。

また、**Integration** -> **Flow Designer** からルールとHTTPサーバーSinkのデータフロートポロジーを確認できます。

## ルールのテスト

1. MQTTXを使ってトピック `t/1` にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
   ```

2. ダッシュボードの**Rule**ページでルール名をクリックし、統計情報を確認します。メトリクスに新しい受信メッセージと送信メッセージが1件ずつ表示されていれば、HTTPサーバーSinkによる処理と転送が成功しています。

3. HTTPサーバーがリクエストを受信していることを確認します。

   PythonのHTTPサーバーが起動中であれば、ターミナルに以下のような出力が表示されます：

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
