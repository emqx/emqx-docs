# MQTTデータをHTTPサーバーに取り込む

HTTPサーバーデータ統合は、EMQXを外部HTTPサービスと迅速に連携させるための機能です。リクエストメソッドやリクエストデータ形式の柔軟な設定をサポートし、HTTPSによる安全な通信や認証機構も提供します。クライアントのメッセージやイベントデータをリアルタイムかつ効率的に柔軟に送信でき、IoTデバイスの状態通知やアラート通知、データ統合などのシナリオに活用できます。

本ページでは、HTTPサーバーデータ統合の機能と特徴を詳しく解説し、HTTPサーバーデータ統合の設定方法について実践的なガイドを提供します。

:::tip 

HTTPサービスとの連携が必要だが、ルールを使ったデータ処理が不要なユーザーには、より簡単で使いやすい[Webhook](./webhook.md)の利用を推奨します。

:::

<video
  src="https://assets.emqx.com/data/video/emqx-docs/data-integration/http_server_integration.mp4"
  preload="metadata"
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## 動作概要

HTTPサーバーデータ統合はEMQXに標準搭載された機能で、簡単な設定により外部HTTPサービスと連携できます。HTTPサービス側では、好みのプログラミング言語やフレームワークでコードを書き、カスタムで柔軟かつ複雑なデータ処理ロジックを実装できます。

<img src="./assets/emqx-integration-http.png" alt="emqx-integration-http" style="zoom:67%;" />

EMQXはルールエンジンとSinkを介してデバイスのイベントやメッセージをHTTPサーバーに転送します。ワークフローは以下の通りです。

1. **デバイスがEMQXに接続**：IoTデバイスが正常に接続すると、デバイスIDや送信元IPアドレスなどの属性を含むオンラインイベントが発生します。
2. **デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
3. **ルールエンジンがメッセージを処理**：ルールエンジンはトピックフィルターに基づきメッセージをマッチングし、フィールドのフィルタリング、データ形式の変換、追加コンテキストによるメッセージの拡充など設定されたルールで処理します。
4. **HTTPサーバーへのブリッジング**：ルールは処理済みのメッセージやイベントをHTTPサーバーに転送するアクションをトリガーします。リクエストヘッダー、リクエストボディ、URLはルール出力から動的に構築可能です。

イベントやメッセージデータをHTTPサーバーに送信後、以下のような柔軟な処理が可能です。

- デバイス管理システムでデバイス状態の更新やイベント記録を行う。
- メッセージデータをデータベースに書き込み保存する。
- SQLルールで検知した異常データに基づきアラートや通知システムをトリガーする。

## 特徴とメリット

EMQXのHTTPサーバー統合を利用することで、以下のような利点があります。

- **より多くの下流システムへのデータ連携を拡張**：HTTPサービスにより、MQTTデータを分析プラットフォームやクラウドサービスなど多様な外部システムとシームレスに連携でき、複数システム間でのデータ配信を容易にします。
- **リアルタイム応答と業務プロセスのトリガー**：HTTPサービスを通じて外部システムがMQTTデータをリアルタイムに受け取り、業務プロセスをトリガー可能で、迅速な対応を実現します。例えばアラートデータを受けて業務フローを起動するケースなどです。
- **カスタムデータ処理**：外部システム側で受信データに対して二次処理を行えるため、EMQXの機能に制約されないより複雑な業務ロジックの実装が可能です。
- **疎結合な連携**：HTTPサービスはシンプルなHTTPインターフェースを利用するため、システム間の疎結合な連携手法を提供します。

まとめると、HTTPサービスはリアルタイムかつ柔軟でカスタマイズ可能なデータ統合機能を提供し、多様で柔軟なアプリケーション開発ニーズに応えます。

## はじめる前に

このセクションでは、HTTPサーバーデータ統合の作成を始める前に必要な準備、簡単なHTTPサーバーの構築方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### 簡単なHTTPサーバーのセットアップ

1. Pythonを使って簡単なHTTPサービスを構築します。このHTTPサービスは `POST /` リクエストを受け取り、リクエスト内容を表示した後に `200 OK` を返します。

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

2. 上記コードを `http_server.py` というファイル名で保存し、以下のコマンドでサーバーを起動します。

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
6. [任意] **Headers** にHTTPリクエストヘッダーを追加します。
7. [任意] **OAuth2 Client Credentials** を有効にすると、EMQXがアクセストークンを取得し、ターゲットHTTPサーバーへのリクエストに追加します。詳細は[OAuth2 Client Credentialsの設定](#configure-oauth2-client-credentials)を参照してください。
8. [任意] **Enable TLS** を有効にすると、ターゲットHTTPサーバーへの接続にTLSを使用します。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。
9. [任意] **Advanced Settings** で接続関連のオプションを設定します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。
10. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがHTTPサーバーに接続できるか確認できます。
11. **Create** をクリックしてコネクターの設定を完了します。

コネクター作成後、ルールをこのコネクターで作成するかどうかのダイアログが表示されます。

- **Create Rule** をクリックすると、ルール作成ページに直接移動して統合設定を続行できます。
- または **Back To Connector List** をクリックしてコネクター一覧に戻り、後から **Integration** -> **Rules** でルールを作成できます。

本例では **Create Rule** をクリックして続行します。

### OAuth2 Client Credentialsの設定

EMQX 6.0.4以降、HTTPサーバーコネクターはOAuth 2.0 Client Credentials Grantをサポートしています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュ・自動更新します。EMQXがターゲットHTTPサーバーを呼び出す際、`Authorization: Bearer <access_token>` ヘッダーにトークンを付与し、ターゲットサーバーはこれによりEMQXを認証できます。

コネクター作成または編集時に **OAuth2 Client Credentials** を有効にし、以下の設定を行います。

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークン取得に使用するOAuth2認可サーバーのエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含んではいけません。 |
| **Client ID** | 必須。アクセストークン取得に使用するOAuth2クライアントID。 |
| **Client Secret** | 必須。アクセストークン取得に使用するOAuth2クライアントシークレット。 |
| **Scope** | 任意。アクセストークンに要求するOAuth2スコープ。 |
| **Token Request Timeout** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは5秒。 |
| **Enable TLS** | トークンエンドポイントへのTLSを有効にするスイッチ。ターゲットHTTPサーバーのTLS設定とは独立しています。 |

HOCON設定では、HTTPサーバーコネクターの設定内で `url`、`headers`、`ssl` と同じ階層に `oauth2` ブロックを追加します。

```hocon
oauth2 {
    enable = true
    grant_type = client_credentials
    token_endpoint = "https://auth.example.com/oauth/token"
    client_id = "emqx-client"
    client_secret = "emqx-client-secret"
    scope = "messages.write"
    timeout = 5s
    ssl {
        enable = true
    }
}
```

EMQXは `application/x-www-form-urlencoded` コンテンツタイプの `POST` リクエストをトークンエンドポイントに送信します。リクエストボディには `grant_type`、`client_id`、`client_secret`、および任意の `scope` が含まれます。トークンエンドポイントは `200` レスポンスで `access_token` を含むJSONボディを返す必要があります。`token_type` と `expires_in` も返せます。存在する場合、`token_type` は `Bearer`、`expires_in` は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2が有効な場合、HTTPサーバーコネクターやそのSinkで `Authorization` ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによる認証はサポートされていません。

:::

EMQXがアクセストークンを取得できない場合、コネクターのヘルスチェックは `disconnected` と報告します。

## HTTPサーバーSinkを使ったルールの作成

このセクションでは、ルールを作成しHTTPサーバーSinkを設定してMQTTメッセージをHTTPサーバーに送信する方法を説明します。

**Create Rule** をクリックすると、自動的に **Create Rule** ページに遷移し、HTTPサーバーSinkを設定するための **Action pane** が表示され、コネクターが利用可能な状態になっています。

1. **Type of Action** と **Action** は自動で `HTTP Server` と `Create Action` に設定され、新しいSinkを作成します。

2. Sinkの名前と説明を入力します。**Connector** は先ほど作成したコネクター（例：`httpserver`）が自動入力されます。

3. HTTPリクエストを設定します。

   - **URL Path**：`/`
   - **Method**：`POST`

   最終的なリクエストURLはコネクターのURLとこのパスを結合して構築されます。

4. **Request Body** を設定し、MQTTメッセージデータをHTTPサーバーに送信します。

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

5. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

6. **Create** をクリックする前に、**Test Connectivity** をクリックしてSinkがHTTPサーバーに接続できるか確認できます。

7. **Create** をクリックしてSinkの設定を完了します。新しいSinkはルールの **Action Outputs** セクションに表示されます。

8. **Rule ID** を入力します。システムによるランダム生成か任意のIDを指定可能です（例：`my_rule`）。

9. **SQLエディター** に以下のSQL文を入力します。

   ```bash
   SELECT
     *
   FROM
     "t/#"
   ```

   このルールは `t/#` 以下のトピックにパブリッシュされたすべてのMQTTメッセージにマッチします。

   ::: tip

   独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   :::

10. ルール設定を確認後、**Save** をクリックしてルールを生成します。

ルール作成後、`t/#` 以下のトピックにパブリッシュされたメッセージはルールで処理され、設定したHTTPサーバーに転送されます。

また、**Integration** -> **Flow Designer** でルールとHTTPサーバーSinkのデータフロートポロジーを確認できます。

## ルールのテスト

1. MQTTXを使ってトピック `t/1` にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
   ```

2. ダッシュボードの **Rule** ページでルール名をクリックし、統計情報を確認します。メトリクスには新規の受信メッセージと送信メッセージが1件ずつ表示され、HTTPサーバーSinkによる正常な処理と転送が示されます。

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

   出力された内容は、EMQXがMQTTメッセージをJSON形式でHTTPサーバーに転送したことを示しています。リクエストボディ内のフィールドはSinkのリクエストボディテンプレートで設定した変数に対応しています。
