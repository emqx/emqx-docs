# MQTTデータをHTTPサーバーに取り込む

HTTPサーバーデータ統合は、EMQXを外部HTTPサービスと迅速に連携させるための機能です。リクエストメソッドやリクエストデータ形式の柔軟な設定をサポートし、HTTPSによる安全な通信や認証機構を提供します。クライアントのメッセージやイベントデータをリアルタイムかつ効率的に柔軟に送信でき、IoTデバイスの状態通知やアラート通知、データ統合などのシナリオに対応可能です。

本ページでは、HTTPサーバーデータ統合の機能と特徴を詳しく解説し、HTTPサーバーデータ統合の設定手順を実践的に案内します。

:::tip

ルールを使ったデータ処理が不要でHTTPサービスと連携したいユーザーには、より簡単で使いやすい[Webhook](./webhook.md)の利用を推奨します。

:::

<video
  src="https://assets.emqx.com/data/video/emqx-docs/data-integration/http_server_integration.mp4"
  preload="metadata"
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## 動作概要

HTTPサーバーデータ統合はEMQXの標準機能であり、簡単な設定で外部HTTPサービスと連携できます。HTTPサービス側では任意のプログラミング言語やフレームワークでコードを記述し、カスタムで柔軟かつ複雑なデータ処理ロジックを実装可能です。

<img src="./assets/emqx-integration-http.png" alt="emqx-integration-http" style="zoom:67%;" />

EMQXはルールエンジンとSinkを通じてデバイスのイベントやメッセージをHTTPサーバーに転送します。ワークフローは以下の通りです。

1. **デバイスがEMQXに接続**：IoTデバイスが正常に接続すると、デバイスIDや送信元IPアドレスなどの属性を含むオンラインイベントが発生します。
2. **デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
3. **ルールエンジンがメッセージを処理**：トピックフィルターに基づいてメッセージをマッチングし、フィールドのフィルタリングやデータ形式の変換、追加コンテキストの付与など設定されたルールで処理します。
4. **HTTPサーバーへのブリッジング**：ルールが処理済みのメッセージやイベントをHTTPサーバーに転送するアクションをトリガーします。リクエストヘッダーやボディ、URLはルールの出力から動的に構築可能です。

イベントやメッセージデータがHTTPサーバーに送信された後は、以下のような柔軟な処理が行えます。

- デバイス管理システムでデバイス状態の更新やイベント記録を行う
- メッセージデータをデータベースに書き込んで保存する
- SQLルールで異常データを検知し、アラートや通知システムを起動する

## 特徴とメリット

EMQXのHTTPサーバー統合を利用することで、以下のようなメリットがあります。

- **より多くの下流システムへのデータ連携を拡張**：HTTPサービスにより、MQTTデータを分析プラットフォームやクラウドサービスなど多様な外部システムとシームレスに連携でき、複数システム間でのデータ分配が容易になります。
- **リアルタイム応答と業務プロセスのトリガー**：HTTPサービスを通じて外部システムがMQTTデータをリアルタイムに受信し、業務プロセスを即座に起動可能です。例えばアラートデータ受信時に業務ワークフローを開始するなどです。
- **カスタムデータ処理**：外部システム側で受信データに対して二次処理を行い、EMQXの機能に制限されない複雑な業務ロジックを実装できます。
- **疎結合な連携**：HTTPサービスはシンプルなHTTPインターフェースを使用し、システム間の疎結合な連携を実現します。

まとめると、HTTPサービスはリアルタイムかつ柔軟でカスタマイズ可能なデータ統合機能を提供し、多様なアプリケーション開発ニーズに応えます。

## はじめる前に

ここではHTTPサーバーデータ統合を作成する前に必要な準備について説明します。簡単なHTTPサーバーのセットアップも含みます。

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

1. ダッシュボードの左メニューから**Integration** -> **Connector**をクリックします。
2. ページ右上の**Create**をクリックします。
3. コネクタータイプで**HTTP Server**を選択し、**Next**をクリックします。
4. コネクターの名前を入力します。名前は英数字の組み合わせとしてください。例：`httpserver`。
5. **URL**にHTTPサーバーのアドレスを設定します。例：`http://localhost:5000`。
6. 【任意】**Headers**にHTTPリクエストヘッダーを追加します。
7. 【任意】**OAuth2 Client Credentials**をオンにすると、EMQXがアクセストークンを取得し、HTTPサーバーへのリクエストに付加します。詳細は[OAuth2クライアント認証の設定](#configure-oauth2-client-credentials)を参照してください。
8. 【任意】**Enable TLS**をオンにすると、HTTPサーバーへの接続にTLSを有効化します。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。
9. 【任意】**Advanced Settings**で接続関連のオプションを設定します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。
10. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがHTTPサーバーに接続できるか確認できます。
11. **Create**をクリックしてコネクターの設定を完了します。

コネクター作成後、ルール作成画面に遷移するかどうかのダイアログが表示されます。

- **Create Rule**をクリックすると、ルール作成画面に直接移動し、統合の設定を続けられます。
- **Back To Connector List**をクリックするとコネクター一覧に戻り、後で**Integration** -> **Rules**からルールを作成できます。

本例では**Create Rule**をクリックして続行します。

### OAuth2クライアント認証の設定

EMQX 6.0.4以降、HTTPサーバーコネクターはOAuth 2.0のクライアントクレデンシャルズグラントをサポートしています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュし、自動的に更新します。HTTPサーバー呼び出し時には`Authorization: Bearer <access_token>`ヘッダーを付加し、認証を行います。

コネクター作成または編集時に**OAuth2 Client Credentials**をオンにし、以下の設定を行います。

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークン取得用のOAuth2認可サーバーエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含まないこと。 |
| **Client ID** | 必須。アクセストークン取得に使うOAuth2クライアントID。 |
| **Client Secret** | 必須。アクセストークン取得に使うOAuth2クライアントシークレット。 |
| **Scope** | 任意。アクセストークン取得時に要求するOAuth2スコープ。 |
| **Token Request Timeout** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは`5`秒。 |
| **Enable TLS** | トークンエンドポイントへの接続にTLSを有効化するか。HTTPサーバーのTLS設定とは独立。 |

HOCON形式では、HTTPサーバーコネクター設定内で`url`、`headers`、`ssl`と同じ階層に`oauth2`ブロックを追加します。

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

EMQXは`application/x-www-form-urlencoded`の`POST`リクエストをトークンエンドポイントに送信し、`grant_type`、`client_id`、`client_secret`、任意の`scope`をリクエストボディに含めます。トークンエンドポイントは`200`レスポンスでJSON形式の`access_token`を返す必要があります。`token_type`と`expires_in`も返せます。`token_type`がある場合は`Bearer`でなければならず、`expires_in`は正の整数である必要があります。

::: warning 重要なお知らせ

- OAuth2を有効にした場合、HTTPサーバーコネクターやそのSinkで`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け付ける必要があります。HTTP Basic認証ヘッダーによる認証はサポートしていません。

:::

EMQXがアクセストークンを取得できない場合、コネクターのヘルスチェックは`disconnected`を報告します。

## HTTPサーバーSinkを使ったルールの作成

このセクションでは、ルールを作成しHTTPサーバーSinkを設定してMQTTメッセージをHTTPサーバーに送信する方法を説明します。

**Create Rule**をクリックすると自動的に**Create Rule**ページに遷移し、HTTPサーバーSink設定用の**Action pane**が表示され、先ほど作成したコネクターが選択済みとなっています。

1. **Type of Action**と**Action**は自動で`HTTP Server`と`Create Action`に設定され、新規Sinkを作成します。

2. Sinkの名前と説明を入力します。**Connector**は先ほど作成した`httpserver`が自動入力されています。

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

   テンプレート内の変数はルールSQLで選択されたフィールドから値が埋め込まれます。

5. **フォールバックアクション（任意）**：メッセージ送信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

6. **Create**をクリックする前に**Test Connectivity**をクリックし、SinkがHTTPサーバーに接続できるか確認できます。

7. **Create**をクリックしてSinkの設定を完了します。新しいSinkは**Create Rule**ページの**Action Outputs**セクションに表示されます。

8. **Rule ID**を入力します。システムでランダム生成するか任意に指定可能です（例：`my_rule`）。

9. **SQL Editor**に以下のSQL文を入力します。

   ```bash
   SELECT
     *
   FROM
     "t/#"
   ```

   このルールは`t/#`配下のすべてのMQTTメッセージにマッチします。

   :::tip

   独自のSQL文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`句に含めるよう注意してください。

   :::

10. ルール設定を確認後、**Save**をクリックしてルールを生成します。

ルール作成後、`t/#`配下のトピックにパブリッシュされたメッセージはルールで処理され、設定したHTTPサーバーに転送されます。

また、**Integration** -> **Flow Designer**でルールとHTTPサーバーSinkのデータフロートポロジーを確認できます。

## ルールのテスト

1. MQTTXを使ってトピック`t/1`にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
   ```

2. ダッシュボードの**Rule**ページでルール名をクリックし、統計情報を確認します。メトリクスに新規の受信メッセージと送信メッセージが1件ずつ表示されていれば、HTTPサーバーSinkによる正常な処理と転送が行われています。

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

   表示された内容は、EMQXがMQTTメッセージをJSON形式でHTTPサーバーに転送したことを示しています。リクエストボディ内のフィールドはSinkのリクエストボディテンプレートで設定した変数に対応しています。
