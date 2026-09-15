# MQTTデータをHTTPサーバーに取り込む

HTTPサーバーデータ統合は、EMQXを外部HTTPサービスと迅速に連携させる方法を提供します。リクエストメソッドやリクエストデータ形式の柔軟な設定をサポートし、HTTPSによる安全な通信や認証機構を備えています。クライアントのメッセージやイベントデータをリアルタイムかつ効率的に柔軟に送信でき、IoTデバイスの状態通知やアラート通知、データ統合などのシナリオに対応可能です。

本ページでは、HTTPサーバーとのデータ統合の機能と特徴を詳述し、HTTPサーバーデータ統合の設定方法について実践的なガイダンスを提供します。

:::tip

ルールを使ったデータ処理が不要でHTTPサービスとの連携のみを希望するユーザーには、より簡単で使いやすい[Webhook](./webhook.md)の利用を推奨します。

:::

<video
  src="https://assets.emqx.com/data/video/emqx-docs/data-integration/http_server_integration.mp4"
  preload="metadata"
  controls
  muted
  playsinline
  style="width: 100%; border-radius: 8px;"></video>

## 動作概要

HTTPサーバーデータ統合はEMQXの標準機能であり、簡単な設定により外部HTTPサービスと連携できます。HTTPサービス側では、お好みのプログラミング言語やフレームワークでコードを記述し、柔軟かつ複雑なデータ処理ロジックを実装可能です。

<img src="./assets/emqx-integration-http.png" alt="emqx-integration-http" style="zoom:67%;" />

EMQXはルールエンジンとSinkを介してデバイスのイベントやメッセージをHTTPサーバーに転送します。ワークフローは以下の通りです。

1. **デバイスがEMQXに接続**：IoTデバイスが正常に接続すると、デバイスIDや送信元IPアドレスなどの属性を含むオンラインイベントが発生します。
2. **デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
3. **ルールエンジンがメッセージを処理**：ルールエンジンはトピックフィルターに基づいてメッセージをマッチングし、フィールドのフィルタリング、データ形式の変換、追加コンテキストによるメッセージの拡充など、設定されたルールに従って処理します。
4. **HTTPサーバーへのブリッジング**：ルールは処理済みメッセージやイベントをHTTPサーバーに転送するアクションをトリガーします。リクエストヘッダー、リクエストボディ、URLはルールの出力から動的に構築可能です。

イベントやメッセージデータがHTTPサーバーに送信された後は、以下のような柔軟な処理が可能です。

- デバイス管理システムでのデバイス状態の更新やデバイスイベントの記録
- メッセージデータのデータベースへの書き込みによる保存
- SQLルールで検出された異常データに基づくアラートや通知システムのトリガー

## 特徴と利点

EMQXのHTTPサーバー統合を利用することで、以下のようなメリットが得られます。

- **より多くの下流システムへのデータ配信拡張**：HTTPサービスにより、MQTTデータを分析プラットフォームやクラウドサービスなど多様な外部システムとシームレスに統合でき、複数システム間でのデータ分配が容易になります。
- **リアルタイム応答と業務プロセスのトリガー**：HTTPサービスを通じて外部システムがMQTTデータをリアルタイムに受信し、業務プロセスを迅速に起動可能です。例えば、アラートデータを受けて業務ワークフローをトリガーするなど。
- **カスタムデータ処理**：外部システム側で受信データに対して二次処理を行うことで、EMQXの機能に制約されないより複雑な業務ロジックを実現できます。
- **疎結合な統合**：HTTPサービスはシンプルなHTTPインターフェースを利用し、システム間の疎結合な連携を実現します。

まとめると、HTTPサービスはリアルタイムかつ柔軟でカスタマイズ可能なデータ統合機能を提供し、多様なアプリケーション開発ニーズに対応します。

## はじめる前に

このセクションでは、HTTPサーバーデータ統合の作成を開始する前に必要な準備、特に簡単なHTTPサーバーのセットアップについて説明します。

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

2. 上記コードを`http_server.py`として保存し、以下のコマンドでサーバーを起動します。

```shell
pip install flask

python3 http_server.py
```

## コネクターの作成

このセクションでは、SinkをHTTPサーバーに接続するためのHTTPサーバーコネクターの設定方法を説明します。

1. ダッシュボードの左メニューから**Integration** -> **Connector**をクリックします。
2. ページ右上の**Create**をクリックします。
3. コネクタータイプとして**HTTP Server**を選択し、**Next**をクリックします。
4. コネクターの名前を入力します。名前は英数字の組み合わせとしてください。例：`httpserver`
5. **URL**にHTTPサーバーのアドレスを設定します。例：`http://localhost:5000`
6. [任意] **Headers**にHTTPリクエストヘッダーを追加します。
7. [任意] **OAuth2 Client Credentials**を有効にすると、EMQXがアクセストークンを取得し、ターゲットHTTPサーバーへのリクエストに追加します。詳細は[OAuth2 Client Credentialsの設定](#configure-oauth2-client-credentials)を参照してください。
8. [任意] **Enable TLS**を有効にすると、ターゲットHTTPサーバーへの接続にTLSを使用します。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。
9. [任意] **Advanced Settings**で接続関連のオプションを設定します。詳細は[Sinkの特徴](./data-bridges.md#features-of-sink)を参照してください。
10. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがHTTPサーバーに接続できるか確認できます。
11. **Create**をクリックしてコネクターの設定を完了します。

コネクター作成後、ルールをこのコネクターで作成するかどうかのダイアログが表示されます。

- **Create Rule**をクリックすると、ルール作成ページに直接移動して統合設定を続行できます。
- あるいは**Back To Connector List**をクリックしてコネクター一覧に戻り、後で**Integration** -> **Rules**からルールを作成できます。

本例では**Create Rule**をクリックして続行します。

### OAuth2 Client Credentialsの設定

EMQX 6.0.4以降、HTTPサーバーコネクターはOAuth 2.0 Client Credentials Grantをサポートしています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュ・自動更新します。EMQXがターゲットHTTPサーバーを呼び出す際、`Authorization: Bearer <access_token>`ヘッダーにトークンを付与し、ターゲットサーバーがEMQXを認証可能にします。

コネクター作成または編集時に**OAuth2 Client Credentials**を有効にし、以下の設定を行います。

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークン取得に使用するOAuth2認可サーバーのエンドポイント。URLはHTTPまたはHTTPSで、ユーザー情報を含んではいけません。 |
| **Client ID** | 必須。アクセストークン取得に使用するOAuth2クライアントID。 |
| **Client Secret** | 必須。アクセストークン取得に使用するOAuth2クライアントシークレット。 |
| **Scope** | 任意。アクセストークンに要求するOAuth2スコープ。 |
| **Token Request Timeout** | トークンエンドポイントへのHTTPリクエストのタイムアウト。デフォルトは`5`秒。 |
| **Enable TLS** | トークンエンドポイントへのTLSを有効にするトグルスイッチ。ターゲットHTTPサーバーのTLS設定とは独立しています。 |

HOCON設定では、HTTPサーバーコネクター設定内の`url`、`headers`、`ssl`と同じレベルに`oauth2`ブロックを追加します。

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

EMQXは`application/x-www-form-urlencoded`コンテンツタイプの`POST`リクエストをトークンエンドポイントに送信し、リクエストボディに`grant_type`、`client_id`、`client_secret`、および任意の`scope`を含みます。トークンエンドポイントは`200`レスポンスで`access_token`を含むJSONボディを返す必要があります。`token_type`と`expires_in`も返すことができ、存在する場合は`token_type`は`Bearer`、`expires_in`は正の整数でなければなりません。

::: warning 重要なお知らせ

- OAuth2を有効にした場合、HTTPサーバーコネクターやそのSinkに`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントはクライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによる認証はサポートされていません。

:::

EMQXがアクセストークンを取得できない場合、コネクターのヘルスチェックは`disconnected`と報告します。

## HTTPサーバーSinkを使ったルールの作成

このセクションでは、ルールを作成しHTTPサーバーSinkを設定してMQTTメッセージをHTTPサーバーに送信する方法を説明します。

**Create Rule**をクリックすると、自動的に**Create Rule**ページに遷移し、HTTPサーバーSinkの設定用の**Action pane**が表示され、コネクターがすぐに使用可能な状態になっています。

1. **Type of Action**と**Action**は自動的に`HTTP Server`と`Create Action`に設定され、新しいSinkが作成されます。

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

   テンプレート内の変数はルールSQLで選択されたフィールドから値が埋め込まれます。

5. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがHTTPサーバーに接続できるか確認できます。

7. **Create**をクリックしてSinkの設定を完了します。新しいSinkは**Create Rule**ページの**Action Outputs**セクションに表示されます。

8. **Rule ID**を入力します。システムがランダム生成するか、自分で任意に定義できます（任意）。例：`my_rule`

9. **SQL Editor**に以下のSQL文を入力します。

   ```bash
   SELECT
     *
   FROM
     "t/#"
   ```

   このルールは`"t/#"`以下のトピックにパブリッシュされたすべてのMQTTメッセージにマッチします。

   :::tip

   独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`部分に含めていることを確認してください。

   :::

10. ルール設定を確認後、**Save**をクリックしてルールを生成します。

ルール作成後、`t/#`以下のトピックにパブリッシュされたメッセージはルールで処理され、設定したHTTPサーバーに転送されます。

また、**Integration** -> **Flow Designer**からルールとHTTPサーバーSinkのデータフロートポロジーを確認できます。

## ルールのテスト

1. MQTTXを使ってトピック`t/1`にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
   ```

2. ダッシュボードの**Rule**ページでルール名をクリックし、統計情報を確認します。メトリクスに新しい受信メッセージ数と送信メッセージ数が1件ずつ表示されていれば、メッセージが正常に処理されHTTPサーバーSinkにより転送されたことを示します。

3. HTTPサーバーがリクエストを受信していることを確認します。

   Python HTTPサーバーが起動中の場合、ターミナルには以下のような出力が表示されます。

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
