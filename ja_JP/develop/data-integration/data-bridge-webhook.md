# MQTTデータをHTTPサーバーに取り込む

HTTPサーバーデータ統合は、EMQXと外部HTTPサービスを迅速に連携させる方法を提供します。リクエストメソッドやリクエストデータ形式の柔軟な設定をサポートし、HTTPSによる安全な通信や認証機構も備えています。クライアントのメッセージやイベントデータをリアルタイムかつ効率的に柔軟に送信でき、IoTデバイスの状態通知、アラート通知、データ統合などのシナリオに対応可能です。

本ページでは、HTTPサーバーデータ統合の機能と特徴を詳述し、HTTPサーバーデータ統合の設定方法について実践的なガイドを提供します。

:::tip

HTTPサービスとの連携が必要で、ルールによるデータ処理が不要なユーザーには、より簡単で使いやすい[Webhook](./webhook.md)の利用を推奨します。

:::

## 動作概要

HTTPサーバーデータ統合はEMQXの標準機能であり、簡単な設定で外部HTTPサービスと連携できます。HTTPサービス側では、好みのプログラミング言語やフレームワークでコードを記述し、柔軟かつ複雑なデータ処理ロジックを実装できます。

<img src="./assets/emqx-integration-http.jpg" alt="emqx-integration-http" style="zoom:67%;" />

EMQXはルールエンジンとSinkを介してデバイスのイベントやデータをHTTPサーバーに転送します。ワークフローは以下の通りです：

1. **デバイスがEMQXに接続**：IoTデバイスが正常に接続すると、デバイスIDや送信元IPアドレスなどの属性を含むオンラインイベントが発生します。
2. **デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
3. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、トピックマッチングに基づき特定のソースからのMQTTメッセージやイベントを処理します。ルールエンジンは対応するルールにマッチし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などを行います。
4. **HTTPサーバーへのブリッジング**：設定したルールが処理済みのメッセージやイベントをHTTPサーバーに転送するアクションをトリガーします。ユーザーはルール処理結果からデータを抽出し、リクエストヘッダーやリクエストボディ、URLを動的に構築でき、外部サービスとの柔軟な連携が可能です。

イベントやメッセージデータがHTTPサーバーに送信された後は、以下のような柔軟な処理が可能です：

- デバイスの状態更新やイベントログの実装により、データに基づくデバイス管理システムの開発。
- メッセージデータをデータベースに書き込み、軽量なデータストレージ機能を実現。
- SQLルールでフィルタリングされた異常データをHTTPサービスで直接アラート通知システムに連携し、デバイス異常監視を実現。

## 特徴と利点

EMQXのHTTPサーバー連携を利用することで、以下のようなメリットがあります：

- **より多くの下流システムへのデータ配信拡張**：HTTPサービスはMQTTデータを分析プラットフォームやクラウドサービスなど多様な外部システムとシームレスに連携し、複数システム間でのデータ分配を容易にします。
- **リアルタイム応答と業務プロセスのトリガー**：HTTPサービスを通じて外部システムはMQTTデータをリアルタイムに受信し、業務プロセスをトリガー可能で迅速な対応を実現します。例として、アラートデータ受信による業務ワークフローの起動があります。
- **カスタムデータ処理**：外部システムは受信データに対して二次処理を行い、EMQXの機能に制約されない複雑なビジネスロジックを実装可能です。
- **疎結合な連携**：HTTPサービスはシンプルなHTTPインターフェースを利用し、システム連携を疎結合に実現します。

まとめると、HTTPサービスはリアルタイムかつ柔軟でカスタマイズ可能なデータ統合機能を提供し、多様なアプリケーション開発ニーズに応えます。

## はじめる前に

このセクションでは、HTTPサーバーデータ統合を作成する前に必要な準備について説明します。簡単なHTTPサーバーのセットアップも含みます。

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

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。

2. 右上の**Create**をクリックし、コネクタータイプとして**HTTP Server**を選択し、**Next**をクリックして**Configuration**ステップに進みます。

3. コネクターを設定します：

   - **Connector Name**：コネクターの名前を入力します。例：`my_httpserver`
   - **Description**（任意）：コネクターの説明を入力します。
   - **URL**：対象HTTPサーバーのURLを入力します。例：`http://localhost:5000`
   - **Headers**（任意）：このコネクター経由で送信されるHTTPリクエストヘッダーを追加します。
   - **OAuth2 Client Credentials**：トグルをオンにすると、EMQXがアクセストークンを取得し、対象HTTPサーバーへのリクエストに追加します。詳細は[OAuth2クライアント認証の設定](#configure-oauth2-client-credentials)を参照してください。
   - **Enable TLS**：トグルをオンにすると、対象HTTPサーバーへの接続にTLSを有効化します。この設定はOAuth2トークンエンドポイントのTLS設定とは独立しています。
   - **Advanced Settings**（任意）：接続に関する詳細オプションを設定します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

4. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがHTTPサーバーに接続できるかテストできます。

5. **Create**をクリックしてコネクターの作成を完了します。

### OAuth2クライアント認証の設定

EMQX 6.0.4以降、HTTPサーバーコネクターはOAuth 2.0 Client Credentials Grantをサポートしています。OAuth2を有効にすると、EMQXは設定されたトークンエンドポイントからアクセストークンを取得・キャッシュし、自動で更新します。EMQXが対象HTTPサーバーにリクエストを送る際、`Authorization: Bearer <access_token>`ヘッダーにトークンを付与し、対象サーバーはEMQXを認証できます。

コネクター作成または編集時に**OAuth2 Client Credentials**をオンにし、以下の設定を行います：

| ダッシュボード設定 | 説明 |
| --- | --- |
| **Token Endpoint** | 必須。アクセストークン取得に使用するOAuth2認可サーバーのエンドポイント。HTTPまたはHTTPSのURLで、ユーザー情報を含まないこと。 |
| **Client ID** | 必須。アクセストークン取得に使用するOAuth2クライアントID。 |
| **Client Secret** | 必須。アクセストークン取得に使用するOAuth2クライアントシークレット。 |
| **Scope** | 任意。アクセストークンに要求するOAuth2スコープ。 |
| **Token Request Timeout** | トークンエンドポイントへのHTTPリクエストのタイムアウト（秒）。デフォルトは`5`秒。 |
| **Enable TLS** | トークンエンドポイントへの接続にTLSを有効化するトグル。対象HTTPサーバーのTLS設定とは独立。 |

HOCON設定では、HTTPサーバーコネクターの`url`、`headers`、`ssl`と同じ階層に`oauth2`ブロックを追加します：

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

EMQXは`application/x-www-form-urlencoded`コンテンツタイプの`POST`リクエストをトークンエンドポイントに送信し、リクエストボディには`grant_type`、`client_id`、`client_secret`、および任意の`scope`を含みます。トークンエンドポイントは`200`レスポンスで`access_token`を含むJSONボディを返す必要があります。`token_type`と`expires_in`も返せます。存在する場合、`token_type`は`Bearer`であり、`expires_in`は正の整数でなければなりません。

:::warning 重要なお知らせ

- OAuth2を有効にしている場合、HTTPサーバーコネクターまたはそのSinkに`Authorization`ヘッダーを設定しないでください。EMQXは自動生成されるBearer認証ヘッダーと競合するため設定を拒否します。
- トークンエンドポイントは、クライアントIDとクライアントシークレットをリクエストボディのフォームフィールドとして受け入れる必要があります。HTTP Basic認証ヘッダーによる認証はサポートされていません。

:::

EMQXがアクセストークンを取得できない場合、コネクターのヘルスチェックは`disconnected`と報告します。

これでHTTPサーバーコネクターが作成されました。次に、HTTPサーバーに書き込むデータを指定するためのルールとSinkを作成します。

## HTTPサーバーSink付きルールの作成

このセクションでは、HTTPサーバーSinkを追加したルールの作成方法を説明します。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Rules**をクリックします。

2. 右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**でルールを設定します。

4. 例として以下の文を**SQL Editor**に入力します。これはトピック`t/#`配下のMQTTメッセージをHTTPサーバーに保存することを意味します。

   注意：独自のSQL構文を指定する場合、Sinkが必要とする全てのフィールドを`SELECT`句に含めていることを確認してください。

   ```bash
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action**ドロップダウンリストから`HTTP Server`を選択し、EMQXがルールで処理したデータをHTTPサーバーに送信するようにします。

   **Action**ドロップダウンは`Create Action`のままにするか、以前に作成したHTTPサーバーアクションを選択できます。この例では新しいSinkを作成し、ルールに追加します。

6. Sinkの**Name**と**Description**テキストボックスに名前と説明を入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-httpserver`を選択します。ドロップダウン横のボタンから新しいコネクターを作成することも可能です。設定パラメータの詳細は[コネクターの作成](#create-connector)を参照してください。

8. **URL**に`http://localhost:5000`を設定し、**Method**ドロップダウンから`POST`を選択します。その他はデフォルト値のままで構いません。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **Create**ボタンをクリックしてSinkの設定を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

11. **Create Rule**ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいHTTPサーバーSinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックしてトポロジーを確認できます。トピック`t/#`配下のメッセージがルール`my_rule`で解析され、HTTPサーバーに送信・保存されていることが確認できます。

## ルールのテスト

MQTTXを使ってトピック`t/1`にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
```

**Rule**ページでルール名をクリックし、統計情報を確認します。Sinkの稼働状況をチェックし、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

メッセージがHTTPサーバーに送信されているかを以下のように確認します：

```
python3 http_server.py
 * Serving Flask app 'http_server' (lazy loading)
 * Environment: production
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
 * Debug mode: off
 * Running on http://127.0.0.1:5000 (Press CTRL+C to quit)

got post request:  b'hello HTTP Server'
```
