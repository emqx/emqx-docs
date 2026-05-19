# Hooks

[Hooks](https://reactjs.org/docs/getting-started.html) は、クラスを書かずに状態管理やその他の React 機能を利用できる拡張機構です。

EMQX も Hooks をサポートしており、関数呼び出し、メッセージの送受信、モジュール間のイベント伝達をインターセプトすることで、システム機能の変更や拡張が可能です。

## 仕組み

システムが **Hooks** 機構を採用していない場合、イベントの入力からハンドラー、結果までの一連の処理フローは見えず、変更もできません。

しかし、処理の途中に HookPoint を設けて関数をマウントできるようにすると、外部プラグインが複数のコールバック関数をマウントして呼び出しチェーンを形成できます。これにより、内部のイベント処理を拡張・変更可能になります。

<img src="./assets/hooks_in_system.png" alt="システム内のHooks" style="zoom:50%;" />

EMQX のいくつかの機能はこのフック機能を使って実装されています：

1. フックシステムを使ってメッセージの多段階ストリーミング処理（エンコード／デコードなど）を実現
2. 設定に応じてメッセージのパブリッシュ時にキャッシュを行う
3. フックのブロッキング機構を使ってメッセージのパブリッシュを遅延させる

システムで一般的に使われる認証／認可もこのロジックに従って実装されています。例えば [多言語拡張](./exhook.md) の場合：

`Built-in Database` 認証のみが有効な場合、上図のイベント処理ロジックに従い、認証モジュールの処理は以下の通りです：

1. EMQX がユーザーの認証リクエスト（Authenticate）を受信
2. EMQX が `ClientInfo` とデフォルトの `AccIn` で認証イベントのフックを実行
```erlang
%% デフォルト AccIn
{ok, #{is_superuser => false}}
```
3. `emqx_exhook` モジュールにコールバックし、この認証が有効と判断して **allow, is_superuser** の結果を取得
```erlang
%% AuthNResult
{ok, #{is_superuser => true}}
```
4. **認証成功** を返し、クライアントはスーパーユーザーとしてシステムに正常にアクセス可能となる

<img src="./assets/hooks_and_internal_model.png" alt="hooks_and_internal_model" style="zoom:50%;" />

このように、**Hooks** は EMQX の柔軟性を大幅に高めます。EMQX の動作をカスタマイズしたい場合、コアコードを変更する必要はなく、EMQX が特定箇所に用意した **HookPoint** に関数をフックするだけで済みます。

この一連の流れで注意すべきポイントは以下の3点です：

1. **HookPoint** の場所：役割、実行タイミング、マウント／アンマウント方法
2. **コールバック関数** の実装：入力パラメータ数、役割、データ構造、戻り値の意味
3. コールバック関数の**チェーン**上での実行機構：実行順序、チェーンの途中での実行終了方法

拡張プラグイン開発で Hooks を使う場合は、上記3点を十分理解し、**フック内でシステムのスループットに影響を与えるブロッキング関数の使用は避けるようにしてください。**

## コールバック関数チェーン

1つの **HookPoint** に対して複数のプラグインがイベントを監視し、対応処理を行う必要があるため、各 **HookPoint** には複数のコールバック関数が存在する場合があります。

この複数のコールバック関数が順次実行される連鎖を **コールバック関数チェーン** と呼びます。

**コールバック関数チェーン** は現在、[Chain-of-Responsibility](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern) パターンの概念に基づいて実装されています。フックの機能性と柔軟性を満たすために、以下の属性を持ちます：

- **順序付けられている**：チェーン上のコールバック関数は一定の順序で実行される必要があります。
- **入力パラメータ**：初期化パラメータが1つ以上あり、オプションでチェーン内で変更される累積値を持ちます。
- **出力結果**：チェーン内の各関数は出力を持ち、実行結果を気にしない場合は `ok` を返します。例えば通知系イベントでは「クライアントが正常にログインした」などの戻り値は不要です。
- **伝達的**：チェーン内のコールバック関数の結果は伝達されます。より柔軟に使えるように、チェーン内コールバック関数の戻り値処理は以下の**2モード**を設計しています。
  - **結果伝達モード**<br />
    チェーンの各コールバック関数はチェーンの入力と、前の関数の戻り値（累積値として解釈可能）を引数に受け取ります。最後の関数の戻り値がチェーン全体の戻り値となります。チェーンは初期累積値を指定して呼び出されます。
  - **結果透過モード**<br />
    チェーンの各関数はチェーンの入力のみを気にし、前の関数の戻り値は無視します。チェーンの戻り値は固定で `ok` となります。<br />
    これは結果伝達モードの特殊ケースで、初期累積値が `ok`、チェーン内の各関数は入力パラメータのみを参照し累積値を `ok` のまま維持します。<br />
    通知系イベントの多くはこちらのロジックに従うため、一般的な **コールバック関数チェーン** 実行モジュールを提供しています。
- **チェーンの途中終了と無視** が可能
  - **途中終了**：この関数の実行完了後、チェーンの実行を直ちに終了し、以降のコールバック関数は無視されます。<br />例えば、ある認証プラグインがログイン許可を出した場合、他の認証プラグインのチェックは不要なので途中終了します。
  - **無視**：チェーンの処理結果を変更せず、前の関数の戻り値をそのまま次の関数に渡します。<br />例えば複数の認証プラグインがある場合、あるプラグインが対象外と判断し処理を変更しない場合に使います。

以上より、チェーン内のコールバック関数の戻り値処理方法に応じて、以下の2つのプログラムフロー図が得られます。

### 結果伝達モード
<img src="./assets/hooks_return_value.png" alt="hooks_return_value" style="zoom:50%;" />

図の意味：
1. 図中には3つのコールバック関数 `Fun1`、`Fun2`、`Fun3` が登録されており、示された順序で実行される
2. コールバック関数の実行順序は優先度で決まり、同じ優先度の場合はマウント順
3. チェーンの入力パラメータは読み取り専用の `Args` と、関数が変更可能な `InitAcc`
4. チェーンの実行が途中終了しても、必ず戻り値を返す。戻り値の形式は以下の通り：
   - コールバック関数が返す値：
     - `ok`：この操作を無視し、前関数の戻り値 `Acc` と読み取り専用 `Args` でチェーンを継続
     - `{ok, NewAcc}`：何らかの処理を行い、`Acc` の内容を変更し、`Args` と新しい `NewAcc` でチェーン継続
   - また、以下の値も返すことが可能：
     - `stop`：チェーンの伝達を停止し、前関数の戻り値 `Acc` を即座に返す
     - `{stop, NewAcc}`：チェーンの伝達を停止し、この関数の修正後の `NewAcc` を即座に返す

### 結果透過モード
<img src="./assets/hooks_multiple_value.png" alt="hooks_multiple_value" style="zoom:50%;" />

このモードは、戻り値を無視する実行モードが結果伝達モードの特殊ケースであることを示しています。
つまり、`InitAcc` が `ok` で、チェーン上の各コールバック関数が `ok | {ok, ok} | stop | {stop, ok}` を返す場合に相当します。

以上がコールバック関数チェーンの主な設計思想であり、フック上のコールバック関数の実行ロジックを規定しています。

以下の [HookPoint](#hookpoint) と [コールバック関数](#callback) の2節では、フックに関するすべての操作は [emqx](https://github.com/emqx/emqx) が提供する Erlang コードレベルの API に依存しており、これがフックロジック実装の基盤です。
- 他言語アプリケーションでのフック利用は、[Extension Hook](./exhook.md) を参照してください。

## HookPoint 一覧

EMQX はクライアントのライフサイクルにおける主要な動作に基づき、多数の **HookPoint** をあらかじめ用意しています。システムにプリセットされているマウントポイントは以下の通りです：

| 名前                  | 説明                         | 実行タイミング                                                                             |
|-----------------------|------------------------------|-------------------------------------------------------------------------------------------|
| client.connect        | 接続パケットの処理           | サーバーがクライアントから接続パケットを受信したとき                                   |
| client.connack        | 接続応答の発行               | サーバーが接続応答メッセージを発行する準備ができたとき                                 |
| client.connected      | 接続成功                     | クライアント認証が完了し、正常に接続された後                                           |
| client.disconnected   | 切断                         | クライアントの接続レイヤーが閉じる準備ができたとき                                     |
| client.authenticate   | 接続認証                     | `client.connect` 実行後                                                                 |
| client.post_authn     | 認証後の書き換え             | `client.authenticate` の認証チェーン完了後（6.1.2で追加）                               |
| client.authorize      | Pub/Sub 認可                 | `publish/subscribe` 操作実行前                                                          |
| client.subscribe      | トピックのサブスクライブ     | サブスクリプションメッセージ受信後、`client.authorize` 実行前                           |
| client.unsubscribe    | サブスクライブ解除           | アン・サブスクライブパケット受信後                                                     |
| session.created       | セッション作成               | `client.connected` 完了後、新規セッション作成時                                        |
| session.subscribed    | セッションのトピック登録     | サブスクライブ操作完了後                                                                |
| session.unsubscribed  | セッションのトピック解除     | アン・サブスクライブ操作完了後                                                          |
| session.resumed       | セッション再開               | `client.connected` 実行時、旧セッション情報が正常に再開されたとき                       |
| session.discarded     | セッション破棄               | **discarded** によってセッションが終了した後                                           |
| session.takenover     | セッション奪取               | **takenover** によってセッションが終了した後                                           |
| session.terminated    | セッション終了               | その他の理由でセッションが終了した後                                                   |
| message.publish       | メッセージパブリッシュ       | サーバーがメッセージをパブリッシュ（ルーティング）する前                               |
| message.delivered     | メッセージ配信               | メッセージがクライアントに配信される直前                                               |
| message.acked         | メッセージアック受信         | クライアントからメッセージの ACK を受信後                                              |
| message.dropped       | メッセージ破棄               | パブリッシュされたメッセージが破棄された後                                             |

::: tip
- **セッション破棄（discarded）**：クライアントが `clean session` 方式でログインした場合、サーバーに既存のセッションがあれば古いセッションは破棄されます。
- **セッション奪取（takenover）**：クライアントが `Reserved Session` 方式でログインした場合、サーバーに既存のセッションがあれば新しい接続により古いセッションが奪取されます。
:::

### Hook と Unhook

EMQX はフックのマウントとアンマウントのための API を提供しています。

**Hook:**

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function, Args}: コールバック関数のモジュール、関数、追加引数
%% Priority：整数、デフォルトは0
emqx:hook(Name, {Module, Function, Args}, Priority).
```

フックが完了すると、コールバック関数は優先度順、同優先度の場合はフック順で実行されます。公式プラグインのフックはすべて優先度 `0` です。

**Unhook**：

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function}: コールバック関数のモジュールと関数
emqx:unhook(Name, {Module, Function}).
```

## コールバック関数

コールバック関数の入力パラメータと戻り値は以下の表の通りです。

パラメータのデータ構造は [emqx_types.erl](https://github.com/emqx/emqx/tree/master/apps/emqx/src/emqx_types.erl) を参照してください。

| 名前                  | 入力パラメータ                                               | 戻り値              |
|-----------------------|--------------------------------------------------------------|---------------------|
| client.connect        | `ConnInfo`: クライアント接続レイヤーパラメータ<br />`Props`: MQTT v5.0 接続パケットのプロパティ | 新しい `Props`      |
| client.connack        | `ConnInfo`: クライアント接続レイヤーパラメータ<br />`Rc`: 戻りコード<br />`Props`: MQTT v5.0 接続応答パケットのプロパティ | 新しい `Props`      |
| client.connected      | `ClientInfo`: クライアント情報パラメータ<br />`ConnInfo`: クライアント接続レイヤーパラメータ | -                   |
| client.disconnected   | `ClientInfo`: クライアント情報パラメータ<br />`ConnInfo`: クライアント接続レイヤーパラメータ<br />`ReasonCode`: 理由コード | -                   |
| client.authenticate   | `ClientInfo`: クライアント情報パラメータ<br />`AuthNResult`: 認証結果 | 新しい `AuthNResult` |
| client.post_authn     | `Context`: マップ `#{client_info := ClientInfo}`（認証応答の `client_attrs` を含む統合クライアント情報） | 新しい `Context` または拒否時は `{error, Reason}`（6.1.2で追加） |
| client.authorize      | `ClientInfo`: クライアント情報パラメータ<br />`Topic`: パブリッシュ／サブスクライブトピック<br />`PubSub`: パブリッシュ／サブスクライブ<br />`AuthZResult`: 認可結果 | 新しい `AuthZResult` |
| client.subscribe      | `ClientInfo`: クライアント情報パラメータ<br />`Props`: MQTT v5.0 サブスクリプションメッセージのプロパティ<br />`TopicFilters`: サブスクライブトピックのリスト | 新しい `TopicFilters` |
| client.unsubscribe    | `ClientInfo`: クライアント情報パラメータ<br />`Props`: MQTT v5.0 アン・サブスクリプションメッセージのプロパティ<br />`TopicFilters`: アン・サブスクライブトピックのリスト | 新しい `TopicFilters` |
| session.created       | `ClientInfo`: クライアント情報パラメータ<br />`SessInfo`: セッション情報 | -                   |
| session.subscribed    | `ClientInfo`: クライアント情報パラメータ<br />`Topic`: サブスクライブトピック<br />`SubOpts`: サブスクライブ操作の設定オプション | -                   |
| session.unsubscribed  | `ClientInfo`: クライアント情報パラメータ<br />`Topic`: アン・サブスクライブトピック<br />`SubOpts`: アン・サブスクライブ操作の設定オプション | -                   |
| session.resumed       | `ClientInfo`: クライアント情報パラメータ<br />`SessInfo`: セッション情報 | -                   |
| session.discarded     | `ClientInfo`: クライアント情報パラメータ<br />`SessInfo`: セッション情報 | -                   |
| session.takenover     | `ClientInfo`: クライアント情報パラメータ<br />`SessInfo`: セッション情報 | -                   |
| session.terminated    | `ClientInfo`: クライアント情報パラメータ<br />`Reason`: 終了理由<br />`SessInfo`: セッション情報 | -                   |
| message.publish       | `Message`: メッセージオブジェクト                            | 新しい `Message`    |
| message.delivered     | `ClientInfo`: クライアント情報パラメータ<br />`Message`: メッセージオブジェクト | 新しい `Message`    |
| message.acked         | `ClientInfo`: クライアント情報パラメータ<br />`Message`: メッセージオブジェクト | -                   |
| message.dropped       | `Message`: メッセージオブジェクト<br />`By`: 破棄者<br />`Reason`: 破棄理由 | -                   |

これらのフックの利用例は [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template) を参照してください。
