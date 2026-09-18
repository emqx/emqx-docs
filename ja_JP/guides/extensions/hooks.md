# Hooks

[Hooks](https://reactjs.org/docs/getting-started.html) は、クラスを書かずに状態やその他の React 機能を使用できる拡張機構です。

EMQX も Hooks をサポートしており、関数呼び出し、メッセージの受け渡し、モジュール間のイベント伝達をインターセプトすることで、システム機能の変更や拡張が可能です。

## 動作原理

システムが **Hooks** 機構を採用していない場合、イベント処理の全フロー（イベントの入力からハンドラーおよび結果まで）は見えず、変更もできません。

しかし、処理の途中に HookPoint を設けて関数をマウントできるようにすると、外部プラグインが複数のコールバック関数をマウントして呼び出しチェーンを形成できます。これにより、内部のイベント処理を拡張・変更できます。

<img src="./assets/hooks_in_system.png" alt="システム内のHooks" style="zoom:50%;" />

EMQX のいくつかの機能はこのフック機能を使って実装されています：

1. フックシステムを使ったメッセージの多段階ストリーミング処理（エンコード／デコードなど）
2. メッセージパブリッシュ時の設定に応じたメッセージのキャッシュ
3. フックのブロッキング機構を使ったメッセージパブリッシュの遅延

システムで一般的に使われる認証／認可もこのロジックに基づいて実装されています。例として [多言語拡張](./exhook.md) を挙げます：

`Built-in Database` 認証のみが有効な場合、上図のイベント処理ロジックに従い、認証モジュールの処理は以下の通りです：

1. EMQX がユーザーの認証リクエスト（Authenticate）を受け取る
2. EMQX が `ClientInfo` とデフォルトの `AccIn` を引数に認証イベントのフックを実行する
```erlang
%% デフォルトの AccIn
{ok, #{is_superuser => false}}
```
3. `emqx_exhook` モジュールにコールバックし、この認証が有効と判断し、**allow, is_superuser** の結果を得る
```erlang
%% AuthNResult
{ok, #{is_superuser => true}}
```
4. **認証成功** を返し、クライアントはスーパーユーザーとして正常にシステムにアクセスできる

<img src="./assets/hooks_and_internal_model.png" alt="hooks_and_internal_model" style="zoom:50%;" />

このように、**Hooks** により EMQX の柔軟性が大幅に向上します。EMQX の挙動をカスタマイズしたい場合、コアコードを変更せず、EMQX が特定箇所に提供する **HookPoint** に関数をフックするだけで済みます。

この一連の処理で注意すべき点は以下の3つです：

1. **HookPoint** の位置：役割、実行タイミング、マウントおよびアンマウント方法
2. **コールバック関数** の実装：入力パラメーター数、役割、データ構造、返り値の意味
3. コールバック関数の **チェーン** 上での実行機構：実行順序、チェーンの途中での早期終了方法

拡張プラグイン開発で Hooks を使う場合は、上記3点を十分理解し、**システムのスループットに影響を与えるため、フック内でブロッキング関数を使わないようにしてください。**

## コールバック関数チェーン

単一の **HookPoint** に対して複数のプラグインがイベントに関心を持ち、対応処理を行う必要があるため、各 **HookPoint** には複数のコールバック関数が存在する場合があります。

この複数のコールバック関数が順次実行される連鎖を **コールバック関数チェーン** と呼びます。

**コールバック関数チェーン** は現在 [Chain-of-Responsibility](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern) の概念に基づいて実装されています。フックの機能性と柔軟性を満たすため、以下の特性を持ちます：

- **順序付けられている**：コールバック関数は一定の順序で実行される必要があります。
- **入力パラメーター**：初期化パラメーターが1つ以上あり、オプションでチェーンによる修正のための累積値があります。
- **出力結果**：チェーン内の各関数は出力を持ち、実行結果を気にしないコールバック関数は `ok` を返します。例えば通知系イベントでは「クライアントが正常にログインした」など、戻り値は不要です。
- **伝達的**：チェーン内のコールバック関数の結果は伝達されます。より柔軟にするため、返り値の扱いには **2つのモード** を設計しています。
  - **結果伝達モード**<br />
    チェーン内の各コールバック関数はチェーンの入力と、前の関数の返り値（累積値と解釈可能）を受け取ります。最後の関数の返り値がチェーン全体の返り値となります。チェーン呼び出し時に累積値の初期値を指定します。
  - **結果透過モード**<br />
    チェーン内の各関数はチェーンの入力のみを気にし、前の関数の返り値は無視します。チェーン全体の返り値は固定で `ok` です。<br />
    これは実質的に **結果伝達モード** の特殊ケースで、初期累積値が `ok` で各関数が返す値も `ok` のまま変えないケースです。通知イベントの多くはこのロジックに従います。これにより一般的な **コールバック関数チェーン** 実行モジュールを提供しています。
- **コールバック関数チェーン** は関数に対して *チェーンの早期終了* と *処理の無視* を許可します。
  - **早期終了**：この関数の実行後にチェーンの実行を即座に終了し、以降のコールバック関数は無視されます。<br />例えば認証でログインを許可した場合、他の認証プラグインのチェックを省略したい場合に使います。
  - **処理の無視**：チェーン上の処理結果を変更せず、そのまま次のコールバック関数に渡します。<br />
    例えば複数の認証プラグインがある場合、ある認証プラグインが対象外のクライアントだと判断した際に処理を変更せずスルーします。

以上より、チェーン上のコールバック関数の返り値の扱いによって、2つのプログラムフロー図が得られます。

### 結果伝達モード
<img src="./assets/hooks_return_value.png" alt="hooks_return_value" style="zoom:50%;" />

図の意味は以下の通りです：
1. 図中には3つのコールバック関数 `Fun1`、`Fun2`、`Fun3` が登録され、示された順に実行されます
2. コールバック関数の実行順序は優先度で決まり、同じ優先度の場合はマウント順です
3. チェーンの入力パラメーターは読み取り専用の `Args` と、関数が修正可能な `InitAcc` です
4. チェーンの実行がどのように終了しても、返り値は常に存在し、返り値の形式に依存します
   - コールバック関数の返り値は：
     - `ok`：処理を無視し、読み取り専用の `Args` と前関数の `Acc` でチェーンを継続
     - `{ok, NewAcc}`：何らかの処理を行い、`Acc` を修正し、新しい `NewAcc` でチェーンを継続
   - また、コールバック関数は：
     - `stop`：チェーンの伝達を停止し、前関数の `Acc` を即座に返す
     - `{stop, NewAcc}`：チェーンの伝達を停止し、この修正済みの `NewAcc` を即座に返す

### 結果透過モード
<img src="./assets/hooks_multiple_value.png" alt="hooks_multiple_value" style="zoom:50%;" />

このモードは最初の実行モードと比較すると、チェーン内の返り値を無視する実行モードは実質的に返り値を伝達するモードの特殊ケースです。
すなわち、`InitAcc` の値が `ok` で、チェーンにマウントされる各コールバック関数が `ok | {ok, ok} | stop | {stop, ok}` を返す場合に相当します。

以上がコールバック関数チェーンの主な設計コンセプトであり、フック上のコールバック関数の実行ロジックを規定しています。

以下の [HookPoint](#hookpoint) と [callback function](#callback) の2節では、フックに関するすべての操作は [emqx](https://github.com/emqx/emqx) が提供する Erlang コードレベルの API に依存しており、これがフックロジック全体の基盤となっています。
- 他言語アプリケーションでのフック利用は、[Extension Hook](./exhook.md) を参照してください。

## HookPoint 一覧

EMQX はクライアントのライフサイクルにおける主要な活動に基づき、多数の **HookPoint** をあらかじめ用意しています。システムにプリセットされているマウントポイントは以下の通りです：

| 名前                 | 説明                         | 実行タイミング                                                                                 |
|----------------------|------------------------------|-----------------------------------------------------------------------------------------------|
| client.connect       | 接続パケットの処理           | サーバーがクライアントから接続パケットを受信したとき                                       |
| client.connack       | 接続応答の発行               | サーバーが接続応答メッセージを発行する準備ができたとき                                     |
| client.connected     | 接続成功                     | クライアント認証完了後、システムへの接続に成功したとき                                     |
| client.disconnected  | 切断                         | クライアントの接続層が閉じる準備ができたとき                                               |
| client.authenticate  | 接続認証                     | `client.connect` 実行後                                                                     |
| client.authorize     | Pub/Sub 認可                 | `publish/subscribe` 操作実行前                                                              |
| client.subscribe     | トピックのサブスクライブ     | サブスクリプションメッセージ受信後、`client.authorize` 実行前                              |
| client.unsubscribe   | サブスクライブ解除           | サブスクライブ解除パケット受信後                                                            |
| session.created      | セッション作成               | `client.connected` 完了後、新規セッション作成時                                            |
| session.subscribed   | セッションのトピックサブスクライブ | サブスクライブ操作完了後                                                                    |
| session.unsubscribed | セッションのサブスクライブ解除 | サブスクライブ解除操作完了後                                                                |
| session.resumed      | セッション再開               | `client.connected` 実行時、旧セッション情報が正常に再開されたとき                          |
| session.discarded    | セッション破棄               | セッションが **discarded** により終了した後                                                |
| session.takenover    | セッション引き継ぎ           | セッションが **takenover** により終了した後                                                |
| session.terminated   | セッション終了               | その他の理由でセッションが終了した後                                                       |
| message.publish      | メッセージパブリッシュ       | サーバーがメッセージをパブリッシュ（ルーティング）する前                                   |
| message.delivered    | メッセージ配信               | メッセージがクライアントに配信される準備ができた前                                        |
| message.acked        | メッセージアック受信         | クライアントからメッセージの ACK を受信した後                                             |
| message.dropped      | メッセージ破棄               | パブリッシュされたメッセージが破棄された後                                                |

::: tip
- **セッション破棄（discarded）**：クライアントが `clean session` 方式でログインした場合、サーバーに既存のセッションがあれば古いセッションが破棄されます。
- **セッション引き継ぎ（takenover）**：クライアントが `Reserved Session` 方式でログインした場合、サーバーに既存のセッションがあれば新しい接続により古いセッションが引き継がれます。
:::

### Hook と Unhook

EMQX はフックとアンフックの操作用 API を提供しています。

**Hook:**

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function, Args}: コールバック関数のモジュール、関数、追加引数
%% Priority：整数、デフォルトは0
emqx:hook(Name, {Module, Function, Args}, Priority).
```

フック完了後、コールバック関数は優先度順、同じ優先度の場合はフック順に実行されます。公式プラグインのマウントフックはすべて優先度 `0` です。

**Unhook**：

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function}: コールバック関数のモジュールと関数
emqx:unhook(Name, {Module, Function}).
```

## コールバック関数

コールバック関数の入力パラメーターと返り値は以下の表の通りです：

パラメーターのデータ構造は [emqx_types.erl](https://github.com/emqx/emqx/tree/master/apps/emqx/src/emqx_types.erl) を参照してください。

| 名前                 | 入力パラメーター                                              | 返り値           |
| -------------------- | ------------------------------------------------------------ | ---------------- |
| client.connect       | `ConnInfo`: クライアント接続層パラメーター<br />`Props`: MQTT v5.0 接続パケットのプロパティ | 新しい `Props`   |
| client.connack       | `ConnInfo`: クライアント接続層パラメーター<br />`Rc`: 返却コード<br />`Props`: MQTT v5.0 接続応答パケットのプロパティ | 新しい `Props`   |
| client.connected     | `ClientInfo`: クライアント情報パラメーター<br />`ConnInfo`: クライアント接続層パラメーター | -                |
| client.disconnected  | `ClientInfo`: クライアント情報パラメーター<br />`ConnInfo`: クライアント接続層パラメーター<br />`ReasonCode`: 理由コード | -                |
| client.authenticate  | `ClientInfo`: クライアント情報パラメーター<br />`AuthNResult`: 認証結果                       | 新しい `AuthNResult` |
| client.authorize     | `ClientInfo`: クライアント情報パラメーター<br />`Topic`: パブリッシュ／サブスクライブトピック<br />`PubSub`: パブリッシュ／サブスクライブ<br />`AuthZResult`: 認可結果 | 新しい `AuthZResult` |
| client.subscribe     | `ClientInfo`: クライアント情報パラメーター<br />`Props`: MQTT v5.0 サブスクリプションメッセージのプロパティ<br />`TopicFilters`: サブスクライブトピックのリスト | 新しい `TopicFilters` |
| client.unsubscribe   | `ClientInfo`: クライアント情報パラメーター<br />`Props`: MQTT v5.0 サブスクライブ解除メッセージのプロパティ<br />`TopicFilters`: サブスクライブ解除トピックのリスト | 新しい `TopicFilters` |
| session.created      | `ClientInfo`: クライアント情報パラメーター<br />`SessInfo`: セッション情報                     | -                |
| session.subscribed   | `ClientInfo`: クライアント情報パラメーター<br />`Topic`: サブスクライブトピック<br />`SubOpts`: サブスクライブ操作の設定オプション | -                |
| session.unsubscribed | `ClientInfo`: クライアント情報パラメーター<br />`Topic`: サブスクライブ解除トピック<br />`SubOpts`: サブスクライブ解除操作の設定オプション | -                |
| session.resumed      | `ClientInfo`: クライアント情報パラメーター<br />`SessInfo`: セッション情報                     | -                |
| session.discarded    | `ClientInfo`: クライアント情報パラメーター<br />`SessInfo`: セッション情報                     | -                |
| session.takenover    | `ClientInfo`: クライアント情報パラメーター<br />`SessInfo`: セッション情報                     |                  |
| session.terminated   | `ClientInfo`: クライアント情報パラメーター<br />`Reason`: 終了理由<br />`SessInfo`: セッション情報 | -                |
| message.publish      | `Message`: メッセージオブジェクト                            | 新しい `Message` |
| message.delivered    | `ClientInfo`: クライアント情報パラメーター<br />`Message`: メッセージオブジェクト              | 新しい `Message` |
| message.acked        | `ClientInfo`: クライアント情報パラメーター<br />`Message`: メッセージオブジェクト              | -                |
| message.dropped      | `Message`: メッセージオブジェクト<br />`By`: 破棄者<br />`Reason`: 破棄理由                   | -                |

これらのフックの利用例は [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template) を参照してください。
