# Hooks

[Hooks](https://reactjs.org/docs/getting-started.html) は、クラスを書かずに状態管理やその他の React 機能を利用できる拡張機構です。

EMQX も Hooks をサポートしており、関数呼び出しやメッセージの受け渡し、モジュール間のイベント伝達をインターセプトすることで、システム機能の修正や拡張が可能です。

## 仕組み

システムに **Hooks** 機構が導入されていない場合、イベントの入力からハンドラー、結果に至るまでの一連の処理フローは見えず、変更もできません。

しかし、処理の途中に関数をマウントする HookPoint を設けることで、外部プラグインが複数のコールバック関数をマウントし呼び出しチェーンを形成できます。これにより、内部のイベント処理を拡張・修正できます。

<img src="./assets/hooks_in_system.png" alt="システム内のHooks" style="zoom:50%;" />

EMQX のいくつかの機能はこのフック機能を使って実装されています：

1. フックシステムを用いたメッセージの多段階ストリーミング処理（エンコード／デコードなど）
2. 設定に応じたメッセージパブリッシュ時のキャッシュ処理
3. フックのブロッキング機構を使ったメッセージパブリッシュの遅延

システムで一般的に使われる認証／認可もこのロジックに基づいて実装されています。例として[多言語拡張](./exhook.md)を挙げます：

`Built-in Database` 認証のみが有効な場合、上図のイベント処理ロジックに従い、認証モジュールの処理は以下の通りです：

1. EMQX はユーザーの認証リクエスト（Authenticate）を受け取る
2. EMQX は `ClientInfo` とデフォルトの `AccIn` を引数に認証イベントのフックを実行する
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

<img src="./assets/hooks_and_internal_model.png" alt="hooksと内部モデル" style="zoom:50%;" />

このように、**Hooks** は EMQX の柔軟性を大幅に高めます。EMQX の挙動をカスタマイズしたい場合、コアコードを修正する必要はなく、EMQX が特定箇所に用意した **HookPoint** に関数をフックするだけで済みます。

この一連の流れで注意すべきポイントは以下の3つです：

1. **HookPoint** の場所：役割、実行タイミング、マウントとアンマウントの方法
2. **コールバック関数** の実装：入力パラメータ数、役割、データ構造、返り値の意味
3. **チェーン上のコールバック関数の実行機構**：実行順序、チェーンの途中での実行停止方法

拡張プラグイン開発で Hooks を使う場合は、これら3点を十分理解し、**システムのスループットに影響を与えるため、フック内でのブロッキング関数の使用は極力避けてください。**

## コールバック関数チェーン

単一の **HookPoint** に複数のプラグインがイベントを監視し対応処理を行う場合があるため、各 **HookPoint** には複数のコールバック関数が存在します。

これら複数のコールバック関数が順次実行される連鎖を **コールバック関数チェーン** と呼びます。

**コールバック関数チェーン** は現在 [Chain-of-Responsibility](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern) パターンに基づいて実装されています。フックの機能性と柔軟性を満たすため、以下の特徴を持ちます：

- **順序性**：チェーン上のコールバック関数は特定の順序で実行される必要があります。
- **入力パラメータ**：初期化パラメータが1つ以上あり、オプションでチェーン内で修正される累積値があります。
- **出力結果**：チェーン内の各関数は出力を持ち、実行結果を気にしないコールバック関数は `ok` を返します。例えば通知系イベントでは「クライアントが正常にログインした」などの戻り値は不要です。
- **伝達性**：チェーン内のコールバック関数の結果は伝達されます。フックの柔軟性のため、返り値の扱いには以下の**2つのモード**があります。
  - **結果伝達モード**<br />
    チェーン内の各関数はチェーンの入力と前関数の返り値（累積値）を引数に受け取り、最後の関数の返り値がチェーン全体の返り値となります。初回の累積値はチェーン呼び出し時に指定します。
  - **結果透過モード**<br />
    各関数はチェーンの入力のみを気にし、前の関数の返り値は無視します。チェーンの返り値は常に `ok` です。<br />
    これは結果伝達モードの特例で、初期累積値が `ok` で、チェーン内の各関数が `ok | {ok, ok} | stop | {stop, ok}` を返す場合に相当します。通知系イベントの多くはこのロジックに従います。<br />このため、一般的な **コールバック関数チェーン** 実行モジュールを提供しています。
- **チェーンの途中終了と無視** の許容
  - **途中終了**：ある関数の実行完了後、チェーンの実行を即座に終了し、それ以降の関数は無視されます。<br />例えば、ある認証プラグインがログインを許可した場合、他の認証プラグインのチェックは不要なので途中終了します。
  - **無視**：チェーンの処理結果を変更せず、そのまま次の関数に渡します。<br />例えば複数の認証プラグインがある場合、あるプラグインが対象外のクライアントと判断し結果を変更しない場合は無視し、前関数の返り値をそのまま次関数に渡します。

以上より、チェーン上のコールバック関数の返り値の扱いによって、2種類の処理フロー図が得られます。

### 結果伝達モード
<img src="./assets/hooks_return_value.png" alt="hooks_return_value" style="zoom:50%;" />

図の意味は以下の通りです：
1. チェーンに3つのコールバック関数 `Fun1` `Fun2` `Fun3` が登録され、図示の順に実行される
2. 実行順序は優先度で決まり、同じ優先度の場合はマウント順に実行される
3. チェーンの入力パラメータは読み取り専用の `Args` と、関数による修正用の `InitAcc`
4. チェーンの実行がどのように終了しても、返り値は必ず存在し、返り値の形式によって以下の動作になる
   - コールバック関数の返り値：
     - `ok`：この操作を無視し、読み取り専用の `Args` と前関数の `Acc` でチェーンを継続
     - `{ok, NewAcc}`：何らかの処理を行い、`Acc` の内容を修正し、新しい `NewAcc` でチェーンを継続
   - また、返り値として：
     - `stop`：チェーンの伝達を停止し、前関数の `Acc` を即座に返す
     - `{stop, NewAcc}`：チェーンの伝達を停止し、この修正済みの `NewAcc` を即座に返す

### 結果透過モード
<img src="./assets/hooks_multiple_value.png" alt="hooks_multiple_value" style="zoom:50%;" />

このモードは前述の結果伝達モードの特例であり、`InitAcc` が `ok` で、チェーン上の各コールバック関数が `ok | {ok, ok} | stop | {stop, ok}` を返す場合に相当します。

以上がコールバック関数チェーンの主な設計思想であり、フック上のコールバック関数の実行ロジックを規定しています。

以下の [HookPoint](#hookpoint) と [コールバック関数](#callback) の節では、フック操作はすべて [emqx](https://github.com/emqx/emqx) が提供する Erlang コードレベルの API に依存しており、これがフックロジック全体の基盤です。
- 他言語でのフック利用については、[Extension Hook](./exhook.md) を参照してください。

## HookPoint 一覧

EMQX はクライアントのライフサイクルにおける主要なアクティビティを基に、多数の **HookPoint** をあらかじめ用意しています。システムにプリセットされたマウントポイントは以下の通りです：

| 名前                  | 説明                          | 実行タイミング                                                                                  |
|-----------------------|-------------------------------|------------------------------------------------------------------------------------------------|
| client.connect        | 接続パケットの処理             | サーバーがクライアントから接続パケットを受信したとき                                         |
| client.connack        | 接続応答の発行                 | サーバーが接続応答メッセージを発行する準備ができたとき                                       |
| client.connected      | 接続成功                     | クライアント認証が完了し、システムへの接続が成功した後                                       |
| client.disconnected   | 切断                         | クライアントの接続層が閉じる準備ができたとき                                                 |
| client.authenticate   | 接続認証                     | `client.connect` 実行後                                                                       |
| client.post_authn     | 認証後のクライアント情報書換 | `client.authenticate` の認証チェーン完了後（6.1.2 で追加）                                    |
| client.authorize      | Pub/Sub 認可                 | `publish/subscribe` 操作実行前                                                                |
| client.subscribe      | トピックのサブスクライブ       | サブスクライブメッセージ受信後、`client.authorize` 実行前                                    |
| client.unsubscribe    | サブスクライブ解除             | サブスクライブ解除パケット受信後                                                               |
| session.created       | セッション作成               | `client.connected` 完了後、新しいセッションが作成されたとき                                  |
| session.subscribed    | セッションのトピックサブスクライブ | サブスクライブ操作完了後                                                                       |
| session.unsubscribed  | セッションのサブスクライブ解除 | サブスクライブ解除操作完了後                                                                   |
| session.resumed       | セッション再開               | `client.connected` 実行時、古いセッション情報が正常に再開されたとき                           |
| session.discarded     | セッション破棄               | **discarded** によりセッションが終了した後                                                    |
| session.takenover     | セッション乗っ取り           | **takenover** によりセッションが終了した後                                                    |
| session.terminated    | セッション終了               | その他の理由でセッションが終了した後                                                          |
| message.publish       | メッセージパブリッシュ       | サーバーがメッセージをパブリッシュ（ルーティング）する前                                      |
| message.delivered     | メッセージ配信               | メッセージがクライアントに配信される直前                                                      |
| message.acked         | メッセージアック受信         | クライアントからメッセージの ACK を受信した後                                                 |
| message.dropped       | メッセージ破棄               | パブリッシュされたメッセージが破棄された後                                                    |

::: tip
- **セッション破棄（discarded）**：クライアントが `clean session` 方式でログインした際、サーバーに既にクライアントのセッションが存在する場合、古いセッションは破棄されます。
- **セッション乗っ取り（takenover）**：クライアントが `Reserved Session` 方式でログインした際、サーバーに既にクライアントのセッションが存在する場合、古いセッションは新しい接続に乗っ取られます。
:::

### フックとアンフック

EMQX はフックの登録と解除のための API を提供しています。

**フック登録（Hook）：**

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function, Args}: コールバック関数のモジュール、関数、追加引数
%% Priority：整数、デフォルトは0
emqx:hook(Name, {Module, Function, Args}, Priority).
```

フック登録後、コールバック関数は優先度順、同じ優先度の場合は登録順に実行されます。公式プラグインのフック登録はすべて優先度 `0` です。

**フック解除（Unhook）：**

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function}: コールバック関数のモジュールと関数
emqx:unhook(Name, {Module, Function}).
```

## コールバック関数

コールバック関数の入力パラメータと返り値は以下の表の通りです。

パラメータのデータ構造は [emqx_types.erl](https://github.com/emqx/emqx/tree/master/apps/emqx/src/emqx_types.erl) を参照してください。

| 名前                  | 入力パラメータ                                                                                   | 返り値              |
|-----------------------|------------------------------------------------------------------------------------------------|---------------------|
| client.connect        | `ConnInfo`：クライアント接続層パラメータ<br />`Props`：MQTT v5.0 接続パケットのプロパティ         | 新しい `Props`       |
| client.connack        | `ConnInfo`：クライアント接続層パラメータ<br />`Rc`：戻りコード<br />`Props`：MQTT v5.0 接続応答パケットのプロパティ | 新しい `Props`       |
| client.connected      | `ClientInfo`：クライアント情報パラメータ<br />`ConnInfo`：クライアント接続層パラメータ            | -                   |
| client.disconnected   | `ClientInfo`：クライアント情報パラメータ<br />`ConnInfo`：クライアント接続層パラメータ<br />`ReasonCode`：理由コード | -                   |
| client.authenticate   | `ClientInfo`：クライアント情報パラメータ<br />`AuthNResult`：認証結果                             | 新しい `AuthNResult` |
| client.post_authn     | `Context`：コンテキスト map `#{client_info := ClientInfo}`。マージされたクライアント情報（認証応答の `client_attrs` を含む）を保持 | 新しい `Context`、または `{error, Reason}` で接続を拒否（6.1.2 で追加） |
| client.authorize      | `ClientInfo`：クライアント情報パラメータ<br />`Topic`：パブリッシュ／サブスクライブトピック<br />`PubSub`：パブリッシュ／サブスクライブ区分<br />`AuthZResult`：認可結果 | 新しい `AuthZResult` |
| client.subscribe      | `ClientInfo`：クライアント情報パラメータ<br />`Props`：MQTT v5.0 サブスクライブメッセージのプロパティ<br />`TopicFilters`：サブスクライブトピックのリスト | 新しい `TopicFilters` |
| client.unsubscribe    | `ClientInfo`：クライアント情報パラメータ<br />`Props`：MQTT v5.0 サブスクライブ解除メッセージのプロパティ<br />`TopicFilters`：サブスクライブ解除トピックのリスト | 新しい `TopicFilters` |
| session.created       | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報                          | -                   |
| session.subscribed    | `ClientInfo`：クライアント情報パラメータ<br />`Topic`：サブスクライブトピック<br />`SubOpts`：サブスクライブ操作の設定オプション | -                   |
| session.unsubscribed  | `ClientInfo`：クライアント情報パラメータ<br />`Topic`：サブスクライブ解除トピック<br />`SubOpts`：サブスクライブ解除操作の設定オプション | -                   |
| session.resumed       | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報                          | -                   |
| session.discarded     | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報                          | -                   |
| session.takenover     | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報                          |                     |
| session.terminated    | `ClientInfo`：クライアント情報パラメータ<br />`Reason`：終了理由<br />`SessInfo`：セッション情報  | -                   |
| message.publish       | `Message`：メッセージオブジェクト                                                               | 新しい `Message`     |
| message.delivered     | `ClientInfo`：クライアント情報パラメータ<br />`Message`：メッセージオブジェクト                   | 新しい `Message`     |
| message.acked         | `ClientInfo`：クライアント情報パラメータ<br />`Message`：メッセージオブジェクト                   | -                   |
| message.dropped       | `Message`：メッセージオブジェクト<br />`By`：破棄者<br />`Reason`：破棄理由                       | -                   |

これらのフックの利用例は [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template) を参照してください。
