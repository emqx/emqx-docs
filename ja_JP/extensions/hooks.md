# Hooks

[Hooks](https://reactjs.org/docs/getting-started.html) は、クラスを書かずに状態管理やその他の React 機能を利用できる拡張機構です。

EMQX も Hooks をサポートしており、関数呼び出し、メッセージの受け渡し、モジュール間のイベント伝達をインターセプトすることで、システム機能の変更や拡張が可能です。

## 仕組み

システムに **Hooks** 機構が採用されていない場合、イベントの入力からハンドラー、結果に至るまでの一連のイベント処理フローは見えず、変更もできません。

しかし、処理の途中に HookPoint を設けて関数をマウントできるようにすると、外部プラグインが複数のコールバック関数をマウントして呼び出しチェーンを形成できます。これにより、内部のイベント処理を拡張・変更できます。

<img src="./assets/hooks_in_system.png" alt="Hooks-In-System" style="zoom:50%;" />

EMQX のいくつかの機能はこのフック機能を利用して実装されています。

1. フックシステムを利用してメッセージの多段階ストリーミング処理（エンコード／デコードなど）を行う
2. メッセージパブリッシュ時に設定に応じてメッセージをキャッシュする
3. フックのブロッキング機構を利用してメッセージパブリッシュを遅延させる

システムで一般的に使われる認証／認可もこのロジックに基づいて実装されています。例として[多言語拡張](./exhook.md)を挙げます。

`Built-in Database` 認証のみ有効な場合、上図のイベント処理ロジックに従うと、認証モジュールの処理は以下のようになります。

1. EMQX がユーザーの認証リクエスト（Authenticate）を受け取る
2. EMQX は `ClientInfo` とデフォルトの `AccIn` を引数に認証イベントのフックを実行する
```erlang
%% デフォルトの AccIn
{ok, #{is_superuser => false}}
```
3. `emqx_exhook` モジュールにコールバックし、この認証を有効と判断し、**allow, is_superuser** の結果を得る
```erlang
%% AuthNResult
{ok, #{is_superuser => true}}
```
4. **認証成功** を返し、クライアントはスーパーユーザーとしてシステムに正常にアクセスできるようになる

<img src="./assets/hooks_and_internal_model.png" alt="hooks_and_internal_model" style="zoom:50%;" />

このように、**Hooks** により EMQX の柔軟性は大幅に向上します。EMQX の挙動をカスタマイズしたい場合、コアコードを修正する必要はなく、EMQX が特定箇所に用意した **HookPoint** に関数をフックするだけで済みます。

この一連の処理で注意すべき点は以下の3つです。

1. **HookPoint** の場所：役割、実行タイミング、マウントおよびアンマウント方法
2. **コールバック関数** の実装：入力パラメータ数、役割、データ構造、戻り値の意味
3. コールバック関数の **チェーン上での実行機構**：実行順序、チェーンの途中での実行終了方法

拡張プラグイン開発で Hooks を使う場合は、これら3点を十分理解し、**フック内でシステムのスループットに影響を与えるブロッキング関数の使用は避ける**ようにしてください。

## コールバック関数チェーン

単一の **HookPoint** に複数のプラグインがイベントに関心を持ち、対応処理を行う場合があります。そのため、各 **HookPoint** には複数のコールバック関数が存在することがあります。

これら複数のコールバック関数が順次実行される連鎖を **コールバック関数チェーン** と呼びます。

**コールバック関数チェーン** は現在、[Chain-of-Responsibility](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern) パターンの概念に基づいて実装されています。フックの機能性と柔軟性を満たすために、以下の属性を持ちます。

- **順序付けられている**：チェーン上のコールバック関数は一定の順序で実行される必要があります。
- **入力パラメータ**：初期化パラメータが1つ以上あり、オプションでチェーン内で修正される累積値があります。
- **出力結果**：チェーン内の各関数は出力を持ち、実行結果を気にしないコールバック関数は `ok` を返すべきです。例として、通知系イベント（「クライアントが正常にログインした」など）は戻り値を必要としません。
- **伝達性**：チェーン内のコールバック関数の結果は伝達されます。フックの柔軟性を高めるために、チェーン内のコールバック関数の戻り値処理には **2つのモード** を設計しています。
  - **結果伝達モード**<br />
    チェーンの各コールバック関数はチェーンの入力値と、前の関数の戻り値（累積値として解釈可能）を引数に受け取ります。最後の関数の戻り値がチェーン全体の戻り値となります。チェーン呼び出し時に初期累積値を渡し、最初の関数が利用します。
  - **結果透過モード**<br />
    チェーンの各関数はチェーンの入力値のみを気にし、前の関数の戻り値は無視します。チェーンの戻り値は固定で `ok` となります。<br />
    これは実質的に前述の **結果伝達モード** の特殊ケースであり、初期累積値が `ok` で、チェーン上の各関数が入力値のみを参照し累積値を `ok` のまま維持する形です。<br />
    通知系イベントの多くはこのロジックに従います。これにより一般的な **コールバック関数チェーン** 実行モジュールを提供しています。
- **コールバック関数チェーン** は、チェーンの途中で *実行を終了* したり、*この操作を無視* したりできる必要があります。
  - **途中終了**：この関数の実行完了後、チェーンの実行を直ちに終了し、以降のコールバック関数はすべて無視されます。<br />例として、ある認証プラグインがログイン許可を出した場合、他の認証プラグインのチェックは不要なので途中終了させます。
  - **操作無視**：チェーン上の処理結果を変更せず、そのまま次のコールバック関数に渡します。<br />
    例えば複数の認証プラグインが存在し、あるプラグインが対象外のクライアントと判断した場合、認証結果を変更せずにそのまま次に渡します。

以上より、チェーン上のコールバック関数の戻り値処理方法に応じて、2種類のプログラムフロー図が得られます。

### 結果伝達モード
<img src="./assets/hooks_return_value.png" alt="hooks_return_value" style="zoom:50%;" />

図の意味は以下の通りです。

1. 図中には3つのコールバック関数 `Fun1`、`Fun2`、`Fun3` が登録されており、示された順に実行されます。
2. コールバック関数の実行順序は優先度で決まり、同じ優先度の場合はマウント順に実行されます。
3. チェーンの入力パラメータは読み取り専用の `Args` と、関数が修正可能な `InitAcc` です。
4. チェーンの実行が途中終了しても、常に戻り値を返します。戻り値の形式は以下の通りです。
   - コールバック関数の戻り値が
     - `ok`：この操作を無視し、読み取り専用の `Args` と前関数の戻り値 `Acc` を使ってチェーンを続行
     - `{ok, NewAcc}`：何らかの処理を行い、`Acc` の内容を修正し、新しい `NewAcc` を使ってチェーンを続行
   - また、コールバック関数は
     - `stop`：チェーンの伝達を停止し、直前の関数の `Acc` を即座に返す
     - `{stop, NewAcc}`：チェーンの伝達を停止し、この関数の修正した `NewAcc` を即座に返す

### 結果透過モード
<img src="./assets/hooks_multiple_value.png" alt="hooks_multiple_value" style="zoom:50%;" />

このモードは、前述の結果伝達モードの特殊ケースです。

初期の `InitAcc` が `ok` であり、チェーン上の各コールバック関数が `ok | {ok, ok} | stop | {stop, ok}` を返す場合に相当します。

以上がコールバック関数チェーンの主要な設計思想であり、フック上のコールバック関数の実行ロジックを規定しています。

以下の [HookPoint](#hookpoint) と [callback function](#callback) の2節では、フックに関するすべての操作は [emqx](https://github.com/emqx/emqx) が提供する Erlang コードレベルの API に依存していることを説明します。これらがフックロジック実装の基盤です。

- 他言語でのフック利用については、[Extension Hook](./exhook.md) を参照してください。

## HookPoint 一覧

EMQX はクライアントのライフサイクルにおける主要な活動に基づき、多数の **HookPoint** をあらかじめ用意しています。システムにプリセットされたマウントポイントは以下の通りです。

| 名称                 | 説明                         | 実行タイミング                                                                              |
|----------------------|------------------------------|--------------------------------------------------------------------------------------------|
| client.connect       | 接続パケットの処理           | サーバーがクライアントから接続パケットを受信したとき                                    |
| client.connack       | 接続応答の発行               | サーバーが接続応答メッセージを発行する準備ができたとき                                  |
| client.connected     | 接続成功                     | クライアント認証が完了し、正常にシステムに接続された後                                  |
| client.disconnected  | 切断                         | クライアントの接続層がクローズ準備完了したとき                                          |
| client.authenticate  | 接続認証                     | `client.connect` 実行後                                                                    |
| client.post_authn    | 認証後の書き換え             | `client.authenticate` の認証チェーン完了後（6.1.2 で追加）                               |
| client.authorize     | Pub/Sub 認可                 | `publish/subscribe` 操作実行前                                                            |
| client.subscribe     | トピックのサブスクライブ     | サブスクライブメッセージ受信後、`client.authorize` 実行前                                |
| client.unsubscribe   | サブスクライブ解除           | アン・サブスクライブパケット受信後                                                        |
| session.created      | セッション作成               | `client.connected` 完了後、新しいセッションが作成されたとき                              |
| session.subscribed   | セッションのトピック購読     | サブスクライブ操作完了後                                                                  |
| session.unsubscribed | セッションのトピック購読解除 | アン・サブスクライブ操作完了後                                                            |
| session.resumed      | セッション再開               | `client.connected` 実行時、古いセッション情報が正常に再開されたとき                      |
| session.discarded    | セッション破棄               | セッションが **discarded** により終了した後                                              |
| session.takenover    | セッション引き継ぎ           | セッションが **takenover** により終了した後                                              |
| session.terminated   | セッション終了               | その他の理由でセッションが終了した後                                                    |
| message.publish      | メッセージパブリッシュ       | サーバーがメッセージをパブリッシュ（ルーティング）する前                                |
| message.delivered    | メッセージ配信               | メッセージがクライアントに配信される直前                                                |
| message.acked        | メッセージアック             | クライアントからメッセージの ACK を受信した後                                            |
| message.dropped      | メッセージ破棄               | パブリッシュされたメッセージが破棄された後                                              |


::: tip
- **セッション破棄（discarded）**：クライアントが `clean session` 方式でログインした場合、サーバーに既存のセッションがあれば古いセッションは破棄されます。
- **セッション引き継ぎ（takenover）**：クライアントが `Reserved Session` 方式でログインした場合、サーバーに既存のセッションがあれば新しい接続に引き継がれます。
:::

### Hook と Unhook

EMQX はフックとアンフックの操作用 API を提供しています。

**Hook:**

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function, Args}: コールバック関数のモジュール、関数、追加パラメータ
%% Priority：整数、デフォルトは 0
emqx:hook(Name, {Module, Function, Args}, Priority).
```

フック完了後、コールバック関数は優先度順、同じ優先度の場合はフック順に実行されます。公式プラグインのフックはすべて優先度 `0` です。

**Unhook**：

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function}: コールバック関数のモジュールと関数
emqx:unhook(Name, {Module, Function}).
```

## コールバック関数

コールバック関数の入力パラメータと戻り値は以下の表の通りです。

パラメータのデータ構造は [emqx_types.erl](https://github.com/emqx/emqx/tree/master/apps/emqx/src/emqx_types.erl) を参照してください。

| 名称                 | 入力パラメータ                                               | 戻り値             |
| -------------------- | ------------------------------------------------------------ | ------------------ |
| client.connect       | `ConnInfo`：クライアント接続層パラメータ<br />`Props`：MQTT v5.0 接続パケットのプロパティ | 新しい `Props`     |
| client.connack       | `ConnInfo`：クライアント接続層パラメータ<br />`Rc`：戻りコード<br />`Props`：MQTT v5.0 接続応答パケットのプロパティ | 新しい `Props`     |
| client.connected     | `ClientInfo`：クライアント情報パラメータ<br />`ConnInfo`：クライアント接続層パラメータ | -                  |
| client.disconnected  | `ClientInfo`：クライアント情報パラメータ<br />`ConnInfo`：クライアント接続層パラメータ<br />`ReasonCode`：理由コード | -                  |
| client.authenticate  | `ClientInfo`：クライアント情報パラメータ<br />`AuthNResult`：認証結果 | 新しい `AuthNResult` |
| client.post_authn    | `Context`：マップ `#{client_info := ClientInfo}`（認証レスポンスの `client_attrs` を含む統合クライアント情報） | 新しい `Context` または 拒否時は `{error, Reason}`（6.1.2 で追加） |
| client.authorize     | `ClientInfo`：クライアント情報パラメータ<br />`Topic`：パブリッシュ／サブスクライブトピック<br />`PubSub`：パブリッシュ／サブスクライブ<br />`AuthZResult`：認可結果 | 新しい `AuthZResult` |
| client.subscribe     | `ClientInfo`：クライアント情報パラメータ<br />`Props`：MQTT v5.0 サブスクライブメッセージのプロパティ<br />`TopicFilters`：サブスクライブトピックのリスト | 新しい `TopicFilters` |
| client.unsubscribe   | `ClientInfo`：クライアント情報パラメータ<br />`Props`：MQTT v5.0 アン・サブスクライブメッセージのプロパティ<br />`TopicFilters`：アン・サブスクライブトピックのリスト | 新しい `TopicFilters` |
| session.created      | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報 | -                  |
| session.subscribed   | `ClientInfo`：クライアント情報パラメータ<br />`Topic`：サブスクライブトピック<br />`SubOpts`：サブスクライブ操作の設定オプション | -                  |
| session.unsubscribed | `ClientInfo`：クライアント情報パラメータ<br />`Topic`：アン・サブスクライブトピック<br />`SubOpts`：アン・サブスクライブ操作の設定オプション | -                  |
| session.resumed      | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報 | -                  |
| session.discarded    | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報 | -                  |
| session.takenover    | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報 |                    |
| session.terminated   | `ClientInfo`：クライアント情報パラメータ<br />`Reason`：終了理由<br />`SessInfo`：セッション情報 | -                  |
| message.publish      | `Message`：メッセージオブジェクト                           | 新しい `Message`   |
| message.delivered    | `ClientInfo`：クライアント情報パラメータ<br />`Message`：メッセージオブジェクト | 新しい `Message`   |
| message.acked        | `ClientInfo`：クライアント情報パラメータ<br />`Message`：メッセージオブジェクト | -                  |
| message.dropped      | `Message`：メッセージオブジェクト<br />`By`：破棄者<br />`Reason`：破棄理由 | -                  |

これらのフックの活用例は [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template) を参照してください。
