# Hooks

[Hooks](https://reactjs.org/docs/getting-started.html) は、クラスを書かずに状態やその他の React 機能を使える拡張機構です。

EMQX も Hooks をサポートしており、関数呼び出し、メッセージの送受信、モジュール間のイベント伝達をインターセプトすることで、システム機能の変更や拡張を可能にしています。

## 動作原理

システムが **Hooks** 機構を採用していない場合、イベントの入力からハンドラーおよび結果に至るまでの一連のイベント処理フローは見えず、変更もできません。

しかし、処理の途中に HookPoint を設けて関数をマウントできるようにすると、外部プラグインは複数のコールバック関数をマウントして呼び出しチェーンを形成できます。これにより、内部のイベント処理を拡張・変更可能になります。

<img src="./assets/hooks_in_system.png" alt="Hooks-In-System" style="zoom:50%;" />

EMQX のいくつかの機能は、このフック機能を使って実装されています。

1. フックシステムを使ったメッセージの多段階ストリーミング処理（エンコード／デコードなど）
2. 設定に応じたメッセージパブリッシュ時のメッセージキャッシュ
3. フックのブロッキング機構を使ったメッセージパブリッシュの遅延

システムで一般的に使われる認証／認可もこのロジックに基づいて実装されています。例として [多言語拡張](./exhook.md) を挙げます。

`Built-in Database` 認証のみが有効な場合、イベントの処理ロジック（上図参照）に従うと、認証モジュールのロジックは以下のようになります。

1. EMQX がユーザーの認証リクエスト（Authenticate）を受信する
2. EMQX は `ClientInfo` とデフォルトの `AccIn` を使って認証イベントのフックを実行する
```erlang
%% デフォルトの AccIn
{ok, #{is_superuser => false}}
```
3. `emqx_exhook` モジュールにコールバックし、この認証が有効と判断し、**allow, is_superuser** の結果を得る
```erlang
%% AuthNResult
{ok, #{is_superuser => true}}
```
4. **認証成功** を返し、クライアントはスーパーユーザーとしてシステムに正常にアクセスできるようになる

<img src="./assets/hooks_and_internal_model.png" alt="hooks_and_internal_model" style="zoom:50%;" />

このように、**Hooks** は EMQX の柔軟性を大幅に高めます。EMQX の動作をカスタマイズしたい場合、コアコードを変更する必要はなく、EMQX が特定箇所に提供する **HookPoint** に関数をフックするだけで済みます。

この一連の処理で注意すべき点は以下の3つです。

1. **HookPoint** の場所：役割、実行タイミング、マウントおよびアンマウント方法
2. **コールバック関数** の実装：入力パラメータ数、役割、データ構造、返り値の意味
3. **チェーン上でのコールバック関数実行の仕組み**：実行順序、チェーンの途中での実行終了方法

拡張プラグインの開発で Hooks を使う場合は、これら3点を十分理解し、**システムのスループットに影響を与えるため、フック内でブロッキング関数を使わないようにしてください。**

## コールバック関数チェーン

1つの **HookPoint** に対して複数のプラグインがイベントに関心を持ち、対応する処理を行う場合があるため、各 **HookPoint** には複数のコールバック関数が存在することがあります。

これら複数のコールバック関数が順次実行されるチェーンを **コールバック関数チェーン** と呼びます。

**コールバック関数チェーン** は現在、[責任連鎖パターン (Chain-of-Responsibility)](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern) の概念に基づいて実装されています。フックの機能性と柔軟性を満たすため、以下の特徴を持ちます。

- **順序付けられている**：チェーン上のコールバック関数は一定の順序で実行される必要があります。
- **入力パラメータ**：初期化パラメータが1つ以上必須で、オプションでコールバック関数チェーンによって修正される累積値を持ちます。
- **出力結果**：チェーン内の各関数は出力を持ち、実行結果を気にしないコールバック関数は `ok` を返すべきです。例えば、通知系イベントで「クライアントが正常にログインした」場合は返り値を必要としません。
- **伝達性**：チェーン内のコールバック関数の結果は伝達されます。フックの柔軟性を高めるため、チェーン内のコールバック関数の返り値処理には**2つのモード**を設計しています。
  - **結果伝達モード**<br />
    チェーン内の各コールバック関数はチェーンの入力値と前の関数の返り値（累積値と解釈可能）を引数として受け取ります。最後の関数の返り値がチェーン全体の返り値となります。チェーン呼び出し時には、最初のコールバック関数が使う累積値の初期値を渡します。
  - **結果透過モード**<br />
    チェーン内の各関数はチェーンの入力値のみを気にし、前の関数の返り値は無視します。チェーンの返り値は常に `ok` となります。<br />
    これは実質的に前述の**結果伝達モード**の特殊ケースであり、初期累積値が `ok` で、チェーン内の各関数が入力値のみを気にして累積値を `ok` のまま維持する形です。<br />
    通知系イベントの多くはこのロジックに従うため、汎用の **コールバック関数チェーン** 実行モジュールを提供しています。
- **コールバック関数チェーン** は、チェーン内の関数が*処理を途中で終了*したり*処理を無視*したりできる必要があります。
  - **途中終了**：この関数の実行が完了した後、チェーンの実行を直ちに終了し、以降のコールバック関数はすべて無視されます。<br />例として、認証が許可したクライアントについては他の認証プラグインをチェックする必要がないため、途中終了させます。
  - **無視**：チェーンの処理結果を変更せずに次のコールバック関数に渡します。<br />
    例えば複数の認証プラグインがある場合、ある認証プラグインが該当クライアントを認証範囲外と判断し、認証結果を変更しない場合はこの操作を無視し、前の関数の返り値をそのまま次に渡します。

以上より、チェーン上のコールバック関数の返り値処理の2通りに基づくプログラムフロー図を示します。

### 結果伝達モード
<img src="./assets/hooks_return_value.png" alt="hooks_return_value" style="zoom:50%;" />

図の意味は以下の通りです。

1. 図では3つのコールバック関数 `Fun1`、`Fun2`、`Fun3` が登録されており、示された順に実行されます。
2. コールバック関数の実行順序は優先度で決まり、同じ優先度の場合はマウント順に実行されます。
3. チェーンの入力パラメータは読み取り専用の `Args` と、チェーン内で修正される `InitAcc` です。
4. チェーンの実行が途中終了しても、必ず返り値を返します。返り値の形式は以下の通りです。
   - コールバック関数の返り値：
     - `ok`：この操作を無視し、読み取り専用の `Args` と前の関数の返した `Acc` でチェーンを続行
     - `{ok, NewAcc}`：何らかの処理を行い、`Acc` の内容を修正して、`Args` と新しい `NewAcc` でチェーンを続行
   - コールバック関数の返り値：
     - `stop`：チェーンの伝達を停止し、前の関数の `Acc` を即座に返す
     - `{stop, NewAcc}`：チェーンの伝達を停止し、この修正済みの `NewAcc` を即座に返す

### 結果透過モード
<img src="./assets/hooks_multiple_value.png" alt="hooks_multiple_value" style="zoom:50%;" />

この実行モードは、返り値を無視するモードであり、結果伝達モードの特殊ケースです。

初期累積値 `InitAcc` が `ok` で、チェーンにマウントされた各コールバック関数が `ok | {ok, ok} | stop | {stop, ok}` を返す場合に相当します。

以上がコールバック関数チェーンの主な設計思想であり、フック上のコールバック関数の実行ロジックを規定しています。

以下の [HookPoint](#hookpoint) と [コールバック関数](#callback) の2節では、すべてのフック操作は [emqx](https://github.com/emqx/emqx) が提供する Erlang コードレベルの API に依存しており、これがフックロジック全体の実装基盤となっています。

- 他言語でのフック利用は、[Extension Hook](./exhook.md) を参照してください。

## HookPoint 一覧

EMQX はクライアントのライフサイクルにおける主要な活動に基づき、多数の **HookPoint** をプリセットしています。システムにプリセットされたマウントポイントは以下の通りです。

| 名称                 | 説明                         | 実行タイミング                                                                             |
|----------------------|------------------------------|-------------------------------------------------------------------------------------------|
| client.connect       | 接続パケットの処理           | サーバーがクライアントから接続パケットを受信したとき                                   |
| client.connack       | 接続応答の発行               | サーバーが接続応答メッセージを発行する準備ができたとき                                 |
| client.connected     | 接続成功                     | クライアント認証が完了し、正常にシステムに接続された後                                 |
| client.disconnected  | 切断                         | クライアントの接続層が閉じる準備ができたとき                                           |
| client.authenticate  | 接続認証                     | `client.connect` 実行後                                                                   |
| client.post_authn    | 認証後の書き換え             | `client.authenticate` の認証チェーン完了後（6.1.2 で追加）                              |
| client.authorize     | Pub/Sub 認可                 | `publish/subscribe` 操作実行前                                                           |
| client.subscribe     | トピックのサブスクライブ     | サブスクリプションメッセージ受信後、`client.authorize` 実行前                           |
| client.unsubscribe   | サブスクライブ解除           | サブスクライブ解除パケット受信後                                                         |
| session.created      | セッション作成               | `client.connected` 完了後、新しいセッションが作成されたとき                             |
| session.subscribed   | セッションのサブスクライブ   | サブスクライブ操作完了後                                                                 |
| session.unsubscribed | セッションのサブスクライブ解除 | サブスクライブ解除操作完了後                                                             |
| session.resumed      | セッション再開               | `client.connected` 実行時に古いセッション情報が正常に再開されたとき                     |
| session.discarded    | セッション破棄               | セッションが **discarded** により終了した後                                             |
| session.takenover    | セッション引き継ぎ           | セッションが **takenover** により終了した後                                             |
| session.terminated   | セッション終了               | その他の理由でセッションが終了した後                                                   |
| message.publish      | メッセージパブリッシュ       | サーバーがメッセージをパブリッシュ（ルーティング）する前                               |
| message.delivered    | メッセージ配信               | メッセージがクライアントに配信される直前                                               |
| message.acked        | メッセージアック             | クライアントからメッセージの ACK を受信した後                                           |
| message.dropped      | メッセージ破棄               | パブリッシュされたメッセージが破棄された後                                             |

::: tip
- **セッション破棄 (discarded)：** クライアントが `clean session` モードでログインした場合、サーバーに既存のセッションがあれば古いセッションは破棄されます。
- **セッション引き継ぎ (takenover)：** クライアントが `Reserved Session` モードでログインした場合、サーバーに既存のセッションがあれば新しい接続により古いセッションが引き継がれます。
:::

### Hook と Unhook

EMQX はフックおよびアンフック操作のための API を提供しています。

**Hook:**

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function, Args}: コールバック関数のモジュール、関数、追加パラメータ
%% Priority：整数、デフォルトは0
emqx:hook(Name, {Module, Function, Args}, Priority).
```

フック完了後、コールバック関数は優先度順、同じ優先度の場合はフックされた順に実行されます。公式プラグインのマウントフックはすべて優先度 `0` です。

**Unhook：**

```erlang
%% Name: フック名（フックポイント）、例：'client.authenticate'
%% {Module, Function}: コールバック関数のモジュールと関数
emqx:unhook(Name, {Module, Function}).
```

## コールバック関数

コールバック関数の入力パラメータと返り値は以下の表の通りです。

パラメータのデータ構造は [emqx_types.erl](https://github.com/emqx/emqx/tree/master/apps/emqx/src/emqx_types.erl) を参照してください。

| 名称                 | 入力パラメータ                                               | 返り値             |
| -------------------- | ------------------------------------------------------------ | ------------------ |
| client.connect       | `ConnInfo`：クライアント接続層パラメータ<br />`Props`：MQTT v5.0 接続パケットのプロパティ | 新しい `Props`     |
| client.connack       | `ConnInfo`：クライアント接続層パラメータ<br />`Rc`：戻りコード<br />`Props`：MQTT v5.0 接続応答パケットのプロパティ | 新しい `Props`     |
| client.connected     | `ClientInfo`：クライアント情報パラメータ<br />`ConnInfo`：クライアント接続層パラメータ | -                  |
| client.disconnected  | `ClientInfo`：クライアント情報パラメータ<br />`ConnInfo`：クライアント接続層パラメータ<br />`ReasonCode`：理由コード | -                  |
| client.authenticate  | `ClientInfo`：クライアント情報パラメータ<br />`AuthNResult`：認証結果 | 新しい `AuthNResult` |
| client.post_authn    | `Context`：マップ `#{client_info := ClientInfo}`（認証応答の `client_attrs` を含む結合済みクライアント情報） | 新しい `Context` または拒否時は `{error, Reason}`（6.1.2 で追加） |
| client.authorize     | `ClientInfo`：クライアント情報パラメータ<br />`Topic`：パブリッシュ／サブスクライブトピック<br />`PubSub`：パブリッシュ／サブスクライブ<br />`AuthZResult`：認可結果 | 新しい `AuthZResult` |
| client.subscribe     | `ClientInfo`：クライアント情報パラメータ<br />`Props`：MQTT v5.0 サブスクライブメッセージのプロパティ<br />`TopicFilters`：サブスクライブするトピックのリスト | 新しい `TopicFilters` |
| client.unsubscribe   | `ClientInfo`：クライアント情報パラメータ<br />`Props`：MQTT v5.0 サブスクライブ解除メッセージのプロパティ<br />`TopicFilters`：サブスクライブ解除するトピックのリスト | 新しい `TopicFilters` |
| session.created      | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報 | -                  |
| session.subscribed   | `ClientInfo`：クライアント情報パラメータ<br />`Topic`：サブスクライブされたトピック<br />`SubOpts`：サブスクライブ操作の設定オプション | -                  |
| session.unsubscribed | `ClientInfo`：クライアント情報パラメータ<br />`Topic`：サブスクライブ解除されたトピック<br />`SubOpts`：サブスクライブ解除操作の設定オプション | -                  |
| session.resumed      | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報 | -                  |
| session.discarded    | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報 | -                  |
| session.takenover    | `ClientInfo`：クライアント情報パラメータ<br />`SessInfo`：セッション情報 | -                  |
| session.terminated   | `ClientInfo`：クライアント情報パラメータ<br />`Reason`：終了理由<br />`SessInfo`：セッション情報 | -                  |
| message.publish      | `Message`：メッセージオブジェクト                           | 新しい `Message`   |
| message.delivered    | `ClientInfo`：クライアント情報パラメータ<br />`Message`：メッセージオブジェクト | 新しい `Message`   |
| message.acked        | `ClientInfo`：クライアント情報パラメータ<br />`Message`：メッセージオブジェクト | -                  |
| message.dropped      | `Message`：メッセージオブジェクト<br />`By`：破棄者<br />`Reason`：破棄理由 | -                  |

これらのフックの利用例は [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template) を参照してください。
