# EMQXプラグインの開発

このページでは、EMQXモノレポ外でカスタムEMQXプラグインを開発する手順を説明します。

EMQX公式プラグインは通常、EMQXモノレポ内で開発されます。詳細は[EMQXプラグイン開発ガイド](https://github.com/emqx/emqx/blob/release-60/PLUGIN.md)をご覧ください。

## 前提条件

開始する前に、以下を準備してください。

- EMQXの[フック](./hooks.md)の知識
- `make`を含む動作するビルド環境（例：`build_essential`）
- [rebar3](https://www.rebar3.org/)
- 対象とするEMQXリリースと同じメジャーバージョンのErlang/OTP。詳細はDockerの`org.opencontainers.image.otp.version`属性や使用バージョンの`.tool-versions`ファイル（例：https://github.com/emqx/emqx/blob/e5.9.0-beta.4/.tool-versions）を参照してください。Erlang/OTPのバージョン管理には[ASDF](https://asdf-vm.com/)の使用を推奨します。あるいは、[こちらのコマンド](https://github.com/emqx/emqx-builder/blob/main/show-latest-images.sh)を実行してemqx-builderイメージを取得できます。

## スタンドアロンプラグイン開発

スタンドアロンプラグイン開発には2つのスタイルがあります。

- **rebar3テンプレート**： [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)を使ってプラグインプロジェクトを生成します。このスタイルは`rebar3`のみを使用します。
- **Gitサブモジュールスタイル**： EMQX 6.0以降、プラグインを独自のリポジトリに保持し、EMQXをGitサブモジュールとして追加し、モノレポのツールでビルドします。

### Gitサブモジュールスタイル

プラグインを別リポジトリに保持し続ける必要がある場合（例：ソースコードが非公開だがEMQXモノレポのツールでビルドしたい場合）にこのスタイルを使用します。

1. EMQXをサブモジュールとして追加します。

   ```bash
   git submodule add --depth 1 git@github.com:emqx/emqx.git emqx
   ```

2. 対象バージョンに対応するEMQXブランチをチェックアウトします（例：EMQX 6.0なら`release-60`）。

3. プラグインリポジトリをサブモジュールの`plugins/`ディレクトリにシンボリックリンクします。

   ```bash
   ln -s ../.. emqx/plugins/{plugin_name}
   ```

4. EMQXサブモジュールからプラグインパッケージをビルドします。

   ```bash
   cd emqx
   make plugin-{plugin_name}
   ```

`.tar.gz`アーティファクトは`emqx/_build/plugins/`以下に生成されます。

`rebar3`テンプレートスタイルの場合は、以下の手順に進んでください。

### プラグインテンプレートのインストール

EMQXはカスタムプラグイン作成を簡単にするために[emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)を提供しています。新しいプラグインを作成するには、`rebar3`テンプレートとして`emqx-plugin-template`をインストールします。

Linux環境では以下のコマンドで`emqx-plugin-template`をダウンロードします。

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template
$ popd
```

::: tip

`REBAR_CACHE_DIR`環境変数が設定されている場合、テンプレートディレクトリは`$REBAR_CACHE_DIR/.config/rebar3/templates`になります。関連Issueは[こちら](https://github.com/erlang/rebar3/issues/2762)です。

:::

インストールを確認するには以下を実行します。

```shell
$ rebar3 new help
```

出力に`emqx-plugin (custom)`がテンプレートとして表示されれば成功です。

### プラグインスケルトンの生成

インストールしたテンプレートを使って新しいプラグインプロジェクトを生成します。

```shell
$ rebar3 new emqx-plugin my_emqx_plugin
```

これにより、`my_emqx_plugin`ディレクトリに動作するスケルトンが作成されます。

### ディレクトリ構成

`rebar3 new emqx-plugin`コマンドは、`emqx`を依存に含む標準的なErlangアプリケーションを以下のように作成します。

```shell
my_emqx_plugin
├── LICENSE
├── Makefile
├── README.md
├── erlang_ls.config
├── priv
│   ├── config.hocon.example
│   ├── config_i18n.json.example
│   ├── config_schema.avsc.enterprise.example
│   └── config_schema.avsc.example
├── rebar.config
├── scripts
│   ├── ensure-rebar3.sh
│   └── get-otp-vsn.sh
└── src
    ├── my_emqx_plugin_app.erl
    ├── my_emqx_plugin.app.src
    ├── my_emqx_plugin_cli.erl
    ├── my_emqx_plugin.erl
    └── my_emqx_plugin_sup.erl
```

- `src`：プラグインのOTPアプリケーションのコードを含みます。
- `priv`：プラグインの設定ファイルやスキーマ（例ファイル付き）を格納します。
- `rebar.config`：アプリケーションのビルドとリリースパッケージ化に使う`rebar3`設定ファイルです。
- `Makefile`：プラグインのビルドのエントリポイントです。
- `scripts`：`Makefile`の補助スクリプトです。**注意：** テンプレートは`emqx`に依存しているため、カスタム版`rebar3`が必要で、同梱の`./scripts/ensure-rebar3.sh`でインストールできます。
- `README.md`：ドキュメントのプレースホルダーです。
- `LICENSE`：プラグインのサンプルライセンスファイルです。

#### 設定ファイル`rebar.config`の理解

`rebar.config`はプラグインのビルドとリリースパッケージ化に使います。内容を確認し、プラグインの要件に応じて調整してください。

重要なセクションは以下です。

- 依存関係（`deps`）セクション
- リリースセクション（`relx`）
- プラグイン説明（`emqx_plugin`）セクション

`deps`セクションでは、プラグインが依存する他のOTPアプリケーションを追加できます。

```
{deps,
    [
        ...
        %% これはプラグインの依存関係
        {map_sets, "1.1.0"}
    ]}.
```

テンプレートは`map_sets`を1つの依存関係として追加しています。不要なら削除可能です。依存関係の詳細は[`rebar3`依存関係ドキュメント](https://www.rebar3.org/docs/configuration/dependencies/)を参照してください。

`relx`セクションでは、リリース名とバージョン、リリースに含めるアプリケーションのリストを指定します。

```
{relx, [ {release, {my_emqx_plugin, "1.0.0"},
            [ my_emqx_plugin
            , map_sets
            ]}
       ...
       ]}.
```

通常、`deps`セクションのランタイム依存アプリケーションをリリースに追加します。

リリース名とバージョンは、プラグインがEMQXにインストールされた際の識別に使われます。APIやCLIでプラグインを指定する一意の識別子（例：`my_emqx_plugin-1.0.0`）を形成します。

プラグイン説明セクションでは、プラグインの追加情報を指定します。

```
{emqx_plugrel,
  [ {authors, ["Your Name"]}
  , {builder,
      [ {name, "Your Name"}
      , {contact, "your_email@example.com"}
      , {website, "http://example.com"}
      ]}
  , {repo, "https://github.com/emqx/emqx-plugin-template"}
  , {functionality, ["Demo"]}
  , {compatibility,
      [ {emqx, "~> 5.0"}
      ]}
  , {description, "Another amazing EMQX plugin"}
  ]
}
```

#### `src`ディレクトリの概要

`src`ディレクトリにはプラグインのOTPアプリケーションのコードが含まれます。

##### `my_emqx_plugin.app.src`

標準的なErlangアプリケーション記述ファイルで、リリース時に`my_emqx_plugin.app`にコンパイルされます。

- アプリケーションのバージョンはリリースバージョンと一致する必要はなく、別のバージョニング方式を使えます。
- `applications`セクションに注意してください。プラグインはOTPアプリケーションとしてビルドされるため、プラグインの起動・停止・再起動はこのOTPアプリケーションの操作と同じです。プラグインが他のアプリケーションに依存する場合は、必ず`applications`セクションに記載してください。

##### `my_emqx_plugin_app.erl`

プラグインのアプリケーションの開始・停止を担当する[`application`ビヘイビア](https://www.erlang.org/doc/man/application.html)（`start/2`と`stop/1`関数）を実装するメインモジュールです。

`start/2`関数でよく行う処理は以下です。

- EMQXのフックポイントにフックする
- CLIコマンドを登録する
- 監督ツリーを起動する

オプションで、`_app.erl`モジュールは`on_config_changed/2`と`on_health_check/1`のコールバック関数を実装できます。

- `on_config_changed/2`は、ダッシュボード、API、CLI経由でプラグインの設定が変更されたときに呼ばれます。
- `on_health_check/1`はプラグインの状態が要求されたときに呼ばれ、プラグインはこの関数から状態を報告できます。

#### その他のファイル

`my_emqx_plugin_cli.erl`モジュールはプラグインのCLIコマンドを実装します。登録されると、`emqx ctl`コマンド経由で呼び出されます。

`my_emqx_plugin_sup.erl`はプラグインの一般的なスーパーバイザーを実装します。

`my_emqx_plugin.erl`はプラグインのメインモジュールで、プラグインのロジックを実装します。スケルトンでは簡単なログを出すデモ用フックをいくつか実装しています。必要に応じて他のモジュールを追加できます。

::: tip 注意

アプリケーションモジュールやファイル名は任意ですが、以下の要件があります。

- アプリケーション名はプラグイン名と同じであること
- アプリケーションモジュール（`_app`）は`{plugin_name}_app`という名前であること
  :::

#### `priv`ディレクトリの概要

`priv`ディレクトリにはプラグインの設定ファイルやスキーマが格納されます。

##### `config.hocon`

プラグインの初期設定を[HOCON形式](https://github.com/lightbend/config/blob/master/HOCON.md)で記述したファイルです。`config.hocon.example`を参照用に利用できます。

##### `config_schema.avsc`

プラグイン設定のスキーマを[Avro形式](https://avro.apache.org/docs/1.11.1/specification/)で定義したファイルです。存在する場合、EMQXは設定更新時にこのスキーマに対してバリデーションを行います。`config.hocon`がスキーマに準拠しない場合、リリースビルドは失敗します。

さらに、このファイルはUIヒントを含めることができ、EMQXダッシュボードでの対話的な設定を可能にします。参考に`config_schema.avsc.enterprise.example`を参照してください。

##### `config_i18n.json`

プラグイン設定UIの翻訳をJSON形式で含むファイルです。例：

```
{
  "$key": {
    "zh": "中文翻译",
    "en": "English translation"
  },
  ...
}
```

翻訳は`config_schema.avsc`のUIヒントで参照されます。詳細は`config_i18n.json.example`と`config_schema.avsc.enterprise.example`を参照してください。

### プラグインの実装

スケルトンが準備できたら、プラグインのロジックを実装します。一般的に必要なロジックは以下です。

- フックとCLIコマンドの実装
- 設定更新の処理
- ヘルスチェックの処理

### フックとCLIコマンドの実装

EMQXは様々なイベントに対するフックポイントを定義しています。任意のアプリケーション（プラグインを含む）はこれらのフックポイントにコールバックを登録し、イベントに反応したりデフォルト動作を変更したりできます。

よく使われるフックポイントはスケルトンファイルに用意されています。フックポイントの一覧、引数、期待される戻り値は[EMQXコード](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl)で確認できます。

フックポイントにコールバックを登録するには`emqx_hooks:add/3`を使います。以下のパラメータを指定します。

- フックポイント名
- コールバックモジュールと関数、必要に応じてEMQXが渡す追加引数
- コールバックの優先度（通常は`?HP_HIGHEST`で最優先に呼ばれます）

コールバックの登録解除は`emqx_hooks:del/2`を使い、フックポイント名とコールバックモジュール/関数を指定します。

例として、`client.authenticate`と`client.authorize`フックポイントの登録・解除：

```
-module(my_emqx_plugin).
...
hook() ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, []}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, []}, ?HP_HIGHEST).

unhook() ->
  emqx_hooks:del('client.authenticate', {?MODULE, on_client_authenticate}),
  emqx_hooks:del('client.authorize', {?MODULE, on_client_authorize}).
```

通常、フックはプラグインの起動・停止に合わせて有効化・無効化するため、プラグインアプリケーションの`start/2`と`stop/1`関数で`hook/unhook`を呼びます。

```
start(_StartType, _StartArgs) ->
    {ok, Sup} = my_emqx_plugin_sup:start_link(),
    my_emqx_plugin:hook(),

    {ok, Sup}.

stop(_State) ->
    my_emqx_plugin:unhook().
```

コールバック関数のシグネチャは[フックポイント仕様](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl)で確認できます。例：

```
-callback 'client.authorize'(
    emqx_types:clientinfo(), emqx_types:pubsub(), emqx_types:topic(), allow | deny
) ->
    fold_callback_result(#{result := allow | deny, from => term()}).

-callback 'client.authenticate'(emqx_types:clientinfo(), ignore) ->
    fold_callback_result(
        ignore
        | ok
        | {ok, map()}
        | {ok, map(), binary()}
        | {continue, map()}
        | {continue, binary(), map()}
        | {error, term()}
    ).
```

コールバック関数の実装例：

```erlang
%% クライアントIDがA-Z、a-z、0-9、アンダースコアのいずれかの文字のみで構成されている接続のみ許可
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.
%% クライアントは/room/{clientid}形式のトピックのみサブスクライブ可能だが、任意のトピックにパブリッシュ可能
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result) -> {ok, Result}.
```

スケルトンアプリでは、フックは`my_emqx_plugin:load/1`で登録し、`my_emqx_plugin:unload/0`で解除しています。

### 設定更新の処理

ユーザーがプラグインの設定を更新すると、プラグインアプリケーションの`on_config_changed/2`コールバックが呼ばれます。

このコールバックでは通常以下を行います。

- 新しい設定の検証
- プラグインが稼働中なら変更に対応

設定検証時、アプリケーションがまだ起動していない可能性があるため、ステートレスなチェックを行い、ノード間で不整合を起こす環境依存チェックは避けてください。

プラグインが稼働中の場合、設定変更を適用します。一般的なパターンは以下です。

- アプリケーション起動時に設定を扱う`gen_server`を起動
- 例：`my_emqx_plugin_config_server`が現在の設定を読み込み状態を初期化
- `on_config_changed/2`が設定を検証し、新しい設定を`my_emqx_plugin_config_server`に送信
- サーバーが稼働中なら状態を更新し、そうでなければ何もしない

### ヘルスチェックの処理

`on_health_check/1`コールバックはEMQXがプラグインの状態を要求したときに呼ばれます。プラグインは以下のように状態を報告できます。

- プラグインが正常なら`ok`を返す
- 問題がある場合はバイナリの理由を含む`{error, Reason}`を返す

外部リソースに依存するプラグインではこのコールバックが重要です。

詳細はスケルトンアプリの`my_emqx_plugin_app:on_health_check/1`を参照してください。

::: tip

この関数は稼働中のプラグインに対して呼ばれますが、起動や停止中の並行処理で呼ばれる場合もあります。

:::

実装例は[カスタムプラグインロジックの実装](./plugin-example.md)に多数あります。

### プラグインパッケージのビルド

以下のコマンドでプラグインのリリースを作成します。

```shell
$ cd my_emqx_plugin
$ make rel
```

これにより、プラグインリリース`_build/default/emqx_plugin/my_emqx_plugin-1.0.0.tar.gz`が作成されます。このパッケージはプラグインのプロビジョニングやインストールに使用できます。

### パッケージ構成

プラグインがリリースとしてビルドされると、パッケージ構成は以下のようになります。

```
└── my_emqx_plugin-1.1.0.tar.gz
    ├── map_sets-1.1.0
    ├── my_emqx_plugin-0.1.0
    ├── README.md
    └── release.json
```

tarballにはコンパイル済みアプリケーション（`rebar.config`の`relx`セクションで指定）、`README.md`、プラグインのメタデータを含む`release.json`が含まれます。

```json
{
    "hidden": false,
    "name": "my_emqx_plugin",
    "description": "Another amazing EMQX plugin.",
    "authors": "Anonymous",
    "builder": {
        "name": "Anonymous",
        "contact": "anonymous@example.org",
        "website": "http://example.com"
    },
    "repo": "https://github.com/emqx/emqx-plugin-template",
    "functionality": "Demo",
    "compatibility": {
        "emqx": "~> 5.7"
    },
    "git_ref": "unknown",
    "built_on_otp_release": "27",
    "emqx_plugrel_vsn": "0.5.1",
    "git_commit_or_build_date": "2025-04-29",
    "metadata_vsn": "0.2.0",
    "rel_apps": [
        "my_emqx_plugin-0.1.0",
        "map_sets-1.1.0"
    ],
    "rel_vsn": "1.1.0",
    "with_config_schema": true
}
```

### 混在プラグインバージョン対応設計

EMQX 6.2.4以降、`node.pinned_plugins`を使ってノードのデプロイ前にプラグインを事前デプロイ・設定できます。各ノードは独立してピン留めプラグインを管理するため、同じプラグインの異なるバージョンをノードごとに実行してもバージョン不一致警告は発生しません。混在バージョン対応が必要な場合は、以下の互換性を保つ必要があります。

- 共有データベーステーブルと保存データフォーマット
- 異なるノード上のプラグインインスタンス間でのRPC呼び出しとメッセージ
- 異なるプラグインバージョン間での設定フォーマット

デプロイおよびライフサイクル動作の詳細は[ノードピン留めプラグインの事前デプロイ](./plugin-management.md#pre-deploy-a-node-pinned-plugin)を参照してください。

## プラグイン拡張APIとUI

プラグインはEMQXプラグインAPIゲートウェイを通じてカスタムHTTPエンドポイントを公開でき、オプションでダッシュボードにネイティブUIを埋め込めます。

### プラグインHTTP API

プラグインAPIゲートウェイはリクエストを以下のパスでプラグインにルーティングします。

```
/api/v5/plugin_api/{plugin_name}/...
```

EMQX 6.3.2以降、プラグインAPIゲートウェイのスコープチェックは制限付きの`plugin_api`スコープか管理者相当の`system`スコープのいずれかを受け入れます。呼び出し元のロールとネームスペース権限も要求操作を許可している必要があります。

`plugin_api`はプラグインが公開するエンドポイントにアクセスする必要があるが、プラグインのインストール・起動・停止・設定は不要な場合に使います。プラグイン管理操作は`system`スコープに残ります。ゲートウェイは後方互換性のため`system`も引き続き受け入れます。

::: warning 重要

`plugin_api`スコープはこのゲートウェイを通じて公開されるすべてのエンドポイントへのアクセス権を与えます。個々のプラグインエンドポイントが何をできるかは制限しません。プラグインが公開する各エンドポイントのセキュリティ影響を必ず確認してください。

:::

これらのリクエストを処理するには、プラグインアプリモジュールで`on_handle_api_call/4`を実装し、メソッドとパスでディスパッチします。参考実装は`plugins/emqx_username_quota/src/emqx_username_quota_app.erl`と`emqx_username_quota_api.erl`を参照してください。

#### コールバック契約

```erlang
on_handle_api_call(Method, PathRemainder, Request, Context) -> Result
```

| パラメータ       | 説明                                                                 |
| --------------- | ------------------------------------------------------------------- |
| `Method`        | `get \| post \| put \| patch \| delete`                             |
| `PathRemainder` | `{plugin_name}`以降のパスセグメントのリスト（パーセントデコード済み） |
| `Request`       | `query_string`、`headers`、および（GET/DELETE以外の）JSONボディを含むマップ |
| `Context`       | 認証メタデータとネームスペース情報を含むマップ                      |

許容される戻り値：

- `{ok, StatusCode, Headers, Body}`
- `{error, StatusCode, Headers, Body}`
- `{error, not_found}`

### ダッシュボードのプラグインネイティブUI

`emqx_plugin`メタデータに`index`フィールドがある場合、EMQXダッシュボードはプラグインのネイティブUIをiframeで表示します。ダッシュボードはプラグインAPIのベースパスを先頭に付加します。

```
/api/v5/plugin_api/{plugin_name}{index}
```

例えば、`index: "/ui"`なら`/api/v5/plugin_api/{plugin_name}/ui`に解決されます。

ネイティブUIを無効化するには`index`フィールドを省略するか空文字にしてください。
