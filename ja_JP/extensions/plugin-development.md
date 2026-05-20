# EMQXプラグインの開発

このページでは、EMQXプラグインテンプレートを使用してカスタムEMQXプラグインを開発する手順を説明します。

## 前提条件

開始する前に、以下を準備してください。

- EMQXの[フック](./hooks.md)の知識
- `make`を含む動作するビルド環境（例：`build_essential`）
- [rebar3](https://www.rebar3.org/)
- 対象とするEMQXリリースと同じメジャーバージョンのErlang/OTP。詳細はDockerの`org.opencontainers.image.otp.version`属性や、使用されているバージョンを示す`.tool-versions`ファイル（例：https://github.com/emqx/emqx/blob/e5.9.0-beta.4/.tool-versions）を参照してください。Erlang/OTPのバージョン管理には[ASDF](https://asdf-vm.com/)の利用を推奨します。もしくは、[こちらのコマンド](https://github.com/emqx/emqx-builder/blob/main/show-latest-images.sh)を実行してemqx-builderイメージを取得できます。

## プラグインテンプレートのインストール

EMQXはカスタムプラグイン作成を簡素化するために[emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)を提供しています。新しいプラグインを作成するには、`rebar3`のテンプレートとして`emqx-plugin-template`をインストールしてください。

Linux環境では、以下のコマンドで`emqx-plugin-template`をダウンロードします。

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template
$ popd
```

::: tip

`REBAR_CACHE_DIR`環境変数が設定されている場合、テンプレートのディレクトリは`$REBAR_CACHE_DIR/.config/rebar3/templates`になります。[関連issueはこちら](https://github.com/erlang/rebar3/issues/2762)です。

:::

## プラグインスケルトンの生成

インストールしたテンプレートを使って新しいプラグインプロジェクトを生成します。

```shell
$ rebar3 new emqx-plugin my_emqx_plugin
```

このコマンドにより、`my_emqx_plugin`ディレクトリにプラグインの動作するスケルトンが作成されます。

### ディレクトリ構成

`rebar3 new emqx-plugin`コマンドは、`emqx`を依存関係に含む標準的なErlangアプリケーションを以下の構成で作成します。

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

- `src`: プラグインのOTPアプリケーションのコードを含みます。
- `priv`: プラグインの設定ファイルやスキーマ（サンプルファイル含む）を格納します。
- `rebar.config`: アプリケーションのビルドおよびリリースパッケージ作成に使用する`rebar3`の設定ファイルです。
- `Makefile`: プラグインのビルドのエントリポイントです。
- `scripts`: `Makefile`の補助スクリプト。**注意:** テンプレートは`emqx`に依存しているため、カスタム版の`rebar3`が必要で、付属の`./scripts/ensure-rebar3.sh`スクリプトでインストールできます。
- `README.md`: ドキュメントのプレースホルダーです。
- `LICENSE`: プラグインのサンプルライセンスファイルです。

#### 設定ファイル `rebar.config` の理解

`rebar.config`はプラグインのビルドおよびリリースパッケージ化に使用されます。内容を確認し、プラグインの要件に応じて調整してください。

重要なセクションは以下です。

- 依存関係（`deps`）セクション
- リリース（`relx`）セクション
- プラグインの説明（`emqx_plugin`）セクション

`deps`セクションでは、プラグインが依存する他のOTPアプリケーションを追加できます。

```
{deps,
    [
        ...
        %% これはプラグインの依存関係です
        {map_sets, "1.1.0"}
    ]}.
```

テンプレートはプラグインに`map_sets`という依存関係を1つ追加しています。不要なら削除可能です。依存関係の詳細は[`rebar3`の依存関係ドキュメント](https://www.rebar3.org/docs/configuration/dependencies/)を参照してください。

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

リリース名とバージョンは、プラグインがEMQXにインストールされた際の識別子として重要です。APIやCLIでプラグインを指定する際の単一識別子（例：`my_emqx_plugin-1.0.0`）となります。

プラグイン説明セクションでは、プラグインに関する追加情報を指定します。

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

#### `src` ディレクトリの概要

`src`ディレクトリにはプラグインのOTPアプリケーションのコードが含まれます。

##### `my_emqx_plugin.app.src`

標準的なErlangアプリケーション記述ファイルで、リリース時に`my_emqx_plugin.app`にコンパイルされます。

- アプリケーションのバージョンはリリースバージョンと一致させる必要はなく、別のバージョニングを採用可能です。
- 特に`applications`セクションに注意してください。プラグインはOTPアプリケーションとしてビルドされるため、プラグインの起動・停止・再起動はこのOTPアプリケーションの操作と同じです。プラグインが他のアプリケーションに依存する場合は、必ずプラグインの設定ファイルの`applications`セクションにそれらを記載してください。

##### `my_emqx_plugin_app.erl`

プラグインのアプリケーションとその監督ツリーを起動・停止するための[`application`ビヘイビア](https://www.erlang.org/doc/man/application.html)（`start/2`と`stop/1`関数）を実装するメインモジュールです。

`start/2`関数でよく行う処理は以下の通りです。

- EMQXのフックポイントへの登録
- CLIコマンドの登録
- 監督ツリーの起動

オプションで、`_app.erl`モジュールは`on_config_changed/2`および`on_health_check/1`のコールバック関数を実装できます。

- `on_config_changed/2`はDashboard、API、CLIからプラグイン設定が変更された際に呼ばれます。
- `on_health_check/1`はプラグインの状態が要求された際に呼ばれ、プラグインの状態を返すことができます。

#### その他のファイル

`my_emqx_plugin_cli.erl`モジュールはプラグインのCLIコマンドを実装します。登録されたCLIコマンドは`emqx ctl`コマンド経由で呼び出されます。

`my_emqx_plugin_sup.erl`はプラグインの典型的なスーパーバイザーを実装します。

`my_emqx_plugin.erl`はプラグインのメインモジュールで、プラグインのロジックを実装します。スケルトンでは簡単なログ出力を行うデモ用のフックをいくつか実装しています。その他のモジュールもプラグインに追加可能です。

::: tip 注意

アプリケーションモジュールやファイル名は任意ですが、以下の要件を満たす必要があります。

- アプリケーション名はプラグイン名と同じであること。
- アプリケーションモジュール（`_app`）は`{plugin_name}_app`という名前であること。

:::

#### `priv` ディレクトリの概要

`priv`ディレクトリにはプラグインの設定ファイルやスキーマが格納されます。

##### `config.hocon`

プラグインの初期設定を[HOCON形式](https://github.com/lightbend/config/blob/master/HOCON.md)で記述したファイルです。`config.hocon.example`を参照用に利用できます。

##### `config_schema.avsc`

プラグインの設定スキーマを[Avro形式](https://avro.apache.org/docs/1.11.1/specification/)で定義したファイルです。存在する場合、EMQXは設定更新時にこのスキーマに対して検証を行います。`config.hocon`がスキーマに準拠しない場合、リリースビルドは失敗します。

さらに、このファイルにはUIヒントを含めることができ、EMQXダッシュボードからの対話的な設定を可能にします。参考として`config_schema.avsc.enterprise.example`を参照してください。

##### `config_i18n.json`

プラグインの設定UIの翻訳をJSON形式で記述したファイルです。例：

```
{
  "$key": {
    "zh": "中文翻译",
    "en": "English translation"
  },
  ...
}
```

翻訳は`config_schema.avsc`のUIヒントで参照されます。詳細は`config_i18n.json.example`および`config_schema.avsc.enterprise.example`を参照してください。

## プラグインの実装

スケルトンが準備できたら、プラグインのロジック実装を開始します。通常、プラグイン実装には以下のロジックが必要です。

- フックとCLIコマンドの実装
- 設定更新の処理
- ヘルスチェックの処理

### フックとCLIコマンドの実装

EMQXはさまざまなイベントに対するフックポイントを定義しています。任意のアプリケーション（プラグインを含む）はこれらのフックポイントにコールバックを登録し、イベントに反応したりデフォルト動作を変更したりできます。

よく使われるフックポイントはスケルトンファイルに用意されています。フックポイントの一覧、引数、期待される戻り値は[EMQXのコード](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl)でも確認可能です。

フックポイントにコールバックを登録するには`emqx_hooks:add/3`関数を使用します。以下のパラメータを指定してください。

- フックポイント名
- コールバックモジュールと関数、必要に応じてEMQXが渡す追加引数
- コールバックの優先度（通常は最優先で呼ばれるよう`?HP_HIGHEST`）

コールバック登録解除は`emqx_hooks:del/2`関数で、フックポイント名とコールバックのモジュール/関数を指定して行います。

例として、`client.authenticate`と`client.authorize`フックポイントの登録/解除は以下のようにします。

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

通常、フックはプラグインの起動・停止と連動して有効化・無効化するため、プラグインのアプリケーションの`start/2`と`stop/1`関数内で`hook/unhook`を呼びます。

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

コールバック関数実装例：

```erlang
%% クライアントIDがA-Z、a-z、0-9、アンダースコアのいずれかの文字のみで構成されている接続のみ許可
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.
%% クライアントは /room/{clientid} 形式のトピックのみサブスクライブ可能だが、任意のトピックにメッセージを送信できる
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result) -> {ok, Result}.
```

スケルトンアプリでは、フックは`my_emqx_plugin:load/1`で登録され、`my_emqx_plugin:unload/0`で解除されます。

### 設定更新の処理

ユーザーがプラグイン設定を更新すると、プラグインのアプリケーションの`on_config_changed/2`コールバックが呼ばれます。

このコールバック内で通常行う処理は以下です。

- 新しい設定の検証
- プラグインが起動中なら変更に対応した処理

設定検証時は、アプリケーションがまだ起動していない可能性があるため、ステートレスなチェックを行い、ノード間で不整合が起きる恐れのある環境依存のチェックは避けてください。

プラグインが起動中の場合は、設定変更を適用できます。一般的なパターンは以下の通りです。

- アプリケーション起動時に設定を扱う`gen_server`（例：`my_emqx_plugin_config_server`）を起動
- このサーバーが現在の設定を読み込み状態を初期化
- `on_config_changed/2`コールバックで設定を検証し、新しい設定を`my_emqx_plugin_config_server`に送信
- サーバーが起動中なら状態を新設定で更新、起動していなければ何もしない

### ヘルスチェックの処理

`on_health_check/1`コールバックはEMQXがプラグインの状態を要求した際に呼ばれます。プラグインは以下のように状態を返せます。

- プラグインが正常なら`ok`を返す
- 問題があれば、バイナリの理由を含む`{error, Reason}`を返す

このコールバックは外部リソースに依存し、それが利用不可になる可能性があるプラグインにとって重要です。

詳細はスケルトンアプリの`my_emqx_plugin_app:on_health_check/1`を参照してください。

::: tip

この関数はプラグイン稼働中に呼ばれますが、起動や停止時の並行処理により呼ばれることもあります。

:::

実装例は[カスタムプラグインロジックの実装](./plugin-example.md)でさらに確認できます。

## プラグインパッケージのビルド

以下のコマンドを実行してプラグインのリリースを作成します。

```shell
$ cd my_emqx_plugin
$ make rel
```

これによりプラグインリリースファイル`_build/default/emqx_plugin/my_emqx_plugin-1.0.0.tar.gz`が作成されます。このパッケージはプラグインのプロビジョニング／インストールに使用できます。

### パッケージ構成

プラグインがリリースとしてビルドされると、パッケージ構成は以下のようになります。

```
└── my_emqx_plugin-1.1.0.tar.gz
    ├── map_sets-1.1.0
    ├── my_emqx_plugin-0.1.0
    ├── README.md
    └── release.json
```

tarballにはコンパイル済みアプリケーション（`rebar.config`の`relx`セクションで指定したもの）、`README.md`、およびプラグインのメタデータを含む`release.json`が含まれます。

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
