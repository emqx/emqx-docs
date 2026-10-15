# EMQXプラグインの開発

このページでは、EMQXモノレポ外でカスタムEMQXプラグインを開発する手順を説明します。

EMQX公式プラグインは通常、EMQXモノレポ内で開発されます。詳細は[EMQXプラグイン開発ガイド](https://github.com/emqx/emqx/blob/release-60/PLUGIN.md)をご参照ください。

## 前提条件

開始する前に、以下を準備してください。

- EMQXの[フック](./hooks.md)の知識
- `make`を含む動作可能なビルド環境（例：`build_essential`）
- [rebar3](https://www.rebar3.org/)
- 対象とするEMQXリリースと同じメジャーバージョンのErlang/OTP。詳細はDockerの`org.opencontainers.image.otp.version`属性や使用バージョンの`.tool-versions`ファイル（例：https://github.com/emqx/emqx/blob/e5.9.0-beta.4/.tool-versions）を参照してください。Erlang/OTPのバージョン管理には[ASDF](https://asdf-vm.com/)の利用を推奨します。あるいは、[こちらのコマンド](https://github.com/emqx/emqx-builder/blob/main/show-latest-images.sh)でemqx-builderイメージを取得可能です。

## スタンドアロンプラグイン開発

スタンドアロンプラグイン開発には以下の2つのスタイルがあります。

- **rebar3テンプレート**： [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)を使ってプラグインプロジェクトを生成します。このスタイルは`rebar3`のみを使用します。
- **Gitサブモジュールスタイル**： EMQX 6.0以降で、プラグインを独立したリポジトリに保持し、EMQXをGitサブモジュールとして追加してモノレポのツールでビルドします。

### Gitサブモジュールスタイル

プラグインのソースコードがプライベートなどの理由で別リポジトリに保持しつつ、EMQXモノレポのツールでビルドしたい場合にこのスタイルを使います。

1. EMQXをサブモジュールとして追加します。

   ```bash
   git submodule add --depth 1 git@github.com:emqx/emqx.git emqx
   ```

2. 対象バージョンに対応するEMQXブランチ（例：EMQX 6.0なら`release-60`）をチェックアウトします。

3. プラグインリポジトリをサブモジュールの`plugins/`ディレクトリにシンボリックリンクします。

   ```bash
   ln -s ../.. emqx/plugins/{plugin_name}
   ```

4. EMQXサブモジュールからプラグインパッケージをビルドします。

   ```bash
   cd emqx
   make plugin-{plugin_name}
   ```

`.tar.gz`の成果物は`emqx/_build/plugins/`以下に生成されます。

`rebar3`テンプレートスタイルの場合は以下の手順を続けてください。

### プラグインテンプレートのインストール

EMQXはカスタムプラグイン作成を簡素化するために[emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)を提供しています。新しいプラグインを作成するには、`rebar3`のテンプレートとして`emqx-plugin-template`をインストールしてください。

Linux環境では以下のコマンドで`emqx-plugin-template`をダウンロードします。

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template
$ popd
```

::: tip

`REBAR_CACHE_DIR`環境変数が設定されている場合、テンプレートのディレクトリは`$REBAR_CACHE_DIR/.config/rebar3/templates`になります。関連issueは[こちら](https://github.com/erlang/rebar3/issues/2762)です。

:::

インストール確認は以下で行います。

```shell
$ rebar3 new help
```

出力に`emqx-plugin (custom)`がテンプレートとして表示されれば成功です。

### プラグインスケルトンの生成

インストールしたテンプレートを使い、新しいプラグインプロジェクトを生成します。

```shell
$ rebar3 new emqx-plugin my_emqx_plugin
```

これにより、`my_emqx_plugin`ディレクトリにプラグインの動作するスケルトンが作成されます。

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
- `priv`：プラグインの設定ファイルやスキーマ（サンプルファイル含む）を格納します。
- `rebar.config`：アプリケーションのビルドとリリースパッケージ化に使う`rebar3`設定ファイルです。
- `Makefile`：プラグインビルドのエントリーポイントです。
- `scripts`：`Makefile`の補助スクリプト。**注意：** テンプレートは`emqx`に依存しており、専用の`rebar3`が必要です。付属の`./scripts/ensure-rebar3.sh`でインストール可能です。
- `README.md`：ドキュメントのプレースホルダー。
- `LICENSE`：プラグインのサンプルライセンスファイル。

#### 設定ファイル `rebar.config` の理解

`rebar.config`はプラグインをビルドしリリースにパッケージ化するための設定ファイルです。内容を確認し、プラグインの要件に応じて調整してください。

主に重要なのは以下のセクションです。

- 依存関係（`deps`）セクション
- リリース（`relx`）セクション
- プラグイン説明（`emqx_plugin`）セクション

`deps`セクションにはプラグインが依存する他のOTPアプリケーションを追加できます。

```
{deps,
    [
        ...
        %% これはプラグインの依存関係
        {map_sets, "1.1.0"}
    ]}.
```

テンプレートは`map_sets`という依存を1つ追加しています。不要なら削除可能です。依存関係の詳細は[`rebar3`依存関係ドキュメント](https://www.rebar3.org/docs/configuration/dependencies/)を参照してください。

`relx`セクションではリリース名とバージョン、リリースに含めるアプリケーションのリストを指定します。

```
{relx, [ {release, {my_emqx_plugin, "1.0.0"},
            [ my_emqx_plugin
            , map_sets
            ]}
       ...
       ]}.
```

通常は`deps`セクションのランタイム依存アプリケーションをリリースに含めます。

リリース名とバージョンは、プラグインがEMQXにインストールされた際の識別子となり、APIやCLIでの呼び出しに使われます（例：`my_emqx_plugin-1.0.0`）。

プラグイン説明セクションではプラグインの追加情報を指定します。

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

- アプリケーションのバージョンはリリースバージョンと一致させる必要はなく、別のバージョニング方式も可能です。
- `applications`セクションに注意してください。プラグインはOTPアプリケーションとしてビルドされるため、プラグインの起動・停止・再起動はこのOTPアプリケーションに対して行います。依存アプリケーションがあれば必ず`applications`に記載してください。

##### `my_emqx_plugin_app.erl`

プラグインのアプリケーションを起動・停止するための[`application`ビヘイビア](https://www.erlang.org/doc/man/application.html)（`start/2`と`stop/1`関数）を実装するメインモジュールです。

`start/2`関数では一般的に以下を行います。

- EMQXのフックポイントにフックを登録
- CLIコマンドの登録
- 監督ツリーの起動

オプションで`on_config_changed/2`と`on_health_check/1`のコールバック関数も実装可能です。

- `on_config_changed/2`はDashboard、API、CLIからプラグイン設定が変更された際に呼ばれます。
- `on_health_check/1`はプラグインの状態が要求された際に呼ばれ、プラグインのヘルス状態を報告できます。

#### その他のファイル

`my_emqx_plugin_cli.erl`はプラグインのCLIコマンドを実装します。登録すると`emqx ctl`コマンド経由で呼び出されます。

`my_emqx_plugin_sup.erl`はプラグインの典型的なスーパーバイザーを実装します。

`my_emqx_plugin.erl`はプラグインのメインモジュールで、プラグインのロジックを実装します。スケルトンでは簡単なログ出力を行うデモ用フックを実装しています。その他のモジュールも追加可能です。

::: tip 注意

アプリケーションモジュールやファイル名は任意ですが、以下の条件は必須です。

- アプリケーション名はプラグイン名と同じであること
- アプリケーションモジュール（`_app`）は`{plugin_name}_app`という名前であること
:::

#### `priv`ディレクトリの概要

`priv`ディレクトリはプラグインの設定ファイルやスキーマを格納します。

##### `config.hocon`

プラグインの初期設定を[HOCON形式](https://github.com/lightbend/config/blob/master/HOCON.md)で記述したファイルです。`config.hocon.example`を参照用に利用できます。

##### `config_schema.avsc`

プラグイン設定のスキーマを[Avro形式](https://avro.apache.org/docs/1.11.1/specification/)で定義したファイルです。存在する場合、EMQXは設定更新時にこのスキーマに対してバリデーションを行います。`config.hocon`がスキーマに準拠しない場合、リリースビルドは失敗します。

さらにUIヒントを含めることができ、EMQXダッシュボードでの対話的な設定が可能になります。参考は`config_schema.avsc.enterprise.example`をご覧ください。

##### `config_i18n.json`

プラグイン設定UIの翻訳をJSON形式で記述したファイルです。例：

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

### プラグインの実装

スケルトンが準備できたら、プラグインのロジック実装を開始します。通常必要なロジックは以下です。

- フックとCLIコマンドの実装
- 設定更新の処理
- ヘルスチェックの処理

### フックとCLIコマンドの実装

EMQXは様々なイベントに対するフックポイントを定義しています。任意のアプリケーション（プラグイン含む）はこれらのフックポイントにコールバックを登録し、イベントに反応したりデフォルト動作を変更したりできます。

よく使われるフックポイントはスケルトンファイルに用意されています。フックポイントの一覧、引数、期待される戻り値は[EMQXコード](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl)に記載されています。

フックポイントにコールバックを登録するには`emqx_hooks:add/3`関数を使います。以下のパラメータを指定してください。

- フックポイント名
- コールバックモジュールと関数、必要に応じてEMQXが渡す追加引数
- コールバックの優先度（通常は`?HP_HIGHEST`で最優先呼び出し）

コールバックの登録解除は`emqx_hooks:del/2`関数で、フックポイント名とコールバックモジュール/関数を指定して行います。

例として`client.authenticate`と`client.authorize`フックポイントの登録/解除は以下のようにします。

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

通常、フックはプラグインの起動・停止に合わせて有効化/無効化するため、`start/2`と`stop/1`関数内で`hook/unhook`を呼びます。

```
start(_StartType, _StartArgs) ->
    {ok, Sup} = my_emqx_plugin_sup:start_link(),
    my_emqx_plugin:hook(),

    {ok, Sup}.

stop(_State) ->
    my_emqx_plugin:unhook().
```

コールバック関数のシグネチャは[フックポイント仕様](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl)にあります。例：

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
%% クライアントIDがA-Z、a-z、0-9、アンダースコアのみの場合に接続を許可
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.
%% クライアントは/room/{clientid}形式のトピックのみサブスクライブ可能、パブリッシュは任意トピック可
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result) -> {ok, Result}.
```

スケルトンアプリでは`my_emqx_plugin:load/1`でフック登録、`my_emqx_plugin:unload/0`で登録解除しています。

### 設定更新の処理

ユーザーがプラグイン設定を更新すると、プラグインアプリケーションの`on_config_changed/2`コールバックが呼ばれます。

このコールバックでは通常以下を行います。

- 新しい設定のバリデーション
- プラグインが稼働中なら設定変更に対応

設定バリデーション時はアプリケーションがまだ起動していない可能性もあるため、ステートレスなチェックを行い、ノード間で不整合が起きる環境依存チェックは避けてください。

プラグインが稼働中なら設定変更を適用します。一般的なパターンは以下です。

- アプリケーション起動時に設定を扱う`gen_server`を起動
- そのサーバー（例：`my_emqx_plugin_config_server`）が現在設定を読み込み状態を初期化
- `on_config_changed/2`で設定をバリデートし、新設定をサーバーに送信
- サーバーが稼働中なら状態を更新し、稼働していなければ何もしない

### ヘルスチェックの処理

`on_health_check/1`コールバックはEMQXがプラグインの状態を要求した際に呼ばれます。プラグインは以下のようにヘルスを報告できます。

- 正常なら`ok`を返す
- 問題があればバイナリの理由を含む`{error, Reason}`を返す

外部リソースに依存し、利用不可になる可能性があるプラグインではこのコールバックが重要です。

詳細はスケルトンアプリの`my_emqx_plugin_app:on_health_check/1`を参照してください。

::: tip

この関数は稼働中のプラグインに対して呼ばれますが、起動や停止中の並行処理で呼ばれることもあります。

:::

さらに実装例は[カスタムプラグインロジックの実装](./plugin-example.md)をご覧ください。

### プラグインパッケージのビルド

以下のコマンドでプラグインのリリースを作成します。

```shell
$ cd my_emqx_plugin
$ make rel
```

これによりプラグインリリース`_build/default/emqx_plugin/my_emqx_plugin-1.0.0.tar.gz`が作成されます。このパッケージはプラグインのプロビジョニング/インストールに使用可能です。

### パッケージ構成

プラグインをリリースとしてビルドすると、パッケージ構成は以下のようになります。

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

## プラグイン拡張APIとUI

プラグインはEMQXプラグインAPIゲートウェイを通じてカスタムHTTPエンドポイントを公開でき、オプションでダッシュボードにネイティブUIを埋め込むことも可能です。

### プラグインHTTP API

プラグインAPIゲートウェイは以下のパスでプラグインへのリクエストをルーティングします。

```
/api/v5/plugin_api/{plugin_name}/...
```

EMQX 6.1.6以降、プラグインAPIゲートウェイのスコープチェックは制限付きの`plugin_api`スコープか管理者相当の`system`スコープのいずれかを許容します。呼び出し元のロールとネームスペースの権限も要求操作を許可している必要があります。

プラグインが公開するエンドポイントを呼び出すだけなら`plugin_api`スコープを使い、プラグインのインストール・起動・停止・設定変更などの管理操作は`system`スコープに残ります。後方互換のためゲートウェイは`system`も引き続き受け入れます。

::: warning 重要

`plugin_api`スコープはこのゲートウェイ経由で公開されるすべてのエンドポイントへのアクセス権を与えます。個々のプラグインエンドポイントの権限は制限しませんので、プラグインが公開する各エンドポイントのセキュリティ影響を十分に検討してください。

:::

これらのリクエストを処理するにはプラグインアプリモジュールで`on_handle_api_call/4`を実装し、HTTPメソッドやパスでディスパッチします。参考実装は`plugins/emqx_username_quota/src/emqx_username_quota_app.erl`や`emqx_username_quota_api.erl`にあります。

#### コールバック契約

```erlang
on_handle_api_call(Method, PathRemainder, Request, Context) -> Result
```

| パラメータ       | 説明                                                                 |
| --------------- | ------------------------------------------------------------------- |
| `Method`        | `get \| post \| put \| patch \| delete`                             |
| `PathRemainder` | `{plugin_name}`以降のパスのバイナリセグメントのリスト（パーセントデコード済み） |
| `Request`       | `query_string`、`headers`、`body`（GET/DELETE以外はJSON）を含むマップ |
| `Context`       | 認証メタデータとネームスペース情報を含むマップ                     |

許容される戻り値：

- `{ok, StatusCode, Headers, Body}`
- `{error, StatusCode, Headers, Body}`
- `{error, not_found}`

### ダッシュボードでのプラグインネイティブUI

`emqx_plugin`メタデータに`index`フィールドが含まれる場合、EMQXダッシュボードはプラグインのネイティブUIをiframeで表示します。ダッシュボードはプラグインAPIのベースパスを先頭に付加します。

```
/api/v5/plugin_api/{plugin_name}{index}
```

例えば`index: "/ui"`なら`/api/v5/plugin_api/{plugin_name}/ui`に解決されます。

ネイティブUIを無効にするには`index`フィールドを省略するか空文字に設定してください。
