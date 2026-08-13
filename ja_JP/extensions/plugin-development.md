# EMQXプラグインの開発

このページでは、EMQXモノリポジトリ外でカスタムEMQXプラグインを開発する手順を説明します。

EMQX公式プラグインは通常EMQXモノリポジトリ内で開発されます。詳細は[EMQXプラグイン開発ガイド](https://github.com/emqx/emqx/blob/release-60/PLUGIN.md)をご参照ください。

## 前提条件

開始する前に、以下を準備してください：

- EMQXの[フック](./hooks.md)の知識
- `make`を含む動作するビルド環境（例：`build_essential`）
- [rebar3](https://www.rebar3.org/)
- 対象とするEMQXリリースと同じメジャーバージョンのErlang/OTP。詳細はDockerの`org.opencontainers.image.otp.version`属性や、使用バージョンを示す`.tool-versions`ファイル（例：https://github.com/emqx/emqx/blob/e5.9.0-beta.4/.tool-versions）を参照してください。Erlang/OTPのバージョン管理には[ASDF](https://asdf-vm.com/)の利用を推奨します。あるいは、[こちらのコマンド](https://github.com/emqx/emqx-builder/blob/main/show-latest-images.sh)でemqx-builderイメージを取得可能です。

## スタンドアロンプラグイン開発

スタンドアロンプラグイン開発には2つのスタイルがあります：

- **rebar3テンプレート**： [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)を使ってプラグインプロジェクトを生成します。このスタイルは`rebar3`のみを使用します。
- **Gitサブモジュールスタイル**： EMQX 6.0以降で、プラグインを独立したリポジトリに保持し、EMQXをGitサブモジュールとして追加してモノリポジトリのツールでビルドします。

### Gitサブモジュールスタイル

プラグインのソースコードがプライベートで別リポジトリに保持する必要がある場合など、プラグインを独立リポジトリにしたままEMQXモノリポジトリのツールでビルドしたい場合にこのスタイルを使用します。

1. EMQXをサブモジュールとして追加します：

   ```bash
   git submodule add --depth 1 git@github.com:emqx/emqx.git emqx
   ```

2. 対象バージョンに対応するEMQXブランチをチェックアウトします。例：EMQX 6.0なら`release-60`。

3. プラグインリポジトリをサブモジュールの`plugins/`ディレクトリにシンボリックリンクします：

   ```bash
   ln -s ../.. emqx/plugins/{plugin_name}
   ```

4. EMQXサブモジュールからプラグインパッケージをビルドします：

   ```bash
   cd emqx
   make plugin-{plugin_name}
   ```

`.tar.gz`アーティファクトは`emqx/_build/plugins/`以下に生成されます。

`rebar3`テンプレートスタイルの場合は、以下の手順に進んでください。

### プラグインテンプレートのインストール

EMQXはカスタムプラグイン作成を簡単にするために[emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)を提供しています。新しいプラグインを作成するには、`emqx-plugin-template`を`rebar3`テンプレートとしてインストールしてください。

Linux環境では以下のコマンドで`emqx-plugin-template`をダウンロードします：

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template
$ popd
```

::: tip

`REBAR_CACHE_DIR`環境変数が設定されている場合、テンプレートのディレクトリは`$REBAR_CACHE_DIR/.config/rebar3/templates`になります。関連issueは[こちら](https://github.com/erlang/rebar3/issues/2762)です。

:::

インストールを確認するには以下を実行します：

```shell
$ rebar3 new help
```

出力に`emqx-plugin (custom)`がテンプレートとして表示されれば成功です。

### プラグインスケルトンの生成

インストール済みテンプレートを使って新しいプラグインプロジェクトを生成します：

```shell
$ rebar3 new emqx-plugin my_emqx_plugin
```

このコマンドで`my_emqx_plugin`ディレクトリに動作するスケルトンが作成されます。

### ディレクトリ構造

`rebar3 new emqx-plugin`コマンドは、`emqx`を依存関係に含む標準的なErlangアプリケーションを以下の構造で作成します：

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
- `rebar.config`：アプリケーションのビルドおよびリリースパッケージ化に使用される`rebar3`設定ファイルです。
- `Makefile`：プラグインのビルドエントリポイントです。
- `scripts`：`Makefile`を補助するスクリプトです。**注意：** テンプレートは`emqx`に依存しているため、カスタム版`rebar3`が必要で、付属の`./scripts/ensure-rebar3.sh`でインストール可能です。
- `README.md`：ドキュメントのプレースホルダーです。
- `LICENSE`：プラグインのサンプルライセンスファイルです。

#### 設定ファイル`rebar.config`の理解

`rebar.config`はプラグインのビルドとリリースパッケージ化に使います。内容を確認し、プラグインの要件に応じて調整してください。

重要なセクションは以下です：

- 依存関係（`deps`）セクション
- リリース（`relx`）セクション
- プラグイン説明（`emqx_plugin`）セクション

`deps`セクションではプラグインが依存する他のOTPアプリケーションを追加できます。

```
{deps,
    [
        ...
        %% これはプラグインの依存関係です
        {map_sets, "1.1.0"}
    ]}.
```

テンプレートでは`map_sets`の依存が1つ追加されています。不要なら削除可能です。依存関係の詳細は[`rebar3`依存関係ドキュメント](https://www.rebar3.org/docs/configuration/dependencies/)を参照してください。

`relx`セクションではリリース名とバージョン、リリースに含めるアプリケーションのリストを指定します。

```
{relx, [ {release, {my_emqx_plugin, "1.0.0"},
            [ my_emqx_plugin
            , map_sets
            ]}
       ...
       ]}.
```

通常は`deps`セクションのランタイム依存アプリケーションをリリースに追加します。

リリース名とバージョンは、プラグインがEMQXにインストールされた際の識別子として重要です。APIやCLIでプラグインを指定する際の単一識別子（例：`my_emqx_plugin-1.0.0`）を形成します。

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

これは標準的なErlangアプリケーションの記述ファイルで、リリース時に`my_emqx_plugin.app`にコンパイルされます。

- アプリケーションのバージョンはリリースバージョンと異なっても構いません。
- `applications`セクションに特に注意してください。プラグインはOTPアプリケーションとしてビルドされるため、プラグインの起動・停止・再起動はこのOTPアプリケーションの操作と同じです。依存する他アプリケーションがあれば必ず`applications`に記載してください。

##### `my_emqx_plugin_app.erl`

プラグインのアプリケーションの開始・停止を行う[`application`ビヘイビア](https://www.erlang.org/doc/man/application.html)（`start/2`と`stop/1`関数）を実装するメインモジュールです。

`start/2`関数でよく行う処理は以下です：

- EMQXのフックポイントにフック登録
- CLIコマンドの登録
- 監督ツリーの起動

オプションで、`_app.erl`モジュールは`on_config_changed/2`と`on_health_check/1`のコールバックも実装可能です。

- `on_config_changed/2`はDashboard、API、CLI経由でプラグイン設定が変更された際に呼ばれます。
- `on_health_check/1`はプラグインの状態確認要求時に呼ばれ、プラグインはここで状態を報告できます。

#### その他のファイル

`my_emqx_plugin_cli.erl`モジュールはプラグインのCLIコマンドを実装します。登録後は`emqx ctl`コマンドから呼び出されます。

`my_emqx_plugin_sup.erl`はプラグインの典型的なスーパーバイザを実装します。

`my_emqx_plugin.erl`はプラグインのメインモジュールで、プラグインのロジックを実装します。スケルトンでは簡単なログ出力を伴うデモ用フックを実装しています。必要に応じて他のモジュールも追加可能です。

::: tip 注意

アプリケーションモジュールやファイル名は任意で構いませんが、以下の条件を満たす必要があります：

- アプリケーション名はプラグイン名と同じであること
- アプリケーションモジュール（`_app`）は`{plugin_name}_app`という名前にすること
:::

#### `priv`ディレクトリの概要

`priv`ディレクトリにはプラグインの設定ファイルやスキーマが格納されます。

##### `config.hocon`

プラグインの初期設定を[HOCON形式](https://github.com/lightbend/config/blob/master/HOCON.md)で記述したファイルです。`config.hocon.example`を参照用に利用できます。

##### `config_schema.avsc`

プラグイン設定のスキーマを[Avro形式](https://avro.apache.org/docs/1.11.1/specification/)で定義したファイルです。存在する場合、EMQXは設定更新時にこのスキーマに対してバリデーションを行います。`config.hocon`がスキーマに準拠しない場合はリリースビルドが失敗します。

さらに、このファイルにはUIヒントを含めることができ、EMQXダッシュボードでの対話的な設定を可能にします。参考として`config_schema.avsc.enterprise.example`を参照してください。

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

スケルトンが用意できたら、プラグインのロジック実装を開始します。一般的に必要なロジックは以下です：

- フックとCLIコマンドの実装
- 設定更新の処理
- ヘルスチェックの処理

### フックとCLIコマンドの実装

EMQXは様々なイベントに対してフックポイントを定義しています。任意のアプリケーション（プラグイン含む）はこれらのフックポイントにコールバックを登録し、イベントに応じた処理やデフォルト動作の変更が可能です。

よく使われるフックポイントはスケルトンファイルに含まれています。フックポイントの一覧、引数、期待される戻り値は[EMQXコード](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl)で確認できます。

フックポイントにコールバックを登録するには`emqx_hooks:add/3`関数を使います。以下のパラメータを指定してください：

- フックポイント名
- コールバックモジュールと関数（およびEMQXが渡す追加引数）
- コールバックの優先度（通常は最優先の`?HP_HIGHEST`）

コールバックの登録解除は`emqx_hooks:del/2`関数を使い、フックポイント名とコールバックモジュール/関数を指定します。

例として、`client.authenticate`と`client.authorize`フックポイントの登録/解除：

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

通常、フックはプラグインの起動・停止に合わせて有効化・無効化するため、`start/2`と`stop/1`関数内で`hook/unhook`を呼びます：

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
%% クライアントIDがA-Z、a-z、0-9、アンダースコアのいずれかの文字のみで構成されている場合のみ接続を許可
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

ユーザーがプラグイン設定を更新すると、プラグインアプリケーションの`on_config_changed/2`コールバックが呼ばれます。

このコールバックでは通常以下を行います：

- 新しい設定の検証
- プラグインが稼働中なら変更に対応する処理

設定検証時はアプリケーションがまだ起動していない可能性があるため、ステートレスなチェックを行い、ノード間で不整合を起こす環境依存チェックは避けてください。

プラグインが稼働中であれば設定変更を適用できます。一般的なパターンは以下です：

- アプリケーション起動時に設定を扱う`gen_server`を起動
- そのサーバー（例：`my_emqx_plugin_config_server`）が現在の設定を読み込み状態を初期化
- `on_config_changed/2`で設定を検証し、新設定をサーバーに送信
- サーバーが稼働中なら状態を更新し、稼働していなければ何もしない

### ヘルスチェックの処理

`on_health_check/1`コールバックはEMQXがプラグインの状態を問い合わせる際に呼ばれます。プラグインは以下のように状態を報告可能です：

- プラグインが正常なら`ok`を返す
- 問題がある場合はバイナリ理由を含む`{error, Reason}`を返す

このコールバックは外部リソースに依存するプラグインで特に重要です。

詳細はスケルトンアプリの`my_emqx_plugin_app:on_health_check/1`を参照してください。

::: tip

この関数は稼働中のプラグインに対して呼ばれますが、起動や停止時の競合状態で呼ばれることもあります。

:::

より多くの実装例は[カスタムプラグインロジックの実装](./plugin-example.md)をご覧ください。

### プラグインパッケージのビルド

以下のコマンドでプラグインのリリースを作成します：

```shell
$ cd my_emqx_plugin
$ make rel
```

これによりプラグインリリース`_build/default/emqx_plugin/my_emqx_plugin-1.0.0.tar.gz`が作成されます。このパッケージはプラグインのプロビジョニングやインストールに使用可能です。

### パッケージ構造

プラグインがリリースとしてビルドされると、パッケージ構造は以下のようになります：

```
└── my_emqx_plugin-1.1.0.tar.gz
    ├── map_sets-1.1.0
    ├── my_emqx_plugin-0.1.0
    ├── README.md
    └── release.json
```

tarballにはコンパイル済みアプリケーション（`rebar.config`の`relx`セクションで指定）、`README.md`、およびプラグインのメタデータを含む`release.json`が含まれます。

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

プラグインAPIゲートウェイは以下のパスでプラグインへのリクエストをルーティングします：

```
/api/v5/plugin_api/{plugin_name}/...
```

これらのリクエストを処理するには、プラグインアプリモジュールで`on_handle_api_call/4`を実装し、HTTPメソッドやパスでディスパッチします。参考実装は`plugins/emqx_username_quota/src/emqx_username_quota_app.erl`および`emqx_username_quota_api.erl`を参照してください。

#### コールバック契約

```erlang
on_handle_api_call(Method, PathRemainder, Request, Context) -> Result
```

| パラメータ       | 説明                                                                 |
| --------------- | ------------------------------------------------------------------- |
| `Method`        | `get \| post \| put \| patch \| delete`                            |
| `PathRemainder` | `{plugin_name}`以降のパスセグメントのバイナリリスト（パーセントデコード済み） |
| `Request`       | `query_string`、`headers`、`body`（GET/DELETE以外はJSON）を含むマップ |
| `Context`       | 認証メタデータやネームスペース情報を含むマップ                     |

許容される戻り値：

- `{ok, StatusCode, Headers, Body}`
- `{error, StatusCode, Headers, Body}`
- `{error, not_found}`

### ダッシュボードのプラグインネイティブUI

`emqx_plugin`メタデータに`index`フィールドが含まれる場合、EMQXダッシュボードはプラグインのネイティブUIをiframeで表示します。ダッシュボードはプラグインAPIのベースパスを付加します：

```
/api/v5/plugin_api/{plugin_name}{index}
```

例えば、`index: "/ui"`なら`/api/v5/plugin_api/{plugin_name}/ui`に解決されます。

ネイティブUIを無効にするには、`index`フィールドを省略するか空文字列に設定してください。
