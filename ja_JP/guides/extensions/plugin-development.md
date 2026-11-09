# EMQXプラグインの開発

このページでは、EMQXモノレポ外でカスタムEMQXプラグインを開発する手順を説明します。

EMQX公式プラグインは通常、EMQXモノレポ内で開発されます。詳細は[EMQXプラグイン開発ガイド](https://github.com/emqx/emqx/blob/release-60/PLUGIN.md)をご参照ください。

## 前提条件

開始する前に、以下を準備してください：

- EMQXの[フック](./hooks.md)の知識
- `make`を含む動作するビルド環境（例：`build_essential`）
- [rebar3](https://www.rebar3.org/)
- 対象とするEMQXリリースと同じメジャーバージョンのErlang/OTP。詳細はDockerの`org.opencontainers.image.otp.version`属性や、使用バージョンの`.tool-versions`ファイル（例：https://github.com/emqx/emqx/blob/e5.9.0-beta.4/.tool-versions）を参照してください。Erlang/OTPのバージョン管理には[ASDF](https://asdf-vm.com/)の利用を推奨します。あるいは、[こちらのコマンド](https://github.com/emqx/emqx-builder/blob/main/show-latest-images.sh)でemqx-builderイメージを取得できます。

## スタンドアロンプラグイン開発

スタンドアロンプラグイン開発には以下の2つのスタイルがあります：

- **rebar3テンプレート**： [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)を使ってプラグインプロジェクトを生成します。このスタイルは`rebar3`のみを使用します。
- **Gitサブモジュールスタイル**： EMQX 6.0以降で、プラグインを独立したリポジトリに保持しつつ、EMQXをGitサブモジュールとして追加し、モノレポのツールでビルドします。

### Gitサブモジュールスタイル

プラグインのソースコードがプライベートで別リポジトリに保持する必要がある場合など、プラグインを独立リポジトリに置きつつEMQXモノレポツールでビルドしたい場合にこのスタイルを使います。

1. EMQXをサブモジュールとして追加します：

   ```bash
   git submodule add --depth 1 git@github.com:emqx/emqx.git emqx
   ```

2. 対象バージョンに対応するEMQXブランチをチェックアウトします（例：EMQX 6.0なら`release-60`）。

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

`rebar3`テンプレートスタイルの場合は、以下の手順を続けてください。

### プラグインテンプレートのインストール

EMQXはカスタムEMQXプラグイン作成を簡単にするために[emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)を提供しています。新しいプラグインを作成するには、`emqx-plugin-template`を`rebar3`のテンプレートとしてインストールしてください。

Linux環境では以下のコマンドで`emqx-plugin-template`をダウンロードします：

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template
$ popd
```

::: tip

`REBAR_CACHE_DIR`環境変数が設定されている場合、テンプレートディレクトリは`$REBAR_CACHE_DIR/.config/rebar3/templates`になります。関連issueは[こちら](https://github.com/erlang/rebar3/issues/2762)です。

:::

インストール確認は以下で行います：

```shell
$ rebar3 new help
```

出力に`emqx-plugin (custom)`がテンプレートとして表示されれば成功です。

### プラグインスケルトンの生成

インストール済みテンプレートを使い、新しいプラグインプロジェクトを生成します：

```shell
$ rebar3 new emqx-plugin my_emqx_plugin
```

このコマンドで`my_emqx_plugin`ディレクトリに動作するスケルトンが作成されます。

### ディレクトリ構成

`rebar3 new emqx-plugin`コマンドは、`emqx`を依存として含む標準的なErlangアプリケーションを以下のように生成します：

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
- `rebar.config`：`rebar3`の設定ファイルで、アプリケーションのビルドとリリースパッケージ化に使います。
- `Makefile`：プラグインビルドのエントリポイントです。
- `scripts`：`Makefile`用の補助スクリプト。**注意：** テンプレートは`emqx`に依存するため、カスタム版`rebar3`が必要で、付属の`./scripts/ensure-rebar3.sh`でインストール可能です。
- `README.md`：ドキュメントのプレースホルダー。
- `LICENSE`：プラグインのサンプルライセンスファイル。

#### 設定ファイル `rebar.config` の理解

`rebar.config`はプラグインのビルドとリリースパッケージ化に使います。内容を確認し、プラグインの要件に応じて調整してください。

重要なセクションは：

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

テンプレートはプラグインに`map_sets`の依存を1つ追加します。不要なら削除可能です。依存関係の詳細は[`rebar3`の依存関係ドキュメント](https://www.rebar3.org/docs/configuration/dependencies/)を参照してください。

`relx`セクションではリリース名とバージョン、リリースに含めるアプリケーションのリストを指定します。

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

#### `src`ディレクトリの概要

`src`ディレクトリにはプラグインのOTPアプリケーションのコードが含まれます。

##### `my_emqx_plugin.app.src`

標準的なErlangアプリケーション記述ファイルで、リリース時に`my_emqx_plugin.app`にコンパイルされます。

- アプリケーションのバージョンはリリースバージョンと一致する必要はなく、別のバージョニング方式を使えます。
- `applications`セクションに特に注意してください。プラグインはOTPアプリケーションとしてビルドされるため、プラグインの起動・停止・再起動はこのOTPアプリケーションに対して行います。プラグインが他のアプリケーションに依存する場合は、必ず`applications`セクションに記載してください。

##### `my_emqx_plugin_app.erl`

プラグインのアプリケーションの起動・停止を担当する[`application`ビヘイビア](https://www.erlang.org/doc/man/application.html)（`start/2`と`stop/1`関数）を実装するメインモジュールです。

`start/2`関数でよく行う処理は：

- EMQXのフックポイントへのフック登録
- CLIコマンドの登録
- 監督ツリーの起動

オプションで、`_app.erl`モジュールは`on_config_changed/2`や`on_health_check/1`コールバック関数を実装できます。

- `on_config_changed/2`はDashboard、API、CLIからプラグイン設定が変更された際に呼ばれます。
- `on_health_check/1`はプラグインの状態が要求された際に呼ばれ、プラグインの状態を報告できます。

#### その他のファイル

`my_emqx_plugin_cli.erl`モジュールはプラグインのCLIコマンドを実装します。登録されると`emqx ctl`コマンド経由で呼ばれます。

`my_emqx_plugin_sup.erl`はプラグインの典型的なスーパーバイザーを実装します。

`my_emqx_plugin.erl`はプラグインのメインモジュールで、プラグインのロジックを実装します。スケルトンではシンプルなログ出力を行うデモ用のフックをいくつか実装しています。その他のモジュールもプラグインに追加可能です。

::: tip 注意

アプリケーションモジュールやファイル名は任意ですが、以下の条件は満たす必要があります：

- アプリケーション名はプラグイン名と同じであること
- アプリケーションモジュール（`_app`）は`{plugin_name}_app`という名前であること
  :::

#### `priv`ディレクトリの概要

`priv`ディレクトリにはプラグインの設定ファイルやスキーマが格納されます。

##### `config.hocon`

プラグインの初期設定を[HOCON形式](https://github.com/lightbend/config/blob/master/HOCON.md)で記述したファイルです。`config.hocon.example`を参照用に利用できます。

##### `config_schema.avsc`

プラグイン設定のスキーマを[Avro形式](https://avro.apache.org/docs/1.11.1/specification/)で定義したファイルです。存在する場合、EMQXは設定更新時にこのスキーマに対してバリデーションを行います。`config.hocon`がスキーマに準拠しない場合、リリースビルドは失敗します。

さらにUIヒントを含めることで、EMQXダッシュボードからの対話的な設定が可能になります。参考は`config_schema.avsc.enterprise.example`です。

##### `config_i18n.json`

プラグイン設定UIの翻訳をJSON形式で格納したファイルです。例：

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

スケルトンが準備できたら、プラグインのロジック実装を開始します。通常、以下のロジックが必要です：

- フックとCLIコマンドの実装
- 設定更新の処理
- ヘルスチェックの処理

### フックとCLIコマンドの実装

EMQXは様々なイベント用のフックポイントを定義しています。任意のアプリケーション（プラグイン含む）はこれらのフックポイントにコールバックを登録し、イベントに反応したりデフォルト動作を変更できます。

最もよく使われるフックポイントはスケルトンファイルに用意されています。フックポイントの一覧、引数、期待される戻り値は[EMQXコード](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl)に記載されています。

フックポイントにコールバックを登録するには、`emqx_hooks:add/3`関数を使います。以下のパラメータを指定します：

- フックポイント名
- コールバックモジュールと関数、必要に応じてEMQXが渡す追加引数
- コールバックの優先度（通常は最優先の`?HP_HIGHEST`）

コールバックを解除するには、`emqx_hooks:del/2`関数にフックポイント名とコールバックモジュール/関数を渡します。

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
%% クライアントIDがA-Z、a-z、0-9、アンダースコアのいずれかの文字のみの場合に接続を許可
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

このコールバックでは通常：

- 新しい設定のバリデーション
- プラグインが稼働中なら変更への対応

を行います。

設定のバリデーション時は、アプリケーションがまだ起動していない可能性があるため、ステートレスなチェックを行い、ノード間で不整合を起こす環境依存チェックは避けてください。

プラグインが稼働中なら設定変更を適用できます。一般的なパターンは：

- アプリケーション起動時に`gen_server`を起動し設定を管理
- 例：`my_emqx_plugin_config_server`が現在の設定を読み込み状態を初期化
- `on_config_changed/2`は設定を検証し、新しい設定を`my_emqx_plugin_config_server`に送信
- サーバーが稼働中なら状態を更新し、そうでなければ何もしない

### ヘルスチェックの処理

`on_health_check/1`コールバックはEMQXがプラグイン状態を要求した際に呼ばれます。プラグインは以下のように状態を報告できます：

- 正常なら`ok`を返す
- 問題があればバイナリの理由を含む`{error, Reason}`を返す

外部リソースに依存し利用不可になる可能性があるプラグインでは、このコールバックが重要です。

詳細はスケルトンアプリの`my_emqx_plugin_app:on_health_check/1`を参照してください。

::: tip

この関数は稼働中のプラグインに対して呼ばれますが、起動や停止中の競合状態でも呼ばれる場合があります。

:::

実装例は[カスタムプラグインロジックの実装](./plugin-example.md)に多数あります。

### プラグインパッケージのビルド

以下のコマンドでプラグインのリリースを作成します：

```shell
$ cd my_emqx_plugin
$ make rel
```

これによりプラグインリリース`_build/default/emqx_plugin/my_emqx_plugin-1.0.0.tar.gz`が作成されます。このパッケージはプロビジョニングやインストールに使用可能です。

### パッケージ構成

プラグインをリリースとしてビルドすると、パッケージ構成は以下のようになります：

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

### 混在プラグインバージョン対応の設計

EMQX 6.2.4以降、`node.pinned_plugins`を使ってノードのデプロイ前にプラグインを事前配置・設定可能です。各ノードはピン留めプラグインを独立管理するため、同じプラグインの異なるバージョンをノード間で混在させてもバージョン不整合警告は発生しません。混在対応が必要な場合、以下の点で互換性を保つ必要があります：

- 共有データベーステーブルや保存データのフォーマット
- 異なるノードのプラグインインスタンス間でのRPC呼び出しやメッセージ
- プラグインバージョン間での設定フォーマット

デプロイやライフサイクルの挙動は[ノードピン留めプラグインの事前デプロイ](./plugin-management.md#pre-deploy-a-node-pinned-plugin)を参照してください。

## プラグイン拡張APIとUI

プラグインはEMQXプラグインAPIゲートウェイを通じてカスタムHTTPエンドポイントを公開でき、オプションでダッシュボードにネイティブUIを埋め込めます。

### プラグインHTTP API

プラグインAPIゲートウェイは以下のパスでプラグインへのリクエストをルーティングします：

```
/api/v5/plugin_api/{plugin_name}/...
```

EMQX 6.2.4以降、プラグインAPIゲートウェイのスコープチェックは制限付きの`plugin_api`スコープか管理者相当の`system`スコープのいずれかを受け入れます。呼び出し元のロールとネームスペースの権限も要求操作を許可している必要があります。

`plugin_api`はプラグインが公開するエンドポイントを呼び出す際に使い、プラグインのインストール・起動・停止・設定変更は`system`スコープで行います。後方互換性のためゲートウェイは`system`も引き続き受け入れます。

::: warning 重要なお知らせ

`plugin_api`スコープはゲートウェイ経由で公開されるすべてのエンドポイントへのアクセスを許可します。個々のプラグインエンドポイントの権限は制限しないため、プラグインが公開する各エンドポイントのセキュリティ影響を十分に検討してください。

:::

これらのリクエストを処理するには、プラグインアプリモジュールで`on_handle_api_call/4`を実装し、メソッドやパスでディスパッチします。参考実装は`plugins/emqx_username_quota/src/emqx_username_quota_app.erl`や`emqx_username_quota_api.erl`を参照してください。

#### コールバック契約

```erlang
on_handle_api_call(Method, PathRemainder, Request, Context) -> Result
```

| パラメータ       | 説明                                                                 |
| --------------- | ------------------------------------------------------------------- |
| `Method`        | `get \| post \| put \| patch \| delete`                             |
| `PathRemainder` | `{plugin_name}`以降のパスセグメントのバイナリリスト（パーセントデコード済み） |
| `Request`       | `query_string`、`headers`、および（非GET/DELETEの場合の）JSON形式の`body`を含むマップ |
| `Context`       | 認証メタデータとネームスペース情報を含むマップ                     |

許容される戻り値：

- `{ok, StatusCode, Headers, Body}`
- `{error, StatusCode, Headers, Body}`
- `{error, not_found}`

### ダッシュボードのプラグインネイティブUI

`emqx_plugin`メタデータに`index`フィールドが含まれる場合、EMQXダッシュボードはプラグインのネイティブUIをiframeで表示します。ダッシュボードはプラグインAPIのベースパスを先頭に付加します：

```
/api/v5/plugin_api/{plugin_name}{index}
```

例：`index: "/ui"`は`/api/v5/plugin_api/{plugin_name}/ui`に解決されます。

ネイティブUIを無効化するには、`index`フィールドを省略するか空文字列に設定してください。
