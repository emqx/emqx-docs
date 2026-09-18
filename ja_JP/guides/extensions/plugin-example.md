# プラグインロジックのカスタマイズ

このページでは、`src/my_emqx_plugin.erl` にあるデフォルトテンプレートのロジックを修正して、EMQXプラグインをカスタマイズする方法について説明します。テンプレートはデフォルトで全ての[利用可能なEMQXフック](./hooks.md)を登録しています。不要なフックは削除し、関連するコールバックに独自のロジックを実装してください。

## フック関数の登録

例えば、認証と認可のロジックを追加するには、`my_emqx_plugin:hook/0` 関数を以下のように登録します。

```erlang
hook() ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, []}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, []}, ?HP_HIGHEST).
```

ここで、`on_client_authenticate/2` はクライアント認証を処理し、`on_client_authorize/4` は認可を管理します。

1つのフック関数はEMQX本体とカスタマイズプラグインの両方からマウントされる場合があるため、プラグインにマウントする際は実行順序を指定する必要があります。`?HP_HIGHEST` は現在のフック関数が最も高い優先度で最初に実行されることを意味します。

## 例：アクセス制御ロジックの追加

以下は基本的なアクセス制御の実装例です。

```erlang
%% クライアントIDが A-Z, a-z, 0-9, アンダースコアのいずれかの文字列にマッチする場合のみ接続を許可する。
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.

%% クライアントは /room/{clientid} 形式のトピックのみサブスクライブ可能だが、任意のトピックにパブリッシュできる。
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result) -> {ok, Result}.
```

このロジックは以下を保証します。

- 有効なクライアントIDを持つクライアントのみ接続を許可
- クライアントは任意のトピックにパブリッシュ可能
- クライアントは自身の `/room/{clientid}` トピックのみサブスクライブ可能（簡易チャットルーム動作）

::: tip

- EMQX設定で `authorization.no_match = deny` を設定すると、マッチしないアクセスをブロックできます。

- ファイルベースの認可ルールについては、[ファイルベース認可ドキュメント](../access-control/authz/file.md)を参照してください。

:::

## 設定スキーマの追加（任意）

EMQX 5.7.0以降、プラグインの設定はREST API経由で動的に管理可能です。この機能を有効にし、設定のバリデーションを行うには、プラグインに以下を含める必要があります。

- 設定構造の検証用に、相対パス `priv/config_schema.avsc` にAvroスキーマ設定ファイルを配置します。このファイルは[Apache Avro仕様](https://avro.apache.org/docs/1.11.1/specification/)に準拠している必要があります。
- Avroスキーマのルールに準拠したデフォルト設定ファイルを `priv/config.hocon` に用意します。

実行時には、更新された設定が `data/plugins/<PLUGIN_NAME>/config.hocon` に保存され、旧設定ファイルは自動的にバックアップされます。

::: tip

プロジェクトディレクトリにある以下の例ファイルを参照してください。

- `priv/config.hocon.example`
- `priv/config_schema.avsc.example`
- `priv/config_schema.avsc.enterprise.example`（UI宣言を含む）
- `priv/config_i18n.json.example`（国際化対応用）

これらはプラグインの設定スキーマやUI構築のテンプレートとして利用できます。

:::

### 宣言的UIの定義（任意）

Avroスキーマには `$ui` フィールドを含めることができ、EMQXダッシュボード上で設定項目をどのように表示・編集するかを定義できます。プラグイン利用者は動的に生成されるフォームを通じて設定を編集可能です。

また、国際化（i18n）用の設定ファイル `priv/config_i18n.json` も任意で用意できます。このファイルは以下のようなキー・バリュー形式で構成されます。

```json
{
  "$msgid": {
    "zh": "消息",
    "en": "Message"
  }
}
```

フィールド名、説明、バリデーションルールのメッセージなど、`$ui` 設定内のUI要素に多言語対応を持たせるには、該当箇所に `$` プレフィックス付きの `$msgid` を使用してください。

**設定項目の説明**

宣言的UIコンポーネントはダッシュボード上で動的にフォームをレンダリングし、多様なフィールドタイプやカスタムコンポーネントに対応します。以下は利用可能なコンポーネントとその設定です。

- `component`  
  必須。異なる値や型のデータを表示・設定するためのコンポーネントタイプを指定します。対応コンポーネントは以下の通りです。

  | コンポーネント名       | 説明                                                         |
  | :--------------------- | :----------------------------------------------------------- |
  | `input`                | 短いテキストや文字列用のテキスト入力ボックス               |
  | `input-password`       | 入力内容を隠すパスワード入力ボックス                         |
  | `input-number`         | 数値のみ入力可能な数値入力ボックス                           |
  | `input-textarea`       | 長文入力用のテキストエリア                                   |
  | `input-array`          | カンマ区切りの値を入力可能な配列入力ボックス（文字列・数値配列対応） |
  | `switch`               | 真偽値用のトグルスイッチ                                     |
  | `select`               | 列挙型の選択肢を表示するドロップダウンボックス               |
  | `code-editor`          | SQLやJSONなど特定フォーマット向けのコードエディター         |
  | `key-value-editor`     | Avroマップのキー・バリュー編集用エディター                   |
  | `maps-editor`          | Avroオブジェクト配列の編集用エディター                       |

- `label`  
  必須。フィールドのラベルまたは名称を定義します。国際化対応には `$msgid` を使用可能です。i18nが設定されていない場合はそのままのテキストが表示されます。

- `description`  
  任意。フィールドの詳細説明を記述します。こちらも `$msgid` による国際化対応が可能です。未設定の場合はテキストがそのまま表示されます。

- `flex`  
  必須。グリッドレイアウトにおけるフィールドの幅を指定します。24が1行分の全幅、12は半分の幅を表します。

- `required`  
  任意。入力必須かどうかを示します。

- `format`（`code-editor` コンポーネントのみ）  
  任意。サポートするデータフォーマット（例：`sql`、`json`）を指定します。

- `options`（`select` コンポーネントのみ）  
  任意。選択可能なオプション一覧をAvroスキーマのシンボルに合わせて指定します。例：

  ```json
  [
    {
      "label": "$mysql",
      "value": "MySQL"
    },
    {
      "label": "$pgsql",
      "value": "postgreSQL"
    }
  ]
  ```

- `items`（`maps-editor` コンポーネントのみ）  
  任意。maps-editorコンポーネント使用時に、フォーム内の項目名と説明を指定します。例：

  ```json
  {
    "items": {
      "optionName": {
        "label": "$optionNameLabel",
        "description": "$optionDesc",
        "type": "string"
      },
      "optionValue": {
        "label": "$optionValueLabel",
        "description": "$optionValueDesc",
        "type": "string"
      }
    }
  }
  ```

- `rules`  
  任意。フィールドのバリデーションルールを定義します。複数のルールを設定可能です。対応するタイプは以下の通りです。

  - `pattern`：正規表現による検証を行います。
  - `range`：数値の範囲検証。最小値（`min`）と最大値（`max`）を同時または単独で指定可能です。
  - `length`：文字数の範囲検証。最小長（`minLength`）と最大長（`maxLength`）を同時または単独で指定可能です。
  - `message`：検証失敗時に表示するエラーメッセージ。多言語対応のため `$msgid` を使用できます。

**バリデーションルールの例**：

以下は例のスニペットです。詳細は `priv/config_schema.avsc.example` を参照してください。

```json
{
    "rules": [
    {
      "type": "pattern",
      "pattern": "^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])(\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9]))*$",
      "message": "$hostname_validate"
    }
  ]
}
```

```json
{
    "rules": [
    {
      "type": "range",
      "min": 1,
      "max": 65535,
      "message": "$port_range_validate"
    }
  ]
}
```

```json
{
    "rules": [
    {
      "type": "length",
      "minLength": 8,
      "maxLength": 128,
      "message": "$password_length_validate"
    },
    {
      "type": "pattern",
      "pattern": "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)[a-zA-Z\\d]*$",
      "message": "$password_validate"
    }
  ]
}
```

プラグインパッケージにAvroスキーマやi18nファイルを含めることで、プラグインのコンパイル・パッケージング時に組み込まれます。プラグインコード内では `emqx_plugins:get_config/1,2,3,4` 関数を使って設定値を取得できます。
