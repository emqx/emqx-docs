# UNS Governance

このプラグインは、ACLチェック時にUnified Namespaceのトピック構造を強制し、UNS Governanceによって管理されているトピックにパブリッシュされたメッセージのペイロードを検証できます。

## プラグインAPI

ベースパス: `/api/v5/plugin_api/emqx_unsgov`

## ブートストラップモデル

- 起動時に、UNS Governanceは `priv/bootstrap_models/*.json` をスキャンします。
- 各ブートストラップモデルについて：
  - その `id` がデータベースに存在しない場合、プラグインはモデルを保存し、アクティブとしてマークします。
  - 既にデータベースに存在する `id` の場合、プラグインは読み込みをスキップし、情報レベルでログを記録します。
- バンドルされたデフォルトのブートストラップモデル：`priv/bootstrap_models/model-v1.json`

> 注意：ブートストラップモデルはクラスター内で最初のプラグイン起動時にデータベースにロードされます。以降のプラグインやノードの再起動では再ロードされません。モデルの更新はAPIを使ってDB内のモデルストアを更新してください。

### JSONデータエンドポイント

- `GET /status` — プラグインのステータス（on_mismatch、exempt_topics）。
- `GET /stats` — クラスター集計済みカウンターと最近のドロップ情報。
- `GET /models` — 保存されているすべてのモデル一覧（各エントリに `active` フラグを含む）。
- `GET /models/:id` — 指定IDのモデルを取得。存在しない場合は404。
- `POST /models` — モデルの作成または更新。オプションで `activate` フラグを指定可能。
- `POST /models/:id/activate` — 保存済みモデルをアクティブ化。
- `POST /models/:id/deactivate` — モデルを非アクティブ化。
- `DELETE /models/:id` — 保存済みモデルを削除。
- `POST /validate/topic` — アクティブモデルに対してトピックの検証を実施。

### その他のエンドポイント

- `GET /ui` — インタラクティブなモデルエディターUI。
- `GET /metrics` — Prometheusテキストエクスポート形式。

## UNSモデルスキーマ

このセクションでは、UNS Governanceが受け入れる完全なモデルJSONフォーマットを定義します。

### トップレベルキー

- `id`（必須、文字列）：モデルID。`^[A-Za-z0-9_-]+$` にマッチする必要があります。評価順はIDのアルファベット順で制御されます。
- `name`（任意、文字列）：モデルの表示名。省略時は `id` が使用されます。
- `variable_types`（任意、オブジェクト）：再利用可能な変数制約。
- `tree`（必須、オブジェクト）：トピックツリーの定義。
- `payload_types`（任意、オブジェクト）：再利用可能なペイロードスキーマ。

### `variable_types`

変数タイプ名から制約オブジェクトへのマップ。

サポートされる形式：
- 文字列正規表現マッチャー：
  - `{"type":"string","pattern":"^...$"}`
- 列挙型マッチャー：
  - `{"type":"enum","values":["A","B","C"]}`

変数タイプが欠落または無効な場合、マッチャーは寛容な `any` にフォールバックします。

### `payload_types`

ペイロードスキーマ名からスキーマオブジェクトへのマップ。

検証にはJSON Schemaを使用し、互換性のためのパッチが1つあります：
- トップレベルの `type` が省略された場合、UNS Governanceは `"object"` にパッチを当てます。
- トップレベルのペイロードスキーマはオブジェクトルートでなければなりません。プリミティブルートは拒否されます。

これにより以下が可能です：
- 完全な自己完結型のオブジェクトJSON Schema。
- 既存の省略形オブジェクトスキーマ（例：`required`/`properties`のみ）。

エンドポイントのペイロードバインディング：
- エンドポイントの `_payload` は `payload_types` のキー、またはペイロード検証をスキップする `"any"` を参照できます。

### `tree`

`tree` はルートトピックセグメントをキー、ノードオブジェクトを値とするオブジェクトです。

ノードオブジェクトのキー：
- `children`（任意、オブジェクト）：子セグメントのマップ。
- `_payload`（任意、文字列）：エンドポイントノードのペイロードタイプ名。デフォルトは `"any"`。
- `_type`（任意、互換性用）：明示的な `namespace | variable | endpoint`。
- `_var_type`（任意、互換性用）：変数タイプ名。

ノードタイプ推論：
- `children` が存在する場合：ノードは非エンドポイント。
- `children` がない場合：ノードはエンドポイント。
- 非エンドポイントのキー：
  - `{name}` => 変数ノード
  - `+` => 変数ワイルドカードノード
  - その他のキー => ネームスペースノード

変数タイプの解決：
- キーが `{name}` の場合：
  - `_var_type` があればそれを使用
  - なければ推論されたタイプ名 `name` を使用
- キーが `+` の場合：
  - マッチャーは `any`（1セグメントにマッチ）

ツリー内のワイルドカードキー：
- `+`：トピックセグメント1つに正確にマッチ。
- `#`：残りのトピックセグメントすべてにマッチ（0セグメントも含む）。

### 完全な例

```json
{
  "id": "model-v1",
  "name": "UNS Model V1",
  "variable_types": {
    "site_id": { "type": "string", "pattern": "^[A-Za-z][A-Za-z0-9_]{0,31}$" },
    "line_id": { "type": "string", "pattern": "^Line[0-9]{1,4}$" },
    "mode": { "type": "enum", "values": ["auto", "manual"] }
  },
  "payload_types": {
    "line_control": {
      "type": "object",
      "required": ["Status", "Mode"],
      "properties": {
        "Status": { "type": "string", "enum": ["running", "stopped"] },
        "Mode": { "type": "string", "enum": ["auto", "manual"] }
      },
      "additionalProperties": false
    }
  },
  "tree": {
    "default": {
      "children": {
        "{site_id}": {
          "children": {
            "Lines": {
              "children": {
                "{line_id}": {
                  "children": {
                    "LineControl": { "_payload": "line_control" }
                  }
                }
              }
            },
            "stream": {
              "children": {
                "#": { "_payload": "any" }
              }
            }
          }
        }
      }
    }
  }
}
```

## 強制動作

UNS Governanceはトピック構造と（オプションで）ペイロードスキーマの両方を検証します。

- トピック違反（`topic_nomatch`、`topic_invalid`、`not_endpoint`）：
  - `topic_nomatch`：アクティブなモデルのトピックフィルターがトピックにマッチしなかった場合。
    （モデル固有の検証は実行されません。）
    アクティブモデルがない場合でUNS Governanceが有効なら、トピックは `topic_nomatch` としてフェイルクローズされます（`exempt_topics` を除く）。
  - `topic_invalid`：選択されたモデルフィルターはマッチしたが、トピックがモデルの構造・セグメント制約に違反した場合。
  - `not_endpoint`：選択されたモデルはトピックパスにマッチしたが、対象ノードがエンドポイントではない場合。
  - QoS 0：メッセージは無視されます。
  - QoS 1/2：パブリッシュは拒否され、クライアントにプロトコル理由コード（`Not Authorized`）が返されます。
  - EMQXの `authorization.deny_action` が `disconnect` に設定されている場合、トピック認可失敗時にクライアントは切断されます（設定は `disconnect` であり `drop` ではありません）。
  - `authorization.deny_action` が `ignore`（デフォルト）の場合、切断は行われませんが、QoS 1/2は拒否理由コードを受け取ります。
  - 観測可能なカウンター：`messages_dropped`、`topic_nomatch`、`topic_invalid`、`not_endpoint`、およびモデルごとのカウンター `per_model`。

- ペイロード違反（`payload_invalid`）：
  - UNS Governanceで管理されているトピックにメッセージがパブリッシュされ、そのトピックにペイロードスキーマがある場合、ペイロード検証はパブリッシュ処理中にすべてのマッチするメッセージに対して実行されます。これには認可キャッシュにヒットしたパブリッシュや、ルールエンジンの再パブリッシュ、ブリッジのイングレスなどの非クライアントパブリッシュも含まれます。
  - メッセージはUNS Governanceによってパブリッシュ処理中にドロップされます。
  - このパスでは認可拒否や切断は不要です。
  - 観測可能なカウンター：`messages_dropped`、`payload_invalid`、およびモデルごとのカウンター `per_model`。

## トピックフィルタープリチェック

複数のモデルがアクティブな場合、UNS Governanceは完全検証の前にモデルをプリスクリーニングします：

- 各モデルはツリーパスから派生したトピックフィルターパターンにコンパイルされます。
- 変数セグメントは単一レベルワイルドカード（`+`）に変換されます。
  - 例：`foo/{bar}/x` は `foo/+/x` になります。
- アクティブモデルはモデルID順に並べられます。
- UNS Governanceはパブリッシュトピックにマッチする最初のモデル（ID順）を選択します。
- プリチェックは直接的なトピック/フィルターマッチのみを使用し、パブリッシュトピックのプレフィックスを暗黙的に展開しません（例：`/#` の付加はしません）。
- 選択されたモデルのみが完全に検証され、UNS Governanceは次のモデルには進みません。
- このプリチェックに失敗したモデルはスキップされ、モデルごとのドロップカウンターには寄与しません。

これにより無関係なアクティブモデルによるカウンターの膨張を防ぎ、モデルの動作を決定的に保ちます。また、モデル間でトピックツリーの重複は避けるべきです。

## カウンター

`GET /stats` はクラスター集計済みのカウンターを返します。

トップレベルカウンター：
- `messages_total`：処理されたメッセージの合計（`messages_allowed + messages_dropped`）。免除トラフィックも含む。
- `messages_allowed`：許可されたメッセージと免除されたメッセージの合計。
- `messages_dropped`：UNS検証失敗によりドロップ／拒否されたメッセージ。
- `topic_nomatch`：アクティブモデルのフィルターにマッチしなかったためドロップ／拒否。
- `topic_invalid`：選択されたモデルのトピック不一致によるドロップ／拒否。
- `not_endpoint`：トピックが非エンドポイントノードにマッチしたためドロップ／拒否。
- `payload_invalid`：ペイロードスキーマ不一致によるドロップ。
- `exempt`：`exempt_topics` によってスキップされたメッセージ。
- `per_model`：モデルIDをキーとするモデル別内訳マップ。
- `recent_drops`：最近のドロップイベント（`topic`、`error_type`、`error_detail`、`timestamp_ms`）。

モデル別カウンター（`per_model.<model_id>`）：
- `messages_total`
- `messages_allowed`
- `messages_dropped`
- `topic_invalid`
- `not_endpoint`
- `payload_invalid`

カウンターの意味：
- `record_allowed` はマッチしたモデルの `messages_total` と `messages_allowed` をインクリメントします。
- トピック／ペイロードのドロップは、`messages_total`、`messages_dropped`、および選択されたモデルの該当理由カウンターをインクリメントします。
- トピックフィルタープリチェックでどのモデルもパスしなかった場合、グローバルで `topic_nomatch` がインクリメントされ、モデル別ドロップカウンターはインクリメントされません。
  これはアクティブモデルセットが空の場合も含みます。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリースのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.2 | 0.1.3 | [emqx_unsgov-0.1.3.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_unsgov-0.1.3.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_unsgov-0.1.3.sha256)) |
| 6.1.3 | 0.1.3 | [emqx_unsgov-0.1.3.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_unsgov-0.1.3.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_unsgov-0.1.3.sha256)) |
| 6.1.4 | 0.1.3 | [emqx_unsgov-0.1.3.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_unsgov-0.1.3.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_unsgov-0.1.3.sha256)) |
| 6.1.5 | 0.1.4 | [emqx_unsgov-0.1.4.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.5/emqx_unsgov-0.1.4.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.5/emqx_unsgov-0.1.4.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
