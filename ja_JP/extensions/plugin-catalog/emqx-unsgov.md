# UNS Governance

このプラグインは、ACLチェック時にUnified Namespaceトピック構造を強制します。

## プラグインAPI

ベースパス: `/api/v5/plugin_api/emqx_unsgov`

## ブートストラップモデル

- 起動時に、UNS Governanceは `priv/bootstrap_models/*.json` をスキャンします。
- 各ブートストラップモデルについて：
  - その `id` がデータベースに存在しない場合、プラグインはモデルを保存し、アクティブとしてマークします。
  - 既にデータベースに `id` が存在する場合、プラグインは読み込みをスキップし、infoレベルでログを記録します。
- バンドルされたデフォルトのブートストラップモデル：`priv/bootstrap_models/model-v1.json`

> 注意：ブートストラップモデルはクラスター内で最初のプラグイン起動時にデータベースにロードされます。以降のプラグインやノードの再起動では再ロードされません。モデルの更新はAPIを使用してDB内のモデルストアを更新してください。

### JSONデータエンドポイント

- `GET /status`：プラグインのステータス（on_mismatch、exempt_topics）。
- `GET /stats`：クラスター集約カウンターと最近のドロップ情報。
- `GET /models`：保存されている全モデルの一覧（各エントリに `active` フラグ付き）。
- `GET /models/:id`：IDで特定のモデルを取得。存在しない場合は404。
- `POST /models`：モデルの作成または更新。オプションで `activate` フラグ。
- `POST /models/:id/activate`：保存済みモデルをアクティブ化。
- `POST /models/:id/deactivate`：モデルを非アクティブ化。
- `DELETE /models/:id`：保存済みモデルを削除。
- `POST /validate/topic`：アクティブモデルに対してトピックを検証。

### その他のエンドポイント

- `GET /ui`：インタラクティブなモデルエディターUI。
- `GET /metrics`：Prometheusテキストエクスポート形式。

## UNSモデルスキーマ

このセクションでは、UNS Governanceが受け入れる完全なモデルJSONフォーマットを定義します。

### トップレベルキー

- `id`（必須、文字列）：モデルID。`^[A-Za-z0-9_-]+$` にマッチする必要があります。評価順序はIDのアルファベット順で制御されます。
- `name`（任意、文字列）：モデル表示名。省略時は `id` と同じになります。
- `variable_types`（任意、オブジェクト）：再利用可能な変数制約。
- `tree`（必須、オブジェクト）：トピックツリー定義。
- `payload_types`（任意、オブジェクト）：再利用可能なペイロードスキーマ。

### `variable_types`

変数タイプ名を制約オブジェクトにマッピングします。

サポートされる形式：
- 文字列正規表現マッチャー：
  - `{"type":"string","pattern":"^...$"}`
- 列挙型マッチャー：
  - `{"type":"enum","values":["A","B","C"]}`

変数タイプが欠落または無効な場合、マッチャーは寛容な `any` にフォールバックします。

### `payload_types`

ペイロードスキーマ名をスキーマオブジェクトにマッピングします。

検証にはJSON Schemaを使用し、以下の互換パッチがあります：
- トップレベルの `type` が省略されている場合、UNS Governanceは `"object"` にパッチします。
- トップレベルのペイロードスキーマはオブジェクトルートでなければなりません。プリミティブルートは拒否されます。

これにより以下が可能です：
- 完全な自己完結型オブジェクトJSON Schema。
- 既存の省略形オブジェクトスキーマ（例：`required`/`properties`のみ）。

エンドポイントのペイロードバインディング：
- エンドポイントの `_payload` は `payload_types` のキー、または `"any"`（ペイロード検証をスキップ）を参照できます。

### `tree`

`tree` は、各キーがルートトピックセグメント、値がノードオブジェクトのオブジェクトです。

ノードオブジェクトのキー：
- `children`（任意、オブジェクト）：子セグメントマップ。
- `_payload`（任意、文字列）：エンドポイントノードのペイロードタイプ名。デフォルトは `"any"`。
- `_type`（任意、互換性用）：明示的な `namespace | variable | endpoint`。
- `_var_type`（任意、互換性用）：変数タイプ名。

ノードタイプ推論：
- `children` が存在する場合：ノードは非エンドポイント。
- `children` がない場合：ノードはエンドポイント。
- 非エンドポイントキーの場合：
  - キー `{name}` → 変数ノード
  - キー `+` → 変数ワイルドカードノード
  - その他のキー → ネームスペースノード

変数タイプ解決：
- キー `{name}` の場合：
  - `_var_type` があれば使用
  - なければ推論されたタイプ名 `name` を使用
- キー `+` の場合：
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
  - `topic_nomatch`：アクティブなモデルのトピックフィルターがトピックにマッチしなかった。
    （モデル固有の検証は実行されません。）
    アクティブモデルが存在せずUNS Governanceが有効な場合、トピックは
    `topic_nomatch` としてフェイルクローズされます（`exempt_topics`を除く）。
  - `topic_invalid`：選択されたモデルフィルターはマッチしたが、トピックがモデルの構造・セグメント制約に違反。
  - `not_endpoint`：選択されたモデルはトピックパスにマッチしたが、対象ノードがエンドポイントではない。
  - QoS 0：メッセージは無視されます。
  - QoS 1/2：パブリッシュは拒否され、クライアントにプロトコル理由コード（`Not Authorized`）が返されます。
  - EMQXの `authorization.deny_action` が `disconnect` に設定されている場合、トピック認可失敗時にクライアントは切断されます（設定は `disconnect` であり `drop` ではありません）。
  - `authorization.deny_action` がデフォルトの `ignore` の場合は切断されず、QoS 1/2は拒否理由コードを受け取ります。
  - 観測可能なカウンター：`messages_dropped`、`topic_nomatch`、`topic_invalid`、`not_endpoint`、およびモデルごとのカウンター `per_model`。

- ペイロード違反（`payload_invalid`）：
  - メッセージはUNS Governanceによってパブリッシュ処理中にドロップされます。
  - この経路では認可拒否や切断は不要です。
  - 観測可能なカウンター：`messages_dropped`、`payload_invalid`、およびモデルごとのカウンター `per_model`。

## トピックフィルタープリチェック

複数のモデルがアクティブな場合、UNS Governanceは完全検証の前にモデルをプリスクリーニングします：

- 各モデルはツリーパスから派生したトピックフィルターパターンにコンパイルされます。
- 変数セグメントは単一レベルワイルドカード（`+`）に変換されます。
  - 例：`foo/{bar}/x` は `foo/+/x` になります。
- アクティブモデルはモデルID順に並べられます。
- UNS Governanceはパブリッシュトピックにマッチする最初のモデル（ID順）を選択します。
- プリチェックは直接のトピック/フィルターマッチのみを使用し、パブリッシュトピックのプレフィックス展開（例：`/#`の付加）は行いません。
- 選択されたモデルのみが完全検証され、次のモデルには進みません。
- プリチェックに失敗したモデルはスキップされ、モデルごとのドロップカウンターには寄与しません。

これにより無関係なアクティブモデルによるカウンターの膨張を防ぎ、モデル動作の決定性を保ちます。また、モデル間でトピックツリーの重複は避けるべきです。

## カウンター

`GET /stats` はクラスター集約カウンターを返します。

トップレベルカウンター：
- `messages_total`：処理されたメッセージ合計（`messages_allowed + messages_dropped`）。免除トラフィックも含む。
- `messages_allowed`：許可されたメッセージと免除メッセージの合計。
- `messages_dropped`：UNS検証失敗によるドロップ／拒否メッセージ。
- `topic_nomatch`：アクティブモデルフィルターにマッチしなかったためのドロップ／拒否。
- `topic_invalid`：選択モデルのトピック不一致によるドロップ／拒否。
- `not_endpoint`：トピックが非エンドポイントノードにマッチしたためのドロップ／拒否。
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
- `record_allowed` は該当モデルの `messages_total` と `messages_allowed` をインクリメントします。
- トピック／ペイロードのドロップは `messages_total`、`messages_dropped`、および該当理由カウンターをインクリメントします。
- トピックフィルタープリチェックでマッチするモデルがない場合、グローバルに `topic_nomatch` がインクリメントされ、モデル別ドロップカウンターは増えません。
  これはアクティブモデルセットが空の場合も含みます。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース用のtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.2.0 | 0.1.2 | [emqx_unsgov-0.1.2.tar.gz](https://packages.emqx.io/emqx-plugins/6.2.0/emqx_unsgov-0.1.2.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
