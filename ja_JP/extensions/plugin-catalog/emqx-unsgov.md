# UNS Governance

このプラグインはACLチェック時にUnified Namespaceトピック構造を強制します。

## プラグインAPI

ベースパス: `/api/v5/plugin_api/emqx_unsgov`

## ブートストラップモデル

- 起動時に、UNS Governanceは`priv/bootstrap_models/*.json`をスキャンします。
- 各ブートストラップモデルについて：
  - その`id`がデータベースに存在しない場合、プラグインはモデルを保存し、アクティブとしてマークします。
  - 既にデータベースに`id`が存在する場合、プラグインは読み込みをスキップし、情報レベルでログを記録します。
- バンドルされたデフォルトのブートストラップモデル：`priv/bootstrap_models/model-v1.json`

> 注意：ブートストラップモデルはクラスター内で最初のプラグイン起動時にデータベースにロードされます。以降のプラグインまたはノードの再起動では再ロードされません。モデルの更新はAPIを使用してデータベース内のモデルストアを更新してください。

### JSONデータエンドポイント

- `GET /status`：プラグインのステータス（on_mismatch、exempt_topics）。
- `GET /stats`：クラスター集約されたカウンターと最近のドロップ情報。
- `GET /models`：保存されているすべてのモデルの一覧（各エントリに`active`フラグを含む）。
- `GET /models/:id`：IDで特定のモデルを取得。存在しない場合は404。
- `POST /models`：モデルの作成または更新。オプションで`activate`フラグ指定可能。
- `POST /models/:id/activate`：保存されたモデルをアクティブ化。
- `POST /models/:id/deactivate`：モデルを非アクティブ化。
- `DELETE /models/:id`：保存されたモデルを削除。
- `POST /validate/topic`：アクティブなモデルに対してトピックを検証。

### その他のエンドポイント

- `GET /ui`：インタラクティブなモデルエディターUI。
- `GET /metrics`：Prometheusテキストエクスポート形式。

## UNSモデルスキーマ

このセクションでは、UNS Governanceが受け入れる完全なモデルJSONフォーマットを定義します。

### トップレベルキー

- `id`（必須、文字列）：モデルID。`^[A-Za-z0-9_-]+$`にマッチする必要があります。評価順序はIDのアルファベット順で制御されます。
- `name`（任意、文字列）：モデルの表示名。デフォルトは`id`。
- `variable_types`（任意、オブジェクト）：再利用可能な変数制約。
- `tree`（必須、オブジェクト）：トピックツリー定義。
- `payload_types`（任意、オブジェクト）：再利用可能なペイロードスキーマ。

### `variable_types`

変数タイプ名から制約オブジェクトへのマップ。

サポートされる形式：
- 文字列の正規表現マッチャー：
  - `{"type":"string","pattern":"^...$"}`
- 列挙型マッチャー：
  - `{"type":"enum","values":["A","B","C"]}`

変数タイプが欠落または無効な場合、マッチャーは許容的な`any`にフォールバックします。

### `payload_types`

ペイロードスキーマ名からスキーマオブジェクトへのマップ。

検証はJSON Schemaを使用し、1つの互換性パッチがあります：
- トップレベルの`type`が省略されている場合、UNS Governanceは`"object"`にパッチを当てます。
- トップレベルのペイロードスキーマはオブジェクトルートでなければなりません。プリミティブルートは拒否されます。

これにより以下が可能です：
- 完全な自己完結型のオブジェクトJSON Schema。
- 既存の省略形オブジェクトスキーマ（例：`required`/`properties`のみ）。

エンドポイントのペイロードバインディング：
- エンドポイントの`_payload`は`payload_types`のキーを参照するか、検証をスキップするために`"any"`を指定可能です。

### `tree`

`tree`は、各キーがルートトピックセグメントであり、各値がノードオブジェクトであるオブジェクトです。

ノードオブジェクトのキー：
- `children`（任意、オブジェクト）：子セグメントのマップ。
- `_payload`（任意、文字列）：エンドポイントノードのペイロードタイプ名。デフォルトは`"any"`。
- `_type`（任意、互換性用）：明示的な`namespace | variable | endpoint`。
- `_var_type`（任意、互換性用）：変数タイプ名。

ノードタイプの推論：
- `children`が存在する場合：ノードは非エンドポイント。
- `children`がない場合：ノードはエンドポイント。
- 非エンドポイントキーの場合：
  - キーが`{name}`の場合：変数ノード
  - キーが`+`の場合：変数ワイルドカードノード
  - その他のキーはネームスペースノード

変数タイプの解決：
- キーが`{name}`の場合：
  - `_var_type`があれば使用
  - なければ推論されたタイプ名`name`を使用
- キーが`+`の場合：
  - マッチャーは`any`（1セグメントにマッチ）

ツリー内のワイルドカードキー：
- `+`：正確に1つのトピックセグメントにマッチ。
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
    アクティブモデルが存在せずUNS Governanceが有効な場合、`exempt_topics`を除きトピックは`topic_nomatch`としてフェイルクローズされます。
  - `topic_invalid`：選択されたモデルのフィルターはマッチしたが、トピックがモデルの構造・セグメント制約に違反。
  - `not_endpoint`：選択されたモデルがトピックパスにマッチしたが、対象ノードがエンドポイントでない。
  - QoS 0：メッセージは無視されます。
  - QoS 1/2：パブリッシュは拒否され、クライアントにプロトコル理由コード（`Not Authorized`）が返されます。
  - EMQXの`authorization.deny_action`が`disconnect`に設定されている場合、トピック認可失敗時にクライアントは切断されます（設定は`disconnect`であり`drop`ではありません）。
  - `authorization.deny_action`が`ignore`（デフォルト）の場合、切断は行われませんが、QoS 1/2は拒否理由コードを受け取ります。
  - 観測可能なカウンター：`messages_dropped`、`topic_nomatch`、`topic_invalid`、`not_endpoint`、および`per_model`内のモデル別カウンター。

- ペイロード違反（`payload_invalid`）：
  - パブリッシュ処理中にUNS Governanceがメッセージをドロップします。
  - このパスでは認可拒否や切断は不要です。
  - 観測可能なカウンター：`messages_dropped`、`payload_invalid`、および`per_model`内のモデル別カウンター。

## トピックフィルタープリチェック

複数のモデルがアクティブな場合、UNS Governanceは完全な検証前にモデルをプリスクリーニングします：

- 各モデルはツリーパスから派生したトピックフィルターパターンにコンパイルされます。
- 変数セグメントは単一レベルワイルドカード（`+`）に変換されます。
  - 例：`foo/{bar}/x`は`foo/+/x`になります。
- アクティブモデルはモデルID順に並べられます。
- UNS Governanceはパブリッシュトピックにマッチする最初のモデル（ID順）を選択します。
- プリチェックは直接のトピック／フィルターマッチングのみを使用し、パブリッシュトピックのプレフィックス展開（例：`/#`の付加）は行いません。
- 選択されたモデルのみが完全検証され、UNS Governanceは次のモデルに進みません。
- プリチェックに失敗したモデルはスキップされ、モデル別ドロップカウンターに寄与しません。

これにより無関係なアクティブモデルによるカウンターの膨張を防ぎ、モデルの動作を決定論的に保ちます。また、モデル間でのトピックツリーの重複は避けるべきです。

## カウンター

`GET /stats`はクラスター集約されたカウンターを返します。

トップレベルカウンター：
- `messages_total`：処理されたメッセージの合計（`messages_allowed + messages_dropped`）。免除トラフィックも含む。
- `messages_allowed`：許可されたメッセージと免除されたメッセージの合計。
- `messages_dropped`：UNS検証失敗によりドロップ／拒否されたメッセージ。
- `topic_nomatch`：アクティブモデルフィルターにマッチしなかったためドロップ／拒否。
- `topic_invalid`：選択モデルのトピック不一致によるドロップ／拒否。
- `not_endpoint`：トピックが非エンドポイントノードにマッチしたためドロップ／拒否。
- `payload_invalid`：ペイロードスキーマ不一致によるドロップ。
- `exempt`：`exempt_topics`によってスキップされたメッセージ。
- `per_model`：モデルIDをキーとしたモデル別内訳マップ。
- `recent_drops`：最近のドロップイベント（`topic`、`error_type`、`error_detail`、`timestamp_ms`）。

モデル別カウンター（`per_model.<model_id>`）：
- `messages_total`
- `messages_allowed`
- `messages_dropped`
- `topic_invalid`
- `not_endpoint`
- `payload_invalid`

カウンターの意味：
- `record_allowed`はマッチしたモデルの`messages_total`と`messages_allowed`をインクリメントします。
- トピック／ペイロードのドロップは`messages_total`、`messages_dropped`、および選択モデルの該当理由カウンターをインクリメントします。
- トピックフィルタープリチェックでモデルが1つも通過しなかった場合、`topic_nomatch`がグローバルにインクリメントされ、モデル別ドロップカウンターは増加しません。
  これはアクティブモデルセットが空の場合も含みます。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリースのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.2.0 | 0.1.2 | [emqx_unsgov-0.1.2.tar.gz](https://packages.emqx.io/emqx-plugins/6.2.0/emqx_unsgov-0.1.2.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
