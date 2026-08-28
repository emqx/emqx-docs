# Mapping Tables

EMQX Mapping Tablesプラグインは、Rule SQL用の名前付きマッピングテーブルを提供します。ルールで安定した識別子、コード、またはバイナリフィールドIDを長い`CASE WHEN`式をSQL文内に維持せずに構造化された値にマッピングする必要がある場合に、このプラグインを使用してください。

このプラグインはEMQX 6.2.3以降で利用可能です。Rule SQL内で`maptab_lookup`を使用する前に、プラグインをインストールして起動してください。

マッピングテーブルは`emqx ctl maptabs` CLIを通じてJSONファイルからロードされ、EMQXのレプリケートされたデータベースに保存されます。ルールは`maptab_lookup` SQL関数でテーブルを照会します。関数のシグネチャやSQLの例については、[組み込みSQL関数](../../../data-integration/rule-sql-builtin-functions.md)を参照してください。

## Table Files

マッピングテーブルはJSONファイルです。拡張子`.json`を除いたファイル名がテーブル名として使用されます。テーブル名には英数字、アンダースコア、ハイフンのみ使用可能です。

JSONファイルは行オブジェクトの配列を含む必要があります。各行は`key`フィールドを含まなければなりません。行内のその他すべてのフィールドがその行の値マップを形成します。

例：

```json
[
  {
    "key": 1,
    "signal_name": "temperature_c",
    "start_bit": 17,
    "length": 8,
    "type": "integer",
    "signedness": "signed",
    "endian": "big"
  },
  {
    "key": 2,
    "signal_name": "pressure_kpa",
    "start_bit": 17,
    "length": 32,
    "type": "float",
    "signedness": "unsigned",
    "endian": "big"
  }
]
```

`key`はJSONの整数または文字列でなければなりません。JSONのネイティブ型が保持されるため、整数の`50`と文字列の`"50"`は異なるキーとして扱われます。

ロードはフェイルクローズ方式です。ファイルに以下の問題がある場合、EMQXはファイル全体を拒否し、以前のテーブルバージョンを保持します。

- 無効なJSON
- 配列でないトップレベルの値
- オブジェクトでない行
- `key`を持たない行
- 重複するキー
- float、boolean、null、配列、オブジェクト型のキー

## CLI Commands

`emqx ctl maptabs` CLIを使用してマッピングテーブルを管理します。

| コマンド | 説明 |
| --- | --- |
| `emqx ctl maptabs list` | ローカルノードにキャッシュされているテーブル一覧を表示。行数とバージョンも含む。 |
| `emqx ctl maptabs status` | 実行中のすべてのノードのテーブル一覧を表示。キャッシュのずれを検出するのに使用。 |
| `emqx ctl maptabs load <file>` | テーブルJSONファイルを検証し、全ノードにレプリケート。 |
| `emqx ctl maptabs reload` | すべての実行中ノードでストレージからキャッシュを再構築。キャッシュ同期が必要な場合に使用。 |
| `emqx ctl maptabs get <name>` | テーブルの保存されたJSONコンテンツを表示。 |
| `emqx ctl maptabs delete <name>` | 全ノードからテーブルを削除。 |

すべてのコマンド出力はJSONですが、`emqx ctl maptabs get <name>`はテーブルが存在する場合、保存されたテーブルのJSONコンテンツを直接出力します。

## Configuration

プラグインは標準のプラグイン設定API `PUT /api/v5/plugins/<name-vsn>/config` またはプラグイン設定ファイルを通じて設定します。

| 設定項目 | デフォルト | 説明 |
| --- | --- | --- |
| `max_tables` | `100` | マッピングテーブルの最大数。この制限を超える新規テーブルのロードは拒否されます。既存テーブルの置換は可能です。 |
| `max_rows_per_table` | `10000` | 1テーブルあたりの最大行数。これを超えるファイルは拒否されます。 |
| `max_table_file_bytes` | `10000000` | テーブルJSONファイルの最大サイズ（バイト）。これを超えるファイルはメモリに読み込まれる前に拒否され、レプリケーションされません。 |

制限はテーブルロード時にチェックされます。制限を変更しても既にロード済みのテーブルは削除や切り詰めされません。

## Cluster Behavior

プラグインはテーブル内容をEMQXの組み込みレプリケートデータベースに保存します。テーブルのロードや削除はクラスター内の全ノードにレプリケートされ、各ノードは保存されたテーブル内容からメモリ内キャッシュを再構築します。

クラスター内のすべてのノードにプラグインをインストールし起動してください。テーブルのロードや削除時にダウンしていたノードは再起動時にストレージからキャッシュを再構築し追いつきます。

キャッシュの更新はリーダーに対してアトミックです。ルールの照会は旧バージョンか新バージョンのいずれかのテーブルを参照し、部分的な更新は見ません。

## Access and Sharing

マッピングテーブルは管理者のみがCLIを通じて管理します。テーブルはテナントのネームスペースを超えて共有されます。照会はクライアントがマルチテナンシーのネームスペースに属しているかどうかに関わらず、すべてのクライアントに同じ行を返します。

テナントごとに異なる行が必要な場合は、テーブルデータにテナントをエンコードしてください。例えば、照会キーにテナントを含める方法があります。

```sql
maptab_lookup('signals', concat(client_attrs.tns, ':', item_id))
```

またはテナントごとに1つのテーブルを用意し、ルール内でテーブル名を構成する方法もあります。テーブル内のすべてのキーと照会箇所に同じ規約を適用してください。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

各EMQXリリース向けのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.2.3 | 0.1.2 | [emqx_maptabs-0.1.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_maptabs-0.1.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_maptabs-0.1.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
