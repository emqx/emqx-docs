# Mapping Tables

EMQX Mapping Tablesプラグインは、Rule SQL用の名前付きマッピングテーブルを提供します。SQL文内で長い`CASE WHEN`式を維持せずに、安定した識別子、コード、またはバイナリフィールドIDを構造化された値にマッピングする必要があるルールでこのプラグインを使用してください。

このプラグインはEMQX 6.3.0以降で利用可能です。`maptab_lookup`をRule SQLで使用する前に、プラグインをインストールして起動してください。

マッピングテーブルはJSONファイルから`emqx ctl maptabs` CLIを通じてロードされ、EMQXのレプリケートされたデータベースに格納されます。ルールは`maptab_lookup` SQL関数でテーブルを照会します。関数のシグネチャやSQL例については、[組み込みSQL関数](../../../../develop/data-integration/rule-sql-builtin-functions.md)を参照してください。

## Table Files

マッピングテーブルはJSONファイルです。ファイル名の`.json`拡張子を除いた部分がテーブル名として使用されます。テーブル名には英数字、アンダースコア、ハイフンのみ使用できます。

JSONファイルは行オブジェクトの配列を含む必要があります。各行は必ず`key`フィールドを含みます。行の他のすべてのフィールドはその行の値のマップを形成します。

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

`key`はJSONの整数または文字列でなければなりません。ネイティブのJSON型は保持されるため、整数の`50`と文字列の`"50"`は異なるキーとして扱われます。

ロードはフェイルクローズ方式です。以下の問題があるファイルはEMQXによって全体が拒否され、以前のテーブルバージョンが保持されます。

- 無効なJSON
- 配列でないトップレベルの値
- オブジェクトでない行
- `key`を持たない行
- 重複するキー
- 浮動小数点数、ブール値、null、配列、オブジェクト型のキー

## CLI Commands

マッピングテーブルの管理には`emqx ctl maptabs` CLIを使用します。

| コマンド | 説明 |
| --- | --- |
| `emqx ctl maptabs list` | ローカルノードにキャッシュされているテーブル一覧を、行数とバージョンと共に表示します。 |
| `emqx ctl maptabs status` | 全稼働ノードのテーブル一覧を表示します。キャッシュのずれを検出するのに使用します。 |
| `emqx ctl maptabs load <file>` | テーブルJSONファイルを検証し、すべてのノードにレプリケートします。 |
| `emqx ctl maptabs reload` | 全稼働ノードでストレージからキャッシュを再構築します。キャッシュを再同期する必要がある場合に使用します。 |
| `emqx ctl maptabs get <name>` | テーブルの保存されたJSON内容を表示します。 |
| `emqx ctl maptabs delete <name>` | すべてのノードからテーブルを削除します。 |

すべてのコマンド出力はJSON形式ですが、`emqx ctl maptabs get <name>`はテーブルが存在する場合、保存されたテーブルJSON内容を直接出力します。

## Configuration

プラグインは標準のプラグイン設定API `PUT /api/v5/plugins/<name-vsn>/config` またはプラグイン設定ファイルで設定します。

| 設定項目 | デフォルト | 説明 |
| --- | --- | --- |
| `max_tables` | `100` | 最大マッピングテーブル数。この制限を超える新規テーブルのロードは拒否されます。既存テーブルの置換は可能です。 |
| `max_rows_per_table` | `10000` | 1テーブルあたりの最大行数。これを超えるファイルは拒否されます。 |
| `max_table_file_bytes` | `10000000` | テーブルJSONファイルの最大サイズ（バイト）。これを超えるファイルはメモリ読み込み前に拒否され、レプリケーションされません。 |

制限はテーブルロード時にチェックされます。制限変更は既にロード済みのテーブルの削除や切り詰めを行いません。

## Cluster Behavior

プラグインはEMQXの組み込みレプリケートデータベースにテーブル内容を保存します。テーブルのロードや削除はクラスター内のすべてのノードにレプリケートされ、各ノードは保存されたテーブル内容からインメモリキャッシュを再構築します。

クラスター内のすべてのノードにプラグインをインストールして起動してください。テーブルロードや削除時にダウンしていたノードは、再起動時にストレージからキャッシュを再構築して追いつきます。

キャッシュの更新はリーダーに対してアトミックです。ルールの照会は古いテーブルバージョンか新しいテーブルバージョンのどちらかを参照し、中途半端な更新は見ません。

## Access and Sharing

マッピングテーブルは管理者がCLI経由でのみ管理します。テーブルはテナントのネームスペースを超えて共有されます。照会はクライアントがマルチテナンシーネームスペースに属しているかどうかに関わらず同じ行を返します。

もしテナントごとに異なる行が必要な場合は、テーブルデータにテナントをエンコードしてください。例えば、照会キーにテナントを含めます。

```sql
maptab_lookup('signals', concat(client_attrs.tns, ':', item_id))
```

またはテナントごとにテーブルを分けて、ルール内でテーブル名を構成する方法もあります。テーブル内のすべてのキーと照会箇所に同じ規約を適用してください。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

各EMQXリリース用のtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.3.0 | 0.1.2 | [emqx_maptabs-0.1.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_maptabs-0.1.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_maptabs-0.1.2.sha256)) |
| 6.3.1 | 0.1.2 | [emqx_maptabs-0.1.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_maptabs-0.1.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_maptabs-0.1.2.sha256)) |
| 6.3.2 | 0.1.2 | [emqx_maptabs-0.1.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.2/emqx_maptabs-0.1.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.2/emqx_maptabs-0.1.2.sha256)) |
| 6.3.3 | 0.1.2 | [emqx_maptabs-0.1.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_maptabs-0.1.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_maptabs-0.1.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
