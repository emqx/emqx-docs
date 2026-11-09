# ホットアップグレード（Relup）

このプラグインは、実行中のEMQXノードに対して `.relup` 形式のコード変更指示を適用することで、オペレーターがVMを再起動せずにパッチリリースを展開できるようにします。

オペレーターは各ノードで `emqx ctl relup ...` CLIを使って操作します。クラスター全体への展開はオペレーターの責任であり（オーケストレーション機能は組み込まれていません）。

## 使用タイミング

ホットアップグレードは以下の場合に適しています：

- 適用したいホップが `emqx ctl relup list-supported-paths` によってリストされている（宣言された `{from, target}` ホップのみサポート）。
- 次のノードに進む前にターゲットノードを検証できる。
- `data/` のバックアップがある。適用済みホップに対するインプレースのロールバックはありません（[ロールバック](#rollback)参照）。

これらを満たせない場合は、通常のローリング再起動を行ってください。

## オペレーターのワークフロー

### 1. プラグインのインストール

以下の[ダウンロード](#download)セクションからEMQXバージョンに合ったtarballを取得し、ダッシュボード（またはREST API / CLI）から他のプラグインと同様にインストールします。

### 2. アップグレードパスがサポートされていることを確認

```bash
emqx ctl relup list-supported-paths
```

出力には、このプラグインバージョンの `priv/relup/` にバンドルされている `{from, target}` ホップがリストされます。ホップがない場合、そのパスのホットアップグレードは利用できません。通常の再起動によるアップグレードに戻ってください。

### 3. 各ノードにターゲットリリースをステージング

各ノードに対して、EMQXプロセスが読み取れるパスに以下の2つのファイルをコピーします：

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQXターゲットリリースのtarball
- `<tarball>.sha256`：sha256ダイジェスト。標準の `sha256sum` フォーマット（`<digest>  <filename>`）が受け入れられます。

### 4. アップグレードをトリガー

各ノードで以下を実行します：

```bash
emqx ctl relup upgrade <TarballPath> [--force]
```

ハンドラーは以下を行います：

- `<TarballPath>.sha256` と実際のダイジェストを照合し、不一致の場合は展開を拒否。
- `data/patches/` に `*.beam` ファイルがある場合は続行を拒否。このディレクトリは `vm.args -pa` でコードパスの先頭に追加されるため、アップグレードターゲットのモジュールより優先されます。ターゲットリリースにホットパッチ済みの修正が含まれている場合でも、古いbeamファイルが読み込まれる可能性があります。パッチファイルを先に削除するか、ターゲットリリース上にパッチを維持したい場合のみ `--force` を指定してください。
- tarballを展開し、`releases/emqx_vars` から `REL_VSN` を読み取る。
- `priv/relup/*.relup` の一致する `{from, target}` ホップを検索し、宣言されたコード変更指示とポストアップグレードコールバックを実行。

### 5. ノードを検証

次の信号を確認してから次に進みます：

- `emqx ctl status` がノードが稼働中であることを報告。
- `<RootDir>/relup/current` がターゲットバージョンと一致し、`<RootDir>/relup/<TargetVsn>/` に `bin/`、`erts-*/`、`lib/`、`releases/` が含まれている。

次回の `emqx start` / `restart` で、`bin/emqx` ラッパーは `relup/current` を検出し、展開済みツリー（新しいERTS、新しいbinスクリプト、新しいlib）にexecします。元の `<RootDir>` は `data/`、`etc/`、`log/`、`plugins/` の権限を保持します。

### 6. 成功後のクリーンアップ

クラスター全体がターゲットバージョンに揃ったら、ステージングしたtarballとその `.sha256` サイドカーを手動で削除してください。プラグインはソースパスを追跡しないため、プラグイン側にクリーンアップすべき状態はありません。

## アップグレード履歴

各ノードは `emqx_relup_log` テーブル（ディスクバック、ローカルコンテンツ）に独自の監査履歴を保持します。履歴はプラグインアンインストール後も残り、再インストールで再接続され、行はそのままです。

CLIで確認またはクリアできます：

```bash
emqx ctl relup logs           # 最近のアップグレード試行を表示
emqx ctl relup logs-clear     # このノードのログ行をすべて削除
```

## ロールバック

適用済みホップに対するインプレースのロールバックはありません。ホットアップグレードはライブVMに対して `code_changes` を実行し、`post_upgrade_callbacks` がディスク上のデータを変更している可能性があるため、プラグインではその逆操作をサポートしていません。

実用的なフォールバック手段：

- **次回再起動前に**、アップグレードが成功したが新コードが不具合を起こし、ディスク上のデータが旧リリースと互換性がある場合：

  ```bash
  rm <RootDir>/relup/current
  # 任意：rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  ラッパーは元の `<RootDir>/bin/emqx` ツリーにフォールバックします。これは起動パスのみ回復し、アップグレード時のVM内のライブ状態は既に失われています。

- **それ以外の場合**、アップグレード前の `data/`（mnesia、設定など）バックアップから復元し、旧EMQXリリースを再インストールしてください。この点を考慮してアップグレードウィンドウを計画してください。

## ホップの作成（開発者向けメモ）

新しいホップを追加するには、必要な各リリースに対して：

1. `priv/relup/<from>-to-<to>.relup` を追加し、ホップの `code_changes` と `post_upgrade_callbacks` を宣言します。プラグインソースの `priv/relup/README.md` にスキーマ、サポートされる命令、ポストアップグレードコールバックの契約が記載されています。特に新しいEMQXの `emqx_post_upgrade` に `pr_NNNNN_*` コールバックを追加する場合、relupホップはコールバックを呼び出す前にそのモジュールをリロードする必要があります。または、このプラグインに `emqx_post_upgrade_<TargetVsn>.erl` としてコールバックを同梱してください。
2. このプラグインの `VERSION` を更新し、再公開します。

プラグインはアプリ起動時にすべての `priv/relup/*.relup` を検証し、不正なエントリには警告をログに出します。悪いファイルはスキップされ、致命的にはなりません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース用のtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.2.1 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_relup-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_relup-1.0.0.sha256)) |
| 6.2.2 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_relup-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_relup-1.0.0.sha256)) |
| 6.2.3 | 1.0.1 | [emqx_relup-1.0.1.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_relup-1.0.1.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_relup-1.0.1.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
