# Hot Upgrade (Relup)

このプラグインは、実行中のEMQXノードに対して `.relup` 形式のコード変更指示を適用することで、VMを再起動せずにパッチリリースを展開できるようにします。

オペレーターは各ノードで `emqx ctl relup ...` CLIを使用して操作を行います。クラスター全体への展開はオペレーターの責任であり（オーケストレーション機能は組み込まれていません）。

## 使用タイミング

ホットアップグレードは以下の場合に適しています。

- 適用したいホップが `emqx ctl relup list-supported-paths` でリストされている（宣言された `{from, target}` ホップのみサポート）。
- 次のノードに進む前にターゲットノードを検証できる。
- `data/` のバックアップがある。適用済みホップのインプレースロールバックはありません（[ロールバック](#rollback)参照）。

これらを満たせない場合は、通常のローリング再起動によるアップグレードを行ってください。

## オペレーターのワークフロー

### 1. プラグインのインストール

以下の[ダウンロード](#download)セクションからEMQXバージョンに合ったtarballを取得し、ダッシュボード（またはREST APIやCLI）から他のプラグインと同様にインストールします。

### 2. アップグレードパスのサポート確認

```bash
emqx ctl relup list-supported-paths
```

出力には、このプラグインバージョンの `priv/relup/` にバンドルされている `{from, target}` ホップが一覧表示されます。ホップがない場合、そのパスのホットアップグレードは利用できません。通常の再起動ベースのアップグレードに戻ってください。

### 3. 各ノードにターゲットリリースを配置

各ノードで、EMQXプロセスが読み取れるパスに以下の2つのファイルをコピーします。

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQXターゲットリリースのtarball
- `<tarball>.sha256`：sha256ダイジェスト。標準の `sha256sum` フォーマット（`<digest>  <filename>`）が受け入れられます。

### 4. アップグレードをトリガー

各ノードで以下を実行します。

```bash
emqx ctl relup upgrade <TarballPath> [--force]
```

ハンドラーは以下を行います。

- `<TarballPath>.sha256` と実際のダイジェストを照合し、不一致の場合は展開を拒否。
- `data/patches/` に `*.beam` ファイルがある場合は続行を拒否。このディレクトリは `vm.args -pa` を通じてコードパスに先行して追加されるため、アップグレードターゲットのモジュールより優先されます。ターゲットリリースにホットパッチ済みの修正が含まれている場合でも、古いbeamファイルが読み込まれる可能性があります。パッチファイルを削除するか、ターゲットリリースの上に適用し続ける意図がある場合のみ `--force` を指定してください。
- tarballを展開し、`releases/emqx_vars` から `REL_VSN` を読み込みます。
- `priv/relup/*.relup` から対応する `{from, target}` ホップを検索し、宣言されたコード変更指示とポストアップグレードコールバックを実行します。

### 5. ノードの検証

次の項目を確認してから次に進みます。

- `emqx ctl status` でノードが稼働中であること。
- `<RootDir>/relup/current` がターゲットバージョンと一致し、`<RootDir>/relup/<TargetVsn>/` に `bin/`、`erts-*/`、`lib/`、`releases/` が含まれていること。

次回の `emqx start` / `restart` 時に、`bin/emqx` ラッパーが `relup/current` を検出し、デプロイ済みツリー（新しいERTS、新しいbinスクリプト、新しいlib）にexecします。元の `<RootDir>` は `data/`、`etc/`、`log/`、`plugins/` の管理を継続します。

### 6. 成功後のクリーンアップ

クラスター全体がターゲットバージョンに移行したら、手動で配置したtarballと `.sha256` のサイドカーを削除してください。プラグインはソースパスを追跡しないため、プラグイン側にクリーンアップすべき状態はありません。

## アップグレード履歴

各ノードは `emqx_relup_log` テーブル（ディスクバックド、ローカルコンテンツ）に独自の監査履歴を保持します。履歴はプラグインのアンインストール後も残り、再インストールで再接続されます。

CLIで確認またはクリアできます。

```bash
emqx ctl relup logs           # 最近のアップグレード試行を表示
emqx ctl relup logs-clear     # このノードのログ行をすべて削除
```

## ロールバック

適用済みホップのインプレースロールバックはありません。ホットアップグレードはライブVMに対して `code_changes` を実行し、`post_upgrade_callbacks` がディスク上のデータを変更している可能性があるため、プラグインでの逆操作はサポートされていません。

実用的なフォールバック方法：

- **次の再起動前に**、アップグレードは成功したが新しいコードに問題があり、ディスク上のデータが旧リリースと互換性がある場合：

  ```bash
  rm <RootDir>/relup/current
  # 任意で: rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  ラッパーは元の `<RootDir>/bin/emqx` ツリーにフォールバックします。これは起動パスのみ回復し、問題のあったアップグレード時点のVM内のライブ状態は失われています。

- **それ以外の場合**、アップグレード前に取得した `data/`（mnesia、設定など）のバックアップから復元し、旧EMQXリリースを再インストールしてください。この点を踏まえてアップグレード期間を計画してください。

## ホップの作成（開発者向けメモ）

新しいホップを追加するには、対象の各リリースに対して以下を行います。

1. `priv/relup/<from>-to-<to>.relup` にホップの `code_changes` と `post_upgrade_callbacks` を宣言します。プラグインソースの `priv/relup/README.md` にスキーマ、サポートされる命令、ポストアップグレードコールバックの契約が記載されています。特に、新しいEMQXの `emqx_post_upgrade` に `pr_NNNNN_*` コールバックを追加する場合、relupホップはコールバック呼び出し前にそのモジュールをリロードする必要があります。または、このプラグインに `emqx_post_upgrade_<TargetVsn>.erl` としてコールバックを同梱してください。
2. このプラグインの `VERSION` を上げて再公開します。

プラグインは起動時にすべての `priv/relup/*.relup` を検証し、不正なエントリは警告ログを出してスキップします。致命的なエラーにはなりません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

各EMQXリリース用のtarball：

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.2.1 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_relup-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_relup-1.0.0.sha256)) |
| 6.2.2 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_relup-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_relup-1.0.0.sha256)) |
| 6.2.3 | 1.0.1 | [emqx_relup-1.0.1.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_relup-1.0.1.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_relup-1.0.1.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
