# Hot Upgrade (Relup)

このプラグインは、実行中のEMQXノードに対して`.relup`形式のコード変更指示セットを適用し、オペレーターがVMを再起動せずにパッチリリースを展開できるようにします。

オペレーターは各ノードで`emqx ctl relup ...` CLIを使って操作します。クラスター全体への展開はオペレーターの責任であり（オーケストレーション機能は組み込まれていません）。

## 使用タイミング

ホットアップグレードは以下の場合に適しています：

- 適用したいホップが`emqx ctl relup list-supported-paths`でリストされている（宣言された`{from, target}`ホップのみサポート）。
- 次に進む前にターゲットノードを検証できる。
- `data/`のバックアップがある。適用済みホップのインプレースロールバックはありません（[ロールバック](#rollback)参照）。

これらを満たせない場合は、通常のローリング再起動によるアップグレードを行ってください。

## オペレーターの作業手順

### 1. プラグインのインストール

以下の[ダウンロード](#download)セクションからEMQXのバージョンに合ったtarballを取得し、ダッシュボード（またはREST API / CLI）から他のプラグインと同様にインストールします。

### 2. アップグレードパスがサポートされていることを確認

```bash
emqx ctl relup list-supported-paths
```

出力には、このプラグインバージョンの`priv/relup/`にバンドルされている`{from, target}`ホップが一覧表示されます。ホップが見つからない場合、そのパスのホットアップグレードは利用できません。通常の再起動ベースのアップグレードに戻ってください。

### 3. 各ノードにターゲットリリースを配置

各ノードに対し、EMQXプロセスが読み取れるパスに以下の2ファイルをコピーします：

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQXターゲットリリースのtarball
- `<tarball>.sha256`：sha256ダイジェスト。標準の`sha256sum`フォーマット（`<digest>  <filename>`）が受け入れられます。

### 4. アップグレードを実行

各ノードで以下を実行します：

```bash
emqx ctl relup upgrade <TarballPath> [--force]
```

ハンドラーは以下を行います：

- `<TarballPath>.sha256`のダイジェストを実際のファイルと照合し、不一致の場合は展開を拒否。
- `data/patches/`に`*.beam`ファイルがある場合は続行を拒否。このディレクトリは`vm.args -pa`でコードパスの先頭に追加されるため、アップグレードターゲットのモジュールより優先されます。ターゲットリリースにホットパッチ済みの修正が含まれている場合、古いbeamファイルが読み込まれる可能性があります。パッチファイルを削除するか、ターゲットリリース上に適用済みのまま維持したい場合のみ`--force`を指定してください。
- tarballを展開し、`releases/emqx_vars`から`REL_VSN`を読み取る。
- `priv/relup/*.relup`内の一致する`{from, target}`ホップを検索し、宣言されたコード変更指示とポストアップグレードコールバックを実行。

### 5. ノードを検証

次の状態を確認してから次に進みます：

- `emqx ctl status`でノードが稼働中であることが報告される。
- `<RootDir>/relup/current`の内容がターゲットバージョンと一致し、`<RootDir>/relup/<TargetVsn>/`に`bin/`、`erts-*/`、`lib/`、`releases/`が存在する。

次回の`emqx start`または`restart`時に、`bin/emqx`ラッパーが`relup/current`を検出し、デプロイ済みツリー（新しいERTS、新しいbinスクリプト、新しいlib）にexecします。元の`<RootDir>`は`data/`、`etc/`、`log/`、`plugins/`の管理権限を保持します。

### 6. 成功後のクリーンアップ

クラスター全体がターゲットバージョンに移行したら、手動で配置したtarballとその`.sha256`ファイルを削除してください。プラグインはソースパスを追跡しないため、プラグイン側にクリーンアップすべき状態はありません。

## アップグレード履歴

各ノードは`emqx_relup_log`テーブル（ディスクバック、ローカル内容）に独自の監査履歴を保持します。履歴はプラグインアンインストール後も残り、再インストールで再接続されます。

CLIで確認・削除可能です：

```bash
emqx ctl relup logs           # 最近のアップグレード試行を表示
emqx ctl relup logs-clear     # このノードのログ行をすべて削除
```

## ロールバック

適用済みホップのインプレースロールバックはありません。ホットアップグレードはライブVMに対して`code_changes`を実行し、`post_upgrade_callbacks`がディスク上のデータを変更している可能性があるため、プラグインでの逆操作はサポートされていません。

実用的なフォールバック方法：

- **次回再起動前**に、アップグレードが成功したが新コードに問題があり、ディスク上のデータが旧リリースと互換性がある場合：

  ```bash
  rm <RootDir>/relup/current
  # 必要に応じて: rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  ラッパーは元の`<RootDir>/bin/emqx`ツリーにフォールバックします。これは起動パスのみ回復し、アップグレード時のVM内のライブ状態は既に失われています。

- **それ以外の場合**は、アップグレード前にバックアップした`data/`（mnesia、設定など）から復元し、旧EMQXリリースを再インストールしてください。アップグレードの計画時にこれを考慮してください。

## ホップの作成（開発者向けメモ）

新しいホップを追加するには、必要な各リリースに対して：

1. `priv/relup/<from>-to-<to>.relup`を作成し、ホップの`code_changes`と`post_upgrade_callbacks`を宣言します。プラグインソースの`priv/relup/README.md`にスキーマ、サポートされる命令、ポストアップグレードコールバックの契約が記載されています。特に、新しいEMQXの`emqx_post_upgrade`に`pr_NNNNN_*`コールバックを追加する場合、relupホップはコールバック呼び出し前にそのモジュールをリロードするか、このプラグインに`emqx_post_upgrade_<TargetVsn>.erl`としてコールバックを同梱する必要があります。
2. このプラグインの`VERSION`を更新し、再公開します。

プラグインは起動時にすべての`priv/relup/*.relup`を検証し、不正なエントリは警告ログを出してスキップします。致命的エラーにはなりません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース用のtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.2 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.2/emqx_relup-1.0.0.tar.gz) |
| 6.1.3 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.3/emqx_relup-1.0.0.tar.gz) |
| 6.1.4 | 1.0.1 | [emqx_relup-1.0.1.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.4/emqx_relup-1.0.1.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
