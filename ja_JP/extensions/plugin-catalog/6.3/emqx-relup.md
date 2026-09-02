# Hot Upgrade (Relup)

このプラグインは、実行中のEMQXノードに対して`.relup`形式のコード変更指示を適用することで、オペレーターがVMを再起動せずにパッチリリースを展開できるようにします。

オペレーターは各ノードで`emqx ctl relup ...` CLIを通じて操作します。クラスター全体への展開はオペレーターの責任であり（オーケストレーション機能は組み込まれていません）。

## 使用タイミング

ホットアップグレードは以下の場合に適しています：

- 適用したいホップが`emqx ctl relup list-supported-paths`でリストされている（宣言済みの`{from, target}`ホップのみサポート）。
- 次のノードに進む前にターゲットノードを検証できる。
- `data/`のバックアップがある。適用済みホップのインプレースロールバックはありません（[ロールバック](#rollback)を参照）。

これらを満たせない場合は、通常のローリング再起動を行ってください。

## オペレーターのワークフロー

### 1. プラグインのインストール

以下の[Download](#download)セクションからEMQXバージョンに対応したtarballをダウンロードし、ダッシュボード（またはREST API / CLI経由で他のプラグインと同様に）からインストールします。

### 2. アップグレードパスがサポートされていることを確認

```bash
emqx ctl relup list-supported-paths
```

出力には、このプラグインバージョンの`priv/relup/`にバンドルされた`{from, target}`ホップが表示されます。ホップが見つからない場合、そのパスのホットアップグレードは利用できません。通常の再起動ベースのアップグレードに戻ってください。

### 3. 各ノードにターゲットリリースをステージング

各ノードで、EMQXプロセスが読み取れるパスに以下2つのファイルをコピーします：

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQXターゲットリリースのtarball
- `<tarball>.sha256`：sha256ダイジェスト。標準の`sha256sum`形式（`<digest>  <filename>`）が受け入れられます。

### 4. アップグレードをトリガー

各ノードで以下を実行：

```bash
emqx ctl relup upgrade <TarballPath> [--force]
```

ハンドラーは以下を行います：

- `<TarballPath>.sha256`を実際のダイジェストと照合し、不一致の場合は展開を拒否。
- `data/patches/`に`*.beam`ファイルが存在する場合は継続を拒否。このディレクトリは`vm.args -pa`経由でコードパスに先行して追加されるため、アップグレードターゲットのモジュールより優先されます。ターゲットリリースにホットパッチ済みの修正が含まれている場合でも、古いbeamファイルがロードされる可能性があります。パッチファイルを削除するか、ターゲットリリース上にパッチを維持する意図がある場合のみ`--force`を指定してください。
- tarballを展開し、`releases/emqx_vars`から`REL_VSN`を読み取る。
- `priv/relup/*.relup`から該当する`{from, target}`ホップを検索し、宣言されたコード変更指示とアップグレード後のコールバックを実行。

### 5. ノードの検証

次の項目を確認してから次に進みます：

- `emqx ctl status`でノードが稼働中であること。
- `<RootDir>/relup/current`がターゲットバージョンと一致し、`<RootDir>/relup/<TargetVsn>/`に`bin/`、`erts-*/`、`lib/`、`releases/`が含まれていること。

次回の`emqx start` / `restart`時に、`bin/emqx`ラッパーが`relup/current`を検出し、デプロイ済みのツリー（新しいERTS、新しいbinスクリプト、新しいlib）にexecします。元の`<RootDir>`は`data/`、`etc/`、`log/`、`plugins/`の権威を保持します。

### 6. 成功後のクリーンアップ

クラスター全体がターゲットバージョンに移行したら、ステージングしたtarballとその`.sha256`サイドカーを手動で削除してください。プラグインはソースパスを追跡しないため、プラグイン側での状態管理はありません。

## アップグレード履歴

各ノードは`emqx_relup_log`テーブル（ディスクバックド、ローカル内容）に独自の監査ログを保持します。履歴はプラグインアンインストール後も残り、再インストールで再接続されます。

CLIで確認またはクリア可能：

```bash
emqx ctl relup logs           # 最近のアップグレード試行を表示
emqx ctl relup logs-clear     # このノードのログ行をすべて削除
```

## ロールバック

適用済みホップのインプレースロールバックはありません。ホットアップグレードはライブVMに対して`code_changes`を実行し、`post_upgrade_callbacks`がディスク上のデータを変更している可能性があるため、これを元に戻すことはプラグインでサポートされていません。

実用的なフォールバックパス：

- **次回の再起動前**に、アップグレードが成功したが新コードが不調で、ディスク上のデータが旧リリースと互換性がある場合：

  ```bash
  rm <RootDir>/relup/current
  # 任意で: rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  ラッパーは元の`<RootDir>/bin/emqx`ツリーにフォールバックします。これは起動パスのみ回復し、アップグレード時のVM内のライブ状態はすでに失われています。

- **それ以外の場合**は、アップグレード前の`data/`（mnesia、設定など）のバックアップから復元し、旧EMQXリリースを再インストールしてください。この点を考慮してアップグレードウィンドウを計画してください。

## ホップの作成（開発者向けメモ）

新しいホップを追加するには、必要な各リリースに対して：

1. `priv/relup/<from>-to-<to>.relup`を追加し、ホップの`code_changes`と`post_upgrade_callbacks`を宣言します。プラグインソースの`priv/relup/README.md`にスキーマ、サポートされる命令、アップグレード後コールバックの契約があります。特に、新しいEMQXの`emqx_post_upgrade`に`pr_NNNNN_*`コールバックを追加する場合、relupホップはコールバック呼び出し前にそのモジュールをリロードする必要があります。または、このプラグイン内に`emqx_post_upgrade_<TargetVsn>.erl`としてコールバックを同梱してください。
2. このプラグインの`VERSION`を更新し、再公開します。

プラグインはアプリ起動時にすべての`priv/relup/*.relup`を検証し、不正なエントリには警告をログに出します。悪いファイルはスキップされ、致命的にはなりません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

各EMQXリリース用のtarball：

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.3.0 | 1.0.2 | [emqx_relup-1.0.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_relup-1.0.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_relup-1.0.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
