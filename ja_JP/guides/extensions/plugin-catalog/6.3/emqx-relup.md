# Hot Upgrade (Relup)

このプラグインは、実行中のEMQXノードに対して`.relup`形式のコード変更指示を適用することで、VMを再起動せずにパッチリリースを展開できるようにします。

オペレーターは各ノードで`emqx ctl relup ...` CLIを使って操作します。クラスター全体への展開はオペレーターの責任であり（オーケストレーション機能は組み込まれていません）。

## いつ使うか

ホットアップグレードは以下の場合に適しています：

- 適用したいホップが`emqx ctl relup list-supported-paths`でリストされている（宣言された`{from, target}`ホップのみサポート）。
- 次のノードに進む前にターゲットノードを検証できる。
- `data/`のバックアップがある。適用済みホップのインプレースロールバックはありません（[ロールバック](#rollback)参照）。

これらを満たせない場合は、通常のローリング再起動を行ってください。

## オペレーターのワークフロー

### 1. プラグインをインストールする

以下の[ダウンロード](#download)セクションからEMQXのバージョンに合ったtarballをダウンロードし、ダッシュボード（またはREST API / CLI）から他のプラグインと同様にインストールします。

### 2. アップグレードパスがサポートされていることを確認する

```bash
emqx ctl relup list-supported-paths
```

出力はこのプラグインバージョンの`priv/relup/`にバンドルされている`{from, target}`ホップを一覧表示します。ホップが見つからない場合、そのパスのホットアップグレードは利用できません。通常の再起動ベースのアップグレードに戻ってください。

### 3. 各ノードにターゲットリリースを配置する

各ノードに対し、EMQXプロセスが読み取れるパスに以下の2つのファイルをコピーします：

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQXターゲットリリースのtarball
- `<tarball>.sha256`：sha256ダイジェスト。標準の`sha256sum`形式（`<digest>  <filename>`）が受け入れられます。

### 4. アップグレードをトリガーする

各ノードで以下を実行します：

```bash
emqx ctl relup upgrade <TarballPath> [--force]
```

ハンドラーは以下を行います：

- `<TarballPath>.sha256`のダイジェストを実際のものと照合し、不一致の場合は展開を拒否します。
- `data/patches/`に`*.beam`ファイルがある場合は継続を拒否します。このディレクトリは`vm.args -pa`でコードパスの先頭に追加されるため、アップグレードターゲットのモジュールより優先されます。ターゲットリリースにホットパッチ済みの修正が含まれている場合でも、古いbeamファイルが読み込まれる可能性があります。パッチファイルを先に削除するか、ターゲットリリース上に適用済みのまま保持したい場合のみ`--force`を指定してください。
- tarballを展開し、`releases/emqx_vars`から`REL_VSN`を読み取ります。
- `priv/relup/*.relup`内の対応する`{from, target}`ホップを検索し、宣言されたコード変更指示とアップグレード後のコールバックを実行します。

### 5. ノードを検証する

次に進む前に以下を確認してください：

- `emqx ctl status`でノードが稼働中であること。
- `<RootDir>/relup/current`がターゲットバージョンと一致し、`<RootDir>/relup/<TargetVsn>/`に`bin/`、`erts-*/`、`lib/`、`releases/`が存在すること。

次回の`emqx start`または`restart`時に、`bin/emqx`ラッパーが`relup/current`を検出し、デプロイ済みツリー（新しいERTS、新しいbinスクリプト、新しいlib）へexecします。元の`<RootDir>`は`data/`、`etc/`、`log/`、`plugins/`の権威を保持します。

### 6. 成功後のクリーンアップ

クラスター全体がターゲットバージョンに揃ったら、手動で配置したtarballと`.sha256`ファイルを削除してください。プラグインはソースパスを追跡しないため、プラグイン側での状態クリーンアップは不要です。

## アップグレード履歴

各ノードは`emqx_relup_log`テーブル（ディスクバック、ローカルコンテンツ）に独自の監査履歴を保持します。この履歴はプラグインアンインストール後も残り、再インストールで再接続されます。

CLIで確認またはクリアできます：

```bash
emqx ctl relup logs           # 最近のアップグレード試行を表示
emqx ctl relup logs-clear     # このノードのログ行をすべて削除
```

## ロールバック

適用済みホップのインプレースロールバックはありません。ホットアップグレードはライブVMに対して`code_changes`を実行し、`post_upgrade_callbacks`がディスク上のデータを変更している可能性があるため、これを元に戻すことはプラグインでサポートされていません。

実用的なフォールバック方法：

- **次回の再起動前**に、アップグレードが成功したが新しいコードが不具合を起こし、ディスク上のデータが旧リリースと互換性がある場合：

  ```bash
  rm <RootDir>/relup/current
  # 任意で: rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  ラッパーは元の`<RootDir>/bin/emqx`ツリーにフォールバックします。これは起動パスのみ回復し、アップグレード時のVM内部のライブ状態は失われています。

- **それ以外の場合**は、アップグレード前にバックアップした`data/`（mnesia、設定など）を復元し、旧EMQXリリースを再インストールしてください。この点を考慮してアップグレードウィンドウを計画してください。

## ホップの作成（開発者向けメモ）

新しいホップを追加するには、必要な各リリースに対して：

1. `priv/relup/<from>-to-<to>.relup`を追加し、そのホップの`code_changes`と`post_upgrade_callbacks`を宣言します。プラグインソース内の`priv/relup/README.md`にスキーマ、サポートされる命令、アップグレード後コールバックの契約が記載されています。特に、新しいEMQXの`emqx_post_upgrade`に`pr_NNNNN_*`コールバックを追加する場合、relupホップはコールバック呼び出し前にそのモジュールをリロードするか、このプラグイン内に`emqx_post_upgrade_<TargetVsn>.erl`としてコールバックを同梱する必要があります。
2. このプラグインの`VERSION`を更新し、再公開します。

プラグインは起動時にすべての`priv/relup/*.relup`を検証し、不正なエントリには警告をログに出します。悪いファイルはスキップされ、致命的エラーにはなりません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリースのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.3.0 | 1.0.2 | [emqx_relup-1.0.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_relup-1.0.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_relup-1.0.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
