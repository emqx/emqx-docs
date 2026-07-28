# Hot Upgrade (Relup)

このプラグインは、実行中のEMQXノードに対して`.relup`形式のコード変更指示を適用し、オペレーターがVMを再起動せずにパッチリリースを展開できるようにします。

オペレーターは各ノードで`emqx ctl relup ...` CLIを使って操作します。クラスター全体への展開はオペレーターの責任であり（オーケストレーション機能は組み込まれていません）。

## 使用タイミング

Hot Upgradeは以下の場合に適しています：

- 適用したいホップが`emqx ctl relup list-supported-paths`でリストされている（宣言された`{from, target}`ホップのみサポート）。
- 次のノードに進む前にターゲットノードを検証できる。
- `data/`のバックアップがある。適用済みホップのインプレースロールバックはありません（[ロールバック](#rollback)参照）。

これらを満たせない場合は、通常のローリング再起動を行ってください。

## オペレーターのワークフロー

### 1. プラグインのインストール

以下の[Download](#download)セクションからEMQXバージョンに合ったtarballをダウンロードし、ダッシュボード（またはREST API／CLI）から他のプラグインと同様にインストールします。

### 2. アップグレードパスのサポート確認

```bash
emqx ctl relup list-supported-paths
```

出力にはこのプラグインバージョンの`priv/relup/`にバンドルされた`{from, target}`ホップが表示されます。ホップが無い場合、そのパスのHot Upgradeは利用できません。通常の再起動ベースのアップグレードに戻ってください。

### 3. 各ノードにターゲットリリースを配置

各ノードで、EMQXプロセスが読み取れるパスに以下の2ファイルをコピーします：

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQXターゲットリリースのtarball
- `<tarball>.sha256`：sha256ダイジェスト。標準の`sha256sum`形式（`<digest>  <filename>`）が受け入れられます。

### 4. アップグレードのトリガー

各ノードで以下を実行します：

```bash
emqx ctl relup upgrade <TarballPath> [--force]
```

ハンドラーは以下を行います：

- `<TarballPath>.sha256`と実際のダイジェストを照合し、不一致の場合は展開を拒否。
- `data/patches/`に`*.beam`ファイルがある場合は続行を拒否。このディレクトリは`vm.args -pa`でコードパスの先頭に追加されるため、アップグレードターゲットのモジュールより優先されます。ターゲットリリースにホットパッチ済みの修正が含まれている場合、古いbeamファイルが読み込まれる可能性があります。パッチファイルを先に削除するか、ターゲットリリース上に適用済みの状態を維持する場合のみ`--force`を指定してください。
- tarballを展開し、`releases/emqx_vars`から`REL_VSN`を読み取る。
- `priv/relup/*.relup`の対応する`{from, target}`ホップを検索し、宣言されたコード変更指示とアップグレード後のコールバックを実行。

### 5. ノードの検証

次の状態を確認してから次に進みます：

- `emqx ctl status`でノードが稼働中であること。
- `<RootDir>/relup/current`がターゲットバージョンと一致し、`<RootDir>/relup/<TargetVsn>/`に`bin/`、`erts-*/`、`lib/`、`releases/`が存在すること。

次回の`emqx start`または`restart`時に、`bin/emqx`ラッパーが`relup/current`を検出し、デプロイ済みツリー（新しいERTS、新しいbinスクリプト、新しいlib）にexecします。元の`<RootDir>`は`data/`、`etc/`、`log/`、`plugins/`の管理を継続します。

### 6. 成功後のクリーンアップ

クラスター全体がターゲットバージョンに揃ったら、配置したtarballとその`.sha256`ファイルを手動で削除してください。プラグインはソースパスを追跡しないため、プラグイン側での状態管理はありません。

## アップグレード履歴

各ノードは`emqx_relup_log`テーブル（ディスクバック、ローカルコンテンツ）に独自の監査履歴を保持します。履歴はプラグインアンインストール後も残り、再インストールで再接続されます。

CLIで確認またはクリア可能です：

```bash
emqx ctl relup logs           # 最近のアップグレード試行を表示
emqx ctl relup logs-clear     # このノードのログ行を全削除
```

## ロールバック

適用済みホップのインプレースロールバックはありません。Hot UpgradeはライブVMに対して`code_changes`を実行し、`post_upgrade_callbacks`でディスク上のデータが変更されている可能性があるため、このプラグインでは元に戻すことをサポートしていません。

実用的なフォールバック方法：

- **次回再起動前**に、アップグレードが成功したが新コードに問題があり、ディスク上のデータが旧リリースと互換性がある場合：

  ```bash
  rm <RootDir>/relup/current
  # 必要に応じて: rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  ラッパーは元の`<RootDir>/bin/emqx`ツリーにフォールバックします。これはブートパスのみ回復し、アップグレード時のVM内ライブ状態は既に失われています。

- **それ以外の場合**は、アップグレード前にバックアップした`data/`（mnesia、設定など）から復元し、旧EMQXリリースを再インストールしてください。この点を考慮してアップグレードウィンドウを計画してください。

## ホップの作成（開発者向けメモ）

新しいホップを追加するには、必要な各リリースに対して：

1. `priv/relup/<from>-to-<to>.relup`を追加し、ホップの`code_changes`と`post_upgrade_callbacks`を宣言します。プラグインソースの`priv/relup/README.md`にスキーマ、サポートされる命令、アップグレード後コールバックの契約が記載されています。特に、新しいEMQXの`emqx_post_upgrade`に`pr_NNNNN_*`コールバックを追加する場合、relupホップはコールバック呼び出し前にそのモジュールをリロードするか、このプラグインに`emqx_post_upgrade_<TargetVsn>.erl`としてコールバックを同梱する必要があります。
2. このプラグインの`VERSION`を更新し、再公開します。

プラグインはアプリ起動時にすべての`priv/relup/*.relup`を検証し、形式不正なエントリは警告ログを出してスキップします。致命的エラーにはなりません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

各EMQXリリース用のtarball：

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.1.2 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.2/emqx_relup-1.0.0.tar.gz) |
| 6.1.3 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.3/emqx_relup-1.0.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
