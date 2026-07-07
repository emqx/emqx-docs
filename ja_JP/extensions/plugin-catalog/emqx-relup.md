# Hot Upgrade (Relup)

このプラグインは、実行中のEMQXノードに対して`.relup`形式のコード変更指示セットを適用することで、オペレーターがVMを再起動せずにパッチリリースを展開できるようにします。

オペレーターは各ノードで`emqx ctl relup ...` CLIを使って操作します。クラスター全体への展開はオペレーターの責任であり（オーケストレーション機能は組み込まれていません）。

## 使用タイミング

ホットアップグレードは以下の場合に適しています：

- 適用したいホップが`emqx ctl relup list-supported-paths`でリストされている（宣言された`{from, target}`ホップのみサポート）。
- 次のノードに進む前にターゲットノードを検証できる。
- `data/`のバックアップがある。適用済みホップのインプレースロールバックはありません（[ロールバック](#rollback)参照）。

これらを満たせない場合は、通常のローリング再起動によるアップグレードを行ってください。

## オペレーターのワークフロー

### 1. プラグインのインストール

以下の[Download](#download)セクションからEMQXバージョンに合ったtarballをダウンロードし、ダッシュボード（またはREST API / CLI）から他のプラグインと同様にインストールします。

### 2. アップグレードパスのサポート確認

```bash
emqx ctl relup list-supported-paths
```

出力には、このプラグインバージョンの`priv/relup/`にバンドルされた`{from, target}`ホップがリストされます。ホップが見つからない場合、そのパスのホットアップグレードは利用できません。通常の再起動ベースのアップグレードに戻ってください。

### 3. 各ノードにターゲットリリースを配置

各ノードで、EMQXプロセスが読み取れるパスに以下の2つのファイルをコピーします：

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQXターゲットリリースのtarball
- `<tarball>.sha256`：sha256ダイジェスト。標準の`sha256sum`形式（`<digest>  <filename>`）を受け付けます。

### 4. アップグレードのトリガー

各ノードで：

```bash
emqx ctl relup upgrade <TarballPath> [--force]
```

ハンドラーは以下を行います：

- `<TarballPath>.sha256`と実際のダイジェストを照合し、不一致の場合は展開を拒否。
- `data/patches/`に`*.beam`ファイルがある場合は続行を拒否。このディレクトリは`vm.args -pa`でコードパスの先頭に追加されるため、アップグレードターゲットのモジュールより優先されます。ターゲットリリースにホットパッチ済みの修正が含まれている場合でも、古いbeamファイルがロードされる可能性があります。パッチファイルを削除するか、`--force`を指定してパッチをターゲットリリース上に残す意図がある場合のみ続行してください。
- tarballを展開し、`releases/emqx_vars`から`REL_VSN`を読み込みます。
- `priv/relup/*.relup`で対応する`{from, target}`ホップを検索し、宣言されたコード変更指示とポストアップグレードコールバックを実行します。

### 5. ノードの検証

次の項目を確認してから次に進みます：

- `emqx ctl status`がノードが稼働中であることを報告している。
- `<RootDir>/relup/current`がターゲットバージョンと一致し、`<RootDir>/relup/<TargetVsn>/`に`bin/`、`erts-*/`、`lib/`、`releases/`が存在する。

次回の`emqx start`または`restart`時に、`bin/emqx`ラッパーが`relup/current`を検出し、デプロイ済みツリー（新しいERTS、新しいbinスクリプト、新しいlib）にexecします。元の`<RootDir>`は`data/`、`etc/`、`log/`、`plugins/`の権威を保持します。

### 6. 成功後のクリーンアップ

クラスター全体がターゲットバージョンに移行したら、配置したtarballとその`.sha256`サイドカーを手動で削除してください。プラグインはソースパスを追跡しないため、プラグイン側での状態管理はありません。

## アップグレード履歴

各ノードは`emqx_relup_log`テーブル（ディスクバック、ローカルコンテンツ）に独自の監査ログを保持します。この履歴はプラグインアンインストール後も残り、再インストールで再度アタッチされてログが維持されます。

CLIで確認またはクリア可能です：

```bash
emqx ctl relup logs           # 最近のアップグレード試行を表示
emqx ctl relup logs-clear     # このノードの全ログ行を削除
```

## ロールバック

適用済みホップのインプレースロールバックはありません。ホットアップグレードはライブVMに対して`code_changes`を実行し、`post_upgrade_callbacks`でディスク上のデータを変更している可能性があるため、これを元に戻すことはプラグインでサポートされていません。

実用的なフォールバックパス：

- **次回再起動前**に、アップグレードが成功したが新コードに問題があり、ディスク上のデータが旧リリースと互換性がある場合：

  ```bash
  rm <RootDir>/relup/current
  # 任意で：rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  ラッパーは元の`<RootDir>/bin/emqx`ツリーにフォールバックします。これは起動パスのみを回復し、アップグレード時のVM内のライブ状態はすでに失われています。

- **それ以外の場合**は、アップグレード前の`data/`（mnesia、設定など）のバックアップから復元し、旧EMQXリリースを再インストールしてください。この点を考慮してアップグレードウィンドウを計画してください。

## ホップの作成（開発者向けメモ）

新しいホップを追加するには、必要な各リリースに対して：

1. `priv/relup/<from>-to-<to>.relup`を追加し、そのホップの`code_changes`と`post_upgrade_callbacks`を宣言します。スキーマ、サポートされる命令、ポストアップグレードコールバックの契約についてはプラグインソースの`priv/relup/README.md`を参照してください。特に、新しいEMQXの`emqx_post_upgrade`に`pr_NNNNN_*`コールバックを追加する場合は、relupホップでコールバック呼び出し前にそのモジュールをリロードするか、このプラグインに`emqx_post_upgrade_<TargetVsn>.erl`としてコールバックを同梱してください。
2. このプラグインの`VERSION`を更新して再公開します。

プラグインはアプリ起動時にすべての`priv/relup/*.relup`を検証し、不正なエントリは警告ログを出しつつスキップします。致命的なエラーにはなりません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

各EMQXリリース用のtarball：

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.0.3 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.0.3/emqx_relup-1.0.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
