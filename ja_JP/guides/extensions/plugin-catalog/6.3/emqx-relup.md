# Hot Upgrade (Relup)

このプラグインは、実行中のEMQXノードに対して`.relup`形式のコード変更指示を適用することで、オペレーターがVMを再起動せずにパッチリリースを展開できるようにします。

オペレーターは各ノードで`emqx ctl relup ...` CLIを使って操作します。クラスター全体への展開はオペレーターの責任であり（オーケストレーション機能は組み込まれていません）。

## 使用タイミング

ホットアップグレードは以下の場合に適しています：

- 適用したいホップが`emqx ctl relup list-supported-paths`でリストされている（宣言された`{from, target}`ホップのみサポート）。
- 次のノードに移る前にターゲットノードを検証できる。
- `data/`のバックアップがある。適用済みホップのインプレースロールバックはありません（[ロールバック](#rollback)参照）。

これらを満たせない場合は、通常のローリング再起動によるアップグレードを行ってください。

## オペレーターのワークフロー

### 1. プラグインのインストール

以下の[ダウンロード](#download)セクションからEMQXバージョンに合ったtarballを取得し、ダッシュボード（またはREST API / CLI経由で他のプラグインと同様に）からインストールします。

### 2. アップグレードパスのサポート確認

```bash
emqx ctl relup list-supported-paths
```

出力にはこのプラグインバージョンの`priv/relup/`にバンドルされた`{from, target}`ホップが表示されます。ホップがなければ、そのパスのホットアップグレードは利用できません。通常の再起動ベースのアップグレードに戻ってください。

### 3. 各ノードにターゲットリリースをステージング

各ノードに対し、EMQXプロセスが読み取れるパスに以下の2ファイルをコピーします：

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQXターゲットリリースのtarball
- `<tarball>.sha256`：sha256ダイジェスト。標準の`sha256sum`形式（`<digest>  <filename>`）を受け付けます。

### 4. アップグレードのトリガー

各ノードで以下を実行します：

```bash
emqx ctl relup upgrade <TarballPath> [--force]
```

ハンドラーは以下を行います：

- `<TarballPath>.sha256`と実際のダイジェストを照合し、不一致の場合は展開を拒否。
- `data/patches/`に`*.beam`ファイルがある場合は続行を拒否。このディレクトリは`vm.args -pa`経由でコードパスに先行して追加されるため、アップグレードターゲットのモジュールより優先されます。ターゲットリリースにホットパッチ済みの修正が含まれている場合、古いbeamファイルが読み込まれる恐れがあります。パッチファイルを先に削除するか、ターゲットリリース上にパッチを残す意図がある場合のみ`--force`を指定してください。
- tarballを展開し、`releases/emqx_vars`から`REL_VSN`を読み込みます。
- `priv/relup/*.relup`で該当する`{from, target}`ホップを検索し、宣言されたコード変更指示とポストアップグレードコールバックを実行します。

### 5. ノードの検証

以下を確認してから次に進みます：

- `emqx ctl status`でノードが稼働中と表示される。
- `<RootDir>/relup/current`がターゲットバージョンと一致し、`<RootDir>/relup/<TargetVsn>/`に`bin/`、`erts-*/`、`lib/`、`releases/`が存在する。

次回の`emqx start` / `restart`時に、`bin/emqx`ラッパーが`relup/current`を検知してデプロイ済みツリー（新しいERTS、新しいbinスクリプト、新しいlib）にexecします。元の`<RootDir>`は`data/`、`etc/`、`log/`、`plugins/`の権威を保持します。

### 6. 成功後のクリーンアップ

クラスター全体がターゲットバージョンに揃ったら、ステージングしたtarballと`.sha256`のサイドカーを手動で削除してください。プラグインはソースパスを追跡しないため、プラグイン側での状態クリーンアップは不要です。

## アップグレード履歴

各ノードは`emqx_relup_log`テーブル（ディスクバック、ローカルコンテンツ）に独自の監査ログを保持します。履歴はプラグインアンインストール後も残り、再インストールで再接続されて行が維持されます。

CLIで確認またはクリア可能です：

```bash
emqx ctl relup logs           # 最近のアップグレード試行を表示
emqx ctl relup logs-clear     # このノードのログ行をすべて削除
```

## ロールバック

適用済みホップのインプレースロールバックはありません。ホットアップグレードはライブVMに対して`code_changes`を実行し、`post_upgrade_callbacks`がディスク上のデータを変更している可能性があるため、プラグインではその逆操作をサポートしていません。

実用的なフォールバック方法：

- **次の再起動前に**、アップグレードが成功したが新しいコードに問題があり、ディスク上のデータが旧リリースと互換性がある場合：

  ```bash
  rm <RootDir>/relup/current
  # 任意で: rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  ラッパーは元の`<RootDir>/bin/emqx`ツリーにフォールバックします。これはブートパスのみの回復であり、問題のあったアップグレード時点のVM内ライブ状態は失われています。

- **それ以外の場合**、アップグレード前に取得した`data/`（mnesia、設定など）のバックアップから復元し、旧EMQXリリースを再インストールしてください。この点を考慮してアップグレードウィンドウを計画してください。

## ホップの作成（開発者向けメモ）

新しいホップを追加するには、必要な各リリースに対して：

1. `priv/relup/<from>-to-<to>.relup`を追加し、ホップの`code_changes`と`post_upgrade_callbacks`を宣言します。プラグインソースの`priv/relup/README.md`にスキーマ、サポートされる命令、ポストアップグレードコールバックの契約が記載されています。特に、新EMQXの`emqx_post_upgrade`に`pr_NNNNN_*`コールバックを追加する場合、relupホップはコールバック呼び出し前にそのモジュールをリロードするか、このプラグイン内に`emqx_post_upgrade_<TargetVsn>.erl`としてコールバックを同梱する必要があります。
2. このプラグインの`VERSION`を更新し、再公開します。

プラグインは起動時にすべての`priv/relup/*.relup`を検証し、不正なエントリは警告ログを出してスキップします。致命的ではありません。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

Tarballs for each EMQX release:

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.3.0 | 1.0.2 | [emqx_relup-1.0.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_relup-1.0.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_relup-1.0.2.sha256)) |
| 6.3.1 | 1.0.2 | [emqx_relup-1.0.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_relup-1.0.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_relup-1.0.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
