# ホットアップグレード

ホットアップグレードは、稼働中のEMQX Enterpriseノードを停止せずにパッチバージョンのアップデートを適用できる機能です。接続はアップグレード中も維持され、ノードは次回の再起動時に新しいリリースツリーに切り替わります。

ホットアップグレードはパッチバージョンの差分（バージョン番号の3番目の数字）のみ対応しています。例えば、5.10.4から5.10.5へのアップグレードはサポートされていますが、5.10.xから5.11.0へのアップグレードはサポートされていません。

ホットアップグレードは`emqx_relup`プラグインによって実行されます。このプラグインはダッシュボードからインストールします。すべてのアップグレード操作は`emqx ctl relup` CLIを通じて行います。

::: warning 重要なお知らせ
ホットアップグレードを実行する前に、`data/`、`etc/`、および`log/`のバックアップを必ず取得してください。アップグレード適用後のインプレースロールバックはできません。
:::

## 前提条件

- 各ノードでEMQX Enterpriseが稼働していること。
- 各ノードにシェルアクセスがあり、`emqx ctl`コマンドを実行できること。
- `emqx_relup`プラグインのバージョンが現在のEMQX Enterpriseのバージョンと一致していること。
- プラグインが対象バージョンをサポートするホップとしてリストしていること（ステップ2で確認）。

## ステップ1: emqx_relupプラグインのインストール

ダッシュボードからプラグインをインストールします。手順は[ダッシュボードからのパッケージインストール](../../guides/extensions/plugin-management.md#install-packages-via-dashboard)を参照してください。プラグインパッケージは`https://packages.emqx.io/emqx-plugins/e<EMQX-VSN>/`に公開されています。

インストール後、プラグインのステータスが`running`であることを確認してください。

## ステップ2: アップグレードパスのサポート確認

インストール済みプラグインが認識しているバージョンホップを一覧表示します。

```bash
emqx ctl relup list-supported-paths
```

`{from, target}`のバージョンペアが出力に含まれていることを確認してから次に進んでください。

## ステップ3: タールボールとSHA256サイドカーの準備

以下の2つのファイルを各ノードにコピーします。EMQXプロセスが読み取れる任意の場所に配置可能です。

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQX Enterpriseの対象リリースのタールボール
- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz.sha256`：タールボールのSHA256ダイジェスト。生のダイジェスト形式と`sha256sum`の出力形式（`<digest>  <filename>`）の両方が受け入れられます。

```bash
scp emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz      node1:/opt/upgrade/
scp emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz.sha256 node1:/opt/upgrade/
```

`.sha256`サイドカーはタールボールと同じディレクトリに置き、同じベース名で`.sha256`拡張子を持つ必要があります。

## ステップ4: アップグレードの実行

各ノードで、`relup upgrade`コマンドにタールボールのパスを指定してアップグレードを開始します。

```bash
emqx ctl relup upgrade /opt/upgrade/emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz
```

アップグレードハンドラーは以下の処理を行います。

1. SHA256ダイジェストを検証し、不一致の場合は処理を拒否します。
2. タールボールを展開し、`releases/emqx_vars`から対象バージョンを読み取ります。
3. ランタイムのサブディレクトリ（`bin/`、`erts-*/`、`lib/`、`releases/`）のみを`<RootDir>/relup/<TargetVsn>/`に展開します。設定、データ、ログ、プラグインは元のインストールルートに残ります。
4. プラグインの`.relup`カタログから該当するコード変更指示を適用します。
5. アップグレード後のコールバックを実行します。
6. `<RootDir>/relup/current`に対象バージョン文字列を書き込みます。

クラスター全体への展開は運用者の責任です。ノードを一台ずつアップグレードし、それぞれの正常性を確認してから次に進んでください。

## ステップ5: ノードの確認

アップグレードコマンドが完了したら、ノードが正常であることを確認します。

```bash
# ノードが稼働中か確認
emqx ctl status

# バージョンマーカーが書き込まれているか確認
cat <RootDir>/relup/current

# アップグレードの状態と履歴を確認
emqx ctl relup status
emqx ctl relup logs
```

`relup/current`には対象バージョン文字列が含まれ、`emqx ctl status`はノードが稼働中であることを報告するはずです。

## ステップ6: 新リリースでの再起動

コード変更アップグレードは実行中のVMに即時反映されますが、新しいERTSランタイムとバイナリに完全に切り替えるにはノードを再起動してください。

```bash
emqx restart
```

再起動時、`bin/emqx`ラッパーは`<RootDir>/relup/current`を検出し、`<RootDir>/relup/<TargetVsn>/`から起動します。元の`<RootDir>`は`data/`、`etc/`、`log/`、`plugins/`の管理を継続します。

## ステップ7: クリーンアップ

クラスター全体が対象バージョンで稼働していることを確認したら、ステージングファイルを削除します。

```bash
rm /opt/upgrade/emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz
rm /opt/upgrade/emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz.sha256
```

プラグインはソースパスを追跡しないため、プラグイン内部のクリーンアップは不要です。

## ロールバック

適用済みホップのインプレースロールバックはありません。コード変更はライブVM上で実行され、アップグレード後のコールバックでディスク上のデータが変更されている可能性があるため、元に戻すことはサポートされていません。

限定的な復旧オプションが2つあります。

- **次回再起動前の場合：** アップグレードしたコードに問題があるがディスク状態が旧リリースと互換性がある場合、バージョンマーカーを削除して旧ツリーから再起動します。

  ```bash
  rm <RootDir>/relup/current
  # 必要に応じて: rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  これは起動パスのみを復旧します。アップグレードによるライブ状態の変更はすでに反映されています。

- **完全復元：** アップグレード前のバックアップから`data/`を復元し、旧EMQXリリースを再インストールします。

これらの制限を踏まえてアップグレードの時間帯を計画してください。

## CLIリファレンス

| コマンド | 説明 |
|---|---|
| `emqx ctl relup upgrade <TarballPath>` | 指定したタールボールからアップグレードホップを適用します。 |
| `emqx ctl relup list-supported-paths` | プラグインカタログ内のサポートされている`{from, target}`バージョンホップを一覧表示します。 |
| `emqx ctl relup status` | 現在のアップグレード状態を表示します：`idle`、`in-progress`、または`hot-upgraded to <vsn>; pending on restart to boot from the new version`。 |
| `emqx ctl relup logs` | このノードのアップグレード履歴を永続ログテーブルから表示します。 |
| `emqx ctl relup logs-clear` | このノードのアップグレードログテーブルを消去します。 |
