# ログ設定

このページでは、設定ファイルを使ってEMQXのログ動作を設定する方法を紹介します。EMQXのログはダッシュボードからも設定可能です。ダッシュボードで設定する場合は、左側のナビゲーションメニューから **Management** -> **Logging** をクリックしてください。ログおよびダッシュボード設定の詳細については、[Logs and Observability - Logs](../observability/log.md) を参照してください。

::: tip

このページでは、設定項目に対応するダッシュボードのUIフィールドも紹介しています。  
ログを設定ファイルから構成する場合は、`emqx.conf` ではなく `base.hocon` を使用することを推奨します。  
`emqx.conf` に設定を記述すると、ダッシュボードでの変更が一時的なものとなり、EMQX再起動時に失われてしまうためです。

:::

EMQXは主に2種類のログ出力をサポートしています。コンソールログとファイルログです。さらに、常にファイルにログを出力する[監査ログ](../dashboard/audit-log.md)もあります。

システムのデフォルトログ出力は環境変数 `EMQX_DEFAULT_LOG_HANDLER` で設定でき、以下の値を受け付けます。

- `file`：ログ出力をファイルに送る
- `console`：ログ出力をコンソールに送る

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルトは `console` ですが、systemdの `emqx.service` ファイル経由で起動すると明示的に `file` に設定されます。

## ファイルとしてログを出力する

RPMおよびDEBインストールの場合、`EMQX_LOG_DIR` のデフォルトは `/var/log/emqx` です。EMQX 6.3.0以降では、`/opt/emqx/log` はこのディレクトリへのシンボリックリンクとなっています。シンボリックリンクは常に `/var/log/emqx` を指しており、`EMQX_LOG_DIR` を変更しても更新されません。

その他のインストール方法では、デフォルトのログディレクトリはEMQXインストールディレクトリの下の `log` です。Dockerコンテナ内ではこのパスは `/opt/emqx/log` となります。

ログをファイルとして出力するには、ダッシュボードでファイルログ出力を設定するか、以下のように `base.hocon` ファイルを直接編集してください。

```bash
log {
  file {
    enable = true
    formatter = text
    level = warning
    path = "/var/log/emqx/emqx.log"
    rotation_count = 10
    rotation_size = 50MB
    time_offset = system
    timestamp_format = auto
  }
```

ここで、

| 設定項目              | ダッシュボードUI       | 説明                                                         | デフォルト値  | 選択可能な値                                               |
| --------------------- | -------------------- | ------------------------------------------------------------ | ------------- | ---------------------------------------------------------- |
| `formatter`           | ログフォーマッター    | ログのフォーマットを設定します。                             | `text`        | `text` は自由テキスト形式。<br />`json` は構造化ログ形式。 |
| `level`               | ログレベル            | 現在のログ出力のログレベル、つまり記録したい最低ログレベルを設定します。 | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`                | ログファイル名        | ログファイルのパスと名前を設定します。<br />デフォルトではEMQXインストールディレクトリの `log` フォルダ内の `emqx.log` に書き込みます。 | `emqx.log`    | --                                                         |
| `rotation_count`      | 最大ログファイル数    | 保存できるログファイルの最大数を設定します。                 | `10`          | `1` - `2,048`                                              |
| `rotation_size`       | ローテーションサイズ  | ログファイルがこのサイズに達するとローテーションされます。`infinity` に設定するとローテーションされません。 | `50MB`        | `1` - `infinity`                                           |
| `time_offset`         | 時間オフセット        | ログのUTCに対する時間オフセットを設定します。                 | `system`      | --                                                         |
| `timestamp_format`    | タイムスタンプ形式    | ログ内のタイムスタンプの形式を設定します。                     | `auto`        | `auto`: ログフォーマッターに応じて自動判別します。textは `rfc3339`、JSONは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日時形式。 |

## コンソールにログを出力する

EMQXをDockerコンテナで起動すると、デフォルトのログ出力は `console` となります。ログレベルやログフォーマットは以下の設定項目で構成可能です。

```bash
log {
  console {
    formatter = json
    level = warning
    time_offset = system
    timestamp_format = auto
  }
}
```

ここで、

| 設定項目              | ダッシュボードUI       | 説明                                                         | デフォルト値  | 選択可能な値                                               |
| --------------------- | -------------------- | ------------------------------------------------------------ | ------------- | ---------------------------------------------------------- |
| `formatter`           | ログフォーマッター    | ログのフォーマットを設定します。                             | `text`        | `text` は自由テキスト形式。<br />`json` は構造化ログ形式。 |
| `level`               | ログレベル            | 現在のログ出力のログレベル、つまり記録したい最低ログレベルを設定します。 | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`         | 時間オフセット        | ログのUTCに対する時間オフセットを設定します。                 | `system`      | --                                                         |
| `timestamp_format`    | タイムスタンプ形式    | ログ内のタイムスタンプの形式を設定します。                     | `auto`        | `auto`: ログフォーマッターに応じて自動判別します。textは `rfc3339`、JSONは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日時形式。 |

::: tip

EMQXはより詳細な設定項目も提供しており、カスタマイズニーズに対応しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) を参照してください。

:::

## Dockerにおけるクラッシュダンプ

Erlang VMが異常終了すると、クラッシュダンプがログディレクトリ（コンテナ内では `/opt/emqx/log`）に `erl_crash.<timestamp>.dump` というファイル名で書き込まれます。このファイルにはノードがダウンした時点の状態が記録されており、クラッシュ解析の主要な証拠となります。

コンソールログはクラッシュダンプファイルを保持しません。コンソールログハンドラーはランタイムログをコンテナの標準出力に書き込み、`docker logs` で確認可能です。クラッシュダンプは別途ファイルに書き込まれます。ログディレクトリをマウントしていない場合、コンテナ削除時にダンプも失われます。

EMQX起動前に、ホスト側でディレクトリを作成し、コンテナ内の `emqx` ユーザー（UID 1000）が書き込み可能にしてください。

```bash
mkdir -p $PWD/log && sudo chown 1000:1000 $PWD/log
```

そしてEMQX起動時に、そのディレクトリを `/opt/emqx/log` にマウントします。

```bash
docker run -d --name emqx \
  -v $PWD/log:/opt/emqx/log \
  emqx/emqx-enterprise:@EE_VERSION@
```

クラッシュ後は以下のコマンドでコンテナの出力を確認してください。

```bash
docker logs emqx
```

以下の行が `done` で終わっていれば、ダンプファイルは正常に書き込まれています。

```text
Crash dump is being written to: /opt/emqx/log/erl_crash.2026.08.31.06.56.22.dump...done
```

クラッシュダンプは数十メガバイトになることがあります。マウント先ディスクの空き容量を考慮し、`node.crash_dump_bytes` を設定してファイルサイズを制限してください。
