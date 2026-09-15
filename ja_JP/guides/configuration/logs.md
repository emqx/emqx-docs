# ログ設定

このページでは、設定ファイルを通じてEMQXのログ動作を設定する方法を紹介します。EMQXのログはダッシュボードからも設定可能です。ダッシュボードで設定する場合は、左側のナビゲーションメニューから **Management** -> **Logging** をクリックしてください。ログおよびダッシュボードの設定に関する詳細は、[Logs and Observability - Logs](../observability/log.md) を参照してください。

::: tip

このページでは、設定項目に対応するダッシュボードのUIフィールドも紹介しています。  
設定ファイルからログを設定する場合は、`emqx.conf` ではなく `base.hocon` を使用することを推奨します。  
これは、`emqx.conf` に設定を記述すると、ダッシュボードからの変更が一時的なものとなり、EMQX再起動時に失われるためです。

:::

EMQXは主に2種類のログハンドラーをサポートしています。Console LogとFile Logです。さらに、ログを常にファイルに出力するための専用ハンドラーである[Audit Log](../dashboard/audit-log.md)も提供しています。

システムのデフォルトのログハンドリングは環境変数 `EMQX_DEFAULT_LOG_HANDLER` で設定可能で、以下の値を受け付けます。

- `file`：ログ出力をファイルに向ける
- `console`：ログ出力をコンソールに向ける

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルトは `console` ですが、systemdの `emqx.service` ファイル経由で起動した場合は明示的に `file` に設定されます。

## ファイルとしてログを出力する

EMQXのログ出力ディレクトリは環境変数 `EMQX_LOG_DIR` によって決まります。RPMやDEBパッケージでインストールした場合は `/var/log/emqx` に設定されます。そうでない場合は、EMQXのインストールディレクトリ内の `log` ディレクトリがログディレクトリになります。

EMQXのDockerコンテナの場合、インストールディレクトリは `/opt/emqx` であるため、ログディレクトリは `/opt/emqx/log` となります。

ログをファイルとして出力するには、ダッシュボードでログハンドラーを設定するか、以下のように `base.hocon` ファイルを直接編集します。

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
}
```

ここで、

| 設定項目              | ダッシュボードUI       | 説明                                                         | デフォルト値   | 選択可能な値                                               |
| --------------------- | -------------------- | ------------------------------------------------------------ | ------------- | ---------------------------------------------------------- |
| `formatter`           | Log Formatter        | ログのフォーマットを設定します。                             | `text`        | `text` は自由形式テキスト。<br />`json` は構造化ログ用。  |
| `level`               | Log Level            | 現在のログハンドラーのログレベル、つまり記録したい最低ログレベルを設定します。 | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`                | Log File Name        | ログファイルのパスとファイル名を設定します。<br />デフォルトではEMQXインストールディレクトリの `log` フォルダ内の `emqx.log` に書き込みます。 | `emqx.log`    | --                                                         |
| `rotation_count`      | Max Log Files Number | 保存可能なログファイルの最大数を設定します。                 | `10`          | `1` - `2,048`                                              |
| `rotation_size`       | Rotation Size        | ログファイルの最大サイズを設定します。指定サイズに達するとログファイルはローテーションされ、古いファイルはアーカイブディレクトリに移動されます。`infinity` に設定するとローテーションしません。 | `50MB`        | `1` - `infinity`                                           |
| `time_offset`         | Time Offset          | ログのUTCに対する時刻オフセットを設定します。                 | `system`      | --                                                         |
| `timestamp_format`    | Timestamp Format     | ログのタイムスタンプのフォーマットを設定します。              | `auto`        | `auto`: 使用中のログフォーマッターに応じて自動判別。テキストフォーマッターは `rfc3339`、JSONフォーマッターは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日付時刻文字列形式。 |

## コンソールにログを出力する

EMQXをDockerコンテナで起動すると、デフォルトのログハンドラーは `console` になります。  
ログレベルやログフォーマットは以下の設定項目で変更可能です。

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

| 設定項目              | ダッシュボードUI     | 説明                                                         | デフォルト値   | 選択可能な値                                               |
| --------------------- | ------------------ | ------------------------------------------------------------ | ------------- | ---------------------------------------------------------- |
| `formatter`           | Log Formatter      | ログのフォーマットを設定します。                             | `text`        | `text` は自由形式テキスト。<br />`json` は構造化ログ用。  |
| `level`               | Log Level          | 現在のログハンドラーのログレベル、つまり記録したい最低ログレベルを設定します。 | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`         | Time Offset        | ログのUTCに対する時刻オフセットを設定します。                 | `system`      | --                                                         |
| `timestamp_format`    | Timestamp Format   | ログのタイムスタンプのフォーマットを設定します。              | `auto`        | `auto`: 使用中のログフォーマッターに応じて自動判別。テキストフォーマッターは `rfc3339`、JSONフォーマッターは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日付時刻文字列形式。 |

::: tip

EMQXはより詳細な設定項目も提供しており、カスタマイズニーズに対応可能です。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご覧ください。

:::

## Dockerにおけるクラッシュダンプ

Erlang VMが異常終了した場合、クラッシュダンプがログディレクトリ内の `erl_crash.<timestamp>.dump` に書き込まれます。Dockerコンテナ内ではこのディレクトリは `/opt/emqx/log` です。このファイルはノードがダウンした瞬間の状態を記録しており、クラッシュ解析の主要な証拠となります。

コンソールログはクラッシュダンプファイルを保持しません。コンソールログハンドラーはランタイムログをコンテナの標準出力に書き込み、`docker logs` で確認可能です。クラッシュダンプは別途ファイルに書き込まれます。ログディレクトリをマウントしていない場合、コンテナ削除時にダンプは失われます。

EMQX起動前に、ホスト側にディレクトリを作成し、コンテナ内の `emqx` ユーザー（UID 1000）が書き込み可能にしてください。

```bash
mkdir -p $PWD/log && sudo chown 1000:1000 $PWD/log
```

その後、EMQX起動時に以下のようにディレクトリを `/opt/emqx/log` にマウントします。

```bash
docker run -d --name emqx \
  -v $PWD/log:/opt/emqx/log \
  emqx/emqx-enterprise:@EE_VERSION@
```

クラッシュ発生後は、以下のコマンドでコンテナの出力を確認してください。

```bash
docker logs emqx
```

以下の行が `done` で終わっていれば、ダンプファイルは正常に書き込まれています。

```text
Crash dump is being written to: /opt/emqx/log/erl_crash.2026.08.31.06.56.22.dump...done
```

クラッシュダンプは数十メガバイトに及ぶことがあります。マウント先のディスク容量に注意し、ファイルサイズ制限のために `node.crash_dump_bytes` を設定してください。
