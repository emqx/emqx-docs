# ログ設定

このページでは、設定ファイルを使ったEMQXのログ動作の設定方法を紹介します。EMQXのログはダッシュボードからも設定可能です。ダッシュボードで設定する場合は、左側のナビゲーションメニューから **Management** -> **Logging** をクリックしてください。ログやダッシュボード設定の詳細については、[Logs and Observability - Logs](../observability/log.md) を参照してください。

::: tip

このページでは、設定項目に対応するダッシュボードUIのフィールドも紹介しています。  
設定ファイルからログを設定する場合は、`emqx.conf` ではなく `base.hocon` を使うことを推奨します。  
`emqx.conf` に設定した場合、ダッシュボードからの変更は一時的なものとなり、EMQX再起動時に失われるためです。

:::

EMQXは主に2種類のログ出力をサポートしています：コンソールログとファイルログに加え、常にファイルに出力される[監査ログ](../dashboard/audit-log.md)があります。

システムのデフォルトログ出力は環境変数 `EMQX_DEFAULT_LOG_HANDLER` により設定可能で、以下の値を受け入れます：

- `file`：ログをファイルに出力します。
- `console`：ログをコンソールに出力します。

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルトは `console` ですが、systemdの `emqx.service` ファイル経由で起動した場合は明示的に `file` に設定されます。

## ファイルとしてログを出力する

EMQXのログ出力ディレクトリは環境変数 `EMQX_LOG_DIR` によって決まり、RPMやDEBパッケージでインストールした場合は `/var/log/emqx` に設定されます。その他の場合は、EMQXインストールディレクトリの `log` フォルダがログディレクトリとなります。

EMQXのDockerコンテナでは、インストールディレクトリが `/opt/emqx` なので、ログディレクトリは `/opt/emqx/log` となります。

ログをファイルとして出力するには、ダッシュボードでファイルログ出力を設定するか、以下のように `base.hocon` ファイルを直接編集します：

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

各設定項目の説明は以下の通りです。

| 設定項目               | ダッシュボードUI       | 説明                                                         | デフォルト値   | 選択可能な値                                               |
| ---------------------- | ---------------------- | ------------------------------------------------------------ | -------------- | ---------------------------------------------------------- |
| `formatter`            | Log Formatter          | ログのフォーマットを設定します。                             | `text`         | `text` は自由形式テキスト。<br />`json` は構造化ログ用。  |
| `level`                | Log Level              | 現在のログ出力のログレベル、つまり記録したい最低ログレベルを設定します。 | `warning`      | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`                 | Log File Name          | ログファイルのパスと名前を設定します。<br />デフォルトではEMQXインストールディレクトリの `log` フォルダ内の `emqx.log` に書き込みます。 | `emqx.log`     | --                                                         |
| `rotation_count`       | Max Log Files Number   | 保存可能なログファイルの最大数を設定します。                 | `10`           | `1` 〜 `2,048`                                            |
| `rotation_size`        | Rotation Size          | ログファイルがこのサイズに達するとローテーションされます。`infinity` に設定するとローテーションされません。 | `50MB`         | `1` 〜 `infinity`                                         |
| `time_offset`          | Time Offset            | ログのUTCに対する時差を設定します。                           | `system`       | --                                                         |
| `timestamp_format`     | Timestamp Format       | ログのタイムスタンプのフォーマットを設定します。               | `auto`         | `auto`: ログフォーマッターに応じて自動判別します。テキストは `rfc3339`、JSONは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日時形式。 |

## コンソールにログを出力する

EMQXをDockerコンテナで起動した場合、デフォルトのログ出力は `console` です。  
ログレベルとログフォーマットは以下の設定項目で変更可能です。

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

各設定項目の説明は以下の通りです。

| 設定項目               | ダッシュボードUI       | 説明                                                         | デフォルト値   | 選択可能な値                                               |
| ---------------------- | ---------------------- | ------------------------------------------------------------ | -------------- | ---------------------------------------------------------- |
| `formatter`            | Log Formatter          | ログのフォーマットを設定します。                             | `text`         | `text` は自由形式テキスト。<br />`json` は構造化ログ用。  |
| `level`                | Log Level              | 現在のログ出力のログレベル、つまり記録したい最低ログレベルを設定します。 | `warning`      | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`          | Time Offset            | ログのUTCに対する時差を設定します。                           | `system`       | --                                                         |
| `timestamp_format`     | Timestamp Format       | ログのタイムスタンプのフォーマットを設定します。               | `auto`         | `auto`: ログフォーマッターに応じて自動判別します。テキストは `rfc3339`、JSONは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日時形式。 |

::: tip

EMQXはより詳細なカスタマイズに対応した設定項目も提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご覧ください。

:::
