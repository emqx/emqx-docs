# ログ設定

このページでは、設定ファイルを通じてEMQXのログ動作を設定する方法を紹介します。EMQXのログはダッシュボードからも設定可能です。ダッシュボードで設定する場合は、左のナビゲーションメニューから **Management** -> **Logging** をクリックしてください。ログおよびダッシュボードの設定に関する詳細は、[Logs and Observability - Logs](../observability/log.md) を参照してください。

::: tip

このページでは、設定項目に対応するダッシュボードUIのフィールドも紹介しています。  
設定ファイルからログを設定する場合は、`emqx.conf` ではなく `base.hocon` を使うことを推奨します。  
これは、`emqx.conf` に設定するとダッシュボードでの変更が一時的なものとなり、EMQX再起動時に失われるためです。

:::

EMQXは主に2種類のログハンドラーをサポートしています。コンソールログとファイルログです。さらに、ログを常にファイルに出力するための専用ハンドラーとして[監査ログ（Audit Log）](../dashboard/audit-log.md)も用意されています。

システムのデフォルトログハンドラーは環境変数 `EMQX_DEFAULT_LOG_HANDLER` により設定可能で、以下の値を受け付けます。

- `file`：ログ出力をファイルに向ける
- `console`：ログ出力をコンソールに向ける

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルトは `console` ですが、systemdの `emqx.service` ファイル経由で起動した場合は明示的に `file` に設定されます。

## ファイルとしてログを出力する

EMQXのログ出力ディレクトリは環境変数 `EMQX_LOG_DIR` で決まります。RPMやDEBパッケージでインストールした場合は `/var/log/emqx` に設定されます。そうでなければ、EMQXインストールディレクトリ内の `log` ディレクトリがログディレクトリとなります。

EMQXのDockerコンテナの場合、インストールディレクトリは `/opt/emqx` なので、ログディレクトリは `/opt/emqx/log` です。

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
```

ここで、

| 設定項目               | ダッシュボードUI       | 説明                                                         | デフォルト値   | 選択可能な値                                               |
| ---------------------- | ---------------------- | ------------------------------------------------------------ | ------------- | ---------------------------------------------------------- |
| `formatter`            | Log Formatter          | ログのフォーマットを設定します。                             | `text`        | `text` は自由テキスト形式。<br />`json` は構造化ログ形式。 |
| `level`                | Log Level              | 現在のログハンドラーのログレベル、つまり記録したい最小ログレベルを設定します。 | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`                 | Log File Name          | ログファイルのパスとファイル名を設定します。<br />デフォルトでは、EMQXインストールディレクトリの `log` フォルダ内の `emqx.log` にログを書き込みます。 | `emqx.log`    | --                                                         |
| `rotation_count`       | Max Log Files Number   | 保存可能なログファイルの最大数を設定します。                 | `10`          | `1` - `2,048`                                              |
| `rotation_size`        | Rotation Size          | ログファイルがローテーションされる最大サイズを設定します。指定サイズに達すると古いログファイルはリネームされアーカイブディレクトリに移動されます。`infinity` に設定するとローテーションされません。 | `50MB`        | `1` - `infinity`                                           |
| `time_offset`          | Time Offset            | ログのタイムスタンプのUTCからの時差を設定します。             | `system`      | --                                                         |
| `timestamp_format`     | Timestamp Format       | ログのタイムスタンプのフォーマットを設定します。             | `auto`        | `auto`: 使用中のログフォーマッターに応じて自動判別。テキストは `rfc3339`、JSONは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日付時刻文字列形式。 |

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

| 設定項目               | ダッシュボードUI       | 説明                                                         | デフォルト値   | 選択可能な値                                               |
| ---------------------- | ---------------------- | ------------------------------------------------------------ | ------------- | ---------------------------------------------------------- |
| `formatter`            | Log Formatter          | ログのフォーマットを設定します。                             | `text`        | `text` は自由テキスト形式。<br />`json` は構造化ログ形式。 |
| `level`                | Log Level              | 現在のログハンドラーのログレベル、つまり記録したい最小ログレベルを設定します。 | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`          | Time Offset            | ログのタイムスタンプのUTCからの時差を設定します。             | `system`      | --                                                         |
| `timestamp_format`     | Timestamp Format       | ログのタイムスタンプのフォーマットを設定します。             | `auto`        | `auto`: 使用中のログフォーマッターに応じて自動判別。テキストは `rfc3339`、JSONは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日付時刻文字列形式。 |

::: tip

EMQXはより詳細なカスタマイズに対応する設定項目を提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご参照ください。

:::
