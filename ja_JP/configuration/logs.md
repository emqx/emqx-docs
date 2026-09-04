# ログ設定

このページでは、設定ファイルを使ったEMQXのログ動作の設定方法について紹介します。EMQXのログはダッシュボードからも設定可能です。ダッシュボードで設定する場合は、左側のナビゲーションメニューから **Management** -> **Logging** をクリックしてください。ログおよびダッシュボード設定の詳細については、[Logs and Observability - Logs](../observability/log.md) を参照してください。

::: tip

このページでは、設定項目に対応するダッシュボードUIのフィールドも紹介しています。  
設定ファイルからログを設定する場合は、`emqx.conf` ではなく `base.hocon` を使うことを推奨します。  
`emqx.conf` に設定を行うと、ダッシュボードでの変更は一時的なものとなり、EMQX再起動時に失われるためです。

:::

EMQXは主に2種類のログ出力をサポートしています。コンソールログとファイルログです。さらに、常にファイルにログを出力する[監査ログ](../dashboard/audit-log.md)もあります。

システムのデフォルトログ出力は環境変数 `EMQX_DEFAULT_LOG_HANDLER` で設定可能で、以下の値を受け入れます。

- `file`：ログ出力をファイルに向けます。
- `console`：ログ出力をコンソールに向けます。

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルトは `console` ですが、systemdの `emqx.service` ファイル経由で起動した場合は明示的に `file` に設定されます。

## ファイルとしてログを出力する

RPMおよびDEBインストールでは、`EMQX_LOG_DIR` のデフォルトは `/var/log/emqx` です。EMQX 6.3.0以降では、`/opt/emqx/log` はこのディレクトリへのシンボリックリンクとなっており、常に `/var/log/emqx` を指しています。`EMQX_LOG_DIR` を変更してもこのリンクは更新されません。

その他のインストール方法では、デフォルトのログディレクトリはEMQXインストールディレクトリの下の `log` です。Dockerコンテナ内ではこのパスは `/opt/emqx/log` となります。

ファイルログ出力を行うには、ダッシュボードでファイルログ出力を設定するか、以下のように `base.hocon` ファイルを直接編集します。

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

各項目の説明は以下の通りです。

| 設定項目              | ダッシュボードUI         | 説明                                                         | デフォルト値  | 選択可能な値                                                |
| --------------------- | ------------------------ | ------------------------------------------------------------ | ------------ | ------------------------------------------------------------ |
| `formatter`           | ログフォーマッター       | ログのフォーマットを設定します。                             | `text`       | `text` は自由形式テキスト。<br />`json` は構造化ログ形式。  |
| `level`               | ログレベル               | 現在のログ出力のログレベル（記録したい最低ログレベル）を設定します。 | `warning`    | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`                | ログファイル名           | ログファイルのパスと名前を設定します。<br />デフォルトではEMQXインストールディレクトリの `log` フォルダ内の `emqx.log` に書き込みます。 | `emqx.log`   | --                                                           |
| `rotation_count`      | 最大ログファイル数       | 保存可能なログファイルの最大数を設定します。                 | `10`         | `1` ～ `2,048`                                              |
| `rotation_size`       | ローテーションサイズ     | ログファイルがこのサイズに達したらローテーションします。`infinity` に設定するとローテーションされません。 | `50MB`       | `1` ～ `infinity`                                           |
| `time_offset`         | 時間オフセット           | ログ内の時間のUTCからのオフセットを設定します。               | `system`     | --                                                           |
| `timestamp_format`    | タイムスタンプ形式       | ログ内のタイムスタンプの形式を設定します。                    | `auto`       | `auto`: ログフォーマッターに応じて自動判別。テキストは `rfc3339`、JSONは `epoch`。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日付時刻形式。 |

## コンソールにログを出力する

EMQXがDockerコンテナ内で起動される場合、デフォルトのログ出力は `console` です。  
以下の設定項目でログレベルやログフォーマットを設定できます。

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

各項目の説明は以下の通りです。

| 設定項目              | ダッシュボードUI         | 説明                                                         | デフォルト値  | 選択可能な値                                                |
| --------------------- | ------------------------ | ------------------------------------------------------------ | ------------ | ------------------------------------------------------------ |
| `formatter`           | ログフォーマッター       | ログのフォーマットを設定します。                             | `text`       | `text` は自由形式テキスト。<br />`json` は構造化ログ形式。  |
| `level`               | ログレベル               | 現在のログ出力のログレベル（記録したい最低ログレベル）を設定します。 | `warning`    | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`         | 時間オフセット           | ログ内の時間のUTCからのオフセットを設定します。               | `system`     | --                                                           |
| `timestamp_format`    | タイムスタンプ形式       | ログ内のタイムスタンプの形式を設定します。                    | `auto`       | `auto`: ログフォーマッターに応じて自動判別。テキストは `rfc3339`、JSONは `epoch`。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日付時刻形式。 |

::: tip

EMQXはより詳細なカスタマイズに対応する設定項目も提供しています。詳しくは [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご覧ください。

:::
