# ログ設定

このページでは、設定ファイルを通じてEMQXのログ動作を設定する方法を紹介します。EMQXのログはダッシュボードからも設定可能です。ダッシュボードで設定する場合は、左側のナビゲーションメニューから **Management** -> **Logging** をクリックしてください。ログおよびダッシュボードの設定に関する詳細は、[Logs and Observability - Logs](../observability/log.md) をご参照ください。

::: tip

このページでは、設定項目に対応するダッシュボードのUIフィールドも紹介しています。  
設定ファイルからログを設定する場合は、`emqx.conf` ではなく `base.hocon` を使用することを推奨します。  
これは、`emqx.conf` に設定を記述すると、ダッシュボードでの変更が一時的なものとなり、EMQX再起動時に失われるためです。

:::

EMQXは主に2つのログハンドラーをサポートしています。コンソールログとファイルログです。さらに、ログを常にファイルへ出力するための[監査ログ](../dashboard/audit-log.md)ハンドラーも用意されています。

システムのデフォルトログハンドラーは環境変数 `EMQX_DEFAULT_LOG_HANDLER` で設定可能で、以下の値を取ります。

- `file`：ログをファイルに出力します。
- `console`：ログをコンソールに出力します。

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルト値は `console` ですが、systemdの `emqx.service` ファイル経由で起動した場合は明示的に `file` に設定されます。

## ファイルとしてログを出力する

EMQXのログ出力ディレクトリは環境変数 `EMQX_LOG_DIR` によって決まり、RPMやDEBパッケージでインストールした場合は `/var/log/emqx` に設定されます。その他の場合は、EMQXインストールディレクトリの `log` フォルダがログディレクトリになります。

EMQXのDockerコンテナではインストールディレクトリが `/opt/emqx` なので、ログディレクトリは `/opt/emqx/log` となります。

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

各設定項目の説明は以下の通りです。

| 設定項目              | ダッシュボードUI       | 説明                                                         | デフォルト値  | 選択可能な値                                                |
| --------------------- | -------------------- | ------------------------------------------------------------ | ------------ | ------------------------------------------------------------ |
| `formatter`           | Log Formatter        | ログのフォーマットを設定します。                             | `text`       | `text`: フリーテキスト形式。<br />`json`: 構造化ログ形式。    |
| `level`               | Log Level            | 現在のログハンドラーで記録する最低ログレベルを設定します。  | `warning`    | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`                | Log File Name        | ログファイルのパスとファイル名を設定します。<br />デフォルトではEMQXインストールディレクトリの `log` フォルダ内の `emqx.log` に出力されます。 | `emqx.log`   | --                                                           |
| `rotation_count`      | Max Log Files Number | 保存可能なログファイルの最大数を設定します。                 | `10`         | `1` ～ `2,048`                                              |
| `rotation_size`       | Rotation Size        | ログファイルがこのサイズに達するとローテーションされます。<br />`infinity` に設定するとローテーションされません。 | `50MB`       | `1` ～ `infinity`                                           |
| `time_offset`         | Time Offset          | ログの時刻のUTCからのオフセットを設定します。                 | `system`     | --                                                           |
| `timestamp_format`    | Timestamp Format     | ログのタイムスタンプのフォーマットを設定します。              | `auto`       | `auto`: 使用中のフォーマッターに応じて自動判別します。テキストは `rfc3339`、JSONは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日時形式。 |

## コンソールにログを出力する

EMQXがDockerコンテナで起動される場合、デフォルトのログハンドラーは `console` です。  
以下の設定項目でログレベルとログフォーマットを設定できます。

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

| 設定項目              | ダッシュボードUI       | 説明                                                         | デフォルト値  | 選択可能な値                                                |
| --------------------- | -------------------- | ------------------------------------------------------------ | ------------ | ------------------------------------------------------------ |
| `formatter`           | Log Formatter        | ログのフォーマットを設定します。                             | `text`       | `text`: フリーテキスト形式。<br />`json`: 構造化ログ形式。    |
| `level`               | Log Level            | 現在のログハンドラーで記録する最低ログレベルを設定します。  | `warning`    | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`         | Time Offset          | ログの時刻のUTCからのオフセットを設定します。                 | `system`     | --                                                           |
| `timestamp_format`    | Timestamp Format     | ログのタイムスタンプのフォーマットを設定します。              | `auto`       | `auto`: 使用中のフォーマッターに応じて自動判別します。テキストは `rfc3339`、JSONは `epoch` を使用。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日時形式。 |

::: tip

EMQXはより詳細な設定項目も提供しており、カスタマイズに対応しています。詳細は[EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご覧ください。

:::

## Dockerでのクラッシュダンプ

Erlang VMが異常終了すると、クラッシュダンプがログディレクトリ（コンテナ内では `/opt/emqx/log`）の `erl_crash.<timestamp>.dump` に書き込まれます。このファイルはノードがダウンした時点の状態を記録しており、クラッシュのトラブルシューティングにおける主要な証拠となります。

コンソールログはクラッシュダンプファイルを保持しません。コンソールログハンドラーはランタイムログをコンテナの標準出力に書き込み、`docker logs` で確認可能です。クラッシュダンプは別途ファイルに書き込まれます。ログディレクトリがマウントされていない場合、コンテナ削除時にダンプは失われます。

EMQX起動前に、ホスト側でディレクトリを作成し、コンテナ内の `emqx` ユーザー（UID 1000）が書き込み可能にしてください。

```bash
mkdir -p $PWD/log && sudo chown 1000:1000 $PWD/log
```

その後、EMQX起動時にそのディレクトリを `/opt/emqx/log` にマウントします。

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

クラッシュダンプは数十メガバイトになることがあります。マウント先のディスク容量を考慮し、`node.crash_dump_bytes` でファイルサイズの上限を設定してください。
