# ログ設定

このページでは、設定ファイルを通じてEMQXのログ動作を設定する方法を紹介します。EMQXのログはダッシュボードからも設定可能です。EMQXダッシュボードで設定する場合は、左側のナビゲーションメニューから **Management** -> **Logging** をクリックしてください。ログおよびダッシュボード設定の詳細については、[Logs and Observability - Logs](../observability/log.md) を参照してください。

::: tip

このページでは、設定項目に対応するダッシュボードのUIフィールドも紹介しています。  
設定ファイルからログを構成する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
`emqx.conf` に設定を記述すると、ダッシュボードでの変更は一時的なものとなり、EMQX再起動時に失われるためです。

:::

EMQXは主に2種類のログ出力をサポートしています：コンソールログとファイルログに加え、常にファイルにログを出力する[監査ログ](../dashboard/audit-log.md)があります。

システムのデフォルトログ出力は環境変数 `EMQX_DEFAULT_LOG_HANDLER` で設定可能で、以下の値を受け付けます：

- `file`：ログ出力をファイルに送る
- `console`：ログ出力をコンソールに送る

環境変数 `EMQX_DEFAULT_LOG_HANDLER` のデフォルトは `console` ですが、systemdの `emqx.service` ファイル経由で起動した場合は明示的に `file` に設定されます。

## ファイルとしてログを出力する

RPMおよびDEBインストールの場合、`EMQX_LOG_DIR` のデフォルトは `/var/log/emqx` です。EMQX 6.3.0以降、`/opt/emqx/log` はこのディレクトリへのシンボリックリンクとなっており、常に `/var/log/emqx` を指します。`EMQX_LOG_DIR` を変更してもこのリンクは更新されません。

その他のインストール方法では、デフォルトのログディレクトリはEMQXインストールディレクトリの下の `log` です。Dockerコンテナ内ではこのパスは `/opt/emqx/log` となります。

ファイルログ出力を行うには、ダッシュボードでファイルログ出力を設定するか、以下のように `base.hocon` ファイルを直接編集してください：

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

| 設定項目               | ダッシュボードUI       | 説明                                                         | デフォルト値   | 選択可能な値                                                |
| ---------------------- | ---------------------- | ------------------------------------------------------------ | -------------- | ----------------------------------------------------------- |
| `formatter`            | Log Formatter          | ログのフォーマットを設定します。                             | `text`         | `text` は自由テキスト。<br />`json` は構造化ログ用。       |
| `level`                | Log Level              | 現在のログ出力のログレベル、つまり記録したい最低ログレベルを設定します。 | `warning`      | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`                 | Log File Name          | ログファイルのパスとファイル名を設定します。<br />デフォルトでは、EMQXはインストールディレクトリの `log` フォルダ内の `emqx.log` にログを書き込みます。 | `emqx.log`     | --                                                          |
| `rotation_count`       | Max Log Files Number   | 保存可能なログファイルの最大数を設定します。                 | `10`           | `1` - `2,048`                                              |
| `rotation_size`        | Rotation Size          | ログファイルがこのサイズに達するとローテーションされます。`infinity` に設定するとローテーションされません。 | `50MB`         | `1` - `infinity`                                           |
| `time_offset`          | Time Offset            | ログのUTCに対する時刻オフセットを設定します。                 | `system`       | --                                                          |
| `timestamp_format`     | Timestamp Format       | ログ内のタイムスタンプのフォーマットを設定します。            | `auto`         | `auto`: ログフォーマッターに応じて自動判別。テキストは `rfc3339`、JSONは `epoch` 形式。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日時文字列形式。 |

## コンソールにログを出力する

EMQXをDockerコンテナで起動した場合、デフォルトのログ出力は `console` です。ログレベルやログフォーマットは以下の設定項目で構成できます。

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

| 設定項目               | ダッシュボードUI       | 説明                                                         | デフォルト値   | 選択可能な値                                                |
| ---------------------- | ---------------------- | ------------------------------------------------------------ | -------------- | ----------------------------------------------------------- |
| `formatter`            | Log Formatter          | ログのフォーマットを設定します。                             | `text`         | `text` は自由テキスト。<br />`json` は構造化ログ用。       |
| `level`                | Log Level              | 現在のログ出力のログレベル、つまり記録したい最低ログレベルを設定します。 | `warning`      | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`          | Time Offset            | ログのUTCに対する時刻オフセットを設定します。                 | `system`       | --                                                          |
| `timestamp_format`     | Timestamp Format       | ログ内のタイムスタンプのフォーマットを設定します。            | `auto`         | `auto`: ログフォーマッターに応じて自動判別。テキストは `rfc3339`、JSONは `epoch` 形式。<br />`epoch`: マイクロ秒精度のUnixエポック形式。<br />`rfc3339`: RFC3339準拠の日時文字列形式。 |

::: tip

EMQXはより詳細なカスタマイズに対応する設定項目も提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご覧ください。

:::

## Dockerでのクラッシュダンプ

Erlang VMが異常終了すると、クラッシュダンプがログディレクトリ（コンテナ内では `/opt/emqx/log`）に `erl_crash.<timestamp>.dump` というファイル名で書き込まれます。このファイルはノードがダウンした時点の状態を記録しており、クラッシュトラブルシューティングの主要な証拠となります。

コンソールログはクラッシュダンプファイルを保存しません。コンソールログハンドラーはランタイムログをコンテナの標準出力に書き込み、`docker logs` コマンドで閲覧可能です。クラッシュダンプは別途ファイルに書き込まれます。ログディレクトリをマウントしていない場合、コンテナ削除時にダンプは失われます。

EMQX起動前に、ホスト側でディレクトリを作成し、コンテナ内の `emqx` ユーザー（UID 1000）が書き込み可能にしてください：

```bash
mkdir -p $PWD/log && sudo chown 1000:1000 $PWD/log
```

その後、EMQX起動時にこのディレクトリを `/opt/emqx/log` にマウントします：

```bash
docker run -d --name emqx \
  -v $PWD/log:/opt/emqx/log \
  emqx/emqx-enterprise:@EE_VERSION@
```

クラッシュ後は、以下のコマンドでコンテナの出力を確認してください：

```bash
docker logs emqx
```

以下の行が `done` で終わっていれば、ダンプファイルは正常に書き込まれています：

```text
Crash dump is being written to: /opt/emqx/log/erl_crash.2026.08.31.06.56.22.dump...done
```

クラッシュダンプは数十メガバイトになることがあります。マウント先のディスク容量に注意し、`node.crash_dump_bytes` でファイルサイズの上限を設定してください。
