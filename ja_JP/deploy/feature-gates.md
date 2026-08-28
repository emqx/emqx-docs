# Feature Gates

EMQX 6.3.0以降、Feature GatesはオプションのEMQX機能を有効化または無効化するデプロイ時の制御機構です。これらはEMQX起動時にのみ解決され、ランタイム中に変更することはできません。

Feature Gatesは`EMQX_FEATURES`環境変数で設定し、デプロイポリシー用に設計されています。HOCON設定ファイルには保存されず、`cluster.hocon`にも永続化されません。また、ダッシュボード、REST API、CLIから変更することもできません。機能セットを変更するには、デプロイ環境の`EMQX_FEATURES`を更新し、EMQXを再起動してください。

## Feature Gatesの設定

`EMQX_FEATURES`に以下のいずれかの値を設定します：

| 値 | 説明 |
| --- | --- |
| 未設定または空 | `FULL`プリセットが使用されます。これはデフォルトのEMQX動作を維持します。 |
| `FULL` | すべてのオプション機能を有効化します。 |
| `ESSENTIAL` | コアアプリケーションのみでEMQXを起動します。すべてのオプション機能は無効化されます。 |
| カスタム機能リスト | 指定した機能とその依存関係を有効化します。小文字の機能名をカンマ、スペース、またはその両方で区切って指定してください。 |

例：

```bash
export EMQX_FEATURES=FULL
export EMQX_FEATURES=ESSENTIAL
export EMQX_FEATURES=dashboard,auth
export EMQX_FEATURES="dashboard auth"
export EMQX_FEATURES=dashboard,auth,data_integration,metrics
```

`dashboard,auth`と`"dashboard auth"`は同じ効果です。スペースで区切る場合は、シェルによる分割を防ぐために値全体を引用符で囲んでください。

プリセットと機能名を混在させないでください。例えば、`EMQX_FEATURES=ESSENTIAL,metrics`は無効です。

::: warning
無効な値を設定するとEMQXは起動しません。未知の機能名がある場合、EMQXは`invalid_feature_specification`をログに記録し、`reason`は`unknown_feature`となり、非ゼロステータスで終了します。
:::

## 利用可能な機能

カスタム`EMQX_FEATURES`リストで使用できるオプション機能は以下の通りです：

| 機能 | 説明 |
| --- | --- |
| `dashboard` | ダッシュボードUI、REST API、ダッシュボードのロールベースアクセス制御、およびダッシュボードのシングルサインオン。 |
| `auth` | 認証および認可のチェーンとバックエンド。 |
| `data_integration` | ルールエンジン、コネクター、アクション、ソース、およびデータブリッジ。 |
| `message_transformation` | メッセージ変換。 |
| `schema_validation` | スキーマ検証。 |
| `schema_registry` | スキーマレジストリおよび関連機能で使用されるスキーマ定義。 |
| `gateways` | MQTT以外のプロトコルゲートウェイ。 |
| `cluster_link` | クラスターリンク。 |
| `multi_tenancy` | マルチテナンシーおよびネームスペース管理。 |
| `ai` | AI機能（AI補完やAgent-to-Agentレジストリを含む）。 |
| `metrics` | Prometheusメトリクスのエクスポート。 |
| `mqtt_extensions` | 遅延パブリッシュ、トピック書き換え、トピックメトリクス、自動サブスクリプション、スロースブスクライバー、MQTTストリーム、メッセージキューなどのMQTT拡張機能。 |
| `file_transfer` | MQTT経由のファイル転送。 |
| `gcp_device` | Google IoT Coreの移行互換シム。 |
| `exhook` | 外部gRPCフック。 |
| `opentelemetry` | OpenTelemetryエクスポーター。 |

## 機能の依存関係

一部の機能は他の機能を必要とします。機能を有効化すると、EMQXは依存関係も自動的に有効化します。

| 機能 | 自動的に有効化される依存機能 |
| --- | --- |
| `data_integration` | `schema_registry` |
| `message_transformation` | `schema_registry` |
| `schema_validation` | `schema_registry` |
| `ai` | `schema_registry` |
| `gateways` | `auth` |
| `gcp_device` | `auth` |
| `metrics` | `dashboard`, `auth` |
| `opentelemetry` | `dashboard` |

例として、`EMQX_FEATURES=metrics`を設定すると、`metrics`、`dashboard`、`auth`が有効化されます。

## コアアプリケーション

`ESSENTIAL`プリセットはオプション機能を無効化しますが、EMQXはブローカーの動作と管理に必要なコアアプリケーションは起動します。コアアプリケーションにはMQTTブローカー、設定システム、CLI、ライセンス検証、プラグインフレームワーク、耐久ストレージ、監査ログ、ノードリバランス、リテイナー、TLS PSK、アウトバウンドテレメトリー、共有リソースおよびブリッジフレームワークアプリケーションが含まれます。

既存の機能固有の設定セクションは、対応するFeature Gateが無効化されていても設定ファイルに残して問題ありません。EMQXは無効化された機能のアプリケーションを起動しないため、これらの設定は機能が再度有効化されるまで使用されません。

## 有効化された機能の確認

EMQXは起動時に解決された機能状態をログに出力します：

```text
feature_gates_resolved
```

ログには解決された`preset`、`enabled`機能リスト、および`disabled`機能リストが含まれます。

`dashboard`機能が有効な場合は、REST APIからも解決済みの状態を問い合わせ可能です：

```bash
curl -u <API_KEY>:<SECRET_KEY> http://localhost:18083/api/v5/features
```

レスポンス例：

```json
{
  "preset": "custom",
  "enabled": ["auth", "dashboard", "metrics"],
  "disabled": ["ai", "cluster_link", "data_integration"]
}
```

リストの内容はEMQXのエディション、バージョン、設定された機能セットによって異なります。

::: tip
`GET /api/v5/features`はダッシュボードおよび管理APIアプリケーションによって提供されます。`dashboard`機能が無効な場合は、起動ログから解決済みの機能セットを確認してください。
:::

## クラスター展開時の注意点

運用の一貫性を保つため、クラスター内のすべてのノードで同じ`EMQX_FEATURES`値を設定してください。機能セットが混在したクラスターでは、ノードごとに異なるREST APIやバックグラウンドアプリケーション、ノード間の挙動が現れる可能性があります。

Docker Compose、Kubernetes、その他のオーケストレーションシステムを使用する場合は、共有のデプロイマニフェストに`EMQX_FEATURES`を設定し、追加や再起動されたノードが同じ値を受け取るようにしてください。

## 関連リンク

- [Dockerを使ったEMQXのインストール](./install-docker.md)
- [Helmチャートを使ったKubernetes上のEMQXデプロイ](./kubernetes/chart.md)
- [設定ファイル](../configuration/configuration.md)
- [REST API](../admin/api.md)
