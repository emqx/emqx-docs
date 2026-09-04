# Feature Gates

EMQX 6.3.0以降、Feature GatesはオプションのEMQX機能を有効化または無効化するデプロイ時の制御機構です。これらはEMQX起動時にのみ解決され、ランタイム中に変更することはできません。

Feature Gatesは`EMQX_FEATURES`環境変数で設定し、デプロイポリシー向けに設計されています。HOCON設定ファイルには保存されず、`cluster.hocon`にも永続化されず、ダッシュボード、REST API、CLIから変更することもできません。有効な機能セットを変更するには、デプロイ環境の`EMQX_FEATURES`を更新し、EMQXを再起動してください。

## Feature Gatesの設定

`EMQX_FEATURES`に以下のいずれかの値を設定します。

| 値 | 説明 |
| --- | --- |
| 未設定または空 | `FULL`プリセットを使用します。これはデフォルトのEMQX動作を維持します。 |
| `FULL` | すべてのオプション機能を有効にします。 |
| `ESSENTIAL` | コアアプリケーションのみでEMQXを起動します。すべてのオプション機能は無効化されます。 |
| カスタム機能リスト | 指定した機能とその依存関係を有効にします。小文字の機能名をカンマ、スペース、またはその両方で区切って指定してください。 |

例：

```bash
export EMQX_FEATURES=FULL
export EMQX_FEATURES=ESSENTIAL
export EMQX_FEATURES=dashboard,plugins
export EMQX_FEATURES="dashboard plugins"
export EMQX_FEATURES=dashboard,data_integration,metrics,plugins
```

`dashboard,plugins` と `"dashboard plugins"` は同じ効果です。スペースで区切る場合は、シェルによる分割を防ぐために値全体を引用符で囲んでください。

プリセットと機能名を混在させてはいけません。例えば、`EMQX_FEATURES=ESSENTIAL,metrics` は無効です。

::: warning
無効な値を設定するとEMQXは起動しません。未知の機能名がある場合、EMQXは`invalid_feature_specification`を`reason`に`unknown_feature`を付けてログ出力し、非ゼロステータスで終了します。
:::

## 利用可能な機能

カスタムの`EMQX_FEATURES`リストで使用可能なオプション機能は以下の通りです。

| 機能 | 説明 |
| --- | --- |
| `dashboard` | ダッシュボードUI、REST API、ダッシュボードのロールベースアクセス制御、ダッシュボードのシングルサインオン。 |
| `data_integration` | ルールエンジン、コネクター、アクション、ソース、データブリッジ。 |
| `message_transformation` | メッセージ変換。 |
| `schema_validation` | スキーマ検証。 |
| `schema_registry` | スキーマレジストリおよび関連機能で使用されるスキーマ定義。 |
| `gateways` | MQTT以外のプロトコルゲートウェイ。 |
| `cluster_link` | クラスターリンク。 |
| `multi_tenancy` | マルチテナンシーおよびネームスペース管理。 |
| `ai` | AI機能（AI補完やAgent-to-Agentレジストリを含む）。 |
| `metrics` | Prometheusメトリクスのエクスポート。 |
| `mqtt_extensions` | 遅延パブリッシュ、トピック書き換え、トピックメトリクス、自動サブスクライブ、スロースブスクライバー、MQTTストリーム、メッセージキューなどのMQTT拡張。 |
| `file_transfer` | MQTTによるファイル転送。 |
| `gcp_device` | Google IoT Coreの移行互換シム。 |
| `exhook` | 外部gRPCフック。 |
| `opentelemetry` | OpenTelemetryエクスポーター。 |
| `plugins` | サードパーティプラグインのインストールと管理のためのプラグインフレームワーク。 |

## 機能の依存関係

一部の機能は他の機能を必要とします。機能を有効にすると、EMQXは依存関係も自動的に有効にします。

| 機能 | 自動的に有効化される依存機能 |
| --- | --- |
| `data_integration` | `schema_registry` |
| `message_transformation` | `schema_registry` |
| `schema_validation` | `schema_registry` |
| `ai` | `schema_registry` |
| `metrics` | `dashboard` |
| `opentelemetry` | `dashboard` |

例えば、`EMQX_FEATURES=metrics`と設定すると、`metrics`と`dashboard`が有効になります。

## コアアプリケーション

`ESSENTIAL`プリセットはオプション機能を無効化しますが、ブローカーの動作と管理に必要なコアアプリケーションは起動します。認証と認可はコア機能であり、すべての`EMQX_FEATURES`設定で利用可能です。その他のコアアプリケーションには、MQTTブローカー、設定システム、CLI、ライセンス検証、永続化ストレージ、監査ログ、ノードリバランス、リテイナー、TLS PSK、アウトバウンドテレメトリー、共有リソースやブリッジフレームワークアプリケーションが含まれます。

既存の機能固有の設定セクションは、対応するFeature Gateが無効でも設定ファイルに残すことができます。EMQXは無効化された機能のアプリケーションを起動しないため、それらの設定セクションは機能が再度有効になるまで使用されません。

## 有効な機能の確認

ダッシュボードの **Monitoring** -> **Cluster Overview** -> **Nodes** をクリックし、**Feature Preset**列を確認してください。この列には各ノードが`full`、`essential`、または`custom`プリセットで起動したかが表示されます。`custom`は`EMQX_FEATURES`に明示的な機能リストが含まれていることを示します。停止中のノードや6.3未満のバージョンのノードはプリセットを報告しません。

ダッシュボードではプリセットのみ表示されます。正確な有効・無効機能リストを確認するには、起動ログまたはREST APIを使用してください。

EMQXは起動時に解決された機能状態をログ出力します。

```text
feature_gates_resolved
```

ログには解決された`preset`、`enabled`機能リスト、`disabled`機能リストが含まれます。

`dashboard`機能が有効な場合は、REST APIからも解決状態を照会できます。

```bash
curl -u <API_KEY>:<SECRET_KEY> http://localhost:18083/api/v5/features
```

応答例：

```json
{
  "preset": "custom",
  "enabled": ["dashboard", "metrics"],
  "disabled": ["ai", "cluster_link", "data_integration", "plugins"]
}
```

正確なリストはEMQXのエディション、バージョン、設定された機能セットに依存します。

::: tip
`GET /api/v5/features`はダッシュボードおよび管理APIアプリケーションで提供されます。`dashboard`機能が無効の場合は、起動ログから解決された機能セットを確認してください。
:::

## クラスター展開の考慮事項

運用の一貫性を保つため、クラスター内のすべてのノードで同じ`EMQX_FEATURES`値を設定してください。機能が混在したクラスターは、ノードごとに異なるREST APIやバックグラウンドアプリケーション、ノード間の挙動を示す可能性があります。

Docker Compose、Kubernetes、その他のオーケストレーションシステムを使用する場合は、共有のデプロイマニフェストに`EMQX_FEATURES`を設定し、追加および再起動されるノードが同じ値を受け取るようにしてください。

## 関連リンク

- [DockerでEMQXをインストールする](./install-docker.md)
- [Helmチャートを使ったKubernetes上のEMQXデプロイ](./kubernetes/chart.md)
- [設定ファイル](../configuration/configuration.md)
- [REST API](../admin/api.md)
