# Bigtable に MQTT データを取り込む

[Cloud Bigtable](https://cloud.google.com/bigtable) は、Google Cloud 上のフルマネージドのワイドカラム型 NoSQL データベースサービスです。時系列データ、テレメトリストレージ、イベント記録、高スループットの IoT データ取り込みなど、大規模かつ低レイテンシのワークロード向けに設計されています。

EMQX はルールエンジンと Bigtable Sink を通じて Bigtable との連携をサポートしています。MQTT メッセージをルール SQL で処理し、選択したフィールドを Bigtable の行キーやセルのミューテーションにマッピングし、処理済みデータをリアルタイムで Bigtable テーブルに書き込みます。

本ページでは、Bigtable データ統合の仕組みを紹介し、EMQX ダッシュボードでの統合作成およびテストのワークフローを説明します。

## 仕組み

Bigtable データ統合は EMQX の標準機能です。MQTT データを Google Cloud にストリーミングし、デバイスのテレメトリやイベントデータを Bigtable に保存して、後のクエリ、分析、下流処理に活用できます。

![bigtable_architecture](./assets/bigtable_architecture.png)

EMQX はルールエンジンと Sink を介して MQTT データを Bigtable に転送します。処理の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスがテレメトリ、ステータス、イベントデータを MQTT トピックにパブリッシュします。
2. **ルールエンジンがメッセージを処理**：ルールエンジンはトピックで MQTT メッセージをマッチングし、SQL で Bigtable に必要なフィールドを抽出または変換します。
3. **Bigtable への書き込み**：Bigtable Sink は各ルール出力レコードを行ミューテーションとして Bigtable テーブルに書き込みます。設定した行キーや `set_cell` ミューテーションフィールドを使用します。下流アプリケーションやサービスは保存されたデータを低レイテンシのアプリケーション、時系列クエリ、分析処理、AI/ML パイプラインに利用できます。

## 特長と利点

EMQX と Bigtable の統合により、以下の利点があります。

- **高スループットの IoT データ取り込み**：大規模なテレメトリやイベントワークロードに対し、MQTT メッセージを Bigtable に書き込みます。
- **柔軟なフィールドマッピング**：ルール SQL で Bigtable の行キー、カラムファミリー、カラム修飾子、タイムスタンプ、セル値として使用するフィールドを明示的に選択・エイリアス設定できます。
- **バッチおよび非同期書き込み**：バッチモードや非同期リクエストモードを利用して書き込みスループットを向上させ、MQTT メッセージのパブリッシュへの影響を軽減します。
- **Google Cloud との統合**：MQTT データを Bigtable に保存し、他の Google Cloud サービスと連携して分析、処理、アプリケーション開発に活用できます。

## はじめる前に

Bigtable データ統合を作成する前に必要な準備を説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識
- Bigtable が有効な Google Cloud プロジェクト
- Bigtable インスタンス、テーブル、および少なくとも1つのカラムファミリー
- 利用予定の認証方式に必要な認証情報：
  - **サービスアカウント JSON**：サービスアカウントキーの JSON ファイル
  - **Workload Identity Federation (WIF)**：ワークロードアイデンティティプール、プロバイダー、プロジェクト ID、プロジェクト番号、サービスアカウントメール、外部 ID プロバイダーからの OAuth 2.0 クライアント認証情報
  - **Attached Service Account**：GCP Compute Engine 上で動作する EMQX デプロイメントで、[Attached Service Account の前提条件](#attached-service-account-prerequisites)を満たすもの

### GCP でサービスアカウントキーを作成する

**サービスアカウント JSON** 認証を利用する場合は、Google Cloud でサービスアカウントを作成し、JSON 形式のキーを生成します。

1. GCP アカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。
2. サービスアカウントに Bigtable インスタンスとテーブルへの書き込み権限を付与します。例として、対象テーブルのデータ読み書きが可能な Bigtable ロールを割り当てます。
3. 作成したサービスアカウントのメールアドレスをクリックします。
4. **Keys** タブをクリックし、**Add key** のドロップダウンから **Create new key** を選択し、JSON 形式でキーをダウンロードします。

   ::: tip

   サービスアカウントキーは安全に保管してください。後で Bigtable コネクター作成時に使用します。

   :::

### GCP で Workload Identity Federation を設定する

Workload Identity Federation (WIF) により、EMQX は長期間有効なサービスアカウントキーを使わずに GCP リソースにアクセスできます。EMQX は Microsoft Azure などの外部 ID プロバイダーからトークンを受け取り、GCP Security Token Service を介して一時的な GCP トークンに交換し、GCP サービスアカウントを代行します。トークンの更新は自動処理されます。

WIF を利用するには、コネクター作成前に GCP プロジェクトで以下を完了してください。

1. Google Cloud コンソールの **IAM & Admin** -> **Workload Identity Federation** でワークロードアイデンティティプールを作成し、**Pool ID** と **Project Number** を控えます。
2. プールにプロバイダーを追加し、**Provider ID** を控えます。OIDC ベース認証の場合、外部 ID プロバイダーから OAuth 2.0 クライアント認証情報（クライアント ID、クライアントシークレット、トークンエンドポイント URI、リクエストスコープ）を取得します。
3. ワークロードアイデンティティプールに Bigtable インスタンスとテーブルにアクセス可能な GCP サービスアカウントの代行権限を付与し、サービスアカウントメールを控えます。
4. Bigtable リソースを含むプロジェクトの **GCP プロジェクト ID** を控えます。

::: tip

詳細は[Workload Identity Federation の設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers)を参照してください。

:::

例：Microsoft Azure (Entra ID)

[Microsoft Entra ID](https://portal.azure.com/) で API を公開するアプリケーションを登録し、クライアントシークレットを作成します。コネクター設定時に以下の値を使用します。

| コネクター項目 | 値 |
| --- | --- |
| **OAuth トークンエンドポイント URI** | `https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token` |
| **OAuth クライアント ID** | `api://<application-id>` 形式のアプリケーション（クライアント）ID |
| **OAuth クライアントシークレット** | アプリケーション用に生成したクライアントシークレット |
| **OAuth リクエストスコープ** | `api://<application-id>/.default` |

::: note

**OAuth リクエストスコープ** はアプリケーションのオーディエンス (`aud`) と完全に一致する必要があります。そうでないと GCP STS とのトークン交換に失敗します。WIF プールにサービスアカウントアクセスを付与する際は、アプリケーション ID ではなく **Object ID** をサブジェクト識別子として使用してください。Object ID は Azure ポータルのアプリケーション概要ページの **Enterprise applications** にて確認できます。

:::

### Attached Service Account の前提条件

**Attached Service Account** 認証を利用するには、EMQX が GCP Compute Engine インスタンス上で動作し、そのインスタンスにサービスアカウントがアタッチされている必要があります。インスタンスの OAuth アクセススコープは Bigtable へのアクセスを許可している必要があります。Google は `cloud-platform` スコープ（`https://www.googleapis.com/auth/cloud-platform`）の使用を推奨し、サービスアカウントの権限は IAM ロールで制限してください。サービスアカウントは対象 Bigtable インスタンスとテーブルへのアクセス権を持つ必要があります。詳細は Google Cloud ドキュメントの[サービスアカウント](https://cloud.google.com/compute/docs/access/service-accounts)を参照してください。

対象の Bigtable インスタンスとテーブルは、Compute Engine インスタンスに関連付けられた GCP プロジェクト内に存在する必要があります。EMQX クラスターの場合、すべてのノードがこれらの要件を満たし、そのプロジェクトの Compute Engine インスタンス上で動作している必要があります。

コネクター起動時、EMQX はインスタンスメタデータエンドポイントから自動的に GCP プロジェクト ID とアクセストークンを取得します。サービスアカウントキーのアップロードは不要です。

### GCP で Bigtable リソースを作成・管理する

EMQX で Bigtable データ統合を設定する前に、Google Cloud で対象の Bigtable リソースを作成してください。

1. Google Cloud コンソールの **Bigtable** ページにアクセスします。
2. Bigtable インスタンスを作成または選択します。インスタンス作成時、**インスタンス名** は Google Cloud コンソールの表示名にのみ使用されます。`EMQX MQTT Messages` のようなわかりやすい名前を入力できます。**インスタンス ID** は後で EMQX で使用する値で、`emqxinst` のようなシンプルで一意な識別子にしてください。
3. テーブルを作成します。テーブル ID（例：`mqtt_messages`）を控えます。
4. テーブルに少なくとも1つのカラムファミリーを作成します（例：`cf`）。

   ::: tip

   EMQX では Google Cloud コンソールの表示名や完全修飾リソース名（`projects/<project-id>/instances/<instance-id>`）ではなく、**インスタンス ID** と **テーブル ID** を使用します。

   :::

## Bigtable コネクターを作成する

Bigtable Sink アクションを追加する前に、EMQX と Bigtable の接続を確立するための Bigtable コネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connectors** に移動します。
2. 画面右上の **Create** をクリックし、**Bigtable** を選択して **Next** をクリックします。
3. コネクター名と説明を入力します（例：`my_bigtable`）。この名前は Bigtable Sink とコネクターを紐付けるために使用され、クラスター内で一意である必要があります。
4. 認証オプションを設定します：
   - **Authentication**：EMQX が GCP に認証する方法を選択します。
     - **Service Account JSON**：[GCP でサービスアカウントキーを作成する](#gcp-でサービスアカウントキーを作成する)でエクスポートした JSON ファイルを **GCP Service Account Credentials** にアップロードします。**Select file** をクリックして JSON ファイルをアップロードできます。
     - **Workload Identity Federation (WIF)**：以下の項目を入力します。この方法はサービスアカウント JSON ファイルを必要としません。前提条件は[WIF の設定](#gcp-で-workload-identity-federation-を設定する)を参照してください。
       - **GCP Project ID**：コネクターがアクセスするリソースの GCP プロジェクト ID
       - **GCP Project Number**：コネクターがアクセスするリソースの GCP プロジェクト番号
       - **Service Account Email**：代行するサービスアカウントのメールアドレス
       - **Workload Identity Pool ID**：WIF トークン交換に使用するワークロードアイデンティティプール ID
       - **Workload Identity Provider ID**：WIF トークン交換に使用するワークロードアイデンティティプロバイダー ID
       - **Credential Type**：外部 ID プロバイダーが使用する認証情報タイプ。現在は OIDC クライアント認証情報をサポート。選択後、以下を入力：
         - **OAuth Client ID**：OAuth サーバーからトークンを要求するためのクライアント ID
         - **OAuth Client Secret**：OAuth サーバーからトークンを要求するためのクライアントシークレット
         - **OAuth Token Endpoint URI**：OIDC プロバイダーの OAuth トークンエンドポイント URI
         - **OAuth Request Scope**：OAuth サーバーからアクセストークンを要求する際のスコープ。プロバイダーが必要な場合に入力。
         - **OAuth Request Audience**：OAuth サーバーからアクセストークンを要求する際のオーディエンス。プロバイダーが必要な場合に入力。
     - **Attached Service Account**：追加項目は不要です。EMQX はインスタンスメタデータエンドポイントから GCP プロジェクト ID とアクセストークンを自動取得します。前提条件は[Attached Service Account の前提条件](#attached-service-account-prerequisites)を参照してください。
   - **Enable TLS**：デプロイメントで TLS が必要な場合は有効にします。
   - **Advanced Settings**：詳細な接続オプションを設定する場合はこのセクションを展開します。
5. **Create** をクリックする前に、**Test Connectivity** をクリックして EMQX が Bigtable に接続できるか確認できます。
6. **Create** をクリックしてコネクター設定を完了します。成功ダイアログが表示され、ルールを今すぐ作成するか尋ねられます。**Create Rule** をクリックするとコネクターが事前選択された状態でルール作成画面に進みます。**Back To Connector List** をクリックするとリストに戻り、後でルールを作成できます。

## Bigtable Sink を使ったルールの作成

MQTT メッセージを Bigtable に書き込むルールの作成方法を説明します。

1. 前のステップで **Create Rule** をクリックした場合、**Add Action** パネルが自動で開き、**Type of Action** が `Bigtable`、コネクターが事前選択されています。まずアクションを設定するため、ステップ5に進んでください。アクション作成後にルールページに戻り、ルール ID と SQL 設定を完了します。そうでない場合はダッシュボードの **Integration** -> **Rules** ページに移動し、右上の **Create** をクリックします。
2. ルール ID に `my_rule` と入力します。
3. **SQL Editor** にルール SQL を入力します。Bigtable Sink は Sink で設定したフィールド名を使ってルール出力から値を参照するため、SQL では Bigtable ミューテーションに必要なすべてのフィールドを明示的に選択し、エイリアスを付ける必要があります。

   例：

   ```sql
   SELECT
     clientid AS rk,
     'cf' AS fn,
     '' AS cq,
     payload AS v,
     publish_received_at * 1000 AS tm
   FROM
     "t/bigtable"
   ```

   この例では：

   - `rk` が Bigtable の行キーとして使われます。
   - `fn` がカラムファミリー名として使われます。
   - `cq` がカラム修飾子として使われます。
   - `tm` がマイクロ秒単位のタイムスタンプとして使われます。
   - `v` がセルの値として使われます。

   ::: tip

   Bigtable Sink のフィールドはルール出力のフィールド名を指すキー名であり、テンプレート式ではありません。SQL で必要なキーが選択されていない場合、そのメッセージの Bigtable ミューテーションを構築できません。

   :::

4. **Add Action** をクリックし、**Add Action** パネルで **Type of Action** ドロップダウンから `Bigtable` を選択します。
5. **Action** は `Create Action` のままにするか、既存の Bigtable Sink を選択します。コネクター成功ダイアログからルール作成した場合は、**Type of Action** がすでに `Bigtable` に設定され、コネクターも事前選択されています。
6. **Name** に Sink 名を入力します。**Description** に説明を入力することも可能です。
7. **Connectors** で、[Bigtable コネクターを作成する](#bigtable-コネクターを作成する)で作成した Bigtable コネクターを選択します。未選択の場合はこのパネルからプラスアイコンで新規作成も可能です。
8. Bigtable アクションパラメーターを設定します：

   | フィールド | 説明 | 例 |
   | --- | --- | --- |
   | **Instance ID** | Bigtable インスタンス識別子。完全修飾の `projects/.../instances/...` ではなくシンプルな ID を使用。 | `emqxinst` |
   | **Table ID** | Bigtable テーブル識別子。完全修飾の `projects/.../instances/.../tables/...` ではなくシンプルな ID を使用。 | `mqtt_messages` |
   | **Row Key** | メッセージの行キーを含むキー名。 | `rk` |
   | **Mutations** | 受信メッセージに適用するセルミューテーションのリスト。**Add** をクリックしてミューテーションを追加。 | - |
   | **Mutation Type** | ミューテーション操作タイプ。現在は Set Cell ミューテーションをサポート。 | `Set Cell` |
   | **Column Family** | ミューテーションのカラムファミリーを含むキー名。 | `fn` |
   | **Column Qualifier** | ミューテーションのカラム修飾子を含むキー名。 | `cq` |
   | **Timestamp (microseconds)** | ミューテーションのタイムスタンプ（マイクロ秒単位）を含むキー名。 | `tm` |
   | **Value** | ミューテーションの値を含むキー名。 | `v` |

9. メッセージ配信失敗時の信頼性向上のために **Fallback Actions** を設定することも可能です。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。
10. 必要に応じて **Advanced Settings** を設定します。詳細は[Advanced Settings](#advanced-settings)を参照してください。
11. **Create** をクリックする前に、**Test Connectivity** をクリックして Sink が Bigtable に接続できるか確認できます。
12. **Create** をクリックして Sink 設定を完了します。
13. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

## ルールのテスト

1. MQTTX を使ってトピック `t/bigtable` にメッセージをパブリッシュします：

   ```bash
   mqttx pub -i emqx_c -t t/bigtable -m '{ "msg": "hello Bigtable" }'
   ```

2. ルールと Sink のメトリクスを確認します。マッチ数と成功数が増加しているはずです。
3. Google Cloud で対象の Bigtable テーブルをクエリし、以下の内容で行が書き込まれていることを確認します：
   - 行キー：MQTT クライアント ID（例：`emqx_c`）
   - カラムファミリー：`cf`
   - カラム修飾子：空文字列
   - セル値：MQTT ペイロード

## Advanced Settings

Bigtable コネクターと Sink の一般的な詳細設定を説明します。

### コネクターの詳細設定

| フィールド | 説明 | デフォルト値 |
| --- | --- | --- |
| **Connection Pool Size** | Bigtable 用のコネクションプール内の接続数。 | `8` |
| **Connect Timeout** | Bigtable への接続確立タイムアウト。 | `5s` |
| **Start Timeout** | コネクター起動タイムアウト。 | `5s` |
| **Health Check Interval** | Bigtable 接続のヘルスチェック間隔。 | `15s` |
| **Health Check Timeout** | コネクターのヘルスチェックタイムアウト。 | `60s` |

### Sink の詳細設定

| フィールド | 説明 | デフォルト値 |
| --- | --- | --- |
| **Buffer Pool Size** | Bigtable へのデータ処理・送信に使用するバッファワーカープロセス数。 | `16` |
| **Dispatch Strategy** | バッファワーカーへのリクエスト振り分け戦略。デフォルトは MQTT クライアント ID ごとに振り分け。 | `Per Client ID` |
| **Request TTL** | バッファに入ってからリクエストが有効な最大時間。期限切れの場合は失効とみなす。 | `45s` |
| **Health Check Interval** | Bigtable 接続のヘルスチェック間隔。 | `15s` |
| **Health Check Interval Jitter** | ヘルスチェック間隔に加えるランダムジッター。 | `0ms` |
| **Health Check Timeout** | コネクターのヘルスチェックタイムアウト。 | `60s` |
| **Max Buffer Queue Size** | 各バッファワーカーの最大バッファキューサイズ。 | `256MB` |
| **Batch Size** | 1バッチあたりの最大書き込みレコード数。`1` に設定するとバッチ処理を無効化。 | `1000` |
| **Query Mode** | リクエストモード。非同期モードでは Bigtable への書き込みが MQTT メッセージパブリッシュをブロックしない。 | `Async` |
| **Inflight Window** | 非同期モードでの最大インフライトリクエスト数。同一 MQTT クライアントからのメッセージで厳密な順序が必要な場合は `1` に設定。 | `100` |

高スループットのデプロイメントでは、**Connection Pool Size**、**Buffer Pool Size**、**Dispatch Strategy**、**Batch Size**、**Inflight Window** を想定されるクラスターのワークロードに応じて調整してください。例えば、クラスター全体で 2 分間に約 11,000,000 メッセージ、5,000～10,000 MQTT 接続のワークロードを想定する場合は、本番利用前に代表的なベンチマークで設定を検証してください。
