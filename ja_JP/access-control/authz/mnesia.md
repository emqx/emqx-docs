# 組み込みデータベースの利用

EMQXは、組み込みデータベースを通じて低コストで即時利用可能な認可ルールの保存方法を提供しています。Dashboardや設定ファイルで組み込みデータベース（Mnesia）をデータソースとして設定し、DashboardまたはHTTP APIを通じて関連する認可チェックルールを追加できます。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)の知識

:::

## Dashboardでの設定

1. [EMQX Dashboard](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで**アクセス制御** -> **認可**をクリックし、**認可**ページに入ります。

2. 右上の**作成**をクリックし、**バックエンド**に**組み込みデータベース**を選択して、**次へ**をクリックします。

   <img src="./assets/authz-mnesia_ee.png" alt="authz-mnesia_ee" style="zoom:40%;" />

3. 組み込みデータベース認可は設定パラメータが不要なため、**作成**をクリックして完了します。

## 設定ファイルでの設定

組み込みデータベース認可は、タイプ `built_in_database` で識別されます。

設定例：

```bash
{
    type = built_in_database
    enable = true
}
```

- `type`: 認可チェッカーのデータソースタイプ。ここでは `built_in_database` を指定します。

- `enable`: このチェッカーを有効化するかどうか。オプション値は `true`、`false`。

<!--詳細なパラメータ一覧は[authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia)を参照してください。-->

## 認可ルールの作成

認可ルールはDashboardまたはAPIを通じて作成できます。

### Dashboardでの作成

Dashboardの**認可**ページで、**組み込みデータベース**バックエンドの**操作**列にある**権限**ボタンをクリックします。

<img src="./assets/authz-config-built-in-rules_ee.png" alt="authz-config-built-in-rules_ee" style="zoom:50%;" />

クライアントID、ユーザー名、またはトピックに基づいて認可チェックを設定できます。

- **Client ID**：**Client ID**タブで、このルールを適用するクライアントを指定します。
- **Username**：**Username**タブで、このルールを適用するユーザーを指定します。
- **Permission**：現在のクライアント／ユーザーからの特定操作リクエストを許可するか拒否するか。オプション値は**Allow**、**Deny**。
- **Action**：このルールに対応する操作を設定。オプション値は**Publish**、**Subscribe**、**Publish & Subscribe**。
- **Topic**：このルールに対応するトピックを設定します。

EMQXは単一のクライアントまたはユーザーに対して複数の認可チェックルールを設定可能で、ページ上の**上へ移動**、**下へ移動**ボタンで異なるルールの実行順序と優先度を調整できます。

複数のクライアントやユーザーに対して一括で認可チェックルールを設定したい場合は、HTTP APIを通じて関連設定をインポートできます。

### APIでの作成

ルールは `/api/v5/authorization/sources/built_in_database` APIを通じて管理されます。

組み込みデータベースバックエンドの認可ルールをAPIで管理する手順は以下の通りです。

#### ステップ1：認証トークンの取得

EMQX Dashboardに認証し、APIアクセス用のトークンを取得します。

```bash
export EMQX_TOKEN=$(curl --silent -X 'POST' "http://localhost:18083/api/v5/login" \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"username": "admin","password": "public"}' | jq -r ".token")
```

#### ステップ2：組み込みデータベース認可ソースの作成

```bash
curl -X 'POST' \
  'http://localhost:18083/api/v5/authorization/sources' \
  -H "Authorization: Bearer $EMQX_TOKEN" \
  -H 'Accept: */*' \
  -H 'Content-Type: application/json' \
  -d '{
        "enable": true,
        "max_rules": 100,
        "type": "built_in_database"
  }'
```

#### ステップ3：認可ルールの作成

以下の対象に対してルールを作成できます。

- **クライアントIDによる特定クライアント**：

  ```bash
  curl -X 'POST' \
    'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/clients' \
    -H "Authorization: Bearer $EMQX_TOKEN" \
    -H 'Accept: */*' \
    -H 'Content-Type: application/json' \
    -d '[
    {
      "clientid": "client1",
      "rules": [
        {
          "action": "publish",
          "permission": "allow",
          "topic": "test/topic/1"
        },
        {
          "action": "subscribe",
          "permission": "allow",
          "topic": "test/topic/2"
        },
        {
          "action": "all",
          "permission": "deny",
          "topic": "eq test/#"
        }
      ]
    }
  ]'
  ```

- **ユーザー名による特定ユーザー**：

  ```bash
  curl -X 'POST' \
    'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/users' \
    -H "Authorization: Bearer $EMQX_TOKEN" \
    -H 'Accept: */*' \
    -H 'Content-Type: application/json' \
    -d '[
    {
      "username": "user1",
      "rules": [
        {
          "action": "publish",
          "permission": "allow",
          "topic": "test/topic/1"
        },
        {
          "action": "subscribe",
          "permission": "allow",
          "topic": "test/topic/2"
        },
        {
          "action": "all",
          "permission": "deny",
          "topic": "eq test/#"
        }
      ]
    }
  ]'
  ```

- **全クライアントに対してグローバルに**：

  ```bash
  curl -X 'POST' \
    'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/all' \
    -H "Authorization: Bearer $EMQX_TOKEN" \
    -H 'Accept: */*' \
    -H 'Content-Type: application/json' \
    -d '{
    "rules": [
      {
        "action": "publish",
        "permission": "allow",
        "topic": "test/topic/1"
      },
      {
        "action": "subscribe",
        "permission": "allow",
        "topic": "test/topic/2"
      },
      {
        "action": "all",
        "permission": "deny",
        "topic": "eq test/#"
      }
    ]
  }'
  ```

各ルールには以下を含みます：

- `permission`：操作を許可するか拒否するか。値は `allow`、`deny`。
- `action`：操作タイプ。値は `publish`、`subscribe`、または `all`。
- `topic`：トピックフィルター。[トピックプレースホルダー](./authz.md#topic-placeholders)をサポート。
- `qos`：（任意）このルールが適用されるQoSレベルの配列。例：`[0, 1]`。指定しない場合は全QoSレベルが対象。
- `retain`：（任意）保持メッセージにルールを適用するかどうか。値は `true`、`false`。デフォルトは保持メッセージを許可。
