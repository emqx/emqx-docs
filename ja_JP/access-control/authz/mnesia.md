# 組み込みデータベースの使用

EMQXは、組み込みデータベースを通じて低コストで即時利用可能な認可ルールの保存方法を提供しています。Dashboardや設定ファイルで組み込みデータベース（Mnesia）をデータソースとして設定し、DashboardまたはHTTP APIを通じて関連する認可チェックルールを追加できます。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)の知識

:::

## Dashboardでの設定

1. [EMQX Dashboard](http://127.0.0.1:18083/#/authentication)の左ナビゲーションツリーで **Access Control** -> **Authorization** をクリックし、**Authorization** ページに入ります。

2. 右上の **Create** をクリックし、**Backend** に **Built-in Database** を選択して **Next** をクリックします。

   <img src="./assets/authz-mnesia_ee.png" alt="authz-mnesia_ee" style="zoom:40%;" />

3. 組み込みデータベース認可は設定パラメータを必要としないため、**Create** をクリックして完了します。

## 設定ファイルでの設定

組み込みデータベース認可は、`type` が `built_in_database` で識別されます。

設定例:

```bash
{
    type = built_in_database
    enable = true
}
```

- `type`: 認可チェッカーのデータソースタイプ。ここには `built_in_database` を指定します。

- `enable`: このチェッカーを有効にするかどうか。オプション値は `true` または `false`。

<!--詳細なパラメータ一覧は [authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia) を参照してください。-->

## 認可ルールの作成

認可ルールはDashboardまたはAPIを通じて作成できます。

### Dashboardでの作成

Dashboardの **Authorization** ページで、**Built-in Database** バックエンドの **Actions** 列にある **Permissions** ボタンをクリックします。

<img src="./assets/authz-config-built-in-rules_ee.png" alt="authz-config-built-in-rules_ee" style="zoom:50%;" />

クライアントID、ユーザー名、またはトピックに基づいて認可チェックを設定できます。

- **Client ID**: **Client ID** タブで、このルールを適用するクライアントを指定します。
- **Username**: **Username** タブで、このルールを適用するユーザーを指定します。
- **Permission**: 現在のクライアント/ユーザーからの特定の操作リクエストを許可するか拒否するか。オプション値は **Allow** または **Deny**。
- **Action**: このルールに対応する操作を設定します。オプション値は **Publish**、**Subscribe**、**Publish & Subscribe**。
- **Topic**: このルールに対応するトピックを設定します。

EMQXは単一のクライアントまたはユーザーに対して複数の認可チェックルールを設定可能であり、ページ上の **Move Up** と **Move Down** ボタンで異なるルールの実行順序と優先度を調整できます。

複数のクライアントやユーザーに対して一括で認可チェックルールを設定したい場合は、HTTP APIを通じて関連設定をインポートできます。

### APIでの作成

ルールは `/api/v5/authorization/sources/built_in_database` APIを通じて管理します。

組み込みデータベースバックエンドの認可ルールをAPIで管理する手順は以下の通りです。

#### ステップ1: 認証トークンの取得

APIアクセス用のトークンを取得するため、EMQX Dashboardに認証します。

```bash
export EMQX_TOKEN=$(curl --silent -X 'POST' "http://localhost:18083/api/v5/login" \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"username": "admin","password": "public"}' | jq -r ".token")
```

#### ステップ2: 組み込みデータベース認可ソースの作成

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

#### ステップ3: 認可ルールの作成

以下の対象に対してルールを作成できます。

- **クライアントIDによる特定クライアント**:

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

- **ユーザー名による特定ユーザー**:

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

- **全クライアントに対してグローバルに**:

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

各ルールには以下の項目が含まれます。

- `permission`: 操作を許可するか拒否するか。値は `allow` または `deny`。
- `action`: 操作タイプ。`publish`、`subscribe`、または `all`。
- `topic`: トピックフィルター。[トピックプレースホルダー](./authz.md#topic-placeholders)をサポート。
- `qos`: *(オプション)* このルールが適用されるQoSレベルの配列。例: `[0, 1]`。指定しない場合はすべてのQoSレベルに適用。
- `retain`: *(オプション)* 保持メッセージに対してルールを適用するかどうか。値は `true` または `false`。デフォルトは保持メッセージを許可。
