# 組み込みデータベースの使用

EMQX は、組み込みデータベースを通じて低コストかつすぐに使える認可ルールの保存方法をユーザーに提供しています。Dashboard または設定ファイルで組み込みデータベース（Mnesia）をデータソースとして設定し、Dashboard または HTTP API を通じて関連する認可チェックルールを追加できます。

::: tip 前提条件

[EMQX 認可の基本概念](./authz.md)の知識

:::

## Dashboard での設定

1. [EMQX Dashboard](http://127.0.0.1:18083/#/authentication) の左側ナビゲーションツリーで **Access Control** -> **Authorization** をクリックし、**Authorization** ページに入ります。

2. 右上の **Create** をクリックし、**Backend** に **Built-in Database** を選択して **Next** をクリックします。

   <img src="./assets/authz-mnesia_ee.png" alt="authz-mnesia_ee" style="zoom:40%;" />

3. 組み込みデータベース認可は設定パラメータが不要なため、**Create** をクリックして完了します。

## 設定ファイルでの設定

組み込みデータベース認可は `type` が `built_in_database` で識別されます。

設定例：

```bash
{
    type = built_in_database
    enable = true
}
```

- `type`: 認可チェッカーのデータソースタイプ。ここでは `built_in_database` を指定します。

- `enable`: このチェッカーを有効にするかどうか。オプション値は `true`、`false`。

<!--詳細なパラメータ一覧は [authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia) を参照してください。-->

## 認可ルールの作成

認可ルールは Dashboard または API から作成できます。

### Dashboard での作成

Dashboard の **Authorization** ページで、**Built-in Database** バックエンドの **Actions** 列にある **Permissions** ボタンをクリックします。

<img src="./assets/authz-config-built-in-rules_ee.png" alt="authz-config-built-in-rules_ee" style="zoom:50%;" />

クライアント ID、ユーザー名、またはトピックに基づいて認可チェックを設定できます。

- **Client ID**: **Client ID** タブで、このルールを適用するクライアントを指定します。
- **Username**: **Username** タブで、このルールを適用するユーザーを指定します。
- **Permission**: 現在のクライアント/ユーザーからの特定の操作リクエストを許可するか拒否するか。オプション値：**Allow**、**Deny**。
- **Action**: このルールに対応する操作を設定します。オプション値：**Publish**、**Subscribe**、**Publish & Subscribe**。
- **Topic**: このルールに対応するトピックを設定します。

EMQX は単一のクライアントまたはユーザーに対して複数の認可チェックルールを設定可能で、ページ上の **Move Up** と **Move Down** ボタンで異なるルールの実行順序や優先度を調整できます。

複数のクライアントやユーザーに対して一括で認可チェックルールを設定したい場合は、HTTP API を通じて関連設定をインポートできます。

### API での作成

ルールは `/api/v5/authorization/sources/built_in_database` API で管理します。

Built-in Database バックエンドの認可ルールを API で管理するには、以下の手順に従います。

#### ステップ 1: 認証トークンの取得

EMQX Dashboard に認証して API アクセス用のトークンを取得します。

```bash
export EMQX_TOKEN=$(curl --silent -X 'POST' "http://localhost:18083/api/v5/login" \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"username": "admin","password": "public"}' | jq -r ".token")
```

#### ステップ 2: 組み込みデータベース認可ソースの作成

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

#### ステップ 3: 認可ルールの作成

以下の対象に対してルールを作成できます。

- **クライアント ID による特定クライアント**：

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

- `permission`: 操作を許可するか拒否するか。値は `allow`、`deny`。
- `action`: 操作タイプ。値は `publish`、`subscribe`、または `all`。
- `topic`: トピックフィルター。[トピックプレースホルダー](./authz.md#topic-placeholders)をサポート。
- `qos`: *(オプション)* このルールが適用される QoS レベルの配列。例: `[0, 1]`。指定しない場合はすべての QoS レベルに適用。
- `retain`: *(オプション)* このルールがリテインメッセージに適用されるかどうか。値は `true`、`false`。指定しない場合はリテインメッセージを許可。
