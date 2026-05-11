# 組み込みデータベースの使用

EMQX は、組み込みデータベースを通じて低コストで即時に利用可能な認可ルールの保存方法を提供します。Dashboard または設定ファイルで組み込みデータベース（Mnesia）をデータソースとして設定し、Dashboard または HTTP API を通じて関連する認可チェックルールを追加できます。

::: tip 前提条件

[EMQX 認可の基本概念](./authz.md)の知識

:::

## Dashboard での設定

1. [EMQX Dashboard](http://127.0.0.1:18083/#/authentication) の左側ナビゲーションツリーで **Access Control** -> **Authorization** をクリックし、**Authorization** ページに入ります。

2. 右上の **Create** をクリックし、**Backend** に **Built-in Database** を選択して **Next** をクリックします。

   <img src="./assets/authz-mnesia_ee.png" alt="組み込みデータベース認可設定画面" style="zoom:40%;" />

3. 組み込みデータベース認可は設定パラメータを必要としないため、**Create** をクリックして完了します。

## 設定ファイルでの設定

組み込みデータベースの認可機能は、`type` が `built_in_database` で識別されます。

設定例：

```bash
{
    type = built_in_database
    enable = true
}
```

- `type`: 認可チェッカーのデータソースタイプ。ここでは `built_in_database` を指定します。

- `enable`: このチェッカーを有効にするかどうか。オプション値は `true` または `false`。

<!--詳細なパラメータ一覧は [authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia) を参照してください。-->

## 認可ルールの作成

認可ルールは Dashboard または API を通じて作成できます。

### Dashboard での作成

Dashboard の **Authorization** ページで、**Built-in Database** バックエンドの **Actions** 列にある **Permissions** ボタンをクリックします。

<img src="./assets/authz-config-built-in-rules_ee.png" alt="組み込みデータベース認可ルール設定画面" style="zoom:50%;" />

クライアント ID、ユーザー名、またはトピックに基づいて認可チェックを設定できます。

- **Client ID**: **Client ID** タブで、このルールを適用するクライアントを指定します。
- **Username**: **Username** タブで、このルールを適用するユーザーを指定します。
- **Permission**: 現在のクライアント／ユーザーからの特定の操作リクエストを許可するか拒否するか。オプション値は **Allow** または **Deny**。
- **Action**: このルールに対応する操作を設定。オプション値は **Publish**、**Subscribe**、**Publish & Subscribe**。
- **Topic**: このルールに対応するトピックを設定。

EMQX は単一のクライアントまたはユーザーに対して複数の認可チェックルールを設定可能で、ページ上の **Move Up** と **Move Down** ボタンで異なるルールの実行順序や優先度を調整できます。

複数のクライアントやユーザーに対して同時に認可チェックルールを設定したい場合は、HTTP API を通じて関連設定をインポートできます。

### API での作成

ルールは `/api/v5/authorization/sources/built_in_database` API で管理します。

各ルールは以下に適用されます：
* clientid で識別される特定のクライアント
  * `/api/v5/authorization/sources/built_in_database/clientid`
* username で識別される特定のクライアント
  * `/api/v5/authorization/sources/built_in_database/username`
* 全クライアント
  * `/api/v5/authorization/sources/built_in_database/all`

以下はクライアント (`client1`) に対するルール作成の簡単な例です：

```bash
curl -X 'POST' \
  'http://localhost:18083/api/v5/authorization/sources/built_in_database/clientid' \
  -H 'accept: */*' \
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

各ルールは以下を含みます：
* `permission`: 現在のクライアント／ユーザーからの特定の操作リクエストを許可するか拒否するか。オプション値は `allow` または `deny`。
* `action`: このルールに対応する操作。オプション値は `publish`、`subscribe`、または `all`。
* `topic`: このルールに対応するトピック。 [トピックプレースホルダー](./authz.md#topic-placeholders) をサポートします。
* `qos`: （オプション）ルールが適用される QoS レベルを指定する数値配列。例：`[0, 1]`、`[1, 2]`。デフォルトはすべての QoS レベル。
* `retain`: （オプション）現在のルールがリテインメッセージをサポートするかどうか。値は `true` または `false`。デフォルトはリテインメッセージを許可。
