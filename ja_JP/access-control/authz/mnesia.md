# 組み込みデータベースの使用

EMQX は、組み込みデータベースを通じて低コストで即時利用可能な認可ルールの保存方法を提供しています。Dashboard または設定ファイルで組み込みデータベース（Mnesia）をデータソースとして設定し、Dashboard または HTTP API を通じて関連する認可チェックルールを追加できます。

::: tip 前提条件

[基本的な EMQX 認可の概念](./authz.md)の知識

:::

## Dashboard での設定

1. [EMQX ダッシュボード](http://127.0.0.1:18083/#/authentication)の左メニューから **アクセス制御** > **認可** に移動し、**認可** ページを開きます。

2. 右上の **作成** をクリックし、**バックエンド** に **組み込みデータベース** を選択して、**次へ** をクリックします。

   ![authz-mnesia_ee](./assets/authz-mnesia_ee.png)

3. **設定** ステップで、クライアントまたはユーザーごとに許可される最大認可ルール数を定義する **Max Rules**（デフォルト：`100`）の値を設定します。

   ::: tip 注意

   ルール数を多く設定するとシステムのパフォーマンスに影響を与える可能性があります。

   :::

4. **作成** をクリックして設定を完了します。

## 設定ファイルでの設定

組み込みデータベース認可チェッカーは、`built_in_database` タイプで識別されます。

設定例：

```bash
{
    type = built_in_database
    enable = true
}
```

- `type`：認可チェッカーのデータソースタイプ。ここには `built_in_database` を指定します。

- `enable`：このチェッカーを有効にするかどうか。指定可能な値は `true` または `false` です。

<!--詳細なパラメーター一覧は [authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia) を参照してください。-->

## 認可ルールの作成

認可ルールは Dashboard または API を通じて作成できます。

### Dashboard での認可ルール作成

Dashboard の **認可** ページで、**組み込みデータベース** バックエンドの **操作** 列にある **権限** ボタンをクリックします。

![authz-mnesia-rule](./assets/authz-mnesia-rule.png)

クライアントID、ユーザー名、またはトピックに基づいて認可チェックを設定できます。

- **Client ID**：**Client ID** タブで、このルールを適用するクライアントを指定します。
- **Username**：**Username** タブで、このルールを適用するユーザーを指定します。
- **Permission**：現在のクライアント/ユーザーからの特定の操作要求を許可または拒否するかを設定します。指定可能な値は **Allow**（許可）、**Deny**（拒否）です。
- **Action**：このルールに対応する操作を設定します。指定可能な値は **Publish**、**Subscribe**、**Publish & Subscribe** です。
- **Topic**：このルールに対応するトピックを設定します。

EMQX は単一のクライアントまたはユーザーに対して複数の認可チェックルールを設定可能で、ページ上の **上へ移動**、**下へ移動** ボタンで異なるルールの実行順序や優先度を調整できます。

複数のクライアントまたはユーザーに対して同時に認可チェックルールを設定したい場合は、HTTP API を通じて関連設定をインポートできます。

### REST API での作成

ルールは `/api/v5/authorization/sources/built_in_database` API で管理されます。

各ルールは以下に適用されます：
* clientid で識別される特定のクライアント
  * `/api/v5/authorization/sources/built_in_database/clientid`
* username で識別される特定のクライアント
  * `/api/v5/authorization/sources/built_in_database/username`
* すべてのクライアント
  * `/api/v5/authorization/sources/built_in_database/all`

以下はクライアント (`client1`) のルールを作成する簡単な例です：

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

各ルールの内容：
* `permission`：現在のクライアント/ユーザーからの特定の操作要求を許可するか拒否するか。指定可能な値は `allow` または `deny`。
* `action`：このルールに対応する操作。指定可能な値は `publish`、`subscribe`、または `all`。
* `topic`：このルールに対応するトピック。トピックプレースホルダーをサポートします（[トピックプレースホルダー](./authz.md#topic-placeholders)参照）。
* `qos`：（オプション）ルールが適用される QoS レベルを指定する数値配列。例：`[0, 1]`、`[1, 2]`。デフォルトはすべての QoS レベル。
* `retain`：（オプション）現在のルールがリテインメッセージをサポートするかどうかを指定。値は `true` または `false`。デフォルトはリテインメッセージを許可。
