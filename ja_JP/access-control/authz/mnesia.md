# 組み込みデータベースの使用

EMQX は、組み込みデータベースを通じて低コストで即時利用可能な認可ルールの保存方法を提供します。Dashboard または設定ファイルで組み込みデータベース（Mnesia）をデータソースとして設定し、Dashboard または HTTP API を通じて関連する認可チェックルールを追加できます。

::: tip 前提条件

[基本的な EMQX 認可の概念](./authz.md)の知識

:::

## Dashboard から組み込みデータベース認可者を作成する

1. [EMQX Dashboard](http://127.0.0.1:18083/#/authentication) の左メニューで **アクセス制御** > **認可** に移動し、**認可** ページを開きます。

2. 右上の **作成** をクリックし、**バックエンド** に **組み込みデータベース** を選択してから **次へ** をクリックします。

   ![authz-mnesia_ee](./assets/authz-mnesia_ee.png)

3. **設定** ステップで、**最大ルール数**（デフォルト：`100`）を設定します。これはクライアントまたはユーザーごとに許可される認可ルールの最大数を定義します。

   ::: tip 注意

   ルール数を多く設定するとシステムのパフォーマンスに影響を与える可能性があります。

   :::

4. **作成** をクリックして設定を完了します。

## 設定ファイルから組み込みデータベース認可者を作成する

組み込みデータベース認可者は `built_in_database` タイプで識別されます。

サンプル設定:

```bash
{
    type = built_in_database
    enable = true
}
```

- `type`: 認可チェッカーのデータソースタイプ。ここには `built_in_database` を指定します。

- `enable`: このチェッカーを有効にするかどうか。オプション値は `true`、`false`。

<!--詳細なパラメーター一覧は [authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia) を参照してください。-->

## 認可ルールの作成

認可ルールは Dashboard または API を通じて作成できます。

### Dashboard から認可ルールを作成する

EMQX Dashboard の **組み込みデータベース** バックエンドの **パーミッション** ページから直接認可ルールを定義できます。

#### パーミッションページへのアクセス

1. Dashboard で **認可** ページに移動します。  
2. **組み込みデータベース** バックエンドの **操作** 列で **パーミッション** をクリックします。

![authz-mnesia-rule](./assets/authz-mnesia-rule.png)

#### 認可ルールのスコープ

認可ルールは以下の3つのスコープで設定可能です：

- **Client ID**：特定のクライアントIDに適用されるルール。  
- **Username**：特定のユーザー名に適用されるルール。  
- **All Users**：すべてのクライアント／ユーザーに適用されるルール。パターンやIPレンジでフィルタリングも可能。

#### 共通ルールフィールド

すべてのルールタイプで利用可能なフィールドは以下の通りです：

| フィールド             | 説明                                                                                          |
| ---------------------- | --------------------------------------------------------------------------------------------- |
| **Action**             | ルールが適用される操作タイプ。選択肢：`Publish`、`Subscribe`、`Publish & Subscribe`。        |
| **Permission**         | 操作を許可するか拒否するか。選択肢：`Allow`、`Deny`。                                        |
| **Topic**              | このルールが適用される MQTT トピック。ワイルドカード（`+`、`#`）対応。                        |
| **QoS**                | 許可される QoS レベル。複数選択可：`0`、`1`、`2`。                                          |
| **Retain**             | ルールが保持メッセージに適用されるか。選択肢：`true`、`false`、`All`。                        |
| **IP Address Range**   | ルールが適用されるクライアントのIPレンジ。CIDR表記（例：`192.168.1.0/24`）または特定IP指定可。 |
| **Listener**           | ルールが適用されるリスナー。`{type}:{name}` 形式（例：`tcp:default`、`ws:default`）。          |
| **Zone**               | ルールが有効となるゾーン。マルチゾーン環境で適用。                                            |

#### スコープ別フィールド

| ルールスコープ | フィールド                                                                                                             |
| -------------- | ---------------------------------------------------------------------------------------------------------------------- |
| **Client ID**  | **Client ID**：（必須）このルールが適用される正確なクライアントID。<br />**Username Pattern**：（任意）このルールが有効なユーザー名の正規表現。 |
| **Username**   | **Username**：（必須）このルールが適用される正確なユーザー名。<br />**Client ID Pattern**：（任意）このルールが有効なクライアントIDの正規表現。 |
| **All Users**  | **Client ID Pattern**：（任意）このルールが有効なクライアントIDの正規表現。<br />**Username Pattern**：（任意）このルールが有効なユーザー名の正規表現。 |

**パターン例：**

- `^device-user-.*`：`device-user-` で始まるユーザー名にマッチ。  
- `^sensor-.*`：`sensor-` で始まるクライアントIDにマッチ。

#### ルールの追加

1. **パーミッション** ページで対象タブ（**Client ID**、**Username**、**All Users**）を選択します。  
2. **追加** をクリックします。  
3. [共通フィールド](#共通ルールフィールド)および[スコープ別フィールド](#スコープ別フィールド)を入力します。  
4. （任意）複数ルールを追加する場合は **パーミッションを追加** をクリックし、**上へ**、**下へ** ボタンでルールの実行順序を調整します。  
5. **追加** をクリックしてルールを保存します。

#### 複数ルールの管理（All Users のみ）

**All Users** ルールは、**操作** 列の **その他** メニューからルールの順序を変更できます：

- 上へ移動  
- 下へ移動  
- 先頭へ移動  
- 末尾へ移動  

ルールは上から順に評価されるため、順序が優先度を決定します。

#### ルールの編集と管理

**パーミッション** ページで既存ルールの編集や削除が可能です：

- 対応するルールの **操作** 列で **編集** ボタンをクリックし、ルールフィールド、マッチングパターン、IPレンジ設定を変更できます。  
- **削除** ボタンをクリックしてルールを削除できます。

### REST API から認可ルールを作成する

REST API を使って認可ルールを管理することも可能です。API エンドポイントは Dashboard の3つのスコープ（Username、Client ID、All Users）に対応しています。

#### エンドポイント

- **Username ルール**  
  - `POST /authorization/sources/built_in_database/rules/users`：ユーザーのルールを作成。  
  - `PUT /authorization/sources/built_in_database/rules/users/:username`：特定ユーザーのルールを置換。  
- **Client ID ルール**  
  - `POST /authorization/sources/built_in_database/rules/clients`：クライアントのルールを作成。  
  - `PUT /authorization/sources/built_in_database/rules/clients/:clientid`：特定クライアントのルールを置換。  
- **All Users ルール**  
  - `POST /authorization/sources/built_in_database/rules/all`：すべてのクライアント／ユーザーに適用されるグローバルルールを作成または置換。  
  - `PUT` リクエストはなく、`POST` で全ルールを更新または作成します。

#### ステップ1：認証トークンの取得

API アクセスには EMQX Dashboard で認証し、トークンを取得する必要があります：

```bash
export EMQX_TOKEN=$(curl --silent -X 'POST' "http://localhost:18083/api/v5/login" \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"username": "admin","password": "public"}' | jq -r ".token")
```

#### ステップ2：組み込みデータベース認可ソースの作成

ルール作成前に組み込みデータベース認可ソースを作成してください：

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

以下のようにルールを作成できます：

- **特定クライアントIDによるルール作成**：

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

- **特定ユーザー名によるルール作成**：

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
          "topic": "v1/devices/#",
          "permission": "allow",
          "action": "publish",
          "qos": [0,1,2],
          "retain": "all"
        }
      ]
    }
  ]'
  ```

#### 例：ユーザーのルールを更新する

```bash
curl -X PUT 'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/users/user1' \
  -H "Authorization: Bearer $EMQX_TOKEN" \
  -H 'Content-Type: application/json' \
  -d '{
    "username": "user1",
    "rules": [
      {
        "topic": "v1/devices/+/state",
        "permission": "allow",
        "action": "subscribe",
        "qos": [0,1],
        "retain": "all"
      }
    ]
  }'
```

#### 例：すべてのユーザーに対するルールを作成する

```bash
curl -X POST 'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/all' \\
  -H "Authorization: Bearer $EMQX_TOKEN" \\
  -H 'Content-Type: application/json' \\
  -d '[
    {
      "rules": [
        {
          "topic": "v1/#",
          "permission": "deny",
          "action": "all"
        }
      ]
    }
  ]'
```

#### ルールフィールド

各ルールには以下のフィールドを含めることができます：

| フィールド                 | 説明                                                                                                  |
| -------------------------- | ----------------------------------------------------------------------------------------------------- |
| **username** / **clientid** | このルールが適用される正確なユーザー名またはクライアントID（エンドポイントにより異なる）。            |
| **topic**                  | このルールが適用される MQTT トピック。ワイルドカード（`+`、`#`）および[トピックプレースホルダー](./authz.md#topic-placeholders)対応。 |
| **permission**             | 現在のクライアント／ユーザーからの操作リクエストを許可するか拒否するか。選択肢：`allow`、`deny`。      |
| **action**                 | 操作タイプ。選択肢：`publish`、`subscribe`、`all`。                                                  |
| **qos**                    | （任意）許可される QoS レベル。例：`[0,1]`。デフォルトはすべてのレベル。                            |
| **retain**                 | （任意）ルールが保持メッセージに適用されるか。選択肢：`true`、`false`、`all`。                        |
