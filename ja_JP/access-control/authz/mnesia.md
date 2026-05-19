# 組み込みデータベースの使用

EMQXは、組み込みデータベースを通じて低コストかつすぐに使える認可ルールの保存方法を提供しています。Dashboardや設定ファイルで組み込みデータベース（Mnesia）をデータソースとして設定し、DashboardまたはHTTP APIを使って関連する認可チェックルールを追加できます。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)の知識

:::

## Dashboardで組み込みデータベース認可者を作成する

1. [EMQX Dashboard](http://127.0.0.1:18083/#/authentication)の左メニューから **アクセス制御** > **認可** を選択し、**認可** ページを開きます。

2. 右上の **作成** をクリックし、**バックエンド** に **組み込みデータベース** を選択してから **次へ** をクリックします。

   ![authz-mnesia_ee](./assets/authz-mnesia_ee.png)

3. **設定** ステップで、**最大ルール数**（デフォルト：`100`）を設定します。これはクライアントまたはユーザーごとに許可される認可ルールの最大数を定義します。

   ::: tip 注意

   ルール数を多く設定するとシステムのパフォーマンスに影響を与える可能性があります。

   :::

4. **作成** をクリックして設定を完了します。

## 設定ファイルで組み込みデータベース認可者を作成する

組み込みデータベース認可者は `built_in_database` タイプで識別されます。

設定例：

```bash
{
    type = built_in_database
    enable = true
}
```

- `type`：認可チェッカーのデータソースタイプ。ここには `built_in_database` を指定します。

- `enable`：このチェッカーを有効にするかどうか。オプション値は `true` または `false`。

<!--詳細なパラメータ一覧は[authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia)を参照してください。-->

## 認可ルールの作成

認可ルールはDashboardまたはAPIを通じて作成できます。

### Dashboardで認可ルールを作成する

EMQX Dashboardの **組み込みデータベース** バックエンドの **Permissions** ページから直接認可ルールを定義できます。

#### Permissionsページへのアクセス

1. Dashboardで **認可** ページに移動します。  
2. **組み込みデータベース** バックエンドの **操作** 列にある **Permissions** をクリックします。

![authz-mnesia-rule](./assets/authz-mnesia-rule.png)

#### 認可ルールのスコープ

認可ルールは以下の3つのスコープで設定可能です：

- **Client ID**：特定のクライアントIDに適用するルール。
- **Username**：特定のユーザー名に適用するルール。
- **全ユーザー**：すべてのクライアント／ユーザーに適用するルール。パターンやIP範囲で絞り込み可能。

#### 共通ルール項目

すべてのルールタイプで利用可能な項目：

| 項目                  | 説明                                                                                   |
| --------------------- | -------------------------------------------------------------------------------------- |
| **Action**            | ルールが適用される操作タイプ。選択肢：`Publish`、`Subscribe`、`Publish & Subscribe`。 |
| **Permission**        | 操作を許可するか拒否するか。選択肢：`Allow`、`Deny`。                                |
| **Topic**             | ルールが適用されるMQTTトピック。ワイルドカード（`+`、`#`）対応。                     |
| **QoS**               | 許可されるQoSレベル。複数選択可：`0`、`1`、`2`。                                    |
| **Retain**            | ルールが保持メッセージに適用されるかどうか。選択肢：`true`、`false`、`All`。          |
| **IP Address Range**  | ルールが適用されるクライアントのIP範囲。CIDR表記（例：`192.168.1.0/24`）や特定IP対応。 |
| **Listener**          | ルールが適用されるリスナー。`{type}:{name}`形式（例：`tcp:default`、`ws:default`）。    |
| **Zone**              | ルールが有効となるゾーン。マルチゾーン環境で適用。                                    |

#### スコープ別の項目

| ルールスコープ  | 項目                                                                                                           |
| --------------- | -------------------------------------------------------------------------------------------------------------- |
| **Client ID**   | **Client ID**：（必須）このルールが適用される正確なクライアントID。<br>**Username Pattern**：（任意）このルールが有効なユーザー名の正規表現。 |
| **Username**    | **Username**：（必須）このルールが適用される正確なユーザー名。<br>**Client ID Pattern**：（任意）このルールが有効なクライアントIDの正規表現。 |
| **全ユーザー**  | **Client ID Pattern**：（任意）このルールが有効なクライアントIDの正規表現。<br>**Username Pattern**：（任意）このルールが有効なユーザー名の正規表現。 |

**パターン例：**

- `^device-user-.*`：`device-user-`で始まるユーザー名にマッチ。
- `^sensor-.*`：`sensor-`で始まるクライアントIDにマッチ。

#### ルールの追加

1. **Permissions** ページで対象のタブ（**Client ID**、**Username**、**全ユーザー**）を選択します。  
2. **追加** をクリックします。  
3. [共通項目](#共通ルール項目)および[スコープ別項目](#スコープ別の項目)を入力します。  
4. （任意）**Add Permission** をクリックして複数ルールを追加可能。**上へ**、**下へ** ボタンでルールの実行順序を調整します。  
5. **追加** をクリックしてルールを保存します。

#### 複数ルールの管理（全ユーザーのみ）

**全ユーザー** ルールは、**操作** 列の **その他** メニューからルールの順序を変更できます：

- 上へ移動  
- 下へ移動  
- 先頭へ移動  
- 末尾へ移動  

ルールは上から順に評価されるため、順序が優先度を決定します。

#### ルールの編集・管理

**Permissions** ページで既存ルールの編集や削除が可能です：

- 対応するルールの **操作** 列で **編集** ボタンをクリックし、ルール項目やマッチングパターン、IP範囲設定を変更できます。  
- **削除** ボタンでルールを削除できます。

### REST APIで認可ルールを作成する

REST APIを使って認可ルールを管理することも可能です。APIエンドポイントはDashboardの3つのスコープ（Username、Client ID、全ユーザー）に対応しています。

#### エンドポイント

- **Usernameルール**  
  - `POST /authorization/sources/built_in_database/rules/users`：ユーザーのルール作成  
  - `PUT /authorization/sources/built_in_database/rules/users/:username`：特定ユーザーのルール置換  
- **Client IDルール**  
  - `POST /authorization/sources/built_in_database/rules/clients`：クライアントのルール作成  
  - `PUT /authorization/sources/built_in_database/rules/clients/:clientid`：特定クライアントのルール置換  
- **全ユーザールール**  
  - `POST /authorization/sources/built_in_database/rules/all`：すべてのクライアント／ユーザーに適用されるグローバルルールの作成または置換  
  - `PUT` リクエストはなく、`POST` でルールの更新または作成を行います。

#### ステップ1：認証トークンの取得

APIアクセス用にEMQX Dashboardで認証し、トークンを取得します：

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

- **特定クライアントIDのルール作成例**：

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

- **特定ユーザー名のルール作成例**：

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

#### 例：ユーザーのルール更新

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

#### 例：全ユーザーのルール作成

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

#### ルール項目

各ルールは以下の項目を含めることができます：

| 項目                         | 説明                                                                                       |
| ---------------------------- | ------------------------------------------------------------------------------------------ |
| **username** / **clientid**  | このルールが適用される正確なユーザー名またはクライアントID（エンドポイントによる）。       |
| **topic**                    | ルールが適用されるMQTTトピック。ワイルドカード（`+`、`#`）および[トピックプレースホルダー](./authz.md#topic-placeholders)対応。 |
| **permission**               | 現在のクライアント／ユーザーからの操作要求を許可するか拒否するか。選択肢：`allow`、`deny`。 |
| **action**                   | 操作タイプ。選択肢：`publish`、`subscribe`、`all`。                                       |
| **qos**                      | （任意）許可されるQoSレベル。例：`[0,1]`。デフォルトはすべてのレベル。                   |
| **retain**                   | （任意）保持メッセージにルールが適用されるかどうか。選択肢：`true`、`false`、`all`。      |
