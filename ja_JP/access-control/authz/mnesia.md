# 組み込みデータベースの使用

<<<<<<< HEAD
EMQXは、組み込みデータベースを通じて低コストで即時利用可能な認可ルールの保存方法を提供しています。Dashboardまたは設定ファイルで組み込みデータベース（Mnesia）をデータソースとして設定し、DashboardまたはHTTP APIを通じて関連する認可チェックルールを追加できます。
=======
EMQXは、組み込みデータベースを通じて低コストかつすぐに使える認可ルールの保存方法を提供しています。Dashboardや設定ファイルで組み込みデータベース（Mnesia）をデータソースとして設定し、DashboardまたはHTTP APIを使って関連する認可チェックルールを追加できます。
>>>>>>> origin/release-5.10

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)の知識

:::

## Dashboardから組み込みデータベース認可者を作成する

<<<<<<< HEAD
1. [EMQX Dashboard](http://127.0.0.1:18083/#/authentication)の左メニューで **アクセス制御** > **認可** に移動し、**認可** ページを開きます。

2. 右上の **作成** をクリックし、**バックエンド** に **組み込みデータベース** を選択してから **次へ** をクリックします。
=======
1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左メニューで **アクセス制御** > **認可** に移動し、**認可** ページを開きます。

2. 右上の **作成** をクリックし、**バックエンド** に **組み込みデータベース** を選択してから、**次へ** をクリックします。
>>>>>>> origin/release-5.10

   ![authz-mnesia_ee](./assets/authz-mnesia_ee.png)

3. **設定** ステップで、**最大ルール数**（デフォルト：`100`）の値を設定します。これはクライアントまたはユーザーごとに許可される最大認可ルール数を定義します。

   ::: tip 注意

   ルール数を多く設定するとシステムのパフォーマンスに影響を与える可能性があります。
<<<<<<< HEAD

   :::

4. **作成** をクリックして設定を完了します。

## 設定ファイルから組み込みデータベース認可者を作成する

組み込みデータベース認可者は、`built_in_database` タイプで識別されます。
=======
>>>>>>> origin/release-5.10

   :::

4. **作成** をクリックして設定を完了します。

## 設定ファイルから組み込みデータベース認可者を作成する

組み込みデータベース認可者は、`built_in_database` タイプで識別されます。

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

<<<<<<< HEAD
- `type`：認可チェッカーのデータソースタイプ。ここでは `built_in_database` を指定します。

- `enable`：このチェッカーを有効化するかどうか。オプション値：`true`、`false`。

<!--詳細なパラメータ一覧は[authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia)を参照してください。-->
=======
- `type`: 認可チェッカーのデータソースタイプ。ここには `built_in_database` を指定します。

- `enable`: このチェッカーを有効にするかどうか。オプション値：`true`、`false`。

<!--詳細なパラメーター一覧は [authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia) を参照してください。-->
>>>>>>> origin/release-5.10

## 認可ルールの作成

認可ルールはDashboardまたはAPIを通じて作成できます。

### Dashboardから認可ルールを作成する

<<<<<<< HEAD
EMQX Dashboardの **組み込みデータベース** バックエンドの **権限** ページから直接認可ルールを定義できます。

#### 権限ページへのアクセス

1. Dashboardで **認可** ページに移動します。  
=======
EMQXダッシュボードの **組み込みデータベース** バックエンドの **権限** ページから直接認可ルールを定義できます。

#### 権限ページへのアクセス

1. ダッシュボードで **認可** ページに移動します。  
>>>>>>> origin/release-5.10
2. **組み込みデータベース** バックエンドの **操作** 列で **権限** をクリックします。

![authz-mnesia-rule](./assets/authz-mnesia-rule.png)

#### 認可ルールのスコープ

<<<<<<< HEAD
認可ルールは以下3つのスコープで設定可能です：

- **クライアントID**：特定のクライアントIDにルールを適用。
- **ユーザー名**：特定のユーザー名にルールを適用。
- **全ユーザー**：すべてのクライアント／ユーザーにルールを適用。パターンやIP範囲でフィルタリング可能。
=======
認可ルールは以下の3つのスコープで設定可能です：

- **クライアントID**：特定のクライアントIDに適用されるルール。
- **ユーザー名**：特定のユーザー名に適用されるルール。
- **すべてのユーザー**：すべてのクライアント/ユーザーに適用されるルール。パターンやIP範囲でフィルタリング可能。
>>>>>>> origin/release-5.10

#### 共通ルールフィールド

すべてのルールタイプで利用可能なフィールド：

<<<<<<< HEAD
| フィールド              | 説明                                                                                  |
| ----------------------- | ------------------------------------------------------------------------------------- |
| **アクション**          | ルールが適用される操作タイプ。選択肢：`Publish`、`Subscribe`、`Publish & Subscribe`。 |
| **権限**                | 操作を許可するか拒否するか。選択肢：`Allow`、`Deny`。                                |
| **トピック**            | ルールが適用されるMQTTトピック。ワイルドカード（`+`、`#`）対応。                     |
| **QoS**                 | 許可されるQoSレベル。複数選択可：`0`、`1`、`2`。                                     |
| **リテイン**            | ルールが保持メッセージに適用されるか。選択肢：`true`、`false`、`All`。               |
| **IPアドレス範囲**      | ルールが適用されるクライアントIP範囲。CIDR表記（例：`192.168.1.0/24`）や特定IP対応。  |
| **リスナー**            | ルールが適用されるリスナー。`{type}:{name}`形式。例：`tcp:default`、`ws:default`。    |
| **ゾーン**              | ルールが有効となるゾーン。マルチゾーン環境で適用。                                   |

#### スコープ別フィールド

| ルールスコープ    | フィールド                                                                                   |
| ----------------- | -------------------------------------------------------------------------------------------- |
| **クライアントID** | **クライアントID**：（必須）このルールが適用される正確なクライアントID。<br>**ユーザーパターン**：（任意）このルールが有効なユーザー名の正規表現。 |
| **ユーザー名**    | **ユーザー名**：（必須）このルールが適用される正確なユーザー名。<br>**クライアントIDパターン**：（任意）このルールが有効なクライアントIDの正規表現。 |
| **全ユーザー**    | **クライアントIDパターン**：（任意）このルールが有効なクライアントIDの正規表現。<br>**ユーザーパターン**：（任意）このルールが有効なユーザー名の正規表現。 |
=======
| フィールド               | 説明                                                                                   |
| ------------------------ | -------------------------------------------------------------------------------------- |
| **アクション**           | ルールが適用される操作タイプ。選択肢：`Publish`、`Subscribe`、`Publish & Subscribe`。 |
| **許可**                 | 操作を許可するか拒否するか。選択肢：`Allow`、`Deny`。                                 |
| **トピック**             | ルールが適用されるMQTTトピック。ワイルドカード（`+`、`#`）をサポート。               |
| **QoS**                  | 許可されるQoSレベル。複数選択可：`0`、`1`、`2`。                                     |
| **リテイン**             | ルールがリテインメッセージに適用されるか。選択肢：`true`、`false`、`All`。            |
| **IPアドレス範囲**       | ルールが適用されるクライアントのIP範囲。CIDR表記（例：`192.168.1.0/24`）や特定IPを指定可。 |
| **リスナー**             | ルールが適用されるリスナー。`{type}:{name}`形式で指定（例：`tcp:default`、`ws:default`）。 |
| **ゾーン**               | ルールが有効になるゾーン。マルチゾーン環境で適用可能。                                |

#### スコープ別フィールド

| ルールスコープ    | フィールド                                                                                         |
| ----------------- | ------------------------------------------------------------------------------------------------ |
| **クライアントID** | **クライアントID**：（必須）このルールが適用される正確なクライアントID。<br>**ユーザーパターン**：（任意）このルールが有効なユーザー名を正規表現で指定。 |
| **ユーザー名**     | **ユーザー名**：（必須）このルールが適用される正確なユーザー名。<br>**クライアントIDパターン**：（任意）このルールが有効なクライアントIDを正規表現で指定。 |
| **すべてのユーザー** | **クライアントIDパターン**：（任意）このルールが有効なクライアントIDを正規表現で指定。<br>**ユーザーパターン**：（任意）このルールが有効なユーザー名を正規表現で指定。 |
>>>>>>> origin/release-5.10

**パターン例：**

- `^device-user-.*`：`device-user-`で始まるユーザー名にマッチ。
- `^sensor-.*`：`sensor-`で始まるクライアントIDにマッチ。

#### ルールの追加

<<<<<<< HEAD
1. **権限** ページで対象のタブ（**クライアントID**、**ユーザー名**、**全ユーザー**）を選択します。  
2. **追加** をクリックします。  
3. [共通フィールド](#共通ルールフィールド)および[スコープ別フィールド](#スコープ別フィールド)を入力します。  
4. （任意）複数ルールを追加する場合は **権限を追加** をクリックし、**上へ**・**下へ** ボタンでルールの実行順序を調整します。  
5. **追加** をクリックしてルールを保存します。

#### 複数ルールの管理（全ユーザーのみ）

**全ユーザー** ルールは、**操作** 列の **その他** メニューからルールの順序を変更できます：
=======
1. **権限** ページで対象のタブ（**クライアントID**、**ユーザー名**、**すべてのユーザー**）を選択します。  
2. **追加** をクリックします。  
3. [共通フィールド](#共通ルールフィールド)および[スコープ固有フィールド](#スコープ別フィールド)を入力します。  
4. （任意）複数のルールを追加する場合は **権限を追加** をクリックし、**上へ**、**下へ** ボタンでルール実行順序を調整します。  
5. **追加** をクリックしてルールを保存します。

#### 複数ルールの管理（すべてのユーザーのみ）

**すべてのユーザー** ルールでは、**操作** 列の **その他** メニューからルールの順序を変更できます：
>>>>>>> origin/release-5.10

- 上へ移動  
- 下へ移動  
- 先頭へ移動  
- 末尾へ移動  

ルールは上から順に評価されるため、順序が優先度を決定します。

<<<<<<< HEAD
#### ルールの編集・管理

**権限** ページで既存ルールの編集や削除が可能です：

- 対応するルールの **操作** 列で **編集** ボタンをクリックし、ルールフィールド、マッチングパターン、IP範囲設定を変更できます。  
- **削除** ボタンをクリックしてルールを削除できます。

### REST APIから認可ルールを作成する

REST APIを使って認可ルールを管理することも可能です。APIエンドポイントはDashboardの3つのスコープ（ユーザー名、クライアントID、全ユーザー）に対応しています。
=======
#### ルールの編集と管理

**権限** ページで既存ルールの編集や削除が可能です：

- 対応するルールの **操作** 列で **編集** ボタンをクリックし、ルールフィールド、マッチングパターン、IP範囲設定を変更します。  
- **削除** ボタンをクリックしてルールを削除します。

### REST APIで認可ルールを作成する

REST APIを使っても認可ルールを管理できます。APIエンドポイントはDashboardの3つのスコープ（ユーザー名、クライアントID、すべてのユーザー）に対応しています。
>>>>>>> origin/release-5.10

#### エンドポイント

- **ユーザー名ルール**  
<<<<<<< HEAD
  - `POST /authorization/sources/built_in_database/rules/users`：ユーザーのルール作成  
  - `PUT /authorization/sources/built_in_database/rules/users/:username`：特定ユーザーのルール置換  
- **クライアントIDルール**  
  - `POST /authorization/sources/built_in_database/rules/clients`：クライアントのルール作成  
  - `PUT /authorization/sources/built_in_database/rules/clients/:clientid`：特定クライアントのルール置換  
- **全ユーザールール**  
  - `POST /authorization/sources/built_in_database/rules/all`：すべてのクライアント／ユーザーに適用されるグローバルルールの作成または置換  
  - `PUT` リクエストはなく、`POST` でルールの更新または作成を行います。

#### ステップ1：認証トークンの取得

APIアクセスのためにEMQX Dashboardで認証し、トークンを取得します：

```bash
export EMQX_TOKEN=$(curl --silent -X 'POST' "http://localhost:18083/api/v5/login" \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"username": "admin","password": "public"}' | jq -r ".token")
```

#### ステップ2：組み込みデータベース認可ソースの作成

ルール作成前に組み込みデータベース認可ソースを作成しておきます：

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

- **クライアントIDで特定のクライアントに対して**：

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

- **ユーザー名で特定のユーザーに対して**：

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

=======
  - `POST /authorization/sources/built_in_database/rules/users`：ユーザーのルールを作成。  
  - `PUT /authorization/sources/built_in_database/rules/users/:username`：特定ユーザーのルールを置換。  
- **クライアントIDルール**  
  - `POST /authorization/sources/built_in_database/rules/clients`：クライアントのルールを作成。  
  - `PUT /authorization/sources/built_in_database/rules/clients/:clientid`：特定クライアントのルールを置換。  
- **すべてのユーザールール**  
  - `POST /authorization/sources/built_in_database/rules/all`：すべてのクライアント/ユーザーに適用されるグローバルルールを作成または置換。  
  - `PUT` リクエストはなく、`POST` で全ルールの更新または作成を行います。

#### 例：ユーザーのルールを作成する

```bash
curl -X POST 'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/users' \
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

>>>>>>> origin/release-5.10
#### 例：ユーザーのルールを更新する

```bash
curl -X PUT 'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/users/user1' \
<<<<<<< HEAD
  -H "Authorization: Bearer $EMQX_TOKEN" \
=======
>>>>>>> origin/release-5.10
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

<<<<<<< HEAD
#### 例：全ユーザーのルールを作成する

```bash
curl -X POST 'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/all' \\
  -H "Authorization: Bearer $EMQX_TOKEN" \\
  -H 'Content-Type: application/json' \\
=======
#### 例：すべてのユーザーのルールを作成する

```bash
curl -X POST 'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/all' \
  -H 'Content-Type: application/json' \
>>>>>>> origin/release-5.10
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

<<<<<<< HEAD
| フィールド                  | 説明                                                                                          |
| --------------------------- | --------------------------------------------------------------------------------------------- |
| **username** / **clientid** | このルールが適用される正確なユーザー名またはクライアントID（エンドポイントにより異なる）。    |
| **topic**                   | ルールが適用されるMQTTトピック。ワイルドカード（`+`、`#`）および[トピックプレースホルダー](./authz.md#topic-placeholders)対応。 |
| **permission**              | 現在のクライアント／ユーザーからの操作要求を許可するか拒否するか。選択肢：`allow`、`deny`。    |
| **action**                  | 操作タイプ。選択肢：`publish`、`subscribe`、`all`。                                          |
| **qos**                     | （任意）許可されるQoSレベル。例：`[0,1]`。デフォルトはすべてのレベル。                      |
| **retain**                  | （任意）保持メッセージにルールを適用するか。選択肢：`true`、`false`、`all`。                 |
=======
| フィールド                  | 説明                                                                                                  |
| --------------------------- | ----------------------------------------------------------------------------------------------------- |
| **username** / **clientid** | このルールが適用される正確なユーザー名またはクライアントID（エンドポイントに応じて）。                 |
| **topic**                   | ルールが適用されるMQTTトピック。ワイルドカード（`+`、`#`）および[トピックプレースホルダー](./authz.md#topic-placeholders)をサポート。 |
| **permission**              | 現クライアント/ユーザーからの操作要求を許可するか拒否するか。選択肢：`allow`、`deny`。                 |
| **action**                  | 操作タイプ。選択肢：`publish`、`subscribe`、`all`。                                                  |
| **qos**                     | （任意）許可されるQoSレベル。例：`[0,1]`。デフォルトはすべてのレベル。                               |
| **retain**                  | （任意）ルールがリテインメッセージに適用されるか。選択肢：`true`、`false`、`all`。                    |
>>>>>>> origin/release-5.10
