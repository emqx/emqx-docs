# 組み込みデータベースの使用

EMQXの組み込みデータベースを、低コストかつすぐに使えるパスワード認証のオプションとして利用できます。有効化すると、EMQXはクライアント認証情報を組み込みデータベース（Mnesiaベース）に保存し、REST APIやダッシュボードでデータを管理します。本ページでは、EMQXダッシュボードおよび設定項目を用いた組み込みデータベースによる認証設定方法を紹介します。

::: tip

[EMQX認証の基本概念](../authn/authn.md)の知識を推奨します。

:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、パスワード認証に組み込みデータベースを設定できます。

1. EMQXダッシュボードの左側ナビゲーションメニューから **Access Control** -> **Authentication** をクリックします。
2. **Authentication** ページの右上にある **Create** をクリックします。
3. **Mechanism** に **Password-Based**、**Backend** に **Built-in Database** を選択し、**Configuration** タブに進みます。以下のように表示されます。

<img src="./assets/authn-built-in-database.png" alt="組み込みデータベース" style="zoom:67%;" />

4. 以下の指示に従って認証バックエンドを設定します：

   - **UserID Type**：クライアントID認証に使用するフィールドを指定します。選択肢は `username`、`clientid`（MQTTクライアントが送信する `CONNECT` メッセージの `Username` または `Client Identifier` フィールドに対応）。
   - **Password Hash**：平文パスワードに適用し、結果をデータベースに保存する前のハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります：
     - `md5`、`sha`、`sha256`、`sha512` の場合：
       - **Salt Position**：パスワードとソルト（ランダムデータ）の混ぜ方を指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX組み込みデータベースへユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。
       - ハッシュ結果は16進数文字列で表現され、大文字・小文字を区別せずに保存済み認証情報と比較されます。
     - `plain` の場合：
       - **Salt Position** は `disable` にしてください。
     - `bcrypt` の場合：
       - **Salt Rounds**：ハッシュ関数の適用回数を定義します。値は _2<sup>Salt Rounds</sup>_ として表され、「コストファクター」とも呼ばれます。デフォルトは `10`、許容範囲は `5` ～ `10` です。セキュリティ向上のために高い値を推奨します。注：コストファクターを1増やすと認証にかかる時間が約2倍になります。
     - `pbkdf2` の場合：
       - **Pseudorandom Function**：鍵生成に用いるハッシュ関数を選択します（例：`sha256`）。
       - **Iteration Count**：ハッシュ関数の実行回数を設定します。デフォルトは `4096`。
       - **Derived Key Length**（任意）：生成される鍵のバイト長を指定します。空欄の場合は選択した擬似乱数関数のデフォルト長になります。
       - ハッシュ結果は16進数文字列で表現され、大文字・小文字を区別せずに保存済み認証情報と比較されます。

   - **Precondition**：[Variform式](../../configuration/configuration.md#variform-expressions)で、この組み込みデータベース認証器をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証器が呼び出されます。それ以外の場合はスキップされます。詳細は[Authenticator Preconditions](./authn.md#authenticator-preconditions)を参照してください。

5. 設定が完了したら、**Create** をクリックします。

## 設定項目による設定

設定項目を使って認証を設定することも可能です。 <!--詳細な手順は[authn-builtin_db:authentication](../../configuration/configuration-manual.html#authn-builtin_db:authentication)を参照してください。-->

例：

```hcl
{
   backend = "built_in_database"
   mechanism = "password_based"
   password_hash_algorithm {
      name = "sha256",
      salt_position = "suffix"
   }
   user_id_type = "username"
   bootstrap_file = "${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv"
   bootstrap_type = "plain"
}
```

## ファイルからのユーザー初期登録（Bootstrap）

`password_based:built_in_database` 認証器は、作成時にローカルファイルからユーザーを読み込むことをサポートしています。

この仕組みは、以下のようなデプロイ時の初期ユーザー登録（シード）に適しています：

- デフォルト管理者アカウントの作成
- 事前定義されたクライアント認証情報のプリロード
- 初回セットアップ時の初期データ準備
- 初期管理者アカウントの事前定義（`is_superuser = true` を設定）

ブートストラップは認証器作成時に一度だけ実行されます。継続的なユーザー管理や大規模なランタイム移行には適していません。EMQX稼働後の一括インポートには[Import Users](./user_management.md#import-users)を利用してください。

### ブートストラップ設定

```hocon
bootstrap_file = "${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv"
bootstrap_type = "plain"  # または "hash"
```

#### `bootstrap_file`

- デフォルト：`${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv`
- 初期ユーザーを読み込むローカルファイルを指定します。

ファイル形式は拡張子で判別されます：

- `.csv`：ヘッダー付きCSV
- `.json`：オブジェクトのJSON配列

EMQXに付属するデフォルトファイルはCSVヘッダー形式です：

```txt
user_id,password,is_superuser
```

#### `bootstrap_type`

- 値：`plain` または `hash`
- デフォルト：`plain`

ファイル内のパスワードデータの解釈方法を指定します。

### ファイル形式の要件

`bootstrap_type = plain` の場合、以下のフィールドが必要です：

- `user_id`
- `password`
- `is_superuser`（任意、デフォルトは `false`）

EMQXは `password` を設定された `password_hash_algorithm` でハッシュ化して保存します。

`bootstrap_type = hash` の場合、以下のフィールドが必要です：

- `user_id`
- `password_hash`
- `salt`（任意、デフォルトは空文字列）
- `is_superuser`（任意、デフォルトは `false`）

EMQXは `password_hash` をそのまま保存し、再ハッシュは行いません。

### ランタイム動作

認証器作成時に以下が行われます：

1. EMQXがブートストラップファイルを読み込みます。
2. CSVまたはJSONからユーザーを解析します。
3. ユーザーを組み込みデータベースに挿入します。

注意点：

- 既存ユーザーは上書きされません（`override = false`）。
- `is_superuser` は以下の場合にのみ `true` とみなされます：
  - JSONの真偽値 `true`
  - CSV/JSONの文字列 `"true"`
  - それ以外はすべて `false` と解釈されます。
- ファイル読み込みや解析エラーは警告ログとして記録されます。
- ファイルにエラーがあっても認証器作成は成功します。

## 外部ストレージからEMQX組み込みデータベースへの移行

MySQL、LDAP、他のMQTTブローカーなど外部システムからEMQX組み込みデータベースへユーザー認証情報を移行する場合は、Import Users APIを使ってユーザーを一括アップロードできます。

ブートストラップとは異なり、インポートはEMQX稼働後に実行し、運用中のデータ移行を目的としています。詳細は[Import Users](./user_management.md#import-users)を参照してください。
