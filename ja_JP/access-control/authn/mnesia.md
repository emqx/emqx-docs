# 組み込みデータベースの使用

<<<<<<< HEAD
EMQXの組み込みデータベースは、低コストかつすぐに使えるパスワード認証のオプションとして利用できます。有効化すると、EMQXはクライアント認証情報を組み込みデータベース（Mnesiaベース）に保存し、REST APIやダッシュボードを通じてデータを管理します。本ページでは、EMQXダッシュボードおよび設定項目を使った組み込みデータベースによる認証設定方法を紹介します。

::: tip

[EMQX認証の基本概念](../authn/authn.md)の知識があると理解が深まります。
=======
EMQXの組み込みデータベースをパスワード認証の低コストかつすぐに使えるオプションとして利用できます。有効化すると、EMQXはクライアントの認証情報を組み込みデータベース（Mnesiaベース）に保存し、REST APIやダッシュボードを通じてデータを管理します。本ページでは、EMQXダッシュボードと設定項目を使った組み込みデータベースによる認証設定方法を紹介します。

::: tip

[EMQX認証の基本概念](../authn/authn.md)の知識があると理解しやすいです。
>>>>>>> origin/release-5.10

:::

## ダッシュボードによる設定

EMQXダッシュボードを使って、パスワード認証に組み込みデータベースを設定できます。

<<<<<<< HEAD
1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を、**バックエンド** に **組み込みデータベース** を選択し、下図のように **設定** タブに進みます。

<img src="./assets/authn-built-in-database.png" alt="組み込みデータベース" style="zoom:67%;" />

4. 以下の指示に従い認証バックエンドを設定します。

   - **UserID Type**：クライアントID認証に使用するフィールドを指定します。選択肢は `username`、`clientid`（MQTTクライアントが送信する`CONNECT`メッセージの`Username`または`Client Identifier`フィールドに対応）。
   - **Password Hash**：平文パスワードに適用するハッシュアルゴリズムを選択し、結果をデータベースに保存します。利用可能なアルゴリズムは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります：
     - `md5`、`sha`、`sha256`、`sha512` の場合：
       - **Salt Position**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`（後置）、`prefix`（前置）、`disable`（無効化）から選択可能です。外部ストレージからEMQX組み込みデータベースにユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。
       - ハッシュ結果は16進数文字列で表現され、大文字・小文字を区別せずに保存済み認証情報と比較されます。
     - `plain` の場合：
       - **Salt Position** は `disable` に設定してください。
     - `bcrypt` の場合：
       - **Salt Rounds**：ハッシュ関数を適用する回数を2のべき乗で表す「コストファクター」です。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためにはより高い値が推奨されます。コストファクターを1増やすごとに認証に必要な時間が倍増します。
     - `pbkdf2` の場合：
       - **Pseudorandom Function**：鍵生成に使うハッシュ関数を選択します（例：`sha256`）。
       - **Iteration Count**：ハッシュ関数の繰り返し回数を設定します。デフォルトは `4096`。
       - **Derived Key Length**（任意）：生成される鍵のバイト長を指定します。未指定の場合は選択した擬似乱数関数に基づく長さになります。
       - ハッシュ結果は16進数文字列で表現され、大文字・小文字を区別せずに保存済み認証情報と比較されます。

   - **Precondition**：[Variform式](../../configuration/configuration.md#variform-expressions)で、この組み込みデータベース認証器をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の `"true"` の場合のみ認証器が呼び出されます。それ以外はスキップされます。詳細は[認証器の前提条件](./authn.md#authenticator-preconditions)を参照してください。
=======
1. EMQXダッシュボードの左側ナビゲーションメニューから **Access Control** -> **Authentication** をクリックします。
2. **Authentication** ページの右上にある **Create** をクリックします。
3. **Mechanism** に **Password-Based** を、**Backend** に **Built-in Database** を選択すると、下図のように **Configuration** タブに遷移します。

<img src="./assets/authn-built-in-database.png" alt="組み込みデータベース" style="zoom:67%;" />

4. 以下の手順に従い認証バックエンドを設定します：

   - **UserID Type**：クライアントID認証に使用するフィールドを指定します。選択肢は `username`、`clientid`（MQTTクライアントが送信する `CONNECT` メッセージの `Username` または `Client Identifier` フィールドに対応）。
   - **Password Hash**：平文パスワードに適用し、結果をデータベースに保存する前のハッシュアルゴリズムを選択します。利用可能なアルゴリズムは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります：
     - `md5`、`sha`、`sha256`、`sha512` の場合：
       - **Salt Position**：ソルト（ランダムデータ）をパスワードに混ぜる位置を指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX組み込みデータベースにユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。
       - ハッシュ結果は16進文字列で表現され、大文字小文字を区別せずに保存済み認証情報と比較されます。
     - `plain` の場合：
       - **Salt Position** は `disable` に設定してください。
     - `bcrypt` の場合：
       - **Salt Rounds**：ハッシュ関数の適用回数を 2のべき乗で表した「コストファクター」です。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためには高い値が推奨されます。コストファクターを1増やすと認証にかかる時間が約2倍になります。
     - `pbkdf2` の場合：
       - **Pseudorandom Function**：鍵生成に使うハッシュ関数を選択します（例：`sha256`）。
       - **Iteration Count**：ハッシュ関数の繰り返し回数。デフォルトは `4096`。
       - **Derived Key Length**（任意）：生成される鍵のバイト長。未指定の場合は選択した擬似乱数関数の長さが使われます。
       - ハッシュ結果は16進文字列で表現され、大文字小文字を区別せずに保存済み認証情報と比較されます。

   - **Precondition**：[Variform式](../../configuration/configuration.md#variform-expressions)で、この組み込みデータベース認証器をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証器が呼び出されます。それ以外の場合はスキップされます。詳細は[Authenticator Preconditions](./authn.md#authenticator-preconditions)を参照してください。
>>>>>>> origin/release-5.10

5. 設定が完了したら **Create** をクリックします。

## 設定項目での設定

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

## ファイルからのユーザー初期登録（ブートストラップ）

<<<<<<< HEAD
`password_based:built_in_database` 認証器は、作成時にローカルファイルからユーザーを読み込むことをサポートしています。

この仕組みは、以下のようなデプロイ時のユーザー初期登録（シード）に適しています。
=======
`password_based:built_in_database` 認証器は、作成時にローカルファイルからユーザーをロードする機能をサポートしています。

この機能は、以下のようなデプロイ時の初期ユーザー登録（シード）に適しています：
>>>>>>> origin/release-5.10

- デフォルト管理者アカウントの作成
- 事前定義されたクライアント認証情報のプリロード
- 初回セットアップ時の初期データ準備
<<<<<<< HEAD
- 初期管理者アカウントの事前定義（`is_superuser = true` の設定による）

ブートストラップは認証器作成時に一度だけ実行され、継続的なユーザー管理や大規模なランタイム移行には適していません。EMQX稼働後の一括インポートには[ユーザーのインポート](./user_management.md#import-users)を利用してください。

### ブートストラップ設定例
=======
- 初期管理者アカウントの事前定義（`is_superuser = true` を設定）

ブートストラップは認証器作成時に一度だけ実行され、継続的なユーザー管理や大規模なランタイム移行には向いていません。EMQX稼働後の一括インポートには[ユーザーインポート](./user_management.md#import-users)を利用してください。

### ブートストラップ設定
>>>>>>> origin/release-5.10

```hocon
bootstrap_file = "${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv"
bootstrap_type = "plain"  # または "hash"
```

#### `bootstrap_file`

<<<<<<< HEAD
- デフォルト：`${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv`
- 初期ユーザーを読み込むローカルファイルを指定します。
=======
- デフォルト値：`${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv`
- 初期ユーザーをロードするローカルファイルを指定します。
>>>>>>> origin/release-5.10

ファイル形式は拡張子で判別されます：

- `.csv`：ヘッダー付きCSV形式
- `.json`：オブジェクトのJSON配列

<<<<<<< HEAD
EMQXに付属するデフォルトファイルはCSVヘッダーを持ちます：
=======
EMQXに付属のデフォルトファイルはCSVヘッダーを持ちます：
>>>>>>> origin/release-5.10

```txt
user_id,password,is_superuser
```

#### `bootstrap_type`

- 値：`plain` または `hash`
- デフォルト：`plain`

ファイル内のパスワードデータの解釈方法を指定します。

<<<<<<< HEAD
### ファイルフォーマット要件
=======
### ファイル形式の要件
>>>>>>> origin/release-5.10

`bootstrap_type = plain` の場合、以下のフィールドが必要です：

- `user_id`
- `password`
<<<<<<< HEAD
- `is_superuser`（省略可、デフォルトは `false`）

EMQXは`password`を設定された`password_hash_algorithm`でハッシュ化して保存します。
=======
- `is_superuser`（任意、デフォルトは `false`）

EMQXは `password` を設定された `password_hash_algorithm` でハッシュ化して保存します。
>>>>>>> origin/release-5.10

`bootstrap_type = hash` の場合、以下のフィールドが必要です：

- `user_id`
- `password_hash`
<<<<<<< HEAD
- `salt`（省略可、デフォルトは空文字列）
- `is_superuser`（省略可、デフォルトは `false`）

EMQXは`password_hash`をそのまま保存し再ハッシュは行いません。

### ランタイム動作

認証器作成時に以下の処理が行われます：

1. EMQXがブートストラップファイルを読み込みます。
2. CSVまたはJSONからユーザー情報を解析します。
3. ユーザーを組み込みデータベースに挿入します。

重要な注意点：

- 既存ユーザーは上書きされません（`override = false`）。
- `is_superuser` は以下の場合にのみ `true` と判定されます：
  - JSONの真偽値 `true`
  - CSV/JSONの文字列 `"true"`
  - それ以外はすべて `false` とみなされます。
- ファイル読み込みや解析エラーは警告ログに記録されるのみです。
=======
- `salt`（任意、デフォルトは空文字列）
- `is_superuser`（任意、デフォルトは `false`）

EMQXは `password_hash` をそのまま保存し、再ハッシュは行いません。

### 実行時の動作

認証器作成時に以下を実行します：

1. ブートストラップファイルを読み込み
2. CSVまたはJSONからユーザー情報を解析
3. 組み込みデータベースにユーザーを挿入

注意点：

- 既存ユーザーは上書きされません（`override = false`）。
- `is_superuser` は以下の場合にのみ `true` とみなされます：
  - JSONの真偽値 `true`
  - CSV/JSONの文字列 `"true"`
  - それ以外はすべて `false` と解釈されます。
- ファイルの読み込みや解析エラーは警告ログに記録されます。
>>>>>>> origin/release-5.10
- ファイルにエラーがあっても認証器作成は成功します。

## 外部ストレージからEMQX組み込みデータベースへの移行

<<<<<<< HEAD
MySQL、LDAP、他のMQTTブローカーなど外部システムからEMQX組み込みデータベースへユーザー認証情報を移行する場合、インポートユーザーAPIを使ってユーザーを一括アップロードできます。

ブートストラップと異なり、インポートはEMQX稼働後に実行され、運用中のデータ移行を目的としています。操作方法は[ユーザーのインポート](./user_management.md#import-users)を参照してください。
=======
MySQL、LDAP、他のMQTTブローカーなど外部システムからEMQX組み込みデータベースへユーザー認証情報を移行する場合、Import Users APIを使ってユーザーを一括アップロードできます。

ブートストラップと異なり、ユーザーインポートはEMQX稼働後に実行され、運用中のデータ移行を目的としています。詳細は[ユーザーインポート](./user_management.md#import-users)を参照してください。
>>>>>>> origin/release-5.10
