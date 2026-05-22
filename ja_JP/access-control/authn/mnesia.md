# 組み込みデータベースの使用

EMQXの組み込みデータベースは、低コストかつすぐに使えるパスワード認証のオプションとして利用できます。有効化すると、EMQXはクライアント認証情報を組み込みデータベース（Mnesiaベース）に保存し、REST APIやダッシュボードを通じてデータを管理します。本ページでは、EMQXダッシュボードおよび設定項目を使った組み込みデータベースによる認証設定方法を紹介します。

::: tip

[EMQX認証の基本概念](../authn/authn.md)の知識があると理解が深まります。

:::

## ダッシュボードによる設定

EMQXダッシュボードを使って、パスワード認証に組み込みデータベースを設定できます。

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
       - **Pseudorandom Function**：鍵生成に用いるハッシュ関数を選択します（例：`sha256`）。
       - **Iteration Count**：ハッシュ関数の繰り返し回数を設定します。デフォルトは `4096`。
       - **Derived Key Length**（任意）：生成される鍵のバイト長を指定します。未指定の場合は選択した擬似乱数関数に基づく長さになります。
       - ハッシュ結果は16進数文字列で表現され、大文字・小文字を区別せずに保存済み認証情報と比較されます。

   - **Precondition**：[Variform式](../../configuration/configuration.md#variform-expressions)で、この組み込みデータベース認証器をクライアント接続に適用するか制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の `"true"` の場合のみ認証器が呼び出されます。それ以外はスキップされます。詳細は[認証器の前提条件](./authn.md#authenticator-preconditions)を参照してください。

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

`password_based:built_in_database` 認証器は、作成時にローカルファイルからユーザーを読み込むことをサポートしています。

この仕組みは、以下のようなデプロイ時のユーザー初期登録（シード）に適しています。

- デフォルト管理者アカウントの作成
- 事前定義されたクライアント認証情報のプリロード
- 初回セットアップ時の初期データ準備
- 初期管理者アカウントの事前定義（`is_superuser = true` の設定による）

ブートストラップは認証器作成時に一度だけ実行され、継続的なユーザー管理や大規模なランタイム移行には適していません。EMQX稼働後の一括インポートには[ユーザーのインポート](./user_management.md#import-users)を利用してください。

### ブートストラップ設定例

```hocon
bootstrap_file = "${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv"
bootstrap_type = "plain"  # または "hash"
```

#### `bootstrap_file`

- デフォルト：`${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv`
- 初期ユーザーを読み込むローカルファイルを指定します。

ファイル形式は拡張子で判別されます：

- `.csv`：ヘッダー付きCSV形式
- `.json`：オブジェクトのJSON配列

EMQXに付属するデフォルトファイルはCSVヘッダーを持ちます：

```txt
user_id,password,is_superuser
```

#### `bootstrap_type`

- 値：`plain` または `hash`
- デフォルト：`plain`

ファイル内のパスワードデータの解釈方法を指定します。

### ファイルフォーマット要件

`bootstrap_type = plain` の場合、以下のフィールドが必要です：

- `user_id`
- `password`
- `is_superuser`（省略可、デフォルトは `false`）

EMQXは`password`を設定された`password_hash_algorithm`でハッシュ化して保存します。

`bootstrap_type = hash` の場合、以下のフィールドが必要です：

- `user_id`
- `password_hash`
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
- ファイルにエラーがあっても認証器作成は成功します。

## 外部ストレージからEMQX組み込みデータベースへの移行

MySQL、LDAP、他のMQTTブローカーなど外部システムからEMQX組み込みデータベースへユーザー認証情報を移行する場合、インポートユーザーAPIを使ってユーザーを一括アップロードできます。

ブートストラップと異なり、インポートはEMQX稼働後に実行され、運用中のデータ移行を目的としています。操作方法は[ユーザーのインポート](./user_management.md#import-users)を参照してください。
