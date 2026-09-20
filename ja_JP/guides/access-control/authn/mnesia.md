# 組み込みデータベースの使用

EMQXの組み込みデータベースを、低コストかつすぐに使えるパスワード認証のオプションとして利用できます。有効化すると、EMQXはクライアント認証情報を組み込みデータベース（Mnesiaベース）に保存し、REST APIやダッシュボードを通じてデータを管理します。本ページでは、EMQXダッシュボードおよび設定項目を使って、組み込みデータベースによる認証を設定する方法を紹介します。

::: tip

[EMQX認証の基本概念](./authn.md)の知識を推奨します。

:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、パスワード認証の組み込みデータベースを設定できます。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を、**バックエンド** に **組み込みデータベース** を選択し、以下のように **設定** タブに進みます。

<img src="./assets/authn-built-in-database.png" alt="組み込みデータベース" style="zoom:67%;" />

4. 以下の指示に従い認証バックエンドを設定します：

   - **ユーザーIDタイプ**：クライアントID認証に使うフィールドを指定します。選択肢は `username`、`clientid`（MQTTクライアントが送信する `CONNECT` メッセージの `Username` または `Client Identifier` フィールドに対応）。
   - **パスワードハッシュ**：平文パスワードに適用し、結果をデータベースに保存する前のハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります：
     - `md5`、`sha`、`sha256`、`sha512` の場合：
       - **ソルトの位置**：パスワードとソルト（ランダムデータ）の結合方法を指定します。`suffix`（後置）、`prefix`（前置）、`disable`（無効）から選択。外部ストレージから組み込みデータベースへユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。
       - ハッシュ結果は16進文字列で表され、大文字小文字を区別せずに保存済み認証情報と比較されます。
     - `plain` の場合：
       - **ソルトの位置** は `disable` にします。
     - `bcrypt` の場合：
       - **ソルトラウンド数**：ハッシュ関数の適用回数を定義し、2のべき乗で表される「コストファクター」です。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のために値を大きくすることが推奨されます。注意：コストファクターを1増やすと認証にかかる時間が倍増します。
     - `pbkdf2` の場合：
       - **疑似乱数関数**：鍵生成に使うハッシュ関数を選択します（例：`sha256`）。
       - **繰り返し回数**：ハッシュ関数の実行回数を設定します。デフォルトは `4096`。
       - **派生鍵の長さ**（任意）：生成される鍵のバイト長を指定します。空欄の場合は疑似乱数関数に依存した長さになります。
       - ハッシュ結果は16進文字列で表され、大文字小文字を区別せずに保存済み認証情報と比較されます。

   - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で、クライアント接続にこの組み込みデータベース認証器を適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証器が呼び出されます。それ以外はスキップされます。詳細は[認証器の前提条件](./authn.md#authenticator-preconditions)を参照してください。

5. 設定が完了したら **作成** をクリックします。

## 設定項目での設定

設定項目を使って認証を設定することも可能です。 <!--詳細な手順は [authn-builtin_db:authentication](../../configuration/configuration-manual.html#authn-builtin_db:authentication) を参照してください。-->

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

## ファイルからのユーザーブートストラップ

`password_based:built_in_database` 認証器は、認証器作成時にローカルファイルからユーザーを読み込むことをサポートしています。

この仕組みは、以下のようなデプロイ時の初期ユーザー登録（シード）に適しています：

- デフォルト管理者アカウントの作成
- 事前定義されたクライアント認証情報のプリロード
- 初回セットアップ時の初期データ準備
- 初期管理者アカウントの事前定義（`is_superuser = true` を設定）

ブートストラップは認証器作成時に一度だけ実行され、継続的なユーザー管理や大規模なランタイム移行には向きません。EMQX稼働後の一括インポートには[ユーザーのインポート](./user_management.md#import-users)を利用してください。

### ブートストラップ設定

```hocon
bootstrap_file = "${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv"
bootstrap_type = "plain"  # または "hash"
```

#### `bootstrap_file`

- デフォルト：`${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv`
- 初期ユーザー読み込みに使うローカルファイルを指定します。

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
2. CSVまたはJSONからユーザー情報を解析します。
3. ユーザーを組み込みデータベースに挿入します。

重要な注意点：

- 既存ユーザーは上書きされません（`override = false`）。
- `is_superuser` は以下の場合のみ `true` とみなされます：
  - JSONの真偽値 `true`
  - CSV/JSONの文字列 `"true"`
  - それ以外はすべて `false` と解釈されます。
- ファイルの読み込みや解析エラーは警告ログに記録されます。
- ファイルにエラーがあっても認証器作成は成功します。

## 外部ストレージからEMQX組み込みデータベースへの移行

MySQLやLDAP、他のMQTTブローカーなどの外部システムからEMQX組み込みデータベースへユーザー認証情報を移行するには、ユーザーインポートAPIを使って一括アップロードできます。

ブートストラップとは異なり、ユーザーインポートはEMQX稼働後に実行され、運用中のデータ移行を目的としています。詳細は[ユーザーのインポート](./user_management.md#import-users)を参照してください。
