# 組み込みデータベースの使用

EMQXの組み込みデータベースを、低コストかつすぐに使えるパスワード認証のオプションとして利用できます。有効化すると、EMQXはクライアントの認証情報を組み込みデータベース（Mnesiaベース）に保存し、REST APIやダッシュボードを通じてデータを管理します。本ページでは、EMQXダッシュボードと設定項目を使って組み込みデータベースによる認証を設定する方法を紹介します。

::: tip

[EMQX認証の基本概念](../authn/authn.md)の知識があると理解が深まります。

:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、パスワード認証に組み込みデータベースを設定できます。

1. EMQXダッシュボードの左ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム** に **パスワードベース** を、**バックエンド** に **組み込みデータベース** を選択し、以下のように **設定** タブに進みます。

<img src="./assets/authn-built-in-database.png" alt="組み込みデータベース" style="zoom:67%;" />

4. 認証バックエンドを設定するために以下の指示に従ってください：

   - **ユーザーIDタイプ**：クライアントID認証に使用するフィールドを指定します。選択肢は `username`、`clientid`（MQTTクライアントが送信する `CONNECT` メッセージの `Username` または `Client Identifier` フィールドに対応）。
   - **パスワードハッシュ**：プレーンテキストのパスワードをデータベースに保存する前に適用するハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります：
     - `md5`、`sha`、`sha256`、`sha512` の場合：
       - **ソルト位置**：ソルト（ランダムデータ）をパスワードにどのように混ぜるかを指定します。`suffix`、`prefix`、`disable` のいずれかです。外部ストレージからEMQX組み込みデータベースにユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。
       - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。
     - `plain` の場合：
       - **ソルト位置** は `disable` に設定してください。
     - `bcrypt` の場合：
       - **ソルトラウンド**：ハッシュ関数を適用する回数を定義し、_2<sup>ソルトラウンド</sup>_ で表される「コストファクター」です。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ強化のためには値を大きくすることが推奨されます。注：コストファクターを1増やすと認証に必要な時間が倍増します。
     - `pbkdf2` の場合：
       - **疑似乱数関数**：キー生成に使うハッシュ関数を選択します（例：`sha256`）。
       - **反復回数**：ハッシュ関数を実行する回数を設定します。デフォルトは `4096`。
       - **派生キー長**（任意）：生成されるキーのバイト長を指定します。空欄の場合は選択した疑似乱数関数のデフォルト長になります。
       - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存された認証情報と比較されます。

   - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で、この組み込みデータベース認証器をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証器が呼び出されます。そうでなければスキップされます。詳細は[認証器の前提条件](./authn.md#authenticator-preconditions)をご覧ください。

5. 設定が完了したら、**作成** をクリックします。

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

## ファイルからのユーザーブートストラップ

`password_based:built_in_database` 認証器は、認証器作成時にローカルファイルからユーザーを読み込むことをサポートしています。

この仕組みは、以下のようなデプロイ時の初期化（シード）に適しています：

- デフォルト管理者アカウントの作成
- 事前定義されたクライアント認証情報のプリロード
- 初回セットアップ時の初期データ準備
- 初期管理者アカウントの事前定義（`is_superuser = true` を設定）

ブートストラップは認証器作成時に一度だけ実行されます。継続的なユーザー管理や大規模なランタイム移行には適していません。EMQX稼働後の一括インポートには[ユーザーのインポート](./user_management.md#import-users)を利用してください。

### ブートストラップ設定

```hocon
bootstrap_file = "${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv"
bootstrap_type = "plain"  # または "hash"
```

#### `bootstrap_file`

- デフォルト：`${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv`
- 初期ユーザーを読み込むためのローカルファイルを指定します。

ファイル形式は拡張子で判別されます：

- `.csv`：ヘッダー付きCSV
- `.json`：オブジェクトのJSON配列

EMQXに付属するデフォルトファイルはCSVヘッダーを使用しています：

```txt
user_id,password,is_superuser
```

#### `bootstrap_type`

- 値：`plain` または `hash`
- デフォルト：`plain`

ファイル内のパスワードデータの解釈方法を決定します。

### ファイル形式の要件

`bootstrap_type = plain` の場合、以下のフィールドが必要です：

- `user_id`
- `password`
- `is_superuser`（任意、デフォルトは `false`）

EMQXは保存前に設定された `password_hash_algorithm` で `password` をハッシュ化します。

`bootstrap_type = hash` の場合、以下のフィールドが必要です：

- `user_id`
- `password_hash`
- `salt`（任意、デフォルトは空文字列）
- `is_superuser`（任意、デフォルトは `false`）

EMQXは `password_hash` を再ハッシュせずにそのまま保存します。

### ランタイム動作

認証器作成時に：

1. EMQXはブートストラップファイルを読み込みます。
2. CSVまたはJSONからユーザーを解析します。
3. ユーザーを組み込みデータベースに挿入します。

重要な注意点：

- 既存ユーザーは上書きされません（`override = false`）。
- `is_superuser` は以下の場合にのみ `true` とみなされます：
  - JSONの真偽値で `true`
  - CSV/JSONの文字列 `"true"`
  - それ以外はすべて `false` と解釈されます。
- ファイルの読み込みや解析エラーは警告ログのみを生成します。
- ファイルにエラーがあっても認証器作成は成功します。

## 外部ストレージからEMQX組み込みデータベースへの移行

MySQL、LDAP、他のMQTTブローカーなどの外部システムからEMQX組み込みデータベースへユーザー認証情報を移行するには、インポートユーザーAPIを使ってユーザーを一括アップロードできます。

ブートストラップとは異なり、ユーザーのインポートはEMQX稼働後に実行され、運用中のデータ移行を目的としています。操作方法の詳細は[ユーザーのインポート](./user_management.md#import-users)をご覧ください。
