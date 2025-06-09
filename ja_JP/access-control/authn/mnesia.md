# 組み込みデータベースの使用

EMQXの組み込みデータベースは、低コストかつすぐに使えるパスワード認証のオプションとして利用できます。有効化すると、EMQXはクライアント認証情報を組み込みデータベース（Mnesiaベース）に保存し、REST APIやダッシュボードを通じてデータを管理します。本ページでは、EMQXダッシュボードと設定項目を使った組み込みデータベースによる認証設定方法を紹介します。

::: tip

[EMQX認証の基本概念](../authn/authn.md)についての知識

:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、パスワード認証に組み込みデータベースを設定できます。

1. EMQXダッシュボードの左ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム** に **Password-Based** を、**バックエンド** に **Built-in Database** を選択し、以下のように **設定** タブに移動します。

<img src="./assets/authn-built-in-database.png" alt="組み込みデータベース" style="zoom:67%;" />

4. 以下の指示に従い認証バックエンドを設定します：

   - **UserID Type**：クライアントID認証に使用するフィールドを指定します。選択肢は `username`、`clientid`（MQTTクライアントが送信する `CONNECT` メッセージの `Username` または `Client Identifier` フィールドに対応）。
   - **Password Hash**：平文パスワードに適用され、結果がデータベースに保存されるハッシュアルゴリズムを選択します。利用可能なオプションは `plain`、`md5`、`sha`、`sha256`、`sha512`、`bcrypt`、`pbkdf2` です。選択したアルゴリズムに応じて追加設定があります：
     - `md5`、`sha`、`sha256`、`sha512` の場合：
       - **Salt Position**：パスワードとソルト（ランダムデータ）の結合方法を指定します。`suffix`（後置）、`prefix`（前置）、`disable`（無効）のいずれかです。外部ストレージからEMQX組み込みデータベースへユーザー認証情報を移行する場合を除き、デフォルト値のままで問題ありません。
       - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存済み認証情報と比較されます。
     - `plain` の場合：
       - **Salt Position** は `disable` に設定します。
     - `bcrypt` の場合：
       - **Salt Rounds**：ハッシュ関数の適用回数を、2のべき乗（2^Salt Rounds）で表す「コストファクター」です。デフォルトは `10`、許容範囲は `5` から `10` です。セキュリティ向上のため値を大きくすることが推奨されます。注：コストファクターを1増やすごとに認証にかかる時間は倍増します。
     - `pbkdf2` の場合：
       - **Pseudorandom Function**：鍵生成に使うハッシュ関数を選択します（例：`sha256`）。
       - **Iteration Count**：ハッシュ関数の繰り返し回数を設定します。デフォルトは `4096`。
       - **Derived Key Length**（任意）：生成される鍵のバイト長を指定します。未指定の場合は選択した擬似乱数関数により決定されます。
       - ハッシュ結果は16進数文字列で表され、大文字小文字を区別せずに保存済み認証情報と比較されます。

   - **Precondition**：この組み込みデータベース認証器をクライアント接続に適用するか制御するための[Variform式](../../configuration/configuration.md#variform-expressions)です。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列 `"true"` の場合のみ認証器が呼び出されます。それ以外はスキップされます。詳細は[認証の事前条件](./authn.md#authentication-preconditions)を参照してください。

5. 設定が完了したら **作成** をクリックします。

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
}
```

## 外部ストレージからEMQX組み込みデータベースへの移行

外部ストレージからEMQX組み込みデータベースへユーザー認証情報を移行するには、.csvや.jsonファイルを使った一括インポートが可能です。操作の詳細は[ユーザーのインポート](./user_management.md#importing-users)を参照してください。
