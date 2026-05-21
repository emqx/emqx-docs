# HTTP API を使用したユーザーデータ管理

<<<<<<< HEAD
組み込みデータベースに保存された認証データは、EMQX ダッシュボードまたは HTTP API を使って、ユーザー認証情報の作成、更新、削除、一覧表示、インポートが可能です。

対象となるのは以下の認証方式です：
=======
組み込みデータベースに保存された認証データについては、EMQX ダッシュボードまたは HTTP API を使用して、ユーザー認証情報の作成、更新、削除、一覧表示、およびインポートが可能です。

対象となるのは以下です：
>>>>>>> origin/release-5.10

- [パスワード認証に組み込みデータベースを使用する](./mnesia.md)
- [MQTT 5.0 強化認証](./scram.md)

## ユーザー管理 API エンドポイント

ユーザー管理のエンドポイントは認証チェーンのスコープによって異なります。

- **グローバル MQTT 認証チェーン**

  ```
  /api/v5/authentication/{id}/users
  ```

- **MQTT リスナー固有の認証チェーン**

  ```
  /api/v5/listeners/{listener_id}/authentication/{id}/users
  ```

- **グローバルゲートウェイプロトコル認証チェーン**

  ```
  /api/v5/gateway/{protocol}/authentication/{id}/users
  ```

- **ゲートウェイプロトコルリスナー固有の認証チェーン**

  ```
  /api/v5/gateway/{protocol}/listeners/{listener_id}/authentication/{id}/users
  ```

識別子の規則やパラメータの説明については、[REST API](../../admin/api.md) を参照してください。

## ユーザーのインポート

<<<<<<< HEAD
ユーザーインポートは `password_based:built_in_database` 認証方式のみサポートされています。
=======
ユーザーインポートは `password_based:built_in_database` 認証器のみサポートされています。
>>>>>>> origin/release-5.10

この機能により、稼働中の EMQX インスタンスに対してユーザーを一括インポートできます。

利用可能なエンドポイントは以下の通りです：

- `/api/v5/authentication/{id}/import_users`
- `/api/v5/gateway/{protocol}/authentication/import_users`
- `/api/v5/gateway/{protocol}/listeners/{listener_id}/import_users`

<<<<<<< HEAD
リクエストは `multipart/form-data` の POST で送信する必要があります。
=======
リクエストは `multipart/form-data` の POST である必要があります。
>>>>>>> origin/release-5.10

例：

```bash
curl -v -u admin:public -X 'POST' \
    -H 'Content-Type: multipart/form-data' \
    -F 'filename=@/tmp/myusers.csv' \
    'http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/import_users'
```

### 対応ファイル形式

<<<<<<< HEAD
ファイル形式は拡張子で判別されます。以下の形式がサポートされています：

* `.csv`
  
  ヘッダー付き CSV ファイル例：
=======
ファイル形式は拡張子によって判別されます。以下の形式がサポートされています：

* `.csv`
  
  ヘッダー付き CSV ファイル：
>>>>>>> origin/release-5.10
  
  ```txt
  user_id,password_hash,salt,is_superuser
  myuser3,b6c743545a7817ae8c8f624371d5f5f0373234bb0ff36b8ffbf19bce0e06ab75,de1024f462fb83910fd13151bd4bd235,true
  myuser4,ee68c985a69208b6eda8c6c9b4c7c2d2b15ee2352cdd64a903171710a99182e8,ad773b5be9dd0613fe6c2f4d8c403139,false
  ```
  
  必須フィールド：
  
  - `user_id`
  - `password_hash`
  
  任意フィールド：
  
<<<<<<< HEAD
  - `salt`（省略時は空文字列）
  - `is_superuser`（省略時は `false`）
  
* `.json`
  
  オブジェクトの配列形式：
=======
  - `salt`（デフォルトは空文字列）
  - `is_superuser`（デフォルトは `false`）
  
* `.json`
  
  オブジェクトの JSON 配列：
>>>>>>> origin/release-5.10
  
  ```json
  [
    {
        "user_id":"myuser1",
        "password_hash":"c5e46903df45e5dc096dc74657610dbee8deaacae656df88a1788f1847390242",
        "salt": "e378187547bf2d6f0545a3f441aa4d8a",
        "is_superuser": true
    },
    {
        "user_id":"myuser2",
        "password_hash":"f4d17f300b11e522fd33f497c11b126ef1ea5149c74d2220f9a16dc876d4567b",
        "salt": "6d3f9bd5b54d94b98adbcfe10b6d181f",
        "is_superuser": false
    }
  ]
  ```
  
  フィールドの要件は CSV と同様です。
