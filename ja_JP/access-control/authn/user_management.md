# HTTP API を使ったユーザーデータ管理

組み込みデータベースに保存されている認証データについては、EMQX ダッシュボードまたは HTTP API を使ってユーザー認証情報の作成、更新、削除、一覧取得が可能です。対象は以下の通りです：

- [パスワード認証に組み込みデータベースを使用する](./mnesia.md)
- [MQTT 5.0 強化認証](./scram.md)

## API エンドポイント

グローバル MQTT チェーンのユーザー用エンドポイントは `/api/v5/authentication/{id}/users` です。  
特定の MQTT リスナーチェーンのユーザー用エンドポイントは `/api/v5/listeners/{listener_id}/authentication/{id}` です。  
グローバルな `gateway` プロトコルチェーンのユーザー用エンドポイントは `/api/v5/gateway/{protocol}/authentication` です。  
`gateway` プロトコルのリスナーチェーンのユーザー用エンドポイントは `/api/v5/gateway/{protocol}/listeners/{listener_id}/authentication` です。

識別子の規則については、[認証 API ドキュメント](./authn.md#http-api) を参照してください。

## ユーザーのインポート

`password_based:built_in_database` 認証方式に対してユーザーインポートがサポートされています。

対応するチェーンにユーザーをインポートするためのエンドポイントは以下の通りです：

- `/api/v5/authentication/{id}/import_users`
- `/api/v5/gateway/{protocol}/authentication/import_users`
- `/api/v5/gateway/{protocol}/listeners/{listener_id}/import_users`

リクエストは multipart form-data の `POST` 形式で送信してください。

例：

```
curl -v -u admin:public -X 'POST' \
    -H 'Content-Type: multipart/form-data' \
    -F 'filename=@/tmp/myusers.csv' \
    'http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/import_users'
```

サポートされているファイル形式（拡張子で判別）は以下の通りです：

* .csv
  ```txt
  user_id,password_hash,salt,is_superuser
  myuser3,b6c743545a7817ae8c8f624371d5f5f0373234bb0ff36b8ffbf19bce0e06ab75,de1024f462fb83910fd13151bd4bd235,true
  myuser4,ee68c985a69208b6eda8c6c9b4c7c2d2b15ee2352cdd64a903171710a99182e8,ad773b5be9dd0613fe6c2f4d8c403139,false
  ```

* .json
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
