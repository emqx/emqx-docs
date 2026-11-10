# Redisとの統合

このオーソライザーは、Redisデータベースに保存されたルールリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装します。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)の知識が必要です。

:::

## データスキーマとクエリ文

ユーザーは以下のデータを返すクエリテンプレートを提供する必要があります。

- `topic`：ルールが適用されるトピックを指定します。トピックフィルターや[トピックプレースホルダー](./authz.md#topic-placeholders)を使用可能です。
- `action`：ルールが適用されるアクションを指定します。利用可能なオプションは `publish`、`subscribe`、および `all` です。
- `qos`（オプション）：現在のルールが適用されるQoSレベルを指定します。値のオプションは `0`、`1`、`2` です。複数のQoSレベルを指定する場合は数値の配列も可能です。デフォルトはすべてのQoSレベルです。
- `retain`（オプション）：ルールが保持メッセージをサポートするかどうかを指定します。値のオプションは `true`、`false` です。デフォルトは保持メッセージを許可します。

例えば、ルールは[Redisハッシュ](https://redis.io/docs/latest/develop/data-types/hashes/)として保存できます。

ユーザー `emqx_u` にトピック `t/1` のサブスクライブ権限を追加する例：

```bash
HSET mqtt_acl:emqx_u t/1 subscribe
```

Redisの構造上の制限により、`qos` と `retain` フィールドを使用する場合、トピック以外のフィールドはJSON文字列として格納する必要があります。例：

- ユーザー `emqx_u` にトピック `t/2` のQoS 1およびQoS 2でのサブスクライブ権限を追加する例：

```bash
HSET mqtt_acl:emqx_u t/2 '{ "action": "subscribe", "qos": [1, 2] }'
```

- ユーザー `emqx_u` にトピック `t/3` への保持メッセージのパブリッシュを拒否する権限を追加する例：

```bash
HSET mqtt_acl:emqx_u t/3 '{ "action": "publish", "retain": false }'
```

対応する設定パラメータは以下の通りです：

```bash
cmd = "HGETALL mqtt_acl:${username}"
```

取得したルールは許可ルールとして扱われます。つまり、トピックフィルターとアクションが一致すればリクエストは許可されます。

:::tip
Redisオーソライザーに追加されるすべてのルールは**許可ルール**であるため、Redisオーソライザーはホワイトリストモードで使用する必要があります。
:::

::: tip 読み取り専用データベース権限の付与

EMQXはパブリッシュやサブスクライブの認可時にRedisへの読み取りアクセスのみを必要とします。`cmd`で許可されるのは `HMGET` と `HGETALL` のみであり、他のコマンドは拒否されるため、認可処理はRedisへの書き込みを行いません。

Redis 6.0以降では、EMQX用に専用ユーザーを設定し、EMQXが実行するヘルスチェックやクエリコマンド、`cmd`で指定されたキーへのアクセスのみを許可してください。

```bash
redis-cli ACL SETUSER emqx_auth reset on '>PASSWORD' '~mqtt_acl:*' +ping +hmget +hgetall
```

Redisクラスターの場合、EMQXはハッシュスロットとRedisノードのマッピングを取得・更新するために `CLUSTER SLOTS` を実行します。これにより各クエリが正しいノードに送信されるため、`+cluster|slots` 権限も付与してください。

```bash
redis-cli ACL SETUSER emqx_auth '+cluster|slots'
```

シングルまたはセンチネルモードで、`database` が0以外に設定されている場合、EMQXは接続後に `SELECT` を実行するため、Redisデータノードユーザーに `+select` 権限を付与してください。デフォルトのデータベース0を使用する場合は不要です。

センチネルモードでセンチネルノードに認証が有効な場合は、`sentinel_username` と `sentinel_password` にセンチネル接続用の認証情報を設定してください。これらはRedisデータノード接続用の `username` と `password` とは別です。

認可ルールの管理には別の管理ユーザーを使用してください。EMQX設定のRedis認証情報が漏洩しても、読み取り専用ユーザーでは認可ルールの変更や削除ができません。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、Redisをユーザー認可に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、**バックエンド** に **Redis** を選択してから **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-redis.png" alt="authz-Redis_ee" style="zoom:67%;" />

3. 以下の指示に従って設定を行います。

   - **Redisモード**：Redisのデプロイ形態を選択します。`Single`、`Sentinel`、`Cluster` があります。

   - **サーバー**：EMQXが接続するサーバーアドレスを指定します（`host:port`）。

   - **データベース**：Redisのデータベース名を指定します。

   - **ユーザー名**：Redis接続に使用するユーザー名を指定します。Redis 6.0で導入された[Redis ACL](https://redis.io/docs/latest/operate/oss_and_stack/management/security/acl/#create-and-edit-user-acls-with-the-acl-setuser-command)を利用している場合は必須です。Redisサーバーがデフォルトユーザー（ACL無効または未適用）を使用している場合は空欄にできます。

     ::: tip

     `username` フィールドはEMQX 5.2.0以降でサポートされています。Redis ACLを利用する場合はこのバージョン以降のEMQXを使用してください。

     :::

   - **パスワード**：Redisユーザーのパスワードを指定します。認証が有効なRedisインスタンスに接続する場合は必須です。

     - ユーザー名を入力した場合は、Redis ACL設定の認証情報と一致している必要があります。
     - ユーザー名がない場合は、`default` ユーザーとして認証されます（有効な場合）。

   - **前提条件**：任意のVariform式を入力します。式が `true` と評価された場合のみEMQXはこのオーソライザーを呼び出します。詳細は[オーソライザの前提条件](./authz.md#authorizer-preconditions)を参照してください。

   - **互換モード**：EMQX 4.xのRedis ACLデータ形式との互換性を有効にするかどうかを制御します。

     - `Disabled (Default)`：現在のルール形式を使用します。
     - `v4`：レガシーなEMQX 4.x Redis ACLデータとの互換性を有効にし、アップグレード時に既存データを変更せずに再利用可能にします。

     ::: tip

     このオプションはEMQX 4.xからのアップグレード時に既存のRedis ACLデータを変更せずに再利用するためのものです。新規導入の場合は無効のままにして現在のルール形式を使用することを推奨します。

     :::

   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。

   - **CMD**：データスキーマに従ったクエリコマンドを入力します。

   - **詳細設定**：同時接続数や接続タイムアウトまでの待機時間を設定します。
     - **プールサイズ**（任意）：EMQXノードからRedisへの同時接続数を整数で指定します。デフォルトは `8` です。

4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目でRedisオーソライザーを構成できます。

Redisオーソライザーは `redis` タイプで識別されます。3種類のRedisデプロイモードに対応しています。

オプションの `precondition` 設定項目はVariform式を受け付けます。式が `true` と評価された場合のみEMQXはこのオーソライザーを呼び出します。省略または空の場合は前提条件は適用されません。詳細は[オーソライザの前提条件](./authz.md#authorizer-preconditions)を参照してください。

設定例：

:::: tabs type: card

::: tab Single

```hocon
{
    type = redis

    redis_type = single
    server = "127.0.0.1:6379"

    cmd = "HGETALL mqtt_user:${username}"
    database = 1
    password = public
    
    compatibility_mode = disabled
}
```

:::

::: tab Sentinel

```hocon
{
    type = redis

    redis_type = sentinel
    servers = "10.123.13.11:6379,10.123.13.12:6379"
    sentinel = "mymaster"

    cmd = "HGETALL mqtt_user:${username}"
    database = 1
    password = public
    
    compatibility_mode = disabled
}
```

:::

::: tab Cluster

```hocon
{
    type = redis

    redis_type = cluster
    servers = "10.123.13.11:6379,10.123.13.12:6379"

    cmd = "HGETALL mqtt_user:${username}"
    password = public
    
    compatibility_mode = disabled
}
```

:::

::::

> `compatibility_mode` はEMQX 4.xからのアップグレード時にレガシーRedis ACLデータを再利用する場合に `v4` に設定できます。
