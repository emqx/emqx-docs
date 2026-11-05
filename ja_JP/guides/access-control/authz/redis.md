# Redisとの統合

このオーソライザーは、Redisデータベースに格納されたルールリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装しています。

::: tip 前提条件

[基本的なEMQX認可の概念](./authz.md)の知識

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

Redisの構造上の制限により、`qos` と `retain` フィールドを使用する場合は、トピック以外のフィールドをJSON文字列として格納する必要があります。例えば：

- ユーザー `emqx_u` にトピック `t/2` をQoS 1およびQoS 2でサブスクライブする権限を追加する例：

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
Redisオーソライザーに追加されるすべてのルールは**許可**ルールであるため、Redisオーソライザーはホワイトリストモードで使用する必要があります。
:::

::: tip 読み取り専用データベース権限の付与

EMQXはパブリッシュまたはサブスクライブ操作の認可時にRedisへの読み取りアクセスのみを必要とします。`cmd`で許可されるのは `HMGET` と `HGETALL` のみであり、他のコマンドは拒否されるため、認可処理でRedisへの書き込みは発生しません。

Redis 6.0以降では、EMQX用に専用ユーザーを設定し、EMQXが実行するヘルスチェックおよびクエリコマンドと、設定された `cmd` が読み取るキーに対してのみ権限を制限してください。

```bash
redis-cli ACL SETUSER emqx_auth reset on '>PASSWORD' '~mqtt_acl:*' +ping +hmget +hgetall
```

Redisクラスターの場合、EMQXはハッシュスロットとRedisノードのマッピングを取得・更新するために `CLUSTER SLOTS` を実行します。各クエリが正しいノードに送信されるように、`+cluster|slots` 権限も付与してください。

```bash
redis-cli ACL SETUSER emqx_auth '+cluster|slots'
```

シングルまたはセンチネルモードで、`database` が0以外に設定されている場合、EMQXは接続後に `SELECT` を実行するため、Redisデータノードユーザーに `+select` 権限を付与してください。デフォルトデータベース0を使用する場合は不要です。

センチネルモードでセンチネルノードに認証が有効な場合は、`sentinel_username` と `sentinel_password` でセンチネル接続用の認証情報を設定してください。これらはRedisデータノード接続用の `username` と `password` とは別です。

認可ルールの管理には別の管理用ユーザーを使用してください。EMQX設定内のRedis認証情報が漏洩しても、読み取り専用ユーザーでは認可ルールの変更や削除はできません。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使って、Redisをユーザー認可に利用する設定が可能です。

1. EMQXダッシュボードの左側ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、**バックエンド**として **Redis** を選択してから **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-redis.png" alt="authz-Redis_ee" style="zoom:67%;" />

3. 以下の指示に従い設定を行います。

   - **Redisモード**：Redisの展開形態を選択します。`Single`、`Sentinel`、`Cluster` から選択可能です。

   - **サーバー**：EMQXが接続するサーバーアドレスを指定します（`host:port`）。

   - **データベース**：Redisのデータベース名を指定します。

   - **ユーザー名**：Redis接続に使用するユーザー名を指定します。Redis 6.0で導入された[Redis ACL](https://redis.io/docs/latest/operate/oss_and_stack/management/security/acl/#create-and-edit-user-acls-with-the-acl-setuser-command)を認証に使用している場合は必須です。デフォルトユーザー（ACLが無効または適用されていない）を使用している場合は空欄のままで構いません。

     ::: tip

     `username` フィールドはEMQX 5.2.0以降でサポートされています。Redis ACLを使用する場合はこのバージョン以降での運用を推奨します。

     :::

   - **パスワード**：Redisユーザーのパスワードを指定します。認証が有効なRedisインスタンスに接続する場合は必須です。

     - ユーザー名を入力した場合、このパスワードはRedis ACL設定の認証情報と一致している必要があります。
     - ユーザー名がない場合、このパスワードは `default` ユーザー（有効な場合）の認証に使用されます。

   - **互換モード**：EMQX 4.xのRedis ACLデータ形式との互換性を有効にするかどうかを制御します。

     - `Disabled (Default)`：現在のルール形式を使用します。
     - `v4`：旧EMQX 4.xのRedis ACLデータと互換性を有効にし、アップグレード時に既存データを変更せずに再利用可能にします。

     ::: tip

     このオプションは、EMQX 4.xで作成された既存のRedis ACLデータを変更せずに再利用するアップグレードシナリオ向けです。新規導入の場合は無効のまま現在のルール形式を使用することを推奨します。

     :::

   - **TLSを有効化**：TLSを有効にする場合はスイッチをオンにします。

   - **CMD**：データスキーマに従ったクエリコマンドを入力します。

   - **詳細設定**：同時接続数や接続タイムアウトまでの待機時間を設定します。
     - **プールサイズ**（任意）：EMQXノードからRedisへの同時接続数を整数で指定します。デフォルトは `8` です。

4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を使ってRedisオーソライザーを設定できます。

Redisオーソライザーは `redis` タイプで識別されます。3種類のRedis展開モードに対応しています。

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

> `compatibility_mode` はEMQX 4.xからのアップグレード時に旧Redis ACLデータを再利用する場合に `v4` に設定可能です。
