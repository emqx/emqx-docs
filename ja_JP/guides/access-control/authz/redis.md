# Redisとの連携

このオーソライザーは、Redisデータベースに格納されたルールリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装しています。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)の知識が必要です。

:::

## データスキーマとクエリ文

ユーザーは以下のデータを返すクエリテンプレートを提供する必要があります。

- `topic`：ルールが適用されるトピックを指定します。トピックフィルターや[トピックプレースホルダー](./authz.md#topic-placeholders)を使用可能です。
- `action`：ルールが適用されるアクションを指定します。利用可能な値は `publish`、`subscribe`、`all` です。
- `qos`（オプション）：現在のルールが適用されるQoSレベルを指定します。値は `0`、`1`、`2` のいずれか、または複数のQoSレベルを指定する数値配列です。デフォルトはすべてのQoSレベルです。
- `retain`（オプション）：ルールが保持メッセージをサポートするかどうかを指定します。値は `true` または `false` です。デフォルトは保持メッセージを許可します。

例えば、ルールは[Redisハッシュ](https://redis.io/docs/manual/data-types/#hashes)として保存できます。

ユーザー `emqx_u` にトピック `t/1` のサブスクライブ権限を追加する例：

```bash
HSET mqtt_acl:emqx_u t/1 subscribe
```

Redisの構造上の制限により、`qos` と `retain` フィールドを使用する場合、トピック以外のフィールドはJSON文字列として格納する必要があります。例えば：

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
Redisオーソライザーに追加されるすべてのルールは**許可ルール**です。したがって、Redisオーソライザーはホワイトリストモードで使用する必要があります。
:::

## ダッシュボードでの設定

EMQXダッシュボードを使用して、Redisをユーザー認可に利用する設定が可能です。

1. EMQXダッシュボードの左ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、**バックエンド** に **Redis** を選択してから **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-redis.png" alt="authz-Redis_ee" style="zoom:67%;" />

3. 以下の指示に従い設定を行います。

   - **Redisモード**：Redisのデプロイ形態を選択します。`Single`、`Sentinel`、`Cluster` があります。
   - **サーバー**：EMQXが接続するRedisサーバーのアドレスを指定します（`host:port`）。
   - **データベース**：Redisのデータベース番号を指定します。
   - **ユーザー名**：Redisの認証に[Redis ACL](https://redis.io/docs/latest/operate/oss_and_stack/management/security/acl/#create-and-edit-user-acls-with-the-acl-setuser-command)（Redis 6.0以降で導入）を使用している場合、接続するRedisユーザー名を指定します。Redisサーバーがデフォルトユーザー（ACL無効または未適用）を使用している場合は空欄にできます。

     ::: tip

     `username` フィールドはEMQX 5.2.0以降でサポートされています。Redis ACLを利用する場合はこのバージョン以降のデプロイを使用してください。

     :::

   - **パスワード**：Redisユーザーのパスワードを指定します。認証が有効なRedisインスタンスに接続する場合は必須です。

     - ユーザー名を入力した場合、このパスワードはRedis ACL設定の認証情報と一致している必要があります。
     - ユーザー名が指定されていない場合、このパスワードは`default`ユーザーとして認証に使用されます（有効な場合）。

   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。

   - **CMD**：データスキーマに従ったクエリコマンドを入力します。

   - **詳細設定**：同時接続数や接続タイムアウトまでの待機時間を設定します。
     - **プールサイズ**（任意）：EMQXノードからRedisへの同時接続数を整数で指定します。デフォルトは `8` です。

4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を用いてRedisオーソライザーを設定できます。

Redisオーソライザーはタイプ `redis` で識別されます。オーソライザーはRedisの3つのデプロイモードに対応しています。<!--詳細な設定情報は以下を参照してください：[redis_single](../../configuration/configuration-manual.html#authz:redis_single)、[authz:redis_sentinel](../../configuration/configuration-manual.html#authz:redis_sentinel)、[authz:redis_cluster](../../configuration/configuration-manual.html#authz:redis_cluster)。-->

設定例：

:::: tabs type: card

::: tab Single

```bash
{
    type = redis

    redis_type = single
    server = "127.0.0.1:6379"

    cmd = "HGETALL mqtt_user:${username}"
    database = 1
    password = public

}
```

:::

::: tab Sentinel

```bash
{
    type = redis

    redis_type = sentinel
    servers = "10.123.13.11:6379,10.123.13.12:6379"
    sentinel = "mymaster"

    cmd = "HGETALL mqtt_user:${username}"
    database = 1
    password = public

}
```

:::

::: tab Cluster

```bash
{
    type = redis

    redis_type = cluster
    servers = "10.123.13.11:6379,10.123.13.12:6379"

    cmd = "HGETALL mqtt_user:${username}"
    password = public
}
```

:::

::::
