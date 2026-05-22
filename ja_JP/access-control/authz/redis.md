# Redisとの連携

<<<<<<< HEAD
このオーソライザーは、Redisデータベースに保存されたルールリストとパブリッシュ／サブスクリプション要求を照合することで認可チェックを実装します。

::: tip 前提条件

[EMQX認可の基本概念](./authz.md)の知識が必要です。
=======
このオーソライザーは、Redisデータベースに保存されたルールリストとパブリッシュ／サブスクリプションリクエストを照合することで認可チェックを実装しています。

::: tip 前提条件

[基本的なEMQX認可の概念](./authz.md)の知識
>>>>>>> origin/release-6.1

:::

## データスキーマとクエリ文

ユーザーは以下のデータを返すクエリテンプレートを用意する必要があります。

- `topic`：ルールが適用されるトピックを指定します。トピックフィルターや[トピックプレースホルダー](./authz.md#topic-placeholders)を使用可能です。
- `action`：ルールが適用されるアクションを指定します。利用可能な値は `publish`、`subscribe`、`all` です。
- `qos`（オプション）：現在のルールが適用されるQoSレベルを指定します。値は `0`、`1`、`2` のいずれか、または複数のQoSレベルを指定する数値配列です。デフォルトはすべてのQoSレベルです。
- `retain`（オプション）：ルールがリテインドメッセージをサポートするかどうかを指定します。値は `true` または `false` です。デフォルトはリテインドメッセージを許可します。

例えば、ルールは[Redisハッシュ](https://redis.io/docs/latest/develop/data-types/hashes/)として保存できます。

ユーザー `emqx_u` にトピック `t/1` のサブスクライブ権限を追加する例：

```bash
HSET mqtt_acl:emqx_u t/1 subscribe
```

<<<<<<< HEAD
Redisの構造上の制約により、`qos` や `retain` フィールドを使用する場合、トピック以外のフィールドはJSON文字列として格納する必要があります。例：

- ユーザー `emqx_u` にトピック `t/2` のQoS 1およびQoS 2でのサブスクライブ権限を追加する場合：
=======
Redisの構造上の制約により、`qos` と `retain` フィールドを使用する場合、トピック以外のフィールドはJSON文字列として格納する必要があります。例えば：

- ユーザー `emqx_u` にトピック `t/2` をQoS 1およびQoS 2でサブスクライブする権限を追加する例：
>>>>>>> origin/release-6.1

```bash
HSET mqtt_acl:emqx_u t/2 '{ "action": "subscribe", "qos": [1, 2] }'
```

<<<<<<< HEAD
- ユーザー `emqx_u` にトピック `t/3` へのリテインドメッセージのパブリッシュを拒否する権限を追加する場合：
=======
- ユーザー `emqx_u` にトピック `t/3` への保持メッセージのパブリッシュを拒否する権限を追加する例：
>>>>>>> origin/release-6.1

```bash
HSET mqtt_acl:emqx_u t/3 '{ "action": "publish", "retain": false }'
```

対応する設定パラメータは以下の通りです。

```bash
cmd = "HGETALL mqtt_acl:${username}"
```

取得したルールは許可ルールとして扱われます。つまり、トピックフィルターとアクションが一致すればリクエストは許可されます。

<<<<<<< HEAD
::: tip
Redisオーソライザーに追加されるすべてのルールは**許可ルール**です。したがって、Redisオーソライザーはホワイトリストモードで使用する必要があります。
=======
:::tip
Redisオーソライザーに追加されるすべてのルールは**許可ルール**であるため、Redisオーソライザーはホワイトリストモードで使用する必要があります。
>>>>>>> origin/release-6.1
:::

## ダッシュボードでの設定

EMQXダッシュボードを使ってRedisをユーザー認可に利用する設定が可能です。

1. EMQXダッシュボードの左ナビゲーションツリーで **アクセス制御** -> **認可** をクリックし、**認可** ページに入ります。

2. 右上の **作成** をクリックし、**バックエンド**として **Redis** を選択してから **次へ** をクリックします。以下のように **設定** タブが表示されます。

   <img src="./assets/authz-redis.png" alt="authz-Redis_ee" style="zoom:67%;" />

3. 以下の指示に従い設定を行います。

<<<<<<< HEAD
   - **Redisモード**：Redisのデプロイ方式を選択します。`Single`、`Sentinel`、`Cluster` から選べます。

   - **サーバー**：EMQXが接続するRedisサーバーのアドレスを指定します（`host:port`形式）。

   - **データベース**：Redisのデータベース番号を指定します。

   - **ユーザー名**：Redisの認証に[Redis ACL](https://redis.io/docs/latest/operate/oss_and_stack/management/security/acl/#create-and-edit-user-acls-with-the-acl-setuser-command)（Redis 6.0以降）を利用している場合に指定します。Redisサーバーがデフォルトユーザー（ACLが無効または適用されていない）を使用している場合は空欄にできます。

     ::: tip

     `username` フィールドはEMQX 5.2.0以降でサポートされています。Redis ACLを利用する場合はこのバージョン以降でのデプロイを推奨します。

     :::

   - **パスワード**：Redisユーザーのパスワードを指定します。認証が有効なRedisインスタンスに接続する場合は必須です。

     - ユーザー名を入力した場合は、Redis ACL設定の認証情報と一致する必要があります。
     - ユーザー名がない場合は、`default`ユーザーとして認証に使用されます（有効な場合）。

   - **互換モード**：EMQX 4.xのRedis ACLデータ形式との互換性を有効にするかどうかを制御します。

     - `Disabled (Default)`：現在のルール形式を使用します。
     - `v4`：既存のEMQX 4.x Redis ACLデータを修正せずに再利用するための互換性を有効にします。

     ::: tip

     このオプションは、EMQX 4.xからのアップグレード時に既存のRedis ACLデータを修正せずに再利用する場合に使用します。新規デプロイでは無効のまま現在のルール形式を使用することを推奨します。

     :::

=======
   - **Redisモード**：Redisのデプロイ方法を選択します。`Single`、`Sentinel`、`Cluster` があります。
   - **サーバー**：EMQXが接続するRedisサーバーのアドレスを指定します（`host:port`）。
   - **データベース**：Redisのデータベース名を指定します。
   - **ユーザー名**：Redisの認証に[Redis ACL](https://redis.io/docs/latest/operate/oss_and_stack/management/security/acl/#create-and-edit-user-acls-with-the-acl-setuser-command)（Redis 6.0以降）を使用している場合に指定します。Redisサーバーがデフォルトユーザー（ACL無効または未適用）を使用している場合は空欄のままで構いません。

     ::: tip

     `username` フィールドはEMQX 5.2.0以降でサポートされています。Redis ACLを利用する場合はこのバージョン以降のデプロイであることを確認してください。

     :::
   - **パスワード**：Redisユーザーのパスワードを指定します。認証が有効なRedisインスタンスに接続する場合は必須です。

     - ユーザー名を入力した場合、このパスワードはRedis ACL設定の認証情報と一致している必要があります。
     - ユーザー名がない場合、このパスワードは`default`ユーザーとして認証するために使用されます（有効な場合）。

>>>>>>> origin/release-6.1
   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。

   - **CMD**：データスキーマに従ったクエリコマンドを入力します。

<<<<<<< HEAD
   - **詳細設定**：同時接続数や接続タイムアウトまでの待機時間を設定します。
     - **プールサイズ**（任意）：EMQXノードからRedisへの同時接続数を整数で指定します。デフォルトは `8` です。
=======
   - **詳細設定**：同時接続数と接続タイムアウトまでの待機時間を設定します。
     - **プールサイズ**（任意）：EMQXノードからRedisへの同時接続数を整数値で指定します。デフォルトは `8` です。
>>>>>>> origin/release-6.1

4. **作成** をクリックして設定を完了します。

## 設定項目による設定

EMQXの設定項目を使ってRedisオーソライザーを設定することも可能です。

<<<<<<< HEAD
Redisオーソライザーは `redis` タイプで識別されます。Redisは3種類のデプロイモードに対応しています。
=======
Redisオーソライザーはタイプ `redis` で識別されます。オーソライザーはRedisの3種類のデプロイモードに接続可能です。 <!--詳細な設定情報は以下を参照してください：[redis_single](../../configuration/configuration-manual.html#authz:redis_single)、[authz:redis_sentinel](../../configuration/configuration-manual.html#authz:redis_sentinel)、[authz:redis_cluster](../../configuration/configuration-manual.html#authz:redis_cluster)。-->
>>>>>>> origin/release-6.1

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

> `compatibility_mode` はEMQX 4.xからのアップグレード時に既存のRedis ACLデータを再利用する場合に `v4` に設定できます。
