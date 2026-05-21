# LDAPとの統合

[Lightweight Directory Access Protocol（LDAP）](https://ldap.com/)は、ディレクトリ情報にアクセスおよび管理するためのプロトコルです。EMQXはパスワード認証のためにLDAPサーバーとの統合をサポートしています。この統合により、ユーザーはLDAPの認証情報を使用してEMQXで認証を行うことが可能になります。

::: tip 前提条件

[EMQX認証の基本概念](../authn/authn.md)の知識

:::

## パスワード認証方式

EMQXのLDAP統合には、以下の2つの異なる認証方式があります。

- **LDAPバインド認証**

   EMQXはLDAPバインドを直接利用してユーザー名とパスワードを認証します。クライアントが接続すると、EMQXは提供されたユーザー名とパスワードを受け取り、設定された`base_dn`と`filter`を用いて識別名（DN）を構築します。その後、これらの認証情報を使ってLDAPサーバーにバインド（ログイン）を試みます。バインド操作が成功すれば認証は承認され、失敗すれば接続は拒否されます。

   この方式は既存のLDAPユーザーエントリのみに依存し、EMQXがパスワードハッシュなどの機密データを取得・処理する必要がありません。設定が簡単で、LDAPスキーマの変更も不要です。

   この方式は以下のような場合に適しています。

   - ユーザーアカウントが既にLDAPサーバーに存在している場合。
   - LDAPスキーマを変更または拡張できない場合。
   - 最小限の設定で、LDAPサーバーが直接認証を処理することを望む場合。

- **ローカルパスワード比較**

   EMQXは設定された`username`と`password`（バインドDN）を使ってLDAPサーバーに接続します。その後、クライアントのLDAPエントリを検索し、特定の属性から保存されているパスワード（通常はハッシュ形式）を取得します。クライアントが提供したパスワードはEMQX内で取得したハッシュとローカルに比較されます。

   この方式は認証プロセスにおいてより柔軟かつ高度な制御を可能にします。複雑な検証ロジックやセキュリティ戦略をサポートし、追加のユーザー属性も扱えます。例えば、EMQXはユーザーの`isSuperUser`フラグをパスワード照会時に取得できるため、認証時にユーザーのスーパーユーザー権限を判別し、権限レベルに応じた異なるアクセスや操作を提供できます。

   この方式は以下のような場合に適しています。

   - カスタム認証属性（例：`isSuperuser`、ACLルール）を保存または処理する必要がある場合。
   - LDAPサーバーのスキーマやデータを設定する権限がある場合。
   - 単純なLDAPバインドを超えた高度なセキュリティや検証ロジックが必要な場合。

## LDAPデータスキーマとクエリ

::: tip

このセクションは「ローカルパスワード比較」認証方式に適用されます。「LDAPバインド認証」方式を使用している場合は、このセクションはスキップできます。

:::

このセクションでは、LDAPスキーマの設定、LDAP認証情報の作成、およびパスワード認証用の認証情報の保存方法について説明します。

LDAPスキーマは、LDAPディレクトリ内で認証データを整理・保存するための構造とルールを定義します。LDAP認証機能はほぼすべてのLDAPスキーマをサポートしています。以下はOpenLDAPの例スキーマです。

```sql
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.1.4 NAME 'isSuperuser'
	EQUALITY booleanMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.7
	SINGLE-VALUE
	USAGE userApplications )

objectclass ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4 NAME 'mqttUser'
	SUP top
	STRUCTURAL
	MAY ( isSuperuser )
    MUST ( uid $ userPassword ) )
```

この例スキーマは、ユーザーがスーパーユーザーかどうかを示す`isSuperuser`属性を定義しています。また、ユーザーを表す`mqttUser`オブジェクトクラスを定義し、このオブジェクトクラスには`userPassword`属性が必須で含まれます。

LDAP認証情報を作成するには、必要な属性名、ベースオブジェクトの識別名（dn）、およびLDAPクエリ用のフィルターを定義する必要があります。

以下は、OpenLDAPの例スキーマに基づく[LDAPデータ交換フォーマット（LDIF）](https://ldap.com/ldif-the-ldap-data-interchange-format/)で指定されたLDAP認証情報のサンプルです。

```sql
## create organization: emqx.io
dn:dc=emqx,dc=io
objectclass: top
objectclass: dcobject
objectclass: organization
dc:emqx
o:emqx,Inc.

## create organization unit: testdevice.emqx.io
dn:ou=testdevice,dc=emqx,dc=io
objectClass: top
objectclass:organizationalUnit
ou:testdevice

## create user=mqttuser0001,
#         password=mqttuser0001,
#         passhash={SHA}mlb3fat40MKBTXUVZwCKmL73R/0=
#         base64passhash=e1NIQX1tbGIzZmF0NDBNS0JUWFVWWndDS21MNzNSLzA9
dn:uid=mqttuser0001,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0001
userPassword:: e1NIQX1tbGIzZmF0NDBNS0JUWFVWWndDS21MNzNSLzA9

## create user=mqttuser0002
#         password=mqttuser0002,
#         passhash={SSHA}n9XdtoG4Q/TQ3TQF4Y+khJbMBH4qXj4M
#         base64passhash=e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=
dn:uid=mqttuser0002,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0002
userPassword:: e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=

## create a superuser mqttuser0003
#         password=mqttuser0003,
#         passhash={MD5}ybsPGoaK3nDyiQvveiCOIw==
#         base64passhash=e01ENX15YnNQR29hSzNuRHlpUXZ2ZWlDT0l3PT0=
dn:uid=mqttuser0003,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0003
isSuperuser: TRUE
userPassword:: e01ENX15YnNQR29hSzNuRHlpUXZ2ZWlDT0l3PT0=
```

LDAP設定ファイル`slapd.conf`を編集し、スキーマとLDIFファイルを含めてLDAPサーバー起動時に読み込まれるようにします。以下は`slapd.conf`の例です。

::: tip

LDAP認証情報の保存方法やアクセス方法は、ビジネスニーズに応じて決定できます。

:::

```sh
include         /usr/local/etc/openldap/schema/core.schema
include         /usr/local/etc/openldap/schema/cosine.schema
include         /usr/local/etc/openldap/schema/inetorgperson.schema
include         /usr/local/etc/openldap/schema/emqx.schema

TLSCACertificateFile  /usr/local/etc/openldap/cacert.pem
TLSCertificateFile    /usr/local/etc/openldap/cert.pem
TLSCertificateKeyFile /usr/local/etc/openldap/key.pem

database mdb
suffix "dc=emqx,dc=io"
rootdn "cn=root,dc=emqx,dc=io"
rootpw {SSHA}eoF7NhNrejVYYyGHqnt+MdKNBh4r1w3W

directory       /usr/local/etc/openldap/data
```

## ダッシュボードでLDAP認証を設定する

EMQXダッシュボードでLDAPをパスワード認証に使用する方法を設定できます。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。

2. **認証** ページの右上にある **作成** をクリックします。

3. **メカニズム**で**パスワードベース**を選択し、**バックエンド**で**LDAP**を選択して、**設定**タブに進みます。以下のように表示されます。

   <img src="./assets/authn-ldap.png" alt="authn-ldap"  />

4. 以下の指示に従って設定を行います。

   - LDAPサーバーに接続するための情報を入力します。

     - **サーバー**：EMQXが接続するサーバーのアドレスを指定します（`host:port`形式）。

     - **ユーザー名**：EMQXがLDAPサーバーにバインドするために使用するアカウント名（バインドDN）を指定します。例：`cn=root,dc=emqx,dc=io`。このアカウントはユーザーエントリを読み取る権限を持ち、通常はLDAP設定ファイル（例：`slapd.conf`）で定義された`rootdn`と同じです。

     - **パスワード**：上記ユーザー名に対応する平文のパスワードで、バインド操作を完了するために使用します。この値はLDAP設定の`rootpw`の実際のパスワードと一致する必要があります。

   - **認証設定**：認証に関する設定を入力します。

     - **パスワード認証方式**：認証方式を選択します。`LDAPバインド認証`（デフォルト）または`ローカルパスワード比較`から選択可能です。

     - **バインドパスワード**：EMQXがLDAPサーバーに対して自身を認証するために使用するパスワードを指定します。これは設定オプションの**パスワード**で定義された実際のパスワードにランタイムで解決されるプレースホルダー`${password}`を通じて参照されます。

     - **ベースDN**：LDAP検索操作の開始点（ベースDN）を指定します。EMQXはこのDNから設定されたフィルターに合致するユーザーエントリの検索を開始します。`${username}`などのプレースホルダーを使ってクライアント識別子を動的に構築可能です。詳細は[RFC 4511 Search Request](https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1)を参照してください。

       ::: tip

       DNは識別名（Distinguished Name）を指します。これは各オブジェクトエントリの一意の識別子であり、情報ツリー内のエントリの位置も示します。

       :::

     - **パスワードハッシュ属性**：認証方式に`ローカルパスワード比較`を選択した場合に適用される、ユーザーのパスワードを表す属性を指定します。この属性の値は[RFC 3112](https://datatracker.ietf.org/doc/html/rfc3112)に準拠し、サポートされるアルゴリズムは`md5`、`sha`、`sha256`、`sha384`、`sha512`、および`ssha`です。

     - **スーパーユーザー属性**：認証方式に`ローカルパスワード比較`を選択した場合に適用される、ユーザーがスーパーユーザーかどうかを示す属性を指定します。この属性の値はブール値で表され、存在しない場合は`false`とみなされます。

     - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で、LDAP認証機能をクライアント接続に適用するかどうかを制御します。この式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列`"true"`の場合にのみ認証機能が呼び出されます。そうでなければスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。

     - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。

     - **フィルター**：LDAPクエリの条件を定義します。フィルターはエントリがマッチとみなされるための条件を設定します。フィルターの構文は[RFC 4515](https://www.rfc-editor.org/rfc/rfc4515)に準拠し、プレースホルダーもサポートします。

     - **詳細設定**：同時接続数や接続タイムアウトまでの待機時間を設定します。
       - **コネクションプールサイズ**（任意）：EMQXノードからLDAPへの同時接続数を整数値で指定します。デフォルトは`8`です。
       - **クエリタイムアウト**（任意）：EMQXがクエリのタイムアウトとみなすまでの待機時間を秒数で指定します。デフォルトは`5`秒です。

5. 設定が完了したら、**作成**をクリックします。

## 設定項目によるLDAP認証の設定

EMQXの設定項目を使ってLDAP認証機能を設定できます。<!--挿入超リンク-->

LDAP認証は`mechanism = password_based`かつ`backend = ldap`で識別されます。

以下は**ローカルパスワード比較**方式の設定例です。

```bash
{
  backend = "ldap"
  mechanism = "password_based"
  method {
    type = hash
    password_attribute = "userPassword"
    is_superuser_attribute = "isSuperuser"
  }
  server = "127.0.0.1:389"
  query_timeout = "5s"
  username = "root"
  password = "root password"
  pool_size = 8
  base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
  filter = "(objectClass=mqttUser)"
}
```

以下は**LDAPバインド認証**方式の設定例です。

```bash
{
  backend = "ldap"
  mechanism = "password_based"
  method {
    type = bind
    bind_password = "${password}"
  }
  server = "127.0.0.1:389"
  query_timeout = "5s"
  username = "root"
  password = "root password"
  pool_size = 8
  base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
  filter = "(objectClass=mqttUser)"
}
```

## LDAPからACLルールを取得する

クライアントの認証に加えて、EMQXは認証時に使用した同じLDAPエントリからユーザーごとのACL（アクセス制御リスト）ルールを取得できます。これにより、認証と認可をLDAPで一元管理できます。

認証プロセス中に、EMQXは設定された`base_dn`と`filter`を使ってユーザーのLDAPエントリを検索します。ACL関連の属性が見つかれば、それらを取得してクライアントのセッションにキャッシュします。これらのルールはパブリッシュ／サブスクライブの権限チェックなどの認可処理に使用され、LDAPへの繰り返しクエリを不要にします。

### サポートされるACL属性

LDAPからACLルールを取得する機能を有効にするには、LDAPスキーマに以下のいずれかの属性を定義する必要があります。

- **`mqttPublishTopic`**：クライアントがパブリッシュを許可されているトピックのホワイトリスト。
- **`mqttSubscriptionTopic`**：クライアントがサブスクライブを許可されているトピックのホワイトリスト。
- **`mqttPubSubTopic`**：クライアントがパブリッシュおよびサブスクライブの両方を許可されているトピック。
- **`mqttAclRule`**：JSON形式で定義された詳細なACLルール。アクション（パブリッシュやサブスクライブ）、許可（許可または拒否）、トピックフィルターなどを細かく制御可能。
- **`mqttAclTtl`**：クライアントセッション内でACLルールが有効な期間（TTL）を指定する任意の属性。

上記の属性名はあくまで例です。LDAP認証機能の設定で適切なフィールドを使いカスタマイズ可能です。

これらの属性の動作や意味は[LDAPオーソライザー](../authz/ldap.md)で定義されたものと同じですが、`mqttAclTtl`はLDAP認証機能固有の属性です。この属性により、取得したACLルールをクライアントのセッションにどのくらいの期間キャッシュするかを制御できます。値は秒単位の数値文字列（例：`60`）または`1s`、`15m`、`1h`、`1d`などの時間単位付きの期間で指定可能です。

指定したTTLが経過すると、EMQXはキャッシュされたルールを使用せず、デフォルトの認可設定にフォールバックします。ただし、その後の認証やセッションで新しいルールが取得されれば再度キャッシュされます。

### ACLルール用LDAPスキーマ例

以下はACLルール用の属性を定義したスキーマ例です。

```
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.1.4 NAME 'isSuperuser'
	EQUALITY booleanMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.7
	SINGLE-VALUE
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.1 NAME ( 'mqttPublishTopic' 'mpt' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.2 NAME ( 'mqttSubscriptionTopic' 'mst' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.3 NAME ( 'mqttPubSubTopic' 'mpst' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.4 NAME ( 'mqttAclRule' 'mar' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.5 NAME ( 'mqttAclTtl' 'mat' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
objectclass ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4 NAME 'mqttUser'
	SUP top
	STRUCTURAL
	MAY ( isSuperuser $ mqttPublishTopic $ mqttSubscriptionTopic $ mqttPubSubTopic $ mqttAclRule $ mqttAclTtl )
  MUST ( uid $ userPassword ))
```

### ACL属性を含むLDIFエントリ例

以下はOpenLDAP用スキーマに基づくACL属性を含むLDAP認証データの[LDAPデータ交換フォーマット（LDIF）](https://ldap.com/ldif-the-ldap-data-interchange-format/)の例です。

```sql
dn:dc=emqx,dc=io
objectclass: top
objectclass: dcobject
objectclass: organization
dc:emqx
o:emqx,Inc.

# testdevice.emqx.io作成
dn:ou=testdevice,dc=emqx,dc=io
objectClass: top
objectclass:organizationalUnit
ou:testdevice

## ユーザー作成: mqttuser0002
#         パスワード=mqttuser0002,
#         パスワードハッシュ={SSHA}n9XdtoG4Q/TQ3TQF4Y+khJbMBH4qXj4M
#         base64passhash=e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=
dn:uid=mqttuser0002,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
objectClass: mqttDevice
objectClass: mqttSecurity
uid: mqttuser0002
isEnabled: TRUE
mqttAccountName: user2
mqttPublishTopic: mqttuser0002/pub/1
mqttPublishTopic: mqttuser0002/pub/+
mqttPublishTopic: mqttuser0002/pub/#
mqttSubscriptionTopic: mqttuser0002/sub/1
mqttSubscriptionTopic: mqttuser0002/sub/+
mqttSubscriptionTopic: mqttuser0002/sub/#
mqttPubSubTopic: mqttuser0002/pubsub/1
mqttPubSubTopic: mqttuser0002/pubsub/+
mqttPubSubTopic: mqttuser0002/pubsub/#
mqttAclRule: [{"permission": "allow", "action": "pub", "topic": "mqttuser0002/complexrule1/1"}]
mqttAclRule: {"permission": "allow", "action": "pub", "topic": "mqttuser0002/complexrule2/#"}
mqttAclTtl: 1s
userPassword:: e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=
```

### LDAP認証機能の設定例

ACLルールの取得とキャッシュを有効にするには、LDAP認証機能の設定で属性名を**明示的に**指定する必要があります。

```bash
{
  backend = "ldap"
  mechanism = "password_based"
  method {
    type = hash
    password_attribute = "userPassword"
    is_superuser_attribute = "isSuperuser"
  }
  server = "127.0.0.1:389"
  query_timeout = "5s"
  username = "root"
  password = "root password"
  pool_size = 8
  base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
  filter = "(objectClass=mqttUser)"
  publish_attribute = "mqttPublishTopic"
  subscribe_attribute = "mqttSubscriptionTopic"
  all_attribute = "mqttPubSubTopic"
  acl_attribute = "mqttAclRule"
  acl_ttl_attribute = "mqttAclTtl"
}
```
