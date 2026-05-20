# LDAPとの統合

[Lightweight Directory Access Protocol (LDAP)](https://ldap.com/) は、ディレクトリ情報にアクセスおよび管理するためのプロトコルです。EMQXはパスワード認証のためにLDAPサーバーとの統合をサポートしています。この統合により、ユーザーはLDAPの認証情報を用いてEMQXで認証を行うことが可能になります。

::: tip 前提条件

[EMQX認証の基本概念](../authn/authn.md)の知識

:::

## パスワード認証方式

EMQXのLDAP統合には、以下の2つの異なる認証方式があります。

- **LDAPバインド認証**

  EMQXはLDAPバインドを直接使用してユーザー名とパスワードを認証します。クライアントが接続すると、EMQXは提供されたユーザー名とパスワードを受け取り、設定された`base_dn`と`filter`を用いて識別名（DN）を構築します。その後、これらの認証情報を使ってLDAPサーバーにバインド（ログイン）を試みます。バインド操作が成功すれば認証が承認され、失敗すれば接続は拒否されます。

  この方式は既存のLDAPユーザーエントリのみに依存し、EMQXがパスワードハッシュなどの機密データを取得・処理する必要がありません。設定が簡単で、LDAPスキーマの変更も不要です。

  この方式は以下のような場合に適しています。

  - ユーザーアカウントがすでにLDAPサーバーに存在している。
  - LDAPスキーマを変更または拡張できない。
  - 最小限の設定でLDAPサーバー側で直接認証を処理したい。

- **ローカルパスワード比較**

  EMQXは`username`と`password`設定オプションで指定されたバインドアカウント（バインドDN）を使ってLDAPサーバーに接続します。次にクライアントのLDAPエントリを検索し、特定の属性から保存されているパスワード（通常はハッシュ形式）を取得します。クライアントから提供されたパスワードはEMQX内で取得したハッシュと比較されます。

  この方式は認証プロセスに対してより柔軟かつ詳細な制御を可能にします。複雑な検証ロジックやセキュリティ戦略をサポートし、追加のユーザー属性も扱えます。例えば、EMQXはユーザーの`isSuperUser`フラグをパスワード照会時に取得できるため、認証時にユーザーのスーパーユーザー権限を判別し、権限レベルに応じた異なるアクセスや操作を提供できます。

  この方式は以下のような場合に適しています。

  - カスタム認証属性（例：`isSuperuser`、ACLルール）を保存または処理する必要がある。
  - LDAPサーバーのスキーマやデータを設定する権限がある。
  - 単純なLDAPバインド以上の高度なセキュリティや検証ロジックが必要。

## LDAPデータスキーマとクエリ

::: tip

このセクションは「ローカルパスワード比較」認証方式に適用されます。「LDAPバインド認証」方式を使用する場合はこのセクションをスキップできます。

:::

このセクションでは、LDAPスキーマの設定、LDAP認証情報の作成、およびパスワード認証用の認証情報の保存方法について説明します。

LDAPスキーマは、LDAPディレクトリ内で認証データを整理・保存するための構造とルールを定義します。LDAP認証機能はほぼすべてのLDAPスキーマをサポートしています。以下はOpenLDAPのスキーマ例です。

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

上記のスキーマ例は、ユーザーがスーパーユーザーかどうかを示す属性`isSuperuser`を定義しています。また、ユーザーを表すオブジェクトクラス`mqttUser`を定義し、このオブジェクトクラスには`userPassword`属性が必須です。

LDAP認証情報を作成するには、必要な属性名、ベースオブジェクトの識別名（dn）、およびLDAPクエリのフィルターを定義する必要があります。

以下は、OpenLDAPのスキーマに基づいた[LDAPデータ交換フォーマット（LDIF）](https://ldap.com/ldif-the-ldap-data-interchange-format/)で指定されたLDAP認証情報のサンプルです。

```sql
## 組織作成: emqx.io
dn:dc=emqx,dc=io
objectclass: top
objectclass: dcobject
objectclass: organization
dc:emqx
o:emqx,Inc.

## 組織単位作成: testdevice.emqx.io
dn:ou=testdevice,dc=emqx,dc=io
objectClass: top
objectclass:organizationalUnit
ou:testdevice

## ユーザー作成: mqttuser0001,
#         パスワード=mqttuser0001,
#         パスワードハッシュ={SHA}mlb3fat40MKBTXUVZwCKmL73R/0=
#         base64passhash=e1NIQX1tbGIzZmF0NDBNS0JUWFVWWndDS21MNzNSLzA9
dn:uid=mqttuser0001,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0001
userPassword:: e1NIQX1tbGIzZmF0NDBNS0JUWFVWWndDS21MNzNSLzA9

## ユーザー作成: mqttuser0002
#         パスワード=mqttuser0002,
#         パスワードハッシュ={SSHA}n9XdtoG4Q/TQ3TQF4Y+khJbMBH4qXj4M
#         base64passhash=e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=
dn:uid=mqttuser0002,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0002
userPassword:: e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=

## スーパーユーザー作成: mqttuser0003
#         パスワード=mqttuser0003,
#         パスワードハッシュ={MD5}ybsPGoaK3nDyiQvveiCOIw==
#         base64passhash=e01ENX15YnNQR29hSzNuRHlpUXZ2ZWlDT0l3PT0=
dn:uid=mqttuser0003,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0003
isSuperuser: TRUE
userPassword:: e01ENX15YnNQR29hSzNuRHlpUXZ2ZWlDT0l3PT0=
```

LDAP設定ファイル`slapd.conf`を編集し、スキーマとLDIFファイルを含めることで、LDAPサーバー起動時にこれらが読み込まれます。以下は`slapd.conf`の例です。

::: tip

LDAP認証情報の保存方法やアクセス方法は、ビジネスニーズに応じて決定してください。

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

## ダッシュボードからLDAP認証を設定する

EMQXダッシュボードでLDAPをパスワード認証に使用する設定が可能です。

1. EMQXダッシュボードの左ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。

2. **認証** ページの右上にある **作成** をクリックします。

3. **メカニズム** に **パスワードベース** を選択し、**バックエンド** に **LDAP** を選択して、**設定** タブに進みます。以下のように表示されます。

   <img src="./assets/authn-ldap.png" alt="authn-ldap"  />

4. 以下の手順に従って設定を行います。

   - LDAPサーバーに接続するための情報を入力します。

     - **サーバー**: EMQXが接続するサーバーのアドレスを指定します（`host:port`形式）。

     - **ユーザー名**: EMQXがLDAPサーバーにバインドするために使用するアカウント名（バインドDN）を指定します。例：`cn=root,dc=emqx,dc=io`。このアカウントはユーザーエントリの読み取り権限を持ち、通常はLDAP設定ファイル（例：`slapd.conf`）で定義された`rootdn`と同じです。

     - **パスワード**: 上記ユーザー名に対応する平文パスワードで、バインド操作を完了するために使用します。この値はLDAP設定の`rootpw`の実際のパスワードと一致する必要があります。

   - **認証設定**: 認証に関する設定を入力します。

     - **パスワード認証方式**: 認証方式を選択します。`LDAPバインド認証`（デフォルト）または`ローカルパスワード比較`から選択します。

     - **バインドパスワード**: EMQXがLDAPサーバーに対して操作やクエリを実行する前に自身を認証するために使用するパスワードを指定します。`${password}`というプレースホルダーで参照され、実行時に設定オプションの**パスワード**で定義された実際のパスワードに解決されます。

     - **Base DN**: LDAP検索操作の開始点（ベースDN）を指定します。EMQXはここから設定されたフィルターに一致するユーザーエントリの検索を開始します。`${username}`などのプレースホルダーを使ってクライアントIDを動的に構築できます。詳細は[RFC 4511 Search Request](https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1)を参照してください。

       ::: tip

       DNは識別名（Distinguished Name）を指します。これは各オブジェクトエントリの一意の識別子であり、情報ツリー内のエントリの位置も示します。

       :::

     - **パスワードハッシュ属性**: 認証方式に`ローカルパスワード比較`を選択した場合に適用される、ユーザーのパスワードを表す属性を指定します。この属性の値は[RFC 3112](https://datatracker.ietf.org/doc/html/rfc3112)に準拠し、サポートされるアルゴリズムは`md5`、`sha`、`sha256`、`sha384`、`sha512`、および`ssha`です。

     - **スーパーユーザー属性**: 認証方式に`ローカルパスワード比較`を選択した場合に適用される、ユーザーがスーパーユーザーかどうかを示す属性を指定します。この属性の値はブール値であり、存在しない場合は`false`とみなされます。

     - **クライアントID上書き属性**: 接続時にクライアントが提供したクライアントIDを上書きするために使用されるLDAP属性の名前を指定します。これにより認証データに基づいて一意のクライアントIDを割り当てられ、マルチテナンシーなどのセッション競合を防止できます。

     - **前提条件**: [Variform式](../../configuration/configuration.md#variform-expressions)で、LDAP認証機能をクライアント接続に適用するかどうかを制御します。この式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の`"true"`の場合のみ認証機能が呼び出されます。そうでなければスキップされます。詳細は[Authenticator Preconditions](./authn.md#authenticator-preconditions)を参照してください。

   - **TLSを有効化**: TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。

   - **フィルター**: LDAPクエリの条件を定義します。フィルターはエントリが一致とみなされるための条件を設定します。フィルターの構文は[RFC 4515](https://www.rfc-editor.org/rfc/rfc4515)に準拠し、プレースホルダーもサポートされます。

   - **詳細設定**: 同時接続数や接続タイムアウトまでの待機時間を設定します。
     - **接続プールサイズ**（任意）: EMQXノードからLDAPへの同時接続数を整数値で指定します。デフォルトは`8`です。
     - **クエリタイムアウト**（任意）: EMQXがクエリのタイムアウトとみなすまでの待機時間を秒単位で指定します。デフォルトは`5`秒です。

5. 設定が完了したら、**作成**をクリックします。

## 設定ファイルによるLDAP認証設定

EMQXの設定項目でLDAP認証機能を設定できます。

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

クライアント認証に加え、EMQXは認証時に使用した同じLDAPエントリからユーザーごとのACL（アクセス制御リスト）ルールを取得できます。これにより認証と認可をLDAPで一元管理可能です。

認証プロセス中に、EMQXは設定された`base_dn`と`filter`を使ってユーザーのLDAPエントリを検索します。ACL関連の属性が見つかればそれらを取得し、クライアントのセッションにキャッシュします。これらのルールはパブリッシュ／サブスクライブの権限チェックなどの認可に使用され、LDAPへの繰り返しクエリは不要になります。

### サポートされるACL属性

LDAPからACLルールを取得する機能を有効にするには、LDAPスキーマに以下のいずれかの属性を定義する必要があります。

- **`mqttPublishTopic`**: クライアントがパブリッシュを許可されているトピックのホワイトリスト。
- **`mqttSubscriptionTopic`**: クライアントがサブスクライブを許可されているトピックのホワイトリスト。
- **`mqttPubSubTopic`**: クライアントがパブリッシュおよびサブスクライブの両方を許可されているトピック。
- **`mqttAclRule`**: JSON形式で定義された詳細なACLルール。アクション（パブリッシュやサブスクライブ）、許可（許可または拒否）、トピックフィルターを細かく制御可能。
- **`mqttAclTtl`**: クライアントセッション内でACLルールの有効期限（TTL）を指定する任意の属性。

上記の属性名は例示であり、LDAP認証機能の設定で適切なフィールドを使ってカスタマイズ可能です。

これらの属性の動作や意味は[LDAPオーソライザー](../authz/ldap.md)で定義されたものと同じですが、`mqttAclTtl`はLDAP認証機能固有の属性です。この属性により、取得したACLルールをクライアントセッション内でどのくらいの期間キャッシュするかを制御できます。値は秒単位の数値文字列（例：`60`）または`1s`、`15m`、`1h`、`1d`などの時間単位付きの期間を指定できます。

指定したTTLが経過すると、EMQXはキャッシュされたルールを使用せず、デフォルトの認可設定にフォールバックします。ただし、後続の認証やセッションで新しいルールが取得されれば再度キャッシュされます。

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

### ACL属性を含むLDAP認証データのLDIF例

以下はOpenLDAPのスキーマに基づいたACL属性を含むLDAP認証データの[LDAPデータ交換フォーマット（LDIF）](https://ldap.com/ldif-the-ldap-data-interchange-format/)の例です。

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

### LDAP認証機能設定例

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
