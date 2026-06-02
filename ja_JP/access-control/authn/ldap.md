# LDAPとの統合

<<<<<<< HEAD
[Lightweight Directory Access Protocol (LDAP)](https://ldap.com/) は、ディレクトリ情報にアクセスおよび管理するためのプロトコルです。EMQXは、パスワード認証のためにLDAPサーバーとの統合をサポートしています。この統合により、ユーザーはEMQXでの認証にLDAPの資格情報を使用できます。

::: tip 前提条件

[EMQX認証の基本概念](../authn/authn.md)に関する知識
=======
[Lightweight Directory Access Protocol (LDAP)](https://ldap.com/) は、ディレクトリ情報へのアクセスおよび管理に使用されるプロトコルです。EMQXはパスワード認証のためにLDAPサーバーとの統合をサポートしています。この統合により、ユーザーはLDAPの認証情報を使ってEMQXで認証を行うことが可能になります。

::: tip 前提条件

[EMQX認証の基本概念](../authn/authn.md)についての知識
>>>>>>> origin/release-5.10

:::

## パスワード認証方式

EMQXのLDAP統合には、以下の2つの異なる認証方式があります。

- **LDAPバインド認証**

<<<<<<< HEAD
  EMQXはLDAPバインドを直接使用してユーザー名とパスワードを認証します。クライアントが接続すると、EMQXは提供されたユーザー名とパスワードを受け取り、設定された`base_dn`と`filter`を使って識別名（DN）を構築します。その後、これらの資格情報を用いてクライアントとしてLDAPサーバーにバインド（ログイン）を試みます。バインド操作が成功すれば認証が承認され、失敗すれば接続は拒否されます。
=======
   EMQXはLDAPバインドを直接使用してユーザー名とパスワードの認証を行います。クライアントが接続すると、EMQXは提供されたユーザー名とパスワードを受け取り、設定された`base_dn`と`filter`を使って識別名（DN）を構築します。その後、これらの認証情報を用いてLDAPサーバーにバインド（ログイン）を試みます。バインド操作が成功すれば認証が承認され、失敗すれば接続は拒否されます。
>>>>>>> origin/release-5.10

  この方式は既存のLDAPユーザーエントリのみに依存し、EMQXがパスワードハッシュなどの機密データを取得または処理する必要がありません。設定が簡単でLDAPスキーマの変更も不要です。

  以下のような場合に適しています。

<<<<<<< HEAD
  - ユーザーアカウントがすでにLDAPサーバーに存在している。
  - LDAPスキーマを変更または拡張できない。
  - 最小限の設定でLDAPサーバーに直接認証を任せたい。

- **ローカルパスワード比較**

  EMQXは`username`と`password`の設定オプションで指定されたバインドアカウント（バインドDN）を使ってLDAPサーバーに接続します。次にクライアントのLDAPエントリを検索し、特定の属性から保存されているパスワード（通常はハッシュ形式）を取得します。提供されたクライアントのパスワードと取得したハッシュをEMQX内でローカルに比較します。

  この方式は認証プロセスに対してより柔軟かつ詳細な制御を提供します。複雑な検証ロジックやセキュリティ戦略をサポートし、追加のユーザー属性も扱えます。例えば、EMQXはユーザーの`isSuperUser`フラグをパスワード照会時に取得できるため、認証時にユーザーがスーパーユーザー権限を持つかどうかを判定し、権限レベルに応じた異なるアクセスや操作を提供できます。
=======
   - ユーザーアカウントがすでにLDAPサーバーに存在している。
   - LDAPスキーマを変更・拡張できない。
   - 最小限の設定で、LDAPサーバー側で直接認証を処理したい。

- **ローカルパスワード比較**

   EMQXは`username`と`password`の設定で指定されたバインドアカウントを使ってLDAPサーバーに接続します（つまりバインドDN）。その後、クライアントのLDAPエントリを検索し、特定の属性から保存されているパスワード（通常はハッシュ形式）を取得します。クライアントが提供したパスワードはEMQX内で取得したハッシュとローカルに比較されます。

   この方式は認証プロセスに対してより柔軟かつ詳細な制御を可能にします。より複雑な検証ロジックやセキュリティ戦略をサポートし、追加のユーザー属性も扱えます。例えば、EMQXはユーザーの`isSuperUser`フラグをパスワード照会時に取得できるため、認証時にユーザーがスーパーユーザーかどうかを判別し、ユーザーの権限レベルに応じて異なるアクセス権限や操作権限を付与できます。
>>>>>>> origin/release-5.10

  以下のような場合に適しています。

<<<<<<< HEAD
  - カスタム認証属性（例：`isSuperuser`、ACLルール）を保存・処理する必要がある。
  - LDAPサーバーのスキーマやデータを設定する権限がある。
  - 単純なLDAPバインド以上の高度なセキュリティや検証ロジックが必要。
=======
   - カスタム認証属性（例：`isSuperuser`、ACLルール）を保存・処理する必要がある。
   - LDAPサーバーのスキーマやデータを設定する権限がある。
   - 単純なLDAPバインド以上の高度なセキュリティや検証ロジックが必要。
>>>>>>> origin/release-5.10

## LDAPデータスキーマとクエリ

::: tip

このセクションは「ローカルパスワード比較」認証方式に適用されます。「LDAPバインド認証」方式を使用する場合は、このセクションをスキップできます。

:::

<<<<<<< HEAD
このセクションでは、LDAPスキーマの設定、LDAP資格情報の作成、およびパスワード認証用の資格情報の保存方法について説明します。

LDAPスキーマは、LDAPディレクトリ内で認証データを整理・保存するための構造とルールを定義します。LDAP認証機能はほぼすべてのLDAPスキーマをサポートしています。以下はOpenLDAPのスキーマ例です。
=======
このセクションではLDAPスキーマの設定、LDAP認証情報の作成、およびパスワード認証用の認証情報の保存方法について説明します。

LDAPスキーマは、LDAPディレクトリ内で認証データを構造化し保存するためのルールと構造を定義します。LDAP認証機能はほぼすべてのLDAPスキーマをサポートしています。以下はOpenLDAPの例です。
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
上記のスキーマ例は、ユーザーがスーパーユーザーかどうかを示す属性`isSuperuser`を定義しています。また、ユーザーを表すオブジェクトクラス`mqttUser`を定義し、このオブジェクトクラスには`userPassword`属性が必須であることを示しています。
=======
上記のスキーマ例では、ユーザーがスーパーユーザーかどうかを示す`isSuperuser`属性を定義しています。また、ユーザーを表すオブジェクトクラス`mqttUser`を定義しており、このオブジェクトクラスには`userPassword`属性が必須です。
>>>>>>> origin/release-5.10

LDAP資格情報を作成するには、必要な属性名、ベースオブジェクトの識別名（dn）、およびLDAPクエリ用のフィルターを定義する必要があります。

<<<<<<< HEAD
以下は、OpenLDAP用のスキーマに基づいた[LDAPデータ交換形式（LDIF）](https://ldap.com/ldif-the-ldap-data-interchange-format/)で指定されたLDAP資格情報のサンプルです。
=======
以下は、OpenLDAP用のスキーマに基づいた[LDAPデータ交換フォーマット（LDIF）](https://ldap.com/ldif-the-ldap-data-interchange-format/)で指定されたサンプルLDAP認証情報です。
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
LDAP設定ファイル`slapd.conf`を編集して、スキーマとLDIFファイルを含めるようにし、LDAPサーバー起動時に読み込まれるようにします。以下は`slapd.conf`の例です。

::: tip

LDAP資格情報の保存方法やアクセス方法は、ビジネスニーズに応じて決定してください。
=======
LDAPサーバー起動時にスキーマとLDIFファイルが読み込まれるように、LDAP設定ファイル`slapd.conf`を編集します。以下は`slapd.conf`の例です。

::: tip

LDAP認証情報の保存方法やアクセス方法は、ビジネスニーズに応じて決定してください。
>>>>>>> origin/release-5.10

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

## ダッシュボードでのLDAP認証設定

EMQXダッシュボードからLDAPをパスワード認証に使用する設定が可能です。

1. EMQXダッシュボードの左ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。

1. EMQXダッシュボードの左側ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。

2. **認証** ページの右上にある **作成** をクリックします。

<<<<<<< HEAD
3. **メカニズム**に **Password-Based** を、**バックエンド**に **LDAP** を選択し、**設定**タブに進みます。以下のように表示されます。
=======
3. **メカニズム**として **パスワードベース** を、**バックエンド**として **LDAP** を選択し、**設定**タブに進みます。以下のように表示されます。
>>>>>>> origin/release-5.10

   <img src="./assets/authn-ldap.png" alt="authn-ldap"  />

4. 以下の指示に従って設定してください。

   - LDAPサーバーへの接続に必要な情報を入力します。

     - **サーバー**：EMQXが接続するサーバーのアドレスを指定します（`host:port`形式）。

<<<<<<< HEAD
     - **ユーザー名**：EMQXがLDAPサーバーにバインドするために使用するアカウント名（バインドDN）を指定します。例：`cn=root,dc=emqx,dc=io`。このアカウントはユーザーエントリの読み取り権限を持っている必要があり、通常はLDAP設定ファイル（例：`slapd.conf`）で定義された`rootdn`と同じです。

     - **パスワード**：上記ユーザー名に対応する平文パスワードで、バインド操作を完了するために使用します。この値はLDAP設定で定義された`rootpw`の実際のパスワードと一致する必要があります。
=======
     - **ユーザー名**：EMQXがLDAPサーバーにバインドするためのアカウント名（バインドDN）を指定します。例：`cn=root,dc=emqx,dc=io`。このアカウントはユーザーエントリの読み取り権限を持っている必要があり、通常はLDAP設定ファイル（例：`slapd.conf`）で定義された`rootdn`と同じです。

     - **パスワード**：上記ユーザー名に対応する平文パスワードで、バインド操作を完了するために使用されます。この値はLDAP設定の`rootpw`の実際のパスワードと一致させる必要があります。
>>>>>>> origin/release-5.10

   - **認証設定**：認証に関する設定を入力します。

     - **パスワード認証方式**：認証方式を選択します。`LDAPバインド認証`（デフォルト）または`ローカルパスワード比較`から選択します。

<<<<<<< HEAD
     - **バインドパスワード**：EMQXがLDAPサーバーに対して自身を認証するために使用するパスワードです。設定オプションの**パスワード**で定義された実際のパスワードに、プレースホルダー`${password}`を使って参照されます。

     - **ベースDN**：LDAP検索操作の開始点（ベースDN）を指定します。EMQXはこのDNから設定されたフィルターに合致するユーザーエントリの検索を開始します。`${username}`などのプレースホルダーを使ってクライアントIDを動的に構築することも可能です。詳細は[RFC 4511 Search Request](https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1)を参照してください。
=======
     - **バインドパスワード**：EMQXがLDAPサーバーに対して自身を認証するために使用するパスワードを指定します。これは設定項目**パスワード**で定義された実際のパスワードで実行時に解決されるプレースホルダー`${password}`を通じて参照されます。

     - **Base DN**：LDAP検索操作の開始点（ベースDN）を指定します。EMQXはこのDNから設定されたフィルターに一致するユーザーエントリの検索を開始します。`${username}`などのプレースホルダーを使ってクライアント識別子を動的に構築可能です。詳細は[RFC 4511 Search Request](https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1)を参照してください。
>>>>>>> origin/release-5.10

       ::: tip

       DNは識別名（Distinguished Name）を指します。これは各オブジェクトエントリの一意の識別子であり、情報ツリー内のエントリの位置を示します。

       :::

     - **パスワードハッシュ属性**：認証方式に`ローカルパスワード比較`を選択した場合に適用される、ユーザーのパスワードを表す属性を指定します。この属性の値は[RFC 3112](https://datatracker.ietf.org/doc/html/rfc3112)に準拠している必要があり、サポートされるアルゴリズムは`md5`、`sha`、`sha256`、`sha384`、`sha512`、`ssha`です。

     - **スーパーユーザー属性**：認証方式に`ローカルパスワード比較`を選択した場合に適用される、ユーザーがスーパーユーザーかどうかを示す属性を指定します。この属性の値はブール値でなければなりません。属性が存在しない場合は`false`とみなされます。

<<<<<<< HEAD
     - **クライアントID上書き属性**：接続時にクライアントが提供したクライアントIDを上書きするために使用されるLDAP属性の名前を指定します。これにより認証データに基づいて一意のクライアントIDを割り当てられ、多様なテナント環境などでのセッション競合を防止できます。

     - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)を使って、このLDAP認証機能をクライアント接続に適用するかどうかを制御します。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の`"true"`の場合のみ認証機能が呼び出されます。そうでなければスキップされます。詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。

   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLSの有効化については[ネットワークとTLS](../../network/overview.md)を参照してください。

   - **フィルター**：LDAPクエリの条件を定義します。フィルターはエントリが条件を満たすかどうかを判定するための式で、構文は[RFC 4515](https://www.rfc-editor.org/rfc/rfc4515)に準拠し、プレースホルダーもサポートします。

=======
     - **前提条件**：[Variform式](../../configuration/configuration.md#variform-expressions)で、LDAP認証機能をクライアント接続に適用するかどうかを制御します。この式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の`"true"`の場合のみ認証機能が呼び出されます。それ以外の場合はスキップされます。前提条件の詳細は[認証機能の前提条件](./authn.md#authenticator-preconditions)を参照してください。

   - **TLSを有効化**：TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。

   - **フィルター**：LDAPクエリの条件を定義します。フィルターはエントリがマッチとみなされる条件を設定します。フィルターの構文は[RFC 4515](https://www.rfc-editor.org/rfc/rfc4515)に準拠し、プレースホルダーもサポートしています。

>>>>>>> origin/release-5.10
   - **詳細設定**：同時接続数や接続タイムアウトまでの待機時間を設定します。
     - **コネクションプールサイズ**（任意）：EMQXノードからLDAPへの同時接続数を整数値で指定します。デフォルトは`8`です。
     - **クエリタイムアウト**（任意）：EMQXがクエリのタイムアウトとみなすまでの待機時間を秒単位で指定します。デフォルトは`5`秒です。

5. 設定が完了したら、**作成**をクリックします。

## 設定ファイルによるLDAP認証設定

<<<<<<< HEAD
EMQXのLDAP認証機能は設定項目で構成可能です。
=======
EMQXの設定ファイルでLDAP認証機能を設定することもできます。
>>>>>>> origin/release-5.10

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

クライアントの認証に加え、EMQXは認証時に使用した同じLDAPエントリからユーザーごとのACL（アクセス制御リスト）ルールを取得することも可能です。これにより認証と認可をLDAPで一元管理できます。

<<<<<<< HEAD
認証処理中に、EMQXは設定された`base_dn`と`filter`を使ってユーザーのLDAPエントリを検索します。ACL関連の属性が見つかれば、それらを取得してクライアントのセッションにキャッシュします。このルールを使ってパブリッシュやサブスクライブの権限チェックを行い、LDAPへの繰り返しクエリを不要にします。
=======
認証プロセス中に、EMQXは設定された`base_dn`と`filter`を使ってユーザーのLDAPエントリを検索します。ACL関連の属性が見つかれば、それらを取得してクライアントのセッションにキャッシュします。その後の認可チェック（パブリッシュ／サブスクライブの権限確認など）では、LDAPクエリを繰り返すことなくキャッシュされたルールを使用します。
>>>>>>> origin/release-5.10

### サポートされるACL属性

LDAPからACLルールを取得する機能を有効にするには、LDAPスキーマに以下のいずれかの属性を定義する必要があります。

- **`mqttPublishTopic`**：クライアントがパブリッシュを許可されているトピックのホワイトリスト。
- **`mqttSubscriptionTopic`**：クライアントがサブスクライブを許可されているトピックのホワイトリスト。
<<<<<<< HEAD
- **`mqttPubSubTopic`**：クライアントがパブリッシュおよびサブスクライブの両方を許可されているトピック。
- **`mqttAclRule`**：JSON形式で定義された詳細なACLルール。アクション（パブリッシュまたはサブスクライブ）、許可（許可または拒否）、トピックフィルターなどを細かく制御可能。
- **`mqttAclTtl`**：オプションの属性で、クライアントセッション内でACLルールが有効な期間（TTL）を指定。

上記の属性名はあくまで例です。LDAP認証機能の設定で適切なフィールドを使い、カスタマイズ可能です。

これらの属性の動作や意味は[LDAPオーソライザー](../authz/ldap.md)で定義されたものと同じですが、`mqttAclTtl`はLDAP認証機能固有の属性です。この属性により、取得したACLルールをクライアントのセッション内でどのくらいの期間キャッシュするかを制御できます。値は秒数の数値文字列（例：`60`）または`1s`、`15m`、`1h`、`1d`などの時間単位付きの期間を指定可能です。

指定したTTLが経過すると、EMQXはキャッシュされたルールを使用せず、デフォルトの認可設定に戻ります。ただし、その後の認証やセッションで新しいルールが取得されれば再度キャッシュされます。
=======
- **`mqttPubSubTopic`**：クライアントがパブリッシュおよびサブスクライブを許可されているトピック。
- **`mqttAclRule`**：JSON形式で定義された詳細なACLルール。アクション（パブリッシュまたはサブスクライブ）、許可（許可または拒否）、トピックフィルターなどを細かく制御可能。
- **`mqttAclTtl`**：クライアントセッション内でACLルールが有効な期間（TTL）を指定する任意の属性。

上記の属性名はあくまで例であり、LDAP認証機能の設定で適切なフィールドを使ってカスタマイズ可能です。

これらの属性の動作や意味は[LDAPオーソライザー](../authz/ldap.md)で定義されたものと同じですが、`mqttAclTtl`はLDAP認証機能固有の属性です。この属性により、取得したACLルールをクライアントのセッション内でどのくらいの期間キャッシュするかを制御できます。値は秒数の数値文字列（例：`60`）または`1s`、`15m`、`1h`、`1d`などの時間単位付きの期間で指定可能です。

指定されたTTLが経過すると、EMQXはキャッシュされたルールを使用しなくなり、新たな認証やセッション時に新しいルールが取得されない限り、デフォルトの認可設定にフォールバックします。
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
以下は上記スキーマに基づくACL属性を含むLDAP認証データの[LDAPデータ交換形式（LDIF）](https://ldap.com/ldif-the-ldap-data-interchange-format/)の例です。
=======
以下はOpenLDAP用スキーマに基づいたACL属性を含むLDAP認証データの[LDAPデータ交換フォーマット（LDIF）](https://ldap.com/ldif-the-ldap-data-interchange-format/)の例です。
>>>>>>> origin/release-5.10

```sql
dn:dc=emqx,dc=io
objectclass: top
objectclass: dcobject
objectclass: organization
dc:emqx
o:emqx,Inc.

# create testdevice.emqx.io
dn:ou=testdevice,dc=emqx,dc=io
objectClass: top
objectclass:organizationalUnit
ou:testdevice

## create user=mqttuser0002
#         password=mqttuser0002,
#         passhash={SSHA}n9XdtoG4Q/TQ3TQF4Y+khJbMBH4qXj4M
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
