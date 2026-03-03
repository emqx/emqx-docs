# LDAPとの統合

[Lightweight Directory Access Protocol (LDAP)](https://ldap.com/) は、ディレクトリ情報へのアクセスおよび管理に使用されるプロトコルです。EMQXは、パスワード認証のためにLDAPサーバーとの統合をサポートしています。この統合により、ユーザーはLDAPの認証情報を使用してEMQXで認証を行うことが可能になります。

::: tip 前提条件

[EMQX認証の基本概念](../authn/authn.md)の知識が必要です。

:::

## パスワード認証方式

EMQXのLDAP統合には、以下の2つの異なる認証方式があります。

- **ローカルパスワード比較**

  EMQXはLDAPに問い合わせてクライアントのパスワードを取得し、その取得したパスワードをEMQX内にローカルで保存されているパスワード情報と比較します。この方式は、LDAPユーザー認証を行う際により柔軟かつ高度な検証ロジックやセキュリティ戦略をサポートし、追加のユーザー属性の処理も可能にします。例えば、EMQXはユーザーのパスワードを問い合わせる際に`isSuperUser`フラグを取得できるため、認証時にユーザーがスーパーユーザー権限を持つかどうかを判別し、ユーザーの権限レベルに応じて異なるアクセス権限や操作機能を提供できます。ただし、この方法はLDAPサーバー上のスキーマやデータの設定権限をユーザーが持っている必要があります。

- **LDAPバインド認証**

  EMQXはLDAPのバインド操作を直接利用してユーザー名とパスワードの認証を行います。この方式はLDAPの`BIND`操作のみを用いた基本的な認証を提供し、既存のユーザー名とパスワードを使うだけで複雑なクエリやデータ処理を行いません。そのため、LDAPサーバーにすでにアカウントデータが存在する場合や、データの追加・変更権限がない場合に適しています。

## LDAPデータスキーマとクエリ

::: tip

このセクションは「ローカルパスワード比較」認証方式に適用されます。「LDAPバインド認証」方式を使用している場合は、このセクションをスキップできます。

:::

このセクションでは、LDAPスキーマの設定、LDAP認証情報の作成、およびパスワード認証用の認証情報の保存方法について説明します。

LDAPスキーマは、LDAPディレクトリ内で認証データを整理・保存するための構造とルールを定義します。LDAP認証機能はほぼすべてのLDAPスキーマをサポートしています。以下はOpenLDAP用のスキーマ例です。

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

上記のスキーマ例では、ユーザーがスーパーユーザーかどうかを示す属性`isSuperuser`を定義しています。また、ユーザーを表すオブジェクトクラス`mqttUser`を定義し、このオブジェクトクラスには`userPassword`属性が必須となっています。

LDAP認証情報を作成するには、必要な属性名、ベースオブジェクトの識別名（dn）、およびLDAPクエリのフィルターを定義する必要があります。

以下は、OpenLDAP用スキーマに基づいた[LDAPデータ交換フォーマット（LDIF）](https://ldap.com/ldif-the-ldap-data-interchange-format/)で指定されたサンプルLDAP認証情報です。

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
#         パスワード: mqttuser0001,
#         パスワードハッシュ: {SHA}mlb3fat40MKBTXUVZwCKmL73R/0=
#         base64パスワードハッシュ: e1NIQX1tbGIzZmF0NDBNS0JUWFVWWndDS21MNzNSLzA9
dn:uid=mqttuser0001,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0001
userPassword:: e1NIQX1tbGIzZmF0NDBNS0JUWFVWWndDS21MNzNSLzA9

## ユーザー作成: mqttuser0002
#         パスワード: mqttuser0002,
#         パスワードハッシュ: {SSHA}n9XdtoG4Q/TQ3TQF4Y+khJbMBH4qXj4M
#         base64パスワードハッシュ: e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=
dn:uid=mqttuser0002,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0002
userPassword:: e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=

## スーパーユーザー作成: mqttuser0003
#         パスワード: mqttuser0003,
#         パスワードハッシュ: {MD5}ybsPGoaK3nDyiQvveiCOIw==
#         base64パスワードハッシュ: e01ENX15YnNQR29hSzNuRHlpUXZ2ZWlDT0l3PT0=
dn:uid=mqttuser0003,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0003
isSuperuser: TRUE
userPassword:: e01ENX15YnNQR29hSzNuRHlpUXZ2ZWlDT0l3PT0=
```

LDAPサーバー起動時にスキーマとLDIFファイルが読み込まれるよう、LDAP設定ファイル`slapd.conf`を編集します。以下は`slapd.conf`の例です。

::: tip

LDAP認証情報の保存方法やアクセス方法は、ビジネス要件に応じて決定してください。

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

EMQXダッシュボードで、LDAPをパスワード認証に使用する設定が可能です。

1. EMQXダッシュボードの左ナビゲーションメニューから **アクセス制御** -> **認証** をクリックします。
2. **認証** ページの右上にある **作成** をクリックします。
3. **メカニズム**に **Password-Based** を、**バックエンド**に **LDAP** を選択し、**設定**タブに進みます。以下のように表示されます。

<img src="./assets/authn-ldap.png" alt="LDAP認証設定画面"  />

4. 以下の手順に従って設定を行います。

   - LDAPサーバーに接続するための情報を入力します。

     - **サーバー**: EMQXが接続するLDAPサーバーのアドレスを指定します（`host:port`形式）。
     - **ユーザー名**: LDAPのルートユーザー名を指定します。
     - **パスワード**: LDAPのルートユーザーパスワードを指定します。

   - **認証設定**: 認証に関する設定を入力します。

     - **パスワード認証方式**: 認証方式を選択します。`LDAP Bind Authentication`（デフォルト）または`Local Password Comparison`から選びます。

     - **バインドパスワード**: EMQXがLDAPサーバーに対して自身を認証するために使用するパスワードを指定します。設定オプションの**パスワード**で定義された実際のパスワードに実行時に解決されるプレースホルダー`${password}`で参照されます。

     - **ベースDN**: 検索を行う基準となるベースオブジェクトエントリ（またはルート）の識別名です。詳細は[RFC 4511 Search Request](https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1)を参照してください。プレースホルダーもサポートされています。

       ::: tip

       DN（Distinguished Name）は、各オブジェクトエントリの一意の識別子であり、情報ツリー内のエントリの位置を示します。

       :::

     - **パスワードハッシュ属性**: 認証方式に`Local Password Comparison`を選択した場合に適用される、ユーザーのパスワードを表す属性を指定します。この属性の値は[RFC 3112](#https://datatracker.ietf.org/doc/html/rfc3112)に準拠し、サポートされるアルゴリズムは`md5`、`sha`、`sha256`、`sha384`、`sha512`、および`ssha`です。

     - **スーパーユーザー属性**: 認証方式に`Local Password Comparison`を選択した場合に適用される、ユーザーがスーパーユーザーであるかを示す属性を指定します。この属性の値はブール値であり、存在しない場合は`false`とみなされます。

- **前提条件**: このLDAP認証機能をクライアント接続に適用するかどうかを制御するための[Variform式](../../configuration/configuration.md#variform-expressions)です。式はクライアントの属性（`username`、`clientid`、`listener`など）に対して評価され、結果が文字列の`"true"`の場合にのみ認証が実行されます。それ以外の場合はスキップされます。前提条件の詳細は[認証の前提条件](./authn.md#authentication-preconditions)を参照してください。
- **TLSを有効化**: TLSを有効にする場合はトグルスイッチをオンにします。TLS有効化の詳細は[ネットワークとTLS](../../network/overview.md)を参照してください。
- **フィルター**: LDAPクエリの条件を定義します。フィルターはエントリが一致とみなされるための条件を設定します。フィルターの構文は[RFC 4515](#https://www.rfc-editor.org/rfc/rfc4515)に準拠し、プレースホルダーもサポートしています。
- **詳細設定**: 同時接続数や接続タイムアウトまでの待機時間を設定します。
  - **接続プールサイズ**（任意）: EMQXノードからLDAPへの同時接続数を整数値で指定します。デフォルトは`8`です。
  - **クエリタイムアウト**（任意）: EMQXがクエリのタイムアウトとみなすまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

設定が完了したら、**作成**をクリックしてください。

## 設定項目でLDAP認証を設定する

EMQXの設定項目を使ってLDAP認証を設定することも可能です。<!--挿入超リンク-->

LDAP認証は`mechanism = password_based`かつ`backend = ldap`で識別されます。

以下は**ローカルパスワード比較**方式のサンプル設定です。

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

以下は**LDAPバインド認証**方式のサンプル設定です。

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
