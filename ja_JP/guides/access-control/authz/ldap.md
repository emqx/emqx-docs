# LDAPとの統合

[Lightweight Directory Access Protocol (LDAP)](https://ldap.com/) は、ディレクトリ情報にアクセスおよび管理するためのプロトコルです。EMQXは認可チェックのためにLDAPサーバーとの統合をサポートしています。LDAPオーソライザーは、パブリッシュ／サブスクリプションのリクエストをLDAPサーバーに保存された属性リストと照合することで認可チェックを実装します。

::: tip 前提条件

- [基本的なEMQX認可の概念](./authz.md)についての知識

:::

## LDAPデータスキーマとクエリ

LDAPオーソライザーは、LDAPディレクトリ内に保存された認可データに基づいてクライアントの認可をチェックします。LDAPスキーマは、認可データの構造と保存ルールを定義します。LDAPオーソライザーはほぼすべての保存スキーマをサポートしています。以下はOpenLDAP向けのスキーマ例です。

```sql
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

objectclass ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4 NAME 'mqttUser'
    SUP top
	STRUCTURAL
	MAY ( mqttPublishTopic $ mqttSubscriptionTopic $ mqttPubSubTopic $ mqttAclRule ) )
```

このスキーマは、異なるMQTT操作に対する認可ルールを指定する複数値属性を導入しています。

- `mqttPublishTopic`：クライアントがパブリッシュ可能なトピック
- `mqttSubscriptionTopic`：クライアントがサブスクライブ可能なトピック
- `mqttPubSubTopic`：クライアントがパブリッシュおよびサブスクライブ可能なトピック
- `mqttAclRule`：高度なアクセス制御のためのJSON形式の詳細なACLルール

EMQXは、最初の3つの属性を使ったワイルドカード対応のシンプルなトピックホワイトリストと、`mqttAclRule`によるより表現力豊かなルールの両方をサポートします。ACLルールのフォーマットについては[アクセスコントロールリスト（ACL）](../authn/acl.md#new-format)を参照してください。

### LDIFエントリーの例

以下は、OpenLDAP向けのスキーマに基づいたLDAP認可データの[LDAP Data Interchange Format (LDIF)](https://ldap.com/ldif-the-ldap-data-interchange-format/)の例です。

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

dn:uid=mqttuser0001,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0001
## 以下3つのトピックへのパブリッシュを許可
mqttPublishTopic: mqttuser0001/pub/1
mqttPublishTopic: mqttuser0001/pub/+
mqttPublishTopic: mqttuser0001/pub/#
## 以下3つのトピックへのサブスクライブを許可
mqttSubscriptionTopic: mqttuser0001/sub/1
mqttSubscriptionTopic: mqttuser0001/sub/+
mqttSubscriptionTopic: mqttuser0001/sub/#
## 以下のトピックはパブリッシュおよびサブスクライブを許可
mqttPubSubTopic: mqttuser0001/pubsub/1
mqttPubSubTopic: mqttuser0001/pubsub/+
mqttPubSubTopic: mqttuser0001/pubsub/#
mqttAclRule: [{"permission": "allow", "action": "pub", "topic": "mqttuser0001/complexrule/1"}]
mqttAclRule: {"permission": "allow", "action": "pub", "topic": "mqttuser0001/complexrule/#"}

dn:uid=mqttuser0002,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0002
mqttPublishTopic: mqttuser0002/pub/#
mqttSubscriptionTopic: mqttuser0002/sub/1
mqttPubSubTopic: mqttuser0002/pubsub/#
```

上記の例では、各操作に対して複数値属性を定義しています。各属性は、その操作で許可されているトピック数に応じて0回以上繰り返されます。

### LDAPサーバー設定の例

LDAPサーバーがスキーマとデータを正しく読み込むように、設定ファイルにスキーマファイルとLDIFエントリーを含める必要があります。以下は`slapd.conf`ファイルの例です。

::: tip

LDAP認可データの保存方法や場所は、ビジネス要件に応じて決定してください。

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

## ダッシュボードからLDAPオーソライザーを設定する

EMQXダッシュボードを使って、LDAPをユーザー認可に利用する設定が可能です。

1. [EMQXダッシュボード](http://127.0.0.1:18083/#/authentication)の左ナビゲーションメニューで、**Access Control** -> **Authorization** をクリックし、**Authorization** ページに入ります。

2. 右上の **Create** をクリックし、**Backend** に **LDAP** を選択してから **Next** をクリックします。以下のような **Configuration** タブが表示されます。

   <img src="./assets/authz-ldap.png" alt="authz-ldap"  />

3. 以下の指示に従って設定を行います。

   **Connect**：LDAPへの接続に必要な情報を入力します。

   - **Server**：EMQXが接続するLDAPサーバーのアドレスを指定します（`host:port`形式）。
   - **Username**：LDAPのルートユーザー名を指定します。
   - **Password**：LDAPのルートユーザーパスワードを指定します。

   **TLS Configuration**：TLSを有効にする場合はトグルスイッチをオンにします。

   **Connection Configuration**：同時接続数と接続タイムアウトまでの待機時間を設定します。

   - **Pool size**（任意）：EMQXノードからLDAPへの同時接続数を整数で指定します。デフォルトは`8`です。
   - **Query Timeout**（任意）：EMQXがクエリのタイムアウトと判断するまでの待機時間を指定します。単位はミリ秒、秒、分、時間が利用可能です。

   **Authorization configuration**：認可に関する設定を入力します。

   - **Base DN**：検索を行う基準となるベースオブジェクトエントリー（またはルート）の名前です。詳細は[RFC 4511 Search Request](https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1)を参照してください。プレースホルダーもサポートされます。

     ::: tip

     DNはDistinguished Nameの略で、各オブジェクトエントリーの一意の識別子であり、情報ツリー内のエントリーの位置を示します。

     :::

   - **Filter**：`Search`が特定のエントリーとマッチするために満たすべき条件を定義する`filter`です。フィルターの構文は[RFC 4515](#https://www.rfc-editor.org/rfc/rfc4515)に準拠し、プレースホルダーもサポートします。

4. **Create** をクリックして設定を完了します。

## 設定ファイルでLDAPオーソライザーを設定する

EMQXの設定項目を使ってLDAPオーソライザーを設定することも可能です。

LDAPオーソライザーはタイプ `ldap` で識別されます。

設定例：

```bash
{
  type = ldap

  server = "127.0.0.1:389"
  publish_attribute = "mqttPublishTopic"
  subscribe_attribute = "mqttSubscriptionTopic"
  all_attribute = "mqttPubSubTopic"
  acl_rule_attribute = "mqttAclRule"
  query_timeout = "5s"
  username = "root"
  password = "root password"
  pool_size = 8
  base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
  filter = "(objectClass=mqttUser)"
}
```
