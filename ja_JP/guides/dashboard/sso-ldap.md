# OpenLDAP と Microsoft Entra ID の SSO 設定

このページでは、Lightweight Directory Access Protocol（LDAP）に基づくシングルサインオン（SSO）の設定と使用方法について説明します。

EMQX は、LDAPv3 プロトコルをサポートするディレクトリサービスと EMQX ダッシュボードを統合することで、LDAP ベースの SSO を実装しています。現在サポートされているディレクトリサービスプロバイダーは以下の通りです。

- [OpenLDAP](https://www.openldap.org/)
- [Microsoft Entra ID（旧 Azure AD）](https://azure.microsoft.com/en-in/products/active-directory)

::: tip 前提条件

[Single Sign-On (SSO)](./sso.md) の基本概念に慣れていることを推奨します。

:::

## OpenLDAP SSO の設定

このセクションでは、EMQX ダッシュボードで OpenLDAP SSO を有効化し設定する方法を案内します。

1. ダッシュボードにアクセスし、左のナビゲーションメニューから **System Settings** -> **Single Sign-On** をクリックします。

2. **LDAP** オプションを選択し、**Enable** ボタンをクリックします。

3. **LDAP Settings** ページで設定情報を入力します。

   | オプション           | 説明                                                                                  |
   | -------------------- | ------------------------------------------------------------------------------------- |
   | Server               | OpenLDAP サーバーのアドレス。例：`localhost:389`                                     |
   | Username             | OpenLDAP サーバーにアクセスするための Bind DN                                       |
   | Password             | OpenLDAP サーバーにアクセスするユーザーパスワード                                   |
   | Base DN              | OpenLDAP ディレクトリのベースオブジェクトエントリ（またはルート）の名前。ユーザー検索の起点となります。 |
   | User Lookup Filter   | OpenLDAP でユーザーにマッチするフィルター。LDAP ユーザークエリ条件内で `${username}` は実際の入力ユーザー名に自動置換されます。<br />標準 LDAP ではデフォルトフィルターは `(&(objectClass=person)(uid=${username}))` です。<br />この変数置換機能により、異なるユーザー属性に基づいた柔軟なクエリフィルターを構築可能です。条件フォーマットの詳細は [LDAP Filters](https://ldap.com/ldap-filters/) を参照してください。 |
   | Enable TLS           | OpenLDAP アクセスのための TLS セキュア通信を有効にするオプション。有効化する場合は証明書設定が必要です。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。 |

4. **Update** ボタンをクリックして設定を保存します。

これで OpenLDAP SSO が有効化されました。LDAP オプションを使ったダッシュボードへのログイン方法は、[ログインとユーザー管理](#login-and-user-management) を参照してください。

## Microsoft Entra ID SSO の設定

このセクションでは、EMQX ダッシュボードで Microsoft Entra ID SSO を有効化し設定する方法を案内します。

### Microsoft Entra ID インスタンスの設定

Dashboard で Microsoft Entra ID SSO を設定する前に、以下の手順で Microsoft Entra ID インスタンスを設定し、LDAP サーバーの基本情報を取得します。

1. [Azure Portal](https://portal.azure.com) にサインインし、[このチュートリアル](https://learn.microsoft.com/en-us/entra/identity/domain-services/tutorial-create-instance) の指示に従って Microsoft Entra ドメインサービスを作成します。

   ![sso-ad-domain-list](./assets/sso-ad-domain-list.png)

2. セキュア LDAP 接続を有効にします。作成した Microsoft Entra ドメインサービスで、左の **Settings** メニューから **Secure LDAP** をクリックします。

   - **Secure LDAP** と **Allow secure LDAP access over the internet** のトグルスイッチを有効にします。
   - 証明書を変更し、ページの指示に従ってネットワークセキュリティグループを設定し、EMQX が Microsoft Entra ドメインサービスインスタンスにアクセスできるようにします。

   ![sso-ad-enable-tls](./assets/sso-ad-enable-tls.png)

3. ドメインサービスの **Setting** -> **Properties** をクリックし、**Secure LDAP external IP addresses** を取得します。これが EMQX が接続する LDAP サーバーの実際の IP アドレスとなります。

   ![sso-ad-get-ip](./assets/sso-ad-get-ip.png)

4. [このドキュメント](https://learn.microsoft.com/en-in/entra/fundamentals/create-new-tenant) の指示に従い、新しい Entra ID テナントを作成します。

5. EMQX で Microsoft Entra ID と SSO を設定するには、多要素認証を無効にする必要があります。Entra ID インスタンスで **Security** -> **Authentication Methods** -> **Settings** ページに移動し、**System-preferred multifactor authentication** を無効にします。

   ![sso-ad-disable-2fa](./assets/sso-ad-disable-2fa.png)

6. Entra ID インスタンスで **Overview** ページに移動し、**Add** -> **Users** -> **Create User** をクリックしてユーザーを追加します。接続用ユーザーと EMQX ダッシュボードログイン用ユーザーの最低2名を追加してください。ユーザー追加後は、Microsoft Entra ID に少なくとも一度ログインし、デフォルトパスワードを変更する必要があります。これにより SSO を使ったダッシュボードログインが可能になります。

   ![sso-ad-add-user](./assets/sso-ad-add-user.png)

   ![sso-ad-add-user-detail](./assets/sso-ad-add-user-detail.png)

### ダッシュボードで Microsoft Entra ID SSO を設定

1. ダッシュボードにアクセスし、左のナビゲーションメニューから **System Settings** -> **Single Sign-On** をクリックします。

2. **LDAP** オプションを選択し、**Enable** ボタンをクリックします。

3. **LDAP Settings** ページで LDAP サーバーの基本情報を入力します。
   - **Server**: Microsoft Entra ID のセキュア LDAP 外部 IP アドレスとポートを `ip:port` 形式で入力します。ポートは暗号化された LDAP 用の `636` です。
   - **Username**、**Password**: Entra ID 接続用に作成したユーザーとそのパスワードを入力します。
   - **Base DN**: Microsoft Entra ドメインサービスのドメイン名に応じて入力します。例：`emqxqa.onmicrosoft.com` は `DC=emqxqa,DC=onmicrosoft,DC=com` と入力します。特定の部署やグループにユーザーを制限したい場合は他の属性を追加可能です。
   - **User Lookup Filter**: Microsoft Entra ID のデフォルトフィルターは `(&(objectClass=user)(sAMAccountName=${username}))` で、アカウント名（メールアドレス）でログインします。`sAMAccountName` を `mail` に置き換えてメールアドレスでログインすることも可能です。
   - IP アドレス + セキュア LDAP 直接アクセスを使用しているため、**Enable TLS** を有効にし、**Verify Server Certificate** は無効にしてください。

     <img src="./assets/sso-ad-dashboard.png" alt="Microsoft Entra ID SSO 設定画面" style="zoom:67%;" />

4. **Update** ボタンをクリックして設定を保存します。

これで Microsoft Entra ID SSO が有効化されました。LDAP オプションを使ったダッシュボードへのログイン方法は、[ログインとユーザー管理](#login-and-user-management) を参照してください。

## ログインとユーザー管理

LDAP ベースの SSO を有効化すると、EMQX ダッシュボードのログインページに LDAP SSO オプションが表示されます。**LDAP** ボタンをクリックし、ユーザーに割り当てられた LDAP 認証情報（ユーザー名とパスワードなど）を入力して、**Login** ボタンをクリックしてください。

<img src="./assets/sso_ldap.png" alt="LDAP ログイン画面" style="zoom:67%;" />

<img src="./assets/ldap_login.png" alt="LDAP 認証入力画面" style="zoom:67%;" />

LDAP 認証に成功すると、EMQX は自動的にダッシュボードユーザーを追加します。追加されたユーザーは [Users](./system.md#users) で管理でき、役割や権限の割り当ても可能です。

## ログアウト

ユーザーはダッシュボード上部のナビゲーションバーにあるユーザー名をクリックし、ドロップダウンメニューの **Logout** ボタンをクリックしてログアウトできます。
