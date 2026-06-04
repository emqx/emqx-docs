# OpenLDAP と Microsoft Entra ID SSO の設定

このページでは、Lightweight Directory Access Protocol（LDAP）に基づくシングルサインオン（SSO）の設定および使用方法について説明します。

EMQX は、LDAPv3 プロトコルをサポートするディレクトリサービスと EMQX ダッシュボードを統合することで、LDAP ベースの SSO を実装しています。現在サポートされているディレクトリサービスプロバイダーは以下の通りです。

- [OpenLDAP](https://www.openldap.org/)
- [Microsoft Entra ID（旧 Azure AD）](https://azure.microsoft.com/en-in/products/active-directory)

::: tip 前提条件

[シングルサインオン（SSO）](./sso.md) の基本概念に慣れていることを推奨します。

:::

## OpenLDAP SSO の設定

<<<<<<< HEAD
このセクションでは、EMQX ダッシュボードで OpenLDAP SSO を有効化および設定する方法を案内します。
=======
このセクションでは、EMQX ダッシュボードで OpenLDAP SSO を有効化および設定する手順を案内します。
>>>>>>> origin/release-6.1

1. ダッシュボードにアクセスし、左側のナビゲーションメニューから **System Settings** -> **Single Sign-On** をクリックします。

2. **LDAP** オプションを選択し、**Enable** ボタンをクリックします。

3. **LDAP Settings** ページで設定情報を入力します。

<<<<<<< HEAD
   | オプション           | 説明                                                         |
   | -------------------- | ------------------------------------------------------------ |
   | Server               | OpenLDAP サーバーのアドレス。例：`localhost:389`。           |
   | Username             | OpenLDAP サーバーにアクセスするための Bind DN。              |
   | Password             | OpenLDAP サーバーにアクセスするためのユーザーパスワード。    |
   | Base DN              | OpenLDAP ディレクトリのベースオブジェクトエントリ名（またはルート）。ユーザー検索の起点となります。 |
   | User Lookup Filter   | OpenLDAP でユーザーにマッチするフィルター。LDAP ユーザー検索条件内で、`${username}` は実際の入力ユーザー名に自動置換されます。<br />標準的な LDAP ではデフォルトフィルターは `(&(objectClass=person)(uid=${username}))` です。<br />この変数置換機能により、ユーザー名の検索・マッチングにおいて異なるユーザー属性に基づく柔軟なクエリフィルターを構築できます。条件フォーマットの詳細は [LDAP Filters](https://ldap.com/ldap-filters/) を参照してください。 |
   | Enable TLS           | OpenLDAP へのアクセスに TLS セキュア通信を有効化するオプション。有効化する場合は証明書設定が必要です。TLS 有効化の詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。 |

4. **Update** ボタンをクリックして設定を保存します。

これで OpenLDAP SSO が有効化されました。LDAP オプションを使ったダッシュボードへのログイン方法は、[ログインとユーザー管理](#login-and-user-management) をご参照ください。
=======
   | オプション           | 説明                                                                                     |
   | -------------------- | ---------------------------------------------------------------------------------------- |
   | Force MFA            | 有効にすると、この LDAP バックエンドのすべてのユーザーはログイン時に MFA の設定と検証が必須になります。デフォルトは無効です。詳細は [SSO ユーザーの強制 MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users) を参照してください。 |
   | Server               | OpenLDAP サーバーのアドレス。例：`localhost:389`                                        |
   | Username             | OpenLDAP サーバーにアクセスするための Bind DN                                         |
   | Password             | OpenLDAP サーバーにアクセスするためのユーザーパスワード                               |
   | Base DN              | OpenLDAP ディレクトリのベースオブジェクトエントリ（またはルート）の名前。ユーザー検索の開始点となります。 |
   | User Lookup Filter   | OpenLDAP でユーザーにマッチするフィルター。LDAP ユーザー検索条件では、`${username}` が実際の入力ユーザー名に自動置換されます。<br />標準 LDAP のデフォルトフィルターは `(&(objectClass=person)(uid=${username}))` です。<br />この変数置換機構により、ユーザー名のクエリとマッチングに異なるユーザー属性を柔軟に利用した検索フィルターを構築できます。条件形式の詳細は [LDAP Filters](https://ldap.com/ldap-filters/) を参照してください。 |
   | Enable TLS           | OpenLDAP アクセスに TLS セキュア通信を有効にするオプション。有効にする場合は証明書設定が必要です。TLS 有効化の詳細は [外部リソースアクセスの TLS](../network/overview.md#tls-for-external-resource-access) を参照してください。 |

4. **Update** ボタンをクリックして設定を保存します。

これで OpenLDAP SSO が有効になりました。LDAP オプションを使ったダッシュボードへのログイン方法は、[ログインとユーザー管理](#login-and-user-management) を参照してください。
>>>>>>> origin/release-6.1

## Microsoft Entra ID SSO の設定

このセクションでは、EMQX ダッシュボードで Microsoft Entra ID SSO を有効化および設定する方法を案内します。

### Microsoft Entra ID インスタンスの設定

<<<<<<< HEAD
ダッシュボードで Microsoft Entra ID SSO を設定する前に、以下の手順に従い Microsoft Entra ID インスタンスを設定し、基本的な LDAP サーバー情報を取得してください。

1. [Azure Portal](https://portal.azure.com) にサインインし、[このチュートリアル](https://learn.microsoft.com/en-us/entra/identity/domain-services/tutorial-create-instance) に従って Microsoft Entra ドメインサービスを作成します。
=======
ダッシュボードで Microsoft Entra ID SSO を設定する前に、Microsoft Entra ID インスタンスを設定して基本的な LDAP サーバー情報を取得する必要があります。

1. [Azure Portal](https://portal.azure.com) にサインインし、[このチュートリアル](https://learn.microsoft.com/en-us/entra/identity/domain-services/tutorial-create-instance) の手順に従って Microsoft Entra ドメインサービスを作成します。
>>>>>>> origin/release-6.1

   ![sso-ad-domain-list](./assets/sso-ad-domain-list.png)

2. セキュア LDAP 接続を有効化します。作成した Microsoft Entra ドメインサービスで、左メニューの **Settings** から **Secure LDAP** をクリックします。

<<<<<<< HEAD
   - **Secure LDAP** と **Allow secure LDAP access over the internet** のトグルスイッチを有効化します。
=======
   - **Secure LDAP** と **Allow secure LDAP access over the internet** のトグルスイッチを有効にします。
>>>>>>> origin/release-6.1
   - 証明書の変更およびネットワークセキュリティグループの設定をページの指示に従って行い、EMQX が Microsoft Entra ドメインサービスインスタンスにアクセスできるようにします。

   ![sso-ad-enable-tls](./assets/sso-ad-enable-tls.png)

3. ドメインサービスの **Setting** -> **Properties** をクリックし、**Secure LDAP external IP addresses** を取得します。これは EMQX が接続する LDAP サーバーの実際の IP アドレスとして保存してください。

   ![sso-ad-get-ip](./assets/sso-ad-get-ip.png)

<<<<<<< HEAD
4. [このドキュメント](https://learn.microsoft.com/en-in/entra/fundamentals/create-new-tenant) に従い、新しい Entra ID テナントを作成します。

5. EMQX で Microsoft Entra ID と SSO を設定するには、多要素認証を無効にする必要があります。Entra ID インスタンスで、**Security** -> **Authentication Methods** -> **Settings** ページに移動し、**System-preferred multifactor authentication** を無効化します。

   ![sso-ad-disable-2fa](./assets/sso-ad-disable-2fa.png)

6. Entra ID インスタンスで、**Overview** ページに移動し、**Add** -> **Users** -> **Create User** をクリックしてユーザーを追加します。少なくとも 2 人のユーザーを追加してください：1 人は Entra ID への接続用、もう 1 人は EMQX ダッシュボードのログイン用です。ユーザー追加後は、Microsoft Entra ID に少なくとも一度ログインし、デフォルトパスワードを変更する必要があります。そうしないと SSO でダッシュボードにログインできません。
=======
4. [このドキュメント](https://learn.microsoft.com/en-in/entra/fundamentals/create-new-tenant) の手順に従って新しい Entra ID テナントを作成します。

5. EMQX で Microsoft Entra ID と SSO を構成するには、多要素認証を無効にする必要があります。Entra ID インスタンスで **Security** -> **Authentication Methods** -> **Settings** ページに移動し、**System-preferred multifactor authentication** を無効にします。

   ![sso-ad-disable-2fa](./assets/sso-ad-disable-2fa.png)

6. Entra ID インスタンスの **Overview** ページで **Add** -> **Users** -> **Create User** をクリックし、ユーザーを追加します。Entra ID への接続用と EMQX ダッシュボードログイン用の少なくとも 2 名のユーザーを追加してください。ユーザー追加後は、Microsoft Entra ID に少なくとも一度ログインし、初期パスワードを変更してから SSO でダッシュボードにログイン可能となります。
>>>>>>> origin/release-6.1

   ![sso-ad-add-user](./assets/sso-ad-add-user.png)

   ![sso-ad-add-user-detail](./assets/sso-ad-add-user-detail.png)

### ダッシュボードでの Microsoft Entra ID SSO 設定

1. ダッシュボードにアクセスし、左側のナビゲーションメニューから **System Settings** -> **Single Sign-On** をクリックします。

2. **LDAP** オプションを選択し、**Enable** ボタンをクリックします。

3. **LDAP Settings** ページで LDAP サーバーの基本情報を入力します。  
<<<<<<< HEAD
   - **Service**：`ip:port` の形式で入力します。IP は Microsoft Entra ID のセキュア LDAP 外部 IP アドレス、ポートは暗号化された LDAP 用の `636` です。

   - **Username**、**Password**：Entra ID への接続用に作成したユーザーとそのパスワードを入力します。

   - **Base DN**：Microsoft Entra ドメインサービスのドメイン名に従って入力します。例：`emqxqa.onmicrosoft.com` は `DC=emqxqa,DC=onmicrosoft,DC=com` と入力します。特定の部署やグループにユーザーを制限するために他の属性を追加することも可能です。

   - **User Query Condition**：Microsoft Entra ID のデフォルトフィルターは `(&(objectClass=user)(sAMAccountName=${username}))` で、アカウント名（メールアドレス）を使ってログインします。`sAMAccountName` を `mail` に置き換えてメールアドレスでのログインも可能です。

   - ここでは IP アドレス + セキュア LDAP 直接アクセスを使用するため、**Enable TLS** をクリックし、**Verify Server Certificate** は無効にしてください。

=======
   - **Service**: Microsoft Entra ID のセキュア LDAP 外部 IP アドレスとポート番号を `ip:port` 形式で入力します。ポートは暗号化された LDAP 用の `636` です。  
   - **Username**, **Password**: Entra ID への接続用に作成したユーザーとそのパスワードを入力します。  
   - **Base DN**: Microsoft Entra ドメインサービスのドメイン名に従って入力します。例：`emqxqa.onmicrosoft.com` は `DC=emqxqa,DC=onmicrosoft,DC=com` と記入します。特定の部署やグループにユーザーを限定する属性を追加することも可能です。  
   - **User Query Condition**: Microsoft Entra ID のデフォルトフィルターは `(&(objectClass=user)(sAMAccountName=${username}))` で、アカウント名（メールアドレス）でログインします。`sAMAccountName` を `mail` に置き換えることでメールアドレスでのログインも可能です。  
   - IP アドレス + セキュア LDAP 直接アクセスを使用しているため、**Enable TLS** をクリックし、**Verify Server Certificate** は無効にします。  
   - **Force MFA**: 必要に応じて有効にすると、このバックエンドのすべてのユーザーにログイン時の TOTP 検証を要求します。デフォルトは無効です。

>>>>>>> origin/release-6.1
     <img src="./assets/sso-ad-dashboard.png" alt="sso-ad-dashboard" style="zoom:67%;" />

4. **Update** ボタンをクリックして設定を保存します。

<<<<<<< HEAD
これで Microsoft Entra ID SSO が有効化されました。LDAP オプションを使ったダッシュボードへのログイン方法は、[ログインとユーザー管理](#login-and-user-management) をご参照ください。
=======
これで Microsoft Entra ID SSO が有効になりました。LDAP オプションを使ったダッシュボードへのログイン方法は、[ログインとユーザー管理](#login-and-user-management) を参照してください。
>>>>>>> origin/release-6.1

## ログインとユーザー管理

LDAP ベースの SSO を有効化すると、EMQX ダッシュボードのログインページに LDAP SSO オプションが表示されます。**LDAP** ボタンをクリックし、ユーザーに割り当てられた LDAP 認証情報（例：ユーザー名とパスワード）を入力して、**Login** ボタンをクリックしてください。

<img src="./assets/sso_ldap.png" alt="sso_ldap" style="zoom:67%;" />

<img src="./assets/ldap_login.png" alt="ldap_login" style="zoom:67%;" />

<<<<<<< HEAD
LDAP 認証に成功すると、EMQX は自動的にダッシュボードユーザーを追加します。追加されたユーザーは [Users](./system.md#users) で管理でき、ロールや権限の割り当ても可能です。
=======
LDAP 認証が成功すると、EMQX は自動的にダッシュボードユーザーを追加します。追加されたユーザーは [Users](./system.md#users) で管理でき、役割や権限の割り当ても可能です。LDAP ユーザーにログイン時の TOTP 二要素認証を必須にする場合は、[SSO ユーザーの強制 MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users) を参照してください。
>>>>>>> origin/release-6.1

## ログアウト

ユーザーはダッシュボードの上部ナビゲーションバーにあるユーザー名をクリックし、ドロップダウンメニューの **Logout** ボタンをクリックしてログアウトできます。
