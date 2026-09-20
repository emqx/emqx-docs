# OpenLDAP と Microsoft Entra ID の SSO 設定

このページでは、Lightweight Directory Access Protocol（LDAP）に基づくシングルサインオン（SSO）の設定および使用方法について説明します。

EMQX は、LDAPv3 プロトコルをサポートするディレクトリサービスと EMQX ダッシュボードを統合することで、LDAP ベースの SSO を実装しています。現在サポートされているディレクトリサービスプロバイダーは以下の通りです。

- [OpenLDAP](https://www.openldap.org/)
- [Microsoft Entra ID（旧 Azure AD）](https://azure.microsoft.com/en-in/products/active-directory)

::: tip 前提条件

[シングルサインオン（SSO）](./sso.md) の基本概念に慣れていることを推奨します。

:::

## OpenLDAP SSO の設定

このセクションでは、EMQX ダッシュボードで OpenLDAP SSO を有効化および設定する手順を案内します。

1. ダッシュボードにアクセスし、左のナビゲーションメニューから **システム設定** -> **シングルサインオン** をクリックします。

2. **LDAP** オプションを選択し、**有効化** ボタンをクリックします。

3. **LDAP 設定** ページで設定情報を入力します。

   | オプション           | 説明                                                                                      |
   | -------------------- | ----------------------------------------------------------------------------------------- |
   | Force MFA            | 有効にすると、この LDAP バックエンドのすべてのユーザーがログイン時に MFA の設定と検証を必須とします。デフォルトは無効です。詳細は [SSO ユーザーの強制 MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users) を参照してください。 |
   | Server               | OpenLDAP サーバーのアドレス。例：`localhost:389`                                         |
   | Username             | OpenLDAP サーバーにアクセスするための Bind DN                                           |
   | Password             | OpenLDAP サーバーにアクセスするためのユーザーパスワード                                 |
   | Base DN              | OpenLDAP ディレクトリのベースオブジェクトエントリ（またはルート）の名前。ユーザー検索の起点となります。 |
   | User Lookup Filter   | OpenLDAP でユーザーにマッチするフィルター。LDAP ユーザークエリ条件内で `${username}` は実際の入力ユーザー名に自動置換されます。<br />標準 LDAP ではデフォルトフィルターは `(&(objectClass=person)(uid=${username}))` です。<br />この変数置換機能により、ユーザー名クエリやマッチングのために異なるユーザー属性に基づく柔軟なクエリフィルターを構築できます。条件形式の詳細は [LDAP Filters](https://ldap.com/ldap-filters/) を参照してください。 |
   | Enable TLS           | OpenLDAP へのアクセスに TLS セキュア通信を有効にするオプション。有効にする場合は証明書の設定が必要です。TLS 有効化の詳細は [外部リソースアクセスの TLS](../network/overview.md#tls-for-external-resource-access) を参照してください。 |

4. **更新** ボタンをクリックして設定を保存します。

これで OpenLDAP SSO が有効になりました。LDAP オプションを使ったダッシュボードへのログイン方法は、[ログインとユーザー管理](#login-and-user-management) を参照してください。

## Microsoft Entra ID SSO の設定

このセクションでは、EMQX ダッシュボードで Microsoft Entra ID SSO を有効化および設定する方法を案内します。

### Microsoft Entra ID インスタンスの設定

Dashboard で Microsoft Entra ID SSO を設定する前に、Microsoft Entra ID インスタンスを設定して基本的な LDAP サーバー情報を取得する必要があります。

1. [Azure Portal](https://portal.azure.com) にサインインし、[このチュートリアル](https://learn.microsoft.com/en-us/entra/identity/domain-services/tutorial-create-instance) に従って Microsoft Entra ドメインサービスを作成します。

   ![sso-ad-domain-list](../assets/sso-ad-domain-list.png)

2. セキュア LDAP 接続を有効にします。作成した Microsoft Entra ドメインサービスで、左の **設定** メニューから **Secure LDAP** をクリックします。

   - **Secure LDAP** と **Allow secure LDAP access over the internet** のトグルスイッチを有効にします。
   - 証明書を変更し、ページの指示に従ってネットワークセキュリティグループを設定し、EMQX が Microsoft Entra ドメインサービスインスタンスにアクセスできるようにします。

   ![sso-ad-enable-tls](../assets/sso-ad-enable-tls.png)

3. ドメインサービスの **設定** -> **プロパティ** をクリックし、**Secure LDAP external IP addresses** を取得します。これを EMQX が接続する LDAP サーバーの実際の IP アドレスとして保存します。

   ![sso-ad-get-ip](../assets/sso-ad-get-ip.png)

4. [このドキュメント](https://learn.microsoft.com/en-in/entra/fundamentals/create-new-tenant) に従って新しい Entra ID テナントを作成します。

5. EMQX で Microsoft Entra ID の SSO を設定するには、多要素認証を無効化する必要があります。Entra ID インスタンスで、**セキュリティ** -> **認証方法** -> **設定** ページに移動し、**システム推奨の多要素認証** を無効にします。

   ![sso-ad-disable-2fa](../assets/sso-ad-disable-2fa.png)

6. Entra ID インスタンスの **概要** ページで、**追加** -> **ユーザー** -> **ユーザーの作成** をクリックしてユーザーを追加します。接続用ユーザーと EMQX ダッシュボードログイン用ユーザーの最低 2 名を追加する必要があります。ユーザー追加後は、Microsoft Entra ID で少なくとも一度ログインし、デフォルトパスワードを変更してから SSO を使ってダッシュボードにログインできます。

   ![sso-ad-add-user](../assets/sso-ad-add-user.png)

   ![sso-ad-add-user-detail](../assets/sso-ad-add-user-detail.png)

### ダッシュボードでの Microsoft Entra ID SSO 設定

1. ダッシュボードにアクセスし、左のナビゲーションメニューから **システム設定** -> **シングルサインオン** をクリックします。

2. **LDAP** オプションを選択し、**有効化** ボタンをクリックします。

3. **LDAP 設定** ページで LDAP サーバーの基本情報を入力します。
   - **Service**：Microsoft Entra ID のセキュア LDAP 外部 IP アドレスと暗号化 LDAP ポート `636` を `ip:port` 形式で入力します。

   - **Username**、**Password**：Entra ID に接続するために作成したユーザーとそのパスワードを入力します。

   - **Base DN**：Microsoft Entra ドメインサービスのドメイン名に応じて入力します。例：`emqxqa.onmicrosoft.com` は `DC=emqxqa,DC=onmicrosoft,DC=com` と入力します。特定の部署やグループにユーザーを制限するために他の属性を追加することも可能です。

   - **User Query Condition**：Microsoft Entra ID のデフォルトフィルターは `(&(objectClass=user)(sAMAccountName=${username}))` で、アカウント名（メールアドレス）を使ってログインします。`sAMAccountName` を `mail` に置き換えてメールアドレスでのログインも可能です。

   - IP アドレス + セキュア LDAP 直接アクセスを使用するため、**Enable TLS** をクリックし、**サーバー証明書の検証** は無効にします。

   - **Force MFA**：任意で有効にすると、このバックエンドのすべてのユーザーにログイン時の TOTP 検証を必須とします。デフォルトは無効です。

     <img src="../assets/sso-ad-dashboard.png" alt="sso-ad-dashboard" style="zoom:67%;" />

4. **更新** ボタンをクリックして設定を保存します。

これで Microsoft Entra ID SSO が有効になりました。LDAP オプションを使ったダッシュボードへのログイン方法は、[ログインとユーザー管理](#login-and-user-management) を参照してください。

## ログインとユーザー管理

LDAP ベースの SSO を有効にすると、EMQX ダッシュボードのログインページに LDAP SSO オプションが表示されます。**LDAP** ボタンをクリックし、ユーザーに割り当てられた LDAP 認証情報（例：ユーザー名とパスワード）を入力して、**ログイン** ボタンをクリックしてください。

<img src="../assets/sso_ldap.png" alt="sso_ldap" style="zoom:67%;" />

<img src="../assets/ldap_login.png" alt="ldap_login" style="zoom:67%;" />

LDAP 認証に成功すると、EMQX は自動的にダッシュボードユーザーを追加します。追加されたユーザーは [ユーザー](./system.md#users) で管理でき、役割や権限の割り当てが可能です。LDAP ユーザーにログイン時の TOTP 二要素認証を必須とする場合は、[SSO ユーザーの強制 MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users) を参照してください。

## ログアウト

ユーザーはダッシュボードの上部ナビゲーションバーにあるユーザー名をクリックし、ドロップダウンメニューの **ログアウト** ボタンをクリックしてログアウトできます。
