# OIDCベースのSSOの設定

このページでは、OpenID Connect（OIDC）プロトコルに基づくシングルサインオン（SSO）の設定と使用方法について説明します。

::: tip 前提条件

[シングルサインオン（SSO）](./sso.md)の基本概念に慣れていることを推奨します。

:::

## 対応するOIDCプロバイダー

<<<<<<< HEAD
EMQXダッシュボードは、OIDCプロトコルをサポートするアイデンティティサービスと連携して、OIDCベースのSSOを実現できます。例えば以下のサービスがあります。

- [Microsoft Entra ID](https://www.microsoft.com/en-us/security/business/identity-access/microsoft-entra-id)
- [Okta](https://www.okta.com/)

## Microsoft Entra IDとの連携によるSSO設定

このセクションでは、Microsoft Entra IDをアイデンティティプロバイダー（IdP）として使用し、SSOを設定する方法を案内します。Microsoft側とEMQXダッシュボード側の両方で設定を完了する必要があります。

### ステップ1：EMQXダッシュボードでOIDCを有効化

1. EMQXダッシュボードで、**System** -> **SSO** に移動します。
2. **OIDC**カードの**Enable**ボタンをクリックします。

### ステップ2：Microsoft Entra IDにアプリケーションを登録

1. 管理者として[MS Azureポータル](https://portal.azure.com/)にログインします。

2. **Microsoft Entra ID** -> **Enterprise Applications** -> **New Application** に移動し、**Create your own application**をクリックします。

   <img src="./assets/entra_id_create_own_app.png" alt="Microsoft Entra IDで独自アプリケーションを作成" style="zoom:50%;" />

3. アプリケーション名（例：`EMQX Dashboard`）を入力し、**Register an application to integrate with Microsoft Entra ID (App you're developing)** を選択して、**Create**をクリックします。

   <img src="./assets/entra_id_oidc_app_parameters.png" alt="Microsoft Entra ID OIDCアプリケーションパラメータ" style="zoom:50%;" />

4. **Register an application**ページで、サポートするアカウントの種類を選択し、EMQXダッシュボードの**ステップ1**で提供された情報を使って**Redirect URL**を設定します。

   - **Redirect URL**：`Web`を選択し、ダッシュボードで提供された**Sign-in Redirect URI**（例：`http://localhost:18083/api/v5/sso/oidc/callback`）を入力します。

5. **Certificates and Secrets** -> **Client secrets**タブに移動し、**New client secret**をクリック、説明を入力し、有効期限を選択して**Add**をクリックします。生成されたシークレット値をコピーしてください。これは**ステップ3**で使用します。

### ステップ3：EMQXダッシュボードの設定を完了

1. 設定画面で以下の情報を入力します。

   - **Provider**：`Generic`のままにします。
   
   - **Issuer URL**：これは**OpenID Connect metadata document**に対応し、**ステップ2**のアプリケーション概要ページの**Endpoints**タブで確認できます。ただし、`/.well-known/openid-configuration`の部分はEMQXが自動で付加するため除きます。例：`https://login.microsoftonline.com/<tenant_id>/v2.0`（`<tenant_id>`はディレクトリ（テナント）IDです）。
   
   - **Client ID**：**ステップ2**のアプリケーション概要ページにある**Application (client) ID**を入力します。
   
     <img src="./assets/entra_id_oidc_app_config.png" alt="Microsoft Entra ID OIDCアプリ設定" style="zoom:50%;" />
   
   - **Client Secret**：**ステップ2**で生成したシークレット値を入力します。
   
   - **Dashboard Address**：ユーザーがダッシュボードにアクセスするためのベースURLを入力します。例：`http://localhost:18083`。このアドレスはIdP側の設定に使う**SSO Address**および**Metadata Address**の生成に自動的に組み合わされます。
   
     <img src="./assets/entra_id_oidc_dashboard.png" alt="EMQXダッシュボードアドレス設定" style="zoom:50%;" />

2. **Update**をクリックして設定を完了します。
=======
EMQXダッシュボードは、OIDCプロトコルをサポートするアイデンティティサービスと連携して、OIDCベースのSSOを実現できます。例えば、[Okta](https://www.okta.com/)が対応しています。
>>>>>>> origin/release-5.10

## Oktaとの連携によるSSOの設定

このセクションでは、Oktaをアイデンティティプロバイダー（IdP）として使用し、SSOを設定する方法を案内します。Okta側とEMQXダッシュボード側の両方で設定を完了する必要があります。

### ステップ1：EMQXダッシュボードでOIDCを有効化

<<<<<<< HEAD
1. EMQXダッシュボードで、**System** -> **SSO** に移動します。
2. **OIDC**カードの**Enable**ボタンをクリックします。

### ステップ2：OktaのアプリケーションカタログにOIDCアプリケーションを追加
=======
1. Oktaに管理者としてログインし、**Okta管理コンソール**にアクセスします。

2. **Applications -> Applications** ページに移動し、**Create App integration** ボタンをクリックして、ポップアップダイアログでサインイン方法として `OIDC - OpenID Connect` を選択します。
>>>>>>> origin/release-5.10

1. 管理者としてOktaにログインし、**Okta Admin Console**にアクセスします。

<<<<<<< HEAD
2. **Applications -> Applications**ページに移動し、**Create App integration**ボタンをクリック、ポップアップでサインイン方法として`OIDC - OpenID Connect`を選択します。

3. **Application type**として`Web Application`を選択し、**Next**をクリックします。

4. **General Settings**タブでアプリケーション名（例：`EMQX Dashboard`）を入力し、**Next**をクリックします。
=======
4. **General Settings** タブでアプリケーション名を入力します。例として `EMQX Dashboard` と入力し、**Next** をクリックします。

5. **LOGIN** タブで、EMQXダッシュボードの**ステップ2**で提供される情報を使って設定します：

   - **Sign-in redirect URIs**：ダッシュボードで提供された**Sign-in Redirect URI**を入力します。例：`http://localhost:18083/api/v5/sso/oidc/callback`。必要に応じて、**ステップ2**完了後にこのURIを更新できます。
   - その他の設定は任意で、要件に応じて設定してください。
>>>>>>> origin/release-5.10

5. **LOGIN**タブで、EMQXダッシュボードの**ステップ1**で提供された情報を使って設定します。

   - **Sign-in redirect URIs**：ダッシュボードで提供された**Sign-in Redirect URI**（例：`http://localhost:18083/api/v5/sso/oidc/callback`）を入力します。
   - その他の設定は任意で、必要に応じて調整してください。
   
6. 設定内容を確認し、**Save**をクリックします。

詳細な手順は[Oktaドキュメント](https://help.okta.com/en-us/content/topics/apps/apps_app_integration_wizard_oidc.htm)を参照してください。

<<<<<<< HEAD
### ステップ3：EMQXダッシュボードの設定を完了
=======
1. EMQXダッシュボードで、**System** -> **SSO** に移動します。

2. **OIDC**カードの**Enable**ボタンをクリックします。

3. 設定ページで以下の情報を入力します：
   - **Force MFA**：このバックエンドのすべてのユーザーに対してログイン時にTOTP認証を必須にする場合に有効化します。デフォルトは無効です。詳細は[SSOユーザーの強制MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)を参照してください。
   - **Provider**：`Okta`を選択するか、その他のプロバイダーの場合は`Generic`を選択します。
   - **Issuer URL**：Oktaの認可サーバーのURLを入力します。例：`https://example-org.okta.com`。
   - **Client ID**：ステップ1で作成したアプリケーションからコピーします。
   - **Client Secret**：ステップ1で作成したアプリケーションからコピーします。
   - **Dashboard Address**：ユーザーがダッシュボードにアクセスするベースURLを入力します。例：`http://localhost:18083`。このアドレスはIdP側の設定で使用する**SSOアドレス**および**メタデータアドレス**の生成に自動的に組み合わされます。
>>>>>>> origin/release-5.10

1. 設定画面で以下の情報を入力します。

   - **Provider**：`Okta`を選択、またはその他のプロバイダーの場合は`Generic`を選択します。
   - **Issuer URL**：Oktaの認可サーバーのURLを入力します。例：`https://example-org.okta.com`。
   - **Client ID**：**ステップ2**で作成したアプリケーションからコピーします。
   - **Client Secret**：**ステップ2**で作成したアプリケーションからコピーします。
   - **Dashboard Address**：ユーザーがダッシュボードにアクセスするためのベースURLを入力します。例：`http://localhost:18083`。このアドレスはIdP側の設定に使う**SSO Address**および**Metadata Address**の生成に自動的に組み合わされます。

2. **Update**をクリックして設定を完了します。

## ログインとユーザー管理

<<<<<<< HEAD
OIDC SSOを有効化すると、EMQXダッシュボードのログインページにSSOオプションが表示されます。**OIDC**ボタンをクリックすると、設定済みのOIDCプロバイダーのログインページに遷移し、ユーザーに割り当てられた認証情報でログインできます。
=======
OIDC SSOを有効にすると、EMQXダッシュボードのログインページにSSOオプションが表示されます。**OIDC**ボタンをクリックすると、あらかじめ設定されたOIDCプロバイダーのログインページに遷移し、割り当てられたユーザー資格情報でログインできます。
>>>>>>> origin/release-5.10

<img src="./assets/sso_oidc.png" alt="OIDCログイン画面" style="zoom:67%;" />

<img src="./assets/okta_login.png" alt="Oktaログイン画面" style="zoom:67%;" />

<<<<<<< HEAD
認証に成功すると、EMQXは自動的にダッシュボードユーザーを追加します。ユーザーは[Users](./system.md#users)で管理でき、役割や権限の割り当ても可能です。

## ログアウト

ユーザーはダッシュボードの上部ナビゲーションバーにあるユーザー名をクリックし、ドロップダウンメニューの**Logout**ボタンをクリックしてログアウトできます。なお、この操作はダッシュボードからのログアウトのみであることにご注意ください。
=======
認証に成功すると、EMQXは自動的にダッシュボードユーザーを追加します。追加されたユーザーは[ユーザー管理](./system.md#users)で管理でき、役割や権限の割り当ても可能です。OIDCユーザーにログイン時のTOTP二要素認証を必須にする場合は、[SSOユーザーの強制MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)を参照してください。

## ログアウト

ユーザーはダッシュボードの上部ナビゲーションバーにあるユーザー名をクリックし、ドロップダウンメニューの**Logout**ボタンをクリックしてログアウトできます。なお、これはダッシュボードからのログアウトのみであることにご注意ください。
>>>>>>> origin/release-5.10
