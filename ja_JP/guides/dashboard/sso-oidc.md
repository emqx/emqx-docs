# OIDCベースのSSOの設定

このページでは、OpenID Connect（OIDC）プロトコルに基づくシングルサインオン（SSO）の設定および利用方法について説明します。

::: tip 前提条件

[シングルサインオン（SSO）](./sso.md)の基本概念に慣れていることを推奨します。

:::

## 対応するOIDCプロバイダー

EMQXダッシュボードは、OIDCプロトコルをサポートするアイデンティティサービスと連携して、OIDCベースのSSOを実現できます。例えば、[Okta](https://www.okta.com/)などが対応しています。

## Oktaとの連携によるSSOの設定

このセクションでは、Oktaをアイデンティティプロバイダー（IdP）として使用し、SSOを設定する手順を案内します。Okta側とEMQXダッシュボード側の両方で設定を完了する必要があります。

### ステップ1：OktaのアプリケーションカタログにOIDCアプリケーションを追加する

1. Oktaに管理者としてログインし、**Okta管理コンソール**にアクセスします。

2. **Applications -> Applications** ページに移動し、**Create App integration** ボタンをクリックします。ポップアップダイアログでサインイン方法として `OIDC - OpenID Connect` を選択します。

3. **Application type** として `Web Application` を選択し、**Next** をクリックします。

4. **General Settings** タブでアプリケーション名を入力します。例として `EMQX Dashboard` と入力し、**Next** をクリックします。

5. **LOGIN** タブで、EMQXダッシュボードの**ステップ2**で提供される情報を使って設定を行います。

   - **Sign-in redirect URIs**：ダッシュボードで提供される**Sign-in Redirect URI**を入力します。例：`http://localhost:18083/api/v5/sso/oidc/callback`。必要に応じて**ステップ2**完了後にこのURIを更新できます。
   - その他の設定は任意で、要件に応じて設定してください。

6. 設定内容を確認し、**Save** をクリックします。

詳細な手順については、[Oktaのドキュメント](https://help.okta.com/en-us/content/topics/apps/apps_app_integration_wizard_oidc.htm)を参照してください。

### ステップ2：EMQXダッシュボードでOIDCを有効化する

1. EMQXダッシュボードで、**System** -> **SSO** に移動します。

2. **OIDC**カードの**Enable**ボタンをクリックします。

3. 設定ページで以下の情報を入力します。

   - **Force MFA**：このバックエンドのすべてのユーザーにログイン時のTOTP認証を必須にする場合は有効にします。デフォルトは無効です。詳細は[SSOユーザーの強制MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)を参照してください。
   - **Provider**：`Okta`を選択するか、その他のプロバイダーの場合は`Generic`を選択します。
   - **Issuer URL**：Oktaの認可サーバーのURLを入力します。例：`https://example-org.okta.com`
   - **Client ID**：ステップ1で作成したアプリケーションからコピーします。
   - **Client Secret**：ステップ1で作成したアプリケーションからコピーします。
   - **Dashboard Address**：ユーザーがダッシュボードにアクセスするためのベースURLを入力します。例：`http://localhost:18083`。このアドレスはIdP側の設定用に**SSO Address**および**Metadata Address**の自動生成に利用されます。

4. **Update**をクリックして設定を完了します。

## ログインとユーザー管理

OIDC SSOを有効化すると、EMQXダッシュボードのログインページにSSOオプションが表示されます。**OIDC**ボタンをクリックすると、プリセットされたOIDCプロバイダーのログインページに遷移し、割り当てられたクレデンシャルでログインできます。

<img src="./assets/sso_oidc.png" alt="OIDCによるSSOログイン画面" style="zoom:67%;" />

<img src="./assets/okta_login.png" alt="Oktaログイン画面" style="zoom:67%;" />

認証に成功すると、EMQXは自動的にダッシュボードユーザーを追加します。追加されたユーザーは[ユーザー管理](./system.md#users)で管理でき、役割や権限の割り当てが可能です。OIDCユーザーにログイン時のTOTP二要素認証を必須にする場合は、[SSOユーザーの強制MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)を参照してください。

## ログアウト

ユーザーはダッシュボードの上部ナビゲーションバーにあるユーザー名をクリックし、ドロップダウンメニューの**Logout**ボタンをクリックしてログアウトできます。なお、この操作はダッシュボードからのログアウトのみを行い、IdP側のログアウトは行いません。
