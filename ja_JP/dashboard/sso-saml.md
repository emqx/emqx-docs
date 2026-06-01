# SAMLベースのSSO設定

このページでは、Security Assertion Markup Language（SAML）2.0標準プロトコルに基づくシングルサインオン（SSO）の設定および利用方法について説明します。

::: tip 前提条件

[シングルサインオン（SSO）](./sso.md)の基本概念に慣れていることを推奨します。

:::

## 対応しているSAMLサービス

EMQXダッシュボードは、SAML 2.0プロトコルをサポートするアイデンティティサービスと連携して、SAMLベースのSSOを実現できます。例えば以下のサービスがあります：

- [Okta](https://www.okta.com/)
- [OneLogin](https://www.onelogin.com/)

その他のアイデンティティプロバイダーは現在統合作業中であり、将来のバージョンでサポート予定です。

## Oktaとの連携によるSSO設定

このセクションでは、Oktaをアイデンティティプロバイダー（IdP）として使用し、SSOを設定する方法を案内します。Okta側とEMQXダッシュボード側の両方で設定を完了する必要があります。

### ステップ1：EMQXダッシュボードでOktaを有効化

1. ダッシュボードの **System** -> **SSO** に移動します。
2. **SAML 2.0** カードの **Enable** ボタンをクリックします。
3. 設定ページで以下の情報を入力します：
   - **Force MFA**：このバックエンドのすべてのユーザーにログイン時のTOTP認証を必須にする場合に有効化します。デフォルトは無効です。詳細は[SSOユーザーの強制MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)を参照してください。
   - **Dashboard Address**：ユーザーがダッシュボードの実際のアクセスアドレスにアクセスできるようにします。特定のパスは指定しません。例：`http://localhost:18083`。このアドレスはIdP側の設定用に**SSO Address**および**Metadata Address**の生成に自動的に連結されます。
   - **SAML Metadata URL**：一時的に空欄のままにして、ステップ2の設定を待ちます。
4. **Update** をクリックして設定を完了します。

### ステップ2：OktaのアプリケーションカタログにSAML 2.0アプリケーションを追加

1. 管理者としてOktaにログインし、**Okta Admin Console**にアクセスします。

2. **Applications -> Applications** ページに移動し、**Create App integration** ボタンをクリックします。ポップアップでサインイン方法として `SAML 2.0` を選択し、**Next** をクリックします。

3. **General Settings** タブでアプリケーション名を入力します。例：`EMQX Dashboard`。**Next** をクリックします。

4. **Configure SAML** タブでステップ1のダッシュボードに表示された情報を設定します：

   - **Single sign-on URL**：ダッシュボードで提供された**SSO Address**を入力します。例：`http://localhost:18083/api/v5/sso/saml/acs`。
   - **Audience URI (SP Entity ID)**：ダッシュボードで提供された**Metadata Address**を入力します。例：`http://localhost:18083/api/v5/sso/saml/metadata`。

   その他の情報は任意で、実際の要件に応じて設定してください。

5. 設定内容を確認し、**Next** をクリックします。

6. **Feedback** タブで **I'm an Okta customer adding an internal app** を選択し、必要に応じてその他の情報を入力して、**Finish** をクリックしアプリケーション作成を完了します。

<img src="./assets/okta_config.png" alt="okta_config" style="zoom:67%;" />

### ステップ3：Oktaでの設定完了とユーザー・グループの割り当て

1. Oktaの **Sign On** タブに移動し、**Metadata URL** をコピーします。
2. ダッシュボードのステップ1での **SAML Metadata URL** にコピーしたURLを貼り付け、**Update** をクリックします。
3. **Okta > Assignments** タブで、EMQXダッシュボードアプリケーションにユーザーとグループを割り当てます。ここで割り当てられたユーザーのみがこのアプリケーションにログイン可能です。

## ログインとユーザー管理

SAMLシングルサインオンを有効にすると、EMQXダッシュボードのログインページにSSOオプションが表示されます。**SAML** ボタンをクリックすると、IdPの事前設定されたログインページに遷移し、割り当てられたユーザーの認証情報でログインできます。

<img src="./assets/sso_saml.png" alt="sso_saml" style="zoom:67%;" />

<img src="./assets/okta_login.png" alt="okta_login" style="zoom:67%;" />

SAML認証に成功すると、EMQXは自動的にダッシュボードユーザーを追加します。このユーザーは[ユーザー管理](./system.md#users)で管理でき、ロールや権限の割り当ても可能です。SAMLユーザーにログイン時のTOTP二要素認証を必須にする場合は、[SSOユーザーの強制MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)を参照してください。

## ログアウト

ユーザーはダッシュボードのトップナビゲーションバーのユーザー名をクリックし、ドロップダウンメニューの **Logout** ボタンをクリックしてログアウトできます。ただし、これはダッシュボードからのログアウトのみであり、SAMLは現時点でシングルサインアウトをサポートしていないことにご注意ください。
