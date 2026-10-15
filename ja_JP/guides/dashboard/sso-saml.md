# SAMLベースのSSOの設定

このページでは、Security Assertion Markup Language（SAML）2.0標準プロトコルに基づくシングルサインオン（SSO）の設定および使用方法について説明します。

::: tip 前提条件

[シングルサインオン（SSO）](./sso.md)の基本概念に慣れていることを推奨します。

:::

## 対応しているSAMLサービス

EMQXダッシュボードは、SAML 2.0プロトコルをサポートするアイデンティティサービスと連携し、SAMLベースのSSOを実現できます。対応例は以下の通りです。

- [Microsoft Entra ID](https://www.microsoft.com/en-us/security/business/identity-access/microsoft-entra-id)
- [Okta](https://www.okta.com/)
- [OneLogin](https://www.onelogin.com/)

その他のアイデンティティプロバイダーも統合準備中で、今後のバージョンでサポート予定です。

## Microsoft Entra IDとの連携によるSSO設定

このセクションでは、Microsoft Entra IDをアイデンティティプロバイダー（IdP）として使用し、SSOを設定する方法を案内します。Microsoft側とEMQXダッシュボード側の両方で設定を完了する必要があります。

### ステップ1：EMQXダッシュボードでSAML SSOを有効化

1. ダッシュボードの **System** -> **SSO** に移動します。
2. **SAML 2.0**カードの **Enable** ボタンをクリックします。
3. 設定ページで以下の情報を入力します：
   - **Dashboard Address**：ユーザーがダッシュボードの実際のアクセスアドレスにアクセスできるようにします。特定のパスは指定しません。例：`http://localhost:18083`。このアドレスは自動的に連結され、IdP側設定用の **SSO Address** と **Metadata Address** が生成されます。
   - **SAML Metadata URL**：一時的に空欄のままにし、ステップ2の設定を待ちます。
   - **Signed Assertions from IdP**：IdPによるSAMLアサーションの署名を要求します。
   - **Signed Response Envelopes from IdP**：IdPによるSAMLレスポンスエンベロープの署名を要求します。

   EMQX 6.3.0以降、両オプションはデフォルトで有効です。これらは独立して設定可能で、IdPの署名動作に合わせて有効化してください。両方を無効にするとSAML署名検証がすべてオフになりますが、これは安全ではなくテスト用途のみにしてください。

### ステップ2：Microsoft Entra IDにアプリケーションを登録

1. 管理者として[MS Azureポータル](https://portal.azure.com/)にログインします。

2. **Microsoft Entra ID** -> **Enterprise Applications** -> **New Application**に移動し、**Create your own application**をクリックします。

   <img src="./assets/entra_id_create_own_app.png" alt="独自アプリケーションの作成" style="zoom:50%;" />

3. アプリケーション名を入力します（例：`EMQX Dashboard`）。**Integrate any other application you don't find in the gallery (Non-gallery)**を選択し、**Create**をクリックします。

   <img src="./assets/entra_id_saml_app_parameters.png" alt="SAMLアプリケーションのパラメータ設定" style="zoom:50%;" />

4. **Assign users and groups**をクリックし、EMQXダッシュボードアプリケーションにアクセス可能なユーザーとグループを割り当てます。
5. **Single sign-on**タブに移動し、**SAML**を選択、**Basic SAML Configuration**セクションの**Edit**ボタンをクリックします。
6. ステップ1のダッシュボードで提供された以下の情報を設定します：

   - **Identifier (Entity ID)**：ダッシュボードで提供された**Metadata Address**を入力します。例：`http://localhost:18083/api/v5/sso/saml/metadata`
   - **Reply URL (Assertion Consumer Service URL)**：ダッシュボードで提供された**SSO Address**を入力します。例：`http://localhost:18083/api/v5/sso/saml/acs`

   その他の情報は任意で、実際の要件に応じて設定してください。
7. **Save**をクリックして設定を保存します。

### ステップ3：EMQXダッシュボードの設定を完了

1. Microsoft Entra IDの作成したアプリケーションの**Single sign-on**タブに移動し、**Token Signing Certificate**セクションの**App Federation Metadata Url**をコピーします。

   <img src="./assets/entra_id_saml_metadata_url.png" alt="SAMLメタデータURL" style="zoom:50%;" />

2. ダッシュボードのステップ1にある**SAML Metadata URL**にコピーしたURLを貼り付けます。
3. いずれかの署名検証オプションが有効な場合、ブラウザでメタデータURLを開きます。返されたXML内の`IDPSSODescriptor`に`use="signing"`の`KeyDescriptor`と空でない`X509Certificate`が含まれていることを確認してください。含まれていない場合は、IdP側で署名証明書を設定してから進めてください。そうしないとSAMLバックエンドが起動できません。
4. **Update**をクリックして設定を完了します。

## Oktaとの連携によるSSO設定

このセクションでは、Oktaをアイデンティティプロバイダー（IdP）として使用し、SSOを設定する方法を案内します。Okta側とEMQXダッシュボード側の両方で設定を完了する必要があります。

### ステップ1：EMQXダッシュボードでOktaを有効化

1. ダッシュボードの **System** -> **SSO** に移動します。
2. **SAML 2.0**カードの **Enable** ボタンをクリックします。
3. 設定ページで以下の情報を入力します：
   - **Force MFA**：このバックエンドのすべてのユーザーにログイン時のTOTP認証を要求する場合に有効化します。デフォルトは無効です。詳細は[Forced MFA for SSO Users](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)を参照してください。
   - **Dashboard Address**：ユーザーがダッシュボードの実際のアクセスアドレスにアクセスできるようにします。特定のパスは指定しません。例：`http://localhost:18083`。このアドレスは自動的に連結され、IdP側設定用の **SSO Address** と **Metadata Address** が生成されます。
   - **SAML Metadata URL**：一時的に空欄のままにし、ステップ2の設定を待ちます。
   - **Signed Assertions from IDP**：IdPによるSAMLアサーションの署名を要求します。
   - **Signed Response Envelopes from IdP**：IdPによるSAMLレスポンスエンベロープの署名を要求します。

   EMQX 6.3.0以降、両オプションはデフォルトで有効です。これらは独立して設定可能で、IdPの署名動作に合わせて有効化してください。両方を無効にするとSAML署名検証がすべてオフになりますが、これは安全ではなくテスト用途のみにしてください。

### ステップ2：OktaのアプリケーションカタログにSAML 2.0アプリを追加

1. 管理者としてOktaにログインし、**Okta Admin Console**にアクセスします。

2. **Applications -> Applications**ページに移動し、**Create App integration**ボタンをクリックします。ポップアップでサインイン方法として`SAML 2.0`を選択し、**Next**をクリックします。

3. **General Settings**タブでアプリケーション名を入力します（例：`EMQX Dashboard`）。**Next**をクリックします。

4. **Configure SAML**タブでステップ1のダッシュボードで提供された情報を設定します：

   - **Single sign-on URL**：ダッシュボードで提供された**SSO Address**を入力します。例：`http://localhost:18083/api/v5/sso/saml/acs`
   - **Audience URI (SP Entity ID)**：ダッシュボードで提供された**Metadata Address**を入力します。例：`http://localhost:18083/api/v5/sso/saml/metadata`

   その他の情報は任意で、実際の要件に応じて設定してください。

5. 設定内容を確認し、**Next**をクリックします。

6. **Feedback**タブで**I'm an Okta customer adding an internal app**を選択し、必要に応じて他の情報を入力して、**Finish**をクリックしアプリケーション作成を完了します。

<img src="../assets/okta_config.png" alt="Okta設定画面" style="zoom:67%;" />

### ステップ3：Oktaで設定完了およびユーザー・グループの割り当て

1. Oktaの**Sign On**タブに移動し、**Metadata URL**をコピーします。
2. ダッシュボードのステップ1にある**SAML Metadata URL**にコピーしたURLを貼り付けます。
3. いずれかの署名検証オプションが有効な場合、ブラウザでメタデータURLを開きます。返されたXML内の`IDPSSODescriptor`に`use="signing"`の`KeyDescriptor`と空でない`X509Certificate`が含まれていることを確認してください。含まれていない場合は、IdP側で署名証明書を設定してから進めてください。そうしないとSAMLバックエンドが起動できません。
4. **Update**をクリックして設定を完了します。
5. **Okta > Assignments**タブで、EMQXダッシュボードアプリケーションに割り当てるユーザーとグループを設定できます。ここで割り当てられたユーザーのみがこのアプリケーションにログイン可能です。

## ログインとユーザー管理

SAMLシングルサインオンを有効にすると、EMQXダッシュボードのログインページにSSOオプションが表示されます。**SAML**ボタンをクリックすると、IdPの事前設定されたログインページに遷移し、割り当てられたユーザー資格情報でログインできます。

<img src="../assets/sso_saml.png" alt="SAMLログイン画面" style="zoom:67%;" />

<img src="../assets/okta_login.png" alt="Oktaログイン画面" style="zoom:67%;" />

SAML認証に成功すると、EMQXは自動的にダッシュボードユーザーを追加します。追加されたユーザーは[Users](./system.md#users)で管理でき、ロールや権限の割り当てが可能です。SAMLユーザーにログイン時のTOTP二要素認証を必須にする場合は、[Forced MFA for SSO Users](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)を参照してください。

## ログアウト

ユーザーはダッシュボードの上部ナビゲーションバーにあるユーザー名をクリックし、ドロップダウンメニューの**Logout**ボタンをクリックしてログアウトできます。これはダッシュボードからのログアウトのみであり、SAMLは現在シングルサインアウトをサポートしていないことにご注意ください。
