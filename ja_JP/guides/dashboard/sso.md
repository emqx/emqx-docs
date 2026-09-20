# シングルサインオン（SSO）

シングルサインオン（SSO）は、ユーザーが複数のアプリケーションやシステムに対して、ユーザー名やパスワードなどの単一の認証情報を使ってログインできる認証機構です。各アプリケーションで個別に認証を行う必要がありません。EMQX ダッシュボードでSSO機能を有効にすると、ユーザーは企業のアカウント認証情報を使って便利にEMQX ダッシュボードにログインできます。組織はユーザーのIDや権限を一元管理でき、ユーザー管理の手間を軽減します。この機能により、企業のデータやシステムのセキュリティを強化しつつ、ユーザーの利便性を向上させます。

EMQXは、LDAP（Lightweight Directory Access Protocol）、SAML 2.0（Security Assertion Markup Language）、およびOpenID Connect（OIDC）に基づくSSO機能を実装しており、[OpenLDAP](https://www.openldap.org/)、[Azure AD (Microsoft Entra ID)](https://azure.microsoft.com/en-in/products/active-directory)、[Okta](https://www.okta.com/)、[OneLogin](https://www.onelogin.com/)などの主要なIDサービスとの連携をサポートしています。

## LDAPベースのSSO

EMQX ダッシュボードは、SSOのためにLDAPとの統合を可能にします。LDAPは分散ディレクトリ情報サービスにアクセスし管理するためのアプリケーション層プロトコルであり、企業環境におけるSSOソリューションで一般的な認証および認可プロトコルです。

LDAP SSOを使用する場合、EMQXはユーザーのLDAP認証情報をディレクトリサーバーに送信して検証を行います。検証に成功すると、ユーザーのセッション情報を作成し、ダッシュボードにログインさせます。

## SAMLベースのSSO

EMQX ダッシュボードは、SAMLをサポートするIdentity Provider（IdP）サービスとの統合を可能にします。SAMLはXMLベースのオープン標準データフォーマットであり、企業環境でのSSOソリューションに広く利用されています。

SAML SSOでは、ユーザーはIdentity Providerで一度だけ認証を行います。Identity Providerはユーザー情報を含むSAMLアサーションを生成し、EMQX ダッシュボードに送信します。EMQX ダッシュボードはこのSAMLアサーションを受け取り、検証に成功するとユーザーのセッション情報を作成し、ダッシュボードにログインさせます。SAMLはクロスドメイン認証および認可を可能にし、複数のアプリケーション間でのシームレスな統合をサポートします。企業は既存のSAML IDシステムにEMQXを簡単に組み込むことができ、ユーザーは安全かつ便利にEMQXのサービスを利用できます。

## OIDCベースのSSO

EMQX ダッシュボードは、OIDCをサポートするIdentity Provider（IdP）サービスとの統合を可能にします。OIDCはOAuth 2.0プロトコル上に構築されたIDレイヤーであり、ユーザーのID検証およびユーザー情報取得の標準化された方法を提供します。

OIDC SSOでは、ユーザーはIdentity Providerで認証を行い、IDトークンにユーザー情報を含めてEMQX ダッシュボードに返します。EMQX ダッシュボードはIDトークンを受け取り、検証に成功するとユーザーのセッション情報を作成し、ダッシュボードにログインさせます。OIDCはモダンでRESTfulな認証方式を提供し、最新のIDサービスとの統合を容易にします。

## 設定および利用のワークフロー

1. 管理者がダッシュボードでSSOを設定・有効化します。設定後、EMQX ダッシュボードのログインページにSSOのエントリポイントが表示されます。  
2. Identity Provider（IdP）側でユーザー情報を設定します。  
3. ユーザーはダッシュボードのログインページで利用可能な複数のSSO方式から選択します。  
4. ログイン成功後、EMQX ダッシュボードはユーザー情報に基づいてセッションを作成し、ユーザーはダッシュボードにアクセスできます。バックエンドで`force_mfa`が有効な場合は、セッション発行前にTOTP認証の完了が必要です。  
5. 管理者はユーザーごとに役割や権限を割り当てます。ユーザーはログインを更新後、対応するリソースにアクセス可能になります。

## SSOユーザーのMFA

EMQX 5.10以降、各SSOバックエンドで`force_mfa`を有効にすることで、SSOユーザーにログイン時のTOTPによる二要素認証を必須化できます。詳細は[SSOユーザーの強制MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)をご参照ください。

## 設定例

各SSO方式の設定例は以下をご覧ください。

- [LDAPシングルサインオンの設定](./sso-ldap.md)  
- [SAMLシングルサインオンの設定](./sso-saml.md)  
- [OIDCシングルサインオンの設定](./sso-oidc.md)
