# シングルサインオン（SSO）

シングルサインオン（SSO）は、ユーザーが複数のアプリケーションやシステムに対して、ユーザー名やパスワードなどの単一の認証情報でログインできる認証機構です。各アプリケーションで個別に認証を行う必要がありません。EMQXダッシュボードでSSO機能を有効にすると、ユーザーは企業アカウントの認証情報を使って簡単にEMQXダッシュボードにログインできます。組織はユーザーのIDと権限を一元管理でき、ユーザー管理の手間を軽減します。この機能により、企業のデータやシステムのセキュリティを強化しつつ、ユーザーの利便性を向上させます。

EMQXは、Lightweight Directory Access Protocol（LDAP）、Security Assertion Markup Language（SAML）2.0標準、およびOpenID Connect（OIDC）に基づくSSO機能を実装しており、[OpenLDAP](https://www.openldap.org/)、[Azure AD（Microsoft Entra ID）](https://azure.microsoft.com/en-in/products/active-directory)、[Okta](https://www.okta.com/)、[OneLogin](https://www.onelogin.com/)などの主要なIDサービスとの連携をサポートしています。

## LDAPベースのSSO

EMQXダッシュボードは、SSOのためにLDAPとの統合を可能にします。LDAPは、分散ディレクトリ情報サービスにアクセスし維持するためのアプリケーション層プロトコルであり、企業環境におけるSSOソリューションで一般的に使用される認証および認可プロトコルです。

LDAP SSOを使用する場合、EMQXはユーザーのLDAP認証情報をディレクトリサーバーに送信して検証します。検証が成功すると、ユーザーのセッション情報を作成し、ダッシュボードへのログインを許可します。

## SAMLベースのSSO

EMQXダッシュボードは、SAMLをサポートするIdentity Provider（IdP）サービスとの統合を可能にします。SAMLは、企業環境で広く使われているXMLベースのオープン標準データフォーマットで、SSOソリューションに利用されます。

SAML SSOでは、ユーザーはIdentity Providerで一度だけ認証すればよく、Identity Providerはユーザー情報を含むSAMLアサーションを生成してEMQXダッシュボードに送信します。EMQXダッシュボードはSAMLアサーションを受信し、検証に成功するとユーザーのセッション情報を作成し、ダッシュボードへのログインを許可します。SAMLはクロスドメイン認証・認可を可能にし、複数アプリケーション間のシームレスな統合をサポートします。企業は既存のSAML IDシステムにEMQXを容易に組み込み、ユーザーは安全かつ便利にEMQXサービスを利用できます。

## OIDCベースのSSO

EMQXダッシュボードは、OIDCをサポートするIdentity Provider（IdP）サービスとの統合を可能にします。OIDCはOAuth 2.0プロトコルの上に構築されたIDレイヤーであり、ユーザーのID検証およびユーザー情報取得の標準化された方法を提供します。

OIDC SSOでは、ユーザーはIdentity Providerで認証し、IDトークンにユーザー情報を含めてEMQXダッシュボードに返します。EMQXダッシュボードはIDトークンを受信し、検証に成功するとユーザーのセッション情報を作成し、ダッシュボードへのログインを許可します。OIDCはモダンでRESTfulな認証手法を提供し、最新のIDサービスとの統合を容易にします。

## 設定および利用のワークフロー

1. 管理者がダッシュボードでSSOを設定・有効化します。設定後、EMQXダッシュボードのログインページにSSOのエントリーポイントが表示されます。
2. Identity Provider（IdP）側でユーザー情報を設定します。
3. ユーザーはダッシュボードのログインページで異なるシングルサインオン方式を選択します。
4. ログイン成功後、EMQXダッシュボードはユーザー情報に基づいてセッションを作成し、ユーザーはダッシュボードにアクセスできます。バックエンドで`force_mfa`が有効な場合は、セッション発行前にTOTP認証を完了する必要があります。
5. 管理者は各ユーザーに役割と権限を割り当てます。ユーザーはログインを更新後、対応するリソースにアクセスできます。

## SSOユーザー向けのMFA

EMQX 5.10以降、各SSOバックエンドで`force_mfa`を有効にすることで、SSOユーザーにログイン時のTOTPによる二要素認証を必須にできます。詳細は[SSOユーザー向けの強制MFA](../multi-factor-authn/multi-factor-authentication.md#forced-mfa-for-sso-users)をご参照ください。

## 設定例

各SSO方式の設定例は以下をご覧ください。

- [LDAPシングルサインオンの設定](./sso-ldap.md)
- [SAMLシングルサインオンの設定](./sso-saml.md)
- [OIDCシングルサインオンの設定](./sso-oidc.md)
