# 单点登录（SSO）

单点登录（SSO）是一种身份验证机制，它允许用户使用一组凭据（例如用户名和密码）登录到多个应用程序或系统中，而无需在每个应用程序中单独进行身份验证。EMQX Dashboard 开启单点登录后，企业能够集中管理用户身份和权限，简化用户管理流程并带来良好的用户体验，在提高企业数据和系统的安全性的同时，兼顾了用户使用的便捷性。

EMQX 基于 LDAP 和 SAML 2.0 标准实施 SSO 功能，支持集成例如 [OpenLDAP](https://www.openldap.org/)、[Microsoft Active Directory](https://azure.microsoft.com/en-in/products/active-directory) 以及 [Okta](https://www.okta.com/)、[Entra ID](https://www.microsoft.com/en-in/security/business/identity-access/microsoft-entra-verified-id) 等主流身份服务。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

## 使用流程

1. 管理员在 Dashboard 中配置并启用单点登录，配置完成后，EMQX Dashboard 会在登录页面展示单点登录入口
2. 在身份服务提供商（Identity Provider，IdP）侧配置用户信息
3. 引导用户在 Dashboard 登录页面选择不同的单点登录方式
4. 登录成功后，EMQX Dashboard 会根据用户信息创建会话，用户即可访问 Dashboard
5. 管理员为不同用户分配角色与权限，用户刷新登录后即可访问对应的资源

## 配置示例

以下为您提供基于 LDAP 与 SAML 2.0 的 SSO 配置示例：

- [配置 LDAP 单点登录](./sso-ldap.md)
- [配置 SAML 单点登录](./sso-ldap.md)
