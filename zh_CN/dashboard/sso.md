# 单点登录（SSO）

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

单点登录（SSO）是一种身份验证机制，它允许用户使用一组凭据（例如用户名和密码）登录到多个应用程序或系统中，而无需在每个应用程序中单独进行身份验证。EMQX Dashboard 开启单点登录后，用户可以方便地使用企业账号管理系统登录到 EMQX Dashboard。 企业能够集中管理用户身份和权限，简化用户管理流程，在提高企业数据和系统的安全性的同时，兼顾了用户使用的便捷性。

EMQX 基于轻量级目录访问协议（LDAP）和安全断言标记语言（SAML ）2.0 标准实施 SSO 功能，支持集成例如 [OpenLDAP](https://www.openldap.org/)、[Azure AD（Microsoft Entra ID）](https://azure.microsoft.com/en-in/products/active-directory) 以及 [Okta](https://www.okta.com/)、[OneLogin](https://www.onelogin.com/) 等主流身份服务提供商。

## 基于 LDAP 的单点登录

EMQX Dashboard 允许您集成 LDAP 实现单点登录。LDAP 是一种应用层协议，用于访问和维护分布式目录信息服务。它是一种常见的身份验证和授权协议，被广泛应用于企业环境中的 SSO 解决方案。

使用 LDAP 单点登录时，EMQX 会将用户 LDAP 凭证发送到目录服务器验证，成功后即可创建用户会话信息并登录进入 Dashboard。

## 基于 SAML 的单点登录

EMQX Dashboard 允许您集成支持 SAML 的身份提供商服务实现单点登录。SAML 是一种基于 XML 的开放标准的数据格式 ，被广泛应用于企业环境中的 SSO 解决方案。

使用 SAML 单点登录时，用户只需在身份提供商验证身份一次，身份提供商即可生成包含用户信息的 SAML 断言发送给 EMQX Dashboard。EMQX Dashboard 收到并验证 SAML 断言成功后，即可创建用户会话信息并登录进入 Dashboard。SAML 提供了跨域认证和授权的能力，支持多个应用程序之间的无缝集成。企业可以轻松将 EMQX 接入已有的 SAML 身份体系中，使用户能更便捷安全地访问 EMQX 提供的服务。

## 配置和使用流程

1. 管理员在 Dashboard 中配置并启用单点登录，配置完成后，EMQX Dashboard 会在登录页面展示单点登录入口。
2. 在身份服务提供商（Identity Provider，IdP）侧配置用户信息。
3. 引导用户在 Dashboard 登录页面选择不同的单点登录方式。
4. 登录成功后，EMQX Dashboard 会根据用户信息创建会话，用户即可访问 Dashboard。
5. 管理员为不同用户分配角色与权限，用户刷新登录后即可访问对应的资源。

## 配置示例

以下为您提供基于 LDAP 与 SAML 2.0 的 SSO 配置示例：

- [配置 LDAP 单点登录](./sso-ldap.md)
- [配置 SAML 单点登录](./sso-saml.md)
