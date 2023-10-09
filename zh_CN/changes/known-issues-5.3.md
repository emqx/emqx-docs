# 已知问题

## e5.3.0

- **SAML 单点登录的限制**

  EMQX Dashboard 支持基于安全断言标记语言（SAML ）2.0 标准的单点登录功能并支持集成 Okta 和 OneLogin 作为身份服务提供商。然而，目前基于 SAML 的单点登录暂时不支持证书签名验证机制，并且由于其复杂性，与 Azure Entra ID 不兼容。

