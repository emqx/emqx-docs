# 认证与鉴权

认证指的是在客户端连接到 MQTT 消息中间件时，需要对客户端的身份进行验证。鉴权则是指对通过身份验证的客户端授予相应的访问级别和权限。通过身份验证的机制和细粒度的授权，能够确保只有经过授权的客户端可以连接到您的 MQTT 消息中间件和执行特定操作。

本汇总了一些学习资源，主要探讨认证和鉴权的概念，以及如何通过身份验证机制来保证 MQTT 消息系统的安全。

**博客**

- [Username and Password Authentication](https://auth0.com/blog/username-password-authentication/)
- [Adding Salt to Hashing: A Better Way to Store Passwords](https://auth0.com/blog/adding-salt-to-hashing-a-better-way-to-store-passwords/)
- [通过用户名密码认证保障 MQTT 接入安全](https://www.emqx.com/zh/blog/securing-mqtt-with-username-and-password-authentication)
- [MQTT 5.0 中的安全认证机制：增强认证介绍](https://www.emqx.com/zh/blog/leveraging-enhanced-authentication-for-mqtt-security)

**网页**

- [Introduction to JSON Web Tokens](https://jwt.io/introduction)

想要了解在 EMQX 中如何实现认证和鉴权以及如何进行功能配置，参见[访问控制](../access-control/overview.md)。
