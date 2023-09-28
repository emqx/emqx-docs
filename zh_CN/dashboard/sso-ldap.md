# 配置 LDAP 单点登录

本页介绍了如何配置和使用基于轻量级目录访问协议（LDAP）协议实施的单点登录功能。

::: 先决条件

了解[单点登录（SSO）](./sso.md)的基本概念。

:::

## 支持的 LDAP 服务器

您可以将 EMQX Dashboard 与支持 LDAPv3 协议的目录服务集成，实现基于 LDAP 的单点登录，例如:

- [OpenLDAP](https://www.openldap.org/)
- [Microsoft Active Directory](https://azure.microsoft.com/en-in/products/active-directory)

## 启用 LDAP 单点登录

本节将指导您如何在 Dashboard 启用 LDAP 单点登录。

1. 转到 Dashboard **系统设置** -> **单点登录** 页面。
2. 选择 **LDAP** 选项，点击 **启用** 按钮。
3. 在配置页面中，输入 LDAP 服务器的基本信息：

| 选项         | 说明                                                         |
| ------------ | ------------------------------------------------------------ |
| 服务         | LDAP服务器的地址，例如`localhost:389`。                      |
| 用户名       | 访问 LDAP 服务器的 绑定 DN（Bind DN）。                                   |
| 密码         | 访问 LDAP 服务器的用户密码。                                 |
| 基本 DN      | LDAP 目录的基本 DN，搜索用户的起点。                         |
| 用户查询条件 | LDAP 中匹配用户的过滤器。在 LDAP 用户查询条件中,系统会自动替换 `${username}` 为实际输入的用户名，进行用户匹配。<br />- 对于标准 LDAP，默认过滤器是 `(&(objectClass=person)(uid=${username}))`。<br />- 对于 Active Directory，默认过滤器是 `(&(objectClass=user)(sAMAccountName=${username}))`。<br />该变量替换机制可以根据不同的用户属性灵活构造查询过滤器，进行用户名查询和匹配。有关条件格式详见 [LDAP过滤器](https://ldap.com/ldap-filters/)。 |
| 启用 TLS     | 是否启用 LDAP 访问的 TLS 安全传输。如果启用需要配置证书。    |

4. 点击 **更新** 按钮，保存配置。

## 登录与用户管理

启用 LDAP 单点登录后，EMQX Dashboard 会在登录页面展示单点登录选项。点击 **LDAP** 按钮，进入 LDAP 单点登录页面，输入为用户分配的 LDAP 凭证（例如用户名与密码）并点击**登录**按钮。

<img src="./assets/sso_ldap.png" alt="image-20230926182522354" style="zoom:67%;" />

<img src="./assets/ldap_login.png" alt="image-20230926182543521" style="zoom:67%;" />

成功进行 LDAP 身份验证后，EMQX 会自动添加一个 Dashboard 用户，您可以在[用户](./system.md#用户)中进行管理，例如为其分配角色与权限。

## 退出登录

用户可以在 Dashboard 顶部导航栏中点击用户名，在下拉菜单点击 **退出登录** 按钮，退出登录。
