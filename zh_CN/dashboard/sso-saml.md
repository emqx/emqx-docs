# 配置 SAML 单点登录

本页介绍了如何配置和使用基于安全断言标记语言（SAML ）2.0 标准协议实施的单点登录功能。

::: tip 先决条件

了解[单点登录（SSO）](./sso.md)的基本概念。

:::

## 支持的 SAML 服务

EMQX Dashboard 可以与以下支持 SAML 2.0 协议的身份服务集成，实现基于 SAML 的单点登录，例如：

- [Microsoft Entra ID](https://www.microsoft.com/zh-cn/security/business/identity-access/microsoft-entra-id)

- [Okta](https://www.okta.com/)
- [OneLogin](https://www.onelogin.com/)

其他身份提供商正在适配中，将在后续版本提供。

## 通过集成 Microsoft Entra ID 配置 SSO

本节指导你如何使用 Microsoft Entra ID 作为身份提供方（IdP）并配置 SSO。您需要分别完成 IdP 侧与 EMQX Dashboard 侧的配置。

### 步骤 1：在 EMQX Dashboard 中启用 SAML

1. 在 Dashboard 中进入**系统设置** -> **单点登录**。
2. 点击 **SAML 2.0** 卡片上的**启用**按钮。
3. 在配置页面输入以下信息：
   - **Dashboard 地址**：确保用户可以访问 Dashboard 的实际访问地址，不需要指定具体路径。例如 `http://localhost:18083`。此地址将被自动拼接以生成用于 IdP 侧配置的 **SSO Address** 和 **Metadata Address**。
   - **SAML 元数据 URL**：暂时留空，等待步骤 2 的配置。

### 步骤 2：注册一个应用以集成 Microsoft Entra ID

1. 以管理员身份登录 [MS Azure Portal](https://portal.azure.com/)。

2. 进入 **Microsoft Entra ID** -> **企业应用程序** -> **新建应用程序**，并点击 **创建你自己的应用程序**。

   <img src="./assets/entra_id_create_own_app.png" alt="entra_id_create_own_app" style="zoom:50%;" />

3. 输入应用名称，例如 `EMQX Dashboard`，选择**集成未在库中找到的任何其他应用程序(非库)**，并点击**创建**。

   <img src="./assets/entra_id_saml_app_parameters.png" alt="entra_id_saml_app_parameters" style="zoom:50%;" />

4. 点击**分配用户和组**以分配可以访问 EMQX Dashboard 应用的用户和组。

5. 进入**单一登录**标签页，选择 **SAML**，并点击**基本 SAML 配置**区域中的**编辑**按钮。

6. 使用步骤 1 中 Dashboard 提供的地址配置以下信息：

   - **标识符 (实体 ID)**：输入 Dashboard 提供的**元数据地址**，例如 `http://localhost:18083/api/v5/sso/saml/metadata`。
   - **回复 URL (断言使用者服务 URL)**：输入 Dashboard 提供的**单点登录地址**，例如 `http://localhost:18083/api/v5/sso/saml/acs`。

   其他信息为可选项，可根据实际需求进行配置。

7. 点击**保存**保存配置。

### 步骤 3：完成 EMQX Dashboard 配置

1. 在 Microsoft Entra ID 中，进入创建的应用的**单一登录**标签页，并在**令牌签名证书**区域中复制**应用联合元数据 URL**。

   <img src="./assets/entra_id_saml_metadata_url.png" alt="entra_id_saml_metadata_url" style="zoom:50%;" />

2. 在 Dashboard 中，将复制的 URL 粘贴到步骤 1 的 **SAML 元数据 URL** 中。

3. 点击**更新**以完成配置。

## 集成 Okta 身份服务配置 SSO

本节将指导您如何使用 Okta 作为身份提供商（IdP）并配置单点登录，您需要分别完成身份提供商（IdP）侧与 EMQX Dashboard 侧的配置。

### 步骤 1：在  EMQX Dashboard 中启用 Okta
1. 转到 Dashboard **系统设置** -> **单点登录**页面。
2. 选择 **SAML 2.0** 选项，点击**启用**按钮。
3. 在配置页面中，输入以下信息：
   - **Dashboard 地址**：确保用户能够访问 Dashboard 的实际访问地址，不需要带具体路径。例如 `http://localhost:18083`。该地址将自动拼接生成**单点登录地址**与**元数据地址**供 IdP 侧配置使用。
   - **SAML 元数据 URL**：暂时留空，等待第 2 步配置生成。

### 步骤 2：在 Okta 的应用程序目录添加 SAML 2.0 应用程序

1. 以管理员身份登录 Okta，然后转至 **Okta 管理控制台**。

2. 转到 **Applications ->  Applications** 页面，点击 **Create App integration** 按钮，在弹出框中选择 **Sign-in method** 为 `SAML 2.0`，点击 **Next** 按钮。

3. 在新打开的 **General Settings** 页签中，**App name** 输入您的应用名称，例如 `EMQX Dashboard`，点击 **Next** 按钮。
4. 在 **Configure SAML** 页签中，配置第 1 步中 Dashboard 提供的信息：

   - **Single sign-on URL**：填写 Dashboard 中提供的**单点登录地址**，例如 `http://localhost:18083/api/v5/sso/saml/acs`。

   - **Audience URI (SP Entity ID)**：填写 Dashboard 中提供的**元数据地址**，例如 `http://localhost:18083/api/v5/sso/saml/metadata`。

     其他信息是可选的，根据实际情况配置。

5. 检查设置并点击 **Next**。

6. 在 **Feedback** 页签中，选择 **I'm an Okta customer adding an internal app**，根据实际情况填写其他信息，点击 **Finish** 按钮完成应用创建。

<img src="./assets/dashboard-sso-saml-create-okta-app.png" alt="Dashboard SSO Okta 创建应用" style="zoom:67%;" />

### 步骤 3：完成 Dashboard 配置，在 Okta 中为应用分配用户与组

1. 在 Okta 中，转到 **Sign On** 选项卡，复制 **Metadata URL**。
2. 在 Dashboard 中，粘贴复制来的  **Metadata URL** 到第 1 步中的 **SAML 元数据 URL** 中，点击**更新**按钮。
3. 在 **Okta > Assignments** 选项卡中，您现在可以将用户和组分配给 EMQX Dashboard 应用，只有分配进来的用户才能登录此应用。
## 登录与用户管理

启用 SAML 单点登录后，EMQX Dashboard 会在登录页面展示单点登录选项。点击 **SAML** 按钮，会进入到 IdP 预设值的登录页面，在新页面中输入为用户分配的凭证进行登录。

<img src="./assets/sso_saml.png" alt="sso_saml" style="zoom:67%;" />

<img src="./assets/okta_login.png" alt="okta_login" style="zoom:67%;" />

登录成功后，将跳转回到 Dashboard，EMQX 会自动添加一个 Dashboard 用户，您可以在[用户](./system.md#用户)中进行管理，例如为其分配角色与权限。

## 退出登录

用户可以在 Dashboard 顶部导航栏中点击用户名，在下拉菜单点击**退出登录**按钮退出。注意，这只是在 Dashboard 侧退出了登录，SAML 暂不支持单点退出登录。
