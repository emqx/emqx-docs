# Configure OIDC-Based SSO

This page explains how to configure and use Single Sign-On (SSO) based on the OpenID Connect (OIDC) protocol.

::: tip Prerequisite

Be familiar with the basic concepts of [Single Sign-On (SSO)](./sso.md).

:::

## Supported OIDC provider

The EMQX Dashboard can integrate with identity services that support the OIDC protocol to enable OIDC-based SSO, such as [Okta](https://www.okta.com/).

## Configure SSO by Integrating with Okta 

This section guides you on how to use Okta as an Identity Provider (IdP) and configure SSO. You need to complete configurations on both the Okta and EMQX Dashboard sides.

### Step 1: Add an OIDC Application to Okta's Application Catalog

1. Log in to Okta as an administrator and go to the **Okta Admin Console**.

2. Go to the **Applications -> Applications** page, click the **Create App integration** button, and select `OIDC - OpenID Connect` as the sign-in method in the pop-up dialog.

3. Select the `Web Application` as the **Application type** and click **Next**.

4. On the **General Settings** tab, enter your application name, for example, `EMQX Dashboard`. Click **Next**.

5. On the **LOGIN** tab, configure the settings using the information provided by the EMQX Dashboard in **Step 2**:

   - **Sign-in redirect URIs**: Enter the **Sign-in Redirect URI** provided in the Dashboard, such as `http://localhost:18083/api/v5/sso/oidc/callback`. You can update this URI later after completing **Step 2** if needed.
   - Additional settings are optional and can be configured according to your specific requirements.
   
6. Review the settings and click **Save**.

For more detailed instructions, refer to the [Okta documentation](https://help.okta.com/en-us/content/topics/apps/apps_app_integration_wizard_oidc.htm).

### Step 2: Enable OIDC in the EMQX Dashboard

1. In the EMQX Dashboard, navigate to **System** -> **SSO**.
2. Click the **Enable** button on the **OIDC** card.
3. On the configuration page, enter the following information:
   - **Provider**: Choose `Okta` or select `Generic` for other providers.
   - **Issuer URL**: This is the URL of your Okta authorization server, e.g., `https://example-org.okta.com`.
   - **Client ID**: Copy it from the application created in **Step 1**.
   - **Client Secret**: Copy it from the application created in **Step 1**.
   - **Dashboard Address**: Enter the base URL where users can access the Dashboard, such as `http://localhost:18083`. This address will be automatically combined to generate the **SSO Address** and **Metadata Address** for configuration on the IdP side.

4. Click **Update** to finish the configuration.

## Login and User Management

After enabling OIDC SSO, the EMQX Dashboard will display the SSO option on the login page. Click the **OIDC** button to go to the OIDC provider preset login page, where you can enter the credentials assigned to the user for login.

<img src="./assets/sso_oidc.png" alt="sso_oidc" style="zoom:67%;" />

<img src="./assets/okta_login.png" alt="okta_login" style="zoom:67%;" />

After successful authentication, EMQX will automatically add a Dashboard user, which you can manage in [Users](./system.md#users), such as assigning roles and permissions.

## Logout

Users can click their username in the top navigation bar of the Dashboard and then click the **Logout** button in the dropdown menu to log out. Please note that this only logs you out of the Dashboard.
