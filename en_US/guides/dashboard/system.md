# System

The EMQX Dashboard's **System** menu includes submenus for **Users**, **API Key**, **License**, and **SSO**. Each of these submenus allows you to efficiently manage and configure user accounts, API keys, license settings, and single sign-on (SSO) configurations on their respective pages.

## Users

The **Users** page provides an overview of all active Dashboard users, including those generated via the [CLI](../cli.md).

To add new users, click the **+ Create** button in the page's top-right corner. A pop-up dialog will appear, prompting you to input the necessary user details. Once entered, click the **Create** button to generate the user account. You can access further user management options through the **Actions** column, such as editing users, updating passwords, or deleting users.

::: tip
For security reasons, starting with EMQX 5.0.0, Dashboard users cannot be used for REST API authentication. Use [API Keys](../api-keys.md) for programmatic access.
:::

<img src="./assets/ee-users.png" alt="image" style="zoom:67%;" />

Starting from EMQX 5.3, Dashboard users are assigned one of two predefined roles that control their access. For details on the available roles and permissions, see [Role-Based Access Control](../dashboard-security.md#role-based-access-control).

## API Key

The **API Key** page lets you create and manage API keys for authenticating [HTTP API](../../guides/api.md) requests. For instructions, see [API Keys](../api-keys.md).

## License

Click on **License** under the **System** menu on the left to access the License page. On this page, you can view the basic information of your current License, including the License connection quota usage, EMQX version, customer, and issue information.

Click **Update License** to upload your License Key. In the **License Settings** section, you can set the high and low watermark limits for the license connection quota usage. For more information about the license, see [Work with EMQX Enterprise License](../../get-started/deploy/license.md).

## Settings

The Settings can be accessed by clicking the settings icon in the top right corner of the page. You can change the language and theme color of the Dashboard. The theme color can be selected if you want to synchronize the OS theme. If this is enabled, the Dashboard theme will be automatically synchronized with the user's OS theme and cannot be selected manually.

<img src="./assets/settings_ee.png" alt="settings_ee" style="zoom:67%;" />

## SSO

The SSO page provides settings for the administrators to configure the SSO feature for user login management. For more information about the SSO feature, see [Single Sign-On (SSO)](../sso.md).

