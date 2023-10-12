# System

{% emqxce %}

The EMQX Dashboard's **System** menu includes submenus for **Users**, **API Keys**, and **Settings**. Each of these submenus allows you to efficiently manage and configure user accounts, API keys, and Dashboard setting preferences on their respective pages.

{% endemqxce %}

{% emqxee %}

The EMQX Dashboard's **System** menu includes submenus for **Users**, **API Keys**, **License**, and **SSO**. Each of these submenus allows you to efficiently manage and configure user accounts, API keys, license settings, and single sign-on (SSO) configurations on their respective pages.

{% endemqxee %}

## Users

The **Users** page provides an overview of all active Dashboard users, including those generated via the [CLI](../admin/cli.md).

To add new users, simply click the **+ Create** button located in the top-right corner of the page. A pop-up dialog will appear, prompting you to input the necessary user details. Once entered, click the **Create** button to generate the user account. For further user management, such as editing user information, updating passwords, or deleting users, you can easily access these options through the **Actions** column.

{% emqxce %}
<img src="./assets/users.png" alt="image" style="zoom:67%;" />

> All users have admin rights to delete other users, but you cannot delete the currently logged in user on the Dashboard.
> For security reasons, starting with EMQX 5.0.0, Dashboard user cannot be used for REST API authentication.

{% endemqxce %}

{% emqxee %}
<img src="./assets/ee-users.png" alt="image" style="zoom:67%;" />

Starting from EMQX 5.3, the Dashboard introduces the Role-Based Access Control (RBAC) feature for its users. RBAC allows you to assign permissions to users based on their roles within the organization. This feature simplifies authorization management, enhances security by restricting access, and improves organizational compliance, making it an essential access control mechanism for the Dashboard.

Currently, either of the following two predefined roles can be set for a user. You can select the role from the **Role** dropdown when you create users.
+ Administrator

    Administrators have full access to manage all EMQX features and resources, including client management, system configuration, API key, and user management.

+ Viewer

    Viewers can access all EMQX data and configurations, corresponding to all `GET` requests in the REST API. However, they do not have the rights to create, modify, or delete any data.

{% endemqxee %}

## API Key

On the API Keys page, you can generate an API key and secret key for accessing the [HTTP API](../admin/api.md) by following the steps below.

1. Click the **+ Create** button on the top right corner of the page to bring up the Create API Key pop-up dialog. 

2. On the Create API Key dialog, configure in the detailed information for the API key. 

   If the **Expire At** textbox is left empty, the API key will never expire.

3. Click the **Confirm** button, and the API key and secret Key are created and displayed in the **Created Successfully** dialog. 

   ::: warning Notice

   You need to save the API Key and Secret Key in a safe place because the secret key will not be shown again.

   :::

    Click the **Close** button to close the dialog.

<img src="./assets/api-key.png" alt="image" style="zoom:67%;" />

You can view the details of the API key by clicking the name in the **Name** column. You click the **Edit** button in the **Actions** column to reset the expiration time, change the status, and edit the note of the API key. If an API key is no longer needed, you can delete it by clicking the **Delete** button.

<img src="./assets/api-key-detail.png" alt="image" style="zoom:50%;" />

{% emqxee %}

## License

Click on **License** under the **System** menu on the left to access the License page. On this page, you can view the basic information of your current License, including the License connection quota usage, EMQX version, customer, and issue information. Click **Update License** to upload your License Key. In the **License Settings** section, you can set the high and low watermark limits for the license connection quota usage.

<img src="./assets/license.png" alt="license" style="zoom:35%;" />

{% endemqxee %}

## Settings

{% emqxce %}

The Settings page can be accessed by clicking on the System menu on the left or under the username drop-down menu in the top right corner of the page. You can change the language and theme color of the Dashboard on the settings page, the theme color can be selected if you want to synchronize the OS theme, if this is enabled, the Dashboard theme will be automatically synchronize with the user's OS theme and cannot be selected manually. If telemetry is enabled, users will share usage information with EMQX to help the EMQX development team understand how users are using EMQX and to continuously improve the product.

![image](./assets/settings.png)

{% endemqxce %}

{% emqxee %}

The Settings can be accessed by clicking the setting icon on the top right corner of the page. You can change the language and theme color of the Dashboard, the theme color can be selected if you want to synchronize the OS theme, if this is enabled, the Dashboard theme will be automatically synchronized with the user's OS theme and cannot be selected manually. 

<img src="./assets/settings_ee.png" alt="settings_ee" style="zoom:67%;" />

{% endemqxee %}

{% emqxee %}

## SSO

The Single Sign-On (SSO) page provides settings for the administrators to configure the SSO feature for user login management. For more information about the SSO feature, see [Single Sign-On (SSO)](./sso.md).

{% endemqxee %} 

