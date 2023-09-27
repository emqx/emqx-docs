# Configure LDAP-Based SSO

This page explains how to configure and use Single Sign-On (SSO) based on the Lightweight Directory Access Protocol (LDAP).

::: Prerequisite

Be familiar with the basic concepts of [Single Sign-On (SSO)](./sso.md).

:::

## Supported LDAP Servers

You can integrate the EMQX Dashboard with directory services that support the LDAPv3 protocol to enable LDAP-based SSO, such as:

- [OpenLDAP](https://www.openldap.org/)
- [Microsoft Active Directory](https://azure.microsoft.com/en-in/products/active-directory)

## Enable LDAP-Based SSO

This section guides you on how to enable LDAP-based SSO in the Dashboard.

1. Go to **System Settings** -> **Single Sign-On** in the Dashboard.
2. Select the **LDAP** option and click the **Enable** button.
3. On the configuration page, enter the basic information of the LDAP server:

| Option             | Description                                                  |
| ------------------ | ------------------------------------------------------------ |
| Server             | The address of the LDAP server, for example, `localhost:389`. |
| Username           | The username to access the LDAP server.                      |
| Password           | The password for the LDAP server user.                       |
| Base DN            | The base DN of the LDAP directory, the starting point for user searches. |
| User Lookup Filter | The filter in LDAP that matches users. In the LDAP user query condition, the system will automatically replace `${username}` with the actual input username for user matching.<br />- For standard LDAP, the default filter is `(&(objectClass=person)(uid=${username}))`.<br />- For Active Directory, the default filter is `(&(objectClass=user)(sAMAccountName=${username}))`.<br />This variable replacement mechanism allows you to construct query filters flexibly based on different user attributes for username queries and matching. For more information on the condition format, see [LDAP Filters](https://ldap.com/ldap-filters/). |
| Enable TLS         | Whether to enable TLS secure transmission for LDAP access. If enabled, certificate configuration is required. |

4. Click the **Update** button to save the configuration.

## Login and User Management

After enabling LDAP SSO, the EMQX Dashboard will display the LDAP SSO option on the login page. Click the **LDAP** button, enter the LDAP credentials assigned to the user (e.g., username and password), and click the **Login** button.

<img src="./assets/sso_ldap.png" alt="image-20230926182522354" style="zoom:67%;" />

<img src="./assets/ldap_login.png" alt="image-20230926182543521" style="zoom:67%;" />

After successfully authenticating with LDAP, EMQX will automatically add a Dashboard user, which you can manage in [Users](./system.md#users), such as assigning roles and permissions.

## Logout

Users can click their username in the top navigation bar of the Dashboard and then click the **Logout** button in the dropdown menu to log out.
