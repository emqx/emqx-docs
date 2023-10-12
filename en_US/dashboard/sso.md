# Single Sign-On (SSO)

Single Sign-On (SSO) is an authentication mechanism that allows users to log in to multiple applications or systems using a single set of credentials, such as a username and password, without the need for separate authentication in each application. When EMQX Dashboard enables the SSO feature, users can conveniently log in to the EMQX Dashboard using their enterprise account credentials. Organizations can centrally manage user identities and permissions and simplify their user management processes. This feature enhances the security of enterprise data and systems while ensuring user convenience.

EMQX implements SSO functionality based on Lightweight Directory Access Protocol (LDAP) and the Security Assertion Markup Language (SAML) 2.0 standard, supporting integration with mainstream identity services such as [OpenLDAP](https://www.openldap.org/), [Microsoft Active Directory](https://azure.microsoft.com/en-in/products/active-directory), [Okta](https://www.okta.com/), [OneLogin](https://www.onelogin.com/), and more. 

{% emqxce %}
::: tip 
EMQX Enterprise Edition feature. EMQX Enterprise Edition offers comprehensive coverage of critical business scenarios, richer data integration support, higher production-grade reliability, and 24/7 global technical support. Feel free to [try it for free](https://www.emqx.com/zh/try?product=enterprise). 
::: 
{% endemqxce %}

## LDAP-Based SSO

EMQX Dashboard allows you to integrate LDAP for SSO. LDAP is an application-layer protocol used to access and maintain distributed directory information services. It is a common authentication and authorization protocol widely used in enterprise environments for SSO solutions.

EMQX sends user LDAP credentials to the directory server for validation when using LDAP SSO. Upon successful validation, it creates user session information and logs the user into the Dashboard.

## SAML-Based SSO

EMQX Dashboard allows you to integrate Identity Provider (IdP) services that support SAML for SSO. SAML is an XML-based open standard data format widely used in enterprise environments for SSO solutions.

With SAML SSO, users only need to authenticate themselves once with the Identity Provider. The Identity Provider generates a SAML assertion containing user information and sends it to EMQX Dashboard. Upon receiving and successfully verifying the SAML assertion, EMQX Dashboard creates user session information and logs the user into the Dashboard. SAML provides the capability for cross-domain authentication and authorization, supporting seamless integration between multiple applications. Enterprises can easily incorporate EMQX into their existing SAML identity systems, enabling users to access EMQX services conveniently and securely.

## Configuration and Usage Workflow

1. Administrators configure and enable SSO in the Dashboard. Once configured, the EMQX Dashboard displays an SSO entry point on the login page.
2. User information is configured on the Identity Provider (IdP) side.
3. Users are guided to choose different Single Sign-On methods on the Dashboard login page.
4. After successful login, EMQX Dashboard creates a session based on user information, allowing users to access the Dashboard.
5. Administrators assign roles and permissions to different users. Users can access corresponding resources after refreshing their login.

## Configuration Examples

Below are configuration examples for SSO based on LDAP and SAML 2.0:

- [Configure LDAP Single Sign-On](./sso-ldap.md)
- [Configure SAML Single Sign-On](./sso-saml.md)
