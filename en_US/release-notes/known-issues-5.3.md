# Known Issues in EMQX 5.3

## e5.3.0

- **Limitation in SAML-Based SSO**

  EMQX Dashboard supports Single Sign-On based on the Security Assertion Markup Language (SAML) 2.0 standard and integrates with Okta and OneLogin as identity providers. However, the SAML-based SSO currently does not support a certificate signature verification mechanism and is incompatible with Azure Entra ID due to its complexity.

