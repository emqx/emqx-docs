# SAML 2.0 Single Sign-On

SAML 2.0 Single Sign-On (SSO) lets users log in to the EMQX Dashboard through your organization's Identity Provider (IdP), such as Keycloak, Okta, or Azure AD, instead of managing a separate Dashboard password.

## How It Works

SAML 2.0 SSO involves two parties:

- **Identity Provider (IdP)**: Your organization's authentication service. It verifies the user's identity and issues a signed assertion.
- **Service Provider (SP)**: EMQX Dashboard. It trusts the IdP's assertion and grants access based on it.

The two parties establish trust by exchanging metadata: EMQX publishes an SP metadata document that the IdP registers, and EMQX fetches the IdP's metadata to verify incoming assertions. Once trust is established, the login flow works as follows:

1. A user clicks **Login with SSO** on the Dashboard login page.
2. EMQX redirects the user to the IdP login page.
3. The user authenticates at the IdP.
4. The IdP posts a signed `SAMLResponse` back to EMQX.
5. EMQX validates the assertion and logs the user in, provisioning their account automatically if it does not already exist.

## Setup Overview

Configuring SAML SSO involves setting up in both EMQX and your identity provider (IdP):

1. **Enable the SAML SSO module in EMQX**: this generates the service provider (SP) metadata and ACS URLs required for the next step.
2. **Register EMQX as a SAML client in your IdP**: provide the SP metadata URL or upload the metadata file, then record the IdP metadata URL.
3. **Finish the EMQX configuration**: enter the IdP metadata URL and configure the signing settings.

## Prerequisites

- EMQX Enterprise 4.4.34 or later.
- A SAML 2.0 compatible IdP. This guide uses Keycloak 26.3 or later as the example.
- Network connectivity between EMQX nodes and the IdP host. EMQX fetches the IdP metadata URL at module load time.
- HTTPS must be enabled on both the EMQX Dashboard and the IdP. Keycloak 26.x requires HTTPS for SAML clients.
- The IdP metadata URL (an XML endpoint provided by the IdP).

## Add the SAML SSO Module

1. In the left navigation panel of the Dashboard, click **Modules**.

2. Click **Add Module**.

3. Select **SAML 2.0 Single Sign-On** and click **Select**.

4. Fill in the configuration fields. See [Configuration Fields](#configuration-fields) for details.

5. Click **Add** to enable the module.

   ![SAML SSO Module Config](./assets/saml_sso_config.png)

On the configuration page, two read-only addresses are displayed:

- **SSO Address**: `<Dashboard Address>/api/v4/sso/saml/acs`. This is the ACS (Assertion Consumer Service) URL, the endpoint where your IdP posts the `SAMLResponse` after authentication. Register this with your IdP as the ACS URL or Valid Redirect URI.
- **Metadata Address**: `<Dashboard Address>/api/v4/sso/saml/metadata`. This is the Service Provider (SP) metadata URL to register as the Client ID with your IdP.

## Configuration Fields

| Field                                          | Default                  | Description                                                  |
| ---------------------------------------------- | ------------------------ | ------------------------------------------------------------ |
| **Dashboard Address**                          | `http://localhost:18083` | The externally reachable base URL of the Dashboard, without a trailing slash or path. EMQX derives the SSO Address and Metadata Address from this value. |
| **IDP Metadata URL**                           | Required                 | The URL from which EMQX fetches the IdP's SAML metadata XML. In Keycloak, this follows the pattern `https://<keycloak-host>/realms/<realm>/protocol/saml/descriptor`. |
| **SP Signs Authentication Requests**           | `false`                  | When enabled, EMQX signs outgoing SAML `AuthnRequest` messages (EMQX -> IdP). Requires a valid SP certificate and private key. |
| **Force MFA for SSO Users**                    | `false`                  | When enabled, all users who log in via SAML SSO must complete TOTP-based [Multi-Factor Authentication](../getting-started/dashboard-mfa.md). Users who have not yet configured MFA are prompted to do so on their first login. |
| **Require Signed Response Envelopes from IDP** | `true`                   | Requires the IdP to sign the SAML `Response` envelope (IdP -> EMQX). Recommended for production. |
| **Require Signed Assertions from IDP**         | `true`                   | Requires the IdP to sign the SAML `Assertion` element (IdP -> EMQX). Recommended for production. |
| **SP Public Key/Certificate**                  | —                        | The SP certificate in PEM format. Required when **SP Signs Authentication Requests** is enabled. Paste the PEM content directly or use **Select file** to upload a file. |
| **SP Private Key**                             | —                        | The SP private key in PEM format. Required when **SP Signs Authentication Requests** is enabled. Paste the PEM content directly or use **Select file** to upload a file. |

::: warning Note

For production deployments, keep at least one of **Require Signed Response Envelopes from IDP** or **Require Signed Assertions from IDP** enabled. Disabling both removes all cryptographic verification of the identity assertion.

:::

## Configure the IdP

The following steps use Keycloak as an example. The exact steps vary by IdP, but the values you register remain the same.

### Register SP Metadata with Your IdP

After enabling the module, EMQX publishes an SP metadata document at the Metadata Address. This XML document contains the SP Entity ID, the ACS URL, and the SP signing certificate (if SP signing is enabled). Your IdP needs this information to trust and communicate with EMQX.

There are two ways to provide it:

- **Automatic import**: If your IdP supports metadata import by URL, paste the Metadata Address directly. The IdP automatically fetches the XML and configures the Entity ID and ACS URL.
- **Manual upload**: If your IdP requires a file, open the Metadata Address in a browser, save the XML, and upload it to your IdP.

### Create a SAML Client in Keycloak

1. Log in to the Keycloak Admin Console and select your realm.

2. Navigate to **Clients** and click **Create client**.

3. Set **Client type** to `SAML`.

4. Set **Client ID** to the SP Metadata Address shown on the EMQX configuration page:

   ```
   https://<dashboard-addr>/api/v4/sso/saml/metadata
   ```

   ::: tip

   EMQX does not support custom SP Client IDs. You must use the Metadata Address exactly as shown.

   :::

5. Set **Valid Redirect URIs** or **ACS URL** to the SSO Address shown on the EMQX configuration page:

   ```
   https://<dashboard-addr>/api/v4/sso/saml/acs
   ```

6. Under the **Keys** tab, enable **Sign documents** and **Sign assertions**. Both options are required unless you explicitly disabled **Require Signed Response Envelopes from IDP** and **Require Signed Assertions from IDP** in EMQX (both default to `true`).

7. Copy the **IDP metadata URL** from **Realm Settings** -> **Endpoints** -> **SAML 2.0 Identity Provider Metadata**. Paste this URL into the **IDP Metadata URL** field in EMQX.

### Prepare SP Certificate and Key (If SP Signing Is Enabled)

If you enable **SP Signs Authentication Requests**, you need an SP certificate and private key. When generating them in Keycloak:

1. Go to **Clients** -> your SAML client -> **Keys** tab.

2. Click **Regenerate** (not **Export**). The key file downloads automatically.

   ::: warning

   Do not use the **Export** button. Exported keys are password-protected, and EMQX does not support password-protected PEM keys.

   :::

3. Keycloak downloads the certificate and key in raw Base64 format without PEM headers. Convert them to PEM format before uploading to EMQX:

   ```bash
   # Convert certificate
   ./scripts/convert-keycloak-certs.sh <downloaded-cert-file> sp_public.pem cert
   
   # Convert private key
   ./scripts/convert-keycloak-certs.sh <downloaded-key-file> sp_private.pem key
   ```

4. Upload or paste the converted PEM files into the **SP Public Key/Certificate** and **SP Private Key** fields in EMQX.

## SSO Login Flow

1. The user opens the Dashboard login page. If SSO is enabled, a **Login with SSO** button is displayed.
2. The user clicks **Login with SSO** and is redirected to the IdP login page.
3. The user authenticates at the IdP.
4. The IdP posts the `SAMLResponse` back to the EMQX ACS endpoint.
5. EMQX validates the assertion, provisions the user if they do not already exist, and redirects the browser to the Dashboard.

If **Force MFA for SSO Users** is enabled, users are prompted to configure or complete MFA before reaching the Dashboard.

### User Provisioning

SSO users are provisioned automatically on their first successful login (Just-in-Time provisioning):

- New SSO users are assigned the `viewer` role by default.
- Existing Dashboard users with a matching username retain their current role and settings.

To grant a higher privilege level to an SSO user, navigate to **General** -> **Users** in the Dashboard after their first login and update their role.

## MFA Integration

When **Force MFA for SSO Users** is enabled, every user who logs in via SAML SSO must configure TOTP-based Multi-Factor Authentication. Users without MFA configured are prompted to set it up immediately after their first successful SAML authentication.

Administrators can still disable MFA for individual SSO users regardless of the global setting. Navigate to **General** -> **Users**, select the user, and toggle off MFA.

For general MFA configuration, see [Multi-Factor Authentication](../getting-started/dashboard-mfa.md).
