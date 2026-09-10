# Client-Info Authentication

Client-Info authentication (`cinfo` type) is a lightweight authentication mechanism that verifies client properties and attributes against user-defined rules. These rules make use of the Variform expressions to define matching conditions and determine the authentication outcome when a match is found. For example, to quickly block clients without a username, you can use the condition `str_eq(username, '')` with a result of `deny`.

## Configure Client-Info Authentication via Dashboard

1. In the EMQX Dashboard, navigate to **Access Control** -> **Authentication** in the left menu to enter the **Authentication** page.
2. Click **Create** at the top right corner, then select **Client Info** as the **Mechanism**. Client-Info authentication does not require selecting a backend, so you can proceed by clicking **Next** to enter the **Configure Parameters** step.
3. Follow the instructions below to configure the backend:
   - **Precondition**: A [Variform expression](../../configuration/configuration.md#variform-expressions) used to control whether this Client Info authenticator should be applied to a client connection. The expression is evaluated against attributes from the client (such as `username`, `clientid`, `listener`, etc.). The authenticator will only be invoked if the expression evaluates to the string `"true"`. Otherwise, it will be skipped. For more information about the precondition, see [Authentication Preconditions](./authn.md#authentication-preconditions).
   - Click **Add** in the **Checks**.
     - In the **Match Conditions** input box, enter the Variform expression used to match client information. If there are multiple expressions, enter each on a new line. When all expressions return `true`, the authenticator will return the relevant result; otherwise, the current check will be skipped. The following variables are supported in the expressions:
       - `username`: Username
       - `clientid`: Client ID
       - `client_attrs.*`: Client Attributes
       - `peerhost`: Client IP
       - `cert_subject`: TLS Certificate Subject
       - `cert_common_name`: TLS Certificate Common Name
     - From the **Result** dropdown menu, select the result to return if the match condition is true:
       - `allow`: Allow the client to connect.
       - `ignore`: Defer the authentication to the next authenticator in the chain.
       - `deny`: Deny the client to connect.
4. Click **Create** to complete the authentication configuration.

## Configure Client-Info Authentication via Configuration Items

Below is a configuration example for the Client-Info authenticator:

```bash
authentication = [
  {
    mechanism = cinfo
    checks = [
      # Allow clients with a username starts with 'super-'
      {
        is_match = "regex_match(username, '^super-.+$')"
        result = allow
      },
      # Deny clients with an empty username and client ID starts with 'v1-'
      {
        # when is_match is an array, it returns 'true' if all checks evaluate to 'true'
        is_match = ["str_eq(username, '')", "str_eq(nth(1,tokens(clientid,'-')), 'v1')"]
        result = deny
      }
      # If all checks are exhausted without an 'allow' or a 'deny' result, proceed to the next authenticator
    ]
  },
  # ... more authenticators ...
  # ...
  # If all authenticators are exhausted without an 'allow' or a 'deny' result, the client is rejected
]
```

More match expression examples:

- Match all clients: `true`.
- Match clients where the TLS certificate common name matches the username: `str_eq(cert_common_name, username)`
- Match clients whose password is the `sha1` hash of the environment variable `EMQXVAR_SECRET` concatenated with the client ID:`str_eq(password, hash(sha1, concat([clientid, getenv('SECRET')])))`
- Match clients whose attribute `client_attrs.group` is not `g0`: `str_neq(client_attrs.group, 'g0')`
- Match client IDs that start with the zone name:`regex_match(clientid, concat(['^', zone, '.+$']))`

