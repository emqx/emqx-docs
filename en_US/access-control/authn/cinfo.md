# Client-Info Authentication

Client-Info authentication (`cinfo` type) is a lightweight authentication mechanism that verifies client properties and attributes against user-defined rules. These rules make use of the Variform expressions to define matching conditions and determine the authentication outcome when a match is found. For example, to quickly block clients without a username, you can use the condition `str_eq(username, '')` with a result of `deny`.

## Configure Client-Info Authentication via Dashboard

In the EMQX Dashboard, navigate to **Access Control** -> **Authentication** in the left menu to enter the **Authentication** page. Click **Create** at the top right corner, then select **Client Info** as the **Mechanism**,  Client-Info authentication does not require selecting a backend, so you can proceed by clicking **Next** to enter the **Configure Parameters** step.

1. Click **Add** in the **Checks**.
   - In the **Match Conditions** input box, enter the Variform expression used to match client information. If there are multiple expressions, enter each on a new line. When all expressions return `true`, the authenticator will return the relevant result; otherwise, the current check will be skipped. The following variables are supported in the expressions:
     - `username`: Username
     - `clientid`: Client ID
     - `client_attrs.*`: Client Attributes
     - `peerhost`: Client IP
     - `cert_subject`: TLS Certificate Subject
     - `cert_common_name`: TLS Certificate Common Name
   - Select `allow`, `ignore`, or `deny` from the **Result** dropdown menu.
2. Click **Create** to complete the authentication configuration.

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
  # If all authenticators are exhausted without an 'allow' or a 'deny' result, the client is not rejected
]
```

More match expression examples:

- Match all clients: `true`.
- Match clients where the TLS certificate common name matches the username: `str_eq(cert_common_name, username)`
- Match clients whose password is the `sha1` hash of the environment variable `EMQXVAR_SECRET` concatenated with the client ID:`str_eq(password, hash(sha1, concat([clientid, getenv('SECRET')])))`
- Match clients whose attribute `client_attrs.group` is not `g0`: `str_neq(client_attrs.group, 'g0')`
- Match client IDs that start with the zone name:`regex_match(clientid, concat(['^', zone, '.+$']))`

