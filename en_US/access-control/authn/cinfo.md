# Client-Info Authentication

::: tip

The client-info authentication is only available in the EMQX Enterprise edition.

:::

Client-info authentication (`cinfo` type) is a lightweight authentication mechanism that verifies client properties and attributes against user-defined rules. These rules make use of the Variform expressions to define matching conditions and determine the authentication outcome when a match is found. For example, to quickly block clients without a username, you can use the condition `str_eq(username, '')` with a result of `deny`.

The authenticator configuration looks like below:

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

## Configure Client-Info Authentication in Dashboard

<!-- TODO --->

