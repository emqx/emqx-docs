# JWT

[JWT](https://jwt.io/) is a token-based authentication mechanism. It does not rely on the server to retain client authentication information or session information.

## Authentication Principle

A client sends JWT as a password. When accepting the connection, EMQX Broker uses the configured key or secret to verify
JWT signature. If the signature is not successfully verified, the authentication fails. On success, the JWT authenticator
verifies JWT _claims_ (payload "fields") like `exp` (expiration time), etc. for validity. A user may
provide some additional checks. If all the checks succeed, the authentication succeeds too. Otherwise, the authentication fails.

Sample JWT:
```
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2NTQyNTQ2MDEsInVzZXJuYW1lIjoibXl1c2VyIn0.yi1FSkrLbd9qv4-TrfEa8opiQ9NqShgvMT2lG3pzhgA
```

The decoded header is:
```json
{
  "alg": "HS256",
  "typ": "JWT"
}
```

Decoded payload:
```
{
  "exp": 1654254601,
  "username": "myuser"
}
```

The corresponding JWT authenticator settings:
```
{
  mechanism = jwt,
  use_jwks = false
  algorithm = "hmac-based"
  secret = "abcdef"
  verify_claims {
    "username" = "${username}"
  }
}
```

::: danger
An individual token cannot be invalidated before it expires. Please properly set the validity time (`exp` claim).
:::


## JWT Authorization

JWT Authenticator can extract ACL rules from the token and keep them associated with the connection.
If provided, the rules will be used for authorizing further operations.

The format for ACL rules is:
```json
{
  "exp": 1654254601,
  "username": "myuser",
  "acl": {
    "pub": [
      "eq testpub1/${username}",
      "testpub2/${clientid}",
      "testpub3/#"
    ],
    "sub": [
      "eq testsub1/${username}",
      "testsub2/${clientid}",
      "testsub3/#"
    ],
    "all": [
      "eq testall1/${username}",
      "testall2/${clientid}",
      "testall3/#"
    ]
  }
}
```
`pub`, `sub`, and `all` lists serve as topic whitelists for the corresponding operation types. Topic entries
allow wildcards and placeholders (`${clientid}` and `${username}`). To suppress placeholder interpolation, one may use the
demonstrated `eq` syntax: `"eq topic/without/interpolation/${clientid}"`.

## Configuration

PostgreSQL authentication is identified with `mechanism = jwt`.

JWT authenticator supports three modes:
* `hmac-based` — use a fixed symmetric secret to verify JWT signatures (`algorithm = "hmac-based"` and `use_jwks = false`)
  {
    mechanism = jwt
    algorithm = "hmac-based"
    use_jwks = false
    secret = "abcdef"
    verify_claims {
      "username" = "${username}"
    }
  }
* `public-key` — use a fixed public key to verify JWT signatures (`algorithm = "public-key"` and `use_jwks = false`)
  {
    mechanism = jwt
    algorithm = "public-key"
    use_jwks = false
    public_key = "path/to/public_key.pem"
    verify_claims {
      "username" = "${username}"
    }
  }
* `jwks` — use public keys that are regularly fetched from a key server (`algorithm = "public-key"` and `use_jwks = true`)
  ```
  {
    mechanism = jwt
    algorithm = "public-key"
    use_jwks = true
    ssl {
      enable = true
      verify = verify_peer
      server_name_indication = "jwks-server.alguna-empresa.com"
      keyfile = "data/certs/client.key"
      certfile = "data/certs/client.crt"
      cacertfile = "data/certs/ca.crt"
    }
    verify_claims {
      "username" = "${username}"
    }
    endpoint = "https://10.212.43.12:8080/jwks.json"
    refresh_interval = 1000
  }

### Common Options

#### `algorithm`
Required field, one of `hmac-based` or `public-key`.

#### `use_jwks`

Required boolean field. For `hmac-based` must be `false`. For the `public-key` algorithm
identifies whether to use a predefined public key or to fetch keys from a JWKS server.

### `hmac-based` Options

#### `secret`

Required string field with a symmetric secret used to verify JWT signatures.

#### `secret_base64_encoded`

Optional boolean field. A true value indicates that provided `secret` is Base64 encoded.
The default value is `false`.

### `public-key` Options (no JWKS server)

#### `public_key`

Required string field with a path to a pem-encoded public key for JWT verification.

### `public-key` Options (with JWKS server)

#### `endpoint`

Required string field with JWKS server URL.

#### `refresh_interval`

Optional field with a positive integer value. Specifies the frequency (in seconds) of key updates from the JWKS server.

#### `pool_size`

Optional field with a positive integer value. Specifies the number of concurrent connections to the JWKS server.
The default value is `8`.

#### `ssl`

Map with SSL options for connecting to the endpoint. Optional, the default is `{enable = false}`.

For `http://` endpoints should be disabled, for `https://` endpoints should be enabled and specify correct SSL options.
Possible options are:
* `enable` — boolean, specifies whether to use SSL. Default is `false`.
* `verify` — `verify_none` or `verify_peer`. Specifies whether to verify endpoint server name.
* `server_name_indication` — optional string with an explicit [SNI](https://wikipedia.org/wiki/Server_Name_Indication) value for the endpoint.

#### `cacertfile`, `certfile`, and `keyfile`
String fields with filenames of the corresponding pem-encrypted keys/certificates for endpoint connection.

