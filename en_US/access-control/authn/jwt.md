# JWT Authenticaton

[JWT](https://jwt.io/) is a token-based authentication mechanism. It does not rely on the server to retain client authentication information or session information.

## Authentication Principle

The client carries the JWT in the connection request, and EMQX uses the pre-configured secret or public key to verify the JWT signature. If the user configures a JWKS endpoint, the JWT authenticator will verify the JWT signature using the list of public keys queried from the JWKS endpoint. If the signature verification is successful, the JWT authenticator proceeds to check the claims. If there are claims such as `iat`, `nbf` or `exp`, the JWT authenticator will actively check the validity of the JWT based on these claims. In addition to this, we also allow users to specify some additional claims checks. The client is finally allowed to login only if the signature verification and claims check pass together.

## Common Usage

The JWT authenticator essentially only checks the signature of the JWT, which means that the JWT authenticator does not guarantee the legitimacy of the client's identity.

The common usage is that the user deploys an independent authentication server. The client first accesses the authentication server, the authentication server verifies the identity of the client, and issues JWT for the legitimate client, and then the client uses the obtained JWT to connect to EMQX .

Note that since the payload in the JWT is only Base64 encoded, anyone who gets the JWT can decode the payload to get the original information by Base64 decoding. Therefore, we do not recommend that users store some sensitive data in the payload of JWT.

In order to reduce the possibility of JWT leakage and theft, in addition to setting a reasonable validity period, we also recommend using TLS to encrypt client connections.

## Authorization

This is an optional function, we define a private Claim `acl`, which is used to carry the access rules of publish and subscribe in the JWT to control the permissions of the client after login.

::: tip
Authorization(ACL) rules returned by JWT will be checked before all Authorizers.
:::

Claim `acl` defines 3 optional fields, `pub`, `sub` and `all`, which are used to specify the whitelist of publish, subscribe and publish-subscribe topics respectively. Topic wildcards and placeholders are allowed in topic entries (currently only `${clientid}` and `${username}` are supported). Since there may be cases where topic content conflicts with placeholder syntax, we also provide the `eq` syntax to cancel placeholder interpolation. Example:

```json
{
  "exp": 1654254601,
  "username": "myuser",
  "acl": {
    "pub": [
      "testpub1/${username}",
      "eq testpub2/${username}"
    ],
    "sub": [
      "testsub1/${username}",
      "testpub2/${clientid}",
      "testsub2/#"
    ],
    "all": [
      "testall1/${username}",
      "testall2/${clientid}",
      "testall3/#"
    ]
  }
}
```

Where `testpub1/${username}` will be replaced with `testpub1/myuser` at runtime, and `eq testpub2/${username}` will still be processed as `testpub2/${username}` at runtime.

## Configure and use

![](./assets/authn-jwt-1.png)

Above is the configuration page for the JWT authenticator.

`JWT From` is used to specify the location of the JWT in the client connection request. The optional values ​​are `password` and `username`. For MQTT clients, these are the Password and Username fields in the MQTT CONNECT packet.

`Algorithm` is used to specify the encryption algorithm of JWT, optional values ​​are `hmac-based` and `public-key`. For different encryption methods, the JWT authenticator will have different configuration requirements.

1. Configured as `hmac-based`, indicating that JWT will use symmetric secret to generate signature and verify signature (support HS256, HS384 and HS512 algorithms), the corresponding configuration will be as follows:
   - `Secret`, the key used to verify the signature, the same key used to generate the signature
   - `Secret Base64 Encode`, indicating whether the `Secret` is Base64 encrypted, that is, whether EMQX needs to perform Base64 decryption on the secret when verifying the signature.

2. Configured as `public-key`, indicating that JWT uses the private key to generate the signature, and needs to use the public key to verify the signature (supports RS256, RS384, RS512, ES256, ES384 and ES512 algorithms). The corresponding configuration will be as follows:
   - `Public Key`, specifying the public key in PEM format used to verify the signature

`Verify Claims` is used to specify additional claims checks that the user wants to perform. It allows users to define multiple key-value pairs, where the key is used to find the corresponding claim in the JWT, so it needs to have the same name as the JWT claim to be checked, and the value is used to compare with the actual value of the claim. Therefore, it usually needs to be used with placeholders. Currently the placeholders supported in `Verify Claims` are `${clientid}` and `${username}`.

EMQX also supports periodically obtaining the latest JWKS from the JWKS endpoint, which is essentially a set of public keys that will be used to verify any JWT issued by the authorization server and signed using the RSA or ECDSA algorithm. If we want to use this feature, we first need to switch to the JWKS configuration page.

![](./assets/authn-jwt-2.png)

So as shown above, we now have two brand new configuration items:

1. `JWKS Server`, specify the server endpoint address for EMQX to query JWKS, the endpoint needs to support GET requests and return a JWKS that conforms to the specification.
2. `JWKS Refresh Interval`, specify the refresh interval of JWKS, that is, the interval for EMQX to query JWKS.
