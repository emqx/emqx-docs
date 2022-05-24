# JWT Authentication

[JWT](https://JWT.io/) Authentication is an authentication mechanism based on Token. It does not rely on the server to retain the authentication information or session information of the client. The authentication information can be issued in batches with the key. The easiest way to authenticate.

## Create module

Open [EMQX Dashboard](http://127.0.0.1:18083/#/modules), click the "Modules" tab on the left, select "Add Module":

![Modules](./assets/auth_jwt1.png)

Then select "JWT Authentication" under "Authentication":

![Modules JWT Selected](./assets/auth_jwt2.png)

JWT authentication provides the following configuration items:

1. From: The field that stores the JWT when the client connects. It currently supports the selection of username or password.
2. Secret: The key used when issuing the JWT. It will be used to verify whether the JWT received by EMQX is legal and is applicable to the JWT issued by the HMAC algorithm.
3. Pubkey: It will be used to verify whether the JWT received by EMQX is legal, and is applicable to the JWT issued by RSA or ECDSA algorithm.
4. JWKs Addr: EMQX will periodically query the latest public key list from the JWKS server and use it to verify whether the received JWT is legitimate, and is applicable to JWTs issued by RSA or ECDSA algorithms.
5. Verify Claims: Whether to verify that the claims in the JWT payload are consistent with the claims.
6. Claims: A list of claims fields used to verify that the claims in the JWT payload are valid. The most common usage is to add a key-value pair with a key of `username` and a value of `%u`, `%u` as a placeholder that will be replaced at runtime with the Username that the client will actually use when connecting. The replaced value will be used to compare with the value of the same-key claim of the JWT Payload to have a one-to-one correspondence between JWT and Username. The following two placeholders are currently supported in the declaration field list:
    1. `%u`: It will be replaced at runtime with the Username used by the client when connecting.
    2. `%c`: It will be replaced at runtime with the Client ID used by the client when connecting.

> Note: When verifying JWT the values for `Secret`, `Pubkey`, and `JWKs Addr` are checked in that specific order. Keys with missing values will be ignored.

![JWT Module Settings](./assets/auth_jwt3.png)

After the configuration is complete, click the "Add" button to successfully add the JWT authentication module.

![Modules JWT Added](./assets/auth_jwt4.png)

## Authentication principle

Client carries JWT with username or password field (depending on the module configuration). When initiating a connection, EMQX uses the key and certificate in the configuration for decryption. If the decryption is successful, the authentication is successful, otherwise the authentication fails.

After JWT authentication is enabled in the default configuration, you can connect with any username + the following password:

```bash
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7ImF1dGhvciI6IndpdndpdiIsInNpdGUiOiJodHRwczovL3dpdndpdi5jb20ifSwiZXhwIjoxNTgyMjU1MzYwNjQyMDAwMCwiaWF0IjoxNTgyMjU1MzYwfQ.FdyAx2fYahm6h3g47m88ttyINzptzKy_speimyUcma4
```

::: tip

The above JWT Token is only for testing and can be generated with related tools according to your own business needs. An online generation tool is provided here: https://www.jsonwebtoken.io/.

:::
