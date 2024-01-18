# JWT Authentication

[JWT](https://JWT.io/) Authentication is an authentication mechanism based on Token. It does not rely on the server to retain the authentication information or session information of the client. The authentication information can be issued in batches with the key. The easiest way to authenticate.

## Create module

Open [EMQX Dashboard](http://127.0.0.1:18083/#/modules), click the "Modules" tab on the left, and choose to add:

![image-20200928161310952](./assets/modules.png)

Click "Select", and then select the JWT authentication module

![image-20200928141305205](./assets/auth_jwt1.png)

You need to configure the basic connection parameter table such as JWT key, authentication source, and public key (optional).

![image-20200928141327836](./assets/auth_jwt2.png)

If you need to add custom verification fields, you can refer to the following configuration:

![image-20200928141558866](./assets/auth_jwt3.png)

Finally, click the "Add" button to add the module successfully.

![image-20200928141558866](./assets/auth_jwt4.png)

## Authentication principle

The client uses Token as the user name or password (depending on the module configuration). When initiating a connection, EMQX uses the key and certificate in the configuration for decryption. If the decryption is successful, the authentication is successful, otherwise the authentication fails.

After JWT authentication is enabled in the default configuration, you can connect with any username + the following password:

```bash
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7ImF1dGhvciI6IndpdndpdiIsInNpdGUiOiJodHRwczovL3dpdndpdi5jb20ifSwiZXhwIjoxNTgyMjU1MzYwNjQyMDAwMCwiaWF0IjoxNTgyMjU1MzYwfQ.FdyAx2fYahm6h3g47m88ttyINzptzKy_speimyUcma4
```

::: tip

The above JWT Token is only for testing and can be generated with related tools according to your own business needs. An online generation tool is provided here: https://www.jsonwebtoken.io/.

:::

## JWT parameter meaning

-Key: JWT key, used to encrypt JWT string.

-Authentication source: The location of the client carrying JWT, used to configure the location of the client JWT string, username and password are optional.
-Public key file: The JWT public key file to be uploaded.
-Signature format: The default is the der format, the JWT native format can be selected, and the option is "raw".