# PSK Authentication

Pre-Shared Key (PSK) authentication is a method of authentication that relies on a pre-shared key for identity verification. Using the PSK authentication method, both the client and EMQX must pre-share the same key before establishing a secured connection. The pre-shared key is then used to encrypt and decrypt data in establishing the TLS connection between the client and EMQX and in subsequent communications. With the PSK authentication enabled, the client and EMQX can authenticate each other and establish a secure connection without the need for certificates or certificate authorities. 

This page introduces how to enable PSK authentication in EMQX.

1. Create a file `data/psk_file.txt` in any directory, containing the identity and secret value of the pre-shared key. 

   ::: tip

   The secret value can be any string, but its length must correspond to the selected cipher. For example, if the cipher is TLS_PSK_WITH_AES_128_CBC_SHA, the secret must be 128 bits long.

   :::

   ```bash
   # One data per line, in the format of PSKIdentity:SharedSecret
   emqx_c:BA0DB2A3448345A3A13A91C2ADA44778
   emqx_a:A6FC9EDF62864125AAE7658BEAE6170C
   ```

2. Add the `psk_authentication` configuration group in the `emqx.conf` configuration file.

   ```bash
   psk_authentication {
     enable = true
     init_file = "data/psk_file.txt"
   }
   ```

3. Configure the SSL listener in the `emqx.conf` configuration file. Modify the `listeners.ssl.default` group by adding the following options. 

   - `ssl_options.versions`: Remove `tlsv1.3` support, since `tlsv1.3` version configuration suppresses PSK ciphers.
   - `ssl_options.ciphers`: Configure to use PSK cipher suits.

   ::: tip

   If the `RSA-PSK` cipher suites are used, the `RSA` certificate is still required, see [RFC4279](https://www.rfc-editor.org/rfc/rfc4279#section-4) for details.

   :::

   ```bash
   listeners.ssl.default {
     acceptors = 4
     bind = 8883
     ssl_options {
       ciphers = ["RSA-PSK-AES256-GCM-SHA384","RSA-PSK-AES256-CBC-SHA384","RSA-PSK-AES128-GCM-SHA256","RSA-PSK-AES128-CBC-SHA256","RSA-PSK-AES256-CBC-SHA","RSA-PSK-AES128-CBC-SHA"]
       versions = ["tlsv1.2", "tlsv1.1", "tlsv1"]
     }
   }
   ```

   





