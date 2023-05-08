# Enable PSK Authentication

<!--What is PSK Authentication, and it's purpose/benefits?-->

You can enable TLS PSK support in EMQX. This feature uses a pre-shared key to establish a secure connection. The client and EMQX negotiate a shared key during their previous interaction, which is then used to encrypt and decrypt data in TLS connections and subsequent communications, allowing both the client and EMQX to authenticate each other and establish a secure connection without the need for certificates or certificate authorities.

<!--Any prerequisites? For example, prepare `data/init.psk` file? How to generate the pre-shared key?-->

You can enable `psk_authentication` in the `emqx.conf` configuration file.

1. Configure the  `psk_authentication` configuration group.

```bash
psk_authentication {
    ## Whether to enable the PSK feature.
    enable = true

    ## If init file is specified, emqx will import PSKs from the file
    ## into the built-in database at startup for use by the runtime.
    ##
    ## The file has to be structured line-by-line, each line must be in
    ## the format: <PSKIdentity>:<SharedSecret>
    init_file = "data/init.psk"

    ## Specifies the separator for PSKIdentity and SharedSecret in the init file.
    ## The default is colon (:)
    separator = ":"

    ## The size of each chunk used to import to the built-in database from psk file
    ## chunk_size = 50
}
```

The file `data/init.psk` containing the identity of the pre-shared key and secret value of the pre-shared key should be created:

```bash
myclient1:8c701116e9127c57a99d5563709af3deaca75563e2c4dd0865701ae839fb6d79
myclient2:d1e617d3b963757bfc21dad3fea169716c3a2f053f23decaea5cdfaabd04bfc4
```

2. Configure the SSL listener in the `emqx.conf` configuration file. Modify the `listeners.ssl.default` group by adding the following options. 

   - `ssl_options.versions`: Configure to use `tlsv1.2`.

   - `ssl_options.ciphers`: Configure to use PSK ciphers. For more information on PSK ciphers, see [Reference - TLS Ciphers](./reference.md#tls-ciphers).

     ::: tip

     If the `RSA-PSK` cipher suites are used, the `RSA` certificate is still required, see [RFC4279](https://www.rfc-editor.org/rfc/rfc4279#section-4) for details.

     :::

```bash
listeners.ssl.default {
  ...
  ssl_options.versions = ["tlsv1.2"]
  ssl_options.ciphers = "PSK-AES256-GCM-SHA384,PSK-AES128-GCM-SHA256,PSK-AES256-CBC-SHA384,PSK-AES256-CBC-SHA,PSK-AES128-CBC-SHA256,PSK-AES128-CBC-SHA,RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,RSA-PSK-RC4-SHA"
  ...
}

```



