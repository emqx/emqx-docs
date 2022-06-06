# SSL(TLS)

## Client options

When connecting to external resources like a database or some HTTP API, EMQX can establish secure connections
via SSL(TLS).

SSL configuration options are common for all external resources.

Example SSL configuration:

```
ssl {
    ciphers = [
        "TLS_AES_256_GCM_SHA384",
        "TLS_AES_128_GCM_SHA256",
        "TLS_CHACHA20_POLY1305_SHA256",
        "TLS_AES_128_CCM_SHA256",
        "TLS_AES_128_CCM_8_SHA256"
    ]
    depth = 10
    enable = true
    reuse_sessions = true
    secure_renegotiate = true
    verify = verify_peer
    versions = [
        "tlsv1.3"
    ]
}
```

All SSL parameters are optional. Also, for the resources that support SSL, the very `ssl` configuration section is optional too. Its absence denotes that TLS layer shouldn't be used, i.e. is equivalent to
```
ssl {
    enable = false
}
```

