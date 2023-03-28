# Dashboard

In EMQX, Dashboard is a web-based graphic interface to manage and monitor EMQX and connected devices in real time. 

For example,  configure a listener for EMQX Dashboard for accepting all incoming connections.

```
dashboard {
  listener {
    bind  =  "0.0.0.0:18083"
    max_connections  =  512
    ssl_options {
      cacertfile = "etc/certs/cacert.pem"
      certfile = "etc/certs/cert.pem"
      keyfile = "etc/certs/key.pem"
    }
  }
}
```

Where,

`bind  =  "0.0.0.0:18083"`  is to set the network address and port number that the listener will bind to. In this case, the listener will bind to all available network interfaces (`0.0.0.0`) on port `18083`.

`max_connections  =  512` is to set the maximum number of concurrent connections that the listener will accept. In this case, the maximum number of connections is set to `512`.

The `ssl_options` block is to set the SSL/TLS options for the listener. SSL/TLS is a protocol used to encrypt network traffic for security. The options specified here indicate the locations of the certificate authority (CA) certificate (`cacertfile`), server certificate (`certfile`), and server private key (`keyfile`) respectively. These files are typically used to verify the identity of the server and to encrypt the network traffic between the client and server.