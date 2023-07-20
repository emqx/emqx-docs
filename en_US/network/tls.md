# Network and TLS

EMQX provides comprehensive SSL/TLS capability support, including support for one-way/two-way authentication and X.509 certificate authentication, which is applicable in the following scenarios:

- Establishing a connection between MQTT clients and EMQX
- Connecting to external resources, such as a database
- Different EMQX nodes in a cluster communicate with each other

::: tip

SSL and TSL protocols are often adopted in network communications to ensure that the data transmission remains confidential and cannot be intercepted or modified by an attacker. 

:::

This chapter mainly provides instructions on how to enable the SSL/TLS connection when an MQTT client connects to EMQX and when EMQX needs to access external resources. 

## Enable SSL/TLS Connection

EMQX can establish secure connections via SSL/TLS when accepting the access of an MQTT Client. You can enable SSL/TLS encrypted connections for all connections, including MQTT connection, to ensure the security of access and message transmission.

### Safety Benefits

Enabling SSL/TLS connection provides the following safety benefits.

1. **Strong Authentication**: Both communicating parties will verify each other's identities by checking the X.509 digital certificate held by the other party. These types of digital certificates are usually issued by trusted Certificate Authorities (CAs) and cannot be forged.
2. **Confidentiality**: Each session will be encrypted using the session key negotiated by both parties. No third party can know the communication content, so even if the session key is compromised, it does not affect the security of other sessions.
3. **Integrity**: The possibility of data being tampered with in encrypted communication is extremely low.s

For client SSL/TLS connections, you can choose one of the following two modes based on your usage scenario:

| Usage Mode                                                   | Advantages                                                   | Disadvantages                                                |
| ------------------------------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| Directly establish SSL/TLS connection between the client and EMQX. | Easy to use, no additional components required               | It will increase EMQX's resource consumption, and if the number of connections is huge, it may lead to high CPU and memory consumption. |
| Terminate TLS connection through a proxy or load balancer.   | No impact on EMQX performance, and provides load balancing capabilities. | Only a few cloud vendors' load balancers support TCP SSL/TLS termination. In addition, users need to deploy software such as HAProxy themselves. |

For information on how to terminate TLS connections through a proxy or load balancer, refer to [Cluster Load Balancing](../tutorial/deploy.md).

## Configure with Configuration File

::: tip

Before you start, you need to prepare the SSL/TLS certificates. EMQX only provides a set of SSL/TLS certificates (located in the `etc/certs` directory of the installation package) for testing purposes. When used in a production environment, reliable certificates signed by a trusted CA should be used. For information on how to apply for relevant certificates, see [Obtain SSL/TLS Certificates](#further-reading).

:::

1. Place your private SSL/TLS certificate files in the `etc/certs` directory of EMQX.
2. Replace the certificate into your own certificate in  `listeners.conf`. If you need to enable one-way authentication, add `verify = verify_none`:

```bash
listener.ssl.external.keyfile = etc/certs/key.pem
listener.ssl.external.certfile = etc/certs/cert.pem
listener.ssl.external.cacertfile = etc/certs/cacert.pem
```

If you need to enable two-way authentication, add the following configuration:

```bash
listener.ssl.external.verify = verify_peer
```

3. Restart EMQX to apply the configuration.

## Further Reading

You can obtain the SSL/TLS certificate in the following two ways:

1. Self-signed certificate: It means using a certificate that is issued by yourself. However, self-signed certificates have many security risks and are only recommended for testing and verification environments.
2. Apply or purchase a certificate: You can apply for a free certificate from [Let's Encrypt](https://letsencrypt.org) or cloud vendors such as Huawei Cloud and Tencent Cloud, or purchase a paid certificate from organizations such as [DigiCert](https://www.digicert.com/). For enterprise users, it is generally recommended to apply for paid OV or above certificates to obtain a higher level of security protection.

### Create Self-Signed Certificate

::: tip Prerequisite

[OpenSSL](https://www.openssl.org/) is installed.

:::

1. Run the following command to generate a key pair. The command will prompt you to enter a password to protect the key, which will be required for generating, issuing, and verifying the certificate. Keep the key and password secure.

   ```bash
   openssl genrsa -des3 -out rootCA.key 2048
   ```

2. Run the following command to generate a CA certificate using the private key from the key pair. The command will prompt you to set the certificate's Distinguished Name (DN).

   ```bash
   openssl req -x509 -new -nodes -key rootCA.key -sha256 -days 3650 -out rootCA.crt
   ```

3. Use the CA certificate from step 2 to issue a server certificate, which is used to verify the identity of the server owner. The server certificate is usually issued to the hostname, server name, or domain name (such as [www.emqx.com](http://www.emqx.com/)). We need to use the CA key (rootCA.key), CA certificate (rootCA.crt), and server CSR (server.csr) to generate the server certificate.

   - Run the following command to generate a key pair for the server certificate:

     ```bash
     openssl genrsa -out server.key 2048
     ```

   - Run the following command to create a CSR using the server key pair. After the CSR is signed by the CA root certificate private key, a certificate public key file can be generated and issued to the user. This command will also prompt you to set the Distinguished Name (DN) for the certificate.

     ```bash
     openssl req -new -key server.key -out server.csr
     ```

   - The system will prompt the following information, with corresponding meanings explained as below:

     ```bash
     You are about to be asked to enter information that will be incorporated
     into your certificate request.
     What you are about to enter is what is called a Distinguished Name or a DN.
     There are quite a few fields but you can leave some blank
     For some fields there will be a default value,
     If you enter '.', the field will be left blank.
     -----
     Country Name (2 letter code) [AU]: # country/region
     State or Province Name (full name) [Some-State]: # state/province
     Locality Name (eg, city) []: # The city or locality
     Organization Name (eg, company) [Internet Widgits Pty Ltd]: # The full name of the organization (or company name), e.g. EMQ
     Organizational Unit Name (eg, section) []: # The name of the department or division within the organization，e.g. EMQX
     Common Name (e.g. server FQDN or YOUR name) []: # The fully-qualified domain name (FQDN) of the server that will use the certificate, e.g. mqtt.emqx.com
     ...
     ```

   - Generate the server certificate and specify the validity period of the certificate, which is set to 365 days in this case:

     ```bash
     openssl x509 -req -in server.csr -CA rootCA.crt -CAkey rootCA.key -CAcreateserial -out server.crt -days 365
     ```

   You now have a set of certificates.

   ```bash
   .
   ├── rootCA.crt
   ├── rootCA.key
   ├── rootCA.srl
   ├── server.crt
   ├── server.csr
   └── server.key
   ```