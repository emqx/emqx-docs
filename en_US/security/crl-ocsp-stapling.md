# CRL/OCSP Stapling

EMQX supports SSL/TLS, you can use X.509 certificate to enable client authentication and transport encryption.

Before the X.509 certificate expires, if the private key is exposed or the certificate is wrong, you need to revoke it to make sure your device is not being used illegally.
In this case you can use the CRL or OCSP Stapling feature to achieve a more secure setup.

## CRL

The CRL(Certificate Revocation List) is a list maintained by the CA that contains the serial numbers and revocation times of certificates that have been revoked.

You can configure the request endpoint of the CA on EMQX and refresh the CRLs regularly.

Different from the browser's mechanism for using CRLs, the MQTT client does not need to manage CRL and only need to verify via EMQX during the connection handshake.

All operations are run by EMQX, no special scripts or adaptations to the client are required.

For more details on how to use it, please refer to [Enable CRL](../configuration/configuration.md#listener-ssl-external-enable-crl-check).

## OCSP Stapling

OCSP(Online Certificate Status Protocol) is another certificate revocation solution, and OCSP Stapling is the latest improvement to OCSP technology.

When OCSP Stapling is enabled, EMQX will request certificate from the OCSP server and cache the result.

When the client sends an SSL handshake request to EMQX, EMQX sends the OCSP info of the certificate to the client along with the certificate chain(Stapling), and the client verifies the certificate validity.

For more details on how to use it, please refer to [Enable OCSP Stapling](../configuration/configuration.md#listener-ssl-external-enable-ocsp-stapling).
