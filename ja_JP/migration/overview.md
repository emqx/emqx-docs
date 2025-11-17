# Migration Guides

This section provides comprehensive guides for migrating your IoT devices and solutions from other platforms to EMQX. Whether you are moving from a public cloud IoT service or another broker, these guides aim to provide a clear "happy path" for a seamless transition.

## Available Guides

Below are the step-by-step guides for migrating from specific platforms.

* [AWS IoT Core](./migrate-from-aws-iot-core.md)
* Azure IoT *(coming soon)*
* HiveMQ *(coming soon)*
* Mosquitto *(coming soon)*

## General Migration Principles

While each platform has unique specifics, most migrations to EMQX follow a similar three-phase pattern, especially when using X.509 client certificate (mTLS) authentication:

1. **Gather Existing Credentials**: Before starting, ensure you have access to your devices' existing cryptographic assets. This typically includes each device's private key and the public CA (Certificate Authority) certificate that was used to issue your device certificates.
2. **Configure EMQX Server-Side**: Set up the appropriate listeners on your EMQX cluster to handle the authentication method used by your devices. For a typical mTLS migration, this involves configuring an SSL/TLS listener to trust the CA that issued your device certificates.
3. **Update Device Client-Side**: Reconfigure your devices to connect to the new EMQX broker. This change is often limited to updating the endpoint address, port, and (if necessary) the server's root CA certificate that the client uses to verify the EMQX server.
