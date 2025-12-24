# Migration Guides

This section provides comprehensive guides for migrating your IoT devices and solutions from other platforms to EMQX. Whether you are moving from a public cloud IoT service or another broker, these guides offer a clear and reliable path to ensure a smooth migration experience.

## Available Guides

Below are the step-by-step guides for migrating from specific platforms.

* [AWS IoT Core](./migrate-from-aws-iot-core.md)
* [Azure IoT Hub](./migrate-from-azure-iot-hub.md)
* [HiveMQ](./migrate-from-hivemq.md)
* [Mosquitto](./migrate-from-mosquitto.md)

## General Migration Principles

While each platform has unique specifics, most migrations to EMQX follow a similar three-phase process, especially when devices use X.509 client certificates for mutual TLS (mTLS) authentication:

1. **Collect Existing Credentials**: Before starting, ensure you have access to your devices' existing cryptographic assets. This typically includes each device's private key and the public CA (Certificate Authority) certificate that was used to issue your device certificates.
2. **Configure EMQX Server Settings**: Configure appropriate listeners on your EMQX cluster to support the authentication mechanism used by your devices. For mTLS-based migrations, this generally involves setting up an SSL/TLS listener and configuring it to trust the CA that issued your device certificates.
3. **Update Device Configuration**: Modify your device settings to connect to the EMQX broker. In most cases, this involves updating the endpoint address, port, and (if necessary) the server's root CA certificate that the client uses to verify the EMQX server.
