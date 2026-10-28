---
description: "Connect MQTTX CLI through a prepared TLS listener, validate the server certificate, and verify MQTT message delivery."
next: false
---

# Connect over TLS

Connect through a prepared MQTT TLS listener, validate its certificate, and exchange a message. TLS protects the transport; keep the MQTT credentials and topic permissions required by that listener.

This is an optional exercise. Complete this lesson when you have a TLS endpoint ready. Otherwise, continue to [MQTT Basics](../develop/mqtt/basics.md); you have already completed the basic connection, messaging, and access-control exercises.

## Prepare the TLS Endpoint

The Quick Start Docker command maps only ports `1883` and `18083`. It does **not** map port `8883` or prepare the certificate files used below. Changing the MQTTX port alone is not enough.

Docker port mappings are set when a container is created. Adding `8883` therefore requires a replacement container; restarting the current one does not add the port. A fresh container does not inherit your MQTT accounts or authorization rules. Keep your existing learning broker until you have planned its data mounts and configuration, or use a separate TLS test deployment with its own credentials. Follow the [Docker deployment guide](deploy/install-docker.md) for persistent data and port mappings.

Before running the client commands, you need:

- A reachable TLS listener with a server certificate and its private key configured.
- The CA trust bundle for that certificate, available on your computer.
- A hostname or IP address covered by the certificate's Subject Alternative Name (SAN).
- MQTT credentials accepted by that broker and permission to publish and subscribe on `docs/tls`.

For the exact local example below, the listener must be reachable at `127.0.0.1:8883`, the server certificate must cover IP address `127.0.0.1`, and the trust bundle must be named `rootCA.crt` in your current terminal directory. Using the explicit IPv4 address matches the loopback Docker mapping; `localhost` can resolve to IPv6 `::1` instead. On another deployment, replace the host, port, and CA path with those supplied by your operator.

If you need to prepare your own endpoint, continue in Develop to [create test certificates](../develop/tls/certificates.md#create-a-private-test-ca) and [configure the TLS listener](../develop/tls/enable.md#enable-server-authentication). For Docker, also meet the [container networking and certificate-file prerequisites](../develop/tls/enable.md#before-you-begin). Return here once the listener and trust bundle are ready. You can continue with [MQTT Basics](../develop/mqtt/basics.md) without completing TLS setup.

## Keep Authentication and Permissions in Place

If this is the broker used in the previous exercises, keep the `mqtt-user` account. Complete the [authorization exercise's cleanup](topic-permissions.md#restore-the-local-learning-policy) before this test so that `docs/tls` is permitted under the restored local policy.

For that authenticated local broker, add `-u mqtt-user -P 'Example-only-password!'` to **both** client commands below. These are the disposable credentials from the earlier lesson. If you use a different broker, use its configured credentials and topic policy instead; the account on your Quick Start instance does not exist there automatically.

<!-- include: ../develop/tls/enable.md#verify-a-message-exchange -->

<!-- include: ../develop/tls/enable.md#troubleshoot-and-clean-up -->

## Continue Learning

Next, review [MQTT Basics](../develop/mqtt/basics.md), followed by [connections and sessions](messaging/mqtt-concepts.md).

For client certificates and mutual TLS, [continue in Develop](../develop/tls/enable.md#require-mutual-tls). When you finish the learning path, remove the disposable broker with [Quick Start cleanup](quick-start.md#clean-up).
