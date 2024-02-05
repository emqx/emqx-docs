# Security Guide

Security is a critical aspect of any MQTT-based application, and this guide is designed to help you understand and implement robust security measures in your EMQX deployments. This chapter explores various security-related topics, focusing on access control, authorization and network security using Transport Layer Security (TLS).

- [Networking and TLS](../network/overview.md) explain how EMQX supports end-to-end encrypted communication, including enabling SSL/TLS connections and obtaining SSL/TLS certificates.

- [Authentication](./authn/authn.md)

  Authentication is the process of verifying the identity of a client. It is essential to most applications and can help protect our services from illegal client connections. EMQX supports several authentication mechanisms to better protect our clients, including:

  - X.509 certificate authentication
  - Username/password authentication
  - JWT authentication
  - Enhanced authentication of MQTT 5.0
  - PSK authentication

  This section introduces how these authentication mechanisms work and how to configure them in EMQX.

- [Authorization](./authz/authz.md)

  In EMQX, authorization refers to the permission control over the publish/subscribe operation of the MQTT clients. This chapter will introduce how to use the built-in database, ACL file, or how to integrate with MySQL, PostgreSQL, MongoDB, or Redis to configure the authorization rules.

- [Banned Clients](./blacklist.md)

  EMQX provides a blacklisting/banning functionality. System admins can block certain clients from accessing EMQX via Dashboard or HTTP API with their client ID, user name, or IP address.

- [Flapping Detect](./flapping-detect.md)

  EMQX automatically bans frequently logging clients to prevent them from consuming server resources that may affect other clients.

- [Dashboard](../dashboard/dashboard.md)

  The Dashboard is a management interface for EMQX, there is also provides security strategy for it, for example, a frequent question is how to disable the swagger documentation, you can see its document for details.
