# Network and TLS

Secure Sockets Layer (SSL) and Transport Layer Security (TSL) protocols are often adopted in network communications to ensure that the data transmission remains confidential and cannot be intercepted or modified by an attacker. The SSL/TLS encryption function encrypts network connections at the transport layer and involves the use of digital certificates to authenticate the identity of the parties involved and to establish a secure communication channel.

EMQX provides comprehensive support for SSL/TLS capabilities, including support for one-way/two-way authentication and X.509 certificate authentication. When accepting an MQTT Client or connecting to external resources like a database, EMQX can establish secure connections via SSL/TLS.

<!--Also include CRL and OCSP Stapling, see 4.4 doc-->

This chapter will cover the following topics:

- [Enable SSL/TLS Authentication](./emqx-mqtt-tls.md)

  This section will introduce how to enable SSL/TLS authentication for clients.

- [Client TLS](./mqtt-client-tls.md)

   This section lists the sample MQTT client code and project. 
   
   





