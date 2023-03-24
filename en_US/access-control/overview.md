# Access Control

This chapter will introduce how to configure the access control policies in EMQX, covering the following topics:

- [Authentication](./authn/authn.md)

  Authentication is the process of verifying the identity of a client. It is an essential part of most applications and can help to protect our services from illegal client connections. EMQX supports several authentication mechanisms to better protect our clients, including:

  - Username/password authentication
  - JWT authentication
  - Enhanced authentication of MQTT 5.0

- [Authorization](./authz/authz.md)

  In EMQX, authorization refers to the permission control over the publish/subscribe operation of the MQTT clients. This chapter will introduce how to use the built-in database, ACL file, or how to integrate with MySQL, PostgreSQL, MongoDB or Redis to configure the authorization rules. 

- [Blacklist](./blacklist.md)

  EMQX provides a blacklisting/banning functionality. System admins can block certain clients from accessing EMQX via Dashboard or HTTP API with their client ID, user name, or IP address. 
  
  



