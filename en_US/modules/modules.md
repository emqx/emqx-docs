# Module Management

The EMQX provides abundant functional modules, including authentication and authorization, protocol access, message delivery, language extension, operation and maintenance monitoring, and internal modules.
On the module management page of dashboard, modules can be started and stopped, as well as their configuration and data can be managed.

## Module list

The modules currently provided by the EMQX include:

- Authentication & Authorization
  - [Builtin ACL File](./internal_acl.md)
  - [MySQL AUTH/ACL](./mysql_authentication.md)
  - [PostgreSQL AUTH/ACL](./pgsql_authentication.md)
  - [Redis AUTH/ACL](./redis_authentication.md)
  - [HTTP AUTH/ACL](./http_authentication.md)
  - [Builtin Database AUTH/ACL](./mnesia_authentication.md)
  - [MongoDB AUTH/ACL](./mongo_authentication.md)
  - [PSK File AUTH](./psk_authentication.md)
  - [LDAP AUTH/ACL](./ldap_authentication.md)
  - [JWT AUTH](./jwt_authentication.md)
  - [GCP IoT Core Device](./GCP_device.md)
- Protocol Access
  - [LwM2M Gateway](./lwm2m_protocol.md)
  - [MQTT-SN Gateway](./mqtt_sn_protocol.md)
  - [TCP Gateway](./tcp_protocol.md)
  - [JT/T808 Gateway](./jt808_protocol.md)
  - [CoAP Gateway](./coap_protocol.md)
  - [Stomp Gateway](./stomp_protocol.md)
- Message Delivery
  - [Kafka Consumer Group](./kafka_consumer.md)
  - [Pulsar Consumer Group](./pulsar_consumer.md)
  - [MQTT Subscriber](./mqtt_subscriber.md)
- Language Extension
  - [Protocol Access](./exproto.md)
  - [Hook](./exhook.md)
- Develop and Maintenance 
  - [Recon](./recon.md)
  - [Prometheus Agent](./prometheus.md)
- Internal Modules
  - [Hot Configuration](./hot_confs.md)
  - [Topic Metrics](./topic_metrics.md)
  - [Online and Offline Notification](./presence.md)
  - [MQTT Proxy Subscription](./subscription.md)
  - [MQTT Topic Rewrite](./topic_rewrite.md)
  - [MQTT Retainer Message](./retainer.md)
  - [MQTT Delayed Publish](./delayed_publish.md)
  - [Audit Log](./audit-log.md)


## Start and Stop Modules

Currently there are two ways to start the module:

1. Load modules with system 
2. Use Dashboard to start and stop the module

**Enable loading modules with system**

If you need to start a certain module when EMQX starts, you can directly add the module that needs to be started in `data/loaded_modules`.

For example, the modules automatically loaded by EMQX are:

```json
[
    {
    "name": "internal_acl",
    "enable": true,
    "configs": {"acl_rule_file": "{{ acl_file }}"}
    },
    {
        "name": "presence",
        "enable": true,
        "configs": {"qos": 0}
    },
    {
        "name": "recon",
        "enable": true,
        "configs": {}
    },
    {
        "name": "retainer",
        "enable": true,
        "configs": {
            "expiry_interval": 0,
            "max_payload_size": "1MB",
            "max_retained_messages": 0,
            "storage_type": "ram"
        }
    }
]
```

**Use Dashboard to start and stop the module**

If the Dashboard module is enabled, you can directly start and stop the module by accessing the module management page at `http://localhost:18083/modules`.
