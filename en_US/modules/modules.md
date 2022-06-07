# Module Management

The EMQX provides abundant functional modules, including authentication and authorization, protocol access, message delivery, language extension, operation and maintenance monitoring, and internal modules.
On the module management page of dashboard, modules can be started and stopped, as well as their configuration and data can be managed.

## Module list

The modules currently provided by the EMQX include:

- Authentication & Authorization
  - Builtin ACL file
  - MySQL Authentication/ACL
  - PostgreSQL Authentication/ACL
  - Redis Authentication/ACL
  - HTTP Authentication/ACL
  - Builtin database Authentication/ACL
  - MongoDB Authentication/ACL
  - LDAP Authentication/ACL
  - JWT Authentication
- Protocol Access
  - LwM2M protocol gateway
  - MQTT-SN protocol gateway
  - TCP protocol gateway
  - JT/T808 Protocol Gateway
  - CoAP protocol gateway
  - Stomp protocol gateway
- Message Delivery
  - Kafka consumer group
  - Pulsar Consumer Group
  - MQTT subscribers
- Language Extension
  - Protocol access
  - Hook
- Develop and Maintenance 
  - Recon
  - Prometheus Agent
  - Trace
- Internal Modules
  - Topic metrics
  - MQTT enhanced certification
  - MQTT online and offline notification
  - MQTT broker subscription
  - MQTT topic rewrite
  - MQTT retainr messages
  - MQTT delayed publish


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
