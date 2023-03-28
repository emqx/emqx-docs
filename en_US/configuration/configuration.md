# Configuration Files

After completing the installation, startup, and testing of EMQX, you can better meet your business needs by modifying the configuration options.

EMQX supports configuration modification through environment variables and configuration options, where:

- Environment variables: Configurations for the overall operating environment and behavior of EMQX, such as listener address, log level, etc. They can be modified at the start or during the runtime of EMQX. In EMQX, environment variables usually have `EMQX_` as their prefix.
- Configuration items: Configuration items are provided in EMQX configuration file `emqx.conf`, which can be set for specific behaviors. Users can customize the settings through the EMQX configuration file.

## Environment Variables

Environment variables can be set by using various methods, such as setting them in the operating system's environment, passing them as command-line arguments, or setting them in a configuration file. <!--to be reviewed-->

For example, environment variable  `EMQX_NODE__NAME=emqx2@127.0.0.1`  will override the following configuration:

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

Configuration items and environment variables can be converted by the following rules:

1. Since the `.` separator in the configuration file cannot be used in environment variables, EMQX uses double underscores `__` as the configuration separator;
2. To distinguish the converted configuration items from other environment variables, EMQX also adds a prefix `EMQX_` to the environment variable;
3. The value of the environment variable is parsed according to the HOCON value, making it possible to use the environment variable to pass the value of complex data types, but please note that special characters such as `:` and `=` need to be wrapped in double quotes `"`.

Conversion example:

```bash
# Environment variables

## localhost:1883 will be parsed into a struct `{"localhost": 1883}`, so it needs to be wrapped in double quotes
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

## Pass the HOCON array directly by character
export EMQX_LISTENERS__SSL__DEFAULT__AUTHENTICATION__SSL__CIPHERS='["TLS_AES_256_GCM_SHA384"]'

# Configuration file
listeners.ssl.default {
  ...
  authentication {
    bind = "127.0.0.1:8883"
    ssl {
      ciphers = ["TLS_AES_256_GCM_SHA384"]
    }
  }
}
```

::: tip

EMQX will ignore undefined root paths, for example,  `EMQX_UNKNOWN_ROOT__FOOBAR` , because `UNKNOWN_ROOT` is not a pre-defined root path.

When a known root path is set with an unknown field name, EMQX will output a `warning` log at startup, for example, when `enable` is incorrectly configured as `enabled`, it will output:

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## Configuration Files



### **Main Configuration File**

EMQX will create a group of directories after installation, among which, `etc` is the folder that keeps all the configuration files. This section will focus on the main configuration file: `emqx.conf`. 

Depending on your installation mode, `emqx.conf` is stored in:

| Installation                       | Path                      |
| ---------------------------------- | ------------------------- |
| Installed with binary package      | `/etc/emqx/emqx.conf`     |
| Installed with compression package | `/opt/emqx/etc/emqx.conf` |
| Installed with Docker              | `./etc/emqx.conf`         |

As the main configuration file, `emqx.conf` contains most of the commonly used configuration items. You can follow the example provided in the `emqx-example.conf` file (located within the same folder) to customize the settings. EMQX will use the default setting for other items to simplify your configuration work.  

### Configuration Rewrite File

`emqx.conf` defines settings at a global level, for cases where you need to customize the settings for a cluster or a node, EMQX also provides a configuration rewrite file implicitly overlaying `emqx.conf`:

**`cluster-override.conf`**

Contains configuration items for the entire cluster, configuration changes made from Dashboard, REST API, and CLI will be persisted to this file.

If a certain cluster node is restarted or some new nodes are added, the node will automatically copy and apply the configuration file from other nodes within the cluster, therefore there is no need nor recommended to configure it manually.

The configuration rewrite files are located in the `$data/configs/` directory, and the path of the `data` directory varies according to the installation method:

| Installation                               | Path             |
| ------------------------------------------ | ---------------- |
| Installed with RPM or DEB package          | `/var/lib/emqx`  |
| Running in docker container                | `/opt/emqx/data` |
| Extracted from portable compressed package | `./data`         |

:::tip 

It is possible to change data directory from config `node.data_dir` or environment variable `EMQX_NODE__DATA_DIR`, however, when running a cluster, all nodes should have the same path. 

:::

By default, most global settings are defined in the `emqx.conf` file, if you perform certain operations on the cluster level with Dashboard, REST API or CLI, the changes will be synced with the `cluster-override.conf` as configuration files and overide the corresponding settings in `emqx.conf`. And this whole process is called hot reload.

For override rules, see [Configure override rules](./HOCON.md).

:::tip

Some configuration items cannot be overridden, for example, `node.name`.

:::

### Dot Syntax

EMQX uses the dot syntax, also known as dot notion, to reference specific elements within a structured data type, including `Struct`, `Map`, or `Array`. For example, for `node.name`, setting the value of a property called `name` on an object or variable called `node`.

::: tip

When accessing elements within an `Array` using dotted notation, a 1-based index should be used. This means that the first element of an Array is referred to using "1", rather than the traditional 0-based indexing. 

:::

Below are some examples, 

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

### TLS ciphers

Starting from v5.0.6, EMQX no longer pre-populates the ciphers list with a default
set of cipher suite names.
Instead, the default ciphers are applied at runtime when starting the listener
for servers, or when establishing a TLS connection as a client.

Below are the default ciphers selected by EMQX.

For tlsv1.3:

```bash
ciphers =
  [ "TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256",
    "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256",
    "TLS_AES_128_CCM_8_SHA256"
  ]
```

For tlsv1.2 or earlier

```bash
ciphers =
  [ "ECDHE-ECDSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-ECDSA-AES256-SHA384",
    "ECDHE-RSA-AES256-SHA384",
    "ECDH-ECDSA-AES256-GCM-SHA384",
    "ECDH-RSA-AES256-GCM-SHA384",
    "ECDH-ECDSA-AES256-SHA384",
    "ECDH-RSA-AES256-SHA384",
    "DHE-DSS-AES256-GCM-SHA384",
    "DHE-DSS-AES256-SHA256",
    "AES256-GCM-SHA384",
    "AES256-SHA256",
    "ECDHE-ECDSA-AES128-GCM-SHA256",
    "ECDHE-RSA-AES128-GCM-SHA256",
    "ECDHE-ECDSA-AES128-SHA256",
    "ECDHE-RSA-AES128-SHA256",
    "ECDH-ECDSA-AES128-GCM-SHA256",
    "ECDH-RSA-AES128-GCM-SHA256",
    "ECDH-ECDSA-AES128-SHA256",
    "ECDH-RSA-AES128-SHA256",
    "DHE-DSS-AES128-GCM-SHA256",
    "DHE-DSS-AES128-SHA256",
    "AES128-GCM-SHA256",
    "AES128-SHA256",
    "ECDHE-ECDSA-AES256-SHA",
    "ECDHE-RSA-AES256-SHA",
    "DHE-DSS-AES256-SHA",
    "ECDH-ECDSA-AES256-SHA",
    "ECDH-RSA-AES256-SHA",
    "ECDHE-ECDSA-AES128-SHA",
    "ECDHE-RSA-AES128-SHA",
    "DHE-DSS-AES128-SHA",
    "ECDH-ECDSA-AES128-SHA",
    "ECDH-RSA-AES128-SHA"
  ]
```

For PSK-enabled listeners

```bash
ciphers =
  [ "RSA-PSK-AES256-GCM-SHA384",
    "RSA-PSK-AES256-CBC-SHA384",
    "RSA-PSK-AES128-GCM-SHA256",
    "RSA-PSK-AES128-CBC-SHA256",
    "RSA-PSK-AES256-CBC-SHA",
    "RSA-PSK-AES128-CBC-SHA"
  ]
```
