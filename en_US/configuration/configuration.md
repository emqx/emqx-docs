# Configuration Files

After completing the installation, startup, and testing of EMQX, you can better meet your business needs by customizing the configurations.

EMQX supports configuration modification through configuration items and environment variables:

- [Configuration items](#configuration-items): Configuration items are various components that are configurable in EMQX. It provides a flexible and powerful way to customize EMQX to meet specific use cases and requirements. 
- [Environment variables](#environment-variables): Configurations for the overall operating environment and behaviour of EMQX, such as listener address, log level, etc. They can be modified at the start or during the runtime of EMQX. In EMQX, environment variables usually have `EMQX_` as their prefix.

## Configuration Items

In EMQX, configuration items are stored in the configuration files. There are 2 configuration files in EMQX, the main configuration file, `emqx.conf` and `cluster-override.conf` for storing cluster-specific configurations. 

::: tip

For most configuration items, you can also modify them with EMQX Dashboard. Changes via the Dashboard will be immediately applied without requiring a restart. The updated settings will be saved in `emqx.conf`, and any previous settings for the same configuration items in that file will be overridden.

:::

### **Main Configuration File**

EMQX creates a group of directories after installation, among which, `etc` is the folder that keeps all the configuration files. This section will focus on the main configuration file: `emqx.conf`. 

Depending on your installation mode, `emqx.conf` is stored in:

| Installation                               | Path                      |
| ------------------------------------------ | ------------------------- |
| Installed with RPM or DEB package          | `/etc/emqx/emqx.conf`     |
| Running in docker container                | `/opt/emqx/etc/emqx.conf` |
| Extracted from portable compressed package | `./etc/emqx.conf`         |

As the main configuration file, `emqx.conf` contains most of the commonly used configuration items. You can follow the example provided in the `emqx-example.conf` file (located within the same folder) to customize the settings. EMQX will use the default setting for other items to simplify your configuration work.  

### Configuration Rewrite File

The `cluster-override.conf` file is the configuration rewrite file. For cases where you need to customize the settings for a cluster or a node with Dashboard, REST API, or CLI, the changes will be persisted to the `cluster-override.conf` and override the corresponding settings in `emqx.conf`. And this whole process is called hot reload. However, Some configuration items cannot be overridden, for example, `node.name`.

::: tip

If a certain cluster node is restarted or some new nodes are added, the node will automatically copy and apply the configuration file from other nodes within the cluster, therefore there is no need nor recommended to configure it manually. 

:::

The configuration rewrite files are located in the `$data/configs/` directory, and the path of the `data` directory varies according to the installation method:

| Installation                               | Path             |
| ------------------------------------------ | ---------------- |
| Installed with RPM or DEB package          | `/var/lib/emqx`  |
| Running in docker container                | `/opt/emqx/data` |
| Extracted from portable compressed package | `./data`         |

:::tip 

It is possible to change the data directory from config `node.data_dir` or environment variable `EMQX_NODE__DATA_DIR`, however, when running a cluster, all nodes should have the same path. 

:::

This chapter will introduce how to customize the configuration items for the following subjects:

- [MQTT](./mqtt.md)
- [Cluster](./cluster.md)
- [Listener](listener.md)
- [Flapping](./flappping.md)
- [Limiter](./limiter)
- [Logs](./logs.md)
- [Prometheus](./prometheus.md)
- [Dashboard](./dashboard.md)

### Configuration File Format

EMQX uses HOCON as the configuration file format since version 5.0. For a detailed explanation of the syntax, data types supported, and override rule, you can continue to read [Configuration File Format](./HOCON.md)

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
