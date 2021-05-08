# Directory

The directory structure of EMQ X Broker obtained by different installation methods will be different:

| Description                            | ZIP          | Binary                   | Homebrew (MacOS)                     |
| -------------------------------------- | ------------ | ------------------------ | ----------------------------------- |
| Executable file directory              | `./bin`      | `/usr/lib/emqx/bin`      | `/usr/local/bin`                    |
| Data files                             | `./data`     | `/var/lib/emqx/data`     | `/usr/local/Cellar/emqx/*/data`     |
| Erlang Virtual machine files           | `./erts-*`   | `/usr/lib/emqx/erts-*`   | `/usr/local/Cellar/emqx/*/erts-`    |
| Configuration file directory           | `./etc`      | `/etc/emqx/etc`          | `/usr/local/Cellar/emqx/*/etc`      |
| Dependency directory                   | `./lib`      | `/usr/lib/emqx/lib`      | `/usr/local/Cellar/emqx/*/lib`      |
| Log file                               | `./log`      | `/var/log/emqx`          | `/usr/local/Cellar/emqx/*/log`      |
| Start related scripts and schema files | `./releases` | `/usr/lib/emqx/releases` | `/usr/local/Cellar/emqx/*/releases` |

In the above directories,  `bin`, `etc`, `data` and`log` are commonly used by users.

## bin directory

**emqx、emqx.cmd**

The executable file of EMQ X Broker can be found in [Basic Command](./command-line.md).

**emqx_ctl、emqx_ctl.cmd**

The executable file of EMQ X Broker management command, you can check [Management Command CLI](../advanced/cli.md) for specific use.

## etc directory

EMQ X Broker is set by configuration files in the `etc` directory. The main configuration files include:

| Configuration file | Description            |
| -------------- | ------------------------- |
| emqx.conf      | EMQ X Broker configuration file |
| acl.conf       | EMQ X Broker default ACL rule configuration file |
| plugins/*.conf | EMQ X Broker various plug-in configuration files |
| certs          | EMQ X Broker SSL certificate file |

The specific configuration content of EMQ X Broker can be viewed in [Configuration Item](../configuration/configuration.md).

## data directory

EMQ X Broker stores the running data in the `data` directory. The main files include:

**configs/app.*.config**

EMQ X Broker reads the configuration in `etc/emqx.conf` and `etc/plugins/*.conf`, converts it to the Erlang native configuration file format, and reads the configuration at runtime.

**loaded_plugins**

The `loaded_plugins` file records the list of plug-ins that was started by EMQ X Broker by default. You can modify this file to add or delete plug-ins. The startup item format in `loaded_plugins` is `{<Plugin Name>, <Enabled>}. `, the `<Enabled> `field is a Boolean type, and EMQ X Broker will determine whether to start this plugin according to the value of `<Enabled> `. For more information about plugins, please see [plug-ins](../advanced/plugins.md).

{% emqxce %}

```bash
$ cat loaded_plugins
{emqx_management,true}.
{emqx_recon,true}.
{emqx_retainer,true}.
{emqx_dashboard,true}.
{emqx_rule_engine,true}.
{emqx_bridge_mqtt,false}.
```

{% endemqxce %}

{% emqxee %}

```bash
$ cat loaded_plugins
{emqx_management, true}.
{emqx_recon, true}.
{emqx_retainer, true}.
{emqx_conf, true}.
{emqx_dashboard, true}.
{emqx_schema_registry, true}.
{emqx_rule_engine, true}.
{emqx_bridge_mqtt, false}.
{emqx_cube, false}.
```

{% endemqxee %}

**mnesia**

Mnesia database is a distributed DBMS in Erlang, which can directly store various data structures of Erlang.

EMQ X Broker uses the Mnesia database to store its own running data, such as alarm records, resources and rules created by the rule engine, Dashbaord user information, etc. These data will be stored under the `mnesia` directory. Once the directory is deleted, EMQ X Broker will lose all business data.

You can query the system information of the Mnesia database in EMQ X Broker through the `emqx_ctl mnesia` command. For details, please see [Management Command CLI](../advanced/cli.md).


## log directory

**emqx.log.***

For the log file generated when EMQ X Broker is running, please check [Log and Trace](./log.md).

**crash.dump**

The crash dump file of EMQ X Broker can be modified through `etc/emqx.conf` , and the specific content can be viewed in [configuration item](../configuration/configuration.md).

**erlang.log.***

It can control the copy of the console log when EMQ X Broker is started in the background with `emqx start`.    