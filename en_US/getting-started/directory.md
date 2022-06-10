# Directory

The directory structure of EMQX Broker obtained by different installation methods will be different:

| Description                            | ZIP          | Binary                   |
| -------------------------------------- | ------------ | ------------------------ |
| Executable file directory              | `./bin`      | `/usr/lib/emqx/bin`      |
| Data files                             | `./data`     | `/var/lib/emqx/`         |
| Erlang Virtual machine files           | `./erts-*`   | `/usr/lib/emqx/erts-*`   |
| Configuration file directory           | `./etc`      | `/etc/emqx/`             |
| Dependency directory                   | `./lib`      | `/usr/lib/emqx/lib`      |
| Log file                               | `./log`      | `/var/log/emqx`          |
| Start related scripts and schema files | `./releases` | `/usr/lib/emqx/releases` |

In the above directories,  `bin`, `etc`, `data` and`log` are commonly used by users.

## bin directory

**emqx、emqx.cmd**

The executable file of EMQX Broker can be found in [Basic Command](./command-line.md).

**emqx_ctl、emqx_ctl.cmd**

The executable file of EMQX Broker management command, you can check [Management Command CLI](../advanced/cli.md) for specific use.

## etc directory

EMQX Broker is set by configuration files in the `etc` directory. The main configuration files include:

| Configuration file | Description            |
| -------------- | ------------------------- |
| emqx.conf      | EMQX Broker configuration file |
| acl.conf       | EMQX Broker default ACL rule configuration file |
| plugins/*.conf | EMQX Broker various plug-in configuration files |
| certs          | EMQX Broker SSL certificate file |

The specific configuration content of EMQX Broker can be viewed in [Configuration Item](../configuration/configuration.md).

## data directory

EMQX Broker stores the running data in the `data` directory. The main files include:

**configs/app.*.config**

EMQX Broker reads the configuration in `etc/emqx.conf` and `etc/plugins/*.conf`, converts it to the Erlang native configuration file format, and reads the configuration at runtime.

**loaded_plugins**

The `loaded_plugins` file records the list of plug-ins that was started by EMQX Broker by default. You can modify this file to add or delete plug-ins. The startup item format in `loaded_plugins` is `{<Plugin Name>, <Enabled>}. `, the `<Enabled> `field is a Boolean type, and EMQX Broker will determine whether to start this plugin according to the value of `<Enabled> `. For more information about plugins, please see [plug-ins](../advanced/plugins.md).


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

EMQX Broker uses the Mnesia database to store its own running data, such as alarm records, resources and rules created by the rule engine, Dashbaord user information, etc. These data will be stored under the `mnesia` directory. Once the directory is deleted, EMQX Broker will lose all business data.

You can query the system information of the Mnesia database in EMQX Broker through the `emqx_ctl mnesia` command. For details, please see [Management Command CLI](../advanced/cli.md).


## log directory

**emqx.log.***

For the log file generated when EMQX Broker is running, please check [Log and Trace](./log.md).

**crash.dump**

The crash dump file of EMQX Broker can be modified through `etc/emqx.conf` , and the specific content can be viewed in [configuration item](../configuration/configuration.md).

**erlang.log.***

It can control the copy of the console log when EMQX Broker is started in the background with `emqx start`.    
