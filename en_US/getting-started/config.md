# Configuration

## Introduction

The configuration files of EMQ X Broker usually have the suffix `.conf`. You can find these configuration files in the `etc` directory.

| File        | Description            |
| ------------------ | ------------------------- |
| etc/emqx.conf      | EMQ X Broker Configuration File |
| etc/acl.conf       | EMQ X Broker default ACL File |
| etc/plugins/*.conf | Configuration Files of Plugins |

It should be noted that for the EMQ X Broker installed by different methods, the path of the `etc` directory may be different. For details, please refer to [directory structure](directory.md#).

## Grammar rules

- Use k = v common format like sysctl
- All information for a single configuration item is on the same line, and a new line means creating a new configuration item
- The key can be layered by `.`, support configuration items managed by tree structure
- Value types can be `integer`, `float`, `percent`, `enum`, `ip`, `string`, `atom`, `flag`, `duration` and `bytesize`
- Any line beginning with # is considered as a comment

**Example:**

```bash
mqtt.max_packet_size = 1MB
```

## Data type

**integer**

**float**

**percent**

The percentage data ending in `%` , that will eventually be converted to `float` type.

**enum**

Usually we will list all its optional values near the configuration item of type `enum`. Of course, you can also search for  [configuration item](../configuration/configuration.md).

**ip**

When you see that the data type of a configuration item is `ip`, it means that you can set the configuration item in the form of <IP>: <Port>`, for example, `0.0.0.0: 1883`.

**string**

Everything in the `*.conf` file except for comments will be parsed into a string and then converted to other types, so there is no need to use double quotes to modify the value of the `string` type value, and this way is not supported.

*Yes!*

```bash
dir = tmp
```

*No!!!*

```bash
dir = "tmp"		
```

**atom**

A value of type `atom` will eventually be converted into Erlang ’s `atom`, but its using method in the `*.conf` file is exactly the same as `string`.

**flag**

`flag` is used for variables that have two possible values. The default available values of `flag` are `on` and `off`, which will be mapped to `true` and `false` for application. If we have established other mapping relationships for a configuration item, we will indicate it in the configuration file, and you can also find this information in [configuration item](../configuration/configuration.md).

**duration**

`duration` is used to specify those fixed time intervals, and you can use the following time units:

- f - fortnight
- w - week
- d - day
- h - hour
- m - minute
- s - second
- ms - millisecond

You can arbitrarily combine these time units, such as `1w13ms`, or you can use floating point numbers, such as` 0.5d`, and these time intervals will eventually be converted to the base unit we specify. It should be noted here is that if you set a configuration item in milliseconds and its base unit is seconds, it will round up to the closest description, for example, `1s50ms` =` 2s`. Therefore, we will list the benchmark units for this type of configuration item.

**bytesize**

`bytesize` supports configuration of message size and buffer size in a more readable way, and the unit can be `KB`, `MB` and `GB`. You can also use lower case, for example `kb`, but mixed case, such as `Kb`, is not supported. It will eventually be converted to bytes. If you do not specify any units, then it is used directly as the number of bytes.

## Default configuration

In the configuration file of EMQ X Broker, you will see a lot of configuration items that are commented out, which means that these configuration items will use their default values. Usually we will list the default values of these configurations.

## Zone & Listener

EMQ X Broker provides a lot of configuration items, and supports global configuration and local configuration. For example, EMQ X Broker provides an anonymous login function, which allows clients to connect to the broker without a user name and password. Usually this feature is disabled by default in the user's production environment, but the user may want this feature enabled in the intranet environment. Since version 3.0, EMQ X Broker has provided this possibility to users through Zone and Listener.

### Listener

Listener is mainly used to configure listening ports and related parameters of different protocols. EMQ X Broker supports configuring multiple Listeners to listen to multiple protocols or ports at the same time. The following are the supported Listeners:

| Listener             | Description                                  |
| ------------------------- | ------------------------------------------------------- |
| TCP Listener              | A listener for MQTT which uses TCP                      |
| SSL Listener              | A secure listener for MQTT which uses TLS               |
| Websocket Listener        | A listener for MQTT over WebSockets                     |
| Secure Websocket Listener | A secure listener for MQTT over secure WebSockets (TLS) |

EMQ X Broker provides 5 Listeners by default, and they will occupy the following ports:

| Port | Description                             |
| ----- | ------------------------------------------ |
| 1883  | MQTT/TCP protocol port              |
| 11883 | MQTT/TCP Protocol internal port, only used for local client connection |
| 8883  | MQTT/SSL protocol port              |
| 8083  | MQTT/WS protocol port               |
| 8084  | MQTT/WSS protocol port              |

The naming rule of the Listener configuration item is  `listener.<Protocol>.<Listener Name>.xxx`, and ` Protocol> ` is the protocol used by the Listener that currently supports `tcp`, `ssl`,` ws`, `wss` . `<Listener Name>` can be named arbitrarily, but it is recommended to use all lowercase words, and `xxx` is a specific configuration item. The `<Listener Name>` of Listeners with different protocols can be repeated. `Listener.tcp.external` and `listener.ssl.external` are two different Listeners.

Due to the existence of the default configuration, we can quickly show how to add a new Listener. Taking TCP Listener as an example, we only need to add the following configuration in `emqx.conf`:

```bash
listener.tcp.example = 12345
```

Of course, in this case, we recommend that you copy the default Listener configuration for modification.

### Zone

A Zone defines a set of configuration items (such as the maximum number of connections), and the Listener can specify the Zone through the configuration item `listener.<Protocol>.<Listener Name>.zone` to use all the configurations under the Zone. Multiple Listeners can share the same Zone. The naming rule of Zone is `zone.<Zone Name>.xxx`. `Zone Name` can be named at will, but it is also recommended to be all lowercase. `xxx` is a specific configuration item, you can find it in [configuration item](../configuration/configuration.md) to view all configuration items supported by Zone.

At this time, there are three available values for each of our configuration items, which are the global value, the value set in Zone and the default value, and their priority order is: Zone> Global> Default.