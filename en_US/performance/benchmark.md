# Benchmark

[emqtt_bench](https://github.com/emqx/emqtt_bench) is a concise and powerful MQTT protocol performance testing tool written with Erlang. If you need  testing services with large-scale scenarios and in-depth customization , the test service provided by EMQ partners [XMeter](https://www.xmeter.net/) is recommended.

## Compile and install

The operation of `emqtt_bench` depends on the operating environment of Erlang/OTP 21.2 and above version. The installation process is skipped. For details, please refer to the online installation tutorials.

After the Erlang environment is installed, download the latest code of `emqtt-bench` and compile it:

```bash
git clone https://github.com/emqx/emqtt-bench
cd emqtt-bench

make
```

After the compilation, an executable script named `emqtt_bench` will be generated in the current directory. Execute the following command to confirm that it can be used normally:

```bash
./emqtt_bench
Usage: emqtt_bench pub | sub | conn [--help]
```

The output of the above content proves that `emqtt_bench` has been correctly installed on the host.


## Use

There are three subcommands of `emqtt_bench`:

1. `pub`: used to create a large number of clients to perform the operation of publishing messages.
2. `sub`: Used to create a large number of clients to subscribe to topics and receive messages.
3. `conn`: used to create a large number of connections.


### Publish

When executing `./emqtt_bench pub --help`, you will get the available parameter output.

| Parameter         | abbreviation | Optional value  | Default value  | Description                                                  |
| ----------------- | ------------ | --------------- | -------------- | ------------------------------------------------------------ |
| --host            | -h           | -               | localhost      | Address of the MQTT server to connect                        |
| --port            | -p           | -               | 1883           | MQTT service port                                            |
| --version         | -V           | 3<br>4<br>5     | 5              | MQTT protocol version used                                   |
| --count           | -c           | -               | 200            | Total number of clients                                      |
| --startnumber     | -n           | -               | 0              | Start number of clients                                      |
| --interval        | -i           | -               | 10             | Interval to create a client; unit: ms                        |
| --interval_of_msg | -I           | -               | 1000           | Interval to publish a message                                |
| --username        | -u           | -               | None; optional | Client username                                              |
| --password        | -P           | -               | None; optional | Client password                                              |
| --topic           | -t           | -               | None; required | Published topics; support placeholders:<br />`%c`： ClientId<br />`%u`： Username<br />`%i`：Client's sequence number |
| --size            | -s           | -               | 256            | Message Payload size; unit: bytes                            |
| --qos             | -q           | -               | 0              | QoS level                                                    |
| --retain          | -r           | true<br />false | false          | Whether the message sets the Retain flag                     |
| --keepalive       | -k           | -               | 300            | Client keepalive time                                        |
| --clean           | -C           | true<br />false | true           | Whether to establish a connection by cleaning the session    |
| --ssl             | -S           | true<br />false | false          | Whether to enable SSL                                        |
| --certfile        | -            | -               | None           | Client SSL certificate                                       |
| --keyfile         | -            | -               | None           | Client SSL key file                                          |
| --ws              | -            | true<br />false | false          | Whether to establish a connection via WebSocket              |
| --ifaddr          | -            | -               | None           | Specifies the local network card used by the client connection |



For example, we start 10 connections and send 100 Qos0 messages to the topic `t` every second, where the size of each message payload is` 16` bytes:

```bash
./emqtt_bench pub -t t -h emqx-server -s 16 -q 0 -c 10 -I 10
```

### Subscribe

Execute `./emqtt_bench sub --help` to get all available parameters of this subcommand. Their explanations have been included in the table above and are omitted here.



For example, we start 500 connections, and each subscribes to the `t` topic with Qos0:

```bash
./emqtt_bench sub -t t -h emqx-server -c 500
```



### Connect

Execute `./emqtt_bench conn --help` to get all available parameters of this subcommand. Their explanations have been included in the table above and are omitted here.



For example, we start 1000 connections:

```bash
./emqtt_bench conn -h emqx-server -c 1000
```



### SSL connection

`emqtt_bench` supports establishing a secure SSL connection and performing tests.

One-way certificate:

```bash
./emqtt_bench sub -c 100 -i 10 -t bench/%i -p 8883 -S
./emqtt_bench pub -c 100 -I 10 -t bench/%i -p 8883 -s 256 -S
```



Two-way certificate:

```bash
./emqtt_bench sub -c 100 -i 10 -t bench/%i -p 8883 --certfile path/to/client-cert.pem --keyfile path/to/client-key.pem
./emqtt_bench pub -c 100 -i 10 -t bench/%i -s 256 -p 8883 --certfile path/to/client-cert.pem --keyfile path/to/client-key.pem
```

## Typical stress test scenario

### Scenario description

We verify the use of the tool in 2 most typical scenarios:

1. Connections: Use `emqtt_bench` to create millions of connections to EMQX Broker.
2. Throughput: Use `emqtt_bench` to create `10W / s Qos0` message throughput in EMQX Broker.

### Device and deployment topology

A total of three 8C16G servers need to be prepared, one for EMQX Broker and two for client presses:

- System: `CentOS Linux release 7.7.1908 (Core)`

- **CPU:**  `Intel Xeon Processor (Skylake)` Main frequency: `2693.670 MHZ`

- **Server：** `emqx-centos7-v4.0.2.zip`

- **Press：**`emqtt-bench v0.3.1`
  - Each press is configured with 10 network cards, which are used to establish a large number of MQTT client connections in the connection test

The topology structure is as follows:

```
	+-----------------------+
	| bench1: 192.168.0.100 | -------					+--------------------------+
	+-----------------------+        \----->  |		    EMQX Broker			 |
	+-----------------------+        /----->  |       192.168.0.99	  	 |
	| bench2: 192.168.0.200 | -------					+--------------------------+
	+-----------------------+
```


### Tuning

Both the client's press and the server's machine need to perform system parameter tuning, refer to [Tuning guide](../deploy/tune.md).


### Connection test

After performing system tuning, start the server:

```bash
./bin/emqx start
```



Then start 50 thousand connections on each network card on `bench1`, which is a total of 50w connections:

```bash

./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.100
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.101
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.102
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.103
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.104
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.105
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.106
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.107
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.108
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.109
```



Perform the same operation on `bench2`.



After all connections are established, execute `./bin/emqx_ctl listeners` and find the following content to view the information about the number of connections in EMQX Broker:

```bash
listener on mqtt:tcp:0.0.0.0:1883
  acceptors       : 8
  max_conns       : 1024000
  current_conn    : 1000000
  shutdown_count  : []
```



### Throughput test

Similarly, first start the server:

```bash
./bin/emqx start
```



Start 500 subscription clients in `bench1`:

```bash
./emqtt_bench sub -t t -h 192.168.0.99 -c 500
```



Then start 20 publishers on `bench2` and publish 10 messages per second:

```bash
./emqtt_bench pub -t t -h 192.168.0.99 -c 20 -I 100
```



Then, go back to the subscribing client on `bench1`, you can see the current rate of receiving messages:

```bash
recv(28006): total=2102563, rate=99725(msg/sec)
```
